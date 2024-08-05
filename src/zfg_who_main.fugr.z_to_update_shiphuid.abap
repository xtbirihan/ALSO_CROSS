FUNCTION z_to_update_shiphuid.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PALLET_ID) TYPE  /SCWM/GUID_HU
*"     VALUE(IT_PACKED) TYPE  /SCWM/TT_ORDIM_O_INT
*"----------------------------------------------------------------------
********************************************************************
*& Key          : BSUGAREV-Dec 13, 2023
*& Request No.  : GAP-016 - Outbound Cartonization Algorithm
********************************************************************
*& Description  : In packing process (SPED) shiphuid of the task is
*&    updated with the pallet pack material ID. IN cases of Split or
*&    MERGE SHIPHUID field is not updated. So we do that here
********************************************************************
  DATA: lt_whohu          TYPE /scwm/tt_whohu_int,
        lt_ordim_param_r  TYPE rseloption,
        ls_ordim_o        TYPE /scwm/ordim_o,
        lt_ordim_prechech TYPE /scwm/tt_ordim_o,
        ls_ordim_o_lock   TYPE /scwm/ordim_o ##NEEDED,
        lt_ordim_o        TYPE /scwm/tt_ordim_o.

  BREAK-POINT ID zcg_out_who_split.

  DATA(ls_defdata) = VALUE #( it_packed[ 1 ] OPTIONAL ).

  IF abap_false = zcl_switch=>get_switch_state(
      iv_lgnum  = ls_defdata-lgnum
      iv_devid  = zif_switch_const=>c_zout_020
      it_fields = VALUE #( ( field = zif_switch_const=>c_doccat
                             field_value = ls_defdata-rdoccat ) ) ).
    LOG-POINT ID zcg_out_who_split
       FIELDS iv_pallet_id
              it_packed
              ls_defdata-rdoccat.
    RETURN.
  ENDIF.

  lt_ordim_param_r = VALUE #(  FOR <ls_param_check> IN  it_packed
                          ( sign   = wmegc_sign_inclusive
                            option = wmegc_option_eq
                            low    = <ls_param_check>-tanum ) ).

  CALL FUNCTION '/SCWM/TO_READ_MULT'
    EXPORTING
      iv_lgnum         = ls_defdata-lgnum
      ir_tanum         = lt_ordim_param_r
      iv_add_to_memory = abap_true
    IMPORTING
      et_ordim_o       = lt_ordim_prechech
    EXCEPTIONS
      wrong_input      = 1
      not_found        = 2
      OTHERS           = 3.

  IF sy-subrc <> 0 AND
    lt_ordim_param_r IS NOT INITIAL.
    LOG-POINT ID zcg_out_who_split
       FIELDS iv_pallet_id
              it_packed
              ls_defdata-rdoccat
              lt_ordim_prechech.
  ELSE.
    ls_defdata-tanum = lt_ordim_prechech[ 1 ]-tanum.
  ENDIF.

  DO 5 TIMES.
    CALL FUNCTION '/SCWM/TO_READ_SINGLE'
      EXPORTING
        iv_lgnum         = ls_defdata-lgnum
        iv_tanum         = ls_defdata-tanum
        iv_flglock       = abap_false
        iv_add_to_memory = abap_false
        iv_read_from_db  = abap_true
      IMPORTING
        es_ordim_o       = ls_ordim_o
      EXCEPTIONS
        wrong_input      = 1
        not_found        = 2
        foreign_lock     = 3
        error            = 4
        OTHERS           = 5.
    IF sy-subrc <> 0.
    ENDIF.

    " In cases (created or update) task will have a new WHO
    IF ls_ordim_o-who = ls_defdata-who.
      WAIT UP TO 1 SECONDS.
      CONTINUE.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

  IF ls_ordim_o-who IS INITIAL.
    LOG-POINT ID zcg_out_who_split
       FIELDS iv_pallet_id
              it_packed
              ls_defdata
              ls_ordim_o.
    RETURN.
  ENDIF.

  " lock tasks after update in standard update sessions is finished
  LOOP AT lt_ordim_prechech ASSIGNING FIELD-SYMBOL(<ls_task>).
    DO 30 TIMES.
      CALL FUNCTION '/SCWM/TO_READ_SINGLE'
        EXPORTING
          iv_lgnum     = <ls_task>-lgnum
          iv_tanum     = <ls_task>-tanum
          iv_flglock   = abap_true
        IMPORTING
          es_ordim_o   = ls_ordim_o_lock
        EXCEPTIONS
          wrong_input  = 1
          not_found    = 2
          foreign_lock = 3
          error        = 4
          OTHERS       = 5.

      IF sy-subrc = 0.
        DATA(lv_error) = abap_false.
        EXIT.
      ENDIF.

      lv_error = abap_true.
      WAIT UP TO 1 SECONDS.
    ENDDO.

    " if we cannot lock any of the tasks, stop processing into the queue
    IF lv_error = abap_true.
      CALL FUNCTION 'RESTART_OF_BACKGROUNDTASK'.

      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDLOOP.

  DATA(lt_change) = VALUE /scwm/tt_to_change_att_int(
      FOR <t> IN lt_ordim_prechech
      ( tanum = <t>-tanum
        tt_changed = VALUE #( ( fieldname = 'SHIPHUID'
                                value_c   = iv_pallet_id ) ) ) ).

  LOOP AT lt_change ASSIGNING FIELD-SYMBOL(<ls_upd>).
    <ls_upd>-seqno = sy-tabix.
  ENDLOOP.

  GET TIME STAMP FIELD DATA(lv_tstmp).

  LOG-POINT ID zcg_out_who_split
     FIELDS iv_pallet_id
            it_packed
            ls_defdata
            lt_change.

  CALL FUNCTION '/SCWM/TO_CHANGE_ATT_UPD'
    EXPORTING
      iv_tstmp  = lv_tstmp
      iv_lgnum  = ls_defdata-lgnum
      it_change = lt_change.

  COMMIT WORK.
ENDFUNCTION.
