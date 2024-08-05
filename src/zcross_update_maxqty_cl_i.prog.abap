*&---------------------------------------------------------------------*
*& Include          ZCROSS_UPDATE_MAXQTY_CL_I
*&---------------------------------------------------------------------*

CLASS lcl_update_maxqty IMPLEMENTATION.

  METHOD constructor.

    mv_lgnum = validate_lgnum( iv_lgnum ).

    TRY.
        IF mo_cuboid_algorithm  IS NOT BOUND.
          mo_cuboid_algorithm = NEW zcl_cuboid_algorithm( mv_lgnum ).
        ENDIF.
        IF mo_mon_prod_service IS NOT BOUND.
          mo_mon_prod_service = NEW /scwm/cl_mon_prod( ).
        ENDIF.

      CATCH zcx_core_exception ##NO_HANDLER.
        display_message( ).
    ENDTRY.

    mo_log = NEW /scwm/cl_log( ).

  ENDMETHOD.

  METHOD display_message.

    MESSAGE ID sy-msgid
                TYPE sy-msgty
                NUMBER sy-msgno
                WITH sy-msgv1
                     sy-msgv2
                     sy-msgv3
                     sy-msgv4.

  ENDMETHOD.

  METHOD check_bintype_exists.

    DATA(lt_lptyp_maxqty) = it_lptyp_maxqty.

    DELETE lt_lptyp_maxqty WHERE lptyp IN it_bintype_r.

    LOOP AT lt_lptyp_maxqty ASSIGNING FIELD-SYMBOL(<ls_lptyp_maxqty>).

      CHECK NOT line_exists( ct_lptyp_delete[ lgnum = <ls_lptyp_maxqty>-lgnum
                                                lgtyp = <ls_lptyp_maxqty>-lgtyp
                                                lptyp = <ls_lptyp_maxqty>-lptyp
                                                matid = <ls_lptyp_maxqty>-matid ] ).

      INSERT <ls_lptyp_maxqty> INTO TABLE ct_lptyp_delete.
    ENDLOOP.

  ENDMETHOD.

  METHOD main.

    DATA: lt_matnr_r           TYPE /scwm/tt_matnr_r,
          lv_matid16           TYPE /scwm/de_matid,
          lt_matidtab          TYPE /scwm/tt_matid_r,
          lt_t303s             TYPE /scwm/tt_t303s ##NEEDED,
          lt_bintype_all       TYPE /scwm/tt_t303s ##NEEDED,
          lt_zdb_maxqty        TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty,
          lt_ztlptyp_to_delete TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty,
          lt_matid_tab         TYPE /sapapo/matid_tab,
          lv_to_commit         TYPE abap_bool VALUE abap_false,
          ls_curr              TYPE REF TO zcl_crud_ztlptyp_maxqty=>ts_lptyp_maxqty.

    " organize material number selection table:
    IF so_matnr IS NOT INITIAL.
      lt_matnr_r = VALUE /sapapo/mat_nr_rtab( FOR <matnr> IN so_matnr
                                             ( sign = wmegc_sign_inclusive
                                               option = wmegc_option_eq
                                               low = <matnr>-low ) ).
    ELSE.
      APPEND VALUE #( sign = wmegc_sign_inclusive
                      option = wmegc_option_cp
                      low = wmegc_mask1 ) TO lt_matnr_r.
    ENDIF.

    DATA(lt_lgtyp_r) = VALUE /scwm/tt_lgtyp_r( FOR <ls_lgtyp> IN so_lgtyp
                                                  ( CORRESPONDING #( <ls_lgtyp> ) ) ).


**********************************************************************
    "This part was implemented in order to delete some entries
    "from ztlptyp_maxqty table for testing purposes
**********************************************************************
    IF p_del EQ abap_true.


      DATA: lt_matid_matnr  TYPE  /scwm/tt_matid_matnr.

      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_READ_RANGE'
            EXPORTING
              it_matnr_range = CONV rseloption( lt_matnr_r )
            IMPORTING
              et_matid       = lt_matid_matnr.

        CATCH /scwm/cx_md_interface
        /scwm/cx_md_internal_error.

      ENDTRY.

      DATA(lt_lptyp_r) = VALUE /scwm/tt_lptyp_r( FOR <ls_lptyp> IN so_lptyp
                                                  ( CORRESPONDING #( <ls_lptyp> ) ) ).

      zcl_crud_ztlptyp_maxqty=>select_multi_by_all_filters(
        EXPORTING
          iv_lgnum          = mv_lgnum                 " Warehouse Number/Warehouse Complex
          it_lgtyp_r        = lt_lgtyp_r
          it_lptyp_r        = lt_lptyp_r       " Range Table Type for Field Name LGTYP
          it_matid_r        = VALUE /scwm/tt_matid_r( FOR <matid_matnr> IN lt_matid_matnr
                                                      ( sign = wmegc_sign_inclusive
                                                        option = wmegc_option_eq
                                                        low = <matid_matnr>-matid ) )              " SELECT-OPTIONS Table
        IMPORTING
          et_ztlptyp_maxqty = lt_zdb_maxqty
      ).

      zcl_crud_ztlptyp_maxqty=>delete(
        EXPORTING
          iv_lgnum       = mv_lgnum                 " Warehouse Number/Warehouse Complex
          it_ztlptyp_del = lt_zdb_maxqty
        IMPORTING
          ev_dbcnt       = DATA(lv_lines_delete)
          ev_subrc       = DATA(lv_subrc_delete)
      ).

      IF lv_subrc_delete EQ 0.
        MESSAGE i012(zmc_crs) INTO DATA(lv_msg) WITH lv_lines_delete ##NEEDED.
        mo_log->add_message( ).
        COMMIT WORK AND WAIT.
      ENDIF.

      mo_log->save_applog(
        EXPORTING
          is_log       = VALUE #( extnumber = |{ TEXT-001 } { mv_lgnum }|
                                  object = zif_wme_c=>gs_msgobj-zewm
                                  subobject = zif_wme_c=>gs_msgsubobj-zcrs_upd_max_qty )
        IMPORTING
          ev_loghandle = DATA(lv_loghandle) ) ##NO_TEXT.

    ELSE.
******************************************************************************
******************************************************************************

      SELECT *
      FROM /scwm/t303s
      WHERE lgnum = @mv_lgnum
      AND   lgtyp IN @lt_lgtyp_r
      INTO TABLE @lt_bintype_all.

      CALL FUNCTION '/SAPAPO/DM_MATID_GET'
        EXPORTING
          i_matnr_rtab  = lt_matnr_r
        IMPORTING
          e_matid_tab   = lt_matid_tab
        EXCEPTIONS
          no_material   = 1
          not_qualified = 2
          OTHERS        = 3.
      IF sy-subrc <> 0.
*      message: "no materials found
        RETURN.
      ENDIF.

      LOOP AT lt_matid_tab ASSIGNING FIELD-SYMBOL(<ls_mat22>).
        "convert GUID22 to GUID16 for the select in MATLWHST table
        CALL FUNCTION '/SCMB/MDL_GUID_CONVERT'
          EXPORTING
            iv_guid22 = <ls_mat22>
          IMPORTING
            ev_guid16 = lv_matid16.

        lt_matidtab = VALUE #( BASE lt_matidtab ( sign = wmegc_sign_inclusive
                                                  option = wmegc_option_eq
                                                  low    = lv_matid16 ) ).
      ENDLOOP.

      NEW zcl_algorithm_facade( )->determine_bintyp_capacity(
        EXPORTING
          iv_lgnum        = mv_lgnum
          it_entitled     = VALUE /scwm/tt_entitled( FOR <entitled> IN so_ent
                                                        ( entitled = <entitled>-low ) )
          iv_query_db_qty = p_append "if the method is to select existing entries from the table and return them, this flag need to be true
          is_select_crit  = VALUE #( lgtyp_r = lt_lgtyp_r
                                     lptyp_r = VALUE #( FOR <ls_lptyp> IN so_lptyp
                                                          ( CORRESPONDING #( <ls_lptyp> ) ) )
                                     matid_r = lt_matidtab )
        IMPORTING
          et_lptyp_maxqty = DATA(lt_lptyp_maxqty_new) ).

******************************************************************************
******************************************************************************
      "Storage Bin Type is not used as a filter here, because we want to select all
      "entries from a given storage type, in order to compare the storage bin types with
      "the storage bin type entries from /scwm/t303s
      "to delete the entries from ztlptyp_maxqty, which don't exist in /scwm/t303s
      zcl_crud_ztlptyp_maxqty=>select_multi_by_all_filters(
        EXPORTING
          iv_lgnum          = mv_lgnum                 " Warehouse Number/Warehouse Complex
          it_lgtyp_r        = lt_lgtyp_r               " Range Table Type for Field Name LGTYP
          it_matid_r        = lt_matidtab              " SELECT-OPTIONS Table
        IMPORTING
          et_ztlptyp_maxqty = lt_zdb_maxqty
      ).

      LOOP AT lt_lptyp_maxqty_new ASSIGNING FIELD-SYMBOL(<ls_maxqty_new>).

        check_bintype_exists(
          EXPORTING
            it_bintype_r    = VALUE rseloption( FOR <ls_bintype_all> IN lt_bintype_all
                                          WHERE ( lgtyp EQ <ls_maxqty_new>-lgtyp )
                                           ( sign = wmegc_sign_inclusive
                                             option = wmegc_option_eq
                                             low = <ls_bintype_all>-lptyp ) )
            it_lptyp_maxqty = VALUE #( FOR <ls_ztlptyp_maxqty> IN lt_zdb_maxqty
                                                WHERE ( lgtyp EQ <ls_maxqty_new>-lgtyp )
                                                ( <ls_ztlptyp_maxqty> ) )
          CHANGING
            ct_lptyp_delete = lt_ztlptyp_to_delete
        ).

        IF <ls_maxqty_new>-max_qty < 1.
          CHECK NOT line_exists( lt_ztlptyp_to_delete[ lgnum = <ls_maxqty_new>-lgnum
                                                       lgtyp = <ls_maxqty_new>-lgtyp
                                                       lptyp = <ls_maxqty_new>-lptyp
                                                       matid = <ls_maxqty_new>-matid ] )
                AND line_exists( lt_zdb_maxqty[ lgnum = <ls_maxqty_new>-lgnum
                                                lgtyp = <ls_maxqty_new>-lgtyp
                                                lptyp = <ls_maxqty_new>-lptyp
                                                matid = <ls_maxqty_new>-matid ] ).

          INSERT <ls_maxqty_new> INTO TABLE lt_ztlptyp_to_delete.
          DELETE lt_lptyp_maxqty_new.
          CONTINUE.
        ENDIF.

        IF line_exists( lt_zdb_maxqty[ lgnum = mv_lgnum
                                       lgtyp = <ls_maxqty_new>-lgtyp
                                       lptyp = <ls_maxqty_new>-lptyp
                                       matid = <ls_maxqty_new>-matid ] ).
          ls_curr = REF #( lt_zdb_maxqty[ lgnum = mv_lgnum
                                          lgtyp = <ls_maxqty_new>-lgtyp
                                          lptyp = <ls_maxqty_new>-lptyp
                                          matid = <ls_maxqty_new>-matid ] ).
        ENDIF.

        IF ls_curr IS BOUND. "entry exists in the z-table
          IF ( p_full EQ abap_true AND "if we are in update/insert mode and the existing entry does not differ in quantity, skip this iteration
                  ls_curr->max_qty EQ <ls_maxqty_new>-max_qty ).
            " set max_qty to 0 to entries, which are going to be deleted
            <ls_maxqty_new>-max_qty = 0.
          ENDIF.
        ENDIF.

      ENDLOOP.

      DELETE lt_lptyp_maxqty_new WHERE max_qty < 1.

      IF lines( lt_lptyp_maxqty_new ) > 0.
        IF p_full EQ abap_true.

          zcl_crud_ztlptyp_maxqty=>modify(
            EXPORTING
              iv_lgnum       = mv_lgnum                " Warehouse Number/Warehouse Complex
              it_ztlptyp_new = lt_lptyp_maxqty_new
            IMPORTING
              ev_dbcnt       = DATA(lv_dbcnt)
              ev_subrc       = DATA(lv_rc)
          ).

          "Update And Insert Entries
          IF lv_rc EQ 0.
            lv_to_commit = abap_true.
            MESSAGE i011(zmc_crs) INTO lv_msg WITH lv_dbcnt ##NEEDED.
            mo_log->add_message( ).
          ENDIF.
        ELSEIF p_append EQ abap_true.

          zcl_crud_ztlptyp_maxqty=>insert(
            EXPORTING
              iv_lgnum       = mv_lgnum                  " Warehouse Number/Warehouse Complex
              it_ztlptyp_new = lt_lptyp_maxqty_new
            IMPORTING
              ev_dbcnt       = lv_dbcnt
              ev_subrc       = lv_rc
          ).

*      "Only Insert Entries
          IF lv_rc EQ 0 OR
            lv_rc EQ 4 AND lv_dbcnt > 0.
            lv_to_commit = abap_true.
            MESSAGE i011(zmc_crs) INTO lv_msg WITH lv_dbcnt.
            mo_log->add_message( ).
          ENDIF.
        ENDIF.

      ENDIF.
*
      IF lines( lt_ztlptyp_to_delete ) > 0.
*
        "Delete redundant entries
        zcl_crud_ztlptyp_maxqty=>delete(
          EXPORTING
            iv_lgnum       = mv_lgnum                 " Warehouse Number/Warehouse Complex
            it_ztlptyp_del = lt_ztlptyp_to_delete
          IMPORTING
            ev_dbcnt       = lv_dbcnt
            ev_subrc       = lv_rc
        ).
        IF lv_rc EQ 0.
          MESSAGE i012(zmc_crs) INTO lv_msg WITH lv_dbcnt.
          mo_log->add_message( ).
          lv_to_commit = abap_true.
        ENDIF.
      ENDIF.
*
      IF lv_to_commit <> abap_true.
        ROLLBACK WORK.

        MESSAGE i013(zmc_crs) INTO lv_msg.
        mo_log->add_message( ).
      ELSE.
        COMMIT WORK AND WAIT.
      ENDIF.

      mo_log->save_applog(
        EXPORTING
          is_log       = VALUE #( extnumber = |{ TEXT-001 } { mv_lgnum }|
                                  object = zif_wme_c=>gs_msgobj-zewm
                                  subobject = zif_wme_c=>gs_msgsubobj-zcrs_upd_max_qty )
        IMPORTING
          ev_loghandle = lv_loghandle ) ##NO_TEXT.                 " Log Handle

      TRY.
          mo_log->save_applog2db( iv_loghandle = lv_loghandle ).
        CATCH /scwm/cx_basics ##NO_HANDLER.
      ENDTRY.

    ENDIF.

    IF sy-batch <> abap_true AND " if the report is being executed in background,
       sy-tcode EQ 'ZUPDATE_LPTYP_MAXQTY'. "and it is called with a different transaction code don't display the log
      TRY.
          mo_log->display_log( iv_loghandle = lv_loghandle ).
        CATCH /scwm/cx_basics ##NO_HANDLER.
      ENDTRY.
    ENDIF.

  ENDMETHOD.

  METHOD dynp_values_read.

    CALL FUNCTION 'DYNP_VALUES_READ'
      EXPORTING
        dyname               = iv_dyname
        dynumb               = iv_dynnr
      TABLES
        dynpfields           = ct_dynpfield
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        invalid_parameter    = 7
        undefind_error       = 8
        double_conversion    = 9
        stepl_not_found      = 10
        OTHERS               = 11.
    IF sy-subrc <> 0.
      display_message( ).
    ENDIF.

  ENDMETHOD.

  METHOD det_shlp.

    DATA: ls_shlp TYPE shlp_descr.

    CALL FUNCTION 'F4IF_DETERMINE_SEARCHHELP'
      EXPORTING
        tabname           = iv_tabname
        fieldname         = iv_fieldname
      IMPORTING
        shlp              = ls_shlp
      EXCEPTIONS
        field_not_found   = 1
        no_help_for_field = 2
        inconsistent_help = 3
        OTHERS            = 4.
    IF sy-subrc <> 0.
      display_message( ).
    ENDIF.

    rs_shlp = ls_shlp.

  ENDMETHOD.

  METHOD strt_val_req.

    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        shlp          = is_shlp
      TABLES
        return_values = ct_values.

  ENDMETHOD.

  METHOD dynp_values_update.

    CALL FUNCTION 'DYNP_VALUES_UPDATE'
      EXPORTING
        dyname               = iv_dyname
        dynumb               = iv_dynnr
      TABLES
        dynpfields           = it_values
      EXCEPTIONS
        invalid_abapworkarea = 1
        invalid_dynprofield  = 2
        invalid_dynproname   = 3
        invalid_dynpronummer = 4
        invalid_request      = 5
        no_fielddescription  = 6
        undefind_error       = 7
        OTHERS               = 8.
    IF sy-subrc <> 0.
      display_message( ).
    ENDIF.

  ENDMETHOD.

  METHOD value_request_lgtyp.

    DATA: lt_dynpread   TYPE TABLE OF dynpread,
          lt_ddshretval TYPE TABLE OF ddshretval,
          ls_shlp       TYPE  shlp_descr.

    APPEND VALUE #( fieldname = c_p_lgnum_fn ) TO lt_dynpread.

    dynp_values_read(
      EXPORTING
        iv_dyname    = sy-repid
        iv_dynnr     = sy-dynnr
      CHANGING
        ct_dynpfield = lt_dynpread
    ).

    ls_shlp = det_shlp(
      EXPORTING
        iv_tabname   = c_lagp_mon_f4
        iv_fieldname = c_lgtyp_fn
    ).

    TRY.
        ls_shlp-interface[ valfield = c_lgnum_fn ]-value = lt_dynpread[ 1 ]-fieldvalue.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    strt_val_req( EXPORTING is_shlp   = ls_shlp
                  CHANGING  ct_values = lt_ddshretval ).

    IF NOT line_exists( lt_ddshretval[ fieldname = c_lgtyp_fn ] ).
      RETURN.
    ENDIF.

    APPEND VALUE #( fieldname = c_so_lgtyp_low_fn
                    fieldvalue = lt_ddshretval[ fieldname = c_lgtyp_fn ]-fieldval ) TO lt_dynpread.

    dynp_values_update(
      EXPORTING
        iv_dyname = sy-repid
        iv_dynnr  = sy-dynnr
        it_values = lt_dynpread
    ).

  ENDMETHOD.

  METHOD value_request_entitled.

    DATA: lt_dynpread   TYPE TABLE OF dynpread,
          lt_ddshretval TYPE TABLE OF ddshretval,
          ls_shlp       TYPE  shlp_descr.

    "READ LGNUM
    APPEND VALUE #( fieldname = c_p_lgnum_fn ) TO lt_dynpread.

    dynp_values_read(
      EXPORTING
        iv_dyname    = sy-repid
        iv_dynnr     = sy-dynnr
      CHANGING
        ct_dynpfield = lt_dynpread
    ).

    ls_shlp = det_shlp(
      EXPORTING
        iv_tabname   = c_sapapo_f4
        iv_fieldname = c_entitled_fn
    ).

    TRY.
        ls_shlp-interface[ valfield = c_lgnum_fn ]-value = lt_dynpread[ 1 ]-fieldvalue.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    strt_val_req(
      EXPORTING
        is_shlp   = ls_shlp
      CHANGING
        ct_values = lt_ddshretval
    ).

    IF NOT line_exists( lt_ddshretval[ fieldname = c_partner_fn ] ).
      RETURN.
    ENDIF.

    APPEND VALUE #( fieldname = c_so_entitled_low_fn
                    fieldvalue = lt_ddshretval[ fieldname = c_partner_fn ]-fieldval ) TO lt_dynpread.

    dynp_values_update(
      EXPORTING
        iv_dyname = sy-repid
        iv_dynnr  = sy-dynnr
        it_values = lt_dynpread
    ).

  ENDMETHOD.

  METHOD value_request_lptyp.

    DATA: lt_dynpread   TYPE TABLE OF dynpread,
          lt_ddshretval TYPE TABLE OF ddshretval,
          ls_shlp       TYPE  shlp_descr.

    "READ LGNUM
    APPEND VALUE #( fieldname = c_p_lgnum_fn ) TO lt_dynpread.

    dynp_values_read(
      EXPORTING
        iv_dyname    = sy-repid
        iv_dynnr     = sy-dynnr
      CHANGING
        ct_dynpfield = lt_dynpread
    ).

    ls_shlp = det_shlp(
      EXPORTING
        iv_tabname   = c_lagp_mon_f4
        iv_fieldname = c_lptyp_fn
    ).

    TRY.
        ls_shlp-interface[ valfield = c_lgnum_fn ]-value = lt_dynpread[ 1 ]-fieldvalue.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    strt_val_req(
      EXPORTING
        is_shlp   = ls_shlp
      CHANGING
        ct_values = lt_ddshretval
    ).

    IF NOT line_exists( lt_ddshretval[ fieldname = c_lptyp_fn ] ).
      RETURN.
    ENDIF.

    APPEND VALUE #( fieldname = c_so_lptyp_low_fn
                    fieldvalue = lt_ddshretval[ fieldname = c_lptyp_fn ]-fieldval ) TO lt_dynpread.

    dynp_values_update(
      EXPORTING
        iv_dyname = sy-repid
        iv_dynnr  = sy-dynnr
        it_values = lt_dynpread
    ).

  ENDMETHOD.

  METHOD validate_lgnum.

    IF iv_lgnum IS INITIAL.
      MESSAGE e112(/isdfps/wm01) DISPLAY LIKE wmegc_severity_err.
    ENDIF.

    CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
      EXPORTING
        iv_lgnum  = iv_lgnum
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE e008(/scmb/prr) DISPLAY LIKE wmegc_severity_err.
    ENDIF.

    rv_lgnum = iv_lgnum.

  ENDMETHOD.

ENDCLASS.
