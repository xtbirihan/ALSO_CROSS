CLASS zcl_cross_bin_type_det DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_slot_det_bt_bef .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CROSS_BIN_TYPE_DET IMPLEMENTATION.


  METHOD /scwm/if_ex_slot_det_bt_bef~det_bintype_before.
**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-042
*& Author        : Alper Ahmedov
*& Date          : 02.05.2023
*& Description   : GAP-042 CrossTopics_bin_capacity
**********************************************************************
* Determine the storage bin type comparing the planned weekly quantity
* and the quantities from the ZTLPTYP_MAXQTY table
* Change 11.12.2023 - if lgtyp is HU managed - determine the lptyp from
* the SCWM/TBINTYP_SQ table
**********************************************************************

    CONSTANTS: c_sign_i TYPE s_sign VALUE wmegc_sign_inclusive,
               c_opt_eq TYPE s_option VALUE wmegc_option_eq.

    DATA: lt_lptyp_maxqty    TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty,
          lt_lptyp_maxqty_db TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty,
          lv_maxqty_plan     TYPE /scwm/de_maxqty_plan,
          lv_lptyp           TYPE /scwm/de_bintype,
          ls_t331            TYPE  /scwm/t331,
          lt_tbintyp_sq      TYPE  /scwm/tt_tbintyp_sq,
          lv_test_lgtyp      TYPE  /scwm/lgtyp VALUE 'PKS1'.

    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_cross_bin_type_det.

    DATA(lv_lgnum) = is_mat_lgtyp-lgnum.

    IF lv_lgnum IS INITIAL OR
       is_mat_lgtyp-lgtyp IS INITIAL OR
       is_mat_lgtyp-matid IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/T331_READ_SINGLE'
      EXPORTING
        iv_lgnum  = lv_lgnum
        iv_lgtyp  = is_mat_lgtyp-lgtyp "lv_test_lgtyp
      IMPORTING
        es_t331   = ls_t331
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF ls_t331-huobl EQ abap_true.
      "HU managed logic

      TRY.
          CALL FUNCTION '/SCWM/TBINTYP_SQ_READ_SINGLE'
            EXPORTING
              iv_lgnum      = lv_lgnum
              iv_lgtyp      = is_mat_lgtyp-lgtyp "lv_test_lgtyp
              iv_hutyp      = VALUE #( it_packinfo[ 1 ]-hutyp OPTIONAL )
            IMPORTING
              et_tbintyp_sq = lt_tbintyp_sq.

        CATCH /scwm/cx_core_no_data.
          RETURN.
      ENDTRY.

      lv_lptyp = VALUE #( lt_tbintyp_sq[ seqno = 1 ]-lptyp OPTIONAL ).

    ELSE.

      lv_maxqty_plan = zcl_slot_maxqty=>get_maxqty( ).

      IF lv_maxqty_plan = 0.
        RETURN.
      ENDIF.

      NEW zcl_algorithm_facade( )->determine_bintyp_capacity(
        EXPORTING
          iv_lgnum        = lv_lgnum
          is_mat_lgtyp    = is_mat_lgtyp
          it_entitled     = VALUE #( ( entitled = is_mat_lgtyp-entitled ) )
          is_select_crit  = VALUE #( lgtyp_r = VALUE #( ( sign = c_sign_i option = c_opt_eq low = is_mat_lgtyp-lgtyp ) )
                                     matid_r = VALUE #( ( sign = c_sign_i option = c_opt_eq low = is_mat_lgtyp-matid ) ) )
        IMPORTING
          et_lptyp_maxqty = lt_lptyp_maxqty ).

      DATA(lv_min) = wmegc_infinity.

      LOOP AT lt_lptyp_maxqty ASSIGNING FIELD-SYMBOL(<ls_maxqty>).

        CHECK ( <ls_maxqty>-max_qty - lv_maxqty_plan ) <= lv_min AND
              <ls_maxqty>-max_qty >= lv_maxqty_plan.

        lv_min = <ls_maxqty>-max_qty - lv_maxqty_plan.
        lv_lptyp = <ls_maxqty>-lptyp.
      ENDLOOP.

      IF lv_lptyp IS INITIAL.

        zcl_crud_ztlptyp_maxqty=>select_multi_by_all_filters(
          EXPORTING
            iv_lgnum          = lv_lgnum                " Warehouse Number/Warehouse Complex
            it_lgtyp_r        = VALUE /scwm/tt_lgtyp_r( (
                                          sign   = wmegc_sign_inclusive
                                          option = wmegc_option_eq
                                          low    = is_mat_lgtyp-lgtyp ) )                 " Range Table Type for Field Name LGTYP
            it_matid_r        = VALUE /scwm/tt_matid_r( ( sign   = wmegc_sign_inclusive
                                                          option = wmegc_option_eq
                                                          low    = is_mat_lgtyp-matid ) )                " SELECT-OPTIONS Table
          IMPORTING
            et_ztlptyp_maxqty = lt_lptyp_maxqty_db
        ).
        IF lt_lptyp_maxqty_db IS NOT INITIAL.
          lt_lptyp_maxqty = lt_lptyp_maxqty_db.
        ENDIF.

        DATA(ls_lptyp_maxqty) = REDUCE zcl_crud_ztlptyp_maxqty=>ts_lptyp_maxqty(
                                    INIT max = VALUE zcl_crud_ztlptyp_maxqty=>ts_lptyp_maxqty( )
                                    FOR <ls_row> IN lt_lptyp_maxqty
                                    NEXT max = COND #( WHEN <ls_row>-max_qty > max-max_qty
                                                       THEN <ls_row>
                                                       ELSE max ) ).

        lv_lptyp = ls_lptyp_maxqty-lptyp.
      ENDIF.

    ENDIF.

    ev_bintyp = lv_lptyp.

    zcl_slot_lptyp=>set_lptyp( iv_lptyp = lv_lptyp ).

  ENDMETHOD.
ENDCLASS.
