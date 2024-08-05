CLASS zcl_cross_dimension_ind DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_slot_det_diminds .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA: st_volume_ind_int TYPE STANDARD TABLE OF /scwm/tdimindd.
    CONSTANTS c_volume_me TYPE meins VALUE 'CDM' ##NO_TEXT.

    METHODS slot_weekly_volume
      IMPORTING
        !is_mat_global    TYPE /scwm/s_material_global
        !is_mat_lgnum     TYPE /scwm/s_material_lgnum
      CHANGING
        !cs_dimindicators TYPE /scwm/s_dimindicators
        !ct_prot          TYPE /scwm/tt_bal_s_msg .
    CLASS-METHODS add_sy_message_to_prot
      CHANGING
        !ct_prot TYPE /scwm/tt_bal_s_msg .
    CLASS-METHODS
      get_volum_ind_interval_in_cd3
        IMPORTING
          iv_lgnum   TYPE /scwm/lgnum
        EXPORTING
          ev_success TYPE abap_bool
        CHANGING
          ct_prot    TYPE /scwm/tt_bal_s_msg.
ENDCLASS.



CLASS ZCL_CROSS_DIMENSION_IND IMPLEMENTATION.


  METHOD /scwm/if_ex_slot_det_diminds~determine_diminds.
**********************************************************************
*& Key           : LH-230314
*& Request No.   : GAP 43 Slot weekly volume
**********************************************************************
*& Determine the volume indicator. First calculate the weekly volume
*&  then find the fitting volume from the Intervals to Dimension Indicators
*&  customizing table. All values are converted to CD3.
**********************************************************************

    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_ex_slot_det_diminds.
    CLEAR: et_prot.
    es_dimindicators = is_dimindicators.
    "*****************************************************
    " Slot weekly volume: set volume indicator
    "*****************************************************

    "Check the  switch.
    IF zcl_switch=>get_switch_state( iv_lgnum = /scwm/cl_tm=>sv_lgnum
                                     iv_devid = zif_switch_const=>c_zcross_001 ) EQ abap_true.

      slot_weekly_volume(
        EXPORTING
          is_mat_global    = is_mat_global
          is_mat_lgnum     = is_mat_lgnum
        CHANGING
          cs_dimindicators = es_dimindicators
          ct_prot          = et_prot
      ).
    ENDIF.


  ENDMETHOD.


  METHOD add_sy_message_to_prot.
    APPEND VALUE bal_s_msg( msgty = sy-msgty
                            msgid = sy-msgid
                            msgno = sy-msgno
                            msgv1 = sy-msgv1
                            msgv2 = sy-msgv2
                            msgv3 = sy-msgv3
                            msgv4 = sy-msgv4 ) TO ct_prot.
  ENDMETHOD.


  METHOD get_volum_ind_interval_in_cd3.
    DATA: lv_last_msg TYPE string ##needed.

    CLEAR ev_success.
    DATA(lo_concore) = NEW /scwm/cl_concepting_core( ).

    SELECT FROM /scwm/tdimindd
      FIELDS *
      WHERE lgnum EQ @iv_lgnum
        AND dimindtp EQ @congc_volume
      INTO TABLE @st_volume_ind_int.

    LOOP AT st_volume_ind_int REFERENCE INTO DATA(lr_vol).

      "Convert the slotting representation into normal
      CALL METHOD /scwm/cl_con_db_accesses=>t006_read_single
        EXPORTING
          iv_msehi  = lr_vol->maxdimuom
        IMPORTING
          es_t006   = DATA(ls_t006)
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc NE 0.
        add_sy_message_to_prot( CHANGING ct_prot = ct_prot ).
        RETURN.
      ENDIF.

      CALL METHOD /scwm/cl_con_db_accesses=>t006d_read_single
        EXPORTING
          iv_dimid  = ls_t006-dimid
        IMPORTING
          es_t006d  = DATA(ls_t006d)
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc NE 0.
        add_sy_message_to_prot( CHANGING ct_prot = ct_prot ).
        RETURN.
      ENDIF.

      IF lr_vol->maxdimuom NE ls_t006d-mssie.
        TRY.
            CALL METHOD lo_concore->convert_dimensions
              EXPORTING
                iv_unit1    = ls_t006d-mssie
                iv_unit2    = lr_vol->maxdimuom
                iv_quan1_31 = lr_vol->maxdim
                iv_decimals = '14'
              IMPORTING
                ev_quan2_31 = DATA(lv_maxdim).

          CATCH /scwm/cx_con.
            MESSAGE i003(zmc_crs) WITH ls_t006d-mssie lr_vol->maxdimuom INTO lv_last_msg.
            add_sy_message_to_prot( CHANGING ct_prot = ct_prot ).
            RETURN.
        ENDTRY.
      ENDIF.

      IF lr_vol->maxdimuom EQ c_volume_me.
        lr_vol->maxdim = lv_maxdim.
      ELSE.
        CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
          EXPORTING
            input                = lv_maxdim                " Input Value
            unit_in              = lr_vol->maxdimuom            " Unit of input value
            unit_out             = c_volume_me           " Unit of output value
          IMPORTING
            output               = lr_vol->maxdim                 " Output value
          EXCEPTIONS
            conversion_not_found = 1                " Conversion factor could not be determined
            division_by_zero     = 2                " Division by zero caught
            input_invalid        = 3                " Input value is not a number
            output_invalid       = 4                " OUTPUT parameter is not a number
            overflow             = 5                " Field overflow
            type_invalid         = 6                " an output parameter is not a number
            units_missing        = 7                " no units specified
            unit_in_not_found    = 8                " UNIT_IN is not maintained
            unit_out_not_found   = 9                " UNIT_OUT is not maintained
            OTHERS               = 10.
        IF sy-subrc <> 0.
          MESSAGE i003(zmc_crs) WITH ls_t006d-mssie lr_vol->maxdimuom INTO lv_last_msg ##needed.
          add_sy_message_to_prot( CHANGING ct_prot = ct_prot ).
          RETURN.
        ENDIF.
      ENDIF.

    ENDLOOP.

    SORT st_volume_ind_int BY maxdim.
    ev_success = abap_true.
  ENDMETHOD.


  METHOD slot_weekly_volume.
**********************************************************************
*& Key           : LH-230314
*& Request No.   : GAP 43 Slot weekly volume
**********************************************************************
*& Determine the volume indicator. First calculate the weekly volume
*&  then find the fitting volume from the Intervals to Dimension Inidcators
*&  customizing table.
**********************************************************************
    DATA:
      lv_last_msg      TYPE string ##needed,
      lv_weekly_quant  TYPE /scwm/de_quantity,
      lv_weekly_volume TYPE /scwm/de_quantity.


    IF is_mat_lgnum-demqty IS INITIAL.
      "No master data is maintained for &1. Volume indicator is cleared
      MESSAGE i001(zmc_crs) WITH is_mat_global-matnr INTO lv_last_msg.
      add_sy_message_to_prot( CHANGING ct_prot = ct_prot ).
      CLEAR cs_dimindicators-volind.
      RETURN.
    ENDIF.

    "Get multiplicator factor
    SELECT FROM /scwm/tdemfac
      FIELDS tpfactor
      WHERE lgnum EQ @is_mat_lgnum-lgnum
        AND tpfactor IS NOT INITIAL
       INTO TABLE @DATA(lt_tpfactor)
      UP TO 1 ROWS.
    IF lt_tpfactor IS INITIAL OR lt_tpfactor[ 1 ]-tpfactor IS INITIAL.
      "No multiplication factor is maintained for &1. Volume indicator is cleared
      MESSAGE i002(zmc_crs) WITH is_mat_lgnum-lgnum INTO lv_last_msg.
      add_sy_message_to_prot( CHANGING ct_prot = ct_prot ).
      CLEAR cs_dimindicators-volind.
      RETURN.
    ENDIF.

    lv_weekly_quant = lt_tpfactor[ 1 ]-tpfactor * is_mat_lgnum-demqty.

    "-->added by tbirihan 25.10.2023
    SELECT SINGLE * FROM marm INTO @DATA(ls_marm)
                    WHERE matnr = @is_mat_global-matnr
                      AND meinh = @is_mat_lgnum-meins.
    "--<<added by tbirihan 25.09.2023
    IF ls_marm-voleh NE c_volume_me. "-->added by tbirihan 25.10.2023
      "Get volume
      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
            EXPORTING
              iv_matid     = is_mat_global-matid
              iv_quan      = lv_weekly_quant
              iv_unit_from = is_mat_lgnum-meins
              iv_unit_to   = c_volume_me
              iv_batchid   = CONV /scwm/de_batchid( 0 )
            IMPORTING
              ev_quan      = lv_weekly_volume.

        CATCH /scwm/cx_md.
          MESSAGE w003(zmc_crs) WITH c_volume_me is_mat_global-matnr INTO lv_last_msg.
          add_sy_message_to_prot( CHANGING ct_prot = ct_prot ).
          CLEAR cs_dimindicators-volind.
          RETURN.
      ENDTRY.
      "-->added by tbirihan 25.10.2023
    ELSE.
      IF ls_marm-volum IS NOT INITIAL.
        lv_weekly_volume = lv_weekly_quant * ls_marm-volum.
      ENDIF.
      "--<<added by tbirihan 25.10.2023
    ENDIF.

    "Get the indicator: Select the intervals which can satisfy the volume,
    "then take the one, which has the lowest capacity
    IF st_volume_ind_int IS INITIAL OR st_volume_ind_int[ 1 ]-lgnum NE is_mat_lgnum-lgnum.
      get_volum_ind_interval_in_cd3(
        EXPORTING
          iv_lgnum   = is_mat_lgnum-lgnum
        IMPORTING
          ev_success = DATA(lv_success)
        CHANGING
          ct_prot    = ct_prot
      ).
      IF lv_success EQ abap_false.
        CLEAR cs_dimindicators-volind.
        RETURN.
      ENDIF.
    ENDIF.

    LOOP AT st_volume_ind_int INTO DATA(ls_volume_dimid)
         WHERE maxdim GT lv_weekly_volume.
      cs_dimindicators-volind = ls_volume_dimid-dimind.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.
      "No fitting volume indicator was found for &1
      MESSAGE i004(zmc_crs) WITH is_mat_global-matnr INTO lv_last_msg.
      add_sy_message_to_prot( CHANGING ct_prot = ct_prot ).
      CLEAR cs_dimindicators-volind.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
