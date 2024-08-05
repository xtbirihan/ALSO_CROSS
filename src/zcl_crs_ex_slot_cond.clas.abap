CLASS zcl_crs_ex_slot_cond DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /scwm/if_ex_slot_cond.
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS c_con_field_ccind TYPE /sapcnd/fieldname  VALUE 'ZZ_CCIND'.
    CONSTANTS c_con_field_mfrnr TYPE /sapcnd/fieldname  VALUE 'ZZ_MFRNR'.
    CONSTANTS c_con_field_prdha TYPE /sapcnd/fieldname  VALUE 'ZZ_PRDHA'.
    CONSTANTS c_con_field_map TYPE /sapcnd/fieldname  VALUE 'ZZ_MAP_CAT'.
    CONSTANTS c_con_field_rot TYPE /sapcnd/fieldname  VALUE 'ZZ_ROT_CAT'.
    CONSTANTS c_con_field_odd_size TYPE /sapcnd/fieldname VALUE 'ZZ_ODD_SIZE'.
    CONSTANTS c_monitor_program TYPE rsvar-report VALUE '/SCWM/SAPLWO_TO_MON'.

    METHODS calculate_rotation_category IMPORTING iv_warehousenumber          TYPE /scwm/lgnum
                                                  iv_material_number          TYPE /scwm/de_matnr
                                        RETURNING VALUE(rv_rotation_category) TYPE char0001.

    METHODS populate_values IMPORTING iv_warehousenumber   TYPE /scwm/lgnum
                                      iv_ccind             TYPE /scwm/pi_ccind
                                      iv_manufacturer      TYPE mfrnr
                                      iv_product_hierarchy TYPE /sapapo/prdha
                                      iv_security_relevant TYPE char0001
                                      iv_rotation_category TYPE char0001
                                      iv_odd_size          TYPE char0001
                            CHANGING  ct_field_values      TYPE /sapcnd/det_attrib_value_t.

    METHODS get_workdays_from_variant IMPORTING iv_warehousenumber TYPE /scwm/lgnum
                                                iv_program_variant TYPE variant
                                      RETURNING VALUE(rv_workdays) TYPE i.

ENDCLASS.



CLASS ZCL_CRS_EX_SLOT_COND IMPLEMENTATION.


  METHOD /scwm/if_ex_slot_cond~fill_fields.
**********************************************************************
*& Key           : AD-20230504
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Implementation for BAdI /SCWM/EX_SLOT_COND
*& Triggers when processing condition records for slotting
**********************************************************************
    DATA lv_matid22 TYPE /sapapo/matid.
    DATA lv_security_relevant TYPE c LENGTH 1.
    DATA lv_rotation_category TYPE c LENGTH 1.
    DATA lv_moving_average_price TYPE bapicurr_d.
    DATA lv_entitled_id TYPE /scwm/de_entitled_id.
    DATA lv_odd_size TYPE c LENGTH 1.
    DATA lt_locprodwh TYPE /sapapo/dm_matlwh_tab.

    " There are no standard function modules to read the manufacturer, the product hierarchy and the moving average price (MAP).
    " Therefore, the information must be read directly from the related database tables.

    BREAK-POINT ID zcg_ex_slot_cond.

    " Get thresholds for MAP
    zcl_param=>get_parameter(
                    EXPORTING iv_lgnum = is_mat_lgnum-lgnum
                              iv_process = zif_param_const=>c_zcross_0002
                              iv_parameter = zif_param_const=>c_map_threshold
                    IMPORTING ev_constant  = DATA(lv_map_threshold) ).

    " Get Manufacturer Number.
    SELECT SINGLE mfrnr
     INTO @DATA(lv_manufacturer)
     FROM mara
     WHERE matnr = @is_mat_global-matnr.

    " Convert GUID16 to GUID22
    " (The GUID22 is needed for accessing table /sapapo/matkey)
    zcl_core_util_convert=>convert_guid( EXPORTING iv_guid16 =  is_mat_global-matid
                                         IMPORTING ev_guid22 =  lv_matid22 ).
    CALL FUNCTION '/SCMB/MDL_GUID_CONVERT'
      EXPORTING
        iv_guid16 = is_mat_global-matid
      IMPORTING
        ev_guid22 = lv_matid22.

    " Get product hierarchy
    SELECT SINGLE prdha
     INTO @DATA(lv_product_hierarchy)
     FROM /sapapo/matkey
    WHERE matid = @lv_matid22.

    " Read MAP (Moving average price)
    SELECT SINGLE *
      INTO @DATA(ls_valute)
      FROM /scwm/t_valuate
     WHERE lgnum = @is_mat_lgnum-lgnum
       AND matid = @is_mat_lgnum-matid
       AND entitled = @is_mat_lgnum-entitled.

    " Convertion for currencies w/o decimal digits
    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = ls_valute-waers
        amount_internal = ls_valute-verpr
      IMPORTING
        amount_external = lv_moving_average_price.

    " Is the MAP > Threshold?
    IF CONV verpr( lv_moving_average_price ) > CONV verpr( lv_map_threshold ).
      lv_security_relevant = 'Y'. " Yes => Material is security relevant
    ELSE.
      lv_security_relevant = 'N'. " No => Material is not security relevant
    ENDIF.

    " Get rotation category
    lv_rotation_category = calculate_rotation_category( iv_warehousenumber = is_mat_lgnum-lgnum
                                                         iv_material_number = is_mat_global-matnr ).

    " Get Odd Size flag
    zcl_core_util_convert=>conv_entitled_to_guid( EXPORTING
                                                     iv_entitled = is_mat_lgnum-entitled
                                                  IMPORTING
                                                     ev_entitled_guid16 = lv_entitled_id ).

    CALL FUNCTION '/SAPAPO/MATLWH_READ_MULTI_2'
      EXPORTING
        it_key              = VALUE /sapapo/dm_matlwh_id_tab( ( matid = lv_matid22 entitled_id = lv_entitled_id ) )
      IMPORTING
        et_locprodwh        = lt_locprodwh
      EXCEPTIONS
        data_not_found      = 1
        interface_incorrect = 2
        error_message       = 3
        OTHERS              = 99.

    IF sy-subrc = 0.
      lv_odd_size = VALUE #( lt_locprodwh[ 1 ]-zz1_odd_whd OPTIONAL ).
    ENDIF.

    " Populate values
    populate_values(
            EXPORTING iv_warehousenumber = is_mat_lgnum-lgnum
                      iv_ccind = is_mat_lgnum-ccind
                      iv_manufacturer = lv_manufacturer
                      iv_product_hierarchy = lv_product_hierarchy
                      iv_security_relevant = lv_security_relevant
                      iv_rotation_category = lv_rotation_category
                      iv_odd_size = lv_odd_size
             CHANGING ct_field_values = ct_items ).

  ENDMETHOD.


  METHOD calculate_rotation_category.
**********************************************************************
*& Key           : AD-20230504
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Calculates the rotation category based on the average picks per day in a given time frame.
*&
**********************************************************************
    DATA lt_monitor_output TYPE /scwm/tt_to_det_mon_out.
    DATA lt_parameters TYPE TABLE OF rsds_range.

    " Get program variant for warehouse monitor
    zcl_param=>get_parameter(
                EXPORTING iv_lgnum = iv_warehousenumber
                          iv_process = zif_param_const=>c_zcross_0002
                          iv_parameter = zif_param_const=>c_program_varaint
                IMPORTING ev_constant = DATA(lv_program_variant) ).

    " Get thresholds for rotation category
    zcl_param=>get_parameter(
                    EXPORTING iv_lgnum = iv_warehousenumber
                              iv_process = zif_param_const=>c_zcross_0002
                              iv_parameter = zif_param_const=>c_rot_threshold
                    IMPORTING ev_constant = DATA(lv_rot_threshold) ).

    " Get number of work days
    DATA(lv_workdays) = get_workdays_from_variant( iv_warehousenumber = iv_warehousenumber iv_program_variant = CONV variant( lv_program_variant ) ).

    IF lv_workdays = 0.
      RETURN.
    ENDIF.

    " Build parameter table to add the material number to the warehouse monitor variant
    lt_parameters = VALUE #( ( tablename = '/SCWM/S_WO_TO_MON_IN'
                               frange_t = VALUE rsds_frange_t( ( fieldname = 'MATNR'
                                                                 selopt_t = VALUE rsds_selopt_t( ( sign = 'I'
                                                                                                   option = 'EQ'
                                                                                                   low = iv_material_number
                                                                                                 )
                                                                                               )
                                                               )
                                                             )
                             )
                           ).

    " Get number of picking warehouse tasks
    TRY.
        CALL FUNCTION '/SCWM/TO_MON'
          EXPORTING
            iv_lgnum     = iv_warehousenumber
            iv_variant   = CONV variant( lv_program_variant )
            iv_mode      = /scwm/cl_wme_monitor_srvc=>c_mode_exec
          IMPORTING
            et_data      = lt_monitor_output
          CHANGING
            ct_tab_range = lt_parameters.

        " Delete all warehouse task that are not outbound deliveries
        DELETE lt_monitor_output WHERE rdoccat <> /scdl/if_dl_doc_c=>sc_doccat_out_prd.
      CATCH /scwm/cx_mon_noexec /scwm/cx_core INTO DATA(lx_exception).
    ENDTRY.

    DATA(lv_number_of_picks) = lines( lt_monitor_output ).

    IF lv_number_of_picks = 0.
      RETURN.
    ENDIF.

    " Calculate average picks per day
    DATA(lv_picks_per_day) = lv_number_of_picks / lv_workdays.

    " Is the average pick per day > threshold?
    IF lv_picks_per_day > CONV int1( lv_rot_threshold ).
      rv_rotation_category = '1'. " Yes => Rotation category is "Fast moving"
    ELSE.
      rv_rotation_category = '2'. " No => Rotation category is "Slow moving"
    ENDIF.

  ENDMETHOD.


  METHOD get_workdays_from_variant.
**********************************************************************
*& Key           : AD-20230505
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Calculates the work days by reading From and to date form a warehouse monitor variant
*&
**********************************************************************
    DATA lv_date_to_internal_format TYPE sy-datum.
    DATA lv_date_from_internal_format TYPE sy-datum.

    DATA lt_variant_values TYPE TABLE OF rsparams.
    DATA lt_holidays TYPE TABLE OF iscal_day.

    rv_workdays = 0.

    SELECT SINGLE factory_calendar
      INTO @DATA(lv_factory_calendar)
      FROM /scwm/t340d
     WHERE lgnum EQ @iv_warehousenumber.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = c_monitor_program
        variant              = iv_program_variant
      TABLES
        valutab              = lt_variant_values
      EXCEPTIONS
        variant_non_existent = 1
        variant_obsolete     = 2
        OTHERS               = 3.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lv_date_from) = VALUE #( lt_variant_values[ selname = 'P_CODFR' ]-low OPTIONAL ).
    DATA(lv_date_to) = VALUE #( lt_variant_values[ selname = 'P_CODTO' ]-low OPTIONAL ).

    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lv_date_to
      IMPORTING
        date_internal            = lv_date_to_internal_format
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
      EXPORTING
        date_external            = lv_date_from
      IMPORTING
        date_internal            = lv_date_from_internal_format
      EXCEPTIONS
        date_external_is_invalid = 1
        OTHERS                   = 2.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rv_workdays = lv_date_to_internal_format - lv_date_from_internal_format + 1.

    CALL FUNCTION 'HOLIDAY_GET'
      EXPORTING
        factory_calendar           = lv_factory_calendar
        date_from                  = lv_date_from_internal_format
        date_to                    = lv_date_to_internal_format
      TABLES
        holidays                   = lt_holidays
      EXCEPTIONS
        factory_calendar_not_found = 1
        holiday_calendar_not_found = 2
        date_has_invalid_format    = 3
        date_inconsistency         = 4
        OTHERS                     = 5.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rv_workdays = rv_workdays - lines( lt_holidays ).

  ENDMETHOD.


  METHOD populate_values.
**********************************************************************
*& Key           : AD-20230505
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Populates values to the internal table or creates new entries
*&
**********************************************************************

    " ZZ_CCIND (ABC classification)
    IF line_exists( ct_field_values[ fieldname = c_con_field_ccind ] ).
      ct_field_values[ fieldname = c_con_field_ccind ]-value = iv_ccind.
    ELSE.
      ct_field_values = VALUE #( BASE ct_field_values ( fieldname = c_con_field_ccind value = iv_ccind ) ).
    ENDIF.

    " ZZ_MFRNR (Manufacturer)
    IF line_exists( ct_field_values[ fieldname = c_con_field_mfrnr ] ).
      ct_field_values[ fieldname = c_con_field_mfrnr ]-value = iv_manufacturer.
    ELSE.
      ct_field_values = VALUE #( BASE ct_field_values ( fieldname = c_con_field_mfrnr value = iv_manufacturer ) ).
    ENDIF.

    " ZZ_PRDHA (Product hierarchy)
    IF line_exists( ct_field_values[ fieldname = c_con_field_prdha ] ).
      ct_field_values[ fieldname = c_con_field_prdha ]-value = iv_product_hierarchy.
    ELSE.
      ct_field_values = VALUE #( BASE ct_field_values ( fieldname = c_con_field_prdha value = iv_product_hierarchy ) ).
    ENDIF.

    " ZZ_MAP (Security relevance)
    IF line_exists( ct_field_values[ fieldname = c_con_field_map ] ).
      ct_field_values[ fieldname = c_con_field_map ]-value = iv_security_relevant.
    ELSE.
      ct_field_values = VALUE #( BASE ct_field_values ( fieldname = c_con_field_map value = iv_security_relevant ) ).
    ENDIF.

    " ZZ_ROT_CAT (Rotation category)
    IF line_exists( ct_field_values[ fieldname = c_con_field_rot ] ).
      ct_field_values[ fieldname = c_con_field_rot ]-value = iv_security_relevant.
    ELSE.
      ct_field_values = VALUE #( BASE ct_field_values ( fieldname = c_con_field_rot value = iv_security_relevant ) ).
    ENDIF.

    " ZZ_ODD_SIZE (Odd Size Goods)
    IF line_exists( ct_field_values[ fieldname = c_con_field_odd_size ] ).
      ct_field_values[ fieldname = c_con_field_odd_size ]-value = iv_odd_size.
    ELSE.
      ct_field_values = VALUE #( BASE ct_field_values ( fieldname = c_con_field_odd_size value = iv_odd_size ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
