CLASS zcl_mdm_model DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF mty_werks_logsys,
        werks  TYPE werks_d,
        logsys TYPE logsys,
      END OF mty_werks_logsys.

    DATA mv_is_mc_additional_maintained TYPE abap_boolean.
    DATA ms_t300_md                     TYPE /scwm/s_t300_md.
    DATA ms_t300t                       TYPE /scwm/s_t300t.
    DATA mo_scu                         TYPE REF TO /scmb/if_scu.
    DATA ms_partner                     TYPE /scmb/mdl_partner.
    DATA ms_t340d                       TYPE /scwm/s_t340d.
    DATA mo_packspeck_model             TYPE REF TO /scwm/cl_packspec_model.
    DATA  BEGIN OF ms_top_block.
    DATA:   sub_section_1 TYPE zstr_product_characteristics.
    DATA:   sub_section_2 TYPE zstr_material_hazardous_info.
    DATA:   sub_section_3 TYPE zstr_ident_numbers.
    DATA: END OF ms_top_block.
    DATA  BEGIN OF ms_midlle_block.
    DATA:   t_unit_of_measures TYPE TABLE OF zstr_unit_of_measures.
    DATA:   s_packspec_header  TYPE zstr_packspec_header.
    DATA: END OF ms_midlle_block.
    DATA  BEGIN OF ms_lower_block.
    DATA:   s_warehouse_data       TYPE zstr_warehouse_data.
    DATA:   s_storage_type_data    TYPE zstr_storage_type_data.
    DATA:   t_storage_bin_type_sel TYPE TABLE OF zstr_storage_type_bin_type_sel.
    DATA:   s_material_texts       TYPE zstr_material_texts.
    DATA: END OF ms_lower_block.
    DATA ms_master_carton_aditional TYPE zstr_additional_data.
    DATA  BEGIN OF master_data.
    DATA:   mara                     TYPE mara.
    DATA:   marm                     TYPE marm_tab.
    DATA:   marc                     TYPE marc.
    DATA:   apo_material             TYPE /sapapo/matlwh_out.
    DATA:   storage_type             TYPE /sapapo/dm_matlwhst_tab.
    DATA:   warehouse_data           TYPE /sapapo/matio_wh.
    DATA:   storage_type_description TYPE TABLE OF /scwm/t301t.
    DATA:   mat_global               TYPE /scwm/s_material_global.
    DATA:   mat_hazard               TYPE /scwm/s_material_hazard.
    DATA:   numbers                  TYPE TABLE OF ztcross_numbers.
    DATA:   patterns                 TYPE TABLE OF ztcross_patterns.
    DATA:   flowrack                 TYPE TABLE OF ztlgtyp_algo.
    DATA:   imei_no_template         TYPE ztcross_patterns-template.
    DATA:   mac_address_template     TYPE ztcross_patterns-template.
    DATA:   identab01                TYPE ztt_patterns.
    DATA:   identab02                TYPE ztt_patterns.
    DATA:   identab03                TYPE ztt_patterns.
    DATA:   identab04                TYPE ztt_patterns.
    DATA:   identab05                TYPE ztt_patterns.
    DATA:   serial_check             TYPE ztcross_serialch.
    DATA:   mt_mat_texts             TYPE STANDARD TABLE OF zcross_texts.
    DATA: END OF master_data.

    METHODS update_material_warehouse_data
      IMPORTING is_mat_whse_data TYPE /sapapo/matio_wh.

    METHODS set_model_data
      CHANGING cs_screen TYPE any.

    METHODS get_model_data
      CHANGING cs_screen TYPE any.

    METHODS read_scanned_value
      IMPORTING VALUE(iv_scanned_value) TYPE zde_scanned_value.

    METHODS clear_sub_section_1
      CHANGING cs_sub_section_1 TYPE zstr_product_characteristics.

    METHODS clear_sub_section_2
      CHANGING cs_sub_section_2 TYPE zstr_material_hazardous_info.

    METHODS clear_sub_section_3
      CHANGING cs_sub_section_3 TYPE zstr_ident_numbers.

    METHODS clear_middle_block
      CHANGING cs_middle_block TYPE ztt_unit_of_measueres.

    METHODS clear_warehouse_data
      CHANGING cs_warehouse_data TYPE zstr_warehouse_data.

    METHODS clear_storage_type_data
      CHANGING cs_storage_type_data TYPE zstr_storage_type_data.

    METHODS clear_storage_bin_type_sel
      CHANGING ct_storage_bin_type_sel TYPE ztt_storage_type_bin_type_sel.

    METHODS clear_master_data.

    METHODS read_location
      RETURNING VALUE(rs_t300_md) TYPE /scwm/s_t300_md.

    METHODS read_warehouse_number_desc
      RETURNING VALUE(rs_t300t) TYPE /scwm/s_t300t.

    "
    METHODS get_supply_chain_unit
      IMPORTING iv_sc_unit    TYPE guid_16
      RETURNING VALUE(ro_scu) TYPE REF TO /scmb/if_scu.

    METHODS partner_read
      IMPORTING iv_partner            TYPE bu_partner
      RETURNING VALUE(rs_mdl_partner) TYPE /scmb/mdl_partner.

    METHODS get_mat_details_from_matid
      IMPORTING iv_matid                   TYPE /scwm/de_matid
      RETURNING VALUE(rs_material_details) TYPE /scmb/mdl_product.

    METHODS check_concstat_entry
      IMPORTING iv_concstat TYPE /scwm/de_concstat.

    METHODS check_dispatchable_entry
      IMPORTING iv_disp_whd    TYPE zz1_disp
      CHANGING  cv_description TYPE text50.

    METHODS check_put_stra
      IMPORTING iv_put_stra    TYPE /scwm/de_put_stra
      CHANGING  cv_description TYPE /scwm/de_desc40.

    METHODS check_volume_indicator
      IMPORTING iv_volind      TYPE /scwm/de_volind
      CHANGING  cv_description TYPE /scwm/de_desc40.

    METHODS check_cycle_counting_ind
      IMPORTING iv_ccind TYPE /sapapo/matio_wh-ccind.

    METHODS check_storage_type
      IMPORTING iv_lgtyp TYPE /scwm/lgtyp.

    METHODS check_sectind
      IMPORTING iv_sectind TYPE /sapapo/matio_whst-sectind.

    METHODS check_bintype
      IMPORTING iv_bintype TYPE /sapapo/matio_whst-bintype.

    METHODS check_material_texts
      IMPORTING iv_text        TYPE zde_picking_text
                iv_text_id     TYPE zde_text_type
      CHANGING  cv_description TYPE zde_text.

    METHODS putaway_quan_class
      IMPORTING iv_quanclaput TYPE /sapapo/matio_whst-quanclaput.

    METHODS get_domain_values
      IMPORTING iv_domname            TYPE dd07l-domname
                iv_langu              TYPE dd07t-ddlanguage
      RETURNING VALUE(rt_fixed_valus) TYPE dd07v_tab.

    METHODS read_storage_type_text
      IMPORTING iv_storage_type       TYPE /scwm/lgtyp
      RETURNING VALUE(rv_description) TYPE /scwm/de_desc40.

    METHODS det_putaway_qty_class
      IMPORTING iv_storage_type      TYPE /scwm/lgtyp
      RETURNING VALUE(rv_quanclaput) TYPE /scwm/de_quancla_put.

    METHODS read_storage_section_ind_text
      IMPORTING iv_storage_section_ind TYPE /scwm/lvs_lgbkz
      RETURNING VALUE(rv_description)  TYPE /scwm/de_desc40.

    METHODS read_identtab_text
      IMPORTING iv_selnum             TYPE zde_selnum
      RETURNING VALUE(rv_description) TYPE zde_number_description.

    METHODS get_packspec_header_detail
      IMPORTING iv_matnr                  TYPE /scmb/mdl_matnr
      RETURNING VALUE(rs_packspec_header) TYPE /scwm/s_ps_header_int.

    METHODS get_packspec_level_detail
      IMPORTING iv_matnr                 TYPE /scmb/mdl_matnr
      RETURNING VALUE(rs_packspec_level) TYPE /scwm/s_ps_level_int.

    METHODS fill_storage_type_data_tab
      IMPORTING is_matlwhst          TYPE /sapapo/dm_matlwhst
      CHANGING  cs_storage_type_data TYPE zstr_storage_type_data.

    METHODS propose_replenishment_data
      IMPORTING is_matlwhst          TYPE /sapapo/dm_matlwhst
                iv_pieces            TYPE umrez
      CHANGING  cs_warehouse_data    TYPE zstr_warehouse_data
                cs_storage_type_data TYPE zstr_storage_type_data.

    METHODS fill_storage_bin_type_sel
      IMPORTING iv_storage_type     TYPE /scwm/lgtyp
      CHANGING  ct_storage_bin_type TYPE ztt_storage_type_bin_type_sel.

    METHODS read_storage_bin_type_text
      IMPORTING iv_storage_bin_type   TYPE /scwm/lvs_lptyp
      RETURNING VALUE(rv_description) TYPE /scwm/de_desc40.

    METHODS read_storage_type_data
      IMPORTING iv_matnr           TYPE /sapapo/matio-matnr
                iv_scuguid         TYPE /scmb/mdl_scuguid
                iv_entitled_id     TYPE /scwm/de_entitled_id
      RETURNING VALUE(rt_matlwhst) TYPE /sapapo/dm_matlwhst_tab.

    METHODS read_material_warehouse_data
      IMPORTING iv_matnr         TYPE /sapapo/matio-matnr
                iv_scuguid       TYPE /scmb/mdl_scuguid
                iv_entitled_id   TYPE /scwm/de_entitled_id
      RETURNING VALUE(rs_matlwh) TYPE /sapapo/matio_wh.

    METHODS fill_storage_type_text
      IMPORTING it_storage_type             TYPE /sapapo/dm_matlwhst_tab
      CHANGING  ct_storage_type_description TYPE /scwm/tt_t301t.

    METHODS fill_warehouse_data_tab
      IMPORTING is_matlwh         TYPE /sapapo/matio_wh
      CHANGING  cs_warehouse_data TYPE zstr_warehouse_data.

    METHODS fill_material_texts_tab
      IMPORTING is_matlwh         TYPE /sapapo/matio_wh
      CHANGING  cs_material_texts TYPE zstr_material_texts.

    METHODS read_store_uom
      IMPORTING is_storage_type_data TYPE zstr_storage_type_data
      RETURNING VALUE(rv_store_uom)  TYPE zde_store_uom.

    METHODS packspec_read_pmat
      IMPORTING iv_guid_ps    TYPE /scwm/de_guid_ps
                iv_aennr      TYPE /scwm/de_ps_count
      RETURNING VALUE(rs_mat) TYPE /scmb/mdl_product.

    METHODS read_material_list_from_ean
      IMPORTING it_mean_tab             TYPE mean_tab
      RETURNING VALUE(rt_material_text) TYPE ztt_material_text_data.

    METHODS read_material_list_from_mpn
      IMPORTING iv_mfrpn                TYPE mfrpn
      RETURNING VALUE(rt_material_text) TYPE ztt_material_text_data.

    METHODS read_warehouse_default_values
      IMPORTING iv_lgnum        TYPE /scwm/lgnum
      RETURNING VALUE(rs_t340d) TYPE /scwm/s_t340d.

    METHODS read_max_num_of_bin_factor
      IMPORTING iv_lgnum         TYPE /scwm/lgnum
                iv_lgtyp         TYPE /scwm/lgtyp
                iv_demind        TYPE /scwm/de_demind DEFAULT 'A'
      RETURNING VALUE(rs_factor) TYPE /scwm/tdemfac.

    METHODS propose_max_num_of_bin
      IMPORTING is_warehouse_data    TYPE zstr_warehouse_data
      CHANGING  cs_storage_type_data TYPE zstr_storage_type_data.

    METHODS read_matlwh_single
      IMPORTING iv_matnr         TYPE /sapapo/matio-matnr
      RETURNING VALUE(rs_matlwh) TYPE /sapapo/matlwh.

  PRIVATE SECTION.
    METHODS is_ean
      IMPORTING VALUE(iv_scanned_value) TYPE zde_scanned_value
      CHANGING  cs_sub_section_1        TYPE zstr_product_characteristics
      RETURNING VALUE(rv_ean)           TYPE abap_boolean.

    METHODS is_manufacturer_part_number
      IMPORTING VALUE(iv_scanned_value)            TYPE zde_scanned_value
      CHANGING  cs_sub_section_1                   TYPE zstr_product_characteristics
      RETURNING VALUE(rv_manufacturer_part_number) TYPE abap_boolean.

    METHODS is_product_number
      IMPORTING VALUE(iv_scanned_value)  TYPE zde_scanned_value
      CHANGING  cs_sub_section_1         TYPE zstr_product_characteristics
      RETURNING VALUE(rv_product_number) TYPE abap_boolean.

    METHODS get_matid_from_matnr
      IMPORTING iv_matnr        TYPE mara-matnr
      RETURNING VALUE(rv_matid) TYPE /scmb/mdl_matid.

    METHODS get_product_details
      IMPORTING iv_matid           TYPE /scmb/mdl_matid
      EXPORTING es_material_global TYPE /scwm/s_material_global
                es_mat_hazard      TYPE /scwm/s_material_hazard
                et_mat_mean        TYPE /scwm/tt_mat_mean.

    METHODS read_material_master_single
      IMPORTING iv_matnr       TYPE mara-matnr
      RETURNING VALUE(rs_mara) TYPE mara.

    METHODS read_unit_of_measures_for_mat
      IMPORTING iv_matnr       TYPE mara-matnr
      RETURNING VALUE(rt_marm) TYPE marm_tab.

    METHODS read_flowrack_algorithm.

    METHODS read_storage_type_control
      IMPORTING iv_lgnum       TYPE /scwm/lgnum
                iv_lgtyp       TYPE /scwm/lgtyp
      RETURNING VALUE(rs_t331) TYPE /scwm/t331.

    METHODS get_values_from_valuation_set
      IMPORTING iv_material             TYPE marc-matnr
      RETURNING VALUE(rs_valuation_set) TYPE /scwm/t_valuate.

    METHODS fill_values_from_ean
      IMPORTING it_mean_tab      TYPE mean_tab
      CHANGING  cs_sub_section_1 TYPE zstr_product_characteristics.

    METHODS fill_values_from_product
      IMPORTING iv_matnr         TYPE mara-matnr
      CHANGING  cs_sub_section_1 TYPE zstr_product_characteristics.

    METHODS fill_values_from_mfrpn          " Manufacturer Part Number
      IMPORTING is_material_master TYPE mara
      CHANGING  cs_sub_section_1   TYPE zstr_product_characteristics.

    METHODS query_manufacturer_part_number
      IMPORTING VALUE(iv_scanned_value) TYPE zde_scanned_value
      RETURNING VALUE(rs_mara)          TYPE mara.

    METHODS fill_hazardous_mat_features
      IMPORTING VALUE(is_material_master)     TYPE mara
                VALUE(is_material_master_apo) TYPE /sapapo/matlwh_out
      CHANGING  cs_sub_section_2              TYPE zstr_material_hazardous_info ##NEEDED.

    METHODS fill_serial_no_id_capturing
      IMPORTING VALUE(is_plant_material) TYPE marc
                VALUE(iv_sn_obligation)  TYPE zde_serial_check
      CHANGING  cs_sub_section_3         TYPE zstr_ident_numbers ##NEEDED.

    METHODS fill_unit_of_measures
      IMPORTING VALUE(is_material_master) TYPE mara
      CHANGING  ct_middle_block           TYPE ztt_unit_of_measueres.

    METHODS get_plant_logsys_by_entitled
      RETURNING VALUE(rs_plant_logsys) TYPE mty_werks_logsys.

    METHODS read_plant_data
      IMPORTING iv_material              TYPE marc-matnr
                iv_plant                 TYPE marc-werks
      RETURNING VALUE(rs_material_plant) TYPE marc.

    METHODS get_article_mixture_text
      IMPORTING iv_opti              TYPE marc-zz1_opti_plt
      RETURNING VALUE(rv_desription) TYPE char60.

    METHODS get_product_hierarchy_text
      IMPORTING iv_product_hierarchy             TYPE prodh_d
      RETURNING VALUE(rv_product_hierarchy_text) TYPE wb2_prodh_text.

    METHODS get_serial_obligation
      IMPORTING iv_product_hierarchy        TYPE prodh_d
                iv_manufacturer             TYPE mfrnr
      RETURNING VALUE(rs_serial_obligation) TYPE ztcross_serialch.

    METHODS create_icon
      IMPORTING iv_icon_name   TYPE iconname
      RETURNING VALUE(rv_icon) TYPE char30 ##RELAX.

    METHODS get_patterns
      IMPORTING iv_idtype         TYPE zde_indenttab
      RETURNING VALUE(rs_pattern) TYPE ztcross_patterns.

    METHODS unlock_material
      IMPORTING is_material_master TYPE mara.

    METHODS check_apo_material
      IMPORTING iv_matnr TYPE /sapapo/matio-matnr.

    METHODS read_apo_material
      IMPORTING iv_matnr               TYPE /sapapo/matio-matnr
                iv_scuguid             TYPE /scmb/mdl_scuguid
                iv_entitled_id         TYPE /scwm/de_entitled_id
      RETURNING VALUE(rs_apo_material) TYPE /sapapo/matlwh_out.

    METHODS create_warehouse_data
      IMPORTING iv_matnr       TYPE /scmb/mdl_matnr
                iv_scuguid     TYPE /scmb/mdl_scuguid
                iv_entitled_id TYPE /scwm/de_entitled_id
                is_matlwh      TYPE /sapapo/matio_wh ##NEEDED.

    METHODS material_get_matid
      IMPORTING iv_matnr        TYPE /sapapo/matnr
      RETURNING VALUE(rv_matid) TYPE /sapapo/matid.

    METHODS get_apo_matid
      IMPORTING iv_matnr        TYPE /sapapo/matio-matnr
      RETURNING VALUE(rv_matid) TYPE /sapapo/matkey-matid.

    METHODS read_material_group_text
      IMPORTING iv_matkl             TYPE matkl
      RETURNING VALUE(rv_matkl_text) TYPE wgbez.

    METHODS read_material_type_text
      IMPORTING iv_mat_type          TYPE mtart
      RETURNING VALUE(rv_mtart_text) TYPE mtbez.

    METHODS read_manufacturer_text
      IMPORTING iv_mfrnr             TYPE mara-mfrnr
      RETURNING VALUE(rv_mfrnr_text) TYPE lfa1-name1.

    METHODS read_dangerous_goods
      IMPORTING iv_matnr           TYPE /sapapo/matio-matnr
      RETURNING VALUE(rt_rdgmdiot) TYPE ehs_rdgmdiot_t.

    METHODS read_putaway_strategy_text
      IMPORTING iv_put_stra           TYPE /scwm/de_put_stra
      RETURNING VALUE(rv_description) TYPE /scwm/de_desc40.

    METHODS get_volume_indicator_text
      IMPORTING iv_volind             TYPE /scwm/de_volind
      RETURNING VALUE(rv_description) TYPE /scwm/de_desc40.

    METHODS packspec_read_keys_from_matnr
      IMPORTING iv_matnr         TYPE /scmb/mdl_matnr
      RETURNING VALUE(rt_ps_key) TYPE /scwm/tt_ps_header_key.

    METHODS packspec_read
      IMPORTING it_ps_key          TYPE /scwm/tt_ps_header_key
      RETURNING VALUE(rt_packspec) TYPE /scwm/tt_packspec_int.

    METHODS mara_read
      IMPORTING iv_matnr       TYPE mara-matnr
      RETURNING VALUE(rs_mara) TYPE mara ##RELAX.

    METHODS update_maxqty.
    METHODS materail_texts_read.
ENDCLASS.



CLASS ZCL_MDM_MODEL IMPLEMENTATION.


  METHOD check_apo_material.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check APO material
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION '/SAPAPO/CHECK_STRING'
      EXPORTING
        string                        = iv_matnr
      EXCEPTIONS
        contain_leading_blanks        = 1
        contain_special_characters    = 2
        contain_non_syntactical_chars = 3
        OTHERS                        = 4.
    CASE sy-subrc.
      WHEN 1.
        " Leading blanks not allowed
        RAISE EXCEPTION NEW zcx_mdm_tool( textid   = zif_c_mdm_tool=>c_leadning_blanks
                                          mv_msgv1 = CONV #( iv_matnr ) ).
      WHEN 2.
        " Characters '*', '&' and ',' not allowed
        RAISE EXCEPTION NEW zcx_mdm_tool( textid   = zif_c_mdm_tool=>c_not_allowed_character
                                          mv_msgv1 = CONV #( iv_matnr ) ).
      WHEN 3.

    ENDCASE.
  ENDMETHOD.


  METHOD check_bintype.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check Bin Type
    "*&
    "*&
    "********************************************************************
    DATA ls_t303 TYPE /scwm/s_t303 ##NEEDED.
    DATA lv_func TYPE tfdir-funcname.

    IF iv_bintype IS INITIAL.
      RETURN.
    ENDIF.
    lv_func = '/SCWM/T303_READ_SINGLE'.
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = lv_func
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.

    IF sy-subrc = 0.
      TRY.
          CALL FUNCTION lv_func
            EXPORTING
              iv_lgnum  = ms_top_block-sub_section_1-lgnum
              iv_lptyp  = iv_bintype
            IMPORTING
              es_t303   = ls_t303
            EXCEPTIONS
              not_found = 1
              OTHERS    = 2 ##FM_SUBRC_OK.
        CATCH /scwm/cx_core_no_data.
          MESSAGE e011(/scwm/wm_sel) WITH iv_bintype me->ms_top_block-sub_section_1-lgnum.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD check_concstat_entry.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check Slotting status
    "*&
    "*&
    "********************************************************************
    DATA lv_concstat TYPE /scwm/de_concstat.

    IF me->ms_top_block-sub_section_1-matnr IS INITIAL.
      RETURN.
    ENDIF.
    DATA(ls_t300_md) = read_location( ).
    DATA(ls_partner) = partner_read( me->ms_top_block-sub_section_1-entitled ).
    DATA(lv_matid)   = material_get_matid( iv_matnr = ms_top_block-sub_section_1-matnr ).

    " get original Data from DB
    SELECT SINGLE concstat FROM /sapapo/matlwh
      INTO lv_concstat
      WHERE matid       = lv_matid
        AND scuguid     = ls_t300_md-scuguid
        AND entitled_id = ls_partner-partner_guid.
    IF sy-subrc = 0.
      IF     ( lv_concstat  = 2 OR  lv_concstat  = 3 )
         AND ( iv_concstat <> 2 AND iv_concstat <> 3 ).
        RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_slotting_status_not_allowed ).
        " Slotting Status is not allowed
      ELSEIF     ( lv_concstat IS INITIAL OR  lv_concstat  = 1 )
             AND ( iv_concstat <> space   AND iv_concstat <> 1 ).
        RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_slotting_status_not_allowed ).
        " Slotting Status is not allowed
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_cycle_counting_ind.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check cycle counting indicator
    "*&
    "*&
    "********************************************************************
    IF iv_ccind IS INITIAL.
      RETURN.
    ENDIF.
    DATA lo_mat_badi TYPE REF TO /sapapo/md_badi_003.
    DATA lv_active   TYPE activity.

    " get badi reference
    GET BADI lo_mat_badi.

    CALL FUNCTION 'ENH_BADI_IMG_IMPL_IS_ACTIVE'
      EXPORTING
        enhname          = '/SCWM/PI_BADI_003'
        impl_name        = '/SCWM/PI_BADI_003'
      IMPORTING
        active           = lv_active
      EXCEPTIONS
        imp_not_existing = 1
        OTHERS           = 2.

    IF sy-subrc = 0 AND lv_active = 'X'.
      CALL BADI lo_mat_badi->check_ccind
        EXPORTING
          iv_lgnum = ms_top_block-sub_section_1-lgnum
          iv_ccind = iv_ccind.

    ENDIF.
  ENDMETHOD.


  METHOD check_dispatchable_entry.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check dispatchable entry
    "*&
    "*&
    "********************************************************************
    IF iv_disp_whd IS NOT INITIAL.
      DATA(lv_dispatchable) = iv_disp_whd.
      lv_dispatchable = to_upper( lv_dispatchable ).
      TRY.
          DATA(lo_cds_value_help) = cl_cfd_sap_gui_cds_value_help=>get_instance( 'ZZ1_DISP_V' ).
          DATA(lv_text) = lo_cds_value_help->get_description( iv_value = lv_dispatchable ).
        CATCH cx_cfd_sap_gui_controller ##NO_HANDLER.
      ENDTRY.
    ENDIF.
    IF lv_text IS INITIAL AND iv_disp_whd IS NOT INITIAL.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_dispatch_value_not_allowed ).
    ENDIF.
    cv_description = lv_text.
  ENDMETHOD.


  METHOD check_material_texts.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check material text
    "*&
    "*&
    "********************************************************************
    DATA(ls_text) = VALUE #( me->master_data-mt_mat_texts[ lgnum     = ms_top_block-sub_section_1-lgnum
                                                           text_type = iv_text_id
                                                           text_id   = iv_text ] OPTIONAL ).
    IF ls_text-text_id IS NOT INITIAL.
      cv_description = ls_text-text.
    ELSE.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_material_text_not_found ).
    ENDIF.
  ENDMETHOD.


  METHOD check_put_stra.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check putaway strategy text
    "*&
    "*&
    "********************************************************************
    IF iv_put_stra IS INITIAL.
      RETURN.
    ENDIF.

    SELECT COUNT( * ) FROM /scwm/t305q
      WHERE lgnum    = @me->ms_top_block-sub_section_1-lgnum
        AND put_stra = @iv_put_stra  ##WARN_OK.
    IF sy-subrc <> 0.
      MESSAGE e301(/scwm/con) WITH iv_put_stra me->ms_top_block-sub_section_1-lgnum.
    ENDIF.

    cv_description = read_putaway_strategy_text( iv_put_stra ).
  ENDMETHOD.


  METHOD check_sectind.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check Section Indicator
    "*&
    "*&
    "********************************************************************
    DATA ls_t304 TYPE /scwm/s_t304 ##NEEDED.
    DATA lv_func TYPE tfdir-funcname.

    IF iv_sectind IS INITIAL.
      RETURN.
    ENDIF.
    lv_func = '/SCWM/T304_READ_SINGLE'.
    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = lv_func
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.

    IF sy-subrc = 0.
      CALL FUNCTION lv_func
        EXPORTING
          iv_lgnum  = ms_top_block-sub_section_1-lgnum
          iv_lgbkz  = iv_sectind
        IMPORTING
          es_t304   = ls_t304
        EXCEPTIONS
          not_found = 1
          OTHERS    = 2.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
                DISPLAY LIKE 'E'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_storage_type.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check storage type
    "*&
    "*&
    "********************************************************************
    IF iv_lgtyp IS INITIAL.
      RETURN.
    ENDIF.
    SELECT COUNT( * ) FROM /scwm/t301
      WHERE lgnum = @me->ms_top_block-sub_section_1-lgnum
        AND lgtyp = @iv_lgtyp ##WARN_OK.
    IF sy-subrc <> 0.
      MESSAGE e003(/scwm/wm_sel) WITH me->ms_top_block-sub_section_1-lgnum.
    ENDIF.
  ENDMETHOD.


  METHOD check_volume_indicator.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : check volume indicator
    "*&
    "*&
    "********************************************************************
    IF iv_volind IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/TDIMIND_READ_SINGLE'
      EXPORTING
        iv_lgnum    = ms_top_block-sub_section_1-lgnum
        iv_dimind   = iv_volind
        iv_dimindtp = '02'
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_mdm_tool
            MESSAGE ID sy-msgid
            TYPE sy-msgty
            NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      cv_description = get_volume_indicator_text( iv_volind ).
    ENDIF.
  ENDMETHOD.


  METHOD clear_master_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clear master data
    "*&
    "*&
    "********************************************************************
    CLEAR me->master_data.
  ENDMETHOD.


  METHOD clear_middle_block.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clear middle block of the screen
    "*&
    "*&
    "********************************************************************
    CLEAR me->ms_midlle_block-t_unit_of_measures.
    cs_middle_block = ms_midlle_block-t_unit_of_measures.
  ENDMETHOD.


  METHOD clear_storage_bin_type_sel.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clear fields
    "*&
    "*&
    "********************************************************************
    CLEAR ms_lower_block-t_storage_bin_type_sel.
    ct_storage_bin_type_sel = ms_lower_block-t_storage_bin_type_sel.
  ENDMETHOD.


  METHOD clear_storage_type_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clear storage type data
    "*&
    "*&
    "********************************************************************
    CLEAR ms_lower_block-s_storage_type_data.
    cs_storage_type_data = ms_lower_block-s_storage_type_data.
  ENDMETHOD.


  METHOD clear_sub_section_1.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clear screen section 1
    "*&
    "*&
    "********************************************************************
    CLEAR:
    ms_top_block-sub_section_1-scanned_value,
    ms_top_block-sub_section_1-entitled_desc,
    ms_top_block-sub_section_1-matnr,
    ms_top_block-sub_section_1-matid,
    ms_top_block-sub_section_1-maktx,
    ms_top_block-sub_section_1-ean11,
    ms_top_block-sub_section_1-mfrpn,
    ms_top_block-sub_section_1-ersda,
    ms_top_block-sub_section_1-mfrnr,
    ms_top_block-sub_section_1-prdha,
    ms_top_block-sub_section_1-text_prdha,
    ms_top_block-sub_section_1-zzserial,
    ms_top_block-sub_section_1-serial_oblg_icon,
    ms_top_block-sub_section_1-opti,
    ms_top_block-sub_section_1-opti_descr,
    ms_top_block-sub_section_1-vprsv,
    ms_top_block-sub_section_1-verpr,
    ms_top_block-sub_section_1-stprs,
    ms_top_block-sub_section_1-peinh,
    ms_top_block-sub_section_1-waers,
    ms_top_block-sub_section_1-timestamp,
    ms_top_block-sub_section_1-is_ean,
    ms_top_block-sub_section_1-is_mfrpn,
    ms_top_block-sub_section_1-is_product_number,
    ms_top_block-sub_section_1-sled_bbd,
    ms_top_block-sub_section_1-tkui,
    ms_top_block-sub_section_1-dgnu,
    ms_top_block-sub_section_1-pdgnud.

    cs_sub_section_1 = ms_top_block-sub_section_1.
  ENDMETHOD.


  METHOD clear_sub_section_2.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "*&
    "*&
    "********************************************************************
    CLEAR ms_top_block-sub_section_2.
    cs_sub_section_2 = ms_top_block-sub_section_2.
  ENDMETHOD.


  METHOD clear_sub_section_3.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clear screen section 3
    "*&
    "*&
    "********************************************************************
    CLEAR ms_top_block-sub_section_3.
    cs_sub_section_3 = ms_top_block-sub_section_3.
  ENDMETHOD.


  METHOD clear_warehouse_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clear warehouse data
    "*&
    "*&
    "********************************************************************
    CLEAR ms_lower_block-s_warehouse_data.
    cs_warehouse_data = ms_lower_block-s_warehouse_data.
  ENDMETHOD.


  METHOD create_icon.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION 'ICON_CREATE'
      EXPORTING
        name                  = iv_icon_name
      IMPORTING
        result                = rv_icon
      EXCEPTIONS
        icon_not_found        = 1
        outputfield_too_short = 2
        OTHERS                = 3.
    IF sy-subrc <> 0 ##NEEDED.
      " Implement suitable error handling here
    ENDIF.
  ENDMETHOD.


  METHOD create_warehouse_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : If warehouse data tab doesn't exist at the first run
    "*& at transaction /SCMW/MAT1, we create this tab first
    "*& otherwise we are not able to run slotting
    "********************************************************************
    DATA lv_func       TYPE char30.
    DATA ls_matlwh_att TYPE /scwm/s_matlwh_att.

    lv_func = '/SCWM/GET_WHSE_PROD_MARC_FLDS'.

    CALL FUNCTION 'FUNCTION_EXISTS'
      EXPORTING
        funcname           = lv_func
      EXCEPTIONS
        function_not_exist = 1
        OTHERS             = 2.

    IF sy-subrc = 0.
      CALL FUNCTION lv_func
        EXPORTING
          iv_entitled = ms_top_block-sub_section_1-entitled
          iv_lgnum    = ms_top_block-sub_section_1-lgnum
          iv_matnr    = iv_matnr
        CHANGING
          cs_matlwh   = ls_matlwh_att.
    ENDIF.

    DATA(lv_matid) = material_get_matid( iv_matnr = iv_matnr ).
    SELECT SINGLE * FROM /sapapo/matlwh       "#EC CI_ALL_FIELDS_NEEDED
      INTO @DATA(ls_matlwh)
      WHERE matid       = @lv_matid
        AND scuguid     = @iv_scuguid
        AND entitled_id = @iv_entitled_id.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.
    " Read Only fields in S4 , must not be updated in redirect relevant tables

    MOVE-CORRESPONDING ls_matlwh_att TO ls_matlwh ##ENH_OK.
    CLEAR : ls_matlwh-ssqss,
            ls_matlwh-prfrq,
            ls_matlwh-batch_req_wh,
            ls_matlwh-docbatch.

    ls_matlwh-matid       = lv_matid.
    ls_matlwh-scuguid     = iv_scuguid.
    ls_matlwh-entitled_id = iv_entitled_id.

    CALL FUNCTION '/SAPAPO/DM_PRODUCT_POST_ONE'
      EXPORTING
        iv_vb         = zif_c_mdm_tool=>c_crud-insert
        is_matlwh     = ls_matlwh
      EXCEPTIONS
        not_qualified = 1
        insert_failed = 2
        update_failed = 3
        OTHERS        = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_mdm_tool
            MESSAGE ID sy-msgid
            TYPE sy-msgty
            NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    COMMIT WORK AND WAIT.
  ENDMETHOD.


  METHOD det_putaway_qty_class.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Putaway quantity class indicator
    "*&
    "*&
    "********************************************************************
    IF iv_storage_type IS INITIAL.
      RETURN.
    ENDIF.
    READ TABLE me->master_data-flowrack WITH KEY lgnum = ms_top_block-sub_section_1-lgnum
                                                 lgtyp = iv_storage_type
                                        INTO DATA(ls_flowrack).
    IF sy-subrc = 0 AND ls_flowrack-algo EQ zif_wme_c=>gs_algorithms-flowrack.
      rv_quanclaput = 2.
    ELSE.
      rv_quanclaput = space.
    ENDIF.
  ENDMETHOD.


  METHOD fill_hazardous_mat_features.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Hazardous section fields
    "*&
    "*&
    "********************************************************************
    ms_top_block-sub_section_2-mhmsign01   = is_material_master_apo-zz1_hmsign01_whd.
    ms_top_block-sub_section_2-mhmsign02   = is_material_master_apo-zz1_hmsign02_whd.
    ms_top_block-sub_section_2-mhmsign03   = is_material_master_apo-zz1_hmsign03_whd.
    ms_top_block-sub_section_2-mhmsingle01 = is_material_master-zz1_mhmsingle01_prd.
    ms_top_block-sub_section_2-mhmsingle02 = is_material_master-zz1_mhmsingle02_prd.
    ms_top_block-sub_section_2-mhmsingle03 = is_material_master-zz1_mhmsingle03_prd.
    ms_top_block-sub_section_2-mhmbox01    = is_material_master_apo-zz1_hmbox01_whd.
    ms_top_block-sub_section_2-mhmbox02    = is_material_master_apo-zz1_hmbox02_whd.
    ms_top_block-sub_section_2-mhmbox03    = is_material_master_apo-zz1_hmbox03_whd.
  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD fill_material_texts_tab.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Fill material texts tab at the screen
    "*&
    "*&
    "********************************************************************
    cs_material_texts-zz1_inb_deco_text_whd = is_matlwh-zz1_inb_deco_text_whd.
    IF cs_material_texts-zz1_inb_deco_text_whd IS NOT INITIAL.
      cs_material_texts-inb_deco_descr =
      VALUE #( me->master_data-mt_mat_texts[ lgnum     = ms_top_block-sub_section_1-lgnum
                                             text_type = zif_wme_c=>gc_text_types-inbound_deco
                                             text_id   = cs_material_texts-zz1_inb_deco_text_whd ]-text OPTIONAL ).

    ENDIF.

    cs_material_texts-zz1_picking_text_whd = is_matlwh-zz1_picking_text_whd.
    IF cs_material_texts-zz1_picking_text_whd IS NOT INITIAL.
      cs_material_texts-picking_descr =
        VALUE #( me->master_data-mt_mat_texts[ lgnum     = ms_top_block-sub_section_1-lgnum
                                               text_type = zif_wme_c=>gc_text_types-cust_general_picking
                                               text_id   = cs_material_texts-zz1_picking_text_whd ]-text OPTIONAL ).
    ENDIF.

    cs_material_texts-zz1_kep_picking_text_whd = is_matlwh-zz1_kep_picking_text_whd.
    IF cs_material_texts-zz1_kep_picking_text_whd IS NOT INITIAL.
      cs_material_texts-kep_picking_descr =
        VALUE #( me->master_data-mt_mat_texts[ lgnum     = ms_top_block-sub_section_1-lgnum
                                               text_type = zif_wme_c=>gc_text_types-kep_material_picking
                                               text_id   = cs_material_texts-zz1_kep_picking_text_whd ]-text OPTIONAL ).
    ENDIF.

    cs_material_texts-zz1_sped_picking_text_whd = is_matlwh-zz1_sped_picking_text_whd.
    IF cs_material_texts-zz1_sped_picking_text_whd IS NOT INITIAL.
      cs_material_texts-sped_picking_descr =
        VALUE #( me->master_data-mt_mat_texts[ lgnum     = ms_top_block-sub_section_1-lgnum
                                               text_type = zif_wme_c=>gc_text_types-sped_material_picking
                                               text_id   = cs_material_texts-zz1_sped_picking_text_whd ]-text OPTIONAL ).
    ENDIF.

    cs_material_texts-zz1_packing_text_whd = is_matlwh-zz1_packing_text_whd.
    IF cs_material_texts-zz1_packing_text_whd IS NOT INITIAL.
      cs_material_texts-packing_descr =
        VALUE #( me->master_data-mt_mat_texts[ lgnum     = ms_top_block-sub_section_1-lgnum
                                               text_type = zif_wme_c=>gc_text_types-cust_packing
                                               text_id   = cs_material_texts-zz1_packing_text_whd ]-text OPTIONAL ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_serial_no_id_capturing.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Serial numbers will be filled here
    "*&
    "*&
    "********************************************************************
    cs_sub_section_3-indenttab01   = is_plant_material-zz1_identtable01_plt.
    cs_sub_section_3-indenttab02   = is_plant_material-zz1_identtable02_plt.
    cs_sub_section_3-indenttab03   = is_plant_material-zz1_identtable03_plt.
    cs_sub_section_3-indenttab04   = is_plant_material-zz1_identtable04_plt.
    cs_sub_section_3-indenttab05   = is_plant_material-zz1_identtable05_plt.
    cs_sub_section_3-reason_code01 = is_plant_material-zz1_reasoncode1_plt.

    SELECT * FROM ztcross_numbers
      INTO TABLE @DATA(lt_numbers)
      WHERE lgnum = @ms_top_block-sub_section_1-lgnum
        AND spras = @sy-langu.

    DATA(ls_num) = VALUE #( lt_numbers[ selnum = ms_top_block-sub_section_3-indenttab01 ] OPTIONAL ) ##WARN_OK.

    cs_sub_section_3-description01 = ls_num-number_description.
    CLEAR ls_num.
    ls_num = VALUE #( lt_numbers[ selnum = ms_top_block-sub_section_3-indenttab02 ] OPTIONAL ) ##WARN_OK.

    cs_sub_section_3-description02 = ls_num-number_description.
    CLEAR ls_num.
    ls_num = VALUE #( lt_numbers[ selnum = ms_top_block-sub_section_3-indenttab03 ] OPTIONAL ) ##WARN_OK.

    cs_sub_section_3-description03 = ls_num-number_description.
    CLEAR ls_num.
    ls_num = VALUE #( lt_numbers[ selnum = ms_top_block-sub_section_3-indenttab04 ] OPTIONAL ) ##WARN_OK.

    cs_sub_section_3-description04 = ls_num-number_description.
    CLEAR ls_num.
    ls_num = VALUE #( lt_numbers[ selnum = ms_top_block-sub_section_3-indenttab05 ] OPTIONAL ) ##WARN_OK.

    cs_sub_section_3-description05 = ls_num-number_description.

    DATA(ls_pattern) = get_patterns( iv_idtype = is_plant_material-zz1_identtable01_plt ).
    cs_sub_section_3-template01 = ls_pattern-template.
    IF ls_pattern-template IS NOT INITIAL.
      cs_sub_section_3-is_template1_available = abap_true.
    ENDIF.

    CLEAR ls_pattern.
    ls_pattern = get_patterns( iv_idtype = is_plant_material-zz1_identtable02_plt ).
    cs_sub_section_3-template02 = ls_pattern-template.
    IF ls_pattern-template IS NOT INITIAL.
      cs_sub_section_3-is_template2_available = abap_true.
    ENDIF.

    CLEAR ls_pattern.
    ls_pattern = get_patterns( iv_idtype = is_plant_material-zz1_identtable03_plt ).
    cs_sub_section_3-template03 = ls_pattern-template.
    IF ls_pattern-template IS NOT INITIAL.
      cs_sub_section_3-is_template3_available = abap_true.
    ENDIF.

    CLEAR ls_pattern.
    ls_pattern = get_patterns( iv_idtype = is_plant_material-zz1_identtable04_plt ).
    cs_sub_section_3-template04 = ls_pattern-template.
    IF ls_pattern-template IS NOT INITIAL.
      cs_sub_section_3-is_template4_available = abap_true.
    ENDIF.

    CLEAR ls_pattern.
    ls_pattern = get_patterns( iv_idtype = is_plant_material-zz1_identtable05_plt ).
    cs_sub_section_3-template05 = ls_pattern-template.
    IF cs_sub_section_3-template04 IS NOT INITIAL.
      cs_sub_section_3-is_template4_available = abap_true.
    ENDIF.
    cs_sub_section_3-reason_code01 = is_plant_material-zz1_reasoncode1_plt.

    master_data-numbers = lt_numbers.

    SELECT * FROM ztcross_patterns
      INTO TABLE @DATA(lt_patterns)
      WHERE lgnum = @ms_top_block-sub_section_1-lgnum
        AND matnr = @me->master_data-mara-matnr.

    master_data-patterns = lt_patterns.

    SELECT SINGLE template FROM ztcross_patterns        "#EC CI_NOORDER
      INTO @me->master_data-imei_no_template
      WHERE lgnum   = @ms_top_block-sub_section_1-lgnum
        AND id_type = '002' ##WARN_OK.

    SELECT SINGLE template FROM ztcross_patterns        "#EC CI_NOORDER
      INTO @me->master_data-mac_address_template
      WHERE lgnum   = @ms_top_block-sub_section_1-lgnum
        AND id_type = '009' ##WARN_OK.

    SORT lt_patterns BY lgnum
                        matnr
                        id_type
                        counter.

    LOOP AT lt_patterns INTO DATA(ls_patterns) ##INTO_OK.
      IF is_plant_material-zz1_identtable01_plt IS NOT INITIAL.
        IF ls_patterns-id_type = is_plant_material-zz1_identtable01_plt.
          APPEND ls_patterns TO cs_sub_section_3-t_indenttab01.
          APPEND ls_patterns TO me->master_data-identab01.
        ENDIF.
      ENDIF.

      IF is_plant_material-zz1_identtable02_plt IS NOT INITIAL.
        IF ls_patterns-id_type = is_plant_material-zz1_identtable02_plt.
          APPEND ls_patterns TO cs_sub_section_3-t_indenttab02.
          APPEND ls_patterns TO me->master_data-identab02.
        ENDIF.
      ENDIF.

      IF is_plant_material-zz1_identtable03_plt IS NOT INITIAL.
        IF ls_patterns-id_type = is_plant_material-zz1_identtable03_plt.
          APPEND ls_patterns TO cs_sub_section_3-t_indenttab03.
          APPEND ls_patterns TO me->master_data-identab03.
        ENDIF.
      ENDIF.

      IF is_plant_material-zz1_identtable04_plt IS NOT INITIAL.
        IF ls_patterns-id_type = is_plant_material-zz1_identtable04_plt.
          APPEND ls_patterns TO cs_sub_section_3-t_indenttab04.
          APPEND ls_patterns TO me->master_data-identab04.
        ENDIF.
      ENDIF.

      IF is_plant_material-zz1_identtable05_plt IS NOT INITIAL.
        IF ls_patterns-id_type = is_plant_material-zz1_identtable05_plt.
          APPEND ls_patterns TO cs_sub_section_3-t_indenttab05.
          APPEND ls_patterns TO me->master_data-identab05.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD fill_storage_bin_type_sel.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Fill Max Capa for Storage Type/Product/Bin Type combination
    "*&
    "*&
    "********************************************************************
    CLEAR me->ms_lower_block-t_storage_bin_type_sel.
    IF iv_storage_type IS INITIAL.
      ct_storage_bin_type = ms_lower_block-t_storage_bin_type_sel.
      RETURN.
    ENDIF.
    SELECT * FROM ztlptyp_maxqty
      WHERE lgnum = @me->ms_top_block-sub_section_1-lgnum
        AND lgtyp = @iv_storage_type
        AND matid = @me->ms_top_block-sub_section_1-matid
      INTO TABLE @DATA(lt_maxqty).

    LOOP AT lt_maxqty ASSIGNING FIELD-SYMBOL(<ls_maxqty>).
      APPEND INITIAL LINE TO me->ms_lower_block-t_storage_bin_type_sel ASSIGNING FIELD-SYMBOL(<ls_line>).
      MOVE-CORRESPONDING <ls_maxqty> TO <ls_line> ##ENH_OK.
      <ls_line>-lptypt = read_storage_bin_type_text( <ls_line>-lptyp ).
    ENDLOOP.
    ct_storage_bin_type = ms_lower_block-t_storage_bin_type_sel.
  ENDMETHOD.


  METHOD fill_storage_type_data_tab.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Fill storage type data tab at the screen
    "*&
    "*&
    "********************************************************************
    IF is_matlwhst IS INITIAL.
      RETURN.
    ENDIF.

    cs_storage_type_data-lgnum   = ms_top_block-sub_section_1-lgnum.
    cs_storage_type_data-lgtyp   = is_matlwhst-lgtyp.
    cs_storage_type_data-sectind = is_matlwhst-sectind.
    IF is_matlwhst-sectind IS INITIAL.
      cs_storage_type_data-sectind = 'B'.
    ENDIF.
    cs_storage_type_data-bintype         = is_matlwhst-bintype.
    cs_storage_type_data-bintype_plan    = is_matlwhst-bintype_plan.
    cs_storage_type_data-repqty_dsp      = is_matlwhst-repqty.
    cs_storage_type_data-minqty_dsp      = is_matlwhst-minqty.
    cs_storage_type_data-maxqty_dsp      = is_matlwhst-maxqty.
    cs_storage_type_data-quanclaput      = is_matlwhst-quanclaput.
    cs_storage_type_data-zz1_dirrpl_stt  = is_matlwhst-zz1_dirrpl_stt.
    cs_storage_type_data-zz1_keepcar_whd = master_data-warehouse_data-zz1_keepcar_whd.
    cs_storage_type_data-zz1_maxput_stt  = is_matlwhst-zz1_maxput_stt.
    IF cs_storage_type_data-zz1_maxput_stt IS INITIAL.
      cs_storage_type_data-zz1_maxput_stt = 1.
    ENDIF.

    cs_storage_type_data-ltypt          = read_storage_type_text( cs_storage_type_data-lgtyp ).
    cs_storage_type_data-sectindt       = read_storage_section_ind_text( cs_storage_type_data-sectind ).
    cs_storage_type_data-bintypet       = read_storage_bin_type_text( cs_storage_type_data-bintype ).
    cs_storage_type_data-bintype_plan_t = read_storage_bin_type_text( cs_storage_type_data-bintype_plan ).
    cs_storage_type_data-store_uom      = read_store_uom( cs_storage_type_data ).
    IF cs_storage_type_data-quanclaput IS INITIAL.
      cs_storage_type_data-quanclaput = 2.
    ENDIF.
  ENDMETHOD.


  METHOD fill_storage_type_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Fill storage type texts
    "*&
    "*&
    "********************************************************************
    CLEAR ct_storage_type_description.

    LOOP AT it_storage_type ASSIGNING FIELD-SYMBOL(<ls_storage_type>).
      APPEND INITIAL LINE TO ct_storage_type_description ASSIGNING FIELD-SYMBOL(<ls_line>).
      MOVE-CORRESPONDING <ls_storage_type> TO <ls_line> ##ENH_OK.
      <ls_line>-ltypt = read_storage_type_text( <ls_storage_type>-lgtyp ).
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_unit_of_measures.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Fill Uniot of Measure values
    "*& There are fixed 3 lines at the table
    "*& this values will be read here
    "********************************************************************
    DATA lv_tote_material TYPE char18.

    DATA(lt_marm) = read_unit_of_measures_for_mat( iv_matnr = is_material_master-matnr ).
    master_data-marm = lt_marm.
    DATA(lt_fixed_values) = get_domain_values( iv_domname = zif_c_mdm_tool=>c_domains-unit
                                               iv_langu   = sy-langu ).

    LOOP AT lt_fixed_values ASSIGNING FIELD-SYMBOL(<ls_fixed_values>).
      CASE <ls_fixed_values>-domvalue_l.
        WHEN zif_c_mdm_tool=>c_units-piece.

          READ TABLE lt_marm INTO DATA(ls_marm) WITH KEY meinh = <ls_fixed_values>-domvalue_l ##WARN_OK.
          APPEND VALUE #( id               = zif_c_mdm_tool=>c_units-piece
                          id_description   = <ls_fixed_values>-ddtext
                          umrez            = ls_marm-umrez
                          umren            = ls_marm-umren
                          meins            = zif_c_mdm_tool=>c_units-piece
                          meinh            = ls_marm-meinh
                          ean11            = ls_marm-ean11
                          pmat             = VALUE #( )
                          pmatid           = VALUE #( )
                          pmat_description = VALUE #( )
                          laeng            = ls_marm-laeng
                          breit            = ls_marm-breit
                          hoehe            = ls_marm-hoehe
                          meabm            = ls_marm-meabm
                          brgew            = ls_marm-brgew
                          ntgew            = is_material_master-ntgew
                          gewei            = COND #( WHEN ls_marm-gewei IS NOT INITIAL
                                                     THEN ls_marm-gewei
                                                     ELSE zif_c_mdm_tool=>c_units-kg ) ) TO ct_middle_block.

        WHEN zif_c_mdm_tool=>c_units-mastercarton.
          DATA(lt_marm_temp) = lt_marm.
          DELETE lt_marm_temp WHERE meinh+0(1) <> <ls_fixed_values>-domvalue_l+0(1).
          SORT lt_marm_temp BY meinh.
          READ TABLE lt_marm_temp INTO ls_marm INDEX 1.
          DATA(lt_carton_mat) = NEW zcl_packmmat_algo(
            iv_lgnum = ms_top_block-sub_section_1-lgnum )->get_pmat_carton( ).
          DATA(lt_tote_mat)   = NEW zcl_packmmat_algo( iv_lgnum = ms_top_block-sub_section_1-lgnum )->get_pmat_totes( ).

          zcl_param=>get_parameter( EXPORTING iv_lgnum     = ms_top_block-sub_section_1-lgnum
                                              iv_process   = zif_param_const=>c_zcross_0005
                                              iv_parameter = zif_param_const=>c_pmat_tote
                                    IMPORTING ev_constant  = DATA(lv_tote_material_const) ).

          lv_tote_material = lv_tote_material_const.
          lv_tote_material = |{ lv_tote_material ALPHA = IN }|.

          DATA(ls_pmat_carton) = VALUE #( lt_carton_mat[
                                              nonconv = ms_lower_block-s_warehouse_data-zz1_nonconveyable_whd ] OPTIONAL ).
          DATA(ls_pmat_tote)   = VALUE #( lt_tote_mat[ matnr = lv_tote_material ] OPTIONAL ).
          IF ls_pmat_tote IS INITIAL.
            ls_pmat_tote = VALUE #( lt_tote_mat[ 1 ] OPTIONAL ).
          ENDIF.

          DATA(ls_material_details)      = get_mat_details_from_matid( iv_matid = ls_pmat_carton-matid ).
          DATA(ls_material_details_tote) = get_mat_details_from_matid( iv_matid = ls_pmat_tote-matid ).

          DATA(ls_mat) = packspec_read_pmat( iv_guid_ps = ms_midlle_block-s_packspec_header-guid_ps
                                             iv_aennr   = ms_midlle_block-s_packspec_header-aennr ).

          DATA(ls_level) = get_packspec_level_detail( iv_matnr = master_data-apo_material-matnr ).

          IF me->ms_lower_block-s_storage_type_data-zz1_dirrpl_stt IS NOT INITIAL. " Only in Case the DIRREPL = "X" Packmat for MC should be chosen from the system

            DATA(lv_pmat) = |{ ls_pmat_carton-matnr ALPHA = OUT }|.
            DATA(lv_id_description) = <ls_fixed_values>-ddtext.

            DATA(lv_pmatid)     = ls_pmat_carton-matid.
            DATA(lv_pmat_descr) = VALUE /scwm/de_ps_description( ls_material_details-txt[ langu = sy-langu ]-maktx OPTIONAL ).
            DATA(lv_hutyp)      = ls_material_details-pac-hutyp.

            DATA(lv_laeng) = COND #( WHEN ls_marm-laeng IS NOT INITIAL " MARM
                                                                             THEN ls_marm-laeng
                                     WHEN ls_level-max_length IS NOT INITIAL " PACKSPEC
                                                                             THEN ls_level-max_length
                                     ELSE                                         ls_mat-pac-maxl ). " Packaging Material

            DATA(lv_breit) = COND #( WHEN ls_marm-breit IS NOT INITIAL       THEN ls_marm-breit
                                     WHEN ls_level-max_weight IS NOT INITIAL THEN ls_level-max_weight
                                     ELSE                                         ls_mat-pac-maxb ).

            DATA(lv_hoehe) = COND #( WHEN ls_marm-hoehe IS NOT INITIAL  " MARM
                                                                             THEN ls_marm-hoehe
                                     WHEN ls_level-max_height IS NOT INITIAL " PACKSPEC
                                                                             THEN ls_level-max_height
                                     ELSE                                         ls_mat-pac-maxh ). " Packaging Material

          ELSE. " the packmat must be a tote packmat as DIRREPL = " "

            lv_pmat = |{ ls_pmat_tote-matnr ALPHA = OUT }|.
            lv_id_description = TEXT-003 ##TEXT_USE.
            lv_pmatid     = ls_pmat_tote-matid.

            lv_pmat_descr = VALUE #( ls_material_details_tote-txt[ langu = sy-langu ]-maktx OPTIONAL ).

            lv_hutyp      = ls_material_details_tote-pac-hutyp.

            lv_laeng      = COND #( WHEN ls_marm-laeng IS NOT INITIAL THEN
                                      ls_marm-laeng
                                    WHEN ls_material_details_tote-pac-maxl IS NOT INITIAL THEN
                                      ls_material_details_tote-pac-maxl
                                    ELSE
                                      ls_mat-pac-maxl ).

            lv_breit      = COND #( WHEN ls_marm-breit IS NOT INITIAL THEN
                                      ls_marm-breit
                                    WHEN ls_material_details_tote-pac-maxb IS NOT INITIAL THEN
                                      ls_material_details_tote-pac-maxb

                                    ELSE
                                      ls_mat-pac-maxb ).

            lv_hoehe      = COND #( WHEN ls_marm-hoehe IS NOT INITIAL THEN
                                      ls_marm-hoehe
                                    WHEN ls_material_details_tote-pac-maxh IS NOT INITIAL THEN
                                      ls_material_details_tote-pac-maxh
                                    ELSE
                                      ls_mat-pac-maxh ).
          ENDIF.

          APPEND VALUE #(
              id               = zif_c_mdm_tool=>c_units-mastercarton
              id_description   = lv_id_description
              umrez            = ls_marm-umrez
              umren            = ls_marm-umren
              meins            = zif_c_mdm_tool=>c_units-piece
              meinh            = ls_marm-meinh
              ean11            = ls_marm-ean11
              pmat             = lv_pmat
              pmatid           = lv_pmatid
              pmat_description = lv_pmat_descr
              hutyp            = lv_hutyp
              laeng            = lv_laeng
              breit            = lv_breit
              hoehe            = lv_hoehe
              meabm            = COND #( WHEN ls_level-unit_max_lwh IS NOT INITIAL THEN ls_level-unit_max_lwh
                                         WHEN ls_marm-meabm IS NOT INITIAL         THEN ls_marm-meabm
                                         ELSE                                           zif_c_mdm_tool=>c_units-mm )
              brgew            = COND #( WHEN ls_level-max_weight IS NOT INITIAL
                                         THEN ls_level-max_weight
                                         ELSE ls_marm-brgew )
              ntgew            = is_material_master-ntgew * ls_marm-umrez
              gewei            = COND #( WHEN ls_level-unit_gw IS NOT INITIAL THEN ls_level-unit_gw
                                         WHEN ls_marm-gewei IS NOT INITIAL    THEN ls_marm-gewei
                                         ELSE                                      zif_c_mdm_tool=>c_units-kg ) ) TO ct_middle_block.
        WHEN zif_c_mdm_tool=>c_units-palet.
          DATA(lt_pallet_mat) = NEW zcl_packmmat_algo(
            iv_lgnum = ms_top_block-sub_section_1-lgnum )->get_pmat_pallet_sped( ).

          DATA(ls_pmat_pallet) = VALUE #( lt_pallet_mat[ 1 ] OPTIONAL ).
          ls_material_details = get_mat_details_from_matid( iv_matid = ls_pmat_pallet-matid ).
          READ TABLE lt_marm INTO ls_marm WITH KEY meinh = <ls_fixed_values>-domvalue_l ##WARN_OK.
          IF ls_marm IS NOT INITIAL.
            APPEND VALUE #( id               = zif_c_mdm_tool=>c_units-palet
                            id_description   = <ls_fixed_values>-ddtext
                            umrez            = ls_marm-umrez
                            umren            = ls_marm-umren
                            meins            = zif_c_mdm_tool=>c_units-piece
                            meinh            = zif_c_mdm_tool=>c_units-palet
                            ean11            = ls_marm-ean11
                            pmat             = |{ ls_pmat_pallet-matnr  ALPHA = OUT }|
                            pmatid           = ls_pmat_pallet-matid
                            pmat_description = VALUE #( ls_material_details-txt[ langu = sy-langu ]-maktx OPTIONAL )
                            hutyp            = ls_material_details-pac-hutyp
                            laeng            = ls_marm-laeng
                            breit            = ls_marm-breit
                            hoehe            = ls_marm-hoehe
                            meabm            = COND #( WHEN ls_marm-meabm IS NOT INITIAL
                                                       THEN ls_marm-meabm
                                                       ELSE zif_c_mdm_tool=>c_units-mm )
                            brgew            = ls_marm-brgew
                            ntgew            = is_material_master-ntgew * ls_marm-umrez
                            gewei            = COND #( WHEN ls_marm-gewei IS NOT INITIAL
                                                       THEN ls_marm-gewei
                                                       ELSE zif_c_mdm_tool=>c_units-kg ) )
                   TO ct_middle_block.
          ELSE.
            APPEND VALUE #( id               = zif_c_mdm_tool=>c_units-palet
                            id_description   = <ls_fixed_values>-ddtext
                            umrez            = VALUE #( ) " ls_material_details-uom[ 1 ]-umrez OPTIONAL
                            umren            = VALUE #( ls_material_details-uom[ 1 ]-umren OPTIONAL )
                            meins            = zif_c_mdm_tool=>c_units-piece
                            meinh            = zif_c_mdm_tool=>c_units-palet
                            ean11            = VALUE #( )
                            pmat             = VALUE #( )
                            pmatid           = VALUE #( )
                            pmat_description = VALUE #( )
                            hutyp            = VALUE #( )
                            laeng            = VALUE #( )
                            breit            = VALUE #( )
                            hoehe            = VALUE #( )
                            meabm            = VALUE #( )
                            brgew            = VALUE #( )
                            ntgew            = is_material_master-ntgew * ls_marm-umrez
                            gewei            = VALUE #( ) )
                   TO ct_middle_block.
          ENDIF.

      ENDCASE.
      CLEAR: lt_carton_mat,
             ls_pmat_carton,
             ls_material_details,
             lt_pallet_mat,
             ls_pmat_pallet,
             ls_marm.
    ENDLOOP.
  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD fill_values_from_ean.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : If scanned value is EAN
    "*& this section will fill screen values from that number
    "*&
    "********************************************************************
    IF it_mean_tab IS INITIAL.
      RETURN.
    ENDIF.

    DATA ls_material TYPE zstr_material_text_data.
    DATA(lt_material_text) = read_material_list_from_ean( it_mean_tab = it_mean_tab ).

    DATA(lv_line) = lines( lt_material_text ).

    IF lv_line > 1.
      CALL FUNCTION 'Z_MDM_MATERIAL_POPUP'
        EXPORTING
          it_material = lt_material_text
        IMPORTING
          es_material = ls_material.
      IF ls_material IS INITIAL.
        RETURN.
      ENDIF.
    ELSEIF lv_line = 1.
      ls_material = VALUE #( lt_material_text[ 1 ] ).
    ENDIF.

    ASSIGN it_mean_tab[ matnr = ls_material-matnr ] TO FIELD-SYMBOL(<fs_mean_tab>).
    IF <fs_mean_tab> IS ASSIGNED.

      cs_sub_section_1-ean11  = <fs_mean_tab>-ean11.
      cs_sub_section_1-matnr  = <fs_mean_tab>-matnr.
      cs_sub_section_1-matid  = get_matid_from_matnr( iv_matnr = <fs_mean_tab>-matnr ).
      cs_sub_section_1-is_ean = abap_true.
      get_product_details( EXPORTING iv_matid           = cs_sub_section_1-matid
                           IMPORTING es_material_global = DATA(ls_material_global)
                                     es_mat_hazard      = DATA(ls_mat_hazard) ).
      master_data-mat_global = ls_material_global.
      master_data-mat_hazard = ls_mat_hazard.

      cs_sub_section_1-maktx  = ls_material_global-maktx.

      cs_sub_section_1-matkl  = ls_material_global-matkl.
      cs_sub_section_1-wgbez  = read_material_group_text( cs_sub_section_1-matkl ).

      cs_sub_section_1-is_ean = abap_true.
      DATA(ls_material_master)   = read_material_master_single( <fs_mean_tab>-matnr ).

      cs_sub_section_1-mtart = ls_material_master-mtart.
      cs_sub_section_1-mtbez = read_material_type_text( cs_sub_section_1-mtart ).

      DATA(ls_matlwh) = read_matlwh_single( iv_matnr = ls_material_master-matnr ).

      DATA lv_tz TYPE ttzz-tzone.

      IF ls_matlwh-createutc IS NOT INITIAL.
        CONVERT TIME STAMP ls_matlwh-createutc TIME ZONE lv_tz
                INTO DATE DATA(lv_date) TIME DATA(lv_time) ##NEEDED.
      ENDIF.

      IF lv_date IS NOT INITIAL.
        cs_sub_section_1-ersda = lv_date.
      ELSE.
        cs_sub_section_1-ersda = ls_material_master-ersda.
      ENDIF.

      cs_sub_section_1-mfrpn      = ls_material_master-mfrpn.
      cs_sub_section_1-mfrnr      = ls_material_master-mfrnr.
      cs_sub_section_1-mhdrz      = ls_material_master-mhdrz.
      cs_sub_section_1-text_mfrnr = read_manufacturer_text( cs_sub_section_1-mfrnr ).
      cs_sub_section_1-prdha      = ls_material_master-prdha.

      IF ls_material_master-sled_bbd = 'E'.
        cs_sub_section_1-sled_bbd = abap_true.
      ENDIF.
      cs_sub_section_1-mhdrz = ls_material_master-mhdrz.
      DATA(ls_valuation_set) = get_values_from_valuation_set( ls_material_master-matnr ).
      cs_sub_section_1-vprsv      = ls_valuation_set-vprsv.
      cs_sub_section_1-verpr      = ls_valuation_set-verpr.
      cs_sub_section_1-stprs      = ls_valuation_set-stprs.
      cs_sub_section_1-peinh      = ls_valuation_set-peinh.

      cs_sub_section_1-text_prdha = get_product_hierarchy_text( iv_product_hierarchy = ls_material_master-prdha ).

      master_data-apo_material = read_apo_material( iv_matnr       = ls_material_master-matnr
                                                    iv_scuguid     = ms_t300_md-scuguid
                                                    iv_entitled_id = CONV #( ms_partner-partner_guid ) ).

      fill_hazardous_mat_features( EXPORTING is_material_master     = ls_material_master
                                             is_material_master_apo = master_data-apo_material
                                   CHANGING  cs_sub_section_2       = ms_top_block-sub_section_2 ).

      master_data-storage_type = read_storage_type_data( iv_matnr       = ls_material_master-matnr
                                                         iv_scuguid     = ms_t300_md-scuguid
                                                         iv_entitled_id = CONV #( ms_partner-partner_guid ) ).

      fill_storage_type_text( EXPORTING it_storage_type             = master_data-storage_type
                              CHANGING  ct_storage_type_description = master_data-storage_type_description ).

      DATA(lt_dangerous) = read_dangerous_goods( ls_material_master-matnr ).

      cs_sub_section_1-tkui   = VALUE #( lt_dangerous[ 1 ]-tkui OPTIONAL ).
      cs_sub_section_1-dgnu   = VALUE #( lt_dangerous[ 1 ]-dgnu OPTIONAL ).
      cs_sub_section_1-pdgnud = VALUE #( lt_dangerous[ 1 ]-pdgnud OPTIONAL ).

      DATA(ls_plant) = get_plant_logsys_by_entitled( ).

      DATA(ls_material_plant) = read_plant_data( iv_material = <fs_mean_tab>-matnr
                                                 iv_plant    = ls_plant-werks ).
      cs_sub_section_1-opti                 = ls_material_plant-zz1_opti_plt.
      cs_sub_section_1-zz1_businessunit_plt = ls_material_plant-zz1_businessunit_plt.
      IF cs_sub_section_1-opti IS NOT INITIAL.
        cs_sub_section_1-opti_descr = get_article_mixture_text( cs_sub_section_1-opti ).
      ENDIF.
    ENDIF.

    DATA(ls_serial_obligation) = get_serial_obligation( iv_product_hierarchy = ls_material_master-prdha
                                                        iv_manufacturer      = ls_material_master-mfrnr ).

    cs_sub_section_1-zzserial = ls_serial_obligation-zzserial.

    fill_serial_no_id_capturing( EXPORTING is_plant_material = ls_material_plant
                                           iv_sn_obligation  = cs_sub_section_1-zzserial
                                 CHANGING  cs_sub_section_3  = ms_top_block-sub_section_3 ).

    IF cs_sub_section_1-zzserial <> 2 AND ls_material_plant-zz1_identtable01_plt IS INITIAL.
      " Scenario 1- if Prod. Hierachry SN-managed = NO and  Product SN-managed = NO
      cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
    ELSEIF cs_sub_section_1-zzserial = 2 AND ls_material_plant-zz1_identtable01_plt IS INITIAL ##BOOL_OK.
      " Scenario 2- if Prod. Hierachry SN-managed = YES and  Product SN-managed = NO, not available on product
      " Scenario 3- if Prod. Hierachry SN-managed = YES and  Product SN-managed = NO, Master data incorrect, SN available on product
      cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-red_flag.
    ELSEIF cs_sub_section_1-zzserial = 2 AND ls_material_plant-zz1_identtable01_plt IS NOT INITIAL.
      " Scenario 4- if Prod. Hierachry SN-managed = YES and  Product SN-managed = YES
      cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
    ELSEIF cs_sub_section_1-zzserial <> 2 AND ls_material_plant-zz1_identtable01_plt IS NOT INITIAL.
      " Scenario 5- if Prod. Hierachry SN-managed = NO and  Product SN-managed = YES
      cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-red_flag.
      IF ls_material_plant-zz1_reasoncode1_plt IS NOT INITIAL.
        cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
      ENDIF.
    ENDIF.

    " In case there is no record found in the table, we have a new situation. The situation “we don’t give a sh.t”.
    IF ls_serial_obligation IS INITIAL.
      cs_sub_section_1-serial_oblg_icon = ''.
    ENDIF.

    ms_midlle_block-s_packspec_header = get_packspec_header_detail( iv_matnr = ls_material_master-matnr ) ##ENH_OK.

    IF me->ms_midlle_block-s_packspec_header IS NOT INITIAL.
      IF me->ms_midlle_block-s_packspec_header-status = /scwm/cl_ppelipak_cntl=>gc_status_active.
        ms_midlle_block-s_packspec_header-icon = /scwm/cl_ppelipak_cntl=>gc_icon_active.
      ELSE.
        ms_midlle_block-s_packspec_header-icon = /scwm/cl_ppelipak_cntl=>gc_icon_inactive.
      ENDIF.
    ELSE.
      ms_midlle_block-s_packspec_header-icon = /scwm/cl_ppelipak_cntl=>gc_icon_part_act.
    ENDIF.

    DATA(ls_warehouse_data) = read_material_warehouse_data( iv_matnr       = ls_material_master-matnr
                                                            iv_scuguid     = ms_t300_md-scuguid
                                                            iv_entitled_id = CONV #( ms_partner-partner_guid ) ).

    create_warehouse_data( iv_matnr       = ls_material_master-matnr
                           iv_scuguid     = ms_t300_md-scuguid
                           iv_entitled_id = CONV #( ms_partner-partner_guid )
                           is_matlwh      = ls_warehouse_data ).

    update_material_warehouse_data( ls_warehouse_data ).
    cs_sub_section_1-block = ls_warehouse_data-block.
    IF ls_warehouse_data-block IS INITIAL.
      cs_sub_section_1-block = '01'.
    ENDIF.

    fill_warehouse_data_tab( EXPORTING is_matlwh         = ls_warehouse_data
                             CHANGING  cs_warehouse_data = ms_lower_block-s_warehouse_data ).

    fill_storage_type_data_tab( EXPORTING is_matlwhst          = VALUE #( me->master_data-storage_type[ 1 ] OPTIONAL )
                                CHANGING  cs_storage_type_data = ms_lower_block-s_storage_type_data ).

    fill_storage_bin_type_sel(
      EXPORTING
        iv_storage_type     = VALUE #( me->master_data-storage_type[ 1 ]-lgtyp OPTIONAL )
      CHANGING
        ct_storage_bin_type = ms_lower_block-t_storage_bin_type_sel ).

    fill_unit_of_measures( EXPORTING is_material_master = ls_material_master
                           CHANGING  ct_middle_block    = ms_midlle_block-t_unit_of_measures ).

    ms_master_carton_aditional-zz1_disp_whd          = ms_lower_block-s_warehouse_data-zz1_disp_whd.
    ms_master_carton_aditional-zz1_nonconveyable_whd = ms_lower_block-s_warehouse_data-zz1_nonconveyable_whd.
    ms_master_carton_aditional-disp_whd_text         = ms_lower_block-s_warehouse_data-disp_whd_text.
    ms_master_carton_aditional-zz1_dirrpl_stt        = ms_lower_block-s_storage_type_data-zz1_dirrpl_stt.
    ms_master_carton_aditional-zz1_keepcar_whd       = ms_lower_block-s_storage_type_data-zz1_keepcar_whd.

    materail_texts_read( ).

    fill_material_texts_tab( EXPORTING is_matlwh         = ls_warehouse_data
                             CHANGING  cs_material_texts = ms_lower_block-s_material_texts ).

    update_maxqty( ).
    read_flowrack_algorithm( ).

    DATA(lt_marm) = master_data-marm.
    DELETE lt_marm WHERE meinh+0(1) <> zif_c_mdm_tool=>c_units-mastercarton+0(1).
    SORT lt_marm BY meinh.
    DATA(lv_pieces) = VALUE #( lt_marm[ 1 ]-umrez OPTIONAL ).

    propose_replenishment_data( EXPORTING is_matlwhst          = VALUE #( me->master_data-storage_type[ 1 ] OPTIONAL )
                                          iv_pieces            = lv_pieces
                                CHANGING  cs_warehouse_data    = ms_lower_block-s_warehouse_data
                                          cs_storage_type_data = ms_lower_block-s_storage_type_data ).
  ENDMETHOD.


  METHOD fill_values_from_mfrpn.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : If scanned value is manufacturer part number
    "*& this section will fill screen values from that number
    "*&
    "********************************************************************
    cs_sub_section_1-matnr    = is_material_master-matnr.
    cs_sub_section_1-matid    = get_matid_from_matnr( is_material_master-matnr ).
    cs_sub_section_1-is_mfrpn = abap_true.
    DATA(ls_material_master)  = read_material_master_single( is_material_master-matnr ).

    get_product_details( EXPORTING iv_matid           = cs_sub_section_1-matid
                         IMPORTING es_material_global = DATA(ls_material_global)
                                   es_mat_hazard      = DATA(ls_mat_hazard)
                                   et_mat_mean        = DATA(lt_mat_mean) ).

    master_data-mat_global = ls_material_global.
    master_data-mat_hazard = ls_mat_hazard.

    ASSIGN lt_mat_mean[ 1 ] TO FIELD-SYMBOL(<fs_mean_tab>).
    IF <fs_mean_tab> IS ASSIGNED.
      cs_sub_section_1-ean11 = <fs_mean_tab>-ean11.
    ENDIF.
    cs_sub_section_1-maktx = ls_material_global-maktx.
    cs_sub_section_1-matkl = ls_material_global-matkl.
    cs_sub_section_1-wgbez = read_material_group_text( cs_sub_section_1-matkl ).

    cs_sub_section_1-mtart = is_material_master-mtart.
    cs_sub_section_1-mtbez = read_material_type_text( cs_sub_section_1-mtart ).

    DATA(ls_matlwh) = read_matlwh_single( iv_matnr = ls_material_master-matnr ).
    DATA lv_tz TYPE ttzz-tzone.

    IF ls_matlwh-createutc IS NOT INITIAL.
      CONVERT TIME STAMP ls_matlwh-createutc TIME ZONE lv_tz
              INTO DATE DATA(lv_date) TIME DATA(lv_time) ##NEEDED.
    ENDIF.

    IF lv_date IS NOT INITIAL.
      cs_sub_section_1-ersda = lv_date.
    ELSE.
      cs_sub_section_1-ersda = ls_material_master-ersda.
    ENDIF.
    cs_sub_section_1-mfrpn      = is_material_master-mfrpn.
    cs_sub_section_1-mfrnr      = is_material_master-mfrnr.
    cs_sub_section_1-mhdrz      = is_material_master-mhdrz.
    cs_sub_section_1-text_mfrnr = read_manufacturer_text( cs_sub_section_1-mfrnr ).
    cs_sub_section_1-prdha      = is_material_master-prdha.
    IF is_material_master-sled_bbd = 'E'.
      cs_sub_section_1-sled_bbd = abap_true.
    ENDIF.
    cs_sub_section_1-mhdrz = ls_material_master-mhdrz.
    DATA(ls_valuation_set) = get_values_from_valuation_set( ls_material_master-matnr ).
    cs_sub_section_1-vprsv      = ls_valuation_set-vprsv.
    cs_sub_section_1-verpr      = ls_valuation_set-verpr.
    cs_sub_section_1-stprs      = ls_valuation_set-stprs.
    cs_sub_section_1-peinh      = ls_valuation_set-peinh.

    cs_sub_section_1-text_prdha = get_product_hierarchy_text( iv_product_hierarchy = is_material_master-prdha ).

    master_data-apo_material = read_apo_material( iv_matnr       = ls_material_master-matnr
                                                  iv_scuguid     = ms_t300_md-scuguid
                                                  iv_entitled_id = CONV #( ms_partner-partner_guid ) ).

    fill_hazardous_mat_features( EXPORTING is_material_master     = ls_material_master
                                           is_material_master_apo = master_data-apo_material
                                 CHANGING  cs_sub_section_2       = ms_top_block-sub_section_2 ).

    master_data-storage_type = read_storage_type_data( iv_matnr       = ls_material_master-matnr
                                                       iv_scuguid     = ms_t300_md-scuguid
                                                       iv_entitled_id = CONV #( ms_partner-partner_guid ) ).

    fill_storage_type_text( EXPORTING it_storage_type             = master_data-storage_type
                            CHANGING  ct_storage_type_description = master_data-storage_type_description ).

    DATA(lt_dangerous) = read_dangerous_goods( ls_material_master-matnr ).

    cs_sub_section_1-tkui   = VALUE #( lt_dangerous[ 1 ]-tkui OPTIONAL ).
    cs_sub_section_1-dgnu   = VALUE #( lt_dangerous[ 1 ]-dgnu OPTIONAL ).
    cs_sub_section_1-pdgnud = VALUE #( lt_dangerous[ 1 ]-pdgnud OPTIONAL ).

    DATA(ls_plant) = get_plant_logsys_by_entitled( ).

    DATA(ls_material_plant) = read_plant_data( iv_material = is_material_master-matnr
                                               iv_plant    = ls_plant-werks ).
    cs_sub_section_1-opti                 = ls_material_plant-zz1_opti_plt.
    cs_sub_section_1-zz1_businessunit_plt = ls_material_plant-zz1_businessunit_plt.

    IF cs_sub_section_1-opti IS NOT INITIAL.
      cs_sub_section_1-opti_descr = get_article_mixture_text( cs_sub_section_1-opti ).
    ENDIF.

    DATA(ls_serial_obligation) = get_serial_obligation( iv_product_hierarchy = ls_material_master-prdha
                                                        iv_manufacturer      = ls_material_master-mfrnr ).

    cs_sub_section_1-zzserial = ls_serial_obligation-zzserial.

    fill_serial_no_id_capturing( EXPORTING is_plant_material = ls_material_plant
                                           iv_sn_obligation  = cs_sub_section_1-zzserial
                                 CHANGING  cs_sub_section_3  = ms_top_block-sub_section_3 ).

    IF cs_sub_section_1-zzserial <> 2 AND ls_material_plant-zz1_identtable01_plt IS INITIAL.
      " Scenario 1- if Prod. Hierachry SN-managed = NO and  Product SN-managed = NO
      cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
    ELSEIF cs_sub_section_1-zzserial = 2 AND ls_material_plant-zz1_identtable01_plt IS INITIAL ##BOOL_OK.
      " Scenario 2- if Prod. Hierachry SN-managed = YES and  Product SN-managed = NO, not available on product
      " Scenario 3- if Prod. Hierachry SN-managed = YES and  Product SN-managed = NO, Master data incorrect, SN available on product
      cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-red_flag.
    ELSEIF cs_sub_section_1-zzserial = 2 AND ls_material_plant-zz1_identtable01_plt IS NOT INITIAL.
      " Scenario 4- if Prod. Hierachry SN-managed = YES and  Product SN-managed = YES
      cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
    ELSEIF cs_sub_section_1-zzserial <> 2 AND ls_material_plant-zz1_identtable01_plt IS NOT INITIAL.
      " Scenario 5- if Prod. Hierachry SN-managed = NO and  Product SN-managed = YES
      cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-red_flag.
      IF ls_material_plant-zz1_reasoncode1_plt IS NOT INITIAL.
        cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
      ENDIF.
    ENDIF.

    " In case there is no record found in the table, we have a new situation. The situation “we don’t give a sh.t”.
    IF ls_serial_obligation IS INITIAL.
      cs_sub_section_1-serial_oblg_icon = ''.
    ENDIF.

    ms_midlle_block-s_packspec_header = get_packspec_header_detail( iv_matnr = ls_material_master-matnr ) ##ENH_OK.

    IF me->ms_midlle_block-s_packspec_header IS NOT INITIAL.
      IF me->ms_midlle_block-s_packspec_header-status = /scwm/cl_ppelipak_cntl=>gc_status_active.
        ms_midlle_block-s_packspec_header-icon = /scwm/cl_ppelipak_cntl=>gc_icon_active.
      ELSE.
        ms_midlle_block-s_packspec_header-icon = /scwm/cl_ppelipak_cntl=>gc_icon_inactive.
      ENDIF.
    ELSE.
      ms_midlle_block-s_packspec_header-icon = /scwm/cl_ppelipak_cntl=>gc_icon_part_act.
    ENDIF.

    DATA(ls_warehouse_data) = read_material_warehouse_data( iv_matnr       = ls_material_master-matnr
                                                            iv_scuguid     = ms_t300_md-scuguid
                                                            iv_entitled_id = CONV #( ms_partner-partner_guid ) ).

    create_warehouse_data( iv_matnr       = ls_material_master-matnr
                           iv_scuguid     = ms_t300_md-scuguid
                           iv_entitled_id = CONV #( ms_partner-partner_guid )
                           is_matlwh      = ls_warehouse_data ).

    update_material_warehouse_data( ls_warehouse_data ).
    cs_sub_section_1-block = ls_warehouse_data-block.
    IF ls_warehouse_data-block IS INITIAL.
      cs_sub_section_1-block = '01'.
    ENDIF.

    fill_warehouse_data_tab( EXPORTING is_matlwh         = ls_warehouse_data
                             CHANGING  cs_warehouse_data = ms_lower_block-s_warehouse_data ).

    fill_storage_type_data_tab( EXPORTING is_matlwhst          = VALUE #( me->master_data-storage_type[ 1 ] OPTIONAL )
                                CHANGING  cs_storage_type_data = ms_lower_block-s_storage_type_data ).

    fill_storage_bin_type_sel(
      EXPORTING
        iv_storage_type     = VALUE #( me->master_data-storage_type[ 1 ]-lgtyp OPTIONAL )
      CHANGING
        ct_storage_bin_type = ms_lower_block-t_storage_bin_type_sel ).

    fill_unit_of_measures( EXPORTING is_material_master = is_material_master
                           CHANGING  ct_middle_block    = ms_midlle_block-t_unit_of_measures ).

    ms_master_carton_aditional-zz1_disp_whd          = ms_lower_block-s_warehouse_data-zz1_disp_whd.
    ms_master_carton_aditional-zz1_nonconveyable_whd = ms_lower_block-s_warehouse_data-zz1_nonconveyable_whd.
    ms_master_carton_aditional-disp_whd_text         = ms_lower_block-s_warehouse_data-disp_whd_text.
    ms_master_carton_aditional-zz1_dirrpl_stt        = ms_lower_block-s_storage_type_data-zz1_dirrpl_stt.
    ms_master_carton_aditional-zz1_keepcar_whd       = ms_lower_block-s_storage_type_data-zz1_keepcar_whd.

    materail_texts_read( ).

    fill_material_texts_tab( EXPORTING is_matlwh         = ls_warehouse_data
                             CHANGING  cs_material_texts = ms_lower_block-s_material_texts ).

    update_maxqty( ).
    read_flowrack_algorithm( ).

    DATA(lt_marm) = master_data-marm.
    DELETE lt_marm WHERE meinh+0(1) <> zif_c_mdm_tool=>c_units-mastercarton+0(1).
    SORT lt_marm BY meinh.
    DATA(lv_pieces) = VALUE #( lt_marm[ 1 ]-umrez OPTIONAL ).

    propose_replenishment_data( EXPORTING is_matlwhst          = VALUE #( me->master_data-storage_type[ 1 ] OPTIONAL )
                                          iv_pieces            = lv_pieces
                                CHANGING  cs_warehouse_data    = ms_lower_block-s_warehouse_data
                                          cs_storage_type_data = ms_lower_block-s_storage_type_data ).
  ENDMETHOD.


  METHOD fill_values_from_product.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : If scanned value is product number
    "*& this section will fill screen values from that number
    "*&
    "********************************************************************
    DATA(ls_material_master) = read_material_master_single( iv_matnr ).
    IF ls_material_master IS INITIAL.
      RETURN.
    ENDIF.
    cs_sub_section_1-matnr             = ls_material_master-matnr.
    cs_sub_section_1-matid             = get_matid_from_matnr( ls_material_master-matnr ).
    cs_sub_section_1-is_product_number = abap_true.

    get_product_details( EXPORTING iv_matid           = cs_sub_section_1-matid
                         IMPORTING es_material_global = DATA(ls_material_global)
                                   es_mat_hazard      = DATA(ls_mat_hazard)
                                   et_mat_mean        = DATA(lt_mat_mean) ).

    master_data-mat_global = ls_material_global.
    master_data-mat_hazard = ls_mat_hazard.

    ASSIGN lt_mat_mean[ 1 ] TO FIELD-SYMBOL(<fs_mean_tab>).
    IF <fs_mean_tab> IS ASSIGNED.
      cs_sub_section_1-ean11 = <fs_mean_tab>-ean11.
    ENDIF.
    cs_sub_section_1-maktx = ls_material_global-maktx.
    cs_sub_section_1-matkl = ls_material_global-matkl.
    cs_sub_section_1-wgbez = read_material_group_text( cs_sub_section_1-matkl ).

    cs_sub_section_1-mtart = ls_material_master-mtart.
    cs_sub_section_1-mtbez = read_material_type_text( cs_sub_section_1-mtart ).

    DATA(ls_matlwh) = read_matlwh_single( iv_matnr = ls_material_master-matnr ).

    DATA lv_tz TYPE ttzz-tzone.

    IF ls_matlwh-createutc IS NOT INITIAL.
      CONVERT TIME STAMP ls_matlwh-createutc TIME ZONE lv_tz
              INTO DATE DATA(lv_date) TIME DATA(lv_time) ##NEEDED.
    ENDIF.

    IF lv_date IS NOT INITIAL.
      cs_sub_section_1-ersda = lv_date.
    ELSE.
      cs_sub_section_1-ersda = ls_material_master-ersda.
    ENDIF.
    cs_sub_section_1-mfrpn      = ls_material_master-mfrpn.
    cs_sub_section_1-mfrnr      = ls_material_master-mfrnr.
    cs_sub_section_1-mhdrz      = ls_material_master-mhdrz.
    cs_sub_section_1-text_mfrnr = read_manufacturer_text( cs_sub_section_1-mfrnr ).
    cs_sub_section_1-prdha      = ls_material_master-prdha.
    IF ls_material_master-sled_bbd = 'E'.
      cs_sub_section_1-sled_bbd = abap_true.
    ENDIF.

    cs_sub_section_1-mhdrz = ls_material_master-mhdrz.
    DATA(ls_valuation_set) = get_values_from_valuation_set( ls_material_master-matnr ).
    cs_sub_section_1-vprsv      = ls_valuation_set-vprsv.
    cs_sub_section_1-verpr      = ls_valuation_set-verpr.
    cs_sub_section_1-stprs      = ls_valuation_set-stprs.
    cs_sub_section_1-peinh      = ls_valuation_set-peinh.

    cs_sub_section_1-text_prdha = get_product_hierarchy_text( iv_product_hierarchy = ls_material_master-prdha ).

    master_data-apo_material = read_apo_material( iv_matnr       = ls_material_master-matnr
                                                  iv_scuguid     = ms_t300_md-scuguid
                                                  iv_entitled_id = CONV #( ms_partner-partner_guid ) ).

    fill_hazardous_mat_features( EXPORTING is_material_master     = ls_material_master
                                           is_material_master_apo = master_data-apo_material
                                 CHANGING  cs_sub_section_2       = ms_top_block-sub_section_2 ).

    master_data-storage_type = read_storage_type_data( iv_matnr       = ls_material_master-matnr
                                                       iv_scuguid     = ms_t300_md-scuguid
                                                       iv_entitled_id = CONV #( ms_partner-partner_guid ) ).

    fill_storage_type_text( EXPORTING it_storage_type             = master_data-storage_type
                            CHANGING  ct_storage_type_description = master_data-storage_type_description ).

    DATA(lt_dangerous) = read_dangerous_goods( ls_material_master-matnr ).

    cs_sub_section_1-tkui   = VALUE #( lt_dangerous[ 1 ]-tkui OPTIONAL ).
    cs_sub_section_1-dgnu   = VALUE #( lt_dangerous[ 1 ]-dgnu OPTIONAL ).
    cs_sub_section_1-pdgnud = VALUE #( lt_dangerous[ 1 ]-pdgnud OPTIONAL ).

    DATA(ls_plant) = get_plant_logsys_by_entitled( ).

    DATA(ls_material_plant) = read_plant_data( iv_material = iv_matnr
                                               iv_plant    = ls_plant-werks ).
    cs_sub_section_1-opti                 = ls_material_plant-zz1_opti_plt.
    cs_sub_section_1-zz1_businessunit_plt = ls_material_plant-zz1_businessunit_plt.
    IF cs_sub_section_1-opti IS NOT INITIAL.
      cs_sub_section_1-opti_descr = get_article_mixture_text( cs_sub_section_1-opti ).
    ENDIF.

    DATA(ls_serial_obligation) = get_serial_obligation( iv_product_hierarchy = ls_material_master-prdha
                                                        iv_manufacturer      = ls_material_master-mfrnr ).

    cs_sub_section_1-zzserial = ls_serial_obligation-zzserial.

    fill_serial_no_id_capturing( EXPORTING is_plant_material = ls_material_plant
                                           iv_sn_obligation  = cs_sub_section_1-zzserial
                                 CHANGING  cs_sub_section_3  = ms_top_block-sub_section_3 ).

    IF cs_sub_section_1-zzserial <> 2 AND ls_material_plant-zz1_identtable01_plt IS INITIAL.
      " Scenario 1- if Prod. Hierachry SN-managed = NO and  Product SN-managed = NO
      cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
    ELSEIF cs_sub_section_1-zzserial = 2 AND ls_material_plant-zz1_identtable01_plt IS INITIAL ##BOOL_OK.
      " Scenario 2- if Prod. Hierachry SN-managed = YES and  Product SN-managed = NO, not available on product
      " Scenario 3- if Prod. Hierachry SN-managed = YES and  Product SN-managed = NO, Master data incorrect, SN available on product
      IF ls_material_plant-zz1_reasoncode1_plt IS NOT INITIAL.
        cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
      ELSE.
        cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-red_flag.
      ENDIF.
    ELSEIF cs_sub_section_1-zzserial = 2 AND ls_material_plant-zz1_identtable01_plt IS NOT INITIAL.
      " Scenario 4- if Prod. Hierachry SN-managed = YES and  Product SN-managed = YES
      cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
    ELSEIF cs_sub_section_1-zzserial <> 2 AND ls_material_plant-zz1_identtable01_plt IS NOT INITIAL.
      " Scenario 5- if Prod. Hierachry SN-managed = NO and  Product SN-managed = YES
      cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-red_flag.
      IF ls_material_plant-zz1_reasoncode1_plt IS NOT INITIAL.
        cs_sub_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
      ENDIF.
    ENDIF.

    " In case there is no record found in the table, we have a new situation. The situation “we don’t give a sh.t”.
    IF ls_serial_obligation IS INITIAL.
      cs_sub_section_1-serial_oblg_icon = ''.
    ENDIF.

    ms_midlle_block-s_packspec_header = get_packspec_header_detail( iv_matnr = ls_material_master-matnr ) ##ENH_OK.

    IF me->ms_midlle_block-s_packspec_header IS NOT INITIAL.
      IF me->ms_midlle_block-s_packspec_header-status = /scwm/cl_ppelipak_cntl=>gc_status_active.
        ms_midlle_block-s_packspec_header-icon = /scwm/cl_ppelipak_cntl=>gc_icon_active.
      ELSE.
        ms_midlle_block-s_packspec_header-icon = /scwm/cl_ppelipak_cntl=>gc_icon_inactive.
      ENDIF.
    ELSE.
      ms_midlle_block-s_packspec_header-icon = /scwm/cl_ppelipak_cntl=>gc_icon_part_act.
    ENDIF.

    DATA(ls_warehouse_data) = read_material_warehouse_data( iv_matnr       = ls_material_master-matnr
                                                            iv_scuguid     = ms_t300_md-scuguid
                                                            iv_entitled_id = CONV #( ms_partner-partner_guid ) ).

    create_warehouse_data( iv_matnr       = ls_material_master-matnr
                           iv_scuguid     = ms_t300_md-scuguid
                           iv_entitled_id = CONV #( ms_partner-partner_guid )
                           is_matlwh      = ls_warehouse_data ).

    update_material_warehouse_data( ls_warehouse_data ).
    cs_sub_section_1-block = ls_warehouse_data-block.
    IF ls_warehouse_data-block IS INITIAL.
      cs_sub_section_1-block = 'B1'.
    ENDIF.

    fill_warehouse_data_tab( EXPORTING is_matlwh         = ls_warehouse_data
                             CHANGING  cs_warehouse_data = ms_lower_block-s_warehouse_data ).

    fill_storage_type_data_tab( EXPORTING is_matlwhst          = VALUE #( me->master_data-storage_type[ 1 ] OPTIONAL )
                                CHANGING  cs_storage_type_data = ms_lower_block-s_storage_type_data ).

    fill_storage_bin_type_sel(
      EXPORTING
        iv_storage_type     = VALUE #( me->master_data-storage_type[ 1 ]-lgtyp OPTIONAL )
      CHANGING
        ct_storage_bin_type = ms_lower_block-t_storage_bin_type_sel ).

    fill_unit_of_measures( EXPORTING is_material_master = ls_material_master
                           CHANGING  ct_middle_block    = ms_midlle_block-t_unit_of_measures ).

    ms_master_carton_aditional-zz1_disp_whd          = ms_lower_block-s_warehouse_data-zz1_disp_whd.
    ms_master_carton_aditional-zz1_nonconveyable_whd = ms_lower_block-s_warehouse_data-zz1_nonconveyable_whd.
    ms_master_carton_aditional-disp_whd_text         = ms_lower_block-s_warehouse_data-disp_whd_text.
    ms_master_carton_aditional-zz1_dirrpl_stt        = ms_lower_block-s_storage_type_data-zz1_dirrpl_stt.
    ms_master_carton_aditional-zz1_keepcar_whd       = ms_lower_block-s_storage_type_data-zz1_keepcar_whd.
    materail_texts_read( ).
    fill_material_texts_tab( EXPORTING is_matlwh         = ls_warehouse_data
                             CHANGING  cs_material_texts = ms_lower_block-s_material_texts ).

    update_maxqty( ).
    read_flowrack_algorithm( ).

    DATA(lt_marm) = master_data-marm.
    DELETE lt_marm WHERE meinh+0(1) <> zif_c_mdm_tool=>c_units-mastercarton+0(1).
    SORT lt_marm BY meinh.
    DATA(lv_pieces) = VALUE #( lt_marm[ 1 ]-umrez OPTIONAL ).

    propose_replenishment_data( EXPORTING is_matlwhst          = VALUE #( me->master_data-storage_type[ 1 ] OPTIONAL )
                                          iv_pieces            = lv_pieces
                                CHANGING  cs_warehouse_data    = ms_lower_block-s_warehouse_data
                                          cs_storage_type_data = ms_lower_block-s_storage_type_data ).
  ENDMETHOD.


  METHOD fill_warehouse_data_tab.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Fill warehouse data tab at the screen
    "*&
    "*&
    "********************************************************************
    cs_warehouse_data-lgnum = ms_top_block-sub_section_1-lgnum.
    cs_warehouse_data-ccind = is_matlwh-ccind.
    IF cs_warehouse_data-ccind IS INITIAL.
      cs_warehouse_data-ccind = 'B'.
    ENDIF.
    cs_warehouse_data-demqty = is_matlwh-demqty.
    IF is_matlwh-demqty IS INITIAL.
      cs_warehouse_data-demqty = 4.
    ENDIF.
    cs_warehouse_data-volind                = is_matlwh-volind.
    cs_warehouse_data-volind_t              = get_volume_indicator_text( is_matlwh-volind ).
    cs_warehouse_data-put_stra              = is_matlwh-put_stra.
    cs_warehouse_data-put_stra_t            = read_putaway_strategy_text( is_matlwh-put_stra ).
    cs_warehouse_data-put_stra_plan         = is_matlwh-put_stra_plan.
    cs_warehouse_data-put_stra_plan_t       = read_putaway_strategy_text( is_matlwh-put_stra_plan ).
    cs_warehouse_data-zz1_nospo_whd         = is_matlwh-zz1_nospo_whd.
    cs_warehouse_data-zz1_noslo_whd         = is_matlwh-zz1_noslo_whd.
    cs_warehouse_data-zz1_disp_whd          = is_matlwh-zz1_disp_whd.
    cs_warehouse_data-zz1_fragile_whd       = is_matlwh-zz1_fragile_whd.
    cs_warehouse_data-zz1_nonconveyable_whd = is_matlwh-zz1_nonconveyable_whd.
    cs_warehouse_data-zz1_odd_whd           = is_matlwh-zz1_odd_whd.

    check_dispatchable_entry( EXPORTING iv_disp_whd    = is_matlwh-zz1_disp_whd
                              CHANGING  cv_description = cs_warehouse_data-disp_whd_text ).
    cs_warehouse_data-disp_whd_text = ms_lower_block-s_warehouse_data-disp_whd_text.
    cs_warehouse_data-zz1_twomh_whd = is_matlwh-zz1_twomh_whd.
    cs_warehouse_data-concstat      = is_matlwh-concstat.
  ENDMETHOD.


  METHOD get_apo_matid.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : GET APO matid from material
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION '/SAPAPO/DM_MATERIAL_GET_MATID'
      EXPORTING
        iv_matnr        = iv_matnr
      IMPORTING
        ev_matid        = rv_matid
      EXCEPTIONS
        matid_not_found = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      " Product not found
      RAISE EXCEPTION NEW zcx_mdm_tool( textid   = zif_c_mdm_tool=>c_product_not_found
                                        mv_msgv1 = CONV #( iv_matnr ) ).
    ENDIF.
  ENDMETHOD.


  METHOD get_article_mixture_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Description of opti field
    "********************************************************************
    SELECT SINGLE description FROM zz1_opti_w
      WHERE code     = @iv_opti
        AND language = @sy-langu
      INTO @rv_desription.
  ENDMETHOD.


  METHOD get_domain_values.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Domain Values
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = iv_domname
        text           = abap_true
        langu          = iv_langu
      TABLES
        dd07v_tab      = rt_fixed_valus
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.
    IF sy-subrc <> 0.
      CALL FUNCTION 'DD_DOMVALUES_GET'
        EXPORTING
          domname        = iv_domname
          text           = abap_true
          langu          = 'E'
        TABLES
          dd07v_tab      = rt_fixed_valus
        EXCEPTIONS
          wrong_textflag = 1
          OTHERS         = 2 ##FM_SUBRC_OK.
    ENDIF.
  ENDMETHOD.


  METHOD get_matid_from_matnr.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get material Id from given product number
    "*&
    "*&
    "********************************************************************
    DATA ls_matid TYPE /scmb/mdl_matid_str.
    DATA ls_matnr TYPE /scmb/mdl_matnr_str.

    ls_matnr-matnr = iv_matnr.
    TRY.
        CALL FUNCTION '/SCMB/MDL_PRODUCT_READ'
          EXPORTING
            iv_appl_component = wmegc_mdl_appl_comp
            is_key            = ls_matnr
          IMPORTING
            es_data           = ls_matid.
      CATCH /scmb/cx_mdl ##NO_HANDLER.
    ENDTRY.
    rv_matid = ls_matid-matid.
  ENDMETHOD.


  METHOD get_mat_details_from_matid.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "*&
    "*&
    "********************************************************************
    IF iv_matid IS INITIAL. RETURN. ENDIF.
    TRY.
        CALL FUNCTION '/SCMB/MDL_PRODUCT_READ'
          EXPORTING
            iv_id   = iv_matid
          IMPORTING
            es_data = rs_material_details.
      CATCH
      /scmb/cx_mdl_interface
      /scmb/cx_mdl_result_empty
      /scmb/cx_mdl_result_toomany
      /scmb/cx_mdl_result ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD get_model_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get data from screen sections
    "********************************************************************
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss1 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection1>).
    IF <ls_subsection1> IS ASSIGNED.
      <ls_subsection1> = ms_top_block-sub_section_1.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss2 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection2>).
    IF <ls_subsection2> IS ASSIGNED.
      <ls_subsection2> = ms_top_block-sub_section_2.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss3 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection3>).
    IF <ls_subsection3> IS ASSIGNED.
      <ls_subsection3> = ms_top_block-sub_section_3.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-middle_ss1 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_middle_uom>).
    IF <ls_middle_uom> IS ASSIGNED.
      <ls_middle_uom> = ms_midlle_block-t_unit_of_measures.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-middle_ss2 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_middle_pack_spec>).
    IF <ls_middle_pack_spec> IS ASSIGNED.
      <ls_middle_pack_spec> = ms_midlle_block-s_packspec_header.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-mc_additional OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_mc_additional>).
    IF <ls_mc_additional> IS ASSIGNED.
      <ls_mc_additional> = ms_master_carton_aditional.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_wrhs OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_warehouse_data>).
    IF <ls_warehouse_data> IS ASSIGNED.
      <ls_warehouse_data> = ms_lower_block-s_warehouse_data.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_strg OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_storage_type_data>).
    IF <ls_storage_type_data> IS ASSIGNED.
      <ls_storage_type_data> = ms_lower_block-s_storage_type_data.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_bin_type OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_bin_type>).
    IF <ls_bin_type> IS ASSIGNED.
      <ls_bin_type> = ms_lower_block-t_storage_bin_type_sel.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_mat_text OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_material_texts>).
    IF <ls_material_texts> IS ASSIGNED.
      <ls_material_texts> = ms_lower_block-s_material_texts.
    ENDIF.
  ENDMETHOD.


  METHOD get_packspec_header_detail.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get pack speck header detail
    "*&
    "*&
    "********************************************************************
    DATA(lt_ps_key) = packspec_read_keys_from_matnr( iv_matnr = iv_matnr ).
    IF lt_ps_key IS NOT INITIAL.
      DATA(lt_packspec) = packspec_read( it_ps_key = lt_ps_key ).
    ENDIF.
    rs_packspec_header = VALUE #( lt_packspec[ 1 ]-header OPTIONAL ).
  ENDMETHOD.


  METHOD get_packspec_level_detail.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Packspec level detail
    "*&
    "*&
    "********************************************************************
    DATA(lt_ps_key) = packspec_read_keys_from_matnr( iv_matnr = iv_matnr ).
    IF lt_ps_key IS NOT INITIAL.
      DATA(lt_packspec) = packspec_read( it_ps_key = lt_ps_key ).
    ENDIF.
    rs_packspec_level = VALUE #( lt_packspec[ 1 ]-levels[ 1 ] OPTIONAL ).
  ENDMETHOD.


  METHOD get_patterns.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Allowed number patterns per ID type
    "*&
    "*&
    "********************************************************************
    IF iv_idtype IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM ztcross_patterns               "#EC CI_NOORDER
      INTO @rs_pattern
      WHERE lgnum   = @ms_top_block-sub_section_1-lgnum
        AND matnr   = @ms_top_block-sub_section_1-matnr
        AND id_type = @iv_idtype ##WARN_OK.
    IF sy-subrc <> 0.
      SELECT SINGLE * FROM ztcross_patterns             "#EC CI_NOORDER
        INTO @rs_pattern
        WHERE lgnum   = @ms_top_block-sub_section_1-lgnum
          AND id_type = @iv_idtype ##WARN_OK.
    ENDIF.
  ENDMETHOD.


  METHOD get_plant_logsys_by_entitled.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :  Get ERP location from entitled
    "*&
    "*&
    "********************************************************************
    DATA lo_ref TYPE REF TO /scwm/if_stockid_mapping.

    CALL FUNCTION '/SCWM/GET_STOCKID_MAP_INSTANCE'
      IMPORTING
        eif_stockid_mapping = lo_ref.
    TRY.
        lo_ref->get_erp_loc_by_entitled( EXPORTING iv_lgnum        = ms_top_block-sub_section_1-lgnum
                                                   iv_entitled     = ms_top_block-sub_section_1-entitled
                                         IMPORTING ev_erp_logsys   = DATA(lv_erp_logsys)
                                                   et_erp_location = DATA(lt_erp_location) ).
      CATCH /scwm/cx_stockid_map ##NO_HANDLER.
    ENDTRY.
    rs_plant_logsys = VALUE #( logsys = lv_erp_logsys
                               werks  = VALUE #( lt_erp_location[ 1 ]-plant OPTIONAL ) ).
  ENDMETHOD.


  METHOD get_product_details.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get product details
    "*&
    "*&
    "********************************************************************
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid      = iv_matid
            iv_lgnum      = ms_top_block-sub_section_1-lgnum
          IMPORTING
            es_mat_global = es_material_global
            es_mat_hazard = es_mat_hazard
            et_mat_mean   = et_mat_mean.
      CATCH /scwm/cx_md ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD get_product_hierarchy_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get product hieararchy texts
    "*&
    "*&
    "********************************************************************
    IF iv_product_hierarchy IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE vtext FROM t179t
      INTO @rv_product_hierarchy_text
      WHERE prodh = @iv_product_hierarchy
        AND spras = @sy-langu.
    IF sy-subrc <> 0 AND sy-langu <> 'E'.

      SELECT SINGLE vtext FROM t179t
        INTO @rv_product_hierarchy_text
        WHERE prodh = @iv_product_hierarchy
          AND spras = 'E'.

    ENDIF.
  ENDMETHOD.


  METHOD get_serial_obligation.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Serial check obligation
    "*&
    "*&
    "********************************************************************
    IF iv_product_hierarchy IS INITIAL AND iv_manufacturer IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM ztcross_serialch
      INTO @rs_serial_obligation
      WHERE entitled = @me->ms_top_block-sub_section_1-entitled
        AND prdha    = @iv_product_hierarchy
        AND mfrnr    = @iv_manufacturer.
    IF sy-subrc <> 0.

      SELECT SINGLE * FROM ztcross_serialch             "#EC CI_NOORDER
        INTO @rs_serial_obligation
        WHERE entitled = @me->ms_top_block-sub_section_1-entitled
          AND prdha    = @iv_product_hierarchy ##WARN_OK.

    ENDIF.

    master_data-serial_check = rs_serial_obligation.
  ENDMETHOD.


  METHOD get_supply_chain_unit.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : get Object for SCU
    "*&
    "*&
    "********************************************************************
    DATA(lo_scu_mgr) = /scmb/cl_scu_system=>so_scu_mgr.

    TRY.
        ro_scu = lo_scu_mgr->get_scu_by_scuguid( iv_scuguid = iv_sc_unit ).
      CATCH /scmb/cx_scu_exception.
        RAISE EXCEPTION TYPE zcx_mdm_tool
              MESSAGE ID sy-msgid
              TYPE sy-msgty
              NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDTRY.
  ENDMETHOD.


  METHOD get_values_from_valuation_set.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Valuation Data for Physical Inventory
    "*&
    "*&
    "********************************************************************
    DATA lv_material18 TYPE char18.
    DATA lt_matid      TYPE /scmb/mdl_matid_tab.

    lv_material18 = iv_material.
    lv_material18 = |{ lv_material18 ALPHA = OUT }|.
    lv_material18 = |{ lv_material18 ALPHA = IN }|.
    TRY.
        " read material-guids without material numbers
        CALL FUNCTION '/SCMB/MDL_PRODUCT_RANGE_GET'
          EXPORTING
            iv_appl_component = wmegc_mdl_appl_comp
            it_ranges         = VALUE rsds_frange_t(
                ( fieldname = 'MATNR'
                  selopt_t  = VALUE rsds_selopt_t(
                                        ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = lv_material18 ) ) ) )
            iv_get_only_id    = abap_true
          IMPORTING
            et_data           = lt_matid.

      CATCH /scmb/cx_mdl ##NO_HANDLER.
    ENDTRY.

    DATA(lv_matid) = VALUE #( lt_matid[ 1 ]-matid OPTIONAL ).
    IF lv_matid IS NOT INITIAL.
      SELECT SINGLE * FROM /scwm/t_valuate
        WHERE lgnum    = @ms_top_block-sub_section_1-lgnum
          AND matid    = @lv_matid
          AND entitled = @ms_top_block-sub_section_1-entitled
        INTO @rs_valuation_set.
    ENDIF.
  ENDMETHOD.


  METHOD get_volume_indicator_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get volume indicator text
    "*&
    "*&
    "********************************************************************
    IF iv_volind IS NOT INITIAL.
      SELECT SINGLE text FROM /scwm/tdimindt
        WHERE langu  = @sy-langu
          AND lgnum  = @me->ms_top_block-sub_section_1-lgnum
          AND dimind = @iv_volind
        INTO @rv_description.
    ENDIF.
  ENDMETHOD.


  METHOD is_ean.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : it checks scanned value is EAN or not
    "*&
    "*&
    "********************************************************************
    DATA lv_ean11    TYPE ean11.
    DATA lt_mean_tab TYPE mean_tab.

    lv_ean11 = iv_scanned_value.
    CALL FUNCTION 'MEAN_GEN_READ_WITH_EAN'
      EXPORTING
        ean11      = lv_ean11
      TABLES
        mean_tab   = lt_mean_tab
      EXCEPTIONS
        wrong_call = 1
        not_found  = 2
        OTHERS     = 3.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    fill_values_from_ean( EXPORTING it_mean_tab      = lt_mean_tab
                          CHANGING  cs_sub_section_1 = cs_sub_section_1 ).
    rv_ean = cs_sub_section_1-is_ean.
  ENDMETHOD.


  METHOD is_manufacturer_part_number.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : it checks scanned value is manufacturer part number or not
    "*&
    "*&
    "********************************************************************
    DATA ls_material TYPE zstr_material_text_data.

    DATA(ls_material_master) = query_manufacturer_part_number( iv_scanned_value = iv_scanned_value ).
    IF ls_material_master IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lt_material_text) = read_material_list_from_mpn( iv_mfrpn = ls_material_master-mfrpn ).

    DATA(lv_line) = lines( lt_material_text ).

    IF lv_line > 1.
      CALL FUNCTION 'Z_MDM_MATERIAL_POPUP'
        EXPORTING
          it_material = lt_material_text
        IMPORTING
          es_material = ls_material.
      IF ls_material IS INITIAL.
        RETURN.
      ENDIF.
    ENDIF.
    IF ls_material IS NOT INITIAL.
      CLEAR ls_material_master.
      ls_material_master = read_material_master_single( iv_matnr = ls_material-matnr ). " Field length 18
    ENDIF.

    IF ls_material_master IS INITIAL.
      RETURN.
    ENDIF.

    fill_values_from_mfrpn( EXPORTING is_material_master = ls_material_master
                            CHANGING  cs_sub_section_1   = cs_sub_section_1 ).
    rv_manufacturer_part_number = cs_sub_section_1-is_mfrpn.
  ENDMETHOD.


  METHOD is_product_number.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : it checks scanned value is product number or not
    "*&
    "*&
    "********************************************************************
    DATA lv_matnr TYPE mara-matnr.

    lv_matnr = |{ iv_scanned_value ALPHA = OUT }|.

    fill_values_from_product( EXPORTING iv_matnr         = lv_matnr
                              CHANGING  cs_sub_section_1 = cs_sub_section_1 ).
    rv_product_number = cs_sub_section_1-is_product_number.
  ENDMETHOD.


  METHOD mara_read.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read all fields of material
    "*&
    "*&
    "********************************************************************
    SELECT SINGLE * FROM mara
      INTO @rs_mara
      WHERE matnr = @iv_matnr.
  ENDMETHOD.


  METHOD materail_texts_read.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Texts fields
    "*&
    "*&
    "********************************************************************
    CLEAR me->master_data-mt_mat_texts.
    SELECT FROM zcross_texts
      FIELDS *
      WHERE lgnum = @me->ms_top_block-sub_section_1-lgnum
      INTO TABLE @me->master_data-mt_mat_texts.
  ENDMETHOD.


  METHOD material_get_matid.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get APO material ID from Material
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION '/SAPAPO/DM_MATERIAL_GET_MATID'
      EXPORTING
        iv_matnr        = iv_matnr
      IMPORTING
        ev_matid        = rv_matid
      EXCEPTIONS
        matid_not_found = 1
        OTHERS          = 2 ##FM_SUBRC_OK.
  ENDMETHOD.


  METHOD packspec_read.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read packspec
    "*&
    "*&
    "********************************************************************
    DATA(ls_read_options) = VALUE /scwm/s_ps_read_options( read_all  = abap_true
                                                           header    = abap_true
                                                           with_text = abap_true ).

    /scwm/cl_ppelipak_cntl=>packspec_read( EXPORTING  it_ps_key       = it_ps_key
                                                      iv_get_active   = VALUE #( )
                                                      it_status_rng   = VALUE #( )
                                           IMPORTING  et_packspec     = rt_packspec
                                           CHANGING   cs_read_options = ls_read_options
                                           EXCEPTIONS read_error      = 1
                                                      OTHERS          = 2 ) ##SUBRC_OK.
  ENDMETHOD.


  METHOD packspec_read_keys_from_matnr.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read packspeck keys from material
    "*&
    "*&
    "********************************************************************
    DATA lt_return TYPE bapirettab ##NEEDED.
    DATA lv_maxrow TYPE int4.

    CALL FUNCTION '/SCWM/API_PACKSPEC_GETLIST'
      EXPORTING
        is_content_query = VALUE /scwm/s_ps_content_query( matnr_rng = VALUE #( ( sign   = wmegc_sign_inclusive
                                                                                  option = wmegc_option_eq
                                                                                  low    = iv_matnr ) ) )
        it_psid_rng      = VALUE /scwm/tt_ps_psid_rtab( )
      IMPORTING
        et_ps_keys       = rt_ps_key
        et_return        = lt_return
      CHANGING
        cv_max_rows      = lv_maxrow.
  ENDMETHOD.


  METHOD packspec_read_pmat.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read packspeck packaging material
    "*&
    "*&
    "********************************************************************
    IF iv_guid_ps IS INITIAL.
      RETURN.
    ENDIF.
    IF iv_aennr IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lo_packspec_model) = NEW /scwm/cl_packspec_model( ).

    CREATE OBJECT lo_packspec_model
      EXPORTING
        iv_guid_ps          = iv_guid_ps
        iv_version          = iv_aennr
        iv_lock             = abap_true
      EXCEPTIONS
        packspec_not_locked = 1
        OTHERS              = 99.
    IF sy-subrc <> 0.
      CREATE OBJECT lo_packspec_model
        EXPORTING
          iv_guid_ps          = iv_guid_ps
          iv_version          = iv_aennr
          iv_lock             = abap_false
        EXCEPTIONS
          packspec_not_locked = 1
          OTHERS              = 99 ##SUBRC_OK.
    ENDIF.
    LOOP AT lo_packspec_model->gt_level INTO DATA(lo_level) ##INTO_OK.
      DATA(lv_matid) = lo_level->go_element_group->gv_main_packmat.
    ENDLOOP.
    rs_mat = get_mat_details_from_matid( lv_matid ).
    IF lo_packspec_model IS BOUND.
      mo_packspeck_model = lo_packspec_model.
    ENDIF.
  ENDMETHOD.


  METHOD partner_read.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Partner in MDL
    "*&
    "*&
    "********************************************************************
    DATA ls_entitled_key TYPE /scmb/mdl_partner_key_str.

    ls_entitled_key-partner = iv_partner.
    TRY.
        CALL FUNCTION '/SCMB/MDL_PARTNER_READ'
          EXPORTING
            is_key  = ls_entitled_key
          IMPORTING
            es_data = rs_mdl_partner.
      CATCH /scmb/cx_mdl.
        RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_party_entitled_dispose ).
        " entitled not maintained as business partner
    ENDTRY.
  ENDMETHOD.


  METHOD propose_max_num_of_bin.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Maximum number of bin proposal
    "*&
    "*&
    "********************************************************************
    IF cs_storage_type_data-lgtyp IS INITIAL OR cs_storage_type_data-bintype IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM ztlptyp_maxqty       "#EC CI_ALL_FIELDS_NEEDED
      INTO @DATA(ls_maxqty)
      WHERE lgnum = @me->ms_top_block-sub_section_1-lgnum
        AND lgtyp = @cs_storage_type_data-lgtyp
        AND lptyp = @cs_storage_type_data-bintype
        AND matid = @me->ms_top_block-sub_section_1-matid.

    DATA(ls_factor) = read_max_num_of_bin_factor( iv_lgnum = ms_top_block-sub_section_1-lgnum
                                                  iv_lgtyp = cs_storage_type_data-lgtyp ).
    IF ls_maxqty-max_qty IS NOT INITIAL.

      DATA(lv_weekly) = CONV /scwm/de_quantity( is_warehouse_data-demqty * ls_factor-tpfactor ).
      IF lv_weekly > ls_maxqty-max_qty.
        DATA(lv_max) = round( val  = ( ( lv_weekly ) / ls_maxqty-max_qty )
                              dec  = 0
                              mode = cl_abap_math=>round_up ).
      ELSE.
        lv_max = 1.
      ENDIF.

      IF lv_max >= 10.
        cs_storage_type_data-zz1_maxput_stt = 9.
      ELSE.
        cs_storage_type_data-zz1_maxput_stt = lv_max.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD propose_replenishment_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : propose replenishment data
    "*&
    "*&
    "********************************************************************

    IF is_matlwhst-lgtyp IS INITIAL.
      RETURN.
    ENDIF.

    IF cs_storage_type_data-bintype IS NOT INITIAL AND cs_storage_type_data-maxqty_dsp IS INITIAL.
      SELECT SINGLE mandt, lgnum, lgtyp, lptyp, matid, max_qty, uom
         FROM ztlptyp_maxqty                            "#EC CI_NOORDER
        INTO @DATA(ls_maxqty)
        WHERE lgnum = @me->ms_top_block-sub_section_1-lgnum
          AND lgtyp = @is_matlwhst-lgtyp
          AND matid = @me->ms_top_block-sub_section_1-matid ##WARN_OK.
      IF sy-subrc = 0.
        cs_storage_type_data-maxqty_dsp = ls_maxqty-max_qty.
      ENDIF.
    ENDIF.

    DATA(ls_flowrack) = VALUE #( me->master_data-flowrack[ lgtyp = is_matlwhst-lgtyp ] OPTIONAL ).
    IF ls_flowrack IS NOT INITIAL.
      IF cs_storage_type_data-minqty_dsp IS INITIAL.
        cs_storage_type_data-minqty_dsp = iv_pieces.
      ENDIF.
      IF cs_storage_type_data-repqty_dsp IS INITIAL.
        cs_storage_type_data-repqty_dsp = iv_pieces.
      ENDIF.
    ELSE.
      DATA(ls_t331) = read_storage_type_control( iv_lgnum = ms_top_block-sub_section_1-lgnum
                                                 iv_lgtyp = is_matlwhst-lgtyp ).
      IF cs_storage_type_data-minqty_dsp IS INITIAL.
        cs_storage_type_data-minqty_dsp = ls_t331-permxqty.
      ENDIF.
      IF cs_storage_type_data-repqty_dsp IS INITIAL.
        cs_storage_type_data-repqty_dsp = 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD putaway_quan_class.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check Quantity Classifications
    "*&
    "*&
    "********************************************************************
    IF iv_quanclaput IS INITIAL.
      RETURN.
    ENDIF.

    SELECT COUNT( * ) FROM /scwm/tquancla
      WHERE quancla = iv_quanclaput  ##WARN_OK.
    IF sy-subrc <> 0.
      MESSAGE e087(/scwm/wm_sel) WITH iv_quanclaput.
    ENDIF.
  ENDMETHOD.


  METHOD query_manufacturer_part_number.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read Material data
    "*&
    "*&
    "********************************************************************
    SELECT SINGLE * FROM mara                           "#EC CI_NOFIELD
      WHERE mfrpn = @iv_scanned_value
      INTO @rs_mara ##WARN_OK.                          "#EC CI_NOORDER
  ENDMETHOD.


  METHOD read_apo_material.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read Warehouse-Dependent Product Data
    "*&
    "*&
    "********************************************************************
    DATA lt_matlwh_id  TYPE /sapapo/dm_matlwh_id_tab.
    DATA lt_matlwh_out TYPE TABLE OF /sapapo/matlwh_out.

    check_apo_material( iv_matnr ).
    DATA(lv_matid) = get_apo_matid( iv_matnr ).

    APPEND VALUE /sapapo/dm_matlwh_id( matid       = lv_matid
                                       scuguid     = iv_scuguid
                                       entitled_id = iv_entitled_id ) TO lt_matlwh_id.

    CALL FUNCTION '/SAPAPO/PROD_LWH_READ'
      EXPORTING
        it_matlwh_id  = lt_matlwh_id
      TABLES
        et_matlwh_out = lt_matlwh_out
      EXCEPTIONS
        not_qualified = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_apo_product_number ).
    ENDIF.
    rs_apo_material = VALUE #( lt_matlwh_out[ 1 ] OPTIONAL ).
  ENDMETHOD.


  METHOD read_dangerous_goods.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read dangerous goods
    "*&
    "*&
    "********************************************************************
    DATA lt_selection_tab       TYPE TABLE OF rdgmdrng.
    DATA lv_flg_changenum_coll  TYPE rcgrhbuf-chcoflg ##NEEDED.
    DATA lv_flg_lockfail        TYPE rcgrhbuf-valflg ##NEEDED.
    DATA ls_further_settings_md TYPE esp5_settings_type.

    lt_selection_tab = VALUE #(
        sign = wmegc_sign_inclusive
        ( option = wmegc_option_cp fieldname = zif_c_mdm_tool=>c_fieldnames-matnr low  = iv_matnr )
        ( option = wmegc_option_cp fieldname = zif_c_mdm_tool=>c_fieldnames-rvlid low  = zif_c_mdm_tool=>c_hazardous-validity_area )
        ( option = wmegc_option_cp fieldname = zif_c_mdm_tool=>c_fieldnames-lwdg  low  = zif_c_mdm_tool=>c_hazardous-validity_area )
        ( option = wmegc_option_ge fieldname = zif_c_mdm_tool=>c_fieldnames-valto low  = sy-datum )
        ( option = wmegc_option_le fieldname = zif_c_mdm_tool=>c_fieldnames-valfr low  = zif_c_mdm_tool=>c_hazardous-end_of_the_calendar ) ).

    ls_further_settings_md = VALUE #( enq_deq_function_name     = zif_c_mdm_tool=>c_hazardous-enq_dec_logic
                                      select_function_name      = zif_c_mdm_tool=>c_hazardous-select_master
                                      flg_return_whole_interval = abap_true
                                      valfr                     = zif_c_mdm_tool=>c_hazardous-start_of_the_calendar
                                      valto                     = zif_c_mdm_tool=>c_hazardous-end_of_the_calendar
                                      flg_no_ch_doc_writing     = abap_true ).

    " -------------------------------------------------------
    " STEP: Read master data / fill IOTAB
    " -------------------------------------------------------
    CALL FUNCTION 'DG10N_DGTMD_LOAD_BUF_AND_IOT'
      EXPORTING
        i_aennr            = VALUE eseaennr( ) " Change number
        i_valdat           = sy-datum
        i_do_locking       = VALUE eseboole( )
        i_further_settings = ls_further_settings_md
      IMPORTING
        e_flg_lockfail     = lv_flg_changenum_coll
        e_flg_chacoll      = lv_flg_lockfail
      TABLES
        e_iotab            = rt_rdgmdiot
        i_selection_tab    = lt_selection_tab
      EXCEPTIONS
        buffer_error       = 1
        no_record_found    = 2
        OTHERS             = 3 ##FM_SUBRC_OK.

    " (EHS 3.2; RK.) We also sort the iotab according to materials and LWDG
    " because that looks better and things are easier to find.
    SORT rt_rdgmdiot BY matnr
                        lwdg
                        valfr.
  ENDMETHOD.


  METHOD read_flowrack_algorithm.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Capacity calculation algorithms for storage types
    "*&
    "*&
    "********************************************************************
    SELECT * FROM ztlgtyp_algo
      INTO TABLE @master_data-flowrack
      WHERE lgnum = @me->ms_top_block-sub_section_1-lgnum.
  ENDMETHOD.


  METHOD read_identtab_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Availiable serial numbers for the material
    "*&
    "*&
    "********************************************************************
    SELECT SINGLE * FROM ztcross_numbers
      INTO @DATA(ls_numbers)
      WHERE lgnum  = @ms_top_block-sub_section_1-lgnum
        AND selnum = @iv_selnum
        AND spras  = @sy-langu.
    IF sy-subrc = 0.
      rv_description = ls_numbers-number_description.
    ENDIF.
  ENDMETHOD.


  METHOD read_location.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : read location and location id for warehouse number
    "*&
    "*&
    "********************************************************************
    "
    CALL FUNCTION '/SCWM/T300_MD_READ_SINGLE'
      EXPORTING
        iv_lgnum   = ms_top_block-sub_section_1-lgnum
      IMPORTING
        es_t300_md = rs_t300_md
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid   = zif_c_mdm_tool=>c_warehouse_number_assignment
                                        mv_msgv1 = CONV #( ms_top_block-sub_section_1-lgnum ) ).
    ENDIF.
  ENDMETHOD.


  METHOD read_manufacturer_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Vendor text
    "*&
    "*&
    "********************************************************************
    DATA ls_lfa1 TYPE lfa1.

    IF iv_mfrnr IS INITIAL.
      RETURN.
    ENDIF.
    CALL FUNCTION 'LFA1_SINGLE_READ'
      EXPORTING
        lfa1_lifnr = iv_mfrnr
      IMPORTING
        wlfa1      = ls_lfa1
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc = 0.
      rv_mfrnr_text = ls_lfa1-name1.
    ENDIF.
  ENDMETHOD.


  METHOD read_material_group_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Material Grup text
    "*&
    "*&
    "********************************************************************
    DATA ls_t023t TYPE t023t.

    IF iv_matkl IS INITIAL.
      RETURN.
    ENDIF.
    CALL FUNCTION 'T023T_SINGLE_READ'
      EXPORTING
        t023t_spras = sy-langu
        t023t_matkl = iv_matkl
      IMPORTING
        wt023t      = ls_t023t
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.
    IF sy-subrc = 0.
      rv_matkl_text = ls_t023t-wgbez.
    ENDIF.
  ENDMETHOD.


  METHOD read_material_list_from_ean.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : There can be one kmore material for a given EAN
    "*& We get list of these materials
    "*&
    "********************************************************************
    DATA(lt_mean) = it_mean_tab.
    SORT lt_mean BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_mean COMPARING matnr.

    SELECT t~*
      FROM makt AS t
             INNER JOIN
               @lt_mean AS m ON  t~matnr = m~matnr ##ITAB_KEY_IN_SELECT
                             AND t~spras = @sy-langu
      INTO TABLE @DATA(lt_makt).

    rt_material_text = VALUE #( BASE rt_material_text FOR ls_makt IN lt_makt
                                ( matnr = ls_makt-matnr maktx = ls_makt-maktx ) ).
  ENDMETHOD.


  METHOD read_material_list_from_mpn.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : There can be one kmore material for a given manufacturer part number
    "*& We get list of these materials
    "*&
    "********************************************************************
    SELECT a~matnr,
           t~maktx
      FROM mara AS a
             INNER JOIN
               makt AS t ON  a~matnr = t~matnr
                         AND t~spras = @sy-langu
      INTO TABLE @DATA(lt_makt)
      WHERE a~mfrpn = @iv_mfrpn.

    SORT lt_makt BY matnr.
    DELETE ADJACENT DUPLICATES FROM lt_makt COMPARING matnr.

    rt_material_text = VALUE #( BASE rt_material_text FOR ls_makt IN lt_makt
                                ( matnr = ls_makt-matnr maktx = ls_makt-maktx ) ).
  ENDMETHOD.


  METHOD read_material_master_single.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read material master single
    "*& the function module locks the materila
    "*&
    "********************************************************************
    DATA lv_matnr    TYPE char18.
    DATA lv_material TYPE mara-matnr.

    lv_matnr = |{ iv_matnr ALPHA = OUT }|.
    lv_matnr = |{ lv_matnr ALPHA = IN }|.
    unlock_material( me->master_data-mara ).
    lv_material = lv_matnr.
    CALL FUNCTION 'MARA_SINGLE_READ'
      EXPORTING
        matnr             = lv_material
        sperrmodus        = zif_c_mdm_tool=>c_lock-exclusive
      IMPORTING
        wmara             = rs_mara
      EXCEPTIONS
        lock_on_material  = 01
        lock_system_error = 02
        not_found         = 03
        OTHERS            = 04.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_mdm_tool
            MESSAGE ID sy-msgid
            TYPE sy-msgty
            NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    master_data-mara = rs_mara.
  ENDMETHOD.


  METHOD read_material_type_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read material type text
    "*&
    "*&
    "********************************************************************
    DATA ls_wt13t TYPE t134t.

    IF iv_mat_type IS INITIAL.
      RETURN.
    ENDIF.
    " -Get Material Type Text
    CALL FUNCTION 'T134T_SINGLE_READ'
      EXPORTING
        materialart = iv_mat_type
        sprache     = sy-langu
      IMPORTING
        wt134t      = ls_wt13t
      EXCEPTIONS
        wrong_call  = 1
        OTHERS      = 2.
    IF sy-subrc = 0.
      rv_mtart_text = ls_wt13t-mtbez.
    ENDIF.
  ENDMETHOD.


  METHOD read_material_warehouse_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read material warehouse data
    "*& this method give same values with
    "*& Warehouse data tab at /SCWM/MAT1 tx
    "********************************************************************
    DATA lt_matlwh_out TYPE TABLE OF /sapapo/matlwh_out.
    DATA ls_matlwh_out LIKE LINE OF lt_matlwh_out.

    DATA lt_matlwh_id  TYPE /sapapo/dm_matlwh_id_tab.
    DATA ls_matlwh_id  LIKE LINE OF lt_matlwh_id.
    DATA ls_t300_md    TYPE /scwm/s_t300_md.
    DATA ls_matlwh     TYPE /scwm/s_matlwh_att.
    DATA lv_func       TYPE funcname.

    DATA lo_scu_mgr    TYPE REF TO /scmb/if_scu_mgr.
    DATA lo_scunit     TYPE REF TO /scmb/if_scu.
    DATA lv_timezone   TYPE tznzone.
    DATA ls_date       TYPE dats.
    DATA ls_time       TYPE tims.

    DATA(lv_matid) = material_get_matid( iv_matnr = iv_matnr ).
    ls_matlwh_id-matid       = lv_matid.
    ls_matlwh_id-scuguid     = iv_scuguid.
    ls_matlwh_id-entitled_id = iv_entitled_id.
    APPEND ls_matlwh_id TO lt_matlwh_id.

    lo_scu_mgr = /scmb/cl_scu_system=>so_scu_mgr.

    CALL FUNCTION '/SAPAPO/PROD_LWH_READ'
      EXPORTING
        it_matlwh_id  = lt_matlwh_id
      TABLES
        et_matlwh_out = lt_matlwh_out
      EXCEPTIONS
        not_qualified = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF. " returned warehouse product table is not initial

    READ TABLE lt_matlwh_out INDEX 1
         INTO ls_matlwh_out.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF. " read warehouse product index 1

    CALL FUNCTION '/SCWM/T300_MD_READ_SINGLE'
      EXPORTING
        iv_scuguid = ls_matlwh_out-scuguid
      IMPORTING
        es_t300_md = ls_t300_md
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF. " T300_MD entry found ?

    MOVE-CORRESPONDING ls_matlwh_out TO rs_matlwh ##ENH_OK.
    lv_timezone = sy-zonlo.
    IF lo_scu_mgr IS NOT INITIAL.
      TRY.
          lo_scunit = lo_scu_mgr->get_scu_by_scuguid( iv_scuguid = iv_scuguid ).
          lv_timezone = lo_scunit->get_timezone( ).
        CATCH /scmb/cx_scu_exception ##NO_HANDLER.
      ENDTRY.
    ENDIF.
    IF lv_timezone <> sy-zonlo.
      CONVERT TIME STAMP rs_matlwh-lastslot
              TIME ZONE lv_timezone
              INTO DATE ls_date
              TIME ls_time.

      lv_timezone = 'UTC'.
      CONVERT DATE ls_date
              TIME ls_time
              INTO TIME STAMP rs_matlwh-lastslot
              TIME ZONE lv_timezone.

      rs_matlwh-lastslot_t = TEXT-500.
    ENDIF.
    rs_matlwh-shelf_life_req_min = ls_matlwh_out-slrq_min.
    " needed for call with Function module which only uses GUID's
    " see //MATERIAL_SHOW_SCC
    rs_matlwh-lgnum              = ls_t300_md-lgnum.

    " in S4 HANA, some special fields of the warehouse product must be retrieved from MARC
    IF abap_true = /scmb/cl_sys_info=>get_instance( )->is_s4h_stack( ).

      " call EWM standard function to get ENTITLED by Plant
      lv_func = '/SCWM/GET_WHSE_PROD_MARC_FLDS'.

      CALL FUNCTION 'FUNCTION_EXISTS'
        EXPORTING
          funcname           = lv_func
        EXCEPTIONS
          function_not_exist = 1
          OTHERS             = 2.

      IF sy-subrc = 0.
        IF rs_matlwh-entitled IS NOT INITIAL.
          CLEAR ls_matlwh.
          MOVE-CORRESPONDING rs_matlwh TO ls_matlwh ##ENH_OK.

          CALL FUNCTION lv_func
            EXPORTING
              iv_entitled = rs_matlwh-entitled
              iv_lgnum    = rs_matlwh-lgnum
              iv_matnr    = iv_matnr
            CHANGING
              cs_matlwh   = ls_matlwh.

          MOVE-CORRESPONDING ls_matlwh TO rs_matlwh ##ENH_OK.
        ENDIF. " entitled is not initial ?
      ENDIF. " function exists ?
    ENDIF. " are we in S4 Hana ?
  ENDMETHOD.


  METHOD read_matlwh_single.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read Location Product for Location Warehouse
    "*&
    "*&
    "********************************************************************
    DATA(lv_matid) = material_get_matid( iv_matnr = iv_matnr ).
    SELECT SINGLE * FROM /sapapo/matlwh
      INTO rs_matlwh
      WHERE matid       = lv_matid
        AND scuguid     = ms_t300_md-scuguid
        AND entitled_id = ms_partner-partner_guid.
  ENDMETHOD.


  METHOD read_max_num_of_bin_factor.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read Demand Indicator to get multiplication factor
    "*&
    "*&
    "********************************************************************

    TRY.
        /scwm/cl_con_db_accesses=>tdemfac_read_single( EXPORTING iv_lgnum   = iv_lgnum
                                                                 iv_lgtyp   = iv_lgtyp
                                                                 iv_demind  = iv_demind
                                                       IMPORTING es_tdemfac = rs_factor ).
      CATCH /scwm/cx_con_no_data ##NO_HANDLER.
        " no action required - table optional
    ENDTRY.
  ENDMETHOD.


  METHOD read_plant_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read Material Plant data
    "*&
    "*&
    "********************************************************************
    DATA lv_matnr TYPE char18.

    lv_matnr = |{ iv_material ALPHA = OUT }|.
    lv_matnr = |{ iv_material ALPHA = IN }|.
    IF lv_matnr IS NOT INITIAL AND iv_plant IS NOT INITIAL.
      SELECT SINGLE * FROM marc
        INTO @rs_material_plant
        WHERE matnr = @lv_matnr
          AND werks = @iv_plant.
    ENDIF.

    master_data-marc = rs_material_plant.
  ENDMETHOD.


  METHOD read_putaway_strategy_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read putaway strategy text
    "*&
    "*&
    "********************************************************************
    DATA ls_t305qt TYPE /scwm/s_t305qt.

    IF iv_put_stra IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR ls_t305qt.

    CALL FUNCTION '/SCWM/T305QT_READ_SINGLE'
      EXPORTING
        iv_lgnum    = ms_top_block-sub_section_1-lgnum
        iv_put_stra = iv_put_stra
      IMPORTING
        es_t305qt   = ls_t305qt
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.
    IF sy-subrc = 0.
      rv_description = ls_t305qt-text.
    ENDIF.
  ENDMETHOD.


  METHOD read_scanned_value.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : It will read scanned value
    "*& this method will control what kind of value has been scanned by user
    "*& Possible Values are Material Number, EAN, Manufacturer Part Number
    "********************************************************************
    IF zcl_mdm_controller=>get_cursor_field( ) <> zif_c_mdm_tool=>c_fieldnames-screen_scanned_value.
      RETURN.
    ENDIF.

    IF me->master_data-mara IS NOT INITIAL.
      unlock_material( is_material_master = master_data-mara ).
    ENDIF.

    clear_sub_section_1( CHANGING cs_sub_section_1 = ms_top_block-sub_section_1 ).

    clear_sub_section_2( CHANGING cs_sub_section_2 = ms_top_block-sub_section_2 ).

    clear_sub_section_3( CHANGING cs_sub_section_3 = ms_top_block-sub_section_3 ).

    clear_middle_block( CHANGING cs_middle_block = ms_midlle_block-t_unit_of_measures ).

    clear_warehouse_data( CHANGING cs_warehouse_data = ms_lower_block-s_warehouse_data ).

    clear_storage_type_data( CHANGING cs_storage_type_data = ms_lower_block-s_storage_type_data ).

    clear_storage_bin_type_sel( CHANGING ct_storage_bin_type_sel = ms_lower_block-t_storage_bin_type_sel ).

    clear_master_data( ).

    ms_top_block-sub_section_1-scanned_value = iv_scanned_value.

    CLEAR: ms_t300_md,
           ms_t300t,
           mo_scu,
           ms_partner,
           mv_is_mc_additional_maintained,
           ms_t340d.

    CLEAR me->master_data.
    IF me->mo_packspeck_model IS BOUND.
      FREE me->mo_packspeck_model.
    ENDIF.

    ms_t300_md = read_location( ).
    ms_t300t   = read_warehouse_number_desc( ).
    mo_scu     = get_supply_chain_unit( ms_t300_md-scuguid ).
    ms_partner = partner_read( me->ms_top_block-sub_section_1-entitled ).
    ms_t340d   = read_warehouse_default_values( iv_lgnum = ms_top_block-sub_section_1-lgnum ).

    DATA(lv_ean) = is_ean( EXPORTING iv_scanned_value = iv_scanned_value
                           CHANGING  cs_sub_section_1 = ms_top_block-sub_section_1 ).
    IF lv_ean IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_manufacturer_part_number) = is_manufacturer_part_number(
      EXPORTING
        iv_scanned_value = iv_scanned_value
      CHANGING
        cs_sub_section_1 = ms_top_block-sub_section_1 ).
    IF lv_manufacturer_part_number IS NOT INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_product_number) = is_product_number( EXPORTING iv_scanned_value = iv_scanned_value
                                                 CHANGING  cs_sub_section_1 = ms_top_block-sub_section_1 ).
    IF lv_product_number IS NOT INITIAL.
      RETURN.
    ENDIF.

    clear_sub_section_1( CHANGING cs_sub_section_1 = ms_top_block-sub_section_1 ).

    RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_invalid_scanned_value ).
  ENDMETHOD.


  METHOD read_storage_bin_type_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get storage bin type text
    "*&
    "*&
    "********************************************************************
    DATA ls_t303t TYPE /scwm/s_t303t.

    IF iv_storage_bin_type IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/T303T_READ_SINGLE'
      EXPORTING
        iv_lgnum  = ms_top_block-sub_section_1-lgnum
        iv_lptyp  = iv_storage_bin_type
      IMPORTING
        es_t303t  = ls_t303t
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc = 0.
      rv_description = ls_t303t-ptypt.
    ENDIF.
  ENDMETHOD.


  METHOD read_storage_section_ind_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get storage section indicator text
    "*&
    "*&
    "********************************************************************
    DATA ls_t304t TYPE /scwm/s_t304t.

    IF iv_storage_section_ind IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/T304T_READ_SINGLE'
      EXPORTING
        iv_lgnum  = ms_top_block-sub_section_1-lgnum
        iv_lgbkz  = iv_storage_section_ind
      IMPORTING
        es_t304t  = ls_t304t
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 0.
      rv_description = ls_t304t-lbkzt.
    ENDIF.
  ENDMETHOD.


  METHOD read_storage_type_control.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read storage type control
    "*&
    "*&
    "********************************************************************
    DATA lt_t331 TYPE /scwm/tt_t331.

    IF iv_lgnum IS INITIAL OR iv_lgtyp IS INITIAL.
      RETURN.
    ENDIF.
    TRY.
        CALL FUNCTION '/SCWM/T331_READ_MULTI'
          EXPORTING
            iv_lgnum = iv_lgnum
            it_lgtyp = VALUE rseloption( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = iv_lgtyp ) )
          IMPORTING
            et_t331  = lt_t331.
        rs_t331 = VALUE #( lt_t331[ 1 ] OPTIONAL ).
      CATCH /scwm/cx_core ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD read_storage_type_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read material storage data
    "*& this method give same values with
    "*& Strorage data tab at /SCWM/MAT1 tx
    "********************************************************************
    DATA(lv_matid) = material_get_matid( iv_matnr = iv_matnr ).

    CALL FUNCTION '/SAPAPO/MATLWHST_READ_SINGLE_2'
      EXPORTING
        iv_matid            = lv_matid
        iv_scuguid          = iv_scuguid
        iv_entitled_id      = iv_entitled_id
*       it_lgtyp            =
      IMPORTING
        et_locprodwhst      = rt_matlwhst
      EXCEPTIONS
        interface_incorrect = 1
        data_not_found      = 2
        OTHERS              = 3.
    IF sy-subrc = 0.
      SORT rt_matlwhst BY lgtyp.
    ENDIF.
  ENDMETHOD.


  METHOD read_storage_type_text.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get storage type texts
    "*&
    "*&
    "********************************************************************
    DATA ls_t301t TYPE /scwm/s_t301t.

    IF iv_storage_type IS INITIAL.
      RETURN.
    ENDIF.
    " read storage type text
    CALL FUNCTION '/SCWM/T301T_READ_SINGLE'
      EXPORTING
        iv_lgnum  = ms_top_block-sub_section_1-lgnum
        iv_lgtyp  = iv_storage_type
      IMPORTING
        es_t301t  = ls_t301t
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc = 0.
      rv_description = ls_t301t-ltypt.
    ENDIF.
  ENDMETHOD.


  METHOD read_store_uom.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Store as MC/ Store as PC/Store as Tote
    "*&
    "*&
    "********************************************************************
    IF is_storage_type_data-lgnum IS INITIAL.
      RETURN.
    ENDIF.
    zcl_crud_ztlgtyp_algo=>select_single_by_lgnum_lgtyp( EXPORTING iv_lgnum        = is_storage_type_data-lgnum                " Warehouse Number/Warehouse Complex
                                                                   iv_lgtyp        = is_storage_type_data-lgtyp                " Storage Type
                                                         IMPORTING es_ztlgtyp_algo = DATA(ls_lgtyp_algo) ).             " Capacity calculation algorithms for storage types

    IF ls_lgtyp_algo-algo = zif_wme_c=>gs_algorithms-cuboid.
      IF is_storage_type_data-zz1_keepcar_whd = abap_true.
        rv_store_uom = TEXT-s01. "'Store as MC'.
      ELSE.
        rv_store_uom = TEXT-s02. "'Store as PC'.
      ENDIF.
    ELSEIF ls_lgtyp_algo-algo = zif_wme_c=>gs_algorithms-flowrack.
      IF    is_storage_type_data-zz1_dirrpl_stt           IS NOT INITIAL
         OR me->ms_master_carton_aditional-zz1_dirrpl_stt IS NOT INITIAL.
        rv_store_uom = TEXT-s01. "'Store as MC'.
      ELSE.
        rv_store_uom = TEXT-s03. "'Store as Tote'
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD read_unit_of_measures_for_mat.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clear screen section 3
    "*&
    "*&
    "********************************************************************
    DATA lv_matnr TYPE char18.

    CLEAR me->master_data-marm.
    lv_matnr = |{ iv_matnr ALPHA = OUT }|.
    lv_matnr = |{ lv_matnr ALPHA = IN }|.
    SELECT * FROM marm
      WHERE matnr = @lv_matnr
      INTO TABLE @rt_marm.
  ENDMETHOD.


  METHOD read_warehouse_default_values.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read defualt warehouse data
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION '/SCWM/T340D_READ_SINGLE'
      EXPORTING
        iv_lgnum  = iv_lgnum
      IMPORTING
        es_t340d  = rs_t340d
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2 ##FM_SUBRC_OK.
    IF sy-subrc <> 0 ##NEEDED.
    ENDIF.
  ENDMETHOD.


  METHOD read_warehouse_number_desc.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : read warehouse number description
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION '/SCWM/T300T_READ_SINGLE'
      EXPORTING
        iv_lgnum  = ms_top_block-sub_section_1-lgnum
        iv_langu  = sy-langu
      IMPORTING
        es_t300t  = rs_t300t
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid   = zif_c_mdm_tool=>c_warehouse_number_assignment
                                        mv_msgv1 = CONV #( ms_top_block-sub_section_1-lgnum ) ).
    ENDIF.
  ENDMETHOD.


  METHOD set_model_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : It is a set method to set values for the screens
    "*&
    "*&
    "********************************************************************
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss1 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection1>).
    IF <ls_subsection1> IS ASSIGNED.
      ms_top_block-sub_section_1 = <ls_subsection1>.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss2 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection2>).
    IF <ls_subsection2> IS ASSIGNED.
      ms_top_block-sub_section_2 = <ls_subsection2>.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss3 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection3>).
    IF <ls_subsection3> IS ASSIGNED.
      ms_top_block-sub_section_3 = <ls_subsection3>.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-middle_ss1 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_middle_uom>).
    IF <ls_middle_uom> IS ASSIGNED.
      ms_midlle_block-t_unit_of_measures = <ls_middle_uom>.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-middle_ss2 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_middle_packspec>).
    IF <ls_middle_packspec> IS ASSIGNED.
      ms_midlle_block-s_packspec_header = <ls_middle_packspec>.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-mc_additional OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_mc_additional>).
    IF <ls_mc_additional> IS ASSIGNED.
      ms_master_carton_aditional = <ls_mc_additional>.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_wrhs OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_warehouse_data>).
    IF <ls_warehouse_data> IS ASSIGNED.
      ms_lower_block-s_warehouse_data = <ls_warehouse_data>.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_strg OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_storage_type_data>).
    IF <ls_storage_type_data> IS ASSIGNED.
      ms_lower_block-s_storage_type_data = <ls_storage_type_data>.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_bin_type OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_bin_type>).
    IF <ls_bin_type> IS ASSIGNED.
      ms_lower_block-t_storage_bin_type_sel = <ls_bin_type>.
    ENDIF.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_mat_text OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_material_texts>).
    IF <ls_material_texts> IS ASSIGNED.
      ms_lower_block-s_material_texts = <ls_material_texts>.
    ENDIF.
  ENDMETHOD.


  METHOD unlock_material.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Unlock the material
    "*&
    "*&
    "********************************************************************
    IF is_material_master-matnr IS INITIAL.
      RETURN.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_EMMARAE'
      EXPORTING
        matnr = is_material_master-matnr ##FM_SUBRC_OK.
    IF sy-subrc <> 0 ##FM_SUBRC_OK.
      RETURN.
    ENDIF.

    DATA lv_matid TYPE mara-scm_matid_guid22.
    SELECT SINGLE scm_matid_guid22 INTO lv_matid FROM mara WHERE matnr = is_material_master-matnr.
    IF sy-subrc = 0.
      CALL FUNCTION 'DEQUEUE_/SAPAPO/EMATKEY'
        EXPORTING
          mode_/sapapo/matkey = zif_c_mdm_tool=>c_lock-exclusive
          mandt               = sy-mandt
          matid               = lv_matid.
    ENDIF.
    CALL FUNCTION 'DEQUEUE_/SAPAPO/E_MATKEY'
      EXPORTING
        mode_/sapapo/matkey_lock = zif_c_mdm_tool=>c_lock-exclusive
        mandt                    = sy-mandt
        matnr                    = is_material_master-matnr.
  ENDMETHOD.


  METHOD update_material_warehouse_data.
    "********************************************************************
    "*& Key          : BSUGAREV-Dec 7, 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "*&
    "*&
    "********************************************************************
    master_data-warehouse_data = is_mat_whse_data.
  ENDMETHOD.


  METHOD update_maxqty.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update max quantity
    "*&
    "*&
    "********************************************************************
    DATA lr_entitled TYPE RANGE OF /scwm/s_lagp_mon_f4-entitled.
    DATA lr_lgtyp    TYPE RANGE OF /scwm/s_lagp_mon_f4-lgtyp.
    DATA lr_lptyp    TYPE RANGE OF /scwm/s_lagp_mon_f4-lptyp.
    DATA lr_matnr    TYPE RANGE OF /scwm/s_lagp_mon_f4-matnr.

    lr_entitled = VALUE #(
        ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = ms_top_block-sub_section_1-entitled ) ).
    lr_matnr    = VALUE #( ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = master_data-mara-matnr ) ).
    IF sy-tcode IS INITIAL.
      sy-tcode = 'ZMDM' ##WRITE_OK. " testing purpose!
    ENDIF.
    SUBMIT zcross_update_maxqty                          "#EC CI_SUBMIT
           WITH p_lgnum EQ ms_top_block-sub_section_1-lgnum
           WITH so_ent   IN lr_entitled
           WITH so_lgtyp IN lr_lgtyp
           WITH so_lptyp IN lr_lptyp
           WITH so_matnr IN lr_matnr
           WITH p_full   EQ abap_true
           WITH p_append EQ abap_false
           WITH p_del    EQ abap_false
           AND RETURN.
  ENDMETHOD.
ENDCLASS.
