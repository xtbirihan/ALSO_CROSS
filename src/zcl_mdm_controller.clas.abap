CLASS zcl_mdm_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA mv_repid TYPE sy-repid .
    DATA mo_model TYPE REF TO zcl_mdm_model .
    DATA mo_view TYPE REF TO zcl_mdm_view .
    DATA mv_check_complete TYPE boolean .
    DATA mo_alv_storage_section TYPE REF TO cl_gui_alv_grid .
    DATA mo_alv_storage_bin_type TYPE REF TO cl_gui_alv_grid .
    DATA mt_screen TYPE t_screen_group .
    DATA:
      mv_is_cursor_set        TYPE c LENGTH 1 .
    DATA:
      mv_inactivate_save      TYPE c LENGTH 1 .
    DATA:
      mv_tab_clicked_tab      TYPE c LENGTH 30 .
    DATA mv_variant_name TYPE rsvar-variant .
    DATA mv_jobname TYPE tbtcjob-jobname .

    METHODS constructor
      IMPORTING
        VALUE(iv_repid) TYPE sy-repid .
    CLASS-METHODS get_cursor_field
      RETURNING
        VALUE(rv_field) TYPE string .
    METHODS set_cursor_field
      IMPORTING
        !is_table_control    TYPE cxtab_control OPTIONAL
        !is_unit_of_measures TYPE zstr_unit_of_measures OPTIONAL
      CHANGING
        !cs_tabstrip         TYPE any OPTIONAL .
    METHODS set_cursor_field_107
      CHANGING
        !cs_tabstrip TYPE any OPTIONAL .
    METHODS at_exit_command .
    METHODS set_pf_status .
    METHODS set_titlebar .
    METHODS get_model_instance .
    METHODS modify_screen_100 .
    METHODS modify_screen_101 .
    METHODS modify_screen_102 .
    METHODS modify_screen_103 .
    METHODS modify_screen_104
      IMPORTING
        !is_unit_of_measures TYPE zstr_unit_of_measures OPTIONAL
      CHANGING
        !cs_table_control    TYPE cxtab_control .
    METHODS modify_screen_104_pack_spec .
    METHODS modify_screen_104_buttons .
    METHODS modify_screen_105 .
    METHODS modify_screen_106 .
    METHODS modify_screen_107 .
    METHODS clear_sections
      CHANGING
        !cs_screen TYPE any .
    METHODS entitled_value_help .
    METHODS check_ean11
      IMPORTING
        !is_middle_block TYPE zstr_unit_of_measures .
    METHODS check_pack_material
      CHANGING
        !cs_middle_block TYPE zstr_unit_of_measures .
    METHODS check_quantities
      CHANGING
        !cs_middle_block TYPE zstr_unit_of_measures .
    METHODS get_parameter_from_caller
      CHANGING
        VALUE(cv_scanned_value) TYPE zde_scanned_value OPTIONAL .
    METHODS create_packspec
      IMPORTING
        !iv_save     TYPE syst-ucomm OPTIONAL
        !iv_ps_group TYPE /scwm/de_ps_group DEFAULT 'PG01'
      CHANGING
        !cs_screen   TYPE zstr_packspec_header OPTIONAL .
    METHODS modify_unit_of_measures
      IMPORTING
        !is_unit_of_measures TYPE zstr_unit_of_measures
        !iv_current_line     TYPE i
      CHANGING
        !ct_unit_of_measures TYPE ztt_unit_of_measueres .
    METHODS calculate_net_weight
      IMPORTING
        !it_unit_of_measures TYPE ztt_unit_of_measueres
        !iv_current_line     TYPE i
      CHANGING
        !cs_unit_of_measures TYPE zstr_unit_of_measures .
    METHODS calculate_pallet
      IMPORTING
        !it_unit_of_measures TYPE ztt_unit_of_measueres
        !iv_current_line     TYPE i
      CHANGING
        !cs_unit_of_measures TYPE zstr_unit_of_measures .
    METHODS calculate_weight_volume
      IMPORTING
        !it_unit_of_measures TYPE ztt_unit_of_measueres
      EXPORTING
        !es_unitsofmeasure   TYPE zstr_unit_of_measure .
    METHODS master_carton_additional
      IMPORTING
        !iv_matid                   TYPE /scwm/de_matid
      CHANGING
        !cs_warehouse_data          TYPE zstr_warehouse_data OPTIONAL
        !cs_master_carton_aditional TYPE zstr_additional_data OPTIONAL
        !cs_storage_type_data       TYPE zstr_storage_type_data OPTIONAL
        !ct_unit_of_measures        TYPE ztt_unit_of_measueres OPTIONAL .
    METHODS update_product_fields .
    METHODS update_units_of_measure
      RETURNING
        VALUE(rs_return) TYPE bapiret2 .
    METHODS dangerous_goods_value_help .
    METHODS value_request_dispatchable
      CHANGING
        !cv_disp_whd TYPE zz1_disp .
    METHODS trigger_enter .
    METHODS set_fixed_slotting_values .
    METHODS update_daily_volume
      IMPORTING
        !iv_skip_popup      TYPE abap_bool OPTIONAL
        !iv_demand_quantity TYPE /scwm/de_demqty
      RETURNING
        VALUE(rv_answer)    TYPE char1 .
    METHODS check_packspeck_data
      RETURNING
        VALUE(rv_is_pack_values_ok) TYPE char1 .
    METHODS run_slotting
      IMPORTING
        !iv_savmod                TYPE /scwm/de_savmod
      CHANGING
        !cs_warehouse_data        TYPE zstr_warehouse_data OPTIONAL
        !cs_storage_type_data     TYPE zstr_storage_type_data OPTIONAL
        !cs_storage_type_bin_type TYPE ztt_storage_type_bin_type_sel OPTIONAL .
    METHODS start_concepting
      IMPORTING
        !is_slot_steps           TYPE /scwm/s_slot_steps
        !it_slot_key             TYPE /scwm/tt_slot_key
      EXPORTING
        !et_mat_lgnum_result_sel TYPE /scwm/tt_material_lgnum
        !et_mat_lgtyp_result_sel TYPE /scwm/tt_material_lgtyp
        !et_mat_lgtyp_orig_sel   TYPE /scwm/tt_material_lgtyp
        !et_binmat_result_sel    TYPE /scwm/tt_binmat .
    METHODS create_storage_section_alv .
    METHODS create_storage_bin_type_alv .
    METHODS check_obligatory_fields .
    METHODS f4_with_customized_params
      IMPORTING
        VALUE(iv_display_only) TYPE abap_bool DEFAULT space
        VALUE(iv_max_records)  TYPE i DEFAULT space
        VALUE(iv_shlp_name)    TYPE shlpname
        VALUE(it_ddshifaces)   TYPE ddshifaces
      EXPORTING
        !et_values             TYPE tfw_ddshretval_tab .
    METHODS handle_template_reason_code
      CHANGING
        !cs_sub_section_3 TYPE zstr_ident_numbers .
    METHODS save
      CHANGING
        !cs_screen TYPE any .
    METHODS selected_storage_type
      CHANGING
        !cs_storage_type_data    TYPE zstr_storage_type_data
        !ct_storage_bin_type_sel TYPE ztt_storage_type_bin_type_sel .
    METHODS storage_type_value_help
      CHANGING
        !cv_lgtyp TYPE /scwm/lgtyp .
    METHODS storage_section_value_help
      CHANGING
        !cv_sectind TYPE /scwm/de_sectind .
    METHODS bintype_value_help
      CHANGING
        !cv_bintype TYPE /scwm/de_bintype .
    METHODS volume_ind_value_help
      CHANGING
        !cv_volind TYPE /scwm/de_volind .
    METHODS put_stra_value_help
      CHANGING
        !cv_put_stra TYPE /scwm/de_put_stra .
    METHODS inb_deco_text_value_help
      CHANGING
        !cv_text_id TYPE zde_picking_text .
    METHODS picking_text_value_help
      CHANGING
        !cv_text_id TYPE zde_picking_text .
    METHODS kep_picking_text_value_help
      CHANGING
        !cv_text_id TYPE zde_picking_text .
    METHODS sped_picking_text_value_help
      CHANGING
        !cv_text_id TYPE zde_picking_text .
    METHODS packing_text_value_help
      CHANGING
        !cv_text_id TYPE zde_picking_text .
    METHODS indenttab01_value_help
      CHANGING
        !cv_identab01 TYPE zde_indenttab_01 .
    METHODS reasoncode01_value_help
      CHANGING
        !cv_reasoncode01 TYPE zde_reason_code .
    METHODS bulk_storage_value_help
      CHANGING
        !cv_bulk TYPE /scwm/lvs_block2 .
    METHODS pmat_value_help
      CHANGING
        !cv_pmat TYPE /scwm/de_pmat .
    METHODS create_job_for_slotting
      IMPORTING
        !is_product_characteristics TYPE zstr_product_characteristics .
    METHODS update_maxqty .
    METHODS copy_pc_to_mc
      CHANGING
        !ct_unit_of_measures TYPE ztt_unit_of_measueres .
    METHODS more_template
      IMPORTING
        !iv_ucomm        TYPE syst-ucomm
      CHANGING
        !cs_identmnumber TYPE zstr_ident_numbers .
    METHODS set_template_icon
      CHANGING
        !cs_sub_section_3 TYPE zstr_ident_numbers .
    METHODS update_reason_code
      CHANGING
        !cs_section_1 TYPE zstr_product_characteristics
        !cs_section_3 TYPE zstr_ident_numbers .
    METHODS update_bulk_storage
      CHANGING
        !cs_section_1 TYPE zstr_product_characteristics .
    METHODS update_indenttab
      IMPORTING
        !iv_identnum  TYPE char2
      CHANGING
        !cs_section_3 TYPE zstr_ident_numbers .
    METHODS run_update_daily_volume_job
      CHANGING
        !cs_warehouse_data TYPE zstr_warehouse_data .
    METHODS bapi_transaction_rollback .
    METHODS bapi_transaction_commit .
    METHODS check_obligatory_fields_uom
      IMPORTING
        !is_table_control     TYPE cxtab_control
        !is_unit_of_measures  TYPE zstr_unit_of_measures
      RETURNING
        VALUE(rv_screen_name) TYPE stringval .
    METHODS check_obligatory_fields_whsd
      RETURNING
        VALUE(rv_screen_name) TYPE stringval .
    METHODS check_obligatory_fields_styp
      RETURNING
        VALUE(rv_screen_name) TYPE stringval .
    METHODS check_obligatory_fields_ident
      RETURNING
        VALUE(rv_screen_name) TYPE stringval .
  PRIVATE SECTION.
    METHODS set_functioncode
      IMPORTING iv_function_code TYPE sy-ucomm.

    METHODS raise_exception_from_sy.

    METHODS read_matid_from_product
      IMPORTING is_matnr        TYPE /scmb/mdl_matnr_str
      RETURNING VALUE(rs_matid) TYPE /scmb/mdl_matid_str.

    METHODS read_product_from_matid
      IMPORTING is_matid          TYPE /scmb/mdl_matid_str
      RETURNING VALUE(rs_product) TYPE /scmb/mdl_product ##RELAX.

    METHODS fill_str
      IMPORTING iv_tabname TYPE  tabname
                iv_file    TYPE  data
      EXPORTING ev_str     TYPE  data.

    METHODS fill_xstr
      IMPORTING iv_tabname TYPE  tabname
                iv_str     TYPE  data
      EXPORTING ev_xstr    TYPE  data.

    METHODS convert_volume
      CHANGING cs_unitsofmeasure TYPE bapi_marm.

    METHODS get_variant_values
      IMPORTING iv_report         TYPE  rsvar-report
                iv_variant        TYPE  rsvar-variant
      RETURNING VALUE(rt_valutab) TYPE rsparams_tt.

    METHODS initialize_alv
      IMPORTING iv_container_name TYPE c
      EXPORTING eo_container      TYPE REF TO cl_gui_custom_container
                eo_alv            TYPE REF TO cl_gui_alv_grid.

    METHODS create_fieldcat
      IMPORTING iv_structure_name  TYPE  dd02l-tabname
      RETURNING VALUE(rt_fieldcat) TYPE lvc_t_fcat.

    METHODS check_mastercarton_fields.

    METHODS create_variant
      IMPORTING iv_report_name    TYPE rsvar-report
                VALUE(it_valutab) TYPE rsparams_tt.

    METHODS variant_delete
      IMPORTING iv_report_name TYPE rsvar-report
                iv_variant     TYPE rsvar-variant.

    METHODS delete_existing_variants
      IMPORTING iv_report_name TYPE rsvar-report.

    METHODS job_open
      EXPORTING VALUE(ev_jobcount) TYPE tbtcjob-jobcount ##RELAX.

    METHODS job_submit
      IMPORTING VALUE(iv_jobcount) TYPE tbtcjob-jobcount ##RELAX.

    METHODS job_close
      IMPORTING VALUE(iv_jobcount) TYPE tbtcjob-jobcount ##RELAX.

ENDCLASS.



CLASS ZCL_MDM_CONTROLLER IMPLEMENTATION.


  METHOD at_exit_command.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Leave screen
    "*&
    "*&
    "********************************************************************
    LEAVE TO SCREEN 0.
  ENDMETHOD.


  METHOD bapi_transaction_commit.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Commit
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDMETHOD.


  METHOD bapi_transaction_rollback.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Rollback
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDMETHOD.


  METHOD bintype_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for bin type
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lptyp valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value     = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-bintype
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_bintype = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD bulk_storage_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for bulk storage
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-block valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value     = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-bulk_storage
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_bulk = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD calculate_net_weight.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Calculate net weight
    "*&
    "*&
    "********************************************************************
    IF iv_current_line = 2. " ne 1 .
      DATA(ls_unit_of_measures) = it_unit_of_measures[ 1 ].
      cs_unit_of_measures-ntgew = ls_unit_of_measures-ntgew * cs_unit_of_measures-umrez.
      cs_unit_of_measures-brgew = ls_unit_of_measures-brgew * cs_unit_of_measures-umrez.
    ENDIF.

    READ TABLE mo_model->ms_midlle_block-t_unit_of_measures
         INTO DATA(ls_middle_block)
         WITH KEY id = cs_unit_of_measures-id.
    IF ls_middle_block-pmat = cs_unit_of_measures-pmat.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD calculate_pallet.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Calculation for pallet
    "*&
    "*&
    "********************************************************************
    DATA ls_matnr       TYPE /scmb/mdl_matnr_str.
    DATA lv_material_18 TYPE char18.

    IF iv_current_line <> 3.
      RETURN.
    ENDIF.

    " Packaging material volume conversion
    DATA(lo_conv) = /scmb/cl_md_access_mdl=>get_md_access( ).

    READ TABLE it_unit_of_measures
         INTO DATA(ls_piece_line)
         WITH KEY id = zif_c_mdm_tool=>c_units-piece.

    READ TABLE it_unit_of_measures
         INTO DATA(ls_mc_tote_line)
         WITH KEY id = zif_c_mdm_tool=>c_units-mastercarton.

    IF cs_unit_of_measures-umrez IS NOT INITIAL AND ls_mc_tote_line-umrez IS NOT INITIAL.
      DATA(lv_div) = CONV int8( cs_unit_of_measures-umrez DIV ls_mc_tote_line-umrez ).
      DATA(lv_mod) = CONV int8( cs_unit_of_measures-umrez MOD ls_mc_tote_line-umrez ).
    ENDIF.

    IF cs_unit_of_measures-pmat IS NOT INITIAL.
      lv_material_18 = |{ cs_unit_of_measures-pmat ALPHA = IN }|.
      ls_matnr-matnr = lv_material_18.
      DATA(ls_matid) = read_matid_from_product( is_matnr = ls_matnr ).
      DATA(ls_material_details) = mo_model->get_mat_details_from_matid( iv_matid = ls_matid-matid ).
      DATA(ls_pmat_uom) = VALUE #( ls_material_details-uom[ 1 ] OPTIONAL ).

      IF ls_pmat_uom-gewei IS NOT INITIAL.
        TRY.
            DATA(lv_conv_qty_brgew) = lo_conv->prod_quan_conversion( iv_prodid   = VALUE #( )
                                                                     iv_uom_from = ls_pmat_uom-gewei
                                                                     iv_uom_to   = zif_c_mdm_tool=>c_units-kg
                                                                     iv_quan     = CONV #( ls_pmat_uom-brgew ) ).
          CATCH /scmb/cx_md_access ##NO_HANDLER.
        ENDTRY.

        TRY.
            DATA(lv_conv_qty_ntgew) = lo_conv->prod_quan_conversion( iv_prodid   = VALUE #( )
                                                                     iv_uom_from = ls_pmat_uom-gewei
                                                                     iv_uom_to   = zif_c_mdm_tool=>c_units-kg
                                                                     iv_quan     = CONV #( ls_pmat_uom-ntgew ) ).
          CATCH /scmb/cx_md_access ##NO_HANDLER.
        ENDTRY.
      ENDIF.

    ENDIF.

    cs_unit_of_measures-ntgew = lv_div * ls_mc_tote_line-brgew + lv_mod * ls_piece_line-brgew + lv_conv_qty_ntgew.
    cs_unit_of_measures-brgew = cs_unit_of_measures-ntgew + lv_conv_qty_brgew.
  ENDMETHOD.


  METHOD calculate_weight_volume.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Weight volume calculation
    "*&
    "*&
    "********************************************************************
    DATA ls_unitsofmeasure_piece   TYPE bapi_marm.
    DATA ls_unitsofmeasure_mc_tote TYPE bapi_marm.
    DATA ls_unitsofmeasure_pallet  TYPE bapi_marm.

    DATA ls_matnr                  TYPE /scmb/mdl_matnr_str.
    DATA lv_material_18            TYPE char18.

    DATA(lo_conv) = /scmb/cl_md_access_mdl=>get_md_access( ).

    READ TABLE me->mo_model->ms_midlle_block-t_unit_of_measures INTO DATA(ls_piece_line)  WITH KEY id = zif_c_mdm_tool=>c_units-piece.
    READ TABLE me->mo_model->ms_midlle_block-t_unit_of_measures INTO DATA(ls_mc_tote_line)  WITH KEY id = zif_c_mdm_tool=>c_units-mastercarton.
    READ TABLE me->mo_model->ms_midlle_block-t_unit_of_measures INTO DATA(ls_palet_line) WITH KEY id = zif_c_mdm_tool=>c_units-palet.

    IF ls_palet_line-umrez IS NOT INITIAL AND ls_mc_tote_line-umrez IS NOT INITIAL.
      DATA(lv_div) = CONV int8( ls_palet_line-umrez DIV ls_mc_tote_line-umrez ).
      DATA(lv_mod) = CONV int8( ls_palet_line-umrez MOD ls_mc_tote_line-umrez ).
    ENDIF.

    IF ls_palet_line-pmat IS NOT INITIAL.
      lv_material_18 = |{ ls_palet_line-pmat ALPHA = IN }|.
      ls_matnr-matnr = lv_material_18.
      DATA(ls_matid) = read_matid_from_product( is_matnr = ls_matnr ).
      DATA(ls_material_details) = mo_model->get_mat_details_from_matid( iv_matid = ls_matid-matid ).
      DATA(ls_pmat_uom) = VALUE #( ls_material_details-uom[ 1 ] OPTIONAL ).

      IF ls_pmat_uom-gewei IS NOT INITIAL.
        TRY.
            DATA(lv_conv_qty_brgew) = lo_conv->prod_quan_conversion( iv_prodid   = VALUE #( )
                                                                     iv_uom_from = ls_pmat_uom-gewei
                                                                     iv_uom_to   = zif_c_mdm_tool=>c_units-kg
                                                                     iv_quan     = CONV #( ls_pmat_uom-brgew ) ).
          CATCH /scmb/cx_md_access ##NO_HANDLER.
        ENDTRY.

        TRY.
            DATA(lv_conv_qty_ntgew) = lo_conv->prod_quan_conversion( iv_prodid   = VALUE #( )
                                                                     iv_uom_from = ls_pmat_uom-gewei
                                                                     iv_uom_to   = zif_c_mdm_tool=>c_units-kg
                                                                     iv_quan     = CONV #( ls_pmat_uom-ntgew ) ).
          CATCH /scmb/cx_md_access ##NO_HANDLER.
        ENDTRY.
      ENDIF.

      " Convert Packaging Material Volume
      IF ls_pmat_uom-meabm IS NOT INITIAL.
        ls_unitsofmeasure_pallet-volumeunit     = mo_model->ms_t340d-voleh.
        ls_unitsofmeasure_pallet-volumeunit_iso = mo_model->ms_t340d-voleh.
        ls_unitsofmeasure_pallet-unit_dim       = COND #( WHEN ls_palet_line-meabm IS NOT INITIAL
                                                          THEN ls_palet_line-meabm
                                                          ELSE zif_c_mdm_tool=>c_units-mm ).
        ls_unitsofmeasure_pallet-unit_dim_iso   = COND #( WHEN ls_palet_line-meabm IS NOT INITIAL
                                                          THEN ls_palet_line-meabm
                                                          ELSE zif_c_mdm_tool=>c_units-mm ).
        ls_unitsofmeasure_pallet-length         = ls_palet_line-laeng.
        ls_unitsofmeasure_pallet-width          = ls_palet_line-breit.
        ls_unitsofmeasure_pallet-height         = ls_palet_line-hoehe.
        convert_volume( CHANGING cs_unitsofmeasure = ls_unitsofmeasure_pallet ).
      ENDIF.

    ENDIF.

    " Convert Piece Line Volume
    ls_unitsofmeasure_piece-volumeunit     = mo_model->ms_t340d-voleh.
    ls_unitsofmeasure_piece-volumeunit_iso = mo_model->ms_t340d-voleh.
    ls_unitsofmeasure_piece-unit_dim       = COND #( WHEN ls_piece_line-meabm IS NOT INITIAL
                                                     THEN ls_piece_line-meabm
                                                     ELSE zif_c_mdm_tool=>c_units-mm ).
    ls_unitsofmeasure_piece-unit_dim_iso   = COND #( WHEN ls_piece_line-meabm IS NOT INITIAL
                                                     THEN ls_piece_line-meabm
                                                     ELSE zif_c_mdm_tool=>c_units-mm ).
    ls_unitsofmeasure_piece-length         = ls_piece_line-laeng.
    ls_unitsofmeasure_piece-width          = ls_piece_line-breit.
    ls_unitsofmeasure_piece-height         = ls_piece_line-hoehe.
    convert_volume( CHANGING cs_unitsofmeasure = ls_unitsofmeasure_piece ).

    " Convert Master Carton Tote Line Volume
    ls_unitsofmeasure_mc_tote-volumeunit     = mo_model->ms_t340d-voleh.
    ls_unitsofmeasure_mc_tote-volumeunit_iso = mo_model->ms_t340d-voleh.
    ls_unitsofmeasure_mc_tote-unit_dim       = COND #( WHEN ls_mc_tote_line-meabm IS NOT INITIAL
                                                       THEN ls_mc_tote_line-meabm
                                                       ELSE zif_c_mdm_tool=>c_units-mm ).
    ls_unitsofmeasure_mc_tote-unit_dim_iso   = COND #( WHEN ls_mc_tote_line-meabm IS NOT INITIAL
                                                       THEN ls_mc_tote_line-meabm
                                                       ELSE zif_c_mdm_tool=>c_units-mm ).
    ls_unitsofmeasure_mc_tote-length         = ls_mc_tote_line-laeng.
    ls_unitsofmeasure_mc_tote-width          = ls_mc_tote_line-breit.
    ls_unitsofmeasure_mc_tote-height         = ls_mc_tote_line-hoehe.
    convert_volume( CHANGING cs_unitsofmeasure = ls_unitsofmeasure_mc_tote ).

    es_unitsofmeasure-net_wt         = lv_div * ls_mc_tote_line-brgew + lv_mod * ls_piece_line-brgew + lv_conv_qty_ntgew.
    es_unitsofmeasure-gross_wt       = es_unitsofmeasure-net_wt + lv_conv_qty_brgew.
    es_unitsofmeasure-unit_of_wt     = zif_c_mdm_tool=>c_units-kg.
    es_unitsofmeasure-unit_of_wt_iso = zif_c_mdm_tool=>c_units-kg.

    es_unitsofmeasure-unit_dim       = zif_c_mdm_tool=>c_units-mm.
    es_unitsofmeasure-unit_dim_iso   = zif_c_mdm_tool=>c_units-mm.

    es_unitsofmeasure-volume         = lv_div * ls_unitsofmeasure_mc_tote-volume + lv_mod * ls_unitsofmeasure_piece-volume + ls_unitsofmeasure_pallet-volume.
    es_unitsofmeasure-volumeunit     = mo_model->ms_t340d-voleh.
    es_unitsofmeasure-volumeunit_iso = mo_model->ms_t340d-voleh.
  ENDMETHOD.


  METHOD check_ean11.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check EAN number value
    "*&
    "*&
    "********************************************************************
    READ TABLE mo_model->ms_midlle_block-t_unit_of_measures
         INTO DATA(ls_middle_block)
         WITH KEY id = is_middle_block-id.
    IF ls_middle_block-ean11 = is_middle_block-ean11.
      RETURN.
    ENDIF.

    READ TABLE mo_model->master_data-marm
         INTO DATA(ls_marm) WITH KEY matnr = mo_model->master_data-mara-matnr
                                     meinh = is_middle_block-id.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    DATA lt_marm_itab TYPE STANDARD TABLE OF marm.
    DATA lt_mean_tab  TYPE STANDARD TABLE OF meani.
    DATA lt_me_tab    TYPE STANDARD TABLE OF meanu.
    DATA lt_y_dmean   TYPE STANDARD TABLE OF sxdmean.

    CALL FUNCTION 'EAN_SYSTEMATIC'
      EXPORTING
        ean_neu    = is_middle_block-ean11
        etyp_neu   = ls_marm-numtp
        p_herkunft = 'M'
      TABLES
        marm_itab  = lt_marm_itab
        mean_tab   = lt_mean_tab
        me_tab     = lt_me_tab
        y_dmean    = lt_y_dmean
      EXCEPTIONS
        ean_error  = 1
        OTHERS     = 2.

    IF sy-subrc <> 0.
      raise_exception_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD check_mastercarton_fields.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check master carton fields
    "*&
    "*&
    "********************************************************************
    DATA(ls_carton) = mo_model->ms_midlle_block-t_unit_of_measures[ id = zif_c_mdm_tool=>c_units-mastercarton ].
    IF    ls_carton-laeng IS INITIAL
       OR ls_carton-breit IS INITIAL
       OR ls_carton-hoehe IS INITIAL
       OR ls_carton-brgew IS INITIAL
       OR ls_carton-ntgew IS INITIAL.

      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_empty_master_carton_field ).
    ENDIF.
  ENDMETHOD.


  METHOD check_obligatory_fields.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check obligatory fields
    "*&
    "*&
    "********************************************************************
    MESSAGE e018(zmc_mdm_tool) INTO DATA(lv_dummy) ##NEEDED. " Fill out all required entry fields.
    raise_exception_from_sy( ).
  ENDMETHOD.


  METHOD check_obligatory_fields_ident.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check mandatory fields
    "*&
    "*&
    "********************************************************************
    LOOP AT me->mt_screen INTO DATA(ls_screen) WHERE       name+0(6) = 'GS_SCR'
                                                     AND ( required  = 2 ) ##INTO_OK.

      DATA(lv_string) = ls_screen-name.
      DO.
        lv_string = substring_after( val = lv_string
                                     sub = '-' ).
        IF lv_string IS INITIAL.
          EXIT.
        ENDIF.
        DATA(lv_name) = lv_string.
      ENDDO.

      ASSIGN COMPONENT lv_name OF STRUCTURE me->mo_model->ms_top_block-sub_section_3 TO FIELD-SYMBOL(<lv_screen_value>).

      IF <lv_screen_value> IS ASSIGNED.
        IF <lv_screen_value> IS INITIAL.
          rv_screen_name = ls_screen-name.
          EXIT.
        ENDIF.
        UNASSIGN <lv_screen_value>.
      ENDIF.
      CLEAR: lv_name,
             lv_string.

    ENDLOOP.
  ENDMETHOD.


  METHOD check_obligatory_fields_styp.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check mandatory fields storage type tab
    "*&
    "*&
    "********************************************************************
    IF me->mo_model->ms_lower_block-s_storage_type_data-lgtyp IS INITIAL.
      rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_lgtyp.
      RETURN.
    ENDIF.

    IF me->mo_model->ms_lower_block-s_storage_type_data-sectind IS INITIAL.
      rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_sectind.
      RETURN.
    ENDIF.

    IF me->mo_model->ms_lower_block-s_storage_type_data-bintype IS INITIAL.
      rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_bintype.
      RETURN.
    ENDIF.

    IF me->mo_model->ms_lower_block-s_storage_type_data-repqty_dsp IS INITIAL.
      rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_repqty_dsp.
      RETURN.
    ENDIF.

    IF me->mo_model->ms_lower_block-s_storage_type_data-minqty_dsp IS INITIAL.
      rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_minqty_dsp.
      RETURN.
    ENDIF.

    IF me->mo_model->ms_lower_block-s_storage_type_data-maxqty_dsp IS INITIAL.
      rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_maxqty_dsp.
      RETURN.
    ENDIF.

    IF me->mo_model->ms_lower_block-s_storage_type_data-zz1_dirrpl_stt IS INITIAL.
      IF me->mo_model->ms_lower_block-s_storage_type_data-tote_quan IS INITIAL.
        rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_tote_quan.
        RETURN.
      ENDIF.
    ENDIF.

    IF me->mo_model->ms_lower_block-s_storage_type_data-zz1_maxput_stt IS INITIAL.
      rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_zz1_maxput_stt.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD check_obligatory_fields_uom.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check mandatory fields UOM tab
    "*&
    "*&
    "********************************************************************
    LOOP AT me->mo_model->ms_midlle_block-t_unit_of_measures INTO DATA(ls_uom) ##INTO_OK.
      CASE ls_uom-id.

        WHEN zif_c_mdm_tool=>c_units-piece. " Piece Line
          IF ls_uom-ean11 IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_ean11.
            EXIT.
          ENDIF.

          IF ls_uom-laeng IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_laeng.
            EXIT.
          ENDIF.

          IF ls_uom-breit IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_breit.
            EXIT.
          ENDIF.

          IF ls_uom-hoehe IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_hoehe.
            EXIT.
          ENDIF.

          IF ls_uom-brgew IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_brgew.
            EXIT.
          ENDIF.

          IF ls_uom-ntgew IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_ntgew.
            EXIT.
          ENDIF.

        WHEN zif_c_mdm_tool=>c_units-mastercarton. " Master Carton Line

          IF ls_uom-umrez IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_umrez.
            EXIT.
          ENDIF.

          IF ls_uom-meinh IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_meinh.
            EXIT.
          ENDIF.

          IF ls_uom-ean11 IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_ean11.
            EXIT.
          ENDIF.

          IF ls_uom-laeng IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_laeng.
            EXIT.
          ENDIF.

          IF ls_uom-breit IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_breit.
            EXIT.
          ENDIF.

          IF ls_uom-hoehe IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_hoehe.
            EXIT.
          ENDIF.

          IF ls_uom-brgew IS INITIAL.
            rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_brgew.
            EXIT.
          ENDIF.

        WHEN zif_c_mdm_tool=>c_units-palet. " Pallet

          IF    ls_uom-umrez IS NOT INITIAL
             OR ls_uom-pmat  IS NOT INITIAL.

            IF ls_uom-umrez IS INITIAL.
              rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_umrez.
              EXIT.
            ENDIF.

            IF ls_uom-pmat IS INITIAL.
              rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_uom_pmat.
              EXIT.
            ENDIF.

          ENDIF.
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.


  METHOD check_obligatory_fields_whsd.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check mandatory fields warehouse data tab
    "*&
    "*&
    "********************************************************************
    IF me->mo_model->ms_lower_block-s_warehouse_data-ccind IS INITIAL.
      rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_ccind.
      RETURN.
    ENDIF.

    IF me->mo_model->ms_lower_block-s_warehouse_data-demqty IS INITIAL.
      rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_demqty.
      RETURN.
    ENDIF.

    IF me->mo_model->ms_lower_block-s_warehouse_data-volind IS INITIAL.
      rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_volind.
      RETURN.
    ENDIF.

    IF me->mo_model->ms_lower_block-s_warehouse_data-put_stra IS INITIAL.
      rv_screen_name = zif_c_mdm_tool=>c_fieldnames-screen_put_stra.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD check_packspeck_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check packspec data
    "*&
    "*&
    "********************************************************************
    DATA(ls_carton) = VALUE #( me->mo_model->ms_midlle_block-t_unit_of_measures[
                                   id = zif_c_mdm_tool=>c_units-mastercarton ] OPTIONAL ).

    IF     me->mo_model->ms_master_carton_aditional-zz1_dirrpl_stt IS INITIAL
       AND me->mo_model->ms_master_carton_aditional-tote_quan      IS NOT INITIAL.
      DATA(lv_quan) = mo_model->ms_master_carton_aditional-tote_quan.
    ELSE.
      lv_quan = ls_carton-umrez.
    ENDIF.
    IF lv_quan IS INITIAL OR ls_carton-pmat IS INITIAL.
      rv_is_pack_values_ok = abap_false.
    ELSE.
      rv_is_pack_values_ok = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD check_pack_material.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check packaging material
    "*&
    "*&
    "********************************************************************
    DATA ls_matnr       TYPE /scmb/mdl_matnr_str.
    DATA lv_material_18 TYPE char18.

    READ TABLE mo_model->ms_midlle_block-t_unit_of_measures
         INTO DATA(ls_middle_block)
         WITH KEY id = cs_middle_block-id.
    IF ls_middle_block-pmat = cs_middle_block-pmat.
      RETURN.
    ENDIF.

    IF     cs_middle_block-id    = zif_c_mdm_tool=>c_units-palet
       AND cs_middle_block-pmat IS INITIAL.
      CLEAR: cs_middle_block-pmat,
             cs_middle_block-pmatid,
             cs_middle_block-pmat_description,
             cs_middle_block-hutyp,
             cs_middle_block-laeng,
             cs_middle_block-breit,
             cs_middle_block-hoehe.
      RETURN.
    ENDIF.
    lv_material_18 = |{ cs_middle_block-pmat ALPHA = IN }|.

    ls_matnr-matnr = lv_material_18.
    DATA(ls_matid) = read_matid_from_product( is_matnr = ls_matnr ).
    IF ls_matid-matid IS INITIAL.
      " TODO: variable is assigned but never used; add pragma ##NEEDED (ABAP cleaner)
      MESSAGE e009(zmc_mdm_tool) WITH cs_middle_block-pmat INTO DATA(lv_dummy)  ##NEEDED.
      raise_exception_from_sy( ).
    ENDIF.

    DATA(ls_material_details) = mo_model->get_mat_details_from_matid( iv_matid = ls_matid-matid ).

    cs_middle_block-pmatid           = ls_matid-matid.
    cs_middle_block-pmat_description = VALUE #( ls_material_details-txt[ 1 ]-maktx OPTIONAL ).
    cs_middle_block-hutyp            = ls_material_details-pac-hutyp.
    DATA(ls_pmat_uom) = VALUE #( ls_material_details-uom[ 1 ] OPTIONAL ).
    cs_middle_block-laeng = ls_material_details-pac-maxl.
    cs_middle_block-breit = ls_material_details-pac-maxb.
    cs_middle_block-hoehe = ls_material_details-pac-maxh.
    cs_middle_block-meabm = COND #( WHEN ls_pmat_uom-meabm IS NOT INITIAL
                                    THEN ls_pmat_uom-meabm
                                    ELSE zif_c_mdm_tool=>c_units-mm ).
  ENDMETHOD.


  METHOD check_quantities.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check quantities
    "*&
    "*&
    "********************************************************************
    IF zcl_mdm_controller=>get_cursor_field( ) <> zif_c_mdm_tool=>c_fieldnames-screen_uom_umrez.
      RETURN.
    ENDIF.

    READ TABLE mo_model->ms_midlle_block-t_unit_of_measures
         INTO DATA(ls_middle_block)
         WITH KEY id = cs_middle_block-id.
    IF cs_middle_block-umrez = ls_middle_block-umrez.
      RETURN.
    ENDIF.

    zcl_param=>get_parameter( EXPORTING iv_lgnum     = mo_model->ms_top_block-sub_section_1-lgnum
                                        iv_process   = zif_param_const=>c_zcross_0005
                                        iv_parameter = zif_param_const=>c_pmat_tote
                              IMPORTING ev_constant  = DATA(lv_tote_material_const) ).

    CASE cs_middle_block-id.
      WHEN zif_c_mdm_tool=>c_units-mastercarton.
        DATA(lt_marm) = mo_model->master_data-marm.
        DELETE lt_marm WHERE meinh+0(1) <> zif_c_mdm_tool=>c_units-mastercarton+0(1).
        READ TABLE lt_marm INTO DATA(ls_marm)
             WITH KEY umrez = cs_middle_block-umrez.
        IF sy-subrc = 0.
          cs_middle_block-umrez = ls_marm-umrez.
          cs_middle_block-umren = ls_marm-umren.
          cs_middle_block-meinh = ls_marm-meinh.
          IF lv_tote_material_const <> ls_middle_block-pmat. " it is tote material keep values.
            cs_middle_block-ean11 = ls_marm-ean11.
            cs_middle_block-laeng = ls_marm-laeng.
            cs_middle_block-breit = ls_marm-breit.
            cs_middle_block-hoehe = ls_marm-hoehe.
            cs_middle_block-meabm = ls_marm-meabm.
            cs_middle_block-brgew = ls_marm-brgew.
          ENDIF.
        ELSE.
          " TODO: variable is assigned but never used (ABAP cleaner)
          DATA(lv_cvalue) = zcl_mdm_controller=>get_cursor_field( )  ##NEEDED .
          lt_marm = mo_model->master_data-marm.
          DELETE lt_marm WHERE meinh+0(1) <> 'Z'.
          SORT lt_marm BY meinh DESCENDING.
          ls_marm = VALUE #( lt_marm[ 1 ] OPTIONAL ).
          DATA(lv_value) = ls_marm-meinh+1(2).
          IF lv_value IS INITIAL.
            lv_value = 0.
          ELSE.
            lv_value += 1.
          ENDIF.
          lv_value = |{ lv_value ALPHA = IN }|.
          DATA(lv_new_meinh) = `Z` && |{ lv_value }|.
          cs_middle_block-meinh = lv_new_meinh.
          IF lv_tote_material_const <> ls_middle_block-pmat. " it is tote material keep values.
            cs_middle_block-ean11 = VALUE #( ).
            cs_middle_block-laeng = VALUE #( ).
            cs_middle_block-breit = VALUE #( ).
            cs_middle_block-hoehe = VALUE #( ).
            cs_middle_block-brgew = VALUE #( ).
          ENDIF.
        ENDIF.

      WHEN zif_c_mdm_tool=>c_units-palet.
        IF cs_middle_block-pmat IS INITIAL AND cs_middle_block-umrez IS NOT INITIAL.
          DATA(lt_pallet_mat) = NEW zcl_packmmat_algo(
            iv_lgnum = mo_model->ms_top_block-sub_section_1-lgnum )->get_pmat_pallet_sped( ).
          DATA(ls_pmat_pallet) = VALUE #( lt_pallet_mat[ 1 ] OPTIONAL ).
          DATA(ls_material_details) = mo_model->get_mat_details_from_matid( iv_matid = ls_pmat_pallet-matid ).

          cs_middle_block-umren            = VALUE #( ls_material_details-uom[ 1 ]-umren OPTIONAL ).
          cs_middle_block-ean11            = VALUE #( ls_material_details-ean[ 1 ]-ean11 OPTIONAL ).
          cs_middle_block-pmat             = |{ ls_pmat_pallet-matnr ALPHA = OUT }|.
          cs_middle_block-pmatid           = ls_pmat_pallet-matid.
          cs_middle_block-pmat_description = VALUE #( ls_material_details-txt[ langu = sy-langu ]-maktx ).
          cs_middle_block-hutyp            = ls_material_details-pac-hutyp.
          cs_middle_block-laeng            = ls_material_details-pac-maxl.
          cs_middle_block-breit            = ls_material_details-pac-maxb.
          cs_middle_block-hoehe            = ls_material_details-pac-maxh.
          cs_middle_block-meabm            = COND #( WHEN ls_material_details-pac-maxdim_uom IS NOT INITIAL
                                                     THEN ls_material_details-pac-maxdim_uom
                                                     ELSE zif_c_mdm_tool=>c_units-mm ).
          cs_middle_block-brgew            = VALUE #( ls_material_details-uom[ 1 ]-brgew OPTIONAL ).
          cs_middle_block-gewei            = VALUE #( ls_material_details-uom[ 1 ]-gewei OPTIONAL ).

        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD clear_sections.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clear screen sections
    "*&
    "*&
    "********************************************************************
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss1 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection1>).
    mo_model->clear_sub_section_1( CHANGING cs_sub_section_1 = <ls_subsection1> ).

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss2 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection2>).
    mo_model->clear_sub_section_2( CHANGING cs_sub_section_2 = <ls_subsection2> ).

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss3 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection3>).
    mo_model->clear_sub_section_3( CHANGING cs_sub_section_3 = <ls_subsection3> ).

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-middle_ss1 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<lt_unit_of_measueres>).
    CLEAR <lt_unit_of_measueres>.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-middle_ss2 OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_packspec_header>).
    CLEAR <ls_packspec_header>.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-mc_additional OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_mc_additional>).
    CLEAR <ls_mc_additional>.

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_wrhs OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_warehouse_data>).
    mo_model->clear_warehouse_data( CHANGING cs_warehouse_data = <ls_warehouse_data> ).

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_strg OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_storage_data>).
    mo_model->clear_storage_type_data( CHANGING cs_storage_type_data = <ls_storage_data> ).

    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_bin_type OF STRUCTURE cs_screen TO FIELD-SYMBOL(<lt_bin_type>).
    mo_model->clear_storage_bin_type_sel( CHANGING ct_storage_bin_type_sel = <lt_bin_type> ).

    mo_model->clear_master_data( ).
  ENDMETHOD.


  METHOD constructor.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "*&
    "*&
    "********************************************************************
    mv_repid = iv_repid.
    mo_view  = zcl_mdm_view=>get_class_instance( ).
    get_model_instance( ).
  ENDMETHOD.


  METHOD convert_volume.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Volume convertor
    "*&
    "*&
    "********************************************************************
    DATA lv_umrez_meabm TYPE dzaehl.
    DATA lv_umren_meabm TYPE nennr.
    DATA lv_exp10_meabm TYPE exp10.
    DATA lv_addko_meabm TYPE addko.
    DATA lv_umrez_voleh TYPE dzaehl.
    DATA lv_umren_voleh TYPE nennr.
    DATA lv_exp10_voleh TYPE exp10.
    DATA lv_addko_voleh TYPE addko.

    TRY.
        cs_unitsofmeasure-volume = cs_unitsofmeasure-length * cs_unitsofmeasure-width * cs_unitsofmeasure-height.
      CATCH cx_sy_arithmetic_overflow ##NO_HANDLER.
    ENDTRY.
    IF cs_unitsofmeasure-volumeunit IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION 'UNIT_PARAMETERS_GET'
      EXPORTING
        unit           = cs_unitsofmeasure-unit_dim
      IMPORTING
        numerator      = lv_umrez_meabm
        denominator    = lv_umren_meabm
        exponent       = lv_exp10_meabm
        add_const      = lv_addko_meabm
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF NOT ( sy-subrc = 0 AND lv_addko_meabm = 0 ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'UNIT_PARAMETERS_GET'
      EXPORTING
        unit           = cs_unitsofmeasure-volumeunit
      IMPORTING
        numerator      = lv_umrez_voleh
        denominator    = lv_umren_voleh
        exponent       = lv_exp10_voleh
        add_const      = lv_addko_voleh
      EXCEPTIONS
        unit_not_found = 1
        OTHERS         = 2.
    IF NOT ( sy-subrc = 0 AND lv_addko_voleh = 0 ).
      RETURN.
    ENDIF.
    IF lv_umren_meabm = 0 OR lv_umrez_voleh = 0.
      RETURN.
    ENDIF.

    TRY.
        cs_unitsofmeasure-volume = cs_unitsofmeasure-volume
                  * ( ( lv_umrez_meabm * ( 10 ** lv_exp10_meabm ) / lv_umren_meabm )
                      ** 3 )
                  * lv_umren_voleh / ( lv_umrez_voleh * ( 10 ** lv_exp10_voleh ) )  ##OPERATOR[**].
      CATCH cx_sy_conversion_overflow ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD copy_pc_to_mc.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Copy unit of measure PC line to master carton line
    "*&
    "*&
    "********************************************************************
    DATA(ls_pc) = VALUE #( ct_unit_of_measures[ 1 ] OPTIONAL ).
    DATA(ls_mc) = VALUE #( ct_unit_of_measures[ 2 ] OPTIONAL ).
    IF ls_pc IS INITIAL OR ls_mc IS INITIAL.
      RETURN.
    ENDIF.

    IF ls_pc-umrez <> 1.
      MESSAGE s025(zmc_mdm_tool) DISPLAY LIKE 'I'.
      RETURN.
    ENDIF.

    ls_pc-meinh = zif_c_mdm_tool=>c_units-mastercarton.

    MODIFY ct_unit_of_measures FROM ls_pc INDEX 2
           TRANSPORTING umrez hoehe breit laeng brgew ntgew ean11 meinh.
  ENDMETHOD.


  METHOD create_fieldcat.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Field catalog
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION 'LVC_FIELDCATALOG_MERGE'
      EXPORTING
        i_structure_name       = iv_structure_name
        i_client_never_display = abap_true
        i_bypassing_buffer     = abap_true
      CHANGING
        ct_fieldcat            = rt_fieldcat
      EXCEPTIONS
        inconsistent_interface = 1
        program_error          = 2
        OTHERS                 = 3.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD create_job_for_slotting.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Slotting Job create
    "*&
    "*&
    "********************************************************************

    NEW zcl_mdm_slotting( iv_lgnum = mo_model->ms_top_block-sub_section_1-lgnum
      iv_entitled = mo_model->ms_top_block-sub_section_1-entitled
      iv_material = mo_model->master_data-mara-matnr
      iv_skip_check = abap_true
      )->run_slotting_job( ).
  ENDMETHOD.


  METHOD create_packspec.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Create packspeck
    "*&
    "*&
    "********************************************************************
    check_mastercarton_fields( ).
    IF iv_save IS INITIAL.
      IF /scwm/cl_rf_tools_srvc=>popup_to_confirm( iv_title = TEXT-001
                                                   iv_text  = TEXT-002 ) <> zif_c_mdm_tool=>c_answer-yes.
        RETURN.
      ENDIF.
    ENDIF.

    NEW zcl_mdm_packspec( iv_lgnum = mo_model->ms_top_block-sub_section_1-lgnum
      iv_entitled = mo_model->ms_top_block-sub_section_1-entitled
      iv_matnr = mo_model->ms_top_block-sub_section_1-matnr
      iv_ps_group = zif_c_mdm_tool=>c_packspec-ps_group "'PG01'
      it_unit_of_measures = mo_model->ms_midlle_block-t_unit_of_measures
      is_master_carton_aditional = mo_model->ms_master_carton_aditional
      io_packspeck_model = mo_model->mo_packspeck_model )->execute_packspec( ).
    COMMIT WORK AND WAIT.

    update_maxqty( ).

    cs_screen = mo_model->get_packspec_header_detail( mo_model->ms_top_block-sub_section_1-matnr ) ##ENH_OK.

    IF cs_screen IS NOT INITIAL.
      IF cs_screen-status = /scwm/cl_ppelipak_cntl=>gc_status_active.
        cs_screen-icon = /scwm/cl_ppelipak_cntl=>gc_icon_active.
      ELSE.
        cs_screen-icon = /scwm/cl_ppelipak_cntl=>gc_icon_inactive.
      ENDIF.
    ELSE.
      cs_screen-icon = /scwm/cl_ppelipak_cntl=>gc_icon_part_act.
    ENDIF.
  ENDMETHOD.


  METHOD create_storage_bin_type_alv.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Storage bin type ALV
    "*&
    "*&
    "********************************************************************
    DATA lo_container      TYPE REF TO cl_gui_custom_container ##NEEDED.
    DATA ls_layout         TYPE lvc_s_layo.
    DATA lt_excl_tab       TYPE ui_functions.
    DATA lo_event_receiver TYPE REF TO lcl_event_receiver.

    " create objects if necessary
    IF me->mo_model->ms_lower_block-s_storage_type_data-lgtyp IS INITIAL.
      RETURN.
    ENDIF.

    IF mo_alv_storage_bin_type IS INITIAL.
      " initialize alv
      initialize_alv( EXPORTING iv_container_name = 'CONT1'
                      IMPORTING eo_container      = lo_container
                                eo_alv            = mo_alv_storage_bin_type ).

      lo_event_receiver = NEW #( ).
      SET HANDLER lo_event_receiver->handle_hotspot_click FOR mo_alv_storage_bin_type.

      DATA(lt_fieldcat) = create_fieldcat( zif_c_mdm_tool=>c_strucutes-storage_type_bin_type ).

      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
        <ls_fieldcat>-col_opt = abap_true.
      ENDLOOP.

      ls_layout-no_toolbar = abap_true.
      ls_layout-zebra      = abap_true.
      ls_layout-col_opt    = abap_true.

      mo_alv_storage_bin_type->set_table_for_first_display(
        EXPORTING
          i_structure_name     = zif_c_mdm_tool=>c_strucutes-storage_type_bin_type
          is_layout            = ls_layout
          it_toolbar_excluding = lt_excl_tab
        CHANGING
          it_fieldcatalog      = lt_fieldcat
          it_outtab            = mo_model->ms_lower_block-t_storage_bin_type_sel ).
    ELSE.
      mo_alv_storage_bin_type->refresh_table_display( is_stable = VALUE lvc_s_stbl( row = abap_true
                                                                                    col = abap_true ) ).
    ENDIF.
  ENDMETHOD.


  METHOD create_storage_section_alv.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Create storage section ALV
    "*&
    "*&
    "********************************************************************
    DATA lo_container      TYPE REF TO cl_gui_custom_container ##NEEDED.
    DATA ls_layout         TYPE lvc_s_layo.
    DATA lt_excl_tab       TYPE ui_functions.
    DATA lo_event_receiver TYPE REF TO lcl_event_receiver.

    " create objects if necessary
    IF me->mo_model->ms_lower_block-s_storage_type_data-lgtyp IS INITIAL.
      RETURN.
    ENDIF.

    IF mo_alv_storage_section IS INITIAL.

      initialize_alv( EXPORTING iv_container_name = 'CONTAINER'
                      IMPORTING eo_container      = lo_container
                                eo_alv            = mo_alv_storage_section ).

      lo_event_receiver = NEW #( ).
      SET HANDLER lo_event_receiver->handle_hotspot_click FOR mo_alv_storage_section.

      DATA(lt_fieldcat) = create_fieldcat( '/SCWM/T301T' ).

      LOOP AT lt_fieldcat ASSIGNING FIELD-SYMBOL(<ls_fieldcat>).
        <ls_fieldcat>-no_out = abap_true.
        IF <ls_fieldcat>-fieldname = 'LGTYP'.
          <ls_fieldcat>-no_out  = ' '.
          <ls_fieldcat>-col_pos = 1.
          <ls_fieldcat>-hotspot = abap_true.
        ENDIF.
        IF <ls_fieldcat>-fieldname = 'LTYPT'.
          <ls_fieldcat>-no_out  = ' '.
          <ls_fieldcat>-col_pos = 2.
        ENDIF.
      ENDLOOP.

      ls_layout-no_toolbar = 'X'.

      mo_alv_storage_section->set_table_for_first_display(
        EXPORTING
          i_structure_name     = '/SCWM/T301T'
          is_layout            = ls_layout
          it_toolbar_excluding = lt_excl_tab
        CHANGING
          it_fieldcatalog      = lt_fieldcat
          it_outtab            = mo_model->master_data-storage_type_description ).
    ELSE.
      mo_alv_storage_section->refresh_table_display( is_stable = VALUE lvc_s_stbl( row = abap_true
                                                                                   col = abap_true ) ).
    ENDIF.
  ENDMETHOD.


  METHOD create_variant.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Variant create for the JOB program
    "*&
    "*&
    "********************************************************************
    DATA ls_varit TYPE varit.
    DATA lt_varid TYPE STANDARD TABLE OF varid ##NEEDED.
    DATA lt_varit TYPE STANDARD TABLE OF varit.
    DATA ls_varid TYPE varid.

    ls_varit-mandt   = sy-mandt.
    ls_varit-langu   = sy-langu.
    ls_varit-report  = iv_report_name.
    ls_varit-variant = mv_variant_name.
    ls_varit-vtext   = TEXT-001.
    APPEND ls_varit TO lt_varit.

    ls_varid-mandt      = sy-mandt.
    ls_varid-report     = iv_report_name.
    ls_varid-variant    = mv_variant_name.
    ls_varid-transport  = zif_c_mdm_tool=>c_variant-transport_f.
    ls_varid-environmnt = zif_c_mdm_tool=>c_variant-env_background_and_online.
    ls_varid-version    = zif_c_mdm_tool=>c_variant-version.
    ls_varid-ename      = sy-uname.
    ls_varid-edat       = sy-datum.
    ls_varid-etime      = sy-uzeit.
    ls_varid-mlangu     = sy-langu.
    APPEND ls_varid TO lt_varid.

    CALL FUNCTION 'RS_CREATE_VARIANT'
      EXPORTING
        curr_report               = iv_report_name
        curr_variant              = mv_variant_name
        vari_desc                 = ls_varid
      TABLES
        vari_contents             = it_valutab
        vari_text                 = lt_varit
      EXCEPTIONS
        illegal_report_or_variant = 1
        illegal_variantname       = 2
        not_authorized            = 3
        not_executed              = 4
        report_not_existent       = 5
        report_not_supplied       = 6
        variant_exists            = 7
        variant_locked            = 8
        OTHERS                    = 9.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD dangerous_goods_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for dangerous goods
    "*&
    "*&
    "********************************************************************
    DATA lt_value_tab  TYPE TABLE OF rcgatf4phr.
    DATA ls_addinf     TYPE rcgaddinf.
    DATA ls_rcgdialctr TYPE rcgdialctr.

    IF get_cursor_field( ) <> zif_c_mdm_tool=>c_fieldnames-screen_pdgnud.
      RETURN.
    ENDIF.
    IF mo_model->ms_top_block-sub_section_1-pdgnud IS INITIAL.
      RETURN.
    ENDIF.

    ls_rcgdialctr-actype = zif_c_mdm_tool=>c_hazardous-actype.
    ls_rcgdialctr-picgrp = zif_c_mdm_tool=>c_hazardous-picgrp.
    ls_rcgdialctr-panel  = zif_c_mdm_tool=>c_hazardous-panel.
    ls_rcgdialctr-trtype = zif_c_mdm_tool=>c_hazardous-trtype.

    ls_addinf-valdat = sy-datum.

    lt_value_tab = VALUE #( ( phrkey = mo_model->ms_top_block-sub_section_1-pdgnud ) ).
    " we need to reset the buffer, because we need all phrases (ESTPJ)
    CALL FUNCTION 'C1AE_ESTPJ_BUF_RESET'.

    CALL FUNCTION 'C14X_PHRSEL_SET_SHOW_DG_DIALOG'
      EXPORTING
        i_object         = zif_c_mdm_tool=>c_hazardous-dgtm2
        i_attribute      = zif_c_mdm_tool=>c_fieldnames-pdgnud
        i_dialctr        = ls_rcgdialctr
        i_addinf         = ls_addinf
        i_language       = sy-langu
        i_check_mode     = zif_c_mdm_tool=>c_hazardous-search
      TABLES
        i_value_tab      = lt_value_tab
        e_value_tab      = lt_value_tab
      EXCEPTIONS
        no_choosen_value = 1
        OTHERS           = 2 ##FM_SUBRC_OK.
  ENDMETHOD.


  METHOD delete_existing_variants.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Delete existing variant to overwrite new variant
    "*&
    "*&
    "********************************************************************
    DATA(lv_variant_name) = zif_c_mdm_tool=>c_variant-variant_name && |%|.
    SELECT * FROM varid
      WHERE report     = @iv_report_name
        AND variant LIKE @lv_variant_name
      INTO TABLE @DATA(lt_varid).
    LOOP AT lt_varid INTO DATA(ls_varid) ##INTO_OK.
      variant_delete( iv_report_name = iv_report_name
                      iv_variant     = ls_varid-variant ).
    ENDLOOP.
  ENDMETHOD.


  METHOD entitled_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get List of Parties Entitled to Dispose by EWM Warehouse
    "*&
    "*&
    "********************************************************************
    DATA lt_sh_entitled_plant TYPE /scwm/tt_sh_entitled_plant.

    CALL FUNCTION '/SCWM/GET_ENTITLED_BY_LGNUM_SH'
      EXPORTING
        iv_lgnum             = mo_model->ms_top_block-sub_section_1-lgnum
      IMPORTING
        et_sh_entitled_plant = lt_sh_entitled_plant.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield    = zif_c_mdm_tool=>c_fieldnames-entitled
        dynpprog    = mv_repid
        dynpnr      = zif_c_mdm_tool=>c_screen-ss_101
        dynprofield = zif_c_mdm_tool=>c_fieldnames-screen_entitled
        value_org   = zif_c_mdm_tool=>c_message_severity-success
      TABLES
        value_tab   = lt_sh_entitled_plant.
  ENDMETHOD.


  METHOD f4_with_customized_params.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Generic value help with customized parameters
    "*&
    "*&
    "********************************************************************
    FIELD-SYMBOLS <ls_iface> TYPE ddshiface.
    FIELD-SYMBOLS <ls_sface> TYPE ddshiface.
    DATA ls_shlp TYPE shlp_descr.
    DATA ls_rc   TYPE sy-subrc ##NEEDED.

    CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
      EXPORTING
        shlpname = iv_shlp_name
        shlptype = 'SH'
      IMPORTING
        shlp     = ls_shlp.

    LOOP AT it_ddshifaces ASSIGNING <ls_iface>.
      READ TABLE ls_shlp-interface[] WITH KEY shlpfield = <ls_iface>-shlpfield ASSIGNING <ls_sface>.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING <ls_iface> TO <ls_sface>.
      ENDIF.

    ENDLOOP.

    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        shlp          = ls_shlp
        disponly      = iv_display_only
        maxrecords    = iv_max_records
        multisel      = space
      IMPORTING
        rc            = ls_rc
      TABLES
        return_values = et_values.
  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD fill_str.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Fiill BAPI structure fields dynamically
    "*&
    "*&
    "********************************************************************

    DATA lt_fields_tabl_iso  TYPE TABLE OF dd03m.
    DATA lt_fields_tabl_bapi TYPE TABLE OF dd03m.

    SELECT * FROM dd03m
      INTO TABLE @DATA(lt_fields_bapi)        "#EC CI_ALL_FIELDS_NEEDED
      WHERE tabname    = @iv_tabname
        AND ddlanguage = @sy-langu.

    SELECT * FROM dd03m
      INTO TABLE @DATA(lt_fields_tabl)        "#EC CI_ALL_FIELDS_NEEDED
      WHERE tabname    = @iv_tabname+5(4)
        AND ddlanguage = @sy-langu.

    lt_fields_tabl_iso  = VALUE #( FOR wa IN lt_fields_tabl
                                   ( fieldname = wa-fieldname rollname = wa-rollname && '_ISO'  ) ).
    lt_fields_tabl_bapi = VALUE #( FOR wa IN lt_fields_tabl
                                   ( fieldname = wa-fieldname rollname = wa-rollname && '_BAPI' ) ).

    LOOP AT lt_fields_bapi INTO DATA(ls_fields_bapi) ##INTO_OK.
      DATA(lv_field) = VALUE #( lt_fields_tabl[ rollname = ls_fields_bapi-rollname ]-fieldname OPTIONAL ).
      IF lv_field IS INITIAL.

        lv_field = VALUE #( lt_fields_tabl_iso[ rollname = ls_fields_bapi-rollname ]-fieldname OPTIONAL ).
        IF lv_field IS INITIAL.
          lv_field = VALUE #( lt_fields_tabl_bapi[ rollname = ls_fields_bapi-rollname ]-fieldname OPTIONAL ).
          IF lv_field IS INITIAL.
            CASE ls_fields_bapi-rollname.
              WHEN zif_c_mdm_tool=>c_fieldnames-bismt40.
                lv_field = zif_c_mdm_tool=>c_fieldnames-bismt.
              WHEN zif_c_mdm_tool=>c_fieldnames-dwerk.
                lv_field = zif_c_mdm_tool=>c_fieldnames-dwerk.
              WHEN zif_c_mdm_tool=>c_fieldnames-prszk_bapi.
                lv_field = zif_c_mdm_tool=>c_fieldnames-zkprs.
              WHEN zif_c_mdm_tool=>c_fieldnames-taxkm.
                IF ls_fields_bapi-position = 4. lv_field = zif_c_mdm_tool=>c_fieldnames-taxm1. ENDIF.
              WHEN zif_c_mdm_tool=>c_fieldnames-tatyp.
                lv_field = zif_c_mdm_tool=>c_fieldnames-kschl.
              WHEN zif_c_mdm_tool=>c_fieldnames-xchpf.
                lv_field = zif_c_mdm_tool=>c_fieldnames-xchpf.
            ENDCASE.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lv_field IS INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_fields_bapi-fieldname OF STRUCTURE ev_str TO FIELD-SYMBOL(<ls_bapi>).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT lv_field OF STRUCTURE iv_file TO FIELD-SYMBOL(<ls_tabl>).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      <ls_bapi> = <ls_tabl>.
    ENDLOOP.

    REFRESH : lt_fields_bapi,lt_fields_tabl.

    SELECT dd03l~fieldname
           dd03l~rollname
      FROM dd03l
      INTO CORRESPONDING FIELDS OF TABLE lt_fields_bapi
      WHERE dd03l~tabname   = iv_tabname
        AND dd03l~precfield = ''
        AND dd03l~domname   = ''
        AND dd03l~as4local  = 'A' ##TOO_MANY_ITAB_FIELDS.

    SELECT dd03l~fieldname
           dd03l~rollname
      FROM dd03l
      INTO CORRESPONDING FIELDS OF TABLE lt_fields_tabl
      WHERE dd03l~tabname   = iv_tabname+5(4)
        AND dd03l~precfield = ''
        AND dd03l~domname   = ''
        AND dd03l~as4local  = 'A' ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_fields_bapi INTO ls_fields_bapi ##INTO_OK.
      CLEAR lv_field.
      lv_field = VALUE #( lt_fields_tabl[ rollname = ls_fields_bapi-rollname ]-fieldname OPTIONAL ).

      IF lv_field IS INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_fields_bapi-fieldname OF STRUCTURE ev_str TO <ls_bapi>.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT lv_field OF STRUCTURE iv_file TO <ls_tabl>.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      <ls_bapi> = <ls_tabl>.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_xstr.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Field BAPI control structure dynamically
    "*&
    "*&
    "********************************************************************

    DATA lt_dfies_tab TYPE STANDARD TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = iv_tabname
      TABLES
        dfies_tab      = lt_dfies_tab
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3 ##FM_SUBRC_OK.
    LOOP AT lt_dfies_tab INTO DATA(ls_dfies_tab) ##INTO_OK.
      ASSIGN COMPONENT ls_dfies_tab-fieldname OF STRUCTURE iv_str TO FIELD-SYMBOL(<ls_bapi>).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_dfies_tab-fieldname OF STRUCTURE ev_xstr TO FIELD-SYMBOL(<ls_bapix>).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      IF <ls_bapi> IS NOT INITIAL.
        IF ls_dfies_tab-keyflag = 'X' OR ls_dfies_tab-fieldname = 'PO_ITEM' OR ls_dfies_tab-fieldname = 'SCHED_LINE'.
          <ls_bapix> = <ls_bapi>.
        ELSE.
          <ls_bapix> = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_cursor_field.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Cursor field location
    "*&
    "*&
    "********************************************************************
    DATA lv_field TYPE string.

    GET CURSOR FIELD lv_field.
    rv_field = lv_field.
  ENDMETHOD.


  METHOD get_model_instance.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get model instance
    "*&
    "*&
    "********************************************************************
    mo_model = COND #( WHEN mo_model IS BOUND
                       THEN mo_model
                       ELSE NEW #( ) ).
  ENDMETHOD.


  METHOD get_parameter_from_caller.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : GET Parameters
    "*&
    "*&
    "********************************************************************
    GET PARAMETER ID 'SCI' FIELD cv_scanned_value.
    SET PARAMETER ID 'SCI' FIELD space.
    IF cv_scanned_value IS NOT INITIAL.
      set_functioncode( zif_c_mdm_tool=>c_user_command-enter ).
    ENDIF.
  ENDMETHOD.


  METHOD get_variant_values.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get variants for the job program
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = iv_report          " Report name
        variant              = iv_variant         " Variant name
      TABLES
        valutab              = rt_valutab     " Table that contains the values (P + S)
      EXCEPTIONS
        variant_non_existent = 1                " Variant does not exist
        variant_obsolete     = 2                " Report does not exist
        OTHERS               = 3 ##FM_SUBRC_OK.
    IF sy-subrc <> 0 ##NEEDED.

    ENDIF.
  ENDMETHOD.


  METHOD handle_template_reason_code.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : handle template reason code
    "*&
    "*&
    "********************************************************************
    DATA lv_string  TYPE string.
    DATA lv_counter TYPE i.
    DATA lv_number  TYPE xfeld.

    IF cs_sub_section_3-template01 IS NOT INITIAL.
      DATA(lv_len) = strlen( cs_sub_section_3-template01 ) ##NEEDED.
      DO strlen( cs_sub_section_3-template01 ) TIMES.

        IF cs_sub_section_3-template01+lv_counter(1) CA '0123456789'.
          lv_number = abap_true.
        ENDIF.
        IF lv_number IS INITIAL.
          IF lv_string IS INITIAL.
            lv_string = cs_sub_section_3-template01+lv_counter(1).
          ELSE.
            lv_string = lv_string && cs_sub_section_3-template01+lv_counter(1).
          ENDIF.
        ELSE.
          lv_string = lv_string && `_`.
        ENDIF.
        lv_counter += 1.
      ENDDO.
      cs_sub_section_3-template01 = lv_string.
      cs_sub_section_3-template01 = to_upper( cs_sub_section_3-template01 ).

      READ TABLE cs_sub_section_3-t_indenttab01 WITH KEY template = cs_sub_section_3-template01 TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND VALUE #( mandt    = sy-mandt
                        lgnum    = mo_model->ms_top_block-sub_section_1-lgnum
                        matnr    = mo_model->ms_top_block-sub_section_1-matnr
                        id_type  = cs_sub_section_3-indenttab01
                        counter  = ''
                        template = cs_sub_section_3-template01 )

               TO cs_sub_section_3-t_indenttab01.
      ENDIF.
    ENDIF.

    CLEAR : lv_string,
            lv_counter,
            lv_number.


    IF cs_sub_section_3-template02 IS NOT INITIAL.
      lv_len = strlen( cs_sub_section_3-template02 ).
      DO strlen( cs_sub_section_3-template02 ) TIMES.
        IF cs_sub_section_3-template02+lv_counter(1) CA '0123456789'.
          lv_number = abap_true.
        ENDIF.
        IF lv_number IS INITIAL.
          IF lv_string IS INITIAL.
            lv_string = cs_sub_section_3-template02+lv_counter(1).
          ELSE.
            lv_string = lv_string && cs_sub_section_3-template02+lv_counter(1).
          ENDIF.
        ELSE.
          lv_string = lv_string && `_`.
        ENDIF.
        lv_counter += 1.
      ENDDO.
      cs_sub_section_3-template02 = lv_string.
      cs_sub_section_3-template02 = to_upper( cs_sub_section_3-template02 ).

      READ TABLE cs_sub_section_3-t_indenttab02 WITH KEY template = cs_sub_section_3-template02 TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND VALUE #( mandt    = sy-mandt
                        lgnum    = mo_model->ms_top_block-sub_section_1-lgnum
                        matnr    = mo_model->ms_top_block-sub_section_1-matnr
                        id_type  = cs_sub_section_3-indenttab02
                        counter  = ''
                        template = cs_sub_section_3-template02 )

               TO cs_sub_section_3-t_indenttab02.
      ENDIF.
    ENDIF.

    CLEAR : lv_string,
            lv_counter,
            lv_number.

    IF cs_sub_section_3-template03 IS NOT INITIAL.
      lv_len = strlen( cs_sub_section_3-template03 ).
      DO strlen( cs_sub_section_3-template03 ) TIMES.

        IF cs_sub_section_3-template03+lv_counter(1) CA '0123456789'.
          lv_number = abap_true.
        ENDIF.
        IF lv_number IS INITIAL.
          IF lv_string IS INITIAL.
            lv_string = cs_sub_section_3-template03+lv_counter(1).
          ELSE.
            lv_string = lv_string && cs_sub_section_3-template03+lv_counter(1).
          ENDIF.
        ELSE.
          lv_string = lv_string && `_`.
        ENDIF.
        lv_counter += 1.
      ENDDO.
      cs_sub_section_3-template03 = lv_string.
      cs_sub_section_3-template03 = to_upper( cs_sub_section_3-template03 ).


      READ TABLE cs_sub_section_3-t_indenttab03 WITH KEY template = cs_sub_section_3-template03 TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND VALUE #( mandt    = sy-mandt
                        lgnum    = mo_model->ms_top_block-sub_section_1-lgnum
                        matnr    = mo_model->ms_top_block-sub_section_1-matnr
                        id_type  = cs_sub_section_3-indenttab03
                        counter  = ''
                        template = cs_sub_section_3-template03 )

               TO cs_sub_section_3-t_indenttab03.
      ENDIF.
    ENDIF.

    CLEAR : lv_string,
            lv_counter,
            lv_number.

    IF cs_sub_section_3-template04 IS NOT INITIAL.
      lv_len = strlen( cs_sub_section_3-template04 ).
      DO strlen( cs_sub_section_3-template04 ) TIMES.

        IF cs_sub_section_3-template04+lv_counter(1) CA '0123456789'.
          lv_number = abap_true.
        ENDIF.
        IF lv_number IS INITIAL.
          IF lv_string IS INITIAL.
            lv_string = cs_sub_section_3-template04+lv_counter(1).
          ELSE.
            lv_string = lv_string && cs_sub_section_3-template04+lv_counter(1).
          ENDIF.
        ELSE.
          lv_string = lv_string && `_`.
        ENDIF.
        lv_counter += 1.
      ENDDO.
      cs_sub_section_3-template04 = lv_string.
      cs_sub_section_3-template04 = to_upper( cs_sub_section_3-template04 ).

      READ TABLE cs_sub_section_3-t_indenttab04 WITH KEY template = cs_sub_section_3-template04 TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND VALUE #( mandt    = sy-mandt
                        lgnum    = mo_model->ms_top_block-sub_section_1-lgnum
                        matnr    = mo_model->ms_top_block-sub_section_1-matnr
                        id_type  = cs_sub_section_3-indenttab04
                        counter  = ''
                        template = cs_sub_section_3-template04 )

               TO cs_sub_section_3-t_indenttab04.
      ENDIF.
    ENDIF.

    CLEAR : lv_string,
            lv_counter,
            lv_number.

    IF cs_sub_section_3-template05 IS NOT INITIAL.
      lv_len = strlen( cs_sub_section_3-template05 ).
      DO strlen( cs_sub_section_3-template05 ) TIMES.
        IF cs_sub_section_3-template05+lv_counter(1) CA '0123456789'.
          lv_number = abap_true.
        ENDIF.
        IF lv_number IS INITIAL.
          IF lv_string IS INITIAL.
            lv_string = cs_sub_section_3-template05+lv_counter(1).
          ELSE.
            lv_string = lv_string && cs_sub_section_3-template05+lv_counter(1).
          ENDIF.
        ELSE.
          lv_string = lv_string && `_`.
        ENDIF.
        lv_counter += 1.
      ENDDO.
      cs_sub_section_3-template05 = lv_string.
      cs_sub_section_3-template05 = to_upper( cs_sub_section_3-template05 ).

      READ TABLE cs_sub_section_3-t_indenttab05 WITH KEY template = cs_sub_section_3-template05 TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        APPEND VALUE #( mandt    = sy-mandt
                        lgnum    = mo_model->ms_top_block-sub_section_1-lgnum
                        matnr    = mo_model->ms_top_block-sub_section_1-matnr
                        id_type  = cs_sub_section_3-indenttab05
                        counter  = ''
                        template = cs_sub_section_3-template05 )

               TO cs_sub_section_3-t_indenttab05.
      ENDIF.
    ENDIF.

    mo_model->ms_top_block-sub_section_3 = cs_sub_section_3.
  ENDMETHOD.


  METHOD inb_deco_text_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for inb deco text
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-text_id valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-text_type  value = zif_wme_c=>gc_text_types-inbound_deco   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-material_texts
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_text_id = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD indenttab01_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for identification tab
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-identtab valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value     = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-identtab
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_identab01 = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD initialize_alv.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Initialize ALV
    "*&
    "*&
    "********************************************************************
    eo_container = NEW #( container_name = iv_container_name ).

    eo_alv = NEW #( i_parent = eo_container ).
  ENDMETHOD.


  METHOD job_close.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Job close
    "*&
    "*&
    "********************************************************************
    DATA lv_running  TYPE tbtcv-run ##NEEDED.
    DATA lv_finished TYPE tbtcv-fin.
    DATA lv_aborted  TYPE tbtcv-abort.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = iv_jobcount
        jobname              = mv_jobname
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        invalid_target       = 8
        OTHERS               = 9.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    DO.
      CALL FUNCTION 'SHOW_JOBSTATE'
        EXPORTING
          jobcount         = iv_jobcount
          jobname          = mv_jobname
        IMPORTING
          finished         = lv_finished
          running          = lv_running
        EXCEPTIONS
          jobcount_missing = 1
          jobname_missing  = 2
          job_notex        = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        " e_error = true.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF lv_finished IS NOT INITIAL OR lv_aborted IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD job_open.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : JOB open
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = mv_jobname
      IMPORTING
        jobcount         = ev_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD job_submit.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : JOB submit
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION 'JOB_SUBMIT'
      EXPORTING
        authcknam               = sy-uname
        jobcount                = iv_jobcount
        jobname                 = mv_jobname
        report                  = zif_c_mdm_tool=>c_variant-report_name_daily_vol
        variant                 = mv_variant_name
      EXCEPTIONS
        bad_priparams           = 1
        bad_xpgflags            = 2
        invalid_jobdata         = 3
        jobname_missing         = 4
        job_notex               = 5
        job_submit_failed       = 6
        lock_failed             = 7
        program_missing         = 8
        prog_abap_and_extpg_set = 9
        OTHERS                  = 10.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD kep_picking_text_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for keep picking text
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-text_id valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-text_type  value = zif_wme_c=>gc_text_types-kep_material_picking   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-material_texts
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_text_id = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD master_carton_additional.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Additional fields for master carton
    "*&
    "*&
    "********************************************************************
    DATA lv_id_description TYPE zde_id_description.
    DATA lv_tote_material  TYPE char18.

    DATA(lv_direct_replenishment) = cs_master_carton_aditional-zz1_dirrpl_stt.

    CALL FUNCTION 'Z_MDM_MASTERCARTON_ADDITION'
      EXPORTING
        iv_matid                       = iv_matid
      CHANGING
        cs_additional_data             = cs_master_carton_aditional
        cs_warehouse_data              = cs_warehouse_data
        cs_storage_type_data           = cs_storage_type_data
        ct_unit_of_measures            = ct_unit_of_measures
        cv_is_mc_additional_maintained = mo_model->mv_is_mc_additional_maintained.

    cs_storage_type_data-store_uom = mo_model->read_store_uom( cs_storage_type_data ).

    IF lv_direct_replenishment = cs_master_carton_aditional-zz1_dirrpl_stt. " OR
      RETURN.
    ENDIF.

    " ->BSUGAREV-20233813 Remove "lv_first_time" check.
    " -> Data Tote/MC in UoM tab has to be updated each time after flag "zz1_dirrpl_stt" is updated
    "        AND lv_first_time IS INITIAL.
    " ->BSUGAREV-20233813

    " Update Second row of the UOM
    DATA(lt_carton_mat) = NEW zcl_packmmat_algo(
      iv_lgnum = mo_model->ms_top_block-sub_section_1-lgnum )->get_pmat_carton( ).
    DATA(lt_tote_mat) = NEW zcl_packmmat_algo(
      iv_lgnum = mo_model->ms_top_block-sub_section_1-lgnum )->get_pmat_totes( ).

    zcl_param=>get_parameter( EXPORTING iv_lgnum     = mo_model->ms_top_block-sub_section_1-lgnum
                                        iv_process   = zif_param_const=>c_zcross_0005
                                        iv_parameter = zif_param_const=>c_pmat_tote
                              IMPORTING ev_constant  = DATA(lv_tote_material_const) ).

    lv_tote_material = lv_tote_material_const.
    lv_tote_material = |{ lv_tote_material ALPHA = IN }|.

    DATA(ls_pmat_carton) = VALUE #( lt_carton_mat[ nonconv = cs_warehouse_data-zz1_nonconveyable_whd ] OPTIONAL ).
    DATA(ls_pmat_tote)   = VALUE #( lt_tote_mat[ matnr = lv_tote_material ] OPTIONAL ).
    IF ls_pmat_tote IS INITIAL.
      ls_pmat_tote = VALUE #( lt_tote_mat[ 1 ] OPTIONAL ).
    ENDIF.

    DATA(ls_material_details)      = mo_model->get_mat_details_from_matid( iv_matid = ls_pmat_carton-matid ).
    DATA(ls_material_details_tote) = mo_model->get_mat_details_from_matid( iv_matid = ls_pmat_tote-matid ).

    DATA(lt_marm_temp) = mo_model->master_data-marm.
    DELETE lt_marm_temp WHERE meinh+0(1) <> 'Z'.
    SORT lt_marm_temp BY meinh.
    READ TABLE ct_unit_of_measures INTO DATA(ls_uom) INDEX 2.
    IF sy-subrc = 0.
      READ TABLE lt_marm_temp INTO DATA(ls_marm) WITH KEY meinh = ls_uom-meinh.
      IF sy-subrc <> 0.
        READ TABLE lt_marm_temp INTO ls_marm INDEX 1.
      ENDIF.
    ELSE.
      READ TABLE lt_marm_temp INTO ls_marm INDEX 1.
    ENDIF.

    IF cs_storage_type_data-zz1_dirrpl_stt IS NOT INITIAL. " Only in Case the DIRREPL = "X" Packmat for MC should be chosen from the system
      DATA(lv_pmat)       = |{ ls_pmat_carton-matnr ALPHA = OUT }|.
      lv_id_description   = TEXT-007.
      DATA(lv_pmatid)     = ls_pmat_carton-matid.
      DATA(lv_pmat_descr) = VALUE /scwm/de_ps_description( ls_material_details-txt[ langu = sy-langu ]-maktx OPTIONAL ).
      DATA(lv_hutyp)      = ls_material_details-pac-hutyp.
      DATA(lv_laeng)      = ls_marm-laeng.
      DATA(lv_breit)      = ls_marm-breit.
      DATA(lv_hoehe)      = ls_marm-hoehe.
    ELSE. " the packmat must be a tote packmat as DIRREPL = " "

      lv_pmat           = |{ ls_pmat_tote-matnr ALPHA = OUT }|.
      lv_id_description = TEXT-008.
      lv_pmatid         = ls_pmat_tote-matid.

      lv_pmat_descr = VALUE #( ls_material_details_tote-txt[ langu = sy-langu ]-maktx OPTIONAL ).
      lv_hutyp      = ls_material_details_tote-pac-hutyp.
      lv_laeng      = COND #( WHEN ls_marm-laeng IS NOT INITIAL THEN
                                ls_marm-laeng
                              WHEN ls_material_details_tote-pac-maxl IS NOT INITIAL THEN
                                ls_material_details_tote-pac-maxl
                              ELSE
                                ls_marm-laeng ).
      lv_breit      = COND #( WHEN ls_marm-breit IS NOT INITIAL THEN
                                ls_marm-breit
                              WHEN ls_material_details_tote-pac-maxb IS NOT INITIAL THEN
                                ls_material_details_tote-pac-maxb
                              ELSE
                                ls_marm-breit ).
      lv_hoehe      = COND #( WHEN ls_marm-hoehe IS NOT INITIAL THEN
                                ls_marm-hoehe
                              WHEN ls_material_details_tote-pac-maxh IS NOT INITIAL THEN
                                ls_material_details_tote-pac-maxh
                              ELSE
                                ls_marm-hoehe ).
    ENDIF.

    IF ls_uom IS NOT INITIAL.
      ls_uom-id_description   = lv_id_description.
      ls_uom-pmat             = lv_pmat.
      ls_uom-pmatid           = lv_pmatid.
      ls_uom-pmat_description = lv_pmat_descr.
      ls_uom-hutyp            = lv_hutyp.
      ls_uom-laeng            = lv_laeng.
      ls_uom-breit            = lv_breit.
      ls_uom-hoehe            = lv_hoehe.
      MODIFY ct_unit_of_measures FROM ls_uom INDEX 2
             TRANSPORTING id_description pmat pmatid pmat_description
                          hutyp laeng breit hoehe.
    ENDIF.
  ENDMETHOD.


  METHOD modify_screen_100.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Modify screen 100
    "*&
    "*&
    "********************************************************************
    LOOP AT SCREEN.
      IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_104_group1 AND mo_model->ms_top_block-sub_section_1-scanned_value IS INITIAL.
        screen-invisible = zif_c_mdm_tool=>c_screen-invisible_1.
        screen-active    = zif_c_mdm_tool=>c_screen-active_0.
        screen-invisible = zif_c_mdm_tool=>c_screen-active_1.
        DATA(lv_inactive) = abap_true.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    IF lv_inactive = abap_true.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD modify_screen_101.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Modify subscreen 101
    "*&
    "*&
    "********************************************************************
    LOOP AT SCREEN.
      IF     screen-group1 = zif_c_mdm_tool=>c_screen-ss_101_group1
         AND ( mo_model->ms_top_block-sub_section_1-scanned_value IS INITIAL OR mo_model->ms_top_block-sub_section_1-matnr IS INITIAL ).
        screen-invisible = zif_c_mdm_tool=>c_screen-invisible_1.
        screen-active    = zif_c_mdm_tool=>c_screen-active_0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-name = zif_c_mdm_tool=>c_fieldnames-screen_mhdrz.
        IF mo_model->ms_top_block-sub_section_1-sled_bbd <> abap_true.
          screen-input = zif_c_mdm_tool=>c_screen-active_0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_screen_102.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Modify subscreen 102
    "*&
    "*&
    "********************************************************************
    LOOP AT SCREEN.
      IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_102_group1 AND mo_model->ms_top_block-sub_section_1-scanned_value IS INITIAL.
        screen-invisible = zif_c_mdm_tool=>c_screen-invisible_1.
        screen-active    = zif_c_mdm_tool=>c_screen-active_0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_screen_103.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Modify subscreen 103
    "*&
    "*&
    "********************************************************************
    CLEAR me->mt_screen.
    LOOP AT SCREEN.
      IF     screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1
         AND mo_model->ms_top_block-sub_section_1-scanned_value IS INITIAL.
        screen-invisible = zif_c_mdm_tool=>c_screen-invisible_1.
        screen-active    = zif_c_mdm_tool=>c_screen-active_0.
        DATA(lv_inactive) = abap_true.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
    IF lv_inactive = abap_true.
      RETURN.
    ENDIF.

    LOOP AT SCREEN.

      IF    me->mo_model->ms_top_block-sub_section_3-reason_code01 = 1
         OR me->mo_model->ms_top_block-sub_section_3-reason_code01 = 2
         OR me->mo_model->ms_top_block-sub_section_3-reason_code01 = 3.

        IF     screen-group1  = zif_c_mdm_tool=>c_screen-ss_103_group1
           AND screen-group3 <> zif_c_mdm_tool=>c_screen-group_icn.

          screen-required = zif_c_mdm_tool=>c_screen-input_0.
          screen-input    = zif_c_mdm_tool=>c_screen-input_0.

        ELSEIF     screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1
               AND screen-group3 = zif_c_mdm_tool=>c_screen-group_icn.

          screen-invisible = zif_c_mdm_tool=>c_screen-input_1.
          screen-active    = zif_c_mdm_tool=>c_screen-input_0.
        ENDIF.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      IF me->mo_model->ms_top_block-sub_section_3-indenttab01 IS NOT INITIAL.

        IF     screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1
           AND screen-group4 = zif_c_mdm_tool=>c_screen-group_e1.
          screen-required = zif_c_mdm_tool=>c_screen-required_2.
        ENDIF.

        IF me->mo_model->ms_top_block-sub_section_1-zzserial = 2 ##NEEDED. " Scenario 4- if Prod. Hierachry SN-managed = YES and  Product SN-managed = YES

        ENDIF.

        IF me->mo_model->ms_top_block-sub_section_1-zzserial <> 2. " Scenario 5- if Prod. Hierachry SN-managed = NO and  Product SN-managed = YES
          IF me->mo_model->ms_top_block-sub_section_3-reason_code01 = 4.
          ELSE.
            IF     screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1
               AND (    screen-group4 = zif_c_mdm_tool=>c_screen-group_e1
                     OR screen-group4 = zif_c_mdm_tool=>c_screen-group_e2
                     OR screen-group2 = zif_c_mdm_tool=>c_screen-group_t2
                     OR screen-group2 = zif_c_mdm_tool=>c_screen-group_t3
                     OR screen-group2 = zif_c_mdm_tool=>c_screen-group_t4
                     OR screen-group2 = zif_c_mdm_tool=>c_screen-group_t5 ).

              screen-required = zif_c_mdm_tool=>c_screen-input_0.
              screen-input    = zif_c_mdm_tool=>c_screen-input_0.
            ENDIF.
*            ENDIF.
          ENDIF.
        ENDIF.

        "--->>>MAINTAIN_TEMPLATE” in table ZTCROSS_NUMBERS. In that field, we can flag a certain SN / ID number as mandatory for template maintenance:
        IF     screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1
           AND screen-group2 = zif_c_mdm_tool=>c_screen-group_t1.
          IF me->mo_model->ms_top_block-sub_section_3-indenttab01 IS NOT INITIAL.
            IF VALUE #( me->mo_model->master_data-numbers[
                            selnum = mo_model->ms_top_block-sub_section_3-indenttab01 ]-maintain_template OPTIONAL ) = abap_true ##WARN_OK.

              IF     me->mo_model->ms_top_block-sub_section_1-zzserial      <> 2
                 AND me->mo_model->ms_top_block-sub_section_3-reason_code01 IS INITIAL.
                screen-required = zif_c_mdm_tool=>c_screen-input_0.
                screen-input    = zif_c_mdm_tool=>c_screen-input_0.
              ELSE.
                screen-required = zif_c_mdm_tool=>c_screen-required_2.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.

        IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1 AND screen-group2 = zif_c_mdm_tool=>c_screen-group_t2.
          IF me->mo_model->ms_top_block-sub_section_3-indenttab02 IS NOT INITIAL.
            IF VALUE #( me->mo_model->master_data-numbers[
                            selnum = mo_model->ms_top_block-sub_section_3-indenttab02 ]-maintain_template OPTIONAL ) = abap_true ##WARN_OK.

              IF     me->mo_model->ms_top_block-sub_section_1-zzserial      <> 2
                 AND me->mo_model->ms_top_block-sub_section_3-reason_code01 IS INITIAL.
                screen-required = zif_c_mdm_tool=>c_screen-input_0.
                screen-input    = zif_c_mdm_tool=>c_screen-input_0.
              ELSE.
                screen-required = zif_c_mdm_tool=>c_screen-required_2.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.

        IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1 AND screen-group2 = zif_c_mdm_tool=>c_screen-group_t3.
          IF me->mo_model->ms_top_block-sub_section_3-indenttab03 IS NOT INITIAL.
            IF VALUE #( me->mo_model->master_data-numbers[
                            selnum = mo_model->ms_top_block-sub_section_3-indenttab03 ]-maintain_template OPTIONAL ) = abap_true ##WARN_OK.

              IF     me->mo_model->ms_top_block-sub_section_1-zzserial      <> 2
                 AND me->mo_model->ms_top_block-sub_section_3-reason_code01 IS INITIAL.
                screen-required = zif_c_mdm_tool=>c_screen-input_0.
                screen-input    = zif_c_mdm_tool=>c_screen-input_0.
              ELSE.
                screen-required = zif_c_mdm_tool=>c_screen-required_2.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.

        IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1 AND screen-group2 = zif_c_mdm_tool=>c_screen-group_t4.
          IF me->mo_model->ms_top_block-sub_section_3-indenttab04 IS NOT INITIAL.
            IF VALUE #( me->mo_model->master_data-numbers[
                            selnum = mo_model->ms_top_block-sub_section_3-indenttab04 ]-maintain_template OPTIONAL ) = abap_true ##WARN_OK.

              IF     me->mo_model->ms_top_block-sub_section_1-zzserial      <> 2
                 AND me->mo_model->ms_top_block-sub_section_3-reason_code01 IS INITIAL.
                screen-required = zif_c_mdm_tool=>c_screen-input_0.
                screen-input    = zif_c_mdm_tool=>c_screen-input_0.
              ELSE.
                screen-required = zif_c_mdm_tool=>c_screen-required_2.
              ENDIF.

            ENDIF.
          ENDIF.
        ENDIF.

        IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1 AND screen-group2 = zif_c_mdm_tool=>c_screen-group_t5.
          IF me->mo_model->ms_top_block-sub_section_3-indenttab05 IS NOT INITIAL.
            IF VALUE #( me->mo_model->master_data-numbers[
                            selnum = mo_model->ms_top_block-sub_section_3-indenttab05 ]-maintain_template OPTIONAL ) = abap_true ##WARN_OK.
              screen-required = zif_c_mdm_tool=>c_screen-required_2.
            ENDIF.
          ENDIF.
        ENDIF.
        "---<<<MAINTAIN_TEMPLATE” in table ZTCROSS_NUMBERS. In that field, we can flag a certain SN / ID number as mandatory for template maintenance:

        "--->>> EXception for IMEI Number 002 and MAC Address 009
        IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1 AND screen-group2 = zif_c_mdm_tool=>c_screen-group_t2.
          IF me->mo_model->ms_top_block-sub_section_3-indenttab02 = 002 OR me->mo_model->ms_top_block-sub_section_3-indenttab02 = 009.
            screen-required = zif_c_mdm_tool=>c_screen-input_0.
            screen-input    = zif_c_mdm_tool=>c_screen-input_0.
          ENDIF.
        ENDIF.

        IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1 AND screen-group2 = zif_c_mdm_tool=>c_screen-group_t3.
          IF me->mo_model->ms_top_block-sub_section_3-indenttab03 = 002 OR me->mo_model->ms_top_block-sub_section_3-indenttab03 = 009.
            screen-required = zif_c_mdm_tool=>c_screen-input_0.
            screen-input    = zif_c_mdm_tool=>c_screen-input_0.
          ENDIF.
        ENDIF.

        IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1 AND screen-group2 = zif_c_mdm_tool=>c_screen-group_t4.
          IF me->mo_model->ms_top_block-sub_section_3-indenttab04 = 002 OR me->mo_model->ms_top_block-sub_section_3-indenttab04 = 009.
            screen-required = zif_c_mdm_tool=>c_screen-input_0.
            screen-input    = zif_c_mdm_tool=>c_screen-input_0.
          ENDIF.
        ENDIF.

        IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1 AND screen-group2 = zif_c_mdm_tool=>c_screen-group_t5.
          IF me->mo_model->ms_top_block-sub_section_3-indenttab05 = 002 OR me->mo_model->ms_top_block-sub_section_3-indenttab05 = 009.
            screen-required = zif_c_mdm_tool=>c_screen-input_0.
            screen-input    = zif_c_mdm_tool=>c_screen-input_0.
          ENDIF.
        ENDIF.
        "---<<< EXception for IMEI Number 002 and MAC Address 009
      ELSE.
        IF     screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1
           AND screen-group2 = zif_c_mdm_tool=>c_screen-ss_103_group2.
          screen-input = zif_c_mdm_tool=>c_screen-input_0.
        ENDIF.
        IF     screen-group1 = zif_c_mdm_tool=>c_screen-ss_103_group1
           AND (    screen-group4 = zif_c_mdm_tool=>c_screen-group_e1
                 OR screen-group4 = zif_c_mdm_tool=>c_screen-group_e2
                 OR screen-group2 = zif_c_mdm_tool=>c_screen-group_t2
                 OR screen-group2 = zif_c_mdm_tool=>c_screen-group_t3
                 OR screen-group2 = zif_c_mdm_tool=>c_screen-group_t4
                 OR screen-group2 = zif_c_mdm_tool=>c_screen-group_t5 ).

          screen-input = zif_c_mdm_tool=>c_screen-input_0.
        ENDIF.
      ENDIF.

      CASE screen-group3.
        WHEN zif_c_mdm_tool=>c_screen-group_icn.

          IF screen-group4 = zif_c_mdm_tool=>c_screen-group_01.
            IF    me->mo_model->ms_top_block-sub_section_3-indenttab01 IS INITIAL
               OR (     me->mo_model->ms_top_block-sub_section_1-zzserial      <> 2
                    AND me->mo_model->ms_top_block-sub_section_3-reason_code01 IS INITIAL ).

              screen-invisible = zif_c_mdm_tool=>c_screen-input_1.
              screen-active    = zif_c_mdm_tool=>c_screen-input_0.

            ENDIF.
          ENDIF.

          IF screen-group4 = zif_c_mdm_tool=>c_screen-group_02.
            IF me->mo_model->ms_top_block-sub_section_3-indenttab02 IS INITIAL.
              screen-invisible = zif_c_mdm_tool=>c_screen-input_1.
              screen-active    = zif_c_mdm_tool=>c_screen-input_0.
            ELSEIF me->mo_model->ms_top_block-sub_section_3-indenttab02 IS NOT INITIAL ##BOOL_OK.
              IF    me->mo_model->ms_top_block-sub_section_3-indenttab02 = 002 OR me->mo_model->ms_top_block-sub_section_3-indenttab02 = 009
                 OR ( me->mo_model->ms_top_block-sub_section_1-zzserial <> 2 AND me->mo_model->ms_top_block-sub_section_3-reason_code01 IS INITIAL ).
                screen-invisible = zif_c_mdm_tool=>c_screen-input_1.
                screen-active    = zif_c_mdm_tool=>c_screen-input_0.
              ENDIF.
            ENDIF.
          ENDIF.

          IF screen-group4 = zif_c_mdm_tool=>c_screen-group_03.
            IF me->mo_model->ms_top_block-sub_section_3-indenttab03 IS INITIAL.
              screen-invisible = zif_c_mdm_tool=>c_screen-input_1.
              screen-active    = zif_c_mdm_tool=>c_screen-input_0.
            ELSEIF me->mo_model->ms_top_block-sub_section_3-indenttab03 IS NOT INITIAL ##BOOL_OK.
              IF    me->mo_model->ms_top_block-sub_section_3-indenttab03 = 002 OR me->mo_model->ms_top_block-sub_section_3-indenttab03 = 009
                 OR ( me->mo_model->ms_top_block-sub_section_1-zzserial <> 2 AND me->mo_model->ms_top_block-sub_section_3-reason_code01 IS INITIAL ).
                screen-invisible = zif_c_mdm_tool=>c_screen-input_1.
                screen-active    = zif_c_mdm_tool=>c_screen-input_0.
              ENDIF.
            ENDIF.
          ENDIF.

          IF screen-group4 = zif_c_mdm_tool=>c_screen-group_04.
            IF me->mo_model->ms_top_block-sub_section_3-indenttab04 IS INITIAL.
              screen-invisible = zif_c_mdm_tool=>c_screen-input_1.
              screen-active    = zif_c_mdm_tool=>c_screen-input_0.
            ELSEIF me->mo_model->ms_top_block-sub_section_3-indenttab04 IS NOT INITIAL ##BOOL_OK.
              IF    me->mo_model->ms_top_block-sub_section_3-indenttab04 = 002 OR me->mo_model->ms_top_block-sub_section_3-indenttab04 = 009
                 OR ( me->mo_model->ms_top_block-sub_section_1-zzserial <> 2 AND me->mo_model->ms_top_block-sub_section_3-reason_code01 IS INITIAL ).
                screen-invisible = zif_c_mdm_tool=>c_screen-input_1.
                screen-active    = zif_c_mdm_tool=>c_screen-input_0.
              ENDIF.
            ENDIF.
          ENDIF.

          IF screen-group4 = zif_c_mdm_tool=>c_screen-group_05.
            IF me->mo_model->ms_top_block-sub_section_3-indenttab05 IS INITIAL.
              screen-invisible = zif_c_mdm_tool=>c_screen-input_1.
              screen-active    = zif_c_mdm_tool=>c_screen-input_0.
            ELSEIF me->mo_model->ms_top_block-sub_section_3-indenttab05 IS NOT INITIAL ##BOOL_OK.
              IF    me->mo_model->ms_top_block-sub_section_3-indenttab05 = 002 OR me->mo_model->ms_top_block-sub_section_3-indenttab05 = 009
                 OR ( me->mo_model->ms_top_block-sub_section_1-zzserial <> 2 AND me->mo_model->ms_top_block-sub_section_3-reason_code01 IS INITIAL ).
                screen-invisible = zif_c_mdm_tool=>c_screen-input_1.
                screen-active    = zif_c_mdm_tool=>c_screen-input_0.
              ENDIF.
            ENDIF.
          ENDIF.

        WHEN OTHERS.
      ENDCASE.
      APPEND screen TO me->mt_screen.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_screen_104 ##NEEDED.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Modify Subscreen 104
    "*&
    "*&
    "********************************************************************
    DATA(ls_marm_pallet) = VALUE #( me->mo_model->master_data-marm[ meinh = zif_c_mdm_tool=>c_units-palet ] OPTIONAL ).
    LOOP AT SCREEN.
      CASE cs_table_control-current_line.
        WHEN '1'. " Piece Line
          IF    screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_umrez
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_meinh
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_pmat.
            screen-input = zif_c_mdm_tool=>c_screen-input_0.
          ENDIF.

          IF    screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_ean11
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_laeng
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_breit
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_hoehe
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_brgew
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_ntgew.
            screen-required = zif_c_mdm_tool=>c_screen-required_2.
          ENDIF.
        WHEN '2'. " Master Carton Line
          IF    screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_umrez
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_meinh
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_ean11
*             screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_pmat     OR
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_laeng
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_breit
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_hoehe
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_brgew.    " OR
*             screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_ntgew.
            screen-required = zif_c_mdm_tool=>c_screen-required_2.
          ENDIF.
          IF    screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_ntgew
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_pmat.
            screen-input = zif_c_mdm_tool=>c_screen-input_0.
          ENDIF.
        WHEN '3'. " Pallet
          IF ( screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_umrez AND ls_marm_pallet IS NOT INITIAL )
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_meinh
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_ean11
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_laeng
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_breit
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_hoehe
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_brgew
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_ntgew.
            screen-input = zif_c_mdm_tool=>c_screen-input_0.
          ENDIF.

          IF    (     screen-name     = zif_c_mdm_tool=>c_fieldnames-screen_uom_umrez
                  AND ls_marm_pallet IS INITIAL )
             OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_pmat.
            screen-required = zif_c_mdm_tool=>c_screen-required_2.
          ENDIF.

          IF    is_unit_of_measures-umrez IS NOT INITIAL
             OR is_unit_of_measures-pmat  IS NOT INITIAL.
            IF    screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_umrez
               OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_pmat.

              screen-required = zif_c_mdm_tool=>c_screen-required_2.
            ENDIF.
          ELSEIF     is_unit_of_measures-umrez IS INITIAL
                 AND is_unit_of_measures-pmat  IS INITIAL ##BOOL_OK.

            IF    screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_umrez
               OR screen-name = zif_c_mdm_tool=>c_fieldnames-screen_uom_pmat.
              screen-input    = zif_c_mdm_tool=>c_screen-input_1.
              screen-required = zif_c_mdm_tool=>c_screen-input_0.
            ENDIF.
          ENDIF.
      ENDCASE.

      IF me->mo_model->mv_is_mc_additional_maintained IS INITIAL.
        screen-input    = zif_c_mdm_tool=>c_screen-input_0.
        screen-required = zif_c_mdm_tool=>c_screen-input_0.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_screen_104_buttons.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Modify screen 104 buttons
    "*&
    "*&
    "********************************************************************
    LOOP AT SCREEN.
      IF me->mo_model->mv_is_mc_additional_maintained IS INITIAL.
        IF screen-name <> zif_c_mdm_tool=>c_fieldnames-mc_additional.
          screen-input    = zif_c_mdm_tool=>c_screen-input_0.
          screen-required = zif_c_mdm_tool=>c_screen-input_0.
          MODIFY SCREEN.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_screen_104_pack_spec.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Modify screen 104 packspec
    "*&
    "*&
    "********************************************************************
    LOOP AT SCREEN.
      IF screen-group1 = 'G6' AND me->mo_model->ms_midlle_block-s_packspec_header-ps_id IS INITIAL.
        screen-active = zif_c_mdm_tool=>c_screen-active_0.
      ENDIF.
      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_screen_105.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Modify subscreen 105
    "*&
    "*&
    "********************************************************************
    LOOP AT SCREEN.
      IF me->mo_model->mv_is_mc_additional_maintained IS INITIAL.
        IF screen-name <> zif_c_mdm_tool=>c_fieldnames-mc_additional.
          screen-input    = zif_c_mdm_tool=>c_screen-input_0.
          screen-required = zif_c_mdm_tool=>c_screen-input_0.
        ENDIF.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
      IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_105_group1.
        screen-required = zif_c_mdm_tool=>c_screen-required_2.
      ENDIF.
      IF     me->mo_model->ms_midlle_block-s_packspec_header-ps_id IS INITIAL
         AND screen-name = 'RUNSLOTTING'.
        screen-active = zif_c_mdm_tool=>c_screen-input_0.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_screen_106.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Modify subscreen 106
    "*&
    "*&
    "********************************************************************
    LOOP AT SCREEN.
      IF me->mo_model->mv_is_mc_additional_maintained IS INITIAL.
        IF screen-name <> zif_c_mdm_tool=>c_fieldnames-mc_additional.
          screen-input    = zif_c_mdm_tool=>c_screen-input_0.
          screen-required = zif_c_mdm_tool=>c_screen-input_0.
        ENDIF.

        MODIFY SCREEN.
        CONTINUE.
      ENDIF.
      IF screen-group1 = zif_c_mdm_tool=>c_screen-ss_106_group1.
        screen-required = zif_c_mdm_tool=>c_screen-required_2.
      ENDIF.
      IF me->mo_model->ms_lower_block-s_storage_type_data-zz1_dirrpl_stt IS NOT INITIAL.
        CASE screen-name.
          WHEN zif_c_mdm_tool=>c_fieldnames-screen_tote_quan.
            screen-input = zif_c_mdm_tool=>c_screen-input_0.
          WHEN OTHERS.
        ENDCASE.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_screen_107.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Modify subscreen 107
    "*&
    "*&
    "********************************************************************
    LOOP AT SCREEN.
      IF me->mo_model->mv_is_mc_additional_maintained IS INITIAL.
        IF screen-name <> zif_c_mdm_tool=>c_fieldnames-mc_additional.
          screen-input    = zif_c_mdm_tool=>c_screen-input_0.
          screen-required = zif_c_mdm_tool=>c_screen-input_0.
        ENDIF.
        MODIFY SCREEN.
        CONTINUE.
      ENDIF.

      MODIFY SCREEN.
    ENDLOOP.
  ENDMETHOD.


  METHOD modify_unit_of_measures.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Modify unit of measures
    "*&
    "*&
    "********************************************************************
    MODIFY ct_unit_of_measures
           FROM is_unit_of_measures
           INDEX iv_current_line.
    mv_check_complete = abap_true.
  ENDMETHOD.


  METHOD more_template.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : More template button
    "*&
    "*&
    "********************************************************************
    DATA ls_x030l     TYPE x030l ##NEEDED.
    DATA lt_dfies     TYPE dfies_tab.
    DATA lt_selfields TYPE TABLE OF se16n_selfields.
    DATA ls_selfields TYPE se16n_selfields.
    DATA lt_multi     TYPE TABLE OF se16n_selfields.
    DATA ls_multi     TYPE se16n_selfields.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname   = zif_c_mdm_tool=>c_strucutes-patterns
      IMPORTING
        x030l_wa  = ls_x030l
      TABLES
        dfies_tab = lt_dfies
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2 ##FM_SUBRC_OK.

    LOOP AT lt_dfies INTO DATA(ls_dfies) ##INTO_OK.
      MOVE-CORRESPONDING ls_dfies TO ls_selfields ##ENH_OK.
      IF sy-tabix = 1.
        ls_selfields-client = abap_true.
      ENDIF.
      ls_selfields-mark = abap_true.
      IF ls_dfies-keyflag = abap_true.
        ls_selfields-key = abap_true.
      ENDIF.
      " .....default sign is inclusive
      ls_selfields-sign = 'I'.
      APPEND ls_selfields TO lt_selfields.
      CLEAR ls_selfields.
    ENDLOOP.

    " .Now call popup to enter more values
    ls_selfields = lt_selfields[ fieldname = zif_c_mdm_tool=>c_fieldnames-template ].

    CASE iv_ucomm+4(2).
      WHEN 01.
        IF cs_identmnumber-template01 IS NOT INITIAL.
          READ TABLE cs_identmnumber-t_indenttab01 WITH KEY template = cs_identmnumber-template01 TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            MOVE-CORRESPONDING ls_selfields TO ls_multi.
            ls_multi-low = cs_identmnumber-template01.
            APPEND ls_multi TO lt_multi.
          ENDIF.
        ENDIF.
        LOOP AT cs_identmnumber-t_indenttab01 ASSIGNING FIELD-SYMBOL(<ls_indenttab>).
          MOVE-CORRESPONDING ls_selfields TO ls_multi.
          ls_multi-low = <ls_indenttab>-template.
          APPEND ls_multi TO lt_multi.
        ENDLOOP.
      WHEN 02.
        IF cs_identmnumber-template02 IS NOT INITIAL.
          READ TABLE cs_identmnumber-t_indenttab02 WITH KEY template = cs_identmnumber-template02 TRANSPORTING NO FIELDS.
          IF sy-subrc <> 0.
            MOVE-CORRESPONDING ls_selfields TO ls_multi.
            ls_multi-low = cs_identmnumber-template02.
            APPEND ls_multi TO lt_multi.
          ENDIF.
        ENDIF.
        LOOP AT cs_identmnumber-t_indenttab02 ASSIGNING <ls_indenttab>.
          MOVE-CORRESPONDING ls_selfields TO ls_multi.
          ls_multi-low = <ls_indenttab>-template.
          APPEND ls_multi TO lt_multi.
        ENDLOOP.
      WHEN 03.
        READ TABLE cs_identmnumber-t_indenttab03 WITH KEY template = cs_identmnumber-template03 TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          IF cs_identmnumber-template03 IS NOT INITIAL.
            MOVE-CORRESPONDING ls_selfields TO ls_multi.
            ls_multi-low = cs_identmnumber-template03.
            APPEND ls_multi TO lt_multi.
          ENDIF.
        ENDIF.
        LOOP AT cs_identmnumber-t_indenttab03 ASSIGNING <ls_indenttab>.
          MOVE-CORRESPONDING ls_selfields TO ls_multi.
          ls_multi-low = <ls_indenttab>-template.
          APPEND ls_multi TO lt_multi.
        ENDLOOP.
      WHEN 04.
        READ TABLE cs_identmnumber-t_indenttab04 WITH KEY template = cs_identmnumber-template04 TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          IF cs_identmnumber-template04 IS NOT INITIAL.
            MOVE-CORRESPONDING ls_selfields TO ls_multi.
            ls_multi-low = cs_identmnumber-template04.
            APPEND ls_multi TO lt_multi.
          ENDIF.
        ENDIF.
        LOOP AT cs_identmnumber-t_indenttab04 ASSIGNING <ls_indenttab>.
          MOVE-CORRESPONDING ls_selfields TO ls_multi.
          ls_multi-low = <ls_indenttab>-template.
          APPEND ls_multi TO lt_multi.
        ENDLOOP.
      WHEN 05.
        READ TABLE cs_identmnumber-t_indenttab05 WITH KEY template = cs_identmnumber-template05 TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          IF cs_identmnumber-template05 IS NOT INITIAL.
            MOVE-CORRESPONDING ls_selfields TO ls_multi.
            ls_multi-low = cs_identmnumber-template05.
            APPEND ls_multi TO lt_multi.
          ENDIF.
        ENDIF.
        LOOP AT cs_identmnumber-t_indenttab05 ASSIGNING <ls_indenttab>.
          MOVE-CORRESPONDING ls_selfields TO ls_multi.
          ls_multi-low = <ls_indenttab>-template.
          APPEND ls_multi TO lt_multi.
        ENDLOOP.
      WHEN OTHERS.
    ENDCASE.

    CALL FUNCTION 'ZMDM_MULTI_FIELD_INPUT'
      EXPORTING
        ls_selfields    = ls_selfields
        ld_currency     = VALUE sycurr( )
      TABLES
        lt_multi_select = lt_multi.

    CASE iv_ucomm+4(2).
      WHEN 01.
        CLEAR cs_identmnumber-t_indenttab01.
      WHEN 02.
        CLEAR cs_identmnumber-t_indenttab02.
      WHEN 03.
        CLEAR cs_identmnumber-t_indenttab03.
      WHEN 04.
        CLEAR cs_identmnumber-t_indenttab04.
      WHEN 05.
        CLEAR cs_identmnumber-t_indenttab05.
      WHEN OTHERS.
    ENDCASE.
    LOOP AT lt_multi INTO ls_multi ##INTO_OK.

      CASE iv_ucomm+4(2).
        WHEN 01.
          APPEND INITIAL LINE TO cs_identmnumber-t_indenttab01 ASSIGNING FIELD-SYMBOL(<ls_line>).
          <ls_line>-template = ls_multi-low.
          <ls_line>-mandt    = sy-mandt.
          <ls_line>-lgnum    = mo_model->ms_top_block-sub_section_1-lgnum.
          <ls_line>-matnr    = mo_model->ms_top_block-sub_section_1-matnr.
          <ls_line>-id_type  = cs_identmnumber-indenttab01.
          <ls_line>-counter  = ''.

          IF cs_identmnumber-template01 IS INITIAL.
            cs_identmnumber-template01 = ls_multi-low.
          ENDIF.
        WHEN 02.
          APPEND INITIAL LINE TO cs_identmnumber-t_indenttab02 ASSIGNING <ls_line>.
          <ls_line>-template = ls_multi-low.
          <ls_line>-mandt    = sy-mandt.
          <ls_line>-lgnum    = mo_model->ms_top_block-sub_section_1-lgnum.
          <ls_line>-matnr    = mo_model->ms_top_block-sub_section_1-matnr.
          <ls_line>-id_type  = cs_identmnumber-indenttab02.
          <ls_line>-counter  = ''.
          IF cs_identmnumber-template02 IS INITIAL.
            cs_identmnumber-template02 = ls_multi-low.
          ENDIF.
        WHEN 03.
          APPEND INITIAL LINE TO cs_identmnumber-t_indenttab03 ASSIGNING <ls_line>.
          <ls_line>-template = ls_multi-low.
          <ls_line>-mandt    = sy-mandt.
          <ls_line>-lgnum    = mo_model->ms_top_block-sub_section_1-lgnum.
          <ls_line>-matnr    = mo_model->ms_top_block-sub_section_1-matnr.
          <ls_line>-id_type  = cs_identmnumber-indenttab03.
          <ls_line>-counter  = ''.
          IF cs_identmnumber-template03 IS INITIAL.
            cs_identmnumber-template03 = ls_multi-low.
          ENDIF.
        WHEN 04.
          APPEND INITIAL LINE TO cs_identmnumber-t_indenttab04 ASSIGNING <ls_line>.
          <ls_line>-template = ls_multi-low.
          <ls_line>-mandt    = sy-mandt.
          <ls_line>-lgnum    = mo_model->ms_top_block-sub_section_1-lgnum.
          <ls_line>-matnr    = mo_model->ms_top_block-sub_section_1-matnr.
          <ls_line>-id_type  = cs_identmnumber-indenttab04.
          <ls_line>-counter  = ''.
          IF cs_identmnumber-template04 IS INITIAL.
            cs_identmnumber-template04 = ls_multi-low.
          ENDIF.
        WHEN 05.
          APPEND INITIAL LINE TO cs_identmnumber-t_indenttab05 ASSIGNING <ls_line>.
          <ls_line>-template = ls_multi-low.
          <ls_line>-mandt    = sy-mandt.
          <ls_line>-lgnum    = mo_model->ms_top_block-sub_section_1-lgnum.
          <ls_line>-matnr    = mo_model->ms_top_block-sub_section_1-matnr.
          <ls_line>-id_type  = cs_identmnumber-indenttab05.
          <ls_line>-counter  = ''.
          IF cs_identmnumber-template05 IS INITIAL.
            cs_identmnumber-template05 = ls_multi-low.
          ENDIF.
        WHEN OTHERS.
      ENDCASE.

    ENDLOOP.
    IF lt_multi IS INITIAL AND iv_ucomm IS NOT INITIAL.
      DATA(lv_template) = zif_c_mdm_tool=>c_fieldnames-template && |{ iv_ucomm+4(2) }|.
      ASSIGN COMPONENT lv_template OF STRUCTURE cs_identmnumber TO FIELD-SYMBOL(<ls_template>).
      IF <ls_template> IS ASSIGNED.
        CLEAR <ls_template>.
        UNASSIGN <ls_template>.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD packing_text_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for packing text
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-text_id valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-text_type  value = zif_wme_c=>gc_text_types-material_packing   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-material_texts
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_text_id = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD picking_text_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for picking text
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-text_id valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-text_type  value = zif_wme_c=>gc_text_types-general_material_picking   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-material_texts
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_text_id = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD pmat_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for packaging materials
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.
    DATA lv_dynindex   TYPE sy-stepl.

    CALL FUNCTION 'DYNP_GET_STEPL'
      IMPORTING
        povstepl = lv_dynindex.   "(v_dynindex  is a  variable  to  hold  the  index  value)

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-matnr valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-langu value     = sy-langu   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-pmat_only
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF     line_exists( lt_values[ 1 ] ) AND lv_dynindex = 3
       AND me->mo_model->mv_is_mc_additional_maintained IS NOT INITIAL.
      cv_pmat = lt_values[ 1 ]-fieldval.

      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD put_stra_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for putaway strategy
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-put_stra valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value     = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-put_stra
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_put_stra = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD raise_exception_from_sy.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Raise system exceptions
    "*&
    "*&
    "********************************************************************
    RAISE EXCEPTION TYPE zcx_mdm_tool
          MESSAGE ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDMETHOD.


  METHOD read_matid_from_product.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read material id from material
    "*&
    "*&
    "********************************************************************
    TRY.
        CALL FUNCTION '/SCMB/MDL_PRODUCT_READ'
          EXPORTING
            is_key  = is_matnr
          IMPORTING
            es_data = rs_matid.
      CATCH /scmb/cx_mdl ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD read_product_from_matid.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : read material attributes that should be displayed on the subscreen
    "*&
    "*&
    "********************************************************************
    TRY.
        CALL FUNCTION '/SCMB/MDL_PRODUCT_READ'
          EXPORTING
            iv_id   = is_matid-matid
          IMPORTING
            es_data = rs_product.
      CATCH /scmb/cx_mdl ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD reasoncode01_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for reason code
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-reasoncode valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value     = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-reasoncode
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_reasoncode01 = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD run_slotting.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Slotting run
    "*&
    "*&
    "********************************************************************
    update_maxqty( ).

    """" !!!!DO NOT DELETE
    zcl_mdm_slotting=>simulate( EXPORTING iv_lgnum       = mo_model->ms_top_block-sub_section_1-lgnum
                                          iv_entitled    = mo_model->ms_top_block-sub_section_1-entitled
                                          iv_matid       = mo_model->ms_top_block-sub_section_1-matid
                                IMPORTING es_dynpro_head = DATA(ls_dynpro_head)
                                          et_dynpro_c    = DATA(lt_dynpro_c) ) ##NEEDED.

    DATA(ls_warehouse_data_old) = cs_warehouse_data.

    " -->> these lines kind of simulation
    DATA(lo_slotting) = NEW zcl_mdm_slotting( iv_lgnum      = mo_model->ms_top_block-sub_section_1-lgnum
                                              iv_entitled   = mo_model->ms_top_block-sub_section_1-entitled
                                              iv_material   = mo_model->master_data-mara-matnr
                                              iv_skip_check = abap_false ).

    lo_slotting->run_slotting( EXPORTING iv_savmod   = zif_c_mdm_tool=>c_slotting-dummy
                               IMPORTING et_mat_slot = DATA(lt_mat_slot) ).

    DATA(ls_mat_slot) = VALUE #( lt_mat_slot[ 1 ] OPTIONAL ).
    CASE ls_mat_slot-lights.
      WHEN '1'.                     " Red 1.
        cs_warehouse_data-icon = /scwm/cl_ppelipak_cntl=>gc_icon_part_act.
      WHEN '2'.                     " Yellow 2
        cs_warehouse_data-icon = /scwm/cl_ppelipak_cntl=>gc_icon_inactive.
      WHEN '3'.                     " Green 3
        cs_warehouse_data-icon = /scwm/cl_ppelipak_cntl=>gc_icon_active.
      WHEN OTHERS.
    ENDCASE.

    " --<< these lines kind of simulation

    NEW zcl_mdm_slotting( iv_lgnum = mo_model->ms_top_block-sub_section_1-lgnum
      iv_entitled = mo_model->ms_top_block-sub_section_1-entitled
      iv_material = mo_model->master_data-mara-matnr
      iv_skip_check = abap_true )->run_slotting_job( ).

    IF NOT ( cs_warehouse_data IS SUPPLIED AND iv_savmod = zif_c_mdm_tool=>c_slotting-save ).
      RETURN.
    ENDIF.

    DATA(ls_warehouse_data) = mo_model->read_material_warehouse_data(
      iv_matnr       = mo_model->master_data-mara-matnr
      iv_scuguid     = mo_model->ms_t300_md-scuguid
      iv_entitled_id = CONV #( me->mo_model->ms_partner-partner_guid ) ).

    mo_model->update_material_warehouse_data( ls_warehouse_data ).

    MOVE-CORRESPONDING ls_warehouse_data TO cs_warehouse_data ##ENH_OK.
    " keep values if user has already entered!
    IF ls_warehouse_data_old-ccind IS NOT INITIAL.
      cs_warehouse_data-ccind = ls_warehouse_data_old-ccind.
    ENDIF.

    mo_model->check_put_stra( EXPORTING iv_put_stra    = cs_warehouse_data-put_stra
                              CHANGING  cv_description = cs_warehouse_data-put_stra_t ).

    mo_model->check_put_stra( EXPORTING iv_put_stra    = cs_warehouse_data-put_stra_plan
                              CHANGING  cv_description = cs_warehouse_data-put_stra_plan_t ).

    mo_model->check_dispatchable_entry( EXPORTING iv_disp_whd    = cs_warehouse_data-zz1_disp_whd
                                        CHANGING  cv_description = cs_warehouse_data-disp_whd_text ).

    mo_model->master_data-storage_type = mo_model->read_storage_type_data(
      iv_matnr       = mo_model->master_data-mara-matnr
      iv_scuguid     = mo_model->ms_t300_md-scuguid
      iv_entitled_id = CONV #( me->mo_model->ms_partner-partner_guid ) ).

    IF line_exists( lt_dynpro_c[ 1 ] ).
      DATA(ls_dynpro_c) = lt_dynpro_c[ 1 ].
      DATA(ls_matlwhst) = VALUE #( me->mo_model->master_data-storage_type[ lgtyp = ls_dynpro_c-lgtyp ] OPTIONAL ).
    ELSE.
      ls_matlwhst = VALUE #( me->mo_model->master_data-storage_type[ 1 ] OPTIONAL ).
    ENDIF.

    ls_matlwhst-zz1_dirrpl_stt = cs_storage_type_data-zz1_dirrpl_stt.
    ls_matlwhst-zz1_maxput_stt = cs_storage_type_data-zz1_maxput_stt.

    mo_model->fill_storage_type_data_tab( EXPORTING is_matlwhst          = ls_matlwhst " VALUE #( me->mo_model->master_data-storage_type[ 1 ] OPTIONAL )
                                          CHANGING  cs_storage_type_data = cs_storage_type_data ).

    DATA(ls_uom) = mo_model->ms_midlle_block-t_unit_of_measures[ id = zif_c_mdm_tool=>c_units-mastercarton ].
    IF ls_uom-umrez IS NOT INITIAL.
      DATA(lv_pieces) = ls_uom-umrez.
    ELSE.
      DATA(lt_marm) = mo_model->master_data-marm.
      DELETE lt_marm WHERE meinh+0(1) <> zif_c_mdm_tool=>c_units-mastercarton+0(1).
      SORT lt_marm BY meinh.
      lv_pieces = VALUE #( lt_marm[ 1 ]-umrez OPTIONAL ).
    ENDIF.

    mo_model->propose_replenishment_data( EXPORTING is_matlwhst          = ls_matlwhst
                                                    iv_pieces            = lv_pieces
                                          CHANGING  cs_warehouse_data    = cs_warehouse_data
                                                    cs_storage_type_data = cs_storage_type_data ).

    mo_model->fill_storage_type_text(
      EXPORTING
        it_storage_type             = mo_model->master_data-storage_type
      CHANGING
        ct_storage_type_description = mo_model->master_data-storage_type_description ).

    mo_model->fill_storage_bin_type_sel( EXPORTING iv_storage_type     = cs_storage_type_data-lgtyp
                                         CHANGING  ct_storage_bin_type = cs_storage_type_bin_type ).
    cs_storage_type_data-ltypt = mo_model->read_storage_type_text( cs_storage_type_data-lgtyp ).

    mo_model->propose_max_num_of_bin( EXPORTING is_warehouse_data    = cs_warehouse_data
                                      CHANGING  cs_storage_type_data = cs_storage_type_data ).
  ENDMETHOD.


  METHOD run_update_daily_volume_job.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update daily volume as background job
    "*&
    "*&
    "********************************************************************
    DATA ls_vardescr TYPE cl_svar_selscreen_variant_srv=>variant_content_description.
    DATA ls_vkey     TYPE cl_svar_selscreen_variant_srv=>varkey.
    DATA lt_intrange TYPE TABLE OF rsintrange.
    DATA ls_intrange TYPE rsintrange.
    DATA lv_sys      TYPE c LENGTH 1.
    DATA lv_date     TYPE rsdatrange-low.
    DATA lv_time     TYPE sy-uzeit.

    zcl_param=>get_parameter( EXPORTING iv_lgnum     = mo_model->ms_top_block-sub_section_1-lgnum
                                        iv_process   = zif_param_const=>c_zcross_0002
                                        iv_parameter = zif_param_const=>c_program_varaint
                              IMPORTING ev_constant  = DATA(lv_variant) ).
    IF lv_variant IS INITIAL.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_parameter_not_found ).
    ENDIF.

    DATA(lt_valutab) = get_variant_values( iv_report  = zif_c_mdm_tool=>c_variant-report_name_wo_to_mon
                                           iv_variant = CONV #( lv_variant ) ).
    IF lt_valutab IS INITIAL.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_variant_value_not_found ).
    ENDIF.

    GET TIME STAMP FIELD DATA(lv_ts).

    mv_variant_name = zif_c_mdm_tool=>c_variant-variant_name && lv_ts.
    mv_jobname      = zif_c_mdm_tool=>c_variant-variant_name && lv_ts.

    ASSIGN lt_valutab[ selname = zif_c_mdm_tool=>c_variant-s_matnr ] TO FIELD-SYMBOL(<ls_value_tab>).
    <ls_value_tab> = VALUE #( selname = zif_c_mdm_tool=>c_variant-s_matnr
                              kind    = zif_c_mdm_tool=>c_variant-select_option
                              sign    = wmegc_sign_inclusive
                              option  = wmegc_option_eq
                              low     = mo_model->master_data-mara-matnr
                              high    = '' ).
    IF <ls_value_tab> IS ASSIGNED.
      UNASSIGN <ls_value_tab>.
    ENDIF.

    ls_vkey-report  = zif_c_mdm_tool=>c_variant-report_name_wo_to_mon.
    ls_vkey-variant = lv_variant.

    cl_svar_selscreen_variant_srv=>import_variant_content_descr( EXPORTING  varkey                      = ls_vkey
                                                                 IMPORTING  variant_content_description = ls_vardescr
                                                                 EXCEPTIONS not_found                   = 1
                                                                            import_error                = 2
                                                                            inconsistent                = 3
                                                                            OTHERS                      = 4 ) ##SUBRC_OK.

    LOOP AT ls_vardescr-varidesc INTO DATA(ls_varidesc) WHERE    type = zif_c_mdm_tool=>c_variant-type_d
                                                              OR type = zif_c_mdm_tool=>c_variant-type_t ##INTO_OK.

      ASSIGN lt_valutab[ selname = ls_varidesc-name ] TO <ls_value_tab>.
      IF <ls_value_tab> IS ASSIGNED.
        ASSIGN lt_valutab[ selname = ls_varidesc-name ]-low TO FIELD-SYMBOL(<ls_low>).
      ENDIF.

      CASE ls_varidesc-type.
        WHEN zif_c_mdm_tool=>c_variant-type_d.
          IF ls_varidesc-vname IS NOT INITIAL.
            LOOP AT ls_vardescr-varivdat INTO DATA(ls_varivdat) WHERE selname = ls_varidesc-name.
              MOVE-CORRESPONDING ls_varivdat TO ls_intrange.
              APPEND ls_intrange TO lt_intrange.
              CLEAR ls_intrange.
            ENDLOOP.
            CALL FUNCTION ls_varidesc-vname+3
              EXPORTING
                systime    = lv_sys
              IMPORTING
                p_date     = lv_date
              TABLES
                p_intrange = lt_intrange
              EXCEPTIONS
                OTHERS     = 4 ##FM_SUBRC_OK.
          ELSE.
            lv_date = <ls_low>+6(4) && <ls_low>+3(2) && <ls_low>+0(2).
          ENDIF.
          <ls_value_tab> = VALUE #( selname = ls_varidesc-name
                                    kind    = ls_varidesc-kind
                                    sign    = wmegc_sign_inclusive
                                    option  = wmegc_option_eq
                                    low     = lv_date
                                    high    = '' ).
        WHEN zif_c_mdm_tool=>c_variant-type_t.
          lv_time = <ls_low>+0(2) && <ls_low>+3(2) && <ls_low>+6(2).
          IF ls_varidesc-name = 'P_CTTTO' OR ls_varidesc-name = 'P_COTTO'.
            lv_time = '235959'.
          ENDIF.
          <ls_value_tab> = VALUE #( selname = ls_varidesc-name
                                    kind    = ls_varidesc-kind
                                    sign    = wmegc_sign_inclusive
                                    option  = wmegc_option_eq
                                    low     = lv_time
                                    high    = '' ).
        WHEN OTHERS.
      ENDCASE.
      IF <ls_value_tab> IS ASSIGNED.
        UNASSIGN <ls_value_tab>.
      ENDIF.
      IF <ls_low> IS ASSIGNED.
        UNASSIGN <ls_low>.
      ENDIF.
      CLEAR: lt_intrange,
             lv_sys,
             lv_date,
             lv_time.
    ENDLOOP.
    delete_existing_variants( zif_c_mdm_tool=>c_variant-report_name_wo_to_mon ).

    create_variant( iv_report_name = zif_c_mdm_tool=>c_variant-report_name_wo_to_mon
                    it_valutab     = lt_valutab ).

    SUBMIT zcross_update_daily_volume                    "#EC CI_SUBMIT
           WITH p_lgnum  EQ mo_model->ms_top_block-sub_section_1-lgnum
           WITH p_vari   EQ mv_variant_name
           WITH p_dsplog EQ space
           AND RETURN.

    variant_delete( iv_report_name = zif_c_mdm_tool=>c_variant-report_name_wo_to_mon
                    iv_variant     = mv_variant_name ).

*     me->mo_model->read_material_warehouse_data
    DATA(ls_warehouse_data) = mo_model->read_material_warehouse_data(
      iv_matnr       = mo_model->master_data-mara-matnr
      iv_scuguid     = mo_model->ms_t300_md-scuguid
      iv_entitled_id = CONV #( me->mo_model->ms_partner-partner_guid ) ).

    IF ls_warehouse_data-demqty IS NOT INITIAL.
      cs_warehouse_data-demqty = ls_warehouse_data-demqty.
    ENDIF.
  ENDMETHOD.


  METHOD save.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Save Button to save everything
    "*&
    "*&
    "********************************************************************
    IF /scwm/cl_rf_tools_srvc=>popup_to_confirm( iv_title = TEXT-005
                                                 iv_text  = TEXT-006 ) <> zif_c_mdm_tool=>c_answer-yes.
      RETURN.
    ENDIF.
*
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss1        OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection1>).
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss2        OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection2>).
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-top_ss3        OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_subsection3>).
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-middle_ss1     OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_middle_uom>).
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-middle_ss2     OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_middle_packspec>).
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-mc_additional  OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_mc_additional>).
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_wrhs     OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_warehouse_data>).
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_strg     OF STRUCTURE cs_screen TO FIELD-SYMBOL(<ls_storage_type_data>).
    ASSIGN COMPONENT zif_c_mdm_tool=>c_screen-lower_bin_type OF STRUCTURE cs_screen TO FIELD-SYMBOL(<lt_bin_type>).

    " 1-Create/Change Packspec
    create_packspec( EXPORTING iv_save   = zif_c_mdm_tool=>c_user_command-save
                     CHANGING  cs_screen = <ls_middle_packspec> ).

    update_maxqty( ).

    " 2-Update Daily Volume
    NEW zcl_mdm_prod_change( io_model = mo_model )->execute( iv_daily_volume_update = abap_true ).

    " 3-Run Slotting and Save

    update_maxqty( ).

    " 5-Update Product Fields
    update_product_fields( ).

    " 6-Update Unit of measures
    DATA(ls_return) = update_units_of_measure( ).
    IF ls_return-type CA 'AEX'.
      bapi_transaction_rollback( ).
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
              WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4
              INTO DATA(lv_message) ##NEEDED.
      raise_exception_from_sy( ).
    ELSE.
      bapi_transaction_commit( ).
      " 7-Update Z tables

      CALL FUNCTION 'ZMDM_UPDATE_PATTERNS' IN UPDATE TASK
        EXPORTING
          is_section_1 = <ls_subsection1>
          is_section_3 = <ls_subsection3>.
      COMMIT WORK.

    ENDIF.
    ASSIGN COMPONENT zif_c_mdm_tool=>c_fieldnames-matnr OF STRUCTURE <ls_subsection1> TO FIELD-SYMBOL(<ls_matnr>).
    IF <ls_matnr> IS ASSIGNED.
      MESSAGE s021(zmc_mdm_tool) WITH <ls_matnr>.
    ELSE.
      MESSAGE s030(zmc_mdm_tool) ##MG_MISSING.
    ENDIF.

    clear_sections( CHANGING cs_screen = cs_screen ).

    "AAHMEDOV-240209
    IF zcl_deco_flag=>get_param_zdeco_nav( ) EQ abap_true.
      LEAVE PROGRAM.
    ENDIF.
    "AAHMEDOV-240209
  ENDMETHOD.


  METHOD selected_storage_type.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get selected storage type from ALV
    "*&
    "*&
    "********************************************************************
    mo_alv_storage_section->get_current_cell( IMPORTING e_row = DATA(lv_current_line) ).

    READ TABLE me->mo_model->master_data-storage_type INTO DATA(ls_matio_whst) INDEX lv_current_line.

    mo_model->fill_storage_type_data_tab(
      EXPORTING
        is_matlwhst          = ls_matio_whst
      CHANGING
        cs_storage_type_data = mo_model->ms_lower_block-s_storage_type_data ).
    cs_storage_type_data = mo_model->ms_lower_block-s_storage_type_data.

    mo_model->fill_storage_bin_type_sel(
      EXPORTING
        iv_storage_type     = ls_matio_whst-lgtyp
      CHANGING
        ct_storage_bin_type = mo_model->ms_lower_block-t_storage_bin_type_sel ).

    ct_storage_bin_type_sel = mo_model->ms_lower_block-t_storage_bin_type_sel.
  ENDMETHOD.


  METHOD set_cursor_field.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Set Cursor field
    "*&
    "*&
    "********************************************************************

    DATA lv_pos    TYPE i.
    DATA lv_string TYPE string.

    CLEAR me->mv_inactivate_save.
    IF me->mv_tab_clicked_tab IS NOT INITIAL.
      CLEAR me->mv_tab_clicked_tab.
      RETURN.
    ENDIF.
*    IF is_table_control IS NOT SUPPLIED.
    IF me->mo_model->mv_is_mc_additional_maintained IS NOT INITIAL.

      LOOP AT SCREEN.
        IF screen-input <> 1.
          CONTINUE.
        ENDIF.

        lv_string = screen-name.
        DO.
          lv_string = substring_after( val = lv_string
                                       sub = '-' ).
          IF lv_string IS INITIAL.
            EXIT.
          ENDIF.
          DATA(lv_name) = lv_string.
        ENDDO.
        IF lv_name = zif_c_mdm_tool=>c_fieldnames-sled_bbd.
          CONTINUE.
        ENDIF.

        IF lv_name <> zif_c_mdm_tool=>c_fieldnames-ean11.
          ASSIGN COMPONENT lv_name OF STRUCTURE me->mo_model->ms_top_block-sub_section_1 TO FIELD-SYMBOL(<lv_screen_value>).
          IF <lv_screen_value> IS NOT ASSIGNED.
            ASSIGN COMPONENT lv_name OF STRUCTURE me->mo_model->ms_top_block-sub_section_3 TO <lv_screen_value>.
          ENDIF.
        ENDIF.

        IF <lv_screen_value> IS ASSIGNED.
          IF <lv_screen_value> IS INITIAL.

            DATA(lv_is_cursor_set) = abap_true.
            mv_is_cursor_set   = abap_false.
            lv_is_cursor_set       = abap_true.
            UNASSIGN <lv_screen_value>.
            SET CURSOR FIELD screen-name OFFSET lv_pos.
            EXIT.
          ENDIF.
          UNASSIGN <lv_screen_value>.
        ENDIF.
        CLEAR: lv_name,
               lv_string.
      ENDLOOP.

      IF lv_is_cursor_set IS INITIAL AND is_table_control IS SUPPLIED AND me->mv_is_cursor_set IS NOT INITIAL.
        LOOP AT SCREEN.
          IF screen-input <> 1.
            CONTINUE.
          ENDIF.

          lv_string = screen-name.
          DO.
            lv_string = substring_after( val = lv_string
                                         sub = '-' ).
            IF lv_string IS INITIAL.
              EXIT.
            ENDIF.
            lv_name = lv_string.
          ENDDO.
          IF lv_name = zif_c_mdm_tool=>c_fieldnames-sled_bbd.
            CONTINUE.
          ENDIF.
          ASSIGN COMPONENT lv_name OF STRUCTURE is_unit_of_measures TO <lv_screen_value>.

          IF <lv_screen_value> IS ASSIGNED.
            IF <lv_screen_value> IS INITIAL.
              lv_is_cursor_set = abap_true.
              mv_is_cursor_set = abap_false.
              UNASSIGN <lv_screen_value>.
              SET CURSOR FIELD screen-name LINE is_table_control-current_line OFFSET lv_pos.
              EXIT.
            ENDIF.
            UNASSIGN <lv_screen_value>.
          ENDIF.
          CLEAR: lv_name,
                 lv_string.
        ENDLOOP.
      ENDIF.

      IF lv_is_cursor_set IS INITIAL AND is_table_control IS NOT SUPPLIED AND me->mv_is_cursor_set IS NOT INITIAL.
        LOOP AT SCREEN.
          IF screen-input <> 1 OR (    screen-group1 = zif_c_mdm_tool=>c_screen-ss_105_group1_c
                                    OR screen-group1 = zif_c_mdm_tool=>c_screen-ss_106_group1_c ).
            CONTINUE.
          ENDIF.

          lv_string = screen-name.
          DO.
            lv_string = substring_after( val = lv_string
                                         sub = '-' ).
            IF lv_string IS INITIAL.
              EXIT.
            ENDIF.
            lv_name = lv_string.
          ENDDO.
          IF lv_name = zif_c_mdm_tool=>c_fieldnames-sled_bbd.
            CONTINUE.
          ENDIF.
          ASSIGN COMPONENT lv_name OF STRUCTURE me->mo_model->ms_lower_block-s_warehouse_data TO <lv_screen_value>.
          IF <lv_screen_value> IS ASSIGNED.
            IF <lv_screen_value> IS INITIAL.
              lv_is_cursor_set = abap_true.
              mv_is_cursor_set = abap_false.
              UNASSIGN <lv_screen_value>.
              SET CURSOR FIELD screen-name OFFSET lv_pos.
              EXIT.
            ENDIF.
            UNASSIGN <lv_screen_value>.
          ENDIF.
          CLEAR: lv_name,
                 lv_string.
        ENDLOOP.
      ENDIF.

      IF lv_is_cursor_set IS INITIAL AND is_table_control IS NOT SUPPLIED AND me->mv_is_cursor_set IS NOT INITIAL.
        LOOP AT SCREEN.
          IF screen-input <> 1 OR (    screen-group1 = zif_c_mdm_tool=>c_screen-ss_105_group1_c
                                    OR screen-group1 = zif_c_mdm_tool=>c_screen-ss_106_group1_c ).
            CONTINUE.
          ENDIF.

          lv_string = screen-name.
          DO.
            lv_string = substring_after( val = lv_string
                                         sub = '-' ).
            IF lv_string IS INITIAL.
              EXIT.
            ENDIF.
            lv_name = lv_string.
          ENDDO.
          IF lv_name = zif_c_mdm_tool=>c_fieldnames-sled_bbd.
            CONTINUE.
          ENDIF.
          ASSIGN COMPONENT lv_name OF STRUCTURE me->mo_model->ms_lower_block-s_storage_type_data TO <lv_screen_value>.
          IF <lv_screen_value> IS ASSIGNED.
            IF <lv_screen_value> IS INITIAL.
              lv_is_cursor_set = abap_true.
              mv_is_cursor_set = abap_false.
              UNASSIGN <lv_screen_value>.
              SET CURSOR FIELD screen-name OFFSET lv_pos.
              EXIT.
            ENDIF.
            UNASSIGN <lv_screen_value>.
          ENDIF.
          CLEAR: lv_name,
                 lv_string.
        ENDLOOP.
      ENDIF.

      DATA(lv_screen_name) = check_obligatory_fields_ident( ).

      lv_screen_name = check_obligatory_fields_uom( is_table_control    = is_table_control
                                                    is_unit_of_measures = is_unit_of_measures ).

      IF lv_screen_name IS INITIAL.
        lv_screen_name = check_obligatory_fields_whsd( ).
      ENDIF.

      IF lv_screen_name IS INITIAL.
        lv_screen_name = check_obligatory_fields_styp( ).
      ENDIF.

      IF lv_screen_name IS INITIAL.
        " TODO: variable is assigned but never used (ABAP cleaner)
        DATA(lv_field) = get_cursor_field( ) ##NEEDED.
        mv_inactivate_save = abap_false.
        SET CURSOR FIELD screen-name OFFSET lv_pos.
        RETURN.
      ENDIF.
    ENDIF.

    IF lv_is_cursor_set IS INITIAL AND me->mv_is_cursor_set IS NOT INITIAL.
      lv_field = get_cursor_field( ).
      SET CURSOR FIELD screen-name OFFSET lv_pos.
    ELSE.
      mv_inactivate_save = abap_true.
    ENDIF.

    IF    me->mo_model->mv_is_mc_additional_maintained IS INITIAL
       OR lv_screen_name IS NOT INITIAL.
      mv_inactivate_save = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD set_cursor_field_107.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Set cursor field for Subscreen 107
    "*&
    "*&
    "********************************************************************
    DATA lv_pos TYPE i.

    ASSIGN COMPONENT 'PRESSED_TAB' OF STRUCTURE cs_tabstrip TO FIELD-SYMBOL(<fs_pressed_tab>).
    IF <fs_pressed_tab> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    CASE <fs_pressed_tab>.
      WHEN zif_c_mdm_tool=>c_tabs_area_mat-tab4.
        IF me->mo_model->ms_lower_block-s_material_texts-zz1_inb_deco_text_whd IS INITIAL.
          SET CURSOR FIELD zif_c_mdm_tool=>c_fieldnames-screen_inb_deco_text OFFSET lv_pos.

        ELSEIF me->mo_model->ms_lower_block-s_material_texts-zz1_picking_text_whd IS INITIAL.
          SET CURSOR FIELD zif_c_mdm_tool=>c_fieldnames-screen_inb_deco_text OFFSET lv_pos.

        ELSEIF me->mo_model->ms_lower_block-s_material_texts-zz1_kep_picking_text_whd IS INITIAL.
          SET CURSOR FIELD zif_c_mdm_tool=>c_fieldnames-screen_kep_picking_text OFFSET lv_pos.

        ELSEIF me->mo_model->ms_lower_block-s_material_texts-zz1_sped_picking_text_whd IS INITIAL.
          SET CURSOR FIELD zif_c_mdm_tool=>c_fieldnames-screen_sped_picking_text OFFSET lv_pos.

        ELSEIF me->mo_model->ms_lower_block-s_material_texts-zz1_packing_text_whd IS INITIAL.
          SET CURSOR FIELD zif_c_mdm_tool=>c_fieldnames-screen_packing_text OFFSET lv_pos.

        ELSEIF me->mo_model->ms_lower_block-s_material_texts-zz1_vastext_whd IS INITIAL.
          SET CURSOR FIELD zif_c_mdm_tool=>c_fieldnames-screen_vastext OFFSET lv_pos.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD set_fixed_slotting_values.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Fixed slotting values
    "*&
    "*&
    "********************************************************************
    DATA lv_name TYPE vrm_id.
    DATA lt_list TYPE vrm_values.

    DATA(lt_fixed_values) = mo_model->get_domain_values( iv_domname = zif_c_mdm_tool=>c_domains-concstat
                                                         iv_langu   = sy-langu ).

    lt_list = VALUE #( FOR ls_fixed_values IN lt_fixed_values
                       ( key  = ls_fixed_values-domvalue_l
                         text = ls_fixed_values-ddtext ) ).
    " value-key
    SORT lt_list BY key.
    lv_name = zif_c_mdm_tool=>c_fieldnames-screen_concstat.
    CALL FUNCTION 'VRM_SET_VALUES'
      EXPORTING
        id     = lv_name
        values = lt_list.
  ENDMETHOD.


  METHOD set_functioncode.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : SET Function code
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = iv_function_code
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2 ##FM_SUBRC_OK.
  ENDMETHOD.


  METHOD set_pf_status.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Set Pf-Status
    "*&
    "*&
    "********************************************************************
    DATA lt_fcode TYPE TABLE OF sy-ucomm.
    DATA ls_fcode TYPE sy-ucomm.

    IF me->mo_model->ms_top_block-sub_section_1-matnr IS INITIAL OR me->mv_inactivate_save = abap_true.
      ls_fcode = zif_c_mdm_tool=>c_user_command-save. APPEND ls_fcode TO lt_fcode.
    ENDIF.
    ls_fcode = zif_c_mdm_tool=>c_user_command-packspec. APPEND ls_fcode TO lt_fcode.
    ls_fcode = zif_c_mdm_tool=>c_user_command-mc_additon. APPEND ls_fcode TO lt_fcode.

    SET PF-STATUS zif_c_mdm_tool=>c_screen-status_100 OF PROGRAM mv_repid EXCLUDING lt_fcode.
  ENDMETHOD.


  METHOD set_template_icon.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Set template icons
    "*&
    "*&
    "********************************************************************
    DATA(lv_line) = lines( cs_sub_section_3-t_indenttab01 ).
    IF cs_sub_section_3-template01 IS INITIAL OR lv_line < 2.
      cs_sub_section_3-push01 = icon_enter_more.
    ELSE.
      cs_sub_section_3-push01 = icon_display_more.
    ENDIF.

    lv_line = lines( cs_sub_section_3-t_indenttab02 ).
    IF cs_sub_section_3-template02 IS INITIAL OR lv_line < 2.
      cs_sub_section_3-push02 = icon_enter_more.
    ELSE.
      cs_sub_section_3-push02 = icon_display_more.
    ENDIF.

    lv_line = lines( cs_sub_section_3-t_indenttab03 ).
    IF cs_sub_section_3-template03 IS INITIAL OR lv_line < 2.
      cs_sub_section_3-push03 = icon_enter_more.
    ELSE.
      cs_sub_section_3-push03 = icon_display_more.
    ENDIF.
    lv_line = lines( cs_sub_section_3-t_indenttab04 ).
    IF cs_sub_section_3-template04 IS INITIAL OR lv_line < 2.
      cs_sub_section_3-push04 = icon_enter_more.
    ELSE.
      cs_sub_section_3-push04 = icon_display_more.
    ENDIF.

    lv_line = lines( cs_sub_section_3-t_indenttab05 ).
    IF cs_sub_section_3-template05 IS INITIAL OR lv_line < 2.
      cs_sub_section_3-push05 = icon_enter_more.
    ELSE.
      cs_sub_section_3-push05 = icon_display_more.
    ENDIF.
  ENDMETHOD.


  METHOD set_titlebar.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Set title bar
    "*&
    "*&
    "********************************************************************
    SET TITLEBAR zif_c_mdm_tool=>c_screen-title_100 OF PROGRAM mv_repid.
  ENDMETHOD.


  METHOD sped_picking_text_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for sped picking text
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-text_id valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-text_type  value = zif_wme_c=>gc_text_types-sped_material_picking   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-material_texts
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_text_id = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD start_concepting.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Slotting conceptiong
    "*&
    "*&
    "********************************************************************
    DATA lo_concore                  TYPE REF TO /scwm/cl_concepting_core ##NEEDED.
    " Tables to update DB from all Slotting Runs
    DATA li_badi_slot_upd_storg_type TYPE REF TO /scwm/ex_slot_upd_storge_ty.
    DATA lv_badi_sttype_upd_mode     TYPE i.

    lo_concore = NEW #( ).

    " Initialize the BAdI here
    TRY.
        GET BADI li_badi_slot_upd_storg_type
          FILTERS
            lgnum = mo_model->ms_top_block-sub_section_1-lgnum.
      CATCH cx_badi ##NO_HANDLER.
    ENDTRY.
    IF li_badi_slot_upd_storg_type IS BOUND.
      CALL BADI li_badi_slot_upd_storg_type->determine_deletion_indicator
        EXPORTING
          flt_val        = mo_model->ms_top_block-sub_section_1-lgnum
          iv_lgnum       = mo_model->ms_top_block-sub_section_1-lgnum
          iv_entitled    = mo_model->ms_top_block-sub_section_1-entitled
        IMPORTING
          ev_delete_mode = lv_badi_sttype_upd_mode.
    ENDIF.

    lo_concore->concept_material_multi(
      EXPORTING
        it_slot_key             = it_slot_key
        iv_lgnum                = mo_model->ms_top_block-sub_section_1-lgnum
        iv_entitled             = mo_model->ms_top_block-sub_section_1-entitled
        is_slot_steps           = is_slot_steps
        iv_nolog                = VALUE #( )
        iv_badi_sttype_upd_mode = lv_badi_sttype_upd_mode
      IMPORTING
        et_mat_lgnum_result     = et_mat_lgnum_result_sel
        et_mat_lgtyp_result     = et_mat_lgtyp_result_sel
        et_binmat_result        = et_binmat_result_sel
        et_mat_lgtyp_orig       = et_mat_lgtyp_orig_sel ).
  ENDMETHOD.


  METHOD storage_section_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for storage section
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgbkz valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value     = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-storage_section_ind
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_sectind = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD storage_type_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for storage type
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgtyp valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value     = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-storage_type
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_lgtyp = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD trigger_enter.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Trigger enter
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = '/00'
      EXCEPTIONS
        function_not_supported = 1 ##FM_SUBRC_OK.
  ENDMETHOD.


  METHOD update_bulk_storage ##NEEDED.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update bulk storage value
    "*&
    "*&
    "********************************************************************
    IF    cs_section_1-lgnum IS INITIAL
       OR cs_section_1-block IS INITIAL.
      RETURN.
    ENDIF.

    SELECT SINGLE * FROM /scwm/t338
      INTO @DATA(ls_t338)
      WHERE lgnum = @cs_section_1-lgnum
        AND block = @cs_section_1-block ##NEEDED.
    IF sy-subrc <> 0.

      RAISE EXCEPTION NEW zcx_mdm_tool( textid   = zif_c_mdm_tool=>c_bulk_storage_not_found
                                        mv_msgv1 = CONV #( cs_section_1-block )
                                        mv_msgv2 = CONV #( cs_section_1-lgnum ) ).
    ENDIF.
  ENDMETHOD.


  METHOD update_daily_volume.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update daily volume field on the screen
    "*&
    "*&
    "********************************************************************

    NEW zcl_mdm_prod_change( io_model = mo_model )->execute( iv_daily_volume_update = abap_true ).

    IF iv_skip_popup IS INITIAL.
      rv_answer = /scwm/cl_rf_tools_srvc=>popup_to_confirm( iv_title = TEXT-003
                                                            iv_text  = TEXT-004 ).
    ENDIF.
  ENDMETHOD.


  METHOD update_indenttab.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update identification tab
    "*&
    "*&
    "********************************************************************
    DATA lt_patterns TYPE ztt_patterns.
    DATA lv_char     TYPE c LENGTH 3.

    DATA(lv_identtab) = zif_c_mdm_tool=>c_fieldnames-tabvalue       && iv_identnum.
    DATA(lv_descr)    = zif_c_mdm_tool=>c_fieldnames-description    && iv_identnum.
    DATA(lv_template) = zif_c_mdm_tool=>c_fieldnames-template       && iv_identnum.
    DATA(lv_t_indenttab) = zif_c_mdm_tool=>c_fieldnames-t_indenttab && iv_identnum.
    DATA(lv_template_available) =   zif_c_mdm_tool=>c_fieldnames-is_template && iv_identnum+1(1) && zif_c_mdm_tool=>c_fieldnames-available.

    ASSIGN COMPONENT lv_identtab    OF STRUCTURE cs_section_3 TO FIELD-SYMBOL(<lv_identtab_value>).
    ASSIGN COMPONENT lv_descr       OF STRUCTURE cs_section_3 TO FIELD-SYMBOL(<lv_descr>).
    ASSIGN COMPONENT lv_template    OF STRUCTURE cs_section_3 TO FIELD-SYMBOL(<lv_template>).
    ASSIGN COMPONENT lv_t_indenttab OF STRUCTURE cs_section_3 TO FIELD-SYMBOL(<lt_indenttab>).
    ASSIGN COMPONENT lv_template_available OF STRUCTURE cs_section_3 TO FIELD-SYMBOL(<lv_template_available>).

    IF <lv_identtab_value> IS NOT ASSIGNED.
      RETURN.
    ENDIF.

    IF <lv_identtab_value> IS INITIAL.
      IF <lv_descr> IS ASSIGNED.
        CLEAR <lv_descr>.
        UNASSIGN <lv_descr>.
      ENDIF.
      IF <lv_template> IS ASSIGNED.
        CLEAR <lv_template>.
        UNASSIGN <lv_template>.
      ENDIF.
      IF <lt_indenttab> IS ASSIGNED.
        CLEAR <lt_indenttab>.
        UNASSIGN <lt_indenttab>.
      ENDIF.
      IF <lv_template_available> IS ASSIGNED.
        CLEAR <lv_template_available>.
        UNASSIGN <lv_template_available>.
      ENDIF.

      EXIT.
    ENDIF.

    "->> same tab value more than once
    DO 5 TIMES.
      DATA(lv_index) = `0` && sy-index.
      IF lv_index <> iv_identnum.
        DATA(lv_identtab_all) = zif_c_mdm_tool=>c_fieldnames-tabvalue && lv_index.
        ASSIGN COMPONENT lv_identtab_all OF STRUCTURE cs_section_3 TO FIELD-SYMBOL(<lv_identtab_all_value>).
        IF <lv_identtab_all_value> = <lv_identtab_value>.
          RAISE EXCEPTION NEW zcx_mdm_tool( textid   = zif_c_mdm_tool=>c_exists_identab_value
                                            mv_msgv1 = CONV #( <lv_identtab_value> ) ).
        ENDIF.
      ENDIF.
    ENDDO.
    "-<< same tab value more than once
    lv_char = <lv_identtab_value>.
    lv_char = |{ lv_char  ALPHA = IN }|.
    <lv_identtab_value> = lv_char.
    IF <lt_indenttab> IS ASSIGNED.
      CLEAR <lt_indenttab>.
    ENDIF.
    IF <lv_template> IS ASSIGNED.
      CLEAR <lv_template>.
    ENDIF.

    READ TABLE me->mo_model->master_data-numbers INTO DATA(ls_identab) WITH KEY selnum = <lv_identtab_value>.
    IF sy-subrc = 0.
      IF <lv_descr> IS ASSIGNED.
        <lv_descr> = ls_identab-number_description.
        UNASSIGN <lv_descr>.
      ENDIF.
    ELSE.
      CLEAR <lv_identtab_value>.
      IF <lv_descr> IS ASSIGNED.
        CLEAR <lv_descr>.
      ENDIF.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_not_valid_identab_value ).
    ENDIF.

    LOOP AT me->mo_model->master_data-patterns INTO DATA(ls_patterns) WHERE id_type = <lv_identtab_value> ##INTO_OK.
      APPEND ls_patterns TO lt_patterns.
    ENDLOOP.
    IF lt_patterns IS NOT INITIAL.
      ASSIGN lt_patterns TO <lt_indenttab>.
      <lv_template> = lt_patterns[ 1 ]-template.
    ENDIF.

    IF <lv_identtab_value> = 002.
      IF <lv_template> IS ASSIGNED.
        <lv_template> = mo_model->master_data-imei_no_template.
        UNASSIGN <lv_template>.
      ENDIF.
    ELSEIF <lv_identtab_value> = 009.
      IF <lv_template> IS ASSIGNED.
        <lv_template> = mo_model->master_data-mac_address_template.
        UNASSIGN <lv_template>.
      ENDIF.
    ENDIF.
    UNASSIGN <lv_identtab_value>.
  ENDMETHOD.


  METHOD update_maxqty.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update maximum quantity
    "*&
    "*&
    "********************************************************************
    DATA lr_entitled TYPE RANGE OF /scwm/s_lagp_mon_f4-entitled.
    DATA lr_lgtyp    TYPE RANGE OF /scwm/s_lagp_mon_f4-lgtyp.
    DATA lr_lptyp    TYPE RANGE OF /scwm/s_lagp_mon_f4-lptyp.
    DATA lr_matnr    TYPE RANGE OF /scwm/s_lagp_mon_f4-matnr.

    lr_entitled = VALUE #(
        ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = mo_model->ms_top_block-sub_section_1-entitled ) ).
    lr_matnr    = VALUE #(
        ( sign = wmegc_sign_inclusive option = wmegc_option_eq low = mo_model->master_data-mara-matnr ) ).
    IF sy-tcode IS INITIAL.
      sy-tcode = 'ZMDM' ##WRITE_OK. " testing purpose!
    ENDIF.
    SUBMIT zcross_update_maxqty                          "#EC CI_SUBMIT
           WITH p_lgnum EQ mo_model->ms_top_block-sub_section_1-lgnum
           WITH so_ent   IN lr_entitled
           WITH so_lgtyp IN lr_lgtyp
           WITH so_lptyp IN lr_lptyp
           WITH so_matnr IN lr_matnr
           WITH p_full   EQ abap_true
           WITH p_append EQ abap_false
           WITH p_del    EQ abap_false
           AND RETURN.
  ENDMETHOD.


  METHOD update_product_fields.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update product fields
    "*&
    "*&
    "********************************************************************
    NEW zcl_mdm_prod_change( io_model = mo_model )->execute( ).
  ENDMETHOD.


  METHOD update_reason_code.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update reason code
    "*&
    "*&
    "********************************************************************
    IF    cs_section_3-reason_code01 = 01
       OR cs_section_3-reason_code01 = 02
       OR cs_section_3-reason_code01 = 03.
      CLEAR: cs_section_3-indenttab01,
             cs_section_3-description01,
             cs_section_3-template01,
             cs_section_3-indenttab02,
             cs_section_3-description02,
             cs_section_3-template02,
             cs_section_3-indenttab03,
             cs_section_3-description03,
             cs_section_3-template03,
             cs_section_3-indenttab04,
             cs_section_3-description04,
             cs_section_3-template04,
             cs_section_3-indenttab05,
             cs_section_3-description05,
             cs_section_3-template05,
             cs_section_3-t_indenttab01,
             cs_section_3-t_indenttab02,
             cs_section_3-t_indenttab03,
             cs_section_3-t_indenttab04,
             cs_section_3-t_indenttab05.

      cs_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
      RETURN.
    ELSE.
      IF cs_section_3-indenttab01 IS INITIAL.
        cs_section_3-indenttab01 = '001'.
        DATA(ls_num) = VALUE #( me->mo_model->master_data-numbers[ selnum = cs_section_3-indenttab01 ] OPTIONAL ) ##WARN_OK.
        cs_section_3-description01 = ls_num-number_description.
      ENDIF.
    ENDIF.

    IF     cs_section_1-zzserial      <> 2
       AND cs_section_3-reason_code01 IS INITIAL.
      CLEAR: cs_section_3-indenttab01,
             cs_section_3-description01.
    ENDIF.

    IF cs_section_1-zzserial <> 2 AND me->mo_model->master_data-marc-zz1_identtable01_plt IS INITIAL.
      " Scenario 1- if Prod. Hierachry SN-managed = NO and  Product SN-managed = NO
      cs_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
      IF cs_section_3-reason_code01 = 04.
        cs_section_3-indenttab01 = '001'.
        ls_num = VALUE #( me->mo_model->master_data-numbers[ selnum = cs_section_3-indenttab01 ] OPTIONAL ) ##WARN_OK.
        cs_section_3-description01 = ls_num-number_description.
      ENDIF.
    ELSEIF cs_section_1-zzserial = 2 AND me->mo_model->master_data-marc-zz1_identtable01_plt IS INITIAL ##BOOL_OK.
      " Scenario 2- if Prod. Hierachry SN-managed = YES and  Product SN-managed = NO, not available on product
      " Scenario 3- if Prod. Hierachry SN-managed = YES and  Product SN-managed = NO, Master data incorrect, SN available on product
      cs_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-red_flag.
      CLEAR: cs_section_3-indenttab01,
             cs_section_3-description01.
      IF cs_section_3-reason_code01 = 04.
        cs_section_3-indenttab01 = '001'.
        ls_num = VALUE #( me->mo_model->master_data-numbers[ selnum = cs_section_3-indenttab01 ] OPTIONAL ) ##WARN_OK.
        cs_section_3-description01 = ls_num-number_description.
        cs_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
      ELSE.
        IF cs_section_3-reason_code01 IS NOT INITIAL.
          CLEAR: cs_section_3-indenttab01,
                 cs_section_3-description01.
          cs_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
        ENDIF.
      ENDIF.

    ELSEIF cs_section_1-zzserial = 2 AND me->mo_model->master_data-marc-zz1_identtable01_plt IS NOT INITIAL.
      " Scenario 4- if Prod. Hierachry SN-managed = YES and  Product SN-managed = YES
      cs_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.

    ELSEIF cs_section_1-zzserial <> 2 AND me->mo_model->master_data-marc-zz1_identtable01_plt IS NOT INITIAL.
      " Scenario 5- if Prod. Hierachry SN-managed = NO and  Product SN-managed = YES
      cs_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-red_flag.
      IF cs_section_3-reason_code01 = 04.
        cs_section_3-indenttab01 = '001'.
        ls_num = VALUE #( me->mo_model->master_data-numbers[ selnum = cs_section_3-indenttab01 ] OPTIONAL ) ##WARN_OK.
        cs_section_3-description01 = ls_num-number_description.
        cs_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.
      ELSE.
        IF cs_section_3-reason_code01 IS NOT INITIAL.

          cs_section_1-serial_oblg_icon = zif_c_mdm_tool=>c_icon-green_flag.

          CLEAR:
                 cs_section_3-indenttab02,
                 cs_section_3-description02,
                 cs_section_3-template02,
                 cs_section_3-indenttab03,
                 cs_section_3-description03,
                 cs_section_3-template03,
                 cs_section_3-indenttab04,
                 cs_section_3-description04,
                 cs_section_3-template04,
                 cs_section_3-indenttab05,
                 cs_section_3-description05,
                 cs_section_3-template05,
                 cs_section_3-t_indenttab02,
                 cs_section_3-t_indenttab03,
                 cs_section_3-t_indenttab04,
                 cs_section_3-t_indenttab05.

          cs_section_3-indenttab02 = mo_model->master_data-marc-zz1_identtable02_plt.
          cs_section_3-indenttab03 = mo_model->master_data-marc-zz1_identtable03_plt.
          cs_section_3-indenttab04 = mo_model->master_data-marc-zz1_identtable04_plt.
          cs_section_3-indenttab05 = mo_model->master_data-marc-zz1_identtable05_plt.

          CLEAR ls_num.
          ls_num = VALUE #( me->mo_model->master_data-numbers[ selnum = cs_section_3-indenttab02 ] OPTIONAL ) ##WARN_OK.

          cs_section_3-description02 = ls_num-number_description.
          CLEAR ls_num.
          ls_num = VALUE #( me->mo_model->master_data-numbers[ selnum = cs_section_3-indenttab03 ] OPTIONAL ) ##WARN_OK.

          cs_section_3-description03 = ls_num-number_description.
          CLEAR ls_num.
          ls_num = VALUE #( me->mo_model->master_data-numbers[ selnum = cs_section_3-indenttab04 ] OPTIONAL ) ##WARN_OK.

          cs_section_3-description04 = ls_num-number_description.
          CLEAR ls_num.
          ls_num = VALUE #( me->mo_model->master_data-numbers[ selnum = cs_section_3-indenttab05 ] OPTIONAL ) ##WARN_OK.

          cs_section_3-description05 = ls_num-number_description.

        ENDIF.

      ENDIF.
    ENDIF.

*    "In case there is no record found in the table, we have a new situation. The situation “we don’t give a sh.t”.

    " In case there is no record found in the table, we have a new situation. The situation “we don’t give a sh.t”.
    IF me->mo_model->master_data-serial_check IS INITIAL.
      cs_section_1-serial_oblg_icon = ''.
    ENDIF.
  ENDMETHOD.


  METHOD update_units_of_measure.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update unit of measure
    "*&
    "*&
    "********************************************************************
    DATA ls_clientdata      TYPE bapi_mara.
    DATA ls_clientdatax     TYPE bapi_marax.
    DATA ls_plantdata       TYPE bapi_marc.
    DATA ls_plantdatax      TYPE bapi_marcx ##NEEDED.
    DATA ls_unitsofmeasure  TYPE bapi_marm.
    DATA ls_unitsofmeasurex TYPE bapi_marmx.
    DATA lt_unitsofmeasure  TYPE STANDARD TABLE OF bapi_marm.
    DATA lt_unitsofmeasurex TYPE STANDARD TABLE OF bapi_marmx.
    DATA lv_material18      TYPE char18.
    DATA ls_return          TYPE bapiret2.

    DATA ls_extensionin     TYPE bapiparex.
    DATA ls_extensioninx    TYPE bapiparexx.
    DATA lt_extensionin     TYPE TABLE OF bapiparex.
    DATA lt_extensioninx    TYPE TABLE OF bapiparexx.

    DATA(ls_mara) = mo_model->master_data-mara.
    DATA(lt_marm) = mo_model->master_data-marm.
    DATA(ls_marc) = mo_model->master_data-marc.

    fill_str( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_mara
                        iv_file    = ls_mara
              IMPORTING ev_str     = ls_clientdata ).

    ASSIGN COMPONENT zif_c_mdm_tool=>c_fieldnames-prdha OF STRUCTURE ls_mara TO FIELD-SYMBOL(<ls_prdha>).
    IF <ls_prdha> IS ASSIGNED.
      ls_clientdata-prod_hier  = <ls_prdha>.
      ls_clientdatax-prod_hier = abap_true.
    ENDIF.
    fill_xstr( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_mara
                         iv_str     = ls_clientdata
               IMPORTING ev_xstr    = ls_clientdatax ).

    READ TABLE me->mo_model->ms_midlle_block-t_unit_of_measures ASSIGNING FIELD-SYMBOL(<fs_unit_of_measures_pc>) WITH KEY id = zif_c_mdm_tool=>c_units-piece.
    IF <fs_unit_of_measures_pc> IS ASSIGNED.
      ls_clientdata-net_weight = <fs_unit_of_measures_pc>-ntgew.
      ls_unitsofmeasure-unit_of_wt     = <fs_unit_of_measures_pc>-gewei.
      ls_unitsofmeasure-unit_of_wt_iso = <fs_unit_of_measures_pc>-gewei.

      ls_clientdatax-net_weight     = abap_true.
      ls_clientdatax-unit_of_wt     = abap_true.
      ls_clientdatax-unit_of_wt_iso = abap_true.
    ENDIF.

    READ TABLE lt_marm ASSIGNING FIELD-SYMBOL(<fs_marm_pc>) WITH KEY meinh = zif_c_mdm_tool=>c_units-piece.

    LOOP AT lt_marm ASSIGNING FIELD-SYMBOL(<fs_marm>).
      <fs_marm>-capause = <fs_marm>-umrez.

      fill_str( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_marm
                          iv_file    = <fs_marm>
                IMPORTING ev_str     = ls_unitsofmeasure ).
      fill_xstr( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_marm
                           iv_str     = ls_unitsofmeasure
                 IMPORTING ev_xstr    = ls_unitsofmeasurex ).

      READ TABLE me->mo_model->ms_midlle_block-t_unit_of_measures ASSIGNING FIELD-SYMBOL(<fs_unit_of_measures>) WITH KEY id = <fs_marm>-meinh.
      IF sy-subrc = 0.
        IF <fs_unit_of_measures>-meinh = <fs_marm>-meinh.
          ls_unitsofmeasure-numerator = <fs_unit_of_measures>-umrez.
          IF <fs_marm>-meinh+0(1) = 'Z'.
            ls_unitsofmeasure-length         = <fs_unit_of_measures>-laeng.
            ls_unitsofmeasure-width          = <fs_unit_of_measures>-breit.
            ls_unitsofmeasure-height         = <fs_unit_of_measures>-hoehe.
            ls_unitsofmeasure-ean_upc        = <fs_unit_of_measures>-ean11.
            ls_unitsofmeasure-ean_cat        = 'HE'.
            ls_unitsofmeasure-volumeunit     = zif_c_mdm_tool=>c_units-cdm.
            ls_unitsofmeasure-volumeunit_iso = zif_c_mdm_tool=>c_units-cdm.
            convert_volume( CHANGING cs_unitsofmeasure = ls_unitsofmeasure ).

            ls_unitsofmeasurex-length         = abap_true.
            ls_unitsofmeasurex-width          = abap_true.
            ls_unitsofmeasurex-height         = abap_true.
            ls_unitsofmeasurex-volumeunit     = abap_true.
            ls_unitsofmeasurex-volumeunit_iso = abap_true.
            ls_unitsofmeasurex-volume         = abap_true.
            ls_unitsofmeasurex-ean_upc        = abap_true.
            ls_unitsofmeasurex-ean_cat        = abap_true.
          ENDIF.

          IF <fs_marm>-meinh = zif_c_mdm_tool=>c_units-piece.

            ls_unitsofmeasure-length         = <fs_unit_of_measures>-laeng.
            ls_unitsofmeasure-width          = <fs_unit_of_measures>-breit.
            ls_unitsofmeasure-height         = <fs_unit_of_measures>-hoehe.
            ls_unitsofmeasure-ean_upc        = <fs_unit_of_measures>-ean11.
            ls_unitsofmeasure-ean_cat        = 'HE'.
            ls_unitsofmeasure-gross_wt       = <fs_unit_of_measures>-brgew.
            ls_unitsofmeasure-unit_dim       = <fs_unit_of_measures>-meabm.
            ls_unitsofmeasure-unit_of_wt     = <fs_unit_of_measures>-gewei.
            ls_unitsofmeasure-unit_of_wt_iso = <fs_unit_of_measures>-gewei.

            IF <fs_marm_pc>-laeng <> <fs_unit_of_measures>-laeng.
              DATA(lv_update_volume) = abap_true.
            ENDIF.

            IF <fs_marm_pc>-breit <> <fs_unit_of_measures>-breit.
              lv_update_volume = abap_true.
            ENDIF.

            IF <fs_marm_pc>-hoehe <> <fs_unit_of_measures>-hoehe.
              lv_update_volume = abap_true.
            ENDIF.

            IF <fs_marm_pc>-meabm <> <fs_unit_of_measures>-meabm.
              lv_update_volume = abap_true.
            ENDIF.
            IF lv_update_volume IS NOT INITIAL.
              ls_unitsofmeasure-volumeunit     = mo_model->ms_t340d-voleh.
              ls_unitsofmeasure-volumeunit_iso = mo_model->ms_t340d-voleh.
              ls_unitsofmeasure-unit_dim       = COND #( WHEN <fs_unit_of_measures>-meabm IS NOT INITIAL
                                                         THEN <fs_unit_of_measures>-meabm
                                                         ELSE zif_c_mdm_tool=>c_units-mm ).
              ls_unitsofmeasure-unit_dim_iso   = COND #( WHEN <fs_unit_of_measures>-meabm IS NOT INITIAL
                                                         THEN <fs_unit_of_measures>-meabm
                                                         ELSE zif_c_mdm_tool=>c_units-mm ).
              ls_unitsofmeasure-length         = <fs_unit_of_measures>-laeng.
              ls_unitsofmeasure-width          = <fs_unit_of_measures>-breit.
              ls_unitsofmeasure-height         = <fs_unit_of_measures>-hoehe.
              convert_volume( CHANGING cs_unitsofmeasure = ls_unitsofmeasure ).

            ENDIF.

            ls_unitsofmeasurex-length         = abap_true.
            ls_unitsofmeasurex-width          = abap_true.
            ls_unitsofmeasurex-height         = abap_true.
            ls_unitsofmeasurex-ean_upc        = abap_true.
            ls_unitsofmeasurex-ean_cat        = abap_true.
            ls_unitsofmeasurex-gross_wt       = abap_true.
            ls_unitsofmeasurex-unit_dim       = abap_true.
            ls_unitsofmeasurex-unit_of_wt     = abap_true.
            ls_unitsofmeasurex-unit_of_wt_iso = abap_true.
            ls_unitsofmeasurex-volumeunit     = abap_true.
            ls_unitsofmeasurex-volumeunit_iso = abap_true.

          ENDIF.

          IF     <fs_marm>-meinh             = zif_c_mdm_tool=>c_units-palet
             AND <fs_unit_of_measures>-pmat IS NOT INITIAL.

            lv_material18 = |{ <fs_unit_of_measures>-pmat ALPHA = OUT }|.
            lv_material18 = |{ lv_material18 ALPHA = IN }|.
            DATA(lv_pmat_id) = read_matid_from_product( is_matnr = CONV #( lv_material18 ) )-matid.
            DATA(ls_pmat_details) = mo_model->get_mat_details_from_matid( lv_pmat_id ).

            ls_unitsofmeasure-length = ls_pmat_details-pac-maxl.
            ls_unitsofmeasure-width  = ls_pmat_details-pac-maxb.
            ls_unitsofmeasure-height = ls_pmat_details-pac-maxh.

            calculate_weight_volume( EXPORTING it_unit_of_measures = mo_model->ms_midlle_block-t_unit_of_measures
                                     IMPORTING es_unitsofmeasure   = DATA(ls_converted_uom) ) ##NEEDED.

            ls_unitsofmeasure-unit_dim       = ls_converted_uom-unit_dim.
            ls_unitsofmeasure-volumeunit     = ls_converted_uom-volumeunit.
            ls_unitsofmeasure-volumeunit_iso = ls_converted_uom-volumeunit_iso.
            ls_unitsofmeasure-volume         = ls_converted_uom-volume.
            ls_unitsofmeasure-gross_wt       = ls_converted_uom-gross_wt.
            ls_unitsofmeasure-unit_of_wt     = ls_converted_uom-unit_of_wt.
            ls_unitsofmeasure-unit_of_wt_iso = ls_converted_uom-unit_of_wt_iso.

            ls_unitsofmeasurex-length         = abap_true.
            ls_unitsofmeasurex-width          = abap_true.
            ls_unitsofmeasurex-height         = abap_true.
            ls_unitsofmeasurex-unit_dim       = abap_true.
            ls_unitsofmeasurex-volumeunit     = abap_true.
            ls_unitsofmeasurex-volumeunit_iso = abap_true.
            ls_unitsofmeasurex-volume         = abap_true.
            ls_unitsofmeasurex-gross_wt       = abap_true.
            ls_unitsofmeasurex-unit_of_wt     = abap_true.
            ls_unitsofmeasurex-unit_of_wt_iso = abap_true.
          ENDIF.

          APPEND ls_unitsofmeasure  TO lt_unitsofmeasure.
          APPEND ls_unitsofmeasurex TO lt_unitsofmeasurex.
        ELSE.
          APPEND ls_unitsofmeasure  TO lt_unitsofmeasure.
          APPEND ls_unitsofmeasurex TO lt_unitsofmeasurex.

          ls_unitsofmeasure-numerator    = <fs_unit_of_measures>-umrez.
          ls_unitsofmeasure-alt_unit     = <fs_unit_of_measures>-meinh.
          ls_unitsofmeasure-alt_unit_iso = <fs_unit_of_measures>-meinh.
          ls_unitsofmeasure-ean_upc      = <fs_unit_of_measures>-ean11.
          ls_unitsofmeasure-length       = <fs_unit_of_measures>-laeng.
          ls_unitsofmeasure-width        = <fs_unit_of_measures>-breit.
          ls_unitsofmeasure-height       = <fs_unit_of_measures>-hoehe.
          ls_unitsofmeasure-gross_wt     = <fs_unit_of_measures>-brgew.

          convert_volume( CHANGING cs_unitsofmeasure = ls_unitsofmeasure ).

          ls_unitsofmeasurex-alt_unit     = <fs_unit_of_measures>-meinh.
          ls_unitsofmeasurex-alt_unit_iso = <fs_unit_of_measures>-meinh.
          APPEND ls_unitsofmeasure  TO lt_unitsofmeasure.
          APPEND ls_unitsofmeasurex TO lt_unitsofmeasurex.
        ENDIF.
      ELSE.
        APPEND ls_unitsofmeasure  TO lt_unitsofmeasure.
        APPEND ls_unitsofmeasurex TO lt_unitsofmeasurex.
      ENDIF.
      CLEAR lv_update_volume.
    ENDLOOP.

    DATA(ls_marm_pallet) = VALUE #( me->mo_model->master_data-marm[ meinh = zif_c_mdm_tool=>c_units-palet ] OPTIONAL ).
    IF ls_marm_pallet IS INITIAL.
      READ TABLE me->mo_model->ms_midlle_block-t_unit_of_measures ASSIGNING <fs_unit_of_measures> WITH KEY id = zif_c_mdm_tool=>c_units-palet.
      IF     sy-subrc = 0
         AND <fs_unit_of_measures>-pmat  IS NOT INITIAL
         AND <fs_unit_of_measures>-umrez IS NOT INITIAL.

        MOVE-CORRESPONDING <fs_unit_of_measures> TO <fs_marm> ##ENH_OK.
        CLEAR: <fs_marm>-voleh,
               <fs_marm>-volum.

        fill_str( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_marm
                            iv_file    = <fs_marm>
                  IMPORTING ev_str     = ls_unitsofmeasure ).
        fill_xstr( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_marm
                             iv_str     = ls_unitsofmeasure
                   IMPORTING ev_xstr    = ls_unitsofmeasurex ) ##ENH_OK.

        ls_unitsofmeasure-numerator    = <fs_unit_of_measures>-umrez.
        ls_unitsofmeasure-alt_unit     = <fs_unit_of_measures>-meinh.
        ls_unitsofmeasure-alt_unit_iso = <fs_unit_of_measures>-meinh.
        ls_unitsofmeasure-ean_upc      = <fs_unit_of_measures>-ean11.

        IF <fs_unit_of_measures>-pmat IS NOT INITIAL.
          lv_material18 = |{ <fs_unit_of_measures>-pmat ALPHA = OUT }|.
          lv_material18 = |{ lv_material18 ALPHA = IN }|.
          lv_pmat_id = read_matid_from_product( is_matnr = CONV #( lv_material18 ) )-matid.
          ls_pmat_details = mo_model->get_mat_details_from_matid( lv_pmat_id ).
          ls_unitsofmeasure-length = ls_pmat_details-pac-maxl.
          ls_unitsofmeasure-width  = ls_pmat_details-pac-maxb.
          ls_unitsofmeasure-height = ls_pmat_details-pac-maxh.
        ELSE.
          ls_unitsofmeasure-length = <fs_unit_of_measures>-laeng.
          ls_unitsofmeasure-width  = <fs_unit_of_measures>-breit.
          ls_unitsofmeasure-height = <fs_unit_of_measures>-hoehe.
        ENDIF.

        ls_unitsofmeasure-ean_cat = ''.

        CLEAR ls_converted_uom.
        calculate_weight_volume( EXPORTING it_unit_of_measures = mo_model->ms_midlle_block-t_unit_of_measures
                                 IMPORTING es_unitsofmeasure   = ls_converted_uom ).

        ls_unitsofmeasure-unit_dim       = ls_converted_uom-unit_dim.
        ls_unitsofmeasure-volumeunit     = ls_converted_uom-volumeunit.
        ls_unitsofmeasure-volumeunit_iso = ls_converted_uom-volumeunit_iso.
        ls_unitsofmeasure-volume         = ls_converted_uom-volume.
        ls_unitsofmeasure-gross_wt       = ls_converted_uom-gross_wt.
        ls_unitsofmeasure-unit_of_wt     = ls_converted_uom-unit_of_wt.
        ls_unitsofmeasure-unit_of_wt_iso = ls_converted_uom-unit_of_wt_iso.
        ls_unitsofmeasurex-alt_unit     = <fs_unit_of_measures>-meinh.
        ls_unitsofmeasurex-alt_unit_iso = <fs_unit_of_measures>-meinh.

        APPEND ls_unitsofmeasure  TO lt_unitsofmeasure.
        APPEND ls_unitsofmeasurex TO lt_unitsofmeasurex.
      ENDIF.
    ENDIF.

    DATA(ls_uom) = VALUE #( me->mo_model->ms_midlle_block-t_unit_of_measures[
                                id = zif_c_mdm_tool=>c_units-mastercarton ] OPTIONAL ).
    DATA(ls_marm_master_carton) = VALUE #( me->mo_model->master_data-marm[ meinh = ls_uom-meinh ] OPTIONAL ).
    IF ls_marm_master_carton IS INITIAL.
      READ TABLE me->mo_model->ms_midlle_block-t_unit_of_measures ASSIGNING <fs_unit_of_measures> WITH KEY id = zif_c_mdm_tool=>c_units-mastercarton.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING <fs_unit_of_measures> TO <fs_marm> ##ENH_OK.
        CLEAR: <fs_marm>-voleh,
               <fs_marm>-volum.

        fill_str( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_marm
                            iv_file    = <fs_marm>
                  IMPORTING ev_str     = ls_unitsofmeasure ).
        fill_xstr( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_marm
                             iv_str     = ls_unitsofmeasure
                   IMPORTING ev_xstr    = ls_unitsofmeasurex ) ##ENH_OK.

        ls_unitsofmeasure-numerator    = <fs_unit_of_measures>-umrez.
        ls_unitsofmeasure-alt_unit     = <fs_unit_of_measures>-meinh.
        ls_unitsofmeasure-alt_unit_iso = <fs_unit_of_measures>-meinh.
        ls_unitsofmeasure-ean_upc      = <fs_unit_of_measures>-ean11.
        ls_unitsofmeasure-length       = <fs_unit_of_measures>-laeng.
        ls_unitsofmeasure-width        = <fs_unit_of_measures>-breit.
        ls_unitsofmeasure-height       = <fs_unit_of_measures>-hoehe.
        ls_unitsofmeasure-ean_cat      = 'HE'.

        convert_volume( CHANGING cs_unitsofmeasure = ls_unitsofmeasure ).
        ls_unitsofmeasure-volumeunit     = zif_c_mdm_tool=>c_units-cdm.
        ls_unitsofmeasure-volumeunit_iso = zif_c_mdm_tool=>c_units-cdm.

        ls_unitsofmeasure-gross_wt       = <fs_unit_of_measures>-brgew.

        ls_unitsofmeasurex-alt_unit     = <fs_unit_of_measures>-meinh.
        ls_unitsofmeasurex-alt_unit_iso = <fs_unit_of_measures>-meinh.

        APPEND ls_unitsofmeasure  TO lt_unitsofmeasure.
        APPEND ls_unitsofmeasurex TO lt_unitsofmeasurex.
      ENDIF.
    ENDIF.

    DATA(ls_headdata) = VALUE bapimathead(
        material_long = COND #( WHEN ls_mara-matnr IS NOT INITIAL THEN ls_mara-matnr ELSE ls_mara-matnr )
        ind_sector    = ls_mara-mbrsh
        matl_type     = ls_mara-mtart
        basic_view    = abap_true
        storage_view  = abap_true ).

    MOVE-CORRESPONDING me->mo_model->master_data-marc TO ls_marc.

    fill_str( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_marc
                        iv_file    = ls_marc
              IMPORTING ev_str     = ls_plantdata ).
    fill_xstr( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_marc
                         iv_str     = ls_plantdata
               IMPORTING ev_xstr    = ls_plantdatax ).

    ls_extensionin-structure       = zif_c_mdm_tool=>c_tabnames-bapi_te_marc.
    ls_extensionin-valuepart1+0(4) = mo_model->master_data-marc-werks.
    ls_extensionin-valuepart1+4(2) = mo_model->ms_top_block-sub_section_3-reason_code01.

    IF    me->mo_model->ms_top_block-sub_section_3-reason_code01 = 1
       OR me->mo_model->ms_top_block-sub_section_3-reason_code01 = 2
       OR me->mo_model->ms_top_block-sub_section_3-reason_code01 = 3.
      ls_extensionin-valuepart1+6(3)  = '   '.
      ls_extensionin-valuepart1+9(3)  = '   '.
      ls_extensionin-valuepart1+12(3) = '   '.
      ls_extensionin-valuepart1+15(3) = '   '.
      ls_extensionin-valuepart1+18(3) = '   '.
    ELSE.
      ls_extensionin-valuepart1+6(3)  = mo_model->ms_top_block-sub_section_3-indenttab01.
      ls_extensionin-valuepart1+9(3)  = mo_model->ms_top_block-sub_section_3-indenttab02.
      ls_extensionin-valuepart1+12(3) = mo_model->ms_top_block-sub_section_3-indenttab03.
      ls_extensionin-valuepart1+15(3) = mo_model->ms_top_block-sub_section_3-indenttab04.
      ls_extensionin-valuepart1+18(3) = mo_model->ms_top_block-sub_section_3-indenttab05.
    ENDIF.

    APPEND ls_extensionin TO lt_extensionin.

    ls_extensioninx-structure        = zif_c_mdm_tool=>c_tabnames-bapi_te_marcx.
    ls_extensioninx-valuepart1+0(4)  = mo_model->master_data-marc-werks.
    ls_extensioninx-valuepart1+4(1)  = abap_true.

    ls_extensioninx-valuepart1+5(1)  = abap_true.
    ls_extensioninx-valuepart1+6(1)  = abap_true.
    ls_extensioninx-valuepart1+7(1)  = abap_true.
    ls_extensioninx-valuepart1+8(1)  = abap_true.
    ls_extensioninx-valuepart1+9(1)  = abap_true.
    ls_extensioninx-valuepart1+10(1) = abap_true.
    APPEND ls_extensioninx TO lt_extensioninx.

    DATA(ls_palet) = VALUE #( me->mo_model->ms_midlle_block-t_unit_of_measures[ id = zif_c_mdm_tool=>c_units-palet ] OPTIONAL ).
    IF     ls_palet-pmat  IS NOT INITIAL
       AND ls_palet-umrez IS NOT INITIAL.

      CLEAR: ls_extensionin,
             ls_extensioninx.
      ls_extensionin-structure       = zif_c_mdm_tool=>c_tabnames-bapi_te_marm.
      ls_extensionin-valuepart1+0(3) = ls_palet-meinh.
      ls_extensionin-valuepart1+3(6) = ls_palet-meinh.
      ls_extensionin-valuepart1+6(4) = ls_palet-hutyp.
      APPEND ls_extensionin TO lt_extensionin.

      ls_extensioninx-structure       = zif_c_mdm_tool=>c_tabnames-bapi_te_marmx.
      ls_extensioninx-valuepart1+0(3) = ls_palet-meinh.
      ls_extensioninx-valuepart1+3(6) = ls_palet-meinh.
      ls_extensioninx-valuepart1+6(1) = abap_true.
      APPEND ls_extensioninx TO lt_extensioninx.

    ENDIF.

    IF mo_model->ms_top_block-sub_section_1-sled_bbd = abap_true.
      ls_clientdata-sled_bbd = 'E'.
      ls_clientdatax-sled_bbd = abap_true.

      ls_clientdata-minremlife = mo_model->ms_top_block-sub_section_1-mhdrz.
      ls_clientdatax-minremlife = abap_true.
    ELSE.
      ls_clientdata-sled_bbd = 'B'.
      ls_clientdatax-sled_bbd = abap_true.

      ls_clientdata-minremlife = mo_model->ms_top_block-sub_section_1-mhdrz.
      ls_clientdatax-minremlife = abap_true.
    ENDIF.

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata        = ls_headdata
        clientdata      = ls_clientdata
        clientdatax     = ls_clientdatax
        plantdata       = ls_plantdata
        plantdatax      = ls_plantdatax
      IMPORTING
        return          = ls_return
      TABLES
        unitsofmeasure  = lt_unitsofmeasure
        unitsofmeasurex = lt_unitsofmeasurex
        extensionin     = lt_extensionin
        extensioninx    = lt_extensioninx.

    rs_return = ls_return.
  ENDMETHOD.


  METHOD value_request_dispatchable.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for dispatchable field
    "*&
    "*&
    "********************************************************************
    DATA rt_search_help_return TYPE TABLE OF ddshretval.
    DATA lv_field_type         TYPE cfd_field_type VALUE 'LIST'.
    DATA lv_user_input         TYPE help_info-fldvalue.

    TRY.
        cl_cfd_runtime_vh=>get_buffer_data( EXPORTING iv_value_help_cds_view_name = 'ZZ1_DISP_V'
                                            IMPORTING es_buffer_data              = DATA(ls_buffer_data)
                                                      et_value_help_fields        = DATA(lt_value_help_fields)
                                                      et_value_help_components    = DATA(lt_value_help_components) ) ##NEEDED.
      CATCH cx_cfd_runtime INTO DATA(lx_previous) ##NEEDED.
        RETURN.
    ENDTRY.

    TRY.
        DATA(lt_value_help_ddic_fields) = cl_cfd_runtime_util=>get_value_help_ddic_fields(
          iv_field_type            = lv_field_type
          it_value_help_components = lt_value_help_components ).
      CATCH cx_cfd_runtime.
        RETURN.
    ENDTRY.

    DATA(rs_search_help_definition) = cl_cfd_sap_gui_utility=>get_initial_shlp_description(
      iv_search_help_name       = |{ lv_field_type }| " Field Type (max. char 10)
      iv_cds_view_name          = 'ZZ1_DISP_V'
      it_value_help_ddic_fields = lt_value_help_ddic_fields ).

    LOOP AT lt_value_help_ddic_fields REFERENCE INTO DATA(lr_field).
      IF lr_field->position = 1.

        INSERT VALUE #( shlpfield = lr_field->fieldname   " For None-SBOs take always first field (Key field) whose value shall be returned
                        valfield  = '~'                   " must be filled, otherwise return values do not have the selected entry
                        value     = lv_user_input )
               INTO TABLE rs_search_help_definition-interface.
      ENDIF.

      INSERT VALUE #( fieldname  = lr_field->fieldname
                      shlpinput  = xsdbool( lr_field->position = 1 ) " For None-SBOs take always first field (Key field) as Input field
                      shlpoutput = abap_true
                      shlpselpos = lr_field->position
                      shlplispos = lr_field->position )
             INTO TABLE rs_search_help_definition-fieldprop.
    ENDLOOP.

    "
    CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
      EXPORTING
        shlp          = rs_search_help_definition
        disponly      = abap_false
      TABLES
        return_values = rt_search_help_return.

    IF lines( rt_search_help_return ) = 1.
      cv_disp_whd = rt_search_help_return[ 1 ]-fieldval. " No conversion to internal representation necessary as this is done in the PBO of the dynpro!
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.


  METHOD variant_delete.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Delete existing variant to overwrite new variant
    "*&
    "*&
    "********************************************************************
    CALL FUNCTION 'RS_VARIANT_DELETE'
      EXPORTING
        report               = iv_report_name
        variant              = iv_variant
        flag_confirmscreen   = abap_true
        flag_delallclient    = abap_true
        suppress_message     = abap_true
      EXCEPTIONS
        not_authorized       = 1
        not_executed         = 2
        no_report            = 3
        report_not_existent  = 4
        report_not_supplied  = 5
        variant_locked       = 6
        variant_not_existent = 7
        no_corr_insert       = 8
        variant_protected    = 9
        OTHERS               = 10 ##FM_SUBRC_OK.
    IF sy-subrc <> 0 ##NEEDED.
    ENDIF.
  ENDMETHOD.


  METHOD volume_ind_value_help.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Value help for volume indicator
    "*&
    "*&
    "********************************************************************
    DATA lt_ddshifaces TYPE ddshifaces.

    lt_ddshifaces = VALUE #(
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-dimind valfield  = abap_true )
        ( shlpfield = zif_c_mdm_tool=>c_fieldnames-lgnum value     = mo_model->ms_top_block-sub_section_1-lgnum   dispfield = abap_true ) ).

    f4_with_customized_params( EXPORTING iv_shlp_name  = zif_c_mdm_tool=>c_value_help-volind
                                         it_ddshifaces = lt_ddshifaces
                               IMPORTING et_values     = DATA(lt_values) ).

    IF line_exists( lt_values[ 1 ] ).
      cv_volind = lt_values[ 1 ]-fieldval.
      trigger_enter( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
