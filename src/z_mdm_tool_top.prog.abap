*&---------------------------------------------------------------------*
*& Include          Z_MDM_TOOL_TOP
*&---------------------------------------------------------------------*
CONTROLS: tc_hazdrs_mat TYPE TABLEVIEW USING SCREEN 0102.

DATA:
  gv_okcode     TYPE sy-ucomm,
  go_object     TYPE REF TO object,
  go_controller TYPE REF TO zcl_mdm_controller,
  gv_ss_102     TYPE sy-dynnr VALUE zif_c_mdm_tool=>c_screen-ss_900,
  gv_ss_103     TYPE sy-dynnr VALUE zif_c_mdm_tool=>c_screen-ss_900,
  gv_ss_104     TYPE sy-dynnr VALUE zif_c_mdm_tool=>c_screen-ss_900,
  gv_ss_105     TYPE sy-dynnr VALUE zif_c_mdm_tool=>c_screen-ss_900,
  gv_ss_106     TYPE sy-dynnr VALUE zif_c_mdm_tool=>c_screen-ss_900,
  gv_ss_107     TYPE sy-dynnr VALUE zif_c_mdm_tool=>c_screen-ss_900.

DATA: BEGIN OF gs_scr.
DATA: BEGIN OF top_block .
DATA: sub_section_1 TYPE zstr_product_characteristics.
DATA: sub_section_2 TYPE zstr_material_hazardous_info.
DATA: sub_section_3 TYPE zstr_ident_numbers.
DATA: END OF top_block.
DATA: BEGIN OF middle_block .
DATA: t_unit_of_measures     TYPE ztt_unit_of_measueres.
DATA: s_unit_of_measures     TYPE zstr_unit_of_measures.
DATA: s_packspec_header      TYPE zstr_packspec_header.
DATA: END OF middle_block.
DATA: BEGIN OF lower_block.
DATA: s_warehouse_data       TYPE zstr_warehouse_data.
DATA: s_storage_type_data    TYPE zstr_storage_type_data.
DATA: t_storage_bin_type_sel TYPE TABLE OF zstr_storage_type_bin_type_sel.
DATA: s_material_texts       TYPE zstr_material_texts.
DATA: END OF lower_block.
DATA: s_master_carton_aditional TYPE zstr_additional_data.
DATA: END OF gs_scr.

CONTROLS: tc_unit_of_meas   TYPE TABLEVIEW USING SCREEN 0104.

CONTROLS:  ts1 TYPE TABSTRIP.
DATA: BEGIN OF gs_ts1,
        subscreen   LIKE sy-dynnr,
        prog        LIKE sy-repid VALUE 'Z_MDM_TOOL',
        pressed_tab LIKE sy-ucomm VALUE zif_c_mdm_tool=>c_tabs_area_mat-tab1,
      END OF gs_ts1.
