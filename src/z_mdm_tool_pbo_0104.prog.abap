*&---------------------------------------------------------------------*
*& Include Z_MDM_TOOL_PBO_0104
*&---------------------------------------------------------------------*

MODULE tc_unit_of_meas_change_tc_attr OUTPUT.
  DESCRIBE TABLE gs_scr-middle_block-t_unit_of_measures LINES tc_unit_of_meas-lines.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module DYNAMIC_MODIFY OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE dynamic_modify_screen_104 OUTPUT.

  go_controller->modify_screen_104(
    EXPORTING
      is_unit_of_measures = gs_scr-middle_block-s_unit_of_measures
    CHANGING
      cs_table_control    = tc_unit_of_meas ).

  go_controller->set_cursor_field(
    EXPORTING
      is_table_control    = tc_unit_of_meas
      is_unit_of_measures = gs_scr-middle_block-s_unit_of_measures
    CHANGING
      cs_tabstrip         = gs_ts1 ).

  gs_scr-middle_block-s_unit_of_measures-scanned_value =  gs_scr-top_block-sub_section_1-scanned_value.

  MODIFY gs_scr-middle_block-t_unit_of_measures
         FROM gs_scr-middle_block-s_unit_of_measures
         INDEX tc_unit_of_meas-current_line
         TRANSPORTING scanned_value.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0104 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0104 OUTPUT.
  go_controller->modify_screen_104_buttons( ).
ENDMODULE.
