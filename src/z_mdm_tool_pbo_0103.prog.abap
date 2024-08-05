*----------------------------------------------------------------------*
***INCLUDE Z_MDM_TOOL_PBO_0103.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0103 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0103 OUTPUT.
  go_controller->modify_screen_103( ).
  go_controller->set_cursor_field( CHANGING cs_tabstrip = gs_ts1 ).
  go_controller->set_template_icon( CHANGING cs_sub_section_3 = gs_scr-top_block-sub_section_3 ).
ENDMODULE.
