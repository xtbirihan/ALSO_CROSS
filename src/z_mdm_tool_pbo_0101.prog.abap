*----------------------------------------------------------------------*
***INCLUDE Z_MDM_TOOL_PBO_0101.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0101 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0101 OUTPUT.
  go_controller->mo_model->set_model_data(
    CHANGING
      cs_screen = gs_scr ).

  go_controller->mv_is_cursor_set = abap_true.
  go_controller->set_cursor_field( CHANGING cs_tabstrip = gs_ts1 ).
  go_controller->modify_screen_101( ).
ENDMODULE.
