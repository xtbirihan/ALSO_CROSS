*----------------------------------------------------------------------*
***INCLUDE Z_MDM_TOOL_PAI_0101.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  HANDLE_SCANNED_VALUE  INPUT
*&---------------------------------------------------------------------*
MODULE handle_scanned_value INPUT.
  IF gv_okcode IS NOT INITIAL.
    RETURN.
  ENDIF.

  DATA(lv_cursorfield) = go_controller->get_cursor_field( ).
  IF lv_cursorfield NE zif_c_mdm_tool=>c_fieldnames-screen_scanned_value.
    RETURN.
  ENDIF.

  go_controller->mo_model->set_model_data(
    CHANGING
      cs_screen = gs_scr ).

  TRY.
      go_controller->mo_model->read_scanned_value( iv_scanned_value = gs_scr-top_block-sub_section_1-scanned_value ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.

  go_controller->mo_model->get_model_data(
    CHANGING
      cs_screen = gs_scr ).

  IF gs_scr-top_block-sub_section_1-scanned_value IS NOT INITIAL.
    gv_ss_102 = zif_c_mdm_tool=>c_screen-ss_102.
    gv_ss_103 = zif_c_mdm_tool=>c_screen-ss_103.
    gv_ss_104 = zif_c_mdm_tool=>c_screen-ss_104.
    gv_ss_105 = zif_c_mdm_tool=>c_screen-ss_105.
    gv_ss_106 = zif_c_mdm_tool=>c_screen-ss_106.
    gv_ss_107 = zif_c_mdm_tool=>c_screen-ss_107.
  ENDIF.

ENDMODULE.

MODULE value_entitled  INPUT.
  go_controller->entitled_value_help( ).
ENDMODULE.
