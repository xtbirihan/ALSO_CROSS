*&---------------------------------------------------------------------*
*& Include Z_MDM_TOOL_PAI_0104
*&---------------------------------------------------------------------*
MODULE tc_unit_of_meas_modify INPUT.
  IF gs_scr-middle_block-s_unit_of_measures-scanned_value IS INITIAL.
    EXIT.
  ENDIF.
  go_controller->modify_unit_of_measures(
    EXPORTING
      is_unit_of_measures = gs_scr-middle_block-s_unit_of_measures
      iv_current_line     = tc_unit_of_meas-current_line
    CHANGING
      ct_unit_of_measures = gs_scr-middle_block-t_unit_of_measures ).
ENDMODULE.
MODULE check_quantities.
  IF gs_scr-middle_block-s_unit_of_measures-scanned_value IS INITIAL.
    EXIT.
  ENDIF.
  TRY.
      go_controller->check_quantities( CHANGING cs_middle_block = gs_scr-middle_block-s_unit_of_measures ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      CLEAR: gv_okcode.
      MESSAGE go_mdm_tool TYPE go_mdm_tool->if_t100_dyn_msg~msgty.
  ENDTRY.

  go_controller->calculate_pallet(
    EXPORTING
      it_unit_of_measures = gs_scr-middle_block-t_unit_of_measures
      iv_current_line     = tc_unit_of_meas-current_line
    CHANGING
      cs_unit_of_measures = gs_scr-middle_block-s_unit_of_measures ).


  go_controller->modify_unit_of_measures(
    EXPORTING
      is_unit_of_measures = gs_scr-middle_block-s_unit_of_measures
      iv_current_line     = tc_unit_of_meas-current_line
    CHANGING
      ct_unit_of_measures = gs_scr-middle_block-t_unit_of_measures ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_EAN  INPUT
*&---------------------------------------------------------------------*
MODULE check_pack_material INPUT.
  IF gs_scr-middle_block-s_unit_of_measures-scanned_value IS INITIAL.
    EXIT.
  ENDIF.
  TRY.
      go_controller->check_pack_material( CHANGING cs_middle_block = gs_scr-middle_block-s_unit_of_measures ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      CLEAR: gv_okcode.
      MESSAGE go_mdm_tool TYPE go_mdm_tool->if_t100_dyn_msg~msgty.
  ENDTRY.

ENDMODULE.

MODULE check_ean INPUT.
  IF gs_scr-middle_block-s_unit_of_measures-scanned_value IS INITIAL.
    EXIT.
  ENDIF.
  TRY.
      go_controller->check_ean11( is_middle_block = gs_scr-middle_block-s_unit_of_measures ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      CLEAR: gv_okcode.
      MESSAGE go_mdm_tool TYPE go_mdm_tool->if_t100_dyn_msg~msgty.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  REFRESH_CURRENT_LINE  INPUT
*&---------------------------------------------------------------------*
MODULE refresh_current_line INPUT ##NEEDED.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_SCANNED_VALUE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_scanned_value INPUT.
  IF gs_scr-middle_block-s_unit_of_measures-scanned_value IS INITIAL.
    EXIT.
  ENDIF.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_PMAT  INPUT
*&---------------------------------------------------------------------*
MODULE value_request_pmat INPUT.
  go_controller->pmat_value_help( CHANGING cv_pmat = gs_scr-middle_block-s_unit_of_measures-pmat ).
ENDMODULE.

MODULE calculate_net_weight.
  go_controller->calculate_net_weight(
    EXPORTING
      it_unit_of_measures = gs_scr-middle_block-t_unit_of_measures
      iv_current_line     = tc_unit_of_meas-current_line
    CHANGING
      cs_unit_of_measures = gs_scr-middle_block-s_unit_of_measures ).

  go_controller->calculate_pallet(
    EXPORTING
      it_unit_of_measures = gs_scr-middle_block-t_unit_of_measures
      iv_current_line     = tc_unit_of_meas-current_line
    CHANGING
      cs_unit_of_measures = gs_scr-middle_block-s_unit_of_measures ).

  go_controller->modify_unit_of_measures(
    EXPORTING
      is_unit_of_measures = gs_scr-middle_block-s_unit_of_measures
      iv_current_line     = tc_unit_of_meas-current_line
    CHANGING
      ct_unit_of_measures = gs_scr-middle_block-t_unit_of_measures ).
ENDMODULE.
