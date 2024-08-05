*----------------------------------------------------------------------*
***INCLUDE Z_MDM_TOOL_PAI_0103.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  HANDLE_TEMPLATE_REASON_CODE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE handle_template_reason_code INPUT.
  go_controller->update_reason_code(
    CHANGING
      cs_section_1 = gs_scr-top_block-sub_section_1
      cs_section_3 = gs_scr-top_block-sub_section_3 ).
  go_controller->handle_template_reason_code( CHANGING cs_sub_section_3 = gs_scr-top_block-sub_section_3 ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_REASON_CODE  INPUT
*&---------------------------------------------------------------------*
MODULE update_reason_code INPUT.
  go_controller->update_reason_code(
    CHANGING
      cs_section_1 = gs_scr-top_block-sub_section_1
      cs_section_3 = gs_scr-top_block-sub_section_3
  ).
ENDMODULE.
MODULE update_bulk_storage.
  TRY.
      go_controller->update_bulk_storage(
        CHANGING
          cs_section_1 = gs_scr-top_block-sub_section_1 ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  UPDATE_REASON_CODE  INPUT
*&---------------------------------------------------------------------*
MODULE update_indenttab01 INPUT.
  TRY.
      go_controller->update_indenttab(
        EXPORTING
          iv_identnum  = zif_c_mdm_tool=>c_indent-tab01
        CHANGING
          cs_section_3 = gs_scr-top_block-sub_section_3 ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error
      DISPLAY LIKE zif_c_mdm_tool=>c_message_severity-warning.
  ENDTRY.
ENDMODULE.
MODULE update_indenttab02 INPUT.
  TRY.
      go_controller->update_indenttab(
        EXPORTING
          iv_identnum  = zif_c_mdm_tool=>c_indent-tab02
        CHANGING
          cs_section_3 = gs_scr-top_block-sub_section_3 ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.
ENDMODULE.

MODULE update_indenttab03 INPUT.
  TRY.
      go_controller->update_indenttab(
        EXPORTING
          iv_identnum  = zif_c_mdm_tool=>c_indent-tab03
        CHANGING
          cs_section_3 = gs_scr-top_block-sub_section_3 ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error
      DISPLAY LIKE zif_c_mdm_tool=>c_message_severity-warning.
  ENDTRY.
ENDMODULE.

MODULE update_indenttab04 INPUT.
  TRY.
      go_controller->update_indenttab(
        EXPORTING
          iv_identnum  = zif_c_mdm_tool=>c_indent-tab04
        CHANGING
          cs_section_3 = gs_scr-top_block-sub_section_3 ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error
      DISPLAY LIKE zif_c_mdm_tool=>c_message_severity-warning.
  ENDTRY.
ENDMODULE.
MODULE update_indenttab05 INPUT.
  TRY.
      go_controller->update_indenttab(
        EXPORTING
          iv_identnum  = zif_c_mdm_tool=>c_indent-tab05
        CHANGING
          cs_section_3 = gs_scr-top_block-sub_section_3 ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error
      DISPLAY LIKE zif_c_mdm_tool=>c_message_severity-warning.
  ENDTRY.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_INDENTTAB01  INPUT
*&---------------------------------------------------------------------*
MODULE value_request_indenttab01 INPUT.
  go_controller->indenttab01_value_help( CHANGING cv_identab01 = gs_scr-top_block-sub_section_3-indenttab01 ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_REASON_CODE01  INPUT
*&---------------------------------------------------------------------*
MODULE value_request_reason_code01 INPUT.
  go_controller->reasoncode01_value_help( CHANGING cv_reasoncode01 = gs_scr-top_block-sub_section_3-reason_code01 ).
ENDMODULE.
MODULE value_request_bulk_storage.
  go_controller->bulk_storage_value_help( CHANGING cv_bulk = gs_scr-top_block-sub_section_1-block ).
ENDMODULE.
