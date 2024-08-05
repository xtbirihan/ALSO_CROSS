*&---------------------------------------------------------------------*
*& Include          Z_MDM_TOOL_PBO_MAIN
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  TRY.
      CREATE OBJECT go_object TYPE (zif_c_mdm_tool=>c_class-controller)
        EXPORTING
          iv_repid    = sy-repid.
    CATCH cx_root INTO DATA(gx_root) ##CATCH_ALL.
      MESSAGE gx_root TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.

  TRY.
      go_controller = CAST #( go_object ).
    CATCH cx_sy_move_cast_error INTO DATA(gx_sy_move_cast_error).
      MESSAGE gx_sy_move_cast_error TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.

  go_controller->get_parameter_from_caller( CHANGING cv_scanned_value = gs_scr-top_block-sub_section_1-scanned_value ).
  CALL SCREEN zif_c_mdm_tool=>c_screen-ms_100.
