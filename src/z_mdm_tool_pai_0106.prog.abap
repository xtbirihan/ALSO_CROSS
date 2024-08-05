*----------------------------------------------------------------------*
***INCLUDE Z_MDM_TOOL_PAI_0106.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  SET_MATLWHST_ATT_TEXTS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_matlwhst_att_texts INPUT.

  gs_scr-lower_block-s_storage_type_data-ltypt          = go_controller->mo_model->read_storage_type_text( gs_scr-lower_block-s_storage_type_data-lgtyp ).
  gs_scr-lower_block-s_storage_type_data-sectindt       = go_controller->mo_model->read_storage_section_ind_text( gs_scr-lower_block-s_storage_type_data-sectind ).
  gs_scr-lower_block-s_storage_type_data-bintypet       = go_controller->mo_model->read_storage_bin_type_text( gs_scr-lower_block-s_storage_type_data-bintype ).
  gs_scr-lower_block-s_storage_type_data-bintype_plan_t = go_controller->mo_model->read_storage_bin_type_text( gs_scr-lower_block-s_storage_type_data-bintype_plan ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_STORAGE_SECTION_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE create_storage_section_alv OUTPUT.
  go_controller->create_storage_section_alv( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_STORAGE_BIN_TYPE_ALV OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE create_storage_bin_type_alv OUTPUT.
  go_controller->create_storage_bin_type_alv( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_STORAGE_TYPE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE update_storage_type INPUT.
  READ TABLE go_controller->mo_model->master_data-storage_type
  INTO DATA(gs_matio_whst)
  WITH KEY lgtyp = gs_scr-lower_block-s_storage_type_data-lgtyp.
  IF sy-subrc EQ 0.
    go_controller->mo_model->fill_storage_type_data_tab(
      EXPORTING
        is_matlwhst          = gs_matio_whst
      CHANGING
        cs_storage_type_data = gs_scr-lower_block-s_storage_type_data ).
  ENDIF.

  gs_scr-lower_block-s_storage_type_data-lgnum          = gs_scr-top_block-sub_section_1-lgnum.
  gs_scr-lower_block-s_storage_type_data-store_uom      = go_controller->mo_model->read_store_uom( gs_scr-lower_block-s_storage_type_data ).

  gs_scr-lower_block-s_storage_type_data-ltypt      = go_controller->mo_model->read_storage_type_text( gs_scr-lower_block-s_storage_type_data-lgtyp ).
  gs_scr-lower_block-s_storage_type_data-quanclaput =  go_controller->mo_model->det_putaway_qty_class( gs_scr-lower_block-s_storage_type_data-lgtyp ).
  go_controller->mo_model->fill_storage_bin_type_sel(
    EXPORTING
      iv_storage_type     = gs_scr-lower_block-s_storage_type_data-lgtyp
    CHANGING
      ct_storage_bin_type = gs_scr-lower_block-t_storage_bin_type_sel ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_STORAGE_TYPE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_storage_type INPUT.
  TRY.
      go_controller->mo_model->check_storage_type(
        EXPORTING
          iv_lgtyp = gs_scr-lower_block-s_storage_type_data-lgtyp ).

    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_SECTIND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_sectind INPUT.
  TRY.
      go_controller->mo_model->check_sectind(
        EXPORTING
          iv_sectind = gs_scr-lower_block-s_storage_type_data-sectind ).

    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_BINTYPE  INPUT
*&---------------------------------------------------------------------*
MODULE check_bintype INPUT.
  TRY.
      go_controller->mo_model->check_bintype(
        EXPORTING
          iv_bintype = gs_scr-lower_block-s_storage_type_data-bintype ).

    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PUTAWAY_QUAN_CLASS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE putaway_quan_class INPUT.
  TRY.
      go_controller->mo_model->putaway_quan_class(
        EXPORTING
          iv_quanclaput = gs_scr-lower_block-s_storage_type_data-quanclaput ).

    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_LGTPY  INPUT
*&---------------------------------------------------------------------*
MODULE value_request_lgtpy INPUT.
  go_controller->storage_type_value_help( CHANGING cv_lgtyp = gs_scr-lower_block-s_storage_type_data-lgtyp ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_SECTIND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_sectind INPUT.
  go_controller->storage_section_value_help( CHANGING cv_sectind = gs_scr-lower_block-s_storage_type_data-sectind ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_BINTYPE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_bintype INPUT.
  go_controller->bintype_value_help( CHANGING cv_bintype = gs_scr-lower_block-s_storage_type_data-bintype ).
ENDMODULE.
