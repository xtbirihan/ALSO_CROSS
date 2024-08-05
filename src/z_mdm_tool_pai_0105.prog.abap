*----------------------------------------------------------------------*
***INCLUDE Z_MDM_TOOL_PAI_0105.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  CHECK_CONCSTAT_ENTRY  INPUT
*&---------------------------------------------------------------------*
MODULE check_concstat_entry INPUT.

***  lv_cursorfield = go_controller->get_cursor_field( ).
***  IF lv_cursorfield NE zif_c_mdm_tool=>c_fieldnames-screen_concstat.
***    RETURN.
***  ENDIF.

  TRY.
      go_controller->mo_model->check_concstat_entry( gs_scr-lower_block-s_warehouse_data-concstat ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_DISPATCHABLE  INPUT
*&---------------------------------------------------------------------*
MODULE value_request_dispatchable INPUT.

  go_controller->value_request_dispatchable(
    CHANGING
      cv_disp_whd = gs_scr-lower_block-s_warehouse_data-zz1_disp_whd ).

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_DISPATCHABLE  INPUT
*&---------------------------------------------------------------------*
MODULE check_dispatchable_entry INPUT.

  TRY.
      go_controller->mo_model->check_dispatchable_entry(
        EXPORTING
          iv_disp_whd    = gs_scr-lower_block-s_warehouse_data-zz1_disp_whd
        CHANGING
          cv_description = gs_scr-lower_block-s_warehouse_data-disp_whd_text
      ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_PUT_STRA  INPUT
*&---------------------------------------------------------------------*
MODULE check_put_stra INPUT.
  TRY.
      go_controller->mo_model->check_put_stra(
        EXPORTING
          iv_put_stra    = gs_scr-lower_block-s_warehouse_data-put_stra
        CHANGING
          cv_description = gs_scr-lower_block-s_warehouse_data-put_stra_t ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.
***  go_controller->mo_model->check_put_stra(
***    EXPORTING
***      iv_put_stra    = gs_scr-lower_block-s_warehouse_data-put_stra_plan
***    CHANGING
***      cv_description = gs_scr-lower_block-s_warehouse_data-put_stra_plan_t ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_VOLUME_INDICATOR  INPUT
*&---------------------------------------------------------------------*
MODULE check_volume_indicator INPUT.

  TRY.
      go_controller->mo_model->check_volume_indicator(
        EXPORTING
          iv_volind      = gs_scr-lower_block-s_warehouse_data-volind
        CHANGING
          cv_description = gs_scr-lower_block-s_warehouse_data-volind_t ).

    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  UPDATE_DAILY_VOLUME  INPUT
*&---------------------------------------------------------------------*
MODULE update_daily_volume INPUT.

  go_controller->mo_model->propose_max_num_of_bin(
    EXPORTING
      is_warehouse_data    = gs_scr-lower_block-s_warehouse_data
    CHANGING
      cs_storage_type_data = gs_scr-lower_block-s_storage_type_data
  ).
  CHECK 1 = 2.
  TRY.
      go_controller->mo_model->ms_lower_block-s_warehouse_data-demqty = gs_scr-lower_block-s_warehouse_data-demqty.

      lv_answer = go_controller->update_daily_volume( gs_scr-lower_block-s_warehouse_data-demqty ).

      IF gs_scr-middle_block-s_packspec_header-ps_id IS INITIAL.
        MESSAGE i023(zmc_mdm_tool). "Packspeck was not created, slotting will not be executed!
        RETURN.
      ENDIF.

      IF lv_answer EQ zif_c_mdm_tool=>c_answer-yes.
        IF go_controller->check_packspeck_data( ) IS INITIAL.
          MESSAGE i024(zmc_mdm_tool) DISPLAY LIKE zif_c_mdm_tool=>c_message_severity-error.  "Please enter proper values for packspec!
          RETURN.
        ENDIF.
        TRY.
            go_controller->create_packspec( CHANGING cs_screen = gs_scr-middle_block-s_packspec_header ).
          CATCH zcx_mdm_tool INTO go_mdm_tool.
            CLEAR: gv_okcode.
            MESSAGE go_mdm_tool TYPE go_mdm_tool->if_t100_dyn_msg~msgty.
        ENDTRY.

        go_controller->run_slotting(
          EXPORTING
            iv_savmod                = zif_c_mdm_tool=>c_slotting-save
          CHANGING
            cs_warehouse_data        = gs_scr-lower_block-s_warehouse_data
            cs_storage_type_data     = gs_scr-lower_block-s_storage_type_data
            cs_storage_type_bin_type = gs_scr-lower_block-t_storage_bin_type_sel ).
      ENDIF.

    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_CCIND  INPUT
*&---------------------------------------------------------------------*
MODULE check_ccind INPUT.
  TRY.
      go_controller->mo_model->check_cycle_counting_ind(
        EXPORTING
          iv_ccind = gs_scr-lower_block-s_warehouse_data-ccind ).
    CATCH zcx_mdm_tool INTO go_mdm_tool.
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
  ENDTRY.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_VOLUME_IND  INPUT
*&---------------------------------------------------------------------*
MODULE value_request_volume_ind INPUT.
  go_controller->volume_ind_value_help( CHANGING cv_volind = gs_scr-lower_block-s_warehouse_data-volind ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_PUT_STRA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_put_stra INPUT.
  go_controller->put_stra_value_help( CHANGING cv_put_stra = gs_scr-lower_block-s_warehouse_data-put_stra ).
ENDMODULE.
