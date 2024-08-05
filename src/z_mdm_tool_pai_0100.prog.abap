*----------------------------------------------------------------------*
***INCLUDE Z_MDM_TOOL_PAI_0100.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CASE gv_okcode.
    WHEN zif_c_mdm_tool=>c_user_command-ok_default.
      go_controller->mo_view->show_popup(
        CHANGING
          cv_lgnum    = gs_scr-top_block-sub_section_1-lgnum
          cv_entitled = gs_scr-top_block-sub_section_1-entitled
        RECEIVING
          rv_change   = DATA(gv_changed) ).
      IF gv_changed IS NOT INITIAL.
        go_controller->clear_sections(
          CHANGING
            cs_screen = gs_scr ).
      ENDIF.
      IF gs_scr-top_block-sub_section_1-scanned_value IS INITIAL.
        gv_ss_102 = gv_ss_103 = gv_ss_104 = gv_ss_105 = gv_ss_106 = gv_ss_107 =
        zif_c_mdm_tool=>c_screen-ss_900.
      ENDIF.

    WHEN zif_c_mdm_tool=>c_user_command-save.
      TRY.
          go_controller->save( CHANGING cs_screen = gs_scr ).
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool TYPE go_mdm_tool->if_t100_dyn_msg~msgty.
      ENDTRY.

    WHEN zif_c_mdm_tool=>c_user_command-packspec.
      IF go_controller->check_packspeck_data( ) IS INITIAL.
        MESSAGE i024(zmc_mdm_tool) DISPLAY LIKE zif_c_mdm_tool=>c_message_severity-error. "Please enter proper values for packspec!
        RETURN.
      ENDIF.
      TRY.
          go_controller->create_packspec( CHANGING cs_screen = gs_scr-middle_block-s_packspec_header ).
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
      ENDTRY.

    WHEN zif_c_mdm_tool=>c_user_command-mc_additon.
      TRY.
          go_controller->master_carton_additional(
            EXPORTING
              iv_matid                   = gs_scr-top_block-sub_section_1-matid
            CHANGING
              cs_warehouse_data          = gs_scr-lower_block-s_warehouse_data
              cs_master_carton_aditional = gs_scr-s_master_carton_aditional
              cs_storage_type_data       = gs_scr-lower_block-s_storage_type_data
              ct_unit_of_measures        = gs_scr-middle_block-t_unit_of_measures ).

        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool TYPE go_mdm_tool->if_t100_dyn_msg~msgty.
      ENDTRY.

    WHEN zif_c_mdm_tool=>c_user_command-slotting.

      IF go_controller->check_packspeck_data( ) IS INITIAL.
        MESSAGE i024(zmc_mdm_tool) DISPLAY LIKE zif_c_mdm_tool=>c_message_severity-error. "Please enter proper values for packspec!
        RETURN.
      ENDIF.

      TRY.
          go_controller->create_packspec( CHANGING cs_screen = gs_scr-middle_block-s_packspec_header ).
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
      ENDTRY.

      TRY.
          DATA(lv_answer) = go_controller->update_daily_volume(
            EXPORTING
              iv_skip_popup      = abap_true
              iv_demand_quantity = gs_scr-lower_block-s_warehouse_data-demqty ).
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
      ENDTRY.

      TRY.
          go_controller->update_product_fields( ).
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
      ENDTRY.

      TRY.
          DATA(ls_return) = go_controller->update_units_of_measure( ).
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
      ENDTRY.

      IF ls_return-type CA 'AEX'.
        DATA: lv_message TYPE string ##DECL_MODUL,
              lv_msg_no  TYPE t100-msgnr ##DECL_MODUL.
        go_controller->bapi_transaction_rollback( ).
        lv_msg_no = ls_return-number.
        CALL FUNCTION 'MESSAGE_PREPARE'
          EXPORTING
            language = sy-langu
            msg_id   = ls_return-id
            msg_no   = lv_msg_no
            msg_var1 = ls_return-message_v1
            msg_var2 = ls_return-message_v2
            msg_var3 = ls_return-message_v3
            msg_var4 = ls_return-message_v4
          IMPORTING
            msg_text = lv_message.

        CLEAR: gv_okcode, ls_return, lv_msg_no.
        MESSAGE lv_message TYPE zif_c_mdm_tool=>c_message_severity-error.
      ELSE.
        go_controller->bapi_transaction_commit( ).
      ENDIF.

      TRY.
          CALL FUNCTION 'ZMDM_UPDATE_PATTERNS' IN UPDATE TASK
            EXPORTING
              is_section_1 = gs_scr-top_block-sub_section_1
              is_section_3 = gs_scr-top_block-sub_section_3.
          COMMIT WORK.
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
      ENDTRY.

      TRY.
          go_controller->run_slotting(
            EXPORTING
              iv_savmod                = zif_c_mdm_tool=>c_slotting-save
            CHANGING
              cs_warehouse_data        = gs_scr-lower_block-s_warehouse_data
              cs_storage_type_data     = gs_scr-lower_block-s_storage_type_data
              cs_storage_type_bin_type = gs_scr-lower_block-t_storage_bin_type_sel ).
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
      ENDTRY.

    WHEN zif_c_mdm_tool=>c_user_command-alv_display.  "display storage type specific master data
      TRY.
          go_controller->selected_storage_type( CHANGING cs_storage_type_data    = gs_scr-lower_block-s_storage_type_data
                                                         ct_storage_bin_type_sel = gs_scr-lower_block-t_storage_bin_type_sel ).
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool  TYPE zif_c_mdm_tool=>c_message_severity-error.
      ENDTRY.
      CLEAR: sy-ucomm.
    WHEN zif_c_mdm_tool=>c_user_command-udemqty.

      go_controller->run_update_daily_volume_job( CHANGING cs_warehouse_data = gs_scr-lower_block-s_warehouse_data ).

      TRY.
          go_controller->mo_model->propose_max_num_of_bin(
            EXPORTING
              is_warehouse_data    = gs_scr-lower_block-s_warehouse_data
            CHANGING
              cs_storage_type_data = gs_scr-lower_block-s_storage_type_data
          ).
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool  TYPE zif_c_mdm_tool=>c_message_severity-error.
      ENDTRY.
      IF 1 = 2.
        TRY.
            go_controller->mo_model->ms_lower_block-s_warehouse_data-demqty = gs_scr-lower_block-s_warehouse_data-demqty.
            IF gs_scr-middle_block-s_packspec_header-ps_id IS INITIAL.
              MESSAGE i023(zmc_mdm_tool). "Packspeck was not created, slotting will not be executed!
              RETURN.
            ENDIF.

            IF lv_answer EQ zif_c_mdm_tool=>c_answer-yes.
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
      ENDIF.

    WHEN zif_c_mdm_tool=>c_user_command-pctomc.

      TRY.
          go_controller->copy_pc_to_mc( CHANGING ct_unit_of_measures = gs_scr-middle_block-t_unit_of_measures ).
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
          MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
      ENDTRY.

    WHEN zif_c_mdm_tool=>c_user_command-more01 OR
         zif_c_mdm_tool=>c_user_command-more02 OR
         zif_c_mdm_tool=>c_user_command-more03 OR
         zif_c_mdm_tool=>c_user_command-more04 OR
         zif_c_mdm_tool=>c_user_command-more05.
      TRY.
          go_controller->more_template(
            EXPORTING
              iv_ucomm        = gv_okcode
            CHANGING
              cs_identmnumber = gs_scr-top_block-sub_section_3
          ).
        CATCH zcx_mdm_tool INTO go_mdm_tool.
          CLEAR: gv_okcode.
      ENDTRY.

    WHEN zif_c_mdm_tool=>c_user_command-sled.
      IF gs_scr-top_block-sub_section_1-sled_bbd IS INITIAL.
        CLEAR: gs_scr-top_block-sub_section_1-mhdrz.
      ELSE.
        gs_scr-top_block-sub_section_1-mhdrz = go_controller->mo_model->master_data-mara-mhdrz.
      ENDIF.
      CLEAR: sy-ucomm.

    WHEN OTHERS.
  ENDCASE.
  CLEAR: gv_okcode, ls_return.

ENDMODULE.
MODULE at_exit_command.
  go_controller->at_exit_command( ).
ENDMODULE.
MODULE tabs_area_mat_active_tab_get INPUT ##NEEDED.
ENDMODULE.
MODULE ts1_active_tab_get INPUT.
  gv_okcode = sy-ucomm.
  CASE gv_okcode.
    WHEN zif_c_mdm_tool=>c_tabs_area_mat-tab1.
      gs_ts1-pressed_tab = zif_c_mdm_tool=>c_tabs_area_mat-tab1.
      go_controller->mv_tab_clicked_tab =  gs_ts1-pressed_tab.
    WHEN zif_c_mdm_tool=>c_tabs_area_mat-tab2.
      gs_ts1-pressed_tab = zif_c_mdm_tool=>c_tabs_area_mat-tab2.
      go_controller->mv_tab_clicked_tab =  gs_ts1-pressed_tab.
    WHEN zif_c_mdm_tool=>c_tabs_area_mat-tab3.
      gs_ts1-pressed_tab = zif_c_mdm_tool=>c_tabs_area_mat-tab3.
      go_controller->mv_tab_clicked_tab =  gs_ts1-pressed_tab.
    WHEN zif_c_mdm_tool=>c_tabs_area_mat-tab4.
      gs_ts1-pressed_tab = zif_c_mdm_tool=>c_tabs_area_mat-tab4.
      go_controller->mv_tab_clicked_tab =  gs_ts1-pressed_tab.
    WHEN OTHERS.
*&SPWIZARD:      DO NOTHING
  ENDCASE.
ENDMODULE.
