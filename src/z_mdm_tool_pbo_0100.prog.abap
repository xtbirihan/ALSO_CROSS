*----------------------------------------------------------------------*
***INCLUDE Z_MDM_TOOL_PBO_0100.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  TRY.
      go_controller->mo_view->return_user_parameter(
        CHANGING
          cv_lgnum         = gs_scr-top_block-sub_section_1-lgnum
          cv_entitled      = gs_scr-top_block-sub_section_1-entitled
          cv_entitled_desc = gs_scr-top_block-sub_section_1-entitled_desc
      ).
    CATCH zcx_mdm_tool INTO DATA(go_mdm_tool).
      MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error
        DISPLAY LIKE zif_c_mdm_tool=>c_message_severity-warning.
      LEAVE TO SCREEN 0.
  ENDTRY.
  go_controller->mo_model->set_model_data(
    CHANGING
      cs_screen = gs_scr ).
  go_controller->set_titlebar( ).
  go_controller->modify_screen_100( ).
ENDMODULE.
MODULE tabs_area_mat_active_tab_set OUTPUT ##NEEDED.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_PF_STATUS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_pf_status OUTPUT.
  go_controller->set_pf_status( ).
ENDMODULE.

*&SPWIZARD: OUTPUT MODULE FOR TS 'TS1'. DO NOT CHANGE THIS LINE!
*&SPWIZARD: SETS ACTIVE TAB
MODULE ts1_active_tab_set OUTPUT.
  ts1-activetab = gs_ts1-pressed_tab.
  CASE gs_ts1-pressed_tab.
    WHEN zif_c_mdm_tool=>c_tabs_area_mat-tab1.
      gs_ts1-subscreen = gv_ss_104.
    WHEN zif_c_mdm_tool=>c_tabs_area_mat-tab2.
      gs_ts1-subscreen = gv_ss_105.
    WHEN zif_c_mdm_tool=>c_tabs_area_mat-tab3.
      gs_ts1-subscreen = gv_ss_106.
    WHEN zif_c_mdm_tool=>c_tabs_area_mat-tab4.
      gs_ts1-subscreen = gv_ss_107.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  CHECK_MATERIAL_TEXTS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE check_material_texts INPUT.
  DATA: gv_text(4) ##DECL_MODUL.
  CLEAR: gv_text.
  IF gs_scr-lower_block-s_material_texts-zz1_inb_deco_text_whd IS NOT INITIAL.
    TRY.
        gv_text = |{ gs_scr-lower_block-s_material_texts-zz1_inb_deco_text_whd  ALPHA = IN }|.
        go_controller->mo_model->check_material_texts(
          EXPORTING
            iv_text        = CONV #( gv_text )
            iv_text_id     = zif_wme_c=>gc_text_types-inbound_deco
          CHANGING
            cv_description = gs_scr-lower_block-s_material_texts-inb_deco_descr ).
      CATCH zcx_mdm_tool INTO go_mdm_tool.
        MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
    ENDTRY.
    gs_scr-lower_block-s_material_texts-zz1_inb_deco_text_whd = gv_text.
  ELSE.
    CLEAR: gs_scr-lower_block-s_material_texts-inb_deco_descr.
  ENDIF.


  IF gs_scr-lower_block-s_material_texts-zz1_picking_text_whd IS NOT INITIAL.
    TRY.
        gv_text = |{ gs_scr-lower_block-s_material_texts-zz1_picking_text_whd  ALPHA = IN }|.
        go_controller->mo_model->check_material_texts(
          EXPORTING
            iv_text        = CONV #( gv_text )
            iv_text_id     = zif_wme_c=>gc_text_types-cust_general_picking
          CHANGING
            cv_description = gs_scr-lower_block-s_material_texts-picking_descr ).
      CATCH zcx_mdm_tool INTO go_mdm_tool.
        MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
    ENDTRY.
    gs_scr-lower_block-s_material_texts-zz1_picking_text_whd = gv_text.
  ELSE.
    CLEAR: gs_scr-lower_block-s_material_texts-picking_descr.
  ENDIF.


  IF gs_scr-lower_block-s_material_texts-zz1_kep_picking_text_whd IS NOT INITIAL.
    TRY.
        gv_text = |{ gs_scr-lower_block-s_material_texts-zz1_kep_picking_text_whd  ALPHA = IN }|.
        go_controller->mo_model->check_material_texts(
          EXPORTING
            iv_text        = CONV #( gv_text )
            iv_text_id     = zif_wme_c=>gc_text_types-kep_material_picking
          CHANGING
            cv_description = gs_scr-lower_block-s_material_texts-kep_picking_descr ).
      CATCH zcx_mdm_tool INTO go_mdm_tool.
        MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
    ENDTRY.
    gs_scr-lower_block-s_material_texts-zz1_kep_picking_text_whd = gv_text.
  ELSE.
    CLEAR: gs_scr-lower_block-s_material_texts-kep_picking_descr.
  ENDIF.


  IF gs_scr-lower_block-s_material_texts-zz1_sped_picking_text_whd IS NOT INITIAL.
    TRY.
        gv_text = |{ gs_scr-lower_block-s_material_texts-zz1_sped_picking_text_whd  ALPHA = IN }|.
        go_controller->mo_model->check_material_texts(
          EXPORTING
            iv_text        = CONV #( gv_text )
            iv_text_id     = zif_wme_c=>gc_text_types-sped_material_picking
          CHANGING
            cv_description = gs_scr-lower_block-s_material_texts-sped_picking_descr ).
      CATCH zcx_mdm_tool INTO go_mdm_tool.
        MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
    ENDTRY.
    gs_scr-lower_block-s_material_texts-zz1_sped_picking_text_whd = gv_text.
  ELSE.
    CLEAR: gs_scr-lower_block-s_material_texts-sped_picking_descr.
  ENDIF.


  IF gs_scr-lower_block-s_material_texts-zz1_packing_text_whd IS NOT INITIAL.
    TRY.
        gv_text = |{ gs_scr-lower_block-s_material_texts-zz1_packing_text_whd  ALPHA = IN }|.
        go_controller->mo_model->check_material_texts(
          EXPORTING
            iv_text        = CONV #( gv_text )
            iv_text_id     = zif_wme_c=>gc_text_types-cust_packing
          CHANGING
            cv_description = gs_scr-lower_block-s_material_texts-packing_descr ).
      CATCH zcx_mdm_tool INTO go_mdm_tool.
        MESSAGE go_mdm_tool TYPE zif_c_mdm_tool=>c_message_severity-error.
    ENDTRY.
    gs_scr-lower_block-s_material_texts-zz1_packing_text_whd = gv_text.
  ELSE.
    CLEAR: gs_scr-lower_block-s_material_texts-packing_descr.
  ENDIF.
ENDMODULE.
