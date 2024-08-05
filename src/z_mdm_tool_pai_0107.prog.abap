*----------------------------------------------------------------------*
***INCLUDE Z_MDM_TOOL_PAI_0107.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_INB_DECO_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_inb_deco_text INPUT.
  go_controller->inb_deco_text_value_help( CHANGING cv_text_id = gs_scr-lower_block-s_material_texts-zz1_inb_deco_text_whd ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_PICKING_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_picking_text INPUT.
  go_controller->picking_text_value_help( CHANGING cv_text_id = gs_scr-lower_block-s_material_texts-zz1_picking_text_whd ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_KEP_PICKING_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_kep_picking_text INPUT.
  go_controller->kep_picking_text_value_help( CHANGING cv_text_id = gs_scr-lower_block-s_material_texts-zz1_kep_picking_text_whd ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_SPED_PICKING_TXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_sped_picking_txt INPUT.
  go_controller->sped_picking_text_value_help( CHANGING cv_text_id = gs_scr-lower_block-s_material_texts-zz1_sped_picking_text_whd ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  VALUE_REQUEST_PACKING_TEXT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE value_request_packing_text INPUT.
  go_controller->packing_text_value_help( CHANGING cv_text_id = gs_scr-lower_block-s_material_texts-zz1_packing_text_whd ).
ENDMODULE.
