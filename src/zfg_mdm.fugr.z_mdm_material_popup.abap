FUNCTION z_mdm_material_popup.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IT_MATERIAL) TYPE  ZTT_MATERIAL_TEXT_DATA
*"  EXPORTING
*"     VALUE(ES_MATERIAL) TYPE  ZSTR_MATERIAL_TEXT_DATA
*"----------------------------------------------------------------------

  CLEAR: gt_material_text, gs_material_text.

  gt_material_text = it_material[].

  DATA: lo_popup_alv  TYPE REF TO cl_salv_table.
  DATA: lo_functions TYPE REF TO cl_salv_functions_list,
        lo_events    TYPE REF TO cl_salv_events_table.
  DATA: lo_columns TYPE REF TO cl_salv_columns_table.
*
  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = lo_popup_alv
    CHANGING
      t_table      = gt_material_text ).


  lo_functions = lo_popup_alv->get_functions( ).
*  lo_functions->set_default( 'X' ).

  lo_columns = lo_popup_alv->get_columns( ).
  lo_columns->set_optimize( ).

  lo_events = lo_popup_alv->get_event( ).
  SET HANDLER lcl_event_handler=>on_double_click FOR lo_events.
*  SET HANDLER lcl_event_handler=>on_link_click FOR lo_events.

* ALV as Popup
  lo_popup_alv->set_screen_popup(
    start_column = 80
    end_column   = 140
    start_line   = 3
    end_line     = 10 ).

* Display
  lo_popup_alv->display( ).

  es_material = gs_material_text.

ENDFUNCTION.
