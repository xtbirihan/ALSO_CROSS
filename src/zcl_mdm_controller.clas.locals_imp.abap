*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      handle_hotspot_click
        FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id
                  es_row_no ##NEEDED.

ENDCLASS.                    "lcl_event_receiver DEFINITION

CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_hotspot_click.

* go to normal okcode execution
     CALL FUNCTION 'SAPGUI_SET_FUNCTIONCODE'
      EXPORTING
        functioncode           = 'ALV_DISPLAY'
      EXCEPTIONS
        function_not_supported = 1
        OTHERS                 = 2 ##FM_SUBRC_OK.
    IF sy-subrc <> 0 ##NEEDED.
    ENDIF.

  ENDMETHOD.                    "handle_hotspot_click
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION
