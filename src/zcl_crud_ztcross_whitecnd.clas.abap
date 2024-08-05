CLASS zcl_crud_ztcross_whitecnd DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      tt_whitecnd TYPE STANDARD TABLE OF ztcross_whitecnd WITH DEFAULT KEY .

    CLASS-METHODS select_multi_by_lgnum
      IMPORTING
        !iv_lgnum          TYPE /scwm/lgnum
      RETURNING
        VALUE(rt_whitecnd) TYPE tt_whitecnd .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTCROSS_WHITECND IMPLEMENTATION.


  METHOD select_multi_by_lgnum.
**********************************************************************
*& Key           : rmanova-230911
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*& Select by warehouse number
**********************************************************************

    SELECT FROM ztcross_whitecnd
    FIELDS *
     WHERE lgnum = @iv_lgnum
      INTO TABLE @rt_whitecnd.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztcross_whitecnd' sy-datum sy-uzeit.
    ENDIF.

    SORT rt_whitecnd BY seq_num.

  ENDMETHOD.
ENDCLASS.
