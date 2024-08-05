class ZCL_CRUD_ZTCROSS_BLACKCND definition
  public
  final
  create public .

public section.

  types:
    tt_blackcnd TYPE STANDARD TABLE OF ztcross_blackcnd WITH DEFAULT KEY .

  class-methods SELECT_MULTI_BY_LGNUM
    importing
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RT_BLACKCND) type TT_BLACKCND .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTCROSS_BLACKCND IMPLEMENTATION.


  METHOD select_multi_by_lgnum.
**********************************************************************
*& Key           : rmanova-230911
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*& Select by warehouse number
**********************************************************************

    SELECT FROM ztcross_blackcnd
    FIELDS *
     WHERE lgnum = @iv_lgnum
      INTO TABLE @rt_blackcnd.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztcross_blackcnd' sy-datum sy-uzeit.
    ENDIF.

    SORT rt_blackcnd BY seq_num.

  ENDMETHOD.
ENDCLASS.
