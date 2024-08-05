class ZCL_CRUD_ZCROSS_TEXTS definition
  public
  final
  create public .

public section.

  types:
    tt_text TYPE STANDARD TABLE OF zcross_texts WITH EMPTY KEY .

  class-methods SELECT_MULTY_BY_LGNUM
    importing
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RT_TEXT) type TT_TEXT .
  class-methods SELECT_MULTY_BY_TTYPE_TID
    importing
      !IV_TEXT_TYPE type ZDE_TEXT_TYPE
      !IV_TEXT_ID type ZDE_TEXT_ID
    returning
      value(RT_TEXT) type TT_TEXT .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZCROSS_TEXTS IMPLEMENTATION.


  METHOD select_multy_by_lgnum.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Read text table for warehouse
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM zcross_texts INTO TABLE @rt_text
             WHERE lgnum EQ @iv_lgnum.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'zcross_texts' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD SELECT_MULTY_BY_TTYPE_TID.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Select the text for text type and id
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM zcross_texts INTO TABLE @rt_text
             WHERE text_type eq @iv_text_type
               and text_id   eq @iv_text_id.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'zcross_texts' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
