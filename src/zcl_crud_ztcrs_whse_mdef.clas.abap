CLASS zcl_crud_ztcrs_whse_mdef DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: tt_whse_mdef TYPE STANDARD TABLE OF ztcrs_whse_mdef WITH DEFAULT KEY.

    CLASS-METHODS select_single_by_key
      IMPORTING
        !iv_lgnum           TYPE /scwm/lgnum
      RETURNING
        VALUE(rs_whse_mdef) TYPE ztcrs_whse_mdef.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTCRS_WHSE_MDEF IMPLEMENTATION.


  METHOD select_single_by_key.
********************************************************************
*& Key          : <BSUGAREV>-02.06.2023 14:06:41
*& Request No.  : GAP-55 – SPED Orders Picking
********************************************************************
*& Description
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE *
      FROM ztcrs_whse_mdef
     WHERE lgnum = @iv_lgnum
      INTO @rs_whse_mdef.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ZTCRS_WHSE_MDEF' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
