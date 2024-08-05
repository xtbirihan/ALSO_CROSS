class ZCL_CRUD_ZTLPTYP_DEFQTY definition
  public
  final
  create public .

public section.

  types TS_LPTYP_DEFQTY type ZTLPTYP_DEFQTY .
  types:
    tt_lptyp_defqty TYPE TABLE OF zcl_crud_ztlptyp_defqty=>ts_lptyp_defqty .

  class-methods SELECT_SINGLE_BY_LGNUM_LPTYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_LPTYP type /SCWM/LVS_LPTYP
    exporting
      !ES_ZTLPTYP_DEFQTY type ZCL_CRUD_ZTLPTYP_DEFQTY=>TS_LPTYP_DEFQTY .
  class-methods SELECT_MULTI_BY_LGNUM_LPTYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_LPTYP_R type /SCWM/TT_LPTYP_R
    exporting
      !ET_ZTLPTYP_DEFQTY type TT_LPTYP_DEFQTY .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTLPTYP_DEFQTY IMPLEMENTATION.


  METHOD select_multi_by_lgnum_lptyp.
**********************************************************************
*& Key           : aahmedov-230412
*& Request No.   : GAPs 42 - CrossTopics: Bin Capacity
**********************************************************************
*& Description (short)
*& Get the default number of totes in each flowrack rail for each storage bin type
**********************************************************************

    BREAK-POINT ID zcg_db_crud.

    SELECT *
      FROM ztlptyp_defqty
      WHERE lgnum EQ @iv_lgnum
      AND lptyp IN @it_lptyp_r
      INTO TABLE @et_ztlptyp_defqty.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztlptyp_defqty' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD select_single_by_lgnum_lptyp.
**********************************************************************
*& Key           : aahmedov-230412
*& Request No.   : GAPs 42 - CrossTopics: Bin Capacity
**********************************************************************
*& Description (short)
*& Get the default number of totes in each flowrack rail for each storage bin type
**********************************************************************

    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE *
      FROM ztlptyp_defqty
      WHERE lgnum = @iv_lgnum
      AND lptyp = @iv_lptyp
      INTO @es_ztlptyp_defqty.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztlptyp_defqty' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
