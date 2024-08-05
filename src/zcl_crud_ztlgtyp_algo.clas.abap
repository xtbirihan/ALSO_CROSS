class ZCL_CRUD_ZTLGTYP_ALGO definition
  public
  final
  create public .

public section.

  types TS_LGTYP_ALGO type ZTLGTYP_ALGO .
  types:
    tt_lgtyp_algo TYPE TABLE OF ts_lgtyp_algo .

  class-methods SELECT_SINGLE_BY_LGNUM_LGTYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_LGTYP type /SCWM/LGTYP
    exporting
      !ES_ZTLGTYP_ALGO type ZCL_CRUD_ZTLGTYP_ALGO=>TS_LGTYP_ALGO .
  class-methods SELECT_MULTI_BY_LGNUM_LGTYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_LGTYP_R type /SCWM/TT_LGTYP_R
    exporting
      !ET_ZTLGTYP_ALGO type ZCL_CRUD_ZTLGTYP_ALGO=>TT_LGTYP_ALGO .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTLGTYP_ALGO IMPLEMENTATION.


  METHOD select_multi_by_lgnum_lgtyp.
**********************************************************************
*& Key           : aahmedov-230412
*& Request No.   : GAPs 42 - CrossTopics: Bin Capacity
**********************************************************************
*& Description (short)
*& Get the bin capacity calculation algorithm for each storage type
**********************************************************************

    BREAK-POINT ID zcg_db_crud.

    SELECT *
      FROM ztlgtyp_algo
      WHERE lgnum = @iv_lgnum
      AND   lgtyp IN @it_lgtyp_r
      INTO TABLE @et_ztlgtyp_algo.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztlgtyp_algo' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD select_single_by_lgnum_lgtyp.
**********************************************************************
*& Key           : aahmedov-230412
*& Request No.   : GAPs 42 - CrossTopics: Bin Capacity
**********************************************************************
*& Description (short)
*& Get the bin capacity calculation algorithm for each storage type
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE *
      FROM ztlgtyp_algo
      WHERE lgnum = @iv_lgnum
      AND   lgtyp = @iv_lgtyp
      INTO @es_ztlgtyp_algo.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztlgtyp_algo' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
