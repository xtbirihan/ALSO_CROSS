CLASS zcl_crud_ztlptyp_maxqty DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES ts_lptyp_maxqty TYPE ztlptyp_maxqty .
    TYPES:
      tt_lptyp_maxqty TYPE SORTED TABLE OF ts_lptyp_maxqty WITH UNIQUE KEY lgnum lgtyp lptyp matid.

    CLASS-METHODS select_single_by_all_filters
      IMPORTING
        !iv_lgnum          TYPE /scwm/lgnum
        !iv_lgtyp          TYPE /scwm/lgtyp
        !iv_lptyp          TYPE /scwm/lvs_lptyp
        !iv_matid          TYPE /scwm/de_matid
      EXPORTING
        !es_ztlptyp_maxqty TYPE ts_lptyp_maxqty .
    CLASS-METHODS select_multi_by_all_filters
      IMPORTING
        !iv_lgnum          TYPE /scwm/lgnum
        !it_lgtyp_r        TYPE /scwm/tt_lgtyp_r OPTIONAL
        !it_matid_r        TYPE /scwm/tt_matid_r OPTIONAL
        !it_lptyp_r        TYPE /scwm/tt_lptyp_r OPTIONAL
      EXPORTING
        !et_ztlptyp_maxqty TYPE tt_lptyp_maxqty .
    CLASS-METHODS select_multi_by_unique_key
      IMPORTING
        !it_keys           TYPE tt_lptyp_maxqty
      EXPORTING
        !et_ztlptyp_maxqty TYPE tt_lptyp_maxqty .
    CLASS-METHODS select_lgtyp_lptyp
      IMPORTING
        !iv_lgnum          TYPE /scwm/lgnum
        !it_lgtyp_r        TYPE /scwm/tt_lgtyp_r
      EXPORTING
        !et_ztlptyp_maxqty TYPE tt_lptyp_maxqty .
    CLASS-METHODS modify
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum
        !it_ztlptyp_new TYPE tt_lptyp_maxqty
      EXPORTING
        !ev_dbcnt       TYPE i
        !ev_subrc       TYPE i .
    CLASS-METHODS insert
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum
        !it_ztlptyp_new TYPE tt_lptyp_maxqty
      EXPORTING
        !ev_dbcnt       TYPE i
        !ev_subrc       TYPE i .
    CLASS-METHODS delete
      IMPORTING
        !iv_lgnum       TYPE /scwm/lgnum
        !it_ztlptyp_del TYPE tt_lptyp_maxqty
      EXPORTING
        !ev_dbcnt       TYPE i
        !ev_subrc       TYPE i .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZCL_CRUD_ZTLPTYP_MAXQTY IMPLEMENTATION.


  METHOD delete.

    IF iv_lgnum IS INITIAL
     OR it_ztlptyp_del IS INITIAL.
      RETURN.
    ENDIF.

    "Delete Entries
    DELETE ztlptyp_maxqty FROM TABLE it_ztlptyp_del.
    IF sy-subrc EQ 0.
      ev_dbcnt = sy-dbcnt.
    ENDIF.

    ev_subrc = sy-subrc.

  ENDMETHOD.


  METHOD insert.

    IF iv_lgnum IS INITIAL
     OR it_ztlptyp_new IS INITIAL.
      RETURN.
    ENDIF.

    "Insert Entries
    INSERT ztlptyp_maxqty FROM TABLE it_ztlptyp_new ACCEPTING DUPLICATE KEYS.
    IF sy-subrc EQ 0.
      ev_dbcnt = sy-dbcnt.
    ENDIF.

    ev_subrc = sy-subrc.

  ENDMETHOD.


  METHOD modify.

    IF iv_lgnum IS INITIAL
      OR it_ztlptyp_new IS INITIAL.
      RETURN.
    ENDIF.

    "Update And Insert Entries
    MODIFY ztlptyp_maxqty FROM TABLE it_ztlptyp_new.
    IF sy-subrc EQ 0.
      ev_dbcnt = sy-dbcnt.
    ENDIF.

    ev_subrc = sy-subrc.

  ENDMETHOD.


  METHOD select_lgtyp_lptyp.
**********************************************************************
*& Key           : aahmedov-020512
*& Request No.   : GAPs 42 - CrossTopics: Bin Capacity
**********************************************************************
*& Description (short)
*& Get the max quantity for each material in the given storage bin type
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    CLEAR et_ztlptyp_maxqty.

    SELECT DISTINCT lgnum, lgtyp, lptyp
      FROM ztlptyp_maxqty
      WHERE lgnum = @iv_lgnum
      AND lgtyp IN @it_lgtyp_r
      INTO CORRESPONDING FIELDS OF TABLE @et_ztlptyp_maxqty.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztlptyp_maxqty' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_multi_by_all_filters.
**********************************************************************
*& Key           : aahmedov-230412
*& Request No.   : GAPs 42 - CrossTopics: Bin Capacity
**********************************************************************
*& Description (short)
*& Get the max quantity for each material in the given storage bin type
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    CLEAR et_ztlptyp_maxqty.

    SELECT *
      FROM ztlptyp_maxqty
      WHERE lgnum = @iv_lgnum
      AND   lgtyp IN @it_lgtyp_r
      AND   lptyp IN @it_lptyp_r
      AND   matid IN @it_matid_r
      ORDER BY max_qty DESCENDING
      INTO TABLE @et_ztlptyp_maxqty.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztlptyp_maxqty' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_multi_by_unique_key.
********************************************************************
*& Key          : <BSUGAREV>-27.04.2023 15:02:38
*& Request No.  :
********************************************************************
*& Description
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM ztlptyp_maxqty
      INTO TABLE @et_ztlptyp_maxqty
       FOR ALL ENTRIES IN @it_keys
     WHERE lgnum = @it_keys-lgnum
       AND lgtyp = @it_keys-lgtyp
       AND lptyp = @it_keys-lptyp
       AND matid = @it_keys-matid.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ZTLPTYP_MAXQTY' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_single_by_all_filters.
**********************************************************************
*& Key           : aahmedov-230412
*& Request No.   : GAPs 42 - CrossTopics: Bin Capacity
**********************************************************************
*& Description (short)
*& Get the max quantity for each material in the given storage bin type
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE *
      FROM ztlptyp_maxqty
      WHERE lgnum = @iv_lgnum
      AND   lgtyp = @iv_lgtyp
      AND   lptyp = @iv_lptyp
      AND   matid = @iv_matid
      INTO @es_ztlptyp_maxqty.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
         FIELDS 'ztlptyp_maxqty' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
