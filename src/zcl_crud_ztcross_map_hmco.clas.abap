class ZCL_CRUD_ZTCROSS_MAP_HMCO definition
  public
  final
  create public .

public section.

  types:
    tt_whse_mdef TYPE STANDARD TABLE OF ztcross_map_hmco WITH DEFAULT KEY .

  class-methods SELECT_SINGLE_BY_KEY
    importing
      !IV_HMSIGN type ZZ1_MHMSIGN01
    returning
      value(RS_ENTRY) type ZTCROSS_MAP_HMCO .
  class-methods SELECT_MULTI_BY_KEY
    importing
      !IT_HMSIGN_R type RSELOPTION
    returning
      value(RT_ENTRIES) type TT_WHSE_MDEF .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTCROSS_MAP_HMCO IMPLEMENTATION.


  METHOD select_multi_by_key.
********************************************************************
*& Key          : <AAHMEDOV>-Feb 5, 2023
*& Request No.  : "GAP-012 - Outbound_VCE_carrier_software_integration"
********************************************************************
*& Description  :
********************************************************************

    BREAK-POINT ID zcg_db_crud.

    SELECT *
      FROM ztcross_map_hmco
     WHERE sign IN @it_hmsign_r
      INTO TABLE @rt_entries.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ZTCROSS_MAP_HMCO' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.


  METHOD select_single_by_key.
**********************************************************************
*& Key           : LH-261223
*& Request No.   : GAP-040 – Additional product master fields
**********************************************************************
*& Description (short)
*& Mapping hazardous product
**********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE *
      FROM ztcross_map_hmco
     WHERE sign = @iv_hmsign
      INTO @rs_entry.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ZTCROSS_MAP_HMCO' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
