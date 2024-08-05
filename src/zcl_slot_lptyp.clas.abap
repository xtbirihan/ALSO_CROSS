class ZCL_SLOT_LPTYP definition
  public
  final
  create public .

public section.

  class-methods GET_INSTANCE
    returning
      value(RO_INSTANCE) type ref to ZCL_SLOT_LPTYP .
  class-methods GET_LPTYP
    returning
      value(RV_LPTYP) type /SCWM/DE_BINTYPE .
  class-methods SET_LPTYP
    importing
      !IV_LPTYP type /SCWM/DE_BINTYPE .
protected section.
private section.

  class-data MO_INSTANCE type ref to ZCL_SLOT_LPTYP .
  class-data MV_LPTYP type /SCWM/DE_BINTYPE .
ENDCLASS.



CLASS ZCL_SLOT_LPTYP IMPLEMENTATION.


  METHOD get_instance.
********************************************************************
*& Key         : <AA>-290523
*& Request No. : TE1K900125 Main Request / GAP-069 - Maximum_qty_in_slotting
********************************************************************
*& Description (short)
********************************************************************

    IF mo_instance IS NOT BOUND.
      mo_instance = NEW zcl_slot_lptyp( ).
    ENDIF.

    ro_instance = mo_instance.

  ENDMETHOD.


  METHOD get_lptyp.
********************************************************************
*& Key         : <AA>-290523
*& Request No. : TE1K900125 Main Request / GAP-069 - Maximum_qty_in_slotting
********************************************************************
*& Description (short)
********************************************************************

    rv_lptyp = mv_lptyp.

  ENDMETHOD.


  METHOD set_lptyp.
********************************************************************
*& Key         : <AA>-290523
*& Request No. : TE1K900125 Main Request / GAP-069 - Maximum_qty_in_slotting
********************************************************************
*& Description (short)
********************************************************************

    IF iv_lptyp IS NOT INITIAL.
      mv_lptyp = iv_lptyp.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
