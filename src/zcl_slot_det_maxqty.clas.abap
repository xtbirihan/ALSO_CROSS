class ZCL_SLOT_DET_MAXQTY definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_SLOT_DET_MAXQTY .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SLOT_DET_MAXQTY IMPLEMENTATION.


  METHOD /scwm/if_ex_slot_det_maxqty~determine_maxqty.
********************************************************************
*& Key          : <AAHMEDOV>-March 29, 2023
*& Request No.  : "GAP-042 CrossTopics_bin_capacity"
********************************************************************
*& Description  :
********************************************************************

    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_cross_bin_type_det.

* Provide maximum quantity to global data for later use in other slotting Badi's
    zcl_slot_maxqty=>set_maxqty( iv_maxqty_plan = iv_maxqty ).

* Populate ev_maqty to prevent red traffic lights in the slotting log
    ev_maxqty = iv_maxqty.

  ENDMETHOD.
ENDCLASS.
