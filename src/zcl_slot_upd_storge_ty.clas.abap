CLASS zcl_slot_upd_storge_ty DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_badi_interface .
    INTERFACES /scwm/if_ex_slot_upd_storge_ty .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_SLOT_UPD_STORGE_TY IMPLEMENTATION.


  METHOD /scwm/if_ex_slot_upd_storge_ty~delete_old_storage_types.
********************************************************************
*& Key          : BSUGAREV-Jan 29, 2024
*& Request No.  : GAP-006 Master data maintenance tool
********************************************************************
*& Description  :
*&
*&
********************************************************************

*    LOOP AT ct_old_lgtyp ASSIGNING FIELD-SYMBOL(<ls_lgtyp>) WHERE lgtyp = 'FBK1'.
*      CLEAR: <ls_lgtyp>-quanclaput.
*    ENDLOOP.

  ENDMETHOD.


  METHOD /scwm/if_ex_slot_upd_storge_ty~determine_deletion_indicator.
********************************************************************
*& Key          : BSUGAREV-Jan 29, 2024
*& Request No.  : GAP-006 Master data maintenance tool
********************************************************************
*& Description  :
*&
*&
********************************************************************
*    ev_delete_mode = 4.
  ENDMETHOD.
ENDCLASS.
