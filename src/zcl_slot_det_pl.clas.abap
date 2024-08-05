class ZCL_SLOT_DET_PL definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces /SCWM/IF_EX_SLOT_DET_PL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_SLOT_DET_PL IMPLEMENTATION.


  METHOD /scwm/if_ex_slot_det_pl~choose_packlevels.
********************************************************************
*& Key          : <AAHMEDOV>-Dec 11, 2023
*& Request No.  : "GAP-069 - Maximum qty in slotting
********************************************************************
*& Description  :
********************************************************************

    BREAK-POINT ID zcg_badi.

    FIELD-SYMBOLS: <fs_packinfo> TYPE LINE OF /scwm/tt_packinfo.

    DATA(ls_marm) = zcl_crud_marm=>select_single_by_key(
      EXPORTING
        iv_matnr = is_mat_global-matnr
        iv_uom   = zif_wme_c=>gs_uom-pal
    ).

    READ TABLE ct_packinfo INDEX 1 ASSIGNING <fs_packinfo>.

    IF <fs_packinfo> IS ASSIGNED.

      <fs_packinfo>-hutyp = ls_marm-zz1_hutype_uom.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
