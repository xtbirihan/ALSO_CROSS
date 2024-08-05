FUNCTION z_cross_but000_pbo.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& PBO of hte BP customer screens
**********************************************************************

  CALL FUNCTION 'BUP_BUPA_BUT000_GET'
    IMPORTING
      e_but000 = gs_but000.

  IF gs_but000-zz_packing_text NE gs_but000_old-zz_packing_text.
    CLEAR gv_packing_text.
    IF gs_but000-zz_packing_text IS NOT INITIAL.
      DATA(lt_text) =  zcl_crud_zcross_texts=>select_multy_by_ttype_tid(
          iv_text_type = zif_wme_c=>gc_text_types-cust_packing
          iv_text_id   = gs_but000-zz_packing_text ).
      IF lt_text IS NOT INITIAL.
        gv_packing_text = lt_text[ 1 ]-text.
      ENDIF.
    ENDIF.
  ENDIF.
  IF gs_but000-zz_picking_text NE gs_but000_old-zz_picking_text.
    CLEAR gv_picking_text.
    IF gs_but000-zz_picking_text IS NOT INITIAL.
      lt_text =  zcl_crud_zcross_texts=>select_multy_by_ttype_tid(
          iv_text_type = zif_wme_c=>gc_text_types-cust_general_picking
          iv_text_id   = gs_but000-zz_picking_text ).
      IF lt_text IS NOT INITIAL.
        gv_picking_text = lt_text[ 1 ]-text.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gs_but000-zz_kep_pick_text NE gs_but000_old-zz_kep_pick_text.
    CLEAR gv_kep_pick_text.
    IF gs_but000-zz_kep_pick_text IS NOT INITIAL.
      lt_text =  zcl_crud_zcross_texts=>select_multy_by_ttype_tid(
          iv_text_type = zif_wme_c=>gc_text_types-cust_kep_picking
          iv_text_id   = gs_but000-zz_kep_pick_text ).
      IF lt_text IS NOT INITIAL.
        gv_kep_pick_text = lt_text[ 1 ]-text.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gs_but000-zz_sped_pick_text NE gs_but000_old-zz_sped_pick_text.
    CLEAR gv_sped_pick_text.
    IF gs_but000-zz_sped_pick_text IS NOT INITIAL.
      lt_text =  zcl_crud_zcross_texts=>select_multy_by_ttype_tid(
          iv_text_type = zif_wme_c=>gc_text_types-cust_sped_picking
          iv_text_id   = gs_but000-zz_sped_pick_text ).
      IF lt_text IS NOT INITIAL.
        gv_sped_pick_text = lt_text[ 1 ]-text.
      ENDIF.
    ENDIF.
  ENDIF.

  IF gs_but000-zz_unloading_text NE gs_but000_old-zz_unloading_text.
    CLEAR gv_unloading_text.
    IF gs_but000-zz_unloading_text IS NOT INITIAL.
      lt_text =  zcl_crud_zcross_texts=>select_multy_by_ttype_tid(
          iv_text_type = zif_wme_c=>gc_text_types-vend_unloading
          iv_text_id   = gs_but000-zz_unloading_text ).
      IF lt_text IS NOT INITIAL.
        gv_unloading_text = lt_text[ 1 ]-text.
      ENDIF.
    ENDIF.
  ENDIF.

  MOVE-CORRESPONDING gs_but000 TO but000.
  gs_but000_old = gs_but000.
ENDFUNCTION.
