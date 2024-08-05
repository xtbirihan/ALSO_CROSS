FUNCTION z_cross_but000_pai.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_SICHT) TYPE  BU_SICHT
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& PAI of the BP customer screenss
**********************************************************************
  MOVE-CORRESPONDING but000 TO gs_but000.
  CALL FUNCTION 'BUP_BUPA_BUT000_COLLECT'
    EXPORTING
      i_subname = 'CI_EEW_BUT000'
      i_but000  = gs_but000.

  zcl_cross_handle_cust_bp_flds=>check_bp_fields( iv_view =  i_sicht is_but000 = gs_but000 ).

ENDFUNCTION.
