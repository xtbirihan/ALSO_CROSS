*&---------------------------------------------------------------------*
*& Report ZCROSS_UPDATE_MAXQTY
*&---------------------------------------------------------------------*
**********************************************************************
*& Key           : <aahmedov>-050423
*& Request No.   : GAP-042 – Update Bin Capacity
**********************************************************************
*& Description (short)
*& Update_bin_capacity
**********************************************************************

INCLUDE zcross_update_maxqty_top.    " Global Data
INCLUDE zcross_update_maxqty_cl_i.
INCLUDE zcross_update_maxqty_f4.

START-OF-SELECTION.

  NEW lcl_update_maxqty( iv_lgnum = p_lgnum )->main( ).
