FUNCTION z_release_flg_upd.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IT_DATA) TYPE  STANDARD TABLE
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : rmanova-230918
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*& Delivery Header - release flag update
**********************************************************************

  DATA:
    lo_helper TYPE REF TO lcl_rel_helper.

  lo_helper = NEW lcl_rel_helper( ).
  lo_helper->release_flg_upd( iv_lgnum = iv_lgnum
                              it_data  = it_data ).

ENDFUNCTION.
