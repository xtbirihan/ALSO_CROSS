FUNCTION z_cross_shlp_texts.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"--------------------------------------------------------------------
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Search help exit of the search help ZSH_CROSS_TEXTS. It selects
*& only  the corresponding texts to the specific field.
**********************************************************************
  IF callcontrol-step = 'SELECT'.
    zcl_text_handling=>get_instance_general( )->change_shlp( CHANGING cs_shlp = shlp ).
  ELSE.
    EXIT.
  ENDIF.

ENDFUNCTION.
