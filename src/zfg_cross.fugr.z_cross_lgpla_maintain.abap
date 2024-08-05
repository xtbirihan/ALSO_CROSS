FUNCTION z_cross_lgpla_maintain .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IT_LAGP_EMPTY) TYPE  /SCWM/TT_LAGP_MAINTAIN
*"     VALUE(IT_LAGP_CAPA) TYPE  /SCWM/TT_LAGP_MAINTAIN
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AAHMEDOV>-11.05.2023 10:39:15
*& Request No.  : GAP-042_FS_CrossTopics_bin_capacity
********************************************************************
*& Description  : Set the storage bin capacity to 0, if
*                 the storage bin is empty
********************************************************************
  DATA: lt_bapiret       TYPE  bapiret2_t,
        lt_lagp_maintain TYPE /scwm/tt_lagp_maintain.

  APPEND LINES OF it_lagp_empty TO lt_lagp_maintain.
  APPEND LINES OF it_lagp_capa TO lt_lagp_maintain.

  CALL FUNCTION '/SCWM/LAGP_MAINTAIN'
    EXPORTING
      iv_lgnum             = iv_lgnum
      it_lagp              = lt_lagp_maintain
      iv_action            = wmegc_update
      iv_commit            = abap_true
    IMPORTING
      et_bapiret           = lt_bapiret
    EXCEPTIONS
      data_invalid         = 1
      action_not_supported = 2
      error                = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE ID '/SCWM/L3'
                TYPE wmegc_severity_err
                NUMBER 302.
  ELSEIF line_exists( lt_bapiret[ type = wmegc_severity_err ] ).
    DATA(ls_bapiret) = lt_bapiret[ type = wmegc_severity_err ].
    MESSAGE ID ls_bapiret-id
         TYPE ls_bapiret-type
         NUMBER ls_bapiret-number.
  ENDIF.

ENDFUNCTION.
