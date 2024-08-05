FUNCTION z_cross_lgtyp_post.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM OPTIONAL
*"     VALUE(IT_LTAP_VB) TYPE  /SCWM/TT_LTAP_VB OPTIONAL
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AAHMEDOV>-23.08.2023
*& Request No.  : GAP-086_Conveyor System Routing
********************************************************************
*& Description  : Get the task with source bin conveyer of the current HU
*                 and confirm it, if the destination bin is not with storage role D or I
********************************************************************

  DATA: lt_ordim_o  TYPE /scwm/tt_ordim_o,
        lt_to_conf  TYPE /scwm/to_conf_tt,
        lt_t331     TYPE /scwm/tt_t331,
        lv_severity TYPE  bapi_mtype.

  zcl_param=>get_parameter(
    EXPORTING
      iv_lgnum     = iv_lgnum                 " Warehouse Number/Warehouse Complex
      iv_process   = zif_param_const=>c_zcross_0004                 " Process ID (Specification, Program, BAdI etc.)
      iv_parameter = zif_param_const=>c_lgtyp_conv                 " Parameter ID for process
    IMPORTING
      et_range     = DATA(lt_lgtyp_conv_r)                " SELECT-OPTIONS Table
  ).

  IF lt_lgtyp_conv_r IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION '/SCWM/TO_READ_SRC'
    EXPORTING
      iv_lgnum     = iv_lgnum
      iv_lgtyp     = CONV /scwm/lgtyp( lt_lgtyp_conv_r[ 1 ]-low )
      it_huident   = VALUE /scwm/tt_huident( FOR <ltap_huident> IN it_ltap_vb
                                             ( lgnum = iv_lgnum
                                               huident = <ltap_huident>-vlenr ) )
    IMPORTING
      et_ordim_o   = lt_ordim_o
    EXCEPTIONS
      wrong_input  = 1
      not_found    = 2
      foreign_lock = 3
      OTHERS       = 4.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  TRY.
      CALL FUNCTION '/SCWM/T331_READ_MULTI'
        EXPORTING
          iv_lgnum = iv_lgnum
          it_lgtyp = VALUE rseloption( FOR <ordim_o> IN lt_ordim_o
                                      ( sign = wmegc_sign_inclusive
                                        option   = wmegc_option_eq
                                        low      = <ordim_o>-nltyp ) )
        IMPORTING
          et_t331  = lt_t331.
    CATCH /scwm/cx_core.
      RETURN.
  ENDTRY.

  LOOP AT lt_ordim_o ASSIGNING FIELD-SYMBOL(<ls_ordim_o>).
    CHECK lt_t331[ lgtyp = <ls_ordim_o>-nltyp ]-st_role <> wmegc_strole_stgarea AND
    lt_t331[ lgtyp = <ls_ordim_o>-nltyp ]-st_role <> wmegc_strole_wc_stgarea.

    lt_to_conf = VALUE #( BASE lt_to_conf ( CORRESPONDING #( <ls_ordim_o> ) ) ).
  ENDLOOP.

  IF lt_to_conf IS  INITIAL.
    RETURN.
  ENDIF.

  "trigger confirmation of dependent inactive WTs in asynchronous task
  CALL FUNCTION '/SCWM/TO_CONFIRM'
    EXPORTING
      iv_lgnum    = iv_lgnum
      it_conf     = lt_to_conf
    IMPORTING
      ev_severity = lv_severity.

  IF lv_severity CA wmegc_severity_eax.
    MESSAGE ID '/SCWM/TO_WHR_UPD'
         TYPE wmegc_severity_err
         NUMBER 003.
  ENDIF.

ENDFUNCTION.
