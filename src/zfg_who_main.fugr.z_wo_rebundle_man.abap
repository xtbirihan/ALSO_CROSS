FUNCTION Z_WO_REBUNDLE_MAN .
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     REFERENCE(IV_KEEP_PICKHU) TYPE  XFELD DEFAULT SPACE
*"     REFERENCE(IV_WRITE_LOG) TYPE  XFELD DEFAULT ABAP_TRUE
*"     REFERENCE(IS_WO_TEMPLATE) TYPE  /SCWM/S_WHO_INT OPTIONAL
*"     REFERENCE(IR_TANUM) TYPE  RSELOPTION
*"     REFERENCE(IT_BAPIRET) TYPE  BAPIRETTAB OPTIONAL
*"     REFERENCE(IT_WHO) TYPE  /SCWM/TT_WHO_INT OPTIONAL
*"  EXPORTING
*"     REFERENCE(EV_LOGNO) TYPE  /SCWM/DE_WHOLOGNO
*"     REFERENCE(EV_SEVERITY) TYPE  BAPI_MTYPE
*"     REFERENCE(ET_WO) TYPE  /SCWM/TT_WHO_INT
*"     REFERENCE(ET_BAPIRET) TYPE  BAPIRETTAB
*"--------------------------------------------------------------------

  DATA: lv_severity    TYPE bapi_mtype,
        lv_mtext       TYPE string,
        lv_lines       TYPE sytabix,
        lv_keep_pickhu TYPE xfeld.
  DATA: lt_wo          TYPE /scwm/tt_who_int,
        lt_ordim_o     TYPE /scwm/tt_ordim_o,
        lt_ordim_c     TYPE /scwm/tt_ordim_c,
        lt_change_att  TYPE /scwm/tt_to_change_att,
        lt_bapiret     TYPE bapirettab,
        lt_orig_pickhu TYPE /scwm/tt_whohu_int.
  DATA: lo_log         TYPE REF TO /scwm/cl_log.
  FIELD-SYMBOLS: <fs_ordim_o> TYPE /scwm/ordim_o,
                 <fs_ordim_c> TYPE /scwm/ordim_c,
                 <fs_wo>      TYPE LINE OF /scwm/tt_who_int,
                 <ls_bapiret> TYPE bapiret2.

  gt_who = it_who.

* initialize log
  PERFORM who_logno_get USING iv_lgnum.
  PERFORM lgnum_check_logging USING iv_lgnum.
* everything will be logged on warehouse level
  MOVE: gc_log_lgnum TO gs_log-act,
        '4'          TO gs_log-actglob.

  "If there are already message from the caller, we add them to the beginning of the WHOLOG
  IF it_bapiret IS NOT INITIAL.
    LOOP AT it_bapiret ASSIGNING <ls_bapiret>.
      PERFORM who_write_log_bapiret USING gc_msg_lvl1 <ls_bapiret> CHANGING gs_log-handle.
    ENDLOOP.
  ENDIF.

  MESSAGE i000 INTO lv_mtext.
  PERFORM who_write_log USING gc_msg_lvl1 CHANGING gs_log-handle.

* read TOs
  CALL FUNCTION '/SCWM/TO_READ_MULT'
    EXPORTING
      iv_lgnum    = iv_lgnum
      ir_tanum    = ir_tanum
    IMPORTING
      et_ordim_o  = lt_ordim_o
      et_ordim_c  = lt_ordim_c
    EXCEPTIONS
      wrong_input = 1
      not_found   = 2
      OTHERS      = 3.
  IF sy-subrc <> 0.
    PERFORM who_write_log USING gc_msg_lvl1 CHANGING gs_log-handle.
  ELSE.
    lv_lines = LINES( lt_ordim_o ).
    lv_lines = lv_lines + LINES( lt_ordim_c ).
    MESSAGE i075 WITH lv_lines INTO lv_mtext.
    PERFORM who_write_log USING gc_msg_lvl1 CHANGING gs_log-handle.
  ENDIF.

* already confirmed TOs cannot be re-bundled
  LOOP AT lt_ordim_c ASSIGNING <fs_ordim_c>.
*   add message to log
    MESSAGE w354(/scwm/l3) WITH <fs_ordim_c>-tanum
                           INTO lv_mtext.
    PERFORM who_write_log USING gc_msg_lvl2 CHANGING gs_log-handle.
  ENDLOOP. "lt_ordim_o

  IF lt_ordim_o IS NOT INITIAL.
*   Check if Labor Management is partially Active -> Warning Message
    PERFORM rebundle_check_lm USING lt_ordim_o.

    "check whether pick HUs of old WOs should be taken over and if it
    "is possible at all
    PERFORM rebundle_check_pickhu USING iv_lgnum iv_keep_pickhu lt_ordim_o
                               CHANGING lv_keep_pickhu lt_orig_pickhu.

*   unassign TOs from current warehouse orders
*   this implies deletion/confirmation of WOs
*   additionally not needed pick HUs will be deleted
    PERFORM rebundle_unassign_to
       USING iv_lgnum  lv_keep_pickhu  lt_ordim_o.

*   all TOs have to be part of the same aarea and queue
*   => create one warehouse order per aarea/queue
    PERFORM rebundle_create_wo
            USING iv_lgnum        is_wo_template
                  lt_orig_pickhu  lt_ordim_o
         CHANGING lt_wo     lt_change_att.
  ELSE.
*   add message to log
    MESSAGE i014(/scwm/wm_sel) INTO lv_mtext.
    PERFORM who_write_log USING gc_msg_lvl2 CHANGING gs_log-handle.
  ENDIF.

*call badi for EEW fields
  data: lt_ordim_o_int TYPE /SCWM/TT_ORDIM_O_INT,
        ls_ordim_o_int type /SCWM/S_ORDIM_O_INT.
  loop at lt_ordim_o assigning <fs_ordim_o>.
    clear: ls_ordim_o_int.
    move-corresponding <fs_ordim_o> to ls_ordim_o_int.
    append ls_ordim_o_int to lt_ordim_o_int.
  endloop.
  PERFORM wo_badi_update_eew USING iv_lgnum
                                   lt_orig_pickhu
                                   lt_ordim_o_int
                             CHANGING lt_wo.

* post data
  IF lt_wo IS NOT INITIAL.
    "WT (read) buffer has to be refreshed to reflect changed WTs
    CALL METHOD /scwm/cl_tm=>register_cleanup_wt( ).

* Setting start time
    READ TABLE lt_wo ASSIGNING <fs_wo> INDEX 1.

    IF sy-subrc = 0.
      GET TIME STAMP FIELD <fs_wo>-started_at.
    ENDIF.

    APPEND LINES OF lt_wo TO gt_who.
    SORT gt_who BY lgnum who.

    PERFORM who_db_update.

*   update TOs with new WO
    CALL FUNCTION '/SCWM/TO_CHANGE_ATT'
      EXPORTING
        iv_lgnum             = iv_lgnum
*       IV_SUBST             = ' '
*       IV_QNAME             = SY-UNAME
*        iv_simulate          = iv_simulate
        iv_update_task       = 'X'
        iv_commit_work       = ' '
*       IV_AUSFB             = ' '
        it_change_att        = lt_change_att
      IMPORTING
        et_bapiret           = lt_bapiret
        ev_severity          = lv_severity.
    IF lt_bapiret IS NOT INITIAL.
      MESSAGE e406 INTO lv_mtext.
      PERFORM who_write_log USING gc_msg_lvl1 CHANGING gs_log-handle.
*      RAISE EXCEPTION TYPE /scwm/cx_core.
    ENDIF.
  ENDIF.

* export WO log no
  IF ev_logno IS REQUESTED.
    ev_logno = gs_log-logid.
  ENDIF.
* map WO log to et_bapiret
  IF ev_severity IS REQUESTED OR et_bapiret IS REQUESTED.
    PERFORM map_wo_log2bapiret CHANGING ev_severity et_bapiret.
  ENDIF.
* export WOs
  IF et_wo IS REQUESTED.
    et_wo = lt_wo.
  ENDIF.
* save the log
  IF iv_write_log IS NOT INITIAL.
    PERFORM who_save_log USING gs_log-handle.
  ENDIF.

  IF ev_severity CA 'EAX'.
    "On error we have to rollback otherwise we could get WT w/o WO assigned
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.

* initialize global WO tables after COMMIT
  /scwm/cl_tm=>cleanup( EXPORTING iv_reason = /SCMB/IF_SP_TRANSACTION=>sc_cleanup_commit ).
ENDFUNCTION.
