FUNCTION zmdm_multi_field_input.
*"--------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(LS_SELFIELDS) LIKE  SE16N_SELFIELDS
*"  STRUCTURE  SE16N_SELFIELDS
*"     VALUE(LD_DISPLAY) TYPE  CHAR1 OPTIONAL
*"     VALUE(LD_CHECK_INPUT_FUNCNAME) TYPE  FUNCNAME OPTIONAL
*"     VALUE(LD_CURRENCY) TYPE  SYCURR OPTIONAL
*"  TABLES
*"      LT_MULTI_SELECT STRUCTURE  SE16N_SELFIELDS OPTIONAL
*"      LT_EXCLUDE_SELOPT STRUCTURE  SE16N_SEL_OPTION OPTIONAL
*"--------------------------------------------------------------------
  DATA: ld_lines  LIKE sy-tabix.
  DATA: ls_dfies  LIKE dfies.
  DATA: ld_lfieldname   LIKE dfies-lfieldname.
  DATA: ld_sign         LIKE se16n_selfields-sign.
  DATA: ls_multi_select LIKE se16n_selfields.

*.global variables for list output
  gd_fieldname = ls_selfields-fieldname.
  gd_scrtext_m = ls_selfields-scrtext_m.
  gd_datatype  = ls_selfields-datatype.
  gd_currency  = ld_currency.

*.in external call the DDIC-Info is not available
  IF ls_selfields-intlen IS INITIAL.
    ld_lfieldname = ls_selfields-fieldname.
    IF ls_selfields-sign <> space.
      ld_sign = ls_selfields-sign.
    ELSE.
      ld_sign = 'I'.
    ENDIF.
    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = ls_selfields-tabname
        lfieldname     = ld_lfieldname
      IMPORTING
        dfies_wa       = ls_dfies
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3.
    IF sy-subrc = 0.
      MOVE-CORRESPONDING ls_dfies TO ls_selfields.
      ls_selfields-sign = ld_sign.
    ENDIF.
  ENDIF.

*.SE16N anyway has gt_selfields filled. Other applications not,
*.but it is needed --> take new structure
  gs_multi_sel = ls_selfields.
  IF gd-tab = space.
    gd-tab = ls_selfields-tabname.
  ENDIF.

*.exit to check input data
  gd_chk_inp_func = ld_check_input_funcname.

*.display mode active ?
  gd_mf_display = ld_display.

*.exclude select-options
  gt_excl_selopt[] = lt_exclude_selopt[].

*.Initialize the possible entry fields, depending on select option
  IF gt_sel_init[] IS INITIAL.
    PERFORM init_sel_opt.
  ENDIF.

  CLEAR: gt_multi_select, lv_cursor_line.
  REFRESH gt_multi_select.
  DESCRIBE TABLE lt_multi_select LINES ld_lines.
*.Already input in there
  IF ld_lines > 0.
*....add DDIC-Info if not available
    CLEAR ls_dfies.
    MOVE-CORRESPONDING ls_selfields TO ls_dfies.
    LOOP AT lt_multi_select WHERE intlen IS INITIAL.
      MOVE-CORRESPONDING lt_multi_select TO ls_multi_select.
      MOVE-CORRESPONDING ls_dfies TO lt_multi_select.
      lt_multi_select-sign = ls_multi_select-sign.
      MODIFY lt_multi_select.
    ENDLOOP.
    gt_multi_select[] = lt_multi_select[].
*....although there are already lines, open new lines as well
*....if no empty ones are available
    LOOP AT gt_multi_select
          WHERE low  = space
            AND high = space
            AND option = space.
    ENDLOOP.
    IF sy-subrc <> 0.
      READ TABLE gt_multi_select INDEX 1.
      CLEAR: gt_multi_select-low,
             gt_multi_select-high,
             gt_multi_select-sign,
             gt_multi_select-option.
      DO new_lines TIMES.
        APPEND gt_multi_select.
      ENDDO.
    ENDIF.
*.not yet any input made
  ELSE.
    MOVE-CORRESPONDING ls_selfields TO gt_multi_select.
    CLEAR: gt_multi_select-low,
           gt_multi_select-high.
    DO new_lines TIMES.
      APPEND gt_multi_select.
    ENDDO.
  ENDIF.

  CALL SCREEN 0600 STARTING AT 74 3 ENDING AT 78 15.

  CASE save_fcode.
*...Cancel, do not change the table
    WHEN '&F12'.
*...Take new input
    WHEN 'TAKE'.
      REFRESH lt_multi_select.
      LOOP AT gt_multi_select WHERE ( NOT ( low IS INITIAL ) OR
                                      NOT ( high IS INITIAL ) ) OR
                                      NOT option IS INITIAL.   "e.g. EQ space
        MOVE-CORRESPONDING gt_multi_select TO lt_multi_select.
        APPEND lt_multi_select.
      ENDLOOP.
  ENDCASE.
  CLEAR gd_currency.

ENDFUNCTION.
