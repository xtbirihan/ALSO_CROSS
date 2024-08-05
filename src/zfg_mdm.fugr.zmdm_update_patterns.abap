FUNCTION zmdm_update_patterns.
*"----------------------------------------------------------------------
*"*"Update Function Module:
*"
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IS_SECTION_1) TYPE  ZSTR_PRODUCT_CHARACTERISTICS
*"     VALUE(IS_SECTION_3) TYPE  ZSTR_IDENT_NUMBERS
*"----------------------------------------------------------------------
  DATA: ls_patterns TYPE ztcross_patterns,
        lt_patterns TYPE TABLE OF ztcross_patterns,
        lv_counter  TYPE i.
  IF is_section_1-lgnum IS INITIAL OR is_section_1-matnr IS INITIAL.
    RETURN.
  ENDIF.

  DELETE FROM ztcross_patterns WHERE lgnum = is_section_1-lgnum
                                 AND matnr = is_section_1-matnr.

  IF is_section_3-reason_code01 EQ '01' OR
     is_section_3-reason_code01 EQ '02' OR
     is_section_3-reason_code01 EQ '03'.
    RETURN.
  ENDIF.
  IF is_section_3-indenttab01 IS NOT INITIAL.
    LOOP AT is_section_3-t_indenttab01 INTO DATA(ls_indenttab).
      ls_patterns-mandt    = sy-mandt.
      ls_patterns-lgnum    = is_section_1-lgnum.
      ls_patterns-matnr    = is_section_1-matnr.
      ls_patterns-id_type  = is_section_3-indenttab01.
      ls_patterns-template = ls_indenttab-template.
      lv_counter += 1.
      ls_patterns-counter = lv_counter .
      APPEND ls_patterns TO lt_patterns.
      CLEAR: ls_patterns.
    ENDLOOP.
  ENDIF.
  CLEAR: lv_counter.

  IF is_section_3-indenttab02 IS NOT INITIAL.
    LOOP AT is_section_3-t_indenttab02 INTO ls_indenttab.
      ls_patterns-mandt    = sy-mandt.
      ls_patterns-lgnum    = is_section_1-lgnum.
      ls_patterns-matnr    = is_section_1-matnr.
      ls_patterns-id_type  = is_section_3-indenttab02.
      ls_patterns-template = ls_indenttab-template.
      lv_counter += 1.
      ls_patterns-counter = lv_counter .
      APPEND ls_patterns TO lt_patterns.
      CLEAR: ls_patterns.
    ENDLOOP.
  ENDIF.
  CLEAR: lv_counter.


  IF is_section_3-indenttab03 IS NOT INITIAL.
    LOOP AT is_section_3-t_indenttab03 INTO ls_indenttab.
      ls_patterns-mandt    = sy-mandt.
      ls_patterns-lgnum    = is_section_1-lgnum.
      ls_patterns-matnr    = is_section_1-matnr.
      ls_patterns-id_type  = is_section_3-indenttab03.
      ls_patterns-template = ls_indenttab-template.
      lv_counter += 1.
      ls_patterns-counter = lv_counter .
      APPEND ls_patterns TO lt_patterns.
      CLEAR: ls_patterns.
    ENDLOOP.
  ENDIF.
  CLEAR: lv_counter.

  IF is_section_3-indenttab04 IS NOT INITIAL.
    LOOP AT is_section_3-t_indenttab04 INTO ls_indenttab.
      ls_patterns-mandt    = sy-mandt.
      ls_patterns-lgnum    = is_section_1-lgnum.
      ls_patterns-matnr    = is_section_1-matnr.
      ls_patterns-id_type  = is_section_3-indenttab04.
      ls_patterns-template = ls_indenttab-template.
      lv_counter += 1.
      ls_patterns-counter = lv_counter .
      APPEND ls_patterns TO lt_patterns.
      CLEAR: ls_patterns.
    ENDLOOP.
  ENDIF.
  CLEAR: lv_counter.


  IF is_section_3-indenttab05 IS NOT INITIAL.
    LOOP AT is_section_3-t_indenttab05 INTO ls_indenttab.
      ls_patterns-mandt    = sy-mandt.
      ls_patterns-lgnum    = is_section_1-lgnum.
      ls_patterns-matnr    = is_section_1-matnr.
      ls_patterns-id_type  = is_section_3-indenttab05.
      ls_patterns-template = ls_indenttab-template.
      lv_counter += 1.
      ls_patterns-counter = lv_counter .
      APPEND ls_patterns TO lt_patterns.
      CLEAR: ls_patterns.
    ENDLOOP.
  ENDIF.
  CLEAR: lv_counter.
  IF lt_patterns IS NOT INITIAL.
    MODIFY ztcross_patterns FROM TABLE lt_patterns.
  ENDIF.


ENDFUNCTION.
