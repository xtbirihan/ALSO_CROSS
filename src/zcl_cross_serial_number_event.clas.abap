CLASS zcl_cross_serial_number_event DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF c_const,
        save      TYPE fcode VALUE 'SAVE',
        processed TYPE char1 VALUE 'P',
        delete    TYPE char1 VALUE 'D',
        new       TYPE char1 VALUE 'N',
        mandt     TYPE dd03d-fieldname VALUE 'MANDT',
        lgnum     TYPE dd03d-fieldname VALUE 'LGNUM',
        selnum    TYPE dd03d-fieldname VALUE 'SELNUM',
        action    TYPE dd03d-fieldname VALUE 'ACTION',
        spras     TYPE dd03d-fieldname VALUE 'SPRAS',
      END OF c_const.

    TYPES: BEGIN OF lty_restore.
             INCLUDE TYPE ztcross_numbers.
    TYPES:   action TYPE char1,
             mark   TYPE char1,
           END OF lty_restore.
    TYPES: ltty_restore TYPE TABLE OF lty_restore.
    DATA: mt_restore       TYPE ltty_restore,
          mv_fcode         TYPE fcode,
          mv_action        TYPE char1,
          mr_total_restore TYPE REF TO data.


    METHODS constructor
      IMPORTING
        !it_total_restore TYPE REF TO data
        !iv_fcode         TYPE char20
        !iv_action        TYPE char1 .

    METHODS save.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS handle_records
      IMPORTING iv_action1 TYPE char1
                iv_action2 TYPE char1
                it_store   TYPE ltty_restore.
ENDCLASS.



CLASS ZCL_CROSS_SERIAL_NUMBER_EVENT IMPLEMENTATION.


  METHOD constructor.
    FIELD-SYMBOLS: <lt_restore> TYPE ltty_restore.
    ASSIGN it_total_restore->* TO <lt_restore>.
    mt_restore = <lt_restore>.
    mv_fcode   = iv_fcode.
    mv_action  = iv_action.
    mr_total_restore = it_total_restore.
  ENDMETHOD.


  METHOD handle_records.

    DATA: lt_numbers      TYPE STANDARD TABLE OF ztcross_numbers.
    DATA: lt_restore_cont TYPE ltty_restore.

    FIELD-SYMBOLS: <lt_restore> TYPE ltty_restore.


    LOOP AT mt_restore ASSIGNING FIELD-SYMBOL(<ls_restore>).
      ASSIGN COMPONENT c_const-mandt  OF STRUCTURE <ls_restore> TO FIELD-SYMBOL(<lv_mandt>).
      ASSIGN COMPONENT c_const-lgnum  OF STRUCTURE <ls_restore> TO FIELD-SYMBOL(<lv_lgnum>).
      ASSIGN COMPONENT c_const-selnum OF STRUCTURE <ls_restore> TO FIELD-SYMBOL(<lv_selnum>).
      ASSIGN COMPONENT c_const-action OF STRUCTURE <ls_restore> TO FIELD-SYMBOL(<lv_action>).
      ASSIGN COMPONENT c_const-spras  OF STRUCTURE <ls_restore> TO FIELD-SYMBOL(<lv_spras>).
      IF <lv_mandt>  IS ASSIGNED AND
         <lv_lgnum>  IS ASSIGNED AND
         <lv_selnum> IS ASSIGNED AND
         <lv_action> IS ASSIGNED AND
         <lv_spras>  IS ASSIGNED AND
         <lv_action> EQ iv_action1.
        LOOP AT it_store ASSIGNING FIELD-SYMBOL(<ls_restore2>) WHERE mandt  = <lv_mandt>
                                                                 AND lgnum  = <lv_lgnum>
                                                                 AND selnum = <lv_selnum>
                                                                 AND spras  <> <lv_spras>
                                                                 AND action <> <lv_action>.
          EXIT.
        ENDLOOP.
        IF sy-subrc NE 0.
          APPEND VALUE #(  mandt = <lv_mandt>
                           lgnum = <lv_lgnum>
                           selnum = <lv_selnum> ) TO lt_numbers.
        ENDIF.
        <ls_restore>-mark = c_const-processed.
      ENDIF.
      IF <lv_action> EQ iv_action2.
        APPEND <ls_restore> TO lt_restore_cont.
      ENDIF.
      IF <lv_mandt>  IS ASSIGNED AND
         <lv_lgnum>  IS ASSIGNED AND
         <lv_selnum> IS ASSIGNED AND
         <lv_action> IS ASSIGNED AND
         <lv_spras>  IS ASSIGNED .
        UNASSIGN:<lv_mandt>,  <lv_lgnum>, <lv_selnum>, <lv_action>, <lv_spras>.
      ENDIF.
    ENDLOOP.

    DELETE mt_restore WHERE mark = c_const-processed.
    SORT lt_numbers BY mandt lgnum selnum.
    DELETE ADJACENT DUPLICATES FROM lt_numbers COMPARING mandt lgnum selnum.

    IF lt_numbers IS NOT INITIAL AND mv_action IS INITIAL.
      INSERT ztcross_numbers FROM TABLE lt_numbers.
*      MODIFY ztcross_numbers FROM TABLE lt_numbers.
    ELSEIF lt_numbers IS NOT INITIAL AND mv_action EQ c_const-delete.
      DELETE ztcross_numbers FROM TABLE lt_numbers.
    ENDIF.
    IF lt_restore_cont IS NOT INITIAL.
      me->handle_records(
        EXPORTING
          iv_action1 = iv_action2
          iv_action2 = iv_action1
          it_store   = lt_restore_cont
      ).
    ENDIF.

  ENDMETHOD.


  METHOD save.
    FIELD-SYMBOLS: <lt_restore> TYPE ltty_restore.
    IF mv_fcode NE c_const-save.
      RETURN.
    ENDIF.

    LOOP AT mt_restore ASSIGNING FIELD-SYMBOL(<ls_restore>) WHERE action IS NOT INITIAL
                                                              AND mark <> c_const-processed.
      LOOP AT mt_restore ASSIGNING FIELD-SYMBOL(<ls_restore2>) WHERE lgnum  = <ls_restore>-lgnum
                                                                 AND selnum = <ls_restore>-selnum
                                                                 AND action <> <ls_restore>-action .
        <ls_restore2>-mark = c_const-processed.
        MODIFY mt_restore  FROM <ls_restore2> TRANSPORTING mark WHERE lgnum  = <ls_restore>-lgnum
                                                                  AND selnum = <ls_restore>-selnum.
        EXIT.
      ENDLOOP.
    ENDLOOP.
    DELETE mt_restore WHERE mark EQ c_const-processed.
    IF mt_restore IS INITIAL.
      RETURN.
    ENDIF.
    IF mv_action IS INITIAL.
      ASSIGN mr_total_restore->* TO <lt_restore>.
      me->handle_records(
        EXPORTING
          iv_action1 = c_const-new
          iv_action2 = c_const-delete
          it_store   = <lt_restore>
      ).
    ELSEIF mv_action EQ c_const-delete.
      ASSIGN mr_total_restore->* TO <lt_restore>.
      me->handle_records(
        EXPORTING
          iv_action1 = c_const-delete
          iv_action2 = c_const-new
          it_store   = <lt_restore>
      ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
