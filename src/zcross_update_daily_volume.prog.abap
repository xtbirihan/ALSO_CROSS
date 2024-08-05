*&---------------------------------------------------------------------*
*& Report ZCROSS_UPDATE_DAILY_VOLUME
*&---------------------------------------------------------------------*
**********************************************************************
*& Key           : LH-050423
*& Request No.   : GAP-041 – Update Daily Volume in Product Master
**********************************************************************
*& Description (short)
*& Select WTs, calculate Daily Volume and write to to product master
**********************************************************************

REPORT zcross_update_daily_volume.

TYPES:
  BEGIN OF ty_mat_quant,
    matnr    TYPE matnr,
    demqty   TYPE /scwm/de_demqty,
    entitled TYPE /scwm/de_entitled,
  END OF ty_mat_quant,
  tt_mat_quant TYPE STANDARD TABLE OF ty_mat_quant WITH DEFAULT KEY.

DATA: go_log TYPE REF TO /scwm/cl_log.

PARAMETERS p_lgnum TYPE /scwm/s_prod_mon_out-lgnum OBLIGATORY.
PARAMETERS p_varprg TYPE  rsvar-report DEFAULT '/SCWM/SAPLWO_TO_MON' NO-DISPLAY.
PARAMETERS p_vari TYPE rsvar-variant OBLIGATORY.
PARAMETERS p_dsplog TYPE flag DEFAULT 'X' NO-DISPLAY.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM get_vari_f4.

LOAD-OF-PROGRAM.


END-OF-SELECTION.

  PERFORM main.

*&---------------------------------------------------------------------*
*& Form main
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM main .

  DATA:
    lt_bapiret       TYPE  bapiret2_tab,
    lt_matkey	       TYPE /scwm/cl_mon_prod=>tt_ext_matkey,
    lt_matkeyx       TYPE /scwm/cl_mon_prod=>tt_ext_matkeyx,
    lt_matlwh_update TYPE /scwm/cl_mon_prod=>tt_ext_matlwh,
    lt_matlwhx       TYPE /scwm/cl_mon_prod=>tt_ext_matlwhx,
    lt_wt            TYPE /scwm/tt_to_det_mon_out,
    lv_days          TYPE i,
    lt_mat_quant     TYPE STANDARD TABLE OF ty_mat_quant,
    lv_error         TYPE abap_bool,
    lx_root          TYPE REF TO cx_root ##needed.

  /scwm/cl_tm=>set_lgnum( p_lgnum ).
  go_log = NEW /scwm/cl_log( ).

  "Get number of working days according to the warehouse calendar
  PERFORM get_nof_days
    CHANGING lv_days lv_error.

  "Get the warehouse tasks from the monitor
  IF lv_error EQ abap_false.
    TRY.
        CALL FUNCTION '/SCWM/TO_MON'
          EXPORTING
            iv_lgnum   = p_lgnum                 " Lagernummer/Lagerkomplex
            iv_variant = p_vari                 " ABAP: Name einer Variante (ohne Programmname)
            iv_mode    = /scwm/cl_wme_monitor_srvc=>c_mode_exec              " Modus für Funktionsbausteine im WM Monitor
          IMPORTING
            et_data    = lt_wt.                  " Transportauftragsdetail für WME Monitor Ausgabe

        DELETE lt_wt WHERE rdoccat NE /scdl/if_dl_doc_c=>sc_doccat_out_prd.

        PERFORM calculate_dem_qty
          USING lt_wt lv_days
          CHANGING lt_mat_quant lv_error.
      CATCH /scwm/cx_mon_noexec /scwm/cx_core INTO lx_root.
        MESSAGE e009(zmc_crs) INTO DATA(lv_msg) ##needed.
        go_log->add_message( ).
        lv_error = abap_true.
    ENDTRY.
  ENDIF.

  "Update product master
  IF lv_error EQ abap_false AND lt_mat_quant IS NOT INITIAL.
    DATA(lo_mon_prod_service) = NEW /scwm/cl_mon_prod( ).

    lt_matkey        = VALUE #( FOR mid IN lt_mat_quant ( ext_matnr = mid-matnr ) ).
    lt_matkeyx       = VALUE #( FOR mid IN lt_mat_quant ( ext_matnr = mid-matnr ) ).
    lt_matlwhx       = VALUE #( FOR mid IN lt_mat_quant ( ext_matnr = mid-matnr ext_entity = p_lgnum ext_entitled = mid-entitled demqty = abap_true ) ).
    lt_matlwh_update = VALUE #( FOR mid IN lt_mat_quant ( ext_matnr = mid-matnr ext_entity = p_lgnum ext_entitled = mid-entitled demqty = ceil( mid-demqty ) ) ).

    SORT lt_matlwhx BY ext_matnr ext_entity ext_entitled. "It is needed because internal logic of the following call

    lo_mon_prod_service->wh_prod_mass_change(
      EXPORTING
        it_matkey        = lt_matkey
        it_matkeyx       = lt_matkeyx
        it_matlwh_update = lt_matlwh_update
        it_matlwhx       = lt_matlwhx
      IMPORTING
        et_return        = lt_bapiret
        ev_lines         = DATA(lv_lines) ).

    go_log->add_log( it_prot = lt_bapiret ).
    IF lv_lines NE 0.
      MESSAGE i008(zmc_crs) WITH lv_lines INTO lv_msg.
      go_log->add_message( ).
    ENDIF.
  ENDIF.

  go_log->save_applog(
    EXPORTING
      is_log       = VALUE #( extnumber = |Warehouse { p_lgnum }, Variant { p_vari } | object = zif_wme_c=>gs_msgobj-zewm subobject = zif_wme_c=>gs_msgsubobj-zwho_upd_dem_qty )
    IMPORTING
      ev_loghandle = DATA(lv_loghandle)                  " Log Handle
  ) ##NO_TEXT.
  TRY.
      go_log->save_applog2db( iv_loghandle = lv_loghandle ).
    CATCH /scwm/cx_basics ##NO_HANDLER.
  ENDTRY.
  IF p_dsplog EQ abap_true.
    TRY.
        go_log->display_log( iv_loghandle = lv_loghandle ).
      CATCH /scwm/cx_basics ##NO_HANDLER.
    ENDTRY.
  ELSE.
    IF go_log->get_severity( ) NA wmegc_severity_ea.
      MESSAGE s008(zmc_crs) WITH lv_lines.
    ELSE.
      MESSAGE i014(zmc_crs) DISPLAY LIKE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.



*&---------------------------------------------------------------------*
*& Form get_vari_f4
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_vari_f4 .

  CALL FUNCTION 'RS_VARIANT_CATALOG'
    EXPORTING
      report        = p_varprg
      internal_call = 'X'
    IMPORTING
      sel_variant   = p_vari
    EXCEPTIONS
      OTHERS        = 0.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form get_nof_days
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LV_DAYS
*&      <-- LV_ERROR
*&---------------------------------------------------------------------*
FORM get_nof_days  CHANGING cv_days TYPE i
                            cv_error TYPE abap_bool.
  DATA:
    lt_holiday TYPE STANDARD TABLE OF iscal_day,
    lt_valutab TYPE STANDARD TABLE OF rsparams,
    lv_dfrom   TYPE d,
    lv_dto     TYPE d.

  CALL FUNCTION 'RS_VARIANT_CONTENTS'
    EXPORTING
      report               = p_varprg               " Report name
      variant              = p_vari                " Variant name
    TABLES
      valutab              = lt_valutab     " Table that contains the values (P + S)
    EXCEPTIONS
      variant_non_existent = 1                " Variant does not exist
      variant_obsolete     = 2                " Report does not exist
      OTHERS               = 3.
  IF sy-subrc <> 0.
    go_log->add_message( ).
    cv_error = abap_true.
    EXIT.
  ENDIF.

  READ TABLE lt_valutab INTO DATA(ls_dfrom)
       WITH KEY selname = 'P_CODFR'.

  READ TABLE lt_valutab INTO DATA(ls_dto)
       WITH KEY selname = 'P_CODTO'.

  SELECT SINGLE FROM /scwm/t340d
         FIELDS factory_calendar
         WHERE lgnum EQ @p_lgnum
         INTO @DATA(lv_fact_cal).

  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external            = ls_dfrom-low                 " external date formatting
    IMPORTING
      date_internal            = lv_dfrom                 " internal date formatting
    EXCEPTIONS
      date_external_is_invalid = 1                " the external date is invalid (not plausible)
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    go_log->add_message( ).
    cv_error = abap_true.
    RETURN.
  ENDIF.

  CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
    EXPORTING
      date_external            = ls_dto-low                 " external date formatting
    IMPORTING
      date_internal            = lv_dto                 " internal date formatting
    EXCEPTIONS
      date_external_is_invalid = 1                " the external date is invalid (not plausible)
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    go_log->add_message( ).
    go_log->add_message( ).
    cv_error = abap_true.
    RETURN.
  ENDIF.

  cv_days = lv_dto - lv_dfrom + 1.

  CALL FUNCTION 'HOLIDAY_GET'
    EXPORTING
      factory_calendar           = lv_fact_cal            " Factory calendar ID
      date_from                  = lv_dfrom
      date_to                    = lv_dto
    TABLES
      holidays                   = lt_holiday
    EXCEPTIONS
      factory_calendar_not_found = 1                " Factory calendar is not in buffer
      holiday_calendar_not_found = 2                " Holiday calendar is not in buffer
      date_has_invalid_format    = 3
      date_inconsistency         = 4
      OTHERS                     = 5.
  IF sy-subrc <> 0.
    go_log->add_message( ).
    cv_error = abap_true.
    RETURN.
  ENDIF.

  cv_days = cv_days - lines( lt_holiday ).


ENDFORM.
*&---------------------------------------------------------------------*
*& Form calculate_dem_qty
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_WT
*&      <-- LT_MAT_QUANT
*&---------------------------------------------------------------------*
FORM calculate_dem_qty  USING    it_wt TYPE /scwm/tt_to_det_mon_out
                                 iv_days TYPE i
                        CHANGING ct_mat_quant TYPE tt_mat_quant
                                 cv_error TYPE abap_bool.

  DATA: lt_map  TYPE /scwm/tt_matid_matnr,
        lx_root TYPE REF TO cx_root ##needed.

  LOOP AT it_wt ASSIGNING FIELD-SYMBOL(<wt>) GROUP BY ( matnr = <wt>-matnr entitled = <wt>-entitled ) ASSIGNING FIELD-SYMBOL(<ls_grp_mat>).
    SELECT FROM /sapapo/matkey
      FIELDS meins
      WHERE matnr EQ @<ls_grp_mat>-matnr
      INTO TABLE @DATA(lt_meins)
      UP TO 1 ROWS.
    DATA(lv_meins) = lt_meins[ 1 ]-meins.

    APPEND INITIAL LINE TO ct_mat_quant REFERENCE INTO DATA(lr_mat_quant).
    DATA(lv_last_insert) = sy-tabix.

    lr_mat_quant->matnr = <ls_grp_mat>-matnr.
    lr_mat_quant->entitled = <ls_grp_mat>-entitled.
    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_RANGE'
          EXPORTING
            it_matnr_range = VALUE rseloption( ( sign = 'I' option = 'EQ' low = lr_mat_quant->matnr ) )
          IMPORTING
            et_matid       = lt_map.
      CATCH /scwm/cx_md_interface /scwm/cx_md_internal_error INTO lx_root.
        MESSAGE e006(zmc_crs) WITH lr_mat_quant->matnr INTO DATA(lv_msg) ##needed.
        go_log->add_message( ).
        cv_error = abap_true.
        RETURN.
    ENDTRY.

    DATA(lv_error_for_mat) = abap_false.
    LOOP AT GROUP <ls_grp_mat> REFERENCE INTO DATA(lr_wt).

      IF lr_wt->meins EQ lv_meins.
        DATA(lv_quantity) = CONV /scwm/de_quantity( lr_wt->vsolm ).
      ELSE.
        TRY.
            CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
              EXPORTING
                iv_matid     = lt_map[ 1 ]-matid                 " Material GUID16  mit Konvertierungsexit
                iv_quan      = CONV /scwm/de_quantity( lr_wt->vsolm )                " Mengenfeld
                iv_unit_from = lr_wt->meins                 " Mengeneinheit
                iv_unit_to   = lv_meins                 " Mengeneinheit
                iv_batchid   = VALUE /scwm/de_batchid( )                 " Charge
              IMPORTING
                ev_quan      = lv_quantity.                  " Mengenfeld
          CATCH /scwm/cx_md_interface     " Import Parameter fehlerhaft
               /scwm/cx_md_batch_required     " Charge ist zur Umrechnung notwendig
               /scwm/cx_md_internal_error     " Interner Fehler
               /scwm/cx_md_batch_not_required " Material nicht Chargenpflichtig, Charge nicht benötigt
               /scwm/cx_md_material_exist
               INTO lx_root.    " Material existiert nicht

            lv_error_for_mat = abap_true.
            MESSAGE e007(zmc_crs) WITH lr_wt->meins lv_meins lr_mat_quant->matnr INTO lv_msg.
            go_log->add_message( ).
        ENDTRY.
      ENDIF.
      ADD lv_quantity TO lr_mat_quant->demqty.
    ENDLOOP.
    IF lv_error_for_mat EQ abap_true.
      DELETE ct_mat_quant INDEX lv_last_insert.
    ENDIF.
  ENDLOOP.
  LOOP AT ct_mat_quant REFERENCE INTO lr_mat_quant.
    DIVIDE lr_mat_quant->demqty BY iv_days.
  ENDLOOP.

ENDFORM.
