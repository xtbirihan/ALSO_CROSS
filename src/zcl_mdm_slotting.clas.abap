CLASS zcl_mdm_slotting DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA mv_lgnum                       TYPE /scwm/lgnum.
    DATA mv_entitled                    TYPE /scwm/de_entitled.
    DATA mv_matid                       TYPE /scwm/de_matid ##NEEDED.
    DATA mv_matnr                       TYPE /scwm/de_matnr.
    DATA mt_matnr                       TYPE rseloption.
    DATA mt_matnr_all                   TYPE rseloption.
    DATA mt_matid_matnr                 TYPE /scwm/tt_matid_matnr.
    DATA mt_matid_matnr_pack            TYPE /scwm/tt_matid_matnr.
    DATA mt_mat_global                  TYPE /scwm/tt_material_global.
    DATA mt_mat_slot                    TYPE /scwm/tt_material_con_intern.
    DATA mt_mat_slot_all                TYPE /scwm/tt_material_con_intern.
    DATA mt_mat_prot                    TYPE /scwm/tt_mat_prot.
    DATA mt_mat_sel                     TYPE lvc_t_row.
    DATA ms_slot_steps                  TYPE /scwm/s_slot_steps.
    DATA mt_mat_alv                     TYPE /scwm/tt_material_con.
    DATA mt_mat_lgnum                   TYPE /scwm/tt_material_lgnum.
    DATA mt_matid                       TYPE /scwm/tt_matid.
    DATA mv_upd_mode                    TYPE /scwm/de_updmode VALUE '1' ##NO_TEXT.
    DATA mv_tzone                       TYPE tznzone. " timezone of warehouse
    DATA mv_transaction_slot            TYPE c LENGTH 1 VALUE 'X' ##NO_TEXT.
    DATA mo_concore                     TYPE REF TO /scwm/cl_concepting_core. "#EC NEEDED.
    DATA mv_variant_name                TYPE rsvar-variant.
    DATA mv_jobname                     TYPE tbtcjob-jobname.
    DATA mi_badi_slot_det_update_tables TYPE REF TO /scwm/ex_slot_sav_upd_tab.
    DATA mi_badi_slot_upd_storg_type    TYPE REF TO /scwm/ex_slot_upd_storge_ty.
    DATA mv_badi_sttype_upd_mode        TYPE i.
    " Tables to update DB from all Slotting Runs
    DATA mt_mat_lgnum_result_sel        TYPE /scwm/tt_material_lgnum.
    DATA mt_mat_lgtyp_result_sel        TYPE /scwm/tt_material_lgtyp.
    DATA mt_mat_lgtyp_orig_sel          TYPE /scwm/tt_material_lgtyp.
    DATA mt_binmat_result_sel           TYPE /scwm/tt_binmat.
    " Tables returned by current Slotting Run
    DATA mt_mat_lgnum_result            TYPE /scwm/tt_material_lgnum.
    DATA mt_mat_lgtyp_result            TYPE /scwm/tt_material_lgtyp.
    DATA mt_binmat_result               TYPE /scwm/tt_binmat.
    " Tables to update DB (just before update)
    DATA mt_mat_lgnum_update            TYPE /scwm/tt_material_lgnum.
    DATA mt_mat_lgtyp_update            TYPE /scwm/tt_material_lgtyp.
    DATA mt_binmat_update               TYPE /scwm/tt_binmat.
    DATA mt_mat_lgtyp_orig_upd          TYPE /scwm/tt_material_lgtyp.

    CONSTANTS mc_upd_mm   TYPE syucomm VALUE 'UPD_MM' ##NO_TEXT.
    CONSTANTS mc_activate TYPE syucomm VALUE 'ACTIVATE' ##NO_TEXT.

    DATA ms_sav_log        TYPE bal_s_log.
    DATA mo_sav_prot       TYPE REF TO /scwm/cl_log.        " 1502360
    DATA mv_sav_log_crtd   TYPE c LENGTH 1.                 " 1502360
    DATA mv_no_save_log    TYPE c LENGTH 1.                 " 1502360
    DATA mt_sav_log_handle TYPE bal_t_logh.                 " 1502360

    METHODS constructor
      IMPORTING iv_lgnum      TYPE /scwm/binmat-lgnum
                iv_entitled   TYPE /scwm/binmat-entitled
                iv_material   TYPE mara-matnr
                iv_skip_check TYPE xfeld OPTIONAL.

    METHODS run_slotting
      IMPORTING iv_savmod           TYPE /scwm/de_savmod
      EXPORTING et_mat_lgnum_update TYPE /scwm/tt_material_lgnum
                et_mat_lgtyp_update TYPE /scwm/tt_material_lgtyp
                et_mat_slot         TYPE /scwm/tt_material_con_intern.

    METHODS run_slotting_job.

    CLASS-METHODS simulate
      IMPORTING VALUE(iv_lgnum)    TYPE /scwm/lgnum
                VALUE(iv_entitled) TYPE /scwm/de_entitled
                VALUE(iv_matid)    TYPE /scwm/de_matid
      EXPORTING es_dynpro_head     TYPE /scwm/s_analyse_dynpro_01
                VALUE(et_dynpro_c) TYPE /scwm/tt_analyse_dynpro_xx.

    METHODS show_log
      IMPORTING iv_container      TYPE char40                         OPTIONAL
      CHANGING  co_container_log  TYPE REF TO cl_gui_custom_container OPTIONAL
                cv_control_handle TYPE balcnthndl                     OPTIONAL.

  PRIVATE SECTION.
    METHODS cleanup.
    METHODS check_storage.
    METHODS check_entitled.
    METHODS default_value_for_entitled.
    METHODS get_time_zone_of_wrh.
    METHODS raise_exception_from_sy.
    METHODS search_mat.
    METHODS init_cl_concepting.

    METHODS build_package_mat
      IMPORTING iv_packsize  TYPE i
      CHANGING  ct_matnr     TYPE rseloption
                ct_matnr_all TYPE rseloption.

    METHODS build_table_mat_slot.
    METHODS build_step_struc.

    METHODS get_variant_values
      IMPORTING iv_report         TYPE rsvar-report
                iv_variant        TYPE rsvar-variant
      RETURNING VALUE(rt_valutab) TYPE rsparams_tt.

    METHODS build_package
      IMPORTING iv_packsize     TYPE i
      CHANGING  ct_mat_slot     TYPE /scwm/tt_material_con_intern
                ct_mat_slot_all TYPE /scwm/tt_material_con_intern.

    METHODS default_mat_selection.
    METHODS clear_lights.

    METHODS start_concepting_alv
      CHANGING cv_slot TYPE abap_bool.

    METHODS start_concepting
      IMPORTING it_slot_key TYPE /scwm/tt_slot_key.

    METHODS clear_buffer.
    METHODS update_result_table.
    METHODS update_logs_batch ##RELAX.
    METHODS update_logs.
    METHODS update_lights.
    METHODS update_gt_mat_slot.

    METHODS save
      IMPORTING iv_fcode TYPE syucomm.

    METHODS activate
      IMPORTING iv_fcode TYPE syucomm.

    METHODS prepare_save
      IMPORTING iv_fcode TYPE syucomm.

    METHODS set_update_flag
      IMPORTING is_mat_slot TYPE /scwm/s_material_con_intern
                iv_upd_mode TYPE /scwm/de_updmode   ##NEEDED
      CHANGING  ev_update   TYPE c.

    METHODS lgnum_steps_inactive
      IMPORTING iv_lgnum TYPE /scwm/s_material_lgnum
      CHANGING  cv_lgnum TYPE /scwm/s_material_lgnum.

    METHODS lgpla_steps_inactive
      IMPORTING iv_lgpla1 TYPE /scwm/s_material_lgtyp
      CHANGING  cv_lgpla2 TYPE /scwm/s_material_lgtyp.

    METHODS set_outcon_flags
      IMPORTING it_mat_lgtyp_update TYPE /scwm/tt_material_lgtyp.

    METHODS handle_save_error
      IMPORTING iv_subrc   TYPE sysubrc
                it_bapiret TYPE bapirettab
                iv_fcode   TYPE syucomm.

    METHODS create_sav_log
      IMPORTING iv_fcode   TYPE syucomm
      CHANGING  cv_sav_log TYPE bal_s_log.

    METHODS sav_log_show.

    METHODS create_variant
      IMPORTING VALUE(it_valutab) TYPE rsparams_tt.

    METHODS variant_delete
      IMPORTING iv_variant TYPE rsvar-variant.

    METHODS delete_existing_variants.

    METHODS job_open
      EXPORTING VALUE(ev_jobcount) TYPE tbtcjob-jobcount.

    METHODS job_submit
      IMPORTING VALUE(iv_jobcount) TYPE tbtcjob-jobcount.

    METHODS job_close
      IMPORTING VALUE(iv_jobcount) TYPE tbtcjob-jobcount.
ENDCLASS.



CLASS ZCL_MDM_SLOTTING IMPLEMENTATION.


  METHOD activate.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Activate slotting
    "********************************************************************
    save( iv_fcode ).
  ENDMETHOD.


  METHOD build_package.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Build package size for given slotting material,
    "*& it will be always exactly one material for the GAP 06
    "*& All of methods copied from /SCWM/SLOT tx to add background job functionality and OO design
    "********************************************************************
    FIELD-SYMBOLS <ls_mat_slot_all> TYPE /scwm/s_material_con_intern.
    DATA lv_counter TYPE i.

    CLEAR ct_mat_slot.

    LOOP AT ct_mat_slot_all ASSIGNING <ls_mat_slot_all>.
      lv_counter += 1.
      IF lv_counter > iv_packsize.
        EXIT.
      ENDIF.
      APPEND <ls_mat_slot_all> TO ct_mat_slot.
      DELETE TABLE ct_mat_slot_all FROM <ls_mat_slot_all>.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_package_mat.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :  Build package size for given slotting material,
    "*& it will be always exactly one material for the GAP 06
    "********************************************************************
    FIELD-SYMBOLS <ls_matnr_all> TYPE rsdsselopt.
    DATA lv_counter TYPE i.

    CLEAR ct_matnr.

    LOOP AT ct_matnr_all ASSIGNING <ls_matnr_all>.
      lv_counter += 1.
      IF lv_counter > iv_packsize.
        EXIT.
      ENDIF.
      APPEND <ls_matnr_all> TO ct_matnr.
      DELETE TABLE ct_matnr_all FROM <ls_matnr_all>.
    ENDLOOP.
  ENDMETHOD.


  METHOD build_step_struc.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Build slotting step structure, it will read values from variant
    "*& it will set up selection screen values of /SCWM/SLOT tx
    "*& If given paramater provides any of the steps, it will be consider during runtime
    "********************************************************************
    zcl_param=>get_parameter( EXPORTING iv_lgnum     = mv_lgnum
                                        iv_process   = zif_param_const=>c_zcross_0003
                                        iv_parameter = zif_param_const=>c_program_varaint
                              IMPORTING ev_constant  = DATA(lv_variant) ).
    IF lv_variant IS INITIAL.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_parameter_not_found ).
    ENDIF.

    DATA(lt_valutab) = get_variant_values( iv_report  = zif_c_mdm_tool=>c_slotting-report
                                           iv_variant = CONV #( lv_variant ) ).
    IF lt_valutab IS INITIAL.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_variant_value_not_found ).
    ENDIF.

    ms_slot_steps-wghtind  = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-weight ]-low OPTIONAL ).
    ms_slot_steps-wghtind  = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-weight ]-low OPTIONAL ).
    ms_slot_steps-volind   = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-volume ]-low OPTIONAL ).
    ms_slot_steps-hghtind  = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-height ]-low OPTIONAL ).
    ms_slot_steps-wdthind  = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-width ]-low OPTIONAL ).
    ms_slot_steps-lgthind  = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-length ]-low OPTIONAL ).
    ms_slot_steps-demqty   = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-demqty ]-low OPTIONAL ).
    ms_slot_steps-nosoi    = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-nosoi ]-low OPTIONAL ).
    ms_slot_steps-rsqty    = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-rsqty ]-low OPTIONAL ).
    ms_slot_steps-putstra  = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-putstr ]-low OPTIONAL ).
    ms_slot_steps-remstra  = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-remstr ]-low OPTIONAL ).
    ms_slot_steps-sectind  = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-secind ]-low OPTIONAL ).
    ms_slot_steps-bintyp   = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-bintyp ]-low OPTIONAL ).
    ms_slot_steps-maxqty   = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-maxqty ]-low OPTIONAL ).
    ms_slot_steps-minqty   = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-minqty ]-low OPTIONAL ).
    ms_slot_steps-repqty   = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-repqty ]-low OPTIONAL ).
    ms_slot_steps-fixbin   = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-fixbin ]-low OPTIONAL ).
    ms_slot_steps-demiind  = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-demind ]-low OPTIONAL ).
    ms_slot_steps-soiind   = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-soiind ]-low OPTIONAL ).
    ms_slot_steps-trgstind = VALUE #( lt_valutab[ selname = zif_c_mdm_tool=>c_slotting-trgind ]-low OPTIONAL ).
  ENDMETHOD.


  METHOD build_table_mat_slot.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Fill material slotting values
    "********************************************************************
    FIELD-SYMBOLS <ls_mat_lgnum> TYPE /scwm/s_material_lgnum.
    DATA ls_mat_global    TYPE /scwm/s_material_global.
    DATA ls_mat_slot      TYPE /scwm/s_material_con_intern.
    DATA lt_mat_slot_back TYPE /scwm/tt_material_con_intern.

    lt_mat_slot_back = mt_mat_slot.

    SORT lt_mat_slot_back BY matid
                             entitled
                             lgnum.
    CLEAR mt_mat_slot.

    LOOP AT mt_mat_lgnum ASSIGNING <ls_mat_lgnum>.
      CLEAR ls_mat_slot.
      ls_mat_slot-entitled = mv_entitled.
      ls_mat_slot-lgnum    = mv_lgnum.

      " If no slotting results yet (table lt_mat_slot_back empty) we can save a read
      IF lt_mat_slot_back IS INITIAL.

        " Create new entry in int. table
        " Fill warehouse-specific data
        MOVE-CORRESPONDING <ls_mat_lgnum> TO ls_mat_slot ##ENH_OK.
        " Fill global data
        READ TABLE mt_mat_global INTO ls_mat_global
             WITH KEY matid = <ls_mat_lgnum>-matid
             BINARY SEARCH.
        MOVE-CORRESPONDING ls_mat_global TO ls_mat_slot ##ENH_OK.

        "<aahmedov>-19012024
        ls_mat_slot-demtyp = '01'.
        "<aahmedov>-19012024

      ELSE.

        " Check if already entry for MATID-ENTITLED-LGNUM in int. table
        READ TABLE lt_mat_slot_back INTO ls_mat_slot
             WITH KEY matid    = <ls_mat_lgnum>-matid
                      lgnum    = <ls_mat_lgnum>-lgnum
                      entitled = <ls_mat_lgnum>-entitled
             BINARY SEARCH.
        IF sy-subrc <> 0.
          " Otherwise create new entry in int. table
          "   Fill warehouse-specific data
          MOVE-CORRESPONDING <ls_mat_lgnum> TO ls_mat_slot ##ENH_OK.
          " Fill global data
          READ TABLE mt_mat_global INTO ls_mat_global
               WITH KEY matid = <ls_mat_lgnum>-matid
               BINARY SEARCH.
          MOVE-CORRESPONDING ls_mat_global TO ls_mat_slot ##ENH_OK.
        ENDIF.

        "<aahmedov>-19012024
        ls_mat_slot-demtyp = '01'.
        "<aahmedov>-19012024

      ENDIF.

      APPEND ls_mat_slot TO mt_mat_slot.
    ENDLOOP.

    SORT mt_mat_slot BY matid lgnum entitled.
  ENDMETHOD.


  METHOD check_entitled.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check Party entitled to dispose are allowed in the warehouse
    "********************************************************************
    " check entitled
    DATA lt_entitled TYPE /scwm/tt_entitled.

    IF mv_lgnum IS INITIAL OR mv_entitled IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/ENTITLED_FOR_LGNUM_READ'
      EXPORTING
        iv_lgnum    = mv_lgnum
      IMPORTING
        et_entitled = lt_entitled.

    READ TABLE lt_entitled TRANSPORTING NO FIELDS
         WITH KEY entitled = mv_entitled.
    IF sy-subrc = 4.
      MESSAGE e054(/scwm/con) WITH mv_entitled mv_lgnum INTO DATA(lv_dummy) ##NEEDED.
      raise_exception_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD check_storage.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check warehouse number
    "********************************************************************
    CALL FUNCTION '/SCWM/T300_READ_SINGLE'
      EXPORTING
        iv_lgnum  = mv_lgnum
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      MESSAGE e080(/scwm/l3) WITH mv_lgnum INTO DATA(lv_dummy) ##NEEDED.
      raise_exception_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD cleanup.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clean up for warehouse number
    "********************************************************************
    /scwm/cl_tm=>cleanup( iv_lgnum = mv_lgnum ).
  ENDMETHOD.


  METHOD clear_buffer.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clear log buffer
    "********************************************************************
    " Clear Buffer of Application Log
    CALL FUNCTION 'BAL_GLB_MEMORY_REFRESH'
      EXPORTING
        i_refresh_all  = 'X'
      EXCEPTIONS
        not_authorized = 1 ##FM_SUBRC_OK
        OTHERS         = 3.

    " Clear Buffer of MDL
    CALL FUNCTION '/SCWM/MATERIAL_BUFFER_CLEAR'.
  ENDMETHOD.


  METHOD clear_lights.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Clear traffic lights
    "********************************************************************
    FIELD-SYMBOLS <ls_mat_slot> TYPE /scwm/s_material_con_intern.

    LOOP AT mt_mat_slot ASSIGNING <ls_mat_slot>.
      IF <ls_mat_slot>-lights IS INITIAL.
        <ls_mat_slot>-lights = '0'.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD constructor.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Constructor Method
    "********************************************************************
    mv_lgnum = iv_lgnum.
    mv_entitled = iv_entitled.
    mv_matnr = iv_material.

    IF iv_skip_check IS INITIAL.
      cleanup( ).
      default_mat_selection( ).
      check_storage( ).
      check_entitled( ).
      default_value_for_entitled( ).
      get_time_zone_of_wrh( ).
      search_mat( ).
      build_table_mat_slot( ).
      build_step_struc( ).
    ENDIF.
  ENDMETHOD.


  METHOD create_sav_log.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Log saver
    "********************************************************************
    DATA lv_tzone     TYPE tznzone.
    DATA lv_timestamp TYPE timestamp.
    DATA lv_date_log  TYPE d.
    DATA lv_time_log  TYPE t.

    DATA ls_log_act   TYPE /scwm/log_act.
    DATA lv_extnum    TYPE bal_s_log-extnumber.

    " get protocol
    /scwm/cl_log=>get_instance( IMPORTING eo_instance = mo_sav_prot ).

    " Init Application Log
    mo_sav_prot->init( ).

    GET TIME.
    " Get time zone of warehouse
    IF mv_lgnum IS NOT INITIAL.
      CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
        EXPORTING
          iv_lgnum        = mv_lgnum
        IMPORTING
          ev_tzone        = lv_tzone
        EXCEPTIONS
          interface_error = 1
          data_not_found  = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.
        " fallback SY-ZONLO
        lv_tzone = sy-zonlo.
      ENDIF.
    ELSE.
      " If warehouse number is even not set via PARAMETER ID
      " SY-ZONLO will be used instead
      lv_tzone = sy-zonlo.
    ENDIF.

    GET TIME STAMP FIELD lv_timestamp.

    CONVERT TIME STAMP lv_timestamp
            TIME ZONE lv_tzone
            INTO DATE lv_date_log
            TIME lv_time_log.

    " Create Appl.Log (and Loghandle)
    CONCATENATE mv_lgnum '-' iv_fcode INTO lv_extnum.
    cv_sav_log-extnumber = lv_extnum.
    cv_sav_log-object    = '/SCWM/WME'.
    cv_sav_log-subobject = 'CONCEPT'.
    cv_sav_log-aluser    = sy-uname.
    cv_sav_log-aldate    = lv_date_log.
    cv_sav_log-altime    = lv_time_log.

    " Read Settings from table /scwm/log_act
    CALL FUNCTION '/SCWM/LOG_ACT_READ_SINGLE'
      EXPORTING
        iv_lgnum     = mv_lgnum
        iv_subobject = cv_sav_log-subobject
      IMPORTING
        es_log_act   = ls_log_act
      EXCEPTIONS
        not_found    = 1
        OTHERS       = 2.
    IF sy-subrc <> 0 ##NEEDED.
      " print all
    ENDIF.

    CALL FUNCTION '/SCWM/APP_LOG_EXPIRY_DATE_DET'
      EXPORTING
        is_log_act = ls_log_act
      CHANGING
        cs_log     = cv_sav_log.
  ENDMETHOD.


  METHOD create_variant.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Create variant for the job program
    "********************************************************************
    DATA ls_varit TYPE varit.
    DATA lt_varid TYPE STANDARD TABLE OF varid ##NEEDED.
    DATA lt_varit TYPE STANDARD TABLE OF varit.
    DATA ls_varid TYPE varid.

    ls_varit-mandt   = sy-mandt.
    ls_varit-langu   = sy-langu.
    ls_varit-report  = zif_c_mdm_tool=>c_variant-report_name.
    ls_varit-variant = mv_variant_name.
    ls_varit-vtext   = TEXT-001.
    APPEND ls_varit TO lt_varit.

    ls_varid-mandt      = sy-mandt.
    ls_varid-report     = zif_c_mdm_tool=>c_variant-report_name.
    ls_varid-variant    = mv_variant_name.
    ls_varid-transport  = zif_c_mdm_tool=>c_variant-transport_f.
    ls_varid-environmnt = zif_c_mdm_tool=>c_variant-env_background_and_online.
    ls_varid-version    = zif_c_mdm_tool=>c_variant-version.
    ls_varid-ename      = sy-uname.
    ls_varid-edat       = sy-datum.
    ls_varid-etime      = sy-uzeit.
    ls_varid-mlangu     = sy-langu.
    APPEND ls_varid TO lt_varid.

    CALL FUNCTION 'RS_CREATE_VARIANT'
      EXPORTING
        curr_report               = zif_c_mdm_tool=>c_variant-report_name
        curr_variant              = mv_variant_name
        vari_desc                 = ls_varid
      TABLES
        vari_contents             = it_valutab
        vari_text                 = lt_varit
      EXCEPTIONS
        illegal_report_or_variant = 1
        illegal_variantname       = 2
        not_authorized            = 3
        not_executed              = 4
        report_not_existent       = 5
        report_not_supplied       = 6
        variant_exists            = 7
        variant_locked            = 8
        OTHERS                    = 9.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD default_mat_selection.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Default mat selection
    "********************************************************************
    APPEND INITIAL LINE TO mt_mat_sel ASSIGNING FIELD-SYMBOL(<ls_row>).
    <ls_row>-index = '0000000001'.
  ENDMETHOD.


  METHOD default_value_for_entitled.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : read default value for entitled
    "********************************************************************
    IF mv_lgnum IS NOT INITIAL AND mv_entitled IS INITIAL.
      CALL FUNCTION '/SCWM/ENTITLED_FOR_LGNUM_READ'
        EXPORTING
          iv_lgnum         = mv_lgnum
        IMPORTING
          ev_entitled_dflt = mv_entitled.
    ENDIF.

    IF mv_entitled IS INITIAL.
      MESSAGE e020(/scwm/con) INTO DATA(lv_dummy) ##NEEDED.
      raise_exception_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD delete_existing_variants.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Delete existing job program variant to overwrite it again
    "********************************************************************
    DATA(lv_variant_name) = zif_c_mdm_tool=>c_variant-variant_name && |%|.
    SELECT * FROM varid
      WHERE report     = @zif_c_mdm_tool=>c_variant-report_name
        AND variant LIKE @lv_variant_name
      INTO TABLE @DATA(lt_varid).
    LOOP AT lt_varid INTO DATA(ls_varid).
      variant_delete( iv_variant = ls_varid-variant ).
    ENDLOOP.
  ENDMETHOD.


  METHOD get_time_zone_of_wrh.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : get time zone of warehouse
    "********************************************************************
    IF mv_lgnum IS NOT INITIAL.
      CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
        EXPORTING
          iv_lgnum        = mv_lgnum
        IMPORTING
          ev_tzone        = mv_tzone
        EXCEPTIONS
          interface_error = 1
          data_not_found  = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.
        " fallback SY-ZONLO
        mv_tzone = sy-zonlo.
      ENDIF.
    ELSE.
      " If warehouse number is even not set via PARAMETER ID
      " SY-ZONLO will be used instead
      mv_tzone = sy-zonlo.
    ENDIF.
  ENDMETHOD.


  METHOD get_variant_values.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : get variant contents for the job program
    "********************************************************************
    CALL FUNCTION 'RS_VARIANT_CONTENTS'
      EXPORTING
        report               = iv_report          " Report name
        variant              = iv_variant         " Variant name
      TABLES
        valutab              = rt_valutab     " Table that contains the values (P + S)
      EXCEPTIONS
        variant_non_existent = 1 ##FM_SUBRC_OK                " Variant does not exist
        variant_obsolete     = 2                " Report does not exist
        OTHERS               = 3.
  ENDMETHOD.


  METHOD handle_save_error.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : If there is an error durig save it will be handled here
    "********************************************************************
    DATA lv_loghandle TYPE balloghndl.

    IF iv_subrc = 1.
      raise_exception_from_sy( ).
    ENDIF.
    LOOP AT it_bapiret TRANSPORTING NO FIELDS ##NEEDED
         WHERE type CA 'EAX'.
    ENDLOOP.
    IF sy-subrc = 0.
      IF mv_sav_log_crtd IS INITIAL.
        create_sav_log( EXPORTING iv_fcode   = iv_fcode
                        CHANGING  cv_sav_log = ms_sav_log ).
        mv_sav_log_crtd = 'X'.
      ENDIF.

      mo_sav_prot->add_log( it_prot = it_bapiret ).
    ENDIF.
    IF     mv_sav_log_crtd  = 'X'
       AND mv_no_save_log  IS INITIAL.
      " save log to database
      mo_sav_prot->save_applog( EXPORTING is_log       = ms_sav_log
                                IMPORTING ev_loghandle = lv_loghandle ).
      TRY.
          mo_sav_prot->save_applog2db( iv_loghandle = lv_loghandle ).
        CATCH /scwm/cx_basics.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDTRY.
      APPEND lv_loghandle TO mt_sav_log_handle.
      " display log
      IF sy-batch <> 'X'.
        sav_log_show( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD init_cl_concepting.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "*&
    "*&
    "********************************************************************
    IF mo_concore IS NOT BOUND.
      mo_concore = NEW #( ).
    ENDIF.
  ENDMETHOD.


  METHOD job_close.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Close the background job
    "********************************************************************
    DATA lv_running  TYPE tbtcv-run ##NEEDED.
    DATA lv_finished TYPE tbtcv-fin.
    DATA lv_aborted  TYPE tbtcv-abort.

    CALL FUNCTION 'JOB_CLOSE'
      EXPORTING
        jobcount             = iv_jobcount
        jobname              = mv_jobname
        strtimmed            = abap_true
      EXCEPTIONS
        cant_start_immediate = 1
        invalid_startdate    = 2
        jobname_missing      = 3
        job_close_failed     = 4
        job_nosteps          = 5
        job_notex            = 6
        lock_failed          = 7
        invalid_target       = 8
        OTHERS               = 9.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
    DO.
      CALL FUNCTION 'SHOW_JOBSTATE'
        EXPORTING
          jobcount         = iv_jobcount
          jobname          = mv_jobname
        IMPORTING
          aborted          = lv_aborted
          finished         = lv_finished
          running          = lv_running
        EXCEPTIONS
          jobcount_missing = 1
          jobname_missing  = 2
          job_notex        = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        " e_error = true.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.
      IF lv_finished IS NOT INITIAL OR lv_aborted IS NOT INITIAL.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.


  METHOD job_open.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Open Job Scheduling Without Dialog
    "********************************************************************
    CALL FUNCTION 'JOB_OPEN'
      EXPORTING
        jobname          = mv_jobname
      IMPORTING
        jobcount         = ev_jobcount
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD job_submit.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : call the job program with variant
    "********************************************************************
    CALL FUNCTION 'JOB_SUBMIT'
      EXPORTING
        authcknam               = sy-uname
        jobcount                = iv_jobcount
        jobname                 = mv_jobname
        report                  = zif_c_mdm_tool=>c_variant-report_name
        variant                 = mv_variant_name
      EXCEPTIONS
        bad_priparams           = 1
        bad_xpgflags            = 2
        invalid_jobdata         = 3
        jobname_missing         = 4
        job_notex               = 5
        job_submit_failed       = 6
        lock_failed             = 7
        program_missing         = 8
        prog_abap_and_extpg_set = 9
        OTHERS                  = 10.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD lgnum_steps_inactive.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Inactivate strategy values
    "********************************************************************
    IF ms_slot_steps-remstra IS INITIAL.
      cv_lgnum-rem_stra = iv_lgnum-rem_stra.
    ENDIF.
    IF ms_slot_steps-putstra IS INITIAL.
      cv_lgnum-put_stra = iv_lgnum-put_stra.
    ENDIF.
  ENDMETHOD.


  METHOD lgpla_steps_inactive.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Inactivate storage data fields
    "********************************************************************
    IF ms_slot_steps-sectind IS INITIAL.
      cv_lgpla2-sectind = iv_lgpla1-sectind.
    ENDIF.
    IF ms_slot_steps-bintyp IS INITIAL.
      cv_lgpla2-bintype = iv_lgpla1-bintype.
    ENDIF.
    IF ms_slot_steps-maxqty IS INITIAL.
      cv_lgpla2-maxqty = iv_lgpla1-maxqty.
    ENDIF.
    IF ms_slot_steps-minqty IS INITIAL.
      cv_lgpla2-minqty = iv_lgpla1-minqty.
    ENDIF.
    IF ms_slot_steps-repqty IS INITIAL.
      cv_lgpla2-repqty = iv_lgpla1-repqty.
    ENDIF.
    IF ms_slot_steps-fixbin IS INITIAL.
      cv_lgpla2-maxfixbin = iv_lgpla1-maxfixbin.
    ENDIF.
  ENDMETHOD.


  METHOD prepare_save.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Preparation of save process during slotting run
    "********************************************************************
    FIELD-SYMBOLS <ls_mat_lgtyp> TYPE /scwm/s_material_lgtyp.
    DATA ls_mat_lgtyp_temp TYPE /scwm/s_material_lgtyp.
    DATA ls_mat_slot       TYPE /scwm/s_material_con_intern.
    DATA ls_mat_lgnum      TYPE /scwm/s_material_lgnum.
    DATA ls_mat_lgnum_temp TYPE /scwm/s_material_lgnum.
    DATA ls_binmat         TYPE /scwm/binmat.
    DATA lv_update         TYPE c LENGTH 1.
    DATA lv_index2         TYPE i.
    DATA ls_mat_sel        TYPE lvc_s_row.

    CLEAR mt_mat_lgnum_update.
    CLEAR mt_mat_lgtyp_update.
    CLEAR mt_binmat_update.
    CLEAR mt_mat_lgtyp_orig_upd.
    " set status of all fields to selected
    READ TABLE mt_mat_slot INDEX 1 TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      ls_mat_sel-index = '1'.
      WHILE ls_mat_sel-index <= sy-tfill.
        APPEND ls_mat_sel TO mt_mat_sel.
        ls_mat_sel-index += 1.
      ENDWHILE.
    ENDIF.

    IF mt_mat_sel IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT mt_mat_sel ASSIGNING FIELD-SYMBOL(<ls_row>).
      CLEAR lv_update.

      READ TABLE mt_mat_slot INTO ls_mat_slot INDEX <ls_row>-index.
      IF sy-subrc = 0.
        lv_index2 = sy-tabix.
        set_update_flag( EXPORTING is_mat_slot = ls_mat_slot
                                   iv_upd_mode = mv_upd_mode
                         CHANGING  ev_update   = lv_update ).
        " activate already saved results
        IF iv_fcode = mc_activate AND ls_mat_slot-status = '1'.
          lv_update = 'X'.
        ENDIF.
      ENDIF.

      " Move items to tables for update
      IF lv_update = 'X'.
        READ TABLE mt_mat_lgnum_result INTO ls_mat_lgnum
             WITH KEY matid    = ls_mat_slot-matid
                      lgnum    = ls_mat_slot-lgnum
                      entitled = ls_mat_slot-entitled.
        IF sy-subrc = 0.
          " Activate
          IF iv_fcode = mc_activate.
            ls_mat_lgnum_temp = ls_mat_lgnum.
            mo_concore->activate_lgnum_lgtyp( CHANGING cs_mat_lgnum = ls_mat_lgnum ).

            lgnum_steps_inactive( EXPORTING iv_lgnum = ls_mat_lgnum_temp
                                  CHANGING  cv_lgnum = ls_mat_lgnum ).
          ENDIF.
          " set actual time stamp
          GET TIME STAMP FIELD ls_mat_lgnum-lastslot.
          APPEND ls_mat_lgnum TO mt_mat_lgnum_update.
          IF mv_badi_sttype_upd_mode IS NOT INITIAL.
            LOOP AT mt_mat_lgtyp_orig_sel ASSIGNING <ls_mat_lgtyp>
                 WHERE     matid    = ls_mat_slot-matid
                       AND lgnum    = ls_mat_slot-lgnum
                       AND entitled = ls_mat_slot-entitled.
              APPEND <ls_mat_lgtyp> TO mt_mat_lgtyp_orig_upd.
            ENDLOOP.
          ENDIF.
        ENDIF.

        " check with new or changed planned values
        LOOP AT mt_mat_lgtyp_result ASSIGNING <ls_mat_lgtyp>
             WHERE     matid    = ls_mat_slot-matid
                   AND lgnum    = ls_mat_slot-lgnum
                   AND entitled = ls_mat_slot-entitled.
          " Activate
          IF iv_fcode = mc_activate.
            ls_mat_lgtyp_temp = <ls_mat_lgtyp>.
            mo_concore->activate_lgnum_lgtyp( CHANGING cs_mat_lgtyp = <ls_mat_lgtyp> ).

            lgpla_steps_inactive( EXPORTING iv_lgpla1 = ls_mat_lgtyp_temp
                                  CHANGING  cv_lgpla2 = <ls_mat_lgtyp> ).

          ENDIF.
          APPEND <ls_mat_lgtyp> TO mt_mat_lgtyp_update.
        ENDLOOP.
      ENDIF.

      IF mt_binmat_result[] IS NOT INITIAL.
        LOOP AT mt_binmat_result INTO ls_binmat
             WHERE     matid    = ls_mat_slot-matid
                   AND lgnum    = ls_mat_slot-lgnum
                   AND entitled = ls_mat_slot-entitled.
          INSERT ls_binmat INTO TABLE mt_binmat_update.
        ENDLOOP.
      ENDIF.

      " Update Status Save/Active
      IF lv_update = 'X'.
        IF ls_mat_slot-status IS INITIAL.
          ls_mat_slot-status = '1'.
          MODIFY mt_mat_slot FROM ls_mat_slot INDEX lv_index2.
        ENDIF.
        IF iv_fcode = mc_activate.
          ls_mat_slot-status = '2'.
          MODIFY mt_mat_slot FROM ls_mat_slot INDEX lv_index2.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD raise_exception_from_sy.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "*&
    "*&
    "********************************************************************
    RAISE EXCEPTION TYPE zcx_mdm_tool
          MESSAGE ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDMETHOD.


  METHOD run_slotting.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "*&
    "*&
    "********************************************************************
    DATA lv_packsize TYPE i VALUE 100.
    DATA lv_counter  TYPE i.
    DATA lv_lines    TYPE i.
    DATA lv_pack     TYPE f.
    DATA lv_slot     TYPE abap_bool.
    DATA ls_mat_alv  TYPE /scwm/s_material_con.

    mt_mat_slot_all = mt_mat_slot.
    DESCRIBE TABLE mt_mat_slot_all LINES lv_lines.

    LOOP AT mt_mat_slot ASSIGNING FIELD-SYMBOL(<ls_mat_slot>).
      MOVE-CORRESPONDING <ls_mat_slot> TO ls_mat_alv ##ENH_OK.
      APPEND ls_mat_alv TO mt_mat_alv.
    ENDLOOP.

    lv_pack = lv_lines / lv_packsize.
    lv_counter = ceil( lv_pack ).
    DO lv_counter TIMES.
      build_package( EXPORTING iv_packsize     = lv_packsize
                     CHANGING  ct_mat_slot     = mt_mat_slot
                               ct_mat_slot_all = mt_mat_slot_all ).

      clear_lights( ).

      start_concepting_alv( CHANGING cv_slot = lv_slot ).

      IF lv_slot = abap_false.
        RETURN.
      ENDIF.

      clear_buffer( ).

      update_result_table( ).
      update_logs( ).

      update_lights( ).
      update_gt_mat_slot( ).

      IF sy-index < lv_counter.
        mv_no_save_log = 'X'.
      ELSE.
        CLEAR mv_no_save_log.
      ENDIF.

      IF iv_savmod = zif_c_mdm_tool=>c_slotting-save.
        save( mc_upd_mm ).
      ENDIF.
      IF iv_savmod = zif_c_mdm_tool=>c_slotting-activate.
        activate( mc_activate ).
      ENDIF.
    ENDDO.

    et_mat_lgnum_update = mt_mat_lgnum_update.
    et_mat_lgtyp_update = mt_mat_lgtyp_update.

    IF iv_savmod = zif_c_mdm_tool=>c_slotting-save AND ( mt_mat_lgnum_update IS NOT INITIAL OR mt_mat_lgtyp_update IS NOT INITIAL ).
      MESSAGE s016(zmc_mdm_tool). " Slotting saved.
    ENDIF.

    IF iv_savmod = zif_c_mdm_tool=>c_slotting-activate AND ( mt_mat_lgnum_update IS NOT INITIAL OR mt_mat_lgtyp_update IS NOT INITIAL ).
      MESSAGE s017(zmc_mdm_tool). " Slotting activated.
    ENDIF.

    IF et_mat_slot IS SUPPLIED.
      et_mat_slot = mt_mat_slot.
    ENDIF.
  ENDMETHOD.


  METHOD run_slotting_job.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Run slotting background job
    "********************************************************************
    zcl_param=>get_parameter( EXPORTING iv_lgnum     = mv_lgnum
                                        iv_process   = zif_param_const=>c_zcross_0003
                                        iv_parameter = zif_param_const=>c_program_varaint
                              IMPORTING ev_constant  = DATA(lv_variant) ).
    IF lv_variant IS INITIAL.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_parameter_not_found ).
    ENDIF.

    DATA(lt_valutab) = get_variant_values( iv_report  = zif_c_mdm_tool=>c_slotting-report
                                           iv_variant = CONV #( lv_variant ) ).
    IF lt_valutab IS INITIAL.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_variant_value_not_found ).
    ENDIF.
    GET TIME STAMP FIELD DATA(lv_ts).

    mv_variant_name = zif_c_mdm_tool=>c_variant-variant_name && lv_ts.
    mv_jobname      = zif_c_mdm_tool=>c_variant-variant_name && lv_ts.

    ASSIGN lt_valutab[ selname = zif_c_mdm_tool=>c_variant-s_matnr ] TO FIELD-SYMBOL(<ls_value_tab>).
    <ls_value_tab> = VALUE #( selname = zif_c_mdm_tool=>c_variant-s_matnr
                              kind    = zif_c_mdm_tool=>c_variant-select_option
                              sign    = wmegc_sign_inclusive
                              option  = wmegc_option_eq
                              low     = mv_matnr
                              high    = '' ).

    delete_existing_variants( ).

    create_variant( it_valutab = lt_valutab ).

    job_open( IMPORTING ev_jobcount = DATA(lv_jobcount) ).

    job_submit( iv_jobcount = lv_jobcount ).

    job_close( iv_jobcount = lv_jobcount ).

    variant_delete( iv_variant = mv_variant_name ).
  ENDMETHOD.


  METHOD save.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Save for slotting
    "********************************************************************
    DATA lv_all_slotted TYPE abap_bool ##NEEDED.
    DATA lt_bapiret     TYPE bapirettab.                    " 1502360

    lv_all_slotted = abap_true.

    " check if all the products has been already activated

    prepare_save( iv_fcode ).

    " BAdI for updating customer-specific tables
    TRY.
        GET BADI mi_badi_slot_det_update_tables
          FILTERS
            lgnum = mv_lgnum.
      CATCH cx_badi ##NO_HANDLER.
    ENDTRY.

    IF mi_badi_slot_det_update_tables IS BOUND.

      CALL BADI mi_badi_slot_det_update_tables->update_tables
        EXPORTING
          iv_lgnum     = mv_lgnum
          it_mat_lgnum = mt_mat_lgnum_update
          it_mat_lgtyp = mt_mat_lgtyp_update
          iv_grnonl    = mv_upd_mode.

    ENDIF.
    IF iv_fcode = mc_activate.
      IF mv_badi_sttype_upd_mode IS NOT INITIAL.
        IF mi_badi_slot_upd_storg_type IS BOUND.
          CALL BADI mi_badi_slot_upd_storg_type->determine_deletion_indicator
            EXPORTING
              flt_val        = mv_lgnum
              iv_lgnum       = mv_lgnum
              iv_entitled    = mv_entitled
              it_new_lgnum   = mt_mat_lgnum_update
              it_old_lgnum   = mt_mat_lgnum
              it_new_lgtyp   = mt_mat_lgtyp_update
              it_old_lgtyp   = mt_mat_lgtyp_orig_upd
            IMPORTING
              ev_delete_mode = mv_badi_sttype_upd_mode.
          " SAP default method
          IF mv_badi_sttype_upd_mode = 1.
            TRY.
                mo_concore->delete_storage_type( iv_lgnum     = mv_lgnum
                                                 iv_entitled  = mv_entitled
                                                 it_old_lgtyp = mt_mat_lgtyp_orig_upd
                                                 it_new_lgtyp = mt_mat_lgtyp_update ).
              CATCH /scwm/cx_con ##NO_HANDLER.
                " Currently not handled due to certain conflicts with existing logging
                " Will be handled in future.
            ENDTRY.
            " Values 2 and 3 reserved for SAP future use
          ELSEIF mv_badi_sttype_upd_mode > 3.
            " Call the BAdI
            CALL BADI mi_badi_slot_upd_storg_type->delete_old_storage_types
              EXPORTING
                flt_val        = mv_lgnum
                iv_lgnum       = mv_lgnum
                iv_entitled    = mv_entitled
                iv_delete_mode = mv_badi_sttype_upd_mode
                it_old_lgnum   = mt_mat_lgnum
                it_new_lgnum   = mt_mat_lgnum_update
                it_new_lgtyp   = mt_mat_lgtyp_update
              CHANGING
                ct_old_lgtyp   = mt_mat_lgtyp_orig_upd.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    mo_concore->material_update( EXPORTING  iv_lgnum     = mv_lgnum
                                            it_mat_lgnum = mt_mat_lgnum_update
                                            it_mat_lgtyp = mt_mat_lgtyp_update
                                            iv_grnonl    = mv_upd_mode
                                 RECEIVING  et_bapiret   = lt_bapiret
                                 EXCEPTIONS error_update = 1 ).

    handle_save_error( iv_subrc   = sy-subrc
                       it_bapiret = lt_bapiret
                       iv_fcode   = iv_fcode ).

    mo_concore->save_fixbins( it_binmat = mt_binmat_update ).

    IF iv_fcode = mc_upd_mm.
      mo_concore->save_hist_data( ).
    ENDIF.

    set_outcon_flags( mt_mat_lgtyp_update ).
  ENDMETHOD.


  METHOD sav_log_show.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Show the saved logs
    "********************************************************************
    DATA l_s_display_profile TYPE bal_s_prof.

    CHECK mt_sav_log_handle[] IS NOT INITIAL.

    " get a prepared profile
    CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
      IMPORTING
        e_s_display_profile = l_s_display_profile
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

    " use grid for display if wanted
    l_s_display_profile-use_grid = 'X'.

    " set report to allow saving of variants
    l_s_display_profile-disvariant-report = sy-repid.
    " when you use also other ALV lists in your report,
    " please specify a handle to distinguish between the display
    " variants of these different lists, e.g:
    l_s_display_profile-disvariant-handle = 'SAVELOG'.

    " call display function module
    CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
      EXPORTING
        i_s_display_profile = l_s_display_profile
        i_t_log_handle      = mt_sav_log_handle
      EXCEPTIONS
        OTHERS              = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.


  METHOD search_mat.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "*&
    "*&
    "********************************************************************
    FIELD-SYMBOLS <ls_mat_global> TYPE /scwm/s_material_global.
    FIELD-SYMBOLS <ls_matid>      TYPE /scwm/s_matid_matnr.

    DATA ls_range           TYPE rsdsselopt.
    DATA lt_entit           TYPE rseloption.
    DATA lt_matgr           TYPE rseloption.
    DATA lt_lastd           TYPE rseloption.
    DATA lt_conc            TYPE rseloption.
    DATA lt_mat_global_back TYPE /scwm/tt_material_global.
    DATA ls_mat_lgnum       TYPE /scwm/s_material_lgnum.
    DATA lt_matid_matnr     TYPE /scwm/tt_matid_matnr.
    DATA lt_matid_matnr2    TYPE /scwm/tt_matid_matnr.
    DATA lv_tzone           TYPE tznzone ##NEEDED.
    DATA lv_packsize        TYPE i VALUE 100.
    DATA lv_counter         TYPE i.
    DATA lv_pack            TYPE f.
    DATA lv_lines           TYPE i.
    DATA lo_slot_badi       TYPE REF TO /scwm/ex_slot_mat_selection.

    CLEAR: mt_matnr,
           mt_matid.

    " get time zone of warehouse
    " needed for the conversion of the date to timestamp
    IF mv_lgnum IS NOT INITIAL.
      CALL FUNCTION '/SCWM/LGNUM_TZONE_READ'
        EXPORTING
          iv_lgnum        = mv_lgnum
        IMPORTING
          ev_tzone        = lv_tzone
        EXCEPTIONS
          interface_error = 1
          data_not_found  = 2
          OTHERS          = 3.
      IF sy-subrc <> 0.
        " fallback SY-ZONLO
        lv_tzone = sy-zonlo.
      ENDIF.
    ELSE.
      " If warehouse number is even not set via PARAMETER ID
      " SY-ZONLO will be used instead
      lv_tzone = sy-zonlo.
    ENDIF.

    " Instantiate class /scwm/cl_concepting_core
    init_cl_concepting( ).
    " at least one of these selection criteria should be fulfilled
    IF mv_matnr IS INITIAL.
      MESSAGE s051(/scwm/con) INTO DATA(lv_dummy) ##NEEDED.
      raise_exception_from_sy( ).
    ENDIF.

    " entitled
    ls_range-sign   = wmegc_sign_inclusive.
    ls_range-option = wmegc_option_eq.
    ls_range-low    = mv_entitled.
    APPEND ls_range TO lt_entit.

    mt_matnr = VALUE #( ( sign   = wmegc_sign_inclusive
                          option = wmegc_option_eq
                          low    = mv_matnr ) ).

    " Build package for MATERIAL_READ_RANGE
    mt_matnr_all = mt_matnr.
    DESCRIBE TABLE mt_matnr_all LINES lv_lines.

    lv_pack = lv_lines / lv_packsize.
    lv_counter = ceil( lv_pack ).

    TRY.
        GET BADI lo_slot_badi.
        CALL BADI lo_slot_badi->mat_selection
          EXPORTING
            iv_lgnum          = mv_lgnum
            it_entitled_range = lt_entit
            it_matnr_range    = mt_matnr
            it_whmatgr_range  = lt_matgr
            it_lastslot_range = lt_lastd
          IMPORTING
            et_matid          = mt_matid_matnr.
      CATCH cx_badi.
        DO lv_counter TIMES.
          build_package_mat( EXPORTING iv_packsize  = lv_packsize
                             CHANGING  ct_matnr     = mt_matnr
                                       ct_matnr_all = mt_matnr_all ).

          " read table if no date selection or if
          " there is an additional group selection
          IF    lt_matgr IS NOT INITIAL
             OR lt_lastd IS INITIAL.
            TRY.
                CALL FUNCTION '/SCWM/MATERIAL_READ_RANGE'
                  EXPORTING
                    it_matnr_range     = mt_matnr
                    it_whmatgr_range   = lt_matgr
                    iv_whmatgr_initial = 'X'
                  IMPORTING
                    et_matid           = mt_matid_matnr_pack.
              CATCH /scwm/cx_md_interface /scwm/cx_md_internal_error.
                MESSAGE e156(/scwm/con).
            ENDTRY.
          ENDIF.

          " get matids from date selection
          IF lt_lastd IS NOT INITIAL.
            IF lt_matgr IS NOT INITIAL.
              lt_matid_matnr = mt_matid_matnr_pack.
              CLEAR mt_matid_matnr_pack.
            ENDIF.

            TRY.
                CALL FUNCTION '/SCWM/MAT_READ_LGNUM_RANGE'
                  EXPORTING
                    iv_lgnum          = mv_lgnum
                    it_entitled_range = lt_entit
                    it_matnr_range    = mt_matnr
                    it_lastslot_range = lt_lastd
                    it_concstat_range = lt_conc
                  IMPORTING
                    et_matid          = lt_matid_matnr2.
              CATCH /scwm/cx_md_interface /scwm/cx_md_internal_error.
                MESSAGE e156(/scwm/con).
            ENDTRY.

            " restrict table of date selection due to matgr
            IF lt_matgr IS NOT INITIAL.
              SORT lt_matid_matnr BY matid.
              LOOP AT lt_matid_matnr2 ASSIGNING <ls_matid>.
                READ TABLE lt_matid_matnr WITH KEY matid = <ls_matid>-matid
                     TRANSPORTING NO FIELDS
                     BINARY SEARCH.
                " copy entry if contained in both tables
                IF sy-subrc = 0.
                  APPEND <ls_matid> TO mt_matid_matnr_pack.
                ENDIF.
              ENDLOOP.
            ELSE.
              mt_matid_matnr = lt_matid_matnr2.
            ENDIF.
          ENDIF.

          IF mt_matid_matnr_pack IS NOT INITIAL OR ( lt_lastd IS NOT INITIAL AND lt_matgr IS NOT INITIAL ).
            APPEND LINES OF mt_matid_matnr_pack TO mt_matid_matnr.
          ENDIF.

        ENDDO.
    ENDTRY.
    " copy needed matids
    LOOP AT mt_matid_matnr ASSIGNING <ls_matid>.
      APPEND <ls_matid>-matid TO mt_matid.
    ENDLOOP.

    " no products found with given selection
    IF mt_matid IS INITIAL.
      MESSAGE e051(/scwm/con) INTO lv_dummy.
      raise_exception_from_sy( ).
    ENDIF.

    " Read Material Master
    CLEAR mt_mat_global.

    TRY.
        mo_concore->material_read( EXPORTING iv_lgnum      = mv_lgnum
                                             iv_entitled   = mv_entitled
                                             it_matid      = mt_matid
                                   IMPORTING et_mat_global = mt_mat_global
                                             et_mat_lgnum  = mt_mat_lgnum ).
      CATCH /scwm/cx_con.
        MESSAGE e156(/scwm/con).
    ENDTRY.

    SORT mt_mat_global BY matid.
    SORT mt_mat_lgnum  BY matid
                          entitled
                          lgnum.

    " Create table entry for warehouse data, if not yet existing on DB
    LOOP AT mt_mat_global ASSIGNING <ls_mat_global>.
      READ TABLE mt_mat_lgnum INTO ls_mat_lgnum
           WITH KEY matid    = <ls_mat_global>-matid
                    entitled = mv_entitled
                    lgnum    = mv_lgnum
           BINARY SEARCH.

      IF sy-subrc <> 0.
        ls_mat_lgnum-matid    = <ls_mat_global>-matid.
        ls_mat_lgnum-entitled = mv_entitled.
        ls_mat_lgnum-lgnum    = mv_lgnum.
        ls_mat_lgnum-meins    = <ls_mat_global>-meins.
        INSERT ls_mat_lgnum INTO mt_mat_lgnum INDEX sy-tabix. " 1514667
      ENDIF.
    ENDLOOP.

    " Filter by selection criteria

    lt_mat_global_back = mt_mat_global.
    CLEAR mt_mat_global.
    LOOP AT lt_mat_global_back ASSIGNING <ls_mat_global>
         WHERE whmatgr IN lt_matgr. " s_matgr.
      APPEND <ls_mat_global> TO mt_mat_global.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_outcon_flags.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update outdated-flags of quants (available qty)
    "********************************************************************
    DATA lt_binmat   TYPE /scwm/tt_binmat.
    DATA ls_binmat   TYPE /scwm/binmat.
    DATA lt_aqua_int TYPE /scwm/tt_aqua_int2.
    DATA ls_matid    TYPE rsdsselopt.
    DATA ls_entitled TYPE rsdsselopt.
    DATA ls_lgtyp    TYPE rsdsselopt.
    DATA lr_matid    TYPE rseloption.
    DATA lr_entitled TYPE rseloption.
    DATA lr_lgtyp    TYPE rseloption.
    DATA lv_lgtyp    TYPE /scwm/lgtyp.
    DATA lt_lgtyp_wh TYPE /scwm/tt_lgtyp.
    DATA lt_t331     TYPE /scwm/tt_t331.
    DATA ls_t331     TYPE /scwm/t331.

    FIELD-SYMBOLS <ls_mat_lgtyp> TYPE /scwm/s_material_lgtyp.
    FIELD-SYMBOLS <ls_aqua_int>  TYPE /scwm/s_aqua_int2.

    TRY.
        CALL FUNCTION '/SCWM/T301_READ_MULTI'
          EXPORTING
            iv_lgnum = mv_lgnum
          IMPORTING
            et_lgtyp = lt_lgtyp_wh.
      CATCH /scwm/cx_core ##NO_HANDLER.
    ENDTRY.

    " Read stock from AQUA
    IF it_mat_lgtyp_update IS INITIAL.
      RETURN.
    ENDIF.

    CLEAR lr_matid.
    LOOP AT it_mat_lgtyp_update ASSIGNING <ls_mat_lgtyp>.
      ls_matid-sign   = 'I'.
      ls_matid-option = 'EQ'.
      ls_matid-low    = <ls_mat_lgtyp>-matid.
      APPEND ls_matid TO lr_matid.
    ENDLOOP.
    SORT lr_matid.
    DELETE ADJACENT DUPLICATES FROM lr_matid.

    CLEAR lr_entitled.
    ls_entitled-sign   = 'I'.
    ls_entitled-option = 'EQ'.
    ls_entitled-low    = <ls_mat_lgtyp>-entitled.
    APPEND ls_entitled TO lr_entitled.

    CLEAR lr_lgtyp.
    LOOP AT lt_lgtyp_wh INTO lv_lgtyp.
      ls_lgtyp-sign   = 'I'.
      ls_lgtyp-option = 'EQ'.
      ls_lgtyp-low    = lv_lgtyp.
      APPEND ls_lgtyp TO lr_lgtyp.
    ENDLOOP.

    CLEAR lt_aqua_int.
    CALL FUNCTION '/SCWM/AQUA_SELECT'
      EXPORTING
        iv_lgnum    = mv_lgnum
        ir_matid    = lr_matid
        ir_entitled = lr_entitled
        ir_lgtyp    = lr_lgtyp
      IMPORTING
        et_aqua_int = lt_aqua_int
      EXCEPTIONS
        wrong_input = 1
        OTHERS      = 2 ##FM_SUBRC_OK.
    IF sy-subrc <> 0 ##NEEDED.
    ENDIF.

    " Check if quants are optimal
    IF lt_aqua_int IS NOT INITIAL.
      TRY.
          CALL FUNCTION '/SCWM/T331_READ_MULTI'
            EXPORTING
              iv_lgnum = mv_lgnum
              it_lgtyp = lr_lgtyp
            IMPORTING
              et_t331  = lt_t331.
        CATCH /scwm/cx_core ##NO_HANDLER.

      ENDTRY.

      LOOP AT lt_aqua_int ASSIGNING <ls_aqua_int>.
        CLEAR ls_t331.
        READ TABLE lt_t331 INTO ls_t331
             WITH KEY lgnum = <ls_aqua_int>-lgnum
                      lgtyp = <ls_aqua_int>-lgtyp.
        IF sy-subrc = 0 AND ls_t331-st_role = wmegc_strole_normal.
          ls_binmat-matid    = <ls_aqua_int>-matid.
          ls_binmat-entitled = <ls_aqua_int>-entitled.
          ls_binmat-lgnum    = <ls_aqua_int>-lgnum.
          ls_binmat-lgpla    = <ls_aqua_int>-lgpla.
          INSERT ls_binmat INTO TABLE lt_binmat.
        ENDIF.
      ENDLOOP.

      TRY.
          mo_concore->get_outcon( CHANGING ct_binmat = lt_binmat ).
        CATCH /scwm/cx_con ##NO_HANDLER.
      ENDTRY.

      " Update /SCWM/AQUA
      "   Sort to avoid Deadlock
      SORT lt_aqua_int BY guid_parent
                          guid_stock.

      LOOP AT lt_aqua_int ASSIGNING <ls_aqua_int>.

        READ TABLE lt_binmat INTO ls_binmat
             WITH KEY matid = <ls_aqua_int>-matid
                      lgnum = <ls_aqua_int>-lgnum
                      lgpla = <ls_aqua_int>-lgpla.
        IF sy-subrc = 0.
          <ls_aqua_int>-outcon = ls_binmat-outcon.
        ENDIF.

        UPDATE /scwm/aqua FROM <ls_aqua_int>.

      ENDLOOP.

    ENDIF.
  ENDMETHOD.


  METHOD set_update_flag.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update flag set
    "********************************************************************
    IF is_mat_slot-status IS NOT INITIAL.
      RETURN.
    ENDIF.

    CASE mv_upd_mode.
      WHEN congc_updmod_green.
        IF is_mat_slot-lights = '3'.
          ev_update = 'X'.
        ENDIF.

      WHEN congc_updmod_yellow.
        IF    ( is_mat_slot-lights = '3' )
           OR ( is_mat_slot-lights = '2' ).
          ev_update = 'X'.
        ENDIF.

      WHEN congc_updmod_all.
        IF is_mat_slot-lights <> '0'.
          ev_update = 'X'.
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD show_log.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Show logs
    "********************************************************************
    DATA lt_log_handl       TYPE bal_t_logh.
    DATA lv_no_dat          TYPE boolean ##NEEDED.
    DATA ls_display_profile TYPE bal_s_prof.
    FIELD-SYMBOLS <ls_bal_s_fcat> TYPE bal_s_fcat.

    ls_display_profile-use_grid = 'X'.

    " Get Loghandle(s)
    IF mo_concore IS BOUND.
      mo_concore->get_prot( EXPORTING it_mat_prot = mt_mat_prot
                            IMPORTING et_logh     = lt_log_handl ).
    ENDIF.

    IF lt_log_handl IS INITIAL.
      RETURN.
    ENDIF.

    " Get Display Profile
    CALL FUNCTION 'BAL_DSP_PROFILE_DETLEVEL_GET'
      IMPORTING
        e_s_display_profile = ls_display_profile.

    ls_display_profile-tree_size = '18'.
    ls_display_profile-exp_level = '0'.

    READ TABLE ls_display_profile-mess_fcat ASSIGNING <ls_bal_s_fcat>
         WITH KEY ref_table = 'BAL_S_SHOW'
                  ref_field = 'T_MSG'.
    IF sy-subrc = 0.
      <ls_bal_s_fcat>-outputlen = 120.
    ENDIF.
    IF co_container_log IS NOT BOUND.
      co_container_log = NEW #( container_name = iv_container ). "'APPL_LOG'.

      CALL FUNCTION 'BAL_CNTL_CREATE'
        EXPORTING
          i_container          = co_container_log
          i_s_display_profile  = ls_display_profile
          i_t_log_handle       = lt_log_handl
        IMPORTING
          e_control_handle     = cv_control_handle
        EXCEPTIONS
          profile_inconsistent = 1
          internal_error       = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      CALL FUNCTION 'BAL_DSP_OUTPUT_INIT'
        EXPORTING
          i_s_display_profile = ls_display_profile.

      CALL FUNCTION 'BAL_DSP_OUTPUT_SET_DATA'               "#EC *
        EXPORTING
          i_t_log_handle       = lt_log_handl
        IMPORTING
          e_no_data_available  = lv_no_dat
        EXCEPTIONS
          internal_error       = 1
          profile_inconsistent = 2
          OTHERS               = 3.

    ELSE.

      CALL FUNCTION 'BAL_CNTL_REFRESH'
        EXPORTING
          i_control_handle = cv_control_handle
          i_t_log_handle   = lt_log_handl.

    ENDIF.
  ENDMETHOD.


  METHOD simulate.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Simulate slotting run
    "********************************************************************
    DATA(lo_analyse) = NEW /scwm/cl_con_analyse( ).
    " read data from material
    TRY.
        lo_analyse->show_detailed_view_mat( EXPORTING iv_matid       = iv_matid
                                                      iv_lgnum       = iv_lgnum
                                                      iv_entitled    = iv_entitled
                                                      iv_planned     = ''
                                            IMPORTING et_dynpro      = et_dynpro_c
                                            CHANGING  cs_dynpro_head = es_dynpro_head ).
      CATCH /scwm/cx_md_mat_lgnum_exist ##NO_HANDLER.
      CATCH /scwm/cx_md_interface ##NO_HANDLER.
      CATCH /scwm/cx_md_material_exist ##NO_HANDLER.
      CATCH /scwm/cx_md_lgnum_locid ##NO_HANDLER.
      CATCH /scwm/cx_md ##NO_HANDLER.
      CATCH /scwm/cx_core_no_data ##NO_HANDLER.

    ENDTRY.
  ENDMETHOD.                                             "#EC CI_VALPAR


  METHOD start_concepting.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Material will be consider here
    "*& There will be alway one material for GAP6
    "********************************************************************
    init_cl_concepting( ).
    CLEAR mt_mat_lgnum_result_sel.
    CLEAR mt_mat_lgtyp_result_sel.
    CLEAR mt_binmat_result_sel.
    CLEAR mt_mat_lgtyp_orig_sel.
    " Initialize the BAdI here
    TRY.
        GET BADI mi_badi_slot_upd_storg_type
          FILTERS
            lgnum = mv_lgnum.
      CATCH cx_badi ##NO_HANDLER.
    ENDTRY.
    IF mi_badi_slot_upd_storg_type IS BOUND.
      CALL BADI mi_badi_slot_upd_storg_type->determine_deletion_indicator
        EXPORTING
          flt_val        = mv_lgnum
          iv_lgnum       = mv_lgnum
          iv_entitled    = mv_entitled
        IMPORTING
          ev_delete_mode = mv_badi_sttype_upd_mode.
    ENDIF.

    mo_concore->concept_material_multi( EXPORTING it_slot_key             = it_slot_key
                                                  iv_lgnum                = mv_lgnum
                                                  iv_entitled             = mv_entitled
                                                  is_slot_steps           = ms_slot_steps
                                                  iv_nolog                = VALUE #( )
                                                  iv_badi_sttype_upd_mode = mv_badi_sttype_upd_mode
                                        IMPORTING et_mat_lgnum_result     = mt_mat_lgnum_result_sel
                                                  et_mat_lgtyp_result     = mt_mat_lgtyp_result_sel
                                                  et_binmat_result        = mt_binmat_result_sel
                                                  et_mat_lgtyp_orig       = mt_mat_lgtyp_orig_sel ).
  ENDMETHOD.


  METHOD start_concepting_alv.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Last preparation before concepting
    "********************************************************************
    DATA ls_mat_exec TYPE /scwm/s_slot_key.
    DATA lt_mat_exec TYPE /scwm/tt_slot_key. " Materials to be concepted
    DATA ls_mat_alv  TYPE /scwm/s_material_con.
    FIELD-SYMBOLS <ls_mat_slot> TYPE /scwm/s_material_con_intern.

    " Batch
    LOOP AT mt_mat_slot ASSIGNING <ls_mat_slot>.
      MOVE-CORRESPONDING <ls_mat_slot> TO ls_mat_exec ##ENH_OK.
      APPEND ls_mat_exec TO lt_mat_exec.
    ENDLOOP.

    " init protocol for all given materials
    LOOP AT mt_mat_sel ASSIGNING FIELD-SYMBOL(<ls_row>).
      READ TABLE mt_mat_alv INTO ls_mat_alv INDEX <ls_row>-index.
      IF sy-subrc = 0.
        DELETE mt_mat_prot WHERE     matid    = ls_mat_alv-matid
                                 AND lgnum    = ls_mat_alv-lgnum
                                 AND entitled = ls_mat_alv-entitled.
      ENDIF.
    ENDLOOP.

    DELETE mt_mat_prot WHERE log IS INITIAL.

    cv_slot = abap_true.

    start_concepting( it_slot_key = lt_mat_exec ).
  ENDMETHOD.


  METHOD update_gt_mat_slot.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Slotting status will be uptaded
    "********************************************************************
    DATA ls_mat_slot TYPE /scwm/s_material_con_intern.

    LOOP AT mt_mat_sel ASSIGNING FIELD-SYMBOL(<ls_row>).
      READ TABLE mt_mat_slot INTO ls_mat_slot INDEX <ls_row>-index.
      IF sy-subrc = 0.
        CLEAR ls_mat_slot-status.
        MODIFY mt_mat_slot FROM ls_mat_slot INDEX <ls_row>-index.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD update_lights.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update traffic lights
    "********************************************************************
    FIELD-SYMBOLS <ls_mat_slot> TYPE /scwm/s_material_con_intern.
    DATA lt_prot TYPE /scwm/tt_mat_prot.
    DATA ls_prot TYPE /scwm/s_mat_prot.

    " Get Loghandle(s)
    IF mo_concore IS BOUND.
      mo_concore->get_prot( EXPORTING it_mat_prot = mt_mat_prot
                            IMPORTING et_prot     = lt_prot ).
    ENDIF.

    LOOP AT mt_mat_slot ASSIGNING <ls_mat_slot>.

      READ TABLE lt_prot INTO ls_prot
           WITH KEY matid    = <ls_mat_slot>-matid
                    entitled = <ls_mat_slot>-entitled
                    lgnum    = <ls_mat_slot>-lgnum
                    type     = '1'.
      IF sy-subrc = 0.

        CASE ls_prot-severity.
          WHEN 'A' OR 'E'.
            <ls_mat_slot>-lights = '1'.                     " Red 1
          WHEN 'W'.
            <ls_mat_slot>-lights = '2'.                     " Yellow 2
          WHEN 'I' OR 'S'.
            <ls_mat_slot>-lights = '3'.                     " Green 3
        ENDCASE.

      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD update_logs.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update bal logs
    "********************************************************************
    DATA ls_mat_prot TYPE /scwm/s_mat_prot.
    DATA ls_mat_alv  TYPE /scwm/s_material_con.

    LOOP AT mt_mat_sel ASSIGNING FIELD-SYMBOL(<row>).
      READ TABLE mt_mat_alv INTO ls_mat_alv INDEX <row>-index.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_mat_alv TO ls_mat_prot ##ENH_OK.
        ls_mat_prot-type = '1'.
        APPEND ls_mat_prot TO mt_mat_prot.
      ENDIF.
    ENDLOOP.

    SORT mt_mat_prot BY matid
                        lgnum
                        entitled
                        type
                        lgtyp.
    DELETE ADJACENT DUPLICATES FROM mt_mat_prot.
  ENDMETHOD.


  METHOD update_logs_batch.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update logs
    "********************************************************************
    DATA ls_mat_prot TYPE /scwm/s_mat_prot.
    DATA ls_mat_slot TYPE /scwm/s_material_con_intern.

    LOOP AT mt_mat_slot INTO ls_mat_slot.
      MOVE-CORRESPONDING ls_mat_slot TO ls_mat_prot ##ENH_OK.
      ls_mat_prot-type = '1'.
      APPEND ls_mat_prot TO mt_mat_prot.
    ENDLOOP.

    SORT mt_mat_prot BY matid
                        lgnum
                        entitled
                        type
                        lgtyp.
    DELETE ADJACENT DUPLICATES FROM mt_mat_prot.
  ENDMETHOD.


  METHOD update_result_table.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update the result table
    "********************************************************************
    FIELD-SYMBOLS <ls_mat_lgnum_sel> TYPE /scwm/s_material_lgnum.
    FIELD-SYMBOLS <ls_mat_lgnum>     TYPE /scwm/s_material_lgnum.
    FIELD-SYMBOLS <ls_mat_lgtyp_sel> TYPE /scwm/s_material_lgtyp.
    FIELD-SYMBOLS <ls_mat_lgtyp>     TYPE /scwm/s_material_lgtyp.
    FIELD-SYMBOLS <ls_binmat_sel>    TYPE /scwm/binmat.
    FIELD-SYMBOLS <ls_binmat>        TYPE /scwm/binmat.
    DATA lv_tabix TYPE sytabix.

    " Update table with MM warehouse data
    SORT mt_mat_lgnum_result.
    LOOP AT mt_mat_lgnum_result_sel ASSIGNING <ls_mat_lgnum_sel>.
      READ TABLE mt_mat_lgnum_result
           WITH KEY matid    = <ls_mat_lgnum_sel>-matid
                    entitled = <ls_mat_lgnum_sel>-entitled
                    lgnum    = <ls_mat_lgnum_sel>-lgnum
           ASSIGNING <ls_mat_lgnum>
           BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_mat_lgnum> = <ls_mat_lgnum_sel>.
      ELSE.
        INSERT <ls_mat_lgnum_sel> INTO mt_mat_lgnum_result INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    " Update table with MM storage type data
    SORT mt_mat_lgtyp_result.
    LOOP AT mt_mat_lgtyp_result_sel ASSIGNING <ls_mat_lgtyp_sel>.
      READ TABLE mt_mat_lgtyp_result
           WITH KEY matid    = <ls_mat_lgtyp_sel>-matid
                    entitled = <ls_mat_lgtyp_sel>-entitled
                    lgnum    = <ls_mat_lgtyp_sel>-lgnum
                    lgtyp    = <ls_mat_lgtyp_sel>-lgtyp
           ASSIGNING <ls_mat_lgtyp>
           BINARY SEARCH.
      IF sy-subrc = 0.
        <ls_mat_lgtyp> = <ls_mat_lgtyp_sel>.
      ELSE.
        INSERT <ls_mat_lgtyp_sel> INTO mt_mat_lgtyp_result INDEX sy-tabix.
      ENDIF.
    ENDLOOP.

    " Update table with Fix Bin data
    LOOP AT mt_binmat_result_sel ASSIGNING <ls_binmat_sel>.
      READ TABLE mt_binmat_result
           ASSIGNING <ls_binmat>
           WITH TABLE KEY matid    = <ls_binmat_sel>-matid
                          lgnum    = <ls_binmat_sel>-lgnum
                          entitled = <ls_binmat_sel>-entitled
                          lgpla    = <ls_binmat_sel>-lgpla.
      lv_tabix = sy-tabix.
      IF sy-subrc = 0.
        <ls_binmat>-outcon         = <ls_binmat_sel>-outcon.
        <ls_binmat>-datout         = <ls_binmat_sel>-datout.
        <ls_binmat>-maxqty         = <ls_binmat_sel>-maxqty.
        <ls_binmat>-maxqty_uom_dsp = <ls_binmat_sel>-maxqty_uom_dsp.
        <ls_binmat>-minqty         = <ls_binmat_sel>-minqty.
        <ls_binmat>-minqty_uom_dsp = <ls_binmat_sel>-minqty_uom_dsp.
      ELSE.
        INSERT <ls_binmat_sel> INTO mt_binmat_result INDEX lv_tabix.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD variant_delete.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Delete variant of the background job
    "********************************************************************
    CALL FUNCTION 'RS_VARIANT_DELETE'
      EXPORTING
        report               = zif_c_mdm_tool=>c_variant-report_name
        variant              = iv_variant
        flag_confirmscreen   = abap_true
        flag_delallclient    = abap_true
        suppress_message     = abap_true
      EXCEPTIONS
        not_authorized       = 1
        not_executed         = 2
        no_report            = 3
        report_not_existent  = 4
        report_not_supplied  = 5
        variant_locked       = 6
        variant_not_existent = 7
        no_corr_insert       = 8
        variant_protected    = 9
        OTHERS               = 10.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
