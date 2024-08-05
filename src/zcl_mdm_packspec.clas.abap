CLASS zcl_mdm_packspec DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA mv_lgnum                   TYPE /scwm/lgnum.
    DATA mv_entitled                TYPE /scwm/de_entitled.
    DATA mv_ps_group                TYPE /scwm/de_ps_group.
    DATA mv_matnr                   TYPE mara-matnr.
    DATA mt_unit_of_measures        TYPE ztt_unit_of_measures.
    DATA ms_master_carton_aditional TYPE zstr_additional_data.
    DATA mo_packspeck_model         TYPE REF TO /scwm/cl_packspec_model.

    METHODS constructor
      IMPORTING iv_lgnum                   TYPE /scwm/lgnum
                iv_entitled                TYPE /scwm/de_entitled
                iv_matnr                   TYPE mara-matnr
                iv_ps_group                TYPE /scwm/de_ps_group
                it_unit_of_measures        TYPE ztt_unit_of_measures
                is_master_carton_aditional TYPE zstr_additional_data
                io_packspeck_model         TYPE REF TO /scwm/cl_packspec_model OPTIONAL.

    METHODS execute_packspec.

private section.

  methods ACTIVATE_STATUS
    changing
      !CO_PACKSPEC_MODEL type ref to /SCWM/CL_PACKSPEC_MODEL .
  methods BAPI_TRANSACTION_COMMIT .
  methods BAPI_TRANSACTION_ROLLBACK .
  methods CHANGE_LEVEL
    importing
      !IS_MATNR type /SCMB/MDL_MATNR_STR
    changing
      !CO_LEVEL type ref to /SCWM/CL_PACKSPEC_LEVEL
      !CO_PACKSPEC_MODEL type ref to /SCWM/CL_PACKSPEC_MODEL
      !CS_LEVEL type /SCWM/S_PS_LEVEL_INT .
  methods CHANGE_PACSPEC
    importing
      !IT_PS_KEY type /SCWM/TT_PS_HEADER_KEY .
  methods CHECK_DIMENSION
    importing
      !IV_UNIT_LWH type /SCWM/DIMEH  ##RELAX.
  methods CHECK_VOLUME
    importing
      !IV_UNIT_VOL type /SCWM/DE_VOL_UOM  ##RELAX.
  methods CHECK_WEIGHT
    importing
      !IV_UNIT_WGT type /SCWM/DE_WGT_UOM  ##RELAX.
  methods CONDITION_REPLICATE
    importing
      !IT_CONDITION type /SCWM/TT_API_CONDITION_HEADER .
  methods CREATE_PACKSPEC .
  methods FILL_ATTRIBUTES
    importing
      !IV_MATNR type /SCMB/MDL_MATNR
      !IV_KSCHL type KSCHL
      !IV_COND_TAB type CHAR8
      !IS_HEADER type /SCWM/S_PS_HEADER_INT
      !IS_CONTENT type /SCWM/S_PS_CONTENT_INT
    changing
      !CT_ATTRIBUTES type /SAPCND/DET_ATTRIB_VALUE_T .
  methods FILL_CONDTION_VALUES
    importing
      !IS_HEADER type /SCWM/S_PS_HEADER_INT
      !IS_CONTENT type /SCWM/S_PS_CONTENT_INT
    returning
      value(RT_CONDITION) type /SCWM/TT_API_CONDITION_HEADER .
  methods FILL_STR
    importing
      !IV_TABNAME type TABNAME
      !IV_FILE type DATA
    exporting
      !EV_STR type DATA .
  methods FILL_XSTR
    importing
      !IV_TABNAME type TABNAME
      !IV_STR type DATA
    exporting
      !EV_XSTR type DATA .
  methods GET_MAT_DETAILS_FROM_MATID
    importing
      !IV_MATID type /SCWM/DE_MATID
    returning
      value(RS_MATERIAL_DETAILS) type /SCMB/MDL_PRODUCT .
  methods GET_PACKSPEC_CONTENT
    importing
      !IO_PACKSPEC_MODEL type ref to /SCWM/CL_PACKSPEC_MODEL
    changing
      !CO_CONTENT type ref to /SCWM/CL_PACKSPEC_CONTENT .
  methods GET_PACKSPEC_LIST
    importing
      !IV_MATID type /SCWM/DE_MATID
      !IV_STATUS type /SCWM/DE_PS_STATUS
    returning
      value(RT_PS_KEYS) type /SCWM/TT_PS_HEADER_KEY .
  methods MANAGER_SAVE_ALL .
  methods PACKSPEC_ACTIVATE
    changing
      !CT_PS_KEYS type /SCWM/TT_PS_HEADER_KEY .
  methods PACKSPEC_READ_FROM_PS_ID
    importing
      !IT_PS_KEY type /SCWM/TT_PS_HEADER_KEY
    returning
      value(RT_PACKSPEC) type /SCWM/TT_PACKSPEC_INT .
  methods PACKSPEC_READ_KEYS_FROM_MATNR
    importing
      !IV_MATNR type /SCMB/MDL_MATNR
    returning
      value(RT_PS_KEY) type /SCWM/TT_PS_HEADER_KEY .
  methods RAISE_EXCEPTION_FROM_SY .
  methods READ_CONDITION_RECORDS
    importing
      !IT_PS_KEY type /SCWM/TT_PS_HEADER_KEY
    returning
      value(RT_CONDITIONS) type /SCWM/TT_PS_COND_REC  ##RELAX.
  methods READ_MATID_FROM_PRODUCT
    importing
      !IS_MATNR type /SCMB/MDL_MATNR_STR
    returning
      value(RS_MATID) type /SCMB/MDL_MATID_STR .
  methods READ_PRODUCT_FROM_MATID
    importing
      !IS_MATID type /SCMB/MDL_MATID_STR
    returning
      value(RS_PRODUCT) type /SCMB/MDL_PRODUCT  ##RELAX.
  methods SELECT_T340D
    importing
      !IV_LGNUM type /SCWM/LGNUM
    returning
      value(RS_T340D) type /SCWM/T340D .
  methods UPDATE_UNITS_OF_MEASURE  ##RELAX.
  methods UPDATE_WEIGHT_VOLUME_FIRST
    changing
      !CO_PACKSPEC_MODEL type ref to /SCWM/CL_PACKSPEC_MODEL .
ENDCLASS.



CLASS ZCL_MDM_PACKSPEC IMPLEMENTATION.


  METHOD activate_status.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Activate packspec status
    "********************************************************************
    co_packspec_model->set_status( EXPORTING  iv_activate = abap_true
                                   IMPORTING  eo_packspec = co_packspec_model
                                   EXCEPTIONS OTHERS      = 99 ).
    IF sy-subrc <> 0.
      raise_exception_from_sy( ).
    ENDIF.
    COMMIT WORK.
    /scwm/cl_tm=>cleanup( ).
  ENDMETHOD.


  METHOD bapi_transaction_commit.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :  Commit
    "********************************************************************
    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      EXPORTING
        wait = abap_true.
  ENDMETHOD.


  METHOD bapi_transaction_rollback.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Rollback
    "********************************************************************
    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
  ENDMETHOD.


  METHOD change_level.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Change level of packspeck
    "********************************************************************
    DATA ls_matnr TYPE /scmb/mdl_matnr_str.

    DATA(lo_element_group)    = co_level->go_element_group.
    DATA(lo_element)          = lo_element_group->gt_elements[ 1 ].
    DATA(ls_s_ps_element_int) = lo_element->get( ).
    ls_matnr-matnr = is_matnr-matnr.
    DATA(ls_matid) = read_matid_from_product( is_matnr = ls_matnr ).
    ls_s_ps_element_int-matnr = ls_matnr-matnr.
    ls_s_ps_element_int-matid = ls_matid-matid.
    ls_s_ps_element_int-unit  = 'ST'.
    ls_s_ps_element_int-quan  = 1.

    lo_element->change( CHANGING   cs_element = ls_s_ps_element_int
                        EXCEPTIONS OTHERS     = 99 ).
    IF sy-subrc <> 0.
      raise_exception_from_sy( ).
    ENDIF.

    co_packspec_model->change_level( EXPORTING  io_level = co_level
                                     CHANGING   cs_level = cs_level
                                     EXCEPTIONS error    = 1
                                                OTHERS   = 2 ).
    IF sy-subrc <> 0.
      raise_exception_from_sy( ).
    ENDIF.
    cs_level = co_level->get( ).
  ENDMETHOD.


  METHOD change_pacspec.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "*&
    "*&
    "********************************************************************
    DATA lo_packspec_model          TYPE REF TO /scwm/cl_packspec_model.
    DATA lo_packspec_model_inactive TYPE REF TO /scwm/cl_packspec_model.
    DATA ls_level_type              TYPE /scwm/tpslevel ##NEEDED.
    DATA ls_level_text              TYPE /scwm/tpslevelt ##NEEDED.
    DATA ls_matnr                   TYPE /scmb/mdl_matnr_str.
    DATA lt_ps_keys                 TYPE /scwm/tt_ps_header_key ##NEEDED.
    DATA lv_material18              TYPE char18.

    DATA(ls_carton) = mt_unit_of_measures[ id = zif_c_mdm_tool=>c_units-mastercarton ].
    DATA(ls_palet)  = mt_unit_of_measures[ id = zif_c_mdm_tool=>c_units-palet ].
    DATA(ls_t340d)  = select_t340d( iv_lgnum = mv_lgnum ).
    DATA(lt_packspec) = packspec_read_from_ps_id( it_ps_key = it_ps_key ).

    IF lines( lt_packspec ) > 1.
      CREATE OBJECT lo_packspec_model_inactive
        EXPORTING
          iv_guid_ps          = VALUE #( lt_packspec[ 2 ]-header-guid_ps )
          iv_version          = VALUE #( lt_packspec[ 2 ]-header-aennr )
          iv_lock             = COND #( WHEN lines( lt_packspec ) > 1 THEN abap_false ELSE abap_true )
        EXCEPTIONS
          packspec_not_locked = 1
          OTHERS              = 99.
      IF sy-subrc <> 0.
        CREATE OBJECT lo_packspec_model_inactive
          EXPORTING
            iv_guid_ps          = VALUE #( lt_packspec[ 2 ]-header-guid_ps )
            iv_version          = VALUE #( lt_packspec[ 2 ]-header-aennr )
            iv_lock             = COND #( WHEN lines( lt_packspec ) > 1 THEN abap_true ELSE abap_false )
          EXCEPTIONS
            packspec_not_locked = 1
            OTHERS              = 99 ##SUBRC_OK.
      ENDIF.
      activate_status( CHANGING co_packspec_model = lo_packspec_model_inactive ).
      manager_save_all( ).
      activate_status( CHANGING co_packspec_model = lo_packspec_model_inactive ).

      CREATE OBJECT lo_packspec_model_inactive
        EXPORTING
          iv_guid_ps = lt_packspec[ 2 ]-header-guid_ps
          iv_version = lt_packspec[ 2 ]-header-aennr
          iv_lock    = ' '
        EXCEPTIONS
          OTHERS     = 3 ##SUBRC_OK.

      manager_save_all( ).

      " Save Pack Specs
      /scwm/cl_ppelipak_cntl=>save_all( EXPORTING  iv_commit    = space
                                        EXCEPTIONS update_error = 1
                                                   OTHERS       = 2 ) ##SUBRC_OK.
    ENDIF.

    IF me->mo_packspeck_model IS BOUND.
      lo_packspec_model = mo_packspeck_model.
    ELSE.
      CREATE OBJECT lo_packspec_model
        EXPORTING
          iv_guid_ps          = VALUE #( lt_packspec[ 1 ]-header-guid_ps )
          iv_version          = VALUE #( lt_packspec[ 1 ]-header-aennr )
          iv_lock             = COND #( WHEN lines( lt_packspec ) > 1 THEN abap_true ELSE abap_false )
        EXCEPTIONS
          packspec_not_locked = 1
          OTHERS              = 99.
      IF sy-subrc <> 0.
        CREATE OBJECT lo_packspec_model
          EXPORTING
            iv_guid_ps          = VALUE #( lt_packspec[ 1 ]-header-guid_ps )
            iv_version          = VALUE #( lt_packspec[ 1 ]-header-aennr )
            iv_lock             = COND #( WHEN lines( lt_packspec ) > 1 THEN abap_false ELSE abap_true )
          EXCEPTIONS
            packspec_not_locked = 1
            OTHERS              = 99 ##SUBRC_OK.
      ENDIF.
    ENDIF.

    lo_packspec_model->generate_version( IMPORTING  eo_packspec = DATA(lo_ps_model)
                                         EXCEPTIONS OTHERS      = 99 ).
    IF sy-subrc <> 0.
      raise_exception_from_sy( ).
    ENDIF.
    lo_packspec_model = lo_ps_model.

    DATA(ls_header) = lo_packspec_model->go_header->get( ).

    LOOP AT lo_packspec_model->gt_level INTO DATA(lo_level).

      DATA(ls_level) = lo_level->get( ).

      CALL FUNCTION '/SCWM/TPSLEVEL_READ_SINGLE'
        EXPORTING
          iv_level_type = ls_level-level_type
        IMPORTING
          es_tpslevel   = ls_level_type
          es_tpslevelt  = ls_level_text
        EXCEPTIONS
          OTHERS        = 99 ##FM_SUBRC_OK.

      IF ls_level_type-level_type = zif_c_mdm_tool=>c_packspec-level_type_01. "'LT01'.
        CLEAR lv_material18.
        lv_material18 =  |{ ls_carton-pmat ALPHA = OUT }|.
        lv_material18 =  |{ lv_material18  ALPHA = IN }|.

        ls_matnr-matnr = lv_material18.
        IF     ms_master_carton_aditional-zz1_dirrpl_stt IS INITIAL
           AND ms_master_carton_aditional-tote_quan      IS NOT INITIAL.
          ls_level-trgqty = ms_master_carton_aditional-tote_quan.
        ELSE.
          ls_level-trgqty = ls_carton-umrez.
        ENDIF.
        ls_level-hutyp = ls_carton-hutyp.

        IF ms_master_carton_aditional-zz1_dirrpl_stt IS NOT INITIAL.
          ls_level-flag_weight  = abap_true.
          ls_level-flag_vol     = abap_true.
          ls_level-flag_dim     = abap_true.

          ls_level-max_length   = ls_carton-laeng.
          ls_level-max_width    = ls_carton-breit.
          ls_level-max_height   = ls_carton-hoehe.
          ls_level-unit_max_lwh = ls_carton-meabm.

          ls_level-max_weight   = ls_carton-brgew.
          ls_level-unit_gw      = ls_carton-gewei.

          ls_level-max_volume   = ls_carton-laeng * ls_carton-breit * ls_carton-hoehe.
          ls_level-unit_gv      = ls_t340d-voleh.
        ENDIF.

        change_level( EXPORTING is_matnr          = ls_matnr
                      CHANGING  co_level          = lo_level
                                co_packspec_model = lo_packspec_model
                                cs_level          = ls_level ).
      ENDIF.

      IF ls_level_type-level_type = zif_c_mdm_tool=>c_packspec-level_type_02.
        CLEAR lv_material18.
        lv_material18 =  |{ ls_palet-pmat ALPHA = OUT }|.
        lv_material18 =  |{ lv_material18 ALPHA = IN }|.

        ls_matnr-matnr = lv_material18.
        ls_level-trgqty = ls_palet-umrez.
        ls_level-hutyp  = ls_palet-hutyp. " 109.

        change_level( EXPORTING is_matnr          = ls_matnr
                      CHANGING  co_level          = lo_level
                                co_packspec_model = lo_packspec_model
                                cs_level          = ls_level ).
      ENDIF.
      CLEAR: ls_level_type,
             ls_level_text,
             ls_level.
    ENDLOOP.

    " update the weight volume first
    update_weight_volume_first( CHANGING co_packspec_model = lo_packspec_model ).

    DATA(ls_ps_keys) = it_ps_key[ 1 ].
    ls_ps_keys-ps_id   = ls_header-ps_id.
    ls_ps_keys-guid_ps = ls_header-guid_ps.
    ls_ps_keys-aennr   = ls_header-aennr.
    APPEND ls_ps_keys TO lt_ps_keys.

    manager_save_all( ).
    activate_status( CHANGING co_packspec_model = lo_packspec_model ).

    CREATE OBJECT lo_packspec_model
      EXPORTING
        iv_guid_ps = ls_ps_keys-guid_ps
        iv_version = ls_ps_keys-aennr
        iv_lock    = ' '
      EXCEPTIONS
        OTHERS     = 3 ##SUBRC_OK.

    manager_save_all( ).

    " Save Pack Specs
    /scwm/cl_ppelipak_cntl=>save_all( EXPORTING  iv_commit    = space
                                      EXCEPTIONS update_error = 1
                                                 OTHERS       = 2 ).
    IF sy-subrc = 0.
      MESSAGE s019(zmc_mdm_tool) WITH ls_ps_keys-ps_id. " Changes made to &1 packaging specifications
    ENDIF.
  ENDMETHOD.


  METHOD check_dimension.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Dimension check
    "********************************************************************
    CALL FUNCTION 'DIMENSIONCHECK_LENGTH'
      EXPORTING
        meabm                = iv_unit_lwh
      EXCEPTIONS
        dimension_not_length = 1
        t006d_entry_missing  = 2
        t006_entry_missing   = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD check_volume.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check Volume
    "********************************************************************
    CALL FUNCTION 'DIMENSIONCHECK_VOLUME'
      EXPORTING
        voleh                = iv_unit_vol
      EXCEPTIONS
        dimension_not_volume = 1
        t006d_entry_missing  = 2
        t006_entry_missing   = 3
        OTHERS               = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD check_weight.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Check weight
    "********************************************************************
    CALL FUNCTION 'DIMENSIONCHECK_MASS'
      EXPORTING
        gewei               = iv_unit_wgt
      EXCEPTIONS
        dimension_not_mass  = 1
        t006d_entry_missing = 2
        t006_entry_missing  = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
  ENDMETHOD.


  METHOD condition_replicate.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Replicat packspeck conditions
    "********************************************************************
    DATA(lv_logsys) = /scwm/cl_prc_md_system_data=>get_own_logsys( ).
    CALL FUNCTION '/SCWM/API_CONDITION_REPLICATE'
      EXPORTING
        it_condition  = it_condition
        sender_system = lv_logsys
        iv_create     = abap_true
      EXCEPTIONS
        error         = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      raise_exception_from_sy( ).
    ENDIF.
    /scwm/cl_ppelipak_cntl=>save_all( EXCEPTIONS update_error = 1
                                                 OTHERS       = 2 ) ##SUBRC_OK.
    COMMIT WORK AND WAIT.
    /scwm/cl_tm=>cleanup( ).
  ENDMETHOD.


  METHOD constructor.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "********************************************************************
    mv_lgnum            = iv_lgnum.
    mv_entitled         = iv_entitled.
    mv_matnr            = iv_matnr.
    mv_ps_group         = iv_ps_group.
    mt_unit_of_measures = it_unit_of_measures.
    ms_master_carton_aditional = is_master_carton_aditional.
    mo_packspeck_model = io_packspeck_model.
  ENDMETHOD.


  METHOD create_packspec.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Packaging specifications and their level will be created.
    "********************************************************************
    DATA ls_packspec   TYPE /scwm/s_packspec_int.
    DATA lo_content    TYPE REF TO /scwm/cl_packspec_content.
    DATA ls_level_type TYPE /scwm/tpslevel.
    DATA ls_level_text TYPE /scwm/tpslevelt ##NEEDED.
    DATA ls_matnr      TYPE /scmb/mdl_matnr_str.
    DATA ls_content    TYPE /scwm/s_ps_content_int.
    DATA lv_material18 TYPE char18.

    DATA(ls_carton) = mt_unit_of_measures[ id = zif_c_mdm_tool=>c_units-mastercarton ].
    DATA(ls_palet)  = mt_unit_of_measures[ id = zif_c_mdm_tool=>c_units-palet ].
    DATA(ls_t340d)  = select_t340d( iv_lgnum = mv_lgnum ).
    " refresh log
    /scwm/cl_packspec=>go_prot->init( ).

    DATA(lo_packspec_model) = NEW /scwm/cl_packspec_model( ).

    lo_packspec_model->go_prot->get_instance( IMPORTING eo_instance = DATA(lo_log) ) ##NEEDED.

    SELECT SINGLE level_set FROM /scwm/tpsgroup
      INTO ls_packspec-header-level_set
      WHERE ps_group = mv_ps_group.

    ls_packspec-header-ps_group       = mv_ps_group.
    ls_packspec-header-ps_description = TEXT-001 && | { mv_matnr } |.

    lo_packspec_model->create( EXPORTING  is_object       = ls_packspec
                               EXCEPTIONS creation_failed = 1
                                          OTHERS          = 2 ).
    IF sy-subrc <> 0.
      raise_exception_from_sy( ).
    ENDIF.

    DATA(ls_header) = lo_packspec_model->go_header->get( ).

    get_packspec_content( EXPORTING io_packspec_model = lo_packspec_model
                          CHANGING  co_content        = lo_content ).

    ls_content-guid        = lo_content->gs_key-guid.
    ls_content-aennr       = lo_content->gs_key-aennr.
    ls_content-content_seq = lo_content->gs_key-content_seq.
    ls_matnr-matnr = mv_matnr.

    ls_content-matid = read_matid_from_product( is_matnr = ls_matnr )-matid.
    IF ls_content-matid IS INITIAL.
      MESSAGE e012(/scwm/ps_basics) WITH ls_content-matnr.
    ENDIF.

    " pass changes to backend
    lo_content->change( CHANGING   cs_content = ls_content
                        EXCEPTIONS OTHERS     = 1 ) ##SUBRC_OK.

    LOOP AT lo_packspec_model->gt_level INTO DATA(lo_level).
      DATA(ls_level) = lo_level->get( ).
      CALL FUNCTION '/SCWM/TPSLEVEL_READ_SINGLE'
        EXPORTING
          iv_level_type = ls_level-level_type
        IMPORTING
          es_tpslevel   = ls_level_type
          es_tpslevelt  = ls_level_text
        EXCEPTIONS
          OTHERS        = 99.
      IF sy-subrc <> 0.
        raise_exception_from_sy( ).
      ENDIF.

      IF ls_level_type-level_type = zif_c_mdm_tool=>c_packspec-level_type_01.
        CLEAR lv_material18.
        lv_material18 = |{ ls_carton-pmat ALPHA = OUT }|.
        lv_material18 = |{ lv_material18  ALPHA = IN }|.

        DATA(lv_pmat_id) = read_matid_from_product( is_matnr = CONV #( lv_material18 ) )-matid.
        DATA(ls_pmat_details) = get_mat_details_from_matid( lv_pmat_id ) ##NEEDED.

        ls_matnr-matnr = lv_material18.
        ls_level-display_seq = 1.
        ls_level-quancla     = 2. " cartons/totes

        IF     ms_master_carton_aditional-zz1_dirrpl_stt IS INITIAL
           AND ms_master_carton_aditional-tote_quan      IS NOT INITIAL.
          ls_level-trgqty = ms_master_carton_aditional-tote_quan.
        ELSE.
          ls_level-trgqty = ls_carton-umrez.
        ENDIF.

        ls_level-hutyp            = ls_carton-hutyp. " 801.
        ls_level-flg_minimum_ps   = abap_true.
        ls_level-band_rnd_nearest = abap_true.

        IF ms_master_carton_aditional-zz1_dirrpl_stt IS NOT INITIAL.
          ls_level-flag_weight  = abap_true.
          ls_level-flag_vol     = abap_true.
          ls_level-flag_dim     = abap_true.

          ls_level-max_length   = ls_carton-laeng.
          ls_level-max_width    = ls_carton-breit.
          ls_level-max_height   = ls_carton-hoehe.
          ls_level-unit_max_lwh = ls_carton-meabm.

          ls_level-max_weight   = ls_carton-brgew.
          ls_level-unit_gw      = ls_carton-gewei.

          ls_level-max_volume   = ls_carton-laeng * ls_carton-breit * ls_carton-hoehe.
          ls_level-unit_gv      = ls_t340d-voleh.
        ENDIF.

        change_level( EXPORTING is_matnr          = ls_matnr
                      CHANGING  co_level          = lo_level
                                co_packspec_model = lo_packspec_model
                                cs_level          = ls_level ).
      ENDIF.

      IF ls_level_type-level_type = zif_c_mdm_tool=>c_packspec-level_type_02.
        CLEAR lv_material18.
        lv_material18 =  |{ ls_palet-pmat ALPHA = OUT }|.
        lv_material18 =  |{ lv_material18 ALPHA = IN }|.

        ls_matnr-matnr = lv_material18.
        ls_level-display_seq = 1.
        ls_level-quancla     = 3. " pallets
        IF ls_carton-umrez IS NOT INITIAL.
          ls_level-trgqty = ls_palet-umrez / ls_carton-umrez.
        ELSE.
          ls_level-trgqty = 1.
        ENDIF.

        ls_level-hutyp = ls_palet-hutyp. " 109.

        change_level( EXPORTING is_matnr          = ls_matnr
                      CHANGING  co_level          = lo_level
                                co_packspec_model = lo_packspec_model
                                cs_level          = ls_level ).
      ENDIF.
      CLEAR: ls_level_type,
             ls_level_text,
             ls_level,
             lv_pmat_id,
             ls_pmat_details.
    ENDLOOP.

    " update the weight volume first
    update_weight_volume_first( CHANGING co_packspec_model = lo_packspec_model ).

    DATA(lt_condition) = fill_condtion_values( is_header  = ls_header
                                               is_content = ls_content ).

    condition_replicate( lt_condition ).

    DATA(lt_ps_keys) = get_packspec_list( iv_matid  = ls_content-matid
                                          iv_status = /scwm/cl_ppelipak_cntl_const=>gc_status_inactive ).
    DATA(ls_ps_keys) = VALUE #( lt_ps_keys[ 1 ] OPTIONAL ).
    IF ls_ps_keys IS NOT INITIAL.
      ls_ps_keys-ps_id   = ls_ps_keys-ps_id ##NEEDED.
      ls_ps_keys-guid_ps = ls_ps_keys-guid_ps ##NEEDED.
      ls_ps_keys-aennr   = '00000001' ##NEEDED.
      APPEND ls_ps_keys TO lt_ps_keys ##NEEDED.
    ELSE.
      ls_ps_keys-ps_id   = ls_header-ps_id.
      ls_ps_keys-guid_ps = ls_header-guid_ps.
      ls_ps_keys-aennr   = '00000001'.
      APPEND ls_ps_keys TO lt_ps_keys ##NEEDED.
    ENDIF.
    packspec_activate( CHANGING ct_ps_keys = lt_ps_keys ).

    manager_save_all( ).
    activate_status( CHANGING co_packspec_model = lo_packspec_model ).

    CREATE OBJECT lo_packspec_model
      EXPORTING
        iv_guid_ps = ls_ps_keys-guid_ps
        iv_version = ls_ps_keys-aennr
        iv_lock    = ' '
      EXCEPTIONS
        OTHERS     = 3 ##SUBRC_OK.

    manager_save_all( ).

    " Save Pack Specs
    /scwm/cl_ppelipak_cntl=>save_all( EXPORTING  iv_commit    = space
                                      EXCEPTIONS update_error = 1
                                                 OTHERS       = 2 ).
    IF sy-subrc = 0.
      MESSAGE s020(zmc_mdm_tool) WITH ls_ps_keys-ps_id. " Packaging specification &1 has been created
    ENDIF.
  ENDMETHOD.


  METHOD execute_packspec.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Change or Create packspeck
    "********************************************************************
    DATA(lt_ps_key) = packspec_read_keys_from_matnr( iv_matnr = mv_matnr ).

    IF lt_ps_key IS NOT INITIAL.
      change_pacspec( it_ps_key = lt_ps_key ).
    ELSE.
      create_packspec( ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_attributes.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Attributes for packspec conditions
    "********************************************************************
    DATA ls_cond_attr      TYPE /sapcnd/det_attrib_value.
    DATA ls_scu            TYPE /scwm/s_t300_md.
    DATA lt_prodids        TYPE /scmb/mdl_extprod_key_tab ##NEEDED.
    DATA lo_data_container TYPE REF TO /scwm/cl_prc_md_data_container.

    lo_data_container = /scwm/cl_prc_md_data_container=>get_instance( ).

    " Get Supply Chain Unit
    ls_scu = /scwm/cl_prc_md=>get_scu_by_lgnum( mv_lgnum ).

    " Get internal Product Names
    lt_prodids = lo_data_container->get_product_ids( mv_entitled ).

    " Fill Attributes
    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-ps_guid.
    ls_cond_attr-value     = is_header-guid_ps.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-object_id.
    ls_cond_attr-value     = '00000000000000000000000000000001'.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-kschl.
    ls_cond_attr-value     = iv_kschl.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    " set additional  attributes  for condition technique
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-client.
    ls_cond_attr-value     = sy-mandt.
    INSERT ls_cond_attr  INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-pak_locid.
    ls_cond_attr-value     = ls_scu-scuguid.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-pak_locno.
    ls_cond_attr-value     = ls_scu-sc_unit.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-pak_matid.
    ls_cond_attr-value     = is_content-matid.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-pak_matnr.
    ls_cond_attr-value     = iv_matnr.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-ps_psid.
    ls_cond_attr-value     = is_header-ps_id.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-ps_seq.
    ls_cond_attr-value     = '01'.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-created_by.
    ls_cond_attr-value     = sy-uname.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-created_on.
    ls_cond_attr-value     = sy-datum.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-date_from.
    ls_cond_attr-value     = sy-datum.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-date_to.
    ls_cond_attr-value     = '99991231'.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-timestamp_from.
    GET TIME STAMP FIELD DATA(lv_timestamp).
    ls_cond_attr-value = lv_timestamp.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-timestamp_to.
    ls_cond_attr-value     = 99991231235959.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-kotabnr.
    ls_cond_attr-value     = iv_cond_tab.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-kvewe.
    ls_cond_attr-value     = 'PS'.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-release_status.
    ls_cond_attr-value     = ' '.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-kappl.
    ls_cond_attr-value     = 'PAK'.
    INSERT ls_cond_attr INTO TABLE ct_attributes.

    CLEAR ls_cond_attr.
    ls_cond_attr-fieldname = zif_c_mdm_tool=>c_fieldnames-mnt_ow_maint_mode_on_select.
    ls_cond_attr-value     = 'B'.
    INSERT ls_cond_attr INTO TABLE ct_attributes.
  ENDMETHOD.


  METHOD fill_condtion_values.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Filling condition values
    "********************************************************************
    DATA ls_condition TYPE /scwm/api_condition_header.

    CLEAR ls_condition.
    ls_condition-dbaction_tabl = wmegc_sign_inclusive.
    ls_condition-ps_guid       = is_header-guid_ps.

    "<AAHMEDOV>-240130
    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zcross_0006                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_condtab_ibd                 " Parameter ID for process
      IMPORTING
        ev_constant  = DATA(lv_condtab_ibd)                " Parameter-Framework Low
    ).

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zcross_0006                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_condtab_zslo                 " Parameter ID for process
      IMPORTING
        ev_constant  = DATA(lv_condtab_zslo)                " Parameter-Framework Low
    ).

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zcross_0006                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_condtype_zslo              " Parameter ID for process
      IMPORTING
        ev_constant  = DATA(lv_condtype_zslo)                " Parameter-Framework Low
    ).

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = mv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zcross_0006                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_condtype_0ibd              " Parameter ID for process
      IMPORTING
        ev_constant  = DATA(lv_condtype_0ibd)                " Parameter-Framework Low
    ).
    "<AAHMEDOV>-240130

*    "Fill Attributes
    fill_attributes( EXPORTING iv_matnr      = mv_matnr
                               iv_kschl      = conv #( lv_condtype_0ibd )
                               iv_cond_tab   = CONV #( lv_condtab_ibd )
                               is_header     = is_header
                               is_content    = is_content
                     CHANGING  ct_attributes = ls_condition-attributes ).
    APPEND ls_condition TO rt_condition.
    CLEAR ls_condition.

    ls_condition-dbaction_tabl = wmegc_sign_inclusive.
    ls_condition-ps_guid       = is_header-guid_ps.

    fill_attributes( EXPORTING iv_matnr      = mv_matnr
                               iv_kschl      = CONV #( lv_condtype_zslo )
                               iv_cond_tab   = CONV #( lv_condtab_zslo )
                               is_header     = is_header
                               is_content    = is_content
                     CHANGING  ct_attributes = ls_condition-attributes ).
    APPEND ls_condition TO rt_condition.
  ENDMETHOD.


  METHOD fill_str.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Dynamically fill material structure for the BAPI
    "********************************************************************

    DATA lt_fields_tabl_iso  TYPE TABLE OF dd03m.
    DATA lt_fields_tabl_bapi TYPE TABLE OF dd03m.

    SELECT * FROM dd03m                       "#EC CI_ALL_FIELDS_NEEDED
      INTO TABLE @DATA(lt_fields_bapi)
      WHERE tabname    = @iv_tabname
        AND ddlanguage = @sy-langu.

    SELECT * FROM dd03m                       "#EC CI_ALL_FIELDS_NEEDED
      INTO TABLE @DATA(lt_fields_tabl)
      WHERE tabname    = @iv_tabname+5(4)
        AND ddlanguage = @sy-langu.

    lt_fields_tabl_iso  = VALUE #( FOR wa IN lt_fields_tabl
                                   ( fieldname = wa-fieldname rollname = wa-rollname && '_ISO'  ) ).
    lt_fields_tabl_bapi = VALUE #( FOR wa IN lt_fields_tabl
                                   ( fieldname = wa-fieldname rollname = wa-rollname && '_BAPI' ) ).

    LOOP AT lt_fields_bapi INTO DATA(ls_fields_bapi) ##INTO_OK.
      DATA(lv_field) = VALUE #( lt_fields_tabl[ rollname = ls_fields_bapi-rollname ]-fieldname OPTIONAL ).
      IF lv_field IS INITIAL.

        lv_field = VALUE #( lt_fields_tabl_iso[ rollname = ls_fields_bapi-rollname ]-fieldname OPTIONAL ).
        IF lv_field IS INITIAL.
          lv_field = VALUE #( lt_fields_tabl_bapi[ rollname = ls_fields_bapi-rollname ]-fieldname OPTIONAL ).
          IF lv_field IS INITIAL.
            CASE ls_fields_bapi-rollname.
              WHEN zif_c_mdm_tool=>c_fieldnames-bismt40.
                lv_field = zif_c_mdm_tool=>c_fieldnames-bismt.
              WHEN zif_c_mdm_tool=>c_fieldnames-dwerk.
                lv_field = zif_c_mdm_tool=>c_fieldnames-dwerk.
              WHEN zif_c_mdm_tool=>c_fieldnames-prszk_bapi.
                lv_field = zif_c_mdm_tool=>c_fieldnames-zkprs.
              WHEN zif_c_mdm_tool=>c_fieldnames-taxkm.
                IF ls_fields_bapi-position = 4. lv_field = zif_c_mdm_tool=>c_fieldnames-taxm1. ENDIF.
              WHEN zif_c_mdm_tool=>c_fieldnames-tatyp.
                lv_field = zif_c_mdm_tool=>c_fieldnames-kschl.
              WHEN zif_c_mdm_tool=>c_fieldnames-xchpf.
                lv_field = zif_c_mdm_tool=>c_fieldnames-xchpf.
            ENDCASE.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lv_field IS INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_fields_bapi-fieldname OF STRUCTURE ev_str TO FIELD-SYMBOL(<ls_bapi>).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT lv_field OF STRUCTURE iv_file TO FIELD-SYMBOL(<ls_tabl>).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      <ls_bapi> = <ls_tabl>.
    ENDLOOP.

    REFRESH : lt_fields_bapi,lt_fields_tabl.

    SELECT dd03l~fieldname
           dd03l~rollname
      FROM dd03l
      INTO CORRESPONDING FIELDS OF TABLE lt_fields_bapi
      WHERE dd03l~tabname   = iv_tabname
        AND dd03l~precfield = ''
        AND dd03l~domname   = ''
        AND dd03l~as4local  = 'A' ##TOO_MANY_ITAB_FIELDS.

    SELECT dd03l~fieldname
           dd03l~rollname
      FROM dd03l
      INTO CORRESPONDING FIELDS OF TABLE lt_fields_tabl
      WHERE dd03l~tabname   = iv_tabname+5(4)
        AND dd03l~precfield = ''
        AND dd03l~domname   = ''
        AND dd03l~as4local  = 'A' ##TOO_MANY_ITAB_FIELDS.

    LOOP AT lt_fields_bapi INTO ls_fields_bapi ##INTO_OK.
      CLEAR lv_field.
      lv_field = VALUE #( lt_fields_tabl[ rollname = ls_fields_bapi-rollname ]-fieldname OPTIONAL ).

      IF lv_field IS INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_fields_bapi-fieldname OF STRUCTURE ev_str TO <ls_bapi>.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT lv_field OF STRUCTURE iv_file TO <ls_tabl>.
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      <ls_bapi> = <ls_tabl>.
    ENDLOOP.
  ENDMETHOD.


  METHOD fill_xstr.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :  Dynamically fill material control structure for the BAPI
    "********************************************************************
    DATA lt_dfies_tab TYPE STANDARD TABLE OF dfies.

    CALL FUNCTION 'DDIF_FIELDINFO_GET'
      EXPORTING
        tabname        = iv_tabname
      TABLES
        dfies_tab      = lt_dfies_tab
      EXCEPTIONS
        not_found      = 1
        internal_error = 2
        OTHERS         = 3 ##FM_SUBRC_OK.

    LOOP AT lt_dfies_tab INTO DATA(ls_dfies_tab) ##INTO_OK.
      ASSIGN COMPONENT ls_dfies_tab-fieldname OF STRUCTURE iv_str TO FIELD-SYMBOL(<ls_bapi>).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      ASSIGN COMPONENT ls_dfies_tab-fieldname OF STRUCTURE ev_xstr TO FIELD-SYMBOL(<ls_bapix>).
      IF sy-subrc IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      IF <ls_bapi> IS NOT INITIAL.
        IF ls_dfies_tab-keyflag = 'X' OR ls_dfies_tab-fieldname = 'PO_ITEM' OR ls_dfies_tab-fieldname = 'SCHED_LINE'.
          <ls_bapix> = <ls_bapi>.
        ELSE.
          <ls_bapix> = abap_true.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_mat_details_from_matid.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "********************************************************************
    IF iv_matid IS INITIAL. RETURN. ENDIF.
    TRY.
        CALL FUNCTION '/SCMB/MDL_PRODUCT_READ'
          EXPORTING
            iv_id   = iv_matid
          IMPORTING
            es_data = rs_material_details.
      CATCH
     /scmb/cx_mdl_interface
     /scmb/cx_mdl_result_empty
     /scmb/cx_mdl_result_toomany
     /scmb/cx_mdl_result ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD get_packspec_content.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "********************************************************************
    DATA(lt_content) = io_packspec_model->gt_content.
    co_content = lt_content[ 1 ].
  ENDMETHOD.


  METHOD get_packspec_list.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get packspec list
    "********************************************************************
    CALL FUNCTION '/SCWM/API_PACKSPEC_GETLIST'
      EXPORTING
        is_content_query = VALUE /scwm/s_ps_content_query( matid = VALUE #( ( matid = iv_matid ) ) )
        it_status_rng    = VALUE rseloption( ( sign   = wmegc_sign_inclusive
                                               option = wmegc_option_eq
                                               low    = iv_status ) )
      IMPORTING
        et_ps_keys       = rt_ps_keys.
  ENDMETHOD.


  METHOD manager_save_all.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Manager saver class
    "********************************************************************
    DATA(lo_mgr) = /scwm/cl_packspec_mgr=>get_instance( ).
    lo_mgr->save_all( EXCEPTIONS error  = 1
                                 OTHERS = 2 ).
    IF sy-subrc <> 0.
      raise_exception_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD packspec_activate.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Activate packspec
    "********************************************************************
    DATA lt_ret TYPE bapirettab ##NEEDED.

    CALL FUNCTION '/SCWM/API_PACKSPEC_ACTIVATE'
      EXPORTING
        iv_activate = abap_true
      IMPORTING
        et_return   = lt_ret
      CHANGING
        ct_ps_key   = ct_ps_keys.
    COMMIT WORK.

    /scwm/cl_tm=>cleanup( ).
    /scwm/cl_ppelipak_cntl=>buffer_refresh( ).
  ENDMETHOD.


  METHOD packspec_read_from_ps_id.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Reading of packspec Id
    "********************************************************************
    DATA ls_read_options TYPE /scwm/s_ps_read_options.

    ls_read_options-with_text = abap_true.
    ls_read_options-header    = ls_read_options-with_text.

    /scwm/cl_ppelipak_cntl=>packspec_read( EXPORTING  it_ps_key       = it_ps_key
                                                      iv_get_active   = VALUE #( )
                                                      it_status_rng   = VALUE #( )
                                           IMPORTING  et_packspec     = rt_packspec
                                           CHANGING   cs_read_options = ls_read_options
                                           EXCEPTIONS read_error      = 1
                                                      OTHERS          = 2 ) ##SUBRC_OK.
  ENDMETHOD.


  METHOD packspec_read_keys_from_matnr.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "********************************************************************
    DATA lt_return TYPE bapirettab ##NEEDED.
    DATA lv_maxrow TYPE int4.

    CALL FUNCTION '/SCWM/API_PACKSPEC_GETLIST'
      EXPORTING
        is_content_query = VALUE /scwm/s_ps_content_query( matnr_rng = VALUE #( ( sign   = wmegc_sign_inclusive
                                                                                  option = wmegc_option_eq
                                                                                  low    = iv_matnr ) ) )
        it_psid_rng      = VALUE /scwm/tt_ps_psid_rtab( )
      IMPORTING
        et_ps_keys       = rt_ps_key
        et_return        = lt_return
      CHANGING
        cv_max_rows      = lv_maxrow.
  ENDMETHOD.


  METHOD raise_exception_from_sy.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Exceptions from system messages
    "********************************************************************
    RAISE EXCEPTION TYPE zcx_mdm_tool
          MESSAGE ID sy-msgid
          TYPE sy-msgty
          NUMBER sy-msgno
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDMETHOD.


  METHOD read_condition_records.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Reading of condition records
    "********************************************************************
    DATA lt_ranges TYPE /sapcnd/t_attrib_selection_ext.
    DATA lt_values TYPE /sapcnd/t_attrib_value_int.

    " simple search.
    CALL FUNCTION '/SCWM/PS_SELECT_CT_PREPARE'
      EXPORTING
        it_ps_key = it_ps_key
      IMPORTING
        et_ranges = lt_ranges
        et_values = lt_values
      EXCEPTIONS
        error     = 1
        OTHERS    = 99.
    IF sy-subrc IS NOT INITIAL.
      raise_exception_from_sy( ).
    ENDIF.

    " Step 3: run CondTech-Selection
    CALL FUNCTION '/SCWM/PS_SELECT_CONDTECH'
      EXPORTING
        it_ranges       = lt_ranges
        it_values       = lt_values
        iv_only_valid   = space
      IMPORTING
        et_records      = rt_conditions
      EXCEPTIONS
        no_record_found = 1
        select_error    = 2
        OTHERS          = 3.
    IF sy-subrc > 1.
      raise_exception_from_sy( ).
    ENDIF.
  ENDMETHOD.


  METHOD read_matid_from_product.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read material Id of given product
    "********************************************************************
    TRY.
        CALL FUNCTION '/SCMB/MDL_PRODUCT_READ'
          EXPORTING
            is_key  = is_matnr
          IMPORTING
            es_data = rs_matid.
      CATCH /scmb/cx_mdl ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD read_product_from_matid.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :  read material attributes that should be displayed on the subscreen
    "********************************************************************
    TRY.
        CALL FUNCTION '/SCMB/MDL_PRODUCT_READ'
          EXPORTING
            iv_id   = is_matid-matid
          IMPORTING
            es_data = rs_product.
      CATCH /scmb/cx_mdl ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD select_t340d.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "********************************************************************
    SELECT SINGLE * FROM /scwm/t340d
      INTO rs_t340d
      WHERE lgnum = iv_lgnum.
    IF sy-subrc <> 0.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid   = zif_c_mdm_tool=>c_not_defined_default_value
                                        mv_msgv1 = CONV #( iv_lgnum ) ).
    ENDIF.
  ENDMETHOD.


  METHOD update_units_of_measure.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :  Update Unit of measure date for given material
    "********************************************************************
    DATA ls_clientdata  TYPE bapi_mara.
    DATA ls_clientdatax TYPE bapi_marax.
    DATA ls_return      TYPE bapiret2.

    SELECT SINGLE * FROM mara                 "#EC CI_ALL_FIELDS_NEEDED
      INTO @DATA(ls_mara)
      WHERE matnr = @mv_matnr.

    fill_str( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_mara
                        iv_file    = ls_mara
              IMPORTING ev_str     = ls_clientdata ).

    ASSIGN COMPONENT zif_c_mdm_tool=>c_fieldnames-prdha OF STRUCTURE ls_mara TO FIELD-SYMBOL(<ls_prdha>).
    IF <ls_prdha> IS ASSIGNED.
      ls_clientdata-prod_hier  = <ls_prdha>.
      ls_clientdatax-prod_hier = abap_true.
    ENDIF.
    fill_xstr( EXPORTING iv_tabname = zif_c_mdm_tool=>c_tabnames-bapi_mara
                         iv_str     = ls_clientdata
               IMPORTING ev_xstr    = ls_clientdatax ).

    DATA(ls_headdata) = VALUE bapimathead(
        material_long = COND #( WHEN ls_mara-matnr IS NOT INITIAL THEN ls_mara-matnr ELSE ls_mara-matnr )
        ind_sector    = ls_mara-mbrsh
        matl_type     = ls_mara-mtart
        basic_view    = abap_true
        storage_view  = abap_true ).

    CALL FUNCTION 'BAPI_MATERIAL_SAVEDATA'
      EXPORTING
        headdata    = ls_headdata
        clientdata  = ls_clientdata
        clientdatax = ls_clientdatax
      IMPORTING
        return      = ls_return.
    IF ls_return-type CA 'AEX'.
      bapi_transaction_rollback( ).
    ELSE.
      bapi_transaction_commit( ).
    ENDIF.
  ENDMETHOD.


  METHOD update_weight_volume_first.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Update Unit of Measures
    "********************************************************************
    co_packspec_model->calculate_weight_volume( EXCEPTIONS OTHERS = 2 ).
    IF sy-subrc <> 0.
      raise_exception_from_sy( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
