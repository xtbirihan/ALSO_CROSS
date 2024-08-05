CLASS zcl_mdm_prod_change DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    DATA mo_model TYPE REF TO zcl_mdm_model.

    TYPES:
      BEGIN OF t_scu_ids,
        scuguid TYPE guid_16,
        entity  TYPE /scmb/oe_entity,
      END OF t_scu_ids.
    TYPES:
      BEGIN OF ty_matid_by_ent,
        matid TYPE /sapapo/matid,
        entit TYPE /scwm/tt_entitled_2,
      END OF ty_matid_by_ent.
    TYPES tt_matid_by_ent TYPE SORTED TABLE OF ty_matid_by_ent WITH UNIQUE KEY matid.
    TYPES:
      BEGIN OF ty_loc_vrsioid_str,
        vrsioid TYPE char22,
      END OF ty_loc_vrsioid_str.
    TYPES tt_where TYPE STANDARD TABLE OF string.
    TYPES:
      BEGIN OF ty_matid_entit,
        matid    TYPE /sapapo/matid,
        entitled TYPE /scwm/de_entitled,
      END OF ty_matid_entit.
    TYPES tt_matid_entit TYPE SORTED TABLE OF ty_matid_entit WITH NON-UNIQUE KEY matid entitled.
    TYPES:
      BEGIN OF ty_matid_entitid,
        matid       TYPE /sapapo/matid,
        entitled_id TYPE /scwm/de_entitled_id,
        meins       TYPE /sapapo/meins,
      END OF ty_matid_entitid.
    TYPES tt_matid_entitid TYPE SORTED TABLE OF ty_matid_entitid WITH NON-UNIQUE KEY matid entitled_id.
    TYPES:
      BEGIN OF ty_matid_meins,
        matid TYPE /sapapo/matid,
        meins TYPE /sapapo/meins,
      END OF ty_matid_meins.
    TYPES tt_matid_meins TYPE SORTED TABLE OF ty_matid_meins WITH NON-UNIQUE KEY matid.
    TYPES ty_matid       TYPE /sapapo/matid_str.
    TYPES tt_matid       TYPE STANDARD TABLE OF /sapapo/matid_str.
    TYPES:
      " section 1
      BEGIN OF ty_matid_range,            " mater id
        sign   TYPE /scmb/sign,
        option TYPE /scmb/option,
        low    TYPE /sapapo/matid,
        high   TYPE /sapapo/matid,
      END OF ty_matid_range.
    TYPES:
      BEGIN OF ty_matnr_range,          " product number
        sign   TYPE /scmb/sign,
        option TYPE /scmb/option,
        low    TYPE /sapapo/matnr,
        high   TYPE /sapapo/matnr,
      END OF ty_matnr_range.
    TYPES:
      BEGIN OF ty_entit_range,            " entitle to party
        sign   TYPE /scmb/sign,
        option TYPE /scmb/option,
        low    TYPE /scwm/de_entitled,
        high   TYPE /scwm/de_entitled,
      END OF ty_entit_range.

    TYPES tt_matid_range   TYPE STANDARD TABLE OF ty_matid_range.
    TYPES tt_matnr_range   TYPE STANDARD TABLE OF ty_matnr_range.
    TYPES tt_entit_range   TYPE STANDARD TABLE OF ty_entit_range.
    TYPES tt_ext_matkeyx   TYPE STANDARD TABLE OF /sapapo/ext_matkeyx.
    TYPES tt_ext_matlwh    TYPE STANDARD TABLE OF /sapapo/ext_matlwh.
    TYPES tt_ext_matlwhx   TYPE STANDARD TABLE OF /sapapo/ext_matlwhx.
    TYPES tt_ext_matlwhst  TYPE STANDARD TABLE OF /sapapo/ext_matlwhst.
    TYPES tt_ext_matlwhstx TYPE STANDARD TABLE OF /sapapo/ext_matlwhstx.
    TYPES tt_ext_matkey    TYPE STANDARD TABLE OF /sapapo/ext_matkey.

    CONSTANTS gc_auth_matkey TYPE c LENGTH 3 VALUE 'MKY' ##NO_TEXT.

    DATA mo_lock_mass_pr TYPE REF TO /scmb/if_md_lock_mass_maint.

    CONSTANTS sc_show_only_wh             TYPE c LENGTH 1 VALUE 'W' ##NO_TEXT.
    CONSTANTS sc_only_products_without_wh TYPE c LENGTH 1 VALUE 'N' ##NO_TEXT.
    CONSTANTS sc_every_product            TYPE c LENGTH 1 VALUE 'E' ##NO_TEXT.

    METHODS constructor
      IMPORTING io_model TYPE REF TO zcl_mdm_model.

    METHODS execute
      IMPORTING iv_daily_volume_update TYPE abap_boolean OPTIONAL.

    METHODS wh_prod_mass_change
      IMPORTING it_matkey          TYPE tt_ext_matkey
                it_matkeyx         TYPE tt_ext_matkeyx
                it_matlwh_create   TYPE tt_ext_matlwh    OPTIONAL
                it_matlwh_update   TYPE tt_ext_matlwh
                it_matlwhx         TYPE tt_ext_matlwhx
                it_matlwhst_create TYPE tt_ext_matlwhst  OPTIONAL
                it_matlwhst_update TYPE tt_ext_matlwhst  OPTIONAL
                it_matlwhstx       TYPE tt_ext_matlwhstx OPTIONAL
      EXPORTING et_return          TYPE bapiret2_t
                ev_abort           TYPE xfeld
                ev_lines           TYPE int8
                ev_lines_st        TYPE int8.

    METHODS get_prod_node_data
      IMPORTING iv_uom_include TYPE abap_bool DEFAULT abap_false
      EXPORTING et_data        TYPE /scwm/tt_prod_mon_out
                et_prod_uom    TYPE /scwm/tt_prod_uom_mon_out
                et_marm        TYPE /sapapo/marm_tab
                et_material    TYPE /sapapo/matkey_out_tab.

    METHODS get_styp_node_data
      IMPORTING iv_lgnum                TYPE /scwm/lgnum
                iv_lgtyp                TYPE /scwm/lgtyp
                it_matnr_range          TYPE tt_matnr_range
                it_entit_range          TYPE tt_entit_range        OPTIONAL
                it_selected_wh_products TYPE /scwm/tt_prod_mon_out OPTIONAL
                iv_only_with_whst       TYPE abap_bool             OPTIONAL
                iv_only_without_whst    TYPE abap_bool             OPTIONAL
                iv_both                 TYPE abap_bool             OPTIONAL
      EXPORTING et_data                 TYPE /scwm/tt_prod_strg_mon_out.

    METHODS get_uom_node_data
      IMPORTING iv_lgnum                TYPE /scwm/lgnum
                it_matid_range          TYPE tt_matid_range        OPTIONAL
                it_matnr_range          TYPE tt_matnr_range        OPTIONAL
                it_entit_range          TYPE tt_entit_range        OPTIONAL
                it_selected_wh_products TYPE /scwm/tt_prod_mon_out OPTIONAL
      EXPORTING et_data                 TYPE /scwm/tt_prod_uom_mon_out.

    METHODS get_scuguid
      IMPORTING iv_lgnum   TYPE /scwm/lgnum
      EXPORTING ev_scuguid TYPE /scmb/mdl_scuguid.

    METHODS quan_convert
      IMPORTING iv_matid TYPE /scmb/mdl_matid_ce
                iv_s_qty TYPE /scwm/de_quan_dsp
                iv_s_uom TYPE meins
                iv_d_uom TYPE meins
      CHANGING  cv_d_qty TYPE /scwm/de_quan_dsp.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA mt_mat_nr_id_guid TYPE tt_prod.
    DATA mo_service        TYPE REF TO /scwm/if_tm_global_info.
    DATA mt_entitled       TYPE bup_partner_guid_t.
    DATA mv_scuguid        TYPE /scmb/mdl_scuguid.
    DATA mo_isolated_doc   TYPE REF TO lif_isolated_doc.

    METHODS integrate_sapapo_data
      IMPORTING it_sapapo_mattxt  TYPE /sapapo/mattxt_tab
                it_sapapo_matpack TYPE /sapapo/matpack_tab
                it_sapapo_matexec TYPE /sapapo/matexec_tab
                iv_lgnum          TYPE /scwm/lgnum
                iv_matid          TYPE /sapapo/matid
      CHANGING  cs_product        TYPE /scwm/s_prod_mon_out.

    METHODS create_list_of_wh_products
      IMPORTING iv_lgnum          TYPE /scwm/lgnum
                it_material       TYPE /sapapo/matkey_out_tab
                it_sapapo_mattxt  TYPE /sapapo/mattxt_tab
                it_sapapo_matpack TYPE /sapapo/matpack_tab
                it_sapapo_matexec TYPE /sapapo/matexec_tab
                iv_selection_mode TYPE c
                it_wh_product     TYPE tt_lgnum_matlwh
      EXPORTING et_data           TYPE /scwm/tt_prod_mon_out.

    METHODS fill_field_mtart_and_mmsta
      IMPORTING
                it_matnr_range TYPE /scwm/if_af_material=>tt_matnr_range
                it_matid_range TYPE /scwm/if_af_material=>tt_m22id_range
      CHANGING  ct_data        TYPE /scwm/tt_prod_mon_out.
ENDCLASS.



CLASS ZCL_MDM_PROD_CHANGE IMPLEMENTATION.


  METHOD constructor.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Consturctor
    "*&
    "*&
    "********************************************************************
    mo_isolated_doc = lcl_isolated_doc=>get_instance( ).
    mo_model = io_model.
    TRY.
        /sapapo/cl_lock_pr=>get_instance_mass_maint( EXPORTING iv_appid    = 'PRODLWH'
                                                     IMPORTING eo_instance = mo_lock_mass_pr ).
      CATCH /scmb/cx_md_lock_system_error ##NO_HANDLER.
    ENDTRY.

    IF mo_service IS INITIAL.
      TRY.
          mo_service ?= /scwm/cl_tm_factory=>get_service( /scwm/cl_tm_factory=>sc_globals ).
        CATCH /scwm/cx_tm_factory.
          ASSERT 1 = 0.
      ENDTRY.
    ENDIF.
  ENDMETHOD.


  METHOD create_list_of_wh_products.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Create warehouse product list
    "*&
    "*&
    "********************************************************************
    DATA ls_wh_product     TYPE ty_lgnum_matlwh.
    DATA ls_product        TYPE /scwm/s_prod_mon_out.
    DATA ls_entitled       TYPE bupa_partner_guid.
    DATA lt_matnr_range    TYPE /scwm/if_af_material=>tt_matnr_range.
    DATA ls_matnr_range    TYPE /scwm/if_af_material=>ty_matnr_range.
    DATA lt_matid_range    TYPE /scwm/if_af_material=>tt_m22id_range.
    DATA ls_matid_range    TYPE /scwm/if_af_material=>ty_m22id_range.
    DATA ls_mat_nr_id_guid TYPE ty_prod.
    DATA lv_meins          TYPE /sapapo/meins.

    FIELD-SYMBOLS <ls_material> TYPE /sapapo/matkey_out.

    SORT mt_mat_nr_id_guid BY matnr.
    LOOP AT it_material ASSIGNING <ls_material>.
      LOOP AT mt_entitled INTO ls_entitled.
        " Do for every entitled in the lgnum
        CLEAR ls_product.
        MOVE-CORRESPONDING <ls_material> TO ls_product ##ENH_OK.
        lv_meins = ls_product-meins.
        ls_product-entitled    = ls_entitled-partner.
        ls_product-entitled_id = ls_entitled-partner_guid.
        ls_product-scuguid     = mv_scuguid.

        " map matnr to matguid
        READ TABLE mt_mat_nr_id_guid INTO ls_mat_nr_id_guid WITH KEY matnr = <ls_material>-matnr BINARY SEARCH.

        " does the warehouse product exist on the DB
        READ TABLE it_wh_product INTO ls_wh_product WITH KEY matid    = ls_mat_nr_id_guid-matguid
                                                             lgnum    = mo_model->ms_top_block-sub_section_1-lgnum
                                                             entitled = ls_entitled-partner BINARY SEARCH.

        IF sy-subrc = 0.
          ls_product-wh_data_exists = TEXT-001.
          IF iv_selection_mode = sc_only_products_without_wh.
            CONTINUE.
          ENDIF.

          MOVE-CORRESPONDING ls_wh_product TO ls_product ##ENH_OK.
          ls_product-meins = lv_meins.
          ls_product-matid = <ls_material>-matid.
        ELSE.
          ls_product-wh_data_exists = TEXT-002.
          IF iv_selection_mode = sc_show_only_wh.
            CONTINUE.
          ENDIF.

          ls_product-no_wh_data = abap_true.
        ENDIF.

        integrate_sapapo_data( EXPORTING it_sapapo_matpack = it_sapapo_matpack
                                         it_sapapo_matexec = it_sapapo_matexec
                                         it_sapapo_mattxt  = it_sapapo_mattxt
                                         iv_lgnum          = iv_lgnum
                                         iv_matid          = ls_mat_nr_id_guid-matid
                               CHANGING  cs_product        = ls_product ).

        ls_matnr_range-sign   = 'I'.
        ls_matnr_range-option = 'EQ'.
        ls_matnr_range-low    = ls_product-matnr.
        APPEND ls_matnr_range TO lt_matnr_range.

        ls_matid_range-sign   = 'I'.
        ls_matid_range-option = 'EQ'.
        ls_matid_range-low    = ls_product-matid.
        APPEND ls_matid_range TO lt_matid_range.

        APPEND ls_product TO et_data.
      ENDLOOP. " at entitled
    ENDLOOP. " at materials

    SORT et_data BY matid
                    scuguid
                    entitled_id.

    fill_field_mtart_and_mmsta( EXPORTING it_matnr_range = lt_matnr_range
                                          it_matid_range = lt_matid_range
                                CHANGING  ct_data        = et_data ).
  ENDMETHOD.


  METHOD execute.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : The method will update product fields
    "*& if daily volume update has set it will update daily volume
    "*& otherwise it will update other fields regardin changin of these fields
    "********************************************************************
    DATA lt_material        TYPE TABLE OF /sapapo/ext_matkey.
    DATA ls_matkey          TYPE /sapapo/ext_matkey.
    DATA lt_matkey          TYPE TABLE OF /sapapo/ext_matkey.
    DATA ls_matkeyx         TYPE /sapapo/ext_matkeyx.
    DATA lt_matkeyx         TYPE TABLE OF /sapapo/ext_matkeyx.
    DATA ls_matlwhx         TYPE /sapapo/ext_matlwhx.
    DATA lt_matlwhx         TYPE TABLE OF /sapapo/ext_matlwhx.
    DATA ls_matlwh          TYPE /sapapo/ext_matlwh.
    DATA lt_matlwh_create   TYPE TABLE OF /sapapo/ext_matlwh.
    DATA lt_matlwh_update   TYPE TABLE OF /sapapo/ext_matlwh.
    DATA lr_scu_mgr         TYPE REF TO /scmb/if_scu_mgr.
    DATA lt_mapping         TYPE /scmb/scu_mapping_out_tab.
    DATA ls_mapping         TYPE /scmb/scu_mapping_out_str.
    DATA lt_matnr_range     TYPE tt_matnr_range.
    DATA ls_matlwhst        TYPE /sapapo/ext_matlwhst.
    DATA lt_matlwhst_create TYPE TABLE OF /sapapo/ext_matlwhst.
    DATA lt_matlwhst_update TYPE TABLE OF /sapapo/ext_matlwhst.
    DATA ls_matlwhstx       TYPE /sapapo/ext_matlwhstx.
    DATA lt_matlwhstx       TYPE TABLE OF /sapapo/ext_matlwhstx.

    get_prod_node_data( EXPORTING iv_uom_include = abap_false
                        IMPORTING et_data        = DATA(lt_data)
                                  et_prod_uom    = DATA(lt_prod_uom) ##NEEDED
                                  et_marm        = DATA(lt_marm)
                                  et_material    = DATA(lt_mat) ).
    IF lt_data IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_daily_volume_update IS INITIAL.
      get_styp_node_data( EXPORTING iv_lgnum                = mo_model->ms_top_block-sub_section_1-lgnum
                                    iv_lgtyp                = VALUE #( )
                                    it_matnr_range          = lt_matnr_range
                                    it_selected_wh_products = lt_data " Product Data
                                    iv_only_with_whst       = abap_true
                          IMPORTING et_data                 = DATA(lt_data_styp) ).
    ENDIF.

    DATA(ls_data) = VALUE #( lt_data[ 1 ] OPTIONAL ).

    IF iv_daily_volume_update IS INITIAL.
      IF     ls_data-zz1_keepcar_whd           = mo_model->ms_master_carton_aditional-zz1_keepcar_whd
         AND ls_data-zz1_disp_whd              = mo_model->ms_master_carton_aditional-zz1_disp_whd
         AND ls_data-demqty                    = mo_model->ms_lower_block-s_warehouse_data-demqty
         AND ls_data-ccind = mo_model->ms_lower_block-s_warehouse_data-ccind
         AND ls_data-volind                    = mo_model->ms_lower_block-s_warehouse_data-volind
         AND ls_data-put_stra                  = mo_model->ms_lower_block-s_warehouse_data-put_stra
         AND ls_data-zz1_nospo_whd             = mo_model->ms_lower_block-s_warehouse_data-zz1_nospo_whd
         AND ls_data-zz1_noslo_whd             = mo_model->ms_lower_block-s_warehouse_data-zz1_noslo_whd
         AND ls_data-zz1_twomh_whd             = mo_model->ms_lower_block-s_warehouse_data-zz1_twomh_whd
         AND ls_data-concstat                  = mo_model->ms_lower_block-s_warehouse_data-concstat
         AND ls_data-zz1_fragile_whd           = mo_model->ms_lower_block-s_warehouse_data-zz1_fragile_whd
         AND ls_data-sled_bbd                  = mo_model->ms_top_block-sub_section_1-sled_bbd
         AND ls_data-block = mo_model->ms_top_block-sub_section_1-block
         AND ls_data-zz1_nonconveyable_whd     = mo_model->ms_lower_block-s_warehouse_data-zz1_nonconveyable_whd
         AND ls_data-zz1_odd_whd               = mo_model->ms_lower_block-s_warehouse_data-zz1_odd_whd
         AND ls_data-zz1_inb_deco_text_whd     = mo_model->ms_lower_block-s_material_texts-zz1_inb_deco_text_whd
         AND ls_data-zz1_picking_text_whd      = mo_model->ms_lower_block-s_material_texts-zz1_picking_text_whd
         AND ls_data-zz1_kep_picking_text_whd  = mo_model->ms_lower_block-s_material_texts-zz1_kep_picking_text_whd
         AND ls_data-zz1_sped_picking_text_whd = mo_model->ms_lower_block-s_material_texts-zz1_sped_picking_text_whd
         AND ls_data-zz1_packing_text_whd      = mo_model->ms_lower_block-s_material_texts-zz1_packing_text_whd
         AND ls_data-zz1_vascustomer_whd       = mo_model->ms_lower_block-s_material_texts-zz1_vascustomer_whd
         AND ls_data-zz1_vastext_whd           = mo_model->ms_lower_block-s_material_texts-zz1_vastext_whd.
*        RETURN.
        DATA(lv_no_change_whs) = abap_true.
      ENDIF.
    ELSE.
      IF ls_data-demqty = mo_model->ms_lower_block-s_warehouse_data-demqty.
        RETURN.
      ENDIF.
    ENDIF.

    DATA(ls_t300_md) = mo_model->read_location( ).

    lr_scu_mgr = /scmb/cl_scu_system=>so_scu_mgr.
    IF lr_scu_mgr IS BOUND.
      DATA(lt_entity) = lr_scu_mgr->get_scus_by_scuguid( it_scuguid = VALUE #( ( ls_t300_md-scuguid ) ) ).
    ENDIF.

    DATA(lo_scu) = VALUE #( lt_entity[ 1 ] OPTIONAL ).
    IF sy-subrc = 0.
      lt_mapping = lo_scu->get_mapping( ).
      READ TABLE lt_mapping WITH KEY scuguid = ls_t300_md-scuguid
           INTO ls_mapping.
      IF sy-subrc IS INITIAL.
        DATA(lv_entity) = ls_mapping-ext_entity.
      ENDIF.
    ENDIF.

    " ---------------------------------
    " build tables matkey and matkeyx
    " --------------------------------
    MOVE-CORRESPONDING lt_data TO lt_material ##ENH_OK.
    LOOP AT lt_material ASSIGNING FIELD-SYMBOL(<ls_material>).
      MOVE-CORRESPONDING <ls_material> TO ls_matkey.
      ls_matkey-method = ''.
      IF ls_matkey-sled_bbd <> mo_model->ms_top_block-sub_section_1-sled_bbd.
        ls_matkey-sled_bbd  = mo_model->ms_top_block-sub_section_1-sled_bbd.
        ls_matkeyx-sled_bbd = abap_true.
      ENDIF.
      APPEND ls_matkey TO lt_matkey.
      ls_matkeyx-ext_matnr = <ls_material>-ext_matnr.
      APPEND ls_matkeyx TO lt_matkeyx.
    ENDLOOP.
    IF lt_matkey IS NOT INITIAL.
      SORT lt_matkey BY ext_matnr.
      DELETE ADJACENT DUPLICATES FROM lt_matkey COMPARING ext_matnr.
    ENDIF.
    IF lt_matkeyx IS NOT INITIAL.
      SORT lt_matkeyx BY ext_matnr.
      DELETE ADJACENT DUPLICATES FROM lt_matkeyx COMPARING ext_matnr.
    ENDIF.

    LOOP AT lt_data INTO ls_data.
      CLEAR ls_matlwh.
      CLEAR ls_matlwhx.

      ls_data-scuguid = ls_t300_md-scuguid.
      MOVE-CORRESPONDING ls_data TO ls_matlwh ##ENH_OK.
      ls_matlwh-ext_entitled = ls_data-entitled.
      ls_matlwh-ext_entity   = lv_entity.

      "BEGIN MOD <AAHMEDOV>-20240123
      ls_matlwh-demtyp = '01'.
      ls_matlwhx-demtyp = abap_true.
      "BEGIN MOD <AAHMEDOV>-20240123

      ls_matlwhx-ext_entitled = ls_data-entitled.
      ls_matlwhx-ext_entity   = lv_entity.
      ls_matlwhx-ext_matnr    = ls_data-ext_matnr.

      IF iv_daily_volume_update IS INITIAL.
        IF ls_data-zz1_keepcar_whd <> mo_model->ms_master_carton_aditional-zz1_keepcar_whd.
          ls_matlwh-zz1_keepcar_whd = mo_model->ms_master_carton_aditional-zz1_keepcar_whd.
          ls_matlwhx-zz1_keepcar_whd = abap_true.
        ENDIF.
        IF ls_data-zz1_disp_whd <> mo_model->ms_master_carton_aditional-zz1_disp_whd.
          ls_matlwh-zz1_disp_whd = mo_model->ms_master_carton_aditional-zz1_disp_whd.
          ls_matlwhx-zz1_disp_whd = abap_true.
        ENDIF.

        IF ls_data-demqty <> mo_model->ms_lower_block-s_warehouse_data-demqty.
          ls_matlwh-demqty =  mo_model->ms_lower_block-s_warehouse_data-demqty.
          ls_matlwhx-demqty = abap_true.
        ENDIF.

        IF ls_data-ccind <> mo_model->ms_lower_block-s_warehouse_data-ccind.
          ls_matlwh-ccind  = mo_model->ms_lower_block-s_warehouse_data-ccind.
          ls_matlwhx-ccind = abap_true.
        ENDIF.
        IF ls_data-volind <> mo_model->ms_lower_block-s_warehouse_data-volind.
          ls_matlwh-volind = mo_model->ms_lower_block-s_warehouse_data-volind.
          ls_matlwhx-volind = abap_true.
        ENDIF.
        IF ls_data-put_stra <> mo_model->ms_lower_block-s_warehouse_data-put_stra.
          ls_matlwh-put_stra = mo_model->ms_lower_block-s_warehouse_data-put_stra.
          ls_matlwhx-put_stra = abap_true.
        ENDIF.
        IF ls_data-zz1_nospo_whd <> mo_model->ms_lower_block-s_warehouse_data-zz1_nospo_whd.
          ls_matlwh-zz1_nospo_whd = mo_model->ms_lower_block-s_warehouse_data-zz1_nospo_whd.
          ls_matlwhx-zz1_nospo_whd = abap_true.
        ENDIF.
        IF ls_data-zz1_noslo_whd <> mo_model->ms_lower_block-s_warehouse_data-zz1_noslo_whd.
          ls_matlwh-zz1_noslo_whd = mo_model->ms_lower_block-s_warehouse_data-zz1_noslo_whd.
          ls_matlwhx-zz1_noslo_whd = abap_true.
        ENDIF.
        IF ls_data-zz1_twomh_whd <> mo_model->ms_lower_block-s_warehouse_data-zz1_twomh_whd.
          ls_matlwh-zz1_twomh_whd = mo_model->ms_lower_block-s_warehouse_data-zz1_twomh_whd.
          ls_matlwhx-zz1_twomh_whd = abap_true.
        ENDIF.
        IF ls_data-concstat <> mo_model->ms_lower_block-s_warehouse_data-concstat.
          ls_matlwh-concstat = mo_model->ms_lower_block-s_warehouse_data-concstat.
          ls_matlwhx-concstat = abap_true.
        ENDIF.

        IF ls_data-zz1_fragile_whd <> mo_model->ms_lower_block-s_warehouse_data-zz1_fragile_whd.
          ls_matlwh-zz1_fragile_whd = mo_model->ms_lower_block-s_warehouse_data-zz1_fragile_whd.
          ls_matlwhx-zz1_fragile_whd = abap_true.
        ENDIF.
        IF ls_data-zz1_nonconveyable_whd <> mo_model->ms_lower_block-s_warehouse_data-zz1_nonconveyable_whd.
          ls_matlwh-zz1_nonconveyable_whd = mo_model->ms_lower_block-s_warehouse_data-zz1_nonconveyable_whd.
          ls_matlwhx-zz1_nonconveyable_whd = abap_true.
        ENDIF.

        IF ls_data-zz1_odd_whd <> mo_model->ms_lower_block-s_warehouse_data-zz1_odd_whd.
          ls_matlwh-zz1_odd_whd = mo_model->ms_lower_block-s_warehouse_data-zz1_odd_whd.
          ls_matlwhx-zz1_odd_whd = abap_true.
        ENDIF.

        IF ls_data-zz1_inb_deco_text_whd <> mo_model->ms_lower_block-s_material_texts-zz1_inb_deco_text_whd.
          ls_matlwh-zz1_inb_deco_text_whd  = mo_model->ms_lower_block-s_material_texts-zz1_inb_deco_text_whd.
          ls_matlwhx-zz1_inb_deco_text_whd = abap_true.
        ENDIF.

        IF ls_data-zz1_picking_text_whd <> mo_model->ms_lower_block-s_material_texts-zz1_picking_text_whd.
          ls_matlwh-zz1_picking_text_whd = mo_model->ms_lower_block-s_material_texts-zz1_picking_text_whd.
          ls_matlwhx-zz1_picking_text_whd = abap_true.
        ENDIF.

        IF ls_data-zz1_kep_picking_text_whd <> mo_model->ms_lower_block-s_material_texts-zz1_kep_picking_text_whd.
          ls_matlwh-zz1_kep_picking_text_whd = mo_model->ms_lower_block-s_material_texts-zz1_kep_picking_text_whd.
          ls_matlwhx-zz1_kep_picking_text_whd = abap_true.
        ENDIF.

        IF ls_data-zz1_sped_picking_text_whd <> mo_model->ms_lower_block-s_material_texts-zz1_sped_picking_text_whd.
          ls_matlwh-zz1_sped_picking_text_whd  = mo_model->ms_lower_block-s_material_texts-zz1_sped_picking_text_whd.
          ls_matlwhx-zz1_sped_picking_text_whd = abap_true.
        ENDIF.

        IF ls_data-zz1_packing_text_whd <> mo_model->ms_lower_block-s_material_texts-zz1_packing_text_whd.
          ls_matlwh-zz1_packing_text_whd  = mo_model->ms_lower_block-s_material_texts-zz1_packing_text_whd.
          ls_matlwhx-zz1_packing_text_whd = abap_true.
        ENDIF.

        IF ls_data-zz1_vascustomer_whd <> mo_model->ms_lower_block-s_material_texts-zz1_vascustomer_whd.
          ls_matlwh-zz1_vascustomer_whd  = mo_model->ms_lower_block-s_material_texts-zz1_vascustomer_whd.
          ls_matlwhx-zz1_vascustomer_whd = abap_true.
        ENDIF.

        IF ls_data-zz1_vastext_whd <> mo_model->ms_lower_block-s_material_texts-zz1_vastext_whd.
          ls_matlwh-zz1_vastext_whd  = mo_model->ms_lower_block-s_material_texts-zz1_vastext_whd.
          ls_matlwhx-zz1_vastext_whd = abap_true.
        ENDIF.

        IF ls_data-block <> mo_model->ms_top_block-sub_section_1-block.
          ls_matlwh-block = mo_model->ms_top_block-sub_section_1-block.
          ls_matlwhx-block = abap_true.
        ENDIF.
      ENDIF.

      IF iv_daily_volume_update IS NOT INITIAL.
        IF ls_data-demqty <> mo_model->ms_lower_block-s_warehouse_data-demqty.
          ls_matlwh-demqty =  mo_model->ms_lower_block-s_warehouse_data-demqty.
          ls_matlwhx-demqty = abap_true.
        ENDIF.
      ENDIF.

      APPEND ls_matlwh  TO lt_matlwh_update.
      APPEND ls_matlwhx TO lt_matlwhx.
      IF iv_daily_volume_update IS INITIAL.
        DATA(lv_update_wh) = abap_true.
      ENDIF.

    ENDLOOP.

    SORT lt_matkey BY ext_matnr.
    SORT lt_matlwh_create BY ext_matnr.
    SORT lt_matlwh_update BY ext_matnr.
    SORT lt_matlwhx BY ext_matnr
                       ext_entitled
                       ext_entity.

    DELETE ADJACENT DUPLICATES FROM lt_matkey.
    DELETE ADJACENT DUPLICATES FROM lt_matlwh_create.
    DELETE ADJACENT DUPLICATES FROM lt_matlwh_update.
    DELETE ADJACENT DUPLICATES FROM lt_matlwhx.

    " ------------------------------------------------------------
    " prepare tables of storage type data to create and to update
    " ------------------------------------------------------------
    IF iv_daily_volume_update IS INITIAL.
      CLEAR: ls_matlwhst,
             ls_matlwhstx.

      READ TABLE lt_data_styp INTO DATA(ls_data_styp) WITH KEY lgtyp = mo_model->ms_lower_block-s_storage_type_data-lgtyp.
      IF sy-subrc = 0. " Existing storage type change
        IF     ls_data_styp-lgtyp          = mo_model->ms_lower_block-s_storage_type_data-lgtyp
           AND ls_data_styp-sectind        = mo_model->ms_lower_block-s_storage_type_data-sectind
           AND ls_data_styp-bintype        = mo_model->ms_lower_block-s_storage_type_data-bintype
           AND ls_data_styp-repqty         = mo_model->ms_lower_block-s_storage_type_data-repqty_dsp
           AND ls_data_styp-minqty         = mo_model->ms_lower_block-s_storage_type_data-minqty_dsp
           AND ls_data_styp-maxqty         = mo_model->ms_lower_block-s_storage_type_data-maxqty_dsp
           AND ls_data_styp-quanclaput     = mo_model->ms_lower_block-s_storage_type_data-quanclaput
           AND ls_data_styp-zz1_dirrpl_stt = mo_model->ms_lower_block-s_storage_type_data-zz1_dirrpl_stt
           AND ls_data_styp-zz1_maxput_stt = mo_model->ms_lower_block-s_storage_type_data-zz1_maxput_stt.
          DATA(lv_no_whst_change) = abap_true.
        ENDIF.

        IF lv_no_whst_change IS INITIAL.

          MOVE-CORRESPONDING ls_data_styp TO ls_matlwhst ##ENH_OK.
          ls_matlwhst-ext_matnr    = ls_data_styp-matnr.
          ls_matlwhst-ext_entitled = ls_data_styp-entitled.
          ls_matlwhst-ext_entity   = lv_entity.

          ls_matlwhstx-ext_matnr    = ls_data_styp-matnr.
          ls_matlwhstx-ext_entitled = ls_data_styp-entitled.
          ls_matlwhstx-ext_entity   = lv_entity.
          ls_matlwhstx-lgtyp        = ls_data_styp-lgtyp.

          IF ls_data_styp-sectind <> mo_model->ms_lower_block-s_storage_type_data-sectind.
            ls_matlwhst-sectind = mo_model->ms_lower_block-s_storage_type_data-sectind.
            ls_matlwhstx-sectind = abap_true.
          ENDIF.

          IF ls_data_styp-bintype <> mo_model->ms_lower_block-s_storage_type_data-bintype.
            ls_matlwhst-bintype = mo_model->ms_lower_block-s_storage_type_data-bintype.
            ls_matlwhstx-bintype = abap_true.
          ENDIF.

          IF ls_data_styp-repqty <> mo_model->ms_lower_block-s_storage_type_data-repqty_dsp.
            ls_matlwhst-repqty = mo_model->ms_lower_block-s_storage_type_data-repqty_dsp.
            ls_matlwhstx-repqty = abap_true.
            ls_matlwhst-repqty_uom_dsp = 'ST'.
            ls_matlwhstx-repqty_uom_dsp = abap_true.
          ENDIF.

          IF ls_data_styp-minqty <> mo_model->ms_lower_block-s_storage_type_data-minqty_dsp.
            ls_matlwhst-minqty = mo_model->ms_lower_block-s_storage_type_data-minqty_dsp.
            ls_matlwhstx-minqty = abap_true.
            ls_matlwhst-minqty_uom_dsp = 'ST'.
            ls_matlwhstx-minqty_uom_dsp = abap_true.
          ENDIF.

          IF ls_data_styp-maxqty <> mo_model->ms_lower_block-s_storage_type_data-maxqty_dsp.
            ls_matlwhst-maxqty = mo_model->ms_lower_block-s_storage_type_data-maxqty_dsp.
            ls_matlwhstx-maxqty = abap_true.
            ls_matlwhst-maxqty_uom_dsp = 'ST'.
            ls_matlwhstx-maxqty_uom_dsp = abap_true.
          ENDIF.

          IF ls_data_styp-quanclaput <> mo_model->ms_lower_block-s_storage_type_data-quanclaput.
            ls_matlwhst-quanclaput = mo_model->ms_lower_block-s_storage_type_data-quanclaput.
            ls_matlwhstx-quanclaput = abap_true.
          ENDIF.

          IF ls_data_styp-zz1_dirrpl_stt <> mo_model->ms_lower_block-s_storage_type_data-zz1_dirrpl_stt.
            ls_matlwhst-zz1_dirrpl_stt = mo_model->ms_lower_block-s_storage_type_data-zz1_dirrpl_stt.
            ls_matlwhstx-zz1_dirrpl_stt = abap_true.
          ENDIF.

          IF ls_data_styp-zz1_maxput_stt <> mo_model->ms_lower_block-s_storage_type_data-zz1_maxput_stt.
            ls_matlwhst-zz1_maxput_stt = mo_model->ms_lower_block-s_storage_type_data-zz1_maxput_stt.
            ls_matlwhstx-zz1_maxput_stt = abap_true.
          ENDIF.

          APPEND ls_matlwhst  TO lt_matlwhst_update.
          APPEND ls_matlwhstx TO lt_matlwhstx.
          DATA(lv_update_whst) = abap_true.
        ENDIF.
      ELSE. " New storage type create.

        ls_matlwhst-ext_matnr    = mo_model->master_data-mara-matnr.
        ls_matlwhst-ext_entitled = mo_model->ms_top_block-sub_section_1-entitled.
        ls_matlwhst-ext_entity   = lv_entity.
        ls_matlwhst-lgtyp        = mo_model->ms_lower_block-s_storage_type_data-lgtyp.
        " --
        " --
        ls_matlwhstx-ext_matnr    = mo_model->master_data-mara-matnr.
        ls_matlwhstx-ext_entitled = mo_model->ms_top_block-sub_section_1-entitled.
        ls_matlwhstx-ext_entity   = lv_entity.
        ls_matlwhstx-lgtyp        = mo_model->ms_lower_block-s_storage_type_data-lgtyp.

        ls_matlwhst-sectind = mo_model->ms_lower_block-s_storage_type_data-sectind.
        ls_matlwhstx-sectind = abap_true.

        ls_matlwhst-bintype = mo_model->ms_lower_block-s_storage_type_data-bintype.
        ls_matlwhstx-bintype = abap_true.

        ls_matlwhst-repqty = mo_model->ms_lower_block-s_storage_type_data-repqty_dsp.
        ls_matlwhstx-repqty = abap_true.
        ls_matlwhst-repqty_uom_dsp = 'ST'.
        ls_matlwhstx-repqty_uom_dsp = abap_true.

        ls_matlwhst-minqty = mo_model->ms_lower_block-s_storage_type_data-minqty_dsp.
        ls_matlwhstx-minqty = abap_true.
        ls_matlwhst-minqty_uom_dsp = 'ST'.
        ls_matlwhstx-minqty_uom_dsp = abap_true.

        ls_matlwhst-maxqty = mo_model->ms_lower_block-s_storage_type_data-maxqty_dsp.
        ls_matlwhstx-maxqty = abap_true.
        ls_matlwhst-maxqty_uom_dsp = 'ST'.
        ls_matlwhstx-maxqty_uom_dsp = abap_true.

        ls_matlwhst-quanclaput = mo_model->ms_lower_block-s_storage_type_data-quanclaput.
        ls_matlwhstx-quanclaput = abap_true.

        ls_matlwhst-zz1_dirrpl_stt = mo_model->ms_lower_block-s_storage_type_data-zz1_dirrpl_stt.
        ls_matlwhstx-zz1_dirrpl_stt = abap_true.

        ls_matlwhst-zz1_maxput_stt = mo_model->ms_lower_block-s_storage_type_data-zz1_maxput_stt.
        ls_matlwhstx-zz1_maxput_stt = abap_true.

        APPEND ls_matlwhst  TO lt_matlwhst_create.
        APPEND ls_matlwhstx TO lt_matlwhstx.
        lv_update_whst = abap_true.
      ENDIF.
    ENDIF.

    SORT lt_matlwhst_create BY ext_matnr
                               ext_entity
                               ext_entitled
                               lgtyp.
    SORT lt_matlwhst_update BY ext_matnr
                               ext_entity
                               ext_entitled
                               lgtyp.
    SORT lt_matlwhstx BY ext_matnr
                         ext_entity
                         ext_entitled
                         lgtyp.
    DELETE ADJACENT DUPLICATES FROM lt_matlwhst_create.
    DELETE ADJACENT DUPLICATES FROM lt_matlwhst_update.
    DELETE ADJACENT DUPLICATES FROM lt_matlwhstx.

    IF lv_no_change_whs IS NOT INITIAL AND lv_no_whst_change IS NOT INITIAL.
      RETURN.
    ENDIF.

    IF    iv_daily_volume_update IS NOT INITIAL
       OR (     iv_daily_volume_update IS INITIAL AND lv_update_wh IS NOT INITIAL
            AND lv_update_whst         IS INITIAL ) ##BOOL_OK.
      " Product mass change
      me->wh_prod_mass_change( EXPORTING it_matkey        = lt_matkey
                                         it_matkeyx       = lt_matkeyx
                                         it_matlwh_create = lt_matlwh_create
                                         it_matlwh_update = lt_matlwh_update
                                         it_matlwhx       = lt_matlwhx
                               IMPORTING et_return        = DATA(lt_return) ##NEEDED
                                         ev_abort         = DATA(lv_abort) ##NEEDED
                                         ev_lines         = DATA(lv_lines) ) ##NEEDED.
    ELSEIF     iv_daily_volume_update IS INITIAL AND lv_update_wh IS NOT INITIAL
           AND lv_update_whst         IS NOT INITIAL ##BOOL_OK.

      me->wh_prod_mass_change( EXPORTING it_matkey          = lt_matkey
                                         it_matkeyx         = lt_matkeyx
                                         it_matlwh_update   = lt_matlwh_update
                                         it_matlwhx         = lt_matlwhx
                                         it_matlwhst_create = lt_matlwhst_create
                                         it_matlwhst_update = lt_matlwhst_update
                                         it_matlwhstx       = lt_matlwhstx
                               IMPORTING et_return          = lt_return
                                         ev_abort           = lv_abort
                                         ev_lines_st        = lv_lines ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_field_mtart_and_mmsta.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Fill Material Type and Plant-Specific Material Status
    "*&
    "*&
    "********************************************************************
    DATA lt_marc             TYPE TABLE OF /scwm/if_af_material=>ty_marc_mmsta.
    DATA lo_stockid_map      TYPE REF TO /scwm/if_stockid_mapping.
    DATA lv_plant            TYPE werks_d.
    DATA lo_saf              TYPE REF TO /scdl/cl_af_management.
    DATA lo_material_service TYPE REF TO /scwm/if_af_material.
    DATA lt_mara             TYPE /scwm/if_af_material=>tt_matid_guid22.

    FIELD-SYMBOLS <fs_data> TYPE LINE OF /scwm/tt_prod_mon_out.
    FIELD-SYMBOLS <fs_marc> TYPE /scwm/if_af_material=>ty_marc_mmsta.
    FIELD-SYMBOLS <fs_mara> TYPE /scwm/if_af_material=>ty_matid_guid22.

    DATA ls_entitled    TYPE bupa_partner_guid.
    DATA ls_werks_range TYPE /scwm/if_af_material=>ty_werks_range.
    DATA lt_werks_range TYPE /scwm/if_af_material=>tt_werks_range.
    TYPES:
      BEGIN OF ty_entitled_plant,
        partner_guid TYPE bu_partner_guid,
        plant        TYPE werks_d,
      END OF ty_entitled_plant.
    DATA ls_entitled_plant TYPE ty_entitled_plant.
    DATA lt_entitled_plant TYPE TABLE OF ty_entitled_plant.

    CALL FUNCTION '/SCWM/GET_STOCKID_MAP_INSTANCE'
      IMPORTING
        eif_stockid_mapping = lo_stockid_map.

    LOOP AT mt_entitled INTO ls_entitled.
      TRY.
          lo_stockid_map->get_plant_by_partnerno( EXPORTING iv_wme_partno = ls_entitled-partner
                                                  IMPORTING ev_erp_plant  = lv_plant ).
        CATCH /scwm/cx_stockid_map.
          CONTINUE.
      ENDTRY.
      ls_werks_range-sign   = 'I'.
      ls_werks_range-option = 'EQ'.
      ls_werks_range-low    = lv_plant.
      APPEND ls_werks_range TO lt_werks_range.

      ls_entitled_plant-partner_guid = ls_entitled-partner_guid.
      ls_entitled_plant-plant        = lv_plant.
      APPEND ls_entitled_plant TO lt_entitled_plant.
    ENDLOOP.

    lo_saf = /scdl/cl_af_management=>get_instance( ).
    lo_material_service ?= lo_saf->get_service( iv_service = '/SCWM/IF_AF_MATERIAL' ).

    lo_material_service->get_marc_mmsta( EXPORTING it_matnr_range = it_matnr_range
                                                   it_werks_range = lt_werks_range
                                         IMPORTING et_marc        = lt_marc ).

    SORT lt_marc BY matnr
                    werks.

    lo_material_service->get_mara_mtart( EXPORTING it_mat_range = it_matid_range
                                         IMPORTING et_matid     = lt_mara ).

    SORT lt_mara BY scm_matid_guid22.

    LOOP AT ct_data ASSIGNING <fs_data>.

      CLEAR lv_plant.

      TRY.
          lv_plant = lt_entitled_plant[ partner_guid = <fs_data>-entitled_id ]-plant.
        CATCH cx_sy_itab_line_not_found.
          CONTINUE.
      ENDTRY.

      READ TABLE lt_marc ASSIGNING <fs_marc> WITH KEY matnr = <fs_data>-matnr
                                                      werks = lv_plant BINARY SEARCH.
      IF sy-subrc = 0.
        <fs_data>-mmsta = <fs_marc>-mmsta.
      ELSE.
        CLEAR <fs_data>-mmsta.
      ENDIF.

      READ TABLE lt_mara ASSIGNING <fs_mara> WITH KEY scm_matid_guid22 = <fs_data>-matid.
      IF sy-subrc = 0.
        <fs_data>-mtart = <fs_mara>-mtart.
        <fs_data>-meins = <fs_mara>-meins.
      ELSE.
        CLEAR <fs_data>-mtart.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD get_prod_node_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get product node data
    "*&
    "*&
    "********************************************************************

    DATA lt_matid_tab        TYPE tt_matid.
    DATA lt_matnr            TYPE /scmb/mdl_matnr_tab.
    DATA lt_sapapo_mattxt    TYPE TABLE OF /sapapo/mattxt_str.
    DATA lt_sapapo_matpack   TYPE TABLE OF /sapapo/matpack_str.
    DATA lt_sapapo_matexec   TYPE TABLE OF /sapapo/matexec_str.
    DATA lv_langu            TYPE sy-langu.
    DATA ls_exclude          TYPE /sapapo/mat_selflags_str.
    DATA lt_prodlwh          TYPE tt_lgnum_matlwh.
    DATA lv_where            TYPE string.
    DATA lt_where            TYPE tt_where.
    DATA lt_prod_mon_out     TYPE /scwm/tt_prod_mon_out.
    DATA lt_prod_mon_outtemp TYPE /scwm/tt_prod_mon_out.
    DATA lt_backflush        TYPE TABLE OF dd07v.
    DATA ls_backflush        TYPE dd07v.
    DATA ls_kit_fixed        TYPE dd07v.
    DATA lt_kit_fixed        TYPE TABLE OF dd07v.
    DATA ls_t305rt           TYPE /scwm/s_t305rt.
    DATA ls_t305qt           TYPE /scwm/s_t305qt.
    DATA ls_twrkldgrt        TYPE /scwm/s_twrkldgrt.
    DATA ls_tptdetindt       TYPE /scwm/s_tptdetindt.
    DATA ls_tprocprflt       TYPE /scwm/s_tprocprflt.
    DATA lt_matnr_range      TYPE tt_matnr_range.
    DATA lt_entit_range      TYPE STANDARD TABLE OF ty_entit_range.
    DATA lt_marm             TYPE TABLE OF /sapapo/marm_str.
    DATA lt_material         TYPE /sapapo/matkey_out_tab.
    DATA lv_failed           TYPE abap_bool.
    DATA lt_return           TYPE bapiret2_t.

    FIELD-SYMBOLS <ls_prod> TYPE /scwm/s_prod_mon_out.

    DATA lt_matid TYPE SORTED TABLE OF /sapapo/matid_str WITH NON-UNIQUE KEY matid.

    DATA(ls_t300_md) = mo_model->read_location( ).
    DATA(ls_partner) = mo_model->partner_read( me->mo_model->ms_top_block-sub_section_1-entitled ).
    mv_scuguid = ls_t300_md-scuguid.

    lt_matnr_range = VALUE #( ( sign   = wmegc_sign_inclusive
                                option = wmegc_option_eq
                                low    = mo_model->ms_top_block-sub_section_1-matnr ) ).

    lt_entit_range = VALUE #( ( sign   = wmegc_sign_inclusive
                                option = wmegc_option_eq
                                low    = mo_model->ms_top_block-sub_section_1-entitled ) ).
    CLEAR mt_entitled.
    mt_entitled = VALUE #( ( partner = ls_partner-partner partner_guid = ls_partner-partner_guid ) ).

    mo_isolated_doc->get_prod_id(
      EXPORTING
        iv_scuguid        = ls_t300_md-scuguid
        it_entitled       = VALUE #( ( partner = ls_partner-partner partner_guid = ls_partner-partner_guid ) )
        iv_selection_mode = sc_show_only_wh
        it_matnr_range    = lt_matnr_range
      IMPORTING
        et_matid_tab      = lt_matid_tab ).

    CLEAR ls_exclude.
    IF lt_matid_tab[] IS NOT INITIAL.
      IF iv_uom_include = abap_false.
        ls_exclude-no_marm = abap_true.
      ENDIF.
      ls_exclude-no_penalty1     = abap_true.
      ls_exclude-no_matvers_head = abap_true.
      ls_exclude-no_matsbod      = abap_true.
      ls_exclude-no_matrgpt      = abap_true.
      ls_exclude-no_matgroup     = abap_true.
      ls_exclude-no_matinfo      = abap_true.
      ls_exclude-no_matapn       = abap_true.
      ls_exclude-no_mean         = abap_true.

      " read product
      mo_isolated_doc->sapapo_product_dm_read( EXPORTING iv_langu      = sy-langu
                                                         is_exclude    = ls_exclude
                                                         it_matid      = lt_matid_tab
                                                         it_matnr      = lt_matnr
                                               IMPORTING et_matkey_out = lt_material
                                                         et_mattxt     = lt_sapapo_mattxt
                                                         et_matpack    = lt_sapapo_matpack
                                                         et_matexec    = lt_sapapo_matexec
                                                         et_marm       = lt_marm ).

      et_marm = lt_marm.

      et_material = lt_material.
      MOVE-CORRESPONDING lt_material[] TO lt_matid[] ##ENH_OK.

      lt_sapapo_mattxt = FILTER #( lt_sapapo_mattxt   IN lt_matid WHERE matid = matid ).
      lt_sapapo_matpack = FILTER #( lt_sapapo_matpack IN lt_matid WHERE matid = matid ).
      lt_sapapo_matexec = FILTER #( lt_sapapo_matexec IN lt_matid WHERE matid = matid ).
      lt_marm = FILTER #( lt_marm IN lt_matid WHERE matid = matid ).

    ELSE.
      RETURN.
    ENDIF.

    " buffer the mapping between Matnr Matid and Matguid

    CLEAR mt_mat_nr_id_guid.

    mo_isolated_doc->buffer_mapping_mat_nr_id_guid( EXPORTING it_material       = lt_material
                                                    IMPORTING et_mat_nr_id_guid = mt_mat_nr_id_guid ).

    " read warehouse product fields from DB including fields which may be in the MARC (S/4)

    mo_isolated_doc->read_matlwh_multi(
      EXPORTING
        iv_lgnum          = mo_model->ms_top_block-sub_section_1-lgnum
        iv_scuguid        = ls_t300_md-scuguid
        it_entitled       = VALUE #( ( partner = ls_partner-partner partner_guid = ls_partner-partner_guid ) )
        it_mat_nr_id_guid = mt_mat_nr_id_guid
        it_material       = lt_material
      IMPORTING
        et_prod_lwh       = lt_prodlwh ).

    SORT et_material BY matid.
    SORT lt_prodlwh BY matid
                       lgnum
                       entitled.
    SORT lt_sapapo_matpack BY matid.
    SORT lt_sapapo_matexec BY matid.
    SORT lt_sapapo_mattxt BY matid.

    " create the output list depending of the option

    create_list_of_wh_products( EXPORTING iv_lgnum          = mo_model->ms_top_block-sub_section_1-lgnum
                                          it_material       = et_material
                                          it_sapapo_mattxt  = lt_sapapo_mattxt
                                          it_sapapo_matpack = lt_sapapo_matpack
                                          it_sapapo_matexec = lt_sapapo_matexec
                                          it_wh_product     = lt_prodlwh
                                          iv_selection_mode = sc_show_only_wh
                                IMPORTING et_data           = lt_prod_mon_outtemp ).

    " filter output entries, if selection criteria have been entered

    CLEAR lt_where.
    CLEAR lv_where.
    lv_where = ' matnr is not initial ' ##NO_TEXT.

    APPEND lv_where TO lt_where.

    IF lt_entit_range IS INITIAL.
      " do nothing
    ELSE.
      IF lt_entit_range IS NOT INITIAL.
        lv_where = ' and ENTITLED in LT_ENTIT_RANGE ' ##NO_TEXT.
        APPEND lv_where TO lt_where.
      ENDIF.
    ENDIF.

    LOOP AT lt_prod_mon_outtemp ASSIGNING <ls_prod> WHERE (lt_where).
      " warehouse data --- Process Block Profile Description
      IF <ls_prod>-procprfl IS NOT INITIAL.
        CALL FUNCTION '/SCWM/TPROCPRFLT_READ_SINGLE'
          EXPORTING
            iv_lgnum      = mo_model->ms_top_block-sub_section_1-lgnum
            iv_procprfl   = <ls_prod>-procprfl
          IMPORTING
            es_tprocprflt = ls_tprocprflt
          EXCEPTIONS
            not_found     = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          <ls_prod>-procprflt = ls_tprocprflt-text.
        ENDIF.
      ENDIF.

      " warehouse data --- ProcTypeDet Ind Description
      IF <ls_prod>-ptdetind IS NOT INITIAL.
        CALL FUNCTION '/SCWM/TPTDETINDT_READ_SINGLE'
          EXPORTING
            iv_lgnum      = mo_model->ms_top_block-sub_section_1-lgnum
            iv_ptdetind   = <ls_prod>-ptdetind
          IMPORTING
            es_tptdetindt = ls_tptdetindt
          EXCEPTIONS
            not_found     = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          <ls_prod>-ptdetindt = ls_tptdetindt-text.
        ENDIF.
      ENDIF.

      " warehouse data --- prod. load category Description
      IF <ls_prod>-wrkldgr IS NOT INITIAL.
        CALL FUNCTION '/SCWM/TWRKLDGRT_READ_SINGLE'
          EXPORTING
            iv_lgnum     = mo_model->ms_top_block-sub_section_1-lgnum
            iv_wrkldgr   = <ls_prod>-wrkldgr
          IMPORTING
            es_twrkldgrt = ls_twrkldgrt
          EXCEPTIONS
            not_found    = 1
            OTHERS       = 2.
        IF sy-subrc = 0.
          <ls_prod>-wrkldgrt = ls_twrkldgrt-text.
        ENDIF.
      ENDIF.

      " putaway strategy text
      IF <ls_prod>-put_stra IS NOT INITIAL.
        CLEAR ls_t305qt.
        CALL FUNCTION '/SCWM/T305QT_READ_SINGLE'
          EXPORTING
            iv_lgnum    = mo_model->ms_top_block-sub_section_1-lgnum
            iv_put_stra = <ls_prod>-put_stra
          IMPORTING
            es_t305qt   = ls_t305qt
          EXCEPTIONS
            not_found   = 1
            OTHERS      = 2.
        IF sy-subrc = 0.
          <ls_prod>-put_stra_t = ls_t305qt-text.
        ENDIF.
      ENDIF.

      " removal strategy text
      IF <ls_prod>-rem_stra IS NOT INITIAL.
        CLEAR ls_t305rt.
        CALL FUNCTION '/SCWM/T305RT_READ_SINGLE'
          EXPORTING
            iv_lgnum    = mo_model->ms_top_block-sub_section_1-lgnum
            iv_rem_stra = <ls_prod>-rem_stra
          IMPORTING
            es_t305rt   = ls_t305rt
          EXCEPTIONS
            not_found   = 1
            OTHERS      = 2.
        IF sy-subrc = 0.
          <ls_prod>-rem_stra_t = ls_t305rt-rem_strat.
        ENDIF.
      ENDIF.

      IF <ls_prod>-kit_fixed_quan IS NOT INITIAL.
        CALL FUNCTION 'DDIF_DOMA_GET'
          EXPORTING
            name      = '/SCWM/DO_KIT_FIXED_QUAN_PROD'
            langu     = lv_langu
          TABLES
            dd07v_tab = lt_kit_fixed
          EXCEPTIONS
            OTHERS    = 1.

        IF sy-subrc = 0.
          READ TABLE lt_kit_fixed WITH KEY domvalue_l = <ls_prod>-kit_fixed_quan INTO ls_kit_fixed.
          IF sy-subrc = 0.
            IF ls_kit_fixed-ddtext IS NOT INITIAL.
              <ls_prod>-kit_fixed_quant = ls_kit_fixed-ddtext.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF <ls_prod>-backflush_prod IS NOT INITIAL.
        CALL FUNCTION 'DDIF_DOMA_GET'
          EXPORTING
            name      = '/SCWM/DO_BACKFLUSH_PROD'
            langu     = lv_langu
          TABLES
            dd07v_tab = lt_backflush
          EXCEPTIONS
            OTHERS    = 1.

        IF sy-subrc = 0.
          READ TABLE lt_backflush WITH KEY domvalue_l = <ls_prod>-backflush_prod INTO ls_backflush.
          IF sy-subrc = 0.
            IF ls_backflush-ddtext IS NOT INITIAL.
              <ls_prod>-backflush_prodt = ls_backflush-ddtext.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.

      IF <ls_prod>-sled_bbd = abap_true.
        <ls_prod>-sled_expd = abap_false.
      ELSE.
        <ls_prod>-sled_expd = abap_true.
      ENDIF.

      <ls_prod>-shelf_life_dur_iprkz   = <ls_prod>-shelf_life_dur.
      <ls_prod>-shlf_lfe_req_min_iprkz = <ls_prod>-shlf_lfe_req_min.

      " STFAC, numc type has to contain 00 for batch processing
      IF <ls_prod>-stfac = '  '.
        <ls_prod>-stfac = '00'.
      ENDIF.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = <ls_prod>-stfac
        IMPORTING
          output = <ls_prod>-stfac
        EXCEPTIONS
          OTHERS = 1.
      IF sy-subrc <> 0 ##NEEDED.
        " Ignore
      ENDIF.

      mo_isolated_doc->product_authority_check( EXPORTING iv_matnr  = <ls_prod>-ext_matnr
                                                          iv_actvt  = '03'
                                                          iv_lgnum  = mo_model->ms_top_block-sub_section_1-lgnum
                                                IMPORTING ev_failed = lv_failed
                                                CHANGING  ct_return = lt_return ).

      IF lv_failed = abap_true.
        CONTINUE.
      ENDIF.
      APPEND <ls_prod> TO lt_prod_mon_out.
    ENDLOOP.
    SORT lt_prod_mon_out BY matnr.
    MOVE-CORRESPONDING lt_prod_mon_out TO et_data.
  ENDMETHOD.


  METHOD get_scuguid.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Supply Chain Unit Guid
    "*&
    "*&
    "********************************************************************
    ev_scuguid = mo_isolated_doc->get_scuguid( iv_lgnum = iv_lgnum ).
  ENDMETHOD.


  METHOD get_styp_node_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get stroga type node data
    "*&
    "*&
    "********************************************************************

    DATA ls_prod_strg_mon_out TYPE /scwm/s_prod_strg_mon_out.
    DATA lt_prodlwhst         TYPE /sapapo/dm_matlwhst_tab.
    DATA lt_new_records       TYPE /sapapo/dm_matlwhst_tab.
    DATA ls_new_record        TYPE /sapapo/dm_matlwhst.
    DATA lt_prodlwhstid       TYPE /sapapo/dm_matlwhst_id_tab.
    DATA ls_prodlwhstid       TYPE /sapapo/dm_matlwhst_id.
    DATA lv_where             TYPE string.
    DATA lt_where             TYPE tt_where.
    DATA lv_mat16             TYPE /scmb/mdl_matid.
    DATA lt_matlwh            TYPE /scwm/tt_prod_mon_out.
    DATA lv_failed            TYPE abap_bool.
    DATA lt_return            TYPE bapiret2_t.

    FIELD-SYMBOLS <fs_matlwh>       TYPE /scwm/s_prod_mon_out.
    FIELD-SYMBOLS <ls_matlwhst>     TYPE /sapapo/dm_matlwhst.
    FIELD-SYMBOLS <fs_data>         TYPE /scwm/s_prod_mon_out.
    FIELD-SYMBOLS <fs_prodlwhst_id> TYPE /sapapo/dm_matlwhst_id.
    " TODO: variable is assigned but never used (ABAP cleaner)
    FIELD-SYMBOLS <fs_prodlwhst>    TYPE /sapapo/dm_matlwhst.

    IF it_selected_wh_products IS INITIAL.
    ELSE.
      " eliminate entries where the warehouse product does not exist
      LOOP AT it_selected_wh_products ASSIGNING <fs_data> WHERE no_wh_data IS INITIAL.
        APPEND <fs_data> TO lt_matlwh.
      ENDLOOP.
    ENDIF.

    " read table /sapapo/matlhwst from DB
    SORT lt_matlwh BY matid
                      scuguid
                      entitled_id.
    IF lt_matlwh IS NOT INITIAL.
      LOOP AT lt_matlwh ASSIGNING <fs_matlwh> WHERE no_wh_data IS INITIAL.
        ls_prodlwhstid-matid       = <fs_matlwh>-matid.
        ls_prodlwhstid-scuguid     = <fs_matlwh>-scuguid.
        ls_prodlwhstid-entitled_id = <fs_matlwh>-entitled_id.
        APPEND ls_prodlwhstid TO lt_prodlwhstid.
      ENDLOOP.
    ENDIF.

    mo_isolated_doc->sapapo_matlwhst_read_multi_2( EXPORTING it_key      = lt_prodlwhstid
                                                   IMPORTING et_prodwhst = lt_prodlwhst ).

    CLEAR lt_new_records.

    " if option 2 or 3, creation of empty records for warehouse products without sto.type data for iv_lgtyp
    IF ( iv_only_without_whst IS NOT INITIAL OR iv_both IS NOT INITIAL ) " option 2 or 3
       AND it_selected_wh_products IS INITIAL. " in drill down case, shown only the existing records
      " remove records with other warehouse types than iv_lgtyp
      LOOP AT lt_prodlwhst ASSIGNING <fs_prodlwhst> WHERE lgtyp <> iv_lgtyp.
        DELETE lt_prodlwhst.
      ENDLOOP.

      SORT lt_prodlwhst BY matid
                           scuguid
                           entitled_id.
      CLEAR ls_new_record.

      LOOP AT lt_prodlwhstid ASSIGNING <fs_prodlwhst_id>.
        READ TABLE lt_prodlwhst WITH KEY matid       = <fs_prodlwhst_id>-matid
                                         scuguid     = <fs_prodlwhst_id>-scuguid
                                         entitled_id = <fs_prodlwhst_id>-entitled_id
                                         lgtyp       = iv_lgtyp
             TRANSPORTING NO FIELDS
             BINARY SEARCH.
        IF sy-subrc <> 0.
          ls_new_record-matid       = <fs_prodlwhst_id>-matid.
          ls_new_record-entitled_id = <fs_prodlwhst_id>-entitled_id.
          ls_new_record-scuguid     = <fs_prodlwhst_id>-scuguid.

          ls_new_record-lgtyp       = iv_lgtyp.
          APPEND ls_new_record TO lt_new_records.
        ENDIF.
      ENDLOOP.

      IF iv_only_without_whst IS NOT INITIAL.
        CLEAR lt_prodlwhst.
      ENDIF.
      APPEND LINES OF lt_new_records TO lt_prodlwhst.
      SORT lt_prodlwhst BY matid
                           scuguid
                           entitled_id
                           lgtyp.

      SORT lt_new_records BY matid
                             scuguid
                             entitled_id
                             lgtyp.
    ENDIF.

    " build the where clause
    CLEAR lt_where.
    CLEAR lv_where.
    lv_where = ' matid is not initial ' ##NO_TEXT.
    APPEND lv_where TO lt_where.

    " build the output table
    CLEAR et_data.
    CLEAR ls_prod_strg_mon_out.

    LOOP AT lt_prodlwhst ASSIGNING <ls_matlwhst> WHERE (lt_where).
      CLEAR ls_prod_strg_mon_out.
      MOVE-CORRESPONDING <ls_matlwhst> TO ls_prod_strg_mon_out ##ENH_OK.

      " mark new created records with 'X' in field NO_WHSTDATA
      READ TABLE lt_new_records WITH KEY matid       = <ls_matlwhst>-matid
                                         scuguid     = <ls_matlwhst>-scuguid
                                         entitled_id = <ls_matlwhst>-entitled_id
                                         lgtyp       = <ls_matlwhst>-lgtyp
           BINARY SEARCH
           TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        ls_prod_strg_mon_out-no_whstdata      = 'X'.
        ls_prod_strg_mon_out-whst_data_exists = TEXT-002.
      ELSE.
        CLEAR ls_prod_strg_mon_out-no_whstdata.
        ls_prod_strg_mon_out-whst_data_exists = TEXT-001.
      ENDIF.

      " get corresponding warehouse product data
      READ TABLE lt_matlwh WITH KEY matid       = <ls_matlwhst>-matid
                                    scuguid     = <ls_matlwhst>-scuguid
                                    entitled_id = <ls_matlwhst>-entitled_id BINARY SEARCH
           ASSIGNING <fs_matlwh>.

      IF sy-subrc <> 0. " must be always the case
        CONTINUE.
      ENDIF. " read warehouse product

      ls_prod_strg_mon_out-matnr    = <fs_matlwh>-matnr.
      ls_prod_strg_mon_out-entitled = <fs_matlwh>-entitled.
      ls_prod_strg_mon_out-meins    = <fs_matlwh>-meins.
      ls_prod_strg_mon_out-mtart    = <fs_matlwh>-mtart.
      ls_prod_strg_mon_out-mmsta    = <fs_matlwh>-mmsta.

      CALL FUNCTION '/SCMB/MDL_GUID_CONVERT'
        EXPORTING
          iv_guid22 = <ls_matlwhst>-matid
        IMPORTING
          ev_guid16 = lv_mat16.

      IF ls_prod_strg_mon_out-repqty_plan IS NOT INITIAL.
        me->quan_convert( EXPORTING iv_matid = lv_mat16
                                    iv_s_qty = ls_prod_strg_mon_out-repqty_plan
                                    iv_s_uom = <fs_matlwh>-meins
                                    iv_d_uom = ls_prod_strg_mon_out-repqty_uom_dsp
                          CHANGING  cv_d_qty = ls_prod_strg_mon_out-repqty_plan ).
      ENDIF.

      IF ls_prod_strg_mon_out-maxqty_plan IS NOT INITIAL.
        me->quan_convert( EXPORTING iv_matid = lv_mat16
                                    iv_s_qty = ls_prod_strg_mon_out-maxqty_plan
                                    iv_s_uom = <fs_matlwh>-meins
                                    iv_d_uom = ls_prod_strg_mon_out-maxqty_uom_dsp
                          CHANGING  cv_d_qty = ls_prod_strg_mon_out-maxqty_plan ).
      ENDIF.

      IF ls_prod_strg_mon_out-minqty_plan IS NOT INITIAL.
        me->quan_convert( EXPORTING iv_matid = lv_mat16
                                    iv_s_qty = ls_prod_strg_mon_out-minqty_plan
                                    iv_s_uom = <fs_matlwh>-meins
                                    iv_d_uom = ls_prod_strg_mon_out-minqty_uom_dsp
                          CHANGING  cv_d_qty = ls_prod_strg_mon_out-minqty_plan ).
      ENDIF.

      IF ls_prod_strg_mon_out-repqty IS NOT INITIAL.
        me->quan_convert( EXPORTING iv_matid = lv_mat16
                                    iv_s_qty = ls_prod_strg_mon_out-repqty
                                    iv_s_uom = <fs_matlwh>-meins
                                    iv_d_uom = ls_prod_strg_mon_out-repqty_uom_dsp
                          CHANGING  cv_d_qty = ls_prod_strg_mon_out-repqty ).
      ENDIF.

      IF ls_prod_strg_mon_out-maxqty IS NOT INITIAL.
        me->quan_convert( EXPORTING iv_matid = lv_mat16
                                    iv_s_qty = ls_prod_strg_mon_out-maxqty
                                    iv_s_uom = <fs_matlwh>-meins
                                    iv_d_uom = ls_prod_strg_mon_out-maxqty_uom_dsp
                          CHANGING  cv_d_qty = ls_prod_strg_mon_out-maxqty ).
      ENDIF.

      IF ls_prod_strg_mon_out-minqty IS NOT INITIAL.
        me->quan_convert( EXPORTING iv_matid = lv_mat16
                                    iv_s_qty = ls_prod_strg_mon_out-minqty
                                    iv_s_uom = <fs_matlwh>-meins
                                    iv_d_uom = ls_prod_strg_mon_out-minqty_uom_dsp
                          CHANGING  cv_d_qty = ls_prod_strg_mon_out-minqty ).
      ENDIF.

      " check authority
      mo_isolated_doc->product_authority_check( EXPORTING iv_lgnum  = mo_model->ms_top_block-sub_section_1-lgnum
                                                          iv_matnr  = ls_prod_strg_mon_out-matnr
                                                          iv_actvt  = '03'           " Display
                                                IMPORTING ev_failed = lv_failed
                                                CHANGING  ct_return = lt_return ).

      IF lv_failed IS NOT INITIAL.
        CONTINUE.
      ENDIF.
      APPEND ls_prod_strg_mon_out TO et_data.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_uom_node_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get unit of measure node data
    "*&
    "*&
    "********************************************************************

    DATA lt_matid_tab    TYPE tt_matid.
    DATA lt_matnr        TYPE /scmb/mdl_matnr_tab.
    DATA ls_exclude      TYPE /sapapo/mat_selflags_str.
    DATA lt_prod_uom_out TYPE /scwm/tt_prod_uom_mon_out.
    DATA lt_prod_uom     TYPE /scwm/tt_prod_uom_mon_out.
    DATA lt_marm         TYPE /sapapo/marm_tab.
    DATA lt_material     TYPE /sapapo/matkey_out_tab.
    DATA lt_wh_product   TYPE /scwm/tt_prod_mon_out.
    DATA lv_failed       TYPE abap_bool.
    DATA lt_return       TYPE bapiret2_t.

    FIELD-SYMBOLS <ls_prod_uom>   TYPE /scwm/s_prod_uom_mon_out.
    FIELD-SYMBOLS <fs_wh_product> TYPE /scwm/s_prod_mon_out.
    FIELD-SYMBOLS <fs_material>   TYPE /sapapo/matkey_out.

    IF it_selected_wh_products IS NOT INITIAL.
      lt_wh_product = it_selected_wh_products.

      CLEAR ls_exclude.
      ls_exclude-no_satnr        = abap_true.
      ls_exclude-no_matmap       = abap_true.
      ls_exclude-no_mattxt       = abap_true.
      ls_exclude-no_matpack      = abap_true.
      ls_exclude-no_matexec      = abap_true.
      ls_exclude-no_penalty1     = abap_true.
      ls_exclude-no_matvers_head = abap_true.
      ls_exclude-no_matsbod      = abap_true.
      ls_exclude-no_matrgpt      = abap_true.
      ls_exclude-no_matgroup     = abap_true.
      ls_exclude-no_matinfo      = abap_true.
      ls_exclude-no_matapn       = abap_true.
      ls_exclude-no_rmatp        = abap_true.
      ls_exclude-no_mean         = abap_true.

      CLEAR lt_matid_tab.
      MOVE-CORRESPONDING lt_wh_product TO lt_matid_tab ##ENH_OK.
      SORT lt_matid_tab BY matid.
      DELETE ADJACENT DUPLICATES FROM lt_matid_tab.

      mo_isolated_doc->sapapo_product_dm_read( EXPORTING iv_langu      = sy-langu
                                                         is_exclude    = ls_exclude
                                                         it_matid      = lt_matid_tab
                                                         it_matnr      = lt_matnr
                                               IMPORTING et_matkey_out = lt_material
                                                         et_marm       = lt_marm ).

    ELSE.
      CLEAR lt_material.

      me->get_prod_node_data( EXPORTING iv_uom_include = abap_true
                              IMPORTING et_data        = lt_wh_product
                                        et_prod_uom    = lt_prod_uom_out
                                        et_marm        = lt_marm
                                        et_material    = lt_material ).
    ENDIF.

    " integrate UOM data from table MARM into the output entries
    IF lt_marm IS NOT INITIAL.
      SORT lt_marm BY matid.

      MOVE-CORRESPONDING lt_marm TO lt_prod_uom ##ENH_OK.

      CLEAR lt_prod_uom_out.
      SORT lt_material BY matid.
      LOOP AT lt_prod_uom ASSIGNING <ls_prod_uom>.

        " check authority
        mo_isolated_doc->product_authority_check( EXPORTING iv_lgnum  = mo_model->ms_top_block-sub_section_1-lgnum
                                                            iv_matnr  = <ls_prod_uom>-matnr
                                                            iv_actvt  = '03'           " Display
                                                  IMPORTING ev_failed = lv_failed
                                                  CHANGING  ct_return = lt_return ).

        IF lv_failed IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        LOOP AT lt_wh_product ASSIGNING <fs_wh_product> WHERE matid = <ls_prod_uom>-matid.
          <ls_prod_uom>-entitled = <fs_wh_product>-entitled.
          <ls_prod_uom>-matnr    = <fs_wh_product>-matnr.

          READ TABLE lt_material  ASSIGNING <fs_material> WITH KEY matid = <ls_prod_uom>-matid BINARY SEARCH.
          IF sy-subrc = 0.
            <ls_prod_uom>-basme = <fs_material>-meins.
          ENDIF.

          APPEND <ls_prod_uom> TO lt_prod_uom_out.
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    MOVE-CORRESPONDING lt_prod_uom_out TO et_data.
  ENDMETHOD.


  METHOD integrate_sapapo_data.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Integrate APO data
    "*&
    "*&
    "********************************************************************
    DATA ls_sapapo_mattxt  TYPE /sapapo/mattxt_str.
    DATA ls_sapapo_matpack TYPE /sapapo/matpack_str.
    DATA ls_sapapo_matexec TYPE /sapapo/matexec_str.

    IF it_sapapo_matpack IS NOT INITIAL.
      READ TABLE it_sapapo_matpack
           WITH KEY matid = iv_matid BINARY SEARCH
           INTO ls_sapapo_matpack.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING ls_sapapo_matpack
                           TO cs_product ##ENH_OK.
      ENDIF.
    ENDIF.

    IF it_sapapo_matexec IS NOT INITIAL.
      READ TABLE it_sapapo_matexec
           WITH KEY matid = iv_matid BINARY SEARCH
           INTO ls_sapapo_matexec.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING ls_sapapo_matexec
                           TO cs_product ##ENH_OK.
      ENDIF.
    ENDIF.

    IF it_sapapo_mattxt IS NOT INITIAL.
      READ TABLE it_sapapo_mattxt
           WITH KEY matid = iv_matid BINARY SEARCH
           INTO ls_sapapo_mattxt.
      IF sy-subrc IS INITIAL.
        MOVE-CORRESPONDING ls_sapapo_mattxt
                           TO cs_product ##ENH_OK.
      ENDIF.
    ENDIF.

    cs_product-lgnum = iv_lgnum.
  ENDMETHOD.


  METHOD quan_convert.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Quantity convert
    "*&
    "*&
    "********************************************************************
    DATA lv_batchid   TYPE /scwm/de_batchid.
    DATA lv_quan_from TYPE /scwm/de_quantity.
    DATA lv_quan_to   TYPE /scwm/de_quantity.

    lv_quan_from = iv_s_qty.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_QUAN_CONVERT'
          EXPORTING
            iv_matid     = iv_matid
            iv_quan      = lv_quan_from
            iv_unit_from = iv_s_uom
            iv_unit_to   = iv_d_uom
            iv_batchid   = lv_batchid
          IMPORTING
            ev_quan      = lv_quan_to.

        cv_d_qty = lv_quan_to.

      CATCH /scwm/cx_md ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD wh_prod_mass_change.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : warehouse data product update
    "*& It will updata the fields under warehouse data tab
    "*& at /SCWM/MAT1 transaction code
    "********************************************************************
    DATA ls_lock_mass_pr           TYPE /sapapo/matkey_lock.
    DATA lt_return                 TYPE bapiret2_t.
    DATA lt_list_locked_prod       TYPE tt_lock_mass_pr.
    DATA ls_matlwh                 TYPE /sapapo/ext_matlwh.
    DATA lt_matlwh                 TYPE tt_ext_matlwh.
    DATA lt_matlwhx                TYPE tt_ext_matlwhx.
    DATA ls_matlwhst               TYPE /sapapo/ext_matlwhst.
    DATA lt_matlwhst               TYPE tt_ext_matlwhst.
    DATA lt_matlwhstx              TYPE tt_ext_matlwhstx.
    DATA lv_lock_failed            TYPE abap_bool.
    DATA lv_authority_check_failed TYPE abap_bool.

    FIELD-SYMBOLS <fs_matkey>    TYPE /sapapo/ext_matkey.
    FIELD-SYMBOLS <fs_matlwh>    TYPE /sapapo/ext_matlwh.
    FIELD-SYMBOLS <fs_matlwhx>   TYPE /sapapo/ext_matlwhx.
    FIELD-SYMBOLS <fs_matlwhst>  TYPE /sapapo/ext_matlwhst.
    FIELD-SYMBOLS <fs_matlwhstx> TYPE /sapapo/ext_matlwhstx.


    LOOP AT it_matkey ASSIGNING <fs_matkey>.

      mo_isolated_doc->enqueue_optimistic( EXPORTING iv_matnr        = <fs_matkey>-ext_matnr
                                                     io_lock_mass_pr = mo_lock_mass_pr
                                           IMPORTING ev_failed       = lv_lock_failed
                                           CHANGING  ct_return       = et_return ).

      IF lv_lock_failed IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      CLEAR ls_lock_mass_pr.
      ls_lock_mass_pr-mandt = sy-mandt.
      ls_lock_mass_pr-matnr = <fs_matkey>-ext_matnr.
      APPEND ls_lock_mass_pr TO lt_list_locked_prod.
    ENDLOOP.

    " -------------------------------
    " Warehouse Products to create
    " -------------------------------
    CLEAR lt_matlwh.
    CLEAR lt_matlwhst.
    CLEAR lt_matlwhx.
    SORT lt_list_locked_prod BY matnr.
    LOOP AT it_matlwh_create ASSIGNING <fs_matlwh>.
      " do not process products that could not be locked
      READ TABLE lt_list_locked_prod WITH KEY matnr = <fs_matlwh>-ext_matnr TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      mo_isolated_doc->product_authority_check( EXPORTING iv_lgnum  = mo_model->ms_top_block-sub_section_1-lgnum
                                                          iv_actvt  = '01'           " Create
                                                          iv_matnr  = <fs_matlwh>-ext_matnr
                                                IMPORTING ev_failed = lv_authority_check_failed
                                                CHANGING  ct_return = et_return ).

      IF lv_authority_check_failed IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE it_matlwhx ASSIGNING <fs_matlwhx> WITH KEY ext_matnr    = <fs_matlwh>-ext_matnr
                                                            ext_entity   = <fs_matlwh>-ext_entity
                                                            ext_entitled = <fs_matlwh>-ext_entitled  BINARY SEARCH ##WARN_OK.
      IF sy-subrc = 0.
        APPEND <fs_matlwhx> TO lt_matlwhx.

        CLEAR ls_matlwh.
        MOVE-CORRESPONDING <fs_matlwh> TO ls_matlwh.
        ls_matlwh-method = 'N'.
        APPEND ls_matlwh TO lt_matlwh.
      ELSE.
        " should never happen
        CONTINUE.
      ENDIF.
    ENDLOOP.

    " -------------------------------
    " Warehouse Products to update
    " -------------------------------

    LOOP AT it_matlwh_update ASSIGNING <fs_matlwh>.
      " do not process products that could not be locked
      READ TABLE lt_list_locked_prod WITH KEY matnr = <fs_matlwh>-ext_matnr TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      mo_isolated_doc->product_authority_check( EXPORTING iv_lgnum  = mo_model->ms_top_block-sub_section_1-lgnum
                                                          iv_actvt  = '02'           " Change
                                                          iv_matnr  = <fs_matlwh>-ext_matnr
                                                IMPORTING ev_failed = lv_authority_check_failed
                                                CHANGING  ct_return = et_return ).

      IF lv_authority_check_failed IS NOT INITIAL.
        CONTINUE.
      ENDIF.

      READ TABLE it_matlwhx ASSIGNING <fs_matlwhx> WITH KEY ext_matnr    = <fs_matlwh>-ext_matnr
                                                            ext_entity   = <fs_matlwh>-ext_entity
                                                            ext_entitled = <fs_matlwh>-ext_entitled BINARY SEARCH ##WARN_OK.
      IF sy-subrc <> 0.
        " should never happen
        CONTINUE.
      ENDIF.

      " update of matlwh
      APPEND <fs_matlwhx> TO lt_matlwhx.

      CLEAR ls_matlwh.
      MOVE-CORRESPONDING <fs_matlwh> TO ls_matlwh.
      ls_matlwh-method = 'C'.
      APPEND ls_matlwh TO lt_matlwh.

      " ------------------------------
      " storage type data to create
      " ------------------------------

      LOOP AT it_matlwhst_create ASSIGNING <fs_matlwhst> WHERE     ext_matnr    = <fs_matlwh>-ext_matnr
                                                               AND ext_entity   = <fs_matlwh>-ext_entity
                                                               AND ext_entitled = <fs_matlwh>-ext_entitled.
        READ TABLE it_matlwhstx ASSIGNING <fs_matlwhstx> WITH KEY ext_matnr    = <fs_matlwhst>-ext_matnr
                                                                  ext_entity   = <fs_matlwhst>-ext_entity
                                                                  ext_entitled = <fs_matlwhst>-ext_entitled
                                                                  lgtyp        = <fs_matlwhst>-lgtyp BINARY SEARCH.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        mo_isolated_doc->product_authority_check( EXPORTING iv_lgnum  = mo_model->ms_top_block-sub_section_1-lgnum
                                                            iv_actvt  = '01'           " Create
                                                            iv_matnr  = <fs_matlwh>-ext_matnr
                                                  IMPORTING ev_failed = lv_authority_check_failed
                                                  CHANGING  ct_return = et_return ).

        IF lv_authority_check_failed IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        APPEND <fs_matlwhstx> TO lt_matlwhstx.

        CLEAR ls_matlwhst.
        MOVE-CORRESPONDING <fs_matlwhst> TO ls_matlwhst.
        ls_matlwhst-method = 'N'.
        APPEND ls_matlwhst TO lt_matlwhst.
      ENDLOOP.

      " ------------------------------
      " storage type data to update
      " ------------------------------

      LOOP AT it_matlwhst_update ASSIGNING <fs_matlwhst> WHERE     ext_matnr    = <fs_matlwh>-ext_matnr
                                                               AND ext_entity   = <fs_matlwh>-ext_entity
                                                               AND ext_entitled = <fs_matlwh>-ext_entitled.
        READ TABLE it_matlwhstx ASSIGNING <fs_matlwhstx> WITH KEY ext_matnr    = <fs_matlwhst>-ext_matnr
                                                                  ext_entity   = <fs_matlwhst>-ext_entity
                                                                  ext_entitled = <fs_matlwhst>-ext_entitled
                                                                  lgtyp        = <fs_matlwhst>-lgtyp BINARY SEARCH.
        IF sy-subrc = 0.
          APPEND <fs_matlwhstx> TO lt_matlwhstx.

          CLEAR ls_matlwhst.
          MOVE-CORRESPONDING <fs_matlwhst> TO ls_matlwhst.
          ls_matlwhst-method = 'C'.
          APPEND ls_matlwhst TO lt_matlwhst.
        ENDIF.
      ENDLOOP.

    ENDLOOP. " at warehouse product to update

    IF lt_matlwh IS NOT INITIAL OR lt_matlwhst IS NOT INITIAL.
      CLEAR lt_return.
      mo_isolated_doc->sapapo_dm_products_maintain( EXPORTING it_ext_matkey       = it_matkey
                                                              it_ext_matkeyx      = it_matkeyx
                                                              it_ext_matlwh       = lt_matlwh
                                                              it_ext_matlwhx      = lt_matlwhx
                                                              it_ext_matlwhst     = lt_matlwhst
                                                              it_ext_matlwhstx    = lt_matlwhstx
                                                              it_list_locked_prod = lt_list_locked_prod
                                                    IMPORTING et_return           = lt_return
                                                              ev_abort            = ev_abort ).

      APPEND LINES OF lt_return TO et_return.
    ENDIF.

    TRY.
        IF mo_lock_mass_pr IS NOT INITIAL.
          mo_lock_mass_pr->dequeue( it_master_data_objects = lt_list_locked_prod
                                    iv_maint_finished      = 'X' ).
        ENDIF.
      CATCH /scmb/cx_md_lock_system_error ##NO_HANDLER.
    ENDTRY.

    ev_lines = lines( lt_matlwh ).
    ev_lines_st = lines( lt_matlwhst ).
  ENDMETHOD.
ENDCLASS.
