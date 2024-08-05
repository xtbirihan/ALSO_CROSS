CLASS zcl_crs_erp_mapin_od_saverepl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_mapin_od_saverepl .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.

    CONSTANTS c_cust_prof_partyrole TYPE /scdl/dl_party_role VALUE 'ZCUSPR' ##NO_TEXT.
  PRIVATE SECTION.

    " The values of following constants must be identical to the constants in class zcl_smod_v50b0001 (ERP).
    CONSTANTS c_identtab_01 TYPE feldname VALUE 'ZZIDENTTTAB01' ##NO_TEXT.
    CONSTANTS c_identtab_02 TYPE feldname VALUE 'ZZIDENTTTAB02' ##NO_TEXT.
    CONSTANTS c_identtab_03 TYPE feldname VALUE 'ZZIDENTTTAB03' ##NO_TEXT.
    CONSTANTS c_identtab_04 TYPE feldname VALUE 'ZZIDENTTTAB04' ##NO_TEXT.
    CONSTANTS c_identtab_05 TYPE feldname VALUE 'ZZIDENTTTAB05' ##NO_TEXT.
    CONSTANTS c_hmsign_01 TYPE feldname VALUE 'ZZHMSIGN01' ##NO_TEXT.
    CONSTANTS c_hmsign_02 TYPE feldname VALUE 'ZZHMSIGN02' ##NO_TEXT.
    CONSTANTS c_hmsign_03 TYPE feldname VALUE 'ZZHMSIGN03' ##NO_TEXT.
    CONSTANTS c_hmsingle_01 TYPE feldname VALUE 'ZZHMSINGLE01' ##NO_TEXT.
    CONSTANTS c_hmsingle_02 TYPE feldname VALUE 'ZZHMSINGLE02' ##NO_TEXT.
    CONSTANTS c_hmsingle_03 TYPE feldname VALUE 'ZZHMSINGLE03' ##NO_TEXT.
    CONSTANTS c_hmbox_01 TYPE feldname VALUE 'ZZHMBOX01' ##NO_TEXT.
    CONSTANTS c_hmbox_02 TYPE feldname VALUE 'ZZHMBOX02' ##NO_TEXT.
    CONSTANTS c_hmbox_03 TYPE feldname VALUE 'ZZHMBOX03' ##NO_TEXT.
    CONSTANTS c_bsark TYPE feldname VALUE 'ZZBSARK' ##NO_TEXT.
    CONSTANTS c_nali TYPE feldname VALUE 'ZZNALI' ##NO_TEXT.
    CONSTANTS c_augru TYPE feldname VALUE 'ZZAUGRU' ##NO_TEXT.
    CONSTANTS c_dti TYPE feldname VALUE 'ZZDT' ##NO_TEXT.
    CONSTANTS c_logcon TYPE feldname VALUE 'ZZLOGCON' ##NO_TEXT.
    CONSTANTS c_salesorg_txt TYPE feldname VALUE 'ZZSALESORG_TXT' ##NO_TEXT.
    CONSTANTS c_commercial_value_hdr TYPE feldname VALUE 'ZZCOMM_VALUE_HDR' ##NO_TEXT.
    CONSTANTS c_commercial_value_itm TYPE feldname VALUE 'ZZCOMM_VALUE_ITM' ##NO_TEXT.
    CONSTANTS c_commercial_value_curr TYPE feldname VALUE 'ZZCOMM_VALUE_CURR' ##NO_TEXT.
    CONSTANTS c_country_of_origin TYPE feldname VALUE 'ZZHERKL' ##NO_TEXT.
    CONSTANTS c_cust_group1 TYPE feldname VALUE 'ZZKVGR1' ##NO_TEXT.
    CONSTANTS c_delivery_window_from TYPE feldname VALUE 'ZZDWFROM' ##NO_TEXT.
    CONSTANTS c_delivery_window_to TYPE feldname VALUE 'ZZDWTO' ##NO_TEXT.
    CONSTANTS c_customer_profile TYPE fieldname VALUE 'ZZKUNNR' ##NO_TEXT.
    CONSTANTS c_zzmatn1 TYPE feldname VALUE 'ZZMATN1' ##NO_TEXT.
    CONSTANTS c_tp_agrn TYPE feldname VALUE 'ZZTP_AGRN' ##NO_TEXT.
    CONSTANTS c_tp_lkz TYPE feldname VALUE 'ZZTP_LKZ' ##NO_TEXT.
    CONSTANTS c_tp_retdesti TYPE feldname VALUE 'ZZTP_RETDESTI' ##NO_TEXT.
    CONSTANTS c_tp_retkey TYPE feldname VALUE 'ZZTP_RETKEY' ##NO_TEXT.
    CONSTANTS c_tp_tourz TYPE feldname VALUE 'ZZTP_TOURZ' ##NO_TEXT.
    CONSTANTS c_tp_trspsys TYPE feldname VALUE 'ZZTP_TRSPSYS' ##NO_TEXT.
    CONSTANTS c_pdf_ed TYPE feldname VALUE 'ZZPDF_ED' ##NO_TEXT.
    CONSTANTS c_pdf_rs TYPE feldname VALUE 'ZZPDF_RS' ##NO_TEXT.
    CONSTANTS c_pdf_wb TYPE feldname VALUE 'ZZPDF_WB' ##NO_TEXT.
    CONSTANTS c_tpdis_field1 TYPE feldname VALUE 'ZZTPDIS_FIELD1' ##NO_TEXT.
    CONSTANTS c_tpdis_field2 TYPE feldname VALUE 'ZZTPDIS_FIELD2' ##NO_TEXT.
    CONSTANTS c_tpdis_field3 TYPE feldname VALUE 'ZZTPDIS_FIELD3' ##NO_TEXT.
    CONSTANTS c_tpdis_field4 TYPE feldname VALUE 'ZZTPDIS_FIELD4' ##NO_TEXT.
    CONSTANTS c_tp_exidv TYPE feldname VALUE 'ZZTP_EXIDV' ##NO_TEXT.
    CONSTANTS c_tp_routc TYPE feldname VALUE 'ZZTP_ROUTC' ##NO_TEXT.
    CONSTANTS c_poc TYPE feldname VALUE 'ZZPOC' ##NO_TEXT.
    CONSTANTS c_pos TYPE feldname VALUE 'ZZPOS' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_CRS_ERP_MAPIN_OD_SAVEREPL IMPLEMENTATION.


  METHOD /scwm/if_ex_mapin_od_saverepl~mapin.
**********************************************************************
*& Key           : AD-230227
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Interface Method for BAdI Implementation /SCWM/EX_ERP_MAPIN_OD_SAVEREPL
*&
**********************************************************************

    BREAK-POINT ID zcg_ex_erp_mapin_od_saverepl.

    " Loop at all delivery requests
    LOOP AT ct_dlv_request ASSIGNING FIELD-SYMBOL(<ls_dlv_request>).
      " Loop at all items of the current delivery request
      LOOP AT <ls_dlv_request>-t_item ASSIGNING FIELD-SYMBOL(<ls_item>).
        " Select Doc- and Itemnumber from key mapping.
        DATA(lv_docnumber) = VALUE #( <ls_item>-t_keymap_item[ 1 ]-docno OPTIONAL ).
        DATA(lv_itemnumber) = VALUE #( <ls_item>-t_keymap_item[ 1 ]-itemno OPTIONAL ).

        " Sales Office and Sales Group
        <ls_item>-s_eew-zzvkbur = VALUE #( it_bapi_item_org[ deliv_numb = lv_docnumber itm_number = lv_itemnumber ]-sales_off OPTIONAL ).
        <ls_item>-s_eew-zzvkgrp = VALUE #( it_bapi_item_org[ deliv_numb = lv_docnumber itm_number = lv_itemnumber ]-sales_grp OPTIONAL ).

        " Ident table fields
        <ls_item>-s_eew-zzindenttab01 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_identtab_01 ]-value OPTIONAL ).
        <ls_item>-s_eew-zzindenttab02 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_identtab_02 ]-value OPTIONAL ).
        <ls_item>-s_eew-zzindenttab03 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_identtab_03 ]-value OPTIONAL ).
        <ls_item>-s_eew-zzindenttab04 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_identtab_04 ]-value OPTIONAL ).
        <ls_item>-s_eew-zzindenttab05 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_identtab_05 ]-value OPTIONAL ).

        " Hazardous Materials
        <ls_item>-s_eew-zzhmsign01 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_hmsign_01 ]-value OPTIONAL ).
        <ls_item>-s_eew-zzhmsign02 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_hmsign_02 ]-value OPTIONAL ).
        <ls_item>-s_eew-zzhmsign03 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_hmsign_03 ]-value OPTIONAL ).

        <ls_item>-s_eew-zzhmsingle01 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_hmsingle_01 ]-value OPTIONAL ).
        <ls_item>-s_eew-zzhmsingle02 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_hmsingle_02 ]-value OPTIONAL ).
        <ls_item>-s_eew-zzhmsingle03 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_hmsingle_03 ]-value OPTIONAL ).

        <ls_item>-s_eew-zzhmbox01 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_hmbox_01 ]-value OPTIONAL ).
        <ls_item>-s_eew-zzhmbox02 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_hmbox_02 ]-value OPTIONAL ).
        <ls_item>-s_eew-zzhmbox03 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_hmbox_03 ]-value OPTIONAL ).

        " Commercial Value + Currency
        <ls_item>-s_eew-zzcomm_value = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_commercial_value_itm ]-value OPTIONAL ).
        <ls_item>-s_eew-zzcomm_value_curr = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_commercial_value_curr ]-value OPTIONAL ).

        " Country of Origin
        <ls_item>-s_eew-zzherkl = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_country_of_origin ]-value OPTIONAL ).

        " Customer Group 1
        <ls_item>-s_eew-zzcust_grp1 = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_cust_group1 ]-value OPTIONAL ).

        " Otto Diva
        <ls_item>-s_eew-zztp_exidv = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_tp_exidv ]-value OPTIONAL ).
        <ls_item>-s_eew-zztp_routc = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_tp_routc ]-value OPTIONAL ).

        "<AAHMEDOV>-240201
        "<AAHMEDOV>-240206
        "Save Otto Diva numbers in Otto Diva table
        "to each Otto Diva delivery item, there will be a unique each Otto Diva number
        "so there is no need to save it in a custom table and mark it as used
        "<AAHMEDOV>-240206
*        IF <ls_item>-s_eew-zztp_exidv IS NOT INITIAL.
*          zcl_crud_ztout_otto_diva=>insert( it_otto_diva = VALUE #( ( lgnum = is_bapi_header_org-whse_no
*                                                                      docno = lv_docnumber
*                                                                      itemno = lv_itemnumber
*                                                                      ext_tracking_nr = <ls_item>-s_eew-zztp_exidv ) ) ).
*        ENDIF.
        "<AAHMEDOV>-240201

        " PO reference
        <ls_item>-s_eew-zzpoc = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_poc ]-value OPTIONAL ).
        <ls_item>-s_eew-zzpos = VALUE #( it_bapi_extension2[ param = lv_docnumber row = lv_itemnumber field = c_pos ]-value OPTIONAL ).
      ENDLOOP.

      " Sales Organisation + Text
      <ls_dlv_request>-s_head-s_eew-zzvkorg =  <ls_dlv_request>-s_head-s_sapext-vkorg.
      <ls_dlv_request>-s_head-s_eew-zzvkorg_txt = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_salesorg_txt ]-value OPTIONAL ).

      " Custom header fields for PPF control
      <ls_dlv_request>-s_head-s_eew-zzbsark = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_bsark ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zznali = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_nali ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zzaugru = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_augru ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zzdti = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_dti ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zzlogcon = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_logcon ]-value OPTIONAL ).

      " Commercial Value + Currency
      <ls_dlv_request>-s_head-s_eew-zzcomm_hdr_value = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_commercial_value_hdr ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zzcomm_hdr_value_curr = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_commercial_value_curr ]-value OPTIONAL ).

      "ALSO customer profile
      DATA(lv_customer_profile) = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_customer_profile ]-value OPTIONAL ).
      IF lv_customer_profile IS NOT INITIAL.
        APPEND INITIAL LINE TO <ls_dlv_request>-s_head-t_partyloc REFERENCE INTO DATA(lr_customer_prof_party).
        lr_customer_prof_party->party_role = c_cust_prof_partyrole.
        lr_customer_prof_party->counter    = 1.
        lr_customer_prof_party->partyno    = lv_customer_profile.
      ENDIF.

      "substitue material
      LOOP AT <ls_dlv_request>-t_item REFERENCE INTO DATA(lr_dlv_item).
        DATA(lv_subst_mat) = VALUE /scmb/md_prodno( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno
                                                                    row   = lr_dlv_item->t_keymap_item[ 1 ]-itemno field = c_zzmatn1 ]-value OPTIONAL ).
        IF lv_subst_mat IS NOT INITIAL.
          TRY.
              /scmb/cl_md_access_mdl=>get_md_access( )->get_prod(
                EXPORTING
                  iv_prodno    = lv_subst_mat
                  iv_with_text = abap_true
                RECEIVING
                  es_prod      = DATA(ls_prod)
              ).
            CATCH /scmb/cx_md_access. " Exception Class for Master Data Accesses
              CLEAR ls_prod.
              CLEAR: lr_dlv_item->s_product-productid, lr_dlv_item->s_product-product_text,
                     lr_dlv_item->s_product-productno, lr_dlv_item->s_product-productno_ext.
              lr_dlv_item->s_eew-zzmaterp  = lr_dlv_item->s_product-productno.
              MESSAGE e049(zmc_out) WITH lv_subst_mat.
              CONTINUE.
          ENDTRY.
          lr_dlv_item->s_eew-zzmaterp  = lr_dlv_item->s_product-productno.
          lr_dlv_item->s_product-productno = lv_subst_mat.
          lr_dlv_item->s_product-productno_ext = lv_subst_mat.
          LOOP AT lr_dlv_item->t_product_ext REFERENCE INTO DATA(lr_prod_ext).
            lr_prod_ext->productno_ext = lv_subst_mat.
          ENDLOOP.

          lr_dlv_item->s_product-productid = ls_prod-prodid.
          lr_dlv_item->s_product-product_text = ls_prod-prodtext.

        ENDIF.
      ENDLOOP.

      " Delivery Window
      <ls_dlv_request>-s_head-s_eew-zzdwfrom = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_delivery_window_from ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zzdwto = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_delivery_window_to ]-value OPTIONAL ).

      " Otto Diva
      <ls_dlv_request>-s_head-s_eew-zztp_agrn = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_tp_agrn ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zztp_lkz = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_tp_lkz ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zztp_retdesti = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_tp_retdesti ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zztp_retkey = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_tp_retkey ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zztp_tourz = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_tp_tourz ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zztp_trspsys = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_tp_trspsys ]-value OPTIONAL ).

      " Custom document links
      <ls_dlv_request>-s_head-s_eew-zzpdf_ed = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_pdf_ed ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zzpdf_rs = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_pdf_rs ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zzpdf_wb = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_pdf_wb ]-value OPTIONAL ).

      " TPDIS fields
      <ls_dlv_request>-s_head-s_eew-zztpdis_field1 = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_tpdis_field1 ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zztpdis_field2 = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_tpdis_field2 ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zztpdis_field3 = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_tpdis_field3 ]-value OPTIONAL ).
      <ls_dlv_request>-s_head-s_eew-zztpdis_field4 = VALUE #( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_tpdis_field4 ]-value OPTIONAL ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
