class ZCL_CROSS_VAS_ORDER_PREP definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces /SCWM/IF_EX_PS_DET_PREPARE .
protected section.
private section.

  types:
    BEGIN OF ty_partner_vas,
      partner TYPE but000-partner,
      zz_vas  TYPE flag,
    END OF ty_partner_vas .

  class-data SS_CUSTOMER_VAS type TY_PARTNER_VAS .
  class-data SS_ALSO_PROF_VAS type TY_PARTNER_VAS .

  class-methods CHECK_VAS_RELEVANCE
    importing
      !IV_PARTNER_NO_1 type /SCDL/DL_PARTYNO
      !IV_PRODUCT type MATNR
      !IV_PARTNER_NO_2 type /SCDL/DL_PARTYNO
    returning
      value(RV_RELEVANT) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_CROSS_VAS_ORDER_PREP IMPLEMENTATION.


  METHOD /scwm/if_ex_ps_det_prepare~prepare.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-3095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Fill the customer field VAS relevance in packing specification
**********************************************************************
    DATA: ls_doc_item_str TYPE /scwm/dlv_docid_item_str.

    CONSTANTS lc_doc_item_str_name TYPE string VALUE '/SCWM/DLV_DOCID_ITEM_STR'.
    DATA(lo_dlv) = /scwm/cl_dlv_management_prd=>get_instance( ).


    LOOP AT ct_det_data REFERENCE INTO DATA(lr_det_data).
      IF cl_abap_typedescr=>describe_by_data_ref( lr_det_data->i_data )->get_relative_name( ) NE lc_doc_item_str_name.
        CONTINUE.
      ENDIF.
      MOVE-CORRESPONDING lr_det_data->i_data->* TO ls_doc_item_str.
      TRY.
          lo_dlv->query(
            EXPORTING
              it_docid        = VALUE #(  ( doccat = ls_doc_item_str-doccat docid = ls_doc_item_str-docid ) )
              is_read_options = VALUE #(  )
              is_include_data = VALUE #( head_partyloc = abap_true item_product_ext = abap_true )
            IMPORTING
              et_headers        = DATA(lt_dlv_header)
              et_items          = DATA(lt_dlv_item)
          ).
        CATCH /scdl/cx_delivery INTO DATA(lo_cx) ##needed ##no_handler.

      ENDTRY.
      READ TABLE lt_dlv_item REFERENCE INTO DATA(lr_item)
           WITH KEY itemid = ls_doc_item_str-itemid .
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      READ TABLE lt_dlv_header REFERENCE INTO DATA(lr_header) INDEX 1.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.
      READ TABLE lr_header->partyloc INTO DATA(ls_party_ship_to)
           WITH  KEY party_role = /scdl/if_dl_partyloc_c=>sc_party_role_stprt.
      IF sy-subrc NE 0.
        CLEAR ls_party_ship_to.
      ENDIF.

      READ TABLE lr_header->partyloc INTO DATA(ls_party_also_prof)
           WITH  KEY party_role = zif_wme_c=>gs_partyloc_c-sc_party_role_zcuspr.
      IF sy-subrc NE 0.
        CLEAR ls_party_also_prof.
      ENDIF.

      IF check_vas_relevance( iv_partner_no_1 = ls_party_ship_to-partyno
                              iv_partner_no_2 = ls_party_also_prof-partyno
                              iv_product = lr_item->product-productno ).
        lr_det_data->fields-zz_vas = abap_true.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_vas_relevance.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-3095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Checking the VAS relevance
**********************************************************************

    DATA ls_mat_lgnum TYPE  /scwm/s_material_lgnum.


    IF iv_partner_no_1 IS NOT INITIAL.
      IF ss_customer_vas-partner EQ iv_partner_no_1.
        rv_relevant = ss_customer_vas-zz_vas.
      ELSE.
        SELECT FROM but000
               FIELDS zz_is_vas_customer
                WHERE partner EQ @iv_partner_no_1
                  AND zz_is_vas_customer EQ @abap_true
                 INTO TABLE @DATA(lt_vas)
                   UP TO 1 ROWS ##needed.
        IF sy-subrc EQ 0.
          rv_relevant = abap_true.
        ENDIF.
        ss_customer_vas-partner = iv_partner_no_1.
        ss_customer_vas-zz_vas = rv_relevant.
      ENDIF.
    ENDIF.

    IF rv_relevant EQ abap_true.
      RETURN.
    ENDIF.

    IF iv_partner_no_2 IS NOT INITIAL.
      IF ss_also_prof_vas-partner EQ iv_partner_no_2.
        rv_relevant = ss_also_prof_vas-zz_vas.
      ELSE.
        SELECT FROM but000
               FIELDS zz_is_vas_customer
                WHERE partner EQ @iv_partner_no_2
                  AND zz_is_vas_customer EQ @abap_true
                 INTO TABLE @lt_vas
                   UP TO 1 ROWS.
        IF sy-subrc EQ 0.
          rv_relevant = abap_true.
          RETURN.
        ENDIF.
        ss_also_prof_vas-partner = iv_partner_no_2.
        ss_also_prof_vas-zz_vas = rv_relevant.
      ENDIF.
    ENDIF.

    IF rv_relevant EQ abap_true.
      RETURN.
    ENDIF.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid     = /scmb/cl_md_access_mdl=>get_md_access( )->get_prod( iv_prodno = iv_product )-prodid                 " Material GUID16  mit Konvertierungsexit
            iv_lgnum     = /scwm/cl_tm=>sv_lgnum                 " Lagernummer/Lagerkomplex
          IMPORTING
            es_mat_lgnum = ls_mat_lgnum.                 " Material: Lagernummerspezifische Daten
      CATCH /scwm/cx_md_interface ##no_handler.       " WME MD Importing Parameter Ausnahmeklasse
      CATCH /scwm/cx_md_material_exist ##no_handler.  " WME MD Material existiert nicht
      CATCH /scwm/cx_md_mat_lgnum_exist ##no_handler. " Material in Lagernummer nicht gepflegt
      CATCH /scwm/cx_md_lgnum_locid ##no_handler.     " Lagernummer ist keiner APO-Lokation zugeordnet
      CATCH /scwm/cx_md ##no_handler.                 " WME MD allgemeine Ausnahmeklasse
      CATCH /scmb/cx_md_access ##no_handler.                 " WME MD allgemeine Ausnahmeklasse
    ENDTRY.
    rv_relevant = ls_mat_lgnum-zz1_vascustomer_whd.
  ENDMETHOD.
ENDCLASS.
