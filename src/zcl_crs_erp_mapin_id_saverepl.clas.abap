CLASS zcl_crs_erp_mapin_id_saverepl DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES /scwm/if_ex_mapin_id_saverepl .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
private section.

  constants C_BARCODE type FELDNAME value 'ZBARCODE' ##NO_TEXT.
  constants C_INBSHIP type FELDNAME value 'ZZINBSHIP' ##NO_TEXT.
  constants C_ZZMATN1 type FELDNAME value 'ZZMATN1' ##NO_TEXT.
ENDCLASS.



CLASS ZCL_CRS_ERP_MAPIN_ID_SAVEREPL IMPLEMENTATION.


  METHOD /scwm/if_ex_mapin_id_saverepl~mapin.
**********************************************************************
*& Key           : AD-230912
*& Request No.   : 230131-091053-TS - SAP EWM Pilot - Realize Phase (01) - Development
**********************************************************************
*& Description (short)
*& Interface Method for BAdI Implementation /SCWM/EX_ERP_MAPIN_ID_SAVEREPL
*&
**********************************************************************
    BREAK-POINT ID zcg_ex_erp_mapin_id_saverepl.

    DATA(lo_delivery_check) = NEW zcl_int_delivery_check(  ).

    LOOP AT ct_dlv_request ASSIGNING FIELD-SYMBOL(<ls_dlv_request>).
      " Barcode and Inbound Shipment Number
      DATA(lv_barcode) = VALUE zde_barcode( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_barcode ]-value OPTIONAL ).
      DATA(lv_entitled) = VALUE /scwm/de_entitled( <ls_dlv_request>-t_item[ 1 ]-s_sapext-entitled OPTIONAL ).

      IF lo_delivery_check->barcode_is_valid( EXPORTING iv_barcode = lv_barcode iv_entitled = lv_entitled ).
        <ls_dlv_request>-s_head-s_eew-zzbarcode = lv_barcode.
      ELSE.
        CLEAR <ls_dlv_request>-s_head-s_eew-zzbarcode.
      ENDIF.

      DATA(lv_shipmentnumber) = VALUE zde_inbship( it_bapi_extension2[ param = <ls_dlv_request>-s_head-t_keymap_head[ 1 ]-docno row = 0 field = c_inbship ]-value OPTIONAL ).
      IF lo_delivery_check->shipmentnumber_is_valid( lv_shipmentnumber ).
        <ls_dlv_request>-s_head-s_eew-zzinbship = lv_shipmentnumber.
      ELSE.
        CLEAR <ls_dlv_request>-s_head-s_eew-zzinbship.
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
              CLEAR: lr_dlv_item->s_product-productid, lr_dlv_item->s_product-product_text, lr_dlv_item->s_product-productno, lr_dlv_item->s_product-productno_ext.
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
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
