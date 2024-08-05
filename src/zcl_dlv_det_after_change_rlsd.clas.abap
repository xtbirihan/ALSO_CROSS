class ZCL_DLV_DET_AFTER_CHANGE_RLSD definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_DLV_DET_AFTER_CHAN .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.

  methods DLV_QUERY
    importing
      !IV_DOCCAT type /SCDL/DL_DOCCAT
      !IT_RELEVANT_KEYS type /SCWM/DLV_DOCID_TAB
      !IO_DELIVERY type ref to /SCWM/CL_DLV_MANAGEMENT_PRD
    exporting
      !ET_HEADERS type /SCWM/DLV_HEADER_OUT_PRD_TAB
      !ET_ITEMS type /SCWM/DLV_ITEM_OUT_PRD_TAB .
ENDCLASS.



CLASS ZCL_DLV_DET_AFTER_CHANGE_RLSD IMPLEMENTATION.


  METHOD /scwm/if_ex_dlv_det_after_chan~check_header.
**********************************************************************
*& Key           : rmanova-230911
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*&
**********************************************************************
*    BREAK-POINT ID zcg_badi.
*    BREAK-POINT ID zcg_ord_release.
*
*    IF zcl_switch=>get_switch_state( iv_lgnum = /scwm/cl_tm=>sv_lgnum
*                                     iv_devid = zif_switch_const=>c_zcross_006 ) EQ abap_false.
*      RETURN.
*    ENDIF.
*
*    ct_relevant_keys = it_keys.

  ENDMETHOD.


  METHOD /scwm/if_ex_dlv_det_after_chan~check_item.
**********************************************************************
*& Key           : rmanova-230911
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*& Empty implementation
**********************************************************************

  ENDMETHOD.


  METHOD /scwm/if_ex_dlv_det_after_chan~execute_header.
**********************************************************************
*& Key           : rmanova-230911
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*&
**********************************************************************
**    DATA:
**      lv_black_flag TYPE abap_bool,
**      lv_to_release TYPE abap_bool,
**      lo_delivery   TYPE REF TO /scwm/cl_dlv_management_prd.
**
**    DATA:
**      ls_eew 	  TYPE /scdl/incl_eew_dlv_head_str,
**      lo_bom    TYPE REF TO /scdl/cl_bo_management,
**      lo_bo     TYPE REF TO /scdl/if_bo,
**      lo_header TYPE REF TO /scdl/cl_dl_header_prd,
**      lo_item   TYPE REF TO /scdl/cl_dl_item_prd.
**
**    BREAK-POINT ID zcg_badi.
**    BREAK-POINT ID zcg_ord_release.
**
**    IF zcl_switch=>get_switch_state( iv_lgnum = /scwm/cl_tm=>sv_lgnum
**                                     iv_devid = zif_switch_const=>c_zcross_006 ) EQ abap_false.
**      RETURN.
**    ENDIF.
**
**    IF iv_doccat <> /scdl/if_dl_doc_c=>sc_doccat_out_prd.
**      RETURN.
**    ENDIF.

**    DATA(lt_cond_sec) = NEW zcl_crud_ztcross_cond_asc( )->select_multi_by_lgnum( iv_lgnum = /scwm/cl_tm=>sv_lgnum ).
**
**    IF lt_cond_sec IS INITIAL.
**      RETURN.
**    ENDIF.
**
**    lo_delivery = /scwm/cl_dlv_management_prd=>get_instance( ).
**
**    " get BOM
**    lo_bom = /scdl/cl_bo_management=>get_instance( ).
**
**    LOOP AT it_relevant_keys ASSIGNING FIELD-SYMBOL(<ls_rel_key>).
**
**      lo_bo = lo_bom->get_bo_by_id( iv_docid = <ls_rel_key>-docid ).
**
**      lo_header ?= lo_bo->get_header( ).
**      IF NOT lo_header IS BOUND.
**        CONTINUE.
**      ENDIF.
**
**      DATA(ls_hdr_eew) = lo_header->get_eew( ).
**
**      IF ls_hdr_eew-zzreleased = abap_true.
**        " If Released flag is populated skip this check
**        CONTINUE.
**      ENDIF.
**
**      " read item IDs
**      lo_bo->get_item_tab(
**        EXPORTING
**          iv_docid = <ls_rel_key>-docid
**        IMPORTING
**          et_item = DATA(lt_items) ).
**
**      IF lt_items IS INITIAL.
**        CONTINUE.
**      ENDIF.
**
**      " Sales organisation
**      DATA(lv_sales_org)    = ls_hdr_eew-zzvkorg.
**      " Order reason
**      DATA(lv_ord_reas)     = ls_hdr_eew-zzaugru.
**      " Follow-up delivery
**      DATA(lv_followup_dlv) = ls_hdr_eew-zznali.
**      " Value of delivery
**      DATA(lv_dlv_val)      = ls_hdr_eew-zzcomm_hdr_value.
**      " Logistics control flag
**      DATA(lv_lod_cond)     = ls_hdr_eew-zzlogcon.
**      " Delivery type (return to vendor)
**      DATA(lv_dlv_type)     = lo_header->mv_doctype.
**
**      LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
**        lo_item ?= <ls_item>-item.
**
**        DATA(ls_item) = lo_item->get_eew( ).
**        lo_item->get_sapext( IMPORTING es_sapext_o = DATA(ls_item_sapext_o) ).
**        DATA(ls_delterm) = lo_item->get_delterm( ).
**        DATA(ls_itm_prod) = lo_item->get_product( ).
**
**        " Sales office
**        DATA(lv_sales_off) = ls_item-zzvkbur.
**        " Shipping condition
**        DATA(lv_ship_cond) = ls_delterm-service_level.
**        " KEP or SPED
**        DATA(lv_wpt) = ls_item_sapext_o-/scwm/procty.
**        " Product number
**        DATA(lv_product)   = ls_itm_prod-productno.
**
**        " Check blacklist table
**        LOOP AT lt_cond_sec ASSIGNING FIELD-SYMBOL(<ls_cond_sec>).
**          CLEAR lv_black_flag.
**
**          DATA(lt_blacklist) = NEW zcl_crud_ztcross_blackcnd( )->select_multi_by_keys_acc_sec(
**                                    iv_lgnum         = /scwm/cl_tm=>sv_lgnum
**                                    is_cond_asc      = <ls_cond_sec>
**                                    iv_sales_org     = lv_sales_org
**                                    iv_sales_off     = lv_sales_off
**                                    iv_ship_cond     = lv_ship_cond
**                                    iv_ord_reason    = lv_ord_reas
**                                    iv_follow_up_dlv = lv_followup_dlv
**                                    iv_wpt           = lv_wpt
**                                    iv_dlv_val       = lv_dlv_val
**                                    iv_cutofftime    = '' " to finish
**                                    iv_log_cond      = lv_lod_cond
**                                    iv_dlv_type      = lv_dlv_type
**                                    iv_product       = lv_product
**                                     ).
**
**          IF lt_blacklist IS NOT INITIAL.
**            lv_black_flag = abap_true.
**            EXIT.
**          ENDIF.
**        ENDLOOP.
**
**        IF lv_black_flag = abap_true.
**          lv_to_release = abap_false.
**          " Do not check the other items. If one of them is a match dlv header ZZRELEASED is empty
**          EXIT.
**        ENDIF.
**
**        " Dlv to be released
**        lv_to_release = abap_true.
**      ENDLOOP.
**
**      IF lv_to_release = abap_false.
**        " Do not update dlv. contienue with the next one
**        CONTINUE.
**      ENDIF.
**
**      " Update dlv header ZZRELEASED = X
**      ls_eew = lo_header->get_eew( ).
**
**      ls_eew-zzreleased = abap_true.
**      lo_header->set_eew( is_eew = ls_eew ).
**
**      CLEAR lv_to_release.
**    ENDLOOP.

  ENDMETHOD.


  METHOD /scwm/if_ex_dlv_det_after_chan~execute_item.
**********************************************************************
*& Key           : rmanova-230911
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*& Empty implementation
**********************************************************************

  ENDMETHOD.


  METHOD dlv_query.
**********************************************************************
*& Key           : rmanova-230911
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*&
**********************************************************************

    DATA:
      ls_read_options TYPE /scwm/dlv_query_contr_str,
      ls_include_data TYPE /scwm/dlv_query_incl_str_prd,
      lt_message      TYPE /scdl/dm_message_tab,
      lo_message      TYPE REF TO /scwm/cl_dm_message_no.

    ls_read_options-data_retrival_only      = abap_true.
    ls_read_options-mix_in_object_instances = abap_true.

    ls_include_data-head_refdoc  = abap_true.
    ls_include_data-item_refdoc  = abap_true.

    TRY.
        io_delivery->query(
          EXPORTING
            iv_doccat       = iv_doccat
            it_selection    = VALUE #( FOR ls_keys IN it_relevant_keys ( fieldname = /scdl/if_dl_logfname_c=>sc_docid_h
                                                                         sign      = wmegc_sign_inclusive
                                                                         option    = wmegc_option_eq
                                                                         low       = ls_keys-docid )  )
            is_read_options = ls_read_options
            is_include_data = ls_include_data
          IMPORTING
            et_headers      = et_headers
            et_items        = et_items
            eo_message      = lo_message
            ).

        IF lo_message IS BOUND.
          lt_message = lo_message->get_messages( ).
        ENDIF.

      CATCH /scdl/cx_delivery INTO DATA(lx_delivery).
        IF lx_delivery->mo_message IS BOUND.
          lo_message->add( lx_delivery->mo_message ).
        ENDIF.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
