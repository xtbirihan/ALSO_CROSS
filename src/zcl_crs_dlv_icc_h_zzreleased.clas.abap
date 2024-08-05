class ZCL_CRS_DLV_ICC_H_ZZRELEASED definition
  public
  final
  create public .

public section.

  interfaces /SCDL/IF_AF_ICC_FIELD_CHECK .

  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.

  class-data SO_BOM type ref to /SCDL/CL_BO_MANAGEMENT .

  class-methods READ_MARA
    importing
      !IV_PRODUCTNO type MARA-MATNR
    returning
      value(RS_MARA) type MARA .
  class-methods READ_KNA1
    importing
      !IV_CUST_NO type KNA1-KUNNR
    returning
      value(RS_KNA1) type KNA1 .
ENDCLASS.



CLASS ZCL_CRS_DLV_ICC_H_ZZRELEASED IMPLEMENTATION.


  METHOD /scdl/if_af_icc_field_check~check.
**********************************************************************
*& Key           : rmanova-230920
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*& ICC Enhancement Reference Documents Delivery Header check
**********************************************************************
    DATA:
      lv_white_flag TYPE abap_bool,
      lv_doccat	    TYPE /scdl/dl_doccat,
      lv_docid      TYPE /scdl/dl_docid,
      ls_mara       TYPE mara,
      ls_kna1       TYPE kna1,
      ls_symsg      TYPE symsg,
      lt_refdoc     TYPE /scdl/dl_refdoc_tab,
      lo_bo         TYPE REF TO /scdl/if_bo,
      lo_header     TYPE REF TO /scdl/cl_dl_header_prd,
      lo_item       TYPE REF TO /scdl/cl_dl_item_prd.


    lo_bo     = so_bom->get_bo_by_id( iv_docid = iv_docid ).
    lo_header ?= lo_bo->get_header( ).

    IF lo_header IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lt_partyloc_carr) = lo_header->get_partyloc(
                               iv_party_role = /scdl/if_dl_partyloc_c=>sc_party_role_carr ).

    DATA(lv_carr) = VALUE #( lt_partyloc_carr[ 1 ]-partyno OPTIONAL ).
    IF lv_carr IS INITIAL.
      RETURN.
    ENDIF.

    DATA(ls_hdr_eew) = lo_header->get_eew( ).

    IF ls_hdr_eew-zzreleased = abap_true.
      RETURN.
    ENDIF.

    " If the IC is called after the monitor block/unblock method - do not execute the check.
    IF zcl_crs_to_release_dlv_hlp=>get_flag( ) = abap_true AND ls_hdr_eew-zzreleased = abap_false.
      " Error in incompleteness check
      MESSAGE e000(/scdl/icc) INTO DATA(lv_string) ##NEEDED.

      ls_symsg-msgty = sy-msgty.
      ls_symsg-msgid = sy-msgid.
      ls_symsg-msgno = sy-msgno.
      ls_symsg-msgv1 = sy-msgv1.
      ls_symsg-msgv2 = sy-msgv2.

      IF eo_message IS NOT BOUND.
        CREATE OBJECT eo_message.
      ENDIF.

      IF lv_doccat IS INITIAL.
        lv_docid  = lo_bo->get_docid( ).
        lv_doccat = lo_bo->get_doccat( ).
      ENDIF.

      eo_message->add_message(
         iv_msgcat   = /scdl/cl_dm_message=>sc_mcat_bus
         iv_doccat   = lv_doccat
         iv_docid    = lv_docid
         is_symsg    = ls_symsg ).

      RETURN.
    ENDIF.

    DATA(lt_blacklist) = NEW zcl_crud_ztcross_blackcnd( )->select_multi_by_lgnum( iv_lgnum = /scwm/cl_tm=>sv_lgnum ).

    DATA(lt_whitelist) = NEW zcl_crud_ztcross_whitecnd( )->select_multi_by_lgnum( iv_lgnum = /scwm/cl_tm=>sv_lgnum ).

    " read item IDs
    lo_bo->get_item_tab(
      EXPORTING
        iv_docid = iv_docid
      IMPORTING
        et_item = DATA(lt_items) ).

    IF lt_items IS INITIAL.
      RETURN.
    ENDIF.

    " Sales organisation
    DATA(lv_sales_org)    = ls_hdr_eew-zzvkorg.
    " Order reason
    DATA(lv_ord_reas)     = ls_hdr_eew-zzaugru.
    " Follow-up delivery
    DATA(lv_followup_dlv) = ls_hdr_eew-zznali.
    " Value of delivery
    DATA(lv_dlv_val)      = ls_hdr_eew-zzcomm_hdr_value.
    " A Value Currency
    DATA(lv_val_cur)      = ls_hdr_eew-zzcomm_hdr_value_curr.
    " Logistics control flag
    DATA(lv_lod_cond)     = ls_hdr_eew-zzlogcon.
    " Delivery type (return to vendor)
    DATA(lv_dlv_type)     = lo_header->mv_doctype.

    DATA(lt_addmeas) =  lo_header->get_addmeas(
                          iv_qty_role     = /scdl/if_dl_addmeas_c=>sc_qtyrole_volume
                          iv_qty_category = /scdl/if_dl_addmeas_c=>sc_qtycat_gross ).
    DATA(lv_dlv_vol) = VALUE #( lt_addmeas[ 1 ]-qty OPTIONAL ).

    DATA(lt_partyloc_stprt) = lo_header->get_partyloc(
                                iv_party_role = /scdl/if_dl_partyloc_c=>sc_party_role_stprt  ). " Ship-To Party
    DATA(lv_ship_to) = VALUE #( lt_partyloc_stprt[ 1 ]-partyno OPTIONAL ).
    " Calculated Parcels of the Delivery ZZPARCEL_NUM
    DATA(lv_parcel_num) = ls_hdr_eew-zzparcel_num.
    " Calculated Pallets of the Delivery ZZPALLET_NUM
    DATA(lv_pallet_num) = ls_hdr_eew-zzpallet_num.

    LOOP AT lt_items ASSIGNING FIELD-SYMBOL(<ls_item>).
      CLEAR: ls_kna1, ls_mara.

      lo_item ?= <ls_item>-item.

      DATA(ls_item) = lo_item->get_eew( ).
      lo_item->get_sapext( IMPORTING es_sapext_o = DATA(ls_item_sapext_o) ).
      DATA(ls_delterm) = lo_item->get_delterm( ).
      DATA(ls_itm_prod) = lo_item->get_product( ).

      " Sales office
      DATA(lv_sales_off) = ls_item-zzvkbur.
      " Shipping condition
      DATA(lv_ship_cond) = ls_delterm-service_level.
      " KEP or SPED
      DATA(lv_wpt) = ls_item_sapext_o-/scwm/procty.
      " Product number
      DATA(lv_product)   = ls_itm_prod-productno.
      " Customer Group
      DATA(lv_cust_grp1) = ls_item-zzcust_grp1.

      DATA(lt_partyloc_sotprt) = lo_item->get_partyloc(
                                  iv_party_role = /scdl/if_dl_partyloc_c=>sc_party_role_sotprt ). " Sold-to Partner
      DATA(lv_sold_to) = VALUE #( lt_partyloc_sotprt[ 1 ]-partyno OPTIONAL ).

      ls_kna1 = read_kna1( VALUE #( lt_partyloc_stprt[ 1 ]-partyno OPTIONAL ) ).
      ls_mara = read_mara( ls_itm_prod-productno ).

      " Sold-To Party PO; Should be identical for all ODO items.
      lt_refdoc = lo_item->get_refdoc( iv_refdoccat = /scdl/if_dl_doc_c=>sc_doccat_out_poc ).
      DATA(lv_sld_to_prty_po) = VALUE #( lt_refdoc[ 1 ]-refdocno OPTIONAL ).

      " Ship-To Party PO; Should be identical for all ODO items.
      CLEAR lt_refdoc.
      lt_refdoc = lo_item->get_refdoc( iv_refdoccat = /scdl/if_dl_doc_c=>sc_doccat_out_pos ).
      DATA(lv_shp_to_prty_po) = VALUE #( lt_refdoc[ 1 ]-refdocno OPTIONAL ).

      " Check whitelist table
      DO lines( lt_whitelist ) TIMES.
        READ TABLE lt_whitelist INDEX sy-index INTO DATA(ls_whitecnd).

        IF ( ls_whitecnd-sales_org       = lv_sales_org      OR ls_whitecnd-sales_org      = space ) AND
           ( ls_whitecnd-cust_grp        = lv_cust_grp1      OR ls_whitecnd-cust_grp       = space ) AND
           ( ls_whitecnd-sales_off       = lv_sales_off      OR ls_whitecnd-sales_off      = space ) AND
           ( ls_whitecnd-ship_cond       = lv_ship_cond      OR ls_whitecnd-ship_cond      = space ) AND
           ( ls_whitecnd-ord_reason      = lv_ord_reas       OR ls_whitecnd-ord_reason     = space ) AND
           ( ls_whitecnd-follow_up_dlv   = lv_followup_dlv   OR ls_whitecnd-follow_up_dlv  = space ) AND
           ( ls_whitecnd-sld_to_prty     = lv_sold_to        OR ls_whitecnd-sld_to_prty    = space ) AND
           ( ls_whitecnd-shp_to_prty     = lv_ship_to        OR ls_whitecnd-shp_to_prty    = space ) AND
           ( ls_whitecnd-carrier         = lv_carr           OR ls_whitecnd-carrier        = space ) AND
           ( ls_whitecnd-wpt             = lv_wpt            OR ls_whitecnd-wpt            = space ) AND
           ( ls_whitecnd-dst_ctry        = ls_kna1-land1     OR ls_whitecnd-dst_ctry       = space ) AND
           ( ls_whitecnd-dst_pstcode    LE ls_kna1-pstlz     OR ls_whitecnd-dst_pstcode    = space ) AND
           ( ls_whitecnd-dst_pstcode_to GE ls_kna1-pstlz     OR ls_whitecnd-dst_pstcode_to = space ) AND
           ( ls_whitecnd-dlv_val         = lv_dlv_val        OR ls_whitecnd-dlv_val        = space ) AND
           ( ls_whitecnd-value_curr      = lv_val_cur        OR ls_whitecnd-value_curr     = space ) AND
           ( ls_whitecnd-log_cond        = lv_lod_cond       OR ls_whitecnd-log_cond       = space ) AND
           ( ls_whitecnd-dlv_type        = lv_dlv_type       OR ls_whitecnd-dlv_type       = space ) AND
           ( ls_whitecnd-product         = lv_product        OR ls_whitecnd-product        = space ) AND
           ( ls_whitecnd-prdha           = ls_mara-prdha     OR ls_whitecnd-prdha          = space ) AND
           ( ls_whitecnd-dlv_vol         = lv_dlv_vol        OR ls_whitecnd-dlv_vol        = space ) AND
           ( ls_whitecnd-syst_time_from LE sy-uzeit          OR ls_whitecnd-syst_time_from = space ) AND
           ( ls_whitecnd-syst_time_to   GE sy-uzeit          OR ls_whitecnd-syst_time_to   = space ) AND
           ( ls_whitecnd-pallet_num      = lv_pallet_num     OR ls_whitecnd-pallet_num     = space ) AND
           ( ls_whitecnd-parcel_num      = lv_parcel_num     OR ls_whitecnd-parcel_num     = space ) AND
           ( ls_whitecnd-sld_to_prty_po  = lv_sld_to_prty_po OR ls_whitecnd-sld_to_prty_po = space ) AND
           ( ls_whitecnd-shp_to_prty_po  = lv_shp_to_prty_po OR ls_whitecnd-shp_to_prty_po = space ).

          " Cond record exists - release ODO
          lv_white_flag = abap_true.
          EXIT.
        ENDIF.

      ENDDO.

      " Check blacklist table
      DO lines( lt_blacklist ) TIMES.
        READ TABLE lt_blacklist INDEX sy-index INTO DATA(ls_blackcnd).

        IF ( ls_blackcnd-sales_org       = lv_sales_org      OR ls_blackcnd-sales_org      = space ) AND
           ( ls_blackcnd-cust_grp        = lv_cust_grp1      OR ls_blackcnd-cust_grp       = space ) AND
           ( ls_blackcnd-sales_off       = lv_sales_off      OR ls_blackcnd-sales_off      = space ) AND
           ( ls_blackcnd-ship_cond       = lv_ship_cond      OR ls_blackcnd-ship_cond      = space ) AND
           ( ls_blackcnd-ord_reason      = lv_ord_reas       OR ls_blackcnd-ord_reason     = space ) AND
           ( ls_blackcnd-follow_up_dlv   = lv_followup_dlv   OR ls_blackcnd-follow_up_dlv  = space ) AND
           ( ls_blackcnd-sld_to_prty     = lv_sold_to        OR ls_blackcnd-sld_to_prty    = space ) AND
           ( ls_blackcnd-shp_to_prty     = lv_ship_to        OR ls_blackcnd-shp_to_prty    = space ) AND
           ( ls_blackcnd-carrier         = lv_carr           OR ls_blackcnd-carrier        = space ) AND
           ( ls_blackcnd-wpt             = lv_wpt            OR ls_blackcnd-wpt            = space ) AND
           ( ls_blackcnd-dst_ctry        = ls_kna1-land1     OR ls_blackcnd-dst_ctry       = space ) AND
           ( ls_blackcnd-dst_pstcode    LE ls_kna1-pstlz     OR ls_blackcnd-dst_pstcode    = space ) AND
           ( ls_blackcnd-dst_pstcode_to GE ls_kna1-pstlz     OR ls_blackcnd-dst_pstcode_to = space ) AND
           ( ls_blackcnd-dlv_val         = lv_dlv_val        OR ls_blackcnd-dlv_val        = space ) AND
           ( ls_blackcnd-value_curr      = lv_val_cur        OR ls_blackcnd-value_curr     = space ) AND
           ( ls_blackcnd-log_cond        = lv_lod_cond       OR ls_blackcnd-log_cond       = space ) AND
           ( ls_blackcnd-dlv_type        = lv_dlv_type       OR ls_blackcnd-dlv_type       = space ) AND
           ( ls_blackcnd-product         = lv_product        OR ls_blackcnd-product        = space ) AND
           ( ls_blackcnd-prdha           = ls_mara-prdha     OR ls_blackcnd-prdha          = space ) AND
           ( ls_blackcnd-dlv_vol         = lv_dlv_vol        OR ls_blackcnd-dlv_vol        = space ) AND
           ( ls_blackcnd-syst_time_from LE sy-uzeit          OR ls_blackcnd-syst_time_from = space ) AND
           ( ls_blackcnd-syst_time_to   GE sy-uzeit          OR ls_blackcnd-syst_time_to   = space ) AND
           ( ls_blackcnd-pallet_num      = lv_pallet_num     OR ls_blackcnd-pallet_num     = space ) AND
           ( ls_blackcnd-parcel_num      = lv_parcel_num     OR ls_blackcnd-parcel_num     = space ) AND
           ( ls_blackcnd-sld_to_prty_po  = lv_sld_to_prty_po OR ls_blackcnd-sld_to_prty_po = space ) AND
           ( ls_blackcnd-shp_to_prty_po  = lv_shp_to_prty_po OR ls_blackcnd-shp_to_prty_po = space ).

          " Cond record exists - block ODO
          lv_white_flag = abap_false.
          EXIT.
        ENDIF.
      ENDDO.

      IF lv_white_flag = abap_false.
        EXIT. " No need to check the other items
      ENDIF.
    ENDLOOP.

    IF lv_white_flag = abap_false.
      " Error in incompleteness check
      MESSAGE e000(/scdl/icc) INTO lv_string.

      ls_symsg-msgty = sy-msgty.
      ls_symsg-msgid = sy-msgid.
      ls_symsg-msgno = sy-msgno.
      ls_symsg-msgv1 = sy-msgv1.
      ls_symsg-msgv2 = sy-msgv2.

      IF eo_message IS NOT BOUND.
        CREATE OBJECT eo_message.
      ENDIF.

      IF lv_doccat IS INITIAL.
        lv_docid  = lo_bo->get_docid( ).
        lv_doccat = lo_bo->get_doccat( ).
      ENDIF.

      eo_message->add_message(
         iv_msgcat   = /scdl/cl_dm_message=>sc_mcat_bus
         iv_doccat   = lv_doccat
         iv_docid    = lv_docid
         is_symsg    = ls_symsg ).

    ELSE.
      " Update dlv header ZZRELEASED = X
      ls_hdr_eew = lo_header->get_eew( ).

      ls_hdr_eew-zzreleased = abap_true.
      lo_header->set_eew( is_eew = ls_hdr_eew ).
    ENDIF.

  ENDMETHOD.


  METHOD class_constructor.
**********************************************************************
*& Key           : rmanova-230920
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*& Constructor method
**********************************************************************

    so_bom = /scdl/cl_bo_management=>get_instance( ).

  ENDMETHOD.


  METHOD read_kna1.
**********************************************************************
*& Key           : rmanova-230920
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*& Read table knai
**********************************************************************

    CALL FUNCTION 'KNA1_SINGLE_READ'
      EXPORTING
        kna1_kunnr = iv_cust_no
      IMPORTING
        wkna1      = rs_kna1
      EXCEPTIONS
        not_found  = 1
        OTHERS     = 2 ##FM_SUBRC_OK.

  ENDMETHOD.


  METHOD read_mara.
**********************************************************************
*& Key           : rmanova-230920
*& Request No.   : GAP 084 - FD Order release
**********************************************************************
*& Description (short)
*& Read table mara
**********************************************************************

    CALL FUNCTION 'MARA_SINGLE_READ'
      EXPORTING
        matnr             = iv_productno
      IMPORTING
        wmara             = rs_mara
      EXCEPTIONS
        lock_on_material  = 1
        lock_system_error = 2
        wrong_call        = 3
        not_found         = 4
        OTHERS            = 5 ##FM_SUBRC_OK.

  ENDMETHOD.
ENDCLASS.
