class ZCL_CROSS_SLOT_REP_QTY definition
  public
  final
  create public .

public section.

  interfaces IF_BADI_INTERFACE .
  interfaces /SCWM/IF_EX_SLOT_DET_REPQTY .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CROSS_SLOT_REP_QTY IMPLEMENTATION.


  METHOD /SCWM/IF_EX_SLOT_DET_REPQTY~DETERMINE_REPLQUAN.
**********************************************************************
*& Key         : <AA>-290523
*& Request No. : TE1K900125 Main Request / GAP-069 - Maximum_qty_in_slotting
**********************************************************************
*& Description (short)
*& Manipulate the maximum storage type quantity,
*& minimum storage type quantity and the minimum replenishment quantity of a product
**********************************************************************

    DATA: ls_t331  TYPE /scwm/t331,
          lv_matnr TYPE matnr.

    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_det_replquan.

    IF zcl_switch=>get_switch_state( EXPORTING iv_lgnum = is_mat_lgtyp-lgnum
                                               iv_devid = zif_switch_const=>c_zcross_004 ) EQ abap_false.
      RETURN.
    ENDIF.

    DATA(lv_lptyp) = zcl_slot_lptyp=>get_lptyp( ).
    DATA(lv_matid) = is_mat_lgnum-matid.

    CALL FUNCTION 'CONVERSION_EXIT_MDLPD_OUTPUT'
      EXPORTING
        input  = is_mat_lgtyp-matid
      IMPORTING
        output = lv_matnr.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input  = lv_matnr
      IMPORTING
        output = lv_matnr.

    CALL FUNCTION '/SCWM/T331_READ_SINGLE'
      EXPORTING
        iv_lgnum  = is_mat_lgtyp-lgnum
        iv_lgtyp  = is_mat_lgtyp-lgtyp
      IMPORTING
        es_t331   = ls_t331
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF ls_t331-huobl EQ abap_true.

      DATA(ls_marm) = zcl_crud_marm=>select_single_by_key(
        EXPORTING
          iv_matnr = lv_matnr
          iv_uom   = zif_wme_c=>gs_uom-pal ).

      ev_repqty = ls_marm-umrez.
      ev_maxqty = ev_repqty * 2.
      ev_minqty = ls_marm-umrez.

    ELSE.

      zcl_crud_ztlptyp_maxqty=>select_single_by_all_filters(
        EXPORTING
          iv_lgnum          = is_mat_lgtyp-lgnum                 " Warehouse Number/Warehouse Complex
          iv_lgtyp          = is_mat_lgtyp-lgtyp                 " Storage Type
          iv_lptyp          = lv_lptyp                 " Storage Bin Type
          iv_matid          = lv_matid                 " Product
        IMPORTING
          es_ztlptyp_maxqty = DATA(ls_lptyp_maxqty)
      ).

      ev_maxqty = COND #( WHEN ls_lptyp_maxqty IS NOT INITIAL
                               THEN ls_lptyp_maxqty-max_qty
                               ELSE 1 ).

      zcl_crud_ztlgtyp_algo=>select_single_by_lgnum_lgtyp(
        EXPORTING
          iv_lgnum        = is_mat_lgtyp-lgnum                " Warehouse Number/Warehouse Complex
          iv_lgtyp        = is_mat_lgtyp-lgtyp                " Storage Type
        IMPORTING
          es_ztlgtyp_algo = DATA(ls_lgtyp_algo)                " Capacity calculation algorithms for storage types
      ).

      CASE ls_lgtyp_algo-algo.
        WHEN zif_wme_c=>gs_algorithms-cuboid.

          zcl_crud_matlwh=>select_single_by_matid(
            EXPORTING
              iv_matid  = lv_matid                 " Product
            IMPORTING
              rs_matlwh = DATA(ls_matlwh)                 " Location Product for Location Warehouse
          ).

          " <AA>-121223
          IF ls_matlwh-zz1_keepcar_whd EQ abap_true.

            IF is_mat_lgtyp-zz1_dirrpl_stt EQ abap_true
              OR zcl_algorithm_facade=>check_pack_mat_is_tote( iv_lgnum = is_mat_lgnum-lgnum
                                                               iv_matid = lv_matid ) <> abap_true.

              ev_repqty = VALUE #( it_packinfo[ quancla = zif_wme_c=>gs_quancla-totes ]-quantity OPTIONAL ).
            ELSE.
              ls_marm = zcl_crud_marm=>select_single_by_key(
                EXPORTING
                  iv_matnr = lv_matnr
                  iv_uom   = zif_wme_c=>gs_uom-z00
              ).

              ev_repqty = ls_marm-umrez.
            ENDIF.
            ev_minqty = ev_repqty.
          ELSE.
            ev_minqty = iv_minqty.
            ev_repqty = 1.
          ENDIF.
          " <AA>-121223

        WHEN zif_wme_c=>gs_algorithms-flowrack.

          ev_repqty = VALUE #( it_packinfo[ quancla = zif_wme_c=>gs_quancla-totes ]-quantity OPTIONAL ).
          ev_minqty = ev_repqty.

        WHEN OTHERS.
      ENDCASE.

    ENDIF.

  ENDMETHOD.
ENDCLASS.
