CLASS zcl_crs_erp_mapout_od_confdec DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_mapout_od_confdec .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
PRIVATE SECTION.

  CONSTANTS:
    BEGIN OF c_fields,
      type   TYPE  datatype_d VALUE 'CHAR',
      length TYPE  ddleng VALUE 3,
    END OF c_fields ,
    BEGIN OF c_doccat,
      ero TYPE /scdl/dl_refdoccat VALUE 'ERO',
    END OF c_doccat .
  METHODS fill_extension2_for_identabs
    IMPORTING
      !is_dlv_info           TYPE /scdl/af_dlv_str
      !is_bapi_techn_control TYPE /scwm/bapidlvcontrol
    CHANGING
      !ct_bapi_extension2    TYPE /scwm/tt_extension2 .
  METHODS get_ewm_warehouse_number
    IMPORTING
      !iv_whnumerp       TYPE /scwm/de_lgnumerp
      !iv_logsys         TYPE logsys
    RETURNING
      VALUE(rv_whnumwme) TYPE /scwm/lgnum .
ENDCLASS.



CLASS ZCL_CRS_ERP_MAPOUT_OD_CONFDEC IMPLEMENTATION.


  METHOD /scwm/if_ex_mapout_od_confdec~mapout.
********************************************************************
*& Key          : TBIRIHAN-Jan 25, 2024
*& Request No.  : GAP-047 Cross Mixing Blending
********************************************************************
*& Description  :
*&
*&
********************************************************************
    BREAK-POINT ID zcg_ex_erp_mapout_od_confdec.

    fill_extension2_for_identabs(
      EXPORTING
        is_dlv_info           = is_dlv_info
        is_bapi_techn_control = is_bapi_techn_control
      CHANGING
        ct_bapi_extension2    = ct_bapi_extension2
    ).

    "Replace substitute product
    LOOP AT ct_bapi_handling_unit_item REFERENCE INTO DATA(lr_hu_itm).
      LOOP AT is_dlv_info-t_item REFERENCE INTO DATA(lr_itm).
        READ TABLE lr_itm->t_refdoc REFERENCE INTO DATA(lr_refdoc)
             WITH KEY refdoccat = /scdl/if_dl_doc_c=>sc_doccat_erp.
        IF sy-subrc EQ 0 AND lr_refdoc->refdocno EQ lr_hu_itm->deliv_numb
                         AND lr_refdoc->refitemno EQ lr_hu_itm->deliv_item.
          lr_hu_itm->material_long = lr_itm->s_eew-zzmaterp.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    LOOP AT CT_BAPI_ITEM_DATA REFERENCE INTO DATA(lr_bapi_itm).
      LOOP AT is_dlv_info-t_item REFERENCE INTO lr_itm.
        READ TABLE lr_itm->t_refdoc REFERENCE INTO lr_refdoc
             WITH KEY refdoccat = /scdl/if_dl_doc_c=>sc_doccat_erp.
        IF sy-subrc EQ 0 AND lr_refdoc->refdocno EQ lr_bapi_itm->deliv_numb
                         AND lr_refdoc->refitemno EQ lr_bapi_itm->deliv_item.
          lr_bapi_itm->material_external = lr_itm->s_eew-zzmaterp.
          lr_bapi_itm->material_long = lr_itm->s_eew-zzmaterp.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD  fill_extension2_for_identabs.
********************************************************************
*& Key          : TBIRIHAN-Jan 25, 2024
*& Request No.  : GAP-047 Cross Mixing Blending
********************************************************************
*& Description  :
*&
*&
********************************************************************
    DATA(rv_whnumwme) = get_ewm_warehouse_number( iv_whnumerp = CONV #( is_bapi_techn_control-recv_whs_no )
                                                  iv_logsys   = is_bapi_techn_control-recv_sys ).

    IF rv_whnumwme IS INITIAL.
      RETURN.
    ENDIF.

    IF zcl_switch=>get_switch_state( iv_lgnum = rv_whnumwme
                                     iv_devid = zif_switch_const=>c_zout_012 ) EQ abap_false.
      RETURN.
    ENDIF.

    SELECT z~* FROM ztcross_cap_nums  AS z
             INNER JOIN @is_dlv_info-t_item AS d ##ITAB_KEY_IN_SELECT
                     ON z~docno  = d~v_refdocno
                    AND z~itemno = d~v_refitemno
             WHERE  z~lgnum = @rv_whnumwme
             INTO TABLE @DATA(lt_cross).

    SORT lt_cross BY lgnum docno itemno.
    LOOP AT is_dlv_info-t_item ASSIGNING FIELD-SYMBOL(<ls_item>).

      DATA(ls_erp_document) = VALUE #( <ls_item>-t_refdoc[ refdoccat = c_doccat-ero ] OPTIONAL ).
      IF ls_erp_document IS INITIAL.
        CONTINUE.
      ENDIF.
      ct_bapi_extension2 = VALUE #( BASE ct_bapi_extension2
                                    FOR ls_cross IN lt_cross
                                    WHERE ( docno  = <ls_item>-v_refdocno
                                      AND   itemno = <ls_item>-v_refitemno )
                                          ( param   = ls_erp_document-refdocno
                                            row     = ls_erp_document-refitemno
                                            type    = c_fields-type
                                            length  = c_fields-length
                                            field   = ls_cross-serial
                                            value   = ls_cross-id_type ) ).
      CLEAR: ls_erp_document.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_ewm_warehouse_number.
********************************************************************
*& Key          : TBIRIHAN-Jan 25, 2024
*& Request No.  : GAP-047 Cross Mixing Blending
********************************************************************
*& Description  :
*&
*&
********************************************************************
    SELECT SINGLE * FROM /scmb/tbussys
        WHERE logsys = @iv_logsys
        INTO  @DATA(ls_tbussys).
    IF sy-subrc EQ 0.
      SELECT SINGLE whnumwme FROM /scwm/tmapwhnum
        WHERE whnumerp = @iv_whnumerp
          AND erpbskey = @ls_tbussys-bskey
        INTO @rv_whnumwme.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
