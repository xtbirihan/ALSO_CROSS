FUNCTION z_cross_lgpla_update.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IT_LTAP_VB) TYPE  /SCWM/TT_LTAP_VB
*"     VALUE(IV_ENTITLED) TYPE  /SCWM/DE_ENTITLED
*"----------------------------------------------------------------------
********************************************************************
*& Key          : <AAHMEDOV>-11.05.2023 10:39:15
*& Request No.  :
********************************************************************
*& Description  : Update the storage bin capacity
*                 when there is an open WT to this storage bin
********************************************************************
  DATA: lt_lagp         TYPE /scwm/tt_lagp,
        lt_lagp_update  TYPE /scwm/tt_lagp,
        lt_lptyp_maxqty TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty,
        lt_lptyp_matid  TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty,
        lt_mapping      TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty,
        lt_bapiret      TYPE  bapiret2_t.
*        lt_lagp_key     TYPE /scwm/tt_lagp_key.

  IF it_ltap_vb IS INITIAL OR
    iv_lgnum IS INITIAL
      OR iv_entitled IS INITIAL.
    RETURN.
  ENDIF.

  DATA(lt_ltap_vb) = it_ltap_vb.

  zcl_crud_ztlgtyp_algo=>select_multi_by_lgnum_lgtyp(
    EXPORTING
      iv_lgnum        = iv_lgnum                " Table Type for Warehouse Number/Warehouse Complex
      it_lgtyp_r      = VALUE #( FOR GROUPS OF <lgtyp> IN it_ltap_vb
                                  GROUP BY <lgtyp>-nltyp
                                      (  sign = wmegc_sign_inclusive
                                         option = wmegc_option_eq
                                         low =  <lgtyp>-nltyp  ) )                " Range Table Type for Field Name LGTYP
    IMPORTING
      et_ztlgtyp_algo = DATA(lt_lgtyp_algo)
  ).

  IF lt_lgtyp_algo IS INITIAL.
    RETURN.
  ENDIF.

  DATA(lt_lgtyp_r) = VALUE /scwm/tt_lgtyp_r( FOR <lgtyp_algo> IN lt_lgtyp_algo
                                                ( sign = wmegc_sign_inclusive
                                                  option = wmegc_option_eq
                                                  low = <lgtyp_algo>-lgtyp ) ).

  DELETE lt_ltap_vb WHERE nltyp NOT IN lt_lgtyp_r.

  IF lt_ltap_vb IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION '/SCWM/LAGP_READ_MULTI'
    EXPORTING
      it_lgpla = VALUE /scwm/tt_lagp_key( FOR GROUPS OF <lgpla> IN lt_ltap_vb
                                          GROUP BY <lgpla>-nlpla
                                          ( lgnum = <lgpla>-lgnum
                                            lgpla =  <lgpla>-nlpla ) )
    IMPORTING
      et_lagp  = lt_lagp.

  LOOP AT lt_ltap_vb ASSIGNING FIELD-SYMBOL(<ls_ltap_vb>).

    CHECK NOT line_exists( lt_mapping[ lgnum = <ls_ltap_vb>-lgnum
                                       lgtyp = <ls_ltap_vb>-nltyp
                                       lptyp = lt_lagp[ lgpla = <ls_ltap_vb>-nlpla ]-lptyp
                                       matid = <ls_ltap_vb>-matid ] ).

    INSERT VALUE #(  lgnum = <ls_ltap_vb>-lgnum
                     lgtyp = <ls_ltap_vb>-nltyp
                     lptyp = lt_lagp[ lgpla = <ls_ltap_vb>-nlpla ]-lptyp
                     matid = <ls_ltap_vb>-matid ) INTO TABLE lt_mapping.

  ENDLOOP.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  LOOP AT lt_mapping ASSIGNING FIELD-SYMBOL(<ls_mapping>).

    zcl_algorithm_facade=>determine_bintyp_capacity(
      EXPORTING
        iv_lgnum        = iv_lgnum
        it_entitled     = VALUE #( ( entitled = iv_entitled ) )
        iv_query_db_qty = abap_true
        is_select_crit  = VALUE #( lgtyp_r = VALUE /scwm/tt_lgtyp_r( ( sign = wmegc_sign_inclusive
                                                                       option = wmegc_option_eq
                                                                       low = <ls_mapping>-lgtyp ) )
                                   lptyp_r = VALUE /scwm/tt_lptyp_r( FOR <lptyp> IN lt_mapping
                                                                     WHERE ( matid = <ls_mapping>-matid )
                                                                     ( sign = wmegc_sign_inclusive
                                                                       option = wmegc_option_eq
                                                                       low = <ls_mapping>-lptyp ) )
                                   matid_r = VALUE /scwm/tt_matid_r( ( sign = wmegc_sign_inclusive
                                                                       option = wmegc_option_eq
                                                                       low = <ls_mapping>-matid ) ) )
      IMPORTING
        et_lptyp_maxqty = lt_lptyp_matid
    ).

    INSERT LINES OF lt_lptyp_matid INTO TABLE lt_lptyp_maxqty.

  ENDLOOP.

  IF lt_lptyp_maxqty IS INITIAL.
    RETURN.
  ENDIF.

  LOOP AT lt_lagp ASSIGNING FIELD-SYMBOL(<ls_lagp>).

    CHECK <ls_lagp>-max_capa <> lt_lptyp_maxqty[ lgnum = <ls_lagp>-lgnum
                                                 lgtyp = <ls_lagp>-lgtyp
                                                 lptyp = <ls_lagp>-lptyp ]-max_qty.

    APPEND INITIAL LINE TO lt_lagp_update ASSIGNING FIELD-SYMBOL(<ls_lagp_update>).

    <ls_lagp_update> = CORRESPONDING #( <ls_lagp> ).
    <ls_lagp_update>-max_capa = VALUE #( lt_lptyp_maxqty[ lgnum = <ls_lagp>-lgnum
                                                   lgtyp = <ls_lagp>-lgtyp
                                                   lptyp = <ls_lagp>-lptyp ]-max_qty OPTIONAL ).

  ENDLOOP.

  IF lt_lagp_update IS INITIAL.
    RETURN.
  ENDIF.

  CALL FUNCTION '/SCWM/LAGP_MAINTAIN'
    EXPORTING
      iv_lgnum             = iv_lgnum
      it_lagp              = VALUE /scwm/tt_lagp_maintain( FOR <lagp> IN lt_lagp_update
                                           WHERE ( max_capa > 0 )
                                       ( CORRESPONDING #( <lagp> ) ) )
      iv_action            = wmegc_update
      iv_commit            = abap_true
    IMPORTING
      et_bapiret           = lt_bapiret
    EXCEPTIONS
      data_invalid         = 1
      action_not_supported = 2
      error                = 3
      OTHERS               = 4.

  IF sy-subrc <> 0.
    MESSAGE ID '/SCWM/L3'
                TYPE wmegc_severity_err
                NUMBER 302.
  ELSEIF line_exists( lt_bapiret[ type = wmegc_severity_err ] ).
    DATA(ls_bapiret) = lt_bapiret[ type = wmegc_severity_err ].
    MESSAGE ID ls_bapiret-id
         TYPE ls_bapiret-type
         NUMBER ls_bapiret-number.
  ENDIF.

ENDFUNCTION.
