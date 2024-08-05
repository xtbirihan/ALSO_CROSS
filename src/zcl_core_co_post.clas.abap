class ZCL_CORE_CO_POST definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_CO_POST .
  interfaces IF_BADI_INTERFACE .

  class-methods GET_EMPTY_BINS
    importing
      !IT_LTAP_VB type /SCWM/TT_LTAP_VB
      !IV_LGNUM type /SCWM/LGNUM
    exporting
      !ET_LAGP_EMPTY type /SCWM/TT_LAGP_MAINTAIN .
  class-methods GET_CAPA_BINS
    importing
      !IT_LTAP_VB type /SCWM/TT_LTAP_VB
      !IV_ENTITLED type /SCWM/DE_ENTITLED
      !IV_LGNUM type /SCWM/LGNUM
    exporting
      !ET_LAGP_CAPA type /SCWM/TT_LAGP_MAINTAIN .
  class-methods VALIDATE_NLTYP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_LGTYP_R type /SCWM/TT_LGTYP_R optional
    exporting
      !ET_LGTYP_R type /SCWM/TT_LGTYP_R .
protected section.
private section.

  methods MAINTAIN_MAX_CAPA
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_LTAP_VB type /SCWM/TT_LTAP_VB .
  methods CONFIRM_CONV_WT
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_LTAP_VB type /SCWM/TT_LTAP_VB .
ENDCLASS.



CLASS ZCL_CORE_CO_POST IMPLEMENTATION.


  METHOD /scwm/if_ex_core_co_post~post.
**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-042
*& Author        : Alper Ahmedov
*& Date          : 02.05.2023
*& Description   : GAP-042 CrossTopics_bin_capacity
**********************************************************************
*& Clear Total Capacity of a source storage bin
*&  when it will be empty after confirming a warehouse task
*& and update total capacity of a bin if the destination storage type
*& is in ZTLGTYP_ALGO table
********************************************************************
*& Request No.  : TE1K900125 Main Request / GAP-086
*& Key          : <AAHMEDOV>-23.08.2023
*& Description  : GAP-086_Conveyor System Routing
********************************************************************
*& Get the task with source bin conveyer of the current HU
*& and confirm it, if the destination bin is not with storage role D or I
**********************************************************************

    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_co_post.

    IF zcl_switch=>get_switch_state( EXPORTING iv_lgnum = iv_lgnum
                                               iv_devid = zif_switch_const=>c_zcross_002 ) EQ abap_true.

      maintain_max_capa(
        EXPORTING
          iv_lgnum   = iv_lgnum                 " Warehouse Number/Warehouse Complex
          it_ltap_vb = VALUE /scwm/tt_ltap_vb( FOR <ltap> IN it_ltap_vb
                                            WHERE ( trart = wmegc_trart_put OR
                                                    trart = wmegc_trart_int )
                                            ( CORRESPONDING #( <ltap> ) ) ) ).

    ENDIF.

    IF zcl_switch=>get_switch_state( EXPORTING iv_lgnum = iv_lgnum
                                               iv_devid = zif_switch_const=>c_zcross_005 ) EQ abap_true.

      confirm_conv_wt(
        EXPORTING
          iv_lgnum   = iv_lgnum                 " Warehouse Number/Warehouse Complex
          it_ltap_vb = it_ltap_vb ).                " Table Type: Warehouse Tasks Internal

    ENDIF.

  ENDMETHOD.


  METHOD confirm_conv_wt.

    zcl_param=>get_parameter(
      EXPORTING
        iv_lgnum     = iv_lgnum                 " Warehouse Number/Warehouse Complex
        iv_process   = zif_param_const=>c_zcross_0004                 " Process ID (Specification, Program, BAdI etc.)
        iv_parameter = zif_param_const=>c_lgtyp_conv                 " Parameter ID for process
      IMPORTING
        et_range     = DATA(lt_lgtyp_conv_r)                " SELECT-OPTIONS Table
    ).

    DATA(lt_ltab_vb) = VALUE /scwm/tt_ltap_vb( FOR <ls_ltap_vb> IN it_ltap_vb
                                                WHERE ( nltyp IN lt_lgtyp_conv_r )
                                                ( CORRESPONDING #( <ls_ltap_vb> ) ) ).

    IF lt_ltab_vb IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_qname) = CONV trfcqnam( |{ zif_wme_c=>gs_lgtyp_autoconf-conv }{ iv_lgnum }{ sy-datum }{ sy-uzeit }| ).

    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
      EXPORTING
        qin_name = lv_qname.

    CALL FUNCTION 'Z_CROSS_LGTYP_POST' IN BACKGROUND TASK
      EXPORTING
        iv_lgnum   = iv_lgnum
        it_ltap_vb = lt_ltab_vb.

  ENDMETHOD.


  METHOD get_capa_bins.
    "here we will validate the destination storage bins
    "using table ztlgtyp_algo table-> here are all the storage
    "types, for which there is an algorithm for
    "maximum storage bin type capacity

    DATA: lt_lagp         TYPE /scwm/tt_lagp,
          lt_lptyp_maxqty TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty,
          lt_lptyp_matid  TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty,
          lt_mapping      TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty.

    CLEAR et_lagp_capa.

    zcl_core_co_post=>validate_nltyp(
      EXPORTING
        iv_lgnum   = iv_lgnum
      IMPORTING
        et_lgtyp_r = DATA(lt_nltyp_r)                " Table Type: Warehouse Tasks Internal
    ).

    DATA(lt_ltap_vb) = VALUE /scwm/tt_ltap_vb( FOR <ltap_vb> IN it_ltap_vb
                                                  WHERE ( nltyp IN lt_nltyp_r )
                                                  ( CORRESPONDING #(  <ltap_vb> ) ) ).
    IF lt_ltap_vb IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/LAGP_READ_MULTI'
      EXPORTING
        it_lgpla = VALUE /scwm/tt_lagp_key( FOR GROUPS OF <lgpla> IN it_ltap_vb
                                        WHERE ( nltyp IN lt_nltyp_r )
                                        GROUP BY <lgpla>-nlpla
                                        ( lgnum = <lgpla>-lgnum
                                          lgpla = <lgpla>-nlpla ) )
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

      APPEND INITIAL LINE TO et_lagp_capa ASSIGNING FIELD-SYMBOL(<ls_lagp_update>).

      <ls_lagp_update> = CORRESPONDING #( <ls_lagp> ).
      <ls_lagp_update>-max_capa = VALUE #( lt_lptyp_maxqty[ lgnum = <ls_lagp>-lgnum
                                                     lgtyp = <ls_lagp>-lgtyp
                                                     lptyp = <ls_lagp>-lptyp ]-max_qty OPTIONAL ).

    ENDLOOP.

  ENDMETHOD.


  METHOD get_empty_bins.
    "here we will validate the source storage bins
    "using table ztlgtyp_algo table-> here are all the storage
    "types with which this BAdI has to work
    "and we will check if the storage bin is empty


    DATA: lt_lagp      TYPE /scwm/tt_lagp.

    CLEAR et_lagp_empty.

    zcl_core_co_post=>validate_nltyp(
      EXPORTING
        iv_lgnum   = iv_lgnum
      IMPORTING
        et_lgtyp_r = DATA(lt_vltyp_r)                " Table Type: Warehouse Tasks Internal
    ).

    DATA(lt_ltap_vb) = VALUE /scwm/tt_ltap_vb( FOR <ls_ltap_vb> IN it_ltap_vb
                                                  WHERE ( vltyp IN lt_vltyp_r )
                                                  ( CORRESPONDING #(  <ls_ltap_vb> ) ) ).
    IF lt_ltap_vb IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCWM/LAGP_READ_MULTI'
      EXPORTING
        it_lgpla = VALUE /scwm/tt_lagp_key( FOR GROUPS OF <lgpla> IN lt_ltap_vb
                                          GROUP BY <lgpla>-vlpla
                                          ( lgnum = <lgpla>-lgnum
                                            lgpla = <lgpla>-vlpla ) )
      IMPORTING
        et_lagp  = lt_lagp.

    DELETE lt_lagp WHERE kzler <> abap_true.

    IF lt_lagp IS INITIAL.
      RETURN.
    ENDIF.

    LOOP AT lt_lagp ASSIGNING FIELD-SYMBOL(<ls_lagp>).

      <ls_lagp>-max_capa = 0.

    ENDLOOP.

    APPEND LINES OF
     VALUE /scwm/tt_lagp_maintain( FOR <lagp> IN lt_lagp
                      ( CORRESPONDING #( <lagp> ) ) ) TO et_lagp_empty.

  ENDMETHOD.


  METHOD maintain_max_capa.

    IF it_ltap_vb IS INITIAL.
      RETURN.
    ENDIF.

    "work only with source storage bins
    zcl_core_co_post=>get_empty_bins(
      EXPORTING
        iv_lgnum      = iv_lgnum
        it_ltap_vb    = it_ltap_vb                 " Storage Bin: Key Fields
      IMPORTING
        et_lagp_empty = DATA(lt_lagp_empty)                " Table Type for Changing Storage Bins
    ).

    "work only with destination storage bins
    zcl_core_co_post=>get_capa_bins(
      EXPORTING
        iv_lgnum     = iv_lgnum
        it_ltap_vb   = it_ltap_vb
        iv_entitled  = VALUE #( it_ltap_vb[ 1 ]-entitled OPTIONAL )
      IMPORTING
        et_lagp_capa = DATA(lt_lagp_capa)                 " Table Type for Changing Storage Bins
    ).

    IF lt_lagp_capa IS INITIAL
      AND lt_lagp_empty IS INITIAL.
      RETURN.
    ENDIF.

    DATA(lv_qname) = CONV trfcqnam( |{ zif_wme_c=>gs_bin_capa-update }{ iv_lgnum }{ sy-datum }{ sy-uzeit }| ).

    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
      EXPORTING
        qin_name = lv_qname.

    CALL FUNCTION 'Z_CROSS_LGPLA_MAINTAIN' IN BACKGROUND TASK
      EXPORTING
        iv_lgnum      = iv_lgnum
        it_lagp_capa  = lt_lagp_capa
        it_lagp_empty = lt_lagp_empty.

  ENDMETHOD.


  METHOD validate_nltyp.

    CLEAR et_lgtyp_r.

    zcl_crud_ztlgtyp_algo=>select_multi_by_lgnum_lgtyp(
      EXPORTING
        iv_lgnum        = iv_lgnum               " Table Type for Warehouse Number/Warehouse Complex
        it_lgtyp_r      = it_lgtyp_r           " Range Table Type for Field Name LGTYP
      IMPORTING
        et_ztlgtyp_algo = DATA(lt_lgtyp_algo)
    ).

    et_lgtyp_r = VALUE /scwm/tt_lgtyp_r( FOR <lgtyp_algo> IN lt_lgtyp_algo
                                                  ( sign = wmegc_sign_inclusive
                                                    option = wmegc_option_eq
                                                    low = <lgtyp_algo>-lgtyp ) ).

  ENDMETHOD.
ENDCLASS.
