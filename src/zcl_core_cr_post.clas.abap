class ZCL_CORE_CR_POST definition
  public
  final
  create public .

public section.

  interfaces /SCWM/IF_EX_CORE_CR_POST .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.

  methods UPDATE_MAX_CAPA
    importing
      !IV_ENTITLED type /SCWM/DE_ENTITLED
      !IV_LGNUM type /SCWM/LGNUM
      !IT_LTAP_VB type /SCWM/TT_LTAP_VB .
ENDCLASS.



CLASS ZCL_CORE_CR_POST IMPLEMENTATION.


  METHOD /scwm/if_ex_core_cr_post~post.
**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-042
*& Author        : Alper Ahmedov
*& e-mail        : alper.ahmedov@qinlox.com
*& Date          : 02.05.2023
**********************************************************************
*& Update Total Capacity of a destination storage bin
*&  when a putaway task has been created
**********************************************************************

    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_cr_post.

    IF zcl_switch=>get_switch_state( EXPORTING iv_lgnum = iv_lgnum
                                               iv_devid = zif_switch_const=>c_zcross_002 ) EQ abap_true.

      update_max_capa( iv_lgnum    = iv_lgnum
                       it_ltap_vb  = it_ltap_vb
                       iv_entitled = VALUE #( it_ordim_o[ 1 ]-entitled OPTIONAL ) ).

    ENDIF.

  ENDMETHOD.


  METHOD update_max_capa.

    DATA(lv_qname) = CONV trfcqnam( |{ zif_wme_c=>gs_bin_capa-update }{ iv_lgnum }{ sy-datum }{ sy-uzeit }| ).

    CALL FUNCTION 'TRFC_SET_QIN_PROPERTIES'
      EXPORTING
        qin_name = lv_qname.

    CALL FUNCTION 'Z_CROSS_LGPLA_UPDATE' IN BACKGROUND TASK
      EXPORTING
        iv_lgnum    = iv_lgnum
        iv_entitled = iv_entitled
        it_ltap_vb  = VALUE /scwm/tt_ltap_vb( FOR <ltap> IN it_ltap_vb
                                           WHERE ( trart = wmegc_trart_put OR
                                           trart = wmegc_trart_int )
                                           ( CORRESPONDING #( <ltap> ) ) ).

  ENDMETHOD.
ENDCLASS.
