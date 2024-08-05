*&---------------------------------------------------------------------*
*& Include          ZCROSS_UPDATE_MAXQTY_CL_D
*&---------------------------------------------------------------------*
CLASS lcl_update_maxqty DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS:

      value_request_lgtyp,

      value_request_lptyp,

      value_request_entitled,

      dynp_values_read IMPORTING iv_dyname    TYPE progname
                                 iv_dynnr     TYPE sychar04
                       CHANGING  ct_dynpfield TYPE /scf/dynpread_tab,

      det_shlp IMPORTING iv_tabname     TYPE tabname
                         iv_fieldname   TYPE fieldname
               RETURNING VALUE(rs_shlp) TYPE shlp_descr,

      strt_val_req IMPORTING is_shlp   TYPE shlp_descr
                   CHANGING  ct_values TYPE /scmb/typ_tab_f4,

      dynp_values_update IMPORTING iv_dyname TYPE progname
                                   iv_dynnr  TYPE sychar04
                                   it_values TYPE /scf/dynpread_tab,

      display_message.

    METHODS:

      constructor IMPORTING iv_lgnum    TYPE /scwm/lgnum,

      main,

      validate_lgnum IMPORTING iv_lgnum        TYPE /scwm/lgnum
                     RETURNING VALUE(rv_lgnum) TYPE /scwm/lgnum,

      check_bintype_exists IMPORTING it_bintype_r    TYPE rseloption
                                     it_lptyp_maxqty TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty
                           CHANGING  ct_lptyp_delete TYPE zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty.

  PRIVATE SECTION.

    DATA: mo_cuboid_algorithm TYPE REF TO zcl_cuboid_algorithm,
          mo_mon_prod_service TYPE REF TO /scwm/cl_mon_prod,
          mo_log              TYPE REF TO /scwm/cl_log.

    DATA: mv_lgnum    TYPE /scwm/lgnum.

    CONSTANTS:
      c_p_lgnum_fn         TYPE dynfnam VALUE 'P_LGNUM',
      c_so_entitled_low_fn TYPE dynfnam VALUE 'SO_ENT-LOW',
      c_lgnum_fn           TYPE fieldname VALUE 'LGNUM',
      c_lgtyp_fn           TYPE fieldname VALUE 'LGTYP',
      c_lptyp_fn           TYPE fieldname VALUE 'LPTYP',
      c_entitled_fn        TYPE fieldname VALUE 'ENTITLED',
      c_partner_fn         TYPE fieldname VALUE 'PARTNER',
      c_so_lptyp_low_fn    TYPE dynfnam VALUE 'SO_LPTYP-LOW',
      c_so_lgtyp_low_fn    TYPE dynfnam VALUE 'SO_LGTYP-LOW',
      c_lagp_mon_f4        TYPE tabname VALUE '/SCWM/S_LAGP_MON_F4',
      c_sapapo_f4          TYPE tabname VALUE '/SAPAPO/MATIO_WM'.

ENDCLASS.
