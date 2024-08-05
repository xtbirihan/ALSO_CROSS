FUNCTION z_cross_bp_customer_events.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_FCODE) TYPE  TBZ4-FCODE
*"     REFERENCE(I_CRSFIELD) TYPE  BUS000TBFL-TBFLD
*"     REFERENCE(I_CRSLINE) TYPE  SY-LILLI
*"  EXPORTING
*"     REFERENCE(E_XHANDLE) TYPE  CLIKE
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Handle the checkbox changes in BP transactions. If a checkbox is changed
*& then a function code is triggered, which is handled here
**********************************************************************
  DATA: ls_but000 TYPE but000.
  CONSTANTS: lc_fcode_vas TYPE syucomm VALUE 'ZZ_VAS'.
  CONSTANTS: lc_fcode_def_addres TYPE syucomm VALUE 'ZZ_ADD'.

  e_xhandle = abap_true.


  CASE i_fcode.

    WHEN lc_fcode_vas.
      IF gs_but000-zz_is_vas_customer EQ abap_false.
        CALL FUNCTION 'BUP_BUPA_BUT000_GET'
          IMPORTING
            e_but000 = ls_but000.

        CLEAR ls_but000-zz_vas_text.

        CALL FUNCTION 'BUP_BUPA_BUT000_COLLECT'
          EXPORTING
            i_subname = 'CI_EEW_BUT000'
            i_but000  = ls_but000.
      ENDIF.
    WHEN lc_fcode_def_addres.
      CALL FUNCTION 'BUP_BUPA_BUT000_GET'
        IMPORTING
          e_but000 = ls_but000.

      IF gs_but000-zz_use_dc_ra EQ abap_true.

        CLEAR: ls_but000-zz_cover, ls_but000-zz_return.


      ELSE.
        DATA: lt_addr TYPE STANDARD TABLE OF busadrdata.
        CALL FUNCTION 'BUA_BUPA_ADDRESSES_GET'
          EXPORTING
            i_xall    = abap_false
          TABLES
            t_address = lt_addr.
        IF lt_addr IS NOT INITIAL.
          DATA(ls_addr) = lt_addr[ 1 ].
          ls_but000-zz_cover-zz_city1      = ls_addr-city1.
          ls_but000-zz_cover-zz_country    = ls_addr-country.
          ls_but000-zz_cover-zz_house_num1 = ls_addr-house_num1.
          ls_but000-zz_cover-zz_post_code1 = ls_addr-post_code1.
          ls_but000-zz_cover-zz_street     = ls_addr-street.
        ENDIF.
      ENDIF.
      CALL FUNCTION 'BUP_BUPA_BUT000_COLLECT'
        EXPORTING
          i_subname = 'CI_EEW_BUT000'
          i_but000  = ls_but000.
    WHEN OTHERS.
      e_xhandle = abap_false.
  ENDCASE.

ENDFUNCTION.
