*CLASS ltcl_ei_capacheck_maxqty DEFINITION DEFERRED.
*CLASS lcl_mock_data_access DEFINITION DEFERRED.
*CLASS zcl_core_pts_capacheck DEFINITION LOCAL FRIENDS ltcl_ei_capacheck_maxqty.
*
**----------------------------------------------------------------------*
**       CLASS ltcl_Ei_Capacheck_Maxqty DEFINITION
**----------------------------------------------------------------------*
**   Unit Test class for example implementation
**----------------------------------------------------------------------*
*CLASS ltcl_ei_capacheck_maxqty DEFINITION FOR TESTING
*  DURATION MEDIUM
*  RISK LEVEL HARMLESS.
*
*  PUBLIC SECTION.
*    CONSTANTS:
*      sc_lgnum    TYPE /scwm/lgnum      VALUE 'AU01',
*      sc_lgpla1_guid   TYPE /scwm/guid_loc   VALUE '0101A',
*      sc_lgpla2_guid   TYPE /scwm/guid_loc   VALUE '0101B',
*      sc_lgpla3_guid   TYPE /scwm/guid_loc   VALUE '0101C',
*      sc_lgpla4_guid   TYPE /scwm/guid_loc   VALUE '0101D',
*      sc_lgpla5_guid   TYPE /scwm/guid_loc   VALUE '0102A',
*      sc_lgpla6_guid   TYPE /scwm/guid_loc   VALUE '0102B',
*      sc_lgpla7_guid   TYPE /scwm/guid_loc   VALUE '0102D'.
*    CONSTANTS:
*      sc_hu000_guid   TYPE /scwm/guid_hu   VALUE '00000',
*      sc_hu001_guid   TYPE /scwm/guid_hu   VALUE '00001',
*      sc_hu002_guid   TYPE /scwm/guid_hu   VALUE '00002',
*      sc_hu003_guid   TYPE /scwm/guid_hu   VALUE '00003',   "#EC NEEDED
*      sc_hu004_guid   TYPE /scwm/guid_hu   VALUE '00004',   "#EC NEEDED
*      sc_hu005_guid   TYPE /scwm/guid_hu   VALUE '00005'.   "#EC NEEDED
*    CONSTANTS:
*      sc_hu800000046  TYPE /scwm/guid_hu   VALUE '00163EA7249C1EE1B5B4D23F7DB9729D', "#EC NEEDED
*      sc_hu800000047  TYPE /scwm/guid_hu   VALUE '00163EA7249C1EE1B5B4D23F7DBA129D', "#EC NEEDED
*      sc_hu800000104  TYPE /scwm/guid_hu   VALUE '00163EA7249C1EE1B5E4B170B01A92C1', "#EC NEEDED
*      sc_hu800000105  TYPE /scwm/guid_hu   VALUE '00163EA7249C1EE1B5E4B8952DB652C1', "#EC NEEDED
*      sc_hu800000106  TYPE /scwm/guid_hu   VALUE '00163EA7249C1EE1B5E4CA411D8292C1', "#EC NEEDED
*      sc_hu800000107  TYPE /scwm/guid_hu   VALUE '00163EA7249C1EE1B5E4DE5BED3452C1', "#EC NEEDED
*      sc_hu800000108  TYPE /scwm/guid_hu   VALUE '00163EA7249C1EE1B5E4DE5BED3452C2'. "#EC NEEDED
*    CONSTANTS:
*      sc_mat_acme_tab   TYPE /scwm/de_matid   VALUE '005056A202261ED1B5B075B7C392B816', "#EC NEEDED
*      sc_mat_acme_hbd   TYPE /scwm/de_matid   VALUE '005056A202261EE1B4C88909938E36DC', "#EC NEEDED
*      sc_mat_acme_blk   TYPE /scwm/de_matid   VALUE '005056A202261EE1B5E1B4EEFC138807', "#EC NEEDED
*      sc_bin_0201b      TYPE /scwm/guid_loc   VALUE '00163EA7249C1EE1B5AD36EED1F5D294'. "#EC NEEDED
*    CONSTANTS:
*      sc_tanum_1        TYPE /scwm/tanum      VALUE '1000000001'.
*    CONSTANTS:
*      sc_lgpla7         TYPE /SCWM/LGPLA      VALUE 'AUNIT-01-01-O'.
*
*  PRIVATE SECTION.
*    CLASS-DATA:
*      st_lptyp_maxqty TYPE zcl_core_pts_capacheck=>lty_tt_lptyp_maxqty.
*    DATA:
*      mr_maxqty     TYPE REF TO zcl_core_pts_capacheck,  "class under test
*      mr_data_mock  TYPE REF TO lcl_mock_data_access.  "mock object for data access
*    CONSTANTS:
*      sc_lptyp1   TYPE /scwm/lvs_lptyp  VALUE 'AU10',
*      sc_lptyp2   TYPE /scwm/lvs_lptyp  VALUE 'AU20',
*      sc_lptyppml TYPE /scwm/lvs_lptyp  VALUE 'PML'.
*
*    CLASS-METHODS:  class_setup,
*                    class_teardown.
*    METHODS:
*      setup,
*      teardown.
*
*    METHODS:
*      custom_capa_prodwt            FOR TESTING,
*      custom_capa_huwt              FOR TESTING,
*      custom_capa_ordim_o           FOR TESTING,
*      custom_capa_advanced          FOR TESTING,
*      cancel_buffered_prodwt        FOR TESTING,
*      cancel_buffered_huwt          FOR TESTING,
*      maxqty_boundary               FOR TESTING,
*      early_capa_check_prodwt       FOR TESTING,
*      early_capa_check_huwt         FOR TESTING,
*      sel_max_qty_lptyp             FOR TESTING.
*ENDCLASS.                    "ltcl_ei_capacheck_maxqty DEFINITION
*
**----------------------------------------------------------------------*
**       CLASS lcl_mock_data_access DEFINITION
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*CLASS lcl_mock_data_access DEFINITION FOR TESTING.
*  PUBLIC SECTION.
*    INTERFACES:
*      /scwm/if_ei_core_capa_data.
*    ALIASES:
*      get_material_uom  FOR /scwm/if_ei_core_capa_data~get_material_uom,
*      select_stock      FOR /scwm/if_ei_core_capa_data~select_stock,
*      read_hu           FOR /scwm/if_ei_core_capa_data~read_hu,
*      read_to_dest_bin  FOR /scwm/if_ei_core_capa_data~read_to_dest_bin.
*ENDCLASS.                    "lcl_mock_data_access DEFINITION
*
**----------------------------------------------------------------------*
**       CLASS ltcl_Ei_Capacheck_Maxqty IMPLEMENTATION
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*CLASS ltcl_ei_capacheck_maxqty IMPLEMENTATION.
*
*  METHOD class_setup.
*    DATA:
*      ls_maxqty   TYPE zcl_core_pts_capacheck=>lty_s_lptyp_maxqty.
*
*    ls_maxqty-lgnum   = sc_lgnum.
*    ls_maxqty-lptyp   = sc_lptyp1.
*    ls_maxqty-matid   = 'ABC'.
*    ls_maxqty-max_qty = 15.
*    ls_maxqty-uom     = 'EA'.
*    INSERT ls_maxqty INTO TABLE st_lptyp_maxqty.
*    ls_maxqty-lptyp   = sc_lptyp2.
*    ls_maxqty-max_qty = 5.
*    ls_maxqty-uom     = 'EA'.
*    INSERT ls_maxqty INTO TABLE st_lptyp_maxqty.
*    ls_maxqty-lptyp   = sc_lptyppml.
*    ls_maxqty-matid   = sc_mat_acme_tab.
*    ls_maxqty-max_qty = 100.
*    ls_maxqty-uom     = 'BOT'.
*    INSERT ls_maxqty INTO TABLE st_lptyp_maxqty.
*
*  ENDMETHOD.                    "class_setup
*
*  METHOD class_teardown.
*
*  ENDMETHOD.                    "class_teardown
*
*  METHOD setup.
*
*    CREATE OBJECT mr_data_mock.
*
*    CREATE OBJECT mr_maxqty
*      EXPORTING
*        io_data_access = mr_data_mock.
*
*    mr_maxqty->mv_lgnum = sc_lgnum.
*    mr_maxqty->mt_lptyp_maxqty = st_lptyp_maxqty.
*
*
*  ENDMETHOD.       "setup
*
*
*  METHOD teardown.
*    mr_maxqty->/scwm/if_tm_appl~cleanup(
*                  /scmb/if_sp_transaction=>sc_cleanup_end ).
*  ENDMETHOD.       "teardown
*
*
*  METHOD custom_capa_prodwt.
*    DATA ls_t331 TYPE /scwm/t331.
*    DATA ls_request TYPE /scwm/s_pack_request.
*    DATA ls_dest_huhdr TYPE /scwm/s_huhdr_int.
*    DATA ls_dest_lgpla TYPE /scwm/s_lagp_int.
*    DATA ls_capa TYPE /scwm/s_pack_result_badi.
*    DATA lv_lptyp TYPE /scwm/lvs_lptyp.
*    DATA lt_ordim TYPE /scwm/tt_ordim_o.
*    DATA lv_capa_ok TYPE boole_d VALUE abap_true.
*    DATA lv_to  TYPE /scwm/tanum.
*    DATA ls_possible_qty TYPE /scwm/s_possible_qty_badi.
*
*    ls_dest_lgpla-lptyp = sc_lptyp1.
*    ls_dest_lgpla-lgpla = ls_request-lgpla = 'AUNIT-01-01-A'.
*    ls_dest_lgpla-guid_loc = sc_lgpla1_guid.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-matid = 'ABC'.
*    ls_request-quan  = 10.
*    ls_request-unit  = 'EA'.
*    ls_request-movehu = wmegc_movehu_no.
*
**   bin is empty - request is only checked against max qty
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to
*        iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed although maxqty not reached'
*      level = if_aunit_constants=>method ).
*
**   test internal buffer of implementation
**   this time, stock from previous request should be on bin
**   it shouldn't be possible to store another 10 EA on the bin
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_false
*      msg   = 'Custom check successful although maxqty was exceeded'
*      level = if_aunit_constants=>method ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = ls_possible_qty-quantity
*      exp   = 5
*      msg   = 'Wrong calculation of possible qty'
*      level = if_aunit_constants=>method ).
*
*    lv_capa_ok = abap_true.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-matid = '0001'.
*    ls_request-quan  = 200.
*    ls_request-unit  = 'EA'.
*    ls_request-movehu = wmegc_movehu_no.
*
**   bin is empty - request is only checked against max qty
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed although no restriction for product'
*      level = if_aunit_constants=>method ).
*
*  ENDMETHOD.                    "custom_capa_prodwt
*
*
*  METHOD sel_max_qty_lptyp.
*    DATA lv_matid   TYPE /scwm/de_matid VALUE 'ABC'.
*    DATA ls_request TYPE /scwm/s_pack_request.
*    DATA lt_maxqty TYPE zcl_core_pts_capacheck=>lty_tt_lptyp_maxqty.
*    FIELD-SYMBOLS:
*      <maxqty>    TYPE zcl_core_pts_capacheck=>lty_s_lptyp_maxqty.
*
*    TRY.
**       select with lgtyp that has no restrictions ...
*        mr_maxqty->sel_max_qty_lptyp(
*          EXPORTING
*            iv_lptyp    = '0NOT'
*            is_request  = ls_request
*          IMPORTING
*            et_maxqty = lt_maxqty ).
*      CATCH /scwm/cx_core.
*        cl_abap_unit_assert=>fail( msg   = 'Exception not expected when selecting maxqty' ).
*    ENDTRY.
*
**   ... should have empty result
*    cl_abap_unit_assert=>assert_initial(
*      act   = lt_maxqty
*      msg   = 'Initial lt_maxqty expected'
*    ).
*
*    TRY.
**       select with values that should be in buffer ...
*        ls_request-matid = lv_matid.
*        mr_maxqty->sel_max_qty_lptyp(
*          EXPORTING
*            iv_lptyp   = sc_lptyp1
*            is_request = ls_request
*          IMPORTING
*            et_maxqty = lt_maxqty ).
*      CATCH /scwm/cx_core.
*        cl_abap_unit_assert=>fail( msg   = 'Exception not expected when selecting maxqty').
*    ENDTRY.
*
**   ... result should not be empty
*    cl_abap_unit_assert=>assert_not_initial(
*      act   = lt_maxqty
*      msg   = 'Entry in lt_maxqty expected for lptyp PM10'
*      quit  = if_aunit_constants=>method
*    ).
*
*    READ TABLE lt_maxqty ASSIGNING <maxqty>
*      WITH TABLE KEY
*        lgnum = sc_lgnum
*        lptyp = sc_lptyp1
*        matid = lv_matid.
*    cl_abap_unit_assert=>assert_subrc(
*      exp   = 0
*      msg   = 'lt_maxqty does not have matching entry'
*    ).
*
*
**   ... should get same entry
*    cl_abap_unit_assert=>assert_equals(
*      act   = <maxqty>-max_qty
*      exp   = 15
*      msg   = 'wrong entry in maxqty'
*    ).
*
*  ENDMETHOD.                    "sel_max_qty_lptyp_for_matid
*
*  METHOD cancel_buffered_prodwt.
*    DATA ls_t331 TYPE /scwm/t331.
*    DATA ls_request TYPE /scwm/s_pack_request.
*    DATA ls_dest_huhdr TYPE /scwm/s_huhdr_int.
*    DATA ls_dest_lgpla TYPE /scwm/s_lagp_int.
*    DATA ls_capa TYPE /scwm/s_pack_result_badi.
*    DATA lv_lptyp TYPE /scwm/lvs_lptyp.
*    DATA lt_ordim TYPE /scwm/tt_ordim_o.
*    DATA lv_capa_ok TYPE boole_d VALUE abap_true.
*    DATA lv_to TYPE /scwm/tanum.
*    DATA ls_possible_qty TYPE /scwm/s_possible_qty_badi.
*
*    ls_dest_lgpla-lptyp = sc_lptyp1.
*    ls_dest_lgpla-lgpla = ls_request-lgpla = 'AUNIT-01-01-B'.
*    ls_dest_lgpla-guid_loc = sc_lgpla2_guid.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-matid = 'ABC'.
*    ls_request-quan  = 10.
*    ls_request-unit  = 'EA'.
*    ls_request-movehu = wmegc_movehu_no.
*
**   bin is empty - request is only checked against max qty
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed although maxqty not reached'
*      level = if_aunit_constants=>method
*    ).
*
**   transient WT is canceled -> internal buffer has to be cleared
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~cancel_buffered_wt(
*      EXPORTING
*        is_t331       =  ls_t331
*        is_request    =  ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        it_ordim = lt_ordim ).
*
*
**   to check whether buffer was cleared, try to create same Prod-WT
**   should work because bin should be empty again
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed although bin buffer was cleared'
*      level = if_aunit_constants=>method ).
*
*  ENDMETHOD.                    "cancel_buffered_prodwt
*
*  METHOD cancel_buffered_huwt.
*    DATA ls_t331 TYPE /scwm/t331.
*    DATA ls_request TYPE /scwm/s_pack_request.
*    DATA ls_dest_huhdr TYPE /scwm/s_huhdr_int.
*    DATA ls_dest_lgpla TYPE /scwm/s_lagp_int.
*    DATA ls_capa TYPE /scwm/s_pack_result_badi.
*    DATA lv_lptyp TYPE /scwm/lvs_lptyp.
*    DATA lt_ordim TYPE /scwm/tt_ordim_o.
*    DATA lv_capa_ok TYPE boole_d VALUE abap_true.
*    DATA lv_to TYPE /scwm/tanum.
*    DATA ls_possible_qty TYPE /scwm/s_possible_qty_badi.
*
*    ls_dest_lgpla-lptyp = sc_lptyp1.
*    ls_dest_lgpla-lgpla = ls_request-lgpla = 'AUNIT-02-01-B'.
*    ls_dest_lgpla-guid_loc = sc_lgpla3_guid.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-movehu = wmegc_movehu_outin.
*    ls_request-sguid_hu = 'CBC0001'.
*
**   bin is empty - request is only checked against max qty
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed although maxqty not reached'
*      level = if_aunit_constants=>method
*    ).
*
**   transient WT is canceled -> internal buffer has to be cleared
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~cancel_buffered_wt(
*      EXPORTING
*        is_t331       =  ls_t331
*        is_request    =  ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        it_ordim = lt_ordim ).
*
*
**   to check whether buffer was cleared, try to create same HU-WT
**   should work because bin should be empty again
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed although bin buffer was cleared'
*      level = if_aunit_constants=>method ).
*
*  ENDMETHOD.                    "cancel_buffered_huwt
*
*  METHOD maxqty_boundary.
*    DATA ls_t331 TYPE /scwm/t331.
*    DATA ls_request TYPE /scwm/s_pack_request.
*    DATA ls_dest_huhdr TYPE /scwm/s_huhdr_int.
*    DATA ls_dest_lgpla TYPE /scwm/s_lagp_int.
*    DATA ls_capa TYPE /scwm/s_pack_result_badi.
*    DATA lv_lptyp TYPE /scwm/lvs_lptyp.
*    DATA lt_ordim TYPE /scwm/tt_ordim_o.
*    DATA lv_capa_ok TYPE boole_d VALUE abap_true.
*    DATA lv_to TYPE /scwm/tanum.
*    DATA ls_possible_qty TYPE /scwm/s_possible_qty_badi.
*
*    ls_dest_lgpla-lptyp = sc_lptyp1.
*    ls_dest_lgpla-lgpla = ls_request-lgpla = 'AUNIT-01-01-C'.
*    ls_dest_lgpla-guid_loc = sc_lgpla4_guid.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-matid = 'ABC'.
*    ls_request-quan  = 15.
*    ls_request-unit  = 'EA'.
*    ls_request-movehu = wmegc_movehu_no.
*
**   bin is empty - request is only checked against max qty
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed when maxqty is filled completely'
*      level = if_aunit_constants=>method
*    ).
*  ENDMETHOD.                    "maxqty_boundary
*
*  METHOD early_capa_check_prodwt.
*    DATA ls_t331 TYPE /scwm/t331.
*    DATA ls_request TYPE /scwm/s_pack_request.
*    DATA ls_dest_huhdr TYPE /scwm/s_huhdr_int.
*    DATA ls_dest_lgpla TYPE /scwm/s_lagp_int.
*    DATA ls_capa TYPE /scwm/s_pack_result_badi.
*    DATA lt_ordim TYPE /scwm/tt_ordim_o.
*    DATA lv_capa_ok TYPE boole_d VALUE abap_true.
*    DATA lv_to TYPE /scwm/tanum.
*    DATA ls_possible_qty TYPE /scwm/s_possible_qty_badi.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-matid = 'ABC'.
*    ls_request-quan  = 10.
*    ls_request-unit  = 'EA'.
*    ls_request-movehu = wmegc_movehu_no.
*
**   early capa check against bin type 1
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = sc_lptyp1  " <<< bin type allows 15 EA
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom early capa check failed for bin type 1'
*      level = if_aunit_constants=>method ).
*
**   same WT should fail on bin type 2 (because lower maxqty on this bin type)
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = sc_lptyp2 " <<< bin type allows only 5 EA
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_false
*      msg   = 'Cust.early check allows WT with more than maxqty for bin type 2'
*      level = if_aunit_constants=>method ).
*
*  ENDMETHOD.                    "early_capa_check_prodwt
*
*  METHOD early_capa_check_huwt.
*    DATA ls_t331 TYPE /scwm/t331.
*    DATA ls_request TYPE /scwm/s_pack_request.
*    DATA ls_dest_huhdr TYPE /scwm/s_huhdr_int.
*    DATA ls_dest_lgpla TYPE /scwm/s_lagp_int.
*    DATA ls_capa TYPE /scwm/s_pack_result_badi.
*    DATA lt_ordim TYPE /scwm/tt_ordim_o.
*    DATA lv_capa_ok TYPE boole_d VALUE abap_true.
*    DATA lv_to TYPE /scwm/tanum.
*    DATA ls_possible_qty TYPE /scwm/s_possible_qty_badi.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-movehu = wmegc_movehu_outin.
*    ls_request-sguid_hu = 'ECC0001'.
*
**   early capa check against bin type 1
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = sc_lptyp1  " <<< bin type allows 15 EA / HU has 10 EA
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom early capa check (HU) failed for bin type 1'
*      level = if_aunit_constants=>method ).
*
**   same WT should fail on bin type 2 (because lower maxqty on this bin type)
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = sc_lptyp2 " <<< bin type allows only 5 EA / HU has 10 EA
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_false
*      msg   = 'Cust.early check allows HU-WT with more than maxqty for bin type 2'
*      level = if_aunit_constants=>method ).
*
*  ENDMETHOD.                    "early_capa_check_huwt
*
*  METHOD custom_capa_huwt.
*    DATA ls_t331 TYPE /scwm/t331.
*    DATA ls_request TYPE /scwm/s_pack_request.
*    DATA ls_dest_huhdr TYPE /scwm/s_huhdr_int.
*    DATA ls_dest_lgpla TYPE /scwm/s_lagp_int.
*    DATA ls_capa TYPE /scwm/s_pack_result_badi.
*    DATA lv_lptyp TYPE /scwm/lvs_lptyp.
*    DATA lt_ordim TYPE /scwm/tt_ordim_o.
*    DATA lv_capa_ok TYPE boole_d VALUE abap_true.
*    DATA lv_to TYPE /scwm/tanum.
*    DATA ls_possible_qty TYPE /scwm/s_possible_qty_badi.
*
*    ls_dest_lgpla-lptyp = sc_lptyp1.
*    ls_dest_lgpla-lgpla = ls_request-lgpla = 'AUNIT-01-01-D'.
*    ls_dest_lgpla-guid_loc = sc_lgpla5_guid.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-sguid_hu = 'CA0001'.
*    ls_request-movehu = wmegc_movehu_outin.
*
*    lv_capa_ok = abap_true.
*
**   bin is empty - request is only checked against max qty
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa   = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed although maxqty not reached'
*      level = if_aunit_constants=>method
*    ).
*
**   test internal buffer of implementation
**   this time, stock from previous request should be on bin
**   it shouldn't be possible to store another HU with 10 EA on the bin
*    ls_request-sguid_hu = 'CB0001'.
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_false
*      msg   = 'Custom check successful although maxqty was exceeded'
*      level = if_aunit_constants=>method
*    ).
*
*    cl_abap_unit_assert=>assert_initial(
*      act   = ls_possible_qty
*      msg   = 'possible qty not cleared for HU-WT'
*      level = if_aunit_constants=>method
*    ).
*
***   same check as first check with open WTs
**    ls_dest_lgpla-lptyp = sc_lptyp1.
**    ls_dest_lgpla-lgpla = ls_request-lgpla = 'AUNIT-01-01-A'.
**
**    ls_request-lgnum = sc_lgnum.
**    ls_request-matid = 'ABC'.
**    ls_request-quan  = 10.
**    ls_request-unit  = 'EA'.
**    ls_request-movehu = wmegc_movehu_no.
**
***   bin is empty - request is only checked against max qty
**    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
**      EXPORTING
**        is_t331 = ls_t331
**        is_request = ls_request
**        is_dest_huhdr = ls_dest_huhdr
**        is_dest_lgpla = ls_dest_lgpla
**        is_capa = ls_capa
**        iv_lptyp = lv_lptyp
**        it_ordim = lt_ordim
**      CHANGING
**        cv_capa_ok = lv_capa_ok
**        cs_possible_qty = ls_possible_qty ).
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-matid = '0001'.
*    ls_request-quan  = 200.
*    ls_request-unit  = 'EA'.
*    ls_request-movehu = wmegc_movehu_no.
*
*    lv_capa_ok = abap_true.
*
**   bin is empty - request is only checked against max qty
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed although no restriction for product'
*      level = if_aunit_constants=>method ).
*
** Scneario:
** open WT on dest. bin is confirmed with changes -> but still enough capacity on bin
*
*    ls_dest_lgpla-lptyp = sc_lptyppml.
*    ls_dest_lgpla-lgpla = ls_request-lgpla = sc_lgpla7.
*    ls_dest_lgpla-guid_loc = sc_lgpla7_guid.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-sguid_hu = sc_hu800000108.
*    ls_request-movehu = wmegc_movehu_outin.
*
*    lv_capa_ok = abap_true.
*
**   bin is empty - request is only checked against max qty
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = sc_tanum_1
*        iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed with open WT to be confirmed'
*      level = if_aunit_constants=>method ).
*
*  ENDMETHOD.                    "custom_capa_huwt
*
*  METHOD custom_capa_ordim_o.
*    DATA ls_t331 TYPE /scwm/t331.
*    DATA ls_request TYPE /scwm/s_pack_request.
*    DATA ls_dest_huhdr TYPE /scwm/s_huhdr_int.
*    DATA ls_dest_lgpla TYPE /scwm/s_lagp_int.
*    DATA ls_capa TYPE /scwm/s_pack_result_badi.
*    DATA lv_lptyp TYPE /scwm/lvs_lptyp.
*    DATA lt_ordim TYPE /scwm/tt_ordim_o.
*    DATA lv_capa_ok TYPE boole_d VALUE abap_true.
*    DATA lv_to TYPE /scwm/tanum.
*    DATA ls_possible_qty TYPE /scwm/s_possible_qty_badi.
*    DATA ls_ordim_o    TYPE /scwm/ordim_o.
*
*    ls_dest_lgpla-lptyp = sc_lptyp1.
*    ls_dest_lgpla-lgpla = ls_request-lgpla = 'AUNIT-01-01-E'.
*    ls_dest_lgpla-guid_loc = sc_lgpla6_guid.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-sguid_hu = ltcl_ei_capacheck_maxqty=>sc_hu000_guid.
*    ls_request-movehu = wmegc_movehu_outin.
*
*    ls_ordim_o-tanum = 1.
*    ls_ordim_o-dguid_hu =
*    ls_ordim_o-sguid_hu = ltcl_ei_capacheck_maxqty=>sc_hu001_guid.
*    ls_ordim_o-nlpla = 'AUNIT-01-01-E'.
*    ls_ordim_o-movehu = wmegc_movehu_outin.
*    APPEND ls_ordim_o TO lt_ordim.
*    ls_ordim_o-tanum = 2.
*    ls_ordim_o-dguid_hu = ltcl_ei_capacheck_maxqty=>sc_hu001_guid.
*    ls_ordim_o-sguid_hu = ltcl_ei_capacheck_maxqty=>sc_hu002_guid.
*    ls_ordim_o-movehu = wmegc_movehu_outin.
*    APPEND ls_ordim_o TO lt_ordim.
*    ls_ordim_o-tanum = 3.
*    ls_ordim_o-dguid_hu = ltcl_ei_capacheck_maxqty=>sc_hu002_guid.
*    ls_ordim_o-sguid_hu = 'AAAAAAAAAAAAA'.
*    ls_ordim_o-movehu = wmegc_movehu_no.
*    ls_ordim_o-matid = 'ABC'.
*    ls_ordim_o-vsolm = 1.
*    ls_ordim_o-meins = 'EA'.
*    APPEND ls_ordim_o TO lt_ordim.
*
*    lv_capa_ok = abap_true.
*
**   bin is empty - request is only checked against max qty
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed although maxqty not reached'
*      level = if_aunit_constants=>method
*    ).
*
*    ls_dest_lgpla-lptyp = sc_lptyp1.
*    ls_dest_lgpla-lgpla = ls_request-lgpla = 'AUNIT-01-01-E'.
*    ls_dest_lgpla-guid_loc = sc_lgpla6_guid.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-matid = 'ABC'.
*    ls_request-quan  = 10.
*    ls_request-unit  = 'EA'.
*    ls_request-movehu = wmegc_movehu_no.
*
**   bin is empty - request is only checked against max qty
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_false
*      msg   = 'Custom check successfull although maxqty exceeded'
*      level = if_aunit_constants=>method
*    ).
*
*
*
*  ENDMETHOD.                    "custom_capa_ordim_o
*
*  METHOD custom_capa_advanced.
*    DATA ls_t331 TYPE /scwm/t331.
*    DATA ls_request TYPE /scwm/s_pack_request.
*    DATA ls_dest_huhdr TYPE /scwm/s_huhdr_int.
*    DATA ls_dest_lgpla TYPE /scwm/s_lagp_int.
*    DATA ls_capa TYPE /scwm/s_pack_result_badi.
*    DATA lv_lptyp TYPE /scwm/lvs_lptyp.
*    DATA lt_ordim TYPE /scwm/tt_ordim_o.
*    DATA lv_capa_ok TYPE boole_d VALUE abap_true.
*    DATA lv_to TYPE /scwm/tanum.
*    DATA ls_possible_qty TYPE /scwm/s_possible_qty_badi.
*    DATA ls_ordim_o    TYPE /scwm/ordim_o.
*
*    ls_ordim_o-tanum = '001000000475'.
*    ls_ordim_o-flghuto = abap_true.
*    ls_ordim_o-dguid_hu = '00163EA7249C1EE1B5E4CA411D8292C1'.
*    ls_ordim_o-sguid_hu = '00163EA7249C1EE1B5B4D23F7DBA129D'.
*    ls_ordim_o-nlpla = 'PM20-02-01-C'.
*    ls_ordim_o-movehu = wmegc_movehu_outin.
*    APPEND ls_ordim_o TO lt_ordim.
*    ls_ordim_o-tanum = '001000000476'.
*    ls_ordim_o-flghuto = abap_false.
*    ls_ordim_o-matid  = '005056A202261EE1B4C88909938E36DC'.
*    ls_ordim_o-vsolm = 5. ls_ordim_o-meins = 'EA'.
*    ls_ordim_o-dguid_hu = '00163EA7249C1EE1B5E4DE5BED3452C1'.
*    ls_ordim_o-sguid_hu = '00163EA7249C1EE1B5AD36EED2023294'.
*    ls_ordim_o-nlpla = 'PM20-02-01-C'.
*    ls_ordim_o-movehu = wmegc_movehu_no.
*    APPEND ls_ordim_o TO lt_ordim.
*    ls_ordim_o-tanum = '001000000477'.
*    ls_ordim_o-flghuto = abap_true.
*    ls_ordim_o-dguid_hu =
*    ls_ordim_o-sguid_hu = '00163EA7249C1EE1B5E4DE5BED3452C1'.
*    ls_ordim_o-nlpla = 'PM20-02-01-B'.
*    ls_ordim_o-movehu = wmegc_movehu_outin.
*    APPEND ls_ordim_o TO lt_ordim.
*    ls_ordim_o-tanum = '001000000479'.
*    ls_ordim_o-flghuto = abap_false.
*    ls_ordim_o-matid = '005056A202261EE1B4C88909938E36DC'.
*    ls_ordim_o-vsolm = 10. ls_ordim_o-meins = 'EA'.
*    ls_ordim_o-dguid_hu = '00163EA7249C1EE1B5AD36EED2025294'.
*    ls_ordim_o-sguid_hu = '00163EA7249C1EE1B5AD36EED2023294'.
*    ls_ordim_o-nlpla = 'PM20-02-01-B'.
*    ls_ordim_o-movehu = wmegc_movehu_no.
*    APPEND ls_ordim_o TO lt_ordim.
*    ls_ordim_o-tanum = '001000000480'.
*    ls_ordim_o-flghuto = abap_true.
*    ls_ordim_o-dguid_hu = '00163EA7249C1EE1B5C4AB02F5BFD2B5'.
*    ls_ordim_o-nlenr = '00000000000800000050'.
*    ls_ordim_o-sguid_hu = '00163EA7249C1EE1B5E4B170B01A92C1'.
*    ls_ordim_o-vlenr = '00000000000800000104'.
*    ls_ordim_o-vlpla = 'PM20-02-01-D'.
*    ls_ordim_o-nlpla = 'PM20-02-01-B'.
*    ls_ordim_o-movehu = wmegc_movehu_outin.
*    APPEND ls_ordim_o TO lt_ordim.
*
*    ls_dest_lgpla-lptyp     = sc_lptyppml.
*    ls_dest_lgpla-lgpla     = ls_request-lgpla = 'PM20-02-01-B'.
*    ls_dest_lgpla-guid_loc  = sc_bin_0201b.
*
*    ls_request-lgnum = sc_lgnum.
*    ls_request-matid = sc_mat_acme_tab.
*    ls_request-quan  = 10.
*    ls_request-unit  = 'BOT'.
*    ls_request-movehu = wmegc_movehu_no.
*
*    lv_capa_ok = abap_true.
*
**   should be 80 BOT of material PM-ACME-TAB for bin + 10 BOT from
**   100 BOT allowed => check should not fail
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok      = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
**   80 BOT for bin, move 10 BOT, 100 BOT allowed  => shouldn't fail
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_true
*      msg   = 'Custom check failed although maxqty not exceeded'
*      level = if_aunit_constants=>method
*    ).
*
*    CLEAR ls_request.
*    ls_request-movehu = wmegc_movehu_outin.
*    ls_request-lgnum = sc_lgnum.
*    ls_request-sguid_hu = sc_hu800000046.
*
*    lv_capa_ok = abap_true.
*
**   should be 90 BOT of material PM-ACME-TAB for bin (because of previous WT)
**   100 BOT allowed => check should fail because move of 20 BOT exceeds maxqty
*    mr_maxqty->/scwm/if_ex_core_pts_capacheck~custom_capa_check(
*      EXPORTING
*        is_t331 = ls_t331
*        is_request = ls_request
*        is_dest_huhdr = ls_dest_huhdr
*        is_dest_lgpla = ls_dest_lgpla
*        is_capa = ls_capa
*        iv_lptyp = lv_lptyp
*        iv_to    = lv_to iv_same_bin = abap_false
*        it_ordim = lt_ordim
*      CHANGING
*        cv_capa_ok      = lv_capa_ok
*        cs_possible_qty = ls_possible_qty ).
*
**   80 BOT for bin, plus 10 BOT, from previous, 100 BOT allowed
**   move HU with 20 BOT should fail
*    cl_abap_unit_assert=>assert_equals(
*      act   = lv_capa_ok
*      exp   = abap_false
*      msg   = 'Custom check did not fail although maxqty exceeded'
*      level = if_aunit_constants=>method
*    ).
*
*  ENDMETHOD.                    "custom_capa_advanced
*
*ENDCLASS.                    "ltcl_ei_capacheck_maxqty IMPLEMENTATION
*
**----------------------------------------------------------------------*
**       CLASS lcl_mock_data_access IMPLEMENTATION
**----------------------------------------------------------------------*
**
**----------------------------------------------------------------------*
*CLASS lcl_mock_data_access IMPLEMENTATION.
*
*  METHOD /scwm/if_ei_core_capa_data~get_material_uom.
*    RETURN.
*  ENDMETHOD.                    "/scwm/if_ei_core_capa_data~get_material_uom
*
*  METHOD /scwm/if_ei_core_capa_data~select_stock.
*    DATA:
*      ls_stock  LIKE LINE OF et_stockitm,
*      ls_huhdr  LIKE LINE OF et_huhdr.
*    CASE iv_bin_loc.
*      WHEN  ltcl_ei_capacheck_maxqty=>sc_lgpla1_guid OR
*        ltcl_ei_capacheck_maxqty=>sc_lgpla2_guid OR
*        ltcl_ei_capacheck_maxqty=>sc_lgpla3_guid OR
*        ltcl_ei_capacheck_maxqty=>sc_lgpla4_guid OR
*        ltcl_ei_capacheck_maxqty=>sc_lgpla5_guid OR
*        ltcl_ei_capacheck_maxqty=>sc_lgpla6_guid.
*      WHEN  ltcl_ei_capacheck_maxqty=>sc_bin_0201b.
*        ls_stock-guid_parent = '00163EA7249C1EE1B5AD36EED2025294'.
*        ls_stock-guid_loc = '00163EA7249C1EE1B5AD36EED1F5D294'.
*        ls_stock-matid = '005056A202261EE1B4C88909938E36DC'.
*        ls_stock-quan = 20.
*        ls_stock-meins = 'EA'.
*        APPEND ls_stock TO et_stockitm.
*        ls_stock-guid_parent = '00163EA7249C1EE1B5AD36EED2025294'.
*        ls_stock-guid_loc = '00163EA7249C1EE1B5AD36EED1F5D294'.
*        ls_stock-matid = ltcl_ei_capacheck_maxqty=>sc_mat_acme_tab.
*        ls_stock-quan = 10.
*        ls_stock-meins = 'BOT'.
*        APPEND ls_stock TO et_stockitm.
*        ls_stock-guid_parent = '00163EA7249C1EE1B5B16B108E0F929A'.
*        ls_stock-guid_loc = '00163EA7249C1EE1B5AD36EED1F5D294'.
*        ls_stock-matid = ltcl_ei_capacheck_maxqty=>sc_mat_acme_tab.
*        ls_stock-quan = 10.
*        ls_stock-meins = 'BOT'.
*        APPEND ls_stock TO et_stockitm.
*        ls_stock-guid_parent = '00163EA7249C1EE1B5C4AB02F5BFD2B5'.
*        ls_stock-guid_loc = '00163EA7249C1EE1B5C4B248DC04D2B5'.
*        ls_stock-matid = ltcl_ei_capacheck_maxqty=>sc_mat_acme_tab.
*        ls_stock-quan = 10.
*        ls_stock-meins = 'BOT'.
*        APPEND ls_stock TO et_stockitm.
*
*        ls_huhdr-guid_hu = iv_bin_loc.
*        ls_huhdr-huident = 'PM20-02-01-B'.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huhdr-guid_hu = '00163EA7249C1EE1B5B16B108E0F929A'.
*        ls_huhdr-huident = '00000000000800000041'.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huhdr-guid_hu = '00163EA7249C1EE1B5C4AB02F5BFD2B5'.
*        ls_huhdr-huident = '00000000000800000050'.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huhdr-guid_hu = '00163EA7249C1EE1B5C4B248DC04D2B5'.
*        ls_huhdr-huident = 'PM-IND-0000023'.
*        APPEND ls_huhdr TO et_huhdr.
*
*    ENDCASE.
*    ls_huhdr-guid_hu = iv_bin_loc.
*    APPEND ls_huhdr TO et_huhdr.
*  ENDMETHOD.                    "/scwm/if_ei_core_capa_data~select_stock
*
*  METHOD /scwm/if_ei_core_capa_data~read_hu.
*    DATA:
*      ls_huhdr    TYPE /scwm/s_huhdr_int,
*      ls_huitm    TYPE /scwm/s_huitm_int.
*
*    CASE iv_guid_hu.
*      WHEN 'CA0001' OR 'CB0001' OR 'ECC0001' OR 'CBC0001'.
*        ls_huhdr-guid_hu = ls_huitm-guid_parent = iv_guid_hu.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huitm-matid = 'ABC'.
*        ls_huitm-quan = 10.
*        ls_huitm-meins = 'EA'.
*        APPEND ls_huitm TO et_huitm.
*      WHEN ltcl_ei_capacheck_maxqty=>sc_hu001_guid.
*        ls_huhdr-guid_hu = ls_huitm-guid_parent = iv_guid_hu.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huitm-matid = 'ABC'.
*        ls_huitm-quan = 10.
*        ls_huitm-meins = 'EA'.
*        APPEND ls_huitm TO et_huitm.
*      WHEN ltcl_ei_capacheck_maxqty=>sc_hu002_guid.
*        ls_huhdr-guid_hu = ls_huitm-guid_parent = iv_guid_hu.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huitm-matid = 'ABC'.
*        ls_huitm-quan = 2.
*        ls_huitm-meins = 'EA'.
*        APPEND ls_huitm TO et_huitm.
*        ls_huitm-matid = 'CDE'.
*        ls_huitm-quan = 50.
*        ls_huitm-meins = 'EA'.
*        APPEND ls_huitm TO et_huitm.
*      WHEN ltcl_ei_capacheck_maxqty=>sc_hu800000107.
*        ls_huhdr-guid_hu = ls_huitm-guid_parent = iv_guid_hu.
*        ls_huhdr-huident = '00000000000800000107'.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huitm-matid = ltcl_ei_capacheck_maxqty=>sc_mat_acme_hbd.
*        ls_huitm-quana = 1.       ls_huitm-quan = 20.
*        ls_huitm-altme = 'PAL'.  ls_huitm-meins = 'EA'.
*        APPEND ls_huitm TO et_huitm.
*
*        ls_huhdr-guid_hu =
*        ls_huitm-guid_parent = ltcl_ei_capacheck_maxqty=>sc_hu800000106.
*        ls_huhdr-huident = '00000000000800000106'.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huitm-matid = ltcl_ei_capacheck_maxqty=>sc_mat_acme_tab.
*        ls_huitm-quana = 1.       ls_huitm-quan = 10.
*        ls_huitm-altme = 'KAR'.  ls_huitm-meins = 'BOT'.
*        APPEND ls_huitm TO et_huitm.
*      WHEN ltcl_ei_capacheck_maxqty=>sc_hu800000047.
*        ls_huhdr-guid_hu = ls_huitm-guid_parent = iv_guid_hu.
*        ls_huhdr-huident = '00000000000800000047'.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huitm-matid = '005056A202261EE1B4C88909938E36DC'.
*        ls_huitm-quana = 1.       ls_huitm-quan = 1.
*        ls_huitm-altme = 'EA'.  ls_huitm-meins = 'EA'.
*        APPEND ls_huitm TO et_huitm.
*        ls_huitm-matid = ltcl_ei_capacheck_maxqty=>sc_mat_acme_tab.
*        ls_huitm-quana = 20.       ls_huitm-quan = 20.
*        ls_huitm-altme = 'BOT'.  ls_huitm-meins = 'BOT'.
*        APPEND ls_huitm TO et_huitm.
*      WHEN ltcl_ei_capacheck_maxqty=>sc_hu800000104.
*        ls_huhdr-guid_hu = ls_huitm-guid_parent = iv_guid_hu.
*        ls_huhdr-huident = '00000000000800000104'.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huitm-matid = ltcl_ei_capacheck_maxqty=>sc_mat_acme_tab.
*        ls_huitm-quana = 1.       ls_huitm-quan = 10.
*        ls_huitm-altme = 'KAR'.  ls_huitm-meins = 'BOT'.
*        APPEND ls_huitm TO et_huitm.
*        ls_huitm-matid = ltcl_ei_capacheck_maxqty=>sc_mat_acme_blk.
*        ls_huitm-quana = 5.       ls_huitm-quan = 100.
*        ls_huitm-altme = 'KAR'.  ls_huitm-meins = 'EA'.
*        APPEND ls_huitm TO et_huitm.
*
*        ls_huhdr-guid_hu = ls_huitm-guid_parent = ltcl_ei_capacheck_maxqty=>sc_hu800000105.
*        ls_huhdr-huident = '00000000000800000105'.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huitm-matid = ltcl_ei_capacheck_maxqty=>sc_mat_acme_tab.
*        ls_huitm-quana = 1.       ls_huitm-quan = 10.
*        ls_huitm-altme = 'KAR'.  ls_huitm-meins = 'BOT'.
*        APPEND ls_huitm TO et_huitm.
*
*      WHEN ltcl_ei_capacheck_maxqty=>sc_hu800000046.
*        ls_huhdr-guid_hu = ls_huitm-guid_parent = iv_guid_hu.
*        ls_huhdr-huident = '00000000000800000046'.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huitm-matid = ltcl_ei_capacheck_maxqty=>sc_mat_acme_tab.
*        ls_huitm-quana = 20.       ls_huitm-quan = 20.
*        ls_huitm-altme = 'BOT'.  ls_huitm-meins = 'BOT'.
*        APPEND ls_huitm TO et_huitm.
*      WHEN ltcl_ei_capacheck_maxqty=>sc_hu800000108.
*        ls_huhdr-guid_hu = ls_huitm-guid_parent = iv_guid_hu.
*        ls_huhdr-huident = '00000000000800000108'.
*        APPEND ls_huhdr TO et_huhdr.
*        ls_huitm-matid = ltcl_ei_capacheck_maxqty=>sc_mat_acme_tab.
*        ls_huitm-quana = 60.     ls_huitm-quan = 60.
*        ls_huitm-altme = 'BOT'.  ls_huitm-meins = 'BOT'.
*        APPEND ls_huitm TO et_huitm.
*    ENDCASE.
*
*
*  ENDMETHOD.                    "/scwm/if_ei_core_capa_data~read_hu
*
*  METHOD /scwm/if_ei_core_capa_data~read_to_dest_bin.
*    DATA:
*      ls_ordim_o    TYPE /scwm/ordim_o.
*
*    CASE is_lagp_dest-guid_loc.
*      WHEN ltcl_ei_capacheck_maxqty=>sc_lgpla6_guid.
*        ls_ordim_o-tanum = 1.
*        ls_ordim_o-dguid_hu =
*        ls_ordim_o-sguid_hu = ltcl_ei_capacheck_maxqty=>sc_hu001_guid.
*        ls_ordim_o-nlpla = 'AUNIT-01-01-E'.
*        ls_ordim_o-movehu = wmegc_movehu_outin.
*        APPEND ls_ordim_o TO et_ordim_o.
*      WHEN ltcl_ei_capacheck_maxqty=>sc_bin_0201b.
*        ls_ordim_o-tanum = '001000000477'.
*        ls_ordim_o-flghuto = abap_true.
*        ls_ordim_o-dguid_hu =
*        ls_ordim_o-sguid_hu = '00163EA7249C1EE1B5E4DE5BED3452C1'.
*        ls_ordim_o-nlpla = 'PM20-02-01-B'.
*        ls_ordim_o-movehu = wmegc_movehu_outin.
*        APPEND ls_ordim_o TO et_ordim_o.
*        ls_ordim_o-tanum = '001000000479'.
*        ls_ordim_o-flghuto = abap_false.
*        ls_ordim_o-matid = '005056A202261EE1B4C88909938E36DC'.
*        ls_ordim_o-vsolm = 10.
*        ls_ordim_o-meins = 'EA'.
*        ls_ordim_o-dguid_hu = '00163EA7249C1EE1B5AD36EED2025294'.
*        ls_ordim_o-sguid_hu = '00163EA7249C1EE1B5AD36EED2023294'.
*        ls_ordim_o-nlpla = 'PM20-02-01-B'.
*        ls_ordim_o-movehu = wmegc_movehu_no.
*        APPEND ls_ordim_o TO et_ordim_o.
*        ls_ordim_o-tanum = '001000000480'.
*        ls_ordim_o-flghuto = abap_true.
*        ls_ordim_o-dguid_hu = '00163EA7249C1EE1B5C4AB02F5BFD2B5'.
*        ls_ordim_o-nlenr = '00000000000800000050'.
*        ls_ordim_o-sguid_hu = '00163EA7249C1EE1B5E4B170B01A92C1'.
*        ls_ordim_o-vlenr = '00000000000800000104'.
*        ls_ordim_o-vlpla = 'PM20-02-01-D'.
*        ls_ordim_o-nlpla = 'PM20-02-01-B'.
*        ls_ordim_o-movehu = wmegc_movehu_outin.
*        APPEND ls_ordim_o TO et_ordim_o.
*      WHEN ltcl_ei_capacheck_maxqty=>sc_lgpla7_guid.
*        ls_ordim_o-lgnum = ltcl_ei_capacheck_maxqty=>sc_lgnum.
*        ls_ordim_o-tanum = ltcl_ei_capacheck_maxqty=>sc_tanum_1.
*        ls_ordim_o-dguid_hu =
*        ls_ordim_o-sguid_hu = ltcl_ei_capacheck_maxqty=>sc_hu800000108.
*        ls_ordim_o-nlpla = ltcl_ei_capacheck_maxqty=>sc_lgpla7.
*        ls_ordim_o-movehu = wmegc_movehu_outin.
*        APPEND ls_ordim_o TO et_ordim_o.
*    ENDCASE.
*
*  ENDMETHOD.                    "/scwm/if_ei_core_capa_data~read_to_dest_bin
*
*ENDCLASS.                    "lcl_mock_data_access IMPLEMENTATION
