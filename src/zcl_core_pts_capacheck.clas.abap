CLASS zcl_core_pts_capacheck DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    INTERFACES if_badi_interface .
    INTERFACES /scwm/if_ex_core_pts_capacheck .
    INTERFACES /scwm/if_tm_appl .

    METHODS constructor
      IMPORTING
        !io_data_access TYPE REF TO /scwm/if_ei_core_capa_data OPTIONAL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF lty_s_lptyp_maxqty,
        lgnum   TYPE /scwm/lgnum,
        lgtyp   TYPE /scwm/lgtyp,
        lptyp	  TYPE /scwm/lvs_lptyp,
        matid	  TYPE /scwm/de_matid,
        max_qty	TYPE /scwm/de_maxqty,
        uom	    TYPE meins,
      END OF lty_s_lptyp_maxqty .
    TYPES:
      lty_tt_lptyp_maxqty TYPE SORTED TABLE OF lty_s_lptyp_maxqty
                                            WITH UNIQUE KEY primary_key
                                                            COMPONENTS lgnum lptyp matid lgtyp .
    TYPES:
      BEGIN OF lty_s_bin_stock,
        lgnum   TYPE /scwm/lgnum,
        bin_loc	TYPE /scwm/guid_loc,
        matid	  TYPE /scwm/de_matid,
        qty	    TYPE /scwm/de_quantity,
        uom	    TYPE meins,
        altme	  TYPE /scwm/de_aunit,
      END OF lty_s_bin_stock .
    TYPES:
      lty_tt_bin_stock TYPE SORTED TABLE OF lty_s_bin_stock
                                            WITH UNIQUE KEY primary_key
                                                            COMPONENTS lgnum bin_loc matid .
    TYPES:
      BEGIN OF lty_s_hu_qty,
        lgnum   TYPE /scwm/lgnum,
        guid_hu	TYPE /scwm/guid_hu,
        matid	  TYPE /scwm/de_matid,
        qty	    TYPE /scwm/de_quantity,
        uom	    TYPE meins,
        altme   TYPE /scwm/de_aunit,
      END OF lty_s_hu_qty .
    TYPES:
      lty_tt_hu_qty TYPE SORTED TABLE OF lty_s_hu_qty
                                            WITH UNIQUE KEY primary_key
                                                            COMPONENTS lgnum guid_hu matid .

    DATA mo_data_access TYPE REF TO /scwm/if_ei_core_capa_data .
    CONSTANTS c_add TYPE char1 VALUE '+' ##NO_TEXT.
    CONSTANTS c_sub TYPE char1 VALUE '-' ##NO_TEXT.
    CONSTANTS c_sign_i TYPE s_sign VALUE wmegc_sign_inclusive ##NO_TEXT.
    CONSTANTS c_opt_eq TYPE s_option VALUE wmegc_option_eq ##NO_TEXT.
    DATA mt_lptyp_maxqty TYPE lty_tt_lptyp_maxqty .
    DATA mt_ordim_o TYPE /scwm/tt_ordim_o .
    DATA mt_bin_stock TYPE lty_tt_bin_stock .
    DATA mv_lgnum TYPE /scwm/lgnum .
    DATA mv_lptyp TYPE /scwm/lvs_lptyp .
    DATA mt_hu_matqty TYPE lty_tt_hu_qty .
    DATA mv_bin_loc TYPE /scwm/guid_loc .
    DATA mt_ordim_bin TYPE /scwm/tt_ordim_o .
    DATA mt_messages TYPE bapirettab .

    METHODS add_ordim_qty_to_bin
      IMPORTING
        !is_ordim_o   TYPE /scwm/ordim_o
      CHANGING
        !ct_bin_stock TYPE lty_tt_bin_stock
      RAISING
        /scwm/cx_core .
    METHODS check_maxqty
      IMPORTING
        !it_bin_stock TYPE lty_tt_bin_stock
        !is_maxqty    TYPE /scwm/s_quan
        !is_request   TYPE /scwm/s_pack_request
        !it_hu_matqty TYPE lty_tt_hu_qty
      CHANGING
        !cv_capa_ok   TYPE boole_d
        !cs_possible  TYPE /scwm/s_possible_qty_badi .
    METHODS check_maxqty_entry
      IMPORTING
        !it_bin_stock TYPE lty_tt_bin_stock
        !is_maxqty    TYPE /scwm/s_quan
        !iv_matid     TYPE /scwm/de_matid
        !is_request   TYPE /scwm/s_pack_request
        !iv_quan      TYPE /scwm/de_quantity
      CHANGING
        !cv_capa_ok   TYPE boole_d
        !cs_possible  TYPE /scwm/s_possible_qty_badi .
    METHODS conv_max_qty_uom
      CHANGING
        !ct_maxqty TYPE lty_tt_lptyp_maxqty
      RAISING
        /scwm/cx_core .
    METHODS det_qty_on_dest_bin
      IMPORTING
        !is_lgpla_dest TYPE /scwm/s_lagp_int
        !iv_to         TYPE /scwm/tanum
      EXPORTING
        !et_bin_stock  TYPE lty_tt_bin_stock
      RAISING
        /scwm/cx_core .
    METHODS det_qty_on_hu
      IMPORTING
        !iv_guid_hu      TYPE /scwm/guid_hu
        !iv_guid_hu_high TYPE /scwm/guid_hu OPTIONAL
      EXPORTING
        !et_hu_matqty    TYPE lty_tt_hu_qty
      RAISING
        /scwm/cx_core .
    METHODS update_messages
      CHANGING
        ct_messages TYPE bapirettab.

    METHODS is_check_required
      IMPORTING
        !iv_capa_ok            TYPE boole_d
        !is_request            TYPE /scwm/s_pack_request
        !is_t331               TYPE /scwm/t331
        !is_dest_lgpla         TYPE /scwm/s_lagp_int
        !iv_to                 TYPE /scwm/tanum
        !iv_same_bin           TYPE boole_d
      RETURNING
        VALUE(rv_check_needed) TYPE boole_d .
    METHODS sel_max_qty_lptyp
      IMPORTING
        !iv_lptyp   TYPE /scwm/lvs_lptyp
        !is_request TYPE /scwm/s_pack_request
      EXPORTING
        !et_maxqty  TYPE lty_tt_lptyp_maxqty
      RAISING
        /scwm/cx_core .
    METHODS upd_qty_buffer
      IMPORTING
        !is_request   TYPE /scwm/s_pack_request
        !iv_operand   TYPE char1
        !it_hu_matqty TYPE lty_tt_hu_qty .
    METHODS upd_qty_buffer_entry
      IMPORTING
        !iv_operand TYPE char1
        !iv_matid   TYPE /scwm/de_matid
        !iv_quan    TYPE /scwm/de_quantity
        !iv_unit    TYPE /scwm/de_unit .
ENDCLASS.



CLASS ZCL_CORE_PTS_CAPACHECK IMPLEMENTATION.


  METHOD /scwm/if_ex_core_pts_capacheck~cancel_buffered_wt.
* deletion of internally created WT (transient WT).
*
* The WT was created within the internal memory but was not yet
* written to the database.
* In SAP standard the capacity of this WT was already buffered
* on the destination bin and HU. This buffered capa was removed.
* In this example implementation the quantities of product per
* bin are buffered -> in case of deletion, buffered qty have
* to be deleted from buffer as well.

    DATA:
      lt_hu_matqty    TYPE lty_tt_hu_qty.

    CLEAR mt_messages.

* check if buffered quantities have to be removed at all
    IF is_request-movehu = wmegc_movehu_no.
      READ TABLE mt_lptyp_maxqty TRANSPORTING NO FIELDS
        WITH TABLE KEY
          lgnum = is_request-lgnum
          lgtyp = is_dest_lgpla-lgtyp
          lptyp = is_dest_lgpla-lptyp
          matid = is_request-matid.
      IF sy-subrc <> 0.
*     no restrictions -> no entries buffered
        RETURN.
      ENDIF.
    ELSE.
      READ TABLE mt_lptyp_maxqty TRANSPORTING NO FIELDS
        WITH KEY
          lgnum = is_request-lgnum
          lptyp = is_dest_lgpla-lptyp.
      IF sy-subrc <> 0.
*     no restrictions
        RETURN.
      ENDIF.
    ENDIF.

    mv_bin_loc = is_dest_lgpla-guid_loc.
    TRY.
        IF is_request-movehu <> wmegc_movehu_no.
*       determine quantities on HU moved by WT (-> from own buffer)
          det_qty_on_hu(
            EXPORTING
              iv_guid_hu   = is_request-sguid_hu
            IMPORTING
              et_hu_matqty = lt_hu_matqty ).
        ENDIF.

*     buffered qty of WT is removed
        upd_qty_buffer(
          EXPORTING
            it_hu_matqty = lt_hu_matqty
            is_request   = is_request
            iv_operand   = c_sub ).
      CATCH /scwm/cx_core.
        update_messages( CHANGING ct_messages = mt_messages ).
    ENDTRY.

    ct_messages = mt_messages.

  ENDMETHOD.


  METHOD /scwm/if_ex_core_pts_capacheck~custom_capa_check.
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
*  IMPORTANT:
*  This example implementation is intended for checking maximum allowed
*  product quantities for storage bin types.
*  Read the documentation of the BAdI, the BAdI interface, and
*  the documentation of this implementation before copying it.
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-042
*& Author        : Alper Ahmedov
*& e-mail        : alper.ahmedov@qinlox.com
*& Date          : 27.03.2023
**********************************************************************
*& Description (short)
**********************************************************************
    DATA:
      lt_maxqty       TYPE lty_tt_lptyp_maxqty,
      lt_hu_matqty    TYPE lty_tt_hu_qty,
      lt_bin_stock    TYPE lty_tt_bin_stock,
      lv_check_needed TYPE boole_d,
      lv_maxqty       TYPE /scwm/s_material_lgtyp-maxqty,
      ls_message      TYPE bapiret2,
      ls_huhdr        TYPE /scwm/s_huhdr_int,
      ls_mat_lgtyp    TYPE /scwm/s_material_lgtyp.

    BREAK-POINT ID zcg_badi.
    BREAK-POINT ID zcg_capa_check_maxqty.

    IF zcl_switch=>get_switch_state( EXPORTING iv_lgnum = is_request-lgnum
                                               iv_devid = zif_switch_const=>c_zcross_003 ) EQ abap_false.
      RETURN.
    ENDIF.

    CLEAR mt_messages.

    IF is_request-flghuto = abap_true AND
      is_request-matid IS INITIAL.
      CALL FUNCTION '/SCWM/HU_READ'
        EXPORTING
          iv_guid_hu = is_request-sguid_hu
          iv_lgnum   = is_request-lgnum
        IMPORTING
          es_huhdr   = ls_huhdr
        EXCEPTIONS
          deleted    = 1
          not_found  = 2
          error      = 3
          OTHERS     = 4.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      IF ls_huhdr-rsrc IS NOT INITIAL.
        RETURN.
      ENDIF.

    ENDIF.

    " cover scenario for RF picking screen
    IF is_dest_lgpla IS INITIAL.
      RETURN.
    ENDIF.

    mt_ordim_o = it_ordim.
    SORT mt_ordim_o BY dguid_hu.

    DATA(ls_request) = CORRESPONDING /scwm/s_pack_request( is_request ).

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid     = is_request-matid
            iv_lgnum     = is_request-lgnum
            iv_lgtyp     = is_request-lgtyp
            iv_entitled  = is_request-entitled
          IMPORTING
            es_mat_lgtyp = ls_mat_lgtyp.

        lv_maxqty = ls_mat_lgtyp-maxqty.
      CATCH /scwm/cx_md.
    ENDTRY.

    IF ls_mat_lgtyp-zz1_dirrpl_stt = abap_true.
      DATA(ls_ltap) = zcl_core_ltap=>get_ltap( ).

      DATA(lv_suom) = abap_false."COND #( WHEN ls_ltap IS NOT INITIAL
      "        THEN ls_ltap-suom
      "        ELSE zcl_core_ltap=>get_suom( ) ).

      ls_request-quan = COND #( WHEN ls_ltap IS NOT INITIAL
                                THEN ls_ltap-reqqty
                                ELSE ls_request-quan ).
    ENDIF.

    "<AAHMEDOV>-240207
    "commented out
*    IF mt_ordim_o IS NOT INITIAL AND
*            lv_suom IS NOT INITIAL.
*
*      DATA(lt_suom_diff) = VALUE meins_tty( FOR GROUPS OF <ls_ordim_o> IN mt_ordim_o
*                                                        WHERE ( suom <> lv_suom AND
*                                                                nltyp EQ is_dest_lgpla-lgpla )
*                                                        GROUP BY <ls_ordim_o>-suom
*                                                         ( <ls_ordim_o>-suom ) ).
*
*      " check if there are any ALTME entries different than ALTME of the current task
*      IF lines( lt_suom_diff ) > 0.
*        MESSAGE i013(zmc_ztlptyp_maxqty) INTO ls_message-message WITH is_dest_lgpla-lgpla.
*        ls_message-type = sy-msgty.
*        ls_message-id   = sy-msgid.
*        ls_message-number = sy-msgno.
*        ls_message-message_v1 = is_dest_lgpla-lgpla.
*        APPEND ls_message TO ct_messages.
*        cv_capa_ok = abap_false.
*        RETURN.
*      ENDIF.
*    ENDIF.
    "<AAHMEDOV>-240207

    " is a check for max. quantity needed?
    lv_check_needed = is_check_required(
      iv_capa_ok    = cv_capa_ok
      is_request    = ls_request
      is_t331       = is_t331
      is_dest_lgpla = is_dest_lgpla
      iv_same_bin   = iv_same_bin
      iv_to         = iv_to ).

    IF lv_check_needed = abap_false.
      RETURN.
    ENDIF.

    mv_lgnum   = ls_request-lgnum.
    mv_bin_loc = is_dest_lgpla-guid_loc.

    IF iv_lptyp IS INITIAL.
      " not early capacity check -> WT to an actual bin
      mv_lptyp = is_dest_lgpla-lptyp.
    ELSE.
      " early capacity check is running
      " it checks if storage bin type fits for WT
      mv_lptyp = iv_lptyp.
    ENDIF.

*    TRY.
*        APPEND mt_lptyp_maxqty[ matid = ls_request-matid
*                                lptyp = mv_lptyp ] TO lt_maxqty.
*      CATCH cx_sy_itab_line_not_found ##NO_HANDLER.
*    ENDTRY.

    TRY.
*        IF lines( lt_maxqty ) = 0.
*
*          " get maxqty from ztlptyp_maxqty table
*          sel_max_qty_lptyp(
*            EXPORTING
*              iv_lptyp   = mv_lptyp                " Storage Bin Type
*              is_request = ls_request                 " Product
*            IMPORTING
*              et_maxqty  = lt_maxqty ).
*
*          IF lines( lt_maxqty ) > 0.
*            " convert unit of measure to base UoM
*            conv_max_qty_uom( CHANGING ct_maxqty = lt_maxqty ).
*
*            LOOP AT lt_maxqty ASSIGNING FIELD-SYMBOL(<ls_maxqty>).
*
*              CHECK NOT line_exists( mt_lptyp_maxqty[ lgnum = <ls_maxqty>-lgnum
*                                                      lgtyp = <ls_maxqty>-lgtyp
*                                                      lptyp = <ls_maxqty>-lptyp
*                                                      matid = <ls_maxqty>-matid ] ).
*
*              INSERT <ls_maxqty> INTO TABLE mt_lptyp_maxqty.
*
*            ENDLOOP.
*
*          ENDIF.
*        ENDIF.
*
*        IF lines( lt_maxqty ) = 0.
*          cv_capa_ok = abap_true.
*          CLEAR: cs_possible_qty.
*          RETURN.
*        ENDIF.

        IF iv_lptyp IS INITIAL.
          " not early capacity check -> WT to an actual bin
          " determine product quantities on destination bin
          det_qty_on_dest_bin(
            EXPORTING
              is_lgpla_dest = is_dest_lgpla
              iv_to         = iv_to
            IMPORTING
              et_bin_stock  = lt_bin_stock ).

           "<AAHMEDOV>-240207
*          IF lines( lt_bin_stock ) > 0 AND
*            lv_suom IS NOT INITIAL.
*            lt_suom_diff = VALUE meins_tty( FOR GROUPS OF <ls_bin_stock> IN lt_bin_stock
*                                                          WHERE ( altme <> lv_suom )
*                                                          GROUP BY <ls_bin_stock>-altme
*                                                           ( <ls_bin_stock>-altme ) ).
*
*            " check if there are any ALTME entries different than ALTME of the current task
*            IF lines( lt_suom_diff ) > 0.
*              MESSAGE i013(zmc_ztlptyp_maxqty) INTO ls_message-message WITH is_dest_lgpla-lgpla.
*              ls_message-type = sy-msgty.
*              ls_message-id   = sy-msgid.
*              ls_message-number = sy-msgno.
*              ls_message-message_v1 = is_dest_lgpla-lgpla.
*
*              APPEND ls_message TO ct_messages.
*              cv_capa_ok = abap_false.
*
*              RETURN.
*            ENDIF.
*          ENDIF.
          "<AAHMEDOV>-240207

        ENDIF.

        IF ls_request-movehu <> wmegc_movehu_no.
          " determine quantity for HU-WT that is created
          det_qty_on_hu(
            EXPORTING
              iv_guid_hu   = ls_request-sguid_hu
            IMPORTING
              et_hu_matqty = lt_hu_matqty ).
        ENDIF.

      CATCH /scwm/cx_core.
        update_messages( CHANGING ct_messages = mt_messages ).

        ct_messages = mt_messages.
        cv_capa_ok = abap_false.

        RETURN.
    ENDTRY.

    " Quantities for MATIDs known that are moved and that are on destination
    " check whether WT can be created
    check_maxqty(
      EXPORTING
        it_bin_stock = lt_bin_stock
        it_hu_matqty = lt_hu_matqty
        is_maxqty    = VALUE #( quan = ls_mat_lgtyp-maxqty unit = ls_mat_lgtyp-maxqty_uom_dsp )
        is_request   = ls_request
      CHANGING
        cv_capa_ok   = cv_capa_ok
        cs_possible  = cs_possible_qty ).

    IF cv_capa_ok = abap_true AND iv_lptyp IS INITIAL.
      " capa check on actual destination finished with OK
      " -> update the buffered quantities on the bin
      upd_qty_buffer(
        EXPORTING
          is_request   = ls_request
          iv_operand   = c_add
          it_hu_matqty = lt_hu_matqty ).

      " Message 000: Check for max.qty per stor.bin type passed successfully
      MESSAGE i000(zmc_ztlptyp_maxqty) INTO ls_message-message.
      update_messages( CHANGING ct_messages = mt_messages ).
    ENDIF.

    ct_messages = mt_messages.

  ENDMETHOD.


  METHOD /scwm/if_tm_appl~check_save.
* CHECK_SAVE is triggered when EWM standard saves data to DB.
* If custom coding has to save data in Z-tables, but was not
* triggered to save its data beforehand, this method should be
* used to notify the EWM transaction manager about a missing save
* for the custom coding.
* Implementation if this method is not needed for this class since
* no custom data has to be saved.
    RETURN.
  ENDMETHOD.


  METHOD /scwm/if_tm_appl~cleanup.
* This method is called from the EWM transaction manager (class /SCWM/CL_TM).
*
* When a COMMIT WORK or ROLLBACK WORK is triggered,
* buffered data has to be cleared at the end of an LUW
* to have a consistent state in the memory for the start of the next LUW.
* The EWM transaction manager triggers all objects that have to clear their
* memory by calling this method (via interface /SCWM/IF_TM_APPL).
* Registering this object at the EWM transaction manager was done
* in method CONSTRUCTOR.

    CLEAR:
      mt_ordim_o,
      mt_ordim_bin,
      mt_hu_matqty,
      mt_bin_stock,
      mt_lptyp_maxqty,
      mv_bin_loc,
      mv_lgnum,
      mv_lptyp.

  ENDMETHOD.


  METHOD add_ordim_qty_to_bin.
    DATA:
      lt_matqty    TYPE lty_tt_hu_qty,
      ls_bin_stock TYPE lty_s_bin_stock.
    FIELD-SYMBOLS:
      <ls_bin_stock> TYPE lty_s_bin_stock,
      <ls_hu_matqty> TYPE lty_s_hu_qty.

    IF is_ordim_o-movehu = wmegc_movehu_no.
      READ TABLE ct_bin_stock ASSIGNING <ls_bin_stock>
        WITH TABLE KEY
          lgnum   = mv_lgnum
          bin_loc = mv_bin_loc
          matid   = is_ordim_o-matid.
      IF sy-subrc <> 0.
        CLEAR ls_bin_stock.
        ls_bin_stock-lgnum = mv_lgnum.
        ls_bin_stock-bin_loc = mv_bin_loc.
        ls_bin_stock-matid = is_ordim_o-matid.
        ls_bin_stock-uom = is_ordim_o-meins.
**********************************************************************
        "GAP-042, aahmedov-<120523>
        "new line, for comparison of suom/altme between replenishment task and bin stock
        ls_bin_stock-altme = is_ordim_o-altme.
**********************************************************************
        INSERT ls_bin_stock INTO TABLE ct_bin_stock ASSIGNING <ls_bin_stock>.
      ENDIF.
      ADD is_ordim_o-vsolm TO <ls_bin_stock>-qty.
    ELSE.

      det_qty_on_hu(
        EXPORTING
          iv_guid_hu   = is_ordim_o-sguid_hu
        IMPORTING
          et_hu_matqty = lt_matqty ).

      LOOP AT lt_matqty ASSIGNING <ls_hu_matqty>.
        READ TABLE ct_bin_stock ASSIGNING <ls_bin_stock>
          WITH TABLE KEY
            lgnum   = mv_lgnum
            bin_loc = mv_bin_loc
            matid   = <ls_hu_matqty>-matid.
        IF sy-subrc <> 0.
          CLEAR ls_bin_stock.
          ls_bin_stock-lgnum = mv_lgnum.
          ls_bin_stock-bin_loc = mv_bin_loc.
          ls_bin_stock-matid = <ls_hu_matqty>-matid.
          ls_bin_stock-uom = <ls_hu_matqty>-uom.
          ls_bin_stock-altme = <ls_hu_matqty>-altme.
          INSERT ls_bin_stock INTO TABLE ct_bin_stock ASSIGNING <ls_bin_stock>.
        ENDIF.
        ADD <ls_hu_matqty>-qty TO <ls_bin_stock>-qty.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD check_maxqty.
    FIELD-SYMBOLS:
      <ls_hu_matqty>     TYPE lty_s_hu_qty.

    IF is_request-movehu = wmegc_movehu_no.

      check_maxqty_entry(
        EXPORTING
          it_bin_stock = it_bin_stock
          is_maxqty    = is_maxqty
          is_request   = is_request
          iv_matid     = is_request-matid
          iv_quan      = is_request-quan
        CHANGING
          cv_capa_ok   = cv_capa_ok
          cs_possible  = cs_possible ).

    ELSE.
      " HU-WTs move potentially more than one stock item
      " -> several restrictions have to be checked
      LOOP AT it_hu_matqty ASSIGNING <ls_hu_matqty>.

        check_maxqty_entry(
          EXPORTING
            it_bin_stock = it_bin_stock
            is_maxqty    = is_maxqty
            is_request   = is_request
            iv_matid     = <ls_hu_matqty>-matid
            iv_quan      = is_request-quan
          CHANGING
            cv_capa_ok   = cv_capa_ok
            cs_possible  = cs_possible ).

        IF cv_capa_ok = abap_false.
          " one entry failed -> whole HU cannot be moved
          RETURN.
        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD check_maxqty_entry.
    DATA:
      ls_message    TYPE bapiret2,
      lv_exceed_qty TYPE /scwm/de_quantity.
*    FIELD-SYMBOLS:
*      <ls_bin_stock> TYPE lty_s_bin_stock.
*      <ls_maxqty>    TYPE lty_s_lptyp_maxqty.

    CLEAR cs_possible.

* for product-WT only check against the entry with same material
*    READ TABLE it_maxqty ASSIGNING <ls_maxqty>
*      WITH TABLE KEY
*        lgnum = mv_lgnum
*        lgtyp = is_request-lgtyp
*        lptyp = mv_lptyp
*        matid = iv_matid.
    IF is_maxqty-quan IS INITIAL.
      " no restrictions for this material on the bin
      cv_capa_ok = abap_true.
      RETURN.
    ENDIF.

    DATA(lv_qty) = VALUE #( it_bin_stock[ lgnum   = mv_lgnum
                                          bin_loc = mv_bin_loc
                                          matid   = iv_matid ]-qty OPTIONAL ).

*    READ TABLE it_bin_stock ASSIGNING <ls_bin_stock>
*      WITH TABLE KEY
*        lgnum   = mv_lgnum
*        bin_loc = mv_bin_loc
*        matid   = iv_matid.
*    IF sy-subrc = 0.
*      ADD <ls_bin_stock>-qty TO lv_qty.
*    ENDIF.

* --------------------------------------------------------------------- *
*  actual check whether destination can still store product quantities
* --------------------------------------------------------------------- *
    IF ( is_maxqty-quan - lv_qty ) >= iv_quan.
      cv_capa_ok = abap_true.
    ELSE.
      cv_capa_ok = abap_false.
      IF is_request-movehu = wmegc_movehu_no.
        cs_possible-quantity = is_maxqty-quan - lv_qty.
        cs_possible-meins = is_maxqty-unit.
      ENDIF.

* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *
* create the message class ztlptyp_maxqty and create T100 messages
* before uncommenting the following lines
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *
      " Message 001: Check for max.qty per stor.bin type failed
      MESSAGE i001(zmc_ztlptyp_maxqty) INTO ls_message-message.
      update_messages( CHANGING ct_messages = mt_messages ).

      WRITE iv_quan TO ls_message-message_v1 UNIT is_maxqty-unit LEFT-JUSTIFIED.
      CONCATENATE ls_message-message_v1 is_maxqty-unit INTO ls_message-message_v1 SEPARATED BY space.
      WRITE iv_matid TO ls_message-message_v2 LEFT-JUSTIFIED.
      lv_exceed_qty = abs( ( is_maxqty-quan - lv_qty ) - iv_quan ).
      WRITE lv_exceed_qty TO ls_message-message_v3 UNIT is_maxqty-unit LEFT-JUSTIFIED.
      CONCATENATE ls_message-message_v3 is_maxqty-unit INTO ls_message-message_v3 SEPARATED BY space.
      " Message 002: Move of &1 for product &2 exceeds max. quantity by &3
      MESSAGE i002(zmc_ztlptyp_maxqty) WITH ls_message-message_v1 ls_message-message_v2 ls_message-message_v3
         INTO ls_message-message.
      update_messages( CHANGING ct_messages = mt_messages ).

      " Message 003: &1 allowed for bin type &2
      WRITE is_maxqty-quan TO ls_message-message_v1 UNIT is_maxqty-unit LEFT-JUSTIFIED.
      CONCATENATE ls_message-message_v1 is_maxqty-unit INTO ls_message-message_v1 SEPARATED BY space.

      ls_message-message_v2 = is_request-lgtyp.

      MESSAGE i003(zmc_ztlptyp_maxqty) WITH ls_message-message_v1 ls_message-message_v2
         INTO ls_message-message.
      update_messages( CHANGING ct_messages = mt_messages ).

    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    DATA:
      lo_tm     TYPE REF TO /scwm/cl_tm.

* reading master data or customizing is capsuled in a seperate class
* this makes it possible to write mock objects for unit testing
    IF io_data_access IS BOUND.
      mo_data_access = io_data_access.
    ELSE.
      CREATE OBJECT mo_data_access
        TYPE lcl_data_access.
    ENDIF.

* This example implementation has an own buffer for application data.
* Cleaning up buffered data at the end of an LUW is triggered by the
* EWM transaction manager (/SCWM/CL_TM).
* In order to have that cleanup of buffered data, objects with own
* buffers have to register at the transaction manager.
* This is done with the following lines:
    TRY.
        lo_tm ?= /scwm/cl_tm_factory=>get_service(
          iv_service = /scwm/cl_tm_factory=>sc_manager ).
        lo_tm->/scwm/if_tm~register_cleanup(
          EXPORTING
            io_application = me ).
      CATCH /scwm/cx_tm_factory.                        "#EC NO_HANDLER
    ENDTRY.
* See also method /SCWM/IF_TM_APPL~CLEANUP in this class


  ENDMETHOD.


  METHOD conv_max_qty_uom.
    DATA:
      lt_matid      TYPE  /scwm/tt_matid,
      lt_mat_uom    TYPE  /scwm/tt_material_uom,
      lt_mat_global TYPE  /scwm/tt_material_global,
      lt_mat_error  TYPE  /scwm/tt_matid_bapiret.           "#EC NEEDED
    FIELD-SYMBOLS:
      <ls_maxqty>     TYPE  lty_s_lptyp_maxqty,
      <ls_mat_uom>    TYPE  /scwm/s_material_uom,
      <ls_mat_global> TYPE  /scwm/s_material_global.

    LOOP AT ct_maxqty ASSIGNING <ls_maxqty>.
      APPEND <ls_maxqty>-matid TO lt_matid.
    ENDLOOP.

    mo_data_access->get_material_uom(
      EXPORTING
        it_matid      = lt_matid
        iv_lgnum      = mv_lgnum
      IMPORTING
        et_mat_uom    = lt_mat_uom
        et_mat_global = lt_mat_global
        et_mat_error  = lt_mat_error ).

    LOOP AT ct_maxqty ASSIGNING <ls_maxqty>.

      READ TABLE lt_mat_global ASSIGNING <ls_mat_global>
        WITH KEY
          matid = <ls_maxqty>-matid.

      READ TABLE lt_mat_uom ASSIGNING <ls_mat_uom>
        WITH KEY
          matid = <ls_maxqty>-matid
          meinh = <ls_maxqty>-uom.

      CHECK <ls_mat_global> IS ASSIGNED AND <ls_mat_uom> IS ASSIGNED.

      IF <ls_mat_uom>-umren <> <ls_mat_uom>-umrez.
*     alternative unit of measure used in table -> convert to BUoM

        <ls_maxqty>-max_qty = <ls_maxqty>-max_qty * ( <ls_mat_uom>-umrez / <ls_mat_uom>-umren ).
        <ls_maxqty>-uom = <ls_mat_global>-meins.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD det_qty_on_dest_bin.
**********************************************************************
    "GAP-042, aahmedov-<120523>
    "new line, for comparison of suom/altme between replenishment task and bin stock
    DATA:
      lt_stock_itm   TYPE /scwm/tt_stock_select,
      lt_stock_huhdr TYPE /scwm/tt_huhdr,
      ls_bin_stock   TYPE lty_s_bin_stock.
    FIELD-SYMBOLS:
      <ls_huhdr>     TYPE /scwm/huhdr,
      <ls_stockitm>  TYPE /scwm/s_stock_select,
      <ls_ordim_o>   LIKE LINE OF mt_ordim_o,
      <ls_bin_stock> TYPE lty_s_bin_stock.

    CLEAR et_bin_stock.

    LOOP AT mt_bin_stock ASSIGNING <ls_bin_stock>
                              WHERE  lgnum   = mv_lgnum
                                AND  bin_loc = mv_bin_loc.
      INSERT <ls_bin_stock> INTO TABLE et_bin_stock.
    ENDLOOP.
    IF sy-subrc <> 0.
*   nothing in local buffer of implementation
*   select stock and dynamic stock situation on destination
      mo_data_access->select_stock(
        EXPORTING
          iv_bin_loc  = mv_bin_loc
          iv_lgnum    = mv_lgnum
        IMPORTING
          et_stockitm = lt_stock_itm
          et_huhdr    = lt_stock_huhdr ).

      LOOP AT lt_stock_itm ASSIGNING <ls_stockitm>.
        READ TABLE et_bin_stock ASSIGNING <ls_bin_stock>
          WITH TABLE KEY
            lgnum   = mv_lgnum
            bin_loc = mv_bin_loc
            matid   = <ls_stockitm>-matid.
        IF sy-subrc <> 0.
          CLEAR ls_bin_stock.
          ls_bin_stock-lgnum    = mv_lgnum.
          ls_bin_stock-bin_loc  = mv_bin_loc.
          ls_bin_stock-matid    = <ls_stockitm>-matid.
          ls_bin_stock-uom      = <ls_stockitm>-meins.
          ls_bin_stock-altme = <ls_stockitm>-altme.
          INSERT ls_bin_stock INTO TABLE et_bin_stock ASSIGNING <ls_bin_stock>.
        ENDIF.
        ADD <ls_stockitm>-quan TO <ls_bin_stock>-qty.
      ENDLOOP.

*   get open WTs for bin from WT buffer to avoid several sorts of mt_ordim_o
*   fast access to WTs because WTs are in internal memory and indexed
      mo_data_access->read_to_dest_bin(
        EXPORTING
          is_lagp_dest = is_lgpla_dest
        IMPORTING
          et_ordim_o   = mt_ordim_bin ).

      DATA(ls_ltap_cancelled) = zcl_core_ltap=>get_ltap_cancelled( ).
      IF ls_ltap_cancelled IS NOT INITIAL.
        DELETE mt_ordim_bin WHERE lgnum EQ ls_ltap_cancelled-lgnum
                              AND tanum EQ ls_ltap_cancelled-tanum.
      ENDIF.

      SORT mt_ordim_bin BY lgnum tanum.
      IF iv_to IS NOT INITIAL.
*     ignore quantities from same WT
*      -> capacity of WT might have changed (e.g. additional items in HU)
        READ TABLE mt_ordim_bin TRANSPORTING NO FIELDS
          WITH KEY
            lgnum = mv_lgnum
            tanum = iv_to
          BINARY SEARCH.
        IF sy-subrc = 0.
          DELETE mt_ordim_bin INDEX sy-tabix.
        ENDIF.
      ENDIF.

      LOOP AT mt_ordim_bin ASSIGNING <ls_ordim_o>.
        add_ordim_qty_to_bin(
          EXPORTING
            is_ordim_o   = <ls_ordim_o>
          CHANGING
            ct_bin_stock = et_bin_stock ).
      ENDLOOP.

      LOOP AT lt_stock_huhdr ASSIGNING <ls_huhdr>.
        CHECK <ls_huhdr>-vhi = wmegc_vhi_real.
        LOOP AT mt_ordim_o ASSIGNING <ls_ordim_o> WHERE dguid_hu = <ls_huhdr>-guid_hu.

*       WT qty already added to qty on bin?
          READ TABLE mt_ordim_bin TRANSPORTING NO FIELDS
            WITH KEY
              lgnum = <ls_ordim_o>-lgnum
              tanum = <ls_ordim_o>-tanum
            BINARY SEARCH.
          CHECK sy-subrc <> 0.

          add_ordim_qty_to_bin(
            EXPORTING
              is_ordim_o   = <ls_ordim_o>
            CHANGING
              ct_bin_stock = et_bin_stock ).
        ENDLOOP.
      ENDLOOP.

      INSERT LINES OF et_bin_stock INTO TABLE mt_bin_stock.

    ENDIF.

  ENDMETHOD.


  METHOD det_qty_on_hu.
    DATA:
      lt_huhdr        TYPE /scwm/tt_huhdr_int,
      lt_huitm        TYPE /scwm/tt_huitm_int,
      ls_hu_matqty    TYPE lty_s_hu_qty,
      lv_guid_hu_high TYPE /scwm/guid_hu.
    FIELD-SYMBOLS:
      <ls_huhdr>     TYPE /scwm/s_huhdr_int,
      <ls_huitm>     TYPE /scwm/s_huitm_int,
      <ls_ordim_o>   LIKE LINE OF mt_ordim_o,
      <ls_hu_matqty> TYPE lty_s_hu_qty.

    IF iv_guid_hu_high IS INITIAL.
*   this is the higher level HU
      lv_guid_hu_high = iv_guid_hu.
    ELSE.
      lv_guid_hu_high = iv_guid_hu_high.
    ENDIF.

    mo_data_access->read_hu(
      EXPORTING
        iv_guid_hu = iv_guid_hu
      IMPORTING
        et_huhdr   = lt_huhdr
        et_huitm   = lt_huitm ).

    SORT lt_huitm BY guid_parent.

    LOOP AT lt_huhdr ASSIGNING <ls_huhdr>.

      LOOP AT lt_huitm ASSIGNING <ls_huitm> WHERE guid_parent = <ls_huhdr>-guid_hu.
        CHECK <ls_huitm>-quan IS NOT INITIAL.
        READ TABLE et_hu_matqty ASSIGNING <ls_hu_matqty>
          WITH TABLE KEY
            lgnum   = mv_lgnum
            guid_hu = lv_guid_hu_high
            matid   = <ls_huitm>-matid.
        IF sy-subrc <> 0.
          CLEAR ls_hu_matqty.
          ls_hu_matqty-lgnum = mv_lgnum.
          ls_hu_matqty-guid_hu = lv_guid_hu_high.
          ls_hu_matqty-matid = <ls_huitm>-matid.
          ls_hu_matqty-uom = <ls_huitm>-meins.
**********************************************************************
          "GAP-042, aahmedov-<150523>
          "new line, for comparison of suom/altme between replenishment task and bin stock
          ls_hu_matqty-altme = <ls_huitm>-altme.
**********************************************************************
          INSERT ls_hu_matqty INTO TABLE et_hu_matqty ASSIGNING <ls_hu_matqty>.
        ENDIF.
        ADD <ls_huitm>-quan TO <ls_hu_matqty>-qty.
      ENDLOOP.

      LOOP AT mt_ordim_o ASSIGNING <ls_ordim_o> WHERE dguid_hu = <ls_huhdr>-guid_hu.

*     ignore HU-WT for TOP-HU
        CHECK <ls_ordim_o>-sguid_hu <> <ls_ordim_o>-dguid_hu.

        IF <ls_ordim_o>-movehu = wmegc_movehu_no.
          READ TABLE et_hu_matqty ASSIGNING <ls_hu_matqty>
            WITH TABLE KEY
              lgnum   = mv_lgnum
              guid_hu = lv_guid_hu_high
              matid   = <ls_ordim_o>-matid.
          IF sy-subrc <> 0.
            CLEAR ls_hu_matqty.
            ls_hu_matqty-lgnum = mv_lgnum.
            ls_hu_matqty-guid_hu = iv_guid_hu.
            ls_hu_matqty-matid = <ls_ordim_o>-matid.
            ls_hu_matqty-uom = <ls_ordim_o>-meins.
**********************************************************************
            "GAP-042, aahmedov-<150523>
            "new line, for comparison of suom/altme between replenishment task and bin stock
            ls_hu_matqty-altme = <ls_ordim_o>-altme.
**********************************************************************

            INSERT ls_hu_matqty INTO TABLE et_hu_matqty ASSIGNING <ls_hu_matqty>.
          ENDIF.
          ADD <ls_ordim_o>-vsolm TO <ls_hu_matqty>-qty.
        ELSE.
          det_qty_on_hu(
            EXPORTING
              iv_guid_hu      = <ls_ordim_o>-sguid_hu
              iv_guid_hu_high = lv_guid_hu_high
            IMPORTING
              et_hu_matqty    = et_hu_matqty ).
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD is_check_required.
    FIELD-SYMBOLS:
      <ls_ordim_o>   TYPE /scwm/ordim_o.

    rv_check_needed = abap_true.

    IF iv_capa_ok = abap_false.
*   standard capacity check already failed
      rv_check_needed = abap_false.
      RETURN.
    ENDIF.

    IF is_t331-kapap = wmegc_capa_no_gv.
*   no capacity check activated for this storage type
      rv_check_needed = abap_false.
      RETURN.
    ENDIF.

    IF iv_same_bin = abap_true.
*   ignore WTs on the same bin because no quantity change on bin
      " TODO posting change?
      rv_check_needed = abap_false.
      RETURN.
    ENDIF.

    IF iv_to IS NOT INITIAL AND is_request-movehu = wmegc_movehu_no.
*   WT was created earlier - check if relevant data for recheck was changed
      READ TABLE mt_ordim_o ASSIGNING <ls_ordim_o>
        WITH KEY tanum = iv_to.
      IF sy-subrc = 0 AND <ls_ordim_o>-nlpla = is_dest_lgpla-lgpla.

        IF is_request-quan = <ls_ordim_o>-vsolm.
*       no change in quantities -> no additional check needed
          rv_check_needed = abap_false.
          RETURN.
        ELSE.
*       subtract outdated quantity from buffered qty for bin
          upd_qty_buffer_entry(
            iv_operand = c_sub
            iv_matid   = <ls_ordim_o>-matid
            iv_quan    = <ls_ordim_o>-vsolm
            iv_unit    = <ls_ordim_o>-meins ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD sel_max_qty_lptyp.
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
* Entries in database table ZTLPTYP_MAXQTY contain the maximum quantity
* of a product allowed on a storage bin type and storage type.
* The table is not delivered with EWM but has to be defined in
* customer namespace.
*
*     Table Structure:
*
*   |-----------------------------------
*   | FIELD   | KEY | DATA ELEMENT
*   |-----------------------------------
*   | MANDT	     X    MANDT
*   | LGNUM	     X    /SCWM/LGNUM
*   | LGTYP	     X    /SCWM/LGTYP
*   | LPTYP	     X    /SCWM/LVS_LPTYP
*   | MATID	     X    /SCWM/DE_MATID
*   | MAX_QTY	        /SCWM/DE_MAXQTY
*   | UOM	            /SCWM/DE_UNIT
*   |-----------------------------------

* In order to use this method, the table ZTLPTYP_MAXQTY has to be created.
* Below in this method, the SELECT on that table has to be uncommented.

* There is one plain SELECT statement because the table is mostly read
* and rarely written. That is why the table is not locked for reading.
*
* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
    DATA:
      ls_maxqty       TYPE  lty_s_lptyp_maxqty,
      lt_lptyp_maxqty TYPE  zcl_crud_ztlptyp_maxqty=>tt_lptyp_maxqty.
    FIELD-SYMBOLS:
      <ls_maxqty>      TYPE  lty_s_lptyp_maxqty.

    CLEAR: et_maxqty.

    IF is_request-movehu = wmegc_movehu_no.
      " only one product to check -> full key can be used for access
      READ TABLE mt_lptyp_maxqty INTO ls_maxqty
        WITH TABLE KEY
          lgnum = mv_lgnum
          lptyp = iv_lptyp
          lgtyp = is_request-lgtyp
          matid = is_request-matid.
      IF sy-subrc = 0.
        " entry found in buffer, no database search needed
        INSERT ls_maxqty INTO TABLE et_maxqty.
        RETURN.
      ENDIF.

      " no entry for material/bin type combination in buffer
      " check whether there are entries in the buffer at all
      READ TABLE mt_lptyp_maxqty TRANSPORTING NO FIELDS
        WITH KEY
          lgnum = mv_lgnum
          lptyp = iv_lptyp.
      IF sy-subrc = 0.
        " entries for storage bin type, but no restrictions for product
        RETURN.
      ENDIF.

    ELSE.
      " possibly more than one product to check, get all entries for bin type
      LOOP AT mt_lptyp_maxqty ASSIGNING <ls_maxqty>
        WHERE lgnum = mv_lgnum
          AND lptyp = iv_lptyp.
        INSERT <ls_maxqty> INTO TABLE et_maxqty.
      ENDLOOP.
      IF sy-subrc = 0.
        " entries found in buffer, no database search needed
        RETURN.
      ENDIF.

    ENDIF. " one material or multiple?

* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *
* Uncomment the SELECT statement below if you have created and activated ZTLPTYP_MAXQTY
* !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! *

* bulk read for all restrictions on storage bin type
* -> less DB activities for bulk creation of warehouse tasks into the same bin type

    NEW zcl_algorithm_facade( )->determine_bintyp_capacity(
      EXPORTING
        iv_lgnum        = mv_lgnum
        it_entitled     = VALUE #( ( entitled = is_request-entitled ) )
        is_select_crit  = VALUE #( lgtyp_r = VALUE #( ( sign = c_sign_i option = c_opt_eq low = is_request-lgtyp ) )
                                   lptyp_r = VALUE #( ( sign = c_sign_i option = c_opt_eq low = iv_lptyp ) )
                                   matid_r = VALUE #( ( sign = c_sign_i option = c_opt_eq low = is_request-matid ) ) )
      IMPORTING
        et_lptyp_maxqty = lt_lptyp_maxqty ).

    IF lines( lt_lptyp_maxqty ) = 0.
      " no entries found -> no restrictions
      CLEAR: et_maxqty.
      RETURN.
    ENDIF.

    DATA(ls_lpt_maxqty) = VALUE #( lt_lptyp_maxqty[ 1 ] OPTIONAL ).
    et_maxqty = VALUE #( ( lgnum   = ls_lpt_maxqty-lgnum
                           lgtyp   = ls_lpt_maxqty-lgtyp
                           lptyp   = ls_lpt_maxqty-lptyp
                           matid   = ls_lpt_maxqty-matid
                           max_qty = ls_lpt_maxqty-max_qty
                           uom     = ls_lpt_maxqty-uom ) ) .

  ENDMETHOD.


  METHOD update_messages.
    DATA:
      ls_message      TYPE bapiret2.

    ls_message-type       = sy-msgty.
    ls_message-id         = sy-msgid.
    ls_message-number     = sy-msgno.
    ls_message-message_v1 = sy-msgv1.
    ls_message-message_v2 = sy-msgv2.
    ls_message-message_v3 = sy-msgv3.
    ls_message-message_v4 = sy-msgv4.
    APPEND ls_message TO ct_messages.

  ENDMETHOD.


  METHOD upd_qty_buffer.
    FIELD-SYMBOLS:
      <ls_hu_qty>        TYPE lty_s_hu_qty.

    IF is_request-movehu = wmegc_movehu_no.
      upd_qty_buffer_entry(
        iv_operand = iv_operand
        iv_matid   = is_request-matid
        iv_quan    = is_request-quan
        iv_unit    = is_request-unit ).
    ELSE.
*   for HU-WTs several product quantities likely
      LOOP AT it_hu_matqty ASSIGNING <ls_hu_qty>.
        upd_qty_buffer_entry(
          iv_operand = iv_operand
          iv_matid   = <ls_hu_qty>-matid
          iv_quan    = <ls_hu_qty>-qty
          iv_unit    = <ls_hu_qty>-uom ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD upd_qty_buffer_entry.
    DATA:
      ls_bin_qty    TYPE lty_s_bin_stock.
    FIELD-SYMBOLS:
      <ls_bin_stock>   TYPE lty_s_bin_stock.

    READ TABLE mt_bin_stock ASSIGNING <ls_bin_stock>
      WITH TABLE KEY
        lgnum   = mv_lgnum
        bin_loc = mv_bin_loc
        matid   = iv_matid.
    IF sy-subrc <> 0.
      CLEAR ls_bin_qty.
      ls_bin_qty-lgnum    = mv_lgnum.
      ls_bin_qty-bin_loc  = mv_bin_loc.
      ls_bin_qty-matid    = iv_matid.
      ls_bin_qty-uom      = iv_unit.
      INSERT ls_bin_qty INTO TABLE mt_bin_stock ASSIGNING <ls_bin_stock>.
    ENDIF.

    IF <ls_bin_stock> IS ASSIGNED AND iv_operand = c_add.
      ADD iv_quan TO <ls_bin_stock>-qty.
    ELSEIF <ls_bin_stock> IS ASSIGNED AND iv_operand = c_sub.
      SUBTRACT iv_quan FROM <ls_bin_stock>-qty.
    ENDIF.

* update does not include updates to source bin or HU (e.g. removal of stock)
* since it is unknown which WT is going to be confirmed first

  ENDMETHOD.
ENDCLASS.
