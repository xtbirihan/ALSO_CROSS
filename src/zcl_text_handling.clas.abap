CLASS zcl_text_handling DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_text_handling .

    CLASS-METHODS get_instance_for_warehouse
      IMPORTING
        !iv_lgnum      TYPE /scwm/lgnum
      RETURNING
        VALUE(ro_inst) TYPE REF TO zif_text_handling .

    CLASS-METHODS get_instance_general
      RETURNING
        VALUE(ro_inst) TYPE REF TO zif_text_handling .

  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: mt_texts TYPE STANDARD TABLE OF zcross_texts.
    CLASS-DATA: st_texts TYPE STANDARD TABLE OF zcross_texts.
ENDCLASS.



CLASS ZCL_TEXT_HANDLING IMPLEMENTATION.


  METHOD get_instance_for_warehouse.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Instance for a warehouse, fill the text buffer
**********************************************************************
    DATA(lo_inst) = NEW zcl_text_handling( ).

    SELECT FROM zcross_texts
           FIELDS *
           WHERE lgnum EQ @iv_lgnum
      INTO TABLE @lo_inst->mt_texts.

    ro_inst = lo_inst.
  ENDMETHOD.


  METHOD get_instance_general.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& General instance for customer handling, if warehouse cannot be determined
**********************************************************************
    DATA(lo_inst) = NEW zcl_text_handling( ).

    IF st_texts IS INITIAL.
      SELECT FROM zcross_texts
             FIELDS *
        INTO TABLE @st_texts.
    ENDIF.

    ro_inst = lo_inst.

  ENDMETHOD.


  METHOD zif_text_handling~change_shlp.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Search help exit for the texts selection
**********************************************************************
    READ TABLE cs_shlp-interface REFERENCE INTO DATA(lr_shlp_if_text)
         WITH KEY shlpfield = 'TEXT_ID'.
    IF sy-subrc NE 0.
      RETURN.
    ENDIF.
    SPLIT lr_shlp_if_text->valfield AT '-' INTO DATA(lv_part1) DATA(lv_part2).
    IF lv_part2 IS INITIAL.
      lv_part2 = lv_part1.
    ENDIF.

    CASE lv_part2.
      WHEN 'ZZ_PICKING_TEXT'.
        DATA(lv_text_type) = zif_wme_c=>gc_text_types-cust_general_picking.
      WHEN 'ZZ_SPED_PICK_TEXT'.
        lv_text_type = zif_wme_c=>gc_text_types-cust_sped_picking.
      WHEN 'ZZ_PACKING_TEXT'.
        lv_text_type = zif_wme_c=>gc_text_types-cust_packing.
      WHEN 'ZZ_KEP_PICK_TEXT'.
        lv_text_type = zif_wme_c=>gc_text_types-cust_kep_picking.
      WHEN 'ZZ_UNLOADING_TEXT'.
        lv_text_type = zif_wme_c=>gc_text_types-vend_unloading.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    IF lv_text_type IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE cs_shlp-selopt REFERENCE INTO DATA(lr_selop_text_type)
         WITH KEY shlpfield = 'TEXT_TYPE'.
    IF sy-subrc NE 0.
      APPEND VALUE #( shlpname = cs_shlp-shlpname
                      shlpfield = 'TEXT_TYPE'
                    ) TO cs_shlp-selopt REFERENCE INTO lr_selop_text_type.
    ENDIF.
    lr_selop_text_type->sign      = 'I'.
    lr_selop_text_type->option    = 'EQ'.
    lr_selop_text_type->low       = lv_text_type.

  ENDMETHOD.


  METHOD zif_text_handling~check_text.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Check the text for existence
**********************************************************************
    IF  mt_texts IS NOT INITIAL AND line_exists( mt_texts[ text_type = iv_text_type text_id   = iv_text_id ] ).
      rv_exists = abap_true.
    ELSEIF mt_texts IS INITIAL AND line_exists( st_texts[ text_type = iv_text_type text_id   = iv_text_id ] ).
      rv_exists = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD zif_text_handling~create_pack_instr_string.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Concatenate text into one string separated by new line
**********************************************************************
    CHECK it_text IS NOT INITIAL.
    rv_pack_instr = it_text[ 1 ]-text.
    LOOP AT it_text INTO DATA(ls_text) FROM 2.
      rv_pack_instr = |{ rv_pack_instr }{ cl_abap_char_utilities=>cr_lf }{ ls_text-text }|.
    ENDLOOP.
  ENDMETHOD.


  METHOD zif_text_handling~get_texts_from_bp.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Get texts for a business partner
**********************************************************************

    SELECT SINGLE * FROM but000 INTO @DATA(ls_bp)
           WHERE partner = @iv_business_partner.
    LOOP AT it_text_type INTO DATA(lv_text_type).
      CASE lv_text_type.
        WHEN zif_wme_c=>gc_text_types-cust_kep_picking.
          DATA(lv_text_id) = ls_bp-zz_kep_pick_text.
        WHEN zif_wme_c=>gc_text_types-cust_packing.
          lv_text_id = ls_bp-zz_packing_text.
        WHEN zif_wme_c=>gc_text_types-cust_general_picking.
          lv_text_id = ls_bp-zz_picking_text.
        WHEN zif_wme_c=>gc_text_types-cust_sped_picking .
          lv_text_id = ls_bp-zz_sped_pick_text.
        WHEN zif_wme_c=>gc_text_types-vend_unloading.
          lv_text_id = ls_bp-zz_unloading_text.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
      IF lv_text_id IS NOT INITIAL.
        READ TABLE mt_texts INTO DATA(ls_text)
             WITH KEY text_type = lv_text_type
                      text_id   = lv_text_id.
        IF sy-subrc EQ 0.
          APPEND VALUE zif_text_handling~ty_text( text_type = lv_text_type text_id = lv_text_id text = ls_text-text )
                    TO rt_text.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF ls_bp-zz_fix_dlv_note_out EQ abap_true AND iv_no_del_note EQ abap_false.
      APPEND VALUE zif_text_handling~ty_text( text_type = space text_id = space text = 'Fix delivery note outside'(fdo) )
                TO rt_text.
    ENDIF.
  ENDMETHOD.


  METHOD zif_text_handling~get_texts_from_prod.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Get the texts from the product master
**********************************************************************
    DATA ls_mat_lgnum TYPE  /scwm/s_material_lgnum.

    TRY.
        CALL FUNCTION '/SCWM/MATERIAL_READ_SINGLE'
          EXPORTING
            iv_matid     = /scmb/cl_md_access_mdl=>get_md_access( )->get_prod( iv_prodno = iv_product )-prodid                 " Material GUID16  mit Konvertierungsexit
            iv_lgnum     = iv_lgnum                 " Lagernummer/Lagerkomplex
            iv_entitled  = iv_entitled
*           iv_lgtyp     =                  " Lagertyp
*           it_lgtyp     =                  " Tabelle von Lagertypen
          IMPORTING
            es_mat_lgnum = ls_mat_lgnum.                 " Material: Lagernummerspezifische Daten
      CATCH /scwm/cx_md_interface.       " WME MD Importing Parameter Ausnahmeklasse
      CATCH /scwm/cx_md_material_exist.  " WME MD Material existiert nicht
      CATCH /scwm/cx_md_mat_lgnum_exist. " Material in Lagernummer nicht gepflegt
      CATCH /scwm/cx_md_lgnum_locid.     " Lagernummer ist keiner APO-Lokation zugeordnet
      CATCH /scwm/cx_md.                 " WME MD allgemeine Ausnahmeklasse
      CATCH /scmb/cx_md_access.                 " WME MD allgemeine Ausnahmeklasse
    ENDTRY.

    IF ls_mat_lgnum IS INITIAL.
      RETURN.
    ENDIF.
    LOOP AT it_text_type INTO DATA(lv_text_type).
      CASE lv_text_type.
        WHEN zif_wme_c=>gc_text_types-general_material_picking.
          DATA(lv_text_id) = ls_mat_lgnum-zz1_picking_text_whd.

        WHEN zif_wme_c=>gc_text_types-inbound_deco.
          lv_text_id = ls_mat_lgnum-zz1_inb_deco_text_whd.

        WHEN zif_wme_c=>gc_text_types-kep_material_picking.
          lv_text_id = ls_mat_lgnum-zz1_kep_picking_text_whd.

        WHEN zif_wme_c=>gc_text_types-material_packing .
          lv_text_id = ls_mat_lgnum-zz1_packing_text_whd.

        WHEN zif_wme_c=>gc_text_types-sped_material_picking.
          lv_text_id = ls_mat_lgnum-zz1_sped_picking_text_whd.
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
      IF lv_text_id IS NOT INITIAL.
        READ TABLE mt_texts INTO DATA(ls_text)
             WITH KEY text_type = lv_text_type
                      text_id   = lv_text_id.
        IF sy-subrc EQ 0.
          APPEND VALUE zif_text_handling~ty_text( text_type = lv_text_type text_id = lv_text_id text = ls_text-text )
                    TO rt_text.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
