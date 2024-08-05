CLASS zcl_crs_texts_fiori_srch_table DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_text_upd,
        action TYPE xfeld.
        INCLUDE TYPE zcross_texts.
  TYPES END OF ty_text_upd.
    TYPES tt_text_upd TYPE STANDARD TABLE OF ty_text_upd WITH EMPTY KEY.

    CONSTANTS:
      c_action_d TYPE xfeld VALUE 'D',
      c_lang_e   TYPE lang VALUE 'E'.

    METHODS update_fiori_schh_tables
      IMPORTING
        it_text TYPE zcl_crs_texts_fiori_srch_table=>tt_text_upd.
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS inbound_deco
      IMPORTING
        is_text TYPE ty_text_upd.
    METHODS general_picking
      IMPORTING
        is_text TYPE ty_text_upd.
    METHODS kep_picking
      IMPORTING
        is_text TYPE ty_text_upd.
    METHODS sped_picking
      IMPORTING
        is_text TYPE ty_text_upd.
    METHODS packing
      IMPORTING
        is_text TYPE ty_text_upd.
ENDCLASS.



CLASS ZCL_CRS_TEXTS_FIORI_SRCH_TABLE IMPLEMENTATION.


  METHOD general_picking.
********************************************************************
*& Key          : <BSUGAREV>-Dec 4, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&      " ZZ1_2A0A04E0DE03 - Configuration table
*&      " ZZ1_9B8A6109847E - Text description table
********************************************************************
    IF is_text-action = c_action_d.
      DELETE FROM zz1_2a0a04e0de03 WHERE code = is_text-text_id.

      DELETE FROM zz1_9b8a6109847e WHERE code = is_text-text_id.

      RETURN.
    ENDIF.

    SELECT SINGLE * FROM zz1_2a0a04e0de03
      INTO @DATA(ls_conf)
     WHERE code = @is_text-text_id.
    IF sy-subrc <> 0.
      ls_conf-code = is_text-text_id.

      MODIFY zz1_2a0a04e0de03 FROM ls_conf.
    ENDIF.

    SELECT SINGLE * FROM zz1_9b8a6109847e
      INTO @DATA(ls_descr)
     WHERE code = @is_text-text_id.
    IF sy-subrc <> 0.
      ls_descr-code = is_text-text_id.
      ls_descr-language = c_lang_e.
      ls_descr-description = is_text-text.
    ELSE.
      ls_descr-description = is_text-text.
    ENDIF.

    MODIFY zz1_9b8a6109847e FROM ls_descr.
  ENDMETHOD.


  METHOD inbound_deco.
********************************************************************
*& Key          : <BSUGAREV>-Dec 4, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&      " ZZ1_9E4B5BD0C579 - Text description table
*&      " zz1_49f2a0b1a237 - Configuration table
*&
********************************************************************
    IF is_text-action = c_action_d.
      DELETE FROM zz1_49f2a0b1a237 WHERE code = is_text-text_id.

      DELETE FROM zz1_9e4b5bd0c579 WHERE code = is_text-text_id.

      RETURN.
    ENDIF.

    SELECT SINGLE * FROM zz1_49f2a0b1a237
      INTO @DATA(ls_conf)
     WHERE code = @is_text-text_id.
    IF sy-subrc <> 0.
      ls_conf-code = is_text-text_id.

      MODIFY zz1_49f2a0b1a237 FROM ls_conf.
    ENDIF.

    SELECT SINGLE * FROM zz1_9e4b5bd0c579
      INTO @DATA(ls_descr)
     WHERE code = @is_text-text_id.
    IF sy-subrc <> 0.
      ls_descr-code = is_text-text_id.
      ls_descr-language = c_lang_e.
      ls_descr-description = is_text-text.
    ELSE.
      ls_descr-description = is_text-text.
    ENDIF.

    MODIFY zz1_9e4b5bd0c579 FROM ls_descr.

  ENDMETHOD.


  METHOD kep_picking.
********************************************************************
*& Key          : <BSUGAREV>-Dec 4, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&      " zz1_293d6e6e49b7 - Configuration table
*&      " zz1_531a4845e26e - Text description table
********************************************************************
    IF is_text-action = c_action_d.
      DELETE FROM zz1_293d6e6e49b7 WHERE code = is_text-text_id.

      DELETE FROM zz1_531a4845e26e WHERE code = is_text-text_id.

      RETURN.
    ENDIF.

    SELECT SINGLE * FROM zz1_293d6e6e49b7
      INTO @DATA(ls_conf)
     WHERE code = @is_text-text_id.
    IF sy-subrc <> 0.
      ls_conf-code = is_text-text_id.

      MODIFY zz1_293d6e6e49b7 FROM ls_conf.
    ENDIF.

    SELECT SINGLE * FROM zz1_531a4845e26e
      INTO @DATA(ls_descr)
     WHERE code = @is_text-text_id.
    IF sy-subrc <> 0.
      ls_descr-code = is_text-text_id.
      ls_descr-language = c_lang_e.
      ls_descr-description = is_text-text.
    ELSE.
      ls_descr-description = is_text-text.
    ENDIF.

    MODIFY zz1_531a4845e26e FROM ls_descr.
  ENDMETHOD.


  METHOD packing.
********************************************************************
*& Key          : <BSUGAREV>-Dec 4, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&      " ZZ1_AAE9D8CBA40C - Configuration table
*&      " ZZ1_7890A60602C7 - Text description table
********************************************************************
    IF is_text-action = c_action_d.
      DELETE FROM zz1_aae9d8cba40c WHERE code = is_text-text_id.

      DELETE FROM zz1_7890a60602c7 WHERE code = is_text-text_id.

      RETURN.
    ENDIF.

    SELECT SINGLE * FROM zz1_aae9d8cba40c
      INTO @DATA(ls_conf)
     WHERE code = @is_text-text_id.
    IF sy-subrc <> 0.
      ls_conf-code = is_text-text_id.

      MODIFY zz1_aae9d8cba40c FROM ls_conf.
    ENDIF.

    SELECT SINGLE * FROM zz1_7890a60602c7
      INTO @DATA(ls_descr)
     WHERE code = @is_text-text_id.
    IF sy-subrc <> 0.
      ls_descr-code = is_text-text_id.
      ls_descr-language = c_lang_e.
      ls_descr-description = is_text-text.
    ELSE.
      ls_descr-description = is_text-text.
    ENDIF.

    MODIFY zz1_7890a60602c7 FROM ls_descr.
  ENDMETHOD.


  METHOD sped_picking.
********************************************************************
*& Key          : <BSUGAREV>-Dec 4, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&      " ZZ1_1B11338828F0 - Configuration table
*&      " ZZ1_26360ED1B8FA - Text description table
********************************************************************
    IF is_text-action = c_action_d.
      DELETE FROM zz1_1b11338828f0 WHERE code = is_text-text_id.

      DELETE FROM zz1_26360ed1b8fa WHERE code = is_text-text_id.

      RETURN.
    ENDIF.

    SELECT SINGLE * FROM zz1_1b11338828f0
      INTO @DATA(ls_conf)
     WHERE code = @is_text-text_id.
    IF sy-subrc <> 0.
      ls_conf-code = is_text-text_id.

      MODIFY zz1_1b11338828f0 FROM ls_conf.
    ENDIF.

    SELECT SINGLE * FROM zz1_26360ed1b8fa
      INTO @DATA(ls_descr)
     WHERE code = @is_text-text_id.
    IF sy-subrc <> 0.
      ls_descr-code = is_text-text_id.
      ls_descr-language = c_lang_e.
      ls_descr-description = is_text-text.
    ELSE.
      ls_descr-description = is_text-text.
    ENDIF.

    MODIFY zz1_26360ed1b8fa FROM ls_descr.
  ENDMETHOD.


  METHOD update_fiori_schh_tables.
********************************************************************
*& Key          : <BSUGAREV>-Dec 4, 2023
*& Request No.  :
********************************************************************
*& Description  :
*&
*&
********************************************************************
    LOOP AT it_text ASSIGNING FIELD-SYMBOL(<ls_text>) WHERE action IS NOT INITIAL.
      CASE <ls_text>-text_type.
        WHEN zif_wme_c=>gc_text_types-inbound_deco.
          inbound_deco( <ls_text> ).
        WHEN zif_wme_c=>gc_text_types-general_material_picking.
          general_picking( <ls_text> ).
        WHEN zif_wme_c=>gc_text_types-kep_material_picking.
          kep_picking( <ls_text> ).
        WHEN zif_wme_c=>gc_text_types-sped_material_picking.
          sped_picking( <ls_text> ).
        WHEN zif_wme_c=>gc_text_types-material_packing.
          packing( <ls_text> ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
