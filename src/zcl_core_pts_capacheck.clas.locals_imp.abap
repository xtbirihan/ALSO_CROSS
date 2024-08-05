*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
CLASS lcl_data_access DEFINITION.
  PUBLIC SECTION.
    INTERFACES:
      /scwm/if_ei_core_capa_data.
    ALIASES:
      get_material_uom  FOR /scwm/if_ei_core_capa_data~get_material_uom,
      read_hu           FOR /scwm/if_ei_core_capa_data~read_hu,
      select_stock      FOR /scwm/if_ei_core_capa_data~select_stock,
      read_to_dest_bin  FOR /scwm/if_ei_core_capa_data~read_to_dest_bin.
  PRIVATE SECTION.
    DATA:
      mt_mat_uom        TYPE  STANDARD TABLE OF /scwm/s_material_uom
                                                WITH NON-UNIQUE SORTED KEY sk_matid
                                                COMPONENTS matid,
      mt_mat_global     TYPE  SORTED TABLE OF /scwm/s_material_global
                                              WITH UNIQUE KEY primary_key
                                              COMPONENTS matid.
ENDCLASS.                    "lcl_data_access DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_data_access IMPLEMENTATION
*----------------------------------------------------------------------*
* wrapper class for data access to EWM entities
*----------------------------------------------------------------------*
CLASS lcl_data_access IMPLEMENTATION.

  METHOD /scwm/if_ei_core_capa_data~get_material_uom.
    DATA:
      lt_matid      LIKE it_matid,
      lt_mat_uom    TYPE /scwm/tt_material_uom,
      lt_mat_global TYPE /scwm/tt_material_global,
      lv_matid      TYPE /scwm/de_matid.
    FIELD-SYMBOLS:
      <mat_uom>     TYPE /scwm/s_material_uom,
      <mat_global>  TYPE /scwm/s_material_global.

    lt_matid = it_matid.
    LOOP AT lt_matid INTO lv_matid.
      READ TABLE mt_mat_global ASSIGNING <mat_global>
        WITH TABLE KEY matid = lv_matid.
      CHECK sy-subrc = 0.
      LOOP AT mt_mat_uom ASSIGNING <mat_uom> USING KEY sk_matid
                                                       WHERE matid = lv_matid.
        INSERT <mat_uom> INTO TABLE: mt_mat_uom, et_mat_uom.
      ENDLOOP.
      ASSERT sy-subrc = 0.
      INSERT <mat_global> INTO TABLE: mt_mat_global, et_mat_global.
      DELETE lt_matid.
    ENDLOOP.

    IF lt_matid IS NOT INITIAL.
      TRY.
          CALL FUNCTION '/SCWM/MATERIAL_READ_MULTIPLE'
            EXPORTING
              it_matid      = lt_matid
              iv_lgnum      = iv_lgnum
              iv_notext     = abap_true
            IMPORTING
              et_mat_global = lt_mat_global
              et_mat_uom    = lt_mat_uom
              et_mat_error  = et_mat_error.
        CATCH  /scwm/cx_md_interface    /scwm/cx_md_material_exist
               /scwm/cx_md_lgnum_locid  /scwm/cx_md.
          RAISE EXCEPTION TYPE /scwm/cx_core.
      ENDTRY.
      INSERT LINES OF lt_mat_global INTO TABLE: mt_mat_global, et_mat_global.
      INSERT LINES OF lt_mat_uom INTO TABLE: mt_mat_uom, et_mat_uom.
    ENDIF.

  ENDMETHOD.                    "/scwm/if_ei_core_capa_data~get_material_uom

  METHOD /scwm/if_ei_core_capa_data~read_hu.

    CALL FUNCTION '/SCWM/HU_READ'
      EXPORTING
        iv_appl    = gchu_appl_wme
        iv_top     = space
        iv_guid_hu = iv_guid_hu
      IMPORTING
        et_huhdr   = et_huhdr
        et_huitm   = et_huitm
      EXCEPTIONS
        OTHERS     = 1.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /scwm/cx_core.
    ENDIF.
  ENDMETHOD.                    "/scwm/if_ei_core_capa_data~read_hu


  METHOD /scwm/if_ei_core_capa_data~select_stock.
    DATA:
      lt_bin_loc      TYPE /scwm/tt_guid_loc,
      ls_bin_loc      TYPE /scwm/s_guid_loc.

    ls_bin_loc-guid_loc = iv_bin_loc.
    APPEND ls_bin_loc TO lt_bin_loc.

    CALL FUNCTION '/SCWM/SELECT_STOCK'
      EXPORTING
        iv_lgnum      = iv_lgnum
        it_guid_lgpla = lt_bin_loc
      IMPORTING
        et_huitm      = et_stockitm
        et_huhdr      = et_huhdr
      EXCEPTIONS
        error         = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /scwm/cx_core.
    ENDIF.
  ENDMETHOD.                    "/scwm/if_ei_core_capa_data~select_stock

  METHOD /scwm/if_ei_core_capa_data~read_to_dest_bin.
    CALL FUNCTION '/SCWM/TO_READ_DES'
      EXPORTING
        iv_lgnum     = is_lagp_dest-lgnum
        iv_lgtyp     = is_lagp_dest-lgtyp
        iv_lgpla     = is_lagp_dest-lgpla
      IMPORTING
        et_ordim_o   = et_ordim_o
      EXCEPTIONS
        not_found    = 0
        wrong_input  = 1
        foreign_lock = 2
        OTHERS       = 99.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE /scwm/cx_core.
    ENDIF.
  ENDMETHOD.

ENDCLASS.                    "lcl_data_access IMPLEMENTATION
