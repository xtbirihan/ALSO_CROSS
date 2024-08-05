FUNCTION z_mdm_mastercarton_addition.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MATID) TYPE  /SCWM/DE_MATID
*"  CHANGING
*"     REFERENCE(CS_ADDITIONAL_DATA) TYPE  ZSTR_ADDITIONAL_DATA
*"     REFERENCE(CS_WAREHOUSE_DATA) TYPE  ZSTR_WAREHOUSE_DATA OPTIONAL
*"     REFERENCE(CS_STORAGE_TYPE_DATA) TYPE  ZSTR_STORAGE_TYPE_DATA
*"       OPTIONAL
*"     REFERENCE(CT_UNIT_OF_MEASURES) TYPE  ZTT_UNIT_OF_MEASUERES
*"       OPTIONAL
*"     REFERENCE(CV_IS_MC_ADDITIONAL_MAINTAINED) TYPE  XFELD OPTIONAL
*"----------------------------------------------------------------------
  DATA: lv_matnr(18).
  CLEAR: gs_additional_data,
         gs_warehouse_data,
         gs_storage_type_data,
         gt_unit_of_measures,
         gv_tote_quanity.

  gs_additional_data = cs_additional_data.
  zstr_additional_data = cs_additional_data.

  gs_warehouse_data    = cs_warehouse_data.
  gs_storage_type_data = cs_storage_type_data.
  gt_unit_of_measures  = ct_unit_of_measures.
  gv_matid             = iv_matid.

  gs_initials = cs_additional_data.

  gv_is_mc_additional_maintained = cv_is_mc_additional_maintained.
  DATA(lt_pmat_totes) = NEW zcl_packmmat_algo( cs_warehouse_data-lgnum )->get_pmat_totes( ).

  zcl_param=>get_parameter(
    EXPORTING
      iv_lgnum     = cs_warehouse_data-lgnum
      iv_process   = zif_param_const=>c_zcross_0005
      iv_parameter = zif_param_const=>c_pmat_tote
    IMPORTING
      ev_constant  = DATA(lv_tote_material) ).

  lv_matnr =  |{ lv_tote_material  ALPHA = IN }|.
  DATA(ls_material) = VALUE #( ct_unit_of_measures[ 1 ] OPTIONAL ).

  IF lv_tote_material IS NOT INITIAL AND ls_material-meins IS NOT INITIAL AND ls_material-meins IS NOT INITIAL.
    DATA(ls_pmat_tote) = VALUE #( lt_pmat_totes[ matnr = lv_matnr ] OPTIONAL ).
    IF ls_pmat_tote-matid IS NOT INITIAL.
      TRY.
          NEW zcl_cuboid_algorithm( cs_warehouse_data-lgnum )->pack_by_best_pmat(
            EXPORTING
              it_materials   = VALUE #( ( matid = iv_matid quan = ls_material-umrez meins = ls_material-meins ) )
              it_packmat     = VALUE #( ( pmat_guid = ls_pmat_tote-matid ) )
            RECEIVING
              rt_pack_result = DATA(lt_pack_result) ).
        CATCH zcx_core_exception INTO DATA(lx_core).
      ENDTRY.
      IF lt_pack_result IS NOT INITIAL.
        DATA(ls_material_details) = VALUE #( lt_pack_result[ 1 ]-mat_details[ 1 ] OPTIONAL ).
        gv_tote_quanity = ls_material_details-pc_max.
        IF cs_additional_data-zz1_dirrpl_stt IS NOT INITIAL.
          CLEAR: zstr_additional_data-tote_quan.
        ELSE.
          zstr_additional_data-tote_quan = gv_tote_quanity.
        ENDIF.
      ENDIF.

    ENDIF.
  ENDIF.


  CALL SCREEN zif_c_mdm_tool=>c_screen-ps_800
    STARTING AT
      zif_c_mdm_tool=>c_master_carton_popup-start_column
      zif_c_mdm_tool=>c_master_carton_popup-start_row
    ENDING AT
      zif_c_mdm_tool=>c_master_carton_popup-end_column
      zif_c_mdm_tool=>c_master_carton_popup-end_end_row.

*  MOVE-CORRESPONDING zstr_additional_data TO cs_additional_data.
  cs_additional_data = gs_additional_data.
  cs_warehouse_data = gs_warehouse_data.
  cs_storage_type_data = gs_storage_type_data.
  cv_is_mc_additional_maintained = gv_is_mc_additional_maintained.
ENDFUNCTION.
