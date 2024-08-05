FUNCTION z_mdm_packspec.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_LGNUM) TYPE  /SCWM/LGNUM
*"     VALUE(IV_ENTITLED) TYPE  /SCWM/DE_ENTITLED
*"     VALUE(IV_MATNR) TYPE  MARA-MATNR
*"     VALUE(IV_PS_GROUP) TYPE  /SCWM/DE_PS_GROUP
*"     VALUE(IT_UNIT_OF_MEASURES) TYPE  ZTT_UNIT_OF_MEASURES
*"     VALUE(IS_MASTER_CARTON_ADITIONAL) TYPE  ZSTR_ADDITIONAL_DATA
*"----------------------------------------------------------------------


  NEW zcl_mdm_packspec(
    iv_lgnum = iv_lgnum
    iv_entitled = iv_entitled
    iv_matnr = iv_matnr
    iv_ps_group = iv_ps_group
    it_unit_of_measures = it_unit_of_measures
    is_master_carton_aditional = is_master_carton_aditional )->execute_packspec( ).


ENDFUNCTION.
