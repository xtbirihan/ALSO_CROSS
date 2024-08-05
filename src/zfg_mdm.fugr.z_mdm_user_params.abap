FUNCTION z_mdm_user_params.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  CHANGING
*"     REFERENCE(CV_LGNUM) TYPE  /SCWM/PI_S_UI_VALUE_SET-LGNUM OPTIONAL
*"     REFERENCE(CV_ENTITLED) TYPE  /SCWM/PI_S_UI_VALUE_SET-ENTITLED
*"       OPTIONAL
*"----------------------------------------------------------------------
  CLEAR: gs_default.
  gs_default-lgnum    = cv_lgnum.
  gs_default-entitled = cv_entitled.

  CALL SCREEN zif_c_mdm_tool=>c_screen-ps_700
    STARTING AT
      zif_c_mdm_tool=>c_user_param_popup-start_column
      zif_c_mdm_tool=>c_user_param_popup-start_row
    ENDING AT
      zif_c_mdm_tool=>c_user_param_popup-end_column
      zif_c_mdm_tool=>c_user_param_popup-end_end_row.

  cv_lgnum    = gs_default-lgnum.
  cv_entitled = gs_default-entitled.

ENDFUNCTION.
