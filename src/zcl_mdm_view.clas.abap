CLASS zcl_mdm_view DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_class_instance
      RETURNING VALUE(return_object) TYPE REF TO zcl_mdm_view.

    METHODS set_user_parameter
      IMPORTING iv_parameter_id    TYPE usr05-parid
                iv_parameter_value TYPE usr05-parva.

    METHODS get_user_parameter
      IMPORTING iv_parameter_id           TYPE usr05-parid
      RETURNING VALUE(rv_parameter_value) TYPE usr05-parva.

    METHODS return_user_parameter
      CHANGING cv_lgnum         TYPE /scwm/pi_s_ui_value_set-lgnum
               cv_entitled      TYPE /scwm/pi_s_ui_value_set-entitled
               cv_entitled_desc TYPE bu_namep_l.

    METHODS get_parameter_from_memory
      IMPORTING iv_parameter_id           TYPE usr05-parid
      RETURNING VALUE(rv_parameter_value) TYPE usr05-parva.

    METHODS show_popup
      CHANGING  cv_lgnum         TYPE /scwm/pi_s_ui_value_set-lgnum
                cv_entitled      TYPE /scwm/pi_s_ui_value_set-entitled
      RETURNING VALUE(rv_change) TYPE abap_boolean.

    METHODS get_entitled_descritpion
      IMPORTING iv_entitled             TYPE /scwm/pi_s_ui_value_set-entitled
      RETURNING VALUE(rv_entitled_desc) TYPE bu_namep_l.

  PRIVATE SECTION.
    CLASS-DATA mo_view TYPE REF TO zcl_mdm_view.

    METHODS partner_check
      IMPORTING iv_partner TYPE but000-partner.

ENDCLASS.



CLASS ZCL_MDM_VIEW IMPLEMENTATION.


  METHOD get_class_instance.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  :
    "********************************************************************
    return_object = COND #( WHEN mo_view IS BOUND THEN mo_view ELSE NEW #( ) ).
    IF mo_view IS INITIAL.
      mo_view = return_object.
    ENDIF.
  ENDMETHOD.


  METHOD get_entitled_descritpion.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read address data of business partner
    "********************************************************************
    TYPES:
      BEGIN OF ltys_data_ship_from,
        partner_guid TYPE bu_partner_guid,
        partner      TYPE bu_partner,
        name         TYPE bu_namep_l,
        adr          TYPE /scmb/mdl_adress_tab,
      END OF ltys_data_ship_from.

    DATA ls_key            TYPE /scmb/mdl_partner_key_str.
    DATA ls_data_ship_from TYPE ltys_data_ship_from.

    ls_key-partner = iv_entitled.

    TRY.
        CALL FUNCTION '/SCMB/MDL_PARTNER_READ'
          EXPORTING
            is_key  = ls_key
          IMPORTING
            es_data = ls_data_ship_from.
      CATCH /scmb/cx_mdl ##NO_HANDLER.
    ENDTRY.
    rv_entitled_desc = ls_data_ship_from-name.
  ENDMETHOD.


  METHOD get_parameter_from_memory.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Read paramater value from memory
    "********************************************************************
    GET PARAMETER ID iv_parameter_id FIELD rv_parameter_value.
  ENDMETHOD.


  METHOD get_user_parameter.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Get default user parameters
    "********************************************************************
    CALL FUNCTION 'G_GET_USER_PARAMETER'
      EXPORTING
        parameter_id    = iv_parameter_id
      IMPORTING
        parameter_value = rv_parameter_value.
  ENDMETHOD.


  METHOD partner_check.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Bussiness partner check
    "********************************************************************
    CALL FUNCTION 'BUP_PARTNER_CHECK'
      EXPORTING
        i_partner                 = iv_partner
        iv_req_blk_msg            = abap_true
        iv_check_chng_actvt       = abap_true
      EXCEPTIONS
        partner_not_found         = 1
        role_not_found            = 2
        role_with_dfval_not_found = 3
        wrong_parameters          = 4
        internal_error            = 5
        blocked_partner           = 6
        OTHERS                    = 7.
    IF sy-subrc <> 0 AND sy-subrc = 6.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid   = zif_c_mdm_tool=>c_blocked_partner
                                        mv_msgv1 = CONV #( iv_partner ) ).
    ENDIF.
  ENDMETHOD.


  METHOD return_user_parameter.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Return user parameters
    "********************************************************************
    cv_lgnum = get_parameter_from_memory( zif_c_mdm_tool=>c_user_parameters-lgn ).
    IF cv_lgnum IS INITIAL.
      cv_lgnum = get_user_parameter( zif_c_mdm_tool=>c_user_parameters-lgn ).
    ENDIF.

    cv_entitled = get_parameter_from_memory( zif_c_mdm_tool=>c_user_parameters-entitled ).
    IF cv_entitled IS INITIAL.
      cv_entitled = get_user_parameter( zif_c_mdm_tool=>c_user_parameters-entitled ).
    ENDIF.

    IF cv_lgnum IS INITIAL OR cv_entitled IS INITIAL.
      show_popup( CHANGING cv_lgnum    = cv_lgnum
                           cv_entitled = cv_entitled ).
    ENDIF.
    IF cv_entitled IS NOT INITIAL.
      cv_entitled_desc = get_entitled_descritpion( iv_entitled = cv_entitled ).
    ENDIF.
    IF cv_lgnum IS INITIAL OR cv_entitled IS INITIAL.
      RAISE EXCEPTION NEW zcx_mdm_tool( textid = zif_c_mdm_tool=>c_empty_default_value ).
    ENDIF.
    IF cv_entitled IS NOT INITIAL.
      partner_check( cv_entitled ).
    ENDIF.
  ENDMETHOD.


  METHOD set_user_parameter.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : Set default user parameters
    "********************************************************************
    CALL FUNCTION 'G_SET_USER_PARAMETER'
      EXPORTING
        parameter_id    = iv_parameter_id
        parameter_value = iv_parameter_value
      EXCEPTIONS
        does_not_exist  = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      " Implement suitable error handling here
    ENDIF.
  ENDMETHOD.


  METHOD show_popup.
    "********************************************************************
    "*& Key          : TBIRIHAN 2023
    "*& Request No.  : GAP-006 Master data maintenance tool
    "********************************************************************
    "*& Description  : User parameters pop-up screen at the begging of ZMDM tx
    "********************************************************************
    DATA(lv_lgnum) = cv_lgnum.
    DATA(lv_entitled) = cv_entitled.

    CALL FUNCTION 'Z_MDM_USER_PARAMS'
      CHANGING
        cv_lgnum    = cv_lgnum
        cv_entitled = cv_entitled.
    IF lv_lgnum <> cv_lgnum OR lv_entitled <> cv_entitled.
      rv_change = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
