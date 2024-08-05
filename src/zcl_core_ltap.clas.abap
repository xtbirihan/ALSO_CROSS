CLASS zcl_core_ltap DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS get_ltap
      RETURNING
        VALUE(rs_ltap) TYPE /scwm/ltap .
    CLASS-METHODS set_ltap
      IMPORTING
        iv_reqqty TYPE /scwm/de_quantity OPTIONAL
        !is_ltap  TYPE /scwm/ltap.
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_core_ltap) TYPE REF TO zcl_core_ltap .
    CLASS-METHODS clear_ltap .
    CLASS-METHODS set_ltap_cancelled
      IMPORTING
        !is_ltap TYPE /scwm/ltap .
    CLASS-METHODS get_ltap_cancelled
      RETURNING
        VALUE(rs_ltap) TYPE /scwm/ltap .
    CLASS-METHODS get_suom
      RETURNING
        VALUE(rv_suom) TYPE /scwm/de_aunit .
    CLASS-METHODS set_suom
      IMPORTING
        !iv_suom TYPE /scwm/de_aunit .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mo_core_ltap TYPE REF TO zcl_core_ltap .
    CLASS-DATA ms_ltap TYPE /scwm/ltap .
    CLASS-DATA ms_ltap_cancelled TYPE /scwm/ltap .
    CLASS-DATA mv_suom TYPE /scwm/de_aunit .
ENDCLASS.



CLASS ZCL_CORE_LTAP IMPLEMENTATION.


  METHOD clear_ltap.

    IF mo_core_ltap IS NOT INITIAL.
      CLEAR mo_core_ltap.
    ENDIF.

  ENDMETHOD.


  METHOD get_instance.

    IF mo_core_ltap IS NOT BOUND.

      mo_core_ltap = NEW zcl_core_ltap( ).

    ENDIF.

    ro_core_ltap = mo_core_ltap.

  ENDMETHOD.


  METHOD get_ltap.

    rs_ltap = ms_ltap.

  ENDMETHOD.


  METHOD get_ltap_cancelled.

    rs_ltap = ms_ltap_cancelled.

  ENDMETHOD.


  METHOD get_suom.

    rv_suom = mv_suom.

  ENDMETHOD.


  METHOD set_ltap.

    IF is_ltap IS INITIAL.
      RETURN.
    ENDIF.

    ms_ltap = is_ltap.

    IF ms_ltap-reqqty IS INITIAL AND iv_reqqty IS NOT INITIAL.
      ms_ltap-reqqty = iv_reqqty.
    ENDIF.

  ENDMETHOD.


  METHOD set_ltap_cancelled.
    IF is_ltap IS INITIAL.
      RETURN.
    ENDIF.

    ms_ltap_cancelled = is_ltap.
  ENDMETHOD.


  METHOD set_suom.

    IF iv_suom IS INITIAL.
      RETURN.
    ENDIF.

    mv_suom = iv_suom.

  ENDMETHOD.
ENDCLASS.
