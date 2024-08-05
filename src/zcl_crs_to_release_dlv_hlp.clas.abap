class ZCL_CRS_TO_RELEASE_DLV_HLP definition
  public
  final
  create public .

public section.

  class-methods GET_INSTANCE
    returning
      value(RO_INST) type ref to ZCL_CRS_TO_RELEASE_DLV_HLP .
  class-methods SET_FLAG
    importing
      !IV_FLAG type ABAP_BOOL .
  class-methods GET_FLAG
    returning
      value(RV_FLAG) type ABAP_BOOL .
protected section.
private section.

  class-data MO_INST type ref to ZCL_CRS_TO_RELEASE_DLV_HLP .
  class-data MO_FLAG type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_CRS_TO_RELEASE_DLV_HLP IMPLEMENTATION.


  METHOD get_flag.
**********************************************************************
*& Key           : RM-231016
*& Request No.   : GAP 084 - Block and Release of the ODO
**********************************************************************
*& Description (short)
*& Get flag
**********************************************************************

    rv_flag = mo_flag.

  ENDMETHOD.


  METHOD get_instance.
**********************************************************************
*& Key           : RM-231016
*& Request No.   : GAP 084 - Block and Release of the ODO
**********************************************************************
*& Description (short)
*& Get an instance of the class
**********************************************************************

    IF mo_inst IS NOT BOUND.
      mo_inst = NEW #( ).
    ENDIF.

    ro_inst = mo_inst.

  ENDMETHOD.


  METHOD set_flag.
**********************************************************************
*& Key           : RM-231016
*& Request No.   : GAP 084 - Block and Release of the ODO
**********************************************************************
*& Description (short)
*& Set flag
**********************************************************************

    mo_flag = iv_flag.

  ENDMETHOD.
ENDCLASS.
