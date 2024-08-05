class ZCL_SLOT_MAXQTY definition
  public
  final
  create public .

public section.

  methods CONSTRUCTOR .
  class-methods GET_INSTANCE
    returning
      value(RO_SLOT_MAXQTY) type ref to ZCL_SLOT_MAXQTY .
  class-methods SET_MAXQTY
    importing
      !IV_MAXQTY_PLAN type /SCWM/DE_MAXQTY_PLAN .
  class-methods GET_MAXQTY
    returning
      value(RV_MAXQTY_PLAN) type /SCWM/DE_MAXQTY_PLAN .
protected section.
private section.

  class-data MO_SLOT_MAXQTY type ref to ZCL_SLOT_MAXQTY .
  class-data MV_MAXQTY_PLAN type /SCWM/DE_MAXQTY_PLAN .
ENDCLASS.



CLASS ZCL_SLOT_MAXQTY IMPLEMENTATION.


  method CONSTRUCTOR.
  endmethod.


  METHOD get_instance.

    IF mo_slot_maxqty IS NOT BOUND.

      mo_slot_maxqty = NEW zcl_slot_maxqty( ).

    ENDIF.

    ro_slot_maxqty = mo_slot_maxqty.

  ENDMETHOD.


  METHOD get_maxqty.
    rv_maxqty_plan = mv_maxqty_plan.
  ENDMETHOD.


  METHOD set_maxqty.

    IF iv_maxqty_plan EQ 0.
      RETURN.
    ENDIF.

    mv_maxqty_plan = iv_maxqty_plan.

  ENDMETHOD.
ENDCLASS.
