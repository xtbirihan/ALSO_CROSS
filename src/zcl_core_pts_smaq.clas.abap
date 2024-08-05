CLASS zcl_core_pts_smaq DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES /scwm/if_ex_core_pts_smaq .
    INTERFACES if_badi_interface .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CORE_PTS_SMAQ IMPLEMENTATION.


  method /SCWM/IF_EX_CORE_PTS_SMAQ~CHECK_SMAQ_LGTYPG.
  endmethod.


  METHOD /scwm/if_ex_core_pts_smaq~decide_smaq_use.
**********************************************************************
*& Request No.   : TE1K900125 Main Request / GAP-042
*& Author        : Alper Ahmedov
*& e-mail        : alper.ahmedov@qinlox.com
*& Date          : 27.03.2023
**********************************************************************
*& Description (short)
**********************************************************************

    BREAK-POINT ID zcg_badi.

    IF is_ltap-reqqty > 0.
      IF is_ltap-act_type EQ zif_wme_c=>gs_act_typ-repl.
        zcl_core_ltap=>set_ltap( is_ltap = is_ltap ).
      ELSE.
        zcl_core_ltap=>set_suom( iv_suom = is_ltap-suom ).
      ENDIF.
    ENDIF.

*    IF
*    elseif zcl_core_ltap=>get_ltap( ) IS NOT INITIAL.
*      zcl_core_ltap=>clear_ltap( ).
*    ENDIF.


*  IF repl -> take whole ltap
*    ELSE -> take only suom
  ENDMETHOD.
ENDCLASS.
