CLASS zcl_cross_handle_cust_bp_flds DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF gc_field_name,
        zz_picking_text    TYPE fieldname VALUE 'BUT000-ZZ_PICKING_TEXT',
        zz_sped_pick_text  TYPE fieldname VALUE 'BUT000-ZZ_SPED_PICK_TEXT',
        zz_packing_text    TYPE fieldname VALUE 'BUT000-ZZ_PACKING_TEXT',
        zz_kep_pick_text   TYPE fieldname VALUE 'BUT000-ZZ_KEP_PICK_TEXT ',
        zz_no_mix_hu_alwd  TYPE fieldname VALUE 'BUT000-ZZ_NO_MIX_HU_ALWD',
        zz_customer_prio   TYPE fieldname VALUE 'BUT000-ZZ_CUSTOMER_PRIO',
        zz_fixed_carr      TYPE fieldname VALUE 'BUT000-ZZ_FIXED_CARR',
        zz_fix_carr_serv1  TYPE fieldname VALUE 'BUT000-ZZ_FIX_CARR_SERV1',
        zz_fix_carr_serv2  TYPE fieldname VALUE 'BUT000-ZZ_FIX_CARR_SERV2',
        zz_fix_carr_serv3  TYPE fieldname VALUE 'BUT000-ZZ_FIX_CARR_SERV3',
        zz_fix_carr_serv4  TYPE fieldname VALUE 'BUT000-ZZ_FIX_CARR_SERV4',
        zz_fix_carr_serv5  TYPE fieldname VALUE 'BUT000-ZZ_FIX_CARR_SERV5',
        zz_unloading_text  TYPE fieldname VALUE 'BUT000-ZZ_UNLOADING_TEXT',
        zz_is_vas_customer TYPE fieldname VALUE 'BUT000-ZZ_IS_VAS_CUSTOMER',
        zz_vas_text        TYPE fieldname VALUE 'BUT000-ZZ_VAS_TEXT',
        zz_cover_addr      TYPE fieldname VALUE 'COV',
        zz_return_addr     TYPE fieldname VALUE 'RET',
      END OF gc_field_name.

    CLASS-METHODS check_bp_fields
      IMPORTING
        !iv_view   TYPE bu_sicht
        !is_but000 TYPE but000 .
    CLASS-METHODS add_bus_message
      IMPORTING
        !iv_field TYPE fieldname OPTIONAL .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CROSS_HANDLE_CUST_BP_FLDS IMPLEMENTATION.


  METHOD add_bus_message.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Add message to the BP framework
**********************************************************************
    CALL FUNCTION 'BUS_MESSAGE_STORE'
      EXPORTING
        arbgb      = sy-msgid                 " Application Area
        msgty      = sy-msgty                 " Message category (S, I, E, W, A)
        txtnr      = sy-msgno                 " Message Number
        msgv1      = sy-msgv1            " Message Variable
        msgv2      = sy-msgv2            " Message Variable
        msgv3      = sy-msgv3            " Message Variable
        msgv4      = sy-msgv4            " Message Variable
        tbfld_strg = iv_field                 " Cursor Field / Bright Fields
      .
  ENDMETHOD.


  METHOD check_bp_fields.
**********************************************************************
*& Key           : LH-041223
*& Request No.   : GAP-095 – "FD Specific logistical requirements in EWM”
**********************************************************************
*& Description (short)
*& Check fields in BP
**********************************************************************
    DATA: lv_msg TYPE string.
    DEFINE check_text.
      IF is_but000-&1 IS NOT INITIAL
        AND NOT lo_text_handling->check_text( iv_text_type =  zif_wme_c=>gc_text_types-&2
                                              iv_text_id =    is_but000-&1 ).

        MESSAGE e021(zmc_crs) INTO lv_msg.
        add_bus_message( iv_field = gc_field_name-&1 ).
      ENDIF.
    END-OF-DEFINITION.

    DATA(lo_text_handling) = zcl_text_handling=>get_instance_general( ).
    check_text: zz_picking_text cust_general_picking,
                zz_sped_pick_text cust_sped_picking,
                zz_packing_text cust_packing,
                zz_kep_pick_text cust_kep_picking,
                zz_unloading_text vend_unloading.

    IF is_but000-zz_fixed_carr IS NOT INITIAL.
      SELECT COUNT(*) FROM ztout_fixed_carr
             WHERE carr EQ @is_but000-zz_fixed_carr.
      IF sy-dbcnt EQ 0.
        MESSAGE e023(zmc_crs) INTO lv_msg.
        add_bus_message( iv_field = gc_field_name-zz_fixed_carr ).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
