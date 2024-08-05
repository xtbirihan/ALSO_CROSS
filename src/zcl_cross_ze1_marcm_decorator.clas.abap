CLASS zcl_cross_ze1_marcm_decorator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES zif_material_master_decorator .
    METHODS: fill_fields
      IMPORTING
        iv_fieldname_ewm TYPE fieldname
        iv_fieldname_erp TYPE fieldname
      CHANGING
        cs_f_marc_ueb    TYPE marc_ueb
        ct_res_fields    TYPE delfields_tt.
  PROTECTED SECTION.
  PRIVATE SECTION.
    DATA: ms_segdata TYPE zstr_ze1_marcm.
ENDCLASS.



CLASS ZCL_CROSS_ZE1_MARCM_DECORATOR IMPLEMENTATION.


  METHOD: fill_fields.
    ASSIGN COMPONENT iv_fieldname_erp OF STRUCTURE ms_segdata TO FIELD-SYMBOL(<lv_erp_value>).
    IF <lv_erp_value> IS ASSIGNED.
      IF <lv_erp_value> IS INITIAL.
        APPEND VALUE #( feldname = zif_material_master_decorator=>c_material_plant-database_table_name &&
                                   zif_material_master_decorator=>c_material_plant-dash &&
                                   |{ iv_fieldname_ewm }| )  TO ct_res_fields.

      ELSEIF <lv_erp_value> NE zif_material_master_decorator=>c_material_plant-no_data.

        ASSIGN COMPONENT iv_fieldname_ewm OF STRUCTURE cs_f_marc_ueb TO FIELD-SYMBOL(<lv_marc_ueb_field>).
        IF <lv_marc_ueb_field> IS  ASSIGNED.
          <lv_marc_ueb_field> = <lv_erp_value>.
          UNASSIGN: <lv_marc_ueb_field>.
        ENDIF.
      ENDIF.
      UNASSIGN: <lv_erp_value>.
    ENDIF.


  ENDMETHOD.


  METHOD zif_material_master_decorator~execute.

    TYPES: BEGIN OF lty_fields,
             fieldname_ewm TYPE fieldname,
             fieldname_erp TYPE fieldname,
           END OF lty_fields,
           ltty_fields TYPE TABLE OF lty_fields.
    DATA: lt_fields TYPE ltty_fields.

    IF is_f_cust_segment-segnam NE zif_material_master_decorator=>c_material_plant-segname.
      RETURN.
    ENDIF.

    ms_segdata = is_f_cust_segment-sdata.

    DO 5 TIMES.
      DATA(lv_const_ewm) = zif_material_master_decorator=>c_material_plant-material_plant &&
                           zif_material_master_decorator=>c_material_plant-dash           &&
                           zif_material_master_decorator=>c_material_plant-identtable     &&
                           |{ sy-index }| &&
                           zif_material_master_decorator=>c_material_plant-suffix_plt.

      DATA(lv_const_erp) = zif_material_master_decorator=>c_material_plant-material_plant &&
                           zif_material_master_decorator=>c_material_plant-dash           &&
                           zif_material_master_decorator=>c_material_plant-identtab       &&
                           |{ sy-index }|.
      ASSIGN zif_material_master_decorator=>(lv_const_ewm) TO FIELD-SYMBOL(<lv_cons_value_ewm>).
      ASSIGN zif_material_master_decorator=>(lv_const_erp) TO FIELD-SYMBOL(<lv_cons_value_erp>).

      IF <lv_cons_value_ewm> IS ASSIGNED AND
         <lv_cons_value_erp> IS ASSIGNED.

        me->fill_fields(
          EXPORTING
            iv_fieldname_ewm = CONV #( <lv_cons_value_ewm> )
            iv_fieldname_erp = CONV #( <lv_cons_value_erp> )
          CHANGING
            cs_f_marc_ueb    = cs_f_marc_ueb
            ct_res_fields    = ct_res_fields
        ).
        UNASSIGN: <lv_cons_value_ewm>, <lv_cons_value_erp>.
      ENDIF.
      CLEAR: lv_const_ewm, lv_const_erp.
    ENDDO.
    lt_fields = VALUE #(
      ( fieldname_ewm = zif_material_master_decorator=>c_material_plant-zz1_retail_plt             fieldname_erp = zif_material_master_decorator=>c_material_plant-zzretail )
      ( fieldname_ewm = zif_material_master_decorator=>c_material_plant-zz1_retaildescription_plt  fieldname_erp = zif_material_master_decorator=>c_material_plant-zzretail_descr )
      ( fieldname_ewm = zif_material_master_decorator=>c_material_plant-zz1_businessunit_plt       fieldname_erp = zif_material_master_decorator=>c_material_plant-zzbu )
      ( fieldname_ewm = zif_material_master_decorator=>c_material_plant-zz1_budescription_plt      fieldname_erp = zif_material_master_decorator=>c_material_plant-zzbubezei )
      ( fieldname_ewm = zif_material_master_decorator=>c_material_plant-zz1_opti_plt               fieldname_erp = zif_material_master_decorator=>c_material_plant-zzopti ) ).

    LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
      me->fill_fields(
        EXPORTING
          iv_fieldname_ewm =  <ls_fields>-fieldname_ewm
          iv_fieldname_erp =  <ls_fields>-fieldname_erp
        CHANGING
          cs_f_marc_ueb    = cs_f_marc_ueb
          ct_res_fields    = ct_res_fields
      ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
