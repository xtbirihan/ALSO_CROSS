class ZCL_CRS_HAZMAT definition
  public
  final
  create public .

public section.

  class-methods CREATE_DANGEROUS_PRODUCT
    importing
      !IV_MATNR type MATNR
      !IV_HAZMATSIGN type ZZ1_MHMSIGN01 .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRS_HAZMAT IMPLEMENTATION.


  METHOD create_dangerous_product.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MATNR) TYPE  MATNR
*"     VALUE(IV_HAZMATSIGN) TYPE  ZZ1_MHMSIGN01
*"----------------------------------------------------------------------
**********************************************************************
*& Key           : LH-260423
*& Request No.   : GAP-040 – Additional product master fields
**********************************************************************
*& Description (short)
*& Crea hazardous material master
**********************************************************************
    DATA: ls_hazmat TYPE bapi1078_dangerousgooddata,
          ls_return TYPE bapiret2.
    CONSTANTS:
      c_very_beginning TYPE d VALUE '00010101',
      c_neverending    TYPE d VALUE '99991231',
      c_val_area_adr   TYPE eservlid VALUE 'ADR'.

    DATA(lv_loghandle) = cl_log_ppf=>create_log(
      EXPORTING
        ip_object    = zif_wme_c=>gs_msgobj-zewm            " Object
        ip_subobject = zif_wme_c=>gs_msgsubobj-zdangerousg    " Subobject
        ip_ext_no    = |Product: { iv_matnr ALPHA = OUT }, Hazmat Sign: { iv_hazmatsign }|                 " External identification
    ).
    "If there is already a dangerous godd master the leave the logic
    SELECT FROM dgtmd
           FIELDS COUNT( * )
           WHERE matnr EQ @iv_matnr
             AND delflg EQ @space.
    IF sy-dbcnt NE 0.
      RETURN.
    ENDIF.

    DATA(ls_map) = zcl_crud_ztcross_map_hmco=>select_single_by_key( iv_hazmatsign ).

    IF ls_map IS INITIAL.
      RETURN.
    ENDIF.
    ls_hazmat-valid_from = c_very_beginning.
    ls_hazmat-valid_to = c_neverending.
    ls_hazmat-material = iv_matnr.
    ls_hazmat-val_area = ls_hazmat-regulation = c_val_area_adr.

    SPLIT ls_map-un_number AT space INTO ls_hazmat-category ls_hazmat-un_no	.
    ls_hazmat-class	= ls_map-dgcl.
    ls_hazmat-sub_class	= ls_map-dgsc.
    ls_hazmat-modtrancat = ls_map-mot.

    DATA(lt_hazmat) = VALUE ehs_dangerousgooddata_t( ( ls_hazmat ) ).
    DATA(lt_hazmatx) = VALUE ehs_dangergooddatax_t( ( material = iv_matnr valid_fromx = abap_true regulation = ls_hazmat-regulation
                                                        valid_tox = abap_true
                                                        categoryx    = abap_true
                                                        un_nox       = abap_true
                                                        classx       = abap_true
                                                        sub_classx   = abap_true
                                                        modtrancatx = abap_true
                                                        val_areax   = abap_true
                                                      ) ).

    DATA(lt_range) = VALUE ehs_dangerousgoodrange_t( ( sign = 'I' option = 'EQ' fieldname = 'VALDAT' low = ls_hazmat-valid_from )
                                                   ).
    CALL FUNCTION 'BAPI_DANGEROUSGOOD_SAVREPMUL'
      IMPORTING
        return             = ls_return
      TABLES
        dangerousgooddata  = lt_hazmat
        dangerousgooddatax = lt_hazmatx
        dangerousgoodrange = lt_range.

    IF ls_return IS NOT INITIAL.
      MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number
              WITH ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4 INTO DATA(lv_msg).
      cl_log_ppf=>add_message( ip_handle   = lv_loghandle ).
      cl_log_ppf=>save_log(
        EXPORTING
          ip_loghandle   = lv_loghandle                 " Log Handle
      ).

    ENDIF.
  ENDMETHOD.
ENDCLASS.
