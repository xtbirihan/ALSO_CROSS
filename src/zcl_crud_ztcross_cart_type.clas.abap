class ZCL_CRUD_ZTCROSS_CART_TYPE definition
  public
  final
  create public .

public section.

  types:
    tt_whse_mdef TYPE STANDARD TABLE OF ztcrs_whse_mdef WITH DEFAULT KEY .

  constants C_PALL_TYPE type ZDE_PACK_CARTONS_TYPE value 'PALL' ##NO_TEXT.
  constants C_MASTER_CARTON_TYPE type ZDE_PACK_CARTONS_TYPE value 'MASTCARTON' ##NO_TEXT.
  constants C_SHIPPING_CARTON_TYPE type ZDE_PACK_CARTONS_TYPE value 'SHIPCARTON' ##NO_TEXT.
  constants C_TOTE_TYPE type ZDE_PACK_CARTONS_TYPE value 'TOTE' ##NO_TEXT.

  class-methods SELECT_BY_CARTON_TYPE
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_PACK_CARTONS_TYPE type ZDE_PACK_CARTONS_TYPE
    returning
      value(RT_CARTONS_TYPE) type ZTT_PALL_CARTONS_TYPE .
  class-methods SELECT_BY_HUTYPE_GROUP
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_HUTYPGRP type /SCWM/DE_HUTYPGRP
    returning
      value(RV_PALL_CARTON_TYPE) type ZDE_PACK_CARTONS_TYPE .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CRUD_ZTCROSS_CART_TYPE IMPLEMENTATION.


  METHOD select_by_carton_type.
********************************************************************
*& Key          : <AYORDANOV>-01.12.2023 14:06:41
*& Request No.  :
********************************************************************
*& Description
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT *
      FROM ztcross_cart_typ
     WHERE lgnum            = @iv_lgnum AND
           pall_carton_type = @iv_pack_cartons_type
      INTO TABLE @rt_cartons_type.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
        SUBKEY sy-uzeit
        FIELDS ' ZTCROSS_CART_TYP'
                 iv_pack_cartons_type.
    ENDIF.

  ENDMETHOD.


  METHOD select_by_hutype_group.
********************************************************************
*& Key          : <AYORDANOV>-01.12.2023 14:06:41
*& Request No.  :
********************************************************************
*& Description
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE FROM ztcross_cart_typ
      FIELDS pall_carton_type
     WHERE lgnum            = @iv_lgnum AND
           hutypgrp         = @iv_hutypgrp
      INTO @rv_pall_carton_type.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
       SUBKEY sy-uzeit
        FIELDS 'ZTCROSS_CART_TYP'
               iv_hutypgrp
               rv_pall_carton_type .
    ENDIF.

  ENDMETHOD.
ENDCLASS.
