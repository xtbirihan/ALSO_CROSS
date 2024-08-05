class ZCL_CRUD_ZTCROSS_CAP_NUMS definition
  public
  final
  create public .

public section.

  types:
    tt_serials TYPE STANDARD TABLE OF gernr WITH EMPTY KEY .
  types:
    tt_ztcross_cap_nums TYPE STANDARD TABLE OF ztcross_cap_nums WITH EMPTY KEY .

  class-methods INSERT_MULTI_ENTRIES
    importing
      !IT_CAP_NUMS type TT_ZTCROSS_CAP_NUMS
      !IV_NO_COMMIT type ABAP_BOOL optional .
  class-methods SELECT_MULTI_BY_DOCNO
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DOCNO type /SCWM/SP_DOCNO_INT
    returning
      value(RT_CAP_NUMS) type TT_ZTCROSS_CAP_NUMS .
  class-methods SELECT_MULTI_BY_HUID
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_HUID type /SCWM/GUID_HU
    returning
      value(RT_CAP_NUMS) type TT_ZTCROSS_CAP_NUMS .
  class-methods SELECT_SINGLE_BY_KEY
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DOCNO type /SCWM/SP_DOCNO_INT
      !IV_ITEMNO type /SCWM/DE_ITEMNO_R
      !IV_SERIAL type GERNR
      !IV_IDTYPE type ZDE_ID_TYPE
    returning
      value(RS_CAP_NUMS) type ZTCROSS_CAP_NUMS .
  class-methods DELETE_MULTI_BY_DOCNO_ITEMNO
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DOCNO type /SCWM/SP_DOCNO_INT
      !IV_ITEMNO type /SCWM/DE_ITEMNO
      !IV_NO_COMMIT type ABAP_BOOL optional .
  class-methods DELETE_MULTI_BY_ENTRIES
    importing
      !IT_ENTRIES type TT_ZTCROSS_CAP_NUMS .
  class-methods DELETE_MULTI_BY_DOC_ITEM_HUID
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DOCNO type /SCWM/SP_DOCNO_INT
      !IV_ITEMNO type /SCWM/DE_ITEMNO
      !IV_GUID_HU type /SCWM/GUID_HU
      !IV_NO_COMMIT type ABAP_BOOL optional .
  class-methods MODIFY_MULTI_ENTRIES
    importing
      !IT_CAP_NUMS type TT_ZTCROSS_CAP_NUMS
      !IV_COMMIT type ABAP_BOOL optional .
  class-methods SELECT_MULTI_BY_FULL_SERIALS
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_SERIALS type TT_ZTCROSS_CAP_NUMS
    returning
      value(RT_CAP_NUMS) type TT_ZTCROSS_CAP_NUMS .
  class-methods SELECT_MULTI_BY_SERIALS
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IT_SERIALS type TT_SERIALS
    returning
      value(RT_CAP_NUMS) type TT_ZTCROSS_CAP_NUMS .
  class-methods SELECT_MULTI_BY_DOCNO_ITEMNO
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DOCNO type /SCWM/SP_DOCNO_INT
      !IV_ITEMNO type /SCWM/DE_ITEMNO
    returning
      value(RT_CAP_NUMS) type TT_ZTCROSS_CAP_NUMS .
  class-methods MODIFY_MULTI_HUID_SN
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_SRC_GUID_HU type /SCWM/GUID_HU
      !IV_DEST_GUID_HU type /SCWM/GUID_HU
      !IV_SN_GROUP type /SCWM/DE_SERID optional
      !IV_COMMIT type ABAP_BOOL optional .
  class-methods MODIFY_REGROUP_SN
    importing
      !IV_LGNUM type /SCWM/LGNUM
      !IV_DEST_GUID_HU type /SCWM/GUID_HU
      !IV_COMMIT type ABAP_BOOL optional .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-DATA mt_cap_nums TYPE tt_ztcross_cap_nums .
ENDCLASS.



CLASS ZCL_CRUD_ZTCROSS_CAP_NUMS IMPLEMENTATION.


  METHOD delete_multi_by_docno_itemno.
********************************************************************
*& Key          : <BSUGAREV>-Sep 12, 2023
*& Request No.  :
********************************************************************
*& Description  : Select single value by key fields
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    DELETE FROM ztcross_cap_nums
      WHERE lgnum = @iv_lgnum
        AND docno = @iv_docno
        AND itemno = @iv_itemno.

    DELETE mt_cap_nums
      WHERE lgnum = iv_lgnum
        AND docno = iv_docno
        AND itemno = iv_itemno.
    IF iv_no_commit EQ abap_false.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD delete_multi_by_doc_item_huid.
********************************************************************
*& Key          : <BSUGAREV>-Sep 12, 2023
*& Request No.  :
********************************************************************
*& Description  : Select single value by key fields
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    DELETE FROM ztcross_cap_nums
      WHERE lgnum   = @iv_lgnum
        AND docno   = @iv_docno
        AND itemno  = @iv_itemno
        AND guid_hu = @iv_guid_hu.

    DELETE mt_cap_nums
      WHERE lgnum   = iv_lgnum
        AND itemno  = iv_itemno
        AND guid_hu = iv_guid_hu.
    IF iv_no_commit EQ abap_false.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD delete_multi_by_entries.
********************************************************************
*& Key          : <BSUGAREV>-Oct 10, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    IF lines( it_entries ) = 0.
      RETURN.
    ENDIF.

    DELETE ztcross_cap_nums FROM TABLE it_entries.
    IF sy-subrc = 0.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD insert_multi_entries.
********************************************************************
*& Key          : <BSUGAREV>-Sep 13, 2023
*& Request No.  :
********************************************************************
*& Description  : add new entries in the table
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    CLEAR: mt_cap_nums.

    INSERT ztcross_cap_nums FROM TABLE it_cap_nums ACCEPTING DUPLICATE KEYS.
    IF iv_no_commit EQ abap_false.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD modify_multi_entries.
********************************************************************
*& Key          : <BSUGAREV>-Sep 13, 2023
*& Request No.  :
********************************************************************
*& Description  : add new entries in the table
*&
*&
********************************************************************
    DATA lt_cap_nums_db TYPE SORTED TABLE OF ztcross_cap_nums WITH UNIQUE KEY lgnum docno itemno serial id_type.

    BREAK-POINT ID zcg_db_crud.

    CLEAR: mt_cap_nums.

    DATA(lt_cap_nums) = it_cap_nums.

    GET TIME STAMP FIELD DATA(lv_current_ts).

    IF it_cap_nums IS NOT INITIAL.
      SELECT FROM ztcross_cap_nums FIELDS *
             FOR ALL ENTRIES IN @it_cap_nums
             WHERE lgnum   = @it_cap_nums-lgnum
               AND docno   = @it_cap_nums-docno
               AND itemno  = @it_cap_nums-itemno
               AND serial  = @it_cap_nums-serial
               AND id_type = @it_cap_nums-id_type
        INTO TABLE @lt_cap_nums_db.
    ENDIF.

    LOOP AT lt_cap_nums REFERENCE INTO DATA(lr_cap_nums).
      READ TABLE lt_cap_nums_db INTO DATA(ls_cap_nums_db)
            WITH TABLE KEY lgnum   = lr_cap_nums->lgnum
                           docno   = lr_cap_nums->docno
                           itemno  = lr_cap_nums->itemno
                           serial  = lr_cap_nums->serial
                           id_type = lr_cap_nums->id_type.
      IF sy-subrc NE 0.
        lr_cap_nums->created_by = sy-uname.
        lr_cap_nums->created_at = lv_current_ts.
      ELSE.
        lr_cap_nums->created_by = ls_cap_nums_db-created_by.
        lr_cap_nums->created_at = ls_cap_nums_db-created_at.
        lr_cap_nums->changed_by = sy-uname.
        lr_cap_nums->changed_at = lv_current_ts.
        lr_cap_nums->gi         = ls_cap_nums_db-gi.
      ENDIF.
    ENDLOOP.

    MODIFY ztcross_cap_nums FROM TABLE lt_cap_nums.

    IF iv_commit EQ abap_true.
      COMMIT WORK.
    ENDIF.
  ENDMETHOD.


  METHOD modify_multi_huid_sn.
********************************************************************
*& Key          : <AYORDANOV>-Jan 29, 2024
*& Request No.  :
********************************************************************
*& Description  : Change SN from one HU to another
*&
*&
********************************************************************

    DATA: lv_hutypgrp    TYPE /scwm/de_hutypgrp,
          ls_huhdr       TYPE /scwm/s_huhdr_int,
          lt_cap_nums_db TYPE SORTED TABLE OF ztcross_cap_nums WITH UNIQUE KEY lgnum docno itemno serial id_type.

    IF iv_dest_guid_hu IS INITIAL OR
       iv_src_guid_hu  IS INITIAL.
      RETURN.
    ENDIF.

    GET TIME STAMP FIELD DATA(lv_current_ts).

    " get all records SN from current source HU
    SELECT FROM ztcross_cap_nums FIELDS *
           WHERE lgnum   = @iv_lgnum AND
                 guid_hu = @iv_src_guid_hu
      INTO TABLE @lt_cap_nums_db.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " we can have scanned SN like grouping ident. So we should move all SN for this group
    IF iv_sn_group IS NOT INITIAL.
      DATA(lv_grp_mc) = VALUE #( lt_cap_nums_db[ full_serial_number = iv_sn_group ]-group_mc OPTIONAL ).
    ELSE.
      lv_grp_mc = lt_cap_nums_db[ 1 ]-group_mc.
    ENDIF.

    " check if destination is a pallet
    CALL FUNCTION '/SCWM/HUHEADER_READ'
      EXPORTING
        iv_appl     = wmegc_huappl_wme
        iv_guid_hu  = iv_dest_guid_hu
      IMPORTING
        es_huheader = ls_huhdr
      EXCEPTIONS
        not_found   = 1
        input       = 2
        error       = 3
        deleted     = 4
        OTHERS      = 99.

    IF sy-subrc <> 0.
      "must not happen because HU was queried before
      RETURN.
    ENDIF.

    IF zcl_crud_ztcross_cart_type=>select_by_hutype_group(
       EXPORTING
         iv_lgnum            = iv_lgnum                 " Warehouse Number/Warehouse Complex
         iv_hutypgrp         = ls_huhdr-hutypgrp ) = zcl_crud_ztcross_cart_type=>c_pall_type.   " Handling Unit Type Group
      DATA(lv_pallet) = abap_true.
    ELSE.
      lv_pallet = abap_false.
    ENDIF.

    LOOP AT lt_cap_nums_db ASSIGNING FIELD-SYMBOL(<ls_cap_num_src>)
                                                   WHERE group_mc = lv_grp_mc.

      DATA(ls_del_cap_nums) = <ls_cap_num_src>.

      <ls_cap_num_src>-created_by = sy-uname.
      <ls_cap_num_src>-created_at = lv_current_ts.
      <ls_cap_num_src>-guid_hu    = iv_dest_guid_hu.

      IF lv_pallet = abap_true.
        <ls_cap_num_src>-group_mc = 9999999999.
      ENDIF.

      DELETE ztcross_cap_nums  FROM ls_del_cap_nums.

    ENDLOOP.

    MODIFY ztcross_cap_nums FROM TABLE lt_cap_nums_db.

    IF iv_commit EQ abap_true.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD modify_regroup_sn.
********************************************************************
*& Key          : <AYORDANOV>-Jan 29, 2024
*& Request No.  :
********************************************************************
*& Description  : Change SN group
*&
*&
********************************************************************
    DATA: lv_group_mc    TYPE /scwm/dl_counter,
          lt_cap_nums_db TYPE SORTED TABLE OF ztcross_cap_nums WITH UNIQUE KEY lgnum docno itemno serial id_type.

    IF iv_dest_guid_hu IS INITIAL.
      RETURN.
    ENDIF.

    GET TIME STAMP FIELD DATA(lv_current_ts).

    SELECT FROM ztcross_cap_nums FIELDS *
           WHERE lgnum   = @iv_lgnum AND
                 guid_hu = @iv_dest_guid_hu AND
                 group_mc IS NOT INITIAL
      INTO TABLE @lt_cap_nums_db.

    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_cap_nums_db ASSIGNING FIELD-SYMBOL(<ls_cap_numdb_regroup>)
                             GROUP BY ( group_mc = <ls_cap_numdb_regroup>-group_mc )
                           ASSIGNING FIELD-SYMBOL(<ls_group_sn>).

      lv_group_mc = lv_group_mc + 1 .

      LOOP AT GROUP <ls_group_sn> ASSIGNING FIELD-SYMBOL(<ls_regroup_sn>).
        <ls_regroup_sn>-group_mc = lv_group_mc.
      ENDLOOP.
    ENDLOOP.

    MODIFY ztcross_cap_nums FROM TABLE lt_cap_nums_db.

    IF iv_commit EQ abap_true.
      COMMIT WORK.
    ENDIF.

  ENDMETHOD.


  METHOD select_multi_by_docno.
********************************************************************
*& Key          : <BSUGAREV>-Sep 12, 2023
*& Request No.  :
********************************************************************
*& Description  : Select single value by key fields
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM ztcross_cap_nums
      INTO TABLE @rt_cap_nums
     WHERE lgnum = @iv_lgnum
       AND docno = @iv_docno.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztcross_cap_nums' sy-datum sy-uzeit.
    ENDIF.

*    rt_cap_nums = mt_cap_nums.
  ENDMETHOD.


  METHOD SELECT_MULTI_BY_DOCNO_ITEMNO.
********************************************************************
*& Key          : <BSUGAREV>-Sep 12, 2023
*& Request No.  :
********************************************************************
*& Description  : Select single value by key fields
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM ztcross_cap_nums
      INTO TABLE @rt_cap_nums
     WHERE lgnum  = @iv_lgnum
       AND docno  = @iv_docno
       AND itemno = @iv_itemno.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztcross_cap_nums' sy-datum sy-uzeit.
    ENDIF.

*    rt_cap_nums = mt_cap_nums.
  ENDMETHOD.


  METHOD select_multi_by_full_serials.
********************************************************************
*& Key          : <AYORDANOV>-Jan 29, 2024
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    CHECK it_serials IS NOT INITIAL.

    SELECT * FROM ztcross_cap_nums
      INTO TABLE @rt_cap_nums
       FOR ALL ENTRIES IN @it_serials
     WHERE lgnum = @iv_lgnum
       AND full_serial_number = @it_serials-full_serial_number.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud
       FIELDS 'ZTCROSS_CAP_NUMS' sy-datum sy-uzeit it_serials.
    ENDIF.

  ENDMETHOD.


  METHOD select_multi_by_huid.
********************************************************************
*& Key          : <BSUGAREV>-Oct 10, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT * FROM ztcross_cap_nums
      INTO TABLE @rt_cap_nums
     WHERE lgnum = @iv_lgnum
       AND guid_hu = @iv_huid.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ZTCROSS_CAP_NUMS' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD SELECT_MULTI_BY_SERIALS.
********************************************************************
*& Key          : <BSUGAREV>-Oct 10, 2023
*& Request No.  :
********************************************************************
*& Description
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    CHECK it_serials is not INITIAL.

    SELECT * FROM ztcross_cap_nums
      INTO TABLE @rt_cap_nums
       FOR ALL ENTRIES IN @it_serials
     WHERE lgnum = @iv_lgnum
       AND serial = @it_serials-table_line.
    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ZTCROSS_CAP_NUMS' sy-datum sy-uzeit.
    ENDIF.

  ENDMETHOD.


  METHOD select_single_by_key.
********************************************************************
*& Key          : <BSUGAREV>-Sep 12, 2023
*& Request No.  :
********************************************************************
*& Description  : Select single value by key fields
*&
*&
********************************************************************
    BREAK-POINT ID zcg_db_crud.

    SELECT SINGLE * FROM ztcross_cap_nums
      INTO @rs_cap_nums
     WHERE lgnum = @iv_lgnum
       AND docno = @iv_docno
       AND itemno = @iv_itemno
       AND serial = @iv_serial
       AND id_type = @iv_idtype.

    IF sy-subrc <> 0.
      LOG-POINT ID zcg_db_crud FIELDS 'ztcross_cap_nums' sy-datum sy-uzeit.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
