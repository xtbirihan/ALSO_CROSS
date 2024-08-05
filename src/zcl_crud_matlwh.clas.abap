class ZCL_CRUD_MATLWH definition
  public
  final
  create public .

public section.

  class-methods SELECT_SINGLE_BY_MATID
    importing
      !IV_MATID type /SCWM/DE_MATID
    exporting
      !RS_MATLWH type /SAPAPO/MATLWH .
protected section.
private section.
ENDCLASS.



CLASS ZCL_CRUD_MATLWH IMPLEMENTATION.


  METHOD select_single_by_matid.

    DATA: lv_matid22 TYPE /sapapo/matid.

    IF iv_matid IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SCMB/MDL_GUID_CONVERT'
      EXPORTING
        iv_guid16 = iv_matid
      IMPORTING
        ev_guid22 = lv_matid22.

    SELECT SINGLE *
      FROM /sapapo/matlwh
      WHERE matid EQ @lv_matid22
      INTO @DATA(ls_matlwh).

    IF sy-subrc <> 0 .
      MESSAGE ID sy-msgid
              TYPE sy-msgty
              NUMBER sy-msgno
              WITH sy-msgv1
                   sy-msgv2
                   sy-msgv3
                   sy-msgv4.
    ENDIF.

    rs_matlwh = ls_matlwh.

  ENDMETHOD.
ENDCLASS.
