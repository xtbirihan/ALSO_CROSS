*&---------------------------------------------------------------------*
*& Include          ZCROSS_UPDATE_MAXQTY_F4
*&---------------------------------------------------------------------*



AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_lgtyp-low.
*
  lcl_update_maxqty=>value_request_lgtyp( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_lptyp-low.

  lcl_update_maxqty=>value_request_lptyp( ).

AT SELECTION-SCREEN ON VALUE-REQUEST FOR so_ent-low.

  lcl_update_maxqty=>value_request_entitled( ).
