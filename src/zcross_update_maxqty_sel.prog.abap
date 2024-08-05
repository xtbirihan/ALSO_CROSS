*&---------------------------------------------------------------------*
*& Include          ZCROSS_UPDATE_MAXQTY_SEL
*&---------------------------------------------------------------------*

TABLES:
  /scwm/s_lagp_mon_f4.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-b01.
  PARAMETERS: p_lgnum TYPE /scwm/lgnum OBLIGATORY.
  SELECT-OPTIONS: so_ent FOR /scwm/s_lagp_mon_f4-entitled OBLIGATORY,
                  so_lgtyp FOR /scwm/s_lagp_mon_f4-lgtyp,
                  so_lptyp FOR /scwm/s_lagp_mon_f4-lptyp,
                  so_matnr FOR /scwm/s_lagp_mon_f4-matnr.
  PARAMETERS: p_full   TYPE xfeld RADIOBUTTON GROUP rg1 DEFAULT 'X',
              p_append TYPE xfeld RADIOBUTTON GROUP rg1,
              p_del    TYPE xfeld RADIOBUTTON GROUP rg1.
SELECTION-SCREEN END OF BLOCK b01.
