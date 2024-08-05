*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_TEXTS.......................................*
TABLES: ZMV_TEXTS, *ZMV_TEXTS. "view work areas
CONTROLS: TCTRL_ZMV_TEXTS
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_TEXTS. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_TEXTS.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_TEXTS_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_TEXTS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_TEXTS_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_TEXTS_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_TEXTS.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_TEXTS_TOTAL.

*.........table declarations:.................................*
TABLES: ZCROSS_TEXTS                   .
