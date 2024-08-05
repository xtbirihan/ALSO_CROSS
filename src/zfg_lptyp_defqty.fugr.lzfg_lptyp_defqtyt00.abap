*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_LPTYP_DEFQTY................................*
TABLES: ZMV_LPTYP_DEFQTY, *ZMV_LPTYP_DEFQTY. "view work areas
CONTROLS: TCTRL_ZMV_LPTYP_DEFQTY
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_LPTYP_DEFQTY. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_LPTYP_DEFQTY.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_LPTYP_DEFQTY_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_LPTYP_DEFQTY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_LPTYP_DEFQTY_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_LPTYP_DEFQTY_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_LPTYP_DEFQTY.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_LPTYP_DEFQTY_TOTAL.

*.........table declarations:.................................*
TABLES: ZTLPTYP_DEFQTY                 .
