*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_LGTYP_ALGO..................................*
TABLES: ZMV_LGTYP_ALGO, *ZMV_LGTYP_ALGO. "view work areas
CONTROLS: TCTRL_ZMV_LGTYP_ALGO
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_LGTYP_ALGO. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_LGTYP_ALGO.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_LGTYP_ALGO_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_LGTYP_ALGO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_LGTYP_ALGO_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_LGTYP_ALGO_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_LGTYP_ALGO.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_LGTYP_ALGO_TOTAL.

*.........table declarations:.................................*
TABLES: ZTLGTYP_ALGO                   .
