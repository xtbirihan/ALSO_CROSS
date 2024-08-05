*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_BLACKCND....................................*
TABLES: ZMV_BLACKCND, *ZMV_BLACKCND. "view work areas
CONTROLS: TCTRL_ZMV_BLACKCND
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_BLACKCND. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_BLACKCND.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_BLACKCND_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_BLACKCND.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_BLACKCND_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_BLACKCND_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_BLACKCND.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_BLACKCND_TOTAL.

*.........table declarations:.................................*
TABLES: ZTCROSS_BLACKCND               .
