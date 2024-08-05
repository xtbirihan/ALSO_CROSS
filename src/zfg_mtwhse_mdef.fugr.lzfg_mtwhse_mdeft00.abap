*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZMV_WHSE_MDEF...................................*
TABLES: ZMV_WHSE_MDEF, *ZMV_WHSE_MDEF. "view work areas
CONTROLS: TCTRL_ZMV_WHSE_MDEF
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZMV_WHSE_MDEF. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZMV_WHSE_MDEF.
* Table for entries selected to show on screen
DATA: BEGIN OF ZMV_WHSE_MDEF_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZMV_WHSE_MDEF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WHSE_MDEF_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZMV_WHSE_MDEF_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZMV_WHSE_MDEF.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZMV_WHSE_MDEF_TOTAL.

*.........table declarations:.................................*
TABLES: ZTCRS_WHSE_MDEF                .
