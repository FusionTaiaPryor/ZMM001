REPORT zrm07mlbd MESSAGE-ID m7.
************************************************************************
*     REPORT ZRM07MLBD   (tcode ZMM001)                                *
*       Report criado como cópia do standard RM07MLBD (tcode MB5B)     *
*       em 26.11.2019                                                  *
************************************************************************

INITIALIZATION.
  "{ begin of MGV_GENERATED_RM07MLBD001
  DATA: lt_sel_dtel TYPE  rsseldtel OCCURS 0,
        ls_sel_dtel TYPE  rsseldtel.

  ls_sel_dtel-name = 'MFRPN'.
  ls_sel_dtel-kind = 'S'.
  ls_sel_dtel-datenelment = 'MFRPN'.
  APPEND ls_sel_dtel TO lt_sel_dtel.

  CALL FUNCTION 'SELECTION_TEXTS_MODIFY_DTEL'
    EXPORTING
      program                     = sy-repid
    TABLES
      sel_dtel                    = lt_sel_dtel
    EXCEPTIONS
      program_not_found           = 1
      program_cannot_be_generated = 2
      OTHERS                      = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  "{ end of MGV_GENERATED_RM07MLBD001
*ENHANCEMENT-POINT rm07mlbd_g4 SPOTS es_rm07mlbd STATIC.
*ENHANCEMENT-POINT rm07mlbd_g5 SPOTS es_rm07mlbd.
*ENHANCEMENT-POINT rm07mlbd_g6 SPOTS es_rm07mlbd STATIC.
*ENHANCEMENT-POINT rm07mlbd_g7 SPOTS es_rm07mlbd.

************************************************************************
*     REPORT RM07MLBD   (Transaktionscode MB5B)                        *
************************************************************************

* new function April 2012 EH                                "n1710850
* - Installed ability for secondary database connection     "n1710850
*   configuration via Tx HDBC                               "n1710850

* improvement September 2011 MS                             "n1558298
* - MSEG was enhanced by most important fields of MKPF      "n1558298
*   These fields are getting redundantly filled by each GM  "n1558298
* - MB5B was improved to select BUDAT from these fields of  "n1558298
*   table MSEG instead of MKPF. Due to this logic the       "n1558298
*   expensive MSEG-MKPF join can be improved exorbitantly   "n1558298

* correction March 2011 PR                                  "n1560727
* Ensure that valuation stock option can also be used for   "n1560727
* individual valuation types that only exist in EBEW/QBEW   "n1560727

* correction Sep 2010 EH                                    "n1509405
* Removed filter function in append lists as it is no more  "n1509405
* supported by the ALV                                      "n1509405

* correction Nov. 2009 PR
* Batches where a MCHB entry no longer exists but a MCHA    "n1404822
* entry is available are not correctly considered when      "n1404822
* flag 'XNOMCHB' is set                                     "n1404822

* correction Oct. 2009 BS                                   "n1399766
* In case you use a layout with filter criteria for         "n1399766
* 'Opening Stock' or 'Closing Stock' you will get a         "n1399766
* short dump.                                               "n1399766

* correction Sept. 2009 PR                                  "n1390970
* When the *Totals only' settings is used and the report is "n1390970
* run for storage/batch stock, the plant is not considered  "n1390970
* when creating the detail document list                    "n1390970

* correction Apr. 2009 MS                                   "n1333069
* The report texts t-096, t-099 ,t-100 and t-103 are not    "n1333069
* properly displayed in all languages. Same with tooltips   "n1333069

* correction Oct. 2008 TW                                   "n1265674
* for active ingredient materials MB5B should not display   "n1265674
* the 141 and 142 movements for the selection valuated      "n1265674
* stock to avoid wrong beginning stock amount.              "n1265674

* correction Nov. 2007 MS                                   "n1117067
* The dates are displayed in the wrong format in the output "n1117067
* list header. No conversion was done.                      "n1117067

* correction June 2007 MS                                   "n1064332
* fields "Date to" and "Date from" are wrong displayed in   "n1064332
* layout change popup in the mode                           "n1064332
* "totals only - hierarchical list"                         "n1064332

* correction Jan. 2007 MS                                   "n1018717
* convert unit of measurement from internal to external     "n1018717
* format. This was wrong displayed in header of output list "n1018717

* correction Nov. 2006 TW                                   "n999530
* plant description should appear behind plant number but   "n999530
* nevertheless the plant description should not be vissible "n999530
* for all possible selection combinations the transaction   "n999530
* MB5L could be started for.                                "n999530

* correction June 2006 MM                                   "n951316
* - do not allow to form sums for the columns quantity and  "n951316
*   value in the mode "totals only - hierarchical list"     "n951316

* correction Feb. 2006 MM                                   "n921165
* - improve performance processing internal tables          "n921165
*                                                           "n921165
* - improve performance of the access database tables MKPF  "n921165
*   and MSEG using database specific hints for the database "n921165
*   systems :                                               "n921165
*   - DB2 and ORACLE :                                      "n921165
*     - one SELECT command with DBI hints                   "n921165
*   - DB6, Informix, MaxDB, MSSQL :                         "n921165
*     - 3 SELECT commands who could be choosen using 3 new  "n921165
*       related parameters pa_dbstd, pa_dbmat, pa_dbdat     "n921165
*                                                           "n921165
* - some dynamic BREAK-POINT from checkpoint-group          "n921165
*   MMIM_REP_MB5B implemented, use transaction SAAB         "n921165

* correction Nov. 2005 MM                                   "n890109
* allow the inter active functions 'Specify drill-down'     "n890109
* and 'Choose' from the menu 'Settings -> Summation levels' "n890109
* These functions are activated by default. The flag        "n890109
* "g_cust_sum_levels" in include "RM07MLBD_CUST_FIELDS"     "n890109
* allows to activate or inactivate these functions          "n890109

* correction Sept 2005 MM                                   "n878753
* reports runs although the user has no authorization       "n878753

* correction Aug. 2005 MM                                   "n856424
* - the start and end dates were shown incorrectly in the   "n856424
*   headlines in the mode valuated stock                    "n856424
* - the fields "entry time", "entry date", and "User" are   "n856424
*   are not filled filled for price change documents        "n856424

* MB5B improved regarding accessibilty                      "n773673
* - the top_of_page headlines are now printed with the
*   ALV tools
* - the non ALV sum function was replaced by a hierarchical
*   and a non-hierarchical ALV lists
* - double click in the sum lists shows the normal list for
*   the selected material
* - the function special processing was changed : This
*   function depended on the retail system settings or
*   a modifiaction. Now this function depends on the found
*   MM documents

* correction June 2004 MM                                   "n747306
* wrong the assignment of the MM and FI documents for data  "n747306
* constellation : n MM doc items --> 1 FI doc item          "n747306

* ABAP-Preprocessor removed                                 "n599218 A
* - this version is for release 4.6C and higher             "n599218 A
* - process database table OBEW always                      "n599218 A
* - IS-OIL specific functions :                             "n599218 A
*   - define IS-OIL workings fields                         "n599218 A
*   - transport and process these fields only when          "n599218 A
*     structure MSEG comprise these fields                  "n599218 A

* Improvements :                       Dec. 2003 MM         "n599218
* - print the page numbers                                  "n599218
*                                                           "n599218
* - send warnings and error messages only when report is    "n599218
*   launched / advoid warnings when user changes entries on "n599218
*   the selection screen                                    "n599218
* - send warning M7 689 when user does not restrict the     "n599218
*   database in dialog or print mode                        "n599218
* - send warning M7 393 when user deletes the initial       "n599218
*   display variant                                         "n599218
*                                                           "n599218
* - allow to process the fields MAT_KDAUF, MAT_KDPOS, and   "n599218
*   MAT_PSPNR from release 4.5B and higher                  "n599218
*                                                           "n599218
* - show the current activity and the progress              "n599218
*                                                           "n599218
* - error message 'programmfehler' improved                 "n599218
*                                                           "n599218
* - new categories for scope of list                        "n599218
*                                                           "n599218
* - use function module for database commit for the update  "n599218
*   of the parameters in table ESDUS. This allows to record "n599218
*   this transaction for a batch input session using        "n599218
*   transaction SHDB                                        "n599218
*                                                           "n599218
* - reset the entries for plant when valuation level is     "n599218
*   is company code and mode is valuated stock              "n599218
*                                                           "n599218
* - enable this report to run in the webreporting mode      "n599218

* Dec. 2002 MM                                              "n571473
* the definition of the selection screen moved from include "n571473
* RM07MLBP into this report                                 "n571473

* Sept 2002 MM                                              "n555246
* log function tax auditor                                  "n555246

* note 547170 :                              August 2002 MM "n547170
* - representation of tied empties improved                 "n547170
*   active this function automatically in retail systems    "n547170
* - FORM routines without preprocessor commands and without "n547170
*   text elements moved to the new include reports          "n547170
*   RM07MLBD_FORM_01and RM07MLBD_FORM_02                    "n547170
* - the function module FI_CHECK_DATE of note 486477 will   "n547170
*   be processed when it exists                             "n547170
* - function and documentation of parameter XONUL improved  "n547170
* - display MM documents with MIGO or MB03 depending from   "n547170
*   the release                                             "n547170
* - get and save the parameters per user in dialog mode     "n547170
*   only in release >= 4.6                                  "n547170

* the following items were improved with note 497992        "n497992
*
* - wrong results when remaining BSIM entries contain       "n497992
*   an other quantity unit as material master MEINS         "n497992
* - improve check FI summarization                          "n497992
* - the messages M7 390, M7 391, and M7 392                 "n497992
* - definition of field g_f_repid for all releases          "n497992
*
* - incomplete key for access of internal table IT134M      "n497992
*   causes wrong plant selection                            "n497992
* - the function "no reversal movement" did not surpress    "n497992
*   the original movements; fields "SJAHR" was moved from   "n497992
*   from report RM07MLBD_CUST_FIELDS to RM07MLBD            "n497992
* - process valuated subcontractor stock from database      "n497992
*   table OBEW if it exists                                 "n497992
* - if FI summarization is active process warning M7 390    "n497992
*   for stock type = valuated stock                         "n497992
* - the user wants to restrict the movement type : process  "n497992
*   warning M7 391                                          "n497992
* - the user wants to surpress the reversal movements :     "n497992
*   process warning M7 392                                  "n497992
* - consider special gain/loss-handling of IS-OIL           "n497992
* - automatic insert of field WAERS currency key into the   "n497992
*   field catalogue :                                       "n497992
*   - at least one ref. field is active -> WAERS active     "n497992
*   - all reference fields are hidden   -> WAERS hidden     "n497992
* - the length of sum fields for values was increased       "n497992

* - customizing for the selection of remaining BSIM entries "n497992
* - customizing for the processing of tied empties          "n497992

* separate time depending authorization for tax auditor     "n486477

* additional fields are displayed in wrong format           "n480130

* report RM07MLBD and its includes improved  Nov 2001       "n451923
* - merging FI doc number into table G_T_MSEG_LEAN improved "n451923
* - handling of the short texts improved                    "n451923
* - some types and data definitions -> include RM07MLDD     "n451923
*----------------------------------------------------------------------*
* error for split valuation and valuated special stock      "n450764
*----------------------------------------------------------------------*
* process 'goods receipt/issue slip' as hidden field        "n450596
*----------------------------------------------------------------------*
* error at start date : material without stock has value    "n443935
*----------------------------------------------------------------------*
* wrong results for docs with customer consignment "W"      "n435403
*----------------------------------------------------------------------*
* error during data selection for plants                    "n433765
*----------------------------------------------------------------------*
* report RM07MLBD and its includes improved  May 10th, 2001 "n400992
*----------------------------------------------------------------------*
* !!! IMPORTANT : DO NOT CHANGE OR DELETE THE COMMENT LINES !!!        *
*----------------------------------------------------------------------*
*
* - consider the material number during looking for FI documents
*
* - field "g_cust_color" in include report "RM07MLBD_CUST_FIELDS"
*   allows the customer to activate or inactivate the colors in the
*   lines with the documents.
*
* - error during calcuation of start stock for special stock "M"
*
* - valuted stocks required : no documents found ? continue and
*   process empty document table
*
* - the length of sum fields for quantities has been increased
*   to advoid decimal overflow
*
* - table ORGAN is replaced by G_T_ORGAN
*   - it is filled by the following ways :
*     - at process time at selection screen if the
*       user wants the selection via cc or plant
*     - otherwise after the database selection of the stock
*       tables
*   - it contains less data fields
*   - it contains all entries twice, for binary search
*     with plant or valuation area
*
* - selection of databases MKPF and MSEG in one SELECT
*   command with an inner JOIN
*
* - authority checks after the database selections
*
* - result of database selection from the both database tables
*   MSEG and MKPF in working table G_F_MSEG_LEAN instead of
*   the tables IMSEG and IMKPF
*
* - the number of processed data fields was reduced
* - the user has the possibility to increase the number of
*   the processed fields deleting the '*' in the types-command
*   in include report RM07MLBD_CUST_FIELDS
*
* - the creation of the field catalog for the ALV considers
*   only the fields of structure G_S_MSEG_LEAN
*
* - the new table G_T_BELEG contains the results for the ALV.
*   the number of fields of table G_T_BELEG corresponds with
*   the number of fields of table G_T_MSEG_LEAN.
*
* - the functions "define breakdown" and "choose" are inactivated
*   in the menue, because they are are not carried out correctly
*   in all blocks of the list
*
************************************************************************
*     Anzeige der Materialbestände in einem Zeitintervall              *
************************************************************************
*  Der Report gliedert sich im wesentlichen in folgende Verarbeitungs- *
*  blöcke:                                                             *
*  1) Definition des Einstiegsbildes und Vorbelegung einzelner         *
*     Selektionsfelder, sowie Prüfung der eingegebenen Selektions-     *
*     parameter und Berechtigungsprüfung                               *
*  2) Lesen der aktuellen Bestandswerte                                *
*  3) Lesen und Verarbeiten der Materialbelege                         *
*  4) Berechnung der Bestandswerte zu den vorgegebenen Datümern        *
*  5) Ausgabe der Bestände und Materialbelege                          *
************************************************************************

  TYPE-POOLS:  imrep,                   " Typen Bestandsführungsreporting
               slis.                    " Typen Listviewer

* allow the interactions 'Specify drill-down' etc..         "n890109
  TYPE-POOLS : kkblo.          "Korrektur ALV                 "n890109

  INCLUDE zrm07mldd.
*  INCLUDE:  rm07mldd.     " reportspezifische Datendefinitionen

* controls the "expensive" checks like authorization, etc.  "n878753
  DATA : g_flag_launched(01)   TYPE  c.                     "n878753

* working fields for the performance improvements           "n921165
  DATA : g_flag_db_parameters(01) TYPE  c,                  "n921165
         g_f_database(03)         TYPE  c,                  "n921165
                                                            "n921165
         g_cnt_radio              TYPE  i,                  "n921165
         g_cnt_error_dba          TYPE  i.                  "n921165
                                                            "n921165
  DATA : g_tabix_set   TYPE  sy-tabix,                      "n921165
         g_flag_sorted TYPE  c.                             "n921165
                                                            "n921165
* these flags allow to ignore multiple stops at dynamic     "n921165
* BREAK-POINTs in LOOPs                                     "n921165
  DATA : BEGIN OF g_flag_break,                             "n921165
           b1(01) TYPE  c   VALUE 'X',                      "n921165
           b2(01) TYPE  c   VALUE 'X',                      "n921165
           b3(01) TYPE  c   VALUE 'X',                      "n921165
           b4(01) TYPE  c   VALUE 'X',                      "n921165
           b5(01) TYPE  c   VALUE 'X',                      "n921165
           b6(01) TYPE  c   VALUE 'X',                      "n921165
           b7(01) TYPE  c   VALUE 'X',                      "n921165
           b8(01) TYPE  c   VALUE 'X',                      "n921165
         END OF g_flag_break.                               "n921165

  DATA: d_from(10) TYPE c,                                  "n1117067
        d_to(10)   TYPE c.                                  "n1117067

  DATA:  g_f_msegex_act(1) TYPE c.                          "n1558298

*----------------- note 1481757 typedefinition for error-messages-------*

  TYPES: BEGIN OF mbarc_message,                            "n1481757
           msgid LIKE sy-msgid,                             "n1481757
           msgno LIKE sy-msgno,                             "n1481757
           msgv1 LIKE sy-msgv1,                             "n1481757
           msgv2 LIKE sy-msgv2,                             "n1481757
           msgv3 LIKE sy-msgv3,                             "n1481757
           msgv4 LIKE sy-msgv4,                             "n1481757
         END OF mbarc_message.                              "n1481757
  TYPES: mbarc_message_tab TYPE mbarc_message OCCURS 0.     "n1481757
  DATA: archive_messages  TYPE mbarc_message_tab WITH HEADER LINE, "n1481757
        g_flag_answer(01) TYPE  c.                          "n1481757

*----------end of note 1481757 typedefinition for error-messages------*

  DATA: gv_switch_ehp6ru TYPE boole_d.

  DATA: dbcon        TYPE dbcon_name,                       "n1710850
        dbcon_active TYPE dbcon_name.                       "n1710850
  CONSTANTS: c_hdb_dbcon_get TYPE funcname VALUE 'MM_HDB_DBCON_GET', "n1710850
             c_hdb_subappl   TYPE program  VALUE 'MB5B'.    "n1710850


  DATA: gv_ui_opt_active TYPE abap_bool.                    "1790231

  DATA: gv_where_clause   TYPE string,                      "n_1899544
        gv_not_authorized TYPE string.                      "n_1899544

*-----------------------------------------------------------
* DEFINE THE SELECTION SCREEN HERE
*-----------------------------------------------------------
  "Dados do Item
  SELECTION-SCREEN BEGIN OF BLOCK database-selection WITH FRAME TITLE TEXT-001.
  SELECT-OPTIONS werks FOR t001w-werks MEMORY ID wrk
                                        NO-EXTENSION
                                        NO INTERVALS
                                        OBLIGATORY.
  SELECT-OPTIONS: matnr FOR mard-matnr MEMORY ID mat
                                       MATCHCODE OBJECT mat1.
  SELECT-OPTIONS mtart FOR mara-mtart.  "Tipo de material
  SELECT-OPTIONS lgort FOR t001l-lgort. "Deposito
  SELECT-OPTIONS lifnr FOR mseg-lifnr.  "Fornecedor
  SELECT-OPTIONS kunnr FOR mseg-kunnr.  "Cliente
  SELECT-OPTIONS aufnr FOR mseg-aufnr.  "Ordem do cliente
  SELECT-OPTIONS bwart FOR mseg-bwart.  "Tipo de movimento
  PARAMETERS sobkz LIKE mseg-sobkz.     "Código de estoque especial
  SELECTION-SCREEN SKIP.
  SELECT-OPTIONS: datum FOR mkpf-budat NO-EXTENSION.
  SELECTION-SCREEN END OF BLOCK database-selection.

  "Dados de cabeçalho
  SELECTION-SCREEN BEGIN OF BLOCK zblock WITH FRAME TITLE TEXT-143.
  SELECT-OPTIONS mblnr FOR mkpf-mblnr.  "Número de doc de material
  SELECT-OPTIONS usnam FOR mkpf-usnam.  "Nome do usuario
  SELECT-OPTIONS vgart FOR mkpf-vgart.  "Tipo de operacao
  SELECT-OPTIONS xblnr FOR mkpf-xblnr.  "Nota fiscal
  SELECTION-SCREEN END OF BLOCK zblock.

  "Campos da MB5B usados nas rotinas standards
  SELECT-OPTIONS mfrpn FOR mara-mfrpn MEMORY ID mpn MATCHCODE OBJECT htn NO-DISPLAY.
  SELECT-OPTIONS bukrs FOR t001-bukrs MEMORY ID buk NO-DISPLAY.
  SELECT-OPTIONS hkont FOR bseg-hkont MODIF  ID hkt NO-DISPLAY.
  SELECT-OPTIONS charg FOR mchb-charg NO-DISPLAY.
  SELECT-OPTIONS bwtar FOR mbew-bwtar NO-DISPLAY.
  PARAMETERS lgbst    LIKE am07m-lgbst          DEFAULT ' ' NO-DISPLAY. "Estoque de depósito/de lotes
  PARAMETERS bwbst    LIKE am07m-bwbst          DEFAULT 'X' NO-DISPLAY. "Estoque avaliado
  PARAMETERS sbbst    LIKE am07m-sbbst          DEFAULT ' ' NO-DISPLAY. "Estoque especial
  PARAMETERS pa_wdzer LIKE am07m-mb5b_xonul     DEFAULT 'X' NO-DISPLAY. "sem estoque inicial/final
  PARAMETERS pa_wdzew LIKE am07m-mb5b_xonul     DEFAULT 'X' NO-DISPLAY. "s/estq.inicial; c/estq.final
  PARAMETERS pa_wdwiz LIKE am07m-mb5b_xonul     DEFAULT 'X' NO-DISPLAY. "c/estq.inicial; s/estq.final
  PARAMETERS pa_wdwuw LIKE am07m-mb5b_xonul     DEFAULT 'X' NO-DISPLAY. "c/estq.inicial/final, modif.
  PARAMETERS pa_wdwew LIKE am07m-mb5b_xonul     DEFAULT 'X' NO-DISPLAY. "c/estq.inicial/final, s/modif.
  PARAMETERS pa_ndzer LIKE am07m-mb5b_xonul     DEFAULT 'X' NO-DISPLAY. "sem estoque inicial/final
  PARAMETERS pa_ndsto LIKE am07m-mb5b_xonul     DEFAULT 'X' NO-DISPLAY. "c/estq.inicial/final, s/modif.
  PARAMETERS xsum     LIKE am07m-xsum           DEFAULT ' ' NO-DISPLAY. "Só totais - hierárquicos
  PARAMETERS pa_suvar LIKE disvariant-variant   DEFAULT ' ' NO-DISPLAY. "Variante de tela
  PARAMETERS pa_sumfl LIKE am07m-xsum           DEFAULT ' ' NO-DISPLAY. "Só totais - não hierárquicos
  PARAMETERS p_grid   TYPE mb_opt_alv_grid_ui   DEFAULT ' ' NO-DISPLAY. "Lista de saída como ALV Grid
  PARAMETERS pa_sflva LIKE disvariant-variant   DEFAULT ' ' NO-DISPLAY. "Variante de tela
  PARAMETERS xchar    LIKE am07m-xchrg          DEFAULT ' ' NO-DISPLAY. "Só materiais com lote obrig.
  PARAMETERS xnomchb  LIKE am07m-mb5b_xnomchb   DEFAULT ' ' NO-DISPLAY. "Incluir lotes sem estoque
  PARAMETERS nosto    LIKE am07m-nosto          DEFAULT ' ' NO-DISPLAY. "Nenhum estorno
  PARAMETERS archive  TYPE mb5b_archive         DEFAULT ' ' NO-DISPLAY. "Flag - docs.arquivados
  PARAMETERS pa_aistr LIKE aind_str1-archindex  DEFAULT ' ' NO-DISPLAY. "Estrututura informação arquivo

*----------------------------------------------------------------------*

*  SELECTION-SCREEN BEGIN OF BLOCK bestandsart
*  WITH FRAME TITLE TEXT-002.
**  Text-002: Bestandsart
*
*  SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN POSITION 33.
*  PARAMETERS lgbst LIKE am07m-lgbst RADIOBUTTON GROUP bart DEFAULT 'X'.
*  SELECTION-SCREEN COMMENT 40(50) TEXT-010 FOR FIELD lgbst.
**  Text-010: Lagerort-/Chargenbestand
*  SELECTION-SCREEN END OF LINE.
*
*  SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN POSITION 33.
*  PARAMETERS bwbst LIKE am07m-bwbst RADIOBUTTON GROUP bart.
*  SELECTION-SCREEN COMMENT 40(50) TEXT-011 FOR FIELD bwbst.
**  Text-011: bewerteter Bestand
*  SELECTION-SCREEN END OF LINE.
*
*  SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN POSITION 33.
*  PARAMETERS sbbst LIKE am07m-sbbst RADIOBUTTON GROUP bart.
*  SELECTION-SCREEN COMMENT 40(50) TEXT-012 FOR FIELD sbbst.
**  Text-012: Sonderbestand
*  SELECTION-SCREEN END OF LINE.
*
*  SELECTION-SCREEN END OF BLOCK bestandsart.

*----------------------------------------------------------------------*

* improved definition of parameters for scope of list       "n599218

*  SELECTION-SCREEN BEGIN OF BLOCK listumfang
*    WITH FRAME TITLE TEXT-003.  "Listumfang
*

* the following 3 parameters became obsolete do not use     "n599218
* anymor. They are still here to inform the user about      "n599218
* that he is using old variants or SUBMIT commands          "n599218
  PARAMETERS :                                              "n599218
    xonul  LIKE am07m-xonul            NO-DISPLAY,          "n599218
    xvbst  LIKE am07m-xvbst            NO-DISPLAY,          "n599218
    xnvbst LIKE am07m-xnvbs            NO-DISPLAY.          "n599218

*
** 7 new categories for the scope of list                    "n599218
**                                                           "n599218
** cat. I docs I stock on   I    I stock on I Parameter      "n599218
**      I      I start date I    I end date I                "n599218
** -----+------+------------+----+----------+----------      "n599218
**  1   I yes  I =  zero    I =  I =  zero  I pa_wdzer       "n599218
**  2   I yes  I =  zero    I <> I <> zero  I pa_wdzew       "n599218
**  3   I yes  I <> zero    I <> I =  zero  I pa_wdwiz       "n599218
**  4   I yes  I <> zero    I <> I <> zero  I pa_wdwuw       "n599218
**  5   I yes  I <> zero    I =  I <> zero  I pa_wdwew       "n599218
**      I      I            I    I          I                "n599218
**  6   I no   I =  zero    I =  I =  zero  I pa_ndzer       "n599218
**  7   I no   I <> zero    I =  I <> zero  I pa_ndsto       "n599218
**                                                           "n599218
** definition of the pushbutton : show or hide the following "n599218
** parameters for the scope of list                          "n599218
*  SELECTION-SCREEN PUSHBUTTON /1(20) pb_liu                 "n599218
*                             USER-COMMAND liu.              "n599218
*                                                            "n599218
** text line : materials with movements                      "n599218
*  SELECTION-SCREEN BEGIN OF LINE.                           "n599218
*  SELECTION-SCREEN COMMENT 1(55) TEXT-072                   "n599218
*                           MODIF ID liu.                    "n599218
*  SELECTION-SCREEN END OF LINE.                             "n599218
*                                                            "n599218
** with movements / start = zero  =  end = zero              "n599218
**  1   I yes  I =  zero    I =  I =  zero  I pa_wdzer       "n599218
*  SELECTION-SCREEN BEGIN OF LINE.                           "n599218
*  SELECTION-SCREEN POSITION 33.                             "n599218
*  PARAMETERS : pa_wdzer    LIKE am07m-mb5b_xonul            "n599218
*  MODIF ID liu.                                             "n599218
**   text-083 : no opening stock ; no closing stock          "n599218
*  SELECTION-SCREEN COMMENT 40(70) TEXT-083                  "n599218
*                           FOR FIELD pa_wdzer               "n599218
*                           MODIF ID liu.                    "n599218
*  SELECTION-SCREEN END OF LINE.                             "n599218
*                                                            "n599218
** with movements / start = zero  =  end <> zero             "n599218
**  2   I yes  I =  zero    I <> I <> zero  I pa_wdzew       "n599218
*  SELECTION-SCREEN BEGIN OF LINE.                           "n599218
*  SELECTION-SCREEN POSITION 33.                             "n599218
*  PARAMETERS : pa_wdzew    LIKE am07m-mb5b_xonul            "n599218
*  MODIF ID liu.                                             "n599218
**   text-084 : no opening stock ; with closing stock        "n599218
*  SELECTION-SCREEN COMMENT 40(70) TEXT-084                  "n599218
*                           FOR FIELD pa_wdzew               "n599218
*                           MODIF ID liu.                    "n599218
*  SELECTION-SCREEN END OF LINE.                             "n599218
*                                                            "n599218
** with movements / start stock <> 0 / end stock = 0         "n599218
**  3   I yes  I <> zero    I <> I =  zero  I pa_wdwiz       "n599218
*  SELECTION-SCREEN BEGIN OF LINE.                           "n599218
*  SELECTION-SCREEN POSITION 33.                             "n599218
*  PARAMETERS : pa_wdwiz    LIKE am07m-mb5b_xonul            "n599218
*  MODIF ID liu.                                             "n599218
**   text-085 : with opening stock ; no closing stock        "n599218
*  SELECTION-SCREEN COMMENT 40(70) TEXT-085                  "n599218
*                           FOR FIELD pa_wdwiz               "n599218
*                           MODIF ID liu.                    "n599218
*  SELECTION-SCREEN END OF LINE.                             "n599218
*                                                            "n599218
** with movements / with start and end stocks / different    "n599218
**  4   I yes  I <> zero    I <> I <> zero  I pa_wdwuw       "n599218
*  SELECTION-SCREEN BEGIN OF LINE.                           "n599218
*  SELECTION-SCREEN POSITION 33.                             "n599218
*  PARAMETERS : pa_wdwuw    LIKE am07m-mb5b_xonul            "n599218
*  MODIF ID liu.                                             "n599218
**   with opening stock ; with closing stock ; changed       "n599218
*  SELECTION-SCREEN COMMENT 40(70) TEXT-086                  "n599218
*                           FOR FIELD pa_wdwuw               "n599218
*                           MODIF ID liu.                    "n599218
*  SELECTION-SCREEN END OF LINE.                             "n599218
*                                                            "n599218
** with movements / with start and end stock / equal         "n599218
**  5   I yes  I <> zero    I =  I <> zero  I pa_wdwew       "n599218
*  SELECTION-SCREEN BEGIN OF LINE.                           "n599218
*  SELECTION-SCREEN POSITION 33.                             "n599218
*  PARAMETERS : pa_wdwew    LIKE am07m-mb5b_xonul            "n599218
*  MODIF ID liu.                                             "n599218
**   with opening stock ; with closing stock ; non-changed   "n599218
*  SELECTION-SCREEN COMMENT 40(70) TEXT-087                  "n599218
*                           FOR FIELD pa_wdwew               "n599218
*                           MODIF ID liu.                    "n599218
*  SELECTION-SCREEN END OF LINE.                             "n599218
*                                                            "n599218
** text line : materials without movements                   "n599218
*  SELECTION-SCREEN BEGIN OF LINE.                           "n599218
*  SELECTION-SCREEN COMMENT 1(55) TEXT-073                   "n599218
*                           MODIF ID liu.                    "n599218
*  SELECTION-SCREEN END OF LINE.                             "n599218
*                                                            "n599218
** materials without movements / stocks = zero               "n599218
**  6   I no   I =  zero    I =  I =  zero  I pa_ndzer       "n599218
*  SELECTION-SCREEN BEGIN OF LINE.                           "n599218
*  SELECTION-SCREEN POSITION 33.                             "n599218
*  PARAMETERS : pa_ndzer    LIKE am07m-mb5b_xonul            "n599218
*  MODIF ID liu.                                             "n599218
**   text-083 : no opening stock ; no closing stock          "n599218
*  SELECTION-SCREEN COMMENT 40(70) TEXT-083                  "n599218
*                           FOR FIELD pa_ndzer               "n599218
*                           MODIF ID liu.                    "n599218
*  SELECTION-SCREEN END OF LINE.                             "n599218
*                                                            "n599218
** materials without movements / with start or end stock     "n599218
**  7   I no   I <> zero    I =  I <> zero  I pa_ndsto       "n599218
*  SELECTION-SCREEN BEGIN OF LINE.                           "n599218
*  SELECTION-SCREEN POSITION 33.                             "n599218
*  PARAMETERS : pa_ndsto    LIKE am07m-mb5b_xonul            "n599218
*  MODIF ID liu.                                             "n599218
**   with opening stock ; with closing stock ; non-changed   "n599218
*  SELECTION-SCREEN COMMENT 40(70) TEXT-087                  "n599218
*                           FOR FIELD pa_ndsto               "n599218
*                           MODIF ID liu.                    "n599218
*  SELECTION-SCREEN END OF LINE.                             "n599218
*                                                            "n599218
*  SELECTION-SCREEN END OF BLOCK listumfang.

*----------------------------------------------------------------------*

*  SELECTION-SCREEN BEGIN OF BLOCK einstellungen
*     WITH FRAME TITLE TEXT-068.  "Settings
*
** parameter for totals only - hierseq. list
** corresponding display variant
*  SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN POSITION 33.
*  PARAMETERS xsum          LIKE am07m-xsum.
*  SELECTION-SCREEN COMMENT 40(60) TEXT-090 FOR FIELD xsum.
*  SELECTION-SCREEN END OF LINE.
*
*  SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT 1(30) TEXT-091 FOR FIELD pa_suvar.
*  SELECTION-SCREEN POSITION 33.
*  PARAMETERS: pa_suvar LIKE disvariant-variant.
*  SELECTION-SCREEN END OF LINE.
*
** parameter for totals only - flat list + corresponding display variant
*  SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN POSITION 33.
*  PARAMETERS pa_sumfl LIKE am07m-xsum.
*  SELECTION-SCREEN COMMENT 40(60) TEXT-092 FOR FIELD pa_sumfl.
*  SELECTION-SCREEN END OF LINE.
*
*  SELECTION-SCREEN BEGIN OF LINE.                           "1790231
*  SELECTION-SCREEN POSITION 33.                             "1790231
*  PARAMETERS: p_grid TYPE mb_opt_alv_grid_ui                "1790231
*  MODIF ID opt USER-COMMAND opt.                            "1790231
*  SELECTION-SCREEN COMMENT 40(50) FOR FIELD p_grid          "1790231
*                      MODIF ID opt.                         "1790231
*  SELECTION-SCREEN END OF LINE.                             "1790231
*
*  SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN COMMENT 1(30) TEXT-091 FOR FIELD pa_sflva.
*  SELECTION-SCREEN POSITION 33.
*  PARAMETERS: pa_sflva LIKE disvariant-variant.
*  SELECTION-SCREEN END OF LINE.
*
*  SELECTION-SCREEN BEGIN OF LINE.
*  SELECTION-SCREEN POSITION 33.
*  PARAMETERS xchar LIKE am07m-xchrg.
*  SELECTION-SCREEN COMMENT 40(50) TEXT-015 FOR FIELD xchar.
**  Text-015: nur chargenpflichtige Materialien
**  Das Kennzeichen 'xchar' bestimmt die Art der Listausgabe entweder
**  auf Material- oder Chargenebene.
*  SELECTION-SCREEN END OF LINE.
*
*  SELECTION-SCREEN BEGIN OF LINE.                           "838360_v
*  SELECTION-SCREEN POSITION 33.
*  PARAMETERS xnomchb LIKE am07m-mb5b_xnomchb.
*  SELECTION-SCREEN COMMENT 40(50) TEXT-089 FOR FIELD xnomchb.
**  Text-089: Auch Chargen ohne Bestandssegment
*  SELECTION-SCREEN END OF LINE.                             "838360_^
*
** the function "No reversal movements" is only         "n571473
** available from relaese 4.5B and higher               "n571473
** ( TEXT-026 : No reversal movements )                 "n571473
*  SELECTION-SCREEN BEGIN OF LINE.                           "n571473
*  SELECTION-SCREEN POSITION 33.                             "n571473
*  PARAMETERS nosto LIKE am07m-nosto.                        "n571473
*  SELECTION-SCREEN COMMENT 40(50) TEXT-026                  "n571473
*                         FOR FIELD nosto.                   "n571473
*  SELECTION-SCREEN END OF LINE.                             "n571473
*
*  SELECTION-SCREEN END OF BLOCK einstellungen.

*----------------------------------------------------------------------*

  SELECTION-SCREEN BEGIN OF BLOCK liste WITH FRAME TITLE TEXT-040.
  PARAMETERS: p_vari LIKE disvariant-variant.
  SELECTION-SCREEN END OF BLOCK liste.

*----------------------------------------------------------------------*

* with these new parameters allow the user to determine     "n921165
* the best database access; these parameters will appear    "n921165
* only when the installed database system is :              "n921165
* DB6, Informix, or MaxDB                                   "n921165
                                                            "n921165
* define database access for best runtime                   "n921165
  SELECTION-SCREEN BEGIN OF BLOCK db                        "n921165
                               WITH FRAME TITLE TEXT-111.   "n921165
                                                            "n921165
* Database determines best access                           "n921165
  SELECTION-SCREEN : BEGIN OF LINE.                         "n921165
  SELECTION-SCREEN POSITION 33.
  PARAMETERS : pa_dbstd    LIKE  am07m-xselk                "n921165
                           MODIF ID dba                     "n921165
                           DEFAULT 'X'                      "n921165
  RADIOBUTTON GROUP db.                                     "n921165
  SELECTION-SCREEN : COMMENT 40(70) TEXT-112                "n921165
                           FOR FIELD pa_dbstd               "n921165
                           MODIF ID dba.                    "n921165
  SELECTION-SCREEN : END OF LINE.                           "n921165
                                                            "n921165
* Access via Material number                                "n921165
  SELECTION-SCREEN : BEGIN OF LINE.                         "n921165
  SELECTION-SCREEN POSITION 33.
  PARAMETERS : pa_dbmat    LIKE  am07m-xselk                "n921165
                           MODIF ID dba                     "n921165
  RADIOBUTTON GROUP db.                                     "n921165
  SELECTION-SCREEN : COMMENT 40(70) TEXT-113                "n921165
                           FOR FIELD pa_dbmat               "n921165
                           MODIF ID dba.                    "n921165
  SELECTION-SCREEN : END OF LINE.                           "n921165
                                                            "n921165
* Access via Posting Date                                   "n921165
  SELECTION-SCREEN : BEGIN OF LINE.                         "n921165
  SELECTION-SCREEN POSITION 33.
  PARAMETERS : pa_dbdat    LIKE  am07m-xselk                "n921165
                           MODIF ID dba                     "n921165
  RADIOBUTTON GROUP db.                                     "n921165
  SELECTION-SCREEN : COMMENT 40(70) TEXT-114                "n921165
                           FOR FIELD pa_dbdat               "n921165
                           MODIF ID dba.                    "n921165
  SELECTION-SCREEN : END OF LINE.                           "n921165
                                                            "n921165
  SELECTION-SCREEN END OF BLOCK db.                         "n921165

*------------------------ begin of note 1481757 ------------------------*
*---------- selection-sreen for archive --------------------------------*

*  SELECTION-SCREEN BEGIN OF BLOCK arch WITH FRAME TITLE TEXT-131. "n1481757
*                                                            "n1481757
** create checkbox on the screen                                 "n1481757
*                                                            "n1481757
*  SELECTION-SCREEN : BEGIN OF LINE.                         "n921165
*  SELECTION-SCREEN POSITION 33.
*  PARAMETERS: archive  TYPE mb5b_archive AS CHECKBOX DEFAULT ' ' "n1481757
*  USER-COMMAND us_archive.                                  "n1481757
*  SELECTION-SCREEN COMMENT 40(30) FOR FIELD archive MODIF ID sys.
*  SELECTION-SCREEN END OF LINE.
*                                                            "n1481757
**  parameter for the archive info structure                     "n1481757
*  PARAMETERS : pa_aistr    LIKE aind_str1-archindex.        "n1481757
*                                                            "n1481757
*  SELECTION-SCREEN END OF BLOCK arch.                       "n1481757

* used for ABAP Unit Test see local class of CL_IM_RM07MLBD_DBSYS_OPT
  PARAMETERS: p_aut TYPE char1 NO-DISPLAY.

* -------------------- end of selection-sreen for archive---------------*

* ------------------- F4-Help --------- get info-structure -------------*
* datadefinition                                               "n1481757
  DATA: g_f_f4_mode(01)  TYPE  c,                           "n1481757
        g_f_f4_archindex LIKE  aind_str1-archindex.         "n1481757
                                                            "n1481757

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
  PERFORM f4_for_variant.

*----------------------------------------------------------------------*

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_sflva.
*  PERFORM                    variant_value_request_f4
*                             USING  pa_sflva  g_s_vari_sumfl.

*----------------------------------------------------------------------*

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_suvar.
*  PERFORM                    variant_value_request_f4
*                             USING  pa_suvar g_s_vari_sumhq.

*-----------------------------------------------------------"n599218
* INITIALIZATION                                            "n599218
*-----------------------------------------------------------"n599218
                                                            "n599218
* pay attentions : this process time will not be processed  "n599218
* in the webreporting mode                                  "n599218

INITIALIZATION.

  CLEAR : g_s_vari_sumhq, g_s_vari_sumfl.
  repid = sy-repid.
  variant_save = 'A'.
  LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\RM07MLBD_03\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* check if Tcode/report is still active in CWM
* if not then there will be an automatically redirecting to
* a different Tcode/report - if that is customized
  CALL FUNCTION '/CWM/CUST_SY_TCODE_CPROG_CHECK'.

*  ENHANCEMENT-POINT rm07mlbd_03 SPOTS es_rm07mlbd.
* preprae the working areas for the variants
  MOVE  : repid              TO  g_s_vari_sumhq-report,
          'SUHQ'             TO  g_s_vari_sumhq-handle,
          repid              TO  g_s_vari_sumfl-report,
          'SUFL'             TO  g_s_vari_sumfl-handle.

  MOVE-CORRESPONDING : g_s_vari_sumhq  TO  g_s_vari_sumhq_def,
                       g_s_vari_sumfl  TO  g_s_vari_sumfl_def.

*  PERFORM  get_the_default_variant
*                             USING  pa_sflva
*                                    g_s_vari_sumfl
*                                    g_s_vari_sumfl_def.
*
*  PERFORM  get_the_default_variant
*                             USING  pa_suvar
*                                    g_s_vari_sumhq
*                                    g_s_vari_sumhq_def.

  PERFORM initialisierung.

** get the parameters from the last run                      "n547170
*  PERFORM                    esdus_get_parameters.          "n547170

* set flag when INITILIZATION is processed
  MOVE  'X'        TO  g_flag_initialization.

* check switch FIN_LOCRU_SFWS_UI_02 activation
  gv_switch_ehp6ru = cl_fin_locru_switch_check=>fin_locru_sfws_ui_02( ).

* begin of secondary database settings                     "n1710850
  CALL FUNCTION 'FUNCTION_EXISTS'
    EXPORTING
      funcname           = c_hdb_dbcon_get
    EXCEPTIONS
      function_not_exist = 1
      OTHERS             = 2.
  IF sy-subrc = 0.
    CALL FUNCTION c_hdb_dbcon_get
      EXPORTING
        i_subappl        = c_hdb_subappl
        i_act_check_only = abap_true
      IMPORTING
        e_dbcon          = dbcon_active.
  ENDIF.
* end of secondary database settings                       "n1710850
  PERFORM check_ui_opti_badi.                               "1790231

*-----------------------------------------------------------"n599218
* AT SELECTION-SCREEN                                       "n599218
*-----------------------------------------------------------"n599218

*----------- Prüfung der eingegebenen Selektionsparameter, ------------*
*---------------------- Berechtigungsprüfung --------------------------*

AT SELECTION-SCREEN.

* the user will get the info about the old variant only     "n921165
* once                                                      "n921165
  IF  g_cnt_error_dba = 1.                                  "n921165
    IF  NOT sy-slset IS INITIAL.                            "n921165
*     Variant & of program & is not the current version     "n921165
      MESSAGE i634(db)       WITH  sy-slset sy-repid.       "n921165
    ENDIF.                                                  "n921165
  ENDIF.                                                    "n921165
                                                            "n921165
* if the installed database system is not DB6, Informix or  "n921165
* MAxDB the parameter PA_DBSTD must be set                  "n921165
  IF  g_flag_db_parameters IS INITIAL.                      "n921165
*  if the radio buttons are not shown,                      "n2308556
*  use standard access for all selections                   "n2308556
    MOVE  'X'                TO  pa_dbstd.                  "n2308556
    CLEAR :                  pa_dbmat, pa_dbdat.            "n2308556
  ENDIF.                                                    "n921165
                                                            "n921165
* check choosen database access agaist restrictions         "n921165
* text-095 : Mismatch Database access - restriction         "n921165
  IF  g_flag_db_parameters = 'X'.                           "n921165
                                                            "n921165
    IF      pa_dbmat = 'X'.                                 "n921165
*     access via material number : material entered ?       "n921165
      IF  matnr[] IS INITIAL.                               "n921165
        SET CURSOR         FIELD  pa_dbmat.                 "n921165
        MESSAGE  w895      WITH  TEXT-115.                  "n921165
      ENDIF.                                                "n921165
                                                            "n921165
    ELSEIF  pa_dbdat = 'X'.                                 "n921165
*     access via posting data : posting date entered ?      "n921165
      IF  datum-low  IS INITIAL AND                         "n921165
          datum-high IS INITIAL.                            "n921165
        SET CURSOR         FIELD  pa_dbdat.                 "n921165
        MESSAGE  w895      WITH  TEXT-115.                  "n921165
      ENDIF.                                                "n921165
                                                            "n921165
    ENDIF.                                                  "n921165
  ENDIF.                                                    "n921165

* the following 3 parameters XONUL, XVBST, and XNVBST       "n599218
* became obsolete; send error when they should be filled.   "n599218
* This could be possible if the user works with old         "n599218
* selection variants or this report is launched by a        "n599218
* SUBMIT command                                            "n599218
  IF  xonul  IS INITIAL  AND                                "n599218
      xvbst  IS INITIAL  AND                                "n599218
      xnvbst IS INITIAL.                                    "n599218
*  ok, the old parameters are empty                         "n599218
  ELSE.                                                     "n599218
*   text-088 : note 599218 : obsolete parameter used        "n599218
    MESSAGE e895             WITH  TEXT-088.                "n599218
  ENDIF.

* did the user hit the pushbutton "Category" ?              "n599218
  CASE     sscrfields-ucomm.                                "n599218
    WHEN  'LIU '.                                           "n599218
*     yes, the pushbutton "Category" was hit                "n599218
      IF  g_flag_status_liu  =  c_hide.                     "n599218
*       show the 7 parameters on the selection srceen       "n599218
        MOVE  c_show         TO  g_flag_status_liu.         "n599218
      ELSE.                                                 "n599218
*       hide the 7 paramaters                               "n599218
        MOVE  c_hide         TO  g_flag_status_liu.         "n599218
      ENDIF.                                                "n599218
  ENDCASE.                                                  "n599218

* carry out the "expensive" checks, like authorization,     "n878753
* only after the user wants to launch this report. In the   "n878753
* case an error message was sent the user can correct the   "n878753
* entries and go on with "ENTER". That means the system     "n878753
* field SY-UCOMM is initial. This correction should make    "n878753
* sure that all checks are done when this report is         "n878753
* launched.                                                 "n878753
  IF  sy-ucomm = 'ONLI'  OR                                 "n878753
      sy-ucomm = 'PRIN'  OR                                 "n878753
      sy-ucomm = 'SJOB'.                                    "n878753
    MOVE  'X'                TO  g_flag_launched.           "n878753
  ENDIF.                                                    "n878753
                                                            "n878753
  CHECK : g_flag_launched = 'X'.                            "n878753

** only one sum function can be processed
*  IF  xsum     = 'X' AND
*      pa_sumfl = 'X'.
*    SET CURSOR               FIELD 'XSUM'.
**   select one sum list only
*    MESSAGE  e895            WITH  TEXT-093.
*  ENDIF.

  PERFORM eingaben_pruefen.

*  SET CURSOR                 FIELD 'PA_SFLVA'.
*  PERFORM  variant_check_existence
*                             USING     pa_sflva
*                                       g_s_vari_sumfl
*                                       g_s_vari_sumfl_def.
*
*  SET CURSOR                 FIELD 'PA_SUVAR'.
*  PERFORM  variant_check_existence
*                             USING     pa_suvar
*                                       g_s_vari_sumhq
*                                       g_s_vari_sumhq_def.

* check whether FI summarization is active and other        "n547170
* restrictions could deliver wrong results                  "n547170
  PERFORM                    f0800_check_restrictions.      "n547170

** - the user wants to surpress the reversal movements :     "n497992
**   process warning M7 392                                  "n497992
*  IF NOT nosto IS INITIAL.                                  "n497992
**   emerge warning ?                                        "n497992
*    CALL FUNCTION 'ME_CHECK_T160M'             "n497992
*      EXPORTING                                         "n497992
*        i_arbgb = 'M7'                         "n497992
*        i_msgnr = '392'                        "n497992
*      EXCEPTIONS                                        "n497992
*        nothing = 0                            "n497992
*        OTHERS  = 1.                           "n497992
*                                                            "n497992
*    IF sy-subrc <> 0.                                       "n497992
*      SET CURSOR               FIELD  'NOSTO'.              "n497992
**       to surpress the reversal movements could cause ...  "n497992
*      MESSAGE                  w392.                        "n497992
*    ENDIF.                                                  "n497992
*  ENDIF.                                                    "n497992

* carry out special authotity check for the tax auditor     "n547170
  PERFORM                    tpc_check_tax_auditor.         "n547170

* does the user wants a selection via company code or a plant ?
* fill range table g_ra_werks
  REFRESH : g_ra_bwkey,  g_ra_werks, g_t_organ.
  CLEAR   : g_ra_bwkey,  g_ra_werks, g_t_organ, g_s_organ.
  REFRESH : g_0000_ra_bwkey,  g_0000_ra_werks,  g_0000_ra_bukrs.
  CLEAR   : g_0000_ra_bwkey,  g_0000_ra_werks,  g_0000_ra_bukrs.

  DESCRIBE TABLE  bukrs      LINES  g_f_cnt_lines_bukrs.
  DESCRIBE TABLE  werks      LINES  g_f_cnt_lines_werks.

  IF  g_f_cnt_lines_bukrs  > 0  OR
      g_f_cnt_lines_werks  > 0.
*   fill range tables for the CREATION OF TABLE G_T_ORGAN
    MOVE : werks[]           TO  g_0000_ra_werks[],
           bukrs[]           TO  g_0000_ra_bukrs[].

    PERFORM  f0000_create_table_g_t_organ
                             USING  c_error.
  ENDIF.

* ----- begin of note "n1481757 ---- check archive-info-structure----*
  DATA: g_flag_exist_as TYPE c.                             "n1481757
  DATA : g_flag_too_many_sel(01) TYPE c.                    "n1481757
  DATA: g_v_fieldname TYPE fieldname.                       "n1481757
                                                            "n1481757
* process the MM docs from the new AS archive               "n1481757
  IF archive = 'X'.                                         "n1481757
    PERFORM check_existence_as USING g_flag_exist_as.       "n1481757
    IF  g_flag_exist_as = 'X'.                              "n1481757
      PERFORM check_archive_index USING g_flag_too_many_sel "n1481757
                                        g_v_fieldname.      "n1481757
      " Materialbelege aus dem Archiv auslesen              "n1481757
      IF g_flag_too_many_sel = 'X'.                         "n1481757
        MESSAGE w432  WITH  g_v_fieldname pa_aistr.         "n1481757
*     Eingrenzungen für Feld &1 wirken nicht                "n1481757
      ENDIF.                                                "n1481757
    ENDIF.                                                  "n1481757
  ENDIF.                                                    "n1481757
* ----- end of note "n1481757 ----- check archive-info-structure ---*

** save the parameters of this run                           "n547170
*  PERFORM                    esdus_save_parameters.         "n547170


*-----------------------------------------------------------"n599218
* AT SELECTION-SCREEN OUTPUT                                "n599218
*-----------------------------------------------------------"n599218
                                                            "n599218

AT SELECTION-SCREEN OUTPUT.                                 "n599218

* check whether the database access parameters fulfil the   "n921165
* radiobutton rules / in the case this report was launched  "n921165
* with a selection variant, the settings of this variant    "n921165
* have been set already                                     "n921165
  IF  g_flag_db_parameters = 'X'.                           "n921165
    CLEAR                    g_cnt_radio.                   "n921165
    IF  pa_dbstd = 'X'.  ADD 1    TO g_cnt_radio.  ENDIF.   "n921165
    IF  pa_dbmat = 'X'.  ADD 1    TO g_cnt_radio.  ENDIF.   "n921165
    IF  pa_dbdat = 'X'.  ADD 1    TO g_cnt_radio.  ENDIF.   "n921165
                                                            "n921165
    IF  g_cnt_radio = 1.                                    "n921165
*     ok                                                    "n921165
    ELSE.                                                   "n921165
*     offended against radiobutton rules : set default      "n921165
      ADD  1                 TO  g_cnt_error_dba.           "n921165
      MOVE : 'X'             TO  pa_dbstd.                  "n921165
      CLEAR :                pa_dbmat, pa_dbdat.            "n921165
    ENDIF.                                                  "n921165
  ENDIF.                                                    "n921165

  IF  g_flag_initialization IS INITIAL.                     "n599218
*   the process time INITIALIZATION was not done, so        "n599218
*   carry out the functions here                            "n599218
    MOVE  'X'                TO g_flag_initialization.      "n599218
                                                            "n599218
    PERFORM                  initialisierung.               "n599218
                                                            "n599218
**   get the parameters from the last run                    "n599218
*    PERFORM                  esdus_get_parameters.          "n599218
  ENDIF.                                                    "n599218
                                                            "n599218
* how to handle the 7 paramaters for the scope of list ?    "n599218
  LOOP AT SCREEN.                                           "n599218
*   modify the selection screen                             "n599218
    CASE    screen-group1.                                  "n599218
      WHEN  'LIU'.                                          "n599218
        IF  g_flag_status_liu  = c_show.                    "n599218
          screen-active = '1'.         "show parameters     "n599218
        ELSE.                                               "n599218
          screen-active = '0'.         "Hide parameters     "n599218
        ENDIF.                                              "n599218
                                                            "n599218
        MODIFY SCREEN.                                      "n599218

      WHEN  'DBA'.                                          "n921165
*       show or hide the parametes for the database access  "n921165
        IF  g_flag_db_parameters = 'X'.                     "n921165
          screen-active = '1'.         "show parameters     "n921165
        ELSE.                                               "n921165
          screen-active = '0'.         "Hide parameters     "n921165
        ENDIF.                                              "n921165
                                                            "n921165
        MODIFY SCREEN.                                      "n921165

      WHEN  'HKT'.
*       show or hide HKONT parameter
        IF gv_switch_ehp6ru = 'X'.
          screen-active = '1'.
        ELSE.
          screen-active = '0'.
        ENDIF.
        MODIFY SCREEN.

      WHEN  'OPT'.                                          "1790231
        IF gv_ui_opt_active = abap_false.                   "1790231
          screen-active = 0.                                "1790231
          MODIFY SCREEN.                                    "1790231
        ENDIF.                                              "1790231


    ENDCASE.                                                "n599218
  ENDLOOP.                                                  "n599218
                                                            "n599218
** adapt the icon on the pushbutton depending on the status  "n599218
*  CASE    g_flag_status_liu.                                "n599218
*    WHEN  c_hide.                                           "n599218
*      MOVE  TEXT-081         TO  pb_liu.  "@0E\Q@ Scope ... "n599218
*    WHEN  c_show.                                           "n599218
*      MOVE  TEXT-082         TO  pb_liu.  "@0H\Q@ Scope ... "n599218
*    WHEN  OTHERS.                                           "n599218
*  ENDCASE.                                                  "n599218
                                                            "n599218
*-----------------------------------------------------------"n599218

*----------------------------------------------------------------------*
* START-OF-SELECTION
*----------------------------------------------------------------------*

START-OF-SELECTION.

  CLEAR gt_t156t[].

* NEW DB                                             "v hana_20120802
  DATA: gr_badi_rm07mlbd_dbsys_opt TYPE REF TO rm07mlbd_dbsys_opt,
        gv_newdb                   TYPE abap_bool,
        gv_no_dbsys_opt            TYPE abap_bool,
        gt_stock_inventory         TYPE stock_inventory_tt,
        gs_stock_inventory         TYPE stock_inventory_s.
  DATA: gv_unittest     TYPE abap_bool,              "v hana_20120821
        bestand_opensql LIKE TABLE OF bestand,
        bestand_new_db  LIKE TABLE OF bestand.                "^ hana_20120821
  DATA: gv_optimization_active TYPE abap_bool.              "n2122205
  DATA: lo_opti_badi           TYPE REF TO mm_perf_optimization. "n2122205
  FIELD-SYMBOLS: <gs_stock_inventory> TYPE stock_inventory_s.

*  IF  ( pa_sumfl = abap_true OR     "aggregate movements only
*        xsum     = abap_true )
*  AND pa_wdzer = abap_true      "view full list scope only
*  AND pa_wdzew = abap_true
*  AND pa_wdwiz = abap_true
*  AND pa_wdwuw = abap_true
*  AND pa_wdwew = abap_true
*  AND pa_ndsto = abap_true
*  AND pa_ndzer = abap_true
*  AND nosto    = abap_false     "no hiding of reversals
*  AND archive  = abap_false     "not with archived data
*  AND bwbst    = abap_false.    "no valuated stocks
*    TRY.                                                    "v n2122205
*        GET BADI lo_opti_badi.
*      CATCH cx_badi_not_implemented
*            cx_badi_multiply_implemented
*            cx_badi_filter_error.
*        gv_optimization_active  = abap_false.
*    ENDTRY.
*    TRY.
*        CALL BADI lo_opti_badi->is_active
*          EXPORTING
*            iv_reportname = sy-repid
*          RECEIVING
*            rv_active     = gv_optimization_active.
*      CATCH cx_badi.
*        gv_optimization_active  = abap_false.
*    ENDTRY.
*    IF gv_optimization_active = abap_true.
*      TRY.
*          GET BADI gr_badi_rm07mlbd_dbsys_opt
*            FILTERS
*              dbsys_type = cl_db_sys=>dbsys_type.
*          gv_newdb = abap_true.
*        CATCH cx_badi_not_implemented
*              cx_badi_multiply_implemented
*              cx_badi_filter_error.
*          gv_newdb = abap_false.
*      ENDTRY.
*    ENDIF.                                                  "^ n2122205
*  ENDIF.                                             "^ hana_20120802
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP616_RM07MLBD_01\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* Optimization 616 - Stored procedures
*
*  p_aut    = parameter for ABAP UNIT tests (O = old version , N = with stored procedures)
*  gv_newdb = varaible to control and use the Stored procedures via BADI implementation
*             (stored procedures are for all DB existing, not only HANA)
    CLEAR: p_aut.
* activate old DB logic and access (without stored procedures)
    gv_newdb = abap_false.
  ENDIF.
*ENHANCEMENT-POINT ehp616_rm07mlbd_01 SPOTS es_rm07mlbd .

  IF p_aut NE space.
* Code injection for ABAP UNIT TEST
* see local class of CL_IM_RM07MLBD_DBSYS_OPT
    CASE p_aut.
      WHEN cl_mm_im_aut_master=>gc_aut_optimization_off.
* old version / without optimization
        gv_newdb = abap_false.
      WHEN  cl_mm_im_aut_master=>gc_aut_optimization_on.
* "new version / with optimization
        gv_newdb = abap_true.
      WHEN OTHERS.
        CLEAR p_aut.
    ENDCASE.
  ENDIF.                       " w/o New DB feature  "^ hana_20120821

* Deactivate old MMIM optimization in SAPSCORE.
* because AMDP for HANA and ADBC for ANYDB are not redirected
* to the new datamodel, so there will be inconsistencies
* once db table NSDM_D_MTDCSA is active.
  gv_newdb = abap_false.  "FOR SAPSCORE

* it makes no sence to carry out this report with an old    "n921165
* and incorrect selection variant                           "n921165
  IF  g_cnt_error_dba > 0.                                  "n921165
    IF  NOT sy-slset IS INITIAL.                            "n921165
*     Variant & of program & is not the current version     "n921165
      MESSAGE e634(db)       WITH  sy-slset sy-repid.       "n921165
    ENDIF.                                                  "n921165
  ENDIF.                                                    "n921165

* create the title line

* If no date is given at all, the range is set to the maximum
* extend (1.1.0000 - 31.12.9999).
* If only datum-low is set, it is interpreted as the day for
* which the analysis is wanted --> datum-high is filled up.
  IF datum-low IS INITIAL.
    datum-low = '00000101'.
    IF datum-high IS INITIAL.
      datum-high = '99991231'.
    ENDIF.
  ELSE.
    IF datum-high IS INITIAL.
      datum-high = datum-low.
    ENDIF.
  ENDIF.
*  Begin of changes of note 1117067                        "n1117067
*  MOVE: datum-low(4)    TO jahrlow,
*        datum-low+4(2)  TO monatlow,
*        datum-low+6(2)  TO taglow,
*        datum-high(4)   TO jahrhigh,
*        datum-high+4(2) TO monathigh,
*        datum-high+6(2) TO taghigh.
*  SET TITLEBAR 'MAN'
*  WITH taglow monatlow jahrlow taghigh monathigh jahrhigh.
* Conversion of the dates from the internal to the external view
  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      date_internal            = datum-low
    IMPORTING
      date_external            = d_from
    EXCEPTIONS
      date_internal_is_invalid = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      date_internal            = datum-high
    IMPORTING
      date_external            = d_to
    EXCEPTIONS
      date_internal_is_invalid = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  SET TITLEBAR 'MAN'
    WITH d_from d_to.
*  End of changes of note 1117067                          "n1117067


* create the headlines using the titelbar                   "n599218
  PERFORM                    create_headline.               "n599218

* calculate the offsets for the list header
  PERFORM                    calculate_offsets.

* for the representation of tied empties                    "n547170
  PERFORM                    f0700_prepare_tied_empties.    "n547170

                                                            "n1784874
  BREAK-POINT                ID mmim_rep_mb5b.              "n921164
* dynamic break-point : is IS-OIL active ?                  "n921164
                                                            "n599218 A
* check whether this is a IS-OIL system                     "n599218 A
  PERFORM                    check_is_oil_system.           "n599218 A
                                                            "n1784874

* create table g_t_mseg_fields with the names of all
* wanted fields from MSEG and MKPF
  PERFORM                    f0300_get_fields.

* create the ALV fieldcatalog for the main list always
  MOVE  'G_T_BELEGE'         TO  g_f_tabname.

  PERFORM                    f0400_create_fieldcat.

* do not print the ALV-statistics and selection criteria
  CLEAR                      g_s_print.
  g_s_print-no_print_selinfos   = 'X'.
  g_s_print-no_print_listinfos = 'X'."

* create the range table for the storage location
  PERFORM                    f0600_create_range_lgort.

* - show the current activity and the progress              "n599218
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'                 "n599218
    EXPORTING                                               "n599218
      text = TEXT-063.       "Reading current stocks        "n599218

  IF gv_newdb = abap_true.                           "v hana_20120802
    PERFORM new_db_run.
  ENDIF.                                             "v hana_20120802

* get the stock tables
  PERFORM                    aktuelle_bestaende.

  PERFORM tabellen_lesen.

  IF gv_newdb = abap_false. "~~~~~~~~~~~~~~~~~~~~~~ "hana_20120607_V1
* - show the current activity and the progress              "n599218
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'               "n599218
      EXPORTING                                             "n599218
        text = TEXT-064.       "Reading MM documents          "n599218
    PERFORM                    f1000_select_mseg_mkpf.
  ENDIF. "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ "hana_20120607_V1

  PERFORM                    belegselektion.

*----------------------------------------------------------------------*
* END-OF-SELECTION
*----------------------------------------------------------------------*

END-OF-SELECTION.

* results of all the autority checks
  PERFORM                    f9100_auth_plant_result.

  IF gv_newdb = abap_false. "~~~~~~~~~~~~~~~~~~~~~~ "hana_20120607_V1
* - show the current activity and the progress              "n599218
    IF bwbst = 'X'.                                         "n599218
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'               "n599218
        EXPORTING                                             "n599218
          text = TEXT-066.     "Calculating Stocks and Values "n599218
    ELSE.                                                   "n599218
      CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'               "n599218
        EXPORTING                                             "n599218
          text = TEXT-067.     "Calculating Stocks            "n599218
    ENDIF.                                                  "n599218

    PERFORM summen_bilden.

    PERFORM bestaende_berechnen.
  ENDIF. "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ "hana_20120607_V1

  IF p_aut NE space.
* Code injection for ABAP UNIT TEST
* see local class of CL_IM_RM07MLBD_DBSYS_OPT
*    EXPORT lt_bestand FROM bestand[]
    IF gt_stock_inventory IS INITIAL.
      LOOP AT bestand.
        MOVE-CORRESPONDING bestand TO gs_stock_inventory.
        INSERT gs_stock_inventory INTO TABLE gt_stock_inventory.
      ENDLOOP.
    ENDIF.
    EXPORT lt_bestand FROM gt_stock_inventory
      TO MEMORY ID cl_mm_im_aut_master=>gc_memory_id_rm07mlbd.
    RETURN.
  ENDIF.

  PERFORM listumfang.

* - show the current activity and the progress              "n599218
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'                 "n599218
    EXPORTING                                               "n599218
      text = TEXT-065.       "Preparing list output         "n599218

* stop if table bestand is empty
  DESCRIBE TABLE bestand     LINES g_f_cnt_lines.

  IF  g_f_cnt_lines IS INITIAL.
*   Keinen Eintrag zu den Suchbegriffen gefunden/selektiert
    MESSAGE                  s083.
*   perform                  anforderungsbild.
  ELSE.
*   process log function if the use is a tax auditor        "n555246
*   and the database selection was successful               "n555246
    IF  g_flag_tpcuser = '1'.                               "n555246
      PERFORM                tpc_write_log.                 "n555246
    ENDIF.                                                  "n555246

    PERFORM feldgruppen_aufbauen.

*   sort table with header data per material
    IF bwbst IS INITIAL.
      SORT bestand BY matnr werks charg.
    ELSE.
      SORT bestand BY matnr bwkey.
    ENDIF.

**   which function does the user want ?
*    IF      xsum = 'X'.
**     hierseq. alv with sums
*      PERFORM                create_table_totals_hq.
*
*      PERFORM                create_fieldcat_totals_hq.
*
*      PERFORM                alv_hierseq_list_totals.
*
*    ELSEIF  pa_sumfl = 'X'.
**     show the sums only in a flat ALV
*      PERFORM                create_table_totals_flat.
*
*      PERFORM                create_fieldcat_totals_flat.
*
*      PERFORM                alv_flat_list_sums_only.
*
*    ELSE.
**     display the full list using the APPEND ALV


*    PERFORM                bestaende_ausgeben.
    PERFORM                zf_displaydata.


*    ENDIF.
  ENDIF.

  CLEAR: g_t_mseg_lean, g_t_bsim_lean, bestand.             "n443935

*&---------------------------------------------------------------------*
*&   PF_STATUS_SET_TOTALS
*&---------------------------------------------------------------------*

FORM pf_status_set_totals                                   "#EC CALLED
                   USING     extab TYPE slis_t_extab.

  SET PF-STATUS 'STANDARD'   EXCLUDING extab.

ENDFORM.                     "PF_STATUS_SET_TOTALS

*----------------------------------------------------------------------*
*    user_parameters_save
*----------------------------------------------------------------------*

FORM user_parameters_save.

  GET PARAMETER ID 'BUK'     FIELD  g_save_params-bukrs.
  GET PARAMETER ID 'WRK'     FIELD  g_save_params-werks.
  GET PARAMETER ID 'MAT'     FIELD  g_save_params-matnr.
  GET PARAMETER ID 'CHA'     FIELD  g_save_params-charg.
  GET PARAMETER ID 'BLN'     FIELD  g_save_params-belnr.
  GET PARAMETER ID 'BUK'     FIELD  g_save_params-bukrs.
  GET PARAMETER ID 'GJR'     FIELD  g_save_params-gjahr.

ENDFORM.                     "user_parameters_save

*----------------------------------------------------------------------*
*    user_parameters_restore
*----------------------------------------------------------------------*

FORM user_parameters_restore.

  SET PARAMETER ID 'BUK'     FIELD  g_save_params-bukrs.
  SET PARAMETER ID 'WRK'     FIELD  g_save_params-werks.
  SET PARAMETER ID 'MAT'     FIELD  g_save_params-matnr.
  SET PARAMETER ID 'CHA'     FIELD  g_save_params-charg.
  GET PARAMETER ID 'BLN'     FIELD  g_save_params-belnr.
  GET PARAMETER ID 'BUK'     FIELD  g_save_params-bukrs.
  GET PARAMETER ID 'GJR'     FIELD  g_save_params-gjahr.

ENDFORM.                     "user_parameters_restore

*&---------------------------------------------------------------------*
*&   USER_COMMAND_TOTALS
*&---------------------------------------------------------------------*

FORM user_command_totals                                    "#EC CALLED
                   USING     r_ucomm     LIKE  sy-ucomm
                             rs_selfield TYPE  slis_selfield.

  CLEAR                      g_s_bestand_key.

  IF      rs_selfield-tabname = 'G_T_TOTALS_HEADER'.
*   get the selected entry from table G_T_TOTALS
    READ TABLE g_t_totals_header
      INTO  g_s_totals_header
        INDEX rs_selfield-tabindex.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING  g_s_totals_header
                             TO  g_s_bestand_key.
    ENDIF.

  ELSEIF  rs_selfield-tabname = 'G_T_TOTALS_ITEM'.
*   get the selected entry from table G_T_TOTALS
    READ TABLE g_t_totals_item
      INTO  g_s_totals_item
        INDEX rs_selfield-tabindex.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING  g_s_totals_item
                             TO  g_s_bestand_key.
    ENDIF.

  ELSEIF  rs_selfield-tabname = 'G_T_TOTALS_FLAT'.
*   get the selected entry from table G_T_TOTALS
    READ TABLE g_t_totals_flat
      INTO  g_s_totals_flat
        INDEX rs_selfield-tabindex.

    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING  g_s_totals_flat
                             TO  g_s_bestand_key.
    ENDIF.
  ENDIF.

  IF g_s_bestand_key IS INITIAL.   "notinh found ?
*   Place the cursor on a table line
    MESSAGE                  s322.
    EXIT.
  ENDIF.

* get the line from the main table BESTAND depending on the mode
  IF bwbst IS INITIAL.
*   sort sequence = matnr werks charg
    READ TABLE bestand
      WITH KEY  matnr = g_s_bestand_key-matnr
                werks = g_s_bestand_key-werks
                charg = g_s_bestand_key-charg
                             BINARY SEARCH.

  ELSE.
*   sort sequence = matnr bwkey
    READ TABLE bestand
      WITH KEY  matnr = g_s_bestand_key-matnr
                bwkey = g_s_bestand_key-bwkey
                             BINARY SEARCH.
  ENDIF.

  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING bestand     TO  g_s_bestand_detail.
    APPEND  g_s_bestand_detail     TO  g_t_bestand_detail.

    PERFORM                  create_table_for_detail.

    PERFORM                  list_output_detail.
  ENDIF.

ENDFORM.                     " USER_COMMAND_TOTALS

*&---------------------------------------------------------------------*
* list_output_detail
*&---------------------------------------------------------------------*

FORM list_output_detail.

* build the auxiliary interface tables for the ALV

  IF  g_cust_color = 'X'.              "colorize numeric fields ?
    layout-coltab_fieldname = 'FARBE_PRO_FELD'.
  ELSE.
    layout-info_fieldname   = 'FARBE_PRO_ZEILE'.
  ENDIF.

  layout-f2code = '9PBP'.

  IF NOT bwbst IS INITIAL.
    layout-min_linesize = '92'.
  ENDIF.

  events-name = 'TOP_OF_PAGE'.
  events-form = 'UEBERSCHRIFT_DETAIL'.
  APPEND events.

  IF  g_flag_break-b3 = 'X'.                                "n921164
    BREAK-POINT              ID mmim_rep_mb5b.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  ENDIF.                                                    "n921164

  IF gv_ui_opt_active = abap_false OR p_grid = abap_false   "1790231
     OR pa_sumfl = space.                                   "1790231
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_interface_check        = g_flag_i_check             "n599218
        i_callback_program       = repid
        i_callback_pf_status_set = 'STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        is_layout                = layout
        it_fieldcat              = fieldcat[]
        it_special_groups        = gruppen[]
        it_sort                  = sorttab[]
        i_default                = 'X'
        i_save                   = 'A'
        is_variant               = variante
        it_events                = events[]
        is_print                 = g_s_print
      TABLES
        t_outtab                 = g_t_belege1
      EXCEPTIONS
        OTHERS                   = 2.

  ELSE.                                                     "1790231
                                                            "1790231
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'                  "1790231
      EXPORTING                                             "1790231
        i_interface_check        = g_flag_i_check
        i_callback_program       = repid
        i_callback_pf_status_set = 'STATUS'
        i_callback_user_command  = 'USER_COMMAND'
        is_layout                = layout
        it_fieldcat              = fieldcat[]
        it_special_groups        = gruppen[]
        it_sort                  = sorttab[]
        i_default                = 'X'
        i_save                   = 'A'
        is_variant               = variante
        it_events                = events[]
        is_print                 = g_s_print
      TABLES
        t_outtab                 = g_t_belege1
      EXCEPTIONS
        OTHERS                   = 2.                       "1790231
                                                            "1790231
  ENDIF.                                                    "1790231

* does the ALV return with an error ?
  IF  NOT sy-subrc IS INITIAL.         "Fehler vom ALV ?
    MESSAGE ID sy-msgid TYPE  'S'     NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                     " list_output_detail

*&---------------------------------------------------------------------*
*&   TOP_OF_PAGE_TOTALS
*&---------------------------------------------------------------------*

FORM top_of_page_totals.                                    "#EC CALLED

* go on when the report runs in print mode -> last line
  CHECK NOT sy-prdsn IS INITIAL.

  DATA: lr_content TYPE REF TO cl_salv_form_element.

*... (1) create the information to be displayed by using
*        the ALV Form elements
  PERFORM top_of_page_totals_render  CHANGING lr_content.

*... (2) Sending the information to the ALV
*        Once the inforation to be displayed has been
*        created the information has to be sent to the ALV
*        This is done by calling the static method
*        CL_SALV_FORM_CONTENT=>SET( <content> ) with the content
*        which is to be displayed.
*        Alternativly the function module REUSE_ALV_COMMENTARY_WRITE
*        can still be used.
  cl_salv_form_content=>set( lr_content ).

ENDFORM.                     "TOP_OF_PAGE_TOTALS

*&---------------------------------------------------------------------*
*&   TOP_OF_PAGE_TOTALS_RENDER
*&---------------------------------------------------------------------*

FORM top_of_page_totals_render
         CHANGING cr_content TYPE REF TO cl_salv_form_element.

  DATA: lr_grid     TYPE REF TO cl_salv_form_layout_grid,
        lr_flow     TYPE REF TO cl_salv_form_layout_flow,
        l_text(500) TYPE c,
        l_char(500) TYPE c.

*... create a grid
  CREATE OBJECT lr_grid.

  lr_flow = lr_grid->create_flow( row = 1  column = 1 ).

  IF  bwbst IS INITIAL.                                     "n599218
*   stocks only                                             "n599218
    WRITE : sy-pagno NO-SIGN      TO  g_s_header_77-page.   "n599218
    MOVE  : g_s_header_77         TO  l_text.
  ELSE.                                                     "n599218
*   stocks and values                                       "n599218
    WRITE : sy-pagno NO-SIGN      TO  g_s_header_91-page.   "n599218
    MOVE  : g_s_header_91         TO  l_text.               "n599218
  ENDIF.                                                    "n599218

* add line to object
  lr_flow->create_text( text = l_text ).

* copy whole header object
  cr_content = lr_grid.

ENDFORM.                     " TOP_OF_PAGE_TOTALS_RENDER

*----------------------------------------------------------------------*
* top_of_page_render.
*----------------------------------------------------------------------*

FORM top_of_page_render.

* interface structurebegin of g_s_bestand.

  DATA: lr_content TYPE REF TO cl_salv_form_element.

*... (1) create the information to be displayed by using
*        the ALV Form elements
  PERFORM create_alv_form_content_top CHANGING lr_content.

*... (2) Sending the information to the ALV
*        Once the inforation to be displayed has been
*        created the information has to be sent to the ALV
*        This is done by calling the static method
*        CL_SALV_FORM_CONTENT=>SET( <content> ) with the content
*        which is to be displayed.
*        Alternativly the function module REUSE_ALV_COMMENTARY_WRITE
*        can still be used.
  cl_salv_form_content=>set( lr_content ).

ENDFORM.                     " top_of_page_render

*----------------------------------------------------------------------*
* create_alv_form_content_top
*----------------------------------------------------------------------*
* baustelle

FORM create_alv_form_content_top
                   CHANGING cr_content TYPE REF TO cl_salv_form_element.

  DATA: lr_grid       TYPE REF TO cl_salv_form_layout_grid,
        lr_flow       TYPE REF TO cl_salv_form_layout_flow,
        l_text(500)   TYPE c,
        l_char(500)   TYPE c,

        lv_label(100) TYPE c,
        lv_value(400) TYPE c.

  DATA: l_row                   TYPE i,
        l_figure(24)            TYPE c,
        l_flag_tied_empties(01) TYPE c.

  DATA: l_f_text(60)        TYPE  c.                        "n999530

*----------------------------------------------------------------------*


*... create a grid
  CREATE OBJECT lr_grid.

* the current data are in interface structure g_s_bestand.

* in the case the report run in print or background mode
* --> print the old headlines

  IF NOT sy-prdsn IS INITIAL.
    ADD  1                    TO  l_row.
    lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

    IF  bwbst IS INITIAL.
*     stocks only
      WRITE : sy-pagno NO-SIGN    TO  g_s_header_77-page.
      MOVE  : g_s_header_77       TO  l_text.
    ELSE.
*     stocks and values
      WRITE : sy-pagno NO-SIGN    TO  g_s_header_91-page.
      MOVE  : g_s_header_91       TO  l_text.
    ENDIF.

*   add line to object
    lr_flow->create_text( text = l_text ).

    ADD  1                    TO  l_row.
  ENDIF.

* first line : plant or valuation area ---------------------------------
  ADD  1                    TO  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  IF  bwbst IS INITIAL.
    PERFORM  f2200_read_t001 USING g_s_bestand-werks.       "n999530

    WRITE g_s_bestand-werks  TO l_f_text.                   "n999530
    CONDENSE l_f_text.                                      "n999530
    CONCATENATE l_f_text     t001w-name1                    "n999530
                             INTO  l_f_text                 "n999530
                             SEPARATED BY space.            "n999530

    MOVE : TEXT-020          TO  l_text,
           l_f_text          TO  l_text+g_offset_header.    "n999530

  ELSE.
*   show valuation area
    MOVE : TEXT-025          TO  l_text,
           g_s_bestand-bwkey TO  l_text+g_offset_header.
  ENDIF.

* add line to object
*  lr_flow->create_text( text = l_text ).
  DATA(lo_label) = lr_flow->create_label( text = l_text(g_offset_header) ).
  DATA(lo_value) = lr_flow->create_text(  text = l_text+g_offset_header ).
  lo_label->set_label_for( lo_value ).

* second line : material number ----------------------------------------
  ADD   1                    TO  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  MOVE  : TEXT-021           TO  l_text.
  WRITE : g_s_bestand-matnr  TO  l_text+g_offset_header.

* add line to object
*  lr_flow->create_text( text = l_text ).
  lo_label = lr_flow->create_label( text = l_text(g_offset_header) ).
  lo_value = lr_flow->create_text(  text = l_text+g_offset_header ).
  lo_label->set_label_for( lo_value ).

* third line : material short text -------------------------------------
  ADD   1                    TO  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  PERFORM  f2100_mat_text    USING  g_s_bestand-matnr.

  MOVE : TEXT-022            TO  l_text,
         g_s_makt-maktx      TO  l_text+g_offset_header.

* add line to object
*  lr_flow->create_text( text = l_text ).
  lo_label = lr_flow->create_label( text = l_text(g_offset_header) ).
  lo_value = lr_flow->create_text(  text = l_text+g_offset_header ).
  lo_label->set_label_for( lo_value ).

* fourth line : batch if required --------------------------------------
  IF xchar = 'X'.
    ADD   1                  TO  l_row.
    lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

    MOVE : TEXT-023           TO  l_text,
           g_s_bestand-charg  TO  l_text+g_offset_header.

*   add line to object
*    lr_flow->create_text( text = l_text ).
    lo_label = lr_flow->create_label( text = l_text(g_offset_header) ).
    lo_value = lr_flow->create_text(  text = l_text+g_offset_header ).
    lo_label->set_label_for( lo_value ).
  ENDIF.

* line : stock and value on start date ------------------------------
* with one empty line
  ADD  2                     TO  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

* convert unit of measurement from internal to external format "n1018717
  WRITE : g_s_bestand-meins       TO  l_f_meins_external.   "n1018717

  CLEAR                           l_text.
  IF bwbst IS INITIAL.
*   stock on start date
    MOVE : g_date_line_from       TO  l_text.
    WRITE  g_s_bestand-anfmenge   TO l_figure
                                  UNIT  g_s_bestand-meins.
    MOVE  l_figure                TO  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  l_f_meins_external     TO  l_text+g_offset_unit.  "n1018717
    IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_01\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* Stock on start date in PUM
      WRITE g_s_bestand-/cwm/anfmenge TO l_figure
                                      UNIT g_s_bestand-/cwm/meins.
      MOVE  l_figure                  TO  l_text+gv_/cwm/offset_qty(24).
      WRITE g_s_bestand-/cwm/meins    TO  l_text+gv_/cwm/offset_unit.
    ENDIF.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_01 SPOTS es_rm07mlbd .
  ELSE.
* stocks and values on start date
    MOVE : g_date_line_from       TO  l_text.
    WRITE  g_s_bestand-anfmenge   TO l_figure
                                  UNIT  g_s_bestand-meins.
    MOVE  l_figure                TO  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  l_f_meins_external     TO  l_text+g_offset_unit.  "n1018717


    WRITE g_s_bestand-anfwert     TO l_figure
                                  CURRENCY  g_s_bestand-waers.
    MOVE  l_figure                TO  l_text+g_offset_value(24).
    MOVE  g_s_bestand-waers       TO  l_text+g_offset_curr.
  ENDIF.

*   add line to object
*  lr_flow->create_text( text = l_text ).
  DATA(lenght) = strlen( g_date_line_from-text ).
  lo_label = lr_flow->create_label( text = l_text(lenght) ).
  lo_value = lr_flow->create_text(  text = l_text+lenght ).
  lo_label->set_label_for( lo_value ).

* line : total quantity and value of goods receipts --------------------
  ADD  1                     TO  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  CLEAR                           l_text.

  IF bwbst IS INITIAL.
*   total quantities of goods receipts
    MOVE : TEXT-005               TO  l_text+2.
    lenght = strlen( l_text ).
    WRITE  g_s_bestand-soll       TO l_figure
                                  UNIT  g_s_bestand-meins.
    MOVE  l_figure                TO  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  l_f_meins_external      TO  l_text+g_offset_unit. "n1018717
    IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_02\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* total quantities of goods receipts in PUM
      WRITE g_s_bestand-/cwm/soll     TO l_figure
                                      UNIT g_s_bestand-/cwm/meins.
      MOVE  l_figure                  TO  l_text+gv_/cwm/offset_qty(24).
      WRITE g_s_bestand-/cwm/meins    TO  l_text+gv_/cwm/offset_unit.
    ENDIF.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_02 SPOTS es_rm07mlbd .
  ELSE.
*   total quantities and values of goods receipts
    MOVE : TEXT-030               TO  l_text+2.
    lenght = strlen( l_text ).
    WRITE  g_s_bestand-soll       TO l_figure
                                  UNIT  g_s_bestand-meins.
    MOVE  l_figure                TO  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  l_f_meins_external      TO  l_text+g_offset_unit. "n1018717

*    ENHANCEMENT-POINT ehp605_rm07mlbd_03 SPOTS es_rm07mlbd .
    WRITE g_s_bestand-sollwert     TO l_figure
                                  CURRENCY  g_s_bestand-waers.
    MOVE  l_figure                TO  l_text+g_offset_value(24).
    MOVE  g_s_bestand-waers       TO  l_text+g_offset_curr.
  ENDIF.

* add line to object
*  lr_flow->create_text( text = l_text ).
  lo_label = lr_flow->create_label( text = l_text(lenght) ).
  lo_value = lr_flow->create_text(  text = l_text+lenght ).
  lo_label->set_label_for( lo_value ).

* line : total quantity and value of goods issues ----------------------
  ADD  1                     TO  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  CLEAR                           l_text.

  IF bwbst IS INITIAL.
*   total quantities of goods issues
    MOVE : TEXT-006               TO  l_text+2.
    lenght = strlen( l_text ).
    COMPUTE  g_s_bestand-haben    =  g_s_bestand-haben * -1.
    WRITE  g_s_bestand-haben      TO l_figure
                                  UNIT  g_s_bestand-meins.
    MOVE  l_figure                TO  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  l_f_meins_external      TO  l_text+g_offset_unit. "n1018717
    IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_04\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* total quantities of goods issues
      COMPUTE g_s_bestand-/cwm/haben =  g_s_bestand-/cwm/haben * -1.
      WRITE g_s_bestand-/cwm/haben   TO l_figure
                                     UNIT g_s_bestand-/cwm/meins.
      MOVE  l_figure                 TO  l_text+gv_/cwm/offset_qty(24).
      WRITE g_s_bestand-/cwm/meins   TO  l_text+gv_/cwm/offset_unit.
    ENDIF.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_04 SPOTS es_rm07mlbd .
  ELSE.
*   total quantities of goods issues
    MOVE : TEXT-031               TO  l_text+2.
    lenght = strlen( l_text ).
    COMPUTE  g_s_bestand-haben    =  g_s_bestand-haben * -1.
    WRITE  g_s_bestand-haben      TO l_figure
                                  UNIT  g_s_bestand-meins.
    MOVE  l_figure                TO  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  l_f_meins_external      TO  l_text+g_offset_unit. "n1018717
    IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_05\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* total quantities of goods issues
      COMPUTE g_s_bestand-/cwm/haben =  g_s_bestand-/cwm/haben * -1.
      WRITE g_s_bestand-/cwm/haben   TO l_figure
                                     UNIT g_s_bestand-/cwm/meins.
      MOVE  l_figure                 TO  l_text+gv_/cwm/offset_qty(24).
      WRITE g_s_bestand-/cwm/meins   TO  l_text+gv_/cwm/offset_unit.
    ENDIF.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_05 SPOTS es_rm07mlbd .
    COMPUTE g_s_bestand-habenwert  =  g_s_bestand-habenwert * -1.
    WRITE g_s_bestand-habenwert   TO l_figure
                                  CURRENCY  g_s_bestand-waers.
    MOVE  l_figure                TO  l_text+g_offset_value(24).
    MOVE  g_s_bestand-waers       TO  l_text+g_offset_curr.
  ENDIF.

* add line to object
*  lr_flow->create_text( text = l_text ).
  lo_label = lr_flow->create_label( text = l_text(lenght) ).
  lo_value = lr_flow->create_text(  text = l_text+lenght ).
  lo_label->set_label_for( lo_value ).

* line : stock and value on end date ------------------------------
  ADD  1                     TO  l_row.
  lr_flow = lr_grid->create_flow( row = l_row  column = 1 ).

  CLEAR                           l_text.

  IF bwbst IS INITIAL.
*   stock on end date
    MOVE : g_date_line_to         TO  l_text.
    WRITE  g_s_bestand-endmenge   TO l_figure
                                  UNIT  g_s_bestand-meins.
    MOVE  l_figure                TO  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  l_f_meins_external      TO  l_text+g_offset_unit. "n1018717
*    ENHANCEMENT-POINT ehp605_rm07mlbd_06 SPOTS es_rm07mlbd .
  ELSE.
* stocks and values on end date
    MOVE : g_date_line_to         TO  l_text.
    WRITE  g_s_bestand-endmenge   TO l_figure
                                  UNIT  g_s_bestand-meins.
    MOVE  l_figure                TO  l_text+g_offset_qty(24).
*   move  g_s_bestand-meins       to  l_text+g_offset_unit.    "n1018717
    MOVE  l_f_meins_external      TO  l_text+g_offset_unit. "n1018717
    IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_07\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* Stock on end date
      WRITE g_s_bestand-/cwm/endmenge TO l_figure
                                      UNIT g_s_bestand-/cwm/meins.
      MOVE  l_figure                TO  l_text+gv_/cwm/offset_qty(24).
      WRITE g_s_bestand-/cwm/meins  TO  l_text+gv_/cwm/offset_unit.
    ENDIF.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_07 SPOTS es_rm07mlbd .
    WRITE g_s_bestand-endwert     TO l_figure
                                  CURRENCY  g_s_bestand-waers.
    MOVE  l_figure                TO  l_text+g_offset_value(24).
    MOVE  g_s_bestand-waers       TO  l_text+g_offset_curr.
  ENDIF.

* add line to object
*  lr_flow->create_text( text = l_text ).
  lenght = strlen( g_date_line_to-text ).
  lo_label = lr_flow->create_label( text = l_text(lenght) ).
  lo_value = lr_flow->create_text(  text = l_text+lenght ).
  lo_label->set_label_for( lo_value ).

* copy whole header object
  cr_content = lr_grid.

ENDFORM.                    " create_alv_form_content_top

*----------------------------------------------------------------------*
*    create_table_totals_hq
*----------------------------------------------------------------------*

FORM create_table_totals_hq.

* create output table
  LOOP AT bestand.
*   part 1 : create header table g_t_totals_header
    MOVE-CORRESPONDING  bestand        TO  g_s_totals_header.
    MOVE  sobkz                        TO  g_s_totals_header-sobkz.

    PERFORM  f2100_mat_text  USING  bestand-matnr.
    MOVE  g_s_makt-maktx     TO  g_s_totals_header-maktx.

    IF  bwbst IS INITIAL.
*     mode : stocks or special stocks
      PERFORM  f2200_read_t001 USING bestand-werks.

      MOVE  t001w-name1      TO  g_s_totals_header-name1.
    ELSE.
*     mode : valuated stocks
      IF  curm = '3'.
*       valuation level is company code
        SELECT SINGLE butxt  FROM t001
                             INTO g_f_butxt
        WHERE  bukrs = bestand-bwkey.

        IF sy-subrc IS INITIAL.
          MOVE  g_f_butxt    TO  g_s_totals_header-name1.
        ELSE.
          CLEAR              g_s_totals_header-name1.
        ENDIF.
      ELSE.
*       valuation level is plant -> take the name of the plant
        PERFORM  f2200_read_t001 USING bestand-werks.

        MOVE  t001w-name1    TO  g_s_totals_header-name1.
      ENDIF.
    ENDIF.

    APPEND  g_s_totals_header     TO  g_t_totals_header.

*   part 2 : create 4 lines in item table g_t_totals_item
    CLEAR                         g_s_totals_item.
    MOVE : bestand-bwkey          TO  g_s_totals_item-bwkey,
           bestand-werks          TO  g_s_totals_item-werks,
           bestand-matnr          TO  g_s_totals_item-matnr,
           bestand-charg          TO  g_s_totals_item-charg,
           bestand-meins          TO  g_s_totals_item-meins,
           bestand-waers          TO  g_s_totals_item-waers.

*   line with the stock on start date
    MOVE : g_date_line_from       TO  g_s_totals_item-stock_type,
           bestand-anfmenge       TO  g_s_totals_item-menge,
           bestand-anfwert        TO  g_s_totals_item-wert.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_08\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
    MOVE : bestand-/cwm/anfmenge  TO  g_s_totals_item-/cwm/menge,
           bestand-/cwm/meins     TO  g_s_totals_item-/cwm/meins.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_08 SPOTS es_rm07mlbd .
    PERFORM                       create_table_totals_hq_1.

*   line with the good receipts
    IF  bwbst = 'X'.
      MOVE : TEXT-030             TO  g_s_totals_item-stock_type+2,
             bestand-soll         TO  g_s_totals_item-menge,
             bestand-sollwert     TO  g_s_totals_item-wert.
    ELSE.
      MOVE : TEXT-005             TO  g_s_totals_item-stock_type+2,
             bestand-soll         TO  g_s_totals_item-menge.
    ENDIF.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_09\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* line with teh goods receipts
    MOVE : bestand-/cwm/soll    TO  g_s_totals_item-/cwm/menge,
           bestand-/cwm/meins   TO  g_s_totals_item-/cwm/meins.

*    ENHANCEMENT-POINT ehp605_rm07mlbd_09 SPOTS es_rm07mlbd .
    PERFORM                       create_table_totals_hq_1.

*   line with the good issues
    IF  bwbst = 'X'.
      MOVE : TEXT-031             TO  g_s_totals_item-stock_type+2.
      g_s_totals_item-menge       = bestand-haben      * -1.
      g_s_totals_item-wert        = bestand-habenwert  * -1.
    ELSE.
      MOVE : TEXT-006             TO  g_s_totals_item-stock_type+2.
      g_s_totals_item-menge       = bestand-haben      * -1.
    ENDIF.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_10\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* line with the good issues
    g_s_totals_item-/cwm/menge  = bestand-/cwm/haben * -1.
    MOVE : bestand-/cwm/meins     TO  g_s_totals_item-/cwm/meins.

*    ENHANCEMENT-POINT ehp605_rm07mlbd_10 SPOTS es_rm07mlbd .
    PERFORM                       create_table_totals_hq_1.

*   line with the tock on end date
    MOVE : g_date_line_to         TO  g_s_totals_item-stock_type,
           bestand-endmenge       TO  g_s_totals_item-menge,
           bestand-endwert        TO  g_s_totals_item-wert.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_11\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* line with the stock on end date
    MOVE : bestand-/cwm/endmenge  TO  g_s_totals_item-/cwm/menge,
           bestand-/cwm/meins     TO  g_s_totals_item-/cwm/meins.

*    ENHANCEMENT-POINT ehp605_rm07mlbd_11 SPOTS es_rm07mlbd .
    PERFORM                       create_table_totals_hq_1.
  ENDLOOP.

ENDFORM.                     " create_table_totals_hq

*----------------------------------------------------------------------*
* create_table_totals_hq_1.
*----------------------------------------------------------------------*

* colorize the numeric fields depending on the sign and append the
* entries into table G_T_TOTALS_ITEM

FORM create_table_totals_hq_1.

  REFRESH                    g_t_color.
  CLEAR                      g_s_color.

* colorize the quntities always
  IF      g_s_totals_item-menge > 0.
*   positive value -> green
    MOVE : 'MENGE'           TO  g_s_color-fieldname,
           '5'               TO  g_s_color-color-col,    "green
           '0'               TO  g_s_color-color-int.
    APPEND  g_s_color        TO  g_t_color.

    MOVE : 'MEINS'           TO  g_s_color-fieldname,
           '5'               TO  g_s_color-color-col,    "green
           '0'               TO  g_s_color-color-int.
    APPEND  g_s_color        TO  g_t_color.

  ELSEIF  g_s_totals_item-menge < 0.
*   negative value -> red
    MOVE : 'MENGE'           TO  g_s_color-fieldname,
           '6'               TO  g_s_color-color-col,    "red
           '0'               TO  g_s_color-color-int.
    APPEND  g_s_color        TO  g_t_color.

    MOVE : 'MEINS'           TO  g_s_color-fieldname,
           '6'               TO  g_s_color-color-col,    "red
           '0'               TO  g_s_color-color-int.
    APPEND  g_s_color        TO  g_t_color.
  ENDIF.

  IF  bwbst = 'X'.
*  colorize the values only in mode valuated stock
    IF      g_s_totals_item-wert > 0.
*     positive value -> green
      MOVE : 'WERT'          TO  g_s_color-fieldname,
             '5'             TO  g_s_color-color-col,    "green
             '0'             TO  g_s_color-color-int.
      APPEND  g_s_color      TO  g_t_color.

      MOVE : 'WAERS'         TO  g_s_color-fieldname,
             '5'             TO  g_s_color-color-col,    "green
             '0'             TO  g_s_color-color-int.
      APPEND  g_s_color      TO  g_t_color.

    ELSEIF  g_s_totals_item-wert < 0.
*     negative value -> red
      MOVE : 'WERT'          TO  g_s_color-fieldname,
             '6'             TO  g_s_color-color-col,    "red
             '0'             TO  g_s_color-color-int.
      APPEND  g_s_color      TO  g_t_color.

      MOVE : 'WAERS'         TO  g_s_color-fieldname,
             '6'             TO  g_s_color-color-col,    "red
             '0'             TO  g_s_color-color-int.
      APPEND  g_s_color      TO  g_t_color.
    ENDIF.
  ENDIF.

  IF  g_t_color[] IS INITIAL.
    CLEAR :                  g_s_totals_item-color.
  ELSE.
*   customizing : set the color information
    IF  g_cust_color  = 'X'.
      MOVE  g_t_color[]      TO  g_s_totals_item-color.
    ENDIF.
  ENDIF.

  ADD   1                    TO  g_s_totals_item-counter.
  APPEND  g_s_totals_item    TO  g_t_totals_item.
  CLEAR :                    g_s_totals_item-stock_type.

ENDFORM.                     " create_table_totals_hq_1.

*----------------------------------------------------------------------*
*    create_table_for_detail
*----------------------------------------------------------------------*

FORM create_table_for_detail.

  STATICS : l_flag_sorted(01)     TYPE  c.
  DATA    : l_tabix               LIKE  sy-tabix.

  IF gv_newdb = abap_true.
*   read it134m from db in form kontiert_aussortieren      "1784986v2!
    REFRESH g_t_mseg_lean.
    REFRESH matnr.
    matnr-sign = 'I'.
    matnr-option = 'EQ'.
    matnr-low = g_s_bestand_detail-matnr.
    matnr-high = space.
    APPEND matnr.
    PERFORM f1000_select_mseg_mkpf.
    PERFORM belege_sortieren.
    PERFORM summen_bilden.                                  "1784986
    SELECT matnr meins mtart FROM mara                      "1784986
      INTO CORRESPONDING FIELDS OF TABLE imara              "1784986
      WHERE  matnr  =  g_s_bestand_detail-matnr             "1784986
    ORDER BY PRIMARY KEY.                                   "1858578
    PERFORM kontiert_aussortieren.                          "1784986
    CLEAR l_flag_sorted.
  ENDIF.

* sort table with the documents
  IF  l_flag_sorted IS INITIAL.
    SORT  g_t_mseg_lean
      BY matnr werks charg budat mblnr zeile belnr.
    MOVE  'X'                TO  l_flag_sorted.
  ENDIF.

  REFRESH                    g_t_belege1.

* find the first entry with this material number
  READ TABLE g_t_mseg_lean   INTO  g_s_mseg_lean
    WITH KEY matnr = g_s_bestand_detail-matnr
      BINARY SEARCH.

  IF  sy-subrc IS INITIAL.
    MOVE  sy-tabix           TO  l_tabix.

    LOOP AT g_t_mseg_lean   INTO  g_s_mseg_lean
                             FROM l_tabix.

*     leave this loop when the material number changes
      IF  g_s_mseg_lean-matnr  NE  g_s_bestand_detail-matnr.
        EXIT.
      ENDIF.

      IF  bwbst IS INITIAL.
        CHECK : g_s_mseg_lean-werks = bestand-werks.        "n1390970
        CHECK : xchar               IS INITIAL       OR
                g_s_mseg_lean-charg = bestand-charg.
        MOVE-CORRESPONDING g_s_mseg_lean
                             TO  g_t_belege1.

*       enrich some fields with color and numeric fields with sign
        PERFORM  f9500_set_color_and_sign
                       USING  g_t_belege1  'G_T_BELEGE1'.
        APPEND                g_t_belege1.
      ELSE.
*       get the valuation area for this plant
        PERFORM  f9300_read_organ
                   USING     c_werks   g_s_mseg_lean-werks.

        CHECK : g_s_organ-bwkey = bestand-bwkey.            "184465
        MOVE-CORRESPONDING  g_s_mseg_lean
                             TO  g_t_belege1.

*       enrich some fields with color and numeric fields with sign
        PERFORM  f9500_set_color_and_sign
                       USING  g_t_belege1  'G_T_BELEGE1'.

        APPEND               g_t_belege1.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.                     " create_table_for_detail

*----------------------------------------------------------------------*
* create_table_totals_flat
*----------------------------------------------------------------------*

FORM create_table_totals_flat.

* create output table G-T-totals_flat
  LOOP AT bestand.
    REFRESH                  g_t_color.
    MOVE-CORRESPONDING  bestand        TO  g_s_totals_flat.
    MOVE  sobkz                        TO  g_s_totals_flat-sobkz.

    PERFORM  f2100_mat_text  USING  bestand-matnr.

*   show the GI with negative sign
    g_s_totals_flat-haben         = g_s_totals_flat-haben     * -1.

    MOVE : g_s_makt-maktx    TO  g_s_totals_flat-maktx,
           datum-low         TO  g_s_totals_flat-start_date,
           datum-high        TO  g_s_totals_flat-end_date.

    PERFORM  colorize_totals_flat   USING 'ANFMENGE'.
    PERFORM  colorize_totals_flat   USING 'SOLL'.
    PERFORM  colorize_totals_flat   USING 'HABEN'.
    PERFORM  colorize_totals_flat   USING 'ENDMENGE'.
    IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_12\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
      g_s_totals_flat-/cwm/haben    = g_s_totals_flat-/cwm/haben * -1.
      PERFORM  colorize_totals_flat   USING '/CWM/ANFMENGE'.
      PERFORM  colorize_totals_flat   USING '/CWM/SOLL'.
      PERFORM  colorize_totals_flat   USING '/CWM/HABEN'.
      PERFORM  colorize_totals_flat   USING '/CWM/ENDMENGE'.

    ENDIF.
*    ENHANCEMENT-POINT ehp605_rm07mlbd_12 SPOTS es_rm07mlbd .

    IF  bwbst = 'X'.
      g_s_totals_flat-habenwert     = g_s_totals_flat-habenwert * -1.

      PERFORM  colorize_totals_flat   USING 'ANFWERT'.
      PERFORM  colorize_totals_flat   USING 'SOLLWERT'.
      PERFORM  colorize_totals_flat   USING 'HABENWERT'.
      PERFORM  colorize_totals_flat   USING 'ENDWERT'.
    ENDIF.

    IF  g_t_color[] IS INITIAL.
      CLEAR                  g_s_totals_flat-color.
    ELSE.
      MOVE  g_t_color[]      TO  g_s_totals_flat-color.
    ENDIF.

* get the name of this plant                                "n999530
    PERFORM f2200_read_t001  USING  g_s_totals_flat-werks.  "n999530

    MOVE  t001w-name1        TO  g_s_totals_flat-name1.     "n999530

    APPEND  g_s_totals_flat  TO  g_t_totals_flat.
  ENDLOOP.

ENDFORM.                     " create_table_totals_flat

*----------------------------------------------------------------------*
*   colorize_totals_flat
*----------------------------------------------------------------------*

FORM  colorize_totals_flat   USING l_fieldname TYPE any.

  DATA : l_f_fieldname(30)   TYPE c.
  FIELD-SYMBOLS : <l_fs_field>.

* customizing : set the color information
  CHECK : g_cust_color  = 'X'.

  CONCATENATE  'G_S_TOTALS_FLAT-' l_fieldname
                             INTO l_f_fieldname.
  ASSIGN (l_f_fieldname)     TO  <l_fs_field>.

  CHECK sy-subrc IS INITIAL.

  IF      <l_fs_field> > 0.
    MOVE : l_fieldname       TO  g_s_color-fieldname,
           '5'               TO  g_s_color-color-col,    "green
           '0'               TO  g_s_color-color-int.
    APPEND  g_s_color        TO  g_t_color.

  ELSEIF  <l_fs_field> < 0.
    MOVE : l_fieldname       TO  g_s_color-fieldname,
         '6'               TO  g_s_color-color-col,    "red
         '0'               TO  g_s_color-color-int.
    APPEND  g_s_color        TO  g_t_color.

  ENDIF.

ENDFORM.                     " colorize_totals_flat

*----------------------------------------------------------------------*
* create_fieldcat_totals_flat
*----------------------------------------------------------------------*

FORM create_fieldcat_totals_flat.

  CLEAR : g_s_fieldcat,      g_f_col_pos.

  IF  bwbst = 'X'.
*   valuated stock
    PERFORM fc_s_flat USING 'BWKEY' 'MBEW' 'BWKEY'.
  ELSE.
*   take the plant
    PERFORM fc_s_flat USING 'WERKS' 'MARC' 'WERKS'.

    IF  xchar = 'X'.
*     take the batch number
      PERFORM fc_s_flat USING 'CHARG' 'MCHB' 'CHARG'.
    ENDIF.
  ENDIF.

  IF bwbst IS INITIAL.                                      "n999530
    MOVE  'X'                 TO  g_s_fieldcat-no_out.      "n999530
    MOVE : TEXT-024           TO  g_s_fieldcat-seltext_l,   "n999530
          'L'                 TO  g_s_fieldcat-ddictxt,     "n999530
          30                  TO  g_s_fieldcat-outputlen.   "n999530
    PERFORM fc_s_flat USING 'NAME1' 'T001W' 'NAME1'.        "n999530
  ENDIF.                                                    "n999530

  PERFORM fc_s_flat USING 'MATNR' 'MARA' 'MATNR'.

  MOVE  'X'                  TO  g_s_fieldcat-no_out.
  PERFORM fc_s_flat USING 'MAKTX' 'MAKT' 'MAKTX'.

  IF  sobkz IS INITIAL.
    MOVE  'X'                TO  g_s_fieldcat-no_out.
  ENDIF.

  PERFORM fc_s_flat USING 'SOBKZ' 'MSLB' 'SOBKZ'.

**********************************************************************
**** START OF NOTE 1064332                                   "n1064332
**** new logic for fields start_date & end_date              "n1064332
**********************************************************************

  ADD  : 1                   TO  g_f_col_pos.
  MOVE : 'START_DATE'        TO  g_s_fieldcat-fieldname,
         g_f_col_pos         TO  g_s_fieldcat-col_pos,
         'G_T_TOTALS_FLAT'   TO  g_s_fieldcat-tabname,
         TEXT-094            TO  g_s_fieldcat-seltext_l, "from date
         TEXT-094            TO  g_s_fieldcat-seltext_m, "from date
         TEXT-094            TO  g_s_fieldcat-seltext_s, "from date
         'L'                 TO  g_s_fieldcat-ddictxt,
         15                  TO  g_s_fieldcat-outputlen,
         'D'                 TO  g_s_fieldcat-inttype,
         'DATS'              TO  g_s_fieldcat-datatype.
* fields l_ref_* are no longer needed
*         l_ref_tabname       to  g_s_fieldcat-ref_tabname,
*         l_ref_fieldname     to  g_s_fieldcat-ref_fieldname.

  APPEND  g_s_fieldcat       TO  g_t_fieldcat_totals_flat.
  CLEAR                      g_s_fieldcat.


  ADD  : 1                   TO  g_f_col_pos.
  MOVE : 'END_DATE'          TO  g_s_fieldcat-fieldname,
         g_f_col_pos         TO  g_s_fieldcat-col_pos,
         'G_T_TOTALS_FLAT'   TO  g_s_fieldcat-tabname,
         TEXT-095            TO  g_s_fieldcat-seltext_l, "from date
         TEXT-095            TO  g_s_fieldcat-seltext_m, "from date
         TEXT-095            TO  g_s_fieldcat-seltext_s, "from date
         'L'                 TO  g_s_fieldcat-ddictxt,
         15                  TO  g_s_fieldcat-outputlen,
         'D'                 TO  g_s_fieldcat-inttype,
         'DATS'              TO  g_s_fieldcat-datatype.
* fields l_ref_* are no longer needed
*         l_ref_tabname       to  g_s_fieldcat-ref_tabname,
*         l_ref_fieldname     to  g_s_fieldcat-ref_fieldname.

  APPEND  g_s_fieldcat       TO  g_t_fieldcat_totals_flat.
  CLEAR                      g_s_fieldcat.


* old logic for fields start_date and end_date
*  move : text-094            to  g_s_fieldcat-selText_l, "from date
*         'L'                 to  g_s_fieldcat-ddictxt,
*         15                  to  g_s_fieldcat-outputlen.
*  perform fc_s_flat using 'START_DATE' 'MKPF' 'BUDAT'.
*
*  move : text-095            to  g_s_fieldcat-selText_l, "to date
*         'L'                 to  g_s_fieldcat-ddictxt,
*         15                  to  g_s_fieldcat-outputlen.
*  perform fc_s_flat using 'END_DATE' 'MKPF' 'BUDAT'.
*
**********************************************************************
**** END OF NOTE 1064332                                     "n1064332
**** new logic for fields start_date & end_date              "n1064332
**********************************************************************

* Always use the text from the text symbol                   "n1333069
  MOVE : TEXT-096            TO  g_s_fieldcat-seltext_l, "opening stock
         TEXT-096            TO  g_s_fieldcat-seltext_m,    "n1333069
         TEXT-096            TO  g_s_fieldcat-seltext_s,    "n1333069
         'L'                 TO  g_s_fieldcat-ddictxt,
         23                  TO  g_s_fieldcat-outputlen,
         'QUAN'              TO  g_s_fieldcat-datatype,     "n1399766
         'MEINS'             TO  g_s_fieldcat-qfieldname.
  MOVE : 'QUAN'              TO  g_s_fieldcat-datatype,     "n1441785
         'P'                 TO  g_s_fieldcat-inttype,      "n1441785
         13                  TO  g_s_fieldcat-intlen.       "n1441785
  PERFORM fc_s_flat USING 'ANFMENGE' '' ''.                 "n1333069

  MOVE : TEXT-097            TO  g_s_fieldcat-seltext_l, "sum receipts
         'L'                 TO  g_s_fieldcat-ddictxt,
         23                  TO  g_s_fieldcat-outputlen,
         'MEINS'             TO  g_s_fieldcat-qfieldname.
  PERFORM fc_s_flat USING 'SOLL' 'MSEG' 'MENGE'.

  MOVE : TEXT-098            TO  g_s_fieldcat-seltext_l, "sum issues
         'L'                 TO  g_s_fieldcat-ddictxt,
         23                  TO  g_s_fieldcat-outputlen,
         'MEINS'             TO  g_s_fieldcat-qfieldname.
  PERFORM fc_s_flat USING 'HABEN' 'MSEG' 'MENGE'.

* Always use the text from the text symbol                   "n1333069
  MOVE : TEXT-099            TO  g_s_fieldcat-seltext_l, "end stock
         TEXT-099            TO  g_s_fieldcat-seltext_m,    "n1333069
         TEXT-099            TO  g_s_fieldcat-seltext_s,    "n1333069
         'L'                 TO  g_s_fieldcat-ddictxt,
         23                  TO  g_s_fieldcat-outputlen,
         'QUAN'              TO  g_s_fieldcat-datatype,     "n1399766
         'MEINS'             TO  g_s_fieldcat-qfieldname.   "n1333069
  MOVE : 'QUAN'              TO  g_s_fieldcat-datatype,     "n1441785
         'P'                 TO  g_s_fieldcat-inttype,      "n1441785
         13                  TO  g_s_fieldcat-intlen.       "n1441785
  PERFORM fc_s_flat USING 'ENDMENGE' '' ''.

  PERFORM fc_s_flat USING 'MEINS' 'MARA' 'MEINS'.

  IF  bwbst = 'X'.
*   process the values, too
    MOVE : TEXT-100     TO  g_s_fieldcat-seltext_l, "opening value
           TEXT-100          TO  g_s_fieldcat-seltext_m,    "n1333069
           TEXT-100          TO  g_s_fieldcat-seltext_s,    "n1333069
           'L'               TO  g_s_fieldcat-ddictxt,
           23                TO  g_s_fieldcat-outputlen,
           'WAERS'           TO  g_s_fieldcat-cfieldname.   "n2459328
    PERFORM fc_s_flat USING 'ANFWERT' 'MSEG' 'DMBTR'.       "n2371746

    MOVE : TEXT-101     TO  g_s_fieldcat-seltext_l,  "sum GR values
           'L'          TO  g_s_fieldcat-ddictxt,
           23           TO  g_s_fieldcat-outputlen,
           'WAERS'      TO  g_s_fieldcat-cfieldname.
    PERFORM fc_s_flat USING 'SOLLWERT' 'MSEG' 'DMBTR'.

    MOVE : TEXT-102     TO  g_s_fieldcat-seltext_l,  "sum GI values
           'L'          TO  g_s_fieldcat-ddictxt,
           23           TO  g_s_fieldcat-outputlen,
           'WAERS'      TO  g_s_fieldcat-cfieldname.
    PERFORM fc_s_flat USING 'HABENWERT' 'MSEG' 'DMBTR'.

    MOVE : TEXT-103     TO  g_s_fieldcat-seltext_l,   "end value
           TEXT-103          TO  g_s_fieldcat-seltext_m,    "n1333069
           TEXT-103          TO  g_s_fieldcat-seltext_s,    "n1333069
           'L'               TO  g_s_fieldcat-ddictxt,
           23                TO  g_s_fieldcat-outputlen,
           'WAERS'           TO  g_s_fieldcat-cfieldname.   "n2459328
    PERFORM fc_s_flat USING 'ENDWERT' 'MSEG' 'DMBTR'.       "n2371746

    PERFORM fc_s_flat USING 'WAERS' 'T001' 'WAERS'.
  ENDIF.
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_13\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
    IF bwbst <> 'X'.
      MOVE:  TEXT-096       TO  g_s_fieldcat-seltext_l,
             'L'            TO  g_s_fieldcat-ddictxt,
             23             TO  g_s_fieldcat-outputlen,
             '/CWM/MEINS'   TO  g_s_fieldcat-qfieldname.
      PERFORM fc_s_flat USING '/CWM/ANFMENGE' 'MSEG' '/CWM/MENGE'.
    ENDIF.

    IF bwbst <> 'X'.
      MOVE:  TEXT-097       TO  g_s_fieldcat-seltext_l,
             'L'            TO  g_s_fieldcat-ddictxt,
             23             TO  g_s_fieldcat-outputlen,
             '/CWM/MEINS'   TO  g_s_fieldcat-qfieldname.
      PERFORM fc_s_flat USING '/CWM/SOLL    ' 'MSEG' '/CWM/MENGE'.
    ENDIF.

    IF bwbst <> 'X'.
      MOVE:  TEXT-098       TO  g_s_fieldcat-seltext_l,
             'L'            TO  g_s_fieldcat-ddictxt,
             23             TO  g_s_fieldcat-outputlen,
             '/CWM/MEINS'   TO  g_s_fieldcat-qfieldname.
      PERFORM fc_s_flat USING '/CWM/HABEN   ' 'MSEG' '/CWM/MENGE'.
    ENDIF.

    IF bwbst <> 'X'.
      MOVE:  TEXT-099       TO  g_s_fieldcat-seltext_l,
             'L'            TO  g_s_fieldcat-ddictxt,
             23             TO  g_s_fieldcat-outputlen,
             '/CWM/MEINS'   TO  g_s_fieldcat-qfieldname.
      PERFORM fc_s_flat USING '/CWM/ENDMENGE' 'MSEG' '/CWM/MENGE'.
    ENDIF.

    IF bwbst <> 'X'.
      PERFORM fc_s_flat USING '/CWM/MEINS'    'MSEG' '/CWM/MEINS'.
    ENDIF.
* use ref to /CWM/VALUM
    IF bwbst = 'X'.
      LOOP AT g_t_fieldcat_totals_flat INTO  g_s_fieldcat.
        CHECK : g_s_fieldcat-fieldname    = 'MEINS'.
        g_s_fieldcat-ref_tabname = 'MARA'.
        g_s_fieldcat-ref_fieldname = '/CWM/VALUM'.
        MODIFY g_t_fieldcat_totals_flat FROM g_s_fieldcat.
        CLEAR                g_s_fieldcat.
      ENDLOOP.
    ENDIF.
  ENDIF.

*  ENHANCEMENT-POINT ehp605_rm07mlbd_13 SPOTS es_rm07mlbd .

ENDFORM.                     " create_fieldcat_totals_flat.

*----------------------------------------------------------------------*
*    FC_S_FLAT
*----------------------------------------------------------------------*

FORM fc_s_flat     USING     l_fieldname     TYPE fieldname
                             l_ref_tabname   TYPE ddobjname
                             l_ref_fieldname TYPE fieldname.

  ADD  : 1                   TO  g_f_col_pos.
  MOVE : l_fieldname         TO  g_s_fieldcat-fieldname,
         g_f_col_pos         TO  g_s_fieldcat-col_pos,
         'G_T_TOTALS_FLAT'   TO  g_s_fieldcat-tabname,
         l_ref_tabname       TO  g_s_fieldcat-ref_tabname,
         l_ref_fieldname     TO  g_s_fieldcat-ref_fieldname.
*  ENHANCEMENT-POINT fc_s_flat_01 SPOTS es_rm07mlbd.
  APPEND  g_s_fieldcat       TO  g_t_fieldcat_totals_flat.
  CLEAR                      g_s_fieldcat.

ENDFORM.                     "fc_s_flat

*----------------------------------------------------------------------*
*    alv_flat_list_sums_only
*----------------------------------------------------------------------*

FORM alv_flat_list_sums_only.

  DATA: lv_lvc_s_glay TYPE lvc_s_glay.                      "1790231

* assign the form routines to the events
  MOVE :  'PF_STATUS_SET'         TO  g_t_events_totals_flat-name,
          'PF_STATUS_SET_TOTALS'  TO  g_t_events_totals_flat-form.
  APPEND                              g_t_events_totals_flat.

  MOVE :  'USER_COMMAND'          TO  g_t_events_totals_flat-name,
          'USER_COMMAND_TOTALS'   TO  g_t_events_totals_flat-form.
  APPEND                              g_t_events_totals_flat.

  MOVE : 'TOP_OF_PAGE'            TO  g_t_events_totals_flat-name,
         'TOP_OF_PAGE_TOTALS'     TO  g_t_events_totals_flat-form.
  APPEND                              g_t_events_totals_flat.

  MOVE :       'END_OF_LIST'      TO  g_t_events_totals_flat-name,
         'PRINT_END_OF_LIST'      TO  g_t_events_totals_flat-form.
  APPEND                              g_t_events_totals_flat.

* handling for double click
  g_s_layout_totals_flat-f2code           = '9PBP'.
  g_s_layout_totals_flat-coltab_fieldname = 'COLOR'.

  IF  g_flag_break-b6 = 'X'.                                "n921164
    BREAK-POINT              ID mmim_rep_mb5b.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  ENDIF.                                                    "n921164

* liste aufbauen
  IF gv_ui_opt_active = abap_false OR p_grid = abap_false.  "1790231

    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
      EXPORTING
        i_interface_check  = g_flag_i_check
        i_callback_program = repid
        is_layout          = g_s_layout_totals_flat
        it_fieldcat        = g_t_fieldcat_totals_flat[]
        it_sort            = g_t_sorttab
        i_default          = 'X'  "allow default variant
        i_save             = 'A'
        is_variant         = g_s_vari_sumfl
        it_events          = g_t_events_totals_flat[]
        is_print           = g_s_print
      TABLES
        t_outtab           = g_t_totals_flat
      EXCEPTIONS
        OTHERS             = 1.

  ELSE.                                                     "1790231
    lv_lvc_s_glay-coll_top_p = abap_true.                   "1790231
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'                  "1790231
      EXPORTING                                             "1790231
        i_interface_check  = g_flag_i_check
        i_callback_program = repid
        i_grid_settings    = lv_lvc_s_glay
        is_layout          = g_s_layout_totals_flat
        it_fieldcat        = g_t_fieldcat_totals_flat[]
        it_sort            = g_t_sorttab
        i_default          = 'X'  "allow default variant
        i_save             = 'A'
        is_variant         = g_s_vari_sumfl
        it_events          = g_t_events_totals_flat[]
        is_print           = g_s_print
      TABLES
        t_outtab           = g_t_totals_flat
      EXCEPTIONS
        OTHERS             = 1.                             "1790231
                                                            "1790231
  ENDIF.                                                    "1790231


  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                     "alv_flat_list_sums_only

*----------------------------------------------------------------------*
*    create_fieldcat_totals_hq
*----------------------------------------------------------------------*
*baustelle
FORM create_fieldcat_totals_hq.

* create fieldcat
  CLEAR : g_s_fieldcat,      g_f_col_pos.

* part 1 : for the header table
  IF  bwbst = 'X'.                                          "n999530
    PERFORM  fc_hq
        USING   'G_T_TOTALS_HEADER' 'BWKEY'  'MBEW'  'BWKEY'.
  ENDIF.                                                    "n999530

  IF bwbst IS INITIAL.                                      "n999530
    PERFORM  fc_hq
        USING   'G_T_TOTALS_HEADER' 'WERKS'  'MARC'  'WERKS'.

    MOVE : 'X'                 TO  g_s_fieldcat-no_out.
    MOVE : 30                  TO  g_s_fieldcat-outputlen,  "n999530
           TEXT-024            TO  g_s_fieldcat-seltext_l,  "n999530
           'L'                 TO  g_s_fieldcat-ddictxt.    "n999530
    PERFORM  fc_hq
      USING  'G_T_TOTALS_HEADER'  'NAME1'  'T001W'  'NAME1'.
  ENDIF.                                                    "n999530

  PERFORM  fc_hq
    USING  'G_T_TOTALS_HEADER'  'MATNR'  'MARA'  'MATNR'.

  IF  sobkz IS INITIAL.
    MOVE : 'X'               TO  g_s_fieldcat-no_out.
  ENDIF.

  PERFORM  fc_hq
    USING  'G_T_TOTALS_HEADER'  'SOBKZ'  'MSLB'  'SOBKZ'.

  PERFORM  fc_hq
    USING  'G_T_TOTALS_HEADER'  'MAKTX'  'MAKT'  'MAKTX'.

  IF  xchar IS INITIAL.
    MOVE : 'X'               TO  g_s_fieldcat-no_out.
  ENDIF.

  PERFORM  fc_hq
      USING  'G_T_TOTALS_HEADER'  'CHARG'  'MCHB'  'CHARG'.

  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_14\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
* MOVE : 'HIIER'        TO  g_s_fieldcat-seltext_l,
    IF bwbst <> 'X'.
      CLEAR: g_s_fieldcat-no_out.
      MOVE : TEXT-104       TO  g_s_fieldcat-seltext_l,
             'L'            TO  g_s_fieldcat-ddictxt,
             23             TO  g_s_fieldcat-outputlen,
             '/CWM/MEINS'   TO  g_s_fieldcat-qfieldname.
      PERFORM fc_hq
          USING 'G_T_TOTALS_ITEM' '/CWM/MENGE' space space.
*      USING 'G_T_TOTALS_HEADER' '/CWM/MENGE' 'MSEG' '/CWM/MENGE'.
* Modify the last added line to positioning the field at the end of output line
      READ TABLE g_t_fieldcat_totals_hq INDEX sy-tabix
                                      INTO g_s_fieldcat
                                      TRANSPORTING col_pos.
      IF sy-subrc = 0.
        g_s_fieldcat-col_pos = 94.
        MODIFY g_t_fieldcat_totals_hq INDEX sy-tabix
                                      FROM g_s_fieldcat
                                      TRANSPORTING col_pos.
      ENDIF.
      CLEAR g_s_fieldcat.
    ENDIF.

    IF bwbst <> 'X'.
      PERFORM  fc_hq
        USING  'G_T_TOTALS_ITEM'  '/CWM/MEINS' 'MSEG' '/CWM/MEINS'.
*    using  'G_T_TOTALS_HEADER'  '/CWM/MEINS' 'MSEG' '/CWM/MEINS'.
* Modify the last added line to positioning the field at the end of output line
      READ TABLE g_t_fieldcat_totals_hq INDEX sy-tabix
                                        INTO g_s_fieldcat
                                        TRANSPORTING col_pos.
      IF sy-subrc = 0.
        g_s_fieldcat-col_pos = 95.
        MODIFY g_t_fieldcat_totals_hq INDEX sy-tabix
                                      FROM g_s_fieldcat
                                      TRANSPORTING col_pos.
      ENDIF.
      CLEAR g_s_fieldcat.
    ENDIF.
  ENDIF.

*  ENHANCEMENT-POINT ehp605_rm07mlbd_14 SPOTS es_rm07mlbd .

** part 2 : for the item table

* hidden key fields
  MOVE : 'X'               TO  g_s_fieldcat-no_out.
  PERFORM  fc_hq
      USING   'G_T_TOTALS_ITEM' 'BWKEY'  'MBEW'  'BWKEY'.

  MOVE : 'X'               TO  g_s_fieldcat-no_out.
  PERFORM  fc_hq
      USING   'G_T_TOTALS_ITEM' 'WERKS'  'MARC'  'WERKS'.

  MOVE : 'X'               TO  g_s_fieldcat-no_out.
  PERFORM  fc_hq
    USING  'G_T_TOTALS_ITEM'  'MATNR'  'MARA'  'MATNR'.

  MOVE : 'X'               TO  g_s_fieldcat-no_out.
  PERFORM  fc_hq
      USING  'G_T_TOTALS_ITEM'  'CHARG'  'MCHB'  'CHARG'.

  MOVE : 'X'               TO  g_s_fieldcat-no_out.
  PERFORM  fc_hq
      USING  'G_T_TOTALS_ITEM'  'COUNTER'  ' '  ''.

  MOVE : 40                   TO  g_s_fieldcat-outputlen.
  PERFORM  fc_hq
      USING  'G_T_TOTALS_ITEM'  'STOCK_TYPE' space space.

* do not allow to form sums for the column quantity         "n951316
  MOVE : 'X'            TO  g_s_fieldcat-no_sum.            "n951316
  MOVE : 23             TO  g_s_fieldcat-outputlen,
         TEXT-104      TO  g_s_fieldcat-seltext_l,  "quantities
         'L'            TO  g_s_fieldcat-ddictxt,
         'MENGE_D'      TO  g_s_fieldcat-rollname,
         'QUAN'         TO  g_s_fieldcat-datatype,          "2288623
         'MEINS'        TO  g_s_fieldcat-qfieldname.
  PERFORM  fc_hq
    USING  'G_T_TOTALS_ITEM'  'MENGE' 'MSEG' 'MENGE'.       "2288623

  PERFORM  fc_hq
    USING  'G_T_TOTALS_ITEM'  'MEINS'   'MARA'  'MEINS'.

  IF  bwbst = 'X'.
*   with valuation
*   do not allow to form sums for the column value          "n951316
    MOVE : 'X'          TO  g_s_fieldcat-no_sum.            "n951316
    MOVE : 23           TO  g_s_fieldcat-outputlen,
           TEXT-105     TO  g_s_fieldcat-seltext_l,   "values
           'L'          TO  g_s_fieldcat-ddictxt,
           'DMBTR'      TO  g_s_fieldcat-rollname,
           'CURR'       TO  g_s_fieldcat-datatype,          "2288623
           'WAERS'      TO  g_s_fieldcat-cfieldname.
    PERFORM  fc_hq
      USING  'G_T_TOTALS_ITEM'  'WERT' 'MSEG' 'DMBTR'.      "2288623

    PERFORM  fc_hq
      USING  'G_T_TOTALS_ITEM'  'WAERS'   'T001'  'WAERS'.
  ENDIF.

ENDFORM.                     "create_fieldcat_totals_hq

*----------------------------------------------------------------------*
*    FC_HQ
*----------------------------------------------------------------------*

FORM fc_hq         USING     l_tabname        TYPE  ddobjname
                             l_fieldname      TYPE  fieldname
                             l_ref_tabname    TYPE  ddobjname
                             l_ref_fieldname  TYPE  fieldname.

  ADD  : 1                   TO  g_f_col_pos.
  MOVE : l_fieldname         TO  g_s_fieldcat-fieldname,
         g_f_col_pos         TO  g_s_fieldcat-col_pos,
         l_tabname           TO  g_s_fieldcat-tabname,
         l_ref_tabname       TO  g_s_fieldcat-ref_tabname,
         l_ref_fieldname     TO  g_s_fieldcat-ref_fieldname.
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\FC_HQ_01\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* use ref to /CWM/VALUM
    IF bwbst = 'X' AND g_s_fieldcat-fieldname = 'MEINS'.
      g_s_fieldcat-ref_tabname   = 'MARA'.
      g_s_fieldcat-ref_fieldname = '/CWM/VALUM'.
    ENDIF.
  ENDIF.
*  ENHANCEMENT-POINT fc_hq_01 SPOTS es_rm07mlbd.

  APPEND  g_s_fieldcat       TO  g_t_fieldcat_totals_hq.
  CLEAR                      g_s_fieldcat.

ENDFORM.                     "fc_hq

*----------------------------------------------------------------------*
* alv_hierseq_list_totals
*----------------------------------------------------------------------*

FORM alv_hierseq_list_totals.

* fill layout : consider double click and color subtables
  g_s_layout_totals_hq-coltab_fieldname = 'COLOR'.
  g_s_layout_totals_hq-f2code           = '9PBP'.

* create other tables and structures
  MOVE : 'BWKEY'             TO  g_s_keyinfo_totals_hq-header01,
         'BWKEY'             TO  g_s_keyinfo_totals_hq-item01,

         'WERKS'             TO  g_s_keyinfo_totals_hq-header02,
         'WERKS'             TO  g_s_keyinfo_totals_hq-item02,

         'MATNR'             TO  g_s_keyinfo_totals_hq-header03,
         'MATNR'             TO  g_s_keyinfo_totals_hq-item03,

         'CHARG'             TO  g_s_keyinfo_totals_hq-header04,
         'CHARG'             TO  g_s_keyinfo_totals_hq-item04,

         'COUNTER'           TO  g_s_keyinfo_totals_hq-item05.

* create the events table
  MOVE : 'PF_STATUS_SET'          TO  events_hierseq-name,
         'PF_STATUS_SET_TOTALS'  TO  events_hierseq-form.
  APPEND                              events_hierseq.

  MOVE : 'USER_COMMAND'           TO  events_hierseq-name,
         'USER_COMMAND_TOTALS'    TO  events_hierseq-form.
  APPEND                              events_hierseq.

  MOVE : 'TOP_OF_PAGE'            TO  events_hierseq-name,
         'TOP_OF_PAGE_TOTALS'     TO  events_hierseq-form.
  APPEND                              events_hierseq.

  MOVE :       'END_OF_LIST'      TO  events_hierseq-name,
         'PRINT_END_OF_LIST'      TO  events_hierseq-form.
  APPEND                              events_hierseq.

* create the sort table g_t_SORT_totals_hq
  CLEAR                           g_s_sort_totals_hq.
  MOVE : 'G_T_TOTALS_ITEM'        TO  g_s_sort_totals_hq-tabname,
         'X'                      TO  g_s_sort_totals_hq-up.

  MOVE  'BWKEY'                   TO  g_s_sort_totals_hq-fieldname.
  ADD     1                       TO  g_s_sort_totals_hq-spos.
  APPEND  g_s_sort_totals_hq      TO  g_t_sort_totals_hq.

  MOVE  'WERKS'                   TO  g_s_sort_totals_hq-fieldname.
  ADD     1                       TO  g_s_sort_totals_hq-spos.
  APPEND  g_s_sort_totals_hq      TO  g_t_sort_totals_hq.

  MOVE  'MATNR'                   TO  g_s_sort_totals_hq-fieldname.
  ADD     1                       TO  g_s_sort_totals_hq-spos.
  APPEND  g_s_sort_totals_hq      TO  g_t_sort_totals_hq.

  MOVE  'CHARG'                   TO  g_s_sort_totals_hq-fieldname.
  ADD     1                       TO  g_s_sort_totals_hq-spos.
  APPEND  g_s_sort_totals_hq      TO  g_t_sort_totals_hq.

  MOVE  'COUNTER'                 TO  g_s_sort_totals_hq-fieldname.
  ADD     1                       TO  g_s_sort_totals_hq-spos.
  APPEND  g_s_sort_totals_hq      TO  g_t_sort_totals_hq.

  IF  g_flag_break-b7 = 'X'.                                "n921164
    BREAK-POINT              ID mmim_rep_mb5b.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  ENDIF.                                                    "n921164

  CALL FUNCTION 'REUSE_ALV_HIERSEQ_LIST_DISPLAY'
    EXPORTING
      i_interface_check  = g_flag_i_check
      i_callback_program = repid
      it_events          = events_hierseq[]
      is_layout          = g_s_layout_totals_hq
      is_print           = g_s_print
      it_fieldcat        = g_t_fieldcat_totals_hq
      it_sort            = g_t_sort_totals_hq
      i_default          = 'X'
      i_save             = 'A'
      is_variant         = g_s_vari_sumhq
      i_tabname_header   = 'G_T_TOTALS_HEADER'
      i_tabname_item     = 'G_T_TOTALS_ITEM'
      is_keyinfo         = g_s_keyinfo_totals_hq
    TABLES
      t_outtab_header    = g_t_totals_header[]
      t_outtab_item      = g_t_totals_item[]
    EXCEPTIONS
      OTHERS             = 1.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                     " alv_hierseq_list_totals

*-----------------------------------------------------------"n599218


*  Aus dieser Unterroutine heraus werden implizit, d.h. in der Schleife
*  über alle selektierten Bestände, die zugehörigen Materialbelege
*  aufgerufen.
*  Die Bestände werden zum Anfangs- und Enddatum als Summen zu
*  folgendem Schlüssel, der im Listkopf geführt wird, ausgegeben:
*  Buchungskreis bzw. Werk, Material, Charge. Nicht-chargenpflichtige
*  Materialien werden auf Materialebene angezeigt.
*  Es folgt jeweils eine Liste mit den einzelnen Belegpositionen.

*********************** Ende HAUPTPROGRAMM *****************************
*
************************* FORMROUTINEN *********************************

*&---------------------------------------------------------------------*
*&      Form  EINGABEN_PRUEFEN
*&---------------------------------------------------------------------*
*       Prüfung der Eingaben auf dem Selektionsbild                    *
*----------------------------------------------------------------------*
FORM eingaben_pruefen.

* check the entries only in releases >= 46B
  CALL FUNCTION 'MMIM_ENTRYCHECK_MAIN'
    TABLES
      it_matnr = matnr
      it_werks = werks
      it_lgort = lgort
      it_bwart = bwart
      it_bukrs = bukrs.

*  Die Selektionseingaben Buchungskreis und Werk werden hierarchisch
*  verstanden, d.h. es werden nur Werke innerhalb der angegebenen
*  Buchungskreise selektiert.
*  Lagerort-/Chargenbestand: Da die Werksbezeichnung eindeutig ist,
*  finden alle Selektionen auf Werksebene bzw. - falls mindestens ein
*  Lagerort eingegeben wurde - auf der Ebene der eingegebenen Lagerorte
*  statt. Die Ausgabe erfolgt auf Werksebene des Materials / der Charge.
*  Bewerteter Bestand: Die Ausgabe erfolgt auf Bewertungskreisebene,
*  d.h. je nach Einstellung in der Tabelle TCURM auf Werks- oder
*  Buchungskreisebene.

*  Feststellen, ob der Bewertungskreis auf Buchungskreis- oder
*  Werksebene liegt:
*  tcurm-bwkrs_cus = 1  =>  Bewertungskreis auf Werksebene,
*  tcurm-bwkrs_cus = 3  =>  Bewertungskreis auf Buchungskreisebene.
  SELECT bwkrs_cus FROM tcurm INTO curm
            ORDER BY PRIMARY KEY.
  ENDSELECT.

  IF xchar = ' ' AND NOT charg-low IS INITIAL.
    xchar = 'X'.
  ENDIF.
  IF xchar = ' ' AND NOT xnomchb IS INITIAL.                "838360_v
    xchar = 'X'.
  ENDIF.                                                    "838360_^

  IF sbbst = 'X' AND sobkz IS INITIAL.
    MESSAGE e286.
*   Bitte ein Sonderbestandskennzeichen eingeben.
  ELSEIF sbbst = ' ' AND NOT sobkz IS INITIAL.
    CLEAR sobkz.
    MESSAGE w287.
*   Sonderbestandskennzeichen wird zurückgesetzt.
  ENDIF.

* reset the entries for plant when valuation area is        "n599218
* company code and mode is valuated stock                   "n599218
*  IF     curm  = '3'      AND                               "n599218
*         bwbst = 'X'.                                       "n599218
*    IF  NOT werks[]  IS INITIAL.                            "n599218
**     reset the restricts for plants                        "n599218
*      CLEAR                  werks.                         "n599218
*      REFRESH                werks.                         "n599218
**     text-074 : valuation area = company code              "n599218
**     text-075 : entries for plant will be reset            "n599218
*      MESSAGE w010(ad) WITH TEXT-074 TEXT-075 space space.  "n599218
*    ENDIF.                                                  "n599218
*  ENDIF.                                                    "n599218

  IF bwbst = 'X' AND NOT charg IS INITIAL
    OR bwbst = 'X' AND NOT xchar IS INITIAL.
    CLEAR charg. REFRESH charg.
    MESSAGE w285.
*   Charge wird zurückgesetzt.
  ENDIF.
*  IF bwbst = 'X' AND NOT lgort IS INITIAL.
*    CLEAR lgort. REFRESH lgort.
*    MESSAGE w284.
**   Lagerort wird zurückgesetzt.
*  ENDIF.

* consider and prepare select-options depending on the required
* special stock indicator
  REFRESH                    g_ra_sobkz.
  CLEAR                      g_ra_sobkz.

  IF      lgbst = 'X'.       "only Storage loc./batch stock
*   create ranges table : select only sobkz = space
    PERFORM f0500_append_ra_sobkz   USING  c_space.

  ELSEIF  bwbst = 'X'.       "only valuated stocks
*   take all special stock indicators / the record selection
*   will be done after the database selection

  ELSEIF  sbbst = 'X'.       "only special stocks
    PERFORM f0500_append_ra_sobkz   USING  sobkz.

*    ENHANCEMENT-SECTION     rm07mlbd_02 SPOTS es_rm07mlbd.

    "{ Begin ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }
* DI A&D SSP
    IF      sobkz  =  'O'  OR
            sobkz  =  'V'  OR
            sobkz  =  'W'  OR
            sobkz  =  'E'  OR
            sobkz  =  'K'  OR
            sobkz  =  'M'  OR
            sobkz  =  'Q'  OR
            sobkz  =  'T'  OR
* DI IS-ADEC-SSP  Customer Stock for DI or DIMP User
            sobkz  =  'B' OR
* A&D IS-ADEC-SUB Sales Order Stock, Project Stock,       "v_n_GA1551829
* Customer Stock, Vendor Consignment or RTP Stock
            sobkz  =  cl_adsub_constants=>c  OR
            sobkz  =  cl_adsub_constants=>f  OR
            sobkz  =  cl_adsub_constants=>i  OR
            sobkz  =  cl_adsub_constants=>j  OR
            sobkz  =  cl_adsub_constants=>r.              "^_n_GA1551829
*            sobkz  =  cl_adsub_constants=>t.       "SIT SPAU "GA1990678

*     ok; no aktion taken
    ELSE.
      SET CURSOR             FIELD  'SOBKZ'.
*     Sonderbestandskennzeichen nicht vorhanden
      MESSAGE                e221.
    ENDIF.
    "{ End ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }

*    END-ENHANCEMENT-SECTION.
  ENDIF.

*  IF bwbst = 'X' AND NOT bwart IS INITIAL.
*    CLEAR bwart. REFRESH bwart.
*    MESSAGE w298.
**   Bewegungsart wird zurückgesetzt
*  ENDIF.
  IF bwbst = ' ' AND NOT bwtar IS INITIAL.
    CLEAR bwtar. REFRESH bwtar.
    MESSAGE w288.
*   Bewertungsart wird zurückgesetzt.
  ENDIF.

  IF gv_switch_ehp6ru = abap_true AND NOT hkont[] IS INITIAL.
    IF bwbst = ' '.
*     G/L account will be reset, if stock type is not Valuated Stock
      CLEAR hkont. REFRESH hkont.
      MESSAGE w481.
    ELSE.
*     Company code or plant should be filled to build G_T_ORGAN table
      IF bukrs[] IS INITIAL AND werks[] IS INITIAL.
        SET CURSOR FIELD 'HKONT-LOW'.
        MESSAGE e480.
      ENDIF.
    ENDIF.
  ENDIF.

* The function "no cancellations" is not possible
* for valuated stock
*   for the selection of the reversal movements only in release >=45B
  IF nosto = 'X' AND bwbst = 'X'.                           "204463
    MESSAGE e151(e1) WITH 'VALUATED_STOCK'                  "204463
                       'NO_CANCELLATIONS'.                  "204463
  ENDIF.                                                    "204463

  IF NOT p_vari IS INITIAL.
    MOVE variante TO def_variante.
    MOVE p_vari TO def_variante-variant.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = variant_save
      CHANGING
        cs_variant = def_variante.
    variante = def_variante.
  ELSE.
*   the user wants no initial display variant               "n599218
    IF  NOT alv_default_variant  IS INITIAL.                "n599218
*     but the SAP-LIST-VIEWER will apply the existing       "n599218
*     initial display variant / emerge warning 393 ?        "n599218
      CALL FUNCTION 'ME_CHECK_T160M'               "n599218
        EXPORTING                                           "n599218
          i_arbgb = 'M7'                         "n599218
          i_msgnr = '393'                        "n599218
        EXCEPTIONS                                          "n599218
          nothing = 0                            "n599218
          OTHERS  = 1.                           "n599218
                                                            "n599218
      IF sy-subrc <> 0.                                     "n599218
*       list will be created using the initial layout &     "n599218
        MESSAGE w393(m7)     WITH  alv_default_variant.     "n599218
      ENDIF.                                                "n599218
    ENDIF.                                                  "n599218

    CLEAR variante.
    variante-report = repid.
  ENDIF.

ENDFORM.                               " EINGABEN_PRÜFEN

*----------------------------------------------------------------------*
*    VARIANT_CHECK_EXISTENCE
*----------------------------------------------------------------------*

FORM variant_check_existence
         USING  l_vari       LIKE  disvariant-variant
                ls_vari      LIKE  disvariant
                ls_vari_def  LIKE  disvariant.


  MOVE  l_vari               TO  ls_vari-variant.

  IF  NOT l_vari IS INITIAL.
*   parameter for the variant is filled.

    CALL FUNCTION 'REUSE_ALV_VARIANT_EXISTENCE'
      EXPORTING
        i_save     = 'A'
      CHANGING
        cs_variant = ls_vari.

*   in the case the variant does not exist this function
*   module sends the error message directly
  ELSE.
*   the user wants no initial display variant
    IF  NOT ls_vari_def-variant  IS INITIAL.
*     but the SAP-LIST-VIEWER will apply the existing       "n599218
*     initial display variant / emerge warning 393 ?        "n599218
      CALL FUNCTION 'ME_CHECK_T160M'               "n599218
        EXPORTING                                           "n599218
          i_arbgb = 'M7'                         "n599218
          i_msgnr = '393'                        "n599218
        EXCEPTIONS                                          "n599218
          nothing = 0                            "n599218
          OTHERS  = 1.                           "n599218
                                                            "n599218
      IF sy-subrc <> 0.                                     "n599218
*       list will be created using the initial layout &     "n599218
        MESSAGE w393(m7)     WITH  ls_vari_def-variant.     "n599218
      ENDIF.                                                "n599218
    ENDIF.                                                  "n599218
  ENDIF.

ENDFORM.                     "VARIANT_CHECK_EXISTENCE

*----------------------------------------------------------------------*
*    get_the_default_VARIANT
*----------------------------------------------------------------------*

FORM get_the_default_variant
         USING     l_vari      LIKE  disvariant-variant
                   ls_vari     LIKE  disvariant
                   ls_vari_def LIKE  disvariant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = variant_save
    CHANGING
      cs_variant = ls_vari_def
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 0.
*   save the initial, e.g. default variant
    MOVE-CORRESPONDING ls_vari_def     TO  ls_vari.
    MOVE : ls_vari_def-variant         TO  l_vari.
  ENDIF.

ENDFORM.                     "VARIANT_VALUE_REQUEST_F4

*----------------------------------------------------------------------*
*    VARIANT_VALUE_REQUEST_F4
*----------------------------------------------------------------------*

FORM variant_value_request_f4
         USING     l_vari    LIKE  disvariant-variant
                   ls_vari   LIKE  disvariant.

  DATA : ls_vari_return      LIKE  disvariant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = ls_vari
      i_save     = 'A'
*     it_default_fieldcat =
    IMPORTING
      e_exit     = variant_exit
      es_variant = ls_vari_return
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF variant_exit = space.
      MOVE  ls_vari_return-variant   TO  l_vari.
    ENDIF.
  ENDIF.

ENDFORM.                     "VARIANT_VALUE_REQUEST_F4

*&---------------------------------------------------------------------*

*---------------------- bewerteter Bestand ----------------------------*

FORM aktuelle_bst_bwbst.

* define local working areas  / for the result of the       "n450764
* database selections and the control break                 "n450764
  DATA : l_t_mbew         TYPE  stab_mbew,                  "n450764
         l_s_mbew         TYPE  stype_mbew,                 "n450764
                                                            "n450764
         l_s_mbew_split   TYPE  stype_mbew,                 "n450764
         l_s_mbew_normal  TYPE  stype_mbew,                 "n450764
         l_flag_split(01) TYPE c.                           "n450764
                                                            "n450764
* build Valuation Class restriction table
  IF gv_switch_ehp6ru = abap_true.
    PERFORM build_bklas_selection.
  ENDIF.

* read the matching valuation entries                       "n450764
  PERFORM hdb_check_table USING 'MBEW' ''.                  "n1710850
  SELECT matnr bwkey bwtar lbkum salk3 bwtty  FROM mbew CONNECTION (dbcon) "n1710850
         INTO CORRESPONDING FIELDS OF TABLE l_t_mbew        "n450764
         WHERE  matnr  IN  matnr                            "n450764
           AND  bwkey  IN  g_ra_bwkey                       "n450764
           AND  bwtar  IN  bwtar                            "n450764
  AND  bklas  IN  ibklas.
                                                            "n450764
                                                            "n450764
* read the matching valuation records of the valuated       "n450764
* special stock sales order                                 "n450764
  PERFORM hdb_check_table USING 'EBEW' ''.                  "n1710850
  SELECT matnr bwkey bwtar bwtty                            "n1227439
         SUM( lbkum ) AS lbkum                              "n450764
         SUM( salk3 ) AS salk3        FROM  ebew CONNECTION (dbcon) "n1710850
         APPENDING CORRESPONDING FIELDS OF TABLE l_t_mbew   "n450764
         WHERE  matnr  IN  matnr                            "n450764
           AND  bwkey  IN  g_ra_bwkey                       "n450764
           AND  bwtar  IN  bwtar                            "n450764
           AND  bklas  IN  ibklas
  GROUP BY  matnr  bwkey bwtar bwtty.                       "n450764
                                                            "n450764
* read the matching valuation records of the valuated       "n450764
* special stock projects                                    "n450764
  PERFORM hdb_check_table USING 'QBEW' ''.                  "n1710850
  SELECT matnr bwkey bwtar bwtty                            "n1227439
         SUM( lbkum ) AS lbkum                              "n450764
         SUM( salk3 ) AS salk3        FROM  qbew CONNECTION (dbcon) "n1710850
         APPENDING CORRESPONDING FIELDS OF TABLE l_t_mbew   "n450764
         WHERE  matnr  IN  matnr                            "n450764
           AND  bwkey  IN  g_ra_bwkey                       "n450764
           AND  bwtar  IN  bwtar                            "n450764
           AND  bklas  IN  ibklas
  GROUP BY  matnr  bwkey bwtar bwtty.                       "n450764

* read the matching valuation records of the valuated       "n497992
* special subcontractor stock OBEW                          "n497992
  PERFORM hdb_check_table USING 'OBEW' ''.                  "n1710850
  SELECT matnr bwkey bwtar bwtty                            "n1227439
         SUM( lbkum ) AS lbkum                              "n497992
         SUM( salk3 ) AS salk3         FROM  obew CONNECTION (dbcon) "n1710850
         APPENDING CORRESPONDING FIELDS OF TABLE l_t_mbew   "n497992
         WHERE  matnr  IN  matnr                            "n497992
           AND  bwkey  IN  g_ra_bwkey                       "n497992
           AND  bwtar  IN  bwtar                            "n497992
           AND  bklas  IN  ibklas
  GROUP BY  matnr  bwkey bwtar bwtty.                       "n497992

  IF l_t_mbew[] IS INITIAL.                                 "n1560727
    MESSAGE s289.                                           "n1560727
*   Kein Material in Selektion vorhanden.                      "n1560727
    PERFORM anforderungsbild.                               "n1560727
  ENDIF.                                                    "n1560727

* create table g_t_organ if it is still empty
  IF  g_t_organ[] IS INITIAL.                               "n433765
*   create working table G_0000_RA_BWKEY with the valuation areas
    LOOP AT l_t_mbew         INTO  l_s_mbew.                "n450764
      ON CHANGE OF l_s_mbew-bwkey.                          "n450764
        MOVE : l_s_mbew-bwkey                               "n450764
                             TO  g_0000_ra_bwkey-low,       "n450764
               'I'           TO  g_0000_ra_bwkey-sign,      "n450764
               'EQ'          TO  g_0000_ra_bwkey-option.    "n450764
        COLLECT              g_0000_ra_bwkey.               "n450764
      ENDON.                                                "n450764
    ENDLOOP.

    PERFORM  f0000_create_table_g_t_organ
                             USING  c_no_error.
  ENDIF.

  SORT  l_t_mbew             BY  matnr  bwkey.              "n450764
                                                            "n450764
  LOOP AT l_t_mbew           INTO  l_s_mbew.                "n450764
*   check if MBEW record is a mother segment (splitval)     "n1227439
    IF  l_s_mbew-bwtar IS INITIAL                           "n1227439
        AND NOT l_s_mbew-bwtty IS INITIAL.                  "n1227439
      CLEAR l_s_mbew-lbkum.                                 "n1227439
      CLEAR l_s_mbew-salk3.                                 "n1227439
    ENDIF.                                                  "n1227439
*   process a single entry / add the stock and value        "n450764
    IF  l_s_mbew-bwtar IS INITIAL.                          "n450764
      MOVE : l_s_mbew-matnr  TO  l_s_mbew_normal-matnr,     "n450764
             l_s_mbew-bwkey  TO  l_s_mbew_normal-bwkey.     "n450764
      ADD :  l_s_mbew-lbkum  TO  l_s_mbew_normal-lbkum,     "n450764
             l_s_mbew-salk3  TO  l_s_mbew_normal-salk3.     "n450764
    ELSE.                                                   "n450764
*     material has split valuation                          "n450764
      MOVE : 'X'             TO  l_flag_split,              "n450764
             l_s_mbew-matnr  TO  l_s_mbew_split-matnr,      "n450764
             l_s_mbew-bwkey  TO  l_s_mbew_split-bwkey.      "n450764
      ADD :  l_s_mbew-lbkum  TO  l_s_mbew_split-lbkum,      "n450764
             l_s_mbew-salk3  TO  l_s_mbew_split-salk3.      "n450764
    ENDIF.                                                  "n450764
                                                            "n450764
*   control break after material and valuation area         "n450764
    AT END OF bwkey.                                        "n450764
*     create a entry for the next working table             "n450764
      IF  l_flag_split = 'X'.                               "n450764
*       if the material has split valuation, take only      "n450764
*       the sums from the entries with valuation type       "n450764
        MOVE-CORRESPONDING  l_s_mbew_split  TO  g_s_mbew.   "n450764
      ELSE.                                                 "n450764
        MOVE-CORRESPONDING  l_s_mbew_normal TO  g_s_mbew.   "n450764
      ENDIF.                                                "n450764
                                                            "n450764
*     check the authority                                   "n450764
      PERFORM  f9300_read_organ                             "n450764
                   USING     c_bwkey   g_s_mbew-bwkey.      "n450764
                                                            "n450764
      IF sy-subrc IS INITIAL.                               "n450764
*       enrich the entries with the field currency key      "n450764
        MOVE g_s_organ-waers TO  g_s_mbew-waers.            "n450764
        APPEND  g_s_mbew     TO  g_t_mbew.                  "n450764
                                                            "n450764
*       create the key table for the material texts         "n450764
        PERFORM  f9400_material_key                         "n450764
                             USING  g_s_mbew-matnr.         "n450764
      ENDIF.                                                "n450764
                                                            "n450764
*     clear the working areas for the next group            "n450764
      CLEAR : l_flag_split, l_s_mbew_normal, l_s_mbew_split. "n450764
    ENDAT.                                                  "n450764
  ENDLOOP.                                                  "n450764

* no entries left in table g_t_mbew ?
  IF  g_t_mbew[] IS INITIAL.                                "n450764
    MESSAGE s289.
*     Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

ENDFORM.                     "aktuelle_bst_bwbst

*&---------------------------------------------------------------------*
*&      Form  BEWEGUNGSARTEN_LESEN
*&---------------------------------------------------------------------*
*       Lesen der Tabellen zur Bewegungsart                            *
*----------------------------------------------------------------------*

FORM  bewegungsarten_lesen.

  DATA: BEGIN OF k2 OCCURS 0,
          bwart LIKE t156s-bwart,
        END OF k2.
  REFRESH k2.

* select the movement types from the selected documents
  LOOP AT g_t_mseg_lean      INTO  g_s_mseg_lean.
    MOVE  g_s_mseg_lean-bwart          TO  k2-bwart.
    COLLECT                            k2.
  ENDLOOP.

* Read data for movement type from new tables
* T156SY/C/Q with function module from release >=46B
  DATA: t_st156s         LIKE st156s OCCURS 0
  WITH HEADER LINE.

  REFRESH it156.
  LOOP AT k2.
    REFRESH t_st156s.

    CALL FUNCTION 'MB_CONTROL_MOVETYPE_GET'
      EXPORTING
        i_bwart              = k2-bwart
      TABLES
        te_st156s_tab        = t_st156s
      EXCEPTIONS
        inconsistent_entries = 1
        no_entries_found     = 2
        OTHERS               = 3.

    IF sy-subrc = 0.                                        "311588
      LOOP AT t_st156s.
        MOVE-CORRESPONDING t_st156s TO it156.
        APPEND it156.
      ENDLOOP.
    ENDIF.
  ENDLOOP.

  SORT it156 BY bwart wertu mengu sobkz kzbew kzzug kzvbr.

  SELECT * FROM t156m INTO CORRESPONDING FIELDS OF TABLE it156x
  FOR ALL ENTRIES IN it156  WHERE bustm EQ it156-bustm.

  LOOP AT it156.
    CLEAR it156-lbbsa.
    READ TABLE it156x WITH KEY bustm = it156-bustm.
    it156-lbbsa = it156x-lbbsa.
    MODIFY it156.
  ENDLOOP.

  DATA: rc TYPE i.                                          "147374
  LOOP AT g_t_mseg_lean      INTO  g_s_mseg_lean.
*   find and delete reversal movements / only in releases >= 45B
    IF NOT nosto IS INITIAL AND
       NOT ( g_s_mseg_lean-smbln IS INITIAL OR
             g_s_mseg_lean-smblp IS INITIAL ).
      MOVE-CORRESPONDING  g_s_mseg_lean
                             TO  storno.

      APPEND storno.
      DELETE                 g_t_mseg_lean.
      CONTINUE.
    ENDIF.

    READ TABLE it156 WITH KEY bwart = g_s_mseg_lean-bwart
                              wertu = g_s_mseg_lean-wertu
                              mengu = g_s_mseg_lean-mengu
                              sobkz = g_s_mseg_lean-sobkz
                              kzbew = g_s_mseg_lean-kzbew
                              kzzug = g_s_mseg_lean-kzzug
                              kzvbr = g_s_mseg_lean-kzvbr
                             BINARY SEARCH.

    rc = sy-subrc.                                          "147374
    IF  g_s_mseg_lean-bustm = space AND
        g_s_mseg_lean-bustw = space AND
        rc                  = 0.                            "147374
      MOVE : it156-bustm     TO  g_s_mseg_lean-bustm,       "147374
             it156-bustw     TO  g_s_mseg_lean-bustw.       "147374
    ENDIF.

    IF rc = 0.                                              "147374
      MOVE : it156-lbbsa     TO  g_s_mseg_lean-lbbsa.

      IF NOT it156-bwagr IS INITIAL.
        MOVE : it156-bwagr   TO  g_s_mseg_lean-bwagr.
      ELSE.
        MOVE : 'REST'        TO  g_s_mseg_lean-bwagr.
      ENDIF.
    ELSE.
      MOVE : 'REST'          TO  g_s_mseg_lean-bwagr.
    ENDIF.                                                  "147374

    MODIFY  g_t_mseg_lean    FROM  g_s_mseg_lean.
  ENDLOOP.

ENDFORM.                    " BEWEGUNGSARTEN_LESEN

*&---------------------------------------------------------------------*
*&      Form  SUMMEN_BILDEN
*&---------------------------------------------------------------------*
*       Bestandssummen zur Berechnung der Bestände                     *
*       zu 'datum-low' und 'datum-high'                                *
*----------------------------------------------------------------------*
FORM summen_bilden.
* Some explanatory words on the strategy of material
* counting/valuation:
* ======================================================
* 1) Stock overview (no valuation):
*    The material document is accepted, if is has not been created
*    automatically or if it is not related to movements out of
*    the stock. For example, if a stock transfer is posted, the
*    system creates a material document with two lines: Out of
*    the old stock (accepted) and into the transfer stock (rejected,
*    because the material is not yet visible in the target location).
*    When the movement into the stock is posted, this is accepted.
* 2) Valuated stock:
*    a) Movements within a single plant (MA05, MA06 =
*       movement types 313-316) are ignored.
*    b) The moving of material out of a plant (303/304)
*       is counted and valuated in the emitting plant and
*       the target plant. The moving in
*       (305/306) is ignored, because
*       the valuated stock appears in the target at the
*       very moment of leaving the emitter.
*    c) Material documents without valuation string are ignored.
*------------- Summen von 'datum-high' bis Gegenwart ------------------*
* Performance Optimization:                                 "1784986
* Form is called from FORM create_table_for_detail!         "1784986
  IF gv_newdb = abap_true.                                  "1784986
    DELETE g_t_mseg_lean WHERE                              "1784986
             ( xauto IS NOT INITIAL ) AND                   "1784986
             ( bustm = 'MA02' OR                            "1784986
               bustm = 'MA05' OR                            "1784986
               bustm = 'MAUO' OR                            "1784986
               bustm = 'MA0L' OR                            "1784986
               bustm = 'MAVA' ).                            "1810543
    RETURN.                                                 "1784986
  ENDIF.                                                    "1784986
  IF NOT index_2 IS INITIAL.
    IF bwbst = ' '.
      IF xchar = ' '.
        SORT imsweg BY werks matnr shkzg.          "auf Materialebene
        LOOP AT imsweg.
*          ENHANCEMENT-SECTION rm07mlbd_20 SPOTS es_rm07mlbd .

          "{ Begin ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }
          IF ( imsweg-xauto IS INITIAL ) OR
             ( imsweg-bustm <> 'MA02' AND
               imsweg-bustm <> 'MA05' AND
               imsweg-bustm <> 'MAUO' AND                   "1767021
               imsweg-bustm <> 'MA0L' AND                   "1767021
               imsweg-bustm <> 'MAVA' AND                   "1835358
               imsweg-bustm <> 'MXCB' AND           "SH note 1728394
               imsweg-bustm <> 'MXCE' AND           "SH note 1728394
               imsweg-bustm <> 'MXAK' AND           "SH note 1728394
               imsweg-bustm <> 'MXCQ' AND           "SH note 1728394
               imsweg-bustm <> '' ).
            MOVE-CORRESPONDING imsweg TO weg_mat.
            COLLECT weg_mat.
          ELSE.
            DELETE imsweg.
          ENDIF.
          "{ End ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }

*          END-ENHANCEMENT-SECTION.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        SORT imsweg BY werks matnr charg shkzg.    "auf Chargenebene
        LOOP AT imsweg.
*          ENHANCEMENT-SECTION rm07mlbd_21 SPOTS es_rm07mlbd .

          "{ Begin ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }
          IF ( imsweg-xauto IS INITIAL ) OR
             ( imsweg-bustm <> 'MA02' AND
               imsweg-bustm <> 'MA05' AND
               imsweg-bustm <> 'MXCB' AND           "SH note 1728394
               imsweg-bustm <> 'MXCE' AND           "SH note 1728394
               imsweg-bustm <> 'MXAK' AND           "SH note 1728394
               imsweg-bustm <> 'MXCQ' AND           "SH note 1728394
               imsweg-bustm <> 'MAUO' AND                   "1767021
               imsweg-bustm <> 'MA0L' AND                   "1767021
               imsweg-bustm <> 'MAVA'   ).                  "1835358
            MOVE-CORRESPONDING imsweg TO weg_char.
            COLLECT weg_char.
          ELSE.
            DELETE imsweg.
          ENDIF.
          "{ End ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }

*          END-ENHANCEMENT-SECTION.
        ENDLOOP.
      ENDIF.

    ELSEIF bwbst = 'X'.
      SORT imsweg BY werks matnr shkzg.
      LOOP AT imsweg.
*       consider special gain/loss-handling of IS-OIL       "n497992

**# IF EXIST OI001
**" IF ( imsweg-bustm <> 'MEU1' )    OR                 "n497992
**"   ( imsweg-bustm = 'MEU1'                           "n497992
**"   AND not imsweg-OIGLCALC IS INITIAL                "n497992
**"   AND not imsweg-OIGLSKU IS INITIAL ).              "n497992
**"          MOVE-CORRESPONDING imsweg TO mat_weg.      "n497992
**"          COLLECT mat_weg.                           "n497992
**" ELSE.                                               "n497992
**"          DELETE             imsweg.                 "n497992
**" ENDIF .                                             "n497992
**# ELSE
*     MOVE-CORRESPONDING imsweg TO mat_weg.             "n497992
*     COLLECT mat_weg.                                  "n497992
**# ENDIF
*       IS-OIL specific functions without ABAP preprocessor "n599218 A
        IF  g_flag_is_oil_active = 'X'.     "IS-OIL ?       "n599218 A
          IF ( imsweg-bustm <> 'MEU1' )    OR               "n599218 A
             ( imsweg-bustm = 'MEU1'                        "n599218 A
               AND NOT imsweg-oiglcalc IS INITIAL           "n599218 A
               AND NOT imsweg-oiglsku IS INITIAL ).         "n599218 A
            MOVE-CORRESPONDING imsweg TO mat_weg.           "n599218 A
            COLLECT mat_weg.                                "n599218 A
          ELSE.                                             "n599218 A
            DELETE           imsweg.                        "n599218 A
          ENDIF.                                            "n599218 A
        ELSE.                                               "n599218 A
          MOVE-CORRESPONDING imsweg TO mat_weg.             "n599218 A
          COLLECT mat_weg.                                  "n599218 A
        ENDIF.                                              "n599218 A

      ENDLOOP.

      LOOP AT mat_weg.
        IF curm = '1'.
          mat_weg-bwkey = mat_weg-werks.
        ELSEIF curm = '3'.
*
*         look for the corresponding valuation area
*         READ TABLE organ WITH KEY werks = mat_weg-werks.
*         mat_weg-bwkey = organ-bwkey.
          PERFORM  f9300_read_organ
                   USING     c_werks   mat_weg-werks.

          MOVE : g_s_organ-bwkey   TO  mat_weg-bwkey.
        ENDIF.
        MODIFY mat_weg.
      ENDLOOP.
      IF curm = '3'.
        SORT mat_weg BY bwkey matnr shkzg.
        LOOP AT mat_weg.
          MOVE-CORRESPONDING mat_weg TO mat_weg_buk.
          COLLECT mat_weg_buk.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

*------------- Summen von 'datum-low' bis 'datum-high' ----------------*
  IF bwbst = ' '.
    IF xchar = ' '.                    "auf Materialebene

      SORT  g_t_mseg_lean    BY werks matnr shkzg DESCENDING.

      LOOP AT g_t_mseg_lean  INTO  g_s_mseg_lean.
*        ENHANCEMENT-SECTION rm07mlbd_22 SPOTS es_rm07mlbd .

        "{ Begin ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }
        IF ( g_s_mseg_lean-xauto IS INITIAL ) OR
           ( g_s_mseg_lean-bustm <> 'MA02' AND
             g_s_mseg_lean-bustm <> 'MA05' AND
             g_s_mseg_lean-bustm <> 'MXCB' AND           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXCE' AND           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXAK' AND           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXCQ' AND           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MAUO' AND              "1767021
             g_s_mseg_lean-bustm <> 'MA0L' AND              "1767021
             g_s_mseg_lean-bustm <> 'MAVA'   ).             "1835358
          MOVE-CORRESPONDING g_s_mseg_lean   TO  sum_mat.
          COLLECT            sum_mat.
        ELSE.
          DELETE             g_t_mseg_lean.
        ENDIF.
        "{ End ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }

*        END-ENHANCEMENT-SECTION.
      ENDLOOP.

    ELSEIF xchar = 'X'.                "auf Chargenebene
      SORT  g_t_mseg_lean    BY werks matnr charg shkzg DESCENDING.

      LOOP AT g_t_mseg_lean  INTO  g_s_mseg_lean.
*        ENHANCEMENT-SECTION rm07mlbd_23 SPOTS es_rm07mlbd .

        "{ Begin ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }
        IF ( g_s_mseg_lean-xauto IS INITIAL ) OR
           ( g_s_mseg_lean-bustm <> 'MA02' AND
             g_s_mseg_lean-bustm <> 'MA05' AND
             g_s_mseg_lean-bustm <> 'MXCB' AND           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXCE' AND           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXAK' AND           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MXCQ' AND           "SH note 1728394
             g_s_mseg_lean-bustm <> 'MAUO' AND              "1767021
             g_s_mseg_lean-bustm <> 'MA0L' AND              "1767021
             g_s_mseg_lean-bustm <> 'MAVA'   ).             "1835358
          MOVE-CORRESPONDING  g_s_mseg_lean
                             TO  sum_char.
          COLLECT            sum_char.
        ELSE.
          DELETE             g_t_mseg_lean.
        ENDIF.
        "{ End ENHO DIPCS_RM07MLBD001 IS-AD-SSP AD_SUB }

*        END-ENHANCEMENT-SECTION.
      ENDLOOP.
    ENDIF.

  ELSEIF bwbst = 'X'.
    SORT  g_t_mseg_lean      BY werks matnr shkzg DESCENDING.
    LOOP AT g_t_mseg_lean    INTO  g_s_mseg_lean.
*       consider special gain/loss-handling of IS-OIL       "n497992

**# IF EXIST OI001
**"    IF ( G_S_MSEG_LEAN-bustm <> 'MEU1' )    OR       "n497992
**"       ( G_S_MSEG_LEAN-bustm = 'MEU1'                "n497992
**"       AND not G_S_MSEG_LEAN-OIGLCALC IS INITIAL     "n497992
**"       AND not G_S_MSEG_LEAN-OIGLSKU IS INITIAL ).   "n497992
**# ENDIF
*      MOVE-CORRESPONDING  G_S_MSEG_LEAN
*                             TO  MAT_SUM.
*      COLLECT                MAT_SUM.
**# IF EXIST OI001
**"    ELSE.                                            "n497992
**"      DELETE               G_T_MSEG_LEAN.            "n497992
**"    ENDIF.                                           "n497992
**# ENDIF
*     IS-OIL specific functions without ABAP preprocessor   "n599218 A
      IF  g_flag_is_oil_active = 'X'.       "IS-OIL ?       "n599218 A
        IF ( g_s_mseg_lean-bustm <> 'MEU1' )    OR          "n599218 A
           ( g_s_mseg_lean-bustm = 'MEU1'                   "n599218 A
           AND NOT g_s_mseg_lean-oiglcalc IS INITIAL        "n599218 A
           AND NOT g_s_mseg_lean-oiglsku IS INITIAL ).      "n599218 A
          MOVE-CORRESPONDING  g_s_mseg_lean                 "n599218 A
                             TO  mat_sum.                   "n599218 A
          COLLECT            mat_sum.                       "n599218 A
        ELSE.                                               "n599218 A
          DELETE             g_t_mseg_lean.                 "n599218 A
        ENDIF.                                              "n599218 A
      ELSE.                                                 "n599218 A
        MOVE-CORRESPONDING  g_s_mseg_lean                   "n599218 A
                             TO  mat_sum.                   "n599218 A
        COLLECT              mat_sum.                       "n599218 A
      ENDIF.                                                "n599218 A
    ENDLOOP.

    LOOP AT mat_sum.
      IF curm = '1'.
        mat_sum-bwkey = mat_sum-werks.
      ELSEIF curm = '3'.
        PERFORM  f9300_read_organ
                   USING     c_werks   mat_sum-werks.

        MOVE : g_s_organ-bwkey     TO  mat_sum-bwkey.
      ENDIF.
      MODIFY mat_sum.
    ENDLOOP.

    IF curm = '3'.            "Materialbelege auf Buchungskreisebene
      SORT mat_sum BY bwkey matnr shkzg DESCENDING.
      LOOP AT mat_sum.
        MOVE-CORRESPONDING mat_sum TO mat_sum_buk.
        COLLECT mat_sum_buk.
      ENDLOOP.
    ENDIF.
  ENDIF.

ENDFORM.                               " SUMMEN_BILDEN

*&---------------------------------------------------------------------*
*&      Form  BELEGSELEKTION
*&---------------------------------------------------------------------*

FORM belegselektion.

* does the user wants the valuated stocks ?
  IF bwbst = 'X'.
*   delete the MM-documents without values
    PERFORM unbewertet_weg.

    IF g_t_mbew[] IS INITIAL.                               "n450764
      MESSAGE s289.
*     Kein Material in Selektion vorhanden.
      PERFORM anforderungsbild.
    ENDIF.

*   select the corresponding FI-documents
    PERFORM                  fi_belege_lesen.
  ENDIF.

  IF sbbst IS INITIAL.
    PERFORM                  kontiert_aussortieren.
  ELSE.                                                     "2120566
    PERFORM                  wesperr_aussortieren.          "2120566
  ENDIF.

  PERFORM                    bewegungsarten_lesen.

* does the user want no reversal movements ? only in releases >=45B
  IF NOT nosto IS INITIAL.
    PERFORM                  storno.
  ENDIF.

* does the user wants the valuated stocks ?
  IF bwbst = 'X'.
    PERFORM                  belege_ergaenzen.
  ENDIF.

  PERFORM                    belege_sortieren.

ENDFORM.                     "BELEGSELEKTION

*&--------------------------------------------------------------------*
*&   FELDGRUPPEN_AUFBAUEN
*&--------------------------------------------------------------------*
*& create table GRUPPEN with the parameter for special groups         *
*&--------------------------------------------------------------------*

FORM feldgruppen_aufbauen.

  gruppen-sp_group = 'H'.
  gruppen-text = TEXT-138.  "Cabeçalho
  APPEND gruppen.

* Gruppendefinitionen Positionsfelder
  gruppen-sp_group = 'M'.
  gruppen-text = TEXT-050.
  APPEND gruppen.
  gruppen-sp_group = 'B'.
  gruppen-text = TEXT-051.
  APPEND gruppen.
  gruppen-sp_group = 'D'.
  gruppen-text = TEXT-052.
  APPEND gruppen.
  gruppen-sp_group = 'O'.
  gruppen-text = TEXT-053.
  APPEND gruppen.
  gruppen-sp_group = 'K'.
  gruppen-text = TEXT-054.
  APPEND gruppen.
  gruppen-sp_group = 'E'.
  gruppen-text = TEXT-055.
  APPEND gruppen.
  gruppen-sp_group = 'V'.
  gruppen-text = TEXT-056.
  APPEND gruppen.
  gruppen-sp_group = 'F'.
  gruppen-text = TEXT-057.
  APPEND gruppen.
  gruppen-sp_group = 'S'.
  gruppen-text = TEXT-058.
  APPEND gruppen.
  layout-group_buttons = ' '.

ENDFORM.                               " FELDGRUPPEN_AUFBAUEN.

*&---------------------------------------------------------------------*
*&      Form  UEBERSCHRIFT
*&---------------------------------------------------------------------*

FORM ueberschrift.                                          "#EC CALLED

  MOVE-CORRESPONDING  bestand
                             TO  g_s_bestand.
  PERFORM                    top_of_page_render.

ENDFORM.                               " UEBERSCHRIFT

*&---------------------------------------------------------------------*
*&      Form  UEBERSCHRIFT1
*&---------------------------------------------------------------------*

FORM ueberschrift1.                                         "#EC CALLED

  MOVE-CORRESPONDING  bestand1
                             TO  g_s_bestand.
  PERFORM                    top_of_page_render.

ENDFORM.                               " UEBERSCHRIFT1

*&---------------------------------------------------------------------*
*&      Form  UEBERSCHRIFT_DETAIL
*&---------------------------------------------------------------------*

FORM ueberschrift_detail.                                   "#EC CALLED

  MOVE-CORRESPONDING  g_s_bestand_detail
                             TO  g_s_bestand.

  PERFORM                    top_of_page_render.

ENDFORM.                               " UEBERSCHRIFT_DETAIL

*&---------------------------------------------------------------------*
*&      Form  STORNO
*&---------------------------------------------------------------------*
*       Stornobewegungen vernachlässigen
*----------------------------------------------------------------------*

* delete the reversal movements from the working
* table with the documents / only in releases >=45B
FORM storno.

  LOOP AT storno.
    DELETE g_t_mseg_lean
             WHERE mblnr = storno-smbln                     "204463
               AND mjahr = storno-sjahr                     "204463
               AND zeile = storno-smblp.                    "204463
  ENDLOOP.

ENDFORM.                   " STORNO

*----------------------------------------------------------------------*
* F0400_CREATE_FIELDCAT
*----------------------------------------------------------------------*
*
* create field catalog for the ALV
* take only the field of structure MSEG_LEAN who are in working
* table g_f_mseg_fields

* --> input    name of ALV input data table
* <-- output   table wilh the field catalog
*
*----------------------------------------------------------------------*

FORM f0400_create_fieldcat.
  DEFINE take_field.
    CLEAR g_s_fieldcat.
    g_s_fieldcat = VALUE #(
      fieldname   = &2
      ref_tabname = &3
      sp_group    = &4
      qfieldname  = &5
      cfieldname  = &6 ).
    PERFORM  f0410_fieldcat USING  c_take   &1. "c_out.
  END-OF-DEFINITION.

  "Campos visiveis
  take_field c_out 'ZEILE' 'MSEG' 'O' '' ''. "Item in material document
  take_field c_out 'MBLNR' 'MKPF' 'O' '' ''. "Number of material document
  take_field c_out 'BUDAT' 'MKPF' 'D' '' ''. "Posting date in the document
  take_field c_out 'WERKS' 'MSEG' 'H' '' ''. "Centro
  IF NOT ( sobkz = 'O' OR sobkz = 'V' OR sobkz = 'W' ).
    take_field c_out 'LGORT' 'MSEG' 'O' '' ''. "Storage location
  ENDIF.
  take_field c_out 'MATNR' 'MSEG' 'H' '' ''. "Numero do material
  take_field c_out 'MAKTX' 'MAKT' 'H' '' ''. "Texto breve de material
  take_field c_out 'MENGE' 'MSEG' 'M' 'MEINS' ''. "Quantity
  take_field c_out 'MEINS' 'MSEG' 'M' '' ''. "Base unit of measure
  IF bwbst IS NOT INITIAL.
    take_field c_out 'DMBTR' 'BSIM' 'M' '' 'WAERS'. "Amount in local currency
  ENDIF.
  take_field c_out 'NAME1' 'T001W' 'H' '' ''. "Nome 1
  take_field c_out 'BWART' 'MSEG' 'M' '' ''. "Movement type
  take_field c_out 'BTEXT' 'T156T' 'M' '' ''. "Txt tipo de movimento
  take_field c_out 'SOBKZ' 'MSEG' 'B' '' ''. "Special stock indicator
  take_field c_out 'EBELN' 'MSEG' 'E' '' ''. "Purchase order number
  take_field c_out 'LIFNR' 'MSEG' 'E' '' ''. "vendor's account number
  take_field c_out 'AUFNR' 'MSEG' 'K' '' ''. "Order Number
  take_field c_out 'RSNUM' 'MSEG' 'B' '' ''. "Number of reservation/dependent requirements
  take_field c_out 'KUNNR' 'MSEG' 'V' '' ''. "account number of customer
  take_field c_out 'KOSTL' 'MSEG' 'K' '' ''. "Cost Center
  take_field c_out 'PRCTR' 'MSEG' 'O' '' ''. "Profit Center
  IF bwbst = 'X'.
    take_field c_out 'BELNR' 'BSIM' 'O' '' ''. "Accounting document number
  ENDIF.
  take_field c_out 'CPUDT' 'MKPF' 'D' '' ''. "Acc. doc. entry date
  take_field c_out 'CPUTM' 'MKPF' 'D' '' ''. "Time of entry
  take_field c_out 'USNAM' 'MKPF' 'O' '' ''. "User name

  "Campos ocultos
  take_field c_no_out 'MTART' 'MARA' 'H' '' ''. "Tipo do material
  take_field c_no_out 'XAUTO' 'MSEG' 'H' '' ''. "Item gerado automaticamente
  take_field c_no_out 'APLZL' 'MSEG' 'H' '' ''. "Contador
  take_field c_no_out 'XMACC' 'MSEG' 'H' '' ''. "Classificação contábil múltipla
  take_field c_no_out 'XBLNR' 'MKPF' 'H' '' ''. "Nota Fiscal
  take_field c_no_out 'BWKEY' 'MBEW' 'H' '' ''. "Área de avaliação

  CLEAR                      g_s_fieldcat.

  "note 201670
*check if segmentation switch is active
  IF cl_ops_switch_check=>sfsw_segmentation_02( ) EQ abap_on.
    g_s_fieldcat-fieldname     = 'SGT_SCAT'.     " Basismengeneinheit
    g_s_fieldcat-ref_tabname   = 'MSEG'.      " Base unit of measure
    g_s_fieldcat-sp_group      = 'M'.
    PERFORM  f0410_fieldcat    USING  c_take   c_no_out.
  ENDIF.
* g_s_fieldcat-fieldname     = 'WAERS'.     " Waehrungs-schluessel
* g_s_fieldcat-ref_tabname   = 'T001'.      " Currency Key
* g_s_fieldcat-sp_group      = 'M'.
* perform  f0410_fieldcat    using  c_take   c_out.

* the following fields are always in g_s_mseg_lean, but they are hidden in the list
  g_s_fieldcat = VALUE #( fieldname = 'MJAHR' ref_tabname = 'MKPF' sp_group = 'D' ).    "Material doc. year
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'GJAHR' ref_tabname = 'BKPF' sp_group = 'D' ).    "Fiscal Year
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'VGART' ref_tabname = 'MKPF' sp_group = 'M' ).    "Transaction/event type
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'SHKZG' ref_tabname = 'MSEG' sp_group = 'M' ).    "Debit/credit indicator
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'BWTAR' ref_tabname = 'MSEG' sp_group = 'B' ).    "Valuation type
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'KZBWS' ref_tabname = 'MSEG' sp_group = 'B' ).    "Indicator: valuation of special stock
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'CHARG' ref_tabname = 'MSEG' sp_group = 'B' ).    "Batch number
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'BUKRS' ref_tabname = 'T001' sp_group = 'O' ).    "Company code
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  IF gv_switch_ehp6ru = abap_true AND bwbst = 'X'.
    g_s_fieldcat = VALUE #( fieldname = 'HKONT' ref_tabname = 'BSEG' sp_group = 'O' ).  "G/L account
    PERFORM  f0410_fieldcat    USING  c_take   c_no_out.
  ENDIF.

  g_s_fieldcat = VALUE #( fieldname = 'KZBEW' ref_tabname = 'MSEG' sp_group = 'M' ).    "Movement indicator
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'KZVBR' ref_tabname = 'MSEG' sp_group = 'M' ).    "Consumption posting
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'KZZUG' ref_tabname = 'MSEG' sp_group = 'M' ).    "Receipt indicator
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'BUSTM' ref_tabname = 'MSEG' sp_group = 'B' ).    "Posting string for quantities
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'BUSTW' ref_tabname = 'MSEG' sp_group = 'B' ).    "Posting string for values
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'MENGU' ref_tabname = 'MSEG' sp_group = 'B' ).    "Quantity Updating in Material Master Record
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'WERTU' ref_tabname = 'MSEG' sp_group = 'B' ).    "Value Updating in Material Master Record
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'BWAGR' ref_tabname = 'T156Q' sp_group = 'M' ).    "Movement type group for stock analysis
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.

  g_s_fieldcat = VALUE #( fieldname = 'XABLN' ref_tabname = 'MKPF' sp_group = 'S' ).    "process 'goods receipt/issue slip'
  PERFORM  f0410_fieldcat    USING  c_take   c_no_out.      "n450596

* the following fields will be processed if they are in working table
* g_t_mseg_fields         Customer Exit :
* these fields can be activated in include RM07MLBD_CUST_FIELDS

  g_s_fieldcat-fieldname     = 'INSMK'.    " Bestandsart
  g_s_fieldcat-ref_tabname   = 'MSEG'.     " stock type
  g_s_fieldcat-sp_group      = 'B'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* process the sales order number and sales order item  "n599218
* depending on the release                             "n599218
* release          field                               "n599218
* 4.5B and higher  MAT_KDAUF, MAT_KDPOS                "n599218
* 4.0B             KDAUF,     KDPOS                    "n599218
*                                                      "n599218
  g_s_fieldcat-fieldname   = 'MAT_KDAUF'.                   "n599218
  g_s_fieldcat-ref_tabname = 'MSEG'.                        "n599218
  g_s_fieldcat-sp_group    = 'V'.                           "n599218
  PERFORM  f0410_fieldcat  USING  c_check  c_no_out.        "n599218
                                                            "n599218
  g_s_fieldcat-fieldname   = 'MAT_KDPOS'.                   "n599218
  g_s_fieldcat-ref_tabname = 'MSEG'.                        "n599218
  g_s_fieldcat-sp_group    = 'V'.                           "n599218
  PERFORM  f0410_fieldcat  USING  c_check  c_no_out.        "n599218

  g_s_fieldcat-fieldname     = 'KDAUF'.     " Kundenauftragsnummer
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Sales Order Number
  g_s_fieldcat-sp_group      = 'V'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'KDPOS'.    " Positionsnummer
  g_s_fieldcat-ref_tabname   = 'MSEG'.     " Item number in Sales Order
  g_s_fieldcat-sp_group      = 'V'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Einteilung Kundenauftrag   Delivery schedule for sales order
  g_s_fieldcat-fieldname     = 'KDEIN'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'F'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Menge in Erfassungsmengeneinheit   Quantity in unit of entry
  g_s_fieldcat-fieldname     = 'ERFMG'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-qfieldname    = 'ERFME'.
  g_s_fieldcat-sp_group      = 'M'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'ERFME'.     " Erfassungsmengeneinheit
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Unit of entry
  g_s_fieldcat-sp_group      = 'M'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Menge in Bestellpreismengeneinheit
* Quantity in purchase order price unit
  g_s_fieldcat-fieldname     = 'BPMNG'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-qfieldname    = 'BPRME'.
  g_s_fieldcat-sp_group      = 'E'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'BPRME'.     " Bestellpreismengeneinheit
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Order price unit
  g_s_fieldcat-sp_group      = 'E'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Positionsnummer des Einkaufsbelegs
* Item Number of Purchasing Document
  g_s_fieldcat-fieldname     = 'EBELP'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'E'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'SJAHR'.     " Materialbelegjahr
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Material doc. year
  g_s_fieldcat-sp_group      = 'D'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'SMBLN'.     " Nummer des Materialbelegs
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Number of material doc.
  g_s_fieldcat-sp_group      = 'O'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'SMBLP'.     " Position im Materialbeleg
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Item in material document
  g_s_fieldcat-sp_group      = 'O'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'ELIKZ'.  " Endlieferungskennzeichen
  g_s_fieldcat-ref_tabname   = 'MSEG'.   "Delivery completed" indicator
  g_s_fieldcat-sp_group      = 'E'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'SGTXT'.     " Positionstext
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Item Text
  g_s_fieldcat-sp_group      = 'E'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'WEMPF'.     " Warenempfänger
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Goods recipient
  g_s_fieldcat-sp_group      = 'V'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'ABLAD'.     " Abladestelle
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Unloading point
  g_s_fieldcat-sp_group      = 'V'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'GSBER'.     " Geschäftsbereich
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Business Area
  g_s_fieldcat-sp_group      = 'O'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Geschäftsbereich des Geschäftspartners
* Trading partner's business area
  g_s_fieldcat-fieldname     = 'PARGB'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'O'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'PARBU'.   " Verrechnender Buchungskreis
  g_s_fieldcat-ref_tabname   = 'MSEG'.    " Clearing company code
  g_s_fieldcat-sp_group      = 'O'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'ANLN1'.     " Anlagen-Hauptnummer
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Main asset number
  g_s_fieldcat-sp_group      = 'K'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Positionsnummer der Reservierung / des Sekundärbedarfs
* Item number of reservation/dependent requirements
  g_s_fieldcat-fieldname     = 'RSPOS'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Final issue for this reservation
  g_s_fieldcat-fieldname     = 'KZEAR'.    " Kennzeichen: Endausfassung
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* empfangendes/abgebendes Material
* Receiving/issuing material
  g_s_fieldcat-fieldname     = 'UMMAT'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Empfangendes/Abgebendes Werk
* Receiving plant/issuing plant
  g_s_fieldcat-fieldname     = 'UMWRK'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Empfangender/Abgebender Lagerort
* Receiving/issuing storage location
  g_s_fieldcat-fieldname     = 'UMLGO'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'UMCHA'.  " Empfangende/Abgebende Charge
  g_s_fieldcat-ref_tabname   = 'MSEG'.   " Receiving/issuing batch
  g_s_fieldcat-sp_group      = 'M'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Empfangende/Abgebende Bwertungsart
* Valuation type of transfer batch
  g_s_fieldcat-fieldname     = 'UMBAR'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Sonderbestandskennzeichen der Umlagerung
* Special stock indicator for physical stock transfer
  g_s_fieldcat-fieldname     = 'UMSOK'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Kennzeichen Wareneingang unbewertet
* Goods receipt, non-valuated
  g_s_fieldcat-fieldname     = 'WEUNB'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Kennzeichen: Grund der Bewegung
* Reason for movement
  g_s_fieldcat-fieldname     = 'GRUND'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'KSTRG'.    " Kostenträger
  g_s_fieldcat-ref_tabname   = 'MSEG'.  " Cost Object
  g_s_fieldcat-sp_group      = 'K'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Nummer für Ergebnisobjekte (CO-PA)
* Profitability segment number (CO-PA)
  g_s_fieldcat-fieldname     = 'PAOBJNR'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'K'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Projektstrukturplanelement (PSP-Element)
* Work breakdown structure element (WBS element)

* process the WBS element depends on the release       "n599218
* release          field                               "n599218
* 4.5B and higher  MAT_PSPNR                           "n599218
* 4.0B             PS_PSP_PNR                          "n599218
*                                                      "n599218
  g_s_fieldcat-fieldname   = 'MAT_PSPNR'.                   "n599218
  g_s_fieldcat-ref_tabname = 'MSEG'.                        "n599218
  g_s_fieldcat-sp_group    = 'K'.                           "n599218
  PERFORM  f0410_fieldcat  USING  c_check  c_no_out.        "n599218

  g_s_fieldcat-fieldname     = 'PS_PSP_PNR'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'K'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Netzplannummer für Kontierung
* Network Number for Account Assignment
  g_s_fieldcat-fieldname     = 'NPLNR'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'K'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Plannummer zu Vorgängen im Auftrag
* Routing number for operations in the order
  g_s_fieldcat-fieldname     = 'AUFPL'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'K'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'AUFPS'.   " Nummer der Auftragsposition
  g_s_fieldcat-ref_tabname   = 'MSEG'.    " Order item number
  g_s_fieldcat-sp_group      = 'K'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Wareneingangsmenge in Bestellmengeneinheit
* Goods receipt quantity in order unit
  g_s_fieldcat-fieldname     = 'BSTMG'.
  g_s_fieldcat-qfieldname    = 'BSTME'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'E'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'BSTME'.    " Bestellmengeneinheit
  g_s_fieldcat-ref_tabname   = 'MSEG'.     " Order unit
  g_s_fieldcat-sp_group      = 'E'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Extern eingegebener Buchungsbetrag in Hauswährung
* Externally entered posting amount in local currency
  g_s_fieldcat-fieldname     = 'EXBWR'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-cfieldname    = 'WAERS'.
  g_s_fieldcat-sp_group      = 'S'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Wert zu Verkaufspreisen mit Mehrwertsteuer
* Value at sales prices including value-added tax
  g_s_fieldcat-fieldname     = 'VKWRT'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-cfieldname    = 'WAERS'.
  g_s_fieldcat-sp_group      = 'V'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Verfallsdatum oder Mindesthaltbarkeitsdatum
* Shelf Life Expiration Date
  g_s_fieldcat-fieldname     = 'VFDAT'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Extern eingegebener Verkaufswert in Hauswährung
* Externally entered sales value in local currency
  g_s_fieldcat-fieldname     = 'EXVKW'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-cfieldname    = 'WAERS'.
  g_s_fieldcat-sp_group      = 'S'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'PPRCTR'.    " Partner-Profit Center
  g_s_fieldcat-ref_tabname   = 'MSEG'.      " Partner-Profit Center
  g_s_fieldcat-sp_group      = 'O'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Material, auf dem der Bestand geführt wird
* Material on which stock is managed
  g_s_fieldcat-fieldname     = 'MATBF'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Empfangendes/Abgebendes Material
* Receiving/issuing material
  g_s_fieldcat-fieldname     = 'UMMAB'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Gesamter bewerteter Bestand
* Total valuated stock before the posting
  g_s_fieldcat-fieldname     = 'LBKUM'.
  g_s_fieldcat-qfieldname    = 'MEINS'.                    "note 201670
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Wert des gesamten bewerteten Bestandes
* Value of total valuated stock before the posting
  g_s_fieldcat-fieldname     = 'SALK3'.
  g_s_fieldcat-cfieldname    = 'WAERS'.                    "note 201670
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'B'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

  g_s_fieldcat-fieldname     = 'VPRSV'.    " Preissteuerungskennzeichen
  g_s_fieldcat-ref_tabname   = 'MSEG'.     " Price control indicator
  g_s_fieldcat-sp_group      = 'S'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Wert zu Verkaufspreisen ohne Mehrwertsteuer
* Value at sales prices excluding value-added tax
  g_s_fieldcat-fieldname     = 'VKWRA'.
  g_s_fieldcat-cfieldname    = 'WAERS'.                   "note 201670
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'S'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Ursprungszeile im Materialbeleg
* Original line in material document
  g_s_fieldcat-fieldname     = 'URZEI'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'S'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Menge in Mengeneinheit aus Lieferschein
* Quantity in unit of measure from delivery note
  g_s_fieldcat-fieldname     = 'LSMNG'.
  g_s_fieldcat-qfieldname    = 'LSMEH'.                  "note 201670
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* Mengeneinheit aus Lieferschein
* Unit of measure from delivery note
  g_s_fieldcat-fieldname     = 'LSMEH'.
  g_s_fieldcat-ref_tabname   = 'MSEG'.
  g_s_fieldcat-sp_group      = 'M'.
  PERFORM  f0410_fieldcat    USING  c_check  c_no_out.

* if the field catalog contains a field with values in currency,
* add the currency to to field-catalogue
  DATA : l_cnt_waers_active TYPE i,                         "n497992
         l_cnt_waers_total  TYPE i.                         "n497992
                                                            "n497992
  LOOP AT fieldcat           INTO  g_s_fieldcat.            "n497992
    CHECK : g_s_fieldcat-cfieldname    = 'WAERS'.           "n497992
*   this field has a reference to the currency key          "n497992
    ADD  1                   TO  l_cnt_waers_total.         "n497992
                                                            "n497992
    CHECK : g_s_fieldcat-no_out IS INITIAL.                 "n497992
*   this field is active                                    "n497992
    ADD  1                   TO  l_cnt_waers_active.        "n497992
  ENDLOOP.                                                  "n497992
                                                            "n497992
  IF    l_cnt_waers_active > 0.                             "n497992
*   there is at least one active reference field            "n497992
*   declare currency key WAERS active, too                  "n497992
    g_s_fieldcat-fieldname     = 'WAERS'.   "Currency Key   "n497992
    g_s_fieldcat-ref_tabname   = 'T001'.                    "n497992
    g_s_fieldcat-sp_group      = 'M'.                       "n497992
    PERFORM  f0410_fieldcat    USING  c_take   c_no_out.    "n497992
                                                            "n497992
  ELSEIF  l_cnt_waers_total > 0.                            "n497992
*   there are only hidden reference fields                  "n497992
*   declare currency key WAERS hidden, too                  "n497992
    g_s_fieldcat-fieldname     = 'WAERS'.   "Currency Key   "n497992
    g_s_fieldcat-ref_tabname   = 'T001'.                    "n497992
    g_s_fieldcat-sp_group      = 'M'.                       "n497992
    PERFORM  f0410_fieldcat    USING  c_take   c_no_out.    "n497992
  ENDIF.                                                    "n497992
*  ENHANCEMENT-POINT rm07mlbd_04 SPOTS es_rm07mlbd.
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_15\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
* Only if no valuated Stock should display
    IF bwbst <> 'X'.
      CLEAR: g_s_fieldcat-no_out.
      g_s_fieldcat-fieldname     = '/CWM/MENGE'.        " Menge
      g_s_fieldcat-ref_tabname   = 'MSEG'.              " Quantity
      g_s_fieldcat-qfieldname    = '/CWM/MEINS'.
      g_s_fieldcat-sp_group      = 'M'.
      PERFORM  f0410_fieldcat    USING  c_take   c_no_out.
    ENDIF.

* Only if no valuated Stock should display
    IF bwbst <> 'X'.
      CLEAR: g_s_fieldcat-no_out.
      g_s_fieldcat-fieldname     = '/CWM/MEINS'.        " Basismengeneinheit
      g_s_fieldcat-ref_tabname   = 'MSEG'.              " Base unit of measure
      g_s_fieldcat-sp_group      = 'M'.
      PERFORM  f0410_fieldcat    USING  c_take   c_no_out.
    ENDIF.

* Now set the minimum Line-Size for new fields in Header, so
* all values in Headerline (now length = 84) will be shown and
* remain intact even the user reduce the number of visible fields
    layout-min_linesize = 85.

* use ref to /CWM/VALUM
    IF bwbst = 'X'.
      LOOP AT fieldcat INTO g_s_fieldcat WHERE fieldname = 'MEINS'.
        g_s_fieldcat-ref_tabname = 'MARA'.
        g_s_fieldcat-ref_fieldname = '/CWM/VALUM'.
        MODIFY fieldcat FROM g_s_fieldcat.
      ENDLOOP.
    ENDIF.
  ENDIF.
*  ENHANCEMENT-POINT ehp605_rm07mlbd_15 SPOTS es_rm07mlbd .

ENDFORM.                     "f0400_create_fieldcat

*----------------------------------------------------------------------*
*    F0410_FIELDCAT
*----------------------------------------------------------------------*

FORM f0410_fieldcat
         USING  l_f_check
                l_f_no_out   TYPE      slis_fieldcat_main-no_out.

  DATA : l_f_continue(01) TYPE c,
         l_f_type(01)     TYPE c,
         l_f_fieldname    TYPE      stype_fields.

  FIELD-SYMBOLS : <l_fs>.

  IF  l_f_check = c_take.
*   take this entry without check
    MOVE  'X'                TO  l_f_continue.
  ELSE.
*   create key and look for fieldname
    CONCATENATE              g_s_fieldcat-ref_tabname
                             '~'
                             g_s_fieldcat-fieldname
                             INTO l_f_fieldname.

    READ TABLE g_t_mseg_fields         INTO g_s_mseg_fields
                             WITH KEY
                             fieldname = l_f_fieldname
                             BINARY SEARCH.

    IF sy-subrc IS INITIAL.
      MOVE  'X'              TO  l_f_continue.
    ELSE.
*     additional fields are displayed in wrong format :     "n480130
*     clear the working area for the field catalog when     "n480130
*     the current field should not be processed             "n480130
      CLEAR                  g_s_fieldcat.                  "n480130
      CLEAR                  l_f_continue.
    ENDIF.
  ENDIF.

* append entry to field catalog if field is in structure
* else leave this routine
  IF l_f_continue IS INITIAL.
    CLEAR                    g_s_fieldcat.
    EXIT.
  ENDIF.

  IF  NOT l_f_no_out IS INITIAL.
    MOVE  l_f_no_out         TO  g_s_fieldcat-no_out.
  ENDIF.

  ADD  : 1                   TO  g_f_col_pos.
  MOVE : g_f_col_pos         TO  g_s_fieldcat-col_pos,
         g_f_tabname         TO  g_s_fieldcat-tabname.
  APPEND g_s_fieldcat        TO  fieldcat.

* create the table with the fields who will be enriched with colors
* and sign
  IF  g_s_fieldcat-fieldname  =  'MENGE'  OR
      g_s_fieldcat-fieldname  =  'MEINS'  OR
      g_s_fieldcat-fieldname  =  'DMBTR'  OR
      g_s_fieldcat-fieldname  =  'WAERS'  OR
      g_s_fieldcat-fieldname  =  'ERFMG'  OR
      g_s_fieldcat-fieldname  =  'ERFME'  OR

      g_s_fieldcat-fieldname  =  'BPMNG'  OR
      g_s_fieldcat-fieldname  =  'BPRME'  OR
      g_s_fieldcat-fieldname  =  'BSTMG'  OR
      g_s_fieldcat-fieldname  =  'BSTME'  OR
      g_s_fieldcat-fieldname  =  'EXBWR'  OR
      g_s_fieldcat-fieldname  =  'VKWRT'  OR

      g_s_fieldcat-fieldname  =  'EXVKW'  OR
      g_s_fieldcat-fieldname  =  'VKWRA'  OR
      g_s_fieldcat-fieldname  =  'LSMNG'  OR
      g_s_fieldcat-fieldname  =  'LSMEH'  OR
      g_s_fieldcat-fieldname  =  'SHKZG'.

*   look for the type of this field
    CONCATENATE              g_s_fieldcat-ref_tabname
                             '-'
                             g_s_fieldcat-fieldname
                             INTO l_f_fieldname.

    ASSIGN  (l_f_fieldname)  TO <l_fs>.

    IF  sy-subrc IS INITIAL.
      DESCRIBE FIELD <l_fs>    TYPE  l_f_type.
      MOVE : g_s_fieldcat-fieldname
                             TO  g_t_color_fields-fieldname,
           l_f_type          TO  g_t_color_fields-type.
      APPEND                 g_t_color_fields.
    ENDIF.
  ENDIF.
  LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_16\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
  IF  g_s_fieldcat-fieldname  =  '/CWM/MENGE'  OR
      g_s_fieldcat-fieldname  =  '/CWM/MEINS'  OR
      g_s_fieldcat-fieldname  =  '/CWM/ERFMG'  OR
      g_s_fieldcat-fieldname  =  '/CWM/ERFME'.
*   look for the type of this field
    CONCATENATE              g_s_fieldcat-ref_tabname
                             '-'
                             g_s_fieldcat-fieldname
                             INTO l_f_fieldname.

    ASSIGN  (l_f_fieldname)  TO <l_fs>.

    IF  sy-subrc IS INITIAL.
      DESCRIBE FIELD <l_fs>    TYPE  l_f_type.
      MOVE : g_s_fieldcat-fieldname
                             TO  g_t_color_fields-fieldname,
           l_f_type          TO  g_t_color_fields-type.
      APPEND                 g_t_color_fields.
    ENDIF.
  ENDIF.

*  ENHANCEMENT-POINT ehp605_rm07mlbd_16 SPOTS es_rm07mlbd .

  CLEAR                      g_s_fieldcat.

ENDFORM.                     "F0410_FIELDCAT

*&----------------------------------------------------------"n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  belege_ergaenzen_2
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM belege_ergaenzen_2.                                    "n443935
                                                            "n443935
* control break : process the entries from a group          "n443935
                                                            "n443935
* look for the matching FI documents with set and get       "n443935
  MOVE : g_s_mseg_old-mblnr  TO  matkey-mblnr,              "n443935
         g_s_mseg_old-mjahr  TO  matkey-mjahr.              "n443935
                                                            "n443935
  READ  TABLE g_t_bsim_lean  INTO  g_s_bsim_lean            "n443935
                   WITH KEY  bukrs  =  g_s_mseg_old-bukrs   "n443935
                             bwkey  =  g_s_mseg_old-bwkey   "n443935
                             matnr  =  g_s_mseg_old-matnr   "n443935
                             bwtar  =  g_s_mseg_old-bwtar   "n443935
                             shkzg  =  g_s_mseg_old-shkzg   "n443935
                             meins  =  g_s_mseg_old-meins   "n443935
                             budat  =  g_s_mseg_old-budat   "n443935
                             blart  =  g_s_mseg_old-blart   "n443935
                             awkey  =  matkey               "n443935
                             BINARY SEARCH.                 "n443935
  IF sy-subrc IS NOT INITIAL.                               "2575057
    READ  TABLE g_t_bsim_lean  INTO  g_s_bsim_lean          "2575057
                 WITH KEY  bukrs  =  g_s_mseg_old-bukrs     "2575057
                           bwkey  =  g_s_mseg_old-bwkey     "2575057
                           matnr  =  g_s_mseg_old-matnr     "2575057
                           bwtar  =  g_s_mseg_old-bwtar     "2575057
                           shkzg  =  g_s_mseg_old-shkzg     "2575057
                           meins  =  g_s_mseg_old-meins     "2575057
                           budat  =  g_s_mseg_old-budat     "2575057
                           awkey  =  matkey.                "2575057
  ENDIF.                                                    "2575057
  IF sy-subrc IS INITIAL.                                   "n443935
    MOVE  sy-tabix           TO  g_f_tabix_start.           "n443935
                                                            "n443935
*   continue with sequential read of working table          "n443935
    LOOP AT g_t_bsim_lean    INTO  g_s_bsim_lean            "n443935
                             FROM  g_f_tabix_start.         "n443935
                                                            "n443935
      IF  g_s_bsim_lean-bukrs  =  g_s_mseg_old-bukrs  AND   "n443935
          g_s_bsim_lean-bwkey  =  g_s_mseg_old-bwkey  AND   "n443935
          g_s_bsim_lean-matnr  =  g_s_mseg_old-matnr  AND   "n443935
          g_s_bsim_lean-bwtar  =  g_s_mseg_old-bwtar  AND   "n443935
          g_s_bsim_lean-shkzg  =  g_s_mseg_old-shkzg  AND   "n443935
          g_s_bsim_lean-meins  =  g_s_mseg_old-meins  AND   "n443935
          g_s_bsim_lean-budat  =  g_s_mseg_old-budat  AND   "n443935
*         IS-OIL posts GR for PO with "MB11" which leads to    "2575057
*         WA in MKPF and WE in BSIM. This is against the       "2575057
*         core rule. Implement a light fuzzy search, check     "2575057
*         only for BLART starting with W like WE,WA,WF,WI,WL.  "2575057
          g_s_bsim_lean-blart(1)  =  'W'              AND   "2575057
          g_s_bsim_lean-awkey  =  matkey.                   "n443935
*       select all matching entries                         "n443935
        ADD   1              TO  g_cnt_bsim_entries.        "n443935
        MOVE-CORRESPONDING  g_s_bsim_lean                   "n443935
                             TO  g_s_bsim_work.             "n443935
        MOVE  sy-tabix       TO  g_s_bsim_work-tabix.       "n443935
        APPEND g_s_bsim_work TO  g_t_bsim_work.             "n443935
      ELSE.                                                 "n443935
        IF g_s_bsim_lean-awkey <> matkey.                   "2575057
          EXIT.                                             "n443935
        ENDIF.                                              "2575057
      ENDIF.                                                "n443935
    ENDLOOP.                                                "n443935
  ENDIF.                                                    "n443935

  IF  g_flag_break-b1 = 'X'.                                "n921164
    BREAK-POINT                ID mmim_rep_mb5b.            "n921164
*   dynamic break-point : results in contol break           "n921164
  ENDIF.

* how many matching entries from BSIM found ?               "n443935
  IF      g_cnt_bsim_entries IS INITIAL.                    "n443935
*   no BSIM entries found -> no action.                     "n443935
                                                            "n443935
  ELSEIF  g_cnt_bsim_entries = 1  AND                       "n443935
          g_cnt_mseg_entries = 1.                           "n443935
*   the ideal case 1 MM and 1 FI document;                  "n443935
*   mark this FI doc for deletion                           "n443935
    LOOP AT g_t_bsim_work    INTO  g_s_bsim_work.           "n443935
      READ  TABLE  g_t_bsim_lean  INTO  g_s_bsim_lean       "n443935
                             INDEX  g_s_bsim_work-tabix.    "n443935
                                                            "n443935
      CHECK : sy-subrc IS INITIAL.                          "n443935
      MOVE  : 'D'            TO  g_s_bsim_lean-accessed.    "n443935
      MODIFY  g_t_bsim_lean  FROM  g_s_bsim_lean            "n443935
                             INDEX  g_s_bsim_work-tabix     "n443935
                             TRANSPORTING  accessed.        "n451923
                                                            "n443935
*     set the FI doc number into the entry of the MM doc    "n443935
      READ  TABLE  g_t_mseg_work  INTO  g_s_mseg_work       "n443935
                             INDEX  1.                      "n443935
      CHECK : sy-subrc IS INITIAL.                          "n443935
                                                            "n443935
      MOVE : g_s_bsim_work-belnr                            "n443935
                             TO  g_s_mseg_work-belnr,       "n443935
             g_s_bsim_work-gjahr                            "n443935
                             TO  g_s_mseg_work-gjahr.       "n443935
      IF gv_switch_ehp6ru = abap_true.
        MOVE: g_s_bsim_work-buzei
                             TO  g_s_mseg_work-buzei.
        MOVE-CORRESPONDING g_s_bsim_work TO g_t_bseg_key.
        APPEND g_t_bseg_key.
      ENDIF.

*     consider special gain/loss-handling of IS-OIL         "n497992
**# IF EXIST OI001
**"    if  g_s_mseg_work-oiglcalc = 'L'  and            "n497992
**"        g_s_mseg_work-shkzg    = 'H'  and            "n497992
**"        g_s_mseg_work-dmbtr    = 0.                  "n497992
**"      move  g_s_bsim_work-dmbtr                      "n497992
**"                  to  g_s_mseg_work-dmbtr.           "n497992
**"    endif.                                           "n497992
**"                                                     "n497992
**"    MODIFY G_T_MSEG_work                             "n497992
**"                 FROM  G_S_MSEG_work                 "n497992
**"                 INDEX  1                            "n497992
**"                 TRANSPORTING BELNR GJAHR dmbtr.     "n497992
**# ELSE
*      MODIFY G_T_MSEG_work  FROM  G_S_MSEG_work        "n443935
*                            INDEX  1                   "n443935
*                            TRANSPORTING  BELNR GJAHR. "n443935
**# ENDIF
*     IS-OIL specific functions without ABAP preprocessor   "n599218 A
      IF  g_flag_is_oil_active = 'X'.       "IS-OIL ?       "n599218 A
        IF  g_s_mseg_work-oiglcalc = 'L'  AND               "n599218 A
            g_s_mseg_work-shkzg    = 'H'  AND               "n599218 A
            g_s_mseg_work-dmbtr    = 0.                     "n599218 A
          MOVE  g_s_bsim_work-dmbtr                         "n599218 A
                             TO  g_s_mseg_work-dmbtr.       "n599218 A
        ENDIF.                                              "n599218 A
                                                            "n599218 A
        MODIFY g_t_mseg_work                                "n599218 A
                   FROM  g_s_mseg_work                      "n599218 A
                   INDEX  1                                 "n599218 A
                   TRANSPORTING belnr gjahr buzei dmbtr.
      ELSE.                                                 "n599218 A
        MODIFY g_t_mseg_work  FROM  g_s_mseg_work           "n599218 A
                            INDEX  1                        "n599218 A
                            TRANSPORTING  belnr gjahr buzei.
      ENDIF.                                                "n599218 A

    ENDLOOP.                                                "n443935
                                                            "n443935
  ELSE.                                                     "n443935
*   there are a lot of MM docs                              "n443935
    PERFORM                  belege_ergaenzen_several_docs. "n443935
                                                            "n443935
  ENDIF.                                                    "n443935
                                                            "n443935
* copy the number and fiscal year into the matching         "n451923
* entry of the main table G_T_MSEG_LEAN                     "n451923
  LOOP AT g_t_mseg_work      INTO  g_s_mseg_work.           "n451923
*   only with useful FI doc data                            "n451923
    CHECK : NOT g_s_mseg_work-belnr IS INITIAL.             "n451923
                                                            "n443935
*   read the original entry and change it                   "n451923
    READ TABLE g_t_mseg_lean INTO  g_s_mseg_update          "n451923
                             INDEX g_s_mseg_work-tabix.     "n451923
                                                            "n443935
    CHECK : sy-subrc IS INITIAL.   "entry found ?           "n451923
    MOVE  : g_s_mseg_work-belnr                             "n451923
                             TO  g_s_mseg_update-belnr,     "n451923
            g_s_mseg_work-gjahr                             "n451923
                             TO  g_s_mseg_update-gjahr.     "n451923
    IF gv_switch_ehp6ru = abap_true.
      MOVE: g_s_mseg_work-buzei
                             TO  g_s_mseg_update-buzei.
      MOVE-CORRESPONDING g_s_mseg_work TO g_t_bseg_key.
      APPEND g_t_bseg_key.
    ENDIF.

*   consider special gain/loss-handling of IS-OIL           "n497992
**# IF EXIST OI001
**"  move  g_s_mseg_work-dmbtr                          "n497992
**"                 to  g_s_mseg_update-dmbtr.          "n497992
**"                                                     "n497992
**"  MODIFY G_T_MSEG_lean                               "n497992
**"                 FROM  G_S_MSEG_update               "n497992
**"                 index g_s_mseg_work-tabix           "n497992
**"                 TRANSPORTING BELNR GJAHR dmbtr.     "n497992
**# ELSE
*    modify  g_t_mseg_lean  from  g_s_mseg_update       "n451923
*                           index g_s_mseg_work-tabix   "n451923
*                           transporting  belnr gjahr.  "n451923
**# ENDIF
*   IS-OIL specific functions without ABAP preprocessor     "n599218 A
    IF  g_flag_is_oil_active = 'X'.        "IS-OIL ?       "n599218 A
      MOVE  g_s_mseg_work-dmbtr                             "n599218 A
                             TO  g_s_mseg_update-dmbtr.     "n599218 A
                                                            "n599218 A
      MODIFY g_t_mseg_lean                                  "n599218 A
                 FROM  g_s_mseg_update                      "n599218 A
                 INDEX g_s_mseg_work-tabix                  "n599218 A
                 TRANSPORTING belnr gjahr buzei dmbtr.
    ELSE.                                                   "n599218 A
      MODIFY g_t_mseg_lean FROM  g_s_mseg_update            "n599218 A
                           INDEX g_s_mseg_work-tabix        "n599218 A
                           TRANSPORTING  belnr gjahr buzei.
    ENDIF.                                                  "n599218 A

  ENDLOOP.                                                  "n451923

  PERFORM                    belege_ergaenzen_clear.        "n443935
                                                            "n443935
ENDFORM.                     "belege_ergaenzen_2            "n443935
                                                            "n443935
*&----------------------------------------------------------"n443935
*& belege_ergaenzen_clear
*&----------------------------------------------------------"n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  belege_ergaenzen_clear
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM belege_ergaenzen_clear.                                "n443935
                                                            "n443935
* clear working areas for the next group                    "n443935
  REFRESH : g_t_bsim_work,   g_t_mseg_work.                 "n443935
  CLEAR   : g_cnt_mseg_entries, g_cnt_mseg_done,            "n443935
            g_cnt_bsim_entries.                             "n443935
                                                            "n443935
ENDFORM.                     "belege_ergaenzen_clear.       "n443935
                                                            "n443935
*&----------------------------------------------------------"n443935
*    belege_ergaenzen_several_docs
*&----------------------------------------------------------"n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  belege_ergaenzen_several_docs
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM belege_ergaenzen_several_docs.                         "n443935
*
  DATA lv_/cwm/menge        LIKE g_s_mseg_work-menge.
*  ENHANCEMENT-POINT ehp605_rm07mlbd_17 SPOTS es_rm07mlbd STATIC .
                                                            "n443935
* first step : the entries must hit quantity and value      "n443935
  LOOP AT g_t_mseg_work    INTO  g_s_mseg_work.             "n443935
                                                            "n443935
*   look for a matching FI doc                              "n443935
    LOOP AT g_t_bsim_work  INTO  g_s_bsim_work.             "n443935
*     ingnore the entries for deletion                      "n443935
      CHECK : g_s_bsim_work-accessed NE 'D'.                "n443935
                                                            "n443935
      IF  g_s_bsim_work-menge = g_s_mseg_work-menge  AND    "n443935
          g_s_bsim_work-dmbtr = g_s_mseg_work-dmbtr.        "n443935
*       mark the entries                                    "n443935
        ADD  1               TO  g_cnt_mseg_done.           "n443935
        MOVE : g_s_bsim_work-belnr                          "n443935
                             TO  g_s_mseg_work-belnr,       "n443935
               g_s_bsim_work-gjahr                          "n443935
                             TO  g_s_mseg_work-gjahr.       "n443935
        IF gv_switch_ehp6ru = abap_true.
          MOVE: g_s_bsim_work-buzei
                             TO  g_s_mseg_work-buzei.
          MOVE-CORRESPONDING g_s_bsim_work TO g_t_bseg_key.
          APPEND g_t_bseg_key.
        ENDIF.

        MODIFY g_t_mseg_work  FROM  g_s_mseg_work           "n443935
                             TRANSPORTING  belnr gjahr buzei.
                                                            "n443935
*       mark the entries for deletion                       "n443935
        MOVE    'D'          TO  g_s_bsim_work-accessed.    "n443935
        MODIFY  g_t_bsim_work  FROM  g_s_bsim_work          "n443935
                             TRANSPORTING  accessed.        "n451923
        EXIT.                "Stop at the firts hit         "n443935
      ENDIF.                                                "n443935
    ENDLOOP.                                                "n443935
  ENDLOOP.                                                  "n443935

  IF  g_flag_break-b2 = 'X'.                                "n921164
    BREAK-POINT                ID mmim_rep_mb5b.            "n921164
*   dynamic break-point : in control break                  "n921164
  ENDIF.                                                    "n921164

  IF  g_cnt_mseg_entries  NE g_cnt_mseg_done.               "n443935
*   there are MM docs without FI doc left                   "n443935
                                                            "n443935
*     subtract the quantity and value from MM doc from      "n443935
*     the fields of the FI doc                              "n443935
    LOOP AT g_t_mseg_work  INTO  g_s_mseg_work.             "n443935
                                                            "n443935
*       take only the entries without FI doc number         "n443935
      CHECK : g_s_mseg_work-belnr IS INITIAL.               "n443935
                                                            "n443935
      LOOP AT g_t_bsim_work  INTO  g_s_bsim_work.           "n443935
*         ingnore the entries for deletion                  "n443935
        CHECK : g_s_bsim_work-accessed NE 'D'.              "n443935
                                                            "n443935
        IF g_s_bsim_work-menge GE g_s_mseg_work-menge AND   "n443935
           g_s_bsim_work-dmbtr GE g_s_mseg_work-dmbtr.      "n443935
                                                            "n443935
          IF NOT g_s_mseg_work-dmbtr IS INITIAL.            "2117567
*           quantities without value are not in BSIM            "2117567
            SUBTRACT :                                      "n443935
              g_s_mseg_work-menge FROM  g_s_bsim_work-menge, "n443935
              g_s_mseg_work-dmbtr FROM  g_s_bsim_work-dmbtr. "n443935
          ENDIF.                                            "2117567
                                                            "n443935
          IF  g_s_bsim_work-menge  IS INITIAL  AND          "n443935
              g_s_bsim_work-dmbtr  IS INITIAL.              "n443935
*           mark the entry for deletion                     "n443935
            MOVE    'D'      TO  g_s_bsim_work-accessed.    "n443935
          ELSE.                                             "n443935
*           set the flag for check the merge process        "n443935
            MOVE    'X'      TO  g_s_bsim_work-accessed.    "n443935
          ENDIF.                                            "n443935
                                                            "n443935
          MODIFY  g_t_bsim_work  FROM  g_s_bsim_work        "n443935
*           change quantity and value in working table, too  "n747306
            TRANSPORTING  accessed menge dmbtr.             "n747306
                                                            "n443935
*         mark the entries                                  "n443935
          ADD  1             TO  g_cnt_mseg_done.           "n443935
          MOVE : g_s_bsim_work-belnr                        "n443935
                             TO  g_s_mseg_work-belnr,       "n443935
                 g_s_bsim_work-gjahr                        "n443935
                             TO  g_s_mseg_work-gjahr.       "n443935
          IF gv_switch_ehp6ru = abap_true.
            MOVE: g_s_bsim_work-buzei
                             TO  g_s_mseg_work-buzei.
            MOVE-CORRESPONDING g_s_bsim_work TO g_t_bseg_key.
            APPEND g_t_bseg_key.
          ENDIF.

          MODIFY g_t_mseg_work  FROM  g_s_mseg_work         "n443935
                             TRANSPORTING  belnr gjahr buzei.
          EXIT.              "Stop at the first hit         "n443935
        ENDIF.                                              "n443935
      ENDLOOP.                                              "n443935
    ENDLOOP.                                                "n443935
  ENDIF.                                                    "n443935
                                                            "n443935
* mark the processed FI docs for deletion                   "n443935
  LOOP AT g_t_bsim_work    INTO  g_s_bsim_work.             "n443935
    CHECK   g_s_bsim_work-accessed = 'D'.                   "n443935
                                                            "n443935
    READ  TABLE  g_t_bsim_lean  INTO  g_s_bsim_lean         "n443935
                             INDEX  g_s_bsim_work-tabix.    "n443935
                                                            "n443935
    CHECK : sy-subrc IS INITIAL.                            "n443935
    MOVE  : 'D'              TO  g_s_bsim_lean-accessed.    "n443935
    MODIFY  g_t_bsim_lean    FROM   g_s_bsim_lean           "n443935
                             INDEX  g_s_bsim_work-tabix     "n443935
                             TRANSPORTING  accessed.        "n451923
  ENDLOOP.                                                  "n443935
  LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_18\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
  LOOP AT g_t_mseg_work      INTO  g_s_mseg_work.

    IF /cwm/cl_md_material=>is_cw_material( g_s_mseg_work-matnr ) = /cwm/cl_md_material=>true.

      IF g_s_mseg_work-matnr   <> imara-matnr.
        READ TABLE imara WITH KEY matnr = g_s_mseg_work-matnr
                         BINARY SEARCH.
      ENDIF.
      IF imara-/cwm/valum = g_s_mseg_work-/cwm/meins.
        lv_/cwm/menge            =  g_s_mseg_work-/cwm/menge.
        g_s_mseg_work-/cwm/menge =  g_s_mseg_work-menge.
        g_s_mseg_work-menge      =  lv_/cwm/menge.
        MODIFY g_t_mseg_work   FROM  g_s_mseg_work
                             TRANSPORTING  menge /cwm/menge.
      ENDIF.
    ENDIF.
  ENDLOOP.
*  ENHANCEMENT-POINT ehp605_rm07mlbd_18 SPOTS es_rm07mlbd .
                                                            "n443935
ENDFORM.                     "belege_ergaenzen_several_docs "n443935
                                                            "n443935
*&---------------------------------------------------------------------*
*&      Form  USER_COMMAND                                             *
*&---------------------------------------------------------------------*

FORM user_command                                           "#EC CALLED
                   USING     r_ucomm      LIKE  sy-ucomm
                             rs_selfield  TYPE  slis_selfield.

  TYPES: BEGIN OF ty_s_sel,
           mblnr LIKE  mseg-mblnr,
           mjahr LIKE  mseg-mjahr,
           zeile LIKE  mseg-zeile,
           bukrs LIKE  mseg-bukrs,
           belnr LIKE  mseg-belnr,
           gjahr LIKE  mseg-gjahr,
         END OF ty_s_sel,

         ty_t_sel TYPE ty_s_sel OCCURS 0.

  DATA: l_value(10) TYPE c,                                 "n1583816
        ls_sel      TYPE ty_s_sel,
        lt_sel      TYPE ty_t_sel,
        l_lines     LIKE sy-tabix,
        ls_fc       TYPE slis_fieldcat_alv,
        lt_fc       TYPE slis_t_fieldcat_alv,
        ls_selfield TYPE slis_selfield,
        l_fi_doc    TYPE c  LENGTH 1.                       "n1511550

  DATA lv_exit TYPE abap_bool.

* Unfortunately the output list of this report consists
* of several ALVs, one started at the end-event of the other.
* This abstrucse programming style was chosen to create a list
* layout similar to the one in release 3.1. Now this causes a severe
* problem: When selecting a line, we do not know which ALV (and there-
* for which line in table IMSEG) has been selected. We can only use
* the value of the selected field to access the data-table.
* In case of ambiguities, a popup has to be transmitted where the
* user has to reselect the document he wants to see. This is
* difficult to understand, if you do not know the problems of
* programming ABAP.

  CASE r_ucomm.
    WHEN 'MMBE' OR 'DOCU' OR 'FIDC'.
      PERFORM zf_toolbar_command USING r_ucomm.
      EXIT.

    WHEN '9PBP'.
*     Get line of IMSEG which "look" like the one selected
      l_value = rs_selfield-value.
      CHECK NOT l_value IS INITIAL.                         "204872
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'              "n1604106
        EXPORTING                                              "n1604106
          input  = l_value                                     "n1604106
        IMPORTING                                              "n1604106
          output = l_value.                                    "n1604106

      IF rs_selfield-sel_tab_field = 'G_T_BELEGE-MBLNR' OR
         rs_selfield-sel_tab_field = 'G_T_BELEGE1-MBLNR'.
        LOOP AT g_t_mseg_lean          INTO  g_s_mseg_lean
                                       WHERE mblnr = l_value.
          ls_sel-mblnr = g_s_mseg_lean-mblnr.
          ls_sel-mjahr = g_s_mseg_lean-mjahr.
          ls_sel-zeile = g_s_mseg_lean-zeile.
          COLLECT ls_sel INTO lt_sel.
        ENDLOOP.

      ELSEIF rs_selfield-sel_tab_field = 'G_T_BELEGE-BELNR' OR
             rs_selfield-sel_tab_field = 'G_T_BELEGE1-BELNR'.
        l_fi_doc = 'X'.
        LOOP AT g_t_mseg_lean          INTO  g_s_mseg_lean
                                       WHERE belnr = l_value.
          ls_sel-belnr = g_s_mseg_lean-belnr.
          ls_sel-gjahr = g_s_mseg_lean-gjahr.
          ls_sel-bukrs = g_s_mseg_lean-bukrs.
          COLLECT ls_sel INTO lt_sel.
        ENDLOOP.

      ELSE.
        "Exibe outros documentos
        PERFORM zf_call_tcode USING rs_selfield.
        EXIT.

      ENDIF.
      SORT lt_sel BY mjahr mblnr zeile bukrs belnr gjahr.

*     Read first line. If L_LINES = 1, LS_SEL is filled properly.
      READ TABLE lt_sel INTO ls_sel INDEX 1.
      DESCRIBE TABLE lt_sel LINES l_lines.
*     If no line found, the cursor was not on a useful value.
      IF l_lines = 0.
        MESSAGE s270.
        EXIT.
      ENDIF.

*     If more than one line found, it gets difficult. We send a popup
*     where the user may select a single line.
      IF l_lines > 1.
*       Create fieldcatalog
        DEFINE fc_add.
          ls_fc-fieldname     = &1.
          ls_fc-ref_tabname   = &2.
          ls_fc-ref_fieldname = &3.
          APPEND ls_fc TO lt_fc.
        END-OF-DEFINITION.
        CLEAR ls_sel.
        IF l_fi_doc IS INITIAL.
          fc_add 'MBLNR' 'MKPF' 'MBLNR'.
          fc_add 'MJAHR' 'MKPF' 'MJAHR'.
          fc_add 'ZEILE' 'MSEG' 'ZEILE'.
        ELSE.
          fc_add 'BUKRS' 'BKPF' 'BUKRS'.
          fc_add 'BELNR' 'BKPF' 'BELNR'.
          fc_add 'GJAHR' 'BKPF' 'GJAHR'.
        ENDIF.
        CALL FUNCTION 'REUSE_ALV_POPUP_TO_SELECT'
          EXPORTING
            i_zebra     = 'X'
            i_tabname   = 'LT_SEL'
            it_fieldcat = lt_fc
          IMPORTING
            es_selfield = ls_selfield
          TABLES
            t_outtab    = lt_sel.
*       Read table with the unique index.
        READ TABLE lt_sel INTO ls_sel INDEX ls_selfield-tabindex.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
      ENDIF.

*     read and save the user parameters before calling
*     transaction MIGO or FB03
      PERFORM                user_parameters_save.

*     Now call the corresponding application. LS_SEL is always filled
*     correctly.
      IF l_fi_doc IS INITIAL.

*     call the display transcation MIGO for the MM document "TEST
        CALL FUNCTION 'MIGO_DIALOG'                       "n547170
          EXPORTING                                       "n547170
            i_action            = 'A04'                   "n547170
            i_refdoc            = 'R02'                   "n547170
            i_notree            = 'X'                     "n547170
            i_no_auth_check     = ' '                     "n547170
            i_deadend           = 'X'                     "n547170
            i_skip_first_screen = 'X'                     "n547170
            i_okcode            = 'OK_GO'                 "n547170
            i_mblnr             = ls_sel-mblnr            "n547170
            i_mjahr             = ls_sel-mjahr            "n547170
            i_zeile             = ls_sel-zeile.           "n547170

      ELSE.
        SET PARAMETER ID 'BLN' FIELD ls_sel-belnr.
        SET PARAMETER ID 'BUK' FIELD ls_sel-bukrs.
        SET PARAMETER ID 'GJR' FIELD ls_sel-gjahr.
        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

      ENDIF.

*     restore the former user parameters
      PERFORM                user_parameters_restore.

  ENDCASE.

ENDFORM.                               " USER_COMMAND

*-----------------------------------------------------------"n547170
*    esdus_get_parameters                                   "n547170
*-----------------------------------------------------------"n547170

FORM esdus_get_parameters.                                  "n547170
*-----------------------------------------------------------"n547170
* Initialization of the user defaults for the checkboxes
* read the settings from table ESDUS
*-----------------------------------------------------------

* only in dialog mode
  CHECK sy-batch IS INITIAL.

  DATA : l_cnt_radiobutton   TYPE i.

* get the parameters from the last run from table ESDUS as
* default values  in release 4.6 and higher

  IF oref_settings IS INITIAL.
    CREATE OBJECT oref_settings
      EXPORTING
        i_action = 'ZRM07MLBD'.
  ENDIF.

** get the parameters from the last run
  lgbst    = oref_settings->get( 'LGBST'  ).
  bwbst    = oref_settings->get( 'BWBST'  ).
  sbbst    = oref_settings->get( 'SBBST'  ).
  xchar    = oref_settings->get( 'XCHAR'  ).
  xsum     = oref_settings->get( 'XSUM'   ).
  pa_sumfl = oref_settings->get( 'PA_SUMFL'   ).
  nosto    = oref_settings->get( 'NOSTO'  ).
  pa_aistr  = oref_settings->get( 'PA_AISTR' ).             "n1481757

***  get the parameters for the list categories              "n599218
  pa_wdzer = oref_settings->get( 'PA_WDZER' ).              "n599218
  pa_wdzew = oref_settings->get( 'PA_WDZEW' ).              "n599218
  pa_wdwiz = oref_settings->get( 'PA_WDWIZ' ).              "n599218
  pa_wdwuw = oref_settings->get( 'PA_WDWUW' ).              "n599218
  pa_wdwew = oref_settings->get( 'PA_WDWEW' ).              "n599218
  pa_ndsto = oref_settings->get( 'PA_NDSTO' ).              "n599218
  pa_ndzer = oref_settings->get( 'PA_NDZER' ).              "n599218
  xnomchb  = oref_settings->get( 'XNOMCHB' ).               "838360

**  check radiobutton rules
  IF  NOT lgbst IS INITIAL.
    ADD  1                 TO  l_cnt_radiobutton.
  ENDIF.

  IF  NOT bwbst IS INITIAL.
    ADD  1                 TO  l_cnt_radiobutton.
  ENDIF.

  IF  NOT sbbst IS INITIAL.
    ADD  1                 TO  l_cnt_radiobutton.
  ENDIF.

  IF  l_cnt_radiobutton NE 1.
**    offend against radiobutton rules ?
**    yes -> set the first and delete the rest
    MOVE : 'X'             TO  lgbst.
    CLEAR :                bwbst, sbbst.
  ENDIF.

* at the first time ( or in a lower release ) all seven     "n599218
* list categories will be initial --> activate them all     "n599218
  PERFORM                    f0850_empty_parameters.        "n599218
                                                            "n599218
  IF  g_cnt_empty_parameter = 7.                            "n599218
    MOVE : 'X'               TO  pa_wdzer,                  "n599218
           'X'               TO  pa_wdzew,                  "n599218
           'X'               TO  pa_wdwiz,                  "n599218
           'X'               TO  pa_wdwuw,                  "n599218
           'X'               TO  pa_wdwew,                  "n599218
           'X'               TO  pa_ndsto,                  "n599218
           'X'               TO  pa_ndzer.                  "n599218
  ENDIF.                                                    "n599218

ENDFORM.                     "esdus_get_parameters          "n547170

*-----------------------------------------------------------"n547170
*    esdus_save_parameters                                  "n547170
*-----------------------------------------------------------"n547170

FORM esdus_save_parameters.                                 "n547170
                                                            "n547170
* only in dialog mode
  CHECK : sy-batch IS INITIAL.

* Save the settings in release 4.6 and higher
  CALL METHOD oref_settings->set(
      i_element = 'LGBST'
      i_active  = lgbst ).
  CALL METHOD oref_settings->set(
      i_element = 'BWBST'
      i_active  = bwbst ).
  CALL METHOD oref_settings->set(
      i_element = 'SBBST'
      i_active  = sbbst ).
  CALL METHOD oref_settings->set(
      i_element = 'XCHAR'
      i_active  = xchar ).
  CALL METHOD oref_settings->set(
      i_element = 'XNOMCHB'    "838360
      i_active  = xnomchb ). "838360


*    CALL METHOD oref_settings->set( i_element = 'XONUL'
*                                    i_active  =  xonul   ).
*
*    CALL METHOD oref_settings->set( i_element = 'XVBST'
*                                    i_active  =  XVBST   ).
*    CALL METHOD oref_settings->set( i_element = 'XNVBST'
*                                    i_active  =  xnvbst  ).

*   save the list categories                                "n599218
  CALL METHOD oref_settings->set(
      i_element = 'PA_WDZER'  "n599218
      i_active  = pa_wdzer ). "n599218
  CALL METHOD oref_settings->set(
      i_element = 'PA_WDZEW'  "n599218
      i_active  = pa_wdzew ). "n599218
  CALL METHOD oref_settings->set(
      i_element = 'PA_WDWIZ'  "n599218
      i_active  = pa_wdwiz ). "n599218
  CALL METHOD oref_settings->set(
      i_element = 'PA_WDWUW'  "n599218
      i_active  = pa_wdwuw ). "n599218
  CALL METHOD oref_settings->set(
      i_element = 'PA_WDWEW'  "n599218
      i_active  = pa_wdwew ). "n599218

  CALL METHOD oref_settings->set(
      i_element = 'PA_NDSTO'  "n599218
      i_active  = pa_ndsto ). "n599218
  CALL METHOD oref_settings->set(
      i_element = 'PA_NDZER'  "n599218
      i_active  = pa_ndzer ). "n599218

  CALL METHOD oref_settings->set(
      i_element = 'XSUM'
      i_active  = xsum ).
  CALL METHOD oref_settings->set(
      i_element = 'PA_SUMFL'
      i_active  = pa_sumfl ).

  CALL METHOD oref_settings->set(
      i_element = 'NOSTO'
      i_active  = nosto ).

  CALL METHOD oref_settings->flush.

*   carry out the database updates only; the normal commit  "n599218
*   command does not allow to record this transaction for   "n599218
*   a batch input session using transaction SHDB            "n599218
  CALL FUNCTION 'DB_COMMIT'. "n599218

ENDFORM.                     "esdus_save_parameters         "n547170

*-----------------------------------------------------------"n547170
                                                            "n599218 A
*&---------------------------------------------------------------------*
*&      Form  check_is_oil_system
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM check_is_oil_system.                                   "n599218 A
                                                            "n599218 A
  MOVE  'OI001'              TO  g_f_dcobjdef_name.         "n599218 A
  CLEAR : g_flag_is_oil_active, g_cnt_is_oil.               "n599218 A
                                                            "n599218 A
* does database OI001 exist in this system ?                "n599218 A
  CALL FUNCTION 'DDIF_NAMETAB_GET'                         "n599218 A
    EXPORTING                                               "n599218 A
      tabname   = g_f_dcobjdef_name                 "n599218 A
    TABLES                                                  "n599218 A
      x031l_tab = g_t_x031l                         "n599218 A
    EXCEPTIONS                                              "n599218 A
      OTHERS    = 1.                                "n599218 A
                                                            "n599218 A
  CHECK sy-subrc IS INITIAL.      "OI001 is available ?     "n599218 A
                                                            "n599218 A
* check definition of MM document item MSEG                 "n599218 A
  MOVE  'MSEG'               TO  g_f_dcobjdef_name.         "n599218 A
                                                            "n599218 A
  CALL FUNCTION 'DDIF_NAMETAB_GET'                          "n599218 A
    EXPORTING                                               "n599218 A
      tabname   = g_f_dcobjdef_name                 "n599218 A
    TABLES                                                  "n599218 A
      x031l_tab = g_t_x031l                         "n599218 A
    EXCEPTIONS                                              "n599218 A
      OTHERS    = 1.                                "n599218 A
                                                            "n599218 A
  CHECK sy-subrc IS INITIAL.      "structure MSEG found     "n599218 A
                                                            "n599218 A
* check whether the IS-OIL specific fields are available    "n599218 A
  LOOP AT g_t_x031l          INTO  g_s_x031l.               "n599218 A
    CASE  g_s_x031l-fieldname.                              "n599218 A
      WHEN  'OIGLCALC'.                                     "n599218 A
        ADD   1              TO  g_cnt_is_oil.              "n599218 A
                                                            "n599218 A
      WHEN  'OIGLSKU'.                                      "n599218 A
        ADD   2              TO  g_cnt_is_oil.              "n599218 A
    ENDCASE.                                                "n599218 A
  ENDLOOP.                                                  "n599218 A
                                                            "n599218 A
* in the case structure MSEG comprises both fields          "n599218 A
* -> activate the IS-OIL function                           "n599218 A
  IF    g_cnt_is_oil = 3.                                   "n599218 A
    MOVE  'X'                TO  g_flag_is_oil_active.      "n599218 A
  ENDIF.                                                    "n599218 A
                                                            "n599218 A
ENDFORM.                     "check_is_oil_system.          "n599218 A
                                                            "n599218 A
*----------------------------------------------------------------------*
*  calculate_offsets.
*----------------------------------------------------------------------*

* calculate the offsets for the list header

FORM calculate_offsets.

*  working area
  DATA : l_text(132)         TYPE c.

* get the maximal length of the text elements to be used
  PERFORM  get_max_text_length USING  TEXT-020.
  PERFORM  get_max_text_length USING  TEXT-021.
  PERFORM  get_max_text_length USING  TEXT-022.
  PERFORM  get_max_text_length USING  TEXT-023.
  PERFORM  get_max_text_length USING  TEXT-025.

  g_offset_header            =  g_f_length_max + 3.

  CLEAR                      g_f_length_max.

  IF  bwbst IS INITIAL.
*     stocks and quantities only
    MOVE   TEXT-007          TO  g_date_line_from-text.
    WRITE : datum-low        TO  g_date_line_from-datum DD/MM/YYYY.
    CONDENSE                 g_date_line_from.
    PERFORM  get_max_text_length USING  g_date_line_from.

    MOVE  TEXT-005           TO  g_text_line-text.
    PERFORM  get_max_text_length USING  g_text_line.

    MOVE  TEXT-006           TO  g_text_line.
    PERFORM  get_max_text_length USING  g_text_line.

    MOVE   TEXT-007          TO  g_date_line_to-text.
    WRITE : datum-high       TO  g_date_line_to-datum DD/MM/YYYY.
    CONDENSE                 g_date_line_to.
  ELSE.
*     stocks, quantities, and values
    MOVE   TEXT-008          TO  g_date_line_from-text.
    WRITE : datum-low        TO  g_date_line_from-datum DD/MM/YYYY.
    CONDENSE                 g_date_line_from.

*     the start and end dates were shown incorrectly in the "n856424
*     headlines in the mode valuated stock                  "n856424
    PERFORM  get_max_text_length USING  g_date_line_from.   "n856424

    MOVE  TEXT-030           TO  g_text_line-text.
    PERFORM  get_max_text_length USING  g_text_line.

    MOVE  TEXT-031           TO  g_text_line-text.
    PERFORM  get_max_text_length USING  g_text_line.

    MOVE   TEXT-008          TO  g_date_line_to-text.
    WRITE : datum-high       TO  g_date_line_to-datum DD/MM/YYYY.
    CONDENSE                 g_date_line_to.
  ENDIF.

* calculate the offsets for the following columns
  g_offset_qty               =  g_f_length_max +  2.
  g_offset_unit              =  g_offset_qty   + 25.
  g_offset_value             =  g_offset_unit  +  8.
  g_offset_curr              =  g_offset_value + 25.
  LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_19\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
* Offset for quantities in PUM
  gv_/cwm/offset_qty          =  g_offset_unit       +  8.
  gv_/cwm/offset_unit         =  gv_/cwm/offset_qty  + 25.

*  ENHANCEMENT-POINT ehp605_rm07mlbd_19 SPOTS es_rm07mlbd .

ENDFORM.                     " calculate_offsets.

*----------------------------------------------------------------------*
*    get_max_text_length
*----------------------------------------------------------------------*

FORM get_max_text_length         USING l_text TYPE any.

  g_f_length = strlen( l_text ).

  IF  g_f_length > g_f_length_max.
    MOVE  g_f_length         TO  g_f_length_max.
  ENDIF.

ENDFORM.                     " get_max_text_length

*----------------------------------------------------------------------*

* contains FORM routines without preprocessor commands and  "n547170
* no text elements                                          "n547170
INCLUDE zrm07mlbd_form_01.
*INCLUDE                      rm07mlbd_form_01.              "n547170

INCLUDE zrm07mlbd_form_02.
*INCLUDE                      rm07mlbd_form_02.              "n547170

*----------------------------------------------------------------------*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_aistr.          "n1481757
*                                                            "n1481757
** look and show the active archive info structures F4 help      "n1481757
*                                                            "n1481757
*  MOVE   'X'                 TO  g_f_f4_mode.               "n1481757
*  EXPORT  g_f_f4_mode        TO  MEMORY ID 'MB51_F4_MODE'.  "n1481757
*                                                            "n1481757
** start this report in F4 mode without any parameters           "n1481757
*  SUBMIT ('RM07DOCS')         AND RETURN.     "#EC CI_SUBMIT  "n1511550
*                                                            "n1481757
** get the selected archive info structure                       "n1481757
*  IMPORT  g_f_f4_archindex   FROM  MEMORY                   "n1481757
*                             ID 'MB51_F4_ARCHINDEX'.        "n1481757
*  MOVE    g_f_f4_archindex   TO  pa_aistr.                  "n1481757
*                                                            "n1481757
*  CLEAR                      g_f_f4_mode.                   "n1481757
*  EXPORT  g_f_f4_mode        TO  MEMORY ID 'MB51_F4_MODE'.  "n1481757
*                                                            "n1481757
** save archive info structure for the next run                  "n1481757
*  IF  archive   =  'X'.                                     "n1481757
*    IF  sy-batch IS INITIAL.  " only in dialog mode             "n1481757
*                                                            "n1481757
*      IF NOT oref_settings IS INITIAL.                      "n1481757
**       this object is already known -> Save the settings       "n1481757
*        CALL METHOD                                         "n1481757
*          oref_settings->set( i_element = 'PA_AISTR'        "n1481757
*                              i_active  =  pa_aistr  ).     "n1481757
*                                                            "n1481757
*        CALL METHOD oref_settings->flush. "n1481757
*                                                            "n1481757
**       carry out the database updates only; the normal         "n1481757
**       commit command does not allow to record this            "n1481757
**       transaction for a batch input session using             "n1481757
**       transaction SHDB                                        "n1481757
*        CALL FUNCTION 'DB_COMMIT'. "n1481757
*      ENDIF.                                                "n1481757
*    ENDIF.                                                  "n1481757
*  ENDIF.                                                    "n1481757

*----- end of note 1481757 ----  F4-Help ----- get info-structure -----*


************************ HAUPTPROGRAMM *********************************

*---------------- F4-Hilfe für Reportvariante -------------------------*


*&--------------------------------------------       "v hana_20120802
*&      Form  NEW_DB_RUN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM new_db_run .

  REFRESH datum.  "delete existing entries because ...
  APPEND datum.   ".. relevant data is appended here
  TRY.
      CALL BADI gr_badi_rm07mlbd_dbsys_opt->calculate_stocks
        EXPORTING
          it_material          = matnr[]
          it_company_code      = bukrs[]
          it_plant             = g_ra_werks[]                 "2053423
          it_storage_location  = lgort[]
          it_batch             = charg[]
          it_valuation_type    = bwtar[]
          it_movement_type     = bwart[]
          it_posting_date      = datum[]
          iv_special_stock_ind = sobkz
          iv_batch_stock       = lgbst
          iv_valuated_stock    = bwbst
          iv_special_stock     = sbbst
          iv_batch_mat_only    = xchar
          iv_batch_no_mchb     = xnomchb
          iv_no_reversals      = nosto
        IMPORTING
          et_stock_inventory   = gt_stock_inventory
          ev_no_dbsys_opt      = gv_no_dbsys_opt.
    CATCH cx_badi.
      IF p_aut EQ space.
* Code injection for ABAP UNIT TEST
* see local class of CL_IM_RM07MLBD_DBSYS_OPT
* The Unittest shall result in an error in case of error in BADI
        gv_newdb = abap_false.
      ENDIF.
  ENDTRY.
  IF gv_no_dbsys_opt = abap_true.
    IF p_aut EQ space.
* Code injection for ABAP UNIT TEST
* see local class of CL_IM_RM07MLBD_DBSYS_OPT
* The Unittest shall result in an error in case of error in BADI
      gv_newdb = abap_false.
    ENDIF.
  ELSE.
    LOOP AT gt_stock_inventory ASSIGNING <gs_stock_inventory>.
      MOVE-CORRESPONDING <gs_stock_inventory> TO bestand.
      MOVE-CORRESPONDING <gs_stock_inventory> TO g_s_makt.
      APPEND bestand.
      APPEND g_s_makt TO g_t_makt.
      CLEAR <gs_stock_inventory>-maktx. "to compare it to bestand in AUT
    ENDLOOP.
* if result is empty, call subroutines to get the detailed error messages "1784986
    IF gv_newdb = abap_true AND sy-subrc NE 0.              "1784986
      gv_newdb = abap_false.                                "1784986
      PERFORM aktuelle_bestaende.                           "1784986
      PERFORM f1000_select_mseg_mkpf.                       "1784986
      gv_newdb = abap_true.                                 "1784986
    ENDIF.                                                  "1784986
    SORT g_t_makt BY matnr.
    DELETE ADJACENT DUPLICATES FROM g_t_makt.
  ENDIF.

ENDFORM.                    " new_db_run             "^ hana_20120802
*&---------------------------------------------------------------------*
*&      Form  Check_Ui_opti_Badi
*&---------------------------------------------------------------------*
*       check active implementation for UI enhancement note  1790231
*----------------------------------------------------------------------*
FORM check_ui_opti_badi.                                    "1790231

* check if BADI has been activated
  DATA: lo_ui_opti_badi TYPE REF TO mm_ui_optimizations.

  GET BADI lo_ui_opti_badi.
  CALL BADI lo_ui_opti_badi->is_active
    EXPORTING
      iv_reportname = sy-repid
    RECEIVING
      rv_active     = gv_ui_opt_active.

ENDFORM.                    "Check_Ui_opti_Badi             "1790231

INCLUDE zrm07mlbd_zforms.
