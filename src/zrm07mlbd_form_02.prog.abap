*&---------------------------------------------------------------------*
*&  Include           RM07MLBD_FORM_02                                 *
*&---------------------------------------------------------------------*

* correction Oct. 2008 TW                                  "n1265674
* for active ingredient materials MB5B should not display  "n1265674
* the 141 and 142 movements for the selection valuated     "n1265674
* stock to avoid wrong beginning stock amount.             "n1265674

* correction May  2007 MS                                  "n1049935
* authority check improved, do only execute authority-check"n1049935
* for plants which are selected, not always for all plants "n1049935
* This is a performance improvement as well.               "n1049935

* correction Nov. 2006 TW                                   "n999530
* plant description should appear behind plant number but   "n999530
* nevertheless the plant description should not be vissible "n999530
* for all possible selection combinations the transaction   "n999530
* MB5L could be started for.                                "n999530

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

* correction Nov. 2005 MM                                   "n890109
* allow the inter active functions 'Specify drill-down'     "n890109
* and 'Choose' from the menu 'Settings -> Summation levels' "n890109

* MB5B improved regarding accessibilty                      "n773673

* Improvements :                       March 2003 MM        "n599218
* - send warning M7 689 when user does not restric the      "n599218
*   database in dialog or print mode                        "n599218
* - error message 'programmfehler' improved                 "n599218

* contains FORM routines without preprocessor commands and  "n547170
* no text elements                                          "n547170

*-- begin of note 1481757 ---------------------------------------------*
* definition of the interface table for the transposrt      "n1481757
* of the select-option for archive access                   "n1481757
TYPES : BEGIN OF ty_frange,                                 "n1481757
          fieldname TYPE  fieldname,                        "n1481757
          selopt_t  TYPE STANDARD TABLE OF                  "n1481757
                            rsdsselopt                      "n1481757
                            WITH DEFAULT KEY,               "n1481757
        END OF ty_frange.                                   "n1481757
                                                            "n1481757
DATA: g_s_selopt TYPE rsdsselopt,                           "n1481757
      g_s_frange TYPE ty_frange,                            "n1481757
      g_t_frange TYPE  TABLE OF  ty_frange.                 "n1481757
                                                            "n1481757
DATA: g_t_selrange LIKE g_t_frange,                         "n1481757
      g_s_selrange LIKE g_s_frange.                         "n1481757

TYPES : stab_frange         TYPE STANDARD TABLE OF          "n1481757
                            ty_frange WITH DEFAULT KEY.     "n1481757

**  table with the index key for reading the AS archiv      "n1481757
TYPES: BEGIN OF stype_as_key,                               "n1481757
         archivekey LIKE  mkpf_aridx-arkey,                 "n1481757
         archiveofs LIKE  mkpf_aridx-archoffset,            "n1481757
         mblnr      LIKE  mkpf-mblnr,                       "n1481757
         mjahr      LIKE  mkpf-mjahr,                       "n1481757
       END OF stype_as_key,                                 "n1481757
                                                            "n1481757
       stab_as_key TYPE STANDARD TABLE OF                   "n1481757
                             stype_as_key                   "n1481757
                             WITH DEFAULT KEY.              "n1481757

DATA : g_t_as_key TYPE  stab_as_key,                        "n1481757
       g_s_as_key TYPE  stype_as_key.                       "n1481757

DATA: g_f_afcat             LIKE  aind_str1-skey.           "n1481757

* data definitions for AS archive                           "n1481757
TYPES: BEGIN OF stype_aind_str1,                            "n1481757
         archindex LIKE  aind_str1-archindex,               "n1481757
         itype     LIKE  aind_str1-itype,                   "n1481757
         skey      LIKE  aind_str1-skey,                    "n1481757
       END OF stype_aind_str1,                              "n1481757
                                                            "n1481757
       stab_aind_str1 TYPE STANDARD TABLE OF                "n1481757
                             stype_aind_str1                "n1481757
                             WITH DEFAULT KEY.              "n1481757
                                                            "n1481757
DATA : g_s_aind_str1_fc  TYPE  stype_aind_str1,             "n1481757
       g_t_aind_str1_fc  TYPE  stab_aind_str1,              "n1481757
       g_s_aind_str1_ais TYPE  stype_aind_str1,             "n1481757
       g_t_aind_str1_ais TYPE  stab_aind_str1.              "n1481757

* working tables with header lines for MM doc MKPF and MSEG "n1481757
DATA : BEGIN OF xmkpf        OCCURS 0.                      "n1481757
    INCLUDE STRUCTURE   mkpf.                               "n1481757
DATA : END OF xmkpf.                                        "n1481757
                                                            "n1481757
DATA : BEGIN OF xmseg        OCCURS 0.                      "n1481757
    INCLUDE STRUCTURE   mseg.                               "n1481757
DATA : END OF xmseg.                                        "n1481757
                                                            "n1481757

* hash-table for tied empties                               "n1481757
                                                            "n1481757
TYPES: BEGIN OF ts_mmdocs_arch,                             "n1481757
         mblnr      LIKE mkpf-mblnr,                        "n1481757
         mjahr      LIKE mkpf-mjahr,                        "n1481757
         archivekey LIKE mkpf_aridx-arkey,                  "n1481757
         offset     LIKE mkpf_aridx-archoffset,             "n1481757
       END OF ts_mmdocs_arch.                               "n1481757
                                                            "n1481757
DATA: ht_mmdocs_arch TYPE HASHED TABLE OF ts_mmdocs_arch    "n1481757
                     WITH UNIQUE KEY mblnr mjahr,
      wa_hashtable   LIKE LINE OF ht_mmdocs_arch.           "n1481757

DATA: gt_ra_xauto TYPE RANGE OF xauto.

DATA:  g_t_mseg_key_te       TYPE  stab_mseg_xauto.
*DATA: lv_sum_stock    TYPE mchb-clabs.                      "n2675738


*---- end of note 1481757 ---------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  PF_STATUS_SET
*&---------------------------------------------------------------------*

FORM status                                      "#EC CALLED  "n1511550
  USING  extab               TYPE slis_t_extab.             "n1511550

  DATA : l_s_extab           TYPE      slis_extab.

* set the buttons and functions for the interactions        "n890109
* 'Specify drill-down' etc.. depending on the content of    "n890109
* "g_cust_sum_levels"                                       "n890109
                                                            "n890109
  IF  g_cust_sum_levels IS INITIAL.                         "n890109
*   inactivate these functions in the menue, because these  "n890109
*   functions are not transferred correctly from ALV list   "n890109
*   to the next ALV list                                    "n890109
    MOVE  : '&XPA'           TO  l_s_extab-fcode. "Expand   "n890109
    APPEND  l_s_extab        TO  extab.                     "n890109
    MOVE  : '&OMP'           TO  l_s_extab-fcode. "Aggregate"n890109
    APPEND  l_s_extab        TO  extab.                     "n890109
    MOVE  : '&KOM'           TO  l_s_extab-fcode. "Choose   "n890109
    APPEND  l_s_extab        TO  extab.                     "n890109
*   Define breakdown                                        "n890109
    MOVE  : '&AUF'           TO  l_s_extab-fcode.           "n890109
    APPEND  l_s_extab        TO  extab.                     "n890109
  ENDIF.                                                    "n890109

* filter is no more supported by the ALV                    "1509405
  IF g_f_cnt_lines GT 1.                                    "1509405
    MOVE  : '&ILT'           TO  l_s_extab-fcode. "Filter  "1509405
    APPEND  l_s_extab        TO  extab.                     "1509405
  ENDIF.                                                    "1509405

  SET PF-STATUS 'STANDARD_FULLSCREEN'   EXCLUDING extab.
*  SET PF-STATUS 'STANDARD'   EXCLUDING extab.

ENDFORM.                     "STATUS

*&---------------------------------------------------------------------*
*&      Form  LISTUMFANG
*&---------------------------------------------------------------------*
*       Prüfung gegen Listumfangsparameter auf Selektionsbild
*----------------------------------------------------------------------*

FORM listumfang.

  DATA : l_category(08)      TYPE c.                        "n599218
  FIELD-SYMBOLS : <l_fs>.                                   "n599218
                                                            "n599218
* carry out the check according the list categories in the  "n599218
* case at least one category is not active                  "n599218
  CHECK : NOT g_cnt_empty_parameter IS INITIAL.             "n599218
*                                                           "n599218
* cat. I docs I stock on   I    I stock on I Parameter      "n599218
*      I      I start date I    I end date I                "n599218
* -----+------+------------+----+----------+----------      "n599218
*  1   I yes  I =  zero    I =  I =  zero  I pa_wdzer       "n599218
*  2   I yes  I =  zero    I <> I <> zero  I pa_wdzew       "n599218
*  3   I yes  I <> zero    I <> I =  zero  I pa_wdwiz       "n599218
*  4   I yes  I <> zero    I <> I <> zero  I pa_wdwuw       "n599218
*  5   I yes  I <> zero    I =  I <> zero  I pa_wdwew       "n599218
*      I      I            I    I          I                "n599218
*  6   I no   I =  zero    I =  I =  zero  I pa_ndzer       "n599218
*  7   I no   I <> zero    I =  I <> zero  I pa_ndsto       "n599218
  LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_RM07MLBD_FORM_02_01\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
                                                            "n599218
* process table BESTAND                                     "n599218
  LOOP AT bestand.                                          "n599218
    CLEAR                    l_category.                    "n599218
                                                            "n599218
*   determine the category of each entry                    "n599218

    IF  bestand-soll      IS INITIAL    AND
        bestand-haben     IS INITIAL    AND
        bestand-/cwm/soll      IS INITIAL    AND
        bestand-/cwm/haben     IS INITIAL    AND
        bestand-sollwert  IS INITIAL    AND
        bestand-habenwert IS INITIAL.
*     material without movements

      IF  bestand-endmenge  IS INITIAL  AND
          bestand-anfmenge  IS INITIAL  AND
          bestand-/cwm/endmenge  IS INITIAL  AND
          bestand-/cwm/anfmenge  IS INITIAL  AND
          bestand-anfwert   IS INITIAL  AND
          bestand-endwert   IS INITIAL.
*       material without movements / no stocks
        MOVE  'PA_NDZER'     TO  l_category.
      ELSE.
*       material without movements / with stocks
        MOVE  'PA_NDSTO'     TO  l_category.
      ENDIF.
    ELSE.
*     material with movements
*
      IF      bestand-anfmenge  IS INITIAL  AND
              bestand-/cwm/anfmenge  IS INITIAL  AND
              bestand-anfwert   IS INITIAL.
*       stock and value on start date are zero
        IF   bestand-endmenge  IS INITIAL  AND
             bestand-/cwm/endmenge  IS INITIAL  AND
             bestand-endwert   IS INITIAL.
*         stock and value on end date are zero, too
          MOVE  'PA_WDZER'     TO  l_category.
        ELSE.
*         stock and value on end date <> zero
          MOVE  'PA_WDZEW'     TO  l_category.
        ENDIF.
      ELSE.
*       stock and value on start date <> zero
        IF     bestand-endmenge  IS INITIAL  AND
               bestand-/cwm/endmenge  IS INITIAL  AND
               bestand-endwert   IS INITIAL.
*         stock and value on end date are zero
          MOVE  'PA_WDWIZ'   TO  l_category.
        ELSEIF bestand-endmenge = bestand-anfmenge  AND
               bestand-/cwm/endmenge = bestand-/cwm/anfmenge  AND
               bestand-anfwert  = bestand-endwert.
*         stock and value on end date are equal
          MOVE  'PA_WDWEW'   TO  l_category.
        ELSE.
*         stock and values on end date are differnt
          MOVE  'PA_WDWUW'   TO  l_category.
        ENDIF.
      ENDIF.
    ENDIF.
*ENHANCEMENT-POINT ehp605_rm07mlbd_form_02_01 SPOTS es_rm07mlbd .

*   evaluate category and corresponding parameter settings  "n599218
    CHECK : NOT l_category IS INITIAL.                      "n599218
    ASSIGN (l_category)      TO  <l_fs>.                    "n599218
                                                            "n599218
    IF  sy-subrc IS INITIAL.                                "n599218
      IF  <l_fs> IS INITIAL.                                "n599218
        DELETE               bestand.                       "n599218
      ENDIF.                                                "n599218
    ENDIF.                                                  "n599218
  ENDLOOP.                                                  "n599218
ENDFORM.                               " LISTUMFANG

*&---------------------------------------------------------------------*
*&      Form  LISTAUSGABE1
*&---------------------------------------------------------------------*

FORM listausgabe1.

  IF  g_cust_color = 'X'.              "colorize numeric fields ?
    layout-coltab_fieldname = 'FARBE_PRO_FELD'.
  ELSE.
    layout-info_fieldname   = 'FARBE_PRO_ZEILE'.
  ENDIF.

  layout-f2code = '9PBP'.
  IF NOT bwbst IS INITIAL.
    layout-min_linesize = '92'.
  ENDIF.

  event_exit-ucomm = '&XP1'.
  event_exit-before = 'X'.
  APPEND event_exit.

  IF  g_flag_break-b8 = 'X'.                                "n921164
    BREAK-POINT              ID mmim_rep_mb5b.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_interface_check        = g_flag_i_check       "n599218
      i_callback_program       = repid
      i_callback_pf_status_set = 'STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = layout
      it_fieldcat              = fieldcat[]
      it_special_groups        = gruppen[]
      it_sort                  = sorttab[]
      it_filter                = filttab[]
      i_default                = 'X'
      i_save                   = 'A'
      is_variant               = variante
      it_events                = events[]
      it_event_exit            = event_exit[]
     is_print                 = g_s_print
    TABLES
      t_outtab                 = g_t_belege1
    EXCEPTIONS
*     program_error            = 1
      OTHERS                   = 2.

* does the ALV return with an error ?
  IF  NOT sy-subrc IS INITIAL.         "Fehler vom ALV ?
    MESSAGE ID sy-msgid TYPE  'S'     NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                               " LISTAUSGABE1

*----------------------------------------------------------------------*
*    F0000_CREATE_TABLE_G_T_ORGAN
*----------------------------------------------------------------------*

FORM f0000_create_table_g_t_organ
                   USING     l_f_error TYPE stype_organ-keytype.

* at least on of these 3 range tables must be filled for the creation
* of table g_t_organ
* field  description         range table
* -----  -----------         -----------
* werks  plant               g_0000_ra_plant
* bwkey  valuation area      g_0000_ra_bwkey
* bukrs  company code        g_0000_ra_plant

* table g_t_organ must be empty
  CHECK : g_t_organ[] IS INITIAL.                           "n433765

* START OF CHANGE for note 1049935                         "n1049935
* First check if there was a selection for company code    "n1049935
* then do the F9000_AUTH_PLANT_CHECK only for necessary    "n1049935
* selected items..                                         "n1049935

* select all plant from table t001w
  SELECT * FROM t001w        WHERE  werks  IN  g_0000_ra_werks.

*   check the valuation area
    CHECK : t001w-bwkey IN g_0000_ra_bwkey.

*   read the valuation area
    SELECT SINGLE * FROM t001k
                             WHERE  bwkey  =  t001w-bwkey.

    CHECK : sy-subrc IS INITIAL.       "entry found ?

*   company code is required ?
    CHECK : t001k-bukrs IN g_0000_ra_bukrs.

    SELECT SINGLE * FROM t001  WHERE  bukrs  =  t001k-bukrs.

    CHECK : sy-subrc IS INITIAL.       "entry found ?


    PERFORM  f9000_auth_plant_check    USING  t001w-werks.

*   go on if the user has authority for this plant
    CHECK : g_flag_authority = 'X'.
* END OF CHANGE for note 1049935                           "n1049935

*   create table g_t_organ_lean
    MOVE : t001w-werks       TO  g_s_organ-werks,
           t001w-bwkey       TO  g_s_organ-bwkey,
           t001-bukrs        TO  g_s_organ-bukrs,
           t001-ktopl        TO  g_s_organ-ktopl,
           t001k-bwmod       TO  g_s_organ-bwmod,
           t001-waers        TO  g_s_organ-waers.

*   write 2 entries for the both search methods _
*   1. with key   valuation area
*   2. with key   plant
    MOVE-CORRESPONDING  g_s_organ
                             TO  g_t_organ.
    MOVE : c_bwkey           TO  g_t_organ-keytype,
           g_s_organ-bwkey   TO  g_t_organ-keyfield.
    APPEND                   g_t_organ.

    MOVE : c_werks           TO  g_t_organ-keytype,
         g_s_organ-werks     TO  g_t_organ-keyfield.
    APPEND                   g_t_organ.
    CLEAR                    g_s_organ.

*   create the range tables for plants
    MOVE : t001w-werks       TO  g_ra_werks-low,
           'I'               TO  g_ra_werks-sign,
           'EQ'              TO  g_ra_werks-option.
    APPEND                   g_ra_werks.

*   create the range tables for plants and valuation areas
    MOVE : t001k-bwkey       TO  g_ra_bwkey-low,
           'I'               TO  g_ra_bwkey-sign,
           'EQ'              TO  g_ra_bwkey-option.
    APPEND                   g_ra_bwkey.
  ENDSELECT.

* is table g_t_organ empty ?
  IF  g_t_organ[] IS INITIAL.                               "n433765
*   no plants for selection found / process error message ?
    IF  l_f_error = c_error.
      MESSAGE e281.
*     Kein Eintrag zur Selektion Buchungskreis Werk Lagerort vorhanden
    ENDIF.
  ELSE.
    SORT  g_t_organ          BY  keytype  keyfield  bwkey  werks.
    DELETE ADJACENT DUPLICATES FROM g_t_organ.

    SORT                     g_ra_werks.
    DELETE ADJACENT DUPLICATES FROM g_ra_werks.

    SORT                     g_ra_bwkey.
    DELETE ADJACENT DUPLICATES FROM g_ra_bwkey.
  ENDIF.

ENDFORM.                     "f0000_create_table_g_t_organ

*----------------------------------------------------------------------*
*    F0300_GET_FIELDS
*----------------------------------------------------------------------*

FORM f0300_get_fields.

  DATA : l_f_type(01)        TYPE c.

* find out the fields of structure g_s_mseg_lean
  DESCRIBE FIELD g_s_mseg_lean INTO g_t_td.

  LOOP AT g_t_td-names       INTO  g_s_nameinfo.
*   select all entries who contain 'MSEG-' oder 'MKPF-'
    CASE  g_s_nameinfo-name(05).
      WHEN  'MKPF-'.
        MOVE    c_tilde      TO  g_s_nameinfo-name+4(01).
        APPEND  g_s_nameinfo-name
                             TO  g_t_mseg_fields.

      WHEN  'MSEG-'.
        MOVE  : g_s_nameinfo-name
                             TO  g_s_mseg_fields-fieldname,
                c_tilde      TO  g_s_mseg_fields-fieldname+4(01).
        APPEND  g_s_mseg_fields        TO  g_t_mseg_fields.

      WHEN  OTHERS.
    ENDCASE.
  ENDLOOP.
                                                            "n1784874
                                                            "n599218 A
  IF  g_flag_is_oil_active = 'X'.           "IS-OIL ?       "n599218 A
*   the 2 IS-OIL specific data fields will be inserted into "n599218 A
*   working table G_T_MSEG_FIELDS. Then these fields will   "n599218 A
*   transported from database table MSEG, too               "n599218 A
    APPEND  'MSEG~OIGLCALC'  TO  g_t_mseg_fields.           "n599218 A
    APPEND  'MSEG~OIGLSKU'   TO  g_t_mseg_fields.           "n599218 A
  ENDIF.                                                    "n599218 A
                                                            "n1784874

* serious error if table g_t_mseg_field does not contain fields
  IF  g_t_mseg_fields[] IS INITIAL.                         "n599218
    MESSAGE e895                                     "#EC *    "n599218
      WITH 'Error, contact system administrator'.    "#EC *    "n599218
  ELSE.
    SORT                     g_t_mseg_fields.
    DELETE ADJACENT DUPLICATES FROM  g_t_mseg_fields.
  ENDIF.

ENDFORM.                     "f0300_get_fields.

*----------------------------------------------------------------------*
*    F0500_APPEND_RA_SOBKZ
*----------------------------------------------------------------------*

FORM f0500_append_ra_sobkz
                   USING     l_f_sobkz LIKE      mseg-sobkz.

* create ranges table with special stock indicator
  CLEAR                      g_ra_sobkz.
  MOVE : l_f_sobkz           TO  g_ra_sobkz-low,
         'I'                 TO  g_ra_sobkz-sign,
         'EQ'                TO  g_ra_sobkz-option.
  APPEND                     g_ra_sobkz.

ENDFORM.                     "f0500_append_ra_sobkz

*----------------------------------------------------------------------*
*    F0600_CREATE_RANGE_LGORT
*----------------------------------------------------------------------*

FORM f0600_create_range_lgort.

  REFRESH                    g_ra_lgort.
  CLEAR                      g_ra_lgort.

  IF      lgbst = 'X'.       "only Storage loc./batch stock
*   copy the existing select-options
    MOVE lgort[]               TO  g_ra_lgort[].

*   add an exclusion for storage location = space
    MOVE : 'E'               TO  g_ra_lgort-sign,
           'EQ'              TO  g_ra_lgort-option.
    APPEND                   g_ra_lgort.

  ELSEIF  bwbst = 'X'.       "only valuated stocks
*   copy the existing select-options
    MOVE lgort[]               TO  g_ra_lgort[].

  ELSEIF  sbbst = 'X'.       "only special stocks
    IF      sobkz  =  'O'  OR
            sobkz  =  'V'  OR
            sobkz  =  'W'.
*     only records with storage location = space allowed
      MOVE : 'I'             TO  g_ra_lgort-sign,
             'EQ'            TO  g_ra_lgort-option.
      APPEND                 g_ra_lgort.
    ELSE.
*     Copy the existing select-options
      MOVE lgort[]             TO  g_ra_lgort[].
    ENDIF.
  ENDIF.

ENDFORM.                     "f0600_create_range_lgort

*-----------------------------------------------------------"n547170
*    f0700_prepare_tied_empties.                            "n547170
*-----------------------------------------------------------"n547170
*
* this flag will be set after the found MM doc items
* contain at least one of these values in indicator
* MSEG-XAUTO
*                                                           "n547170
* 2.1 stock type = storage location / batch stock           "n547170
*     use : 'F', 'L', 'M', and 'W'                          "n547170
*                                                           "n547170
* 2.2 stock type = valuated stock                           "n547170
*     use : 'F', 'L', 'M', 'W', 'S', and 'U'                "n547170
*                                                           "n547170
*-----------------------------------------------------------"n547170
                                                            "n547170
*&---------------------------------------------------------------------*
*&      Form  f0700_prepare_tied_empties
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f0700_prepare_tied_empties.                            "n547170
                                                            "n547170
  MOVE : 'F'                 TO  g_ra_xauto-low,            "n547170
         space               TO  g_ra_xauto-high,           "n547170
         'EQ'                TO  g_ra_xauto-option,         "n547170
         'I'                 TO  g_ra_xauto-sign.           "n547170
  APPEND                     g_ra_xauto.                    "n547170
                                                            "n547170
  MOVE : 'L'                 TO  g_ra_xauto-low.            "n547170
  APPEND                     g_ra_xauto.                    "n547170
                                                            "n547170
  MOVE : 'M'                 TO  g_ra_xauto-low.            "n547170
  APPEND                     g_ra_xauto.                    "n547170
                                                            "n547170
  MOVE : 'W'                 TO  g_ra_xauto-low.            "n547170
  APPEND                     g_ra_xauto.                    "n547170
                                                            "n547170
  IF  NOT bwbst IS INITIAL.                                 "n547170
*   plus these for stock type = valuated stock              "n547170
    MOVE : 'S'               TO  g_ra_xauto-low.            "n547170
    APPEND                   g_ra_xauto.                    "n547170
                                                            "n547170
    MOVE : 'U'               TO  g_ra_xauto-low.            "n547170
    APPEND                   g_ra_xauto.                    "n547170
  ENDIF.                                                    "n547170
                                                            "n547170
ENDFORM.                     "f0700_prepare_tied_empties.   "n547170
                                                            "n547170
*-----------------------------------------------------------"n547170
*    f0800_check_restrictions                               "n547170
*-----------------------------------------------------------"n547170
* check whether FI summarization is active and other        "n547170
* restrictions could deliver wrong results                  "n547170
*-----------------------------------------------------------"n547170
                                                            "n547170
*&---------------------------------------------------------------------*
*&      Form  f0800_check_restrictions
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f0800_check_restrictions.                              "n547170
                                                            "n547170
  DATA : l_flag_m7390(01) TYPE c,
         l_flag_m7482(01) TYPE c,
         lv_dummy         TYPE string.                      "n1481757

* - if FI summarization is active process warning M7 390    "n497992
*   for stock type = valuated stock                         "n497992
  IF  NOT bwbst IS INITIAL.                                 "n497992

    IF archive = 'X'.                                       "n1481757
*       emerge warning ?                                    "n1481757
      CALL FUNCTION 'ME_CHECK_T160M'                   "n1481757
        EXPORTING                                       "n1481757
          i_arbgb     = 'M7'                       "n1481757
          i_msgnr     = '449'                      "n1481757
        EXCEPTIONS                                      "n1481757
          error       = 1                         "n1481757
          nothing     = 2                         "n1481757
          warning     = 3                         "n1481757
          success     = 4                         "n1481757
          popup       = 5                         "n1481757
          information = 6.                        "n1481757
                                                            "n1481757
      CASE sy-subrc.                                        "n1481757
        WHEN 1.                                             "n1481757
          MESSAGE e449.                                     "n1481757
* Kombination bewerteter Bestand und Lesen Archiv kann      "n1481757
* zu Fehlern führen                                         "n1481757
        WHEN 2.                                             "n1481757
          MESSAGE w449 INTO lv_dummy.                       "n1481757
* Kombination bewerteter Bestand und Lesen Archiv kann      "n1481757
* zu Fehlern führen                                         "n1481757
        WHEN 3.                                             "n1481757
          MESSAGE w449.                                     "n1481757
* Kombination bewerteter Bestand und Lesen Archiv kann      "n1481757
* zu Fehlern führen                                         "n1481757
        WHEN 4.                                             "n1481757
          MESSAGE s449.                                     "n1481757
* Kombination bewerteter Bestand und Lesen Archiv kann      "n1481757
* zu Fehlern führen                                         "n1481757
        WHEN 5.                                             "n1481757
          MESSAGE i449.                                     "n1481757
* Kombination bewerteter Bestand und Lesen Archiv kann      "n1481757
* zu Fehlern führen                                         "n1481757
        WHEN 6.                                             "n1481757
          MESSAGE i449.                                     "n1481757
* Kombination bewerteter Bestand und Lesen Archiv kann      "n1481757
* zu Fehlern führen                                         "n1481757
        WHEN OTHERS.                                        "n1481757
          MESSAGE w449.                                     "n1481757
*             MESSAGE w895 WITH text-138.                   "n1481757
* Kombination bewerteter Bestand und Lesen Archiv kann      "n1481757
* zu Fehlern führen                                         "n1481757
      ENDCASE.                                              "n1481757
    ENDIF.                                                  "n1481757

    BREAK-POINT                ID mmim_rep_mb5b.            "n921164
*   dynamic break-point : customizing for FI summarization "n921164
                                                            "n497992
*   reference procedures for checking FI summarization :    "n497992
*   MKPF, RMRP, MLHD, PRCHG                                 "n497992
    REFRESH                  g_ra_awtyp.                    "n547170
                                                            "n547170
    MOVE : 'EQ'              TO  g_ra_awtyp-option,         "n497992
           'I'               TO  g_ra_awtyp-sign,           "n497992
           'MKPF'            TO  g_ra_awtyp-low.            "n497992
    APPEND                   g_ra_awtyp.                    "n497992
                                                            "n497992
    MOVE   'RMRP'            TO  g_ra_awtyp-low.            "n497992
    APPEND                   g_ra_awtyp.                    "n497992
                                                            "n497992
    MOVE   'MLHD'            TO  g_ra_awtyp-low.            "n497992
    APPEND                   g_ra_awtyp.                    "n497992
                                                            "n497992
    MOVE   'PRCHG'           TO  g_ra_awtyp-low.            "n497992
    APPEND                   g_ra_awtyp.                    "n497992
                                                            "n497992
    SELECT * FROM ttypv                                     "n497992
      WHERE awtyp  IN g_ra_awtyp                            "n497992
      AND   tabname NE 'BSET'.                              "n1785687
                                                            "n1278202
      IF ttypv-awtyp = 'MKPF'.                              "n1278202
*       any entry from AWTYP = MKPF could lead to wrong     "n1278202
*       results -> send message                             "n1278202
        MOVE  'X'         TO  l_flag_m7390.                 "n1278202
        EXIT.                                               "n1278202
      ENDIF.                                                "n1278202
                                                            "n497992
      IF ttypv-fieldname = '*'      OR                      "n497992
         ttypv-fieldname = 'MATNR'.                         "n497992
*       avoid error reported by the code inspector : to
*       emerge this message during this SELECT - ENDSELECT
*       loop will create a problem for the database cursor
        MOVE  'X'         TO  l_flag_m7390.
        EXIT.                                               "n497992
      ENDIF.                                                "n497992
    ENDSELECT.                                              "n497992


    IF l_flag_m7390 IS INITIAL.                             "v_2197941
      REFRESH: g_ra_bukrs.

      IF bukrs IS NOT INITIAL.
        g_ra_bukrs[] = bukrs[].
        MOVE : 'EQ'       TO  g_ra_bukrs-option,
               'I'        TO  g_ra_bukrs-sign,
               '    '     TO  g_ra_bukrs-low.
        APPEND                g_ra_bukrs.
      ELSEIF werks IS NOT INITIAL.
        SELECT bukrs INTO g_ra_bukrs-low
           FROM       t001k AS t001k
           INNER JOIN t001w AS t001w                   "#EC CI_BUFFJOIN
           ON   t001w~bwkey EQ t001k~bwkey
          WHERE t001w~werks IN werks.
          IF sy-subrc EQ 0.
            MOVE : 'EQ'       TO  g_ra_bukrs-option,
                   'I'        TO  g_ra_bukrs-sign.
            APPEND                g_ra_bukrs.
          ENDIF.
        ENDSELECT.
        MOVE : 'EQ'       TO  g_ra_bukrs-option,
               'I'        TO  g_ra_bukrs-sign,
               '    '     TO  g_ra_bukrs-low.
        APPEND                g_ra_bukrs.
        DELETE ADJACENT DUPLICATES FROM g_ra_bukrs.
      ENDIF.

      SELECT * FROM ttypvx
              WHERE awtyp   IN g_ra_awtyp
               AND  bukrs   IN g_ra_bukrs
               AND  tabname NE 'BSET'.
        IF ttypvx-awtyp = 'MKPF'.
          MOVE  'X'       TO  l_flag_m7390.
          EXIT.
        ENDIF.
        IF ttypvx-fieldname = '*'      OR
           ttypvx-fieldname = 'MATNR'.
          MOVE  'X'       TO  l_flag_m7390.
          EXIT.
        ENDIF.
      ENDSELECT.

    ENDIF.                                                  "^_2197941

*   emerge message after this SELECT - ENDSELECT loop if
*   an error was detected
    IF  l_flag_m7390 = 'X'.
*       emerge warning ?                                    "n497992
      CALL FUNCTION 'ME_CHECK_T160M'                        "n497992
        EXPORTING                                           "n497992
          i_arbgb = 'M7'                           "n497992
          i_msgnr = '390'                          "n497992
        EXCEPTIONS                                          "n497992
          nothing = 0                              "n497992
          OTHERS  = 1.                             "n497992
                                                            "n497992
      IF sy-subrc <> 0.                                     "n497992
*         FI summarization active / results could be wrong  "n497992
        MESSAGE            w390.                            "n497992
      ENDIF.                                                "n497992
    ENDIF.
  ENDIF.                                                    "n497992
                                                            "n497992
* - the user wants to restrict the movement type : process  "n497992
*   warning M7 391                                          "n497992
  IF NOT bwart[] IS INITIAL.                                "n497992
*   emerge warning ?                                        "n497992
    CALL FUNCTION 'ME_CHECK_T160M'               "n497992
      EXPORTING                                         "n497992
        i_arbgb = 'M7'                         "n497992
        i_msgnr = '391'                        "n497992
      EXCEPTIONS                                        "n497992
        nothing = 0                            "n497992
        OTHERS  = 1.                           "n497992
                                                            "n497992
    IF sy-subrc <> 0.                                       "n497992
      SET CURSOR             FIELD  'BWART_LOW'.            "n497992
*     to restric the mov.type could cause wrong results     "n497992
      MESSAGE                w391.                          "n497992
    ENDIF.                                                  "n497992
  ENDIF.                                                    "n497992

* - send warning M7 689 when user does not restric the      "n599218
*   database in dialog or print mode                        "n599218
  IF  sy-ucomm  =  'ONLI'     OR                            "n599218
      sy-ucomm  =  'PRIN'.                                  "n599218
*   only in dialog or online-print mode                     "n599218
    IF  matnr[] IS INITIAL AND                              "n599218
        bukrs[] IS INITIAL AND                              "n599218
        werks[] IS INITIAL AND                              "n599218
        lgort[] IS INITIAL AND                              "n599218
        charg[] IS INITIAL AND                              "n599218
        bwtar[] IS INITIAL.                                 "n599218
      MESSAGE  w689.         "Selection was not restricted  "n599218
    ENDIF.                                                  "n599218
  ENDIF.                                                    "n599218

* - send warning M7 482 when user wants to restrict
*   G/L account together with material, etc.
  IF gv_switch_ehp6ru = abap_true AND hkont[] IS NOT INITIAL.

    IF NOT matnr[] IS INITIAL.
      MOVE 'X' TO l_flag_m7482.
      SET CURSOR FIELD 'MATNR-LOW'.

    ELSEIF NOT werks[] IS INITIAL.
      MOVE 'X' TO l_flag_m7482.
      SET CURSOR FIELD 'WERKS-LOW'.

    ELSEIF NOT bwtar[] IS INITIAL.
      MOVE 'X' TO l_flag_m7482.
      SET CURSOR FIELD 'BWTAR-LOW'.
    ENDIF.

    IF l_flag_m7482 = 'X'.
*     emerge warning ?
      CALL FUNCTION 'ME_CHECK_T160M'
        EXPORTING
          i_arbgb = 'M7'
          i_msgnr = '482'
        EXCEPTIONS
          nothing = 0
          OTHERS  = 1.
      IF sy-subrc <> 0.
        MESSAGE w482.
      ENDIF.
    ENDIF.
  ENDIF.

* check the indicators for the scope of list categories     "n599218
  PERFORM                    f0850_empty_parameters.        "n599218
                                                            "n599218
  CASE  g_cnt_empty_parameter.  "evaluate the result        "n599218
    WHEN  0.                                                "n599218
*    all parameters are set -> take all entries             "n599218
    WHEN  7.                                                "n599218
      CASE  g_flag_status_liu.                              "n599218
        WHEN  c_hide.                                       "n599218
          MOVE  'S'          TO  g_flag_status_liu.         "n599218
          SET  CURSOR        FIELD 'PB_LIU'.                "n599218
        WHEN  c_show.                                       "n599218
          SET  CURSOR        FIELD  'PA_WDZER'.             "n599218
      ENDCASE.                                              "n599218
                                                            "n599218
*     Please choose at least one scope of list              "n599218
      MESSAGE                 e829.                         "n599218
                                                            "n599218
    WHEN  OTHERS.                                           "n599218
*      process selection for scope of list                  "n599218
  ENDCASE.                                                  "n599218

ENDFORM.                     "f0800_check_restrictions      "n547170
                                                            "n547170
*-----------------------------------------------------------"n547170
*    f0850_empty_parameters                                 "n599218
*-----------------------------------------------------------"n599218
                                                            "n599218
*&---------------------------------------------------------------------*
*&      Form  f0850_empty_parameters
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f0850_empty_parameters.                                "n599218
                                                            "n599218
* check the indicators for the scope of list categories     "n599218
* how many parameters are empty ?                           "n599218
  CLEAR                      g_cnt_empty_parameter.         "n599218
                                                            "n599218
  DEFINE macro_check_parameter.                             "n599218
    IF  &1 IS INITIAL.                                      "n599218
      ADD  1                 TO  g_cnt_empty_parameter.     "n599218
    ENDIF.                                                  "n599218
  END-OF-DEFINITION.                                        "n599218
                                                            "n599218
  macro_check_parameter      pa_wdzer.                      "n599218
  macro_check_parameter      pa_wdzew.                      "n599218
  macro_check_parameter      pa_wdwiz.                      "n599218
  macro_check_parameter      pa_wdwuw.                      "n599218
  macro_check_parameter      pa_wdwew.                      "n599218
  macro_check_parameter      pa_ndzer.                      "n599218
  macro_check_parameter      pa_ndsto.                      "n599218
                                                            "n599218
ENDFORM.                     "f0850_empty_parameters        "n599218
                                                            "n599218
*-----------------------------------------------------------"n547170
*    F1000_SELECT_MSEG_MKPF
*----------------------------------------------------------------------*

FORM f1000_select_mseg_mkpf.

* selection of material docs in database tables MKPF and MSEG

*--- begin of note 921165 ----------------------------------"n921165
* improve performance of the access database tables MKPF    "n921165
* and MSEG using database specific hints                    "n921165
                                                            "n921165
  BREAK-POINT                ID mmim_rep_mb5b.              "n921164
* dynamic break-point : before the main SELECT command      "n921164
                                                            "n921165

* read data from cold store also if necessary
  DATA(lv_date) = datum-low.
  IF lv_date IS INITIAL OR lv_date(4) = '0000'.
    lv_date = '00010101'.
  ENDIF.
  DATA(lv_cold_store_access) = cl_nsdm_matdoc_aging_helper=>is_cold_store_relevant( lv_date ).
  IF lv_cold_store_access = abap_true.
    cl_abap_stack_temperature=>set_temperature( lv_date ).
  ENDIF.

* was the MSEG conversion done? then choose new logic       "n1558298
  IF g_f_msegex_act IS INITIAL.                             "n1558298
* what kind of database access does the user choose ?
    IF      pa_dbstd = 'X'.
*   standard access, the database optimizer looks for the
*   access path
      PERFORM hdb_check_table USING 'MKPF' 'MSEG'.          "n1710850
      SELECT (g_t_mseg_fields)
           INTO CORRESPONDING FIELDS OF TABLE g_t_mseg_lean
           FROM mkpf AS mkpf  JOIN mseg AS mseg
                    ON mkpf~mandt  =  mseg~mandt  AND
                       mkpf~mblnr  =  mseg~mblnr  AND
                       mkpf~mjahr  =  mseg~mjahr
           CONNECTION (dbcon)                               "n1710850
           WHERE mseg~matnr  IN  matnr
             AND mseg~werks  IN  g_ra_werks
*             AND mseg~lgort  IN  g_ra_lgort
             AND mseg~charg  IN  charg
             AND mseg~bwtar  IN  bwtar
*             AND mseg~bwart  IN  bwart
             AND mkpf~budat  GE  datum-low
             AND mseg~sobkz  IN  g_ra_sobkz

*             "Novos filtros da tela de selecao
*             AND mseg~lifnr  IN  lifnr "Filtros movidos para a subrotina FILL_DATA_TABLE
*             AND mseg~kunnr  IN  kunnr "Filtros movidos para a subrotina FILL_DATA_TABLE
*             AND mseg~aufnr  IN  aufnr "Filtros movidos para a subrotina FILL_DATA_TABLE

      %_HINTS                                   "#EC CI_HINTS "n1511550
      DB2    '&SUBSTITUTE VALUES&'
      ORACLE '&SUBSTITUTE VALUES&'.

    ELSEIF  pa_dbmat = 'X'.
*   database access via material number and MSEG MM doc items
      PERFORM hdb_check_table USING 'MKPF' 'MSEG'.          "n1710850
      SELECT (g_t_mseg_fields)
           INTO CORRESPONDING FIELDS OF TABLE g_t_mseg_lean
           FROM mseg AS mseg  JOIN mkpf AS mkpf
                    ON mkpf~mandt  =  mseg~mandt  AND
                       mkpf~mblnr  =  mseg~mblnr  AND
                       mkpf~mjahr  =  mseg~mjahr
           CONNECTION (dbcon)                               "n1710850
           WHERE mseg~matnr  IN  matnr
             AND mseg~werks  IN  g_ra_werks
             AND mseg~lgort  IN  g_ra_lgort
             AND mseg~charg  IN  charg
             AND mseg~bwtar  IN  bwtar
             AND mseg~bwart  IN  bwart
             AND mkpf~budat  GE  datum-low
             AND mseg~sobkz  IN  g_ra_sobkz

*             "Novos filtros da tela de selecao
*             AND mseg~lifnr  IN  lifnr "Filtros movidos para a subrotina FILL_DATA_TABLE
*             AND mseg~kunnr  IN  kunnr "Filtros movidos para a subrotina FILL_DATA_TABLE
*             AND mseg~aufnr  IN  aufnr "Filtros movidos para a subrotina FILL_DATA_TABLE

      %_HINTS                                   "#EC CI_HINTS "n1511550
      ADABAS   'ORDERED'
      INFORMIX 'ORDERED'
      MSSQLNT  'OPTION FORCE ORDER'                         "921165
      DB6 '<NLJOIN><IXSCAN TABLE=''MSEG'' SAP_INDEX=''M''/>'
      DB6 '<IXSCAN TABLE=''MKPF'' SAP_INDEX=''0''/></NLJOIN>'.

    ELSEIF  pa_dbdat = 'X'.
*   database access via posting date and MKPF MM doc headers
      PERFORM hdb_check_table USING 'MKPF' 'MSEG'.          "n1710850
      SELECT (g_t_mseg_fields)
           INTO CORRESPONDING FIELDS OF TABLE g_t_mseg_lean
           FROM mkpf AS mkpf  JOIN mseg AS mseg
                    ON mkpf~mandt  =  mseg~mandt  AND
                       mkpf~mblnr  =  mseg~mblnr  AND
                       mkpf~mjahr  =  mseg~mjahr
           CONNECTION (dbcon)                               "n1710850
           WHERE mseg~matnr  IN  matnr
             AND mseg~werks  IN  g_ra_werks
             AND mseg~lgort  IN  g_ra_lgort
             AND mseg~charg  IN  charg
             AND mseg~bwtar  IN  bwtar
             AND mseg~bwart  IN  bwart
             AND mkpf~budat  GE  datum-low
             AND mseg~sobkz  IN  g_ra_sobkz

*             "Novos filtros da tela de selecao
*             AND mseg~lifnr  IN  lifnr "Filtros movidos para a subrotina FILL_DATA_TABLE
*             AND mseg~kunnr  IN  kunnr "Filtros movidos para a subrotina FILL_DATA_TABLE
*             AND mseg~aufnr  IN  aufnr "Filtros movidos para a subrotina FILL_DATA_TABLE

      %_HINTS                                   "#EC CI_HINTS "n1511550
      INFORMIX 'ORDERED'
      ADABAS 'ORDERED'
      MSSQLNT  'OPTION FORCE ORDER'                         "n921165
      DB6 '<NLJOIN><IXSCAN TABLE=''MKPF'' SAP_INDEX=''BUD''/>'
      DB6 '<IXSCAN TABLE=''MSEG'' SAP_INDEX=''0''/></NLJOIN>'.
    ELSE.
*   not allowed
      MOVE  1                  TO  sy-subrc.                "n921165
    ENDIF.                                                  "n921165
*--- end of note 921165 ------------------------------------"n921165
  ELSE.                                                     "n1558298
* MSEG conversion was done - use new logic via MSEG-BUDAT   "n1558298
    IF g_f_msegex_act = 'H'.                                "n1558298
*   use hints - this can maybe removed in a later stage     "n1558298
      PERFORM hdb_check_table USING 'MKPF' 'MSEG'.          "n1710850
      SELECT (g_t_mseg_fields)                              "n1558298
           INTO CORRESPONDING FIELDS OF TABLE g_t_mseg_lean "n1558298
           FROM mkpf AS mkpf  JOIN mseg AS mseg             "n1558298
                    ON mkpf~mandt  =  mseg~mandt  AND       "n1558298
                       mkpf~mblnr  =  mseg~mblnr  AND       "n1558298
                       mkpf~mjahr  =  mseg~mjahr            "n1558298
           CONNECTION (dbcon)                               "n1710850
           WHERE mseg~matnr       IN  matnr                 "n1558298
             AND mseg~werks       IN  g_ra_werks            "n1558298
             AND mseg~lgort       IN  g_ra_lgort            "n1558298
             AND mseg~charg       IN  charg                 "n1558298
             AND mseg~bwtar       IN  bwtar                 "n1558298
             AND mseg~bwart       IN  bwart                 "n1558298
             AND mseg~budat_mkpf  GE  datum-low             "n1558298
             AND mseg~sobkz       IN  g_ra_sobkz            "n1558298

*             "Novos filtros da tela de selecao
*             AND mseg~lifnr  IN  lifnr "Filtros movidos para a subrotina FILL_DATA_TABLE
*             AND mseg~kunnr  IN  kunnr "Filtros movidos para a subrotina FILL_DATA_TABLE
*             AND mseg~aufnr  IN  aufnr "Filtros movidos para a subrotina FILL_DATA_TABLE

      %_HINTS                                   "#EC CI_HINTS "n1558298
      DB2    '&SUBSTITUTE VALUES&'                          "n1558298
      ORACLE '&SUBSTITUTE VALUES&'.                         "n1558298
    ELSE.                                                   "n1558298
      PERFORM hdb_check_table USING 'MKPF' 'MSEG'.          "n1710850
      SELECT (g_t_mseg_fields)                              "n1558298
       INTO CORRESPONDING FIELDS OF TABLE g_t_mseg_lean     "n1558298
       FROM mkpf AS mkpf  JOIN mseg AS mseg                 "n1558298
                ON mkpf~mandt  =  mseg~mandt  AND           "n1558298
                   mkpf~mblnr  =  mseg~mblnr  AND           "n1558298
                   mkpf~mjahr  =  mseg~mjahr                "n1558298
       CONNECTION (dbcon)                                   "n1710850
       WHERE mseg~matnr       IN  matnr                     "n1558298
         AND mseg~werks       IN  g_ra_werks                "n1558298
         AND mseg~lgort       IN  g_ra_lgort                "n1558298
         AND mseg~charg       IN  charg                     "n1558298
         AND mseg~bwtar       IN  bwtar                     "n1558298
         AND mseg~bwart       IN  bwart                     "n1558298
         AND mseg~budat_mkpf  GE  datum-low                 "n1558298
         AND mseg~sobkz       IN  g_ra_sobkz.               "n1558298

*             "Novos filtros da tela de selecao
*             AND mseg~lifnr  IN  lifnr "Filtros movidos para a subrotina FILL_DATA_TABLE
*             AND mseg~kunnr  IN  kunnr "Filtros movidos para a subrotina FILL_DATA_TABLE
*             AND mseg~aufnr  IN  aufnr "Filtros movidos para a subrotina FILL_DATA_TABLE

    ENDIF.                                                  "n1558298
  ENDIF.                                                    "n1558298

  IF lv_cold_store_access = abap_true.
    cl_abap_stack_temperature=>set_hot( ).
  ENDIF.

*--- begin of note 1481757 ---------------------------------"n1481757
  IF archive = 'X'.                                         "n1481757
    IF pa_aistr IS NOT INITIAL.                             "n1481757
* process the MM docs from the new AS archive               "n1481757
      "enable read with binary search                       "n1858578
      SORT  g_t_mseg_lean BY mblnr mjahr zeile.             "n1858578
      PERFORM process_archive_mm_doc.                       "n1481757
    ENDIF.                                                  "n1481757
  ENDIF.                                                    "n1481757
*----- end of note 1481757 ---------------------------------"n1481757
  IF  g_t_mseg_lean IS INITIAL.
*   no material documents found
    MESSAGE                  s842.
  ENDIF.

  DATA: lt_imchb_tmp LIKE imchb OCCURS 0,                   "838360
        ls_imchb     LIKE imchb.                            "838360

* check whether the found MM doc items contain retail and
* and beverage specific values
* check authority in this loop - endloop
  LOOP AT g_t_mseg_lean      ASSIGNING <g_fs_mseg_lean>.
*   a) check authorisation
    PERFORM        f9000_auth_plant_check
                             USING  <g_fs_mseg_lean>-werks.

    IF  g_flag_authority IS INITIAL.
      DELETE                g_t_mseg_lean.
      CONTINUE.             "take the next entry
    ENDIF.

*   b) look for special indicators
    IF  NOT <g_fs_mseg_lean>-xauto IS INITIAL.
      IF  <g_fs_mseg_lean>-xauto IN g_ra_xauto.
        MOVE  'X'            TO  g_cust_tied_empties.
      ENDIF.
    ENDIF.
*   838360_v
*   according to note 62272 MCHB may be archived even if the batches
*   are still in use. Therefore we try to find such batches by
*   collecting the batches used in MSEG.
    IF NOT xnomchb IS INITIAL AND
       NOT <g_fs_mseg_lean>-charg IS INITIAL.
      ls_imchb-matnr = <g_fs_mseg_lean>-matnr.
      ls_imchb-werks = <g_fs_mseg_lean>-werks.
      ls_imchb-lgort = <g_fs_mseg_lean>-lgort.
      ls_imchb-charg = <g_fs_mseg_lean>-charg.
      APPEND ls_imchb TO lt_imchb_tmp.
    ENDIF.
*   838360_^
* for active ingredient materials MB5B should not display  "n1265674
* the 141 and 142 movements for the selection valuated     "n1265674
* stock to avoid wrong beginning stock amount.             "n1265674
    IF bwbst = 'X' AND <g_fs_mseg_lean>-bustw = 'MB08'.     "n1265674
      DELETE                 g_t_mseg_lean.                 "n1265674
      CONTINUE.              "take the next entry          "n1265674
    ENDIF.                                                  "n1265674
  ENDLOOP.

* 838360_v
  IF NOT lt_imchb_tmp[] IS INITIAL.
*   the following lines merge the batches found in MCHB with the ones
*   found in the material documents.
    SORT lt_imchb_tmp.                                      "n1928950
    DELETE ADJACENT DUPLICATES FROM lt_imchb_tmp
                               COMPARING werks matnr lgort charg.
*   imchb might have entries with negative stock. In this case "n1928950
*   we have to keep the imchb entries. This happens when       "n1928950
*   negative stocks are allowed for the material               "n1928950
    LOOP AT imchb.                                          "n1928950
      IF imchb-clabs < 0 OR imchb-cumlm < 0 OR              "n1928950
         imchb-cinsm < 0 OR imchb-ceinm < 0 OR              "n1928950
         imchb-cspem < 0 OR imchb-cretm < 0.                "n1928950
*        code removed from note 1928950                        "n2675738
                                                            "n1928950
        READ TABLE lt_imchb_tmp INTO ls_imchb               "n1928950
                             WITH KEY werks = imchb-werks   "n1928950
                                      matnr = imchb-matnr   "n1928950
                                      lgort = imchb-lgort   "n1928950
                                      charg = imchb-charg   "n1928950
                                      BINARY SEARCH.        "n1928950
        IF sy-subrc = 0.                                    "n1928950
*         delte record from matdoc collection as it already    "n2675738
*         exists in imchb which is the leading source          "n2675738
          DELETE lt_imchb_tmp WHERE werks = imchb-werks AND "n1928950
                                    matnr = imchb-matnr AND "n1928950
                                    lgort = imchb-lgort AND "n1928950
                                    charg = imchb-charg.    "n1928950
        ENDIF.                                              "n1928950
      ENDIF.                                                "n1928950
    ENDLOOP.                                                "n1928950
    APPEND LINES OF lt_imchb_tmp TO imchb.
*   due to the merge of mchb batches and mseg batches there will be
*   duplicate entries in imchb by now. They will be deleted. The
*   following sort makes sure that only lines without a quantity are
*   deleted by the 'delete adjacent duplicates' command.
    SORT imchb DESCENDING BY werks matnr lgort charg
                             clabs cumlm cinsm ceinm cspem cretm.
    DELETE ADJACENT DUPLICATES FROM imchb
                               COMPARING matnr werks lgort charg.
    FREE lt_imchb_tmp.
  ENDIF.
* 838360_^

* function for tied empties is active and                   "n547170
* stock type = storage location/batch ?                     "n547170
  IF  NOT g_cust_tied_empties IS INITIAL   AND              "n547170
      NOT lgbst               IS INITIAL.                   "n547170
*   sort the results by documents numbers und items         "n547170
    SORT  g_t_mseg_lean      BY  mblnr mjahr zeile.         "n547170
  ENDIF.                                                    "n547170

* process table withe the results form the database selection
  LOOP AT g_t_mseg_lean      INTO  g_s_mseg_lean.
    PERFORM                  f1100_check_lgort_sokzg.

    IF  g_flag_delete = 'X'.
      DELETE                 g_t_mseg_lean.
      CONTINUE.              "take the next entry
    ENDIF.

*   function for tied empties is active and                 "n547170
*   stock type = storage location/batch ?                   "n547170
    IF  NOT g_cust_tied_empties IS INITIAL   AND            "n547170
        NOT lgbst               IS INITIAL.                 "n547170
                                                            "n547170
*     check whether this line was generated automatically   "n547170
      IF  g_s_mseg_lean-xauto = 'X'.                        "n547170
*       look for the origin line                            "n547170
        COMPUTE  g_f_zeile = g_s_mseg_lean-zeile - 1.       "n547170
                                                            "n547170
*       check whether the previous line contains the        "n547170
*       original posting                                    "n547170
        IF  g_s_mseg_pr-matnr = g_s_mseg_lean-matnr AND     "n547170
            g_s_mseg_pr-mblnr = g_s_mseg_lean-mblnr AND     "n547170
            g_s_mseg_pr-mjahr = g_s_mseg_lean-mjahr AND     "n547170
            g_s_mseg_pr-zeile = g_f_zeile.                  "n547170
*         the previous entry contains the original line     "n547170
                                                            "n547170
          IF  g_s_mseg_pr-xauto IN g_ra_xauto.              "n547170
*           the previous line contains a matching value     "n547170
*           XAUTO -> save it in working table               "n547170
            APPEND  g_s_mseg_pr     TO  g_t_mseg_or.        "n547170
          ENDIF.                                            "n547170
        ELSE.                                               "n547170
*         the previous entry does not contain the original  "n547170
*         posting : save the key                            "n547170
          g_s_mseg_key-matnr      = g_s_mseg_lean-matnr.    "n547170
          g_s_mseg_key-mblnr      = g_s_mseg_lean-mblnr.    "n547170
          g_s_mseg_key-mjahr      = g_s_mseg_lean-mjahr.    "n547170
          g_s_mseg_key-zeile      = g_f_zeile.              "n547170
          APPEND  g_s_mseg_key    TO  g_t_mseg_key.         "n547170
        ENDIF.                                              "n547170
      ENDIF.                                                "n547170
                                                            "n547170
*     save the current entry in the buffer previous entry   "n547170
      MOVE-CORRESPONDING g_s_mseg_lean TO  g_s_mseg_pr.     "n547170
    ENDIF.                                                  "n547170
  ENDLOOP.

* function for tied empties is active and                   "n547170
* stock type = storage location/batch ?                     "n547170
  IF  NOT g_cust_tied_empties IS INITIAL   AND              "n547170
      NOT lgbst               IS INITIAL.                   "n547170
                                                            "n547170
*   Select the missing items with the origin posting lines  "n547170
*   and append them into the working table                  "n547170

    IF  NOT g_t_mseg_key[] IS INITIAL.                      "n1481757
                                                            "n1481757
      IF  archive = 'X'.                                    "n1481757
        PERFORM  fill_table_g_t_mseg_or                     "n1481757
                      USING    ht_mmdocs_arch               "n1481757
                      CHANGING g_t_mseg_key                 "n1481757
                               g_t_mseg_or.                 "n1481757
        " g_ra_xauto.                 "n1481757
      ENDIF.                                                "n1481757

      REFRESH g_t_mseg_key.
      MOVE g_t_mseg_key_te[] TO g_t_mseg_key[].

      IF  NOT g_t_mseg_key[] IS INITIAL.                    "n547170
* are there any keys left after reading mm docs from archive?
        DATA(lv_year) = CONV mjahr('9999').
        LOOP AT g_t_mseg_key[] INTO DATA(ls_mseg_key).
          IF ls_mseg_key-mjahr < lv_year.
            lv_year = ls_mseg_key-mjahr.
          ENDIF.
        ENDLOOP.
        DATA(lv_temperature) = CONV data_temperature( lv_year && '0101' ).
        IF cl_nsdm_matdoc_aging_helper=>is_cold_store_relevant( lv_temperature ).
          cl_abap_stack_temperature=>set_temperature( lv_temperature ).
        ENDIF.

        PERFORM hdb_check_table USING 'MSEG' ''.            "n1710850
        SELECT mblnr mjahr zeile matnr xauto                "n547170
             FROM  mseg  CONNECTION (dbcon)                 "n1710850
           APPENDING TABLE g_t_mseg_or                      "n547170
           FOR ALL ENTRIES IN g_t_mseg_key                  "n547170
           WHERE  mblnr = g_t_mseg_key-mblnr                "n547170
             AND  mjahr = g_t_mseg_key-mjahr                "n547170
             AND  zeile = g_t_mseg_key-zeile                "n547170
             AND  xauto IN g_ra_xauto.   "only F, L, M, W     "n547170

        cl_abap_stack_temperature=>set_hot( ).
      ENDIF.                                                "n547170
    ENDIF.
                                                            "n547170
    SORT  g_t_mseg_or      BY  mblnr mjahr zeile matnr.     "n547170
                                                            "n547170
*   process the MM docs in any cases :                      "n547170
*   - delete the lines with the special value for XAUTO     "n547170
*   - check lines who were created automatically whether    "n547170
*     the original line has a special value for XAUTO       "n547170
    LOOP AT g_t_mseg_lean INTO  g_s_mseg_lean.              "n547170
      CLEAR                g_flag_delete.                   "n547170
                                                            "n547170
*     evaluate field XAUTO                                  "n547170
      IF      g_s_mseg_lean-xauto IS INITIAL.               "n547170
*       no action                                           "n547170
      ELSEIF  g_s_mseg_lean-xauto IN g_ra_xauto.            "n547170
*       delete MM docs with XAUTO = 'F', 'L', 'M', 'W'      "n547170
        MOVE  'X'            TO  g_flag_delete.             "n547170
                                                            "n547170
      ELSEIF  g_s_mseg_lean-xauto = 'X'.                    "n547170
        IF  NOT g_t_mseg_or[] IS INITIAL.                   "n547170
*         check field XAUTO of the original MM doc item     "n547170
          COMPUTE  g_f_zeile = g_s_mseg_lean-zeile - 1.     "n547170
                                                            "n547170
          READ TABLE g_t_mseg_or                            "n547170
                    INTO  g_s_mseg_or                       "n547170
                    WITH KEY mblnr = g_s_mseg_lean-mblnr    "n547170
                             mjahr = g_s_mseg_lean-mjahr    "n547170
                             zeile = g_f_zeile              "n547170
                             matnr = g_s_mseg_lean-matnr    "n547170
                             BINARY SEARCH.                 "n547170
                                                            "n547170
          IF  sy-subrc IS INITIAL.                          "n547170
*           the original line was a posting for a tied      "n547170
*           empties material -> delete this entry           "n547170
            MOVE  'X'        TO  g_flag_delete.             "n547170
          ENDIF.                                            "n547170
        ENDIF.                                              "n547170
      ENDIF.                                                "n547170
                                                            "n547170
      IF  g_flag_delete = 'X'.                              "n547170
        DELETE               g_t_mseg_lean.                 "n547170
      ENDIF.                                                "n547170
    ENDLOOP.                                                "n547170
                                                            "n547170
*   release the space of the working tables                 "n547170
    FREE : g_t_mseg_or, g_t_mseg_key.                       "n547170
  ENDIF.                                                    "n547170

* is the table g_t_mseg_lean empty and no authority problems
  DESCRIBE TABLE g_t_mseg_lean         LINES  g_f_cnt_lines.

  IF  g_f_cnt_lines   IS INITIAL  AND
      g_f_cnt_before  =  g_f_cnt_after.
*   Keinen Eintrag zu den Suchbegriffen gefunden/selektiert
    MESSAGE                  s083.
  ENDIF.
  IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_F1000_SELECT_MSEG_MK_01\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).

    DATA: ls_cwm_mseg TYPE mseg.

* if the user will see valuated stocks, change MEINS to VALUM, if needed
    IF bwbst = 'X'.
      CLEAR imara-matnr.
      LOOP AT g_t_mseg_lean INTO g_s_mseg_lean.
        IF g_s_mseg_lean-matnr <> imara-matnr.
          READ TABLE imara WITH KEY matnr = g_s_mseg_lean-matnr
                          BINARY SEARCH.
          IF sy-subrc <> 0.
            CLEAR imara.
          ENDIF.
        ENDIF.
        IF NOT imara-/cwm/valum IS INITIAL
        AND    imara-/cwm/valum <> g_s_mseg_lean-meins.
          g_s_mseg_lean-meins = g_s_mseg_lean-/cwm/meins.
          g_s_mseg_lean-menge = g_s_mseg_lean-/cwm/menge.
          MODIFY g_t_mseg_lean FROM g_s_mseg_lean
                               TRANSPORTING meins
                                            menge.
        ENDIF.
      ENDLOOP.
*   documents created with the old architecture must be changed
*   otherwise these documents would be not considered for the display "Valuated Stock"
*   In subroutine BELEGE_ERGAENZEN these documents would be deleted from the internal
*   table because of initial BUSTW
*   commented 20160718 (BK)
*    LOOP AT g_t_mseg_lean INTO g_s_mseg_lean
*                          WHERE bustw IS INITIAL.
*      SELECT SINGLE * FROM mseg INTO ls_cwm_mseg
*                      WHERE mblnr = g_s_mseg_lean-mblnr
*                        AND mjahr = g_s_mseg_lean-mjahr
*                        AND zeile = g_s_mseg_lean-zeile.
*      IF sy-subrc NE 0.
*        CONTINUE.
*      ENDIF.
*      IF ls_cwm_mseg-/cwm/bustw IS NOT INITIAL AND
*        ( ls_cwm_mseg-/cwm/fi = '1' OR (
*          ls_cwm_mseg-xauto = 'X' AND ls_cwm_mseg-dmbtr > 0 ) ). "2047789
*        g_s_mseg_lean-bustw = ls_cwm_mseg-/cwm/bustw.
*        MODIFY g_t_mseg_lean FROM g_s_mseg_lean
*                             TRANSPORTING bustw.
*      ENDIF.
*    ENDLOOP.

    ENDIF.
  ENDIF.
*ENHANCEMENT-POINT ehp605_f1000_select_mseg_mk_01 SPOTS es_rm07mlbd .

ENDFORM.                     "f1000_select_mseg_mkpf

*----------------------------------------------------------------------*
*    F1100_CHECK_LGORT_SOKZG
*----------------------------------------------------------------------*

FORM f1100_check_lgort_sokzg.
  CLEAR                      g_flag_delete.
*   c) additional checks if valuated stock is required
  IF  bwbst = 'X'.
*     check fields sobkz and kzwbs for valuated stocks
    IF      g_s_mseg_lean-sobkz  =  c_space  OR
            g_s_mseg_lean-sobkz  =  'O'      OR
            g_s_mseg_lean-sobkz  =  'V'      OR
            g_s_mseg_lean-sobkz  =  'W'.                    "n435403
*       OK : special stock indicator = ' ', 'O', 'W' or 'V'
    ELSEIF  g_s_mseg_lean-kzbws = 'A'  OR
            g_s_mseg_lean-kzbws = 'M'.
*       ok : document with valuated special stock
    ELSE.
      MOVE  'X'            TO  g_flag_delete.
    ENDIF.

    "{ Begin ENHO DIPCS_RM07MLBD_FORM_02 IS-AD-SSP AD_SUB }
* GA1551829
*     A&D subcontracting valuated stock = 'F' or 'R'
    IF g_flag_delete IS NOT INITIAL AND
       ( g_s_mseg_lean-sobkz = cl_adsub_constants=>f OR
         g_s_mseg_lean-sobkz = cl_adsub_constants=>r ).
      IF g_s_mseg_lean-kzbws IS NOT INITIAL.
        CLEAR g_flag_delete.
      ELSE.
*         before note 1592894, automatic 541 line has empty KZBWS
        CALL FUNCTION 'GET_BUSTR_FROM_MOVEMENT_TYPE'
          EXPORTING
            bwart    = g_s_mseg_lean-bwart
          IMPORTING
            bustr    = gv_bustr_tp
          EXCEPTIONS
            no_entry = 1
            OTHERS   = 2.
        IF gv_bustr_tp EQ cl_adsub_constants=>bustr_tp.
          CLEAR g_flag_delete.
        ENDIF.
      ENDIF.
    ENDIF.
    "{ End ENHO DIPCS_RM07MLBD_FORM_02 IS-AD-SSP AD_SUB }

*ENHANCEMENT-POINT f1100_check_lgort_sokzg_01 SPOTS es_rm07mlbd .

  ELSE.
*   b) check the combination of special stock indicator and
*      storage location
    IF       g_s_mseg_lean-sobkz = 'O'  OR
             g_s_mseg_lean-sobkz = 'T'  OR                "SIT
             g_s_mseg_lean-sobkz = 'V'  OR
             g_s_mseg_lean-sobkz = 'W'.
*     these entries must not have a storage location
      IF  NOT g_s_mseg_lean-lgort IS INITIAL.
        MOVE  'X'            TO  g_flag_delete.
      ENDIF.
    ELSE.
*     the others entries should have a storage location
      IF  g_s_mseg_lean-lgort IS INITIAL.
        MOVE  'X'            TO  g_flag_delete.
      ENDIF.
    ENDIF.

    "{ Begin ENHO DIPCS_RM07MLBD_FORM_02 IS-AD-SSP AD_SUB }
* GA1551829
* A&D subcontracting stock: 'C', 'F', 'I', 'J' or 'R'
* these entries must not have a storage location
    IF g_s_mseg_lean-lgort IS INITIAL AND
       g_flag_delete       IS NOT INITIAL AND
       ( g_s_mseg_lean-sobkz = cl_adsub_constants=>c OR
         g_s_mseg_lean-sobkz = cl_adsub_constants=>f OR
         g_s_mseg_lean-sobkz = cl_adsub_constants=>i OR
         g_s_mseg_lean-sobkz = cl_adsub_constants=>j OR
         g_s_mseg_lean-sobkz = cl_adsub_constants=>r ).
      CLEAR g_flag_delete.
    ENDIF.
    "{ End ENHO DIPCS_RM07MLBD_FORM_02 IS-AD-SSP AD_SUB }

*ENHANCEMENT-POINT f1100_check_lgort_sokzg_02 SPOTS es_rm07mlbd .

  ENDIF.

ENDFORM.                     "f1100_check_lgort_sokzg.

*----------------------------------------------------------------------*
*    F2100_MAT_TEXT
*----------------------------------------------------------------------*

FORM f2100_mat_text
                   USING     l_f_matnr TYPE stype_mat_key-matnr.

  IF  l_f_matnr  NE  g_s_makt-matnr.                        "n451923
*   read in table imakt                                     "n451923
    READ TABLE g_t_makt      INTO  g_s_makt                 "n451923
                             WITH KEY matnr = l_f_matnr     "n451923
                             BINARY SEARCH.                 "n451923
                                                            "n451923
    IF  sy-subrc <> 0.                                      "n451923
*     record not found                                      "n451923
      CLEAR                  g_s_makt-maktx.                "n451923
    ENDIF.                                                  "n451923
  ENDIF.                                                    "n451923

ENDFORM.                     "f2100_mat_text

*----------------------------------------------------------------------*
*    F2200_READ_T001
*----------------------------------------------------------------------*

FORM f2200_read_t001
                   USING     l_f_werks LIKE t001w-werks.

  STATICS : BEGIN OF l_s_t001w,                             "n999530
              werks TYPE  t001w-werks,                      "n999530
              name1 TYPE  t001w-name1,                      "n999530
            END OF l_s_t001w.                               "n999530

* read name of this plant after the plant has changed       "n999530
  IF  l_f_werks <> l_s_t001w-werks.                         "n999530
    SELECT SINGLE werks name1                               "n999530
      FROM t001w                                            "n999530
        INTO CORRESPONDING FIELDS OF l_s_t001w              "n999530
          WHERE werks = l_f_werks.                          "n1574925

    IF NOT sy-subrc IS INITIAL.                             "n999530
      CLEAR                  l_s_t001w.                     "n999530
      MOVE  l_f_werks        TO  l_s_t001w-werks.           "n999530
    ENDIF.                                                  "n999530
  ENDIF.                                                    "n999530

  MOVE  l_s_t001w-name1      TO  t001w-name1.               "n999530

ENDFORM.                     "f2200_read_t001

*----------------------------------------------------------------------*
*    F9000_AUTH_PLANT_CHECK
*----------------------------------------------------------------------*

FORM f9000_auth_plant_check
                   USING     l_f_werks LIKE marc-werks.

  CLEAR                      g_flag_authority.
  ADD  1                     TO  g_f_cnt_before.

  READ TABLE g_t_auth_plant  WITH KEY
                             werks = l_f_werks BINARY SEARCH.

  IF sy-subrc IS INITIAL.
*   plant found in buffer; take the result from the buffer
    MOVE  g_t_auth_plant-ok            TO  g_flag_authority.
  ELSE.
*   new plant / do the authority check / save result in buffer table
    AUTHORITY-CHECK OBJECT 'M_MSEG_WMB'
                    ID 'ACTVT' FIELD actvt03
                    ID 'WERKS' FIELD  l_f_werks.

    IF  sy-subrc IS INITIAL.
      MOVE : 'X'             TO  g_t_auth_plant-ok,
             'X'             TO  g_flag_authority.
    ELSE.
      CLEAR : g_t_auth_plant-ok, g_flag_authority.
    ENDIF.

    MOVE  l_f_werks          TO  g_t_auth_plant-werks.
    APPEND                   g_t_auth_plant.
    SORT                     g_t_auth_plant.
  ENDIF.

  IF  g_flag_authority = 'X'.
    ADD  1                   TO  g_f_cnt_after.
  ENDIF.

ENDFORM.                     "f9000_authority_check_plant

*----------------------------------------------------------------------*
*     F9100_AUTH_PLANT_RESULT
*----------------------------------------------------------------------*

FORM  f9100_auth_plant_result.

  CASE    g_f_cnt_after.     "results ?
    WHEN  g_f_cnt_before.
*     user has authority for all plants in G_RA_WERKS
    WHEN  0.
*     user has no authority for the plants in G_RA_WERKS
      MESSAGE s124.   "Wegen fehlender Berechtigung ist ...
*     leave report to selection screen
      PERFORM                 anforderungsbild.
    WHEN  OTHERS.
*     user has authority for only a part of the plants
      MESSAGE s124.   "Wegen fehlender Berechtigung ist ...
  ENDCASE.

* clear the counter fields for the next check
  CLEAR : g_f_cnt_after, g_f_cnt_before.

ENDFORM.                     "f9100_auth_plant_result

*----------------------------------------------------------------------*
*    F9200_COLLECT_PLANT
*----------------------------------------------------------------------*

FORM f9200_collect_plant
                   USING     l_f_werks LIKE      t001w-werks.

  CHECK : g_t_organ[] IS INITIAL.                           "n433765

* build the range table g_0000_ra_werks
  MOVE : l_f_werks           TO  g_0000_ra_werks-low,
         'I'                 TO  g_0000_ra_werks-sign,
         'EQ'                TO  g_0000_ra_werks-option.
  COLLECT                    g_0000_ra_werks.

ENDFORM.                     "f9200_collect_plant

*----------------------------------------------------------------------*
*    F9300_READ_ORGAN
*----------------------------------------------------------------------*

FORM f9300_read_organ
                   USING     l_f_keytype   LIKE g_s_organ-keytype
                             l_f_keyfield  LIKE g_s_organ-keyfield.

* buffer
  STATICS : l_s_old      TYPE      stype_organ,
            l_9300_subrc LIKE      sy-subrc.

  IF  l_f_keytype   =  l_s_old-keytype  AND
      l_f_keyfield  =  l_s_old-keyfield.
*   the same key : take the data from the buffer
    MOVE-CORRESPONDING  l_s_old        TO  g_s_organ.
    MOVE  l_9300_subrc                 TO  sy-subrc.
  ELSE.
*   the key has changed : read in table g_t_organ
    READ TABLE g_t_organ     WITH KEY
                             keytype   =  l_f_keytype
                             keyfield  =  l_f_keyfield
                             BINARY SEARCH.

    IF  sy-subrc IS INITIAL.
*     entry found
      MOVE-CORRESPONDING : g_t_organ   TO  g_s_organ,
                           g_t_organ   TO  l_s_old.
      CLEAR                            l_9300_subrc.
    ELSE.
*     entry not found / fill the buffer
      CLEAR : l_s_old,       g_s_organ.
      MOVE  : sy-subrc       TO  l_9300_subrc,
              l_f_keytype    TO  l_s_old-keytype,
              l_f_keyfield   TO  l_s_old-keyfield.
    ENDIF.
  ENDIF.

ENDFORM.                     "f9300_read_organ

*----------------------------------------------------------------------*
*    F9400_MATERIAL_KEY
*----------------------------------------------------------------------*

FORM f9400_material_key
                   USING     l_f_matnr LIKE      mara-matnr.

* create key table with material number
  MOVE : l_f_matnr           TO g_t_mat_key-matnr.
  COLLECT                    g_t_mat_key.

ENDFORM.                     "f9400_material_key

*----------------------------------------------------------------------*
*    f9500_set_color_and_sign
*----------------------------------------------------------------------*

FORM f9500_set_color_and_sign
         USING  l_s_belege   TYPE  stype_belege
                l_f_tabname  TYPE  stype_fields-fieldname.

  DATA : l_f_fieldname       TYPE  stype_fields-fieldname.
  FIELD-SYMBOLS : <l_fs_field>.

* clear the table with the color information
  REFRESH color. CLEAR color.

  LOOP AT g_t_color_fields.
    CONCATENATE  l_f_tabname
                 '-'
                 g_t_color_fields-fieldname
                             INTO l_f_fieldname.
    ASSIGN (l_f_fieldname)   TO  <l_fs_field>.

    CHECK sy-subrc IS INITIAL.

    MOVE : g_t_color_fields-fieldname
                             TO  color-fieldname,
           0                 TO  color-color-int.

*   the color and the sign of this numeric field depend on the
*   debit/credit-indicator
    CASE    l_s_belege-shkzg.
      WHEN  'H'.
        color-color-col = '6'.         "red
        APPEND color.

        IF  g_t_color_fields-type  <>  'C'.
          COMPUTE : <l_fs_field> = <l_fs_field> * -1.
        ENDIF.

      WHEN  'S'.
        color-color-col = '5'.         "green
        APPEND color.
    ENDCASE.
  ENDLOOP.

* customizing : set the color information
  IF  g_cust_color  = 'X'.
*   default : colorize the numeric fields
    MOVE  color[]            TO  l_s_belege-farbe_pro_feld.
  ELSE.
*   the performant way : colorize the line on when GI
    IF  l_s_belege-shkzg = 'H'.
      MOVE  'C21'  TO  l_s_belege-farbe_pro_zeile. "grey
    ELSE.
      MOVE  'C20'  TO  l_s_belege-farbe_pro_zeile. "light grey
    ENDIF.
  ENDIF.

ENDFORM.                     "f9500_set_color_and_sign

*-----------------------------------------------------------"n547170
*    tpc_check_tax_auditor                                  "n547170
*-----------------------------------------------------------"n547170
                                                            "n547170
*&---------------------------------------------------------------------*
*&      Form  tpc_check_tax_auditor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tpc_check_tax_auditor.                                 "n547170
                                                            "n547170
* - the function module FI_CHECK_DATE of note 486477 will   "n547170
*   be processed when it exists                             "n547170
  CALL FUNCTION 'FUNCTION_EXISTS'                           "n547170
    EXPORTING                                               "n547170
      funcname           = 'FI_CHECK_DATE'              "n547170
    EXCEPTIONS                                              "n547170
      function_not_exist = 1                            "n547170
      OTHERS             = 2.                           "n547170
                                                            "n547170
  IF sy-subrc IS INITIAL.                                   "n547170
*   the function module FI_CHECK_DATE exists -> go on       "n547170
                                                            "n547170
*   separate time depending authorization for tax auditor   "n547170
*   first step : check, whether the user is a tax auditor   "n547170
    MOVE  sy-repid             TO  g_f_repid.               "n486477
                                                            "n486477
    CALL FUNCTION 'FI_CHECK_DATE'                           "n486477
      EXPORTING                                             "n486477
        i_bukrs           = space                           "n486477
        i_user            = sy-uname                        "n486477
        i_program         = g_f_repid                       "n486477
      IMPORTING                                             "n486477
        e_return          = g_flag_tpcuser                  "n486477
      EXCEPTIONS                                            "n486477
        no_authority_prog = 1                               "n486477
        no_authority_date = 2                               "n486477
        wrong_parameter   = 3                               "n486477
        OTHERS            = 4.                              "n486477
                                                            "n486477
    CASE  sy-subrc.                                         "n486477
      WHEN  0.                                              "n486477
*       what kind of user : g_flag_tpcuser = 1 tax auditor  "n486477
*                           g_flag_tpcuser = 4 other other  "n486477
      WHEN  1.                                              "n486477
*       user is tax auditor, but program is not allowed     "n486477
        MESSAGE  e001(ca_check_date) WITH g_f_repid.        "n486477
                                                            "n486477
      WHEN  OTHERS.                                         "n486477
*       other error                                         "n486477
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno   "n486477
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.   "n486477
    ENDCASE.                                                "n486477
  ENDIF.                                                    "n547170
                                                            "n486477
  IF  g_flag_tpcuser = '1'.                                 "n486477
*   check and complete the selection-dates                  "n486477
    IF datum-low IS INITIAL.                                "n486477
      datum-low = '00000101'.                               "n486477
      IF datum-high IS INITIAL.                             "n486477
        datum-high = '99991231'.                            "n486477
      ENDIF.                                                "n486477
    ELSE.                                                   "n486477
      IF datum-high IS INITIAL.                             "n486477
        datum-high = datum-low.                             "n486477
      ENDIF.                                                "n486477
    ENDIF.                                                  "n486477
                                                            "n486477
*   second step : the user is an auditor -> check periods   "n486477
    PERFORM                  tpc_check_date_for_all_cc.     "n486477
  ENDIF.                                                    "n486477
                                                            "n547170
ENDFORM.                     "tpc_check_tax_auditor         "n547170
                                                            "n547170
*-----------------------------------------------------------"n486477
*    tpc_check_date_for_all_CC                              "n486477
*-----------------------------------------------------------"n486477
                                                            "n486477
*&---------------------------------------------------------------------*
*&      Form  tpc_check_date_for_all_cc
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tpc_check_date_for_all_cc.                             "n486477
                                                            "n486477
* clear working aeras and ranges                            "n486477
  CLEAR    : g_s_bukrs,  g_s_t001k,  g_s_t001w.             "n486477
  REFRESH  : g_t_bukrs,  g_t_t001k,  g_t_t001w.             "n486477
                                                            "n486477
  IF  werks[] IS INITIAL.                                   "n486477
*   no restriction for plant :                              "n486477
*   get all matching company codes from table T001          "n486477
    SELECT bukrs             FROM t001                      "n486477
         INTO CORRESPONDING FIELDS OF TABLE g_t_bukrs       "n486477
         WHERE  bukrs IN bukrs.                             "n486477
                                                            "n486477
    IF  sy-subrc <> 0.                                      "n486477
      SET  CURSOR            FIELD  'BUKRS-LOW'.            "n486477
      MESSAGE  e282(m7).     "Company code does not exist   "n486477
    ENDIF.                                                  "n486477
  ELSE.                                                     "n486477
*   look for the corresponding company codes                "n486477
    PERFORM                  tpc_check_get_all_cc.          "n486477
  ENDIF.                                                    "n486477
                                                            "n486477
* check the selected company codes and the dates            "n486477
  LOOP AT g_t_bukrs          INTO  g_s_bukrs.               "n486477
*   check the authorization for dates and company code      "n486477
    CALL FUNCTION 'FI_CHECK_DATE'                           "n486477
      EXPORTING                                             "n486477
        i_bukrs           = g_s_bukrs-bukrs                 "n486477
        i_user            = sy-uname                        "n486477
        i_program         = g_f_repid                       "n486477
        i_from_date       = datum-low                       "n486477
        i_to_date         = datum-high                      "n486477
      EXCEPTIONS                                            "n486477
        no_authority_prog = 1                               "n486477
        no_authority_date = 2                               "n486477
        wrong_parameter   = 3                               "n486477
        OTHERS            = 4.                              "n486477
                                                            "n486477
    CASE sy-subrc.                                          "n486477
      WHEN 0.                                               "n486477
*       authorization ok --> take this company code         "n486477
                                                            "n486477
      WHEN 2.                                               "n486477
*       send 2 messages to show the company code           "n486477
        SET  CURSOR          FIELD  'BUKRS-LOW'.            "n486477
*       no display authorization for company code &         "n486477
        MESSAGE  i113(fg)    WITH  g_s_bukrs-bukrs.         "n486477
                                                            "n486477
        SET CURSOR           FIELD  'BWKEY-LOW'.            "n486477
*       Keine Berechtigung zur Anzeige von Daten aus ...    "n486477
        MESSAGE              e002(ca_check_date).           "n486477
                                                            "n486477
      WHEN OTHERS.                                          "n486477
*       an error occurred                                   "n486477
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno   "n486477
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.   "n486477
    ENDCASE.                                                "n486477
  ENDLOOP.                                                  "n486477
                                                            "n486477
  FREE : g_t_bukrs,  g_t_t001k,  g_t_t001w.                 "n486477
                                                            "n486477
ENDFORM.                     "tpc_check_date_for_all_CC     "n486477
                                                            "n486477
*-----------------------------------------------------------"n486477
*   tpc_check_get_all_cc                                    "n486477
*   look for the corresponding company codes                "n486477
*-----------------------------------------------------------"n486477
                                                            "n486477
*&---------------------------------------------------------------------*
*&      Form  TPC_CHECK_GET_ALL_CC
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tpc_check_get_all_cc.                                  "n486477
                                                            "n486477
* select the matching plants from T001W                     "n486477
  SELECT werks bwkey         FROM  t001w                    "n486477
         INTO CORRESPONDING FIELDS OF TABLE g_t_t001w       "n486477
         WHERE  werks IN werks.                             "n486477
                                                            "n486477
  IF  sy-subrc IS INITIAL.                                  "n486477
    SORT  g_t_t001w          BY  werks bwkey.               "n486477
  ELSE.                                                     "n486477
    SET CURSOR               FIELD  'WERKS-LOW'.            "n486477
*   Plant & does not exist                                  "n486477
    MESSAGE e892             WITH   werks-low.              "n486477
  ENDIF.                                                    "n486477
                                                            "n486477
  LOOP AT g_t_t001w          INTO  g_s_t001w.               "n486477
*   select the matching valuation areas and comany codes    "n486477
    SELECT SINGLE bwkey bukrs FROM  t001k                   "n486477
         INTO CORRESPONDING FIELDS OF g_s_t001k             "n486477
         WHERE  bwkey  =  g_s_t001w-bwkey                   "n486477
           AND  bukrs  IN bukrs.                            "n486477
                                                            "n486477
    IF  sy-subrc IS INITIAL.                                "n486477
      MOVE  g_s_t001k-bukrs  TO  g_s_bukrs-bukrs.           "n486477
      COLLECT  g_s_bukrs     INTO  g_t_bukrs.               "n486477
    ELSE.                                                   "n486477
      SET CURSOR             FIELD  'WERKS-LOW'.            "n486477
      MESSAGE  e283          WITH   g_s_t001w-werks.        "n486477
    ENDIF.                                                  "n486477
  ENDLOOP.                                                  "n486477
                                                            "n486477
ENDFORM.                     "tpc_check_get_all_cc          "n486477
                                                            "n486477
*-----------------------------------------------------------"n486477
*    tpc_write_log                                          "n555246
*-----------------------------------------------------------"n555246
                                                            "n555246
*&---------------------------------------------------------------------*
*&      Form  tpc_write_log
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM tpc_write_log.                                         "n555246
                                                            "n555246
* check whether the function module is available            "n555246
  CALL FUNCTION 'FUNCTION_EXISTS'                           "n555246
    EXPORTING                                               "n555246
      funcname           = 'CA_WRITE_LOG'                   "n555246
    EXCEPTIONS                                              "n555246
      function_not_exist = 1                                "n555246
      OTHERS             = 2.                               "n555246
                                                            "n555246
  CHECK : sy-subrc IS INITIAL.                              "n555246
                                                            "n555246
* write the entries of the selection screen into log file   "n555246
  CALL FUNCTION 'CA_WRITE_LOG'         "#EC EXISTS     "n555246
    EXPORTING                                           "n555246
      i_program   = g_f_repid                         "n555246
    EXCEPTIONS                                          "n555246
      write_error = 1                                 "n555246
      OTHERS      = 2.                                "n555246
                                                            "n555246
  IF sy-subrc IS INITIAL.                                   "n555246
    COMMIT WORK.                                            "n555246
  ELSE.                                                     "n555246
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno       "n555246
                WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.   "n555246
  ENDIF.                                                    "n555246
                                                            "n555246
ENDFORM.                     "tpc_write_log                 "n555246
                                                            "n555246
*-----------------------------------------------------------"n555246
*&---------------------------------------------------------------------*
*&      Form  PROCESS_ARCHIVE_MM_DOC
*&---------------------------------------------------------------------*
FORM process_archive_mm_doc .
  DATA: g_flag_ok_mkpf(01) TYPE c,                          "n1481757
        g_flag_ok_mseg(01) TYPE c.                          "n1481757
                                                            "n1481757
  BREAK-POINT ID mmim_rep_mb5b.                             "n1481757
* the result of function module should be the keys for the  "n1481757
* archive                                                   "n1481757
* show the current activity and the progress                "n1481757
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'                 "n1481757
    EXPORTING                                               "n1481757
      text = TEXT-136.       "reading archive info records  "n1481757
                                                            "n1481757
  PERFORM  call_as_api_read_block_keys                      "n1481757
                 TABLES    g_t_frange[]                     "n1481757
                           g_t_as_key[].                    "n1481757
                                                            "n1481757
* eliminate duplicates                                      "n1481757
  SORT  g_t_as_key           BY  mblnr mjahr.               "n1481757
  DELETE ADJACENT DUPLICATES FROM g_t_as_key                "n1481757
                        COMPARING mblnr mjahr.              "n1481757
                                                            "n1481757
* show the current activity and the progress                "n1481757
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'                 "n1481757
    EXPORTING                                               "n1481757
      text = TEXT-135.       "Reading MM docs in archive    "n1481757
                                                            "n1481757
* get the full data of the MM documents from the AS archiv  "n1481757
  LOOP AT g_t_as_key         INTO  g_s_as_key.              "n1481757
                                                            "n1481757
    REFRESH : xmkpf, xmseg.                                 "n1481757
                                                            "n1481757
    CALL FUNCTION 'ASH_MM_MATBEL_READ'                      "n1481757
      EXPORTING                                             "n1481757
        i_archivekey           = g_s_as_key-archivekey       "n1481757
        i_offset               = g_s_as_key-archiveofs       "n1481757
      TABLES                                                "n1481757
        et_mkpf                = xmkpf                       "n1481757
        et_mseg                = xmseg                       "n1481757
      EXCEPTIONS                                            "n1481757
        not_in_infostructure   = 1                          "n1481757
        not_in_archive         = 2                          "n1481757
        no_instructure_defined = 3                          "n1481757
        OTHERS                 = 4.                         "n1481757
                                                            "n1481757
    CASE  sy-subrc.                                         "n1481757
      WHEN  0.               " MM document found            "n1481757
                                                            "n1481757
      WHEN  1 OR 2.                                         "n1481757
*       this message will never be send here, it is only    "n1481757
*       here for the where-use list of the messages         "n1481757
*       using transcation SE91                              "n1481757
        IF 'A' = 'B'.                                       "n1481757
*         BA 109 : No data object found for key &           "n1481757
          MESSAGE e109(ba)   WITH  space.                   "n1481757
        ENDIF.                                              "n1481757
                                                            "n1481757
        CLEAR                archive_messages.              "n1481757
        MOVE : 'BA'          TO  archive_messages-msgid,    "n1481757
               '109'         TO  archive_messages-msgno.    "n1481757

        DATA offset TYPE string.
        offset = g_s_as_key-archiveofs.
                                                            "n1481757
        CONCATENATE g_s_as_key-mblnr                        "n1481757
                    g_s_as_key-mjahr                        "n1481757
                    g_s_as_key-archivekey                   "n1481757
                             INTO  archive_messages-msgv1   "n1481757
                             SEPARATED BY space.            "n1481757
        APPEND               archive_messages.              "n1481757
        CONTINUE.            " take the next entry          "n1481757
                                                            "n1481757
      WHEN OTHERS.                                          "n1481757
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno   "n1481757
          WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.         "n1481757
    ENDCASE.                                                "n1481757
                                                            "n1481757
*   process table with MM doc header                        "n1481757
    LOOP AT xmkpf.                                          "n1481757
*      CHECK: xmkpf-budat IN datum.                         "n2110304
**     process the corresponding items                      "n1481757
      LOOP AT xmseg                                         "n1481757
        WHERE  mblnr = xmkpf-mblnr                          "n1481757
          AND  mjahr = xmkpf-mjahr.                         "n1481757
        CHECK: xmseg-bwart IN bwart.                        "n1481757
        CHECK: xmseg-charg IN charg.                        "n1481757
        CHECK: xmseg-lgort IN lgort.                        "n1481757
        CHECK: xmseg-matnr IN matnr.                        "n1481757
        IF bwbst IS INITIAL.                                "n1995040
          CHECK: xmseg-sobkz = sobkz.                       "n1481757
        ENDIF.                                              "n1995040
        CHECK: xmseg-bukrs IN bukrs.                        "n1481757
        CHECK: xmseg-bwart IN bwart.                        "n1481757

        wa_hashtable-mblnr = xmseg-mblnr.                   "n1481757
        wa_hashtable-mjahr = xmseg-mjahr.                   "n1481757
                                                            "n1481757
        INSERT wa_hashtable INTO TABLE ht_mmdocs_arch.      "n1481757
                                                            "n1481757
        PERFORM add_aridx_doc_to_g_t_mseg_lean.             "n1481757
                                                            "n1481757
      ENDLOOP.                                              "n1481757
    ENDLOOP.                                                "n1481757
  ENDLOOP.                                                  "n1481757
                                                            "n1481757
                                                            "n1481757
ENDFORM.                    " PROCESS_ARCHIVE_MM_DOC
*&---------------------------------------------------------------------*
*&      Form  ADD_ARIDX_DOC_TO_G_T_MSEG_LEAN
*&---------------------------------------------------------------------*
FORM add_aridx_doc_to_g_t_mseg_lean .
* Eliminate dublettes from the archive and tranfer data       "n1481757
* into working ITAB                                           "n1481757
  READ TABLE g_t_mseg_lean   INTO g_s_mseg_lean             "n1481757
                             WITH KEY mblnr = xmseg-mblnr   "n1481757
                             mjahr = xmseg-mjahr            "n1481757
                             zeile = xmseg-zeile            "n1481757
                             BINARY SEARCH.                 "n1481757
                                                            "n1481757
  CASE  sy-subrc.                                           "n1481757
    WHEN  0.                                                "n1481757
*       MM doc is available from database -> ignore it        "n1481757
                                                            "n1481757
    WHEN  4.                                                "n1481757
*       MM doc not found -> insert entry                      "n1481757
      MOVE  sy-tabix       TO  g_f_tabix.                   "n1481757
      MOVE-CORRESPONDING :                                  "n1481757
        xmkpf TO g_s_mseg_lean,                             "n1481757
        xmseg TO g_s_mseg_lean.                             "n1481757
                                                            "n1481757
      INSERT g_s_mseg_lean INTO g_t_mseg_lean INDEX  g_f_tabix. "n1481757
      CLEAR g_s_mseg_lean .                                 "n1481757
                                                            "n1481757
    WHEN  8.                                                "n1481757
*       MM doc not found / key to high -> append entry        "n1481757
      MOVE-CORRESPONDING :                                  "n1481757
        xmkpf TO g_s_mseg_lean,                             "n1481757
        xmseg TO g_s_mseg_lean.                             "n1481757
                                                            "n1481757
      APPEND g_s_mseg_lean TO g_t_mseg_lean.                "n1481757
      CLEAR g_s_mseg_lean .                                 "n1481757
                                                            "n1481757
  ENDCASE.                                                  "n1481757
                                                            "n1481757
                                                            "n1481757
ENDFORM.                    " ADD_ARIDX_DOC_TO_G_T_MSEG_LEAN
*&---------------------------------------------------------------------*
*&      Form  CHECK_EXISTENCE_AS
*&---------------------------------------------------------------------*
FORM check_existence_as  USING    lv_g_flag_exist_as.
* check whether the functions modules of the AS archive     "n1481757
* are available in the system, otherwise the functions for  "n1481757
* the SA archive will be not carried out                    "n1481757
  lv_g_flag_exist_as = ' '.                                 "n1481757
                                                            "n1481757
  CALL FUNCTION 'FUNCTION_EXISTS'                           "n1481757
    EXPORTING                                               "n1481757
      funcname           = 'AS_API_INFOSTRUC_FIND'          "n1481757
    EXCEPTIONS                                              "n1481757
      function_not_exist = 1                                "n1481757
      OTHERS             = 2.                               "n1481757
                                                            "n1481757
                                                            "n1481757
  CHECK : sy-subrc IS INITIAL.                              "n1481757
                                                            "n1481757
  CALL FUNCTION 'FUNCTION_EXISTS'                           "n1481757
    EXPORTING                                               "n1481757
      funcname           = 'ASH_MM_MATBEL_READ'             "n1481757
    EXCEPTIONS                                              "n1481757
      function_not_exist = 1                                "n1481757
      OTHERS             = 2.                               "n1481757
                                                            "n1481757
  CHECK : sy-subrc IS INITIAL.                              "n1481757
                                                            "n1481757
  CALL FUNCTION 'FUNCTION_EXISTS'                           "n1481757
    EXPORTING                                               "n1481757
      funcname           = 'AS_API_READ'                    "n1481757
    EXCEPTIONS                                              "n1481757
      function_not_exist = 1                                "n1481757
      OTHERS             = 2.                               "n1481757
                                                            "n1481757
  CHECK : sy-subrc IS INITIAL.                              "n1481757
  lv_g_flag_exist_as = 'X'.                                 "n1481757
                                                            "n1481757
ENDFORM.                    " CHECK_EXISTENCE_AS
*&---------------------------------------------------------------------*
*&      Form  CHECK_ARCHIVE_INDEX
*&---------------------------------------------------------------------*
FORM check_archive_index  USING    lv_g_flag_too_many_sel
                                   lv_fieldname.
  TYPES: BEGIN OF aind_st_nametab,                          "n1481757
           tabname   LIKE dd02d-tabname,                    "n1481757
           fieldname LIKE dd03d-fieldname,                  "n1481757
           keyflag   LIKE dd03d-keyflag,                    "n1481757
           scr_tab   LIKE dd02d-tabname,                    "n1481757
           scr_field LIKE dd03d-fieldname,                  "n1481757
           key_oblig TYPE c,                                "n1481757
         END OF aind_st_nametab,                            "n1481757
         aind_tt_nametab TYPE aind_st_nametab OCCURS 20.    "n1481757
                                                            "n1481757
  DATA : lt_nametab TYPE  aind_tt_nametab,                  "n1481757
         l_nametab  TYPE  aind_st_nametab.                  "n1481757
                                                            "n1481757
                                                            "n1481757
* get the fields of the arch. info structure                "n1481757
  CALL FUNCTION 'AIND_NAMETAB_GET'                          "n1481757
    EXPORTING                                               "n1481757
      i_archindex      = pa_aistr                           "n1481757
      i_reffields_only = 'X'                                "n1481757
    TABLES                                                  "n1481757
      t_nametab        = lt_nametab                         "n1481757
    EXCEPTIONS                                              "n1481757
      index_not_found  = 1.                                 "n1481757
                                                            "n1481757
  IF sy-subrc <> 0.                                         "n1481757
    SET CURSOR               FIELD 'PA_AISTR'.              "n1481757
*   Enter a suitable info structure                         "n1481757
    MESSAGE  e509(q6)        WITH  pa_aistr.                "n1481757
  ENDIF.                                                    "n1481757
                                                            "n1481757
* clear working areas                                       "n1481757
  REFRESH :  g_t_frange.                                    "n1481757
  CLEAR: lv_g_flag_too_many_sel.                            "n1481757
                                                            "n1481757
* create the selection table for the this archive index     "n1481757
  PERFORM  fill_frange.                                     "n1481757
                                                            "n1481757
* compare user-selction with archiv index                   "n1481757
                                                            "n1481757
  LOOP AT g_t_frange INTO g_s_frange.                       "n1481757
    READ TABLE lt_nametab                                   "n1481757
         WITH KEY fieldname = g_s_frange-fieldname          "n1481757
         INTO l_nametab.                                    "n1481757
    IF sy-subrc <> 0.                                       "n1481757
      lv_g_flag_too_many_sel = 'X'.                         "n1481757
      lv_fieldname = g_s_frange-fieldname.                  "n1481757
    ELSE.                                                   "n1481757
* create the selection table for the archive index compared to select-options
      MOVE-CORRESPONDING g_s_frange TO g_s_selrange.        "n1481757
      APPEND g_s_selrange TO g_t_selrange.                  "n1481757
    ENDIF.                                                  "n1481757
  ENDLOOP.                                                  "n1481757
                                                            "n1481757
ENDFORM.                    " CHECK_ARCHIVE_INDEX
*&---------------------------------------------------------------------*
*&      Form  FILL_FRANGE
*&---------------------------------------------------------------------*
FORM fill_frange .
* copy select-options for field FELDNAME                      "n1481757
* using select-option table FELDNAME                          "n1481757
                                                            "n1481757
  IF matnr IS NOT INITIAL.                                  "n1481757
    REFRESH g_s_frange-selopt_t.                            "n1904996
    LOOP AT matnr.                                          "n1904996
      MOVE-CORRESPONDING matnr TO g_s_selopt.               "n1481757
      APPEND  g_s_selopt  TO  g_s_frange-selopt_t.          "n1481757
    ENDLOOP.                                                "n1904996
                                                            "n1481757
    MOVE 'MATNR' TO g_s_frange-fieldname.                   "n1481757
    APPEND  g_s_frange   TO  g_t_frange.                    "n1481757
  ENDIF.                                                    "n1481757
                                                            "n1481757
  IF bukrs IS NOT INITIAL.                                  "n1481757
    REFRESH g_s_frange-selopt_t.                            "n1904996
    LOOP AT bukrs.                                          "n1904996
      MOVE-CORRESPONDING bukrs TO g_s_selopt.               "n1481757
      APPEND  g_s_selopt  TO  g_s_frange-selopt_t.          "n1481757
    ENDLOOP.                                                "n1904996
                                                            "n1481757
    MOVE 'BUKRS' TO g_s_frange-fieldname.                   "n1481757
    APPEND  g_s_frange   TO  g_t_frange.                    "n1481757
  ENDIF.                                                    "n1481757
                                                            "n1481757
  IF werks IS NOT INITIAL.                                  "n1481757
    REFRESH g_s_frange-selopt_t.                            "n1904996
    LOOP AT werks.                                          "n1904996
      MOVE-CORRESPONDING werks TO g_s_selopt.               "n1481757
      APPEND  g_s_selopt  TO  g_s_frange-selopt_t.          "n1481757
    ENDLOOP.                                                "n1904996
                                                            "n1481757
    MOVE 'WERKS' TO g_s_frange-fieldname.                   "n1481757
    APPEND  g_s_frange   TO  g_t_frange.                    "n1481757
  ENDIF.                                                    "n1481757
                                                            "n1481757
  IF lgort IS NOT INITIAL.                                  "n1481757
    REFRESH g_s_frange-selopt_t.                            "n1904996
    LOOP AT lgort.                                          "n1904996
      MOVE-CORRESPONDING lgort TO g_s_selopt.               "n1481757
      APPEND  g_s_selopt  TO  g_s_frange-selopt_t.          "n1481757
    ENDLOOP.                                                "n1904996
                                                            "n1481757
    MOVE 'LGORT' TO g_s_frange-fieldname.                   "n1481757
    APPEND  g_s_frange   TO  g_t_frange.                    "n1481757
  ENDIF.                                                    "n1481757
                                                            "n1481757
  IF charg IS NOT INITIAL.                                  "n1481757
    REFRESH g_s_frange-selopt_t.                            "n1904996
    LOOP AT charg.                                          "n1904996
      MOVE-CORRESPONDING charg TO g_s_selopt.               "n1481757
      APPEND  g_s_selopt  TO  g_s_frange-selopt_t.          "n1481757
    ENDLOOP.                                                "n1904996
                                                            "n1481757
    MOVE 'CHARG' TO g_s_frange-fieldname.                   "n1481757
    APPEND  g_s_frange   TO  g_t_frange.                    "n1481757
  ENDIF.                                                    "n1481757
                                                            "n1481757
  IF bwart IS NOT INITIAL.                                  "n1481757
    REFRESH g_s_frange-selopt_t.                            "n1904996
    LOOP AT bwart.                                          "n1904996
      MOVE-CORRESPONDING bwart TO g_s_selopt.               "n1481757
      APPEND  g_s_selopt  TO  g_s_frange-selopt_t.          "n1481757
    ENDLOOP.                                                "n1904996
                                                            "n1481757
    MOVE 'BWART' TO g_s_frange-fieldname.                   "n1481757
    APPEND  g_s_frange   TO  g_t_frange.                    "n1481757
  ENDIF.                                                    "n1481757
                                                            "n1481757
  IF bwtar IS NOT INITIAL.                                  "n1481757
    REFRESH g_s_frange-selopt_t.                            "n1904996
    LOOP AT bwtar.                                          "n1904996
      MOVE-CORRESPONDING bwtar TO g_s_selopt.               "n1481757
      APPEND  g_s_selopt  TO  g_s_frange-selopt_t.          "n1481757
    ENDLOOP.                                                "n1904996
                                                            "n1481757
    MOVE 'BWTAR' TO g_s_frange-fieldname.                   "n1481757
    APPEND  g_s_frange   TO  g_t_frange.                    "n1481757
  ENDIF.                                                    "n1481757
                                                            "n1481757
  IF datum IS NOT INITIAL.                                  "n2110304
    REFRESH g_s_frange-selopt_t.                            "n2110304
*   datum is defined with NO EXTENSION and has such max.    "n2110304
*   one entry. We take only datum-low as we need all        "n2110304
*   documents from archive till datum-low for the back      "n2110304
*   calculation of the stock.                               "n2110304
    READ TABLE datum INTO g_s_selopt INDEX 1.               "n2110304
    CLEAR g_s_selopt-high.                                  "n2110304
    g_s_selopt-sign = 'I'.                                  "n2110304
    g_s_selopt-option = 'GE'.                               "n2110304
    APPEND  g_s_selopt  TO  g_s_frange-selopt_t.            "n2110304
                                                            "n2110304
    MOVE 'BUDAT' TO g_s_frange-fieldname.                   "n2110304
    APPEND  g_s_frange   TO  g_t_frange.                    "n2110304
  ENDIF.                                                    "n2110304
ENDFORM.                    " FILL_FRANGE
*&---------------------------------------------------------------------*
*&      Form  CALL_AS_API_READ_BLOCK_KEYS
*&---------------------------------------------------------------------*
FORM call_as_api_read_block_keys
                TABLES l_t_frange   TYPE  stab_frange       "n1481757
                       l_t_as_key   TYPE  stab_as_key.      "n1481757
                                                            "n1481757
  PERFORM get_archive_field_catalogs.                       "n1481757
                                                            "n1481757
  MOVE   g_s_aind_str1_ais-skey   TO  g_f_afcat.            "n1481757
                                                            "n1481757
* get the keys for the archive only                         "n1481757
* the type of the assigned table for "E_RESULTS" determines "n1481757
* the results                                               "n1481757
* AS_API_READ replaced with AS_API_SYSTEM_SELECT to         "v2244652
* select the archive directly via entered infostructure
  CALL FUNCTION 'AS_API_SYSTEM_SELECT'
    EXPORTING
      i_infostruc         = pa_aistr
      i_selections        = l_t_frange[]
    IMPORTING
      e_result            = l_t_as_key[]
    EXCEPTIONS
      parameters_invalid  = 1
      infostruc_not_found = 2
      OTHERS              = 3.                         "^2244652
  IF  NOT sy-subrc IS INITIAL.                              "n1481757
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno       "n1481757
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.           "n1481757
  ENDIF.                                                    "n1481757
                                                            "n1481757
ENDFORM.                    " CALL_AS_API_READ_BLOCK_KEYS
*&---------------------------------------------------------------------*
*&      Form  GET_ARCHIVE_FIELD_CATALOGS
*&---------------------------------------------------------------------*
FORM get_archive_field_catalogs .
  CLEAR   : g_f_afcat.                                      "n1481757
  REFRESH : g_t_aind_str1_fc.                               "n1481757
                                                            "n1481757
*   look for active archive info structures from the        "n1481757
*   customizing tables aind_str1 and aind_str2              "n1481757
  SELECT aind_str1~archindex                  "#EC CI_BYPASS  "n1511550
       aind_str1~itype                                      "n1481757
       aind_str1~skey                                       "n1481757
  INTO TABLE g_t_aind_str1_ais                              "n1481757
    FROM aind_str1 JOIN aind_str2           "#EC CI_BUFFJOIN  "n1511550
    ON aind_str1~archindex = aind_str2~archindex            "n1481757
         WHERE itype  = 'I'                                 "n1481757
           AND aind_str1~archindex = pa_aistr               "n1481757
           AND object = 'MM_MATBEL'                         "n1481757
           AND active = 'X'.                                "n1481757
* there must be one entry                                   "n1481757
  IF sy-dbcnt <> 1.                                         "n1481757
    SET CURSOR               FIELD 'PA_AISTR'.              "n1481757
*   Enter a suitable info structure                         "n1481757
    MESSAGE e509(q6)         WITH pa_aistr.                 "n1481757
  ENDIF.                                                    "n1481757
                                                            "n1481757
  READ TABLE g_t_aind_str1_ais                              "n1481757
    INTO g_s_aind_str1_ais                                  "n1481757
      INDEX 1.                                              "n1481757
                                                            "n1481757
  CHECK : sy-subrc IS INITIAL.                              "n1481757
ENDFORM.                    " GET_ARCHIVE_FIELD_CATALOGS
*&---------------------------------------------------------------------*
*&      Form  FILL_TABLE_G_T_MSEG_OR
*&---------------------------------------------------------------------*
FORM fill_table_g_t_mseg_or                                 "n1481757
            USING    uht_mmdocs_arch
            CHANGING ct_mseg_key TYPE stab_mseg_xauto
                     ct_mseg_or TYPE stab_mseg_xauto.
  " c_ra_xauto.

  DATA: lt_mseg_key       TYPE stab_mseg_xauto,
        lt_mseg_key_group LIKE lt_mseg_key.

  TYPES: BEGIN OF ts_group,
           mblnr LIKE mkpf-mblnr,
           mjahr LIKE mkpf-mjahr,
         END OF ts_group.

  DATA: ls_group_new TYPE ts_group,
        ls_group_old TYPE ts_group.

*data: lt_ra_xauto like gt_ra_xauto.

  FIELD-SYMBOLS  <ls_mseg_key> LIKE LINE OF ct_mseg_key.

*-----------------------------------------------------------------------*

  MOVE ct_mseg_key[] TO lt_mseg_key[].
  REFRESH ct_mseg_key.

*APPEND c_ra_xauto to lt_ra_xauto.

  SORT lt_mseg_key BY mblnr mjahr zeile.

  LOOP AT lt_mseg_key ASSIGNING <ls_mseg_key>.
    MOVE-CORRESPONDING <ls_mseg_key> TO ls_group_new.

    IF ls_group_new <> ls_group_old.
      PERFORM  fill_table_g_t_mseg_or_group
*            using     lt_ra_xauto
              CHANGING  lt_mseg_key_group
                        lt_mseg_key
                        ct_mseg_or.

      MOVE ls_group_new TO ls_group_old.
      REFRESH lt_mseg_key_group.
    ENDIF.

    APPEND <ls_mseg_key>  TO lt_mseg_key_group.

  ENDLOOP.

  PERFORM  fill_table_g_t_mseg_or_group
*            using     lt_ra_xauto
             CHANGING  lt_mseg_key_group
                       lt_mseg_key
                       ct_mseg_or.
ENDFORM.                    " FILL_TABLE_G_T_MSEG_OR         "n1481757
*&---------------------------------------------------------------------*
*&      Form  FILL_TABLE_G_T_MSEG_OR_GROUP
*&---------------------------------------------------------------------*
FORM fill_table_g_t_mseg_or_group                           "n1481757
*            using    ut_ra_xauto       like gt_ra_xauto
            CHANGING ct_mseg_key_group TYPE stab_mseg_xauto
                     ct_mseg_key       TYPE stab_mseg_xauto
                     ct_mseg_or        TYPE stab_mseg_xauto.

*--------------------------------------------------------------------*
  DATA: ls_mseg_key_group LIKE LINE OF ct_mseg_key_group,
        ls_mseg_or        LIKE LINE OF  ct_mseg_or.
*      lt_mseg_archiv Like mseg.

  DATA : BEGIN OF lt_mseg_archiv  OCCURS 0.
      INCLUDE STRUCTURE   mseg.
  DATA : END OF lt_mseg_archiv.

  FIELD-SYMBOLS : <ls_mseg_archiv> LIKE LINE OF lt_mseg_archiv.

*--------------------------------------------------------------------*

  READ TABLE ct_mseg_key_group INTO ls_mseg_key_group
      INDEX 1.

  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  READ TABLE  ht_mmdocs_arch INTO wa_hashtable
     WITH TABLE KEY mblnr = ls_mseg_key_group-mblnr
                    mjahr = ls_mseg_key_group-mjahr.

* check if there are mm docs from the archive

  IF sy-subrc <> 0.
    APPEND LINES OF ct_mseg_key_group TO g_t_mseg_key_te.
    RETURN.
  ENDIF.

  CALL FUNCTION 'ASH_MM_MATBEL_READ'
    EXPORTING
      i_archivekey   = wa_hashtable-archivekey
      i_offset       = wa_hashtable-offset
    TABLES
      et_mseg        = lt_mseg_archiv
    EXCEPTIONS
      not_in_archive = 1
      OTHERS         = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  LOOP AT lt_mseg_archiv ASSIGNING <ls_mseg_archiv>.

    READ TABLE ct_mseg_key_group INTO ls_mseg_key_group
      WITH KEY mblnr = <ls_mseg_archiv>-mblnr
               mjahr = <ls_mseg_archiv>-mjahr
               zeile = <ls_mseg_archiv>-zeile BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.
*  delete ct_mseg_key_group.

    IF ls_mseg_key_group-xauto IN g_ra_xauto.
      MOVE-CORRESPONDING <ls_mseg_archiv> TO ls_mseg_or.
      APPEND ls_mseg_or TO ct_mseg_or.
    ENDIF.

    IF ct_mseg_key_group IS INITIAL.
      EXIT.
    ENDIF.

  ENDLOOP.                                                  "n1481757
ENDFORM.             " FILL_TABLE_G_T_MSEG_OR_GROUP          "n1481757
