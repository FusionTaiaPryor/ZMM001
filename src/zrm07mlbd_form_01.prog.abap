*&---------------------------------------------------------------------*
*&  Include           RM07MLBD_FORM_01                                 *
*&---------------------------------------------------------------------*

* new function April 2012 EH                                "n1710850
* - Installed ability for secondary database connection     "n1710850
*   configuration via Tx HDBC                               "n1710850

* correction Feb. 2007                                      "n1031056
* incorrect results for subcontractor special stocks of     "n1031056
* materials with batch management                           "n1031056

* correction May 2006 MM                                    "n944522
* - the negative sign was not set for GI postings           "n944522

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

* correction Aug. 2005 MM                                   "n856424
* - the fields "entry time", "entry date", and "User" are   "n856424
*   are not filled filled for price change documents        "n856424

* MB5B improved regarding accessibilty                      "n773673

* Improvements :                       March 2003 MM        "n599218
* - print the page numbers                                  "n599218
* - send warning M7 393 when user deletes the initial       "n599218
*   display variant                                         "n599218
* - show the current activity and the progress              "n599218

* contains FORM routines without preprocessor commands and  "n547170
* no text elements                                          "n547170

*&---------------------------------------------------------------------*
*&      Form  INITIALISIERUNG
*&---------------------------------------------------------------------*
*       Vorbelegung der Anzeigevariante                                *
*----------------------------------------------------------------------*

FORM initialisierung.

  repid = sy-repid.
  variant_save = 'A'.
  CLEAR variante.
  variante-report = repid.
* Default-Variante holen:
  def_variante = variante.

  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save     = variant_save
    CHANGING
      cs_variant = def_variante
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 0.
*   save the initial, e.g. default variant                  "n599218
    MOVE  def_variante-variant  TO  alv_default_variant.    "n599218
    p_vari = def_variante-variant.
  ENDIF.
*  print-no_print_listinfos = 'X'.

* show the block with the parameters for the best database  "n921165
* access depending on the database system                   "n921165
  MOVE  sy-dbsys+0(03)       TO  g_f_database.              "n921165
                                                            "n921165
  BREAK-POINT                ID mmim_rep_mb5b.              "n921164
* dynamic break-point : check installed database system     "n921165
                                                            "n921165
  IF  g_f_database = 'INF'   OR                             "n921165
      g_f_database = 'DB6'   OR                             "n921165
      g_f_database = 'MSS'   OR   " consider MSSQL, too     "n921165
      g_f_database = 'ADA'.                                 "n921165
*   show the 3 parameters                                   "n921165
    MOVE : 'X'               TO  g_flag_db_parameters.      "n921165
  ELSE.                                                     "n921165
*   ORACLE and DB2 work hist histograms                     "n921165
*   do not show the 3 parameters on the selection screen    "n921165
    CLEAR                    g_flag_db_parameters.          "n921165
*   set hidden parameters                                   "n921165
    MOVE  'X'                TO  pa_dbstd.                  "n921165
    CLEAR :                  pa_dbmat, pa_dbdat.            "n921165
  ENDIF.                                                    "n921165

  CALL FUNCTION 'MB_CHECK_MSEG_CONVERSION_DONE'             "n1558298
    IMPORTING                                               "n1558298
      e_conversion_done = g_f_msegex_act.             "n1558298

ENDFORM.                               " INITIALISIERUNG

*&---------------------------------------------------------------------*
*&      Form  AKTUELLE_BESTAENDE
*&---------------------------------------------------------------------*
*      Ermittlung der aktuellen eigenen Bestände,
*      d.h. der bewerteten Bestände und des Retourensperrbestandes,
*      auf Lagerortebene und auf Material- bzw. Chargenebene;
*      folgende Sonderbestände können gesondert ausgewiesen werden:
*       Lohnbearbeitung         ( Sonderbestandskennzeichen  O )
*       Kundenkonsignation      (             "              V, W, M )
*       Lieferantenkonsignation (             "              K )
*       Projektbestand          (             "              Q )
*       Kundenauftragsbestand   (             "              E )
*----------------------------------------------------------------------*

FORM aktuelle_bestaende.

  DATA: lt_plant          TYPE STANDARD TABLE OF t001w-werks,
        lo_converter_osql TYPE REF TO if_auth_objects_to_sql. "vn_1899544

  FIELD-SYMBOLS: <lv_plant> TYPE t001w-werks.

  IF bwbst = abap_false AND gv_optimization_active = abap_true. "2195175
    lo_converter_osql = cl_auth_objects_to_sql=>create_for_open_sql( ).
    lo_converter_osql->add_authorization_object(
            EXPORTING iv_authorization_object = 'M_MSEG_WMB'
                     it_activities = VALUE #(
                                     ( auth_field = 'ACTVT' value = '03' )
                                                                 )
                     it_field_mapping = VALUE #(
                                     ( auth_field = 'WERKS'
                                       view_field = VALUE #( table_ddic_name = 'T001W'
                                                             table_alias = ''
                                                             field_name = 'WERKS' ) )
                                                        ) ).
    CLEAR: gv_where_clause, gv_not_authorized.
    TRY.
        gv_where_clause = lo_converter_osql->get_sql_condition( ).
      CATCH cx_auth_not_authorized.
        gv_not_authorized = abap_true.
    ENDTRY.
  ENDIF.                                                    "^n_1899544

* delete the range tables for the creation of table g_t_organ
  IF  g_t_organ[] IS INITIAL.                               "n433765
    REFRESH : g_0000_ra_werks, g_0000_ra_bwkey, g_0000_ra_bukrs.
    CLEAR   : g_0000_ra_werks, g_0000_ra_bwkey, g_0000_ra_bukrs.
  ENDIF.

* Begin of correction 1916359
* Retrieve plant records for which the user has no authority to issue the corresponding authority message
* Only for compatibility reasons after code pushdown of authority check to DB
  IF gv_where_clause IS NOT INITIAL AND gv_not_authorized = abap_false AND NOT bwbst = 'X'.
    IF lgbst = 'X' AND xchar = ' '.
      PERFORM hdb_check_table USING 'MARD' ''.
*     MMACCESS_2015W2_ADJUSTED
      SELECT DISTINCT werks FROM v_mard_md INTO TABLE lt_plant  CONNECTION (dbcon) "sLog
                                                 WHERE werks IN g_ra_werks
                                                 AND NOT (gv_where_clause)
                                                 AND lgort IN g_ra_lgort
                                                 AND matnr IN matnr.
    ELSEIF lgbst = 'X' AND xchar = 'X' AND xnomchb IS NOT INITIAL.
      PERFORM hdb_check_table USING 'MCHA' ''.              "n1710850
      SELECT DISTINCT werks FROM mcha INTO TABLE lt_plant CONNECTION (dbcon)
                                                 WHERE werks IN g_ra_werks
                                                 AND NOT (gv_where_clause)
                                                 AND matnr IN matnr
                                                 AND charg IN charg.
    ELSEIF sbbst = 'X'.
      CASE    sobkz.
        WHEN  'O'.
          PERFORM hdb_check_table USING 'MSLB' ''.
*         MMACCESS_2015W2_ADJUSTED
          SELECT DISTINCT werks FROM v_mslb_md INTO TABLE lt_plant CONNECTION (dbcon) "sLog
                                                     WHERE werks IN g_ra_werks
                                                     AND NOT (gv_where_clause)
                                                     AND matnr IN matnr
                                                     AND charg IN charg
                                                     AND sobkz = sobkz.
        WHEN  'V' OR  'W'.
          PERFORM hdb_check_table USING 'MSKU' ''.
*         MMACCESS_2015W2_ADJUSTED
          SELECT DISTINCT werks FROM v_msku_md INTO TABLE lt_plant CONNECTION (dbcon) "sLog
                                                     WHERE werks IN g_ra_werks
                                                     AND NOT (gv_where_clause)
                                                     AND matnr IN matnr
                                                     AND charg IN charg
                                                     AND sobkz = sobkz.
        WHEN  'K' OR  'M'.
          PERFORM hdb_check_table USING 'MKOL' ''.
*         MMACCESS_2015W2_ADJUSTED
          SELECT DISTINCT werks FROM v_mkol_md INTO TABLE lt_plant CONNECTION (dbcon) "sLog
                                                     WHERE werks IN g_ra_werks
                                                     AND NOT (gv_where_clause)
                                                     AND lgort IN g_ra_lgort
                                                     AND matnr IN matnr
                                                     AND charg IN charg
                                                     AND sobkz = sobkz.
        WHEN  'Q'.
          PERFORM hdb_check_table USING 'MSPR' ''.
*         MMACCESS_2015W2_ADJUSTED
          SELECT DISTINCT werks FROM v_mspr_md INTO TABLE lt_plant CONNECTION (dbcon) "sLog
                                                     WHERE werks IN g_ra_werks
                                                     AND NOT (gv_where_clause)
                                                     AND lgort IN g_ra_lgort
                                                     AND matnr IN matnr
                                                     AND charg IN charg
                                                     AND sobkz = sobkz.
        WHEN  'E' OR 'T'.
          PERFORM hdb_check_table USING 'MSKA' ''.
*         MMACCESS_2015W2_ADJUSTED
          SELECT DISTINCT werks FROM v_mska_md INTO TABLE lt_plant CONNECTION (dbcon) "sLog
                                                     WHERE werks IN g_ra_werks
                                                     AND NOT (gv_where_clause) "n_1899544
                                                     AND lgort IN g_ra_lgort
                                                     AND matnr IN matnr
                                                     AND charg IN charg
                                                     AND sobkz = sobkz.
        WHEN  OTHERS.
      ENDCASE.
    ENDIF.
    LOOP AT lt_plant ASSIGNING <lv_plant>.
      PERFORM f9000_auth_plant_check USING <lv_plant>.
    ENDLOOP.
  ENDIF.
* End of correction 1916359

  IF      bwbst = 'X'.
*   select the valuated stocks
    PERFORM                  aktuelle_bst_bwbst.

  ELSEIF lgbst = 'X'.
*   all own stock from storage locations or batches
    IF xchar = ' '.
      PERFORM                aktuelle_bst_lgbst_mard.
    ELSEIF  xchar = 'X'.
      PERFORM                aktuelle_bst_lgbst_xchar.
    ENDIF.

  ELSEIF   sbbst = 'X'.
*    special stocks
*    ENHANCEMENT-SECTION     aktuelle_bestaende_01 SPOTS es_rm07mlbd.

    "{ Begin ENHO DIPCS_RM07MLBD_FORM_01 IS-AD-SSP AD_SUB }
* DI A&D SSP
    CASE    sobkz.
      WHEN  'O'.
        PERFORM              aktuelle_bst_sbbst_o.
      WHEN  'V' OR  'W'.
        PERFORM              aktuelle_bst_sbbst_v_w.
      WHEN  'K' OR  'M'.
        PERFORM              aktuelle_bst_sbbst_k_m.
      WHEN  'Q'.
        PERFORM              aktuelle_bst_sbbst_q.
      WHEN  'E'.
        PERFORM              aktuelle_bst_sbbst_e.
** DI IS-ADEC-SSP Customer Stock
      WHEN  'B'.
        PERFORM              aktuelle_bst_sbbst_b.
** A&D IS-ADEC-SUB Customer Stock with Vendor               "v_GA1551829
      WHEN  cl_adsub_constants=>c.
        PERFORM              aktuelle_bst_sbbst_c.
** A&D IS-ADEC-SUB Sales Order Stock with Vendor
      WHEN  cl_adsub_constants=>f.
        PERFORM              aktuelle_bst_sbbst_f.
** A&D IS-ADEC-SUB Project Stock with Vendor
      WHEN  cl_adsub_constants=>r.
        PERFORM              aktuelle_bst_sbbst_r.
** A&D IS-ADEC-SUB Vendor Consignment / RTP Stock with Vendor
      WHEN  cl_adsub_constants=>i OR  cl_adsub_constants=>j.
        PERFORM              aktuelle_bst_sbbst_i_j.       "^_GA1551829
      WHEN  cl_adsub_constants=>t.                            "SIT SPAU
        PERFORM              aktuelle_bst_sbbst_t.            "SIT SPAU
      WHEN  OTHERS.
*       Angegebener Sonderbestand nicht vorhanden.
        MESSAGE s290.
        PERFORM              anforderungsbild.
    ENDCASE.
    "{ End ENHO DIPCS_RM07MLBD_FORM_01 IS-AD-SSP AD_SUB }

*    END-ENHANCEMENT-SECTION.
  ENDIF.

* create table g_t_organ with the plants and valuation areas from
* the database selection if table g_t_organ is empty
  PERFORM  f0000_create_table_g_t_organ
                             USING  c_no_error.

ENDFORM.                     "aktuelle_bestaende.

*&---------------------------------------------------------------------*
*&   AKTUELLE_BST_LGBST_MARD
*&---------------------------------------------------------------------*

FORM aktuelle_bst_lgbst_mard.
*---------------- eigener Bestand auf Lagerortebene -------------------*
*---------------- ... auf Materialebene -------------------------------*

*  ENHANCEMENT-SECTION     aktuelle_bst_lgbst_mard_01 SPOTS es_rm07mlbd.

  "{ Begin ENHO AD_MPN_PUR2_RM07MLBD IS-AD-MPN-MD AD_MPN }
  IF cl_immpn_cust=>check_mpn_active( ) = abap_true.
* DI A&D MPN
    IF mfrpn[] IS INITIAL.
      PERFORM hdb_check_table USING 'MARD' ''.              "1779989
      IF gv_not_authorized = abap_false.                    "2038356

        SELECT * FROM mard INTO CORRESPONDING FIELDS OF TABLE imard CONNECTION (dbcon) "1779989
                                           WHERE werks IN  g_ra_werks
                                           AND   (gv_where_clause) "2038356
                                           AND   lgort IN  g_ra_lgort
                                           AND   matnr IN matnr.
      ELSE.                                                 "2038356
        sy-subrc = 4.                                       "2038356
      ENDIF.                                                "2038356

*       LOOP AT IMARD.
*         MOVE-CORRESPONDING IMARD TO L_BUFFER.
*         APPEND L_BUFFER.
*       ENDLOOP.
    ELSE.
      SELECT * FROM mard AS e INNER JOIN mara AS m
        ON e~matnr = m~matnr
        INTO CORRESPONDING FIELDS OF imard
      WHERE e~werks IN g_ra_werks
        AND   e~lgort IN g_ra_lgort
        AND   e~matnr IN matnr
        AND   m~mfrpn IN mfrpn.

*        MOVE-CORRESPONDING wa_mpn_mard-mpn_mard TO imard.
*        APPEND imard.
*                MOVE-CORRESPONDING WA_MPN_MARD-MPN_MARD TO L_BUFFER.
*                APPEND L_BUFFER.
      ENDSELECT.
    ENDIF.
*    CALL FUNCTION 'MPN01_BUFFER_MATNR_MPN'
*         TABLES
*              T_BUFFER = L_BUFFER.
*
*
  ELSE.

    PERFORM hdb_check_table USING 'MARD' ''.
    IF gv_not_authorized = abap_false.                      "n_1899544
      SELECT * FROM mard INTO CORRESPONDING FIELDS OF TABLE imard CONNECTION (dbcon) "n1710850
                                             WHERE werks IN g_ra_werks
                                             AND   (gv_where_clause) "n_1899544
                                             AND   lgort IN g_ra_lgort
                                             AND   matnr IN matnr.
    ELSE.                                                   "n_1899544
      sy-subrc = 4.                                         "n_1899544
    ENDIF.                                                  "n_1899544
  ENDIF.
  "{ End ENHO AD_MPN_PUR2_RM07MLBD IS-AD-MPN-MD AD_MPN }




*  END-ENHANCEMENT-SECTION.

  IF sy-subrc NE 0.          "no records found ?
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM                  anforderungsbild.
  ENDIF.

* does the user has the the authority for the found entries ?
  LOOP AT imard.                                            "v2195175
    PERFORM    f9000_auth_plant_check
                             USING  imard-werks.

    IF  g_flag_authority IS INITIAL.
      DELETE             imard.
    ELSE.
      PERFORM  f9200_collect_plant     USING  imard-werks.

      PERFORM  f9400_material_key      USING  imard-matnr.
    ENDIF.
  ENDLOOP.                                                  "^2195175

  DESCRIBE TABLE imard       LINES g_f_cnt_lines.
  IF  g_f_cnt_lines IS INITIAL.       "no records left  ?
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM                  anforderungsbild.
  ENDIF.

  IF NOT charg-low IS INITIAL OR NOT charg-high IS INITIAL.
    CLEAR charg.
    MESSAGE w285.
*   Charge wird zurückgesetzt.
  ENDIF.

ENDFORM.                     "aktuelle_bst_lgbst_mard

*----------------------------------------------------------------------*
*    AKTUELLE_BST_LGBST_XCHAR
*----------------------------------------------------------------------*

FORM aktuelle_bst_lgbst_xchar.

* read the stock table mchb for batches
  PERFORM hdb_check_table USING 'MCHB' ''.                  "n1710850

  IF gv_not_authorized = abap_false.                        "n_1899544
    SELECT * FROM mchb INTO CORRESPONDING FIELDS OF TABLE imchb CONNECTION (dbcon) "n1710850
                               WHERE   werks  IN  g_ra_werks
                                 AND   (gv_where_clause)    "n_1899544
                               AND   lgort  IN  g_ra_lgort
                               AND   matnr  IN  matnr
                               AND   charg  IN  charg.
  ENDIF.                                                    "n_1899544

  DESCRIBE TABLE imchb       LINES  g_f_cnt_lines.
  IF g_f_cnt_lines IS INITIAL         "no records found ?
       AND xnomchb IS INITIAL.                              "n1404822

    MESSAGE s821 WITH matnr-low werks-low lgort-low.
*   Keine Chargen zu Material & in Werk & Lagerort & vorhanden.
    PERFORM anforderungsbild.
  ENDIF.


  IF xnomchb IS NOT INITIAL.                                "v_n1404822
* read the table mcha for batches
    PERFORM hdb_check_table USING 'MCHA' ''.                "n1710850
    IF gv_not_authorized = abap_false.                      "n_1899544
      SELECT * FROM mcha INTO CORRESPONDING FIELDS OF TABLE imcha CONNECTION (dbcon) "n1710850
                                WHERE   werks  IN  g_ra_werks
                                  AND   (gv_where_clause)   "n_1899544
                                  AND   matnr  IN  matnr
                                  AND   charg  IN  charg.
    ENDIF.                                                  "n_1899544

    DESCRIBE TABLE imcha       LINES  g_f_cnt_lines.
    IF g_f_cnt_lines IS INITIAL.         "no records found ?
      MESSAGE s821 WITH matnr-low werks-low lgort-low.
*   Keine Chargen zu Material & in Werk & Lagerort & vorhanden.
      PERFORM anforderungsbild.
    ENDIF.

* process working table with the batches
    LOOP AT imcha.                                          "v2195175
*     does the user has the the authority for the found entries ?
      PERFORM    f9000_auth_plant_check
                               USING  imcha-werks.

      IF  g_flag_authority IS INITIAL.
        DELETE             imcha.
      ELSE.
        PERFORM  f9200_collect_plant     USING  imcha-werks.

        PERFORM  f9400_material_key      USING  imcha-matnr.
      ENDIF.
    ENDLOOP.
  ENDIF.                                                    "^2195175


* process working table with the batches
  LOOP AT imchb.                                            "v2195175
*   does the user has the the authority for the found entries ?
    PERFORM    f9000_auth_plant_check
                             USING  imchb-werks.

    IF  g_flag_authority IS INITIAL.
      DELETE             imchb.
    ELSE.
      PERFORM  f9200_collect_plant     USING  imchb-werks.

      PERFORM  f9400_material_key      USING  imchb-matnr.
    ENDIF.
  ENDLOOP.                                                  "^2195175

ENDFORM.                     "aktuelle_bst_lgbst_xchar

*----------------------------------------------------------------------*
*    AKTUELLE_BST_SBBST_O
*----------------------------------------------------------------------*

FORM aktuelle_bst_sbbst_o.

* process Special Stocks with Vendor
* Bemerkung: Im Gegensatz zu den anderen Sonderbeständen existieren
*            der Lohnbearbeitungs- und Kundenkonsignationsbestand
*            nur auf Werksebene.
  PERFORM hdb_check_table USING 'MSLB' ''.                  "n1710850
  IF gv_not_authorized = abap_false.                        "n_1899544
    SELECT * FROM mslb INTO CORRESPONDING FIELDS OF TABLE xmslb CONNECTION (dbcon) "n1710850
                               WHERE  werks  IN  g_ra_werks
                                 AND  (gv_where_clause)     "n_1899544
                               AND  matnr  IN  matnr
                               AND  charg  IN  charg
                               AND  sobkz  =   'O'.
  ELSE.                                                     "n_1899544
    sy-subrc = 4.                                           "n_1899544
  ENDIF.                                                    "n_1899544

  IF sy-subrc <> 0.                     "no records found ?
    MESSAGE s289.
*    Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

* process the found records special stock vendor
  LOOP AT xmslb.                                            "v2195175
*   check the authority
    PERFORM  f9000_auth_plant_check
                             USING      xmslb-werks.

    IF  g_flag_authority IS INITIAL.
      DELETE                 xmslb.
    ELSE.
*     fill range table g_0000_ra_werks if it is still empty
      PERFORM  f9200_collect_plant     USING  xmslb-werks.

      PERFORM  f9400_material_key      USING  xmslb-matnr.
    ENDIF.
  ENDLOOP.                                                  "^2195175

* error, if no records are left
  DESCRIBE TABLE xmslb       LINES g_f_cnt_lines.
  IF  g_f_cnt_lines IS INITIAL.
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

  SORT xmslb.
  LOOP AT xmslb.
    MOVE-CORRESPONDING xmslb TO imslb.
    COLLECT imslb.
  ENDLOOP.
  FREE xmslb. REFRESH xmslb.

  IF xchar = ' '.
    LOOP AT imslb.
      MOVE-CORRESPONDING imslb TO imslbx.
      COLLECT imslbx.
    ENDLOOP.
    SORT imslbx.
  ELSEIF xchar = 'X'.
    LOOP AT imslb.
      CHECK imslb-charg IS INITIAL.
      DELETE imslb.
    ENDLOOP.
  ENDIF.

ENDFORM.                     "aktuelle_bst_sbbst_o.

*----------------------------------------------------------------------*
*    AKTUELLE_BST_SBBST_V_W
*----------------------------------------------------------------------*

FORM aktuelle_bst_sbbst_v_w.
*---------------- Sonderbestand Kundenkonsignation --------------------*
*   elseif sobkz = 'V' or sobkz = 'W'.
  PERFORM hdb_check_table USING 'MSKU' ''.                  "n1710850
  IF gv_not_authorized = abap_false.                        "n_1899544
    SELECT * FROM msku INTO CORRESPONDING FIELDS OF TABLE xmsku CONNECTION (dbcon) "n1710850
                                       WHERE werks IN g_ra_werks
                                       AND   (gv_where_clause) "n_1899544
                                     AND   matnr IN matnr
                                     AND   charg IN charg
                                     AND   sobkz EQ sobkz.
  ELSE.                                                     "n_1899544
    sy-subrc = 4.                                           "n_1899544
  ENDIF.                                                    "n_1899544

  IF sy-subrc <> 0.          "no records found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

* process Special Stocks with Customer
  LOOP AT xmsku.                                            "v2195175
    PERFORM  f9000_auth_plant_check    USING     xmsku-werks.

    IF  g_flag_authority IS INITIAL.
      DELETE                 xmsku.
    ELSE.
      PERFORM  f9200_collect_plant     USING  xmsku-werks.

      PERFORM  f9400_material_key      USING  xmsku-matnr.
    ENDIF.
  ENDLOOP.                                                  "^2195175

  DESCRIBE TABLE xmsku       LINES  g_f_cnt_lines.
  IF g_f_cnt_lines IS INITIAL.         "no records found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

  SORT xmsku.
  LOOP AT xmsku.
    MOVE-CORRESPONDING xmsku TO imsku.
    COLLECT imsku.
  ENDLOOP.
  FREE xmsku. REFRESH xmsku.

  IF xchar = ' '.
    LOOP AT imsku.
      MOVE-CORRESPONDING imsku TO imskux.
      COLLECT imskux.
    ENDLOOP.
    SORT imskux.
  ELSEIF xchar = 'X'.
    LOOP AT imsku.
      CHECK imsku-charg IS INITIAL.
      DELETE imsku.
    ENDLOOP.
  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE s042.                                    "#EC *    "n443935
*       Charge ist nicht vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

ENDFORM.                     "aktuelle_bst_sbbst_v_w

*----------------------------------------------------------------------*
*    AKTUELLE_BST_SBBST_K_M
*----------------------------------------------------------------------*

FORM aktuelle_bst_sbbst_k_m.
*-------------- Sonderbestand Lieferantenkonsignation -----------------*
*   elseif sobkz = 'K' or sobkz = 'M'.
  PERFORM hdb_check_table USING 'MKOL' ''.                  "n1710850
  IF gv_not_authorized = abap_false.                        "n_1899544
    SELECT * FROM mkol INTO CORRESPONDING FIELDS OF TABLE xmkol CONNECTION (dbcon) "n1710850
                                      WHERE werks IN g_ra_werks
                                      AND   (gv_where_clause) "n_1899544
                                    AND   lgort IN g_ra_lgort
                                    AND   matnr IN matnr
                                    AND   charg IN charg
                                    AND   sobkz EQ sobkz.
  ELSE.                                                     "n_1899544
    sy-subrc = 4.                                           "n_1899544
  ENDIF.                                                    "n_1899544

  IF sy-subrc <> 0.          "no records found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

* process Special Stocks from Vendor
  LOOP AT xmkol.                                            "v2195175
    PERFORM  f9000_auth_plant_check    USING  xmkol-werks.

    IF  g_flag_authority IS INITIAL.
      DELETE             xmkol.
    ELSE.
      PERFORM  f9200_collect_plant     USING  xmkol-werks.

      PERFORM  f9400_material_key      USING  xmkol-matnr.
    ENDIF.
  ENDLOOP.                                                  "^2195175

  DESCRIBE TABLE xmkol       LINES  g_f_cnt_lines.
  IF g_f_cnt_lines IS INITIAL.         "no records found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

  SORT xmkol.
  LOOP AT xmkol.
    MOVE-CORRESPONDING xmkol TO imkol.
    COLLECT imkol.
  ENDLOOP.
  FREE xmkol. REFRESH xmkol.

  IF xchar = ' '.
    LOOP AT imkol.
      MOVE-CORRESPONDING imkol TO imkolx.
      COLLECT imkolx.
    ENDLOOP.
    SORT imkolx.
  ELSEIF xchar = 'X'.
    LOOP AT imkol.
      CHECK imkol-charg IS INITIAL.
      DELETE imkol.
    ENDLOOP.
  ENDIF.

  IF sy-subrc NE 0.
    MESSAGE s042.                                    "#EC *    "n443935
*       Charge ist nicht vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

ENDFORM.                     "aktuelle_bst_sbbst_k_m.

*----------------------------------------------------------------------*
*    AKTUELLE_BST_SBBST_Q
*----------------------------------------------------------------------*

FORM aktuelle_bst_sbbst_q.
*----------------------- Projektbestand -------------------------------*
*   elseif sobkz = 'Q'.
  PERFORM hdb_check_table USING 'MSPR' ''.                  "n1710850
  IF gv_not_authorized = abap_false.                        "n_1899544
    SELECT * FROM mspr INTO CORRESPONDING FIELDS OF TABLE xmspr CONNECTION (dbcon) "n1710850
                                       WHERE werks IN g_ra_werks
                                       AND   (gv_where_clause) "n_1899544
                                     AND   lgort IN g_ra_lgort
                                     AND   matnr IN matnr
                                     AND   charg IN charg
                                     AND   sobkz EQ sobkz.
  ELSE.                                                     "n_1899544
    sy-subrc = 4.                                           "n_1899544
  ENDIF.                                                    "n_1899544

  IF sy-subrc <> 0.          "no record found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

* process project stock
  LOOP AT xmspr.                                            "v2195175
    PERFORM  f9000_auth_plant_check    USING  xmspr-werks.

    IF  g_flag_authority IS INITIAL.
      DELETE                 xmspr.
    ELSE.
      PERFORM  f9200_collect_plant     USING  xmspr-werks.

      PERFORM  f9400_material_key      USING  xmspr-matnr.
    ENDIF.
  ENDLOOP.                                                  "^2195175

  DESCRIBE TABLE xmspr       LINES  g_f_cnt_lines.
  IF  g_f_cnt_lines IS INITIAL.        "no record left
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

  SORT xmspr.
  LOOP AT xmspr.
    MOVE-CORRESPONDING xmspr TO imspr.
    COLLECT imspr.
  ENDLOOP.
  FREE xmspr. REFRESH xmspr.

  IF xchar = ' '.
    LOOP AT imspr.
      MOVE-CORRESPONDING imspr TO imsprx.
      COLLECT imsprx.
    ENDLOOP.
    SORT imsprx.
  ELSEIF xchar = 'X'.
    LOOP AT imspr.
      CHECK imspr-charg IS INITIAL.
      DELETE imspr.
    ENDLOOP.
  ENDIF.

ENDFORM.:                     "aktuelle_bst_sbbst_q

*----------------------------------------------------------------------*
*    AKTUELLE_BST_SBBST_E
*----------------------------------------------------------------------*

FORM aktuelle_bst_sbbst_e.
*---------------------- Kundenauftragsbestand -------------------------*
  PERFORM hdb_check_table USING 'MSKA' ''.                  "n1710850
  IF gv_not_authorized = abap_false.                        "n_1899544
    SELECT * FROM mska INTO CORRESPONDING FIELDS OF TABLE xmska CONNECTION (dbcon) "n1710850
                                       WHERE werks IN g_ra_werks
                                       AND   (gv_where_clause) "n_1899544
                                     AND   lgort IN g_ra_lgort
                                     AND   matnr IN matnr
                                     AND   charg IN charg
                                     AND   sobkz EQ sobkz.
  ELSE.                                                     "n_1899544
    sy-subrc = 4.                                           "n_1899544
  ENDIF.                                                    "n_1899544

  IF sy-subrc <> 0.            "no records found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

* process Sales Order Stock
  LOOP AT xmska.                                            "v2195175
    PERFORM  f9000_auth_plant_check    USING  xmska-werks.

    IF  g_flag_authority IS INITIAL.
      DELETE                   xmska.
    ELSE.
      PERFORM  f9200_collect_plant     USING  xmska-werks.

      PERFORM  f9400_material_key      USING  xmska-matnr.
    ENDIF.
  ENDLOOP.                                                  "^2195175

  DESCRIBE TABLE xmska       LINES  g_f_cnt_lines.
  IF  g_f_cnt_lines IS INITIAL.        "no records left ?
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

  SORT xmska.
  LOOP AT xmska.
    MOVE-CORRESPONDING xmska TO imska.
    COLLECT imska.
  ENDLOOP.
  FREE xmska. REFRESH xmska.

  IF xchar = ' '.
    LOOP AT imska.
      MOVE-CORRESPONDING imska TO imskax.
      COLLECT imskax.
    ENDLOOP.
    SORT imskax.
  ELSEIF xchar = 'X'.
    LOOP AT imska.
      CHECK imska-charg IS INITIAL.
      DELETE imska.
    ENDLOOP.
  ENDIF.

ENDFORM.                     "aktuelle_bst_sbbst_e

*----------------------------------------------------------------------*
*    AKTUELLE_BST_SBBST_T
*----------------------------------------------------------------------*

FORM aktuelle_bst_sbbst_t.
*---------------------- Buchungskreisübergreifender Transitbestand ----*
  PERFORM hdb_check_table USING 'MSKA' ''.                  "n1710850
  IF gv_not_authorized = abap_false.                        "n_1899544
    SELECT * FROM mska INTO CORRESPONDING FIELDS OF TABLE xmska CONNECTION (dbcon) "n1710850
                                       WHERE werks IN g_ra_werks
                                       AND   (gv_where_clause) "n_1899544
                                     AND   lgort IN g_ra_lgort
                                     AND   matnr IN matnr
                                     AND   charg IN charg
                                     AND   sobkz EQ sobkz.
  ELSE.                                                     "n_1899544
    sy-subrc = 4.                                           "n_1899544
  ENDIF.                                                    "n_1899544

  IF sy-subrc <> 0.            "no records found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

* process CTS Stock
  LOOP AT xmska.                                            "v2195175
    PERFORM  f9000_auth_plant_check    USING  xmska-werks.

    IF  g_flag_authority IS INITIAL.
      DELETE                   xmska.
    ELSE.
      PERFORM  f9200_collect_plant     USING  xmska-werks.

      PERFORM  f9400_material_key      USING  xmska-matnr.
    ENDIF.
  ENDLOOP.                                                  "^2195175

  DESCRIBE TABLE xmska       LINES  g_f_cnt_lines.
  IF  g_f_cnt_lines IS INITIAL.        "no records left ?
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

  SORT xmska.
  LOOP AT xmska.
    MOVE-CORRESPONDING xmska TO imska.
    COLLECT imska.
  ENDLOOP.
  FREE xmska. REFRESH xmska.

  IF xchar = ' '.
    LOOP AT imska.
      MOVE-CORRESPONDING imska TO imskax.
      COLLECT imskax.
    ENDLOOP.
    SORT imskax.
  ELSEIF xchar = 'X'.
    LOOP AT imska.
      CHECK imska-charg IS INITIAL.
      DELETE imska.
    ENDLOOP.
  ENDIF.

ENDFORM.                     "aktuelle_bst_sbbst_e

*&---------------------------------------------------------------------*
*&      Form  TABELLEN_LESEN
*&---------------------------------------------------------------------*
*       Lesen der Materialkurztexte (Tabelle MAKT),                    *
*       der Mengeneinheiten (Tabelle MARA) und                         *
*       Mengen- und Wertfortschreibung zum Material (Tabelle T134M)    *
*       (Letzteres ist zum Aussortieren der unbewerteten bzw.          *
*       kontierten Warenbewegungen notwendig)                          *
*----------------------------------------------------------------------*

FORM tabellen_lesen.

  IF  NOT g_t_mat_key[] IS INITIAL.                         "n451923
*    ENHANCEMENT-SECTION ehp605_tabellen_lesen_01 SPOTS es_rm07mlbd .
    LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_TABELLEN_LESEN_01\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*   select the material masters
    PERFORM hdb_check_table USING 'MARA' ''.            "1791598/1710850
    SELECT matnr meins mtart
           /cwm/valum                                       "1734569
           /cwm/xcwmat FROM mara CONNECTION (dbcon)     "1791598/1710850
                   INTO CORRESPONDING FIELDS OF TABLE imara
                   FOR ALL ENTRIES IN g_t_mat_key
                             WHERE  matnr  =  g_t_mat_key-matnr.
*    END-ENHANCEMENT-SECTION.

*   select the short text for all materials
*   take only the necessary fields                          "n451923
    PERFORM hdb_check_table USING 'MAKT' ''.                "n1710850
    SELECT matnr maktx       FROM makt CONNECTION (dbcon)   "n1710850
         INTO CORRESPONDING FIELDS OF TABLE g_t_makt        "n451923
                   FOR ALL ENTRIES IN g_t_mat_key
                   WHERE  matnr = g_t_mat_key-matnr
                     AND  spras = sy-langu.

    SORT  imara              BY  matnr.                     "n451923
    SORT  g_t_makt           BY  matnr.                     "n451923
    FREE                     g_t_mat_key.
  ENDIF.

  DATA: BEGIN OF k1 OCCURS 0,
          mtart LIKE t134m-mtart,
        END OF k1.
  REFRESH k1.

  LOOP AT imara.
    k1-mtart = imara-mtart.
    COLLECT k1.
  ENDLOOP.

  IF  NOT k1[] IS INITIAL.                                  "n451923
    SELECT * FROM t134m                                 "#EC CI_GENBUFF
           INTO CORRESPONDING FIELDS OF TABLE it134m
           FOR ALL ENTRIES IN k1         WHERE mtart = k1-mtart
                                         AND   bwkey IN g_ra_bwkey.
  ENDIF.                                                    "n451923

  LOOP AT it134m.
*   read table organ with key bwkey = it134m-bwkey.
    PERFORM  f9300_read_organ
                   USING     c_bwkey     it134m-bwkey.

    IF sy-subrc NE 0.
      DELETE it134m.
    ENDIF.
  ENDLOOP.

* To find postings with valuation string, but without relevance for
* the valuated stock, Big-G recommended this logic:
* Take lines from MSEG where for the combination BUSTW/XAUTO=XBGBB
* there is an entry in T156W with key BSX.
  SELECT bustw xbgbb FROM t156w
                     INTO CORRESPONDING FIELDS OF TABLE it156w
                     WHERE vorsl = 'BSX'.
  SORT it156w BY bustw xbgbb.
  DELETE ADJACENT DUPLICATES FROM it156w.
  DELETE it156w WHERE bustw = space.

ENDFORM.                               " TABELLEN_LESEN

*&---------------------------------------------------------------------*
*&      Form  UNBEWERTET_WEG
*&---------------------------------------------------------------------*
*       Löschen der unbewerteten Materialien aus der internen          *
*       Tabelle IMBEW
*----------------------------------------------------------------------*

FORM unbewertet_weg.

  SORT  it134m               BY bwkey mtart.                "n451923
                                                            "n450764
* delete the materials in plants without valuation          "n450764
  LOOP AT g_t_mbew           INTO  g_s_mbew.                "n450764
    READ TABLE imara                                        "n450764
                   WITH KEY matnr = g_s_mbew-matnr          "n450764
                   BINARY SEARCH.                           "n450764
                                                            "n450764
    READ TABLE it134m WITH KEY bwkey = g_s_mbew-bwkey       "n450764
                               mtart = imara-mtart BINARY SEARCH.
    IF sy-subrc NE 0.
*     message ...
      DELETE                 g_t_mbew.                      "n450764
    ELSE.
      IF it134m-wertu = ' '.
        DELETE               g_t_mbew.                      "n450764
      ELSE.                                                 "n450764
*       enrich the entries with the quantity unit           "n450764
        MOVE    imara-meins  TO    g_s_mbew-meins.          "n450764
        IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_UNBEWERTET_WEG_01\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
          MOVE    imara-/cwm/valum  TO    g_s_mbew-meins.
        ENDIF.
*        ENHANCEMENT-POINT ehp605_unbewertet_weg_01 SPOTS es_rm07mlbd .

        MODIFY  g_t_mbew     FROM  g_s_mbew                 "n450764
                             TRANSPORTING  meins.           "n450764
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " UNBEWERTET_WEG

*&---------------------------------------------------------------------*
*&      Form  FI_BELEGE_LESEN                                          *
*&---------------------------------------------------------------------*
*       Lesen der Buchhaltungsbelege                                   *
*----------------------------------------------------------------------*
*  Beim Erfassen der Werte ist es notwendig, die Buchhaltungsbelege    *
*  zum Material zu lesen, um abweichende Werte zwischen Wareneingang   *
*  und Rechnungseingang sowie Nachbelastungen zu berücksichtigen.      *
*----------------------------------------------------------------------*

FORM fi_belege_lesen.

* Not related to note 184465, but a significant performance issue
* if ORGAN is large due to many plants/storage locations.
  DATA: BEGIN OF t_bwkey OCCURS 0,                          "184465
          bwkey LIKE bsim-bwkey,                            "184465
        END OF t_bwkey.                                     "184465

  LOOP AT g_t_organ          WHERE  keytype  =  c_bwkey.
    MOVE g_t_organ-bwkey     TO  t_bwkey-bwkey.
    COLLECT t_bwkey.                                        "184465
  ENDLOOP.                                                  "184465

  READ TABLE t_bwkey INDEX 1.                               "184465
  CHECK sy-subrc = 0.                                       "184465

  PERFORM hdb_check_table USING 'BSIM' ''.                  "n1710850
  SELECT * FROM bsim  CONNECTION (dbcon)                    "n1710850
         INTO CORRESPONDING FIELDS OF TABLE g_t_bsim_lean   "n443935
           FOR ALL ENTRIES IN t_bwkey   WHERE  bwkey = t_bwkey-bwkey
                                        AND    matnr IN matnr
                                        AND    bwtar IN bwtar
                                        AND    budat >= datum-low.

  LOOP AT g_t_bsim_lean      INTO  g_s_bsim_lean.           "n443935
    PERFORM  f9300_read_organ
                   USING     c_bwkey  g_s_bsim_lean-bwkey.  "n443935

    IF  sy-subrc IS INITIAL.
*     record found : the user has the authority, go on
      MOVE  g_s_organ-bukrs  TO  g_s_bsim_lean-bukrs.       "n443935
      MODIFY  g_t_bsim_lean  FROM  g_s_bsim_lean            "n443935
                             TRANSPORTING  bukrs.           "n451923

*     create working table with the keys for the FI documents
      MOVE-CORRESPONDING  g_s_bsim_lean                     "n443935
                             TO  g_t_bkpf_key.              "n443935
      APPEND                 g_t_bkpf_key.
    ELSE.
      DELETE                 g_t_bsim_lean.                 "n443935
    ENDIF.
  ENDLOOP.

ENDFORM.                               " FI_BELEGE_LESEN

*&---------------------------------------------------------------------*
*&      Form  BELEGE_SORTIEREN
*&---------------------------------------------------------------------*
*    Die Materialbelege werden anhand des Buchungsdatums sortiert.
*    Die Materialbelege mit Buchungsdatum zwischen 'datum-high'
*    und dem aktuellen Datum werden in der internen Tabelle IMSWEG
*    gesammelt, während die Materialbelege mit Buchungsdatum
*    zwischen 'datum-low' und 'datum-high' in der internen Tabelle
*    IMSEG verbleiben.
*----------------------------------------------------------------------*

FORM belege_sortieren.

  aktdat = sy-datlo + 30.
  IF NOT ( datum-high IS INITIAL OR datum-high > aktdat ).
    LOOP AT g_t_mseg_lean    INTO  g_s_mseg_lean
                             WHERE budat > datum-high.
      MOVE-CORRESPONDING g_s_mseg_lean TO imsweg.
      APPEND imsweg.
      DELETE                 g_t_mseg_lean.
    ENDLOOP.
  ENDIF.

  DESCRIBE TABLE imsweg LINES index_2.

ENDFORM.                               " BELEGE_SORTIEREN

*&---------------------------------------------------------------------*
*&      Form  KONTIERT_AUSSORTIEREN
*&---------------------------------------------------------------------*
*       Aussortierung der kontierten Belegpositionen,                  *
*       da diese Mengen nicht bestandsrelevant sind                    *
*----------------------------------------------------------------------*

FORM kontiert_aussortieren.

* process table g_t_mseg_lean
* loop at imseg where kzvbr <> space and                         "144845
*     ( kzbew = 'B' or kzbew = 'F' ).                            "144845
*     read table imara with key matnr = imseg-matnr.
*     read table it134m with key mtart = imara-mtart.
*     if not it134m-mengu is initial and not it134m-wertu is initial.
*  Die Felder 'mengu' und 'wertu' (Mengen- bzw. Wertfortschreibung)
*  sind ab Release 3.0 D auch in die Tabelle MSEG aufgenommen.
*  Die Einträge in der Tabelle T134M stellen nach wie vor die generelle
*  Einstellung dar; auf Positionsebene sind jedoch Abänderungen möglich,
*  die anhand der Einträge in der Tabelle MSEG nachverfolgt werden
*  können.
*       delete imseg.
*     endif.
* endloop.

  DATA : l_f_bwkey           LIKE  t001k-bwkey.             "n497992

  SORT  it134m               BY  bwkey  mtart.              "n497992

  LOOP AT g_t_mseg_lean      INTO  g_s_mseg_lean
                             WHERE  kzvbr <> space
                               AND ( kzbew = 'B' OR kzbew = 'F' ).

*   get the valuation area                                  "n497992
    IF  curm = '3'.                                         "n497992
*     valuation level is company code                       "n497992
      IF  g_s_mseg_lean-bukrs IS INITIAL.                   "n497992
*       get the valuation area for this plant               "n497992
        PERFORM  f9300_read_organ                           "n497992
                   USING     c_werks   g_s_mseg_lean-werks. "n497992
                                                            "n497992
        MOVE  g_s_organ-bwkey     TO  l_f_bwkey.            "n497992
      ELSE.                                                 "n497992
        MOVE  g_s_mseg_lean-bukrs TO  l_f_bwkey.            "n497992
      ENDIF.                                                "n497992
    ELSE.                                                   "n497992
*     valuation level is plant                              "n497992
      MOVE  g_s_mseg_lean-werks   TO  l_f_bwkey.            "n497992
    ENDIF.                                                  "n497992

    READ TABLE imara WITH KEY matnr = g_s_mseg_lean-matnr
                             BINARY SEARCH.

    IF  sy-subrc IS INITIAL.
      READ TABLE it134m      WITH KEY  bwkey = l_f_bwkey    "n497992
                                       mtart = imara-mtart  "n497992
                             BINARY SEARCH.

      IF  sy-subrc IS INITIAL.
        IF NOT it134m-mengu IS INITIAL AND
           NOT it134m-wertu IS INITIAL.
          DELETE              g_t_mseg_lean.
        ENDIF.
      ENDIF.
    ELSE.
      DELETE                  g_t_mseg_lean.
    ENDIF.
  ENDLOOP.

ENDFORM.                               " KONTIERT_AUSSORTIEREN

*&---------------------------------------------------------------------*
*&      Form  BELEGE_ERGAENZEN (engl. enrich documents)
*&---------------------------------------------------------------------*
* Material documents and FI documents from BSIM are merged together.
* Complications:
* - A material document can have more than one FI document.
* - There are FI documents without material documnts
* - There are material documents without FI documents
* - The document type is customizeable
* - There is no link from the materia document position to
*   the FI document entry in BSIM (except URZEILE, but this
*   can be filled incorrectly)
*----------------------------------------------------------------------*

FORM belege_ergaenzen.                         "Version from note 204872

* - show the current activity and the progress              "n599218
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'                 "n599218
    EXPORTING                                               "n599218
      text = TEXT-061.       "Reading FI documents          "n599218

* Eliminate material documents with valuation string, but without
* relevance to the valuated stock. IT156W contains all valuation
* strings with posting key BSX. XBGBB says: "I am an accrural posting".
* For more details please ask Big-G.

  LOOP AT g_t_mseg_lean      INTO  g_s_mseg_lean.
*   special processing for tied empties active ?            "n497992
    IF  NOT g_cust_tied_empties IS INITIAL.                 "n497992
*     look for MM documents with xauto = L and change       "n497992
*     indicators                                            "n497992
      CASE  g_s_mseg_lean-xauto.                            "n497992
        WHEN  'X'.                                          "n497992
        WHEN  space.                                        "n497992

        WHEN  OTHERS.                                       "n547170
*         range table g_ra_xauto contains the special       "n547170
*         indicators for the transfer movements of the      "n547170
*         tied empties                                      "n547170
          IF  g_s_mseg_lean-xauto IN g_ra_xauto.            "n547170
            MOVE  g_s_mseg_lean-xauto                       "n497992
                             TO  g_s_mseg_lean-retail.      "n497992
            CLEAR              g_s_mseg_lean-xauto.         "n497992
            MODIFY  g_t_mseg_lean    FROM  g_s_mseg_lean    "n497992
                             TRANSPORTING xauto retail.     "n497992
          ENDIF.                                            "n547170
      ENDCASE.                                              "n497992
    ENDIF.                                                  "n497992

    READ TABLE it156w        WITH KEY
                             bustw = g_s_mseg_lean-bustw
                             xbgbb = g_s_mseg_lean-xauto
                             TRANSPORTING NO FIELDS
                             BINARY SEARCH.
*ENHANCEMENT-SECTION belege_ergaenzen_02 SPOTS es_rm07mlbd .
    IF sy-subrc <> 0.
      DELETE                 g_t_mseg_lean.
    ELSE.                                                   "n443935
*     enrich the current entry with the company code        "n443935
      PERFORM f9300_read_organ                              "n443935
                   USING     c_werks  g_s_mseg_lean-werks.  "n443935
                                                            "n443935
      CHECK : sy-subrc IS INITIAL.                          "n443935
      MOVE  g_s_organ-bukrs  TO  g_s_mseg_lean-bukrs.       "n443935
      MODIFY  g_t_mseg_lean  FROM  g_s_mseg_lean            "n443935
                             TRANSPORTING  bukrs.           "n451923
    ENDIF.
*END-ENHANCEMENT-SECTION.
  ENDLOOP.

* For all available FI documents from BSIM, read the header data
* from BKPF to get the link to the originating material document.

  IF  NOT g_t_bkpf_key[] IS INITIAL.
*   look for the header of the matching FI documents

    SORT  g_t_bkpf_key       BY  bukrs belnr gjahr.
    DELETE ADJACENT DUPLICATES FROM g_t_bkpf_key.

*   save result from database selection into global hashed  "n856424
*   table g_t_bkpf                                          "n856424
    PERFORM hdb_check_table USING 'BKPF' ''.                "n1710850
    SELECT  *                 FROM bkpf CONNECTION (dbcon)  "n1710850
      INTO CORRESPONDING FIELDS OF TABLE g_t_bkpf           "n856424
           FOR ALL ENTRIES IN g_t_bkpf_key
                   WHERE  bukrs = g_t_bkpf_key-bukrs
                     AND  belnr = g_t_bkpf_key-belnr
                     AND  gjahr = g_t_bkpf_key-gjahr.

    IF  sy-subrc IS INITIAL.
*     create working table l_t_keytab_m
      FREE                   g_t_bkpf_key.

      LOOP AT g_t_bsim_lean  INTO  g_s_bsim_lean.           "n443935
*       enrich the working table g_t_bsim_lean with the     "n443935
*       MM doc info                                         "n443935
                                                            "n443935
*       look for the matching FI document header            "n443935
        READ TABLE g_t_bkpf  ASSIGNING <g_fs_bkpf>          "n856424
                   WITH KEY  bukrs = g_s_bsim_lean-bukrs    "n443935
                             belnr = g_s_bsim_lean-belnr    "n443935
                             gjahr = g_s_bsim_lean-gjahr.   "n443935
                                                            "n443935
        IF  sy-subrc IS INITIAL.                            "n443935
*         enrich table G_T_BSIM_LEAN with the MM doc        "n443935

*         consider only FI docs created by MM docs here     "n856424
          CHECK : <g_fs_bkpf>-awtyp = 'MKPF'.               "n856424
          MOVE  <g_fs_bkpf>-awkey TO  g_s_bsim_lean-awkey.  "n856424

          MODIFY  g_t_bsim_lean   FROM  g_s_bsim_lean       "n443935
                                  TRANSPORTING  awkey.      "n451923
        ENDIF.                                              "n443935
      ENDLOOP.                                              "n443935
                                                            "n443935
      BREAK-POINT                ID mmim_rep_mb5b.          "n921164
*     dynamic break-point : G_T_BSIM_LEAN is available     "n921164
                                                            "n443935
*     sort working table for acces with MM document         "n443935
      SORT  g_t_bsim_lean    BY  bukrs                      "n443935
                                 bwkey                      "n443935
                                 matnr                      "n443935
                                 bwtar                      "n443935
                                 shkzg                      "n443935
                                 meins                      "n443935
                                 budat                      "n443935
                                 blart                      "n443935
                                 awkey.                     "n443935
    ENDIF.
  ENDIF.

* For each material document, write the number of the created
* FI document into IMSEG. If there are more than one FI document,
* the one with the same BLART and the same posting date is chosen.
* BLART alone is not sufficient as the document type of the
* revaluation document is customizeable (T158-BLAUM).
* If a document as been found to have an entry in KEYTAB, this
* entry is marked as "accessed". So later on the FI document is
* known to be already in the list via this material document.

* clear the working areas                                   "n443935
  PERFORM                    belege_ergaenzen_clear.        "n443935
                                                            "n443935
* sort main table with MM document                          "n443935
  SORT  g_t_mseg_lean        BY  bukrs                      "n443935
                                 werks                      "n443935
                                 matnr                      "n443935
                                 bwtar                      "n443935
                                 shkzg                      "n443935
                                 meins                      "n443935
                                 budat                      "n443935
                                 blart                      "n443935
                                 mjahr                      "2117567
                                 mblnr                      "2117567
                                 zeile.                     "2117567
                                                            "n443935
* process the table with the MM docs with control break     "n443935
* the sequential reading is done in a DO-Loop, because      "n451923
* the MODIFY does not refer the current entry               "n451923
  CLEAR                      g_cnt_loop.                    "n451923
                                                            "n451923
  DO.                                                       "n451923
    ADD  1                   TO  g_cnt_loop.                "n443935
                                                            "n451923
    READ TABLE g_t_mseg_lean INTO  g_s_mseg_lean            "n451923
                             INDEX  g_cnt_loop.             "n451923
                                                            "n451923
    IF  sy-subrc <> 0.       "end of table reached ?        "n443935
      EXIT.                                                 "n443935
    ENDIF.                                                  "n443935
                                                            "n443935
*   fill group key                                          "n443935
    MOVE-CORRESPONDING  g_s_mseg_lean                       "n443935
                             TO  g_s_mseg_new.              "n443935
                                                            "n443935
*   valuation area depends on the customizing settings      "n443935
    IF curm = '3'.                                          "n443935
*     the valuation level is company code                   "n443935
      MOVE : g_s_mseg_lean-bukrs                            "n443935
                             TO  g_s_mseg_new-bwkey.        "n443935
    ELSE.                                                   "n443935
*     the valuation level is plant                          "n443935
      MOVE : g_s_mseg_lean-werks                            "n443935
                             TO  g_s_mseg_new-bwkey.        "n443935
    ENDIF.                                                  "n443935
                                                            "n443935
*   control break                                           "n443935
    IF  g_cnt_loop > 1.                                     "n443935
      IF  g_s_mseg_new NE g_s_mseg_old.                     "n443935
        PERFORM              belege_ergaenzen_2.            "n443935
      ENDIF.                                                "n443935
    ENDIF.                                                  "n443935
                                                            "n443935
*   save the entry in the working table for this group      "n443935
    ADD  1                   TO  g_cnt_mseg_entries.        "n443935
    MOVE-CORRESPONDING  g_s_mseg_new                        "n443935
                             TO  g_s_mseg_old.              "n443935
    MOVE-CORRESPONDING  g_s_mseg_lean                       "n451923
                             TO  g_s_mseg_work.             "n451923
    MOVE  g_cnt_loop         TO  g_s_mseg_work-tabix.       "n451923
    APPEND  g_s_mseg_work    TO  g_t_mseg_work.             "n443935
  ENDDO.                                                    "n451923
                                                            "n443935
* process the last group                                    "n443935
  PERFORM                    belege_ergaenzen_2.            "n443935
                                                            "n443935
* Append FI-documents without material documents (price change,
* invoice, revaluation document, ...).

  BREAK-POINT                ID mmim_rep_mb5b.              "n921164
* dynamic break-point : process remaining FI docs          "n921164
                                                            "n443935
* process the remaining FI documents                        "n443935
  LOOP AT g_t_bsim_lean      INTO  g_s_bsim_lean.           "n443935
    CLEAR                    g_s_mseg_lean.                 "n443935
                                                            "n443935
    CASE    g_s_bsim_lean-accessed.                         "n443935
      WHEN  'D'.                                            "n443935
*       this FI could be assigned to a MM doc successfully  "n443935
        CONTINUE.            "-> ignore this entry          "n443935
                                                            "n443935
      WHEN  'X'.                                            "n443935
*       take this entry; but there could be inconsistencies "n443935
*       between the MM and FI documents and set '???' to    "n443935
*       movement type in the list                           "n443935
        MOVE  '???'          TO  g_s_mseg_lean-bwart.       "n443935
                                                            "n443935
      WHEN  OTHERS.                                         "n443935
    ENDCASE.                                                "n443935
                                                            "n443935

*   customizing for the selection of remaining BSIM entries "n497992
*   ( FI document ) without matching MSEG ( MM document )   "n497992
*   like price changes, account adjustments, etc...         "n497992

    IF  g_flag_break-b4 = 'X'.                              "n921164
      BREAK-POINT                ID mmim_rep_mb5b.          "n921164
*     dynamic break-point : stop here when strange          "n921164
*     FI documents are shown                                "n921164
    ENDIF.                                                  "n921164

    IF NOT g_cust_bseg_bsx IS INITIAL.                      "n497992
      DATA l_s_ktosl         LIKE      bseg-ktosl.          "n497992
                                                            "n497992
*     look for the matching BSEG entry                      "n497992
      SELECT SINGLE ktosl    FROM bseg                      "n497992
                             INTO l_s_ktosl                 "n497992
         WHERE  bukrs  =  g_s_bsim_lean-bukrs               "n497992
           AND  belnr  =  g_s_bsim_lean-belnr               "n497992
           AND  gjahr  =  g_s_bsim_lean-gjahr               "n497992
           AND  buzei  =  g_s_bsim_lean-buzei.              "n497992
                                                            "n497992
      IF  sy-subrc IS INITIAL.                              "n497992
        IF l_s_ktosl  =  'BSX'.                             "n497992
*         ok: entry found; transaction key is BSX           "n497992
        ELSE.                                               "n497992
          CONTINUE.          "Do not process this entry     "n497992
        ENDIF.                                              "n497992
      ENDIF.                                                "n497992
    ENDIF.                                                  "n497992

*   create a entry in the main working table G_T_MSEG_LEAN  "n443935
*   for this remaining FI document, delete the quantity,    "n443935
*   and set the info of the original MM doc                 "n443935
    MOVE-CORRESPONDING  g_s_bsim_lean                       "n443935
                             TO  g_s_mseg_lean.             "n443935
    CLEAR                    g_s_mseg_lean-menge.           "n443935
    MOVE : g_s_bsim_lean-awkey                              "n443935
                             TO  matkey,                    "n443935
           matkey-mblnr      TO  g_s_mseg_lean-mblnr,       "n443935
           matkey-mjahr      TO  g_s_mseg_lean-mjahr.       "n443935
                                                            "n443935
    PERFORM  f9300_read_organ                               "n443935
                   USING     c_bwkey  g_s_bsim_lean-bwkey.  "n443935
                                                            "n443935
    MOVE : g_s_organ-werks   TO  g_s_mseg_lean-werks,       "n443935
           g_s_organ-waers   TO  g_s_mseg_lean-waers.       "n443935

*   complete this line with CPU-date, CPU-time and user     "n856424
*   read FI doc header in working table G_T_BKPF            "n856424
    READ TABLE g_t_bkpf      ASSIGNING <g_fs_bkpf>          "n856424
      WITH KEY bukrs  =  g_s_bsim_lean-bukrs                "n856424
               belnr  =  g_s_bsim_lean-belnr                "n856424
               gjahr  =  g_s_bsim_lean-gjahr.               "n856424
                                                            "n856424
    IF sy-subrc IS INITIAL.                                 "n856424
      MOVE : <g_fs_bkpf>-cpudt    TO  g_s_mseg_lean-cpudt,  "n856424
             <g_fs_bkpf>-cputm    TO  g_s_mseg_lean-cputm,  "n856424
             <g_fs_bkpf>-usnam    TO  g_s_mseg_lean-usnam.  "n856424
    ENDIF.                                                  "n856424

    IF gv_switch_ehp6ru = abap_true.
      MOVE-CORRESPONDING g_s_bsim_lean TO g_t_bseg_key.
      APPEND  g_t_bseg_key.
    ENDIF.
    IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
      LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BELEGE_ERGAENZEN_01\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
*
      IF g_s_mseg_lean-mblnr IS INITIAL.
        CLEAR g_s_mseg_lean-menge.
        CLEAR g_s_mseg_lean-meins.
      ENDIF.
    ENDIF.
*    ENHANCEMENT-POINT ehp605_belege_ergaenzen_01 SPOTS es_rm07mlbd .

    APPEND  g_s_mseg_lean    TO  g_t_mseg_lean.             "n443935
  ENDLOOP.                                                  "n443935
                                                            "n443935
  FREE :                     g_t_bsim_lean.                 "n443935
  FREE :                     g_t_bkpf.                      "n856424

  FIELD-SYMBOLS:
    <fs_mseg_lean> TYPE stype_mseg_lean,
    <fs_bseg>      TYPE stype_bseg.

  DATA:
    ls_accdet   TYPE stype_accdet.

* add G/L account data to G_T_MSEG_LEAN
* (if available - from FI doc item, if not - from current settings)
  IF gv_switch_ehp6ru = abap_true.
    SORT g_t_bseg_key BY bukrs belnr gjahr buzei.
    DELETE ADJACENT DUPLICATES FROM g_t_bseg_key.

*   save result from database selection into hashed table
    IF NOT g_t_bseg_key[] IS INITIAL.
      SELECT bukrs belnr gjahr buzei hkont FROM bseg
        INTO CORRESPONDING FIELDS OF TABLE g_t_bseg
        FOR ALL ENTRIES IN g_t_bseg_key
          WHERE bukrs = g_t_bseg_key-bukrs
            AND belnr = g_t_bseg_key-belnr
            AND gjahr = g_t_bseg_key-gjahr
            AND buzei = g_t_bseg_key-buzei
        ORDER BY PRIMARY KEY.
    ENDIF.

    LOOP AT g_t_mseg_lean ASSIGNING <fs_mseg_lean>.
*     look for the matching FI document item
      READ TABLE g_t_bseg ASSIGNING <fs_bseg>
        WITH KEY bukrs = <fs_mseg_lean>-bukrs
                 belnr = <fs_mseg_lean>-belnr
                 gjahr = <fs_mseg_lean>-gjahr
                 buzei = <fs_mseg_lean>-buzei.
      IF  sy-subrc IS INITIAL.
*       enrich table G_T_MSEG_LEAN with the G/L account
        <fs_mseg_lean>-hkont = <fs_bseg>-hkont.
      ELSE.
*       get G/L account from current account determination settings
        CLEAR ls_accdet.
        MOVE-CORRESPONDING <fs_mseg_lean> TO ls_accdet.
        PERFORM get_acc_det CHANGING ls_accdet.
        <fs_mseg_lean>-hkont = ls_accdet-hkont.
      ENDIF.
    ENDLOOP.
    FREE: g_t_bseg_key, g_t_bseg.
  ENDIF.

* filter documents by G/L account, in case G/L account is restricted
  IF gv_switch_ehp6ru = abap_true AND hkont IS NOT INITIAL.
    DELETE g_t_mseg_lean WHERE hkont NOT IN hkont.
*   leave program if no records left
    IF g_t_mseg_lean IS INITIAL.
      MESSAGE s289.
      PERFORM anforderungsbild.
    ENDIF.
  ENDIF.

ENDFORM.                     "belege_ergaenzen

*&---------------------------------------------------------------------*
*&      Form  BESTAENDE_BERECHNEN
*&---------------------------------------------------------------------*
*       Berechnung der Bestände zu 'datum-high' und 'datum-low'        *
*----------------------------------------------------------------------*

FORM bestaende_berechnen.

*------------------- Bestände zu 'datum-high' -------------------------*
  IF bwbst = 'X'.
    SORT mat_weg     BY bwkey matnr shkzg.                  "144845
    SORT mat_weg_buk BY bwkey matnr shkzg.                  "144845

    LOOP AT g_t_mbew         INTO  g_s_mbew.                "n450764
      CLEAR: mat_weg, mat_weg_buk.                          "184465
*     table g_s_mbew contains already currency and qty unit "n450764
      MOVE-CORRESPONDING g_s_mbew      TO bestand.          "n450764

      IF curm = '1'.
        READ TABLE mat_weg WITH KEY bwkey = g_s_mbew-bwkey  "n450764
                                    matnr = g_s_mbew-matnr  "n450764
                                    shkzg = 'S' BINARY SEARCH.
        bestand-endmenge = g_s_mbew-lbkum - mat_weg-menge.  "n450764
        bestand-endwert  = g_s_mbew-salk3 - mat_weg-dmbtr.  "n450764
      ELSEIF curm = '3'.
        READ TABLE mat_weg_buk                              "n450764
                   WITH KEY bwkey = g_s_mbew-bwkey          "n450764
                            matnr = g_s_mbew-matnr          "n450764
                                        shkzg = 'S' BINARY SEARCH.
        bestand-endmenge = g_s_mbew-lbkum - mat_weg_buk-menge. "n450764
        bestand-endwert  = g_s_mbew-salk3 - mat_weg_buk-dmbtr. "n450764
      ENDIF.                                                "184465
      CLEAR: mat_weg, mat_weg_buk.                          "184465
      IF curm = '1'.
        READ TABLE mat_weg WITH KEY bwkey = g_s_mbew-bwkey  "n450764
                                    matnr = g_s_mbew-matnr  "n450764
                                    shkzg = 'H' BINARY SEARCH.
        bestand-endmenge = bestand-endmenge + mat_weg-menge.
        bestand-endwert  = bestand-endwert  + mat_weg-dmbtr. "184465
      ELSEIF curm = '3'.
        READ TABLE mat_weg_buk
                   WITH KEY bwkey = g_s_mbew-bwkey          "n450764
                            matnr = g_s_mbew-matnr          "n450764
                                        shkzg = 'H' BINARY SEARCH.
        bestand-endmenge = bestand-endmenge + mat_weg_buk-menge.
        bestand-endwert  = bestand-endwert  + mat_weg_buk-dmbtr. "184465
      ENDIF.
      COLLECT bestand.
    ENDLOOP.

    FREE                     g_s_mbew.                      "n450764

  ELSEIF lgbst = 'X'.
*-------------------- ... auf Materialebene ---------------------------*
    IF xchar = ' '.
      SORT imara BY matnr.                                  "2459529
      SORT weg_mat BY werks lgort matnr shkzg.              "2459529
      LOOP AT imard.
        CLEAR weg_mat-menge.
        LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_01\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).

        CLEAR weg_mat-/cwm/menge.

        MOVE-CORRESPONDING imard TO bestand.
* In 'bestand' wird über die Lagerorte summiert.
        READ TABLE weg_mat WITH KEY werks = imard-werks
                                    lgort = imard-lgort    " P30K140665
                                    matnr = imard-matnr
                                    shkzg = 'S'
                                    BINARY SEARCH.          "2459529
        bestand-endmenge = imard-labst + imard-insme + imard-speme
                         + imard-einme +               imard-retme
                         - weg_mat-menge.
        bestand-/cwm/endmenge = imard-/cwm/labst
                              + imard-/cwm/insme
                              + imard-/cwm/speme
                              + imard-/cwm/einme
                              + imard-/cwm/retme
                              - weg_mat-/cwm/menge.
        CLEAR weg_mat-/cwm/menge.
        IF imard-matnr NE imara-matnr.                      "1987245
          CLEAR bestand-/cwm/meins.                         "1987245
        ENDIF.                                              "1987245
*        ENHANCEMENT-POINT ehp605_bestaende_berechnen_01 SPOTS es_rm07mlbd .
        CLEAR weg_mat-menge.
        READ TABLE weg_mat WITH KEY werks = imard-werks
                                    lgort = imard-lgort    " P30K140665
                                    matnr = imard-matnr
                                    shkzg = 'H'
                                    BINARY SEARCH.          "2459529
        bestand-endmenge = bestand-endmenge + weg_mat-menge.
        READ TABLE imara WITH KEY matnr  = bestand-matnr
                                           BINARY SEARCH.   "2459529
        MOVE imara-meins TO bestand-meins.
        IF   /cwm/cl_switch_check=>client( ) = /cwm/cl_switch_check=>true.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_02\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge
                                + weg_mat-/cwm/menge.
          IF bestand-/cwm/meins IS INITIAL                  "1987245
            AND NOT imara-/cwm/xcwmat IS INITIAL.           "1987245
*         get parallel unit                                     "1736526
            DATA l_/cwm/meins TYPE /cwm/meins.              "1736526
            CALL FUNCTION '/CWM/MDMM_2TQ_BASE_UNIT_GET'             "1736526
              EXPORTING                                               "1736526
                i_material              = imara-matnr           "1736526
              IMPORTING                                               "1736526
                e_base_unit_of_2tq      = l_/cwm/meins          "1736526
              EXCEPTIONS                                              "1736526
                material_not_found      = 1                     "1736526
                conversion_not_found    = 2                     "1736526
                base_unit_2tq_not_found = 3                     "1736526
                OTHERS                  = 4.                    "1736526
            IF sy-subrc = 0 AND NOT l_/cwm/meins IS INITIAL. "1736526
              MOVE l_/cwm/meins TO bestand-/cwm/meins.      "1736526
            ELSEIF bestand-/cwm/meins IS INITIAL.           "1736526
              MOVE imara-/cwm/valum TO bestand-/cwm/meins.  "1736526
            ENDIF.                                          "1736526
          ENDIF.                                            "1987245
        ENDIF.
*        ENHANCEMENT-POINT ehp605_bestaende_berechnen_02 SPOTS es_rm07mlbd .
        COLLECT bestand.
      ENDLOOP.
*-------------------- ... auf Chargenebene ----------------------------*
    ELSEIF xchar = 'X'.
      LOOP AT imchb.
        CLEAR weg_char-menge.
        LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_03\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
        CLEAR weg_char-/cwm/menge.
        MOVE-CORRESPONDING imchb TO bestand.
        READ TABLE weg_char WITH KEY werks = imchb-werks
                                     lgort = imchb-lgort   " P30K140665
                                     matnr = imchb-matnr
                                     charg = imchb-charg
                                     shkzg = 'S'.
        bestand-endmenge = imchb-clabs + imchb-cinsm + imchb-cspem
                         + imchb-ceinm +               imchb-cretm
                         - weg_char-menge.
        bestand-/cwm/endmenge = imchb-/cwm/clabs
                              + imchb-/cwm/cinsm
                              + imchb-/cwm/cspem
                              + imchb-/cwm/ceinm
                              + imchb-/cwm/cretm
                              - weg_char-/cwm/menge.
        CLEAR weg_char-/cwm/menge.
        IF imard-matnr NE imara-matnr.                      "2649478
          CLEAR bestand-/cwm/meins.                         "2649478
        ENDIF.                                              "2649478
*        ENHANCEMENT-POINT ehp605_bestaende_berechnen_03 SPOTS es_rm07mlbd .
        CLEAR weg_char-menge.
        READ TABLE weg_char WITH KEY werks = imchb-werks
                                     lgort = imchb-lgort   " P30K140665
                                     matnr = imchb-matnr
                                     charg = imchb-charg
                                     shkzg = 'H'.
        bestand-endmenge = bestand-endmenge + weg_char-menge.
        READ TABLE imara WITH KEY matnr  = bestand-matnr.
        MOVE imara-meins TO bestand-meins.
        LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_04\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
        bestand-/cwm/endmenge = bestand-/cwm/endmenge
                              + weg_char-/cwm/menge.
        IF bestand-/cwm/meins IS INITIAL                    "2649478
          AND NOT imara-/cwm/xcwmat IS INITIAL.             "2649478
*         get parallel unit                                     "2649478
          CALL FUNCTION '/CWM/MDMM_2TQ_BASE_UNIT_GET'           "2649478
            EXPORTING                                             "2649478
              i_material              = imara-matnr         "2649478
            IMPORTING                                             "2649478
              e_base_unit_of_2tq      = l_/cwm/meins        "2649478
            EXCEPTIONS                                            "2649478
              material_not_found      = 1                   "2649478
              conversion_not_found    = 2                   "2649478
              base_unit_2tq_not_found = 3                   "2649478
              OTHERS                  = 4.                  "2649478
          IF sy-subrc = 0 AND NOT l_/cwm/meins IS INITIAL.  "2649478
            MOVE l_/cwm/meins TO bestand-/cwm/meins.        "2649478
          ELSEIF bestand-/cwm/meins IS INITIAL.             "2649478
            MOVE imara-/cwm/valum TO bestand-/cwm/meins.    "2649478
          ENDIF.                                            "2649478
        ENDIF.                                              "2649478
*        ENHANCEMENT-POINT ehp605_bestaende_berechnen_04 SPOTS es_rm07mlbd .
        COLLECT bestand.
      ENDLOOP.
    ENDIF.
*------------------------ Sonderbestände ------------------------------*
  ELSEIF sbbst = 'X'.
    IF sobkz = 'O'.
      IF xchar = ' '.
        LOOP AT imslbx.
          CLEAR weg_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_05\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR weg_mat-/cwm/menge.
          MOVE-CORRESPONDING imslbx TO bestand.
          READ TABLE weg_mat WITH KEY werks = imslbx-werks
                                      matnr = imslbx-matnr
                                      shkzg = 'S'.
          bestand-endmenge = imslbx-lblab + imslbx-lbins + imslbx-lbein
                             - weg_mat-menge.               "2691442
          bestand-/cwm/endmenge = imslbx-/cwm/lblab
                                + imslbx-/cwm/lbins
                                + imslbx-/cwm/lbein
                                + imslbx-/cwm/lbuml         "1421484
                                - weg_mat-/cwm/menge.
          CLEAR weg_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_05 SPOTS es_rm07mlbd .
          CLEAR weg_mat-menge.
          READ TABLE weg_mat WITH KEY werks = imslbx-werks
                                      matnr = imslbx-matnr
                                      shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_mat-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_06\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge
                                + weg_mat-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_06 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT imslb.
          CLEAR weg_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_07\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR weg_char-/cwm/menge.
          MOVE-CORRESPONDING imslb TO bestand.
          READ TABLE weg_char WITH KEY werks = imslb-werks
                                       matnr = imslb-matnr
                                       charg = imslb-charg
                                       shkzg = 'S'.
          bestand-endmenge = imslb-lblab + imslb-lbins + imslb-lbein
                             - weg_char-menge.              "2691442
          bestand-/cwm/endmenge = imslb-/cwm/lblab
                                + imslb-/cwm/lbins
                                + imslb-/cwm/lbein
                                + imslb-/cwm/lbuml          "1421484
                                - weg_char-/cwm/menge.
          CLEAR weg_char-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_07 SPOTS es_rm07mlbd .
          CLEAR weg_char-menge.
          READ TABLE weg_char WITH KEY werks = imslb-werks
                                       matnr = imslb-matnr
                                       charg = imslb-charg
                                       shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_char-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_08\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge
                                  + weg_char-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_08 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ENDIF.
    ELSEIF sobkz = 'V' OR sobkz = 'W'.
      IF xchar = ' '.
        LOOP AT imskux.
          CLEAR weg_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_09\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR weg_mat-/cwm/menge.
          MOVE-CORRESPONDING imskux TO bestand.
          READ TABLE weg_mat WITH KEY werks = imskux-werks
                                      matnr = imskux-matnr
                                      shkzg = 'S'.
          bestand-endmenge = imskux-kulab + imskux-kuins + imskux-kuein
                           - weg_mat-menge.
          bestand-/cwm/endmenge = imskux-/cwm/kulab
                                + imskux-/cwm/kuins
                                + imskux-/cwm/kuein
                                - weg_mat-/cwm/menge.
          CLEAR weg_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_09 SPOTS es_rm07mlbd .
          CLEAR weg_mat-menge.
          READ TABLE weg_mat WITH KEY werks = imskux-werks
                                      matnr = imskux-matnr
                                      shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_mat-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_10\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge
                                + weg_mat-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_10 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT imsku.
          CLEAR weg_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_11\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR weg_char-/cwm/menge.
          MOVE-CORRESPONDING imsku TO bestand.
          READ TABLE weg_char WITH KEY werks = imsku-werks
                                       matnr = imsku-matnr
                                       charg = imsku-charg
                                       shkzg = 'S'.
          bestand-endmenge = imsku-kulab + imsku-kuins + imsku-kuein
                           - weg_char-menge.
          bestand-/cwm/endmenge = imsku-/cwm/kulab
                                + imsku-/cwm/kuins
                                + imsku-/cwm/kuein
                                - weg_char-/cwm/menge.
          CLEAR weg_char-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_11 SPOTS es_rm07mlbd .
          CLEAR weg_char-menge.
          READ TABLE weg_char WITH KEY werks = imsku-werks
                                       matnr = imsku-matnr
                                       charg = imsku-charg
                                       shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_char-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_12\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge
                                + weg_char-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_12 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ENDIF.
    ELSEIF sobkz = 'K' OR sobkz = 'M'.
      IF xchar = ' '.
        LOOP AT imkolx.
          CLEAR weg_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_13\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR weg_mat-/cwm/menge.
          MOVE-CORRESPONDING imkolx TO bestand.
          READ TABLE weg_mat WITH KEY werks = imkolx-werks
                                      matnr = imkolx-matnr
                                      lgort = imkolx-lgort
                                      shkzg = 'S'.
          bestand-endmenge = imkolx-slabs + imkolx-sinsm + imkolx-seinm
                           + imkolx-sspem - weg_mat-menge.
          bestand-/cwm/endmenge = imkolx-/cwm/slabs
                                + imkolx-/cwm/sinsm
                                + imkolx-/cwm/seinm
                                + imkolx-/cwm/sspem
                                - weg_mat-/cwm/menge.
          CLEAR weg_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_13 SPOTS es_rm07mlbd .
          CLEAR weg_mat-menge.
          READ TABLE weg_mat WITH KEY werks = imkolx-werks
                                      matnr = imkolx-matnr
                                      lgort = imkolx-lgort
                                      shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_mat-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_14\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge
                                + weg_mat-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_14 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT imkol.
          CLEAR weg_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_15\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR weg_char-/cwm/menge.
          MOVE-CORRESPONDING imkol TO bestand.
          READ TABLE weg_char WITH KEY werks = imkol-werks
                                       matnr = imkol-matnr
                                       lgort = imkol-lgort
                                       charg = imkol-charg
                                       shkzg = 'S'.
          bestand-endmenge = imkol-slabs + imkol-sinsm + imkol-seinm
                           + imkol-sspem - weg_char-menge.
          bestand-/cwm/endmenge = imkol-/cwm/slabs
                                + imkol-/cwm/sinsm
                                + imkol-/cwm/seinm
                                + imkol-/cwm/sspem
                                - weg_char-/cwm/menge.
          CLEAR weg_char-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_15 SPOTS es_rm07mlbd .
          CLEAR weg_char-menge.
          READ TABLE weg_char WITH KEY werks = imkol-werks
                                       matnr = imkol-matnr
                                       lgort = imkol-lgort
                                       charg = imkol-charg
                                       shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_char-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_16\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge + weg_char-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_16 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ENDIF.
    ELSEIF sobkz = 'Q'.
      IF xchar = ' '.
        LOOP AT imsprx.
          CLEAR weg_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_17\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR weg_mat-/cwm/menge.
          MOVE-CORRESPONDING imsprx TO bestand.
          READ TABLE weg_mat WITH KEY werks = imsprx-werks
                                      matnr = imsprx-matnr
                                      lgort = imsprx-lgort
                                      shkzg = 'S'.
          bestand-endmenge = imsprx-prlab + imsprx-prins + imsprx-prspe
                           + imsprx-prein - weg_mat-menge.
          bestand-/cwm/endmenge = imsprx-/cwm/prlab
                                + imsprx-/cwm/prins
                                + imsprx-/cwm/prspe
                                + imsprx-/cwm/prein
                                - weg_mat-/cwm/menge.
          CLEAR weg_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_17 SPOTS es_rm07mlbd .
          CLEAR weg_mat-menge.
          READ TABLE weg_mat WITH KEY werks = imsprx-werks
                                      matnr = imsprx-matnr
                                      lgort = imsprx-lgort
                                      shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_mat-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_18\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge
                                + weg_mat-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_18 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT imspr.
          CLEAR weg_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_19\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR weg_char-/cwm/menge.
          MOVE-CORRESPONDING imspr TO bestand.
          READ TABLE weg_char WITH KEY werks = imspr-werks
                                       matnr = imspr-matnr
                                       lgort = imspr-lgort
                                       charg = imspr-charg
                                       shkzg = 'S'.
          bestand-endmenge = imspr-prlab + imspr-prins + imspr-prspe
                           + imspr-prein - weg_char-menge.
          bestand-/cwm/endmenge = imspr-/cwm/prlab
                                + imspr-/cwm/prins
                                + imspr-/cwm/prspe
                                + imspr-/cwm/prein
                                - weg_char-/cwm/menge.
          CLEAR weg_char-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_19 SPOTS es_rm07mlbd .
          CLEAR weg_char-menge.
          READ TABLE weg_char WITH KEY werks = imspr-werks
                                       matnr = imspr-matnr
                                       lgort = imspr-lgort
                                       charg = imspr-charg
                                       shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_char-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_20\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge + weg_char-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_20 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ENDIF.
    ELSEIF sobkz = 'E'.
      IF xchar = ' '.
        LOOP AT imskax.
          CLEAR weg_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_21\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).

          CLEAR weg_mat-/cwm/menge.

          MOVE-CORRESPONDING imskax TO bestand.
          READ TABLE weg_mat WITH KEY werks = imskax-werks
                                      matnr = imskax-matnr
                                      lgort = imskax-lgort
                                      shkzg = 'S'.
          bestand-endmenge = imskax-kalab + imskax-kains + imskax-kaspe
                           + imskax-kaein - weg_mat-menge.
          bestand-/cwm/endmenge = imskax-/cwm/kalab
                                + imskax-/cwm/kains
                                + imskax-/cwm/kaspe
                                + imskax-/cwm/kaein
                                - weg_mat-/cwm/menge.
          CLEAR weg_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_21 SPOTS es_rm07mlbd .
          CLEAR weg_mat-menge.
          READ TABLE weg_mat WITH KEY werks = imskax-werks
                                      matnr = imskax-matnr
                                      lgort = imskax-lgort
                                      shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_mat-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_22\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge
                                + weg_mat-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_22 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT imska.
          CLEAR weg_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_23\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR weg_char-/cwm/menge.
          MOVE-CORRESPONDING imska TO bestand.
          READ TABLE weg_char WITH KEY werks = imska-werks
                                       matnr = imska-matnr
                                       lgort = imska-lgort
                                       charg = imska-charg
                                       shkzg = 'S'.
          bestand-endmenge = imska-kalab + imska-kains + imska-kaspe
                           + imska-kaein - weg_char-menge.
          bestand-/cwm/endmenge = imska-/cwm/kalab
                                + imska-/cwm/kains
                                + imska-/cwm/kaspe
                                + imska-/cwm/kaein
                                - weg_char-/cwm/menge.
          CLEAR weg_char-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_23 SPOTS es_rm07mlbd .
          CLEAR weg_char-menge.
          READ TABLE weg_char WITH KEY werks = imska-werks
                                       matnr = imska-matnr
                                       lgort = imska-lgort
                                       charg = imska-charg
                                       shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_char-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_24\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge + weg_char-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_24 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ENDIF.
    ELSEIF sobkz = 'T'.                                      "SIT
      IF xchar = ' '.
        LOOP AT imskax.
          CLEAR weg_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_49\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR weg_mat-/cwm/menge.
          MOVE-CORRESPONDING imskax TO bestand.
          READ TABLE weg_mat WITH KEY werks = imskax-werks
                                      matnr = imskax-matnr
                                      lgort = imskax-lgort
                                      shkzg = 'S'.
          bestand-endmenge = imskax-kalab + imskax-kains + imskax-kaspe
                           + imskax-kaein - weg_mat-menge.
          bestand-/cwm/endmenge = imskax-/cwm/kalab
                                + imskax-/cwm/kains
                                + imskax-/cwm/kaspe
                                + imskax-/cwm/kaein
                                - weg_mat-/cwm/menge.
          CLEAR weg_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_49 SPOTS es_rm07mlbd .
          CLEAR weg_mat-menge.
          READ TABLE weg_mat WITH KEY werks = imskax-werks
                                      matnr = imskax-matnr
                                      lgort = imskax-lgort
                                      shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_mat-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_50\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge
                                + weg_mat-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_50 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT imska.
          CLEAR weg_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_51\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).

          CLEAR weg_char-/cwm/menge.

          MOVE-CORRESPONDING imska TO bestand.
          READ TABLE weg_char WITH KEY werks = imska-werks
                                       matnr = imska-matnr
                                       lgort = imska-lgort
                                       charg = imska-charg
                                       shkzg = 'S'.
          bestand-endmenge = imska-kalab + imska-kains + imska-kaspe
                           + imska-kaein - weg_char-menge.
          bestand-/cwm/endmenge = imska-/cwm/kalab
                                + imska-/cwm/kains
                                + imska-/cwm/kaspe
                                + imska-/cwm/kaein
                                - weg_char-/cwm/menge.
          CLEAR weg_char-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_51 SPOTS es_rm07mlbd .
          CLEAR weg_char-menge.
          READ TABLE weg_char WITH KEY werks = imska-werks
                                       matnr = imska-matnr
                                       lgort = imska-lgort
                                       charg = imska-charg
                                       shkzg = 'H'.
          bestand-endmenge = bestand-endmenge + weg_char-menge.
          READ TABLE imara WITH KEY matnr  = bestand-matnr.
          MOVE imara-meins TO bestand-meins.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_52\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          bestand-/cwm/endmenge = bestand-/cwm/endmenge + weg_char-/cwm/menge.
          MOVE imara-/cwm/valum TO bestand-/cwm/meins.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_52 SPOTS es_rm07mlbd .
          COLLECT bestand.
        ENDLOOP.
      ENDIF.
    ELSE.

      "{ Begin ENHO DIPCS_RM07MLBD_FORM_01 IS-AD-SSP AD_SUB }
* DI A&D SSP
** DI IS-ADEC-SSP Customer stock
      IF sobkz = 'B'.
        IF xchar = ' '.
          LOOP AT imcsdx.
            CLEAR weg_mat-menge.
            MOVE-CORRESPONDING imcsdx TO bestand.
            READ TABLE weg_mat WITH KEY werks = imcsdx-werks
                                        matnr = imcsdx-matnr
                                        lgort = imcsdx-lgort
                                        shkzg = 'S'.
            bestand-endmenge = imcsdx-sdlab + imcsdx-sdins + imcsdx-sdspe
                             + imcsdx-sdein - weg_mat-menge.
            CLEAR weg_mat-menge.
            READ TABLE weg_mat WITH KEY werks = imcsdx-werks
                                        matnr = imcsdx-matnr
                                        lgort = imcsdx-lgort
                                        shkzg = 'H'.
            bestand-endmenge = bestand-endmenge + weg_mat-menge.
            READ TABLE imara WITH KEY matnr  = bestand-matnr.
            MOVE imara-meins TO bestand-meins.
            COLLECT bestand.
          ENDLOOP.
        ELSEIF xchar = 'X'.
          LOOP AT imcsd.
            CLEAR weg_char-menge.
            MOVE-CORRESPONDING imcsd TO bestand.
            READ TABLE weg_char WITH KEY werks = imcsd-werks
                                         matnr = imcsd-matnr
                                         lgort = imcsd-lgort
                                         charg = imcsd-charg
                                         shkzg = 'S'.
            bestand-endmenge = imcsd-sdlab + imcsd-sdins + imcsd-sdspe
                             + imcsd-sdein - weg_char-menge.
            CLEAR weg_char-menge.
            READ TABLE weg_char WITH KEY werks = imcsd-werks
                                         matnr = imcsd-matnr
                                         lgort = imcsd-lgort
                                         charg = imcsd-charg
                                         shkzg = 'H'.
            bestand-endmenge = bestand-endmenge + weg_char-menge.
            READ TABLE imara WITH KEY matnr  = bestand-matnr.
            MOVE imara-meins TO bestand-meins.
            COLLECT bestand.
          ENDLOOP.
        ENDIF.
      ENDIF.
* DI A&D Subcontracting                                   "v_n_GA1551829
** A&D IS-ADEC-SUB Customer stock with Vendor
      IF sobkz = cl_adsub_constants=>c.
        IF xchar = ' '.
          LOOP AT imscdx.
            CLEAR weg_mat-menge.
            MOVE-CORRESPONDING imscdx TO bestand.
            READ TABLE weg_mat WITH KEY werks = imscdx-werks
                                        matnr = imscdx-matnr
                                        shkzg = 'S'.
            bestand-endmenge = imscdx-cdlab + imscdx-cdins
                             + imscdx-cdein - weg_mat-menge.
            CLEAR weg_mat-menge.
            READ TABLE weg_mat WITH KEY werks = imscdx-werks
                                        matnr = imscdx-matnr
                                        shkzg = 'H'.
            bestand-endmenge = bestand-endmenge + weg_mat-menge.
            READ TABLE imara WITH KEY matnr  = bestand-matnr.
            MOVE imara-meins TO bestand-meins.
            COLLECT bestand.
          ENDLOOP.
        ELSEIF xchar = 'X'.
          LOOP AT imscd.
            CLEAR weg_char-menge.
            MOVE-CORRESPONDING imscd TO bestand.
            READ TABLE weg_char WITH KEY werks = imscd-werks
                                         matnr = imscd-matnr
                                         charg = imscd-charg
                                         shkzg = 'S'.
            bestand-endmenge = imscd-cdlab + imscd-cdins
                             + imscd-cdein - weg_char-menge.
            CLEAR weg_char-menge.
            READ TABLE weg_char WITH KEY werks = imscd-werks
                                         matnr = imscd-matnr
                                         charg = imscd-charg
                                         shkzg = 'H'.
            bestand-endmenge = bestand-endmenge + weg_char-menge.
            READ TABLE imara WITH KEY matnr  = bestand-matnr.
            MOVE imara-meins TO bestand-meins.
            COLLECT bestand.
          ENDLOOP.
        ENDIF.
      ENDIF.
** A&D IS-ADEC-SUB Sales Order stock with Vendor
      IF sobkz = cl_adsub_constants=>f.
        IF xchar = ' '.
          LOOP AT imsfdx.
            CLEAR weg_mat-menge.
            MOVE-CORRESPONDING imsfdx TO bestand.
            READ TABLE weg_mat WITH KEY werks = imsfdx-werks
                                        matnr = imsfdx-matnr
                                        shkzg = 'S'.
            bestand-endmenge = imsfdx-fdlab + imsfdx-fdins
                             + imsfdx-fdein - weg_mat-menge.
            CLEAR weg_mat-menge.
            READ TABLE weg_mat WITH KEY werks = imsfdx-werks
                                        matnr = imsfdx-matnr
                                        shkzg = 'H'.
            bestand-endmenge = bestand-endmenge + weg_mat-menge.
            READ TABLE imara WITH KEY matnr  = bestand-matnr.
            MOVE imara-meins TO bestand-meins.
            COLLECT bestand.
          ENDLOOP.
        ELSEIF xchar = 'X'.
          LOOP AT imsfd.
            CLEAR weg_char-menge.
            MOVE-CORRESPONDING imsfd TO bestand.
            READ TABLE weg_char WITH KEY werks = imsfd-werks
                                         matnr = imsfd-matnr
                                         charg = imsfd-charg
                                         shkzg = 'S'.
            bestand-endmenge = imsfd-fdlab + imsfd-fdins
                             + imsfd-fdein - weg_char-menge.
            CLEAR weg_char-menge.
            READ TABLE weg_char WITH KEY werks = imsfd-werks
                                         matnr = imsfd-matnr
                                         charg = imsfd-charg
                                         shkzg = 'H'.
            bestand-endmenge = bestand-endmenge + weg_char-menge.
            READ TABLE imara WITH KEY matnr  = bestand-matnr.
            MOVE imara-meins TO bestand-meins.
            COLLECT bestand.
          ENDLOOP.
        ENDIF.
      ENDIF.
** A&D IS-ADEC-SUB Project stock with Vendor
      IF sobkz = cl_adsub_constants=>r.
        IF xchar = ' '.
          LOOP AT imsrdx.
            CLEAR weg_mat-menge.
            MOVE-CORRESPONDING imsrdx TO bestand.
            READ TABLE weg_mat WITH KEY werks = imsrdx-werks
                                        matnr = imsrdx-matnr
                                        shkzg = 'S'.
            bestand-endmenge = imsrdx-rdlab + imsrdx-rdins
                             + imsrdx-rdein - weg_mat-menge.
            CLEAR weg_mat-menge.
            READ TABLE weg_mat WITH KEY werks = imsrdx-werks
                                        matnr = imsrdx-matnr
                                        shkzg = 'H'.
            bestand-endmenge = bestand-endmenge + weg_mat-menge.
            READ TABLE imara WITH KEY matnr  = bestand-matnr.
            MOVE imara-meins TO bestand-meins.
            COLLECT bestand.
          ENDLOOP.
        ELSEIF xchar = 'X'.
          LOOP AT imsrd.
            CLEAR weg_char-menge.
            MOVE-CORRESPONDING imsrd TO bestand.
            READ TABLE weg_char WITH KEY werks = imsrd-werks
                                         matnr = imsrd-matnr
                                         charg = imsrd-charg
                                         shkzg = 'S'.
            bestand-endmenge = imsrd-rdlab + imsrd-rdins
                             + imsrd-rdein - weg_char-menge.
            CLEAR weg_char-menge.
            READ TABLE weg_char WITH KEY werks = imsrd-werks
                                         matnr = imsrd-matnr
                                         charg = imsrd-charg
                                         shkzg = 'H'.
            bestand-endmenge = bestand-endmenge + weg_char-menge.
            READ TABLE imara WITH KEY matnr  = bestand-matnr.
            MOVE imara-meins TO bestand-meins.
            COLLECT bestand.
          ENDLOOP.
        ENDIF.
      ENDIF.
** A&D IS-ADEC-SUB Vendor Consignment / RTP stock with Vendor
      IF sobkz = cl_adsub_constants=>i OR sobkz = cl_adsub_constants=>j.
        IF xchar = ' '.
          LOOP AT imsidx.
            CLEAR weg_mat-menge.
            MOVE-CORRESPONDING imsidx TO bestand.
            READ TABLE weg_mat WITH KEY werks = imsidx-werks
                                        matnr = imsidx-matnr
                                        shkzg = 'S'.
            bestand-endmenge = imsidx-idlab + imsidx-idins
                             + imsidx-idein - weg_mat-menge.
            CLEAR weg_mat-menge.
            READ TABLE weg_mat WITH KEY werks = imsidx-werks
                                        matnr = imsidx-matnr
                                        shkzg = 'H'.
            bestand-endmenge = bestand-endmenge + weg_mat-menge.
            READ TABLE imara WITH KEY matnr  = bestand-matnr.
            MOVE imara-meins TO bestand-meins.
            COLLECT bestand.
          ENDLOOP.
        ELSEIF xchar = 'X'.
          LOOP AT imsid.
            CLEAR weg_char-menge.
            MOVE-CORRESPONDING imsid TO bestand.
            READ TABLE weg_char WITH KEY werks = imsid-werks
                                         matnr = imsid-matnr
                                         charg = imsid-charg
                                         shkzg = 'S'.
            bestand-endmenge = imsid-idlab + imsid-idins
                             + imsid-idein - weg_char-menge.
            CLEAR weg_char-menge.
            READ TABLE weg_char WITH KEY werks = imsid-werks
                                         matnr = imsid-matnr
                                         charg = imsid-charg
                                         shkzg = 'H'.
            bestand-endmenge = bestand-endmenge + weg_char-menge.
            READ TABLE imara WITH KEY matnr  = bestand-matnr.
            MOVE imara-meins TO bestand-meins.
            COLLECT bestand.
          ENDLOOP.
        ENDIF.
      ENDIF.                                                "^_n_GA1551829
      "{ End ENHO DIPCS_RM07MLBD_FORM_01 IS-AD-SSP AD_SUB }

*      ENHANCEMENT-POINT bestaende_berechnen_01 SPOTS es_rm07mlbd.

    ENDIF.
  ENDIF.
*-------------------- Bestände zu 'datum-low' -------------------------*
  IF bwbst = 'X'.
    SORT mat_sum     BY bwkey matnr shkzg.                  "144845
    SORT mat_sum_buk BY bwkey matnr shkzg.                  "144845
    LOOP AT bestand.
      CLEAR: mat_sum, mat_sum_buk.                          "184465
      IF curm = '1'.
        READ TABLE mat_sum WITH KEY bwkey = bestand-bwkey
                                    matnr = bestand-matnr
                                    shkzg = 'S' BINARY SEARCH.
        MOVE mat_sum-menge TO bestand-soll.
        MOVE mat_sum-dmbtr TO bestand-sollwert.             "184465
      ELSEIF curm = '3'.
        READ TABLE mat_sum_buk WITH KEY bwkey = bestand-bwkey
                                        matnr = bestand-matnr
                                        shkzg = 'S' BINARY SEARCH.
        MOVE mat_sum_buk-menge TO bestand-soll.
        MOVE mat_sum_buk-dmbtr TO bestand-sollwert.         "184465
      ENDIF.
      CLEAR: mat_sum, mat_sum_buk.                          "184465
      IF curm = '1'.
        READ TABLE mat_sum WITH KEY bwkey = bestand-bwkey
                                    matnr = bestand-matnr
                                    shkzg = 'H' BINARY SEARCH.
        MOVE mat_sum-menge TO bestand-haben.
        MOVE mat_sum-dmbtr TO bestand-habenwert.            "184465
      ELSEIF curm = '3'.
        READ TABLE mat_sum_buk WITH KEY bwkey = bestand-bwkey
                                        matnr = bestand-matnr
                                        shkzg = 'H' BINARY SEARCH.
        MOVE mat_sum_buk-menge TO bestand-haben.
        MOVE mat_sum_buk-dmbtr TO bestand-habenwert.        "184465
      ENDIF.
      bestand-anfmenge = bestand-endmenge - bestand-soll
                                          + bestand-haben.
      bestand-anfwert = bestand-endwert - bestand-sollwert
                                        + bestand-habenwert.
      MODIFY bestand.
    ENDLOOP.
*-------------------- ... auf Materialebene ---------------------------*
  ELSEIF lgbst = 'X'.
    IF xchar = ' '.
      LOOP AT bestand.
        CLEAR sum_mat-menge.
        LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_25\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
        CLEAR sum_mat-/cwm/menge.
        READ TABLE sum_mat WITH KEY werks = bestand-werks
                                    matnr = bestand-matnr
                                    shkzg = 'S'.
        MOVE sum_mat-menge TO bestand-soll.
        MOVE sum_mat-/cwm/menge TO bestand-/cwm/soll.
        CLEAR sum_mat-/cwm/menge.
*        ENHANCEMENT-POINT ehp605_bestaende_berechnen_25 SPOTS es_rm07mlbd .
        CLEAR sum_mat-menge.
        READ TABLE sum_mat WITH KEY werks = bestand-werks
                                    matnr = bestand-matnr
                                    shkzg = 'H'.
        MOVE sum_mat-menge TO bestand-haben.
        bestand-anfmenge = bestand-endmenge - bestand-soll
                                            + bestand-haben.
        LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_26\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
        MOVE sum_mat-/cwm/menge TO bestand-/cwm/haben.
        bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                              - bestand-/cwm/soll
                              + bestand-/cwm/haben.

*        ENHANCEMENT-POINT ehp605_bestaende_berechnen_26 SPOTS es_rm07mlbd .
        MODIFY bestand.
      ENDLOOP.
*-------------------- ... auf Chargenebene ----------------------------*
    ELSEIF xchar = 'X'.
      LOOP AT bestand.
        CLEAR sum_char-menge.
        LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_27\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
        CLEAR sum_char-/cwm/menge.
        READ TABLE sum_char WITH KEY werks = bestand-werks
                                     matnr = bestand-matnr
                                     charg = bestand-charg
                                     shkzg = 'S'.
        MOVE sum_char-menge TO bestand-soll.
        MOVE sum_char-/cwm/menge TO bestand-/cwm/soll.
        CLEAR sum_char-/cwm/menge.
*        ENHANCEMENT-POINT ehp605_bestaende_berechnen_27 SPOTS es_rm07mlbd .
        CLEAR sum_char-menge.
        READ TABLE sum_char WITH KEY werks = bestand-werks
                                     matnr = bestand-matnr
                                     charg = bestand-charg
                                     shkzg = 'H'.
        MOVE sum_char-menge TO bestand-haben.
        bestand-anfmenge = bestand-endmenge - bestand-soll
                                            + bestand-haben.
        LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_28\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
        MOVE sum_char-/cwm/menge TO bestand-/cwm/haben.
        bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                              - bestand-/cwm/soll
                              + bestand-/cwm/haben.
*        ENHANCEMENT-POINT ehp605_bestaende_berechnen_28 SPOTS es_rm07mlbd .
        MODIFY bestand.
      ENDLOOP.
    ENDIF.
*------------------------ Sonderbestände ------------------------------*
  ELSEIF sbbst = 'X'.
    IF sobkz = 'O'.
      IF xchar = ' '.
        LOOP AT bestand.
          CLEAR sum_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_29\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR sum_mat-/cwm/menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'S'.
          MOVE sum_mat-menge TO bestand-soll.
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_29 SPOTS es_rm07mlbd .
          CLEAR sum_mat-menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'H'.
          MOVE sum_mat-menge TO bestand-haben.
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_30\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                                - bestand-/cwm/soll
                                + bestand-/cwm/haben.

*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_30 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT bestand.
          CLEAR sum_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_31\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR sum_mat-/cwm/menge.
          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'S'.
          MOVE sum_char-menge TO bestand-soll.              "n1031056
          MOVE sum_char-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_char-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_31 SPOTS es_rm07mlbd .
          CLEAR sum_char-menge.
          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'H'.
          MOVE sum_char-menge TO bestand-haben.             "n1031056
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_32\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_char-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                                - bestand-/cwm/soll
                                + bestand-/cwm/haben.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_32 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ENDIF.

    ELSEIF sobkz = 'V' OR sobkz = 'W'.
      IF xchar = ' '.
        LOOP AT bestand.
          CLEAR sum_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_33\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR sum_mat-/cwm/menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'S'.
          MOVE sum_mat-menge TO bestand-soll.
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_33 SPOTS es_rm07mlbd .
          CLEAR sum_mat-menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'H'.
          MOVE sum_mat-menge TO bestand-haben.
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_34\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                                - bestand-/cwm/soll
                                + bestand-/cwm/haben.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_34 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT bestand.
          CLEAR sum_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_35\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR sum_char-/cwm/menge.
          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'S'.
          MOVE sum_char-menge TO bestand-soll.
          MOVE sum_char-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_char-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_35 SPOTS es_rm07mlbd .
          CLEAR sum_char-menge.
          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'H'.
          MOVE sum_char-menge TO bestand-haben.
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_36\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_char-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                                - bestand-/cwm/soll
                                + bestand-/cwm/haben.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_36 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ENDIF.
*   consider special stock M ,too
    ELSEIF sobkz = 'K' OR sobkz = 'M'.
      IF xchar = ' '.
        LOOP AT bestand.
          CLEAR sum_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_37\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR sum_mat-/cwm/menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'S'.
          MOVE sum_mat-menge TO bestand-soll.
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_37 SPOTS es_rm07mlbd .
          CLEAR sum_mat-menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'H'.
          MOVE sum_mat-menge TO bestand-haben.
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_38\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                                - bestand-/cwm/soll
                                + bestand-/cwm/haben.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_38 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT bestand.
          CLEAR sum_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_39\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR sum_char-/cwm/menge.
          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'S'.
          MOVE sum_char-menge TO bestand-soll.
          MOVE sum_char-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_char-/cwm/menge.

*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_39 SPOTS es_rm07mlbd .
          CLEAR sum_char-menge.
          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'H'.
          MOVE sum_char-menge TO bestand-haben.
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_40\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_char-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                               - bestand-/cwm/soll
                               + bestand-/cwm/haben.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_40 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ENDIF.
    ELSEIF sobkz = 'Q'.
      IF xchar = ' '.
        LOOP AT bestand.
          CLEAR sum_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_41\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR sum_mat-/cwm/menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'S'.
          MOVE sum_mat-menge TO bestand-soll.
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_41 SPOTS es_rm07mlbd .
          CLEAR sum_mat-menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'H'.
          MOVE sum_mat-menge TO bestand-haben.
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_42\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                               - bestand-/cwm/soll
                               + bestand-/cwm/haben.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_42 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT bestand.
          CLEAR sum_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_43\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR sum_char-/cwm/menge.
          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'S'.
          MOVE sum_char-menge TO bestand-soll.
          MOVE sum_char-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_char-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_43 SPOTS es_rm07mlbd .
          CLEAR sum_char-menge.
          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'H'.
          MOVE sum_char-menge TO bestand-haben.
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_44\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_char-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                                - bestand-/cwm/soll
                                + bestand-/cwm/haben.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_44 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ENDIF.
    ELSEIF sobkz = 'E'.
      IF xchar = ' '.
        LOOP AT bestand.
          CLEAR sum_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_45\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR sum_mat-/cwm/menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'S'.
          MOVE sum_mat-menge TO bestand-soll.
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_45 SPOTS es_rm07mlbd .
          CLEAR sum_mat-menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'H'.
          MOVE sum_mat-menge TO bestand-haben.
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_46\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                                - bestand-/cwm/soll
                                + bestand-/cwm/haben.

*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_46 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT bestand.
          CLEAR sum_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_47\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).

          CLEAR sum_char-/cwm/menge.

          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'S'.
          MOVE sum_char-menge TO bestand-soll.
          MOVE sum_char-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_char-/cwm/menge.

*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_47 SPOTS es_rm07mlbd .
          CLEAR sum_char-menge.
          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'H'.
          MOVE sum_char-menge TO bestand-haben.
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_48\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_char-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                                - bestand-/cwm/soll
                                + bestand-/cwm/haben.

*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_48 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ENDIF.
    ELSEIF sobkz = 'T'.                                      "SIT
      IF xchar = ' '.
        LOOP AT bestand.
          CLEAR sum_mat-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_53\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR sum_mat-/cwm/menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'S'.
          MOVE sum_mat-menge TO bestand-soll.
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_mat-/cwm/menge.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_53 SPOTS es_rm07mlbd .
          CLEAR sum_mat-menge.
          READ TABLE sum_mat WITH KEY werks = bestand-werks
                                      matnr = bestand-matnr
                                      shkzg = 'H'.
          MOVE sum_mat-menge TO bestand-haben.
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_54\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_mat-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                                - bestand-/cwm/soll
                                + bestand-/cwm/haben.
*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_54 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ELSEIF xchar = 'X'.
        LOOP AT bestand.
          CLEAR sum_char-menge.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_55\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          CLEAR sum_char-/cwm/menge.
          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'S'.
          MOVE sum_char-menge TO bestand-soll.
          MOVE sum_char-/cwm/menge TO bestand-/cwm/soll.
          CLEAR sum_char-/cwm/menge.

*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_55 SPOTS es_rm07mlbd .
          CLEAR sum_char-menge.
          READ TABLE sum_char WITH KEY werks = bestand-werks
                                       matnr = bestand-matnr
                                       charg = bestand-charg
                                       shkzg = 'H'.
          MOVE sum_char-menge TO bestand-haben.
          bestand-anfmenge = bestand-endmenge - bestand-soll
                                              + bestand-haben.
          LOG-POINT ID /cwm/enh SUBKEY to_upper( sy-tcode && '\/CWM/APPL_MM_RM07MLBD\EHP605_BESTAENDE_BERECHNEN_56\' && sy-cprog ) FIELDS /cwm/cl_enh_layer=>get_field( ).
          MOVE sum_char-/cwm/menge TO bestand-/cwm/haben.
          bestand-/cwm/anfmenge = bestand-/cwm/endmenge
                                - bestand-/cwm/soll
                                + bestand-/cwm/haben.

*          ENHANCEMENT-POINT ehp605_bestaende_berechnen_56 SPOTS es_rm07mlbd .
          MODIFY bestand.
        ENDLOOP.
      ENDIF.
    ELSE.

      "{ Begin ENHO DIPCS_RM07MLBD_FORM_01 IS-AD-SSP AD_SUB }
* DI A&D SSP
** DI IS-ADEC-SSP Customer Stock
      IF sobkz = 'B' OR
         sobkz = cl_adsub_constants=>c OR                   "v_n_GA1551829
         sobkz = cl_adsub_constants=>f OR
         sobkz = cl_adsub_constants=>i OR
         sobkz = cl_adsub_constants=>j OR
         sobkz = cl_adsub_constants=>r.                     "^_n_GA1551829
        IF xchar = ' '.
          LOOP AT bestand.
            CLEAR sum_mat-menge.
            READ TABLE sum_mat WITH KEY werks = bestand-werks
                                        matnr = bestand-matnr
                                        shkzg = 'S'.
            MOVE sum_mat-menge TO bestand-soll.
            CLEAR sum_mat-menge.
            READ TABLE sum_mat WITH KEY werks = bestand-werks
                                        matnr = bestand-matnr
                                        shkzg = 'H'.
            MOVE sum_mat-menge TO bestand-haben.
            bestand-anfmenge = bestand-endmenge - bestand-soll
                                                + bestand-haben.
            MODIFY bestand.
          ENDLOOP.
        ELSEIF xchar = 'X'.
          LOOP AT bestand.
            CLEAR sum_char-menge.
            READ TABLE sum_char WITH KEY werks = bestand-werks
                                         matnr = bestand-matnr
                                         charg = bestand-charg
                                         shkzg = 'S'.
            MOVE sum_char-menge TO bestand-soll.
            CLEAR sum_char-menge.
            READ TABLE sum_char WITH KEY werks = bestand-werks
                                         matnr = bestand-matnr
                                         charg = bestand-charg
                                         shkzg = 'H'.
            MOVE sum_char-menge TO bestand-haben.
            bestand-anfmenge = bestand-endmenge - bestand-soll
                                                + bestand-haben.
            MODIFY bestand.
          ENDLOOP.
        ENDIF.
      ENDIF.
      "{ End ENHO DIPCS_RM07MLBD_FORM_01 IS-AD-SSP AD_SUB }

*      ENHANCEMENT-POINT bestaende_berechnen_02 SPOTS es_rm07mlbd.

    ENDIF.
  ENDIF.

ENDFORM.                               " BESTAENDE_BERECHNEN

*&---------------------------------------------------------------------*
*&      Form  BESTAENDE_AUSGEBEN
*&---------------------------------------------------------------------*
*       Ausgabe der Bestände zu 'datum-low' und 'datum-high'           *
*       und der Zu- und Abgänge in diesem Zeitintervall                *
*       für den Lagerort-/Chargen- und den Sonderbestand               *
*       bzw. für den bewerteten Bestand                                *
*----------------------------------------------------------------------*

FORM bestaende_ausgeben.
*  ENHANCEMENT-POINT bestaende_ausgeben_01 SPOTS es_rm07mlbd.

*   show the wole list with the ALV
  READ TABLE bestand INDEX 1.
  MOVE-CORRESPONDING bestand TO bestand1.
  APPEND bestand1.

  CLEAR g_t_belege. REFRESH g_t_belege.

  IF bwbst IS INITIAL.
*     fill the data table for the ALV with the              "n921165
*     corresponding MM documents for mode = stock           "n921165
    PERFORM  fill_data_table                                "n921165
                           TABLES    g_t_belege1            "n921165
                           USING     bestand-matnr          "n921165
                                     bestand-werks          "n921165
                                     bestand-charg.         "n921165

  ELSEIF NOT bwbst IS INITIAL.
*     fill the data table for the ALV with the              "n921165
*     corresponding MM documents for mode = valuated stock  "n921165
    PERFORM  process_plants_of_bwkey                        "n921165
                           TABLES    g_t_belege1            "n921165
                           USING     bestand-matnr          "n921165
                                     bestand-bwkey.         "n921165
  ENDIF.

  SORT g_t_belege1         BY budat mblnr zeile.

  events-name = 'TOP_OF_PAGE'.
  events-form = 'UEBERSCHRIFT1'.
  APPEND events.

*   set this event depending on the entries in working      "n599218
*   table BESTAND                                           "n599218
  DESCRIBE TABLE bestand   LINES  g_f_cnt_lines.            "n599218
                                                            "n599218
  IF  g_f_cnt_lines = 1.                                    "n599218
    events-form  =  'PRINT_END_OF_LIST'.                    "n599218
  ELSE.                                                     "n599218
    events-form = 'LISTE'.                                  "n599218
  ENDIF.                                                    "n599218

  events-name = 'END_OF_LIST'.
  APPEND events.

  PERFORM listausgabe1.

ENDFORM.                               " BESTAENDE_AUSGEBEN

*&---------------------------------------------------------------------*
*       FORM ANFORDERUNGSBILD                                          *
*----------------------------------------------------------------------*
*       Rücksprung zum Anforderungsbild                                *
*----------------------------------------------------------------------*
FORM anforderungsbild.

  IF NOT sy-calld IS INITIAL.
    LEAVE.
  ELSE.
    LEAVE TO TRANSACTION sy-tcode.
  ENDIF.

ENDFORM.                               " ANFORDERUNGSBILD

*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
*       F4-Hilfe für Reportvariante                                    *
*----------------------------------------------------------------------*
FORM f4_for_variant.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = variante
      i_save     = variant_save
*     it_default_fieldcat =
    IMPORTING
      e_exit     = variant_exit
      es_variant = def_variante
    EXCEPTIONS
      not_found  = 2.
  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    IF variant_exit = space.
      p_vari = def_variante-variant.
    ENDIF.
  ENDIF.

ENDFORM.                               " F4_FOR_VARIANT

*&---------------------------------------------------------------------*
*&      Form  LISTAUSGABE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM listausgabe.

  IF  g_cust_color = 'X'.              "colorize numeric fields ?
    layout-coltab_fieldname = 'FARBE_PRO_FELD'.
  ELSE.
    layout-info_fieldname   = 'FARBE_PRO_ZEILE'.
  ENDIF.

  layout-f2code = '9PBP'.
  IF NOT bwbst IS INITIAL.
    layout-min_linesize = '92'.
  ENDIF.

* allow the functions for the interactions
* 'Specify drill-down' etc.. depending on the content of    "n890109
* "g_cust_sum_levels"                                       "n890109
                                                            "n890109
  IF  g_cust_sum_levels = 'X'.                              "n890109
*   the following function modules make sure that the       "n890109
*   interactions will be transferred to all "append lists"  "n890109
    DATA: l_level TYPE i.                                   "n890109
    DATA: lt_sort TYPE kkblo_t_sortinfo.                    "n890109
                                                            "n890109
    CALL FUNCTION 'K_KKB_SUMLEVEL_OF_LIST_GET'              "n890109
      IMPORTING                                             "n890109
        e_sumlevel = l_level                      "n890109
      EXCEPTIONS                                            "n890109
        OTHERS     = 1.                           "n890109
                                                            "n890109
    IF  NOT sy-subrc IS INITIAL.                            "n890109
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno     "n890109
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.       "n890109
    ENDIF.                                                  "n890109
                                                            "n890109
    IF NOT l_level IS INITIAL.                              "n890109
      CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA'               "n890109
        EXPORTING                                           "n890109
          it_sort = sorttab[]                    "n890109
        IMPORTING                                           "n890109
          et_sort = lt_sort[].                   "n890109
                                                            "n890109
      CALL FUNCTION 'K_KKB_SUMLEVEL_SELECT'                 "n890109
        EXPORTING                                           "n890109
          i_no_dialog = 'X'                          "n890109
          i_sumlevel  = l_level                      "n890109
        CHANGING                                            "n890109
          ct_sort     = lt_sort[]                    "n890109
        EXCEPTIONS                                          "n890109
          OTHERS      = 1.                           "n890109
                                                            "n890109
      IF  NOT sy-subrc IS INITIAL.                          "n890109
        MESSAGE ID sy-msgid TYPE  sy-msgty NUMBER sy-msgno  "n890109
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.       "n890109
      ENDIF.                                                "n890109
                                                            "n890109
      CALL FUNCTION 'REUSE_ALV_TRANSFER_DATA_BACK'          "n890109
        EXPORTING                                           "n890109
          it_sort = lt_sort[]                    "n890109
        IMPORTING                                           "n890109
          et_sort = sorttab[]                    "n890109
        EXCEPTIONS                                          "n890109
          OTHERS  = 1.                           "n890109
                                                            "n890109
      IF  NOT sy-subrc IS INITIAL.                          "n890109
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno   "n890109
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.       "n890109
      ENDIF.                                                "n890109
                                                            "n890109
      layout-totals_only = 'X'.                             "n890109
    ELSE.                                                   "n890109
      CLEAR layout-totals_only.                             "n890109
    ENDIF.                                                  "n890109
  ENDIF.                                                    "n890109

  IF  g_flag_break-b5 = 'X'.                                "n921164
    BREAK-POINT              ID mmim_rep_mb5b.              "n921164
*   dynamic break-point : check input data for list viewer  "n921164
  ENDIF.                                                    "n921164

  CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
    EXPORTING
      i_interface_check        = g_flag_i_check       "n599218
      i_callback_program       = repid
      i_callback_pf_status_set = 'STATUS'
      i_callback_user_command  = 'USER_COMMAND'
*     I_STRUCTURE_NAME         =
      is_layout                = layout
      it_fieldcat              = fieldcat[]
*     IT_EXCLUDING             =
*     IT_SPECIAL_GROUPS        =
      it_sort                  = sorttab[]
      it_filter                = filttab[]
*     IS_SEL_HIDE              =
      i_default                = 'X'
*     i_save                   = 'A'               "note 311825
*     is_variant               = variante          "note 311825
      it_events                = events[]
      it_event_exit            = event_exit[]
      is_print                 = g_s_print
*     I_SCREEN_START_COLUMN    = 0
*     I_SCREEN_START_LINE      = 0
*     I_SCREEN_END_COLUMN      = 0
*     I_SCREEN_END_LINE        = 0
*      IMPORTING
*     e_exit_caused_by_caller  = 'X'
*     es_exit_caused_by_user   = 'X'
    TABLES
*     t_outtab                 = belege.
      t_outtab                 = g_t_belege
    EXCEPTIONS
*     program_error            = 1
      OTHERS                   = 2.

* does the ALV return with an error ?
  IF  NOT sy-subrc IS INITIAL.         "Fehler vom ALV ?
    MESSAGE ID sy-msgid TYPE  'S'     NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                               " LISTAUSGABE

*&---------------------------------------------------------------------*
*&      Form  LISTE
*&---------------------------------------------------------------------*

FORM liste.                                      "#EC CALLED  "n1511550
**--- begin of note 1481757 ---------------------------------"n1481757

* in the case the archive access delivered errors, send     "n1481757
* a popup and prepare the errors printing at the end of     "n1481757
* the list output                                           "n1481757
  IF NOT archive_messages[] IS INITIAL.                     "n1481757
    SORT archive_messages                                   "n1481757
                   BY msgid msgno msgv1 msgv2 msgv3 msgv4.  "n1481757

    IF sy-batch IS INITIAL.
      IF matnr IS INITIAL.                                  "n1481757
        MOVE : 'I'               TO  matnr-sign,            "n1481757
               'GT'              TO  matnr-option.          "n1481757
        APPEND                   matnr.                     "n1481757
* send pop-up : go on or cancel                             "n1481757
        CALL FUNCTION 'POPUP_TO_CONFIRM'                      "n1481757
          EXPORTING                                           "n1481757
            titlebar              = TEXT-137                 "n1481757
*     show available ( incomplete ) data ?                  "n1481757
            text_question         = TEXT-132                 "n1481757
            text_button_1         = TEXT-133        "yes     "n1481757
            icon_button_1         = 'ICON_OKAY'              "n1481757
            text_button_2         = TEXT-134        "no      "n1481757
            icon_button_2         = 'ICON_CANCEL'           "n1481757
            default_button        = '2'                      "n1481757
            display_cancel_button = ' '                      "n1481757
          IMPORTING                                           "n1481757
            answer                = g_flag_answer            "n1481757
          EXCEPTIONS                                          "n1481757
            OTHERS                = 1.                       "n1481757
                                                            "n1481757
        IF sy-subrc <> 0.                                   "n1481757
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno "n1481757
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.       "n1481757
        ENDIF.                                              "n1481757
                                                            "n1481757
        IF  g_flag_answer = '2'.                            "n1481757
*   the user answered with "no" : delete table with the     "n1481757
*   found MM docs and give an empty table to the ALV        "n1481757
          CLEAR g_t_mseg_lean.
          PERFORM nachrichtenausgabe.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
* get the number of lines to be processed = total - 1       "n599218
  DESCRIBE TABLE bestand     LINES  g_f_cnt_bestand_total.  "n599218
  SUBTRACT  1                FROM   g_f_cnt_bestand_total.  "n599218
  CLEAR                      g_f_cnt_bestand_curr.          "n599218

  LOOP AT bestand FROM 2.
*   clear belege. refresh belege.
    CLEAR g_t_belege. REFRESH g_t_belege.
    ADD  1                   TO  g_f_cnt_bestand_curr.      "n599218

    IF bwbst IS INITIAL.
*     fill the data table for the ALV with the              "n921165
*     corresponding MM documents for mode = stock           "n921165
      PERFORM  fill_data_table                              "n921165
                             TABLES    g_t_belege           "n921165
                             USING     bestand-matnr        "n921165
                                       bestand-werks        "n921165
                                       bestand-charg.       "n921165

    ELSEIF NOT bwbst IS INITIAL.
*     fill the data table for the ALV with the              "n921165
*     corresponding MM documents for mode = valuated stock  "n921165
      PERFORM  process_plants_of_bwkey                      "n921165
                             TABLES    g_t_belege           "n921165
                             USING     bestand-matnr        "n921165
                                       bestand-bwkey.       "n921165
    ENDIF.

*   sort belege by budat mblnr zeile.
    SORT g_t_belege          BY budat mblnr zeile.

    CLEAR events. REFRESH events.
    events-name = 'TOP_OF_PAGE'.
    events-form = 'UEBERSCHRIFT'.
    APPEND events.

*   the last ALV block should print the end line            "n599218
    IF  g_f_cnt_bestand_total = g_f_cnt_bestand_curr.       "n599218
*     this is the very last                                 "n599218
      events-name =       'END_OF_LIST'.                    "n599218
      events-form = 'PRINT_END_OF_LIST'.                    "n599218
      APPEND events.                                        "n599218
    ENDIF.                                                  "n599218

    CLEAR sorttab. REFRESH sorttab.
    CLEAR filttab. REFRESH filttab.

    CALL FUNCTION 'REUSE_ALV_LIST_LAYOUT_INFO_GET'
      IMPORTING
*       es_layout     = layout
        et_fieldcat   = fieldcat[]
        et_sort       = sorttab[]
        et_filter     = filttab[]
*       ES_LIST_SCROLL =
*       ES_VARIANT    =
      EXCEPTIONS
        no_infos      = 1
        program_error = 2
        OTHERS        = 3.

    layout-list_append = 'X'.
    PERFORM listausgabe.
  ENDLOOP.


  PERFORM nachrichtenausgabe.                               "n1481757


*----- end of note 1481757 ---------------------------------"n1481757

ENDFORM.                               " LISTE

*&---------------------------------------------------------------------*
*    print_end_of_list                                      "n599218
*-----------------------------------------------------------"n599218
                                                            "n599218
FORM print_end_of_list.                          "#EC CALLED  "n1511550
                                                            "n599218
* go on when the report runs in print mode -> last line
  CHECK NOT sy-prdsn IS INITIAL.

  DATA: lr_content TYPE REF TO cl_salv_form_element.

*... (1) create the information to be displayed by using
*        the ALV Form elements
  PERFORM print_end_of_list_render  CHANGING lr_content.

*... (2) Sending the information to the ALV
*        Once the inforation to be displayed has been
*        created the information has to be sent to the ALV
*        This is done by calling the static method
*        CL_SALV_FORM_CONTENT=>SET( <content> ) with the content
*        which is to be displayed.
*        Alternativly the function module REUSE_ALV_COMMENTARY_WRITE
*        can still be used.
  cl_salv_form_content=>set( lr_content ).
                                                            "n599218
ENDFORM.                     "print_end_of_list             "n599218
                                                            "n599218
*----------------------------------------------------------------------*
*     print_end_of_list_render
*----------------------------------------------------------------------*

FORM  print_end_of_list_render
         CHANGING cr_content TYPE REF TO cl_salv_form_element.

  DATA: lr_grid     TYPE REF TO cl_salv_form_layout_grid,
        lr_flow     TYPE REF TO cl_salv_form_layout_flow,
        l_text(500) TYPE c,
        l_char(500) TYPE c.

*... create a grid
  CREATE OBJECT lr_grid.

  lr_flow = lr_grid->create_flow( row = 1  column = 1 ).

  IF  bwbst IS INITIAL.
*     stocks only
    MOVE  : g_end_line_77       TO  l_text.
  ELSE.
*     stocks and values
    MOVE  : g_end_line_91       TO  l_text.
  ENDIF.

*   add line to object
  lr_flow->create_text( text = l_text ).

* copy whole header object
  cr_content = lr_grid.

ENDFORM.                     " print_end_of_list_render

*-----------------------------------------------------------"n599218
*    create_headline                                        "n599218
*-----------------------------------------------------------"n599218
                                                            "n599218
FORM create_headline.                                       "n599218
                                                            "n599218
  DATA : l_offset TYPE i,                                   "n599218
         l_strlen TYPE i.                                   "n599218
                                                            "n599218
* get the length of the title                               "n599218
  COMPUTE  l_strlen          = strlen( sy-title ).          "n599218
                                                            "n599218
  IF  bwbst IS INITIAL.                                     "n599218
*   stocks only --> small line with 77 bytes                "n599218
    IF      l_strlen   =  59.                               "n599218
      MOVE : sy-title        TO  g_s_header_77-title.       "n599218
    ELSEIF  l_strlen   >  59.                               "n599218
      MOVE : sy-title        TO  g_s_header_77-title,       "n599218
             '...'          TO  g_s_header_77-title+56(03). "n599218
    ELSE.                                                   "n599218
      COMPUTE  l_offset      =  ( 59 - l_strlen ) / 2.      "n599218
      MOVE : sy-title     TO  g_s_header_77-title+l_offset. "n599218
    ENDIF.                                                  "n599218
                                                            "n599218
    WRITE : sy-datlo DD/MM/YYYY   TO  g_s_header_77-date.   "n599218
  ELSE.                                                     "n599218
*   stocks and values --> wide line with 91 bytes           "n599218
    IF      l_strlen   =  73.                               "n599218
      MOVE : sy-title        TO  g_s_header_91-title.       "n599218
    ELSEIF  l_strlen   >  73.                               "n599218
      MOVE : sy-title        TO  g_s_header_91-title,       "n599218
             '...'          TO  g_s_header_91-title+70(03). "n599218
    ELSE.                                                   "n599218
      COMPUTE  l_offset      =  ( 73 - l_strlen ) / 2.      "n599218
      MOVE : sy-title     TO  g_s_header_91-title+l_offset. "n599218
    ENDIF.                                                  "n599218
                                                            "n599218
    WRITE : sy-datlo DD/MM/YYYY   TO  g_s_header_91-date.   "n599218
  ENDIF.                                                    "n599218
                                                            "n599218
* create the end lines, too                                 "n599218
  CONCATENATE  TEXT-062      "End of List                   "n599218
               ':'           sy-title                       "n599218
                             INTO  g_end_line_77            "n599218
                             SEPARATED BY space.            "n599218
  MOVE : g_end_line_77       TO  g_end_line_91.             "n599218
                                                            "n599218
ENDFORM.                     "create_headline               "n599218
                                                            "n599218
*-----------------------------------------------------------"n599218

"{ Begin ENHO DIPCS_RM07MLBD_FORM_01 IS-AD-SSP AD_SUB }
* DI A&D SSP
** DI IS-ADEC-SSP Customer Stock
*----------------------------------------------------------------------*
*    AKTUELLE_BST_SBBST_B
*----------------------------------------------------------------------*

FORM aktuelle_bst_sbbst_b.
*--------------------  Kundenbeistellbestand  -------------------------*
*   elseif sobkz = 'B'.
  PERFORM hdb_check_table USING 'MCSD' ''.                                     "SH note 1787730
  SELECT * FROM mcsd INTO CORRESPONDING FIELDS OF TABLE xmcsd CONNECTION (dbcon) "1787730
                                     WHERE werks IN g_ra_werks
                                     AND   lgort IN g_ra_lgort
                                     AND   matnr IN matnr
                                     AND   charg IN charg
                                     AND   sobkz EQ sobkz.

  IF sy-subrc <> 0.          "no record found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

* process customer stock
  LOOP AT xmcsd.
    PERFORM  f9000_auth_plant_check    USING  xmcsd-werks.

    IF  g_flag_authority IS INITIAL.
      DELETE                 xmcsd.
    ELSE.
      PERFORM  f9200_collect_plant     USING  xmcsd-werks.

      PERFORM  f9400_material_key      USING  xmcsd-matnr.
    ENDIF.
  ENDLOOP.

  DESCRIBE TABLE xmcsd       LINES  g_f_cnt_lines.
  IF  g_f_cnt_lines IS INITIAL.        "no record left
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.

  SORT xmcsd.
  LOOP AT xmcsd.
    MOVE-CORRESPONDING xmcsd TO imcsd.
    COLLECT imcsd.
  ENDLOOP.
  FREE xmcsd. REFRESH xmcsd.

  IF xchar = ' '.
    LOOP AT imcsd.
      MOVE-CORRESPONDING imcsd TO imcsdx.
      COLLECT imcsdx.
    ENDLOOP.
    SORT imcsdx.
  ELSEIF xchar = 'X'.
    LOOP AT imcsd.
      CHECK imcsd-charg IS INITIAL.
      DELETE imcsd.
    ENDLOOP.
  ENDIF.

ENDFORM.                     "aktuelle_bst_sbbst_b

** A&D IS-ADEC-SUB Customer Stock with Vendor             "v_n_GA1551829
*----------------------------------------------------------------------*
*    AKTUELLE_BST_SBBST_C
*----------------------------------------------------------------------*
FORM aktuelle_bst_sbbst_c.
*---------------------- Kundenbeistellbestand -------------------------*
*   elseif sobkz = 'C'.
  PERFORM hdb_check_table USING 'MSCD' ''.
  SELECT * FROM mscd INTO CORRESPONDING FIELDS OF TABLE xmscd CONNECTION (dbcon)
                                     WHERE werks IN g_ra_werks
                                     AND   matnr IN matnr
                                     AND   charg IN charg
                                     AND   sobkz EQ sobkz.
  IF sy-subrc <> 0.            "no records found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.
* process A&D Sales Order Stock
  LOOP AT xmscd.
    PERFORM  f9000_auth_plant_check    USING  xmscd-werks.
    IF  g_flag_authority IS INITIAL.
      DELETE                   xmscd.
    ELSE.
      PERFORM  f9200_collect_plant     USING  xmscd-werks.
      PERFORM  f9400_material_key      USING  xmscd-matnr.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE xmscd       LINES  g_f_cnt_lines.
  IF  g_f_cnt_lines IS INITIAL.        "no records left ?
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.
  SORT xmscd.
  LOOP AT xmscd.
    MOVE-CORRESPONDING xmscd TO imscd.
    COLLECT imscd.
  ENDLOOP.
  FREE xmscd. REFRESH xmscd.
  IF xchar = ' '.
    LOOP AT imscd.
      MOVE-CORRESPONDING imscd TO imscdx.
      COLLECT imscdx.
    ENDLOOP.
    SORT imscdx.
  ELSEIF xchar = 'X'.
    LOOP AT imscd.
      CHECK imscd-charg IS INITIAL.
      DELETE imscd.
    ENDLOOP.
  ENDIF.
ENDFORM.                     "aktuelle_bst_sbbst_c

** A&D IS-ADEC-SUB Sales Order Stock
*----------------------------------------------------------------------*
*    AKTUELLE_BST_SBBST_F
*----------------------------------------------------------------------*
FORM aktuelle_bst_sbbst_f.
*---------------------- Kundenauftragsbestand -------------------------*
*   elseif sobkz = 'F'.
  PERFORM hdb_check_table USING 'MSFD' ''.
  SELECT * FROM msfd INTO CORRESPONDING FIELDS OF TABLE xmsfd CONNECTION (dbcon)
                                     WHERE werks IN g_ra_werks
                                     AND   matnr IN matnr
                                     AND   charg IN charg
                                     AND   sobkz EQ sobkz.
  IF sy-subrc <> 0.            "no records found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.
* process A&D Sales Order Stock
  LOOP AT xmsfd.
    PERFORM  f9000_auth_plant_check    USING  xmsfd-werks.
    IF  g_flag_authority IS INITIAL.
      DELETE                   xmsfd.
    ELSE.
      PERFORM  f9200_collect_plant     USING  xmsfd-werks.
      PERFORM  f9400_material_key      USING  xmsfd-matnr.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE xmsfd       LINES  g_f_cnt_lines.
  IF  g_f_cnt_lines IS INITIAL.        "no records left ?
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.
  SORT xmsfd.
  LOOP AT xmsfd.
    MOVE-CORRESPONDING xmsfd TO imsfd.
    COLLECT imsfd.
  ENDLOOP.
  FREE xmsfd. REFRESH xmsfd.
  IF xchar = ' '.
    LOOP AT imsfd.
      MOVE-CORRESPONDING imsfd TO imsfdx.
      COLLECT imsfdx.
    ENDLOOP.
    SORT imsfdx.
  ELSEIF xchar = 'X'.
    LOOP AT imsfd.
      CHECK imsfd-charg IS INITIAL.
      DELETE imsfd.
    ENDLOOP.
  ENDIF.
ENDFORM.                     "aktuelle_bst_sbbst_f

** A&D IS-ADEC-SUB Project Stock with Vendor
*----------------------------------------------------------------------*
*    AKTUELLE_BST_SBBST_R
*----------------------------------------------------------------------*
FORM aktuelle_bst_sbbst_r.
*----------------------- Projektbestand -------------------------------*
*   elseif sobkz = 'R'.
  PERFORM hdb_check_table USING 'MSRD' ''.
  SELECT * FROM msrd INTO CORRESPONDING FIELDS OF TABLE xmsrd CONNECTION (dbcon)
                                     WHERE werks IN g_ra_werks
                                     AND   matnr IN matnr
                                     AND   charg IN charg
                                     AND   sobkz EQ sobkz.
  IF sy-subrc <> 0.          "no record found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.
* process A&D Project Stock
  LOOP AT xmsrd.
    PERFORM  f9000_auth_plant_check    USING  xmsrd-werks.
    IF  g_flag_authority IS INITIAL.
      DELETE                 xmsrd.
    ELSE.
      PERFORM  f9200_collect_plant     USING  xmsrd-werks.
      PERFORM  f9400_material_key      USING  xmsrd-matnr.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE xmsrd       LINES  g_f_cnt_lines.
  IF  g_f_cnt_lines IS INITIAL.        "no record left
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.
  SORT xmsrd.
  LOOP AT xmsrd.
    MOVE-CORRESPONDING xmsrd TO imsrd.
    COLLECT imsrd.
  ENDLOOP.
  FREE xmsrd. REFRESH xmsrd.
  IF xchar = ' '.
    LOOP AT imsrd.
      MOVE-CORRESPONDING imsrd TO imsrdx.
      COLLECT imsrdx.
    ENDLOOP.
    SORT imsrdx.
  ELSEIF xchar = 'X'.
    LOOP AT imsrd.
      CHECK imsrd-charg IS INITIAL.
      DELETE imsrd.
    ENDLOOP.
  ENDIF.
ENDFORM.                     "aktuelle_bst_sbbst_r

** A&D IS-ADEC-SUB Vendor Consignment / RTP Stock with Vendor
*----------------------------------------------------------------------*
*    AKTUELLE_BST_SBBST_I_J
*----------------------------------------------------------------------*
FORM aktuelle_bst_sbbst_i_j.
*---------------------- RTP bestand -------------------------*
*   elseif sobkz = 'I' or sobkz = 'J'.
  PERFORM hdb_check_table USING 'MSID' ''.
  SELECT * FROM msid INTO CORRESPONDING FIELDS OF TABLE xmsid CONNECTION (dbcon)
                                     WHERE werks IN g_ra_werks
                                     AND   matnr IN matnr
                                     AND   charg IN charg
                                     AND   sobkz EQ sobkz.
  IF sy-subrc <> 0.            "no records found
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.
* process A&D Vendor Consignment or RTP Stock
  LOOP AT xmsid.
    PERFORM  f9000_auth_plant_check    USING  xmsid-werks.
    IF  g_flag_authority IS INITIAL.
      DELETE                   xmsid.
    ELSE.
      PERFORM  f9200_collect_plant     USING  xmsid-werks.
      PERFORM  f9400_material_key      USING  xmsid-matnr.
    ENDIF.
  ENDLOOP.
  DESCRIBE TABLE xmsid       LINES  g_f_cnt_lines.
  IF  g_f_cnt_lines IS INITIAL.        "no records left ?
    MESSAGE s289.
*   Kein Material in Selektion vorhanden.
    PERFORM anforderungsbild.
  ENDIF.
  SORT xmsid.
  LOOP AT xmsid.
    MOVE-CORRESPONDING xmsid TO imsid.
    COLLECT imsid.
  ENDLOOP.
  FREE xmsid. REFRESH xmsid.
  IF xchar = ' '.
    LOOP AT imsid.
      MOVE-CORRESPONDING imsid TO imsidx.
      COLLECT imsidx.
    ENDLOOP.
    SORT imsidx.
  ELSEIF xchar = 'X'.
    LOOP AT imsid.
      CHECK imsid-charg IS INITIAL.
      DELETE imsid.
    ENDLOOP.
  ENDIF.
ENDFORM.                     "aktuelle_bst_sbbst_ij       "^_n_GA1551829
"{ End ENHO DIPCS_RM07MLBD_FORM_01 IS-AD-SSP AD_SUB }

*ENHANCEMENT-POINT rm07mlbd_form_01_01 SPOTS es_rm07mlbd STATIC.

*---- begin of note 921165 ---------------------------------"n921165
*&----------------------------------------------------------"n921165
*&      Form  fill_data_table                               "n921165
*&----------------------------------------------------------"n921165
                                                            "n921165
* - improve performance processing internal tables          "n921165

FORM fill_data_table
         TABLES    l_t_belege      TYPE  stab_belege
         USING     l_matnr         TYPE  mseg-matnr
                   l_werks         TYPE  mseg-werks
                   l_charg         TYPE  mseg-charg.

* define local data fields
  DATA : l_s_belege          TYPE  stype_belege.

* sort table with the MM docs only once
  IF  g_flag_sorted  IS INITIAL.
    MOVE  'X'                TO  g_flag_sorted.
    SORT  g_t_mseg_lean      BY  matnr  werks  charg.
  ENDIF.

* read the first matching line depending on the batch
  IF  l_charg IS INITIAL.
    READ  TABLE g_t_mseg_lean     ASSIGNING  <g_fs_mseg_lean>
      WITH KEY matnr = l_matnr
               werks = l_werks    BINARY SEARCH.
  ELSE.
    READ  TABLE g_t_mseg_lean     ASSIGNING  <g_fs_mseg_lean>
      WITH KEY matnr = l_matnr
               werks = l_werks
               charg = l_charg    BINARY SEARCH.
  ENDIF.

* the first entry found ? -> go on
  CHECK sy-subrc IS INITIAL.

  MOVE  sy-tabix             TO  g_tabix_set.

* go on with sequential reading
  LOOP AT g_t_mseg_lean     INTO  g_s_mseg_lean
    FROM g_tabix_set
    WHERE usnam IN usnam  "Os novos filtros da tela de selecao foram inseridos
      AND vgart IN vgart  "aqui para nao impactar a performace otimizada do
      AND xblnr IN xblnr  "acesso ao BD
      AND mblnr IN mblnr
      AND lifnr IN lifnr
      AND kunnr IN kunnr
      AND aufnr IN aufnr
*      AND mtart IN mtart

      AND bwart IN bwart
      AND lgort IN g_ra_lgort.

*   take this entry when the key fields match
    IF  g_s_mseg_lean-matnr = l_matnr   AND
        g_s_mseg_lean-werks = l_werks.

*     cnsider the batches when this report runs in mode
*     "storage loc/batches" or "special stock"
      IF  bwbst IS INITIAL.
        CHECK : xchar               IS INITIAL       OR
                g_s_mseg_lean-charg = l_charg.
      ENDIF.

      MOVE-CORRESPONDING g_s_mseg_lean
                             TO  l_s_belege.

      "Recupera valores dos campos novos
      PERFORM zf_fill_cust_values CHANGING l_s_belege.
      CHECK l_s_belege-mtart IN mtart.

*     enrich some fields with color and numeric fields with sign
*     the negative sign was not set for GI postings         "n944522
      PERFORM  f9500_set_color_and_sign                     "n944522
                       USING  l_s_belege  'L_S_BELEGE'.     "n944522

      APPEND  l_s_belege     TO  l_t_belege.
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " fill_data_table

*&----------------------------------------------------------"n921165
*&      Form  process_plants_of_bwkey                       "n921165
*&----------------------------------------------------------"n921165

FORM process_plants_of_bwkey
         TABLES    l2_t_belege  TYPE  stab_belege
         USING     l2_matnr     TYPE  mseg-matnr
                   l2_bwkey     TYPE  mbew-bwkey.

* define local working fields
  FIELD-SYMBOLS :
    <l_fs_organ>             TYPE  stype_organ.
  DATA : l_tabix_start       TYPE  sy-tabix.


  IF  curm = '1'.
*   valuation level is plant / process the MM docs
    PERFORM  fill_data_table TABLES    l2_t_belege          "n921165
                             USING     l2_matnr             "n921165
                                       l2_bwkey             "n921165
                                       space.               "n921165
    EXIT.                    " leave this routine
  ENDIF.

* valuation leve = company code : plenty of plants could
* be assigned to the valuation area, look for the assigned
* plants and look for the MM doc per plant

* look for the first valuation area
  READ TABLE g_t_organ       ASSIGNING  <l_fs_organ>
    WITH KEY keytype  = c_bwkey
             keyfield = l2_bwkey
                             BINARY SEARCH.

* go on when a valuation area was found
  CHECK sy-subrc IS INITIAL.

  MOVE  sy-tabix             TO  l_tabix_start.

* seq. read of all matching entries
  LOOP AT g_t_organ          ASSIGNING <l_fs_organ>
    FROM l_tabix_start.

    IF  <l_fs_organ>-keytype   = c_bwkey      AND
        <l_fs_organ>-keyfield  = l2_bwkey.
*     process the MM docs from this plant
      PERFORM  fill_data_table                              "n921165
                             TABLES    l2_t_belege          "n921165
                             USING     l2_matnr             "n921165
                                       <l_fs_organ>-werks   "n921165
                                       space.               "n921165
    ELSE.
      EXIT.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " process_plants_of_bwkey       "n921165
                                                            "n921165
*---- end of note 921165 -----------------------------------"n921165
*&---------------------------------------------------------------------*
*&      Form  NACHRICHTENAUSGABE
*&---------------------------------------------------------------------*
FORM nachrichtenausgabe .
* Report errors if found                                    "n1481757
  IF  NOT archive_messages[] IS INITIAL.                    "n1481757
    SORT archive_messages                                   "n1481757
       BY msgid msgno msgv1 msgv2 msgv3 msgv4.              "n1481757
                                                            "n1481757
    TYPES: BEGIN OF slis_fieldcat,                          "n1481757
             row_pos     LIKE sy-curow, " output in row      "n1481757
             col_pos     LIKE sy-cucol, " position of the column "n1481757
             fieldname   TYPE slis_fieldname,               "n1481757
             ref_tabname TYPE slis_tabname,                 "n1481757
             msgid       LIKE sy-msgid,                     "n1481757
             msgno       LIKE sy-msgno,                     "n1481757
             msgv1       LIKE sy-msgv1,                     "n1481757
             msgv2       LIKE sy-msgv2,                     "n1481757
             msgv3       LIKE sy-msgv3,                     "n1481757
             msgv4       LIKE sy-msgv4,                     "n1481757
           END OF slis_fieldcat.                            "n1481757
*                                                           "n1481757
    DATA: BEGIN OF outtab OCCURS 0,                         "n1481757
            msgid    LIKE sy-msgid,                         "n1481757
            msgno    LIKE sy-msgno,                         "n1481757
            text(80),                                       "n1481757
          END OF outtab.                                    "n1481757
    DATA: fc TYPE slis_fieldcat_alv OCCURS 0 WITH HEADER LINE. "n1481757
    DATA: souttab LIKE LINE OF outtab.                      "n1481757
    REFRESH outtab.                                         "n1481757
*                                                           "n1481757
    LOOP AT archive_messages.                               "n1481757
      MOVE-CORRESPONDING archive_messages TO outtab.        "n1481757
      MESSAGE ID     archive_messages-msgid                 "n1481757
              TYPE   'E'                                    "n1481757
              NUMBER archive_messages-msgno                 "n1481757
              WITH   archive_messages-msgv1                 "n1481757
                     archive_messages-msgv2                 "n1481757
                     archive_messages-msgv3                 "n1481757
                     archive_messages-msgv4                 "n1481757
              INTO   outtab-text.                           "n1481757
      APPEND outtab.                                        "n1481757
    ENDLOOP.                                                "n1481757
*                                                           "n1481757
    REFRESH fc.                                             "n1481757
    fc-fieldname = 'MSGID'.                                 "n1481757
    fc-ref_tabname = 'SYST'.                                "n1481757
    APPEND fc.                                              "n1481757
    fc-fieldname = 'MSGNO'.                                 "n1481757
    fc-ref_tabname = 'SYST'.                                "n1481757
    APPEND fc.                                              "n1481757
    fc-fieldname = 'TEXT'.                                  "n1481757
    fc-ref_tabname = 'T100'.                                "n1481757
    APPEND fc.                                              "n1481757
                                                            "n1481757

    DATA: it_commentary TYPE slis_t_listheader.
    DATA: is_commentary LIKE LINE OF it_commentary.

    LOOP AT outtab INTO souttab.
      is_commentary-typ = 'H'.
      is_commentary-info = souttab-text.
      APPEND is_commentary TO it_commentary.
    ENDLOOP.

    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = it_commentary.         "n1481757
                                                            "n1481757
  ENDIF.                                                    "n1481757

ENDFORM.                    " NACHRICHTENAUSGABE

*&---------------------------------------------------------------------*
*&      Form  BUILD_BKLAS_SELECTION
*&---------------------------------------------------------------------*
*       Build internal selection options for Valuation Class
*       restriction, in case G/L account is restricted
*----------------------------------------------------------------------*
*  -->  HKONT       Selection screen options
*  <--  iBKLAS      Internal selection options
*----------------------------------------------------------------------*
FORM build_bklas_selection .

  DATA:
    lv_msgtext(40) TYPE c,
    ls_t030        LIKE t030,
    ls_t030r       LIKE t030r,
    lt_organ       LIKE g_t_organ[],
    lt_t030        TYPE TABLE OF t030 WITH HEADER LINE.

  CHECK hkont IS NOT INITIAL.

* table G_T_ORGAN should be already filled in; otherwise settings for all
* charts of accounts need to be read which is very time consuming
  lt_organ = g_t_organ[].
  SORT lt_organ BY ktopl bwmod.
  DELETE ADJACENT DUPLICATES FROM lt_organ COMPARING ktopl bwmod.
  IF lt_organ IS INITIAL.
    MESSAGE s480.
    PERFORM anforderungsbild.
  ENDIF.

* read account determination rules
  SELECT * FROM t030r INTO ls_t030r FOR ALL ENTRIES IN lt_organ
           WHERE ktopl EQ lt_organ-ktopl
             AND ktosl EQ 'BSX'
        ORDER BY PRIMARY KEY.
*   read valuation classes and relevant accounts
    SELECT * FROM t030 INTO ls_t030
             WHERE ktopl EQ ls_t030r-ktopl
               AND ktosl EQ 'BSX'
               AND konts IN hkont
        ORDER BY PRIMARY KEY.
      IF NOT ls_t030-komok IS INITIAL OR
        ls_t030-konts NE ls_t030-konth.
        MOVE ls_t030-ktopl TO lv_msgtext.
        WRITE 'BSX' TO lv_msgtext+10.
        WRITE ls_t030-konts TO lv_msgtext+20.
        CONDENSE lv_msgtext.
        MESSAGE s147(m8) WITH lv_msgtext.
*       Account determination is not possible
        PERFORM anforderungsbild.
      ENDIF.
      LOOP AT lt_organ TRANSPORTING NO FIELDS
        WHERE ktopl = ls_t030-ktopl AND bwmod = ls_t030-bwmod.
      ENDLOOP.
      CHECK sy-subrc IS INITIAL.
      IF ls_t030r-xbkla IS INITIAL.
        CHECK ls_t030-bklas IS INITIAL.
      ENDIF.
      APPEND ls_t030 TO lt_t030.
    ENDSELECT.
  ENDSELECT.

* create internal table for valuation class restriction
  SORT lt_t030 BY bklas konts.
  DELETE ADJACENT DUPLICATES FROM lt_t030 COMPARING bklas konts.
  LOOP AT lt_t030 INTO ls_t030.
    ON CHANGE OF ls_t030-bklas OR ls_t030-konts.
      CHECK ls_t030-konts IN hkont.
*     create iBKLAS table
      MOVE 'I'  TO ibklas-sign.
      MOVE 'EQ' TO ibklas-option.
      MOVE ls_t030-bklas TO ibklas-low.
      APPEND ibklas.
    ENDON.
  ENDLOOP.
  SORT ibklas. DELETE ADJACENT DUPLICATES FROM ibklas.
  IF ibklas[] IS INITIAL.
    MESSAGE s289.
*   no data contained in the selection
    PERFORM anforderungsbild.
  ENDIF.

ENDFORM.                    " BUILD_BKLAS_SELECTION
*&---------------------------------------------------------------------*
*&      Form  GET_ACC_DET
*&---------------------------------------------------------------------*
*       Get G/L account from current account determination settings
*----------------------------------------------------------------------*
FORM get_acc_det  CHANGING cs_accdet  TYPE stype_accdet.

  TYPES:
    BEGIN OF ltt_acc,
      bklas LIKE t030-bklas,
      bwmod LIKE t030-bwmod,
      ktopl LIKE t030-ktopl,
      hkont LIKE t030-konts,
    END OF ltt_acc.

  STATICS:
    lt_acc TYPE HASHED TABLE OF ltt_acc WITH UNIQUE KEY bklas bwmod ktopl.
  DATA:
    ls_acc TYPE ltt_acc.

* get organizational data
  PERFORM f9300_read_organ
    USING c_werks cs_accdet-werks.
  CHECK g_s_organ-bukrs = cs_accdet-bukrs.
  cs_accdet-bwkey = g_s_organ-bwkey.
  cs_accdet-ktopl = g_s_organ-ktopl.
  cs_accdet-bwmod = g_s_organ-bwmod.

* check for special stock that is valuated in MBEW
  IF ( cs_accdet-sobkz = 'Q' OR cs_accdet-sobkz = 'E' ) AND
     cs_accdet-kzbws = 'A'.
    CLEAR: cs_accdet-sobkz.
  ENDIF.
  IF cs_accdet-sobkz = 'O' AND cs_accdet-xobew IS INITIAL.
    CLEAR: cs_accdet-sobkz.
  ENDIF.

* get special stock data
* (if not filled yet via customer's enhancement of type STYPE_MB5B_ADD)
  CASE cs_accdet-sobkz.
    WHEN 'Q'.
      IF cs_accdet-mat_pspnr IS INITIAL.
        PERFORM hdb_check_table USING 'MSEG' ''.            "n1710850
        SELECT SINGLE mat_pspnr FROM mseg CONNECTION (dbcon) "n1710850
          INTO cs_accdet-mat_pspnr
          WHERE mblnr = cs_accdet-mblnr
            AND mjahr = cs_accdet-mjahr
            AND zeile = cs_accdet-zeile.
      ENDIF.
      CLEAR: cs_accdet-mat_kdauf, cs_accdet-mat_kdpos, cs_accdet-lifnr.
    WHEN 'E'.
      IF cs_accdet-mat_kdauf IS INITIAL OR cs_accdet-mat_kdpos IS INITIAL.
        PERFORM hdb_check_table USING 'MSEG' ''.            "n1710850
        SELECT SINGLE mat_kdauf mat_kdpos FROM mseg CONNECTION (dbcon) "n1710850
          INTO (cs_accdet-mat_kdauf, cs_accdet-mat_kdpos)
          WHERE mblnr = cs_accdet-mblnr
            AND mjahr = cs_accdet-mjahr
            AND zeile = cs_accdet-zeile.
      ENDIF.
      CLEAR: cs_accdet-mat_pspnr, cs_accdet-lifnr.
    WHEN 'O'.
      IF cs_accdet-lifnr IS INITIAL.
        PERFORM hdb_check_table USING 'MSEG' ''.            "n1710850
        SELECT SINGLE lifnr FROM mseg CONNECTION (dbcon)    "n1710850
          INTO cs_accdet-lifnr
          WHERE mblnr = cs_accdet-mblnr
            AND mjahr = cs_accdet-mjahr
            AND zeile = cs_accdet-zeile.
      ENDIF.
      CLEAR: cs_accdet-mat_kdauf, cs_accdet-mat_kdpos, cs_accdet-mat_pspnr.
    WHEN OTHERS.
      CLEAR: cs_accdet-mat_kdauf, cs_accdet-mat_kdpos,
             cs_accdet-mat_pspnr, cs_accdet-lifnr.
  ENDCASE.

* get valuation class
  PERFORM get_bklas CHANGING cs_accdet.

* get G/L account
  READ TABLE lt_acc INTO ls_acc
    WITH TABLE KEY bklas = cs_accdet-bklas
                   bwmod = cs_accdet-bwmod
                   ktopl = cs_accdet-ktopl.
  IF sy-subrc = 0.
    cs_accdet-hkont = ls_acc-hkont.
  ELSE.
    CALL FUNCTION 'MR_ACCOUNT_ASSIGNMENT'
      EXPORTING
        bewertungsklasse       = cs_accdet-bklas
        bewertung_modif        = cs_accdet-bwmod
        kontenplan             = cs_accdet-ktopl
        soll_haben_kennzeichen = 'S'
        vorgangsschluessel     = 'BSX'
      IMPORTING
        konto                  = cs_accdet-hkont
      EXCEPTIONS
        OTHERS                 = 5.
    IF sy-subrc <> 0.
      CLEAR cs_accdet-hkont.
    ENDIF.
    ls_acc-bklas = cs_accdet-bklas.
    ls_acc-bwmod = cs_accdet-bwmod.
    ls_acc-ktopl = cs_accdet-ktopl.
    ls_acc-hkont = cs_accdet-hkont.
    INSERT ls_acc INTO TABLE lt_acc.
  ENDIF.

ENDFORM.                    " GET_ACC_DET
*&---------------------------------------------------------------------*
*&      Form  GET_BKLAS
*&---------------------------------------------------------------------*
*      Get valuation class from current settings
*----------------------------------------------------------------------*
FORM get_bklas  CHANGING cs_accdet  TYPE stype_accdet.

  TYPES:
    BEGIN OF ltt_bklas,
      matnr     LIKE mbew-matnr,
      bwkey     LIKE mbew-bwkey,
      bwtar     LIKE mbew-bwtar,
      sobkz     LIKE qbew-sobkz,
      mat_pspnr LIKE qbew-pspnr,
      mat_kdauf LIKE ebew-vbeln,
      mat_kdpos LIKE ebew-posnr,
      lifnr     LIKE obew-lifnr,
      bklas     LIKE mbew-bklas,
    END OF ltt_bklas.

  STATICS:
    lt_bklas TYPE HASHED TABLE OF ltt_bklas WITH UNIQUE KEY matnr bwkey
      bwtar sobkz mat_pspnr mat_kdauf mat_kdpos lifnr.
  DATA:
    ls_bklas TYPE ltt_bklas.

  READ TABLE lt_bklas INTO ls_bklas
    WITH TABLE KEY matnr     = cs_accdet-matnr
                   bwkey     = cs_accdet-bwkey
                   bwtar     = cs_accdet-bwtar
                   sobkz     = cs_accdet-sobkz
                   mat_pspnr = cs_accdet-mat_pspnr
                   mat_kdauf = cs_accdet-mat_kdauf
                   mat_kdpos = cs_accdet-mat_kdpos
                   lifnr     = cs_accdet-lifnr.
  IF sy-subrc = 0.
    cs_accdet-bklas = ls_bklas-bklas.
  ELSE.
    CASE cs_accdet-sobkz.
      WHEN 'Q'.
        PERFORM hdb_check_table USING 'QBEW' ''.            "n1710850
        SELECT SINGLE bklas FROM qbew CONNECTION (dbcon)    "n1710850
          INTO cs_accdet-bklas
          WHERE matnr = cs_accdet-matnr
            AND bwkey = cs_accdet-bwkey
            AND bwtar = cs_accdet-bwtar
            AND sobkz = cs_accdet-sobkz
            AND pspnr = cs_accdet-mat_pspnr.
      WHEN 'E'.
        PERFORM hdb_check_table USING 'EBEW' ''.            "n1710850
        SELECT SINGLE bklas FROM ebew CONNECTION (dbcon)    "n1710850
          INTO cs_accdet-bklas
          WHERE matnr = cs_accdet-matnr
            AND bwkey = cs_accdet-bwkey
            AND bwtar = cs_accdet-bwtar
            AND sobkz = cs_accdet-sobkz
            AND vbeln = cs_accdet-mat_kdauf
            AND posnr = cs_accdet-mat_kdpos.
      WHEN 'O'.
        PERFORM hdb_check_table USING 'OBEW' ''.            "n1710850
        SELECT SINGLE bklas FROM obew CONNECTION (dbcon)    "n1710850
          INTO cs_accdet-bklas
          WHERE matnr = cs_accdet-matnr
            AND bwkey = cs_accdet-bwkey
            AND bwtar = cs_accdet-bwtar
            AND sobkz = cs_accdet-sobkz
            AND lifnr = cs_accdet-lifnr.
      WHEN OTHERS.
        PERFORM hdb_check_table USING 'MBEW' ''.            "n1710850
        SELECT SINGLE bklas FROM mbew CONNECTION (dbcon)    "n1710850
          INTO cs_accdet-bklas
          WHERE matnr = cs_accdet-matnr
            AND bwkey = cs_accdet-bwkey
            AND bwtar = cs_accdet-bwtar.
    ENDCASE.
    ls_bklas-matnr     = cs_accdet-matnr.
    ls_bklas-bwkey     = cs_accdet-bwkey.
    ls_bklas-bwtar     = cs_accdet-bwtar.
    ls_bklas-sobkz     = cs_accdet-sobkz.
    ls_bklas-mat_pspnr = cs_accdet-mat_pspnr.
    ls_bklas-mat_kdauf = cs_accdet-mat_kdauf.
    ls_bklas-mat_kdpos = cs_accdet-mat_kdpos.
    ls_bklas-lifnr     = cs_accdet-lifnr.
    ls_bklas-bklas     = cs_accdet-bklas.
    INSERT ls_bklas INTO TABLE lt_bklas.
  ENDIF.

ENDFORM.                    " GET_BKLAS

FORM hdb_check_table  USING                                 "n1710850
                      lv_tab1 TYPE tabname
                      lv_tab2 TYPE tabname.

* clear dbcon, set only at the end if OK
  CLEAR dbcon.

  IF dbcon_active IS INITIAL.
    RETURN.
  ENDIF.

  DATA: lt_chk_tab TYPE typ_t_tablename.

  IF lv_tab1 IS NOT INITIAL.
    APPEND lv_tab1 TO lt_chk_tab.
  ENDIF.
  IF lv_tab2 IS NOT INITIAL.
    APPEND lv_tab2 TO lt_chk_tab.
  ENDIF.

  CALL FUNCTION c_hdb_dbcon_get
    EXPORTING
      i_subappl  = c_hdb_subappl
*     I_ACT_CHECK_ONLY       =
      it_req_tab = lt_chk_tab
    IMPORTING
      e_dbcon    = dbcon.

ENDFORM.                                                    "n1710850
* begin of note 2120566                                        "v2120566
*&---------------------------------------------------------------------*
*&      Form  wesperr_aussortieren
*&---------------------------------------------------------------------*
*       Delete the non-valuated GR blocked stock for special stock OVW *
*----------------------------------------------------------------------*
FORM wesperr_aussortieren.

  LOOP AT g_t_mseg_lean      INTO   g_s_mseg_lean
                              WHERE sobkz CA 'OVW'
                              AND   bustm EQ 'ME11'.
    DELETE              g_t_mseg_lean.
  ENDLOOP.

ENDFORM.                            "wesperr_aussortieren
* end of note 2120566                                          "^2120566
