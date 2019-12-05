*----------------------------------------------------------------------*
*
*   INCLUDE RM07MLDD for report RM07MLBD
*
*----------------------------------------------------------------------*

* correction Nov. 2006 TW                                   "n999530
* plant description should appear behind plant number but   "n999530
* nevertheless the plant description should not be vissible "n999530
* for all possible selection combinations the transaction   "n999530
* MB5L could be started for.                                "n999530

* correction Aug. 2005 MM                                   "n856424
* - the fields "entry time", "entry date", and "User" are   "n856424
*   are not filled filled for price change documents        "n856424

* MB5B improved regarding accessibilty                      "n773673
*----------------------------------------------------------------------*
* Improvements :                        Dec. 2003 MM        "n599218
* - print the page numbers                                  "n599218
* - send warning M7 393 when user deletes the initial       "n599218
*   display variant                                         "n599218
* - new categories for scope of list                        "n599218
* - enable this report to run in the webreporting mode      "n599218
*----------------------------------------------------------------------*

* representation of tied empties improved    August 2002 MM "n547170

* report RM07MLBD and its includes improved  May 2002       "n497992
* - customizing table for FI summariation                   "n497992
* - consider special gain/loss-handling of IS-OIL           "n497992
* - the length of sum fields for values was increased       "n497992
* - ignore quantity unit and currency key in working tables "n497992

*----------------------------------------------------------------------*
* report RM07MLBD and its includes improved  Nov 2001       "n451923
*----------------------------------------------------------------------*
* error for split valuation and valuated special stock      "n450764
*----------------------------------------------------------------------*
* report RM07MLBD and its includes improved  May 10th, 2001 "n400992
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
* - the length of sum fields for quantities has been increased
*   to advoid decimal overflow
*----------------------------------------------------------------------*

*------------------------ DATENTYPEN ----------------------------------*

* Typen für Sonderbestände:
TYPES: BEGIN OF mslb_typ,
         werks LIKE mslb-werks,
         matnr LIKE mslb-matnr,
         sobkz LIKE mslb-sobkz,
         lblab LIKE mslb-lblab,
         lbins LIKE mslb-lbins,
         lbein LIKE mslb-lbein,
         lbuml LIKE mslb-lbuml.                             "1421484
*
TYPES: /cwm/lblab LIKE mslb-/cwm/lblab,
       /cwm/lbins LIKE mslb-/cwm/lbins,
       /cwm/lbein LIKE mslb-/cwm/lbein,
       /cwm/lbuml LIKE mslb-/cwm/lbuml.                   " analog 1421484
*ENHANCEMENT-POINT ehp605_rm07mldd_01 SPOTS es_rm07mlbd STATIC .
TYPES: END OF mslb_typ.

TYPES: BEGIN OF cmslb_typ.
    INCLUDE TYPE mslb_typ.
TYPES:   charg LIKE mslb-charg.
TYPES: END OF cmslb_typ.

TYPES: BEGIN OF msku_typ,
         werks LIKE msku-werks,
         matnr LIKE msku-matnr,
         sobkz LIKE msku-sobkz,
         kulab LIKE msku-kulab,
         kuins LIKE msku-kuins,
         kuein LIKE msku-kuein,
         kuuml LIKE msku-kuuml.                             "1421484
*
TYPES: /cwm/kulab LIKE msku-/cwm/kulab,
       /cwm/kuins LIKE msku-/cwm/kuins,
       /cwm/kuein LIKE msku-/cwm/kuein,
       /cwm/kuuml LIKE msku-/cwm/kuuml.                  " analog 1421484
*ENHANCEMENT-POINT ehp605_rm07mldd_02 SPOTS es_rm07mlbd STATIC .
TYPES: END OF msku_typ.

TYPES: BEGIN OF cmsku_typ.
    INCLUDE TYPE msku_typ.
TYPES:   charg LIKE msku-charg.
TYPES: END OF cmsku_typ.

TYPES: BEGIN OF mspr_typ,
         werks LIKE mspr-werks,
         lgort LIKE mspr-lgort,
         matnr LIKE mspr-matnr,
         sobkz LIKE mspr-sobkz,
         prlab LIKE mspr-prlab,
         prins LIKE mspr-prins,
         prspe LIKE mspr-prspe,
         prein LIKE mspr-prein.
*
TYPES: /cwm/prlab LIKE mspr-/cwm/prlab,
       /cwm/prins LIKE mspr-/cwm/prins,
       /cwm/prspe LIKE mspr-/cwm/prspe,
       /cwm/prein LIKE mspr-/cwm/prein.
*ENHANCEMENT-POINT ehp605_rm07mldd_03 SPOTS es_rm07mlbd STATIC .
TYPES: END OF mspr_typ.

TYPES: BEGIN OF cmspr_typ.
    INCLUDE TYPE mspr_typ.
TYPES:   charg LIKE mspr-charg.
TYPES: END OF cmspr_typ.


"{ Begin ENHO AD_MPN_PUR2_RM07MLBD IS-AD-MPN-MD AD_MPN }

* DI A&D MPN
DATA: BEGIN OF wa_mpn_mard,
        mpn_mard TYPE mard,
        mpn_mara TYPE mara,
      END OF wa_mpn_mard.

DATA: conv_exit_check1 LIKE t130f-kzkma,
      conv_exit_check2 LIKE tka53-sign.


DATA: l_buffer TYPE mpnbuffr OCCURS 0 WITH HEADER LINE.

"{ End ENHO AD_MPN_PUR2_RM07MLBD IS-AD-MPN-MD AD_MPN }
*
DATA: gv_/cwm/offset_qty  TYPE i,
      gv_/cwm/offset_unit TYPE i.


"{ Begin ENHO DIPCS_RM07MLDD IS-AD-SSP AD_SUB }
* DI IS-ADEC-SSP Customer Stock
TYPES: BEGIN OF mcsd_typ,
         werks LIKE mcsd-werks,
         lgort LIKE mcsd-lgort,
         matnr LIKE mcsd-matnr,
         sobkz LIKE mcsd-sobkz,
         sdlab LIKE mcsd-sdlab,
         sdins LIKE mcsd-sdins,
         sdspe LIKE mcsd-sdspe,
         sdein LIKE mcsd-sdein.
TYPES: END OF mcsd_typ.

TYPES: BEGIN OF cmcsd_typ.
    INCLUDE TYPE mcsd_typ.
TYPES:   charg LIKE mcsd-charg.
TYPES: END OF cmcsd_typ.

** DI IS-ADEC-SSP Customer Stock
DATA: xmcsd  TYPE cmcsd_typ OCCURS 0 WITH HEADER LINE.
DATA: imcsd  TYPE cmcsd_typ OCCURS 0 WITH HEADER LINE.
DATA: imcsdx TYPE mcsd_typ  OCCURS 0 WITH HEADER LINE.

** A&D IS-ADEC-SUB customer stock with vendor             "n_v_GA1551829
TYPES: BEGIN OF mscd_typ,
         werks LIKE mscd-werks,
*        LIFNR LIKE MSCD-LIFNR,                                         "2686185
         matnr LIKE mscd-matnr,
         sobkz LIKE mscd-sobkz,
         cdlab LIKE mscd-cdlab,
         cdins LIKE mscd-cdins,
         cdein LIKE mscd-cdein.
TYPES: END OF mscd_typ.

TYPES: BEGIN OF cmscd_typ.
    INCLUDE TYPE mscd_typ.
TYPES:   charg LIKE mscd-charg.
TYPES: END OF cmscd_typ.

** A&D IS-ADEC-SUB sales order stock with vendor
TYPES: BEGIN OF msfd_typ,
         werks LIKE msfd-werks,
*        LIFNR LIKE MSFD-LIFNR,                                         "2686185
         matnr LIKE msfd-matnr,
         sobkz LIKE msfd-sobkz,
         fdlab LIKE msfd-fdlab,
         fdins LIKE msfd-fdins,
         fdein LIKE msfd-fdein.
TYPES: END OF msfd_typ.

TYPES: BEGIN OF cmsfd_typ.
    INCLUDE TYPE msfd_typ.
TYPES:   charg LIKE msfd-charg.
TYPES: END OF cmsfd_typ.

** A&D IS-ADEC-SUB project stock with vendor
TYPES: BEGIN OF msrd_typ,
         werks LIKE msrd-werks,
*         LIFNR LIKE MSRD-LIFNR,                                        "2686185
         matnr LIKE msrd-matnr,
         sobkz LIKE msrd-sobkz,
         rdlab LIKE msrd-rdlab,
         rdins LIKE msrd-rdins,
         rdein LIKE msrd-rdein.
TYPES: END OF msrd_typ.

TYPES: BEGIN OF cmsrd_typ.
    INCLUDE TYPE msrd_typ.
TYPES:   charg LIKE msrd-charg.
TYPES: END OF cmsrd_typ.

** A&D IS-ADEC-SUB vendor consignment / RTP stock with vendor
TYPES: BEGIN OF msid_typ,
         werks LIKE msid-werks,
*        LIFNR LIKE MSID-LIFNR,                                         "2686185
         matnr LIKE msid-matnr,
         sobkz LIKE msid-sobkz,
         idlab LIKE msid-idlab,
         idins LIKE msid-idins,
         idein LIKE msid-idein.
TYPES: END OF msid_typ.

TYPES: BEGIN OF cmsid_typ.
    INCLUDE TYPE msid_typ.
TYPES:   charg LIKE msid-charg.
TYPES: END OF cmsid_typ.

** A&D IS-ADEC-SUB customer stock with vendor
DATA: xmscd  TYPE cmscd_typ OCCURS 0 WITH HEADER LINE.
DATA: imscd  TYPE cmscd_typ OCCURS 0 WITH HEADER LINE.
DATA: imscdx TYPE mscd_typ  OCCURS 0 WITH HEADER LINE.

** A&D IS-ADEC-SUB sales order stock with vendor
DATA: xmsfd  TYPE cmsfd_typ OCCURS 0 WITH HEADER LINE.
DATA: imsfd  TYPE cmsfd_typ OCCURS 0 WITH HEADER LINE.
DATA: imsfdx TYPE msfd_typ  OCCURS 0 WITH HEADER LINE.

** A&D IS-ADEC-SUB project stock with vendor
DATA: xmsrd  TYPE cmsrd_typ OCCURS 0 WITH HEADER LINE.
DATA: imsrd  TYPE cmsrd_typ OCCURS 0 WITH HEADER LINE.
DATA: imsrdx TYPE msrd_typ  OCCURS 0 WITH HEADER LINE.

** A&D IS-ADEC-SUB vendor consignment / RTP stock with vendor
DATA: xmsid  TYPE cmsid_typ OCCURS 0 WITH HEADER LINE.
DATA: imsid  TYPE cmsid_typ OCCURS 0 WITH HEADER LINE.
DATA: imsidx TYPE msid_typ  OCCURS 0 WITH HEADER LINE.

DATA: gv_bustr_tp TYPE t156-bustr.                        "^_v_GA1551829
"{ End ENHO DIPCS_RM07MLDD IS-AD-SSP AD_SUB }


*ENHANCEMENT-POINT rm07mldd_01 SPOTS es_rm07mlbd STATIC.

TYPES: BEGIN OF mkol_typ,
         werks LIKE mkol-werks,
         lgort LIKE mkol-lgort,
         matnr LIKE mkol-matnr,
         sobkz LIKE mkol-sobkz,
         slabs LIKE mkol-slabs,
         sinsm LIKE mkol-sinsm,
         seinm LIKE mkol-seinm,
         sspem LIKE mkol-sspem.
*
TYPES: /cwm/slabs LIKE mkol-/cwm/slabs,
       /cwm/sinsm LIKE mkol-/cwm/sinsm,
       /cwm/seinm LIKE mkol-/cwm/seinm,
       /cwm/sspem LIKE mkol-/cwm/sspem.
*ENHANCEMENT-POINT ehp605_rm07mldd_04 SPOTS es_rm07mlbd STATIC .
TYPES: END OF mkol_typ.

TYPES: BEGIN OF cmkol_typ.
    INCLUDE TYPE mkol_typ.
TYPES:   charg LIKE mkol-charg.
TYPES: END OF cmkol_typ.

TYPES: BEGIN OF mska_typ,
         werks LIKE mska-werks,
         lgort LIKE mska-lgort,
         matnr LIKE mska-matnr,
         sobkz LIKE mska-sobkz,
         kalab LIKE mska-kalab,
         kains LIKE mska-kains,
         kaspe LIKE mska-kaspe,
         kaein LIKE mska-kaein.
*
TYPES: /cwm/kalab LIKE mska-/cwm/kalab,
       /cwm/kains LIKE mska-/cwm/kains,
       /cwm/kaspe LIKE mska-/cwm/kaspe,
       /cwm/kaein LIKE mska-/cwm/kaein.
*ENHANCEMENT-POINT ehp605_rm07mldd_05 SPOTS es_rm07mlbd STATIC .
TYPES: END OF mska_typ.

TYPES: BEGIN OF cmska_typ.
    INCLUDE TYPE mska_typ.
TYPES:   charg LIKE mska-charg.
TYPES: END OF cmska_typ.

*------------------------- TABELLEN -----------------------------------*

TABLES: bkpf,                  "Buchhaltungsbelegkopf
        bsim,                 "Buchhaltungsbelege
        makt,                 "Materialkurztext
        mara,                 "allg. zum Material
        mard,                 "Materialbestände auf Lagerortebene
        mchb,                 "Chargenbestände auf Lagerortebene
        mcha,                                               "134317
        mbew,                 "Bewertungssegment
        ebew,                 "bewerteter Sonderbestand 'E'
        qbew,                 "bewerteter Sonderbestand 'Q'
        mkol,                 "Sonderbestand Lieferantenkonsignation
        mkpf,                 "Materialbelegköpfe
        mseg,                 "Materialbelege
        mska,                 "Auftragsbestand
        msku,                 "Sonderbestand Kundenkonsignation
        mslb,                 "Sonderbestand Lohnbearbeitung
        mspr,                 "Projektbestand
        rpgri,                "Texttabelle Gruppierung Bewegungsarten
        t001,                 "Prüftabelle Buchungskreise
        t001k,                "Prüftabelle Bewertungskreise
        t001w,                "Prüftabelle Werke
        t001l,                "Prüftabelle Lagerorte
        t134m,                "Prüftabelle Materialart
        t156m,                "Mengenstrings
        t156s,                "Bewegungsarten
        tcurm,                "Bewertungskreisebene
        bseg,
        acchd.

* for checking the FI summarization                        "n497992
TABLES : ttypv,     "customizing table FI summarization    "n497992
         ttypvx.    "customizing table FI summarization    "2197941

TABLES : sscrfields. "for the definition of pushbuttons    "n599218

*-------------------- DATENDEKLARATIONEN ------------------------------*

DATA: it001   TYPE imrep_t001_typ      OCCURS 0 WITH HEADER LINE.
DATA: it001k  TYPE imrep_t001k_typ     OCCURS 0 WITH HEADER LINE.
DATA: it001w  TYPE imrep_t001w_typ     OCCURS 0 WITH HEADER LINE.
DATA: it001l  TYPE imrep_t001l_typ     OCCURS 0 WITH HEADER LINE.
DATA: organ   TYPE imrep_organ_typ     OCCURS 0 WITH HEADER LINE.
DATA: header  TYPE imrep_matheader_typ OCCURS 0 WITH HEADER LINE.

*------------------------ Prüftabellen --------------------------------*

DATA: BEGIN OF it134m OCCURS 100,
        bwkey LIKE t134m-bwkey,
        mtart LIKE t134m-mtart,
        mengu LIKE t134m-mengu,
        wertu LIKE t134m-wertu,
      END OF it134m.

DATA: BEGIN OF it156 OCCURS 100,
        bwart LIKE t156s-bwart,
        wertu LIKE t156s-wertu,
        mengu LIKE t156s-mengu,
        sobkz LIKE t156s-sobkz,
        kzbew LIKE t156s-kzbew,
        kzzug LIKE t156s-kzzug,
        kzvbr LIKE t156s-kzvbr,
        bustm LIKE t156s-bustm,
        bustw LIKE t156s-bustw,                             "147374
        lbbsa LIKE t156m-lbbsa,
        bwagr LIKE t156s-bwagr,
      END OF it156.

DATA: BEGIN OF it156w OCCURS 100,                           "149448
        bustw LIKE t156w-bustw,                             "149448
        xbgbb LIKE t156w-xbgbb,                             "149448
      END OF it156w.                                        "149448

DATA: BEGIN OF it156x OCCURS 100,
        bustm LIKE t156s-bustm,
        lbbsa LIKE t156m-lbbsa,
      END OF it156x.

*--------------- übergeordnete Materialtabellen -----------------------*

* working table with material short texts / contains only   "n451923
* the necessary fields                                      "n451923
TYPES : BEGIN OF stype_makt,                                "n451923
          matnr LIKE      makt-matnr,                       "n451923
          maktx LIKE      makt-maktx,                       "n451923
        END OF stype_makt,                                  "n451923
                                                            "n451923
        stab_makt TYPE STANDARD TABLE OF                    "n451923
                             stype_makt WITH DEFAULT KEY.   "n451923
                                                            "n451923
DATA : g_s_makt TYPE  stype_makt,                           "n451923
       g_t_makt TYPE  stab_makt.                            "n451923

DATA: BEGIN OF imara OCCURS 100,
        matnr LIKE mara-matnr,
        meins LIKE mara-meins,
        mtart LIKE mara-mtart.
*
DATA: /cwm/valum  LIKE mara-/cwm/valum,
      /cwm/xcwmat LIKE mara-/cwm/xcwmat.
*ENHANCEMENT-POINT ehp605_rm07mldd_06 SPOTS es_rm07mlbd STATIC .
DATA: END OF imara.

* definition of working area for valuation tables improved  "n450764
TYPES : BEGIN OF stype_mbew,                                "n450764
          matnr     LIKE      mbew-matnr,                   "n450764
          bwkey     LIKE      mbew-bwkey,                   "n450764
          bwtar     LIKE      mbew-bwtar,                   "n450764
          lbkum(09) TYPE p    DECIMALS 3,                   "n450764
          salk3(09) TYPE p    DECIMALS 2,                   "n450764
          meins     LIKE      mara-meins,                   "n450764
          waers     LIKE      t001-waers,                   "n450764
          bwtty     LIKE      mbew-bwtty,                   "n1227439
        END OF stype_mbew,                                  "n450764
                                                            "n450764
        stab_mbew TYPE STANDARD TABLE OF                    "n450764
                             stype_mbew WITH DEFAULT KEY.   "n450764
                                                            "n450764
DATA: g_s_mbew TYPE  stype_mbew,                            "n450764
      g_t_mbew TYPE  stab_mbew.                             "n450764

DATA: BEGIN OF imcha OCCURS 100,                            "n1404822
        matnr LIKE mcha-matnr,                              "n1404822
        werks LIKE mcha-werks,                              "n1404822
        charg LIKE mcha-charg,                              "n1404822
      END OF imcha.

TYPES : BEGIN OF stype_accdet,
          mblnr     LIKE mseg-mblnr,
          mjahr     LIKE mseg-mjahr,
          zeile     LIKE mseg-zeile,
          matnr     LIKE mseg-matnr,
          werks     LIKE mseg-werks,
          bukrs     LIKE mseg-bukrs,
          ktopl     LIKE t001-ktopl,
          bwkey     LIKE t001w-bwkey,
          bwmod     LIKE t001k-bwmod,
          bwtar     LIKE mseg-bwtar,
          sobkz     LIKE mseg-sobkz,
          kzbws     LIKE mseg-kzbws,
          xobew     LIKE mseg-xobew,
          mat_pspnr LIKE mseg-mat_pspnr,
          mat_kdauf LIKE mseg-mat_kdauf,
          mat_kdpos LIKE mseg-mat_kdpos,
          lifnr     LIKE mseg-lifnr,
          bklas     LIKE mbew-bklas,
          hkont     LIKE bseg-hkont,
        END OF stype_accdet.

*--------------- Materialtabellen auf Lagerortebene -------------------*

DATA: BEGIN OF imard OCCURS 100,    "aktueller Materialbestand
        werks     LIKE mard-werks,      "Werk
        matnr     LIKE mard-matnr,      "Material
        lgort     LIKE mard-lgort,      "Lagerort
        labst     LIKE mard-labst,      "frei verwendbarer Bestand
        umlme     LIKE mard-umlme,      "Umlagerungsbestand
        insme     LIKE mard-insme,      "Qualitätsprüfbestand
        einme     LIKE mard-einme,      "nicht frei verwendbarer Bestand
        speme     LIKE mard-speme,      "gesperrter Bestand
        retme     LIKE mard-retme,      "gesperrter Bestand
        klabs     LIKE mard-klabs,      "frei verw. Konsignationsbestand
        lbkum     LIKE mbew-lbkum,      "bewerteter Bestand
        salk3(09) TYPE p    DECIMALS 2,                     "n497992
        waers     LIKE t001-waers.      "Währungseinheit
*
DATA: /cwm/labst LIKE mard-/cwm/labst,           "frei verwendbarer Bestand
      /cwm/umlme LIKE mard-/cwm/umlme,         "Umlagerungsbestand
      /cwm/insme LIKE mard-/cwm/insme,         "Qualitätsprüfbestand
      /cwm/einme LIKE mard-/cwm/einme,         "nicht frei verwendbarer Bestand
      /cwm/speme LIKE mard-/cwm/speme,         "gesperrter Bestand
      /cwm/retme LIKE mard-/cwm/retme,         "gesperrter Bestand
      /cwm/klabs LIKE mard-/cwm/klabs.         "frei verw. Konsignationsbestand
*ENHANCEMENT-POINT ehp605_rm07mldd_07 SPOTS es_rm07mlbd STATIC .
DATA: END OF imard.

DATA: BEGIN OF imchb OCCURS 100,    "aktueller Chargenbestand
        werks LIKE mchb-werks,
        matnr LIKE mchb-matnr,
        lgort LIKE mchb-lgort,
        charg LIKE mchb-charg,
        clabs LIKE mchb-clabs,      "frei verwendbarer Chargenbestand
        cumlm LIKE mchb-cumlm,      "Umlagerungsbestand
        cinsm LIKE mchb-cinsm,      "Qualitätsprüfbestand
        ceinm LIKE mchb-ceinm,      "nicht frei verwendbarer Bestand
        cspem LIKE mchb-cspem,      "gesperrter Bestand
        cretm LIKE mchb-cretm.      "gesperrter Bestand
*
DATA: /cwm/clabs LIKE mchb-/cwm/clabs,           "frei verwendbarer Chargenbestand
      /cwm/cumlm LIKE mchb-/cwm/cumlm,         "Umlagerungsbestand
      /cwm/cinsm LIKE mchb-/cwm/cinsm,         "Qualitätsprüfbestand
      /cwm/ceinm LIKE mchb-/cwm/ceinm,         "nicht frei verwendbarer Bestand
      /cwm/cspem LIKE mchb-/cwm/cspem,         "gesperrter Bestand
      /cwm/cretm LIKE mchb-/cwm/cretm.         "gesperrter Bestand
*ENHANCEMENT-POINT ehp605_rm07mldd_08 SPOTS es_rm07mlbd STATIC .
DATA: END OF imchb.

*-------------------------- Sonderbestände ----------------------------*

DATA: xmslb  TYPE cmslb_typ OCCURS 0 WITH HEADER LINE.
DATA: imslb  TYPE cmslb_typ OCCURS 0 WITH HEADER LINE.
DATA: imslbx TYPE mslb_typ  OCCURS 0 WITH HEADER LINE.

DATA: xmsku  TYPE cmsku_typ OCCURS 0 WITH HEADER LINE.
DATA: imsku  TYPE cmsku_typ OCCURS 0 WITH HEADER LINE.
DATA: imskux TYPE msku_typ  OCCURS 0 WITH HEADER LINE.

DATA: xmspr  TYPE cmspr_typ OCCURS 0 WITH HEADER LINE.
DATA: imspr  TYPE cmspr_typ OCCURS 0 WITH HEADER LINE.
DATA: imsprx TYPE mspr_typ  OCCURS 0 WITH HEADER LINE.

DATA: xmkol  TYPE cmkol_typ OCCURS 0 WITH HEADER LINE.
DATA: imkol  TYPE cmkol_typ OCCURS 0 WITH HEADER LINE.
DATA: imkolx TYPE mkol_typ  OCCURS 0 WITH HEADER LINE.

DATA: xmska  TYPE cmska_typ OCCURS 0 WITH HEADER LINE.
DATA: imska  TYPE cmska_typ OCCURS 0 WITH HEADER LINE.
DATA: imskax TYPE mska_typ  OCCURS 0 WITH HEADER LINE.

* global working table for the FI doc headers BKPF          "n856424
TYPES : BEGIN OF stype_bkpf,                                "n856424
          bukrs LIKE  bkpf-bukrs,                           "n856424
          belnr LIKE  bkpf-belnr,                           "n856424
          gjahr LIKE  bkpf-gjahr,                           "n856424
          blart LIKE  bkpf-blart,                           "n856424
          budat LIKE  bkpf-budat,                           "n856424
          awkey LIKE  bkpf-awkey,                           "n856424
          cpudt LIKE  bkpf-cpudt,                           "n856424
          cputm LIKE  bkpf-cputm,                           "n856424
          usnam LIKE  bkpf-usnam,                           "n856424
          awtyp LIKE  bkpf-awtyp,                           "n856424
        END OF stype_bkpf.                                  "n856424
                                                            "n856424
* global working table for the FI doc items BSEG
TYPES : BEGIN OF stype_bseg,
          bukrs LIKE  bseg-bukrs,
          belnr LIKE  bseg-belnr,
          gjahr LIKE  bseg-gjahr,
          buzei LIKE  bseg-buzei,
          hkont LIKE  bseg-hkont,
        END OF stype_bseg.

FIELD-SYMBOLS : <g_fs_bkpf>  TYPE  stype_bkpf.              "n856424
                                                            "n856424
DATA : g_t_bkpf    TYPE  HASHED TABLE OF stype_bkpf         "n856424
                         WITH UNIQUE KEY bukrs belnr gjahr. "n856424
DATA : g_t_bseg    TYPE  HASHED TABLE OF stype_bseg
                         WITH UNIQUE KEY bukrs belnr gjahr buzei.

*--------------- Summations- und Bestandstabellen ---------------------*

DATA: BEGIN OF bestand OCCURS 100,
        bwkey         LIKE mbew-bwkey,
        werks         LIKE mseg-werks,
        matnr         LIKE mseg-matnr,
        charg         LIKE mseg-charg,
*(DEL)  endmenge like mard-labst,          "Bestand zu 'datum-high' XJD
        endmenge(09)  TYPE p DECIMALS 3,    "Bestand zu 'datum-high' XJD
*(DEL)  anfmenge like mard-labst,          "Bestand zu 'datum-low'  XJD
        anfmenge(09)  TYPE p DECIMALS 3,   "Bestand zu 'datum-low'   XJD
        meins         LIKE mara-meins,             "Mengeneinheit
*       values at date-low and date-high                    "n497992
        endwert(09)   TYPE p    DECIMALS 2,                 "n497992
        anfwert(09)   TYPE p    DECIMALS 2,                 "n497992

*(DEL)  soll  like mseg-menge,                                     "XJD
        soll(09)      TYPE p DECIMALS 3,                                "XJD
*(DEL)  haben like mseg-menge,                                     "XJD
        haben(09)     TYPE p DECIMALS 3,                               "XJD
        sollwert(09)  TYPE p    DECIMALS 2,                 "n497992
        habenwert(09) TYPE p    DECIMALS 2,                 "n497992
        waers         LIKE t001-waers.             "Währungsschlüssel

"{ Begin ENHO AD_MPN_PUR2_RM07MLBD IS-AD-MPN-MD AD_MPN }

* DI A&D MPN IC
*DATA:
* sort_criteria(52) TYPE c.          " Used for mpn sorting

"{ End ENHO AD_MPN_PUR2_RM07MLBD IS-AD-MPN-MD AD_MPN }
*
DATA: /cwm/soll     TYPE /cwm/menge,
      /cwm/haben    TYPE /cwm/menge,
      /cwm/endmenge TYPE /cwm/menge,
      /cwm/anfmenge TYPE /cwm/menge,
      /cwm/meins    TYPE /cwm/meins.
*ENHANCEMENT-POINT rm07mldd_02 SPOTS es_rm07mlbd STATIC.
DATA:
      END OF bestand.

DATA: BEGIN OF bestand1 OCCURS 100,
        bwkey         LIKE mbew-bwkey,
        werks         LIKE mseg-werks,
        matnr         LIKE mseg-matnr,
        charg         LIKE mseg-charg,
*(DEL)  endmenge like mard-labst,          "Bestand zu 'datum-high' XJD
        endmenge(09)  TYPE p DECIMALS 3,    "Bestand zu 'datum-high' XJD
*(DEL)  anfmenge like mard-labst,          "Bestand zu 'datum-low'  XJD
        anfmenge(09)  TYPE p DECIMALS 3,    "Bestand zu 'datum-low'  XJD
        meins         LIKE mara-meins,             "Mengeneinheit
        endwert(09)   TYPE p    DECIMALS 2,                 "n497992
        anfwert(09)   TYPE p    DECIMALS 2,                 "n497992
*(DEL)  soll  like mseg-menge,                                     "XJD
        soll(09)      TYPE p DECIMALS 3,                                "XJD
*(DEL)  haben like mseg-menge,                                     "XJD
        haben(09)     TYPE p DECIMALS 3,                               "XJD
        sollwert(09)  TYPE p    DECIMALS 2,                 "n497992
        habenwert(09) TYPE p    DECIMALS 2,                 "n497992
        waers         LIKE t001-waers.             "Währungsschlüssel
*
DATA: /cwm/soll     TYPE /cwm/menge,
      /cwm/haben    TYPE /cwm/menge,
      /cwm/endmenge TYPE /cwm/menge,
      /cwm/anfmenge TYPE /cwm/menge,
      /cwm/meins    TYPE /cwm/meins.
*ENHANCEMENT-POINT ehp605_rm07mldd_09 SPOTS es_rm07mlbd STATIC .
DATA: END OF bestand1.

DATA: BEGIN OF sum_mat OCCURS 100,
        werks     LIKE mseg-werks,
        matnr     LIKE mseg-matnr,
        shkzg     LIKE mseg-shkzg,
        menge(09) TYPE p DECIMALS 3.                               "XJD
*
DATA:   /cwm/menge     TYPE    /cwm/menge.
*ENHANCEMENT-POINT ehp605_rm07mldd_10 SPOTS es_rm07mlbd STATIC .
DATA: END OF sum_mat.

TYPES: BEGIN OF ty_sum_char,                                "2296009
         werks     LIKE mseg-werks,
         matnr     LIKE mseg-matnr,
         charg     LIKE mseg-charg,
         shkzg     LIKE mseg-shkzg,
         menge(09) TYPE p DECIMALS 3.                               "XJD
TYPES:  /cwm/menge     TYPE    /cwm/menge.              "2296009/2302400
*ENHANCEMENT-POINT ehp605_rm07mldd_11 SPOTS es_rm07mlbd STATIC .
TYPES: END OF ty_sum_char.                                  "2296009

TYPES tty_sum_char TYPE SORTED TABLE OF ty_sum_char         "2296009
      WITH UNIQUE KEY werks matnr charg shkzg.              "2296009

DATA: sum_char TYPE tty_sum_char WITH HEADER LINE.          "2296009

DATA: BEGIN OF weg_mat OCCURS 100,
        werks     LIKE mseg-werks,
        lgort     LIKE mseg-lgort,                             " P30K140665
        matnr     LIKE mseg-matnr,
        shkzg     LIKE mseg-shkzg,
        menge(09) TYPE p DECIMALS 3.                               "XJD
*
DATA:   /cwm/menge     TYPE    /cwm/menge.
*ENHANCEMENT-POINT ehp605_rm07mldd_12 SPOTS es_rm07mlbd STATIC .
DATA: END OF weg_mat.

TYPES: BEGIN OF ty_weg_char,                                "2296009
         werks     LIKE mseg-werks,
         matnr     LIKE mseg-matnr,
         lgort     LIKE mseg-lgort,                             " P30K140665
         charg     LIKE mseg-charg,
         shkzg     LIKE mseg-shkzg,
         menge(09) TYPE p DECIMALS 3.                               "XJD
TYPES:  /cwm/menge     TYPE    /cwm/menge.              "2296009/2302400
*ENHANCEMENT-POINT ehp605_rm07mldd_13 SPOTS es_rm07mlbd STATIC .
TYPES: END OF ty_weg_char.                                  "2296009

TYPES tty_weg_char TYPE SORTED TABLE OF ty_weg_char         "2296009
      WITH UNIQUE KEY werks matnr lgort charg shkzg.        "2296009

DATA: weg_char TYPE tty_weg_char WITH HEADER LINE.          "2296009

DATA: BEGIN OF mat_sum OCCURS 100,
        bwkey     LIKE mbew-bwkey,
        werks     LIKE mseg-werks,
        matnr     LIKE mseg-matnr,
        shkzg     LIKE mseg-shkzg,
        menge(09) TYPE p DECIMALS 3,                               "XJD
        dmbtr(09) TYPE p    DECIMALS 3.                     "n497992
*
DATA:   /cwm/menge     TYPE    /cwm/menge.
*ENHANCEMENT-POINT ehp605_rm07mldd_14 SPOTS es_rm07mlbd STATIC .
DATA: END OF mat_sum.

DATA: BEGIN OF mat_sum_buk OCCURS 100,
        bwkey     LIKE mbew-bwkey,
        matnr     LIKE mseg-matnr,
        shkzg     LIKE mseg-shkzg,
        menge(09) TYPE p DECIMALS 3,                               "XJD
        dmbtr(09) TYPE p    DECIMALS 3.                     "n497992
*
DATA:   /cwm/menge     TYPE    /cwm/menge.
*ENHANCEMENT-POINT ehp605_rm07mldd_15 SPOTS es_rm07mlbd STATIC .
DATA: END OF mat_sum_buk.

DATA: BEGIN OF mat_weg OCCURS 100,
        bwkey     LIKE mbew-bwkey,
        werks     LIKE mseg-werks,
        matnr     LIKE mseg-matnr,
        shkzg     LIKE mseg-shkzg,
        menge(09) TYPE p DECIMALS 3,                               "XJD
        dmbtr(09) TYPE p    DECIMALS 3.                     "n497992
*
DATA:   /cwm/menge     TYPE    /cwm/menge.
*ENHANCEMENT-POINT ehp605_rm07mldd_16 SPOTS es_rm07mlbd STATIC .
DATA: END OF mat_weg.

DATA: BEGIN OF mat_weg_buk OCCURS 100,
        bwkey     LIKE mbew-bwkey,
        matnr     LIKE mseg-matnr,
        shkzg     LIKE mseg-shkzg,
        menge(09) TYPE p DECIMALS 3,                               "XJD
        dmbtr(09) TYPE p    DECIMALS 3.                     "n497992
*
DATA:   /cwm/menge     TYPE    /cwm/menge.
*ENHANCEMENT-POINT ehp605_rm07mldd_17 SPOTS es_rm07mlbd STATIC .
DATA: END OF mat_weg_buk.

*----------------------- Feldleisten ----------------------------------*

DATA: BEGIN OF leiste,
        werks LIKE mseg-werks,
        bwkey LIKE mbew-bwkey,
        matnr LIKE mseg-matnr,
        charg LIKE mseg-charg,
      END OF leiste.

*------------------------ Hilfsfelder ---------------------------------*

DATA: curm          LIKE tcurm-bwkrs_cus,
      bukr          LIKE t001-bukrs,
      bwkr          LIKE t001k-bwkey,
      werk          LIKE t001w-werks,
      name          LIKE t001w-name1,
      lort          LIKE t001l-lgort,
      waer          LIKE t001-waers,
      index_0       LIKE sy-tabix,
      index_1       LIKE sy-tabix,
      index_2       LIKE sy-tabix,
      index_3       LIKE sy-tabix,
      index_4       LIKE sy-tabix,
      aktdat        LIKE sy-datlo,
      sortfield(30),
      material(30),

      new_bwagr     LIKE t156s-bwagr,
      old_bwagr     LIKE t156s-bwagr,
      leer(1)       TYPE c,
      counter       LIKE sy-tabix,
      inhalt(10)    TYPE n.

DATA: jahrlow(4)   TYPE c,
      monatlow(2)  TYPE c,
      taglow(2)    TYPE c,
      jahrhigh(4)  TYPE c,
      monathigh(2) TYPE c,
      taghigh(2)   TYPE c.

* zur Berechtigungsprüfung:
DATA actvt03 LIKE tact-actvt VALUE '03'.         "anzeigen

*-------------------- FELDER FÜR LISTVIEWER ---------------------------*

DATA: repid      LIKE sy-repid.
DATA: fieldcat   TYPE slis_t_fieldcat_alv.
DATA: xheader    TYPE slis_t_listheader WITH HEADER LINE.
DATA: keyinfo    TYPE slis_keyinfo_alv.
DATA: color      TYPE slis_t_specialcol_alv WITH HEADER LINE.
DATA: layout     TYPE slis_layout_alv.
DATA: events     TYPE slis_t_event WITH HEADER LINE.
DATA: event_exit TYPE slis_t_event_exit WITH HEADER LINE.
DATA: sorttab    TYPE slis_t_sortinfo_alv WITH HEADER LINE.
DATA: filttab    TYPE slis_t_filter_alv WITH HEADER LINE.
*data: extab      type slis_t_extab with header line.              "XJD

* Listanzeigevarianten
DATA: variante        LIKE disvariant,                " Anzeigevariante
      def_variante    LIKE disvariant,                " Defaultvariante
      variant_exit(1) TYPE c,
      variant_save(1) TYPE c,
      variant_def(1)  TYPE c.

* save the name of the default display variant              "n599218
DATA: alv_default_variant    LIKE  disvariant-variant.      "n599218

* Gruppen Positionsfelder
DATA: gruppen TYPE slis_t_sp_group_alv WITH HEADER LINE.

* structure for print ALV print parameters
DATA: g_s_print              TYPE      slis_print_alv.

*----------------------------------------------------------------------*

* working area : get the field names of a structure
TYPE-POOLS : sydes.

DATA : g_t_td       TYPE      sydes_desc,
       g_s_typeinfo TYPE      sydes_typeinfo,
       g_s_nameinfo TYPE      sydes_nameinfo.

* new definitions for table organ
TYPES : BEGIN OF stype_organ,
          keytype(01) TYPE c,
          keyfield    LIKE      t001w-werks,
          bwkey       LIKE      t001k-bwkey,
          werks       LIKE      t001w-werks,
          bukrs       LIKE      t001-bukrs,
          ktopl       LIKE      t001-ktopl,
          bwmod       LIKE      t001k-bwmod,
          waers       LIKE      t001-waers,
        END OF stype_organ,

        stab_organ TYPE STANDARD TABLE OF stype_organ
                             WITH KEY keytype keyfield bwkey werks.

DATA : g_s_organ TYPE      stype_organ,
       g_t_organ TYPE      stab_organ
                                       WITH HEADER LINE.

* buffer table for check authority for plants
TYPES : BEGIN OF stype_auth_plant,
          werks  LIKE      mseg-werks,
          ok(01) TYPE c,
        END OF stype_auth_plant.

TYPES : stab_auth_plant      TYPE STANDARD TABLE OF
                             stype_auth_plant WITH KEY werks.
DATA : g_t_auth_plant        TYPE stab_auth_plant WITH HEADER LINE.

* for the assignment of the MM and FI documents             "n443935
TYPES : BEGIN OF stype_bsim_lean,                           "n443935
          bukrs    LIKE bkpf-bukrs,                         "n443935
          bwkey    LIKE bsim-bwkey,                         "n443935
          matnr    LIKE bsim-matnr,                         "n443935
          bwtar    LIKE bsim-bwtar,                         "n443935
          shkzg    LIKE bsim-shkzg,                         "n443935
          meins    LIKE bsim-meins,                         "n443935
          budat    LIKE bsim-budat,                         "n443935
          blart    LIKE bsim-blart,                         "n443935
          buzei    LIKE      bsim-buzei,                    "n497992
                                                            "n443935
          awkey    LIKE bkpf-awkey,                         "n443935
          belnr    LIKE bsim-belnr,                         "n443935
          gjahr    LIKE bsim-gjahr,                         "n443935
          menge    LIKE bsim-menge,                         "n443935
          dmbtr    LIKE bsim-dmbtr,                         "n443935
          accessed TYPE c,                                  "n443935
          tabix    LIKE  sy-tabix,                          "n443935
        END OF stype_bsim_lean,                             "n443935
                                                            "n443935
        stab_bsim_lean TYPE STANDARD TABLE OF               "n443935
                             stype_bsim_lean                "n443935
                             WITH DEFAULT KEY.              "n443935

DATA : g_t_bsim_lean TYPE  stab_bsim_lean,                  "n443935
       g_s_bsim_lean TYPE  stype_bsim_lean,                 "n443935
       g_t_bsim_work TYPE  stab_bsim_lean,                  "n443935
       g_s_bsim_work TYPE  stype_bsim_lean.                 "n443935

* for the control break                                     "n443935
TYPES : BEGIN OF stype_mseg_group,                          "n443935
          mblnr LIKE  mkpf-mblnr,                           "n443935
          mjahr LIKE  mkpf-mjahr,                           "n443935
          bukrs LIKE  bkpf-bukrs,                           "n443935
          bwkey LIKE  bsim-bwkey,                           "n443935
          matnr LIKE  mseg-matnr,                           "n443935
          bwtar LIKE  mseg-bwtar,                           "n443935
          shkzg LIKE  mseg-shkzg,                           "n443935
          meins LIKE  mseg-meins,                           "n443935
          budat LIKE  mkpf-budat,                           "n443935
          blart LIKE  mkpf-blart,                           "n443935
        END OF stype_mseg_group.                            "n443935
                                                            "n443935
DATA : g_s_mseg_old TYPE  stype_mseg_group,                 "n443935
       g_s_mseg_new TYPE  stype_mseg_group.                 "n443935
                                                            "n443935
* Structure to separate AWKEY into MBLNR/MJAHR in a         "n443935
* clean way.                                                "n443935
DATA: BEGIN OF matkey,                                      "n443935
        mblnr LIKE mkpf-mblnr,                              "n443935
        mjahr LIKE mkpf-mjahr,                              "n443935
      END OF matkey.                                        "n443935

* global contants
CONSTANTS : c_space(01)      TYPE c    VALUE ' ',
            c_bwkey(01)      TYPE c    VALUE 'B',
            c_error(01)      TYPE c    VALUE 'E',
            c_no_error(01)   TYPE c    VALUE 'N',
            c_werks(01)      TYPE c    VALUE 'W',
            c_no_space(01)   TYPE c    VALUE 'N',
            c_space_only(01) TYPE c    VALUE 'S',
            c_tilde(01)      TYPE c    VALUE '~',
            c_check(01)      TYPE c    VALUE 'C',
            c_take(01)       TYPE c    VALUE 'T',
            c_out(01)        TYPE c    VALUE c_space,
            c_no_out(01)     TYPE c    VALUE 'X'.

* for the use of the pushbutton                             "n599218
CONSTANTS : c_show(01) TYPE c    VALUE 'S',                 "n599218
            c_hide(01) TYPE c    VALUE 'H'.                 "n599218

* for the field catalog
DATA : g_s_fieldcat TYPE      slis_fieldcat_alv,
       g_f_tabname  TYPE      slis_tabname,
       g_f_col_pos  TYPE i.

* global used fields
DATA : g_flag_delete(01)    TYPE c,
       g_flag_authority(01) TYPE c,
       g_f_cnt_lines        TYPE i,
       g_f_cnt_lines_bukrs  TYPE i,
       g_f_cnt_lines_werks  TYPE i,
       g_f_cnt_before       TYPE i,
       g_f_cnt_after        TYPE i.

* working fields for the headlines and page numbers         "n599218
DATA : g_f_cnt_bestand_total TYPE i,                        "n599218
       g_f_cnt_bestand_curr  TYPE i.                        "n599218
                                                            "n599218
DATA : BEGIN OF g_s_header_77,                              "n599218
         date(10)      TYPE c,                              "n599218
         filler_01(01) TYPE c,                              "n599218
         title(59)     TYPE c,                              "n599218
         filler_02(01) TYPE c,                              "n599218
         page(06)      TYPE c,                              "n599218
       END OF g_s_header_77,                                "n599218
                                                            "n599218
       BEGIN OF g_s_header_91,                              "n599218
         date(10)      TYPE c,                              "n599218
         filler_01(01) TYPE c,                              "n599218
         title(73)     TYPE c,                              "n599218
         filler_02(01) TYPE c,                              "n599218
         page(06)      TYPE c,                              "n599218
       END OF g_s_header_91.                                "n599218
                                                            "n599218
DATA : g_end_line_77(77) TYPE c,                            "n599218
       g_end_line_91(91) TYPE c.                            "n599218
                                                            "n599218
* for the scope of list                                     "n599218
DATA : g_cnt_empty_parameter TYPE i.                        "n599218
DATA : g_flag_status_liu(01) TYPE c    VALUE 'H'.           "n599218
                                                            "n599218
* flag to be set when INITIALIZATION was processed          "n599218
DATA g_flag_initialization(01) TYPE c.                      "n599218
                                                            "n599218
* flag for activate ALV ivterface check                     "n599218
DATA g_flag_i_check(01)      TYPE c.                        "n599218

DATA : g_f_bwkey          LIKE  mbew-bwkey,                 "n443935
       g_f_tabix          LIKE  sy-tabix,                   "n443935
       g_f_tabix_start    LIKE  sy-tabix,                   "n443935
       g_cnt_loop         LIKE  sy-tabix,                   "n443935
       g_cnt_mseg_entries LIKE  sy-tabix,                   "n443935
       g_cnt_bsim_entries LIKE  sy-tabix,                   "n443935
       g_cnt_mseg_done    LIKE  sy-tabix.                   "n443935

* for the processing of tied empties                        "n497992
DATA : g_f_werks_retail      LIKE      t001w-werks.         "n497992

* reference procedures for checking FI summarization        "n497992
RANGES : g_ra_awtyp          FOR  ttypv-awtyp,              "n497992
         g_ra_bukrs          FOR  ttypvx-bukrs.             "2197941

* global range tables for the database selection
RANGES : g_ra_bwkey          FOR t001k-bwkey,    "valuation area
         g_ra_werks          FOR t001w-werks,    "plant
         g_ra_sobkz          FOR mseg-sobkz,     "special st. ind.
         g_ra_lgort          FOR mseg-lgort.     "storage location

* global range tables for the creation of table g_t_organ
RANGES : g_0000_ra_bwkey     FOR t001k-bwkey,    "valuation area
         g_0000_ra_werks     FOR t001w-werks,    "plant
         g_0000_ra_bukrs     FOR t001-bukrs.     "company code

* internal range for valuation class restriction
RANGES : ibklas     FOR mbew-bklas.

* global table with the material numbers as key for reading MAKT
TYPES : BEGIN OF stype_mat_key,
          matnr LIKE      mara-matnr,
        END OF   stype_mat_key.

TYPES : stab_mat_key         TYPE STANDARD TABLE OF stype_mat_key
                             WITH KEY matnr.

DATA: g_t_mat_key            TYPE      stab_mat_key
                             WITH HEADER LINE.

* global table with the key for the FI documents BKPF
TYPES : BEGIN OF stype_bkpf_key,
          bukrs LIKE      bkpf-bukrs,
          belnr LIKE      bkpf-belnr,
          gjahr LIKE      bkpf-gjahr,
        END OF   stype_bkpf_key.

* global table with the key for the FI documents BSEG
TYPES : BEGIN OF stype_bseg_key,
          bukrs LIKE      bseg-bukrs,
          belnr LIKE      bseg-belnr,
          gjahr LIKE      bseg-gjahr,
          buzei LIKE      bseg-buzei,
        END OF   stype_bseg_key.

TYPES : stab_bkpf_key        TYPE STANDARD TABLE OF stype_bkpf_key
                             WITH KEY bukrs belnr gjahr.
TYPES : stab_bseg_key        TYPE STANDARD TABLE OF stype_bseg_key
                             WITH KEY bukrs belnr gjahr buzei.

DATA: g_t_bkpf_key           TYPE      stab_bkpf_key
                             WITH HEADER LINE.
DATA: g_t_bseg_key           TYPE      stab_bseg_key
                             WITH HEADER LINE.

* separate time depending authorization for tax auditor     "n486477
* define working areas for time depending authority check   "n486477
DATA : g_f_budat      LIKE      bsim-budat,                 "n486477
       g_f_budat_work LIKE      bsim-budat.                 "n486477
                                                            "n486477
TYPES : BEGIN OF stype_bukrs,                               "n486477
          bukrs LIKE      t001-bukrs,                       "n486477
        END OF stype_bukrs,                                 "n486477
                                                            "n486477
        stab_bukrs TYPE STANDARD TABLE OF                   "n486477
                             stype_bukrs WITH DEFAULT KEY.  "n486477
                                                            "n486477
DATA : g_t_bukrs TYPE  stab_bukrs,                          "n486477
       g_s_bukrs TYPE  stype_bukrs.                         "n486477
                                                            "n486477
TYPES : BEGIN OF stype_work,                                "n486477
          werks LIKE      t001w-werks,                      "n486477
          bwkey LIKE      t001k-bwkey,                      "n486477
          bukrs LIKE      t001-bukrs,                       "n486477
        END OF stype_work.                                  "n486477
                                                            "n486477
DATA : g_s_t001w TYPE  stype_work,                          "n486477
       g_t_t001w TYPE  stype_work   OCCURS 0,               "n486477
       g_s_t001k TYPE  stype_work,                          "n486477
       g_t_t001k TYPE  stype_work   OCCURS 0.               "n486477
                                                            "n486477
DATA : g_flag_tpcuser(01) TYPE c,                           "n486477
*      1 = carry out the special checks for this user       "n486477
       g_f_repid          LIKE  sy-repid.                   "n497992

* for the representation of tied empties                    "n547170
* range table for special indicators of field MSEG-XAUTO    "n547170
RANGES : g_ra_xauto          FOR  mseg-xauto.               "n547170
                                                            "n547170
DATA   : g_f_zeile           LIKE  mseg-zeile.              "n547170
                                                            "n547170
TYPES : BEGIN OF stype_mseg_xauto,                          "n547170
          mblnr LIKE  mseg-mblnr,                           "n547170
          mjahr LIKE  mseg-mjahr,                           "n547170
          zeile LIKE  mseg-zeile,                           "n547170
          matnr LIKE  mseg-matnr,                           "n547170
          xauto LIKE  mseg-xauto,                           "n547170
        END OF stype_mseg_xauto,                            "n547170
                                                            "n547170
        stab_mseg_xauto TYPE STANDARD TABLE OF              "n547170
                             stype_mseg_xauto               "n547170
                             WITH DEFAULT KEY.              "n547170
                                                            "n547170
* working area for the previous entry                       "n547170
DATA : g_s_mseg_pr  TYPE  stype_mseg_xauto,                 "n547170
                                                            "n547170
* table for the original MM doc posting lines               "n547170
       g_s_mseg_or  TYPE  stype_mseg_xauto,                 "n547170
       g_t_mseg_or  TYPE  stab_mseg_xauto,                  "n547170
                                                            "n547170
* table for the keys of the original MM doc lines           "n547170
       g_s_mseg_key TYPE  stype_mseg_xauto,                 "n547170
       g_t_mseg_key TYPE  stab_mseg_xauto.                  "n547170

*----------------------------------------------------------------------*
* new data definitions
*----------------------------------------------------------------------*

*   for the selection of the reversal movements only in release >=45B
DATA: BEGIN OF storno OCCURS 0,
        mblnr LIKE mseg-mblnr,
        mjahr LIKE mseg-mjahr,
        zeile LIKE mseg-zeile,
        smbln LIKE mseg-smbln,
        sjahr LIKE mseg-sjahr,
        smblp LIKE mseg-smblp,
      END OF storno.

* working fields for reading structures from DDIC           "n599218 A
* and check whether IS-OIL is active                        "n599218 A
TYPES : stab_x031l           TYPE STANDARD TABLE OF x031l   "n599218 A
                             WITH DEFAULT KEY.              "n599218 A
                                                            "n599218 A
DATA : g_s_x031l TYPE x031l,                                "n599218 A
       g_t_x031l TYPE stab_x031l.                           "n599218 A
                                                            "n599218 A
DATA : g_f_dcobjdef_name        LIKE dcobjdef-name,         "n599218 A
       g_flag_is_oil_active(01) TYPE c,                     "n599218 A
       g_cnt_is_oil             TYPE i.                     "n599218 A

DATA : g_flag_found(01)      TYPE c.

DATA : g_f_butxt          LIKE  t001-butxt,
       g_f_tabname_totals LIKE dcobjdef-name,
       g_f_tabname_belege LIKE dcobjdef-name.

DATA : BEGIN OF g_save_params,
         werks LIKE  mseg-werks,
         matnr LIKE  mseg-matnr,
         charg LIKE  mseg-charg,
         belnr LIKE  bseg-belnr,
         bukrs LIKE  bseg-bukrs,
         gjahr LIKE  bseg-gjahr,
       END OF g_save_params.

DATA: g_t_events_totals_flat TYPE slis_t_event WITH HEADER LINE.
DATA: events_hierseq         TYPE slis_t_event WITH HEADER LINE.

DATA: g_t_fieldcat_totals_hq   TYPE slis_t_fieldcat_alv,
      g_t_fieldcat_totals_flat TYPE slis_t_fieldcat_alv.

DATA: fieldcat_hierseq       TYPE slis_t_fieldcat_alv.

DATA: g_s_keyinfo_totals_hq  TYPE slis_keyinfo_alv.

DATA: g_s_sorttab TYPE slis_sortinfo_alv,
      g_t_sorttab TYPE slis_t_sortinfo_alv.

DATA: g_s_sort_totals_hq TYPE slis_sortinfo_alv,
      g_t_sort_totals_hq TYPE slis_t_sortinfo_alv.

DATA: g_s_vari_sumhq     LIKE disvariant,
      g_s_vari_sumhq_def LIKE disvariant,
      g_s_vari_sumfl     LIKE disvariant,
      g_s_vari_sumfl_def LIKE disvariant.

* contains the a structure with the max. number of fields of
* the database table MSEG, but those lines are comment lines
* with a '*'. The customer can achtivate those lines.
* The activated fields will be selected from the database table
* and are hidden in the list. With the settings in the display
* variant the can be shown.
INCLUDE                      zrm07mlbd_cust_fields.

* common types structure for working tables
* a) g_t_mseg_lean   results form database selection
* b) g_t_beleg       data table for ALV
TYPES : BEGIN OF stype_mseg_lean,
          mblnr        LIKE      mkpf-mblnr,
          mjahr        LIKE      mkpf-mjahr,
          vgart        LIKE      mkpf-vgart,
          blart        LIKE      mkpf-blart,
          budat        LIKE      mkpf-budat,
          cpudt        LIKE      mkpf-cpudt,
          cputm        LIKE      mkpf-cputm,
          usnam        LIKE      mkpf-usnam,
* process 'goods receipt/issue slip' as hidden field        "n450596
          xabln        LIKE      mkpf-xabln,                "n450596

          lbbsa        LIKE      t156m-lbbsa,
          bwagr        LIKE      t156s-bwagr,
          bukrs        LIKE      t001-bukrs,

          belnr        LIKE      bkpf-belnr,
          gjahr        LIKE      bkpf-gjahr,
          buzei        LIKE      bseg-buzei,
          hkont        LIKE      bseg-hkont,

          waers        LIKE      mseg-waers,
          zeile        LIKE      mseg-zeile,
          bwart        LIKE      mseg-bwart,
          matnr        LIKE      mseg-matnr,
          werks        LIKE      mseg-werks,
          lgort        LIKE      mseg-lgort,
          charg        LIKE      mseg-charg,
          bwtar        LIKE      mseg-bwtar,
          kzvbr        LIKE      mseg-kzvbr,
          kzbew        LIKE      mseg-kzbew,
          sobkz        LIKE      mseg-sobkz,
          kzzug        LIKE      mseg-kzzug,
          bustm        LIKE      mseg-bustm,
          bustw        LIKE      mseg-bustw,
          mengu        LIKE      mseg-mengu,
          wertu        LIKE      mseg-wertu,
          shkzg        LIKE      mseg-shkzg,
          menge        LIKE      mseg-menge,
          meins        LIKE      mseg-meins,
          dmbtr        LIKE      mseg-dmbtr,
          dmbum        LIKE      mseg-dmbum,
          xauto        LIKE      mseg-xauto,
          kzbws        LIKE      mseg-kzbws,
          xobew        LIKE      mseg-xobew,
          sgt_scat     LIKE      mseg-sgt_scat,
*          special flag for retail                          "n497992
          retail(01)   TYPE c,                              "n497992

* define the fields for the IO-OIL specific functions       "n599218 A
*          mseg-oiglcalc     CHAR          1                "n599218 A
*          mseg-oiglsku      QUAN         13                "n599218 A
          oiglcalc(01) TYPE  c,                             "n599218 A
          oiglsku(07)  TYPE  p  DECIMALS 3,                 "n599218 A
          insmk        LIKE      mseg-insmk,                "n599218 A

* the following fields are used for the selection of
* the reversal movements
          smbln        LIKE      mseg-smbln,    " No. doc
          sjahr        LIKE      mseg-sjahr,    " Year          "n497992
          smblp        LIKE      mseg-smblp.    " Item in doc
*
TYPES : /cwm/menge LIKE mseg-/cwm/menge,
        /cwm/meins LIKE      mseg-/cwm/meins,
        /cwm/valum LIKE      mara-/cwm/valum.
*ENHANCEMENT-POINT ehp605_rm07mldd_18 SPOTS es_rm07mlbd STATIC .
* additional fields : the user has the possibility to activate
* these fields in the following include report
    INCLUDE           TYPE      stype_mb5b_add.
TYPES : END OF stype_mseg_lean.

TYPES: stab_mseg_lean        TYPE STANDARD TABLE OF stype_mseg_lean
                             WITH KEY mblnr mjahr.

TYPES : BEGIN OF stype_bestand_key,
          matnr LIKE  mseg-matnr,
          werks LIKE  mseg-werks,
          bwkey LIKE  mbew-bwkey,
          charg LIKE  mseg-charg,
        END OF stype_bestand_key.

DATA : g_s_bestand_key       TYPE  stype_bestand_key.

* data tables with the results for the ALV
TYPES : BEGIN OF stype_belege,

          sel,  "Campo de selecao de linha da ALV

          bwkey LIKE      mbew-bwkey.
    INCLUDE            TYPE      stype_mseg_lean.
TYPES : farbe_pro_feld      TYPE slis_t_specialcol_alv,
        farbe_pro_zeile(03) TYPE c.
TYPES : END OF stype_belege.

TYPES : stab_belege          TYPE STANDARD TABLE OF stype_belege
                             WITH KEY  budat mblnr zeile.

DATA : g_t_belege    TYPE   stab_belege WITH HEADER LINE,
       g_t_belege1   TYPE   stab_belege WITH HEADER LINE,
       g_t_belege_uc TYPE   stab_belege WITH HEADER LINE.

TYPES ty_stab_belege LIKE LINE OF g_t_belege1[].

* new output tables for to list in total mode
TYPES : BEGIN OF stype_totals_header,
          bwkey LIKE      mbew-bwkey,
          werks LIKE      mseg-werks,
          matnr LIKE      mbew-matnr,
          charg LIKE      mseg-charg,
          sobkz LIKE      mslb-sobkz,

          name1 LIKE      t001w-name1,
          maktx LIKE      makt-maktx,
        END OF stype_totals_header.

TYPES: BEGIN OF stype_totals_item,
         bwkey          LIKE      mbew-bwkey,
         werks          LIKE      mseg-werks,
         matnr          LIKE      mbew-matnr,
         charg          LIKE      mseg-charg,

         counter        TYPE  i,
         stock_type(40) TYPE  c,
         menge(09)      TYPE  p   DECIMALS 3,
         meins          LIKE      mara-meins,
         wert(09)       TYPE  p   DECIMALS 2.
*
TYPES: /cwm/menge LIKE mseg-/cwm/menge,
       /cwm/meins LIKE      mseg-/cwm/meins.
*ENHANCEMENT-POINT ehp605_rm07mldd_19 SPOTS es_rm07mlbd STATIC .
TYPES: waers LIKE t001-waers,                "Währungsschlüssel
       color TYPE      slis_t_specialcol_alv,
       END OF stype_totals_item.

TYPES:  stab_totals_header TYPE STANDARD TABLE OF
                             stype_totals_header
                             WITH DEFAULT KEY,

        stab_totals_item   TYPE STANDARD TABLE OF
                             stype_totals_item
                             WITH DEFAULT KEY.

DATA : g_s_totals_header TYPE stype_totals_header,
       g_t_totals_header TYPE stab_totals_header.

DATA : g_s_totals_item TYPE stype_totals_item,
       g_t_totals_item TYPE stab_totals_item.

* new output table for flat list in total mode
TYPES : BEGIN OF stype_totals_flat,
          matnr        LIKE      mbew-matnr,
          maktx        LIKE      makt-maktx,
          bwkey        LIKE      mbew-bwkey,
          werks        LIKE      mseg-werks,
          charg        LIKE      mseg-charg,
          sobkz        LIKE      mslb-sobkz,
          name1        LIKE      t001w-name1,               "n999530

          start_date   LIKE      sy-datlo,
          end_date     LIKE      sy-datlo,

          anfmenge(09) TYPE p    DECIMALS 3,
          meins        LIKE      mara-meins,
          soll(09)     TYPE p DECIMALS 3,
          haben(09)    TYPE p DECIMALS 3,
          endmenge(09) TYPE p DECIMALS 3.
*
TYPES: /cwm/anfmenge LIKE mseg-/cwm/menge,
       /cwm/meins    LIKE      mseg-/cwm/meins,
       /cwm/soll     LIKE      mseg-/cwm/menge,
       /cwm/haben    LIKE      mseg-/cwm/menge,
       /cwm/endmenge LIKE      mseg-/cwm/menge.
*ENHANCEMENT-POINT ehp605_rm07mldd_20 SPOTS es_rm07mlbd STATIC .
TYPES: anfwert(09)   TYPE p DECIMALS 2,
       waers         LIKE t001-waers,             "Währungsschlüssel
       sollwert(09)  TYPE p    DECIMALS 2,
       habenwert(09) TYPE p    DECIMALS 2,
       endwert(09)   TYPE p    DECIMALS 2,
       color         TYPE      slis_t_specialcol_alv,
       END OF stype_totals_flat,

       stab_totals_flat TYPE STANDARD TABLE OF stype_totals_flat
                   WITH DEFAULT KEY.

DATA : g_s_totals_flat TYPE  stype_totals_flat,
       g_t_totals_flat TYPE  stab_totals_flat.

* for the colorizing of the numeric fields
DATA : g_s_color TYPE  slis_specialcol_alv,
       g_t_color TYPE  slis_t_specialcol_alv.

DATA : g_s_layout_totals_hq   TYPE slis_layout_alv,
       g_s_layout_totals_flat TYPE slis_layout_alv.

DATA : g_f_length     TYPE i,
       g_f_length_max TYPE i.

DATA : g_offset_header TYPE i,
       g_offset_qty    TYPE i,
       g_offset_unit   TYPE i,
       g_offset_value  TYPE i,
       g_offset_curr   TYPE i.

TYPES : BEGIN OF stype_date_line,
          text(133) TYPE c,
          datum(10) TYPE c,
        END OF stype_date_line.

DATA : g_date_line_from TYPE  stype_date_line,
       g_date_line_to   TYPE  stype_date_line.

DATA : BEGIN OF g_text_line,
         filler(02) TYPE  c,
         text(133)  TYPE  c,
       END OF g_text_line.

* interface structure for new TOP_OF_PAGE and the detail list
TYPES : BEGIN OF stype_bestand.
    INCLUDE STRUCTURE  bestand.
TYPES : END OF stype_bestand.

TYPES : stab_bestand         TYPE STANDARD TABLE OF stype_bestand
                             WITH DEFAULT KEY.

DATA : g_s_bestand        TYPE  stype_bestand,
       g_s_bestand_detail TYPE  stype_bestand,
       g_t_bestand_detail TYPE  stab_bestand.

DATA : l_f_meins_external       TYPE  mara-meins.           "n1018717


* global working areas data from MSEG and MKPF
FIELD-SYMBOLS : <g_fs_mseg_lean>       TYPE stype_mseg_lean.
DATA : g_s_mseg_lean   TYPE stype_mseg_lean,
       g_s_mseg_update TYPE stype_mseg_lean,                "n443935
       g_t_mseg_lean   TYPE stab_mseg_lean.

* working table for the control break                       "n451923
TYPES : BEGIN OF stype_mseg_work.                           "n451923
    INCLUDE            TYPE      stype_mseg_lean.           "n451923
TYPES : tabix LIKE sy-tabix,                                "n451923
        END OF stype_mseg_work,                             "n451923
                                                            "n451923
        stab_mseg_work TYPE STANDARD TABLE OF               "n451923
                             stype_mseg_work                "n451923
                             WITH DEFAULT KEY.              "n451923
                                                            "n451923
DATA : g_t_mseg_work TYPE  stab_mseg_work,                  "n443935
       g_s_mseg_work TYPE  stype_mseg_work.                 "n443935

* working table for the requested field name from MSEG and MKPF
TYPES: BEGIN OF stype_fields,
         fieldname TYPE      name_feld,
       END OF stype_fields.

TYPES: stab_fields           TYPE STANDARD TABLE OF stype_fields
                             WITH KEY fieldname.

DATA: g_t_mseg_fields TYPE      stab_fields,
      g_s_mseg_fields TYPE      stype_fields.

* working table for the requested numeric fields of MSEG
TYPES : BEGIN OF stype_color_fields,
          fieldname TYPE      name_feld,
          type(01)  TYPE c,
        END OF stype_color_fields,

        stab_color_fields TYPE STANDARD TABLE OF
                    stype_color_fields
                    WITH DEFAULT KEY.

DATA: g_t_color_fields       TYPE      stab_color_fields
                             WITH HEADER LINE.

DATA: BEGIN OF imsweg OCCURS 1000,
        mblnr        LIKE mseg-mblnr,
        mjahr        LIKE mseg-mjahr,
        zeile        LIKE mseg-zeile,
        matnr        LIKE mseg-matnr,
        charg        LIKE mseg-charg,
        bwtar        LIKE mseg-bwtar,
        werks        LIKE mseg-werks,
        lgort        LIKE mseg-lgort,
        sobkz        LIKE mseg-sobkz,
        bwart        LIKE mseg-bwart,
        shkzg        LIKE mseg-shkzg,
        xauto        LIKE mseg-xauto,
        menge        LIKE mseg-menge,
        meins        LIKE mseg-meins,
        dmbtr        LIKE mseg-dmbtr,
        dmbum        LIKE mseg-dmbum,
        bustm        LIKE mseg-bustm,
        bustw        LIKE mseg-bustw,                       "147374

* define the fields for the IO-OIL specific functions       "n599218 A
*       mseg-oiglcalc        CHAR          1                "n599218 A
*       mseg-oiglsku         QUAN         13                "n599218 A
        oiglcalc(01) TYPE  c,                               "n599218 A
        oiglsku(07)  TYPE  p  DECIMALS 3,                   "n599218 A
        insmk        LIKE      mseg-insmk.                  "n599218 A
*
DATA:   /cwm/menge     TYPE    /cwm/menge.
*ENHANCEMENT-POINT ehp605_rm07mldd_21 SPOTS es_rm07mlbd STATIC .
DATA:
      END OF imsweg.

* User settings for the checkboxes                          "n547170
DATA: oref_settings TYPE REF TO cl_mmim_userdefaults.       "n547170


"Tab com txt dos tipos de movimentos
DATA gt_t156t TYPE TABLE OF t156t.
DATA gt_mara  TYPE TABLE OF mara.
