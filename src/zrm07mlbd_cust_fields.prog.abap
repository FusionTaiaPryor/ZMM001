***INCLUDE RM07MLBD_CUST_FIELDS .

* correction Nov. 2005 MM                                   "n890109
* allow the inter active functions 'Specify drill-down'     "n890109
* and 'Choose' from the menu 'Settings -> Summation levels' "n890109

* Improvements :                       March 2003 MM        "n599218
* - allow to process the fields MAT_KDAUF, MAT_KDPOS, and   "n599218
*   MAT_PSPNR from release 4.5B and higher                  "n599218

* representation of tied empties improved    August 2002 MM "n547170

* field "SJAHR" was moved to report RM07MBLD                "n497992
* new custimizing settings                                  "n497992
* - customizing for the selection of remaining BSIM entries "n497992
* - customizing for the processing of tied empties          "n497992

*----------------------------------------------------------------------*
* report RM07MLBD and its includes improved  May 10th, 2001 "n400992
*----------------------------------------------------------------------*

* This include contains the structure for additional fields for report
* RM07MLBD ( transaction MB5B )

* the following fields are not shown in the list of report
* RM07MLBD
* If you want to insert some of these fields in the list of the
* hidden fields delete the '*' in the type definition.
*
* There are only fields from database table MSEG possible
* Please use only the following fields, because these fields
* are considered during the creation of the field catalog;
* plaese consider, that each active field will cost performance

*ENHANCEMENT-SECTION RM07MLBD_CUST_FIELDS_01 SPOTS ES_RM07MLBD_CUST_FIELDS STATIC .
TYPES : BEGIN OF stype_mb5b_add,
          dummy(01)  TYPE c,             "filler

          "Novos campos
          maktx      LIKE makt-maktx,
          xblnr      LIKE mkpf-xblnr,
          name1      LIKE t001w-name1,
          btext      LIKE t156t-btext,
          mtart      LIKE mara-mtart,
          aplzl      LIKE mseg-aplzl,
          xmacc      LIKE mseg-xmacc,
*
* the following fields of database table MSEG can be activated
*          insmk      LIKE      mseg-insmk,    "Bestandsart - Tipo de estoque
          lifnr      LIKE      mseg-lifnr,    "Kontonummer Lieferant - Nº conta do fornecedor
          kunnr      LIKE      mseg-kunnr,    "Kontonummer Kunde - Nº conta do cliente

*  Caution : functionalitiy was changed after release 4.0B  "n599218
*  you will find the sales order in the following fields :  "n599218
*  Field       Description           release                "n599218
*  MAT_KDAUF   sales order number    4.5B and higher        "n599218
*  MAT_KDPOS   sales order item      4.5B and higher        "n599218
*  KDAUF       sales order number    4.0B                   "n599218
*  KDPOS       sales order item      4.0B                   "n599218
*                                                           "n599218
          mat_kdauf  LIKE    mseg-mat_kdauf,                "n599218
          mat_kdpos  LIKE    mseg-mat_kdpos,                "n599218
          kdauf      LIKE      mseg-kdauf,    "Kundenauftragsnummer
          kdpos      LIKE      mseg-kdpos,    "Positionsnummer

          kdein      LIKE      mseg-kdein,    "Einteilung Kundenauftrag

* please activate or deactivate the following two fields togehter
          erfmg      LIKE      mseg-erfmg,    "Menge in ERFME
          erfme      LIKE      mseg-erfme,    "Erfassungsmengeneinheit

* please activate or deactivate the following two fields togehter
          bpmng      LIKE      mseg-bpmng,    " Menge in BPRME
          bprme      LIKE      mseg-bprme,    " Bestellpreismengeneinheit

          ebeln      LIKE      mseg-ebeln,    " Bestellnummer
          ebelp      LIKE      mseg-ebelp,    " Positionsnummer

          elikz      LIKE      mseg-elikz,    " Endlieferungskennzeichen
          sgtxt      LIKE      mseg-sgtxt,    " Segment-Text
          wempf      LIKE      mseg-wempf,    " Warenempfänger
          ablad      LIKE      mseg-ablad,    " Abladestelle
          gsber      LIKE      mseg-gsber,    " Geschäftsbereich

          pargb      LIKE      mseg-pargb,    " Geschäftsbereich
          parbu      LIKE      mseg-parbu,    " Verrechnender
          kostl      LIKE      mseg-kostl,    " Kostenstelle
          aufnr      LIKE      mseg-aufnr,    " Auftragsnummer
          anln1      LIKE      mseg-anln1,    " Anlagen-Hauptnummer

          rsnum      LIKE      mseg-rsnum,    " Nummer der Reservierung/
          rspos      LIKE      mseg-rspos,    " Positionsnummer der
          kzear      LIKE      mseg-kzear,    " Kennzeichen Endausfassung
          ummat      LIKE      mseg-ummat,    " Empfangendes/Abgebendes
          umwrk      LIKE      mseg-umwrk,    " Empfangendes/Abgebendes

          umlgo      LIKE      mseg-umlgo,    " Empfangender/Abgebender
          umcha      LIKE      mseg-umcha,    " Empfangende/Abgebende
          umbar      LIKE      mseg-umbar,    " Bewertungsart der
          umsok      LIKE      mseg-umsok,    " Sonderbestandskennzeichen
          weunb      LIKE      mseg-weunb,    " Kennzeichen Wareneingang

          grund      LIKE      mseg-grund,    " Kennzeichen: Grund
          kstrg      LIKE      mseg-kstrg,    " Kostenträger
          paobjnr    LIKE      mseg-paobjnr,  " Nummer für
          prctr      LIKE      mseg-prctr,    " Profit Center

*  Caution : functionalitiy was changed after release 4.0B  "n599218
*  you will find the WBS element in the following fields :  "n599218
*  Field       Description           use in release         "n599218
*  MAT_PSPNR   WBS elemnet           4.5B and higher        "n599218
*  PS_PSP_PNR  WBS element           4.0B                   "n599218
*                                                           "n599218
          mat_pspnr  LIKE   mseg-mat_pspnr,                 "n599218
          ps_psp_pnr LIKE   mseg-ps_psp_pnr, "Projektstrukturplanel.

          nplnr      LIKE      mseg-nplnr,    " Netzplannummer
          aufpl      LIKE      mseg-aufpl,    " Plannummer zu Vorgängen
          aufps      LIKE      mseg-aufps,    " Nummer der

* please activate or deactivate the following two fields togehter
          bstmg      LIKE      mseg-bstmg,    " Wareneingangsmenge
          bstme      LIKE      mseg-bstme,    " Bestellmengeneinheit

          exbwr      LIKE      mseg-exbwr,    " Extern eingegebener
          vkwrt      LIKE      mseg-vkwrt,    " Wert zu Verkaufspreisen
          vfdat      LIKE      mseg-vfdat,    " Verfallsdatum oder
          exvkw      LIKE      mseg-exvkw,    " Extern eingegebener
          pprctr     LIKE      mseg-pprctr,   " Partner-Profit Center

          matbf      LIKE      mseg-matbf,    " Material, auf dem der
          ummab      LIKE      mseg-ummab,    " Empfangendes/Abgebendes
          lbkum      LIKE      mseg-lbkum,    " Gesamter bewerteter
          salk3      LIKE      mseg-salk3,    " Wert des gesamten
          vprsv      LIKE      mseg-vprsv,    " Preissteuerungskennz.

          vkwra      LIKE      mseg-vkwra,    " Wert zu Verkaufspreisen
          urzei      LIKE      mseg-urzei,    " Ursprungszeile im

* please activate or deactivate the following two fields togehter
          lsmng      LIKE      mseg-lsmng,    " Menge in Mengeneinheit
          lsmeh      LIKE      mseg-lsmeh,    " Mengeneinheit aus
        END OF stype_mb5b_add.
*END-ENHANCEMENT-SECTION.

*----------------------------------------------------------------------*

* customizing for the color in the lines with documents
*      value 'X' : the fields with quantities will be colored
*      value ' ' : no colors ( improve the performance )
DATA : g_cust_color(01)    TYPE c    VALUE 'X'.
* data : G_cust_color(01)    type c    value ' '.

*----------------------------------------------------------------------*

* customizing for the selection of remaining BSIM entries   "n497992
* ( FI document ) without matching MSEG ( MM document )     "n497992
* like price changes, account adjustments, etc...           "n497992
*                                                           "n497992
* value ' ' : (default) take all remaining BSIM entries     "n497992
* value 'X' : take only the remaining BSIM entries when     "n497992
*             when the original BSEG entry contains the     "n497992
*             transaction key ( KTOSL ) 'BSX'               "n497992
*             this mode will cost performance               "n497992
DATA : g_cust_bseg_bsx(01) TYPE c    VALUE ' '.             "n497992
* data : G_cust_bseg_bsx(01) type c    value 'X'.           "n497992

*----------------------------------------------------------------------*

* customizing for the processing of tied emptied materials  "n497992
*                                                           "n497992
* value ' ' : (default) do not consider tied empties        "n497992
* value 'X' : consider tied empties materials with their    "n497992
*             special rules in the inventory management     "n497992
*             this mode will cost performance               "n497992
DATA : g_cust_tied_empties(01)  TYPE c    VALUE ' '.        "n497992
* data : G_cust_tied_empties(01)  type c    value 'X'.      "n547170

*----------------------------------------------------------------------*

* customizing for the interactive functions                 "n890109
* 'Specify drill-down' and 'Choose' from the menu           "n890109
* 'Settings -> Summation levels'                            "n890109
*                                                           "n890109
* value 'X' : (default) allow the interactive functions     "n890109
*             'Specify drill-down' and 'Choose' from the    "n890109
*             menu 'Settings -> Summation levels'           "n890109
* value ' ' : do not allow these functions                  "n890109
DATA : g_cust_sum_levels(01)    TYPE c    VALUE 'X'.        "n890109
* data : g_cust_sum_levels(01)    type c    value ' '.      "n890109
                                                            "n890109
*-----------------------------------------------------------"n890109
