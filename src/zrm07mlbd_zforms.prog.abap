*----------------------------------------------------------------------*
***INCLUDE ZRM07MLBD_ZFORMS.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form ZF_TOOLBAR_COMMAND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_UCOMM
*&---------------------------------------------------------------------*
FORM zf_toolbar_command  USING p_ucomm TYPE sy-ucomm.

  DATA ls_selected TYPE ty_stab_belege.

  CHECK p_ucomm IS NOT INITIAL.

  "Recupera linha da ALV selecionada
  PERFORM f_get_selected_line CHANGING ls_selected.
  CHECK ls_selected IS NOT INITIAL.

  CASE p_ucomm.
    WHEN 'MMBE'.
      "Preenche fieldcat do popup
      DATA(lt_fieldcat) = VALUE slis_t_fieldcat_alv(
        ( fieldname = 'MATNR' ref_tabname = 'MSEG' ref_fieldname = 'MATNR' )
        ( fieldname = 'WERKS' ref_tabname = 'MSEG' ref_fieldname = 'WERKS' )
      ).

      "Salva parametros do usuario
      GET PARAMETER ID 'MAT' FIELD DATA(lv_bkp_mat).
      GET PARAMETER ID 'WRK' FIELD DATA(lv_bkp_wrk).
      GET PARAMETER ID 'LAG' FIELD DATA(lv_bkp_lag).

      "Abre MMBE
      SET PARAMETER ID 'MAT' FIELD ls_selected-matnr.
      SET PARAMETER ID 'WRK' FIELD ls_selected-werks.
      SET PARAMETER ID 'LAG' FIELD ''.
      CALL TRANSACTION 'MMBE' AND SKIP FIRST SCREEN.

      "Seta parametros do usuario de volta
      GET PARAMETER ID 'MAT' FIELD lv_bkp_mat.
      GET PARAMETER ID 'WRK' FIELD lv_bkp_wrk.
      GET PARAMETER ID 'LAG' FIELD lv_bkp_lag.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_CALL_TCODE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_SELFIELD
*&---------------------------------------------------------------------*
FORM zf_call_tcode  USING p_selfield TYPE slis_selfield.

  CASE p_selfield-sel_tab_field.
      "Pedido (ME23N)
    WHEN 'G_T_BELEGE-EBELN' OR 'G_T_BELEGE1-EBELN'.

      "Salva parametros do usuario
      GET PARAMETER ID 'BES' FIELD DATA(lv_bkp_bes).

      "Abre ME23N
      SET PARAMETER ID 'BES' FIELD p_selfield-value.
      CALL TRANSACTION 'ME23N' AND SKIP FIRST SCREEN.

      "Seta parametros do usuario de volta
      SET PARAMETER ID 'BES' FIELD lv_bkp_bes.

      "Ordem (CO03)
    WHEN 'G_T_BELEGE-AUFNR' OR 'G_T_BELEGE1-AUFNR'.
      "Salva parametros do usuario
      GET PARAMETER ID 'ANR' FIELD DATA(lv_bkp_aufnr).

      "Abre CO03
      DATA(lv_aufnr) = CONV aufnr( |{ p_selfield-value ALPHA = OUT }| ).
      SET PARAMETER ID 'ANR' FIELD lv_aufnr.
      CALL TRANSACTION 'CO03' AND SKIP FIRST SCREEN.

      "Seta parametros do usuario de volta
      SET PARAMETER ID 'ANR' FIELD lv_bkp_aufnr.

      "Nº reserva (MB23)
    WHEN 'G_T_BELEGE-RSNUM' OR 'G_T_BELEGE1-RSNUM'.
      "Salva parametros do usuario
      GET PARAMETER ID 'RES' FIELD DATA(lv_bkp_rsnum).

      "Abre MB23
      DATA(lv_rsnum) = CONV rsnum( |{ p_selfield-value ALPHA = OUT }| ).
      SET PARAMETER ID 'RES' FIELD lv_rsnum.
      CALL TRANSACTION 'MB23' AND SKIP FIRST SCREEN.

      "Seta parametros do usuario de volta
      SET PARAMETER ID 'RES' FIELD lv_bkp_rsnum.

  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_SELECTED_LINE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- PS_SELECTED
*&---------------------------------------------------------------------*
FORM f_get_selected_line  CHANGING ps_selected TYPE ty_stab_belege.

  TYPES tt_stab_belege LIKE g_t_belege1[].

  CLEAR ps_selected.

  TRY .
      "Recupera linhas selecionadas
      DATA(lt_selected) = VALUE tt_stab_belege( FOR ls_line IN g_t_belege[]
        WHERE ( sel = abap_true ) ( ls_line )
      ).

      "Apenas uma linha
      DATA(lv_lines) = lines( lt_selected[] ).
      IF lv_lines = 1.
        ps_selected = lt_selected[ 1 ].

      ELSE.
        "Nenhuma linha
        IF lv_lines < 1.
          MESSAGE 'Nenhuma linha selecionada'
            TYPE 'S' DISPLAY LIKE 'E'.
          "Mais de uma linha
        ELSEIF lv_lines > 1.
          MESSAGE 'Selecionar apenas uma linha'
            TYPE 'S' DISPLAY LIKE 'E'.
        ENDIF.
        EXIT.
      ENDIF.

    CATCH cx_root.

  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_FILL_CUST_VALUES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- PS_ALVLINE
*&---------------------------------------------------------------------*
FORM zf_fill_cust_values  CHANGING ps_alvline TYPE stype_belege.

  "Recupera descricao do material
  PERFORM f2100_mat_text USING ps_alvline-matnr.
  ps_alvline-maktx = g_s_makt-maktx.

  "Recupera Nome 1
  ps_alvline-name1 = t001w-name1.

  "Recupera texto do tipo de movimento
  PERFORM f_get_btext CHANGING ps_alvline.

  "Recupera tipo do material
  PERFORM f_get_mtart CHANGING ps_alvline.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_BTEXT
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- PS_ALVLINE
*&---------------------------------------------------------------------*
FORM f_get_btext  CHANGING ps_alvline TYPE stype_belege.

  READ TABLE gt_t156t[] INTO DATA(ls_t156t)
    WITH KEY spras = 'P'
             bwart = ps_alvline-bwart
             sobkz = ps_alvline-sobkz
             kzbew = ps_alvline-kzbew
             kzzug = ps_alvline-kzzug
             kzvbr = ps_alvline-kzvbr.
  IF sy-subrc IS NOT INITIAL.

    SELECT *
      FROM t156t
      APPENDING TABLE gt_t156t[]
      WHERE spras = 'P'
        AND bwart = ps_alvline-bwart
        AND sobkz = ps_alvline-sobkz
        AND kzbew = ps_alvline-kzbew
        AND kzzug = ps_alvline-kzzug
        AND kzvbr = ps_alvline-kzvbr.
    IF sy-subrc IS INITIAL.

      READ TABLE gt_t156t[] INTO ls_t156t
        WITH KEY spras = 'P'
                 bwart = ps_alvline-bwart
                 sobkz = ps_alvline-sobkz
                 kzbew = ps_alvline-kzbew
                 kzzug = ps_alvline-kzzug
                 kzvbr = ps_alvline-kzvbr.

    ENDIF.
  ENDIF.

  IF ls_t156t IS NOT INITIAL.
    ps_alvline-btext = ls_t156t-btext.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_GET_MTART
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- PS_ALVLINE
*&---------------------------------------------------------------------*
FORM f_get_mtart  CHANGING ps_alvline TYPE stype_belege.

  READ TABLE gt_mara[] INTO DATA(ls_mara)
    WITH KEY matnr = ps_alvline-matnr.
  IF sy-subrc IS NOT INITIAL.

    SELECT *
      FROM mara
      APPENDING TABLE gt_mara[]
      WHERE matnr = ps_alvline-matnr.
    IF sy-subrc IS INITIAL.

      READ TABLE gt_mara[] INTO ls_mara
        WITH KEY matnr = ps_alvline-matnr.

    ENDIF.
  ENDIF.

  IF ls_mara IS NOT INITIAL.
    ps_alvline-mtart = ls_mara-mtart.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_ADD_COUNT_LINES
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- PT_BELEGE1
*&---------------------------------------------------------------------*
FORM zf_add_count_lines USING    p_color    LIKE color[]
                        CHANGING pt_alvdata LIKE g_t_belege1[].

  DATA ls_alvsum LIKE LINE OF pt_alvdata[].

  "Insere linhas de saldo para os materiais
  LOOP AT pt_alvdata INTO DATA(ls_alvdata).
    DATA(lv_next_index) = sy-tabix + 1.

    "Verifica se ja náo é uma linha de somatorio
    CHECK NOT line_exists( ls_alvdata-farbe_pro_feld[ color-col = 3 ] ).

    "Verifica é a ultima linha para aquele material
    READ TABLE pt_alvdata[] ASSIGNING FIELD-SYMBOL(<fs_nextline>) INDEX lv_next_index.
    IF sy-subrc IS INITIAL.
      CHECK <fs_nextline>-matnr NE ls_alvdata-matnr.
      UNASSIGN <fs_nextline>.
    ENDIF.

*    "Insere somatorios do material
*    LOOP AT bestand1 INTO DATA(ls_sum) WHERE ( bwkey = ls_alvdata-werks OR bwkey = ls_alvdata-bwkey
*                                            OR werks = ls_alvdata-bwkey OR werks = ls_alvdata-werks )
*                                         AND matnr = ls_alvdata-matnr
*                                         AND charg = ls_alvdata-charg.
    "Insere Saldo Inicial na ALV
    ls_alvsum = VALUE #(
      matnr = bestand1-matnr
      werks = COND #( WHEN bestand1-werks IS NOT INITIAL
        THEN bestand1-werks
        ELSE bestand1-bwkey )
      maktx = TEXT-139            "Saldo Inicial
      menge = bestand1-anfmenge     "estoque inicial
      meins = bestand1-meins        "moeda
      dmbtr = bestand1-anfwert      "valor inicial
      waers = bestand1-waers        "moeda
      farbe_pro_feld = p_color[]  "cor da linha
    ).
    INSERT ls_alvsum INTO pt_alvdata[] INDEX lv_next_index.
    ADD 1 TO lv_next_index.

    "Insere Entradas na ALV
    ls_alvsum = VALUE #(
      maktx = TEXT-141            "Entradas
      menge = bestand1-soll         "estoque das entradas
      meins = bestand1-meins        "un. medida
      dmbtr = bestand1-sollwert     "valor das entradas
      waers = bestand1-waers        "moeda
      farbe_pro_feld = p_color[]  "cor da linha
    ).
    INSERT ls_alvsum INTO pt_alvdata[] INDEX lv_next_index.
    ADD 1 TO lv_next_index.

    "Insere Saidas na ALV
    ls_alvsum = VALUE #(
      maktx = TEXT-142                "Saídas
      menge = bestand1-haben * -1       "estoque das saidas
      meins = bestand1-meins            "un. medida
      dmbtr = bestand1-habenwert * -1   "valor das saidas
      waers = bestand1-waers            "moeda
      farbe_pro_feld = p_color[]      "cor da linha
    ).
    INSERT ls_alvsum INTO pt_alvdata[] INDEX lv_next_index.
    ADD 1 TO lv_next_index.

    "Insere Saldo Final na ALV
    ls_alvsum = VALUE #(
      maktx = TEXT-140            "Saldo Final
      menge = bestand1-endmenge     "estoque final
      meins = bestand1-meins        "un. medida
      dmbtr = bestand1-endwert      "valor final
      waers = bestand1-waers        "moeda
      farbe_pro_feld = p_color[]  "cor da linha
    ).
    INSERT ls_alvsum INTO pt_alvdata[] INDEX lv_next_index.
    ADD 1 TO lv_next_index.

*      EXIT.
*    ENDLOOP.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_GET_COLOR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- PT_COLOR[]
*&---------------------------------------------------------------------*
FORM zf_get_color  CHANGING pt_color LIKE color[].

  LOOP AT fieldcat[] ASSIGNING FIELD-SYMBOL(<fs_field>).

    APPEND VALUE #(
      fieldname = <fs_field>-fieldname
      color-col = 3 "Color number (1 - 9)
      color-int = 1 "Intensified (0 = off, 1 = on)
      color-inv = 1 "Inverse (0 = off, 1 = on)
    ) TO pt_color[].

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_DISPLAYDATA
*&---------------------------------------------------------------------*
FORM zf_displaydata .

  DATA lt_color  LIKE color[].

  "Monta tabela de colocaracao da linha
  PERFORM zf_get_color CHANGING lt_color[].

  CLEAR g_t_belege. REFRESH g_t_belege.

  SORT bestand BY matnr.

  "Monta ALV
  LOOP AT bestand.
    "Recupera valores calculados
    MOVE-CORRESPONDING bestand TO bestand1.
    APPEND bestand1.

    CLEAR g_t_belege1. REFRESH g_t_belege1.

    "Recupera registros para o material
    IF bwbst IS INITIAL.
*     fill the data table for the ALV with the              "n921165
*     corresponding MM documents for mode = stock           "n921165
      PERFORM  fill_data_table                              "n921165
                             TABLES    g_t_belege1          "n921165
                             USING     bestand-matnr        "n921165
                                       bestand-werks        "n921165
                                       bestand-charg.       "n921165

    ELSEIF NOT bwbst IS INITIAL.
*     fill the data table for the ALV with the              "n921165
*     corresponding MM documents for mode = valuated stock  "n921165
      PERFORM  process_plants_of_bwkey                      "n921165
                             TABLES    g_t_belege1          "n921165
                             USING     bestand-matnr        "n921165
                                       bestand-bwkey.       "n921165
    ENDIF.

    CHECK g_t_belege1[] IS NOT INITIAL.

    "Ordena as linhas do material
    SORT g_t_belege1 BY budat.

    "Insere Saldo Inicial na ALV
    APPEND VALUE #(
      matnr = bestand1-matnr
      werks = COND #( WHEN bestand1-werks IS NOT INITIAL
        THEN bestand1-werks
        ELSE bestand1-bwkey )
      maktx = TEXT-139              "Saldo Inicial
      menge = bestand1-anfmenge     "estoque inicial
      meins = bestand1-meins        "moeda
      dmbtr = bestand1-anfwert      "valor inicial
      waers = bestand1-waers        "moeda
      farbe_pro_feld = lt_color[]   "cor da linha
    ) TO g_t_belege1[].

    "Insere Entradas na ALV
    APPEND VALUE #(
      maktx = TEXT-141              "Entradas
      menge = bestand1-soll         "estoque das entradas
      meins = bestand1-meins        "un. medida
      dmbtr = bestand1-sollwert     "valor das entradas
      waers = bestand1-waers        "moeda
      farbe_pro_feld = lt_color[]   "cor da linha
    ) TO g_t_belege1[].

    "Insere Saidas na ALV
    APPEND VALUE #(
      maktx = TEXT-142                  "Saídas
      menge = bestand1-haben * -1       "estoque das saidas
      meins = bestand1-meins            "un. medida
      dmbtr = bestand1-habenwert * -1   "valor das saidas
      waers = bestand1-waers            "moeda
      farbe_pro_feld = lt_color[]       "cor da linha
    ) TO g_t_belege1[].

    "Insere Saldo Final na ALV
    APPEND VALUE #(
      maktx = TEXT-140            "Saldo Final
      menge = bestand1-endmenge     "estoque final
      meins = bestand1-meins        "un. medida
      dmbtr = bestand1-endwert      "valor final
      waers = bestand1-waers        "moeda
      farbe_pro_feld = lt_color[]  "cor da linha
    ) TO g_t_belege1[].

    "Insere linhas na tabela da ALV
    APPEND LINES OF g_t_belege1[] TO g_t_belege[].
  ENDLOOP.

*  "Insere linhas de saldo
*  PERFORM zf_add_count_lines USING    lt_color[]
*                             CHANGING g_t_belege1[].

  "Chama ALV
  PERFORM zf_call_alv CHANGING g_t_belege[].

ENDFORM.
*&---------------------------------------------------------------------*
*& Form ZF_CALL_ALV
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM zf_call_alv CHANGING pt_belege LIKE g_t_belege[].

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

  "Ajusta layout
  layout-colwidth_optimize = abap_true.
  layout-box_fieldname = 'SEL'.  "Campo de selecao

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_interface_check        = g_flag_i_check
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
*      is_print                 = g_s_print
    TABLES
      t_outtab                 = pt_belege[]
    EXCEPTIONS
*     program_error            = 1
      OTHERS                   = 2.

* does the ALV return with an error ?
  IF  NOT sy-subrc IS INITIAL.         "Fehler vom ALV ?
    MESSAGE ID sy-msgid TYPE  'S'     NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.
