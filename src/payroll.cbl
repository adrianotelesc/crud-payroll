       IDENTIFICATION DIVISION.
       PROGRAM-ID. FOLHA-DE-PAGAMENTO.
      ******************************************************************
      * Author: Adriano Teles
      * Date: 03/11/2016
      ******************************************************************
      *-----------------------------------------------------------------
       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
                  SPECIAL-NAMES.
                   DECIMAL-POINT IS COMMA.

           INPUT-OUTPUT SECTION.
                 FILE-CONTROL.
                       SELECT FOLHA1 ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE IS RANDOM
                       FILE STATUS IS FS-STAT
                       RECORD KEY IS FS-CHAVE.
       DATA DIVISION.
       FILE SECTION.
       FD FOLHA1.
       01 FS-COLABORADOR.
           02 FS-CHAVE.
               03 FS-REF-MES PIC 99.
               03 FS-REF-ANO PIC 9999.
               03 FS-MAT PIC 9(5).
           02 FS-DADOS-NUMERICOS.
               03 FS-ADMISSAO.
                   04 FS-ADM-DIA PIC 99.
                   04 FS-ADM-MES PIC 99.
                   04 FS-ADM-ANO PIC 9999.
               03 FS-FALTAS PIC 9(2).
               03 FS-SALBA PIC 9(6)V9(2).
               03 FS-CH PIC 999.
               03 FS-HE PIC 99.
               03 FS-DEP PIC 99.
               03 FS-FILHOS PIC 99.
               03 FS-SALBR PIC 9(6)V9(2).
               03 FS-SALIQ PIC 9(6)V9(2).
               03 FS-TOTAL-HE PIC 9(6)V9(2).
               03 FS-TOTAL-FALTAS PIC 9(6)V9(2).
               03 FS-INSS PIC 9(6)V9(2).
               03 FS-IRRF PIC 9(6)V9(2).
               03 FS-TOTAL-SFM PIC 9(6)V9(2).
               03 FS-TOTAL-DEP PIC 9(6)V9(2).
               03 FS-VT PIC 9(6)V9(2).
               03 FS-DSR PIC 9(6)V9(2).
               03 FS-FGTS PIC 9(6)V9(2).
           02 FS-DADOS-ALFABETICOS.
               03 FS-NOME PIC X(35).
               03 FS-FUNC PIC X(30).
               03 FS-OP-VT PIC A.
                   88 VT-SIM VALUE 'S'.
       WORKING-STORAGE SECTION.
       77 FS-STAT PIC 9(02).
           88 FS-OK         VALUE ZEROS.
           88 FS-CANCELA    VALUE 99.
           88 FS-NAO-EXISTE VALUE 35.
       77 WS-ERRO PIC X.
           88 E-SIM VALUE 'S' 's'.
           88 E-NAO VALUE 'N' 'n'.
       77 WS-MSGERRO PIC X(35).
       77 WS-MSG PIC X(30).
       77  WS-MAIUSCULAS PIC X(026) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
       77  WS-MINUSCULAS PIC X(026) VALUE 'abcdefghijklmnopqrstuvwxyz'.
       77  MASC-MONEY PIC ZZZ.ZZ9,99.
       77  MASC-DATE-DMY PIC 99/99/9999.
       77  MASC-DATE-MY PIC 99/9999.
       77  WS-OPCAO PIC X.
           88 CADASTRAR VALUE '1'.
           88 PESQUISAR VALUE '2'.
           88 MODIFICAR VALUE '3'.
           88 DELETAR VALUE '4'.
           88 SAIR VALUE 'X' 'x'.
           COPY screenio.
           SCREEN SECTION.

           01 SS-TELA-OPCAO HIGHLIGHT FOREGROUND-COLOR 7.
               02 BLANK SCREEN.
               02 LINE 2 COL 3  VALUE "FOLHA DE PAGAMENTO - MENU".
               02 LINE 4 COL 5  VALUE '1 - CADASTRAR'.
               02 LINE 5 COL 5  VALUE "2 - PESQUISAR".
               02 LINE 6 COL 5  VALUE "3 - MODIFICAR".
               02 LINE 7 COL 5  VALUE "4 - DELETAR".
               02 LINE 8 COL 5  VALUE 'X - SAIR'.
               02 LINE 10 COL 5  VALUE 'ESCOLHA:'.
               02 LINE 10 COL PLUS 1 PIC X USING WS-OPCAO AUTO.


           01 SS-TELA-FOLHA HIGHLIGHT FOREGROUND-COLOR 7.
               02 BLANK SCREEN.
                   02 SS-CHAVE.
                       03 LINE 2  COL 3  VALUE 'REFERENCIA:'.
                       03 LINE 2  COL PLUS 1  PIC 9(2) USING FS-REF-MES
                           HIGHLIGHT FOREGROUND-COLOR 3 AUTO.
                       03 LINE 2  COL 17 VALUE '/'
                           HIGHLIGHT FOREGROUND-COLOR 3 AUTO.
                       03 LINE 2  COL 18  PIC 9(4) USING FS-REF-ANO
                           HIGHLIGHT FOREGROUND-COLOR 3.
                       03 LINE 5  COL 5  VALUE 'MATRICULA:'.
                       03 LINE 5  COL PLUS 1 PIC 9(5) USING FS-MAT
                           HIGHLIGHT FOREGROUND-COLOR 3.
                       03 LINE 2  COL 70 PIC X(30) FROM WS-MSG.
               02 LINE 3  COL 3  VALUE '________________________________
      -'________________________________________________________________
      -'________________________________________________________________
      -'__'.
               02 LINE 5  COL 26 VALUE 'NOME:'.
               02 LINE 5 COL PLUS 1 PIC X(35) USING FS-NOME
                   HIGHLIGHT FOREGROUND-COLOR 3.
               02 LINE 5  COL PLUS 5  VALUE 'FUNCAO:'.
               02 LINE 5 COL PLUS 1 PIC X(30) USING FS-FUNC
                   HIGHLIGHT FOREGROUND-COLOR 3.
               02 ADMISSAO.
                   03 LINE 5  COL PLUS 5  VALUE 'ADMISSAO:'.
                   03 LINE 5  COL PLUS 1 PIC 9(2) USING FS-ADM-DIA
                       HIGHLIGHT FOREGROUND-COLOR 3 AUTO.
                   03 LINE 5  COL 127 VALUE '/'
                       HIGHLIGHT FOREGROUND-COLOR 3.
                   03 LINE 5  COL 128 PIC 9(2) USING FS-ADM-MES
                       HIGHLIGHT FOREGROUND-COLOR 3 AUTO.
                   03 LINE 5  COL 130 VALUE '/'
                       HIGHLIGHT FOREGROUND-COLOR 3.
                   03 LINE 5  COL 131 PIC 9(4) USING FS-ADM-ANO
                       HIGHLIGHT FOREGROUND-COLOR 3.
               02 LINE 5 COL PLUS 5 VALUE 'FALTAS:'.
               02 LINE 5 COL PLUS 1 PIC 9(2) USING FS-FALTAS
                   HIGHLIGHT FOREGROUND-COLOR 3.
               02 LINE 6  COL 5  VALUE 'SALARIO BASE:'.
               02 LINE 6 COL PLUS 1 VALUE 'R$'
                   HIGHLIGHT FOREGROUND-COLOR 3.
               02 LINE 6 COL PLUS 1 PIC 999999,99 USING FS-SALBA
                   HIGHLIGHT FOREGROUND-COLOR 3.
               02 LINE 6  COL PLUS 5 VALUE 'CARGA HORARIA:'.
               02 LINE 6 COL PLUS 1 PIC 9(3) USING FS-CH
                   HIGHLIGHT FOREGROUND-COLOR 3.
               02 LINE 6  COL PLUS 5 VALUE 'HORAS EXTRAS:'.
               02 LINE 6  COL PLUS 1 PIC 9(2) USING FS-HE
                   HIGHLIGHT FOREGROUND-COLOR 3.
               02 LINE 6 COL PLUS 5  VALUE 'DEPENDENTES:'.
               02 LINE 6 COL PLUS 1 PIC 9(2) USING FS-DEP
                   HIGHLIGHT FOREGROUND-COLOR 3.
               02 LINE 6 COL PLUS 5  VALUE 'FILHOS:'.
               02 LINE 6 COL PLUS 1 PIC 9(2) USING FS-FILHOS
                   HIGHLIGHT FOREGROUND-COLOR 3.
               02 LINE 6 COL PLUS 5 VALUE 'VALE-TRANSPORTE (S/N):'.
               02 LINE 6 COL PLUS 1 PIC A USING FS-OP-VT
                   HIGHLIGHT FOREGROUND-COLOR 3 .
               02 line 7  COL 3  VALUE '________________________________
      -'________________________________________________________________
      -'________________________________________________________________
      -'__'.
               02 LINE 9  COL 5  VALUE 'DESCRICAO'.
               02 LINE 9  COL 115 VALUE 'PROVENTOS'.
               02 LINE 9  COL 154 VALUE 'DESCONTOS'.
               02 LINE 10 COL 5  VALUE "SALARIO BASE"
                   HIGHLIGHT FOREGROUND-COLOR 2.
               02 LINE 10 COL 114 PIC ZZZ.ZZ9,99 FROM FS-SALBA
                   HIGHLIGHT FOREGROUND-COLOR 2.
               02 LINE 11 COL 5  VALUE "HORAS EXTRAS"
                   HIGHLIGHT FOREGROUND-COLOR 2.
               02 LINE 11 COL 114 PIC ZZZ.ZZ9,99 FROM FS-TOTAL-HE
                   HIGHLIGHT FOREGROUND-COLOR 2.
               02 LINE 12 COL 5  VALUE "DSR"
                   HIGHLIGHT FOREGROUND-COLOR 2.
               02 LINE 12 COL 114 PIC ZZZ.ZZ9,99 FROM FS-DSR
                   HIGHLIGHT FOREGROUND-COLOR 2.
               02 LINE 13 COL 5  VALUE "VALE-TRANSPORTE"
                   HIGHLIGHT FOREGROUND-COLOR 4.
               02 LINE 13 COL 153 PIC ZZZ.ZZ9,99 FROM FS-VT
                   HIGHLIGHT FOREGROUND-COLOR 4.
               02 LINE 14 COL 5 VALUE 'FALTAS'
                   HIGHLIGHT FOREGROUND-COLOR 4.
               02 LINE 14 COL 153 PIC ZZZ.ZZ9,99 FROM FS-TOTAL-FALTAS
                   HIGHLIGHT FOREGROUND-COLOR 4.
               02 LINE 15 COL 5  VALUE "INSS"
                   HIGHLIGHT FOREGROUND-COLOR 4.
               02 LINE 15 COL 153 PIC ZZZ.ZZ9,99 FROM FS-INSS
                   HIGHLIGHT FOREGROUND-COLOR 4.
               02 LINE 16 COL 5  VALUE "IRRF"
                   HIGHLIGHT FOREGROUND-COLOR 4.
               02 LINE 16 COL 153 PIC ZZZ.ZZ9,99 FROM FS-IRRF
                   HIGHLIGHT FOREGROUND-COLOR 4.
               02 line 18  COL 3  VALUE '________________________________
      -'________________________________________________________________
      -'________________________________________________________________
      -'__'.
               02 LINE 20 COL 5 VALUE 'OUTROS / BENEFICIOS'.
               02 LINE 21 COL 5 VALUE 'FGTS'
                   HIGHLIGHT FOREGROUND-COLOR 6.
               02 LINE 21 COL 153 PIC ZZZ.ZZ9,99 FROM  FS-FGTS
                   HIGHLIGHT FOREGROUND-COLOR 6.
               02 LINE 22 COL 5  VALUE "SALARIO FAMILIA"
                   HIGHLIGHT FOREGROUND-COLOR 6.
               02 LINE 22 COL 153 PIC ZZZ.ZZ9,99 FROM FS-TOTAL-SFM
                   HIGHLIGHT FOREGROUND-COLOR 6.
               02 LINE 28 COL 97  VALUE 'SALARIO BRUTO: R$'.
               02 LINE 28 COL 114 PIC ZZZ.ZZ9,99 FROM FS-SALBR.
               02 LINE 28 COL 134 VALUE 'SALARIO LIQUIDO: R$'.
               02 LINE 28 COL 153 PIC ZZZ.ZZ9,99 FROM FS-SALIQ.
               02 line 25  COL 3  VALUE '________________________________
      -'________________________________________________________________
      -'________________________________________________________________
      -'__'.
           01 SS-ERRO HIGHLIGHT FOREGROUND-COLOR 7.
               02 LINE 29 COL 5 VALUE '** MENSAGEM **'.
               02 LINE 30 COLUMN 5 PIC X(35) FROM WS-MSGERRO BELL.
               02 LINE 30 COL PLUS 1 PIC X USING WS-ERRO AUTO.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           SET ENVIRONMENT 'COB_SCREEN_EXCEPTIONS' TO 'Y'.
           SET ENVIRONMENT 'COB_SCREEN_ESC' TO 'Y'.
           SET ENVIRONMENT 'ESCDELAY' TO '25'.
           PERFORM ABRIR-ARQUIVOS
           PERFORM UNTIL SAIR
               MOVE SPACES TO WS-OPCAO
               ACCEPT SS-TELA-OPCAO
               EVALUATE TRUE
                   WHEN CADASTRAR
                       PERFORM CADASTRO THRU CADASTRO-FIM
                   WHEN PESQUISAR
                       PERFORM PESQUISA THRU PESQUISA-FIM
                   WHEN MODIFICAR
                       PERFORM MODIFICA THRU MODIFICA-FIM
                   WHEN DELETAR
                       PERFORM DELETA THRU DELETA-FIM
               END-EVALUATE
           END-PERFORM.
       FINALIZA.
           CLOSE FOLHA1.
           STOP RUN.

       CADASTRO.
           MOVE 'FOLHA DE PAGAMENTO - CADASTRO' TO WS-MSG.
      *    INICIO - LIMPANDO O CONTEUDO DAS VARIAVEIS
           MOVE ZEROS TO FS-CHAVE.
           MOVE ZEROS TO FS-DADOS-NUMERICOS.
           MOVE SPACES TO FS-DADOS-ALFABETICOS.
           MOVE SPACES TO WS-ERRO.
      *    FIM - LIMPANDO O CONTEUDO DAS VARIAVEIS
       CADASTRO-LOOP.
           ACCEPT SS-TELA-FOLHA.
           IF COB-CRT-STATUS = COB-SCR-ESC
               GO CADASTRO-FIM
           END-IF.
       CALCULOS.
      *    INICIO - CONVERTENDO DE MINUSCULO PARA MAIUSCULO
           INSPECT FS-NOME CONVERTING WS-MINUSCULAS TO WS-MAIUSCULAS
           INSPECT FS-FUNC CONVERTING WS-MINUSCULAS TO WS-MAIUSCULAS
           INSPECT FS-OP-VT CONVERTING WS-MINUSCULAS TO WS-MAIUSCULAS
      *    FIM - CONVERTENDO DE MINUSCULO PARA MAIUSCULO

      *    INICIO - VALIDAÇÃO DE TODOS OS DADOS INFORMADOS
              IF FS-REF-MES < 1 OR FS-REF-MES > 12 OR FS-REF-ANO < 1959
                  OR FS-NOME = SPACES OR FS-NOME IS NOT ALPHABETIC OR
                  FS-FUNC = SPACES OR FS-FUNC IS NOT ALPHABETIC OR
                  FS-ADM-DIA < 1 OR FS-ADM-DIA > 31 OR FS-ADM-MES < 1 OR
                  FS-ADM-MES > 12 OR FS-ADM-ANO < 1959 OR
                  (FS-OP-VT <> 'S' AND FS-OP-VT <> 'N' OR FS-SALBA = 0
                  OR FS-CH = 0)
                  MOVE "CAMPO(S) INVALIDOS" TO WS-MSGERRO
                  PERFORM MOSTRA-ERRO
                  GO TO CADASTRO-LOOP
              END-IF.
      *    FIM - VALIDAÇÃO DE TODOS OS DADOS INFORMADOS

      *    INICIO - CALCULANDO HORAS EXTRAS
           COMPUTE FS-TOTAL-HE = ( (FS-SALBA / FS-CH) + (FS-SALBA / FS-C
      -H) * 50 / 100 ) * FS-HE.
      *    FIM - CALCULANDO HORAS EXTRAS

      *    INICIO - CALCULANDO DESCANSO SEMANAL REMUNERADO
           COMPUTE FS-DSR = (FS-TOTAL-HE / 26) * 4.
      *    FIM - CALCULANDO DESCANSO SEMANAL REMUNERADO

      *    INICIO - CALCULANDO SALARIO BRUTO
           COMPUTE FS-SALBR = FS-SALBA + FS-TOTAL-HE + FS-DSR.
      *    FIM - CALCULANDO SALARIO BRUTO

      *    FIM - CALCULANDO VALE-TRANSPORTE, CASO O USUARIO QUEIRA
           IF VT-SIM
               COMPUTE FS-VT = FS-SALBA * 0,06
           ELSE
               COMPUTE FS-VT = 0
           END-IF.
      *    FIM - CALCULANDO VALE-TRANSPORTE, CASO O USUARIO QUEIRA

      *    INICIO - CALCULANDO INSS
           EVALUATE TRUE
           WHEN (FS-SALBR > 0) AND (FS-SALBR <= 1556,94)
               COMPUTE FS-INSS = FS-SALBR * 0,08
           WHEN (FS-SALBR > 1556,94) AND (FS-SALBR <= 2594,92)
               COMPUTE FS-INSS = FS-SALBR * 0,09
           WHEN (FS-SALBR > 2594,92) AND (FS-SALBR <= 5189,82)
               COMPUTE FS-INSS = FS-SALBR * 0,11
           WHEN (FS-SALBR > 5189,82)
               COMPUTE FS-INSS = 5189,82 * 0,11
           END-EVALUATE.
      *    FIM - CALCULANDO INSS

      *    INICIO - CALCULANDO VALOR POR DEPENDENTE PARA IRRF
           COMPUTE FS-TOTAL-DEP = FS-DEP * 189,59.
      *    FIM - CALCULANDO VALOR POR DEPENDENTE PARA IRRF

      *    INICIO - CALCULANDO IRRF
           EVALUATE TRUE
           WHEN ((FS-SALBR - FS-INSS) - FS-TOTAL-DEP > 1903,98) AND
      -(((FS-SALBR - FS-INSS) - FS-TOTAL-DEP) <= 2826,65)
               COMPUTE FS-IRRF = (((FS-SALBR - FS-INSS) - FS-TOTAL-DEP)
      - * 0,075) - 142,80
           WHEN (((FS-SALBR - FS-INSS) - FS-TOTAL-DEP) > 2826,65
      -) AND (((FS-SALBR - FS-INSS) - (FS-DEP * 189,59)) <= 3751,05)
               COMPUTE FS-IRRF = (((FS-SALBR - FS-INSS) - FS-TOTAL-DEP)
      - * 0,150) - 354,80
           WHEN (((FS-SALBR - FS-INSS) - FS-TOTAL-DEP) > 3751,05
      -) AND (((FS-SALBR - FS-INSS) - FS-TOTAL-DEP) <= 4664,68)
               COMPUTE FS-IRRF = (((FS-SALBR - FS-INSS) - FS-TOTAL-DEP)
      - * 0,225) - 636,13
           WHEN (((FS-SALBR - FS-INSS) - FS-TOTAL-DEP) > 4664,68)
               COMPUTE FS-IRRF = (((FS-SALBR - FS-INSS) - FS-TOTAL-DEP)
      - * 0,275) - 869,36
           END-EVALUATE.
      *    FIM - CALCULANDO IRRF

      *    INICIO - CALCULANDO DO SALARIO FAMILIA
           EVALUATE TRUE
               WHEN FS-FILHOS > 0 AND FS-SALBA <= 806,80
                   COMPUTE FS-TOTAL-SFM = 41,37 * FS-FILHOS
               WHEN FS-SALBA > 806,81 AND FS-SALBA <= 1212,64 AND
       FS-FILHOS > 0
                   COMPUTE FS-TOTAL-SFM = 29,16 * FS-FILHOS
           END-EVALUATE.
      *    FIM - CALCULANDO DO SALARIO FAMILIA
      *    INICIO - CALCULANDO DESCONTO DAS FALTAS
           COMPUTE FS-TOTAL-FALTAS = (FS-SALBA / 30) * FS-FALTAS.

      *    INICIO - CALCULANDO FGTS
           COMPUTE FS-FGTS = FS-SALBR * 0,08.
      *    FIM - CALCULANDO FGTS

           COMPUTE FS-SALIQ = FS-SALBR - FS-INSS - FS-IRRF - FS-VT
               - FS-TOTAL-FALTAS.
       CALCULOS-FIM.
           DISPLAY SS-TELA-FOLHA.
           MOVE 'CONFIRMA O CADASTRO (S/N)?' TO WS-MSGERRO.
           PERFORM UNTIL E-SIM OR E-NAO
           PERFORM MOSTRA-ERRO
           EVALUATE TRUE
           WHEN E-SIM
               OPEN OUTPUT FOLHA1
               WRITE FS-COLABORADOR
               INVALID KEY
               MOVE "CLIENTE JA EXISTE" TO WS-MSGERRO
               PERFORM MOSTRA-ERRO
               MOVE ZEROS TO FS-CHAVE
               END-WRITE
           WHEN E-NAO
               MOVE SPACES TO WS-ERRO
               GO TO CADASTRO-LOOP
           END-EVALUATE
           END-PERFORM.
           GO TO CADASTRO.
           CADASTRO-FIM.

       PESQUISA.
           MOVE 'FOLHA DE PAGAMENTO - PESQUISA' TO WS-MSG.
           IF COB-CRT-STATUS = COB-SCR-ESC
               GO INICIO
           END-IF

      *    INICIO - LIMPANDO O CONTEUDO DAS VARIAVEIS
           MOVE ZEROS TO FS-CHAVE.
           MOVE ZEROS TO FS-DADOS-NUMERICOS.
           MOVE SPACES TO FS-DADOS-ALFABETICOS.
           MOVE SPACES TO WS-ERRO.
      *    FIM - LIMPANDO O CONTEUDO DAS VARIAVEIS     MOVE SPACES TO FS-COLABORADOR.

           PERFORM LE-CLIENTE
           IF FS-CANCELA
               GO CADASTRO-FIM
           END-IF
           IF FS-OK
               DISPLAY SS-TELA-FOLHA
               MOVE "PRESSIONE ENTER" TO WS-MSGERRO
               PERFORM MOSTRA-ERRO
           END-IF.
           GO PESQUISA.
       PESQUISA-FIM.

       MODIFICA.
           MOVE 'FOLHA DE PAGAMENTO - MODIFICA' TO WS-MSG.
      *    INICIO - LIMPANDO O CONTEUDO DAS VARIAVEIS
           MOVE ZEROS TO FS-CHAVE.
           MOVE ZEROS TO FS-DADOS-NUMERICOS.
           MOVE SPACES TO FS-DADOS-ALFABETICOS.
           MOVE SPACES TO WS-ERRO.
      *    FIM - LIMPANDO O CONTEUDO DAS VARIAVEIS

           DISPLAY SS-TELA-FOLHA.
           PERFORM LE-CLIENTE THRU LE-CLIENTE-FIM.
           IF FS-CANCELA
               GO TO MODIFICA-FIM
           END-IF
           IF FS-OK
               ACCEPT SS-TELA-FOLHA
               PERFORM CALCULOS
               DISPLAY SS-TELA-FOLHA
               MOVE 'CONFIRMA MODIFICAR A FOLHA  (S/N)?' TO WS-MSGERRO.
               PERFORM UNTIL E-SIM OR E-NAO
               PERFORM MOSTRA-ERRO
                   EVALUATE TRUE
                       WHEN E-SIM
                           REWRITE FS-COLABORADOR
                           INVALID KEY
                               MOVE "ERRO AO GRAVAR" TO WS-MSGERRO
                               PERFORM MOSTRA-ERRO
                           NOT INVALID KEY
                               CONTINUE
                           END-REWRITE
                       WHEN E-NAO
                           GO TO MODIFICA
           END-EVALUATE
           END-PERFORM
           IF COB-CRT-STATUS = COB-SCR-ESC
                   GO MODIFICA
           ELSE
               GO MODIFICA
           END-IF
            REWRITE FS-COLABORADOR
                INVALID KEY
                    MOVE "ERRO AO GRAVAR" TO WS-MSGERRO
                    PERFORM MOSTRA-ERRO
                NOT INVALID KEY
                    CONTINUE
            END-REWRITE.
            GO MODIFICA.
       MODIFICA-FIM.

       DELETA.
           MOVE 'FOLHA DE PAGAMENTO - DELETA' TO WS-MSG.
      *    INICIO - LIMPANDO O CONTEUDO DAS VARIAVEIS
           MOVE ZEROS TO FS-CHAVE.
           MOVE ZEROS TO FS-DADOS-NUMERICOS.
           MOVE SPACES TO FS-DADOS-ALFABETICOS.
           MOVE SPACES TO WS-ERRO.
      *    FIM - LIMPANDO O CONTEUDO DAS VARIAVEIS

           DISPLAY SS-TELA-FOLHA.
           PERFORM LE-CLIENTE THRU LE-CLIENTE-FIM.
           IF FS-CANCELA
               GO DELETA-FIM
           END-IF
           IF NOT FS-OK
               GO DELETA
           END-IF
           DISPLAY SS-TELA-FOLHA.
           MOVE "N" TO WS-ERRO.
           MOVE "CONFIRMA DELETAR A FOLHA (S/N)?" TO WS-MSGERRO.
           ACCEPT SS-ERRO.
           IF NOT E-SIM
               GO DELETA
           END-IF
           DELETE FOLHA1
               INVALID KEY
                   MOVE "ERRO AO EXCLUIR" TO WS-MSGERRO
                   PERFORM MOSTRA-ERRO
           END-DELETE.
       DELETA-FIM.

       LE-CLIENTE.
           DISPLAY SS-TELA-FOLHA.
           ACCEPT SS-CHAVE.
           IF NOT COB-CRT-STATUS = COB-SCR-ESC
               READ FOLHA1
                   INVALID KEY
                       MOVE "FOLHA NAO ENCONTRADA" TO WS-MSGERRO
                       PERFORM MOSTRA-ERRO
               END-READ
           ELSE
               MOVE 99 to FS-STAT
           END-IF.
       LE-CLIENTE-FIM.

       ABRIR-ARQUIVOS.
           OPEN I-O FOLHA1
           IF FS-NAO-EXISTE THEN
               OPEN OUTPUT FOLHA1
               CLOSE FOLHA1
               OPEN I-O FOLHA1
           END-IF.


           MOSTRA-ERRO.
               ACCEPT SS-ERRO.
