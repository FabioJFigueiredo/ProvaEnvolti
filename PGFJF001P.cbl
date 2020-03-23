       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     PGFJF001P.                       
      *----------------------------------------------------------------* 
      * PROGRAMA.....: PGFJF001P                                       *
      * ANALISTA.....: FABIO FIGUEIREDO                                *
      * DATA.........: 22/03/2020                                      *
      * OBJETIVO.....: PROGRAMA PRINCIPAL                              *
      * ARQUIVOS.....:                                                 *
      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      *----------------------------------------------------------------*
       DATA                            DIVISION. 
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       01  WS-OPCAO                    PIC  9(001)         VALUE ZEROS.
       01  WS-CAD-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-REL-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-EXE-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-CLI-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-VEN-OPCAO                PIC  9(001)         VALUE ZEROS.
       01  WS-CONFIRMA                 PIC  X(001)         VALUE ZEROS.
       01  WS-LINHA-BRANCO             PIC  X(078)         VALUE SPACES.
       01  WS-MSG-ERRO                 PIC  X(050)         VALUE
           "FAVOR INFORMAR UMA OPCAO VALIDA".
      * INFORMACOES CLIENTES
       01  WS-CODIGO-CLI               PIC  9(007)         VALUE ZEROS.
       01  WS-CNPJ-CLI                 PIC  9(014)         VALUE ZEROS.
       01  WS-RAZAO-SOCIAL             PIC  X(040)         VALUE SPACES.
       01  WS-LATITUDE-CLI             PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-LONGITUDE-CLI            PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-NOME-ARQ-CLI             PIC  X(020)         VALUE SPACES.
      * INFORMACOES VENDEDOR
       01  WS-CODIGO-VEND              PIC  9(003)         VALUE ZEROS.
       01  WS-CPF-VEND                 PIC  9(011)         VALUE ZEROS.
       01  WS-NOME-VEND                PIC  X(040)         VALUE SPACES.
       01  WS-LATITUDE-VEND            PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-LONGITUDE-VEND           PIC S9(003)V9(008)  VALUE ZEROS.
       01  WS-NOME-ARQ-VEND            PIC  X(020)         VALUE SPACES.
      * RELATORIO CLIENTE
       01  WS-RC-TIPO-ORD              PIC  X(001)         VALUE SPACES.
       01  WS-RC-TIPO-CLA              PIC  X(001)         VALUE SPACES.
       01  WS-RC-COD-CLI               PIC  9(007)         VALUE ZEROS.
       01  WS-RC-RAZ-SOC               PIC  X(040)         VALUE SPACES.
       01  WS-RC-COD-VEND              PIC  9(003)         VALUE ZEROS.
      * RELATORIO VENDEDOR
       01  WS-RV-TIPO-ORD              PIC  X(001)         VALUE SPACES.
       01  WS-RV-TIPO-CLA              PIC  X(001)         VALUE SPACES.
       01  WS-RV-COD-VEND              PIC  9(003)         VALUE ZEROS.
       01  WS-RV-NOME-VEND             PIC  X(040)         VALUE SPACES.
      *
      *----------------------------------------------------------------*
      * AREAS DE COMUNICAÇÃO COM OUTROS PROGRAMAS                      *
      *----------------------------------------------------------------*
       01  WS-PGFJF002                 PIC  X(009)         VALUE
           'PGFJF002P'.
       01  WS-PGFJF004                 PIC  X(009)         VALUE
           'PGFJF004P'.
       01  WS-PGFJF005                 PIC  X(009)         VALUE
           'PGFJF005P'.
       01  WS-PGFJF006                 PIC  X(009)         VALUE
           'PGFJF006P'.
       01  WS-PGFJF007                 PIC  X(009)         VALUE
           'PGFJF007P'.

       COPY LNKG002L.
       COPY LNKG004L.
       COPY LNKG005L.
       COPY LNKG006L.
       COPY LNKG007L.
      *----------------------------------------------------------------*
       SCREEN                          SECTION.
      *----------------------------------------------------------------*
       01  T001-MENU.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "MENU PRINCIPAL".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "1 - CADASTROS".
           05  LINE 09 COL 10          VALUE "2 - RELATORIOS".
           05  LINE 10 COL 10          VALUE "3 - EXECUTAR".
           05  LINE 11 COL 10          VALUE "9 - SAIR DO SISTEMA".
           05  LINE 15 COL 10          VALUE 
           "INFORME A OPCAO DESEJADA E TECLE ENTER: ".
           05  LINE 15 COL 49,         PIC  9(001)
                                       TO WS-OPCAO.
                                       
       01  T002-CADASTRO.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "MENU DE CADASTROS".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "1 - CADASTRO DE CLIENTE".
           05  LINE 09 COL 10          VALUE "2 - CADASTRO DE VENDEDOR".
           05  LINE 10 COL 10          VALUE 
           "3 - VOLTAR AO MENU PRINCIPAL".
           05  LINE 11 COL 10          VALUE "9 - SAIR DO SISTEMA".
           05  LINE 15 COL 10          VALUE  
           "INFORME A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 15 COL 49,         PIC  9(001) 
                                       TO WS-CAD-OPCAO.
                                       
       01  T003-CLIENTE.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "CADASTRO DE CLIENTES".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "1 - INCLUIR CLIENTE".
           05  LINE 09 COL 10          VALUE "2 - ALTERAR CLIENTE".
           05  LINE 10 COL 10          VALUE "3 - EXCLUIR CLIENTE".
           05  LINE 11 COL 10          VALUE 
           "5 - VOLTAR AO MENU PRINCIPAL".
           05  LINE 15 COL 10          VALUE 
           "INFORME A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 15 COL 49          PIC  9(001)
                                       TO WS-CLI-OPCAO.

       01  T004-INCLUIR-CLIENTE.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "INCLUIR CLIENTE".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO CLIENTE:".
           05  LINE 08 COL 26,         PIC  ZZZZZZ9 
                                       TO WS-CODIGO-CLI.
           05  LINE 09 COL 10          VALUE "CNPJ..........:".
           05  LINE 09 COL 26,         PIC  9(014) 
                                       TO WS-CNPJ-CLI.
           05  LINE 10 COL 10          VALUE "RAZAO SOCIAL..:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       TO WS-RAZAO-SOCIAL.
           05  LINE 11 COL 10          VALUE "LATITUDE......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       TO WS-LATITUDE-CLI.
           05  LINE 12 COL 10          VALUE "LONGITUDE.....:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       TO WS-LONGITUDE-CLI.
           05  LINE 15 COL 10          VALUE 
              "CONFIRMA A INCLUSAO DO CLIENTE?(S/N):".
           05  LINE 15 COL 48,         PIC  X(001) 
                                       TO WS-CONFIRMA.

       01  T005-PESQUISAR-CLIENTE.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "PESQUISAR CLIENTE".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO CLIENTE:".
           05  LINE 08 COL 26,         PIC  ZZZZZZ9 
                                       TO WS-CODIGO-CLI.
           05  LINE 09 COL 10          VALUE "OU CNPJ.......:".
           05  LINE 09 COL 26,         PIC  9(014) 
                                       TO WS-CNPJ-CLI.
           05  LINE 15 COL 10          VALUE 
           "INFORME OS DADOS E TECLE ENTER:".

       01  T006-MODIFICAR-CLIENTE.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "MODIFICAR CLIENTE".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO CLIENTE:".
           05  LINE 08 COL 26,         PIC ZZZZZZ9 FROM WS-CODIGO-CLI.
           05  LINE 09 COL 10          VALUE "CNPJ..........:".
           05  LINE 09 COL 26,         PIC  9(014) FROM WS-CNPJ-CLI.
           05  LINE 10 COL 10          VALUE "RAZAO SOCIAL..:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       USING WS-RAZAO-SOCIAL AUTO.
           05  LINE 11 COL 10          VALUE "LATITUDE......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       USING WS-LATITUDE-CLI AUTO.
           05  LINE 12 COL 10          VALUE "LONGITUDE.....:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       USING WS-LONGITUDE-CLI AUTO.
           05  LINE 15 COL 10          VALUE 
           "CONFIRMA A ALTERACAO DO CLIENTE?(S/N):".
           05  LINE 15 COL 49,         PIC  X(001) 
                                       TO WS-CONFIRMA.

       01  T007-DELETAR-CLIENTE.
           05  BLANK SCREEN.
           05  LINE 01 COL 1           VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "DELETAR CLIENTE".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO CLIENTE:".
           05  LINE 08 COL 26,         PIC ZZZZZZ9 
                                       FROM WS-CODIGO-CLI.
           05  LINE 09 COL 10          VALUE "CNPJ..........:".
           05  LINE 09 COL 26,         PIC  9(014) 
                                       FROM WS-CNPJ-CLI.
           05  LINE 10 COL 10          VALUE "RAZAO SOCIAL..:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       FROM WS-RAZAO-SOCIAL.
           05  LINE 11 COL 10          VALUE "LATITUDE......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       FROM WS-LATITUDE-CLI.
           05  LINE 12 COL 10          VALUE "LONGITUDE.....:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       FROM WS-LONGITUDE-CLI.
           05  LINE 15 COL 10          VALUE 
           "CONFIRMA A EXCLUSAO DO CLIENTE?(S/N):".
           05  LINE 15 COL 48,         PIC  X(001) 
                                       TO WS-CONFIRMA.
                                       
       01  T008-VENDEDOR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "CADASTRO DE VENDEDORES".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "1 - INCLUIR VENDEDOR".
           05  LINE 09 COL 10          VALUE "2 - ALTERAR VENDEDOR".
           05  LINE 10 COL 10          VALUE "3 - EXCLUIR VENDEDOR".    
           05  LINE 11 COL 10          VALUE 
           "5 - VOLTAR AO MENU PRINCIPAL".
           05  LINE 15 COL 10          VALUE
           "INFORME A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 15 COL 49          PIC  9(001)
                                       TO WS-VEN-OPCAO.

       01  T009-INCLUIR-VENDEDOR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "INCLUIR VENDEDOR".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO VENDEDOR:".
           05  LINE 08 COL 26,         PIC  ZZ9 
                                       TO WS-CODIGO-VEND.
           05  LINE 09 COL 10          VALUE "CPF............:".
           05  LINE 09 COL 26,         PIC  9(011) 
                                       TO WS-CPF-VEND.
           05  LINE 10 COL 10          VALUE "NOME...........:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       TO WS-NOME-VEND.
           05  LINE 11 COL 10          VALUE "LATITUDE.......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       TO WS-LATITUDE-VEND.
           05  LINE 12 COL 10          VALUE "LONGITUDE......:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       TO WS-LONGITUDE-VEND.
           05  LINE 15 COL 10          VALUE
              "CONFIRMA A INCLUSAO DO VENDEDOR?(S/N):".
           05  LINE 15 COL 49,         PIC  X(001) 
                                       TO WS-CONFIRMA.

       01  T010-PESQUISAR-VENDEDOR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "PESQUISAR VENDEDOR".      
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO VENDEDOR:".
           05  LINE 08 COL 26,         PIC  ZZ9 
                                       TO WS-CODIGO-VEND.
           05  LINE 09 COL 10          VALUE "OU CPF.........:".
           05  LINE 09 COL 26,         PIC  9(011) 
                                       TO WS-CPF-VEND.
           05  LINE 15 COL 10          VALUE
           "INFORME OS DADOS E TECLE ENTER:".

       01  T011-MODIFICAR-VENDEDOR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "MODIFICAR VENDEDOR".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO VENDEDOR:".
           05  LINE 08 COL 26,         PIC ZZ9 FROM WS-CODIGO-VEND.
           05  LINE 09 COL 10          VALUE "CPF............:".
           05  LINE 09 COL 26,         PIC  9(011) FROM WS-CPF-VEND.
           05  LINE 10 COL 10          VALUE "NOME...........:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       USING WS-NOME-VEND AUTO.
           05  LINE 11 COL 10          VALUE "LATITUDE.......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       USING WS-LATITUDE-VEND AUTO.
           05  LINE 12 COL 10          VALUE "LONGITUDE......:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       USING WS-LONGITUDE-VEND AUTO.
           05  LINE 15 COL 10          VALUE
           "CONFIRMA A ALTERACAO DO VENDEDOR?(S/N):".
           05  LINE 15 COL 50,         PIC  X(001) 
                                       TO WS-CONFIRMA.

       01  T012-DELETAR-VENDEDOR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "DELETAR VENDEDOR".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "CODIGO VENDEDOR:".
           05  LINE 08 COL 26,         PIC ZZ9 
                                       FROM WS-CODIGO-VEND.
           05  LINE 09 COL 10          VALUE "CPF............:".
           05  LINE 09 COL 26,         PIC  9(011) 
                                       FROM WS-CPF-VEND.
           05  LINE 10 COL 10          VALUE "NOME...........:".
           05  LINE 10 COL 26,         PIC  X(040) 
                                       FROM WS-NOME-VEND.
           05  LINE 11 COL 10          VALUE "LATITUDE.......:".
           05  LINE 11 COL 26,         PIC -ZZ9,99999999 
                                       FROM WS-LATITUDE-VEND.
           05  LINE 12 COL 10          VALUE "LONGITUDE......:".
           05  LINE 12 COL 26,         PIC -ZZ9,99999999 
                                       FROM WS-LONGITUDE-VEND.
           05  LINE 15 COL 10          VALUE 
           "CONFIRMA A EXCLUSAO DO VENDEDOR?(S/N):".
           05  LINE 15 COL 49,         PIC  X(001) 
                                       TO WS-CONFIRMA.


       01  T014-RELATORIO.
           05  BLANK SCREEN.
           05 LINE 01 COL 1           VALUE
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "MENU DE RELATORIOS".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE "1 - RELATORIO DE CLIENTE".
           05  LINE 09 COL 10          VALUE 
           "2 - RELATORIO DE VENDEDOR".
           05  LINE 10 COL 10          VALUE 
           "3 - VOLTAR AO MENU PRINCIPAL".
           05  LINE 11 COL 10          VALUE "9 - SAIR DO SISTEMA".
           05  LINE 15 COL 10          VALUE 
           "INFORME A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 15 COL 49,         PIC  9(001) 
                                       TO WS-REL-OPCAO.

       01  T015-REL-CLIENTES.
           05  BLANK SCREEN.
           05  LINE 01 COL 1           VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "RELATORIO DE CLIENTES".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 06 COL 10          VALUE "TIPO DE ORDENACAO".
	       05  LINE 07 COL 10          VALUE 
           "(A)ASCENDENTE (D)DECRESCENTE.: ".
           05  LINE 07 COL 41,         PIC  X(001)
                                       TO WS-RC-TIPO-ORD.
           05  LINE 09 COL 10          VALUE "TIPO DE CLASSIFICACAO".
           05  LINE 10 COL 10          VALUE
           "(C)COD CLIENTE (R)RAZ SOCIAL.: ".
           05  LINE 10 COL 41,         PIC  X(001)
                                       TO WS-RC-TIPO-CLA.
           05  LINE 12 COL 10          VALUE "CODIGO CLIENTE.: ".
           05  LINE 12 COL 28,         PIC  ZZZZZZ9 
                                       TO WS-RC-COD-CLI.
           05  LINE 13 COL 10          VALUE "RAZAO SOCIAL...: ".
           05  LINE 13 COL 28,         PIC  X(040) 
                                       TO WS-RC-RAZ-SOC.
           05  LINE 14 COL 10          VALUE "CODIGO VENDEDOR: ".
           05  LINE 14 COL 28,         PIC  ZZ9 
                                       TO WS-RC-COD-VEND.
           05  LINE 16 COL 10          VALUE 
           "1-GERAR RELATORIO   2-VOLTAR   9-SAIR DO SISTEMA".
           05  LINE 17 COL 10          VALUE
           "INFORME A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 17 COL 49,         PIC  9(001) 
                                       TO WS-REL-OPCAO.

       01  T016-REL-VENDEDORES.
           05  BLANK SCREEN.
           05  LINE 01 COL 1           VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "RELATORIO DE VENDEDORES".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE 
           "**********************************************************".
           05  LINE 06 COL 10          VALUE "TIPO DE ORDENACAO".
	       05  LINE 07 COL 10          VALUE 
           "(A)ASCENDENTE (D)DECRESCENTE.....: ".
           05  LINE 07 COL 45,         PIC  X(001)
                                       TO WS-RV-TIPO-ORD.
           05  LINE 09 COL 10          VALUE "TIPO DE CLASSIFICACAO".
           05  LINE 10 COL 10          VALUE
           "(C)COD VENDEDOR (N)NOME VENDEDOR.: ".
           05  LINE 10 COL 45,         PIC  X(001)
                                       TO WS-RV-TIPO-CLA.
           05  LINE 12 COL 10          VALUE "CODIGO VENDEDOR: ".
           05  LINE 12 COL 28,         PIC  ZZ9 
                                       TO WS-RV-COD-VEND.
           05  LINE 13 COL 10          VALUE "NOME VENDEDOR..: ".
           05  LINE 13 COL 28,         PIC  X(040) 
                                       TO WS-RV-NOME-VEND.
           05  LINE 15 COL 10          VALUE 
           "1-GERAR RELATORIO   2-VOLTAR   9-SAIR DO SISTEMA".
           05  LINE 16 COL 10          VALUE
           "INFORME A OPCAO DESEJADA E TECLE ENTER:".
           05  LINE 16 COL 49,         PIC  9(001) 
                                       TO WS-REL-OPCAO.
                                       
       01  T017-EXECUTAR.
           05  BLANK SCREEN.
           05  LINE 01 COL 01          VALUE 
           "**********************************************************".
           05  LINE 04 COL 01          VALUE "*".
           05  LINE 04 COL 21          VALUE "MENU EXECUTAR".
           05  LINE 04 COL 58          VALUE "*".
           05  LINE 05 COL 01          VALUE
           "**********************************************************".
           05  LINE 08 COL 10          VALUE
           "1 - EXECUTAR DISTRIBUICAO DE CLIENTE".
           05  LINE 09 COL 10          VALUE 
           "2 - VOLTAR AO MENU PRINCIPAL".
           05  LINE 10 COL 10          VALUE "9 - SAIR DO SISTEMA".
           05  LINE 15 COL 10          VALUE 
           "INFORME A OPCAO DESEJADA E TECLE ENTER".
           05  LINE 15 COL 49          PIC  9(001) 
                                       TO WS-EXE-OPCAO.
       
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION.
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
       
           PERFORM 1000-INICIALIZA
           PERFORM 2000-PROCESSA
           PERFORM 9000-FINALIZA
           
           .
       0000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZAÇÃO                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZA                 SECTION.
       
            PERFORM 3000-INICIALIZA-CLIENTE
            PERFORM 4000-INICIALIZA-VENDEDOR
       
            .       
       1000-EXIT.
           EXIT.                                                        
      *----------------------------------------------------------------*
      * PROCESSAMENTO PRINCIPAL                                        *
      *----------------------------------------------------------------*
       2000-PROCESSA                   SECTION.
       
           DISPLAY T001-MENU
           ACCEPT  T001-MENU
       
           EVALUATE WS-OPCAO
               WHEN 1
                    PERFORM 2100-CADASTROS 
               WHEN 2
                    PERFORM 2200-RELATORIOS
               WHEN 3
                    PERFORM 2300-EXECUTAR
               WHEN 9
                    STOP RUN
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2000-PROCESSA
           END-EVALUATE
           
           .
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ESCOLHA DO TIPO DE CADASTRO                                    *
      *----------------------------------------------------------------*
       2100-CADASTROS                  SECTION.
       
           DISPLAY T002-CADASTRO
           ACCEPT  T002-CADASTRO
       
           EVALUATE WS-CAD-OPCAO
               WHEN 1
                    PERFORM 2110-CAD-CLIENTE
               WHEN 2
                    PERFORM 2120-CAD-VENDEDOR
               WHEN 3
                    PERFORM 1000-INICIALIZA
                    PERFORM 2000-PROCESSA
               WHEN 9
                    STOP RUN
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2100-CADASTROS
           END-EVALUATE

           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE CADASTRAMENTO DE CLIENTES                            *
      *----------------------------------------------------------------*
       2110-CAD-CLIENTE                SECTION.
       
           DISPLAY T003-CLIENTE
           ACCEPT  T003-CLIENTE
       
           EVALUATE WS-CLI-OPCAO
               WHEN 1
                    PERFORM 2111-INSERIR-CLIENTE
               WHEN 2
                    PERFORM 2112-ALTERAR-CLIENTE                        
               WHEN 3
                    PERFORM 2113-EXCLUIR-CLIENTE                       
               WHEN 9
                    PERFORM 3000-INICIALIZA-CLIENTE
                    PERFORM 2000-PROCESSA
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2110-CAD-CLIENTE
           END-EVALUATE

           .
       2110-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INSERÇÃO DE CLIENTES                                 *
      *----------------------------------------------------------------*
       2111-INSERIR-CLIENTE            SECTION.
           
           PERFORM 3000-INICIALIZA-CLIENTE
           
           DISPLAY T004-INCLUIR-CLIENTE
           ACCEPT  T004-INCLUIR-CLIENTE
       
           IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
              MOVE 1                   TO COD-FUNCAO-LNKG002
              PERFORM 5000-CARREGA-DADOS-CLIENTE                        
              PERFORM 6000-CHAMA-PGFJF002
              DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
              DISPLAY MSG-RETORNO-LNKG002
                                       AT 1820
              STOP ' '
              PERFORM 3000-INICIALIZA-CLIENTE
              PERFORM 2000-PROCESSA
           ELSE
              IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                 PERFORM 3000-INICIALIZA-CLIENTE
                 PERFORM 2110-CAD-CLIENTE
              ELSE
                 DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                 DISPLAY WS-MSG-ERRO   AT 1820
                 STOP ' '
                 PERFORM 3000-INICIALIZA-CLIENTE
                 PERFORM 2110-CAD-CLIENTE
              END-IF
           END-IF

           .
       2111-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE ALTERAÇAO DE CLIENTES                                *
      *----------------------------------------------------------------*
       2112-ALTERAR-CLIENTE            SECTION.
       
           PERFORM 3000-INICIALIZA-CLIENTE
           
           DISPLAY T005-PESQUISAR-CLIENTE
           ACCEPT  T005-PESQUISAR-CLIENTE
      
           MOVE 0                     TO COD-FUNCAO-LNKG002       
           PERFORM 5000-CARREGA-DADOS-CLIENTE
           PERFORM 6000-CHAMA-PGFJF002
            
           EVALUATE COD-RETORNO-LNKG002
               WHEN ZEROS
                    MOVE COD-CLIENTE-LNKG002  TO WS-CODIGO-CLI
                    MOVE CNPJ-LNKG002         TO WS-CNPJ-CLI
                    DISPLAY T006-MODIFICAR-CLIENTE
                    ACCEPT  T006-MODIFICAR-CLIENTE
                    IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
                       MOVE 2          TO COD-FUNCAO-LNKG002
                       PERFORM 5000-CARREGA-DADOS-CLIENTE               
                       PERFORM 6000-CHAMA-PGFJF002
                       DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                       DISPLAY MSG-RETORNO-LNKG002            
                                       AT 1820
                       STOP ' '
                       PERFORM 3000-INICIALIZA-CLIENTE           
                       PERFORM 2000-PROCESSA
                    ELSE
                       IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                          PERFORM 3000-INICIALIZA-CLIENTE
                          PERFORM 2110-CAD-CLIENTE
                       ELSE
                          DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                          DISPLAY WS-MSG-ERRO                 
                                       AT 1820
                          STOP ' '
                          PERFORM 2110-CAD-CLIENTE
                       END-IF
                    END-IF
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY MSG-RETORNO-LNKG002                       
                                       AT 1820
                    STOP ' '
                    PERFORM 3000-INICIALIZA-CLIENTE
                    PERFORM 2000-PROCESSA
           END-EVALUATE

           .
       2112-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * EXCLUSÃO DE CLIENTES                                           *
      *----------------------------------------------------------------*
       2113-EXCLUIR-CLIENTE            SECTION.
           
           PERFORM 3000-INICIALIZA-CLIENTE
           
           DISPLAY T005-PESQUISAR-CLIENTE
           ACCEPT  T005-PESQUISAR-CLIENTE
      
           MOVE 0                      TO COD-FUNCAO-LNKG002     
           PERFORM 5000-CARREGA-DADOS-CLIENTE
           PERFORM 6000-CHAMA-PGFJF002
            
           EVALUATE COD-RETORNO-LNKG002
               WHEN ZEROS
                    MOVE COD-CLIENTE-LNKG002   TO WS-CODIGO-CLI
                    MOVE CNPJ-LNKG002          TO WS-CNPJ-CLI
                    MOVE LATITUDE-CLI-LNKG002  TO WS-LATITUDE-CLI
                    MOVE LONGITUDE-CLI-LNKG002 TO WS-LONGITUDE-CLI
                    MOVE RAZAO-SOCIAL-LNKG002  TO WS-RAZAO-SOCIAL
                    DISPLAY T007-DELETAR-CLIENTE
                    ACCEPT  T007-DELETAR-CLIENTE
                    IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
                       MOVE 3          TO COD-FUNCAO-LNKG002           
                       PERFORM 6000-CHAMA-PGFJF002
                       DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                       DISPLAY MSG-RETORNO-LNKG002
                                       AT 1820
                       STOP ' '
                       PERFORM 3000-INICIALIZA-CLIENTE
                       PERFORM 2000-PROCESSA
                    ELSE
                       IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                          PERFORM 3000-INICIALIZA-CLIENTE
                          PERFORM 2110-CAD-CLIENTE
                       ELSE
                          DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                          DISPLAY WS-MSG-ERRO                 
                                       AT 1820
                          STOP ' '
                          PERFORM 2110-CAD-CLIENTE
                       END-IF
                   END-IF 
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY MSG-RETORNO-LNKG002                       
                                       AT 1820
                    STOP ' '
                    PERFORM 3000-INICIALIZA-CLIENTE
                    PERFORM 2000-PROCESSA
           END-EVALUATE

           .
       2113-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE CADASTRAMENTO DE VENDEDORES                          *
      *----------------------------------------------------------------*
       2120-CAD-VENDEDOR               SECTION.
       
           DISPLAY T008-VENDEDOR
           ACCEPT  T008-VENDEDOR
       
           EVALUATE WS-VEN-OPCAO
               WHEN 1
                    PERFORM 2121-INSERIR-VENDEDOR
               WHEN 2
                    PERFORM 2122-ALTERAR-VENDEDOR                       
               WHEN 3
                    PERFORM 2123-EXCLUIR-VENDEDOR            
               WHEN 9
                    PERFORM 4000-INICIALIZA-VENDEDOR
                    PERFORM 2000-PROCESSA
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2120-CAD-VENDEDOR
           END-EVALUATE

           .
       2120-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * INSERÇÃO DE VENDEDORES                                         *
      *----------------------------------------------------------------*
       2121-INSERIR-VENDEDOR           SECTION.
           
           PERFORM 4000-INICIALIZA-VENDEDOR
           
           DISPLAY T009-INCLUIR-VENDEDOR
           ACCEPT  T009-INCLUIR-VENDEDOR
       
           IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
              MOVE 1                   TO COD-FUNCAO-LNKG004
              PERFORM 7000-CARREGA-DADOS-VENDEDOR                 
              PERFORM 8000-CHAMA-PGFJF004
              DISPLAY WS-LINHA-BRANCO  AT 1802
              DISPLAY MSG-RETORNO-LNKG004
                                       AT 1820
              STOP ' '
              PERFORM 4000-INICIALIZA-VENDEDOR
              PERFORM 2000-PROCESSA
           ELSE
              IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                 PERFORM 4000-INICIALIZA-VENDEDOR
                 PERFORM 2120-CAD-VENDEDOR
              ELSE
                 DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                 DISPLAY WS-MSG-ERRO   AT 1820
                 STOP ' '
                 PERFORM 4000-INICIALIZA-VENDEDOR
                 PERFORM 2120-CAD-VENDEDOR
              END-IF
           END-IF

           .
       2121-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ALTERAÇAO DE VENDEDORES                                        *
      *----------------------------------------------------------------*
       2122-ALTERAR-VENDEDOR           SECTION.
       
           PERFORM 4000-INICIALIZA-VENDEDOR
           
           DISPLAY T010-PESQUISAR-VENDEDOR
           ACCEPT  T010-PESQUISAR-VENDEDOR
      
           MOVE 0                     TO COD-FUNCAO-LNKG004       
           PERFORM 7000-CARREGA-DADOS-VENDEDOR                          
           PERFORM 8000-CHAMA-PGFJF004
            
           EVALUATE COD-RETORNO-LNKG004
               WHEN ZEROS
                    MOVE COD-VENDEDOR-LNKG004  TO WS-CODIGO-VEND
                    MOVE CPF-LNKG004           TO WS-CPF-VEND
                    DISPLAY T011-MODIFICAR-VENDEDOR
                    ACCEPT  T011-MODIFICAR-VENDEDOR
                    IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
                       MOVE 2          TO COD-FUNCAO-LNKG004
                       PERFORM 7000-CARREGA-DADOS-VENDEDOR              
                       PERFORM 8000-CHAMA-PGFJF004
                       DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                       DISPLAY MSG-RETORNO-LNKG004            
                                       AT 1820
                       STOP ' '
                       PERFORM 4000-INICIALIZA-VENDEDOR
                       PERFORM 2000-PROCESSA
                    ELSE
                       IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                          PERFORM 4000-INICIALIZA-VENDEDOR
                          PERFORM 2120-CAD-VENDEDOR
                       ELSE
                          DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                          DISPLAY WS-MSG-ERRO                 
                                       AT 1820
                          STOP ' '
                          PERFORM 2120-CAD-VENDEDOR
                       END-IF
                    END-IF 
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY MSG-RETORNO-LNKG004                       
                                       AT 1820
                    STOP ' '
                    PERFORM 4000-INICIALIZA-VENDEDOR
                    PERFORM 2000-PROCESSA
           END-EVALUATE

           .
       2122-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * EXCLUSÃO DE VENDEDORES                                         *
      *----------------------------------------------------------------*
       2123-EXCLUIR-VENDEDOR           SECTION.
           
           PERFORM 4000-INICIALIZA-VENDEDOR
                     
           DISPLAY T010-PESQUISAR-VENDEDOR
           ACCEPT  T010-PESQUISAR-VENDEDOR
      
           MOVE 0                      TO COD-FUNCAO-LNKG004     
           PERFORM 7000-CARREGA-DADOS-VENDEDOR                          
           PERFORM 8000-CHAMA-PGFJF004
            
           EVALUATE COD-RETORNO-LNKG004
               WHEN ZEROS
                    MOVE COD-VENDEDOR-LNKG004  TO WS-CODIGO-VEND
                    MOVE CPF-LNKG004           TO WS-CPF-VEND
                    MOVE LATITUDE-VEN-LNKG004  TO WS-LATITUDE-VEND
                    MOVE LONGITUDE-VEN-LNKG004 TO WS-LONGITUDE-VEND
                    MOVE NOME-VENDEDOR-LNKG004 TO WS-NOME-VEND
                    DISPLAY T012-DELETAR-VENDEDOR
                    ACCEPT  T012-DELETAR-VENDEDOR
                    IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'S'
                       MOVE 3          TO COD-FUNCAO-LNKG004           
                       PERFORM 8000-CHAMA-PGFJF004
                       DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                       DISPLAY MSG-RETORNO-LNKG004
                                       AT 1820
                       STOP ' '
                       PERFORM 4000-INICIALIZA-VENDEDOR
                       PERFORM 2000-PROCESSA
                    ELSE
                       IF FUNCTION UPPER-CASE(WS-CONFIRMA)
                                       EQUAL 'N'
                          PERFORM 4000-INICIALIZA-VENDEDOR
                          PERFORM 2120-CAD-VENDEDOR
                       ELSE
                          DISPLAY WS-LINHA-BRANCO
                                       AT 1802
                          DISPLAY WS-MSG-ERRO                 
                                       AT 1820
                          STOP ' '
                          PERFORM 2120-CAD-VENDEDOR
                       END-IF
                    END-IF 
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY MSG-RETORNO-LNKG004                       
                                       AT 1820
                    STOP ' '
                    PERFORM 4000-INICIALIZA-VENDEDOR
                    PERFORM 2000-PROCESSA
           END-EVALUATE

           .
       2123-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE RELATORIOS                                           *
      *----------------------------------------------------------------*
       2200-RELATORIOS                 SECTION.
       
           DISPLAY T014-RELATORIO
           ACCEPT  T014-RELATORIO
       
           EVALUATE WS-REL-OPCAO
               WHEN 1
                    PERFORM 2210-RELAT-CLIENTE 
               WHEN 2
                    PERFORM 2220-RELAT-VENDEDOR
               WHEN 3
                    PERFORM 2000-PROCESSA
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2000-PROCESSA
           END-EVALUATE

           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DO RELATORIO DE CLIENTES                             *
      *----------------------------------------------------------------*
       2210-RELAT-CLIENTE              SECTION.
       
           MOVE ZEROS                  TO WS-RC-COD-CLI
                                          WS-RC-COD-VEND
           MOVE SPACES                 TO WS-RC-TIPO-ORD
                                          WS-RC-TIPO-CLA
                                          WS-RC-RAZ-SOC

           DISPLAY T015-REL-CLIENTES
           ACCEPT  T015-REL-CLIENTES

           EVALUATE WS-REL-OPCAO
               WHEN 1
                    PERFORM 2211-CHAMA-PGFJF005
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1902
                    DISPLAY MSG-RETORNO-LNKG005L
                                       AT 1920
                    STOP ' '
                    PERFORM 2210-RELAT-CLIENTE
               WHEN 2
                    PERFORM 2200-RELATORIOS
               WHEN 9
                    STOP RUN
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1802
                    DISPLAY WS-MSG-ERRO
                                       AT 1820
                    STOP ' '
                    PERFORM 2210-RELAT-CLIENTE
           END-EVALUATE

           .
       2210-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    GERACAO DO RELATORIO DE CLIENTES                            *
      *----------------------------------------------------------------*
       2211-CHAMA-PGFJF005             SECTION.

           MOVE WS-RC-TIPO-ORD       TO TIPO-ORD-LNKG005 
           MOVE WS-RC-TIPO-CLA       TO TIPO-CLA-LNKG005
           MOVE WS-RC-COD-CLI        TO COD-CLI-LNKG005
           MOVE WS-RC-RAZ-SOC        TO RAZ-SOC-LNKG005
           MOVE WS-RC-COD-VEND       TO COD-VEND-LNKG005
           
           CALL WS-PGFJF005          USING LNKG005L

           .
       2211-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DO RELATORIO DE VENDEDORES                           *
      *----------------------------------------------------------------*
       2220-RELAT-VENDEDOR             SECTION.
       
           MOVE ZEROS                  TO WS-RV-COD-VEND
           MOVE SPACES                 TO WS-RV-TIPO-ORD
                                          WS-RV-TIPO-CLA
                                          WS-RV-NOME-VEND

           DISPLAY T016-REL-VENDEDORES
           ACCEPT  T016-REL-VENDEDORES
               
           EVALUATE WS-REL-OPCAO
               WHEN 1
                    PERFORM 2211-CHAMA-PGFJF006
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1902
                    DISPLAY MSG-RETORNO-LNKG006L
                                       AT 1920
                    STOP ' '
                    PERFORM 2220-RELAT-VENDEDOR
               WHEN 2
                    PERFORM 2200-RELATORIOS
               WHEN 9
                    STOP RUN
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1902
                    DISPLAY WS-MSG-ERRO
                                       AT 1920
                    STOP ' '
                    PERFORM 2220-RELAT-VENDEDOR
           END-EVALUATE

           .
       2220-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    ROTINA DO GERACAO DO RELATORIO DE VENDEDORES                *
      *----------------------------------------------------------------*
       2211-CHAMA-PGFJF006             SECTION.

           MOVE WS-RV-TIPO-ORD       TO TIPO-ORD-LNKG006
           MOVE WS-RV-TIPO-CLA       TO TIPO-CLA-LNKG006
           MOVE WS-RV-COD-VEND       TO COD-VEND-LNKG006
           MOVE WS-RV-NOME-VEND      TO NOME-VEND-LNKG006

           CALL WS-PGFJF006            USING LNKG006L

           .
       2221-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE EXECUÇÃO                                             *
      *----------------------------------------------------------------*
       2300-EXECUTAR                   SECTION.
       
           DISPLAY T017-EXECUTAR
           ACCEPT  T017-EXECUTAR
       
           EVALUATE WS-EXE-OPCAO
               WHEN 1
                    PERFORM 2310-CHAMA-PGFJF007
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1902
                    DISPLAY MSG-RETORNO-LNKG007
                                       AT 1920
                    STOP ' '
                    PERFORM 2300-EXECUTAR
               WHEN 2
                    PERFORM 2000-PROCESSA
               WHEN 9
                    STOP RUN
               WHEN OTHER
                    DISPLAY WS-LINHA-BRANCO     
                                       AT 1902
                    DISPLAY WS-MSG-ERRO
                                       AT 1920
                    STOP ' '
                    PERFORM 2300-EXECUTAR
           END-EVALUATE

           .
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      *    EFETUA DISTRIBUICAO CLIENTE X VENDEDOR                      *
      *----------------------------------------------------------------*
       2310-CHAMA-PGFJF007             SECTION.

           CALL WS-PGFJF007            USING LNKG007L

           .
       2310-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * INICIALIZAÇÃO DE VARIAVEIS CLIENTE                             *
      *----------------------------------------------------------------*
       3000-INICIALIZA-CLIENTE           SECTION.
       
           MOVE ZEROS                  TO WS-CODIGO-CLI            
                                          WS-CNPJ-CLI                 
                                          WS-LATITUDE-CLI
                                          WS-LONGITUDE-CLI
           MOVE SPACES                 TO WS-RAZAO-SOCIAL

           .     
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * INICIALIZAÇÃO DE VARIAVEIS VENDEDOR                            *
      *----------------------------------------------------------------*
       4000-INICIALIZA-VENDEDOR           SECTION.
       
           MOVE ZEROS                  TO WS-CODIGO-VEND               
                                          WS-CPF-VEND                 
                                          WS-LATITUDE-VEND
                                          WS-LONGITUDE-VEND
           MOVE SPACES                 TO WS-NOME-VEND

           .     
       4000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CARREGAMENTO DE DADOS DO CLIENTE                               *
      *----------------------------------------------------------------*
       5000-CARREGA-DADOS-CLIENTE      SECTION.                         
           
           MOVE WS-CODIGO-CLI          TO COD-CLIENTE-LNKG002
           MOVE WS-CNPJ-CLI            TO CNPJ-LNKG002
           MOVE WS-LATITUDE-CLI        TO LATITUDE-CLI-LNKG002
           MOVE WS-LONGITUDE-CLI       TO LONGITUDE-CLI-LNKG002
           MOVE WS-RAZAO-SOCIAL        TO RAZAO-SOCIAL-LNKG002
           MOVE WS-NOME-ARQ-CLI        TO NOME-ARQ-CLI-LNKG002
     
           .
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ACESSAR PROGRAMA PGFJF002P                                     *
      *----------------------------------------------------------------*
       6000-CHAMA-PGFJF002             SECTION.

           CALL WS-PGFJF002            USING LNKG002L. 
           
           . 
       6000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * CARREGAMENTO DE DADOS VENDEDOR                                 *
      *----------------------------------------------------------------*
       7000-CARREGA-DADOS-VENDEDOR     SECTION.                         
           
           MOVE WS-CODIGO-VEND         TO COD-VENDEDOR-LNKG003
           MOVE WS-CPF-VEND            TO CPF-LNKG003
           MOVE WS-LATITUDE-VEND       TO LATITUDE-VEN-LNKG003
           MOVE WS-LONGITUDE-VEND      TO LONGITUDE-VEN-LNKG003
           MOVE WS-NOME-VEND           TO NOME-VENDEDOR-LNKG003
           MOVE WS-NOME-ARQ-VEND       TO NOME-ARQ-VEN-LNKG003
     
           .
       7000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA PARA ACESSAR PROGRAMA PGFJF004P                         *
      *----------------------------------------------------------------*
       8000-CHAMA-PGFJF004             SECTION.

           CALL WS-PGFJF004            USING LNKG004L. 
           
           . 
       8000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZAÇÃO                                          *
      *----------------------------------------------------------------*
       9000-FINALIZA                   SECTION.

           GOBACK.       
     
       9000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FIM DO PROGRAMA PGFJF001P                                      *
      *----------------------------------------------------------------*
       END PROGRAM                     PGFJF001P.
      *----------------------------------------------------------------*