       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     PGFJF006P.                       
      *----------------------------------------------------------------* 
      * PROGRAMA.....: PGFJF006P                                       *
      * ANALISTA.....: FABIO FIGUEIREDO                                *
      * DATA.........: 22/03/2020                                      *
      * OBJETIVO.....: REALIZAR RELATORIO DE VENDEDORES                *
      * ARQUIVOS.....: ARQ-VENDEDOR - ARQUIVO DE VENDEDORES            *
      *                ARQ-SORT     - ARQUIVO DE SORT                  *
      *                REL-VENDEDOR - ARQUIVO DE RELATORIO             *
      *                CSV-VENDEDOR - ARQUIVO DE REL CSV               *
      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *----------------------------------------------------------------*
       INPUT-OUTPUT                    SECTION.
      *----------------------------------------------------------------*
       FILE-CONTROL.
      *----------------------------------------------------------------*
      *     
           SELECT ARQ-VENDEDOR       ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD  KEY IS COD-VENDEDOR
                       ALTERNATE RECORD KEY IS CPF
                       ALTERNATE RECORD KEY IS NOME-VENDEDOR
                                  LOCK MODE IS MANUAL
                                FILE STATUS IS WS-FL-STATUS-VEND.

           SELECT ARQ-SORT           ASSIGN TO "SORTVENDEDOR.TMP"
                                FILE STATUS IS WS-FL-STATUS-SORT.

           SELECT REL-VENDEDOR       ASSIGN TO "RELVENDEDOR.TXT"
                               ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS WS-FL-STATUS-REL.

           SELECT CSV-VENDEDOR       ASSIGN TO "RELVENDEDOR.CSV"
                               ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS WS-FL-STATUS-CSV.

      *----------------------------------------------------------------*
       DATA                            DIVISION. 
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'ArqVendedor'.
       COPY "VENDEDOR.CPY".

       SD  ARQ-SORT.
       01  REG-SORT.
           05  SORT-COD-VEND           PIC  9(003).
           05  SORT-CPF-VEND           PIC  9(011).
           05  SORT-NOME-VEND          PIC  X(040).
           05  SORT-LATITUDE           PIC S9(003)V9(008).
           05  SORT-LONGITUDE          PIC S9(003)V9(008).
       
       FD  REL-VENDEDOR.
       01  REG-REL-VENDEDOR            PIC X(105).

       FD  CSV-VENDEDOR.
       01  REG-CSV-VENDEDOR            PIC X(100).
       
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  WS-FL-STATUS-VEND           PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-SORT           PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-REL            PIC  X(002)         VALUE "00".     
       77  WS-FL-STATUS-CSV            PIC  X(002)         VALUE "00".                
      *
       01  WS-ARQ-VENDEDOR.
           05  WS-ARQ-CODIGO-VEND      PIC  9(003)         VALUE ZEROS.
           05  WS-ARQ-CPF-VEND         PIC  9(011)         VALUE ZEROS.
           05  WS-NOME-VEND            PIC  X(040)         VALUE SPACES.     
           05  WS-ARQ-LATITUDE-VEND    PIC S9(003)V9(008)  VALUE ZEROS.
           05  WS-ARQ-LONGITUDE-VEND   PIC S9(003)V9(008)  VALUE ZEROS.
           
      *----------------------------------------------------------------*
      * AREA DE DECLARACAO DO LAY-OUT DO RELATORIO                     *
      *----------------------------------------------------------------*
       01  CAB1.
           03 FILLER                   PIC  X(105)         VALUE
              "  RELATORIO DE VENDEDORES".

       01  CAB2.
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 FILLER                   PIC  X(012)         VALUE 
              "COD VENDEDOR".
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 FILLER                   PIC  X(003)         VALUE 
              "CPF".
           03 FILLER                   PIC  X(010)         VALUE SPACES.
           03 FILLER                   PIC  X(013)         VALUE 
              "NOME VENDEDOR".
           03 FILLER                   PIC  X(029)         VALUE SPACES.
           03 FILLER                   PIC  X(008)         VALUE 
              "LATITUDE".
           03 FILLER                   PIC  X(007)         VALUE SPACES.
           03 FILLER                   PIC  X(009)         VALUE 
              "LONGITUDE".
           03 FILLER                   PIC  X(010)         VALUE SPACES.

       01  DET.
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 DET-COD-VEND             PIC  9(003)         VALUE ZEROS.
           03 FILLER                   PIC  X(011)         VALUE SPACES.
           03 DET-CPF-VEND             PIC  9(011)         VALUE ZEROS.
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 DET-NOME-VEND            PIC  X(040)         VALUE SPACES.
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 DET-LATITUDE             PIC +ZZ9,99999999   VALUE ZEROS.
           03 FILLER                   PIC  X(002)         VALUE SPACES.
           03 DET-LONGITUDE            PIC +ZZ9,99999999   VALUE ZEROS.
           03 FILLER                   PIC  X(006)         VALUE SPACES.

       01  LINHA-TRACO.
           03 FILLER                   PIC  X(105)         VALUE 
              ALL "-".

       01  LINHA-BRANCO.
           03 FILLER                   PIC  X(105)         VALUE 
              ALL SPACES.

      *----------------------------------------------------------------*
      * AREA DE DECLARACAO DO LAY-OUT DO RELATORIO CSV                 *
      *----------------------------------------------------------------*
       01  CAB-CSV                     PIC  X(100)         VALUE
           "COD VEND;CNPJ;NOME VEND;LATITUDE;LONGITUDE;".

       01  DET-CSV.
           03 CSV-COD-VEND             PIC  9(003)         VALUE ZEROS.
           03 FILLER                   PIC  X(001)         VALUE ";".
           03 CSV-CPF-VEND             PIC  9(011)         VALUE ZEROS.
           03 FILLER                   PIC  X(001)         VALUE ";".
           03 CSV-NOME-VEND            PIC  X(040)         VALUE SPACES.
           03 FILLER                   PIC  X(001)         VALUE ";".
           03 CSV-LATITUDE             PIC +ZZ9,99999999   VALUE ZEROS.
           03 FILLER                   PIC  X(001)         VALUE ";".
           03 CSV-LONGITUDE            PIC +ZZ9,99999999   VALUE ZEROS.

      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       COPY LNKG006L.    
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION USING LNKG006L.        
      *----------------------------------------------------------------*
       0000-PRINCIPAL                  SECTION.
       
           PERFORM 1000-INICIALIZA
           PERFORM 2000-PROCESSA
           PERFORM 3000-FINALIZA
           
           .
       0000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INICIALIZAÇÃO                                        *
      *----------------------------------------------------------------*
       1000-INICIALIZA                 SECTION.
           
           MOVE ZEROS                  TO COD-RETORNO-LNKG006
           MOVE "RELATORIO GERADO COM SUCESSO"
                                       TO MSG-RETORNO-LNKG006

           PERFORM 1100-VALIDA-PREENCHIMENTO

           IF COD-VEND-LNKG006       NOT EQUAL ZEROS  AND
              NOME-VEND-LNKG006      NOT EQUAL SPACES
              PERFORM 1200-BUSCAR-VENDEDOR
           END-IF

           PERFORM 1300-ABRE-ARQUIVOS 

           .
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE VALIDACAO DO PRRENCHIMENTO DOS CAMPOS DA TELA        *
      *----------------------------------------------------------------*
       1100-VALIDA-PREENCHIMENTO       SECTION.

           IF TIPO-ORD-LNKG006       NOT EQUAL "A" AND
              TIPO-ORD-LNKG006       NOT EQUAL "D"           
              MOVE 1                   TO COD-RETORNO-LNKG006
              MOVE "TIPO DE ORDENACAO COM VALOR NAO PERMITIDO"
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           IF TIPO-CLA-LNKG006       NOT EQUAL "C" AND
              TIPO-CLA-LNKG006       NOT EQUAL "N"
              MOVE 2                   TO COD-RETORNO-LNKG006
              MOVE "TIPO DE CLASSIFICACAO COM VALOR NAO PERMITIDO"
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           IF COD-VEND-LNKG006       NOT NUMERIC
              MOVE 3                   TO COD-RETORNO-LNKG006
              MOVE "COD VENDEDOR COM VALOR NAO PERMITIDO"
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           .
       1100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE BUSCA DE VENDEDOR                                    *
      *----------------------------------------------------------------*
       1200-BUSCAR-VENDEDOR            SECTION.
   
           PERFORM 9100-OPEN-ARQ-VENDEDOR
                              
           MOVE COD-VEND-LNKG006     TO COD-VENDEDOR

           READ ARQ-VENDEDOR    RECORD INTO WS-ARQ-VENDEDOR
                                   KEY IS COD-VENDEDOR
           
           IF WS-FL-STATUS-VEND        NOT EQUAL ZEROS
              IF NOME-VEND-LNKG006   NOT EQUAL SPACES
                 MOVE NOME-VEND-LNKG006
                                       TO NOME-VENDEDOR
                 READ ARQ-VENDEDOR 
                                RECORD INTO WS-ARQ-VENDEDOR
                                   KEY IS NOME-VENDEDOR

                   IF WS-FL-STATUS-VEND NOT EQUAL ZEROS
                       MOVE 5          TO COD-RETORNO-LNKG006
                       MOVE "VENDEDOR NAO ENCONTRADO NA BASE"
                                       TO MSG-RETORNO-LNKG006
                   END-IF
               ELSE
                   MOVE 6              TO COD-RETORNO-LNKG006
                   MOVE "VENDEDOR NAO ENCONTRADO NA BASE"
                                       TO MSG-RETORNO-LNKG006
               END-IF
           END-IF
           
           PERFORM 9200-CLOSE-ARQ-VENDEDOR

           .
       1200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DOS ARQUIVOS                                *
      *----------------------------------------------------------------*
       1300-ABRE-ARQUIVOS              SECTION.

           PERFORM 9100-OPEN-ARQ-VENDEDOR           

           OPEN OUTPUT REL-VENDEDOR
           
           IF WS-FL-STATUS-REL         NOT EQUAL ZEROS
              MOVE 9                   TO COD-RETORNO-LNKG006
              MOVE "ERRO NA ABERTURA DO ARQUIVO REL-VENDEDOR" 
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           OPEN OUTPUT CSV-VENDEDOR
           
           IF WS-FL-STATUS-CSV         NOT EQUAL ZEROS
              MOVE 9                   TO COD-RETORNO-LNKG006
              MOVE "ERRO NA ABERTURA DO ARQUIVO CSV-VENDEDOR" 
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           .
       1300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO                                        *
      *----------------------------------------------------------------*
       2000-PROCESSA                   SECTION.

           IF  TIPO-ORD-LNKG006      EQUAL "A"
               IF  TIPO-CLA-LNKG006  EQUAL "C"
                   SORT ARQ-SORT
                   ON ASCENDING    KEY SORT-COD-VEND
                   INPUT PROCEDURE  IS 2100-INPUT-SORT
                   OUTPUT PROCEDURE IS 2200-OUTPUT-SORT
               ELSE
                   SORT ARQ-SORT
                   ON ASCENDING    KEY SORT-NOME-VEND
                   INPUT PROCEDURE  IS 2100-INPUT-SORT
                   OUTPUT PROCEDURE IS 2200-OUTPUT-SORT
               END-IF
           ELSE
               IF  TIPO-CLA-LNKG006  EQUAL "C"
                   SORT ARQ-SORT
                   ON DESCENDING   KEY SORT-COD-VEND
                   INPUT PROCEDURE  IS 2100-INPUT-SORT
                   OUTPUT PROCEDURE IS 2200-OUTPUT-SORT
               ELSE
                   SORT ARQ-SORT
                   ON DESCENDING   KEY SORT-NOME-VEND               
                   INPUT PROCEDURE  IS 2100-INPUT-SORT
                   OUTPUT PROCEDURE IS 2200-OUTPUT-SORT
               END-IF
           END-IF
       
           .
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE INPUT DO SORT                                        *
      *----------------------------------------------------------------*
       2100-INPUT-SORT                 SECTION.

           READ ARQ-VENDEDOR NEXT
           
           IF  WS-FL-STATUS-VEND       NOT EQUAL ZEROS
           AND WS-FL-STATUS-VEND       NOT EQUAL "10"
              MOVE 10                  TO COD-RETORNO-LNKG006
              MOVE "ERRO NA LEITURA DO ARQUIVO ARQ-VENDEDOR" 
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           PERFORM UNTIL WS-FL-STATUS-VEND
                                       NOT EQUAL "00"
                   PERFORM 2110-ALIMENTA-SORT
           END-PERFORM             
             
           PERFORM 9200-CLOSE-ARQ-VENDEDOR
       
           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE ALIMENTACAO DO SORT                                  *
      *----------------------------------------------------------------*
       2110-ALIMENTA-SORT              SECTION.

           IF COD-VEND-LNKG006       EQUAL ZEROS
              IF NOME-VEND-LNKG006   EQUAL SPACES
                 RELEASE REG-SORT      FROM ARQ-VENDEDOR
              ELSE
                 IF NOME-VENDEDOR    EQUAL NOME-VEND-LNKG006
                    RELEASE REG-SORT   FROM ARQ-VENDEDOR
                 END-IF
              END-IF
           ELSE
              IF COD-VENDEDOR EQUAL COD-VEND-LNKG006
                 RELEASE REG-SORT      FROM ARQ-VENDEDOR
              END-IF
           END-IF    
       
           READ ARQ-VENDEDOR NEXT

           IF  WS-FL-STATUS-VEND       NOT EQUAL ZEROS
           AND WS-FL-STATUS-VEND       NOT EQUAL "10"
              MOVE 11                  TO COD-RETORNO-LNKG006
              MOVE "ERRO NA LEITURA DO ARQUIVO ARQ-VENDEDOR" 
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           .
       2110-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA OUTPUT DO SORT                                          *
      *----------------------------------------------------------------*
       2200-OUTPUT-SORT                SECTION.
       
           RETURN ARQ-SORT

           WRITE REG-REL-VENDEDOR      FROM LINHA-TRACO
           WRITE REG-REL-VENDEDOR      FROM CAB1
           WRITE REG-REL-VENDEDOR      FROM LINHA-BRANCO
           WRITE REG-REL-VENDEDOR      FROM CAB2
           WRITE REG-REL-VENDEDOR      FROM LINHA-TRACO

           WRITE REG-CSV-VENDEDOR      FROM CAB-CSV

           PERFORM UNTIL WS-FL-STATUS-SORT
                                       NOT EQUAL "00"
               PERFORM 2210-GERA-RELATORIO 
           END-PERFORM             
           
           CLOSE REL-VENDEDOR
           
           IF WS-FL-STATUS-REL         NOT EQUAL ZEROS
              MOVE 14                  TO COD-RETORNO-LNKG006
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO DE RELATORIO" 
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           CLOSE CSV-VENDEDOR
           
           IF WS-FL-STATUS-CSV         NOT EQUAL ZEROS
              MOVE 14                  TO COD-RETORNO-LNKG006
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO DE RELATORIO CSV" 
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE GERACAO DO RELATORIO DE VENDEDORES                   *
      *----------------------------------------------------------------*
       2210-GERA-RELATORIO             SECTION.

           MOVE SORT-COD-VEND          TO DET-COD-VEND
                                          CSV-COD-VEND
           MOVE SORT-CPF-VEND          TO DET-CPF-VEND
                                          CSV-CPF-VEND
           MOVE SORT-NOME-VEND         TO DET-NOME-VEND
                                          CSV-NOME-VEND
           MOVE SORT-LATITUDE          TO DET-LATITUDE
                                          CSV-LATITUDE
           MOVE SORT-LONGITUDE         TO DET-LONGITUDE               
                                          CSV-LONGITUDE 

           WRITE REG-REL-VENDEDOR      FROM DET

           IF WS-FL-STATUS-REL         NOT EQUAL ZEROS
              MOVE 13                  TO COD-RETORNO-LNKG006
              MOVE "ERRO AO GRAVAR DETALHE NO REL DE VENDEDORES" 
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           WRITE REG-CSV-VENDEDOR      FROM DET-CSV

           IF WS-FL-STATUS-CSV         NOT EQUAL ZEROS
              MOVE 13                  TO COD-RETORNO-LNKG006
              MOVE "ERRO AO GRAVAR DETALHE NO REL DE VENDEDORES CSV" 
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           RETURN ARQ-SORT

           .
       2210-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZAÇÃO                                          *
      *----------------------------------------------------------------*
       3000-FINALIZA                   SECTION.

           GOBACK
     
           .            
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE ABERTURA DO ARQUIVO DE VENDEDORES                    *
      *----------------------------------------------------------------*
       9100-OPEN-ARQ-VENDEDOR          SECTION.

           OPEN INPUT ARQ-VENDEDOR
           
           IF WS-FL-STATUS-VEND        NOT EQUAL ZEROS
              MOVE 9                   TO COD-RETORNO-LNKG006
              MOVE "ERRO NA ABERTURA DO ARQUIVO VENDEDOR - 6" 
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           .
       9100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE FECHAMENTO DO ARQUIVO DE VENDEDORES                  *
      *----------------------------------------------------------------*
       9200-CLOSE-ARQ-VENDEDOR         SECTION.

           CLOSE ARQ-VENDEDOR                                         
           
           IF WS-FL-STATUS-VEND        NOT EQUAL ZEROS
              MOVE 9                   TO COD-RETORNO-LNKG006
              MOVE "ERRO NA ABERTURA DO ARQUIVO ARQ-VENDEDOR" 
                                       TO MSG-RETORNO-LNKG006
              PERFORM 3000-FINALIZA
           END-IF

           .
       9200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FIM DO PROGRAMA PGFJF006P                                      *
      *----------------------------------------------------------------*
       END PROGRAM                     PGFJF006P.
      *----------------------------------------------------------------*