       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     PGFJF007P.                       
      *----------------------------------------------------------------* 
      * PROGRAMA.....: PGFJF007P                                       *
      * ANALISTA.....: FABIO FIGUEIREDO                                *
      * DATA.........: 22/03/2020                                    *
      * OBJETIVO.....: REALIZAR DISTRIBUIÇÃO DE CLIENTES X VENDEDORES  *
      * ARQUIVOS.....:                                                 *
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
           SELECT ARQ-CLIENTE        ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD  KEY IS COD-CLIENTE
                       ALTERNATE RECORD KEY IS CNPJ
                       ALTERNATE RECORD KEY IS RAZAO-SOCIAL
                                  LOCK MODE IS MANUAL
                                FILE STATUS IS WS-FL-STATUS-CLI.

           SELECT ARQ-VENDEDOR       ASSIGN TO DISK
                               ORGANIZATION IS INDEXED
                                ACCESS MODE IS DYNAMIC
                                RECORD  KEY IS COD-VENDEDOR
                       ALTERNATE RECORD KEY IS CPF
                       ALTERNATE RECORD KEY IS NOME-VENDEDOR
                                  LOCK MODE IS MANUAL
                                FILE STATUS IS WS-FL-STATUS-VEN.

           SELECT ARQ-DISTRIB   ASSIGN TO DISK
                               ORGANIZATION IS LINE SEQUENTIAL
                                FILE STATUS IS WS-FL-STATUS-DIS.
                                
      *----------------------------------------------------------------*
       DATA                            DIVISION. 
      *----------------------------------------------------------------*
       FILE                            SECTION.
      *----------------------------------------------------------------*
       FD  ARQ-CLIENTE
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'ArqCliente'.
       COPY "CLIENTES.CPY".
       
       FD  ARQ-VENDEDOR
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'ArqVendedor'.
       COPY "VENDEDOR.CPY".
       
       FD  ARQ-DISTRIB
           LABEL RECORD IS STANDARD
           VALUE OF FILE-ID IS 'ArqDistribuicao'.
       COPY "DISTRIB.CPY".
       
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       77  WS-FL-STATUS-CLI            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-VEN            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-DIS            PIC  X(002)         VALUE "00".
       77  WS-FL-STATUS-REL            PIC  X(002)         VALUE "00".

       01  WS-MENOR-DISTANCIA          PIC  9(009)V9(002)  VALUE       
           999999999.
       01  WS-CALC-DISTANCIA           PIC  9(009)V9(002)  VALUE ZEROS.
       01  WS-LAT-CLI                  PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-LAT-VEN                  PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-LON-CLI                  PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-LON-VEN                  PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-DLA                      PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-DLO                      PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-A                        PIC S9(003)V9(008)  VALUE ZEROS. 
       01  WS-C                        PIC S9(003)V9(008)  VALUE ZEROS.
      
      *----------------------------------------------------------------*
      * AREAS DE COMUNICAÇÃO COM OUTROS PROGRAMAS                      *
      *----------------------------------------------------------------*
       01  WS-PGFJF008                 PIC  X(009)         VALUE
           'PGFJF008P'.
           
       COPY PGFJF008L.
      
      *----------------------------------------------------------------*
       LINKAGE                         SECTION.
      *----------------------------------------------------------------*
       COPY HBSIS007L.    
      *----------------------------------------------------------------*
       PROCEDURE                       DIVISION USING HBSIS007L.
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
       
           MOVE ZEROS                  TO COD-RETORNO-LNKG007
           MOVE "DISTRIBUICAO REALIZADA COM SUCESSO" 
                                       TO MSG-RETORNO-LNKG007
           
           .
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO                                        *
      *----------------------------------------------------------------*
       2000-PROCESSA                   SECTION.
           
           PERFORM 2100-OPEN-ARQ-CLIENTE
           PERFORM 2200-OPEN-ARQ-DISTRIB
           
           PERFORM 2300-LER-ARQ-CLIENTE
           
           PERFORM 2400-TRATA-CLIENTE UNTIL 
                   WS-FL-STATUS-CLI   NOT EQUAL "00"
           
           PERFORM 2500-CLOSE-ARQ-CLIENTE
           PERFORM 2700-CLOSE-ARQ-DISTRIB
           
           PERFORM 2800-GERA-RELATORIO
           
           .
       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ABERTURA DE ARQUIVO CLIENTE                                    *
      *----------------------------------------------------------------*
       2100-OPEN-ARQ-CLIENTE           SECTION.

           OPEN INPUT ARQ-CLIENTE                                       
           
           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-LNKG007
              MOVE "ERRO NA ABERTURA DO ARQUIVO DE CLIENTE" 
                                       TO MSG-RETORNO-LNKG007
              PERFORM 3000-FINALIZA
           END-IF

           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ABERTURA DE ARQUIVO DISTRIBUICAO                               *
      *----------------------------------------------------------------*
       2200-OPEN-ARQ-DISTRIB      SECTION.

           OPEN OUTPUT ARQ-DISTRIB                                 
           
           IF WS-FL-STATUS-DIS         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-LNKG007
              MOVE "ERRO NA ABERTURA DO ARQUIVO DE DISTRIBUICAO" 
                                       TO MSG-RETORNO-LNKG007
              PERFORM 3000-FINALIZA
           END-IF

           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * LEITURA DE ARQUIVO CLIENTE                                     *
      *----------------------------------------------------------------*
       2300-LER-ARQ-CLIENTE            SECTION.

           READ ARQ-CLIENTE NEXT                                        
           
           IF WS-FL-STATUS-CLI         EQUAL ZEROS OR '10' 
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-LNKG007
              MOVE "ERRO NA LEITURA DO ARQUIVO DE CLIENTE" 
                                       TO MSG-RETORNO-LNKG007
              PERFORM 3000-FINALIZA
           END-IF

           .
       2300-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * EFETUA DISTRIBUICAO                                            *
      *----------------------------------------------------------------*
       2400-TRATA-CLIENTE              SECTION.
       
           MOVE COD-CLIENTE  TO COD-CLIENTE-DISTRIB
           MOVE RAZAO-SOCIAL TO RAZAO-SOCIAL-DISTRIB

           PERFORM 2410-OPEN-ARQ-VENDEDOR
           PERFORM 2420-LER-ARQ-VENDEDOR
           
           PERFORM 2430-TRATA-VENDEDOR UNTIL WS-FL-STATUS-VEN           
                                       NOT EQUAL "00"
           
           MOVE  WS-MENOR-DISTANCIA    TO DISTANCIA-DISTRIB
           MOVE  999999999             TO WS-MENOR-DISTANCIA            
           WRITE ARQ-DISTRIB
           
           PERFORM 2600-CLOSE-ARQ-VENDEDOR
           
           PERFORM 2300-LER-ARQ-CLIENTE.

       2400-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ABERTURA DE ARQUIVO VENDEDOR                                   *
      *----------------------------------------------------------------*
       2410-OPEN-ARQ-VENDEDOR      SECTION.

           OPEN INPUT ARQ-VENDEDOR                                      
           
           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-LNKG007
              MOVE "ERRO NA ABERTURA DO ARQUIVO DE VENDEDOR - 7" 
                                       TO MSG-RETORNO-LNKG007
              PERFORM 3000-FINALIZA
           END-IF

           .
       2410-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * LEITURA DE ARQUIVO VENDEDOR                                    *
      *----------------------------------------------------------------*
       2420-LER-ARQ-VENDEDOR            SECTION.

           READ ARQ-VENDEDOR NEXT                                       
           
           IF WS-FL-STATUS-VEN         EQUAL ZEROS OR '10' 
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-LNKG007
              MOVE "ERRO NA LEITURA DO ARQUIVO DE CLIENTE" 
                                       TO MSG-RETORNO-LNKG007
              PERFORM 3000-FINALIZA
           END-IF

           .
       2420-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * TRATA VENDEDOR                                                 *
      *----------------------------------------------------------------*
       2430-TRATA-VENDEDOR             SECTION.
       
           COMPUTE WS-LAT-CLI = LATITUDE-CLI
                              * FUNCTION PI
                              / 180
       
           COMPUTE WS-LAT-VEN = LATITUDE-VEND                 
                              * FUNCTION PI
                              / 180

           COMPUTE WS-LON-CLI = LONGITUDE-CLI                 
                              * FUNCTION PI
                              / 180
                              
           COMPUTE WS-LON-VEN = LONGITUDE-VEND                
                              * FUNCTION PI
                              / 180

           COMPUTE WS-DLA = WS-LAT-VEN - (WS-LAT-CLI) 

           COMPUTE WS-DLO = WS-LON-VEN - (WS-LON-CLI)                   

           COMPUTE WS-A = FUNCTION SIN(WS-DLA / 2)
                        * FUNCTION SIN(WS-DLA / 2)
                        + FUNCTION COS(WS-LAT-CLI)
                        * FUNCTION COS(WS-LAT-VEN)
                        * FUNCTION SIN(WS-DLO / 2)
                        * FUNCTION SIN(WS-DLO / 2)
           
           COMPUTE WS-C = 2 * FUNCTION ATAN(FUNCTION SQRT(WS-A) /
                                            FUNCTION SQRT(1 - WS-A))

           COMPUTE WS-CALC-DISTANCIA = 6731 * WS-C * 1000     
                                                    
           IF WS-CALC-DISTANCIA         LESS WS-MENOR-DISTANCIA         
              MOVE WS-CALC-DISTANCIA    TO WS-MENOR-DISTANCIA           
              MOVE COD-VENDEDOR         TO COD-VENDEDOR-DISTRIB
              MOVE NOME-VENDEDOR        TO NOME-VEND-DISTRIB
           END-IF
           
           PERFORM 2420-LER-ARQ-VENDEDOR
           
           .
       2430-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FECHAMENTO DE ARQUIVO                                          *
      *----------------------------------------------------------------*
       2500-CLOSE-ARQ-CLIENTE          SECTION.

           CLOSE ARQ-CLIENTE                                            
           
           IF WS-FL-STATUS-CLI         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-LNKG007
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO CLIENTE" 
                                       TO MSG-RETORNO-LNKG007
              PERFORM 3000-FINALIZA
           END-IF

           .
       2500-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FECHAMENTO DE ARQUIVO                                          *
      *----------------------------------------------------------------*
       2600-CLOSE-ARQ-VENDEDOR         SECTION.

           CLOSE ARQ-VENDEDOR                                           
           
           IF WS-FL-STATUS-VEN         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-LNKG007
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO VENDEDOR" 
                                       TO MSG-RETORNO-LNKG007
              PERFORM 3000-FINALIZA
           END-IF

           .
       2600-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FECHAMENTO DE ARQUIVO                                          *
      *----------------------------------------------------------------*
       2700-CLOSE-ARQ-DISTRIB     SECTION.

           CLOSE ARQ-DISTRIB                                       
           
           IF WS-FL-STATUS-DIS         EQUAL ZEROS
              CONTINUE
           ELSE
              MOVE 9                   TO COD-RETORNO-LNKG007
              MOVE "ERRO NO FECHAMENTO DO ARQUIVO DISTRIBUICAO" 
                                       TO MSG-RETORNO-LNKG007
              PERFORM 3000-FINALIZA
           END-IF

           .
       2700-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * GERA RELATORIO                                                 *
      *----------------------------------------------------------------*
       2800-GERA-RELATORIO             SECTION.

           CALL WS-PGFJF008            USING PGFJF008L                  
           
           .
       2800-EXIT.
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
      * FIM DO PROGRAMA PGFJF007P                                      *
      *----------------------------------------------------------------*
       END PROGRAM                     PGFJF007P.
      *----------------------------------------------------------------*
