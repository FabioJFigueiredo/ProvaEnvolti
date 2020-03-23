       IDENTIFICATION                  DIVISION.
      *----------------------------------------------------------------*
       PROGRAM-ID.                     PGFJF003P.                       
      *----------------------------------------------------------------* 
      * PROGRAMA.....: PGFJF003P                                       *
      * ANALISTA.....: FABIO FIGUEIREDO                                *
      * DATA.........: 22/03/2020                                      *
      * OBJETIVO.....: REALIZAR VALIDAÇÃO CPF/CNPJ                     *
      * ARQUIVOS.....: N/A                                             *
      *----------------------------------------------------------------*
       ENVIRONMENT                     DIVISION.
      *----------------------------------------------------------------*
       CONFIGURATION                   SECTION.
      *----------------------------------------------------------------*
       SPECIAL-NAMES.
           DECIMAL-POINT               IS COMMA.
      *----------------------------------------------------------------*
       DATA                            DIVISION. 
      *----------------------------------------------------------------*

      *-----------------------------------------------------------------
      *  PROGRAMA      : CADDIGIT
      *  OBJETIVO      : VERIFICA O DIGITO DO CPF CNPJ OU PIS/PSASEP
      *  ANALISTA      : CARLOS ALBERTO DORNELLES
      *  LINGUAGEM     : COBOL
      *  MODO OPERACAO : BATCH
      *  COMO USAR     : LKS-NUMERO-I ....: NUMERO INFORMADO
      *                : LKS-NUMERO-F ....: NUMERO CALCULADO
      *                : LKS-TIPO-CALCULO : CPF, CGC OU PIS
      *                : LKS-ACAO ........: C - CALCULA
      *                                     V - VERIFICA
      *-----------------------------------------------------------------
      *  VERSAO DD.MM.AAAA  HISTORICO/AUTOR
      *  ------  ---------- ---------------
      *    001  24.09.2004  PROGRAMA INICIAL
      *-----------------------------------------------------------------
      *----------------------------------------------------------------*
       WORKING-STORAGE                 SECTION.
      *----------------------------------------------------------------*
       01  WS-AUXILIARES.
           05 WSS-IND-N                  PIC 9(002)  VALUE ZEROES.
           05 WSS-IND-O                  PIC 9(002)  VALUE ZEROES.
           05 WSS-IND-P                  PIC 9(002)  VALUE ZEROES.
           05 WSS-SOMA                   PIC 9(008)  VALUE ZEROES.
           05 WSS-NUMERO                 PIC 9(015)  VALUE ZEROES.
           05 WSS-NUMERO-R               REDEFINES WSS-NUMERO.
              10  WSS-NUMERO-T           PIC 9(001)  OCCURS 15 TIMES.
           05 WSS-PESOS                  PIC X(028)  VALUE SPACES.
           05 WSS-PESOS-R                REDEFINES WSS-PESOS.
              10  WSS-PESOS-T            PIC 9(002)  OCCURS 14 TIMES.
           05 WSS-QUOCI                  PIC 9(008)  VALUE ZEROES.
           05 WSS-RESTO                  PIC 9(008)  VALUE ZEROES.
           05 WSS-MENSAGEM               PIC X(078)  VALUE SPACES.
           05 WSS-PESOS-CPF              PIC X(028)  VALUE
                                   '0000000011100908070605040302'.
           05 WSS-PESOS-CGC              PIC X(028)  VALUE
                                   '0706050403020908070605040302'.
      *-----------------------------------------------------------------
       LINKAGE                         SECTION.
      *-----------------------------------------------------------------
       COPY LNKG003L. 
      *-----------------------------------------------------------------
       PROCEDURE                       DIVISION USING LNKG003L.
      *-----------------------------------------------------------------
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
      
           MOVE ZEROS                  TO COD-RETORNO-LNKG003  
           MOVE CPF-CNPJ-LNKG003      TO WSS-NUMERO
           
           .
       1000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE PROCESSAMENTO                                        *
      *----------------------------------------------------------------*
       2000-PROCESSA                   SECTION.

           EVALUATE COD-FUNCAO-LNKG003
               WHEN 1
                    PERFORM 2100-VALIDA-CPF 
               WHEN 2
                    PERFORM 2200-VALIDA-CNPJ
               WHEN OTHER
                    MOVE 9             TO COD-RETORNO-LNKG003
                    MOVE 'CODIGO DA FUNCAO INVALIDA'
                                       TO MSG-RETORNO-LNKG003
                    PERFORM 9000-FINALIZA
           END-EVALUATE.

       2000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * VALIDAR CPF                                                    *
      *----------------------------------------------------------------*
       2100-VALIDA-CPF                 SECTION.
      
           MOVE WSS-PESOS-CPF          TO WSS-PESOS
           MOVE 05                     TO WSS-IND-N
           MOVE 06                     TO WSS-IND-P
           MOVE 13                     TO WSS-IND-O
           MOVE ZEROS                  TO WSS-SOMA
           PERFORM 3000-VAL-DIGITO-01 

           MOVE 05                     TO WSS-IND-N
           MOVE 05                     TO WSS-IND-P
           MOVE 14                     TO WSS-IND-O
           MOVE ZEROS                  TO WSS-SOMA
           PERFORM 4000-VAL-DIGITO-02
           PERFORM 5000-VALIDACAO-FINAL
                              
           .
       2100-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * VALIDAR CNPJ                                                   *
      *----------------------------------------------------------------*
       2200-VALIDA-CNPJ                SECTION.

           MOVE WSS-PESOS-CGC          TO WSS-PESOS
           MOVE 01                     TO WSS-IND-N
           MOVE 02                     TO WSS-IND-P
           MOVE 13                     TO WSS-IND-O
           MOVE ZEROES                 TO WSS-SOMA
           PERFORM 3000-VAL-DIGITO-01 

           MOVE 01                     TO WSS-IND-N
           MOVE 01                     TO WSS-IND-P
           MOVE 14                     TO WSS-IND-O
           MOVE ZEROES                 TO WSS-SOMA
           PERFORM 4000-VAL-DIGITO-02 
           PERFORM 5000-VALIDACAO-FINAL
                              
           .
       2200-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * VALIDAR DIGITO 1                                               *
      *----------------------------------------------------------------*
       3000-VAL-DIGITO-01              SECTION.

           MOVE ZEROS                  TO WSS-SOMA
           
           PERFORM UNTIL WSS-IND-N     GREATER WSS-IND-O
                 COMPUTE WSS-SOMA = WSS-SOMA +
                                   (WSS-NUMERO-T (WSS-IND-N) *
                                    WSS-PESOS-T  (WSS-IND-P))
                 ADD 1                 TO WSS-IND-N
                                          WSS-IND-P
           END-PERFORM
           
           DIVIDE WSS-SOMA BY 11 GIVING WSS-QUOCI REMAINDER WSS-RESTO
           
           IF WSS-RESTO                EQUAL 0 OR 1
              MOVE ZEROS               TO WSS-NUMERO-T (14)
           ELSE
              SUBTRACT WSS-RESTO FROM 11 GIVING WSS-NUMERO-T (14)
           END-IF.

           . 
       3000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * VALIDAR DIGITO 2                                               *
      *----------------------------------------------------------------*
       4000-VAL-DIGITO-02              SECTION.
      
           MOVE ZEROS                  TO WSS-SOMA
           
           PERFORM UNTIL WSS-IND-N     GREATER WSS-IND-O
                 COMPUTE WSS-SOMA = WSS-SOMA +
                                   (WSS-NUMERO-T (WSS-IND-N) *
                                    WSS-PESOS-T  (WSS-IND-P))
                 ADD 1                 TO WSS-IND-N
                                          WSS-IND-P
           END-PERFORM
           
           DIVIDE WSS-SOMA BY 11 GIVING WSS-QUOCI REMAINDER WSS-RESTO
           
           IF WSS-RESTO                EQUAL 0 OR 1
              MOVE ZEROES TO WSS-NUMERO-T (15)
           ELSE
              SUBTRACT WSS-RESTO FROM 11 GIVING WSS-NUMERO-T (15)
           END-IF

           . 
       4000-FIM.
           EXIT.
      *----------------------------------------------------------------*
      * VALIDAR DIGITO 2                                               *
      *----------------------------------------------------------------*
       5000-VALIDACAO-FINAL            SECTION.
      
           IF CPF-CNPJ-LNKG003        EQUAL WSS-NUMERO      
              MOVE 0                   TO COD-RETORNO-LNKG003          
           ELSE                                    
              MOVE 3                   TO COD-RETORNO-LNKG003
              IF COD-FUNCAO-LNKG003   EQUAL 1
                 MOVE 'CPF INVALIDO'   TO MSG-RETORNO-LNKG003
              ELSE
                 MOVE 'CNPJ INVALIDO'  TO MSG-RETORNO-LNKG003
              END-IF
           END-IF                                  
           
           .
       5000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * ROTINA DE FINALIZAÇÃO                                          *
      *----------------------------------------------------------------*
       9000-FINALIZA                   SECTION.

           GOBACK.       
     
       9000-EXIT.
           EXIT.
      *----------------------------------------------------------------*
      * FIM DO PROGRAMA PGFJF003P                                      *
      *----------------------------------------------------------------*
       END PROGRAM                     PGFJF003P.
      *----------------------------------------------------------------*