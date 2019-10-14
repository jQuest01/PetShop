       IDENTIFICATION DIVISION.
       PROGRAM-ID. PETSHOP.
       AUTHOR. GUILHERME CAMPOS FERREIRA DA CRUZ.
      **************************************
      * MANUTENCAO DO CADASTRO DE PET SHOP *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADPET ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                            WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADPET
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPET.DAT".
       01 REGPET.
                03 COD	           PIC 9(05).
                03 NOME            PIC X(30).
		03 IDADE	   PIC 9(03).
                03 RACA            PIC X(30).
                03 PESO            PIC 9(02)V99.
                03 UNDSEXO         PIC X(01).
		03 SEXO		   PIC X(05).
		03 TPSERV	   PIC 9(01).
		03 SERVICO	   PIC X(12).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(05) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
      *
       01 TABSRV.
	  03 SRV0  PIC X(12) VALUE "BANHO       ".
	  03 SRV1  PIC X(12) VALUE "TOSA        ".
	  03 SRV2  PIC X(12) VALUE "BANHO E TOSA".
      *
       01 TAB0 REDEFINES TABSRV.
           03 TABSERV PIC X(12) OCCURS 3 TIMES.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELAPET.
           05  BLANK SCREEN.
           05  LINE 04  COLUMN 01 
               VALUE  "                          CADASTRO PARA".
           05  LINE 04  COLUMN 41 
               VALUE  "PETSHOP".
           05  LINE 05  COLUMN 01 
               VALUE  "  SENHA :".
           05  LINE 06  COLUMN 01 
               VALUE  "  NOME  :".
           05  LINE 07  COLUMN 01 
               VALUE  "  IDADE :".
           05  LINE 08  COLUMN 01 
               VALUE  "  RA€A  :".
           05  LINE 09  COLUMN 01 
               VALUE  "  PESO  :".
           05  LINE 10  COLUMN 01 
               VALUE  "  SEXO  :     -".
           05  LINE 11  COLUMN 01 
               VALUE  "  SERVICO:     -".
           05  SENHA
               LINE 05  COLUMN 11  PIC 9(05)
               USING  COD
               HIGHLIGHT.
           05  T-NOME
               LINE 06  COLUMN 11  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TIDD
               LINE 07  COLUMN 11  PIC 9(03)
               USING  IDADE
               HIGHLIGHT.
           05  T-RACA
               LINE 08  COLUMN 11  PIC X(30)
               USING  RACA
               HIGHLIGHT.
           05  T-PESO
               LINE 09  COLUMN 11  PIC Z9,99
               USING  PESO
               HIGHLIGHT.
           05  TEX
               LINE 10  COLUMN 11  PIC X(01)
               USING  UNDSEXO
               HIGHLIGHT.
           05  SEX
               LINE 10  COLUMN 19  PIC X(05)
               USING  SEXO
               HIGHLIGHT.
           05  TPSRV
               LINE 11  COLUMN 12  PIC 9(01)
               USING  TPSERV
               HIGHLIGHT.
           05  SRV
               LINE 11  COLUMN 20  PIC X(12)
               USING  SERVICO
               HIGHLIGHT.
           05  MEN
               LINE 19  COLUMN 25  PIC X(50)
               USING  MENS
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O CADPET
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CADPET
                      CLOSE CADPET
                      MOVE "*** ARQUIVO CADPET SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS  TO COD IDADE PESO TPSERV 
                MOVE SPACES TO NOME RACA UNDSEXO SEXO
                               SERVICO
                DISPLAY TELAPET.
       INC-002.
                ACCEPT SENHA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CADPET
                   GO TO ROT-FIM.
                IF COD  = 0
                   MOVE "*** PET INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-PRODUTO01.
                MOVE 0 TO W-SEL
                READ CADPET
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TELAPET
                      MOVE "*** PET JA CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ. PRODUTO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT T-NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF NOME = SPACES
                   MOVE "*** INVALIDO ***" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS2
		   GO TO INC-003.
       INC-004.
                ACCEPT TIDD
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-003.
                IF IDADE = 0
                   MOVE "*** INVALIDO ***" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS2
		   GO TO INC-004.
       INC-005.
                ACCEPT T-RACA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-004.
                IF RACA = SPACES
                   MOVE "*** INVALIDO ***" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS2
		   GO TO INC-005.
       INC-006.
                ACCEPT T-PESO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF PESO = 0
                   MOVE "*** INVALIDO ***" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS2
		   GO TO INC-006.
       INC-007.
                ACCEPT TEX
                ACCEPT W-ACT FROM ESCAPE KEY
		IF W-ACT = 02 GO TO INC-006.
                IF UNDSEXO = SPACES
                   MOVE "*** INVALIDO ***" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS2
		   GO TO INC-007.
		IF UNDSEXO = "M" OR UNDSEXO = "m"
		   MOVE "MACHO" TO SEXO
		   DISPLAY TELAPET
		ELSE
		   IF UNDSEXO = "F" OR UNDSEXO = "f"
		      MOVE "FEMEA" TO SEXO
		      DISPLAY TELAPET
		   ELSE
		      MOVE "*** INVALIDO ***" TO MENS
		      PERFORM ROT-MENS THRU ROT-MENS2
		      GO TO INC-007.
       INC-008.
                ACCEPT TPSRV
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT= 02 GO TO INC-007.
                IF TPSERV = ZEROS
                   MOVE "*** INVALIDO ***" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS2
		   GO TO INC-008.
		MOVE TABSERV(TPSERV) TO SERVICO
		IF SERVICO = SPACES
                   MOVE "*** INVALIDO ***" TO MENS
		   PERFORM ROT-MENS THRU ROT-MENS2
		   GO TO INC-008.
		DISPLAY TELAPET
      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGPET
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
                IF ST-ERRO = "22"
                      MOVE "*** PRODUTO JA EXISTE ***     " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADPET RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGPET
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY TELAPET
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY TELAPET.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
		   MOVE SPACES TO MENS
                   DISPLAY TELAPET.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
