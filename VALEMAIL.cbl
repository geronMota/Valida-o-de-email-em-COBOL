      ******************************************************************
      * Author:JEFEFRSON MOTA(GERO)
      * Date:26/03/23
      * Purpose:VALIDACAO DE EMAIL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. VALEMAIL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 VERIFICA                              PIC X.
        88 VERIFICA-OK                          VALUE "S" FALSE "N".
       77 WS-NB-CHAR                            PIC 9(10).
       77 WS-ARROBA                             PIC 9(1).
       77 WS-DOMINIO                            PIC 9(1).


       77 WS-USER-EMAIL                         PIC X(25).
       PROCEDURE DIVISION.

           PERFORM UNTIL VERIFICA-OK

      *
           MOVE 0 TO WS-NB-CHAR
           DISPLAY "CADASTRE O EMAIL DO USUARIO"
           ACCEPT WS-USER-EMAIL

           DISPLAY"==================================================="

           INSPECT WS-USER-EMAIL TALLYING WS-ARROBA FOR ALL "@"
                           WS-NB-CHAR FOR CHARACTERS BEFORE "@"
            WS-DOMINIO FOR ALL "hotmail.com" ALL "gmail.com"

           DISPLAY"==================================================="
           IF WS-ARROBA EQUAL 1
               DISPLAY "arroba :"WS-ARROBA
               CONTINUE
           ELSE
           DISPLAY"DIGITE O EMAIL COM @ "
                  "EX.antoniocar@gmail.com"
           END-IF
           DISPLAY"==================================================="
           IF WS-NB-CHAR > 9
               DISPLAY "CHAR 10: "WS-NB-CHAR
             CONTINUE
           ELSE
               DISPLAY "O EMAIL DEVE CONTER NO MINIMO 10 CARACTERES"
           END-IF
           DISPLAY"==================================================="
           IF WS-DOMINIO EQUAL 1
               DISPLAY "DOMINIO: " WS-DOMINIO
               CONTINUE
           ELSE
               DISPLAY "O DOMINIO DEVERA SER "
                     "@hotmail.com ou @gmail.com"
           END-IF
           DISPLAY"==================================================="
           IF WS-ARROBA EQUAL 1 AND WS-NB-CHAR >=10 AND WS-DOMINIO = 1
               SET VERIFICA-OK  TO TRUE
               EXIT PERFORM
            ELSE
            DISPLAY "DIGITE O FORMATO CORRETO EX.antoniocar@gmail.com"

                     SET VERIFICA-OK TO FALSE
                     MOVE ZEROS TO WS-ARROBA
                     MOVE ZEROS TO WS-NB-CHAR
            END-IF
           END-PERFORM
           DISPLAY"===================================================".
           DISPLAY "VERIFICA @ :" WS-ARROBA
           DISPLAY "VERIFICA CARACTERES ANTES DO @ :" WS-NB-CHAR
           DISPLAY "VERIFICA DOMINIO :" WS-DOMINIO
            STOP RUN.
       END PROGRAM VALEMAIL.
