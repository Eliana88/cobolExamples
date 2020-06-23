      ******************************************************************
      * Author: Eliana
      * Date: 26-05-2020
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. REEMPLAZO-CARACTERES.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VARIABLES.
        03 WS-ENTRADA         PIC X(50) VALUE 'JULIAN & PABLO& ROSSANA'.
        03 WS-SALIDA          PIC X(80) VALUE SPACES.
        03 WS-I               PIC 9(2)  VALUE ZEROES.
        03 WS-J               PIC 9(2)  VALUE 1.


       PROCEDURE DIVISION.

           PERFORM VARYING WS-I FROM 1 BY 1
           UNTIL WS-I = LENGTH OF WS-ENTRADA
               EVALUATE WS-ENTRADA(WS-I:1)
                   WHEN '&'
                       MOVE 'AND' TO WS-SALIDA(WS-J:)
                       ADD 3 TO WS-J
                   WHEN OTHER
                       MOVE WS-ENTRADA(WS-I:1) TO WS-SALIDA(WS-J:)
                       ADD 1 TO WS-J
                   END-EVALUATE
           END-PERFORM.

           DISPLAY WS-SALIDA.


      *Otra opción.

           STRING WS-ENTRADA(1:6) ' AND ' WS-ENTRADA(10:5) ' AND '
            WS-ENTRADA(17:7)
           DELIMITED BY SIZE INTO WS-SALIDA.

           DISPLAY WS-SALIDA.


           STOP RUN.

       END PROGRAM REEMPLAZO-CARACTERES.
