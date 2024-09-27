000100 IDENTIFICATION DIVISION.
       PROGRAM-ID. PSNETS33.
       AUTHOR. LUIS F VELASQUEZ T.
       DATE-WRITTEN. 06/09/2024.
       DATE-COMPILED.
      * PROGRAMA DE CONSULTA DE NOTAS POR ESTUDIANTE  20/09/2024
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
       INPUT-OUTPUT SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * COPY AREAS PARA FECHAS
      * COPY UFECHAS.
       01  WS-NOMBRES-MESES.
           03 WS-NOM-MESES
                PIC X(36) VALUE
             "ENEFEBMARABRMAYJUNJULAGOSEPOCTNOVDIC".
           03 WS-NOM-MES REDEFINES WS-NOM-MESES
                PIC XXX OCCURS 12 TIMES INDEXED BY IX-MES.
       01  WS-CURRENT-DATE-FIELDS.
           05 WS-CURRENT-DATE.
             10 WS-CURRENT-YEAR     PIC 9999.
             10 WS-CURRENT-MONTH    PIC 99.
             10 WS-CURRENT-DAY      PIC 99.
           05 WS-CURRENT-TIME.
             10 WS-CURRENT-HH       PIC 99.
             10 WS-CURRENT-MM       PIC 99.
             10 WS-CURRENT-SS       PIC 99.
             10 WS-CURRENT-CC       PIC 99.
       01  WS-FECHA-AAAA-MM-DD.
             10 WS-AAAA             PIC 9999.
             10 FILLER              PIC X VALUE "-".
             10 WS-MM               PIC 99.
             10 FILLER              PIC X VALUE "-".
             10 WS-DD               PIC 99.
       01  WS-HORA-MINUTOS-SEGUNDOS.
             10 WS-HH-F1            PIC 99.
             10 FILLER              PIC X VALUE ".".
             10 WS-MM-F1            PIC 99.
             10 FILLER              PIC X VALUE ":".
             10 WS-SS-F1            PIC 99.
             10 FILLER              PIC X VALUE ":".
             10 WS-CC-F1            PIC 99.
       01  WS-FECHA-DD-MMM-AA.
             10 WS-DD-F2            PIC 99.
             10 FILLER              PIC X VALUE "-".
             10 WS-MM-F2            PIC XXX.
             10 FILLER              PIC X VALUE "-".
             10 WS-AAAA-F2          PIC 9999.

      * COPY MSNNE03 MAPA DEL PROGRAMA DE CONSULTA DE NOTAS
        COPY MSNNE03.
      *  COPY MSNNE03R.
      *  MSNNE03R REDEFINE MSNNE03 ADECUA LINEAS DE SCROLL A OCCURS
       01  MSNNE03OR REDEFINES MSNNE03O.
           05  FILLER PIC X(397).
           05  MLINEA OCCURS 5 TIMES INDEXED BY IX-LINEA.
               07 MCODCUR.
                  10 MCODESP    PICTURE X(3).
                  10 MCODCOL    PICTURE X.
                  10 MCODPRO    PICTURE X.
                  10 MCODHIG    PICTURE X.
                  10 MVODVVV    PICTURE X.
                  10 MCODUND    PICTURE X.
                  10 MCODIGO    PIC X(9).
               07 MDESCRI.
                  10 MDESESP    PICTURE X(3).
                  10 MDESCOL    PICTURE X.
                  10 MDESPRO    PICTURE X.
                  10 MDESHIG    PICTURE X.
                  10 MDESVVV    PICTURE X.
                  10 MDESUND    PICTURE X.
                  10 MDESCUR    PICTURE X(25).
               07 MAAACRS.
                  10 MAAAESP    PICTURE X(3).
                  10 MAAACOL    PICTURE X.
                  10 MAAAPRO    PICTURE X.
                  10 MAAAHIG    PICTURE X.
                  10 MAAAVVV    PICTURE X.
                  10 MAAAUND    PICTURE X.
                  10 MAAACUR    PICTURE X(4).

               07 MNOTCRS.
                  10 MNOTESP    PICTURE X(3).
                  10 MNOTCOL    PICTURE X.
                  10 MNOTPRO    PICTURE X.
                  10 MNOTHIG    PICTURE X.
                  10 MNOTVVV    PICTURE X.
                  10 MNOTUND    PICTURE X.
                  10 MNOTCUR    PICTURE X(4).

       01 DATOS-PRUEBA.
             05 W-VARIOS-PRUEBA.
                10 W-DNI-BUSCADO PIC X(9).
                10 W-DNI-HALLADO PIC X VALUE "N".
                10 W-IND-FROM   PIC 9999.
                10 W-IND-TO     PIC 9999.
                10 W-IND-LISTA  PIC 9999.
                10 W-IND-VISOR  PIC 9999.
                10 W-M          PIC 9(4) VALUE 5.
                10 W-EDI-NOTA   PIC X(4).
                10 W-EDI-NOTA-R REDEFINES W-EDI-NOTA PIC 999V9.
                10 W-EDI-NOTA-N PIC ZZ.9.
             05 DNI-DNI OCCURS 6 TIMES INDEXED BY IX-DNI.
                07 DNI-ESTUD  PIC X(9).
                07 DNI-NOMEST PIC X(25).
                07 DNI-APEEST PIC X(25).
                07 DNI-FROM   PIC 9(4).
                07 DNI-TO     PIC 9(4).
             05 PRUEBA-NOTAS.
                07 FILLER PIC X(15) VALUE "OS390NO01202325".
                07 FILLER PIC X(15) VALUE "GITHBNO02202432".
                07 FILLER PIC X(15) VALUE "OSZSENO03202341".
                07 FILLER PIC X(15) VALUE "CODB2NO04202446".
                07 FILLER PIC X(15) VALUE "CODB2NO05202432".
                07 FILLER PIC X(15) VALUE "OSZSENO06202441".
                07 FILLER PIC X(15) VALUE "CCICSNO07202447".
                07 FILLER PIC X(15) VALUE "OS390NO08202441".
                07 FILLER PIC X(15) VALUE "GITHBNO09202335".
                07 FILLER PIC X(15) VALUE "OSZSENO10202346".
                07 FILLER PIC X(15) VALUE "OS390NO11202331".
                07 FILLER PIC X(15) VALUE "GITHBNO12202435".
                07 FILLER PIC X(15) VALUE "CODB2NO13202432".
                07 FILLER PIC X(15) VALUE "OSZSENO14202426".
                07 FILLER PIC X(15) VALUE "CCICSNO15202447".
                07 FILLER PIC X(15) VALUE "OS390NO16202335".
                07 FILLER PIC X(15) VALUE "CODB2NO17202326".
                07 FILLER PIC X(15) VALUE "CODB2NO18202431".
                07 FILLER PIC X(15) VALUE "CODB2NO19202340".
                07 FILLER PIC X(15) VALUE "CODB2NO20202350".
             05 NOTAS-ESTUD REDEFINES PRUEBA-NOTAS.
                07 NOTAEST OCCURS 20 TIMES INDEXED BY IX-NOTA.
                   09 CURSO PIC XXXXX.
                   09 NNOTA PIC XXXX.
                   09 AAAA  PIC XXXX.
                   09 NOTA  PIC 9V9.
      * COPY WSDEBDIS.
       01  WS-SW-DEBUG.
           05 W-SW-DEBUG    PIC X VALUE "S".
       01  WS-DEBDIS-MSG.
           05 W-TIME-DIS    PIC X(11).
           05 FILLER        PIC X VALUE SPACES.
           05 W-PARRAFO     PIC X(10) VALUE SPACES.
           05 FILLER        PIC X VALUE SPACES.
           05 W-CAMPO       PIC X(10) VALUE SPACES.
           05 FILLER        PIC X VALUE "=".
           05 W-VALOR       PIC X(20).
           05 FILLER        PIC X VALUE SPACES.
           05 W-CAMPO1      PIC X(10) VALUE SPACES.
           05 FILLER        PIC X VALUE "=".
           05 W-VALOR1      PIC X(20).
           05 FILLER        PIC X VALUE SPACES.
           05 W-FECHA-DIS   PIC X(10).

      * AYUDAS DE BMS
       COPY DFHAID.
       COPY DFHBMSCA.
       01 MI-COMMAREA PIC X(100).
       01 MI-COMMAREA-RED REDEFINES MI-COMMAREA.
          03 CA-TRANS  PIC XXXX.
          03 CA-TERM   PIC XXXX.
          03 CA-DNI    PIC X(9).
          03 CA-RESTO  PIC X(83).
       01 MI-COMMAREA-OTHERS.
          03 CA-LEN    PIC 9(4).
       01 MENSAJES.
          03 MSG-FIN-SESION PIC X(18) VALUE "AU REVOIR MON AMI!".
          03 MSG    PIC X(50) VALUE SPACES.
       LINKAGE SECTION.
       01 DFHCOMMAREA PIC X(100).

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INICIO-DE-PROGRAMA
           PERFORM 2000-MANEJO-DE-CICLOS-DE-CICS
           PERFORM 9999-FIN-DE-PROGRAMA
           .
       1000-INICIO-DE-PROGRAMA.
           MOVE "-> 1000-INICIO--" TO W-PARRAFO
           MOVE "EIBCALEN" TO W-CAMPO
           MOVE EIBCALEN TO W-VALOR
           MOVE LOW-VALUES TO MSNNE03O
           PERFORM 9100-DEB-DIS
           PERFORM 3500-GENERA-NOTAS-DE-PRUEBA
           .
       2000-MANEJO-DE-CICLOS-DE-CICS.
           IF EIBCALEN = 0 THEN
              PERFORM 3100-CARGA-DATOS-PARA-MAPA
              PERFORM 7000-ENVIO-MAPA
              PERFORM 7800-RETORNO-TRANSACCION
           ELSE
              MOVE DFHCOMMAREA TO MI-COMMAREA
              PERFORM 7100-RECIBO-DE-MAPA
           END-IF
           .
       3000-6999-TODOS-PARRAFOS-PROGR.
           CONTINUE
           .
       3100-CARGA-DATOS-PARA-MAPA.
           PERFORM 3100-GET-DATES
           MOVE WS-FECHA-AAAA-MM-DD    TO MFECHAO
           MOVE "-> 3100-CARGA-DATOS-MAPA--" TO W-PARRAFO
           MOVE "EIBCALEN" TO W-CAMPO
           MOVE EIBCALEN TO W-VALOR
           PERFORM 9100-DEB-DIS
           MOVE "000000001" TO MDNIESO
           MOVE "NOMBRE ESTUDIANTE 1" TO MNOMESO
           MOVE "APELLIDOS ESTUDIANTE 1" TO MAPEESO
           MOVE W-M TO W-IND-TO
           MOVE 1 TO W-IND-FROM

           IF W-IND-TO NOT > W-IND-FROM + W-M THEN
              MOVE 1 TO W-IND-VISOR
              MOVE W-IND-FROM TO W-IND-LISTA
              PERFORM 3600-MUEVE-LISTA-A-VISOR
                  UNTIL W-IND-LISTA > W-IND-TO
           END-IF

      *    MOVE "--FORMATO INICIAL--" TO MTXMSGO
           .
      * COPY RUTINA FORMATO DE FECHAS
      * COPY UGETDATE.
       3100-GET-DATES.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS
           MOVE WS-CURRENT-YEAR       TO WS-AAAA
           MOVE WS-CURRENT-MONTH      TO WS-MM
           MOVE WS-CURRENT-DAY        TO WS-DD
           MOVE WS-CURRENT-YEAR       TO WS-AAAA-F2
           MOVE WS-CURRENT-DAY        TO WS-DD-F2
           SET IX-MES TO WS-CURRENT-MONTH
           MOVE WS-NOM-MES (IX-MES) TO WS-MM-F2
           MOVE WS-CURRENT-HH         TO WS-HH-F1
           MOVE WS-CURRENT-MM         TO WS-MM-F1
           MOVE WS-CURRENT-SS         TO WS-SS-F1
           MOVE WS-CURRENT-CC         TO WS-CC-F1
           .

       3200-CONSULTA-NOTAS.
           MOVE "--3200-CONSULTA-NOTAS--" TO W-PARRAFO
           MOVE "EIBCALEN" TO W-CAMPO
           MOVE EIBCALEN TO W-VALOR
           MOVE "MDNIESI" TO W-CAMPO1 MOVE MDNIESI TO W-VALOR1
           PERFORM 9100-DEB-DIS

           PERFORM 3100-GET-DATES
           MOVE WS-FECHA-AAAA-MM-DD    TO MFECHAO
           MOVE "**NO REGISTRADO **" TO MNOMESO MAPEESO
           MOVE "N" TO W-DNI-HALLADO
           SET IX-DNI TO 1
           SEARCH  DNI-DNI
                  AT END MOVE "N" TO W-DNI-HALLADO
                  WHEN DNI-ESTUD (IX-DNI) = W-DNI-BUSCADO
                       MOVE "S" TO W-DNI-HALLADO
                       MOVE DNI-NOMEST (IX-DNI) TO MNOMESO
                       MOVE DNI-APEEST (IX-DNI) TO MAPEESO
                       MOVE DNI-FROM (IX-DNI) TO W-IND-FROM
                       MOVE DNI-TO (IX-DNI) TO W-IND-TO
           END-SEARCH
           MOVE 1 TO W-IND-VISOR
           PERFORM 3650-BORRAR-VISOR
                   UNTIL W-IND-VISOR > W-M
           IF W-IND-TO NOT > W-IND-FROM + W-M
              AND W-DNI-HALLADO = "S" THEN
                  MOVE 1 TO W-IND-VISOR
                  MOVE W-IND-FROM TO W-IND-LISTA
                  PERFORM 3600-MUEVE-LISTA-A-VISOR
                       UNTIL W-IND-LISTA > W-IND-TO
           END-IF
           PERFORM 7000-ENVIO-MAPA
           PERFORM 7800-RETORNO-TRANSACCION
           .
       3300-HACIA-ADELANTE.

           MOVE "--3300-HACIA-ADELANTE--" TO W-PARRAFO
           MOVE "EIBCALEN" TO W-CAMPO
           MOVE EIBCALEN TO W-VALOR
           PERFORM 9100-DEB-DIS
           .
       3400-HACIA-ATRAS.

           MOVE "--3400-HACIA-ATRAS--" TO W-PARRAFO
           MOVE "EIBCALEN" TO W-CAMPO
           MOVE EIBCALEN TO W-VALOR
           PERFORM 9100-DEB-DIS
           .
       3500-GENERA-NOTAS-DE-PRUEBA.
           MOVE "-> 3500-GENERA-DATOS--" TO W-PARRAFO
           MOVE "EIBCALEN" TO W-CAMPO
           MOVE EIBCALEN TO W-VALOR
           PERFORM 9100-DEB-DIS
           SET IX-DNI TO 1    MOVE "9533075  " TO DNI-ESTUD (IX-DNI)
           MOVE "FLAVIO NESTOR" TO DNI-NOMEST (IX-DNI)
           MOVE "PATINO CORREA" TO DNI-APEEST (IX-DNI)
           MOVE 1 TO DNI-FROM (IX-DNI) MOVE 4 TO DNI-TO (IX-DNI)
           SET IX-DNI UP BY 1 MOVE "52438489 " TO DNI-ESTUD (IX-DNI)
           MOVE "DORY CONSTANZA" TO DNI-NOMEST (IX-DNI)
           MOVE "SANCHEZ ARAGON" TO DNI-APEEST (IX-DNI)
           MOVE 5 TO DNI-FROM (IX-DNI) MOVE 9 TO DNI-TO (IX-DNI)
           SET IX-DNI UP BY 1 MOVE "52056163 " TO DNI-ESTUD (IX-DNI)
           MOVE "LUZ MERY" TO DNI-NOMEST (IX-DNI)
           MOVE "ALARCON MEJIA" TO DNI-APEEST (IX-DNI)
           MOVE 10 TO DNI-FROM (IX-DNI) MOVE 12 TO DNI-TO (IX-DNI)
           SET IX-DNI UP BY 1 MOVE "52057782 " TO DNI-ESTUD (IX-DNI)
           MOVE "MONICA LILIANA" TO DNI-NOMEST (IX-DNI)
           MOVE "RODRIGUEZ RODRIGUEZ" TO DNI-APEEST (IX-DNI)
           MOVE 13 TO DNI-FROM (IX-DNI) MOVE 13 TO DNI-TO (IX-DNI)
           SET IX-DNI UP BY 1 MOVE "52263924 " TO DNI-ESTUD (IX-DNI)
           MOVE "ROSA CANDIDA" TO DNI-NOMEST (IX-DNI)
           MOVE "OJEDA" TO DNI-APEEST (IX-DNI)
           MOVE 14 TO DNI-FROM (IX-DNI) MOVE 17 TO DNI-TO (IX-DNI)
           SET IX-DNI UP BY 1 MOVE "52825799 " TO DNI-ESTUD (IX-DNI)
           MOVE "MAGDA MILENA" TO DNI-NOMEST (IX-DNI)
           MOVE "RODRIGUEZ" TO DNI-APEEST (IX-DNI)
           MOVE 18 TO DNI-FROM (IX-DNI) MOVE 18 TO DNI-TO (IX-DNI)
           .
       3600-MUEVE-LISTA-A-VISOR.
           MOVE CURSO (W-IND-LISTA) TO MCODIGO (W-IND-VISOR)
           MOVE NNOTA (W-IND-LISTA) TO MDESCUR (W-IND-VISOR)
           MOVE AAAA  (W-IND-LISTA) TO MAAACUR (W-IND-VISOR)
           MOVE NOTA  (W-IND-LISTA) TO W-EDI-NOTA-N
           MOVE W-EDI-NOTA-N TO MNOTCUR (W-IND-VISOR)
           ADD 1 TO W-IND-VISOR  W-IND-LISTA
           .
       3650-BORRAR-VISOR.
           MOVE SPACES TO MCODIGO (W-IND-VISOR)
           MOVE SPACES TO MDESCUR (W-IND-VISOR)
           MOVE SPACES TO MAAACUR (W-IND-VISOR)
           MOVE SPACES TO MNOTCUR (W-IND-VISOR)
           ADD 1 TO W-IND-VISOR
           .
       7000-ENVIO-MAPA.
           MOVE EIBTRMID TO CA-TERM
           MOVE EIBTRNID TO CA-TRANS
           EXEC CICS SEND
                   MAP('MSNNE03')
                MAPSET('MSNNE03')
                 FROM(MSNNE03O)
                NOHANDLE
           END-EXEC
           MOVE "-> 7000-ENVIO-MAPA--" TO W-PARRAFO
           MOVE "EIBTRNID" TO W-CAMPO
           MOVE EIBTRNID TO W-VALOR
           PERFORM 9100-DEB-DIS
           .
       7100-RECIBO-DE-MAPA.
           EXEC CICS RECEIVE
                MAP('MSNNE03')
                MAPSET('MSNNE03')
                INTO (MSNNE03I)
                NOHANDLE
           END-EXEC
           EXEC CICS
                IGNORE CONDITION MAPFAIL
           END-EXEC

           MOVE "-> 7100-RECIBO-DE-MAPA--" TO W-PARRAFO
           MOVE "EIBCALEN" TO W-CAMPO
           MOVE EIBCALEN TO W-VALOR
           MOVE "MDNIESI" TO W-CAMPO1
           MOVE MDNIESI TO W-VALOR1
           PERFORM 9100-DEB-DIS

           MOVE "EIBAID" TO W-CAMPO
           MOVE EIBAID TO W-VALOR
           PERFORM 9100-DEB-DIS

           EVALUATE EIBAID
                WHEN DFHENTER
                     MOVE MDNIESI TO W-DNI-BUSCADO
                     PERFORM 3200-CONSULTA-NOTAS
                WHEN DFHPF3
                     PERFORM 9999-FIN-DE-PROGRAMA
                WHEN DFHPF7
                     PERFORM 3400-HACIA-ATRAS
                WHEN DFHPF8
                     PERFORM 3300-HACIA-ADELANTE
                WHEN OTHER
                     MOVE "-TECLA NO HABILITADA-" TO MSG
                     PERFORM 7200-MENSAJES-A-LA-PANTALLA
           END-EVALUATE
           .
       7200-MENSAJES-A-LA-PANTALLA.
           DISPLAY MSG
           .
       7800-RETORNO-TRANSACCION.
           MOVE "-> 7800-RETORNO-TRANS--" TO W-PARRAFO
           MOVE "EIBTRNID" TO W-CAMPO
           MOVE EIBTRNID TO W-VALOR
           MOVE "EIBCALEN" TO W-CAMPO1
           MOVE EIBCALEN TO W-VALOR1
           PERFORM 9100-DEB-DIS
           MOVE EIBTRMID TO CA-TERM
           MOVE EIBTRNID TO CA-TRANS

           EXEC CICS
                RETURN
                TRANSID(EIBTRNID)
                COMMAREA(MI-COMMAREA)
                LENGTH(LENGTH OF MI-COMMAREA)
           END-EXEC
           .
       7900-CONTROL-A-OTRO-PROGRAMA.
           CONTINUE.
       9000-ERRORES.
           CONTINUE.
      * COPY PDEBDIS.
       9100-DEB-DIS.
            PERFORM 3100-GET-DATES
            MOVE WS-FECHA-AAAA-MM-DD TO W-FECHA-DIS
            MOVE WS-HORA-MINUTOS-SEGUNDOS TO W-TIME-DIS
            IF W-SW-DEBUG = "S" THEN
               DISPLAY WS-DEBDIS-MSG
            END-IF

            .

       9999-FIN-DE-PROGRAMA.
           EXEC CICS
                SEND TEXT FROM (MSG-FIN-SESION)
                     ERASE FREEKB
           END-EXEC
           EXEC CICS RETURN
           END-EXEC
           GOBACK
           .
