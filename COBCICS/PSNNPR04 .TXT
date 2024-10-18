      ******************************************************************
      * PROYECTO .: SNIU-SISTEMA DE NOTAS DE IDENTIFICACIÛN DE USUARIOS*
      * PROGRAMA .: PSNNPR04                                           *
      * VERSION ..: ULTIMA VERSION DE LA ACTIVIDAD                     *
      * TITULO: ..: PROGRAMA SISTEMA NOTAS CONSULTA NOTAS PROFESOR 04  *
      *                                                                *
      * TIPO .......:                                                  *
      *   - LENGUAJE ..................: COBOL II                      *
      *   - ENTORNO ...................: CICS                          *
      *   - BASE DE DATOS .............: DB2                           *
      *                                                                *
      * DESCRIPCION.....:    FECHA SEP 21 HORA 20:56                   *
      *   - PSNNPR04         TIENE LA COMMAREA Y LINK ENCAPS USUARIOS  *
      *   -                                                            *
      *   - USA CURSORES PARA F7-ANTERIOR Y F8-SIGUIENTE               *
      *   - SE MUESTRA EN LA PANTALLA LA IDENTIFICACIÛN DEL PROFESOR   *
      *     A CONSULTAR Y EN LAS PRÛXIMAS LÌNEAS SE MUESTRAN , EL DNI, *
      *     APELLIDOS Y NOMBRE, CURSO, AÒO DEL CURSO Y LA NOTA FINAL   *
      *     DE CADA ESTUDIANTE DE LOS CURSOS DICTADO POR EL PROFESOR.  *
      *     PARA EL USUARIO ADMIN SE PUEDE CONSULTAR LAS NOTAS DE TODOS*
      *     LOS PROFESORES, PERO PARA EL USUARIO PROFESOR, SOLO PUEDE  *
      *     VER LAS NOTAS DE LOS ESTUDIANTES DE LOS CURSOS QUE DICTA   *
      ******************************************************************
      * MODIFICACIONES:                                                *
      * OCT 9-2024 : SE AGREGO A LOS NOMBRES DE LAS COLAS, EL NUMERO   *
      * SAUL         DE LA TERMINAL PARA QUE FUNCIONE CUANDO EL SISTEMA*
      *              ESTE EN UN SERVIDOR                               *
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PSNNPR04.
       AUTHOR.     URIEL ESCUDERO.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      *------------------------------------------------
       01  MI-COMMAREA.
           03 CA-TRANS             PIC X(04).
           03 CA-TERM              PIC X(04).
           03 CA-CICLO             PIC X(02).
      *SAUL
           03 CA-ID-DNI            PIC X(09).
           03 CA-ID-TIPOUSUA       PIC X(05).
           03 CA-ID-NOMBRE         PIC X(20).
           03 CA-ID-APELLIDOS      PIC X(20).
      *FINSAUL
           03 CA-US-DNI            PIC X(09).
           03 CA-US-TIPOUSUA       PIC X(05).
           03 CA-US-NOMBRE         PIC X(20).
           03 CA-US-APELLIDOS      PIC X(20).
           03 CA-NUM-REGISTRO      PIC 9(5).
           03 CA-CONTADOR-REGISTRO PIC S9(5).
           03 CA-FECHA             PIC X(10).
      *SAUL
       01  ID-COMMAREA.
           03 ID-TRANS                    PIC X(04).
           03 ID-TERM                     PIC X(04).
           03 ID-CICLO                    PIC X(02).
           03 ID-ID-DNI                   PIC X(09).
           03 ID-ID-TIPOUSUA              PIC X(05).
           03 ID-ID-NOMBRE                PIC X(20).
           03 ID-ID-APELLIDOS             PIC X(20).
           03 ID-PF-IDPERFIL              PIC X(3).
           03 ID-PF-NOMPERFIL             PIC X(20).
           03 ID-PF-CONS                  PIC X(2).
           03 ID-PF-BORR                  PIC X(2).
           03 ID-PF-INGR                  PIC X(2).
           03 ID-PF-MODI                  PIC X(2).
       01  AREA-LINK             PIC X(132).
       01  AREA-LINK-RUS             REDEFINES AREA-LINK.
           03 LK-US-TIPO-OPE     PIC 9(01).
           03 LK-US-SUBTIPO-OPE  PIC 9(02).
           03 LK-US-DNI          PIC X(09).
           03 LK-US-TIPOUSUARIO  PIC X(05).
           03 LK-US-CLAVE        PIC X(08).
           03 LK-US-NOMBRE       PIC X(20).
           03 LK-US-APELLIDOS    PIC X(20).
           03 LK-US-DIRECCION    PIC X(35).
           03 LK-US-CARGO        PIC X(20).
           03 LK-US-ESTADO       PIC X(01).
           03 LK-US-COD-RET      PIC 9(02).
           03 LK-US-SQLCODE      PIC S9(9).
       01  AREA-LINK-RCR            REDEFINES AREA-LINK.
           03 LK-CR-TIPO-OPE     PIC 9(01).
           03 LK-CR-SUBTIPO-OPE  PIC 9(02).
           03 LK-CR-CURSO        PIC X(05).
           03 LK-CR-DESCRIPCION  PIC X(20).
           03 LK-CR-DNIPROFESOR  PIC X(09).
           03 LK-CR-ESTADO       PIC X(01).
           03 LK-CR-COD-RET      PIC 9(02).
           03 LK-CR-SQLCODE      PIC S9(9).
       01  AREA-LINK-TNT   .
           03 LK-NT-TIPO-OPE     PIC 9(01).
           03 LK-NT-SUBTIPO-OPE  PIC 9(02).
           03 LK-NT-CURSO        PIC X(05).
           03 LK-NT-DNIESTUD     PIC X(09).
           03 LK-NT-AAAA         PIC X(04).
           03 LK-NT-NOTA         PIC 99V99.
           03 LK-NT-CONTADOR     PIC 9(10).
           03 LK-NT-COD-RET      PIC 9(02).
           03 LK-NT-SQLCODE      PIC S9(9).
           03 LK-NT-FILLER       PIC X(86).
      *-- ZONA COMMAREA -------------------------------
      *
      *-- VARIABLES -----------------------------------
       01 COLA-CURSOS.
          05 KCR-CURSO       PIC X(05).
          05 KCR-DESCRIPCION PIC X(20).
          05 KCR-DNIPROFESOR PIC X(09).
       01 WS-COLA-CURSOS PIC X(08) .
       01 COLA-PROFESORES.
          05 KPR-CURSO         PIC X(05).
          05 KPR-DNIESTUDIANTE PIC X(09).
          05 KPR-AAAA          PIC X(04).
          05 KPR-NOTA          PIC S9(5)V99.
       01 WS-COLA-PROFESORES   PIC X(08).
       01 RESPUESTA      PIC S9(08) COMP.
       01 WS-EOF         PIC X VALUE SPACES.
       01 WS-COLATRABAJO PIC X(08) VALUE 'KCICS01'.
       01 COLA-TRABAJO.
          05 T-AREA-LINK1 PIC X(20).
          05 T-AREA-LINKF1 PIC XX.
          05 T-AREA-LINK2 PIC X(20).
          05 T-AREA-LINKF2 PIC XX.
          05 T-AREA-LINK3 PIC X(100).
          05 T-DNI       PIC X(09).
          05 T-PAS       PIC X(08).
          05 T-CLAVE     PIC X(08).
          05 T-NOMBRE    PIC X(20).
          05 T-APELLIDOS PIC X(20).
          05 T-DIRECCION PIC X(35).
          05 T-SQLCODE   PIC X(10).
       01 WS-PROFE-ESTUD-NOTA.
          03 WS-UTIPOUSUA     PIC X(05).
          03 WS-UDNI          PIC X(09).
          03 WS-UNOMBRE       PIC X(20).
          03 WS-UAPELLIDOS    PIC X(20).
          03 WS-CCURSO        PIC X(05).
          03 WS-CESTADO       PIC X(1).
          03 WS-EDNI          PIC X(09).
          03 WS-NAAAA         PIC X(04).
          03 WS-ENOTA         PIC -Z9.99.
          03 WS-ENOTA-RED     PIC 99V99.
          03 WS-ENOTAU        PIC S99V99 USAGE COMP-3.
          03 WS-UENOMBRE      PIC X(20) VALUE 'DUMMY - N'.
          03 WS-UEAPELLIDOS   PIC X(20) VALUE 'DUMMY - A'.
          03 WS-CDNIPROFESOR  PIC X(09).
       01  WS-AREA-FECHOR.
           05  WS-CURRENT-DATE-DATA.
               10  WS-CURRENT-DATE.
                   15  WS-CURRENT-YEAR     PIC 9(04).
                   15  WS-CURRENT-MES      PIC 9(02).
                   15  WS-CURRENT-DIA      PIC 9(02).
               10  WS-CURRENT-TIME.
                   15  WS-CURRENT-HRA      PIC 9(02).
                   15  WS-CURRENT-MIN      PIC 9(02).
                   15  WS-CURRENT-SEG      PIC 9(02).
                   15  WS-CURRENT-MSEG     PIC 9(02).
           05 WS-COD-RET                   PIC X(02).
       01  WS-FECHOR.
           05  WS-LAFECHA.
               10  WS-ELANO                PIC 9(04).
               10  FILLER                  PIC X(01) VALUE "/".
               10  WS-ELMES                PIC 9(02).
               10  FILLER                  PIC X(01) VALUE "/".
               10  WS-ELDIA                PIC 9(02).
           05  WSEPARADOR                  PIC X(02) VALUE "--".
           05  WS-LATIME.
               10  WS-LAHORA               PIC 9(02).
               10  FILLER                  PIC X(01) VALUE ":".
               10  WS-ELMINUTO             PIC 9(02).
               10  FILLER                  PIC X(01) VALUE ":".
               10  WS-ELSEGUNDO            PIC 9(02).
      *------------------------------------------------
       01 WS-MSG-TEXT        PIC X(80) VALUE 'FIN DE SESION'.
       01 WS-RESPUESTA       PIC S9(8) COMP.
       01 WS-RESPUESTA2      PIC S9(8) COMP.
       01 WS-PARRAFO         PIC X(10)      VALUE SPACES.
       01 WS-TRAZA           PIC X(160)      VALUE SPACES.
       01 WS-RESP            PIC 9(4)  COMP VALUE 0.
       01 WS-COLATS          PIC X(8)       VALUE 'NOTAS_TS'.
      *-- CONSTANTES ----------------------------------
      *------------------------------------------------
      *-- SWITCHES ------------------------------------
       01  SW-SWITCHES.
           03 SW-CICLO             PIC X(02).
              88 CICLO-0                  VALUE "00".
              88 CICLO-1                  VALUE "01".
              88 CICLO-9                  VALUE "09".
              88 CICLO-88                 VALUE "88".
      *------------------------------------------------
       77 WS-PRIMER-REGISTRO   PIC S9(4) COMP.
       77 WS-SCROLL            PIC 9(2) VALUE 8.
       77 WS-NUM-REGISTRO      PIC S9(4) COMP.
       77 WS-CA-NUM-REGISTRO   PIC S9(4) COMP.
       77 WS-RESULTADO         PIC 9(2).
       77 WS-COCIENTE          PIC S9(2).
       77 WS-RESIDUO           PIC S9(2).
       77 WS-PROXIMO           PIC 9    VALUE ZEROS.
       77 WS-NOMAPE            PIC X(40).
       77 MUPO                 PIC X.
       77 MDOWNO               PIC X.
       77 NUMCO                PIC 9.
       77 WS-NOTA              PIC Z9.99.
      *   COPY DEL MAPA GENERADO Y OTROS
       COPY DFHAID.
       COPY DFHBMSCA.
       COPY CSNNP04.
       COPY CCICS68.
      *------------------------------------------------
      *------------------------------------------------
       LINKAGE SECTION.
       01 DFHCOMMAREA     PIC X(138).
       PROCEDURE DIVISION.
      ***************************************************************
      *0000-RAIZ
      ***************************************************************
       0000-RAIZ.
           PERFORM 1000-INICIO
           PERFORM 2000-PROCESO
           PERFORM 0300-FIN
           .
      ***************************************************************
      *1000-INICIO
      ***************************************************************
       1000-INICIO.
           INITIALIZE WS-CURRENT-DATE-DATA
                      WS-COD-RET

           PERFORM 790-CONSEGUIR-FECHOR
           MOVE 'KSNC'        TO WS-COLA-CURSOS(1:4)
           MOVE EIBTRMID      TO WS-COLA-CURSOS(5:4)
           MOVE 'KSNP'        TO WS-COLA-PROFESORES(1:4)
           MOVE EIBTRMID      TO WS-COLA-PROFESORES(5:4)
      *
           EVALUATE TRUE
             WHEN EIBCALEN = 0
                SET CICLO-0 TO TRUE
             WHEN OTHER
                MOVE DFHCOMMAREA TO MI-COMMAREA
                MOVE WS-LAFECHA  TO CA-FECHA
                MOVE CA-NUM-REGISTRO TO WS-CA-NUM-REGISTRO
           END-EVALUATE
           EVALUATE CA-CICLO
             WHEN '00'
               SET CICLO-0 TO TRUE
             WHEN '01'
               SET CICLO-1 TO TRUE
             WHEN '88'
               MOVE DFHCOMMAREA     TO ID-COMMAREA
               MOVE ID-TRANS        TO CA-TRANS
               MOVE ID-TERM         TO CA-TERM
               MOVE ID-CICLO        TO CA-CICLO
               MOVE ID-ID-DNI       TO CA-ID-DNI
               MOVE ID-ID-TIPOUSUA  TO CA-ID-TIPOUSUA
               MOVE ID-ID-NOMBRE    TO CA-ID-NOMBRE
               MOVE ID-ID-APELLIDOS TO CA-ID-APELLIDOS
               SET CICLO-0          TO TRUE
             WHEN OTHER
               MOVE "CICLO NO PROGRAMADO" TO WS-MSG-TEXT
               PERFORM 7810-RETORNO-CICS-FIN
           END-EVALUATE
           .

      ***************************************************************
      *2000-PROCESO
      ***************************************************************
       2000-PROCESO.
              EVALUATE TRUE
                 WHEN CICLO-0
                    PERFORM 2210-CICLO-0
                 WHEN CICLO-1
                    PERFORM 2215-CICLO-1
              END-EVALUATE
              .
      ********************************************************
      * FIN DE LA TRANSACCION, LLAMA O RETORNA AL PROGRAMA DE*
      *                SEGURIDAD                             *
      ********************************************************
       0300-FIN.
           IF CA-ID-TIPOUSUA = 'PROFE'
             MOVE 'XCTL'     TO CA-TRANS
             MOVE '00'       TO CA-CICLO
             EXEC CICS XCTL
                  PROGRAM('PSNIDU01')
                  COMMAREA(MI-COMMAREA)
                  LENGTH(LENGTH OF MI-COMMAREA)
                  RESP(RESPUESTA)
             END-EXEC
           ELSE
             MOVE 'XCTL'     TO CA-TRANS
             EXEC CICS XCTL
                PROGRAM('PSNMEN02')
                COMMAREA(MI-COMMAREA)
                LENGTH(LENGTH OF MI-COMMAREA)
                RESP(RESPUESTA)
             END-EXEC
           END-IF
           EXEC CICS RETURN
           END-EXEC
           GOBACK
           .
      *************************** ***********************************
      *2210-CICLO-0
      ***************************************************************
       2210-CICLO-0.
           MOVE LOW-VALUES TO MSNNP04O
           IF CA-ID-TIPOUSUA = 'PROFE'
             MOVE DFHBMPRO     TO UDNIF
             MOVE CA-ID-DNI    TO UDNIO
             MOVE CA-ID-NOMBRE TO UNOMBO
           END-IF
           MOVE CA-FECHA        TO MFECHAO
           MOVE CA-ID-APELLIDOS TO LAPEO
           MOVE CA-ID-NOMBRE    TO LNOMO
           EXEC CICS SEND MAP('MSNNP04')
               ERASE
               FROM(MSNNP04O)
               NOHANDLE
           END-EXEC
           MOVE '01'       TO  CA-CICLO
           PERFORM 7800-RETORNO-TRANS-CICS
              .
      ***************************************************************
      *2215-CICLO-1
      ***************************************************************
       2215-CICLO-1.
           EXEC CICS RECEIVE MAP('MSNNP04')
                INTO(MSNNP04I)
                NOHANDLE
           END-EXEC

           EXEC CICS
                IGNORE CONDITION MAPFAIL
           END-EXEC
           MOVE 'PROFE' TO CA-US-TIPOUSUA
           EVALUATE EIBAID
              WHEN DFHENTER
                 PERFORM 3240-CONSULTAR-REGISTRO
              WHEN DFHPF8
                 PERFORM 0251-PROCESE-PROXIMO
              WHEN DFHPF7
                 PERFORM 0252-PROCESE-ANTERIOR
              WHEN OTHER
                 PERFORM 0232-VALIDE-PF3
            END-EVALUATE
           .
      ***************************************************************
      *3240-CONSULTAR-REGISTRO. <ENTER>
      ***************************************************************
       3240-CONSULTAR-REGISTRO.
           IF CA-ID-TIPOUSUA = 'PROFE'
             MOVE DFHBMPRO   TO UDNIF
             MOVE CA-ID-DNI  TO UDNIO
             MOVE CA-ID-NOMBRE TO UNOMBO
           END-IF
           IF UDNII EQUAL LOW-VALUES OR '_________' OR SPACES THEN
              MOVE "DEBE DIGITAR IDENTIFICACION DEL PROFESOR"
                TO MSGO
              PERFORM 730-ENVIAR-MSG
           END-IF
           MOVE SPACES            TO AREA-LINK
           MOVE 9                 TO LK-US-COD-RET
           MOVE 1                 TO LK-US-TIPO-OPE
           MOVE 1                 TO LK-US-SUBTIPO-OPE
           MOVE UDNII             TO LK-US-DNI CA-US-DNI

           EXEC CICS LINK
                     PROGRAM('RSNUSUAR')
                     COMMAREA(AREA-LINK)
                     LENGTH(LENGTH OF AREA-LINK)
                     RESP(WS-RESPUESTA)
           END-EXEC
           IF LK-US-COD-RET NOT = 0
             EVALUATE LK-US-COD-RET
                WHEN 1
                   MOVE DFHBLINK  TO   MSGH
                   MOVE 'USUARIO NO EXISTE ......' TO MSGO
                   PERFORM 730-ENVIAR-MSG

                WHEN OTHER
                   MOVE "OJO CON BASE DE DATOS" TO WS-MSG-TEXT
                   PERFORM 7810-RETORNO-CICS-FIN
             END-EVALUATE
           END-IF
           MOVE LK-US-DNI         TO  CA-US-DNI
           MOVE LK-US-NOMBRE      TO  CA-US-NOMBRE
           MOVE LK-US-APELLIDOS   TO  CA-US-APELLIDOS
           MOVE LK-US-TIPOUSUARIO TO  CA-US-TIPOUSUA
           IF LK-US-COD-RET = 0 AND LK-US-TIPOUSUARIO NOT = 'PROFE'
             MOVE 1 TO LK-US-COD-RET
           END-IF
           EVALUATE LK-US-COD-RET
              WHEN 0
                PERFORM 280-VER-CURSOS-NOTASFIN
                PERFORM 0250-EMPIECE-TRAN

           END-EVALUATE
           .
      ***************************************************************
      *CONSULTA LOS CURSOS DEL PROFESOR Y SUS NOTAS
      ***************************************************************
       280-VER-CURSOS-NOTASFIN.
      *
           MOVE 9         TO LK-CR-COD-RET
           MOVE 5         TO LK-CR-TIPO-OPE
           MOVE 1         TO LK-CR-SUBTIPO-OPE
           MOVE LK-US-DNI TO LK-CR-DNIPROFESOR

           EXEC CICS LINK
                     PROGRAM('RSNCURSO')
                     COMMAREA(AREA-LINK)
                     LENGTH(LENGTH OF AREA-LINK)
                     RESP(WS-RESPUESTA)
           END-EXEC

      *
             MOVE 1 TO WS-NUM-REGISTRO
             EXEC CICS READQ  TS
                  ITEM(WS-NUM-REGISTRO)
                  QUEUE (WS-COLA-CURSOS)
                  INTO (COLA-CURSOS)
                  RESP (RESPUESTA)
             END-EXEC
             IF RESPUESTA = DFHRESP(NORMAL)
               MOVE 'N' TO WS-EOF
             ELSE
               IF RESPUESTA = DFHRESP(NOTFND)
                 MOVE 'Y' TO WS-EOF
               ELSE
                 MOVE 'Y' TO WS-EOF
               END-IF
             END-IF
             EXEC CICS DELETEQ TS
               QUEUE (WS-COLA-PROFESORES)
               RESP (RESPUESTA)
             END-EXEC
             PERFORM CREE-COLA-PROFESORES UNTIL WS-EOF = 'Y'

           .
       CREE-COLA-PROFESORES.
           MOVE 9         TO LK-NT-COD-RET
           MOVE 5         TO LK-NT-TIPO-OPE
           MOVE 1         TO LK-NT-SUBTIPO-OPE
           MOVE KCR-CURSO TO LK-NT-CURSO
           MOVE SPACES    TO LK-NT-DNIESTUD LK-NT-FILLER
           MOVE ZEROS     TO LK-NT-CONTADOR LK-NT-COD-RET
           MOVE ZEROS     TO LK-NT-FILLER

           EXEC CICS LINK
                     PROGRAM('RSNNOFIN')
                     COMMAREA(AREA-LINK-TNT)
                     LENGTH(LENGTH OF AREA-LINK)
                     RESP(WS-RESPUESTA)
           END-EXEC
           MOVE LK-NT-CONTADOR   TO    CA-CONTADOR-REGISTRO
           IF WS-RESPUESTA = DFHRESP(NORMAL) THEN
             MOVE LK-NT-CONTADOR   TO    CA-CONTADOR-REGISTRO
           END-IF
           ADD  1 TO WS-NUM-REGISTRO
           EXEC CICS READQ  TS
                ITEM(WS-NUM-REGISTRO)
                QUEUE (WS-COLA-CURSOS)
                INTO (COLA-CURSOS)
                RESP (RESPUESTA)
           END-EXEC
           IF RESPUESTA = DFHRESP(NORMAL)
                MOVE 'N' TO WS-EOF
           ELSE
              IF RESPUESTA = DFHRESP(NOTFND)
                MOVE 'Y' TO WS-EOF
              ELSE
                MOVE 'Y' TO WS-EOF
              END-IF
           END-IF
           .
       0250-EMPIECE-TRAN.
           MOVE LOW-VALUE          TO MSNNP04O
           MOVE CA-US-DNI          TO UDNIO
           MOVE CA-US-NOMBRE       TO WS-NOMAPE(1:20)
           MOVE CA-US-APELLIDOS    TO WS-NOMAPE(21:20)
           MOVE WS-NOMAPE          TO UNOMBO
           MOVE CA-CONTADOR-REGISTRO TO NUMCO
           MOVE 0 TO CA-NUM-REGISTRO WS-CA-NUM-REGISTRO
           MOVE 8 TO WS-PROXIMO
           PERFORM 3200-PRESENTA
           IF CA-ID-TIPOUSUA = 'PROFE'
             MOVE DFHBMPRO     TO UDNIF
             MOVE CA-ID-DNI    TO UDNIO
             MOVE CA-ID-NOMBRE TO UNOMBO
           END-IF
           MOVE CA-FECHA        TO MFECHAO
           MOVE CA-ID-APELLIDOS TO LAPEO
           MOVE CA-ID-NOMBRE    TO LNOMO
           EXEC CICS SEND MAP('MSNNP04')
               ERASE
               FROM(MSNNP04O)
               NOHANDLE
           END-EXEC
           MOVE '01'       TO  CA-CICLO
           PERFORM 7800-RETORNO-TRANS-CICS
           .
       0251-PROCESE-PROXIMO.
           IF WS-CA-NUM-REGISTRO > CA-CONTADOR-REGISTRO
             DIVIDE CA-CONTADOR-REGISTRO BY WS-SCROLL
                 GIVING WS-COCIENTE REMAINDER WS-RESIDUO
             IF WS-RESIDUO = 0
               COMPUTE WS-CA-NUM-REGISTRO = (WS-COCIENTE - 1) *
                       WS-SCROLL
             ELSE
               COMPUTE WS-CA-NUM-REGISTRO = (WS-COCIENTE) * WS-SCROLL
             END-IF
           END-IF
           MOVE LOW-VALUE          TO MSGO
           MOVE 8 TO WS-PROXIMO
           PERFORM 3200-PRESENTA
           MOVE CA-CONTADOR-REGISTRO TO NUMCO
           IF CA-ID-TIPOUSUA = 'PROFE'
             MOVE DFHBMPRO     TO UDNIF
             MOVE CA-ID-DNI    TO UDNIO
             MOVE CA-ID-NOMBRE TO UNOMBO
           END-IF
           MOVE CA-FECHA        TO MFECHAO
           MOVE CA-ID-APELLIDOS TO LAPEO
           MOVE CA-ID-NOMBRE    TO LNOMO
           EXEC CICS SEND MAP('MSNNP04')
                  ERASE
                  FROM(MSNNP04O)
                  NOHANDLE
           END-EXEC
           MOVE '01'     TO CA-CICLO
           PERFORM 7800-RETORNO-TRANS-CICS
              .
      *
       0252-PROCESE-ANTERIOR.
           IF WS-CA-NUM-REGISTRO > CA-CONTADOR-REGISTRO
              COMPUTE WS-CA-NUM-REGISTRO = CA-CONTADOR-REGISTRO
           END-IF
           MOVE LOW-VALUE          TO MSGO
           DIVIDE WS-CA-NUM-REGISTRO BY WS-SCROLL
                  GIVING WS-COCIENTE REMAINDER WS-RESIDUO
           IF WS-RESIDUO > 0
             COMPUTE WS-CA-NUM-REGISTRO = WS-CA-NUM-REGISTRO -
                                          WS-SCROLL - WS-RESIDUO
           ELSE
             COMPUTE WS-CA-NUM-REGISTRO = WS-CA-NUM-REGISTRO -
                                       2 * WS-SCROLL
           END-IF
           IF WS-CA-NUM-REGISTRO < 1
             MOVE 0 TO CA-NUM-REGISTRO WS-CA-NUM-REGISTRO
             MOVE DFHGREEN                   TO MSGO
             MOVE 'PAGINA INICIAL        '   TO MSGO
           END-IF
           MOVE 8 TO WS-PROXIMO
           PERFORM 3200-PRESENTA
           MOVE CA-CONTADOR-REGISTRO TO NUMCO
           MOVE CA-FECHA        TO MFECHAO
           MOVE CA-ID-APELLIDOS TO LAPEO
           MOVE CA-ID-NOMBRE    TO LNOMO
           EXEC CICS SEND MAP('MSNNP04')
                  ERASE
                  FROM(MSNNP04O)
                  NOHANDLE
           END-EXEC
           MOVE '01'     TO CA-CICLO
           PERFORM 7800-RETORNO-TRANS-CICS
              .
      ************************************************************
      *--- IMPORTANTE: ESTA PENDIENTE CONTAR REGISTROS F(FILTROS)*
      *--- ESTO SE HAR A SOBRE EL DATO DB2-COUNT1 *
      *--- SI NU HUBIERA NO SE PODR A MOSRTAR NADA -> MENSAJE *
      *--- POR AHORA FORZAMOS... *
      *
       3200-PRESENTA.
      *
           SET L    TO 1
           MOVE 'N' TO WS-EOF
           IF WS-CA-NUM-REGISTRO - 8 < 0
             MOVE '-' TO MUPO
           ELSE
             MOVE '+' TO MUPO
           END-IF
           IF WS-CA-NUM-REGISTRO + 8 < CA-CONTADOR-REGISTRO
             MOVE '+' TO MDOWNO
           ELSE
             MOVE '-' TO MDOWNO
           END-IF
           MOVE 'N' TO WS-EOF
           PERFORM 3090-PROCESA-SCROLL UNTIL WS-EOF = 'Y' OR L > 8
           IF WS-EOF = 'Y' AND L < 8
             PERFORM 3100-NO-HAY-MAS
           .
      ***************************************************
      *--- PROCESA SCROLL                               *
      ***************************************************
       3090-PROCESA-SCROLL.
           IF WS-PROXIMO = 8
              COMPUTE WS-CA-NUM-REGISTRO = WS-CA-NUM-REGISTRO + 1
           ELSE
             IF WS-PROXIMO = 7
               COMPUTE WS-CA-NUM-REGISTRO = WS-CA-NUM-REGISTRO - 1
             END-IF
           END-IF
           EXEC CICS READQ  TS
                ITEM(WS-CA-NUM-REGISTRO)
                QUEUE (WS-COLA-PROFESORES)
                INTO (COLA-PROFESORES)
                RESP (RESPUESTA)
           END-EXEC
           IF RESPUESTA = DFHRESP(NORMAL) AND
              WS-CA-NUM-REGISTRO <= CA-CONTADOR-REGISTRO
             PERFORM 3100-PRESENTA-LINEA
             SET L UP BY 1
           ELSE
             MOVE 'Y' TO WS-EOF
           END-IF
           .
      ***************************************************
      *--- PRESENTA LA LINEA DEFINIDA POR EL INDICE L EN*
      *    PANTALLA                                     *
      ***************************************************
       3100-PRESENTA-LINEA.
           MOVE SPACES            TO AREA-LINK
           MOVE 9                 TO LK-US-COD-RET
           MOVE 1                 TO LK-US-TIPO-OPE
           MOVE 1                 TO LK-US-SUBTIPO-OPE
           MOVE KPR-DNIESTUDIANTE TO LK-US-DNI CA-US-DNI

           EXEC CICS LINK
                     PROGRAM('RSNUSUAR')
                     COMMAREA(AREA-LINK)
                     LENGTH(LENGTH OF AREA-LINK)
                     RESP(WS-RESPUESTA)
           END-EXEC
           IF LK-US-COD-RET = 0 AND LK-US-TIPOUSUARIO NOT = 'ESTUD'
             MOVE LK-US-TIPOUSUARIO     TO LK-US-APELLIDOS
           END-IF
           IF LK-US-COD-RET NOT = 0
             MOVE 'IDENTIFICACION'      TO LK-US-NOMBRE
             MOVE 'SIN USUARIO   '      TO LK-US-APELLIDOS
           END-IF
           MOVE LK-US-NOMBRE              TO WS-NOMAPE(1:20)
           MOVE LK-US-APELLIDOS           TO WS-NOMAPE(21:20)
           MOVE KPR-DNIESTUDIANTE         TO EDNI-VALOR(L)
           MOVE WS-NOMAPE                 TO UENOMB-VALOR(L)
           MOVE KPR-CURSO                 TO CCURSO-VALOR(L)
           MOVE KPR-AAAA                  TO NAAAA-VALOR(L)
           MOVE KPR-NOTA                  TO WS-NOTA
           MOVE WS-NOTA                   TO ENOTA-VALOR(L)
           .
       3100-PRESENTA-LINEA-EXIT.
           EXIT.
      *----------------------------------------------------------------
      *--- NO HAY MAS CLIENTES EN EL CURSOR ACTUAL
       3100-NO-HAY-MAS.
      *
           PERFORM UNTIL L > 8
             MOVE SPACES                  TO EDNI-VALOR(L)
             MOVE SPACES                  TO UENOMB-VALOR(L)
             MOVE SPACES                  TO CCURSO-VALOR(L)
             MOVE SPACES                  TO NAAAA-VALOR(L)
             MOVE SPACES                  TO ENOTA-VALOR(L)
             SET L                        UP BY 1
           END-PERFORM
           MOVE DFHGREEN                   TO MSGO
           MOVE 'NO HAY MAS PAGINAS    '   TO MSGO
           .
      *
       3100-NO-HAY-MAS-EXIT.
           EXIT.
      ***************************************************************
      *3282-MUEVE-DATOS-DB2-O        TODOS
      ***************************************************************
      ***************************************************************
      *8700-CURSORANT-DECLARAR-ABRIR
      ***************************************************************
      ***************************************************************
      *7200-ENVIAR-MSG-MAPA-O-RT
      ***************************************************************
       7200-ENVIAR-MSG-MAPA-O-RT.

            EXEC CICS SEND MAP('MSNNP04')
                 FROM(MSNNP04O)
                 ERASE
                 NOHANDLE
            END-EXEC

            PERFORM 7800-RETORNO-TRANS-CICS
            .
      ***************************************************************
      ********************************************
      * SE SALE DEL TODO DEL SISTEMA              *
      *********************************************
       0232-VALIDE-PF3.
              IF EIBAID EQUAL DFHPF3   THEN
                MOVE '99'     TO CA-CICLO
                PERFORM 0300-FIN
              END-IF
            MOVE '99'     TO CA-CICLO
            PERFORM 0300-FIN
              .
       790-CONSEGUIR-FECHOR.
      *
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-DATA
           MOVE 'OK'                  TO WS-COD-RET
           MOVE WS-CURRENT-YEAR       TO WS-ELANO
           MOVE WS-CURRENT-MES        TO WS-ELMES
           MOVE WS-CURRENT-DIA        TO WS-ELDIA
           MOVE WS-CURRENT-HRA        TO WS-LAHORA
           MOVE WS-CURRENT-MIN        TO WS-ELMINUTO
           MOVE WS-CURRENT-SEG        TO WS-ELSEGUNDO
           .
      *
      *
      * 730-ENVIAR-MSG.
      *
       730-ENVIAR-MSG.
      *
           MOVE DFHBLINK  TO  MSGH

              EXEC CICS SEND MAP('MSNNP04')
                   FROM(MSNNP04O)
                   NOHANDLE
              END-EXEC
           PERFORM 7800-RETORNO-TRANS-CICS
           .
      *7210-ENVIAR-MSG-TEXT.
      ***************************************************************
       7210-ENVIAR-MSG-TEXT.
            EXEC CICS
              SEND TEXT FROM (WS-MSG-TEXT)
              ERASE
              LENGTH (LENGTH OF WS-MSG-TEXT)
            END-EXEC
            EXIT
            .
      *****************************************
      * DEVUELVE EL CONTROL A LA TRANSACCION  *
      *****************************************
      ***************************************************************
      *7800-RETORNO-TRANS-CICS.
      ***************************************************************
       7800-RETORNO-TRANS-CICS.
            MOVE WS-CA-NUM-REGISTRO      TO CA-NUM-REGISTRO
            MOVE EIBTRMID  TO CA-TERM
            MOVE 'SN04'    TO CA-TRANS
            EXEC CICS RETURN
                 TRANSID('SN04')
                 COMMAREA(MI-COMMAREA)
                 LENGTH(LENGTH OF MI-COMMAREA)
            END-EXEC
            GOBACK
            .
      ***************************************************************
      *7810-RETORNO-CICS-FIN. - FIN-SESION -
      ***************************************************************
       7810-RETORNO-CICS-FIN.
            EXEC CICS
            SEND TEXT FROM (WS-MSG-TEXT)
            ERASE
            FREEKB
            END-EXEC
      *
            EXEC CICS RETURN
            END-EXEC
            GOBACK
            .
      ***************************************************************
      * 8888-BUSCAR-NOMBRE-ESTUDIANTE  == ENCAPSULADOR USUARIOS ==
      *****************************************************************
