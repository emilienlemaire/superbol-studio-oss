       IDENTIFICATION DIVISION.
       PROGRAM-ID. CREATEDB.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM           PIC 9(6).
       01 NUM-COUNT     PIC 9(6).
       01 NUM-SUM       PIC 9(8).

       EXEC SQL BEGIN DECLARE SECTION END-EXEC.
       01 DB-SOURCE     PIC  X(50) VALUE SPACE.
       01 DB-USER       PIC  X(30) VALUE SPACE.
       01 DB-PASS       PIC  X(20) VALUE SPACE.
       EXEC SQL END DECLARE SECTION END-EXEC.

       EXEC SQL INCLUDE SQLCA END-EXEC.

       PROCEDURE DIVISION.

       MAIN.

      * NOTE: IT SEEMS SQL ERROR HANDLERS ALWAYS CAUSE
      * THE PROGRAM TO BE TERMINATED AFTER BEING CALLED
           EXEC SQL WHENEVER SQLERROR PERFORM SQL_ERROR END-EXEC
           EXEC SQL WHENEVER SQLWARNING PERFORM SQL_ERROR END-EXEC

           DISPLAY "CONNECTING"

           ACCEPT DB-SOURCE FROM ENVIRONMENT "COB_DBSOURCE"
           ACCEPT DB-USER FROM ENVIRONMENT "COB_DBUSER"
           ACCEPT DB-PASS FROM ENVIRONMENT "COB_DBPASS"

           EXEC SQL
             CONNECT TO :DB-SOURCE USER :DB-USER USING :DB-PASS
           END-EXEC

           DISPLAY "COUNTING NUMBERS"

           EXEC SQL
             SELECT COUNT(*) INTO :NUM-COUNT FROM NUMBERS
           END-EXEC.
           DISPLAY "NUMBER COUNT: " NUM-COUNT

           DISPLAY "BEGINING TRANSACTION"

      * NOTE: REQUIRED TO USE CURSORS
           EXEC SQL
             START TRANSACTION
           END-EXEC

           DISPLAY "CREATING CURSOR"

           EXEC SQL
             DECLARE CUR CURSOR FOR
             SELECT NUMBER FROM NUMBERS
      * NOTE: BUG IF MISSING PERIOD HERE
           END-EXEC.
           EXEC SQL
             OPEN CUR
           END-EXEC

           DISPLAY "SUMMING NUMBERS"

           EXEC SQL
             FETCH CUR INTO :NUM
           END-EXEC
           PERFORM UNTIL SQLCODE NOT = ZERO
              ADD NUM TO NUM-SUM
              EXEC SQL
                FETCH CUR INTO :NUM
              END-EXEC
           END-PERFORM
           DISPLAY "NUMBER SUM: " NUM-SUM

           DISPLAY "CLOSING CURSOR"

           EXEC SQL
             CLOSE CUR
           END-EXEC

           DISPLAY "ENDING TRANSACTION"

           EXEC SQL
             COMMIT WORK
           END-EXEC.

           DISPLAY "DISCONNECTING"

           EXEC SQL
             DISCONNECT ALL
           END-EXEC

           DISPLAY "DONE"
           STOP RUN.

       SQL_ERROR.
           DISPLAY "SQL ERROR !"
           DISPLAY "SQLCODE: " SQLCODE
           DISPLAY "ERRCODE: " SQLSTATE
           DISPLAY SQLERRMC.

