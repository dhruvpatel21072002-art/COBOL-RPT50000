       IDENTIFICATION DIVISION.
       PROGRAM-ID. RPT5000.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO CUSTMAST
               ORGANIZATION IS SEQUENTIAL.

           SELECT REPORT-FILE
               ASSIGN TO RPT5000
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD  CUSTOMER-FILE
           RECORD CONTAINS 130 CHARACTERS
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 130 CHARACTERS.
       01  CUSTOMER-MASTER-RECORD.
           05 CM-BRANCH-NUMBER        PIC 9(2).
           05 CM-SALESREP-NUMBER      PIC 9(2).
           05 CM-CUSTOMER-NUMBER      PIC 9(5).
           05 CM-CUSTOMER-NAME        PIC X(20).
           05 CM-SALES-THIS-YTD       PIC 9(7).
           05 CM-SALES-LAST-YTD       PIC 9(7).
           05 FILLER                  PIC X(87).

       FD  REPORT-FILE
           RECORD CONTAINS 130 CHARACTERS
           LABEL RECORDS ARE STANDARD
           BLOCK CONTAINS 130 CHARACTERS.
       01  REPORT-REC                 PIC X(130).

       WORKING-STORAGE SECTION.

      *----------------------------------------------------------------*
      * SWITCHES
      *----------------------------------------------------------------*
       01  WS-SWITCHES.
           05 WS-END-OF-FILE          PIC X VALUE 'N'.
               88  END-OF-FILE        VALUE 'Y'.
           05 WS-FIRST-RECORD         PIC X VALUE 'Y'.
               88  FIRST-RECORD       VALUE 'Y' false "N".

       01  WS-PAGE-NO                 PIC 9(3) VALUE 0.
       01  WS-LINE-COUNT              PIC 99 VALUE 99.
       01  WS-MAX-LINES               PIC 99 VALUE 50.

       01  WS-DATE-FIELDS.
           05 WS-YYYY                 PIC 9(4).
           05 WS-MM                   PIC 9(2).
           05 WS-DD                   PIC 9(2).

       01  WS-FORMATTED-DATE.
           05 FMT-MM                  PIC 99.
           05 FILLER                  PIC X VALUE '/'.
           05 FMT-DD                  PIC 99.
           05 FILLER                  PIC X VALUE '/'.
           05 FMT-YYYY                PIC 9(4).

       01  WS-FORMATTED-TIME.
           05 FMT-HH                  PIC 99.
           05 FILLER                  PIC X VALUE ':'.
           05 FMT-MI                  PIC 99.

       01  WS-SYSTEM-DATE             PIC 9(8).
       01  WS-SYSTEM-TIME             PIC 9(8).

       01  WS-HOLD-FIELDS.
           05 WS-BRANCH-HOLD          PIC 9(2) VALUE 0.
           05 WS-SALESREP-HOLD        PIC 9(2) VALUE 0.

       01  WS-CALC-FIELDS.
           05 WS-THIS-AMT             PIC S9(7)V99 VALUE 0.
           05 WS-LAST-AMT             PIC S9(7)V99 VALUE 0.
           05 WS-CHANGE-AMT           PIC S9(7)V99 VALUE 0.
           05 WS-CHANGE-PCT           PIC S9(5)V9  VALUE 0.

      *----------------------------------------------------------------*
      * SALESREP TOTALS
      *----------------------------------------------------------------*
       01  WS-SALESREP-TOTALS.
           05 WS-SR-THIS-TOT          PIC S9(9)V99 VALUE 0.
           05 WS-SR-LAST-TOT          PIC S9(9)V99 VALUE 0.
           05 WS-SR-CHG-TOT           PIC S9(9)V99 VALUE 0.
           05 WS-SR-PCT               PIC S9(5)V9  VALUE 0.

       01  WS-BRANCH-TOTALS.
           05 WS-BR-THIS-TOT          PIC S9(9)V99 VALUE 0.
           05 WS-BR-LAST-TOT          PIC S9(9)V99 VALUE 0.
           05 WS-BR-CHG-TOT           PIC S9(9)V99 VALUE 0.
           05 WS-BR-PCT               PIC S9(5)V9  VALUE 0.

       01  WS-GRAND-TOTALS.
           05 WS-GR-THIS-TOT          PIC S9(9)V99 VALUE 0.
           05 WS-GR-LAST-TOT          PIC S9(9)V99 VALUE 0.
           05 WS-GR-CHG-TOT           PIC S9(9)V99 VALUE 0.
           05 WS-GR-PCT               PIC S9(5)V9  VALUE 0.

       01  WS-EDIT-FIELDS.
           05 ED-THIS                 PIC ZZ,ZZZ,ZZ9.99.
           05 ED-LAST                 PIC ZZ,ZZZ,ZZ9.99.
           05 ED-CHG                  PIC ZZ,ZZZ,ZZ9.99-.
           05 ED-PCT                  PIC ZZ9.9-.

       01  HEADING-1.
           05 FILLER                  PIC X(6)  VALUE 'DATE: '.
           05 H1-DATE                 PIC X(10).
           05 FILLER                  PIC X(24) VALUE SPACES.
           05 FILLER                  PIC X(25) VALUE 'YTD SALE REPORT'.
           05 FILLER                  PIC X(18) VALUE SPACES.
           05 FILLER                  PIC X(6)  VALUE 'PAGE: '.
           05 H1-PAGE                 PIC ZZ9.

       01  HEADING-2.
           05 FILLER                  PIC X(6)  VALUE 'TIME: '.
           05 H2-TIME                 PIC X(5).
           05 FILLER                  PIC X(103) VALUE SPACES.
           05 FILLER                  PIC X(7)  VALUE 'RPT5000'.

       01  HEADING-3.
           05 FILLER PIC X(130) VALUE SPACES.

       01  HEADING-4.
           05 FILLER PIC X(7)   VALUE 'BRANCH '.
           05 FILLER PIC X(6)   VALUE 'SALES '.
           05 FILLER PIC X(5)   VALUE 'CUST '.
           05 FILLER PIC X(21)  VALUE 'CUSTOMER NAME      '.
           05 FILLER PIC X(13)  VALUE 'SALES        '.
           05 FILLER PIC X(13)  VALUE 'SALES        '.
           05 FILLER PIC X(13)  VALUE 'CHANGE       '.
           05 FILLER PIC X(14)  VALUE 'CHANGE        '.
           05 FILLER PIC X(38)  VALUE SPACES.

       01  HEADING-5.
           05 FILLER PIC X(7)   VALUE 'NUM    '.
           05 FILLER PIC X(6)   VALUE 'REP   '.
           05 FILLER PIC X(5)   VALUE 'NUM  '.
           05 FILLER PIC X(21)  VALUE SPACES.
           05 FILLER PIC X(13)  VALUE 'THIS YTD     '.
           05 FILLER PIC X(13)  VALUE 'LAST YTD     '.
           05 FILLER PIC X(13)  VALUE 'AMOUNT       '.
           05 FILLER PIC X(14)  VALUE 'PERCENT      '.
           05 FILLER PIC X(38)  VALUE SPACES.

       01  DETAIL-LINE.
           05 DL-BRANCH               PIC X(2).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 DL-SALESREP             PIC X(2).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 DL-CUST                 PIC X(5).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 DL-NAME                 PIC X(20).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 DL-THIS                 PIC X(12).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 DL-LAST                 PIC X(12).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 DL-CHG                  PIC X(12).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 DL-PCT                  PIC X(6).
           05 FILLER                  PIC X(45) VALUE SPACES.

       01  TOTAL-LINE.
           05 FILLER                  PIC X(18) VALUE SPACES.
           05 TL-LABEL                PIC X(20).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 TL-THIS                 PIC X(12).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 TL-LAST                 PIC X(12).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 TL-CHG                  PIC X(12).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 TL-PCT                  PIC X(6).
           05 FILLER                  PIC X(42) VALUE SPACES.

       01  SALESREP-TOTAL-LINE.


           05 FILLER                  PIC X(18) VALUE SPACES.
           05 SR-TL-LABEL             PIC X(20).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 SR-TL-THIS              PIC X(12).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 SR-TL-LAST              PIC X(12).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 SR-TL-CHG               PIC X(12).
           05 FILLER                  PIC X(2)  VALUE SPACES.
           05 SR-TL-PCT               PIC X(6).
           05 FILLER                  PIC X(42) VALUE SPACES.


       01  DASH-LINE.
           05 FILLER PIC X(56) VALUE SPACES.
           05 FILLER PIC X(12) VALUE '------------'.
           05 FILLER PIC X(2)  VALUE SPACES.
           05 FILLER PIC X(12) VALUE '------------'.
           05 FILLER PIC X(2)  VALUE SPACES.
           05 FILLER PIC X(12) VALUE '------------'.
           05 FILLER PIC X(2)  VALUE SPACES.
           05 FILLER PIC X(6)  VALUE '------'.
           05 FILLER PIC X(26) VALUE SPACES.

       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-INITIALIZE
           PERFORM 200-PROCESS-RECORDS UNTIL END-OF-FILE
           PERFORM 300-FINAL-SALESREP
           PERFORM 310-FINAL-BRANCH
           PERFORM 400-GRAND-TOTAL
           PERFORM 900-CLOSE-FILES
           GOBACK
           .

       100-INITIALIZE.
           OPEN INPUT CUSTOMER-FILE
           OPEN OUTPUT REPORT-FILE

           ACCEPT WS-SYSTEM-DATE FROM DATE YYYYMMDD
           ACCEPT WS-SYSTEM-TIME FROM TIME

           MOVE WS-SYSTEM-DATE(1:4) TO WS-YYYY
           MOVE WS-SYSTEM-DATE(5:2) TO WS-MM
           MOVE WS-SYSTEM-DATE(7:2) TO WS-DD

           MOVE WS-MM   TO FMT-MM
           MOVE WS-DD   TO FMT-DD
           MOVE WS-YYYY TO FMT-YYYY

           MOVE WS-SYSTEM-TIME(1:2) TO FMT-HH
           MOVE WS-SYSTEM-TIME(3:2) TO FMT-MI

           PERFORM 110-READ-CUSTOMER

           IF NOT END-OF-FILE
               MOVE CM-BRANCH-NUMBER    TO WS-BRANCH-HOLD
               MOVE CM-SALESREP-NUMBER  TO WS-SALESREP-HOLD
               PERFORM 120-WRITE-HEADINGS
           END-IF
           .

       110-READ-CUSTOMER.
           READ CUSTOMER-FILE
               AT END
                   SET END-OF-FILE TO TRUE
           END-READ
           .

       120-WRITE-HEADINGS.
           ADD 1 TO WS-PAGE-NO
           MOVE 0 TO WS-LINE-COUNT

           MOVE WS-FORMATTED-DATE TO H1-DATE
           MOVE WS-PAGE-NO        TO H1-PAGE
           MOVE WS-FORMATTED-TIME TO H2-TIME

           MOVE HEADING-1 TO REPORT-REC
           WRITE REPORT-REC

           MOVE HEADING-2 TO REPORT-REC
           WRITE REPORT-REC

           MOVE HEADING-3 TO REPORT-REC
           WRITE REPORT-REC

           MOVE HEADING-4 TO REPORT-REC
           WRITE REPORT-REC

           MOVE HEADING-5 TO REPORT-REC
           WRITE REPORT-REC

           MOVE SPACES TO REPORT-REC
           WRITE REPORT-REC
           .

       200-PROCESS-RECORDS.
           IF END-OF-FILE
               EXIT PARAGRAPH
           END-IF

           EVALUATE TRUE
               WHEN CM-BRANCH-NUMBER NOT = WS-BRANCH-HOLD
                   PERFORM 300-FINAL-SALESREP
                   PERFORM 310-FINAL-BRANCH
                   MOVE CM-BRANCH-NUMBER   TO WS-BRANCH-HOLD
                   MOVE CM-SALESREP-NUMBER TO WS-SALESREP-HOLD
               WHEN CM-SALESREP-NUMBER NOT = WS-SALESREP-HOLD
                   PERFORM 300-FINAL-SALESREP
                   MOVE CM-SALESREP-NUMBER TO WS-SALESREP-HOLD
               WHEN OTHER
                   CONTINUE
           END-EVALUATE

           PERFORM 210-BUILD-DETAIL
           PERFORM 220-WRITE-DETAIL
           PERFORM 110-READ-CUSTOMER
           .

       210-BUILD-DETAIL.
           COMPUTE WS-THIS-AMT   = CM-SALES-THIS-YTD / 100
           COMPUTE WS-LAST-AMT   = CM-SALES-LAST-YTD / 100
           COMPUTE WS-CHANGE-AMT = WS-THIS-AMT - WS-LAST-AMT

           IF WS-LAST-AMT NOT = 0
               COMPUTE WS-CHANGE-PCT ROUNDED =
                   (WS-CHANGE-AMT / WS-LAST-AMT) * 100
           ELSE
               MOVE 999.9 TO WS-CHANGE-PCT
           END-IF

           ADD WS-THIS-AMT   TO WS-SR-THIS-TOT
           ADD WS-LAST-AMT   TO WS-SR-LAST-TOT
           ADD WS-CHANGE-AMT TO WS-SR-CHG-TOT

           ADD WS-THIS-AMT   TO WS-BR-THIS-TOT
           ADD WS-LAST-AMT   TO WS-BR-LAST-TOT
           ADD WS-CHANGE-AMT TO WS-BR-CHG-TOT

           ADD WS-THIS-AMT   TO WS-GR-THIS-TOT
           ADD WS-LAST-AMT   TO WS-GR-LAST-TOT
           ADD WS-CHANGE-AMT TO WS-GR-CHG-TOT

           MOVE SPACES TO DETAIL-LINE

           EVALUATE TRUE
               WHEN FIRST-RECORD
                   MOVE CM-BRANCH-NUMBER   TO DL-BRANCH
                   MOVE CM-SALESREP-NUMBER TO DL-SALESREP
                   SET FIRST-RECORD        TO FALSE
               WHEN CM-BRANCH-NUMBER = WS-BRANCH-HOLD
                 AND CM-SALESREP-NUMBER = WS-SALESREP-HOLD
                   MOVE SPACES TO DL-BRANCH
                   MOVE SPACES TO DL-SALESREP
               WHEN CM-BRANCH-NUMBER NOT = WS-BRANCH-HOLD
                   MOVE CM-BRANCH-NUMBER   TO DL-BRANCH
                   MOVE CM-SALESREP-NUMBER TO DL-SALESREP
               WHEN OTHER
                   MOVE SPACES             TO DL-BRANCH
                   MOVE CM-SALESREP-NUMBER TO DL-SALESREP
           END-EVALUATE

           MOVE CM-CUSTOMER-NUMBER TO DL-CUST
           MOVE CM-CUSTOMER-NAME   TO DL-NAME

           MOVE WS-THIS-AMT   TO ED-THIS
           MOVE WS-LAST-AMT   TO ED-LAST
           MOVE WS-CHANGE-AMT TO ED-CHG
           MOVE WS-CHANGE-PCT TO ED-PCT

           MOVE ED-THIS TO DL-THIS
           MOVE ED-LAST TO DL-LAST
           MOVE ED-CHG  TO DL-CHG
           MOVE ED-PCT  TO DL-PCT
           .

       220-WRITE-DETAIL.
           IF WS-LINE-COUNT > WS-MAX-LINES
               PERFORM 120-WRITE-HEADINGS
           END-IF

           MOVE DETAIL-LINE TO REPORT-REC
           WRITE REPORT-REC
           ADD 1 TO WS-LINE-COUNT
           .

      *----------------------------------------------------------------*
      * SALESREP TOTAL
      *----------------------------------------------------------------*
       300-FINAL-SALESREP.
           IF WS-SR-THIS-TOT = 0 AND WS-SR-LAST-TOT = 0
              AND WS-SR-CHG-TOT = 0
               EXIT PARAGRAPH
           END-IF

           IF WS-SR-LAST-TOT NOT = 0
               COMPUTE WS-SR-PCT ROUNDED =
                   (WS-SR-CHG-TOT / WS-SR-LAST-TOT) * 100
           ELSE
               MOVE 999.9 TO WS-SR-PCT
           END-IF

           MOVE DASH-LINE TO REPORT-REC
           WRITE REPORT-REC

           MOVE SPACES           TO TOTAL-LINE
           MOVE '** SALESREP TOTAL' TO TL-LABEL

           MOVE WS-SR-THIS-TOT TO ED-THIS
           MOVE WS-SR-LAST-TOT TO ED-LAST
           MOVE WS-SR-CHG-TOT  TO ED-CHG
           MOVE WS-SR-PCT      TO ED-PCT

           MOVE ED-THIS TO TL-THIS
           MOVE ED-LAST TO TL-LAST
           MOVE ED-CHG  TO TL-CHG
           MOVE ED-PCT  TO TL-PCT

           MOVE TOTAL-LINE TO REPORT-REC
           WRITE REPORT-REC

           MOVE SPACES TO REPORT-REC
           WRITE REPORT-REC

           MOVE 0 TO WS-SR-THIS-TOT
                     WS-SR-LAST-TOT
                     WS-SR-CHG-TOT
                     WS-SR-PCT
           .

      *----------------------------------------------------------------*
      * BRANCH TOTAL
      *----------------------------------------------------------------*
       310-FINAL-BRANCH.
           IF WS-BR-THIS-TOT = 0 AND WS-BR-LAST-TOT = 0
              AND WS-BR-CHG-TOT = 0
               EXIT PARAGRAPH
           END-IF

           IF WS-BR-LAST-TOT NOT = 0
               COMPUTE WS-BR-PCT ROUNDED =
                   (WS-BR-CHG-TOT / WS-BR-LAST-TOT) * 100
           ELSE
               MOVE 999.9 TO WS-BR-PCT
           END-IF

           MOVE DASH-LINE TO REPORT-REC
           WRITE REPORT-REC

           MOVE SPACES            TO TOTAL-LINE
           MOVE '*** BRANCH TOTAL' TO TL-LABEL

           MOVE WS-BR-THIS-TOT TO ED-THIS
           MOVE WS-BR-LAST-TOT TO ED-LAST
           MOVE WS-BR-CHG-TOT  TO ED-CHG
           MOVE WS-BR-PCT      TO ED-PCT

           MOVE ED-THIS TO TL-THIS
           MOVE ED-LAST TO TL-LAST
           MOVE ED-CHG  TO TL-CHG
           MOVE ED-PCT  TO TL-PCT

           MOVE TOTAL-LINE TO REPORT-REC
           WRITE REPORT-REC

           MOVE SPACES TO REPORT-REC
           WRITE REPORT-REC

           MOVE 0 TO WS-BR-THIS-TOT
                     WS-BR-LAST-TOT
                     WS-BR-CHG-TOT
                     WS-BR-PCT
           .

       400-GRAND-TOTAL.
           IF WS-GR-LAST-TOT NOT = 0
               COMPUTE WS-GR-PCT ROUNDED =
                   (WS-GR-CHG-TOT / WS-GR-LAST-TOT) * 100
           ELSE
               MOVE 999.9 TO WS-GR-PCT
           END-IF

           MOVE DASH-LINE TO REPORT-REC
           WRITE REPORT-REC

           MOVE SPACES              TO TOTAL-LINE
           MOVE '**** GRAND TOTAL'  TO TL-LABEL

           MOVE WS-GR-THIS-TOT TO ED-THIS
           MOVE WS-GR-LAST-TOT TO ED-LAST
           MOVE WS-GR-CHG-TOT  TO ED-CHG
           MOVE WS-GR-PCT      TO ED-PCT

           MOVE ED-THIS TO TL-THIS
           MOVE ED-LAST TO TL-LAST
           MOVE ED-CHG  TO TL-CHG
           MOVE ED-PCT  TO TL-PCT

           MOVE TOTAL-LINE TO REPORT-REC
           WRITE REPORT-REC
           .

       900-CLOSE-FILES.
           CLOSE CUSTOMER-FILE
                 REPORT-FILE
           .
