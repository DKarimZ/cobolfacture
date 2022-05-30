       IDENTIFICATION DIVISION. 
       PROGRAM-ID. ASSERT-EQUAL2.

      *> Ce programme sert à nofifier les erreurs lors de l'execution
      *> des testts unitaires 

       DATA DIVISION. 
       WORKING-STORAGE SECTION.
       LINKAGE SECTION. 
              
       COPY 'test-context.cpy'.

       01 TEST-NAME    PIC x(30).
       01 EXPECTED1     PIC 9(5)V99.
       01 ACTUAL1       PIC 9(5)V99.
       01 EXPECTED2     PIC X(3).
       01 ACTUAL2       PIC X(3).
       01 EXPECTED3     PIC X(25).
       01 ACTUAL3       PIC X(25).

       PROCEDURE DIVISION USING TEST-CONTEXT,TEST-NAME, EXPECTED1,
        ACTUAL1, EXPECTED2, ACTUAL2,EXPECTED3,ACTUAL3.

       0100-MAIN-PROCEDURE.
           ADD 1 TO TESTS-RUN.
           IF ACTUAL1 = EXPECTED1 AND ACTUAL2 = EXPECTED2
           AND ACTUAL3 = EXPECTED3
            then ADD 1 TO PASSES
           ELSE 
           DISPLAY 'FAILED: ' TEST-NAME '. résultat attendu: 'EXPECTED3 
              ' résultat obtenu ' ACTUAL3
              ADD 1 TO FAILURES.
           GOBACK.
       END PROGRAM ASSERT-EQUAL2.
