       IDENTIFICATION DIVISION. 
       PROGRAM-ID. ASSERT-EQUAL3.

      *> Ce programme sert à nofifier les erreurs lors de l'execution
      *> des testts unitaires 

       DATA DIVISION. 
       WORKING-STORAGE SECTION.
       LINKAGE SECTION. 
              
       COPY 'test-context.cpy'.

       01 TEST-NAME    PIC x(30).
       01 EXPECTED1     PIC 9(5)V99.
       01 ACTUAL1       PIC 9(5)V99.
       01 EXPECTED2     PIC X(30).
       01 ACTUAL2       PIC X(30).

       PROCEDURE DIVISION USING TEST-CONTEXT,TEST-NAME, EXPECTED1,
        ACTUAL1, EXPECTED2, ACTUAL2.

       0100-MAIN-PROCEDURE.
           ADD 2 TO TESTS-RUN.
           IF ACTUAL1 = EXPECTED1 then ADD 1 TO PASSES
           ELSE
           DISPLAY 'FAILED: ' TEST-NAME '. résultat attendu: 'EXPECTED1 
              ' résultat obtenu ' ACTUAL1
              ADD 1 TO FAILURES.
           IF ACTUAL2 = EXPECTED2 then ADD 1 TO PASSES
           ELSE
           DISPLAY 'FAILED: ' TEST-NAME '. résultat attendu: 'EXPECTED2
              ' résultat obtenu ' ACTUAL2
              ADD 1 TO FAILURES.
           GOBACK.
       END PROGRAM ASSERT-EQUAL3.
