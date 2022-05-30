       IDENTIFICATION DIVISION.
       PROGRAM-ID. DB2SQLMSG.
      
       ENVIRONMENT DIVISION.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ERROR                     PIC S9(9) COMP-5.
       01 WS-STATE                     PIC S9(9) COMP-5.
       01 WS-BUFFER-SIZE               PIC S9(4) COMP-5 
                                                 VALUE 1024.
      *> we do not need new line, therefore max width
       01 WS-LINE-WIDTH                PIC S9(4) COMP-5 
                                                 VALUE 1024.
       01 WS-ERROR-BUFFER              PIC X(1024).
       01 WS-STATE-BUFFER              PIC X(1024).
       
       LINKAGE SECTION.
       COPY "sqlca.cbl" REPLACING ==VALUE "SQLCA   "== BY == ==
                                  ==VALUE 136==        BY == ==.
       COPY "LNSQLMSG.cpy".
       
       PROCEDURE DIVISION USING SQLCA 
                                LN-SQLMSG.
                                
      *>------------------------------------------------------------------------
       MAIN-DB2SQLMSG SECTION.
      *>------------------------------------------------------------------------
      
          INITIALIZE LN-SQLMSG
          MOVE SPACES TO WS-ERROR-BUFFER
          MOVE SPACES TO WS-STATE-BUFFER
      
      *>  get SQLCODE message    
          CALL "sqlgintp" USING BY VALUE     WS-BUFFER-SIZE
                                BY VALUE     WS-LINE-WIDTH
                                BY REFERENCE SQLCA
                                BY REFERENCE WS-ERROR-BUFFER
                          RETURNING WS-ERROR
          END-CALL
          
      *>  replace hexa 00-0F with spaces
          INSPECT WS-ERROR-BUFFER CONVERTING
               X"000102030405060708090A0B0C0D0E0F" 
             TO "                "

      *>  replace hexa 10-1F with spaces
          INSPECT WS-ERROR-BUFFER CONVERTING
               X"101112131415161718191A1B1C1D1E1F" 
             TO "                "
             
      *>  replace German chars
      *>  INSPECT WS-ERROR-BUFFER CONVERTING
      *>        X"81848E94999AE1" 
      *>     TO X"7561416F4F5573"
             
      *>  get SQLSTATE message    
          CALL "sqlggstt" USING BY VALUE     WS-BUFFER-SIZE
                                BY VALUE     WS-LINE-WIDTH
                                BY REFERENCE SQLSTATE
                                BY REFERENCE WS-STATE-BUFFER
                          RETURNING WS-STATE
          END-CALL

      *>  replace hexa 00-0F with spaces
          INSPECT WS-STATE-BUFFER CONVERTING
               X"000102030405060708090A0B0C0D0E0F" 
             TO "                "

      *>  replace hexa 10-1F with spaces
          INSPECT WS-STATE-BUFFER CONVERTING
               X"101112131415161718191A1B1C1D1E1F" 
             TO "                "
             
      *>  replace German chars
      *>  INSPECT WS-STATE-BUFFER CONVERTING
      *>        X"81848E94999AE1" 
      *>     TO X"7561416F4F5573"
          
          IF WS-ERROR > ZEROES
          THEN
             MOVE WS-ERROR-BUFFER( 1:80) TO LN-MSG-1
             MOVE WS-ERROR-BUFFER(81:80) TO LN-MSG-2
          END-IF    
          
          IF WS-STATE > ZEROES
          THEN
             MOVE WS-STATE-BUFFER( 1:80) TO LN-MSG-3
             MOVE WS-STATE-BUFFER(81:80) TO LN-MSG-4
          END-IF    
      
          GOBACK
      
          .
       MAIN-DB2SQLMSG-EX.
          EXIT.
          
       END PROGRAM DB2SQLMSG.
       