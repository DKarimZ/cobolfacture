             IDENTIFICATION DIVISION.
       PROGRAM-ID. relevehtml.

      *> Ce programme permet de faire appel à un module facture et
      *> d'afficher les différentes opérations réalisées dans un
      *> format HTML

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION. 
       FILE-CONTROL. 

           SELECT FLYER ASSIGN TO "FLYRFILE.HTML".

           SELECT DATAS ASSIGN TO "PRINTFILE.DAT"
           ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD FLYER.
       01 FLYER-FILE        PIC X(10000) VALUE SPACES.

       FD DATAS.
       01 DATA-DETAILS.
           88 END-OF-FILE      VALUES HIGH-VALUES.
           02 INV-DATA.
            05 INC-RECORD.
      *          10 D-CLIENT-ID               PIC S9(3).
      *          10 FILLER                  PIC X(3) VALUE SPACES.
                10 D-CLIENT-NOM              PIC X(20).
                10 D-CLIENT-PRENOM           PIC X(20).
      *          10 D-CLIENT-TYPECLIENT       PIC X(20).
      *          10 D-CLIENT-EXTERNE          PIC 9.
      *          10 D-COMPTE-ID               PIC 9(3).
      *          10 FILLER                  PIC X(3) VALUE SPACES.
      *          10 D-COMPTE-IBAN             PIC X(50).
      *          10 D-COMPTE-DTOUV            PIC X(14).
                10 D-COMPTE-SOLDE            PIC 9(5).
                10 FILLER                  PIC X(3) VALUE SPACES.
                10 D-LIBELLE-OP              PIC X(30).
                10 FILLER                  PIC X(3) VALUE SPACES.
                10 D-MONTANT-OP              PIC 9(5)V99.
                10 FILLER                  PIC X(3) VALUE SPACES.
                10 D-DATE-OP                 PIC X(10).
      *          10 D-STATUS-OP               PIC X(20).
                                
                

       WORKING-STORAGE SECTION.
       01  DATAS-LINE                PIC X(146)  VALUES SPACES.
       
       01  INV-REC-CNT               PIC 9(1) VALUE 1.
  
       01  TODAYS-DATE               PIC 9(8) VALUE 20220525.
       01  TODAYS-DATE-INT           PIC 9(10).
       01  SALE-END-DATE-INT         PIC 9(10).
       01  prod-img-broken           PIC x(99) VALUE "https://ibmzxplore
      -     "-static.s3.eu-gb.cloud-object-storage.appdomain.cloud
      -     "unknown.png".
 
       01  WS-DATAS.
      *     05 CLIENT-ID               PIC S9(3).
      *     05 FILLER                  PIC X(3) VALUE SPACES.
           05 CLIENT-NOM              PIC X(20).
           05 CLIENT-PRENOM           PIC X(20).
      *     05 CLIENT-TYPECLIENT       PIC X(15).
      **     05 CLIENT-EXTERNE          PIC 9.
      **     05 COMPTE-ID               PIC S9(3).
      **     05 FILLER                  PIC X(3) VALUE SPACES.
      *     05 COMPTE-IBAN             PIC X(25).
      *     05 COMPTE-DTOUV            PIC X(14).
           05 COMPTE-SOLDE            PIC 9(5).
           05 FILLER                  PIC X(3) VALUE SPACES.
           05 LIBELLE-OP              PIC X(30).
           05 MONTANT-OP              PIC 9(5).99.
           05 DATE-OP                 PIC X(10).
           
           

       01  NBRECORD                PIC 9(2).
 
       01  DAYSTOEXPIRY              PIC ZZ9.
       01  DAYSTOSELLALL             PIC ZZ9.
       01  EXPIRY-DATE-INT           PIC 9(10).
       01  FLYERFORMAT               PIC x(4)  VALUE 'HTML'.

       1 HTMLHEADER1 pic x(151) value "<html><head><style>body{font-fami
      -    "ly:IBM Plex Sans;color:black;}img{width:2
      -    "50px;}table{margin-left:auto;margin-right:auto;border:2px ".
       1 HTMLHEADER2 pic x(151) value "solid black;width:900px;backgroun
      -    "d:white;}#title{text-align:center;font-family:IBM Plex Sans;
      -    "}.price{font-size:15px;}.discount{;fo".
       1 HTMLHEADER3 pic x(300) value "nt-size:15px;}.product{font-size:
      -    "15px;}.tab{border-bottom: 0px;
      -    "border-right: 0px; border-left:0px;}
      -     "#footer{text-align:center;font-size:larger;}</style></
      -    "head><body><div id=""title""><h1>Releve d'operations</h1>
      -    "</div>".
       1 HTMLTABLESTART    pic x(100) value "<table class=""tab"">
      -    "<tr><td colspan
      -    "=3>".
       1 HTMLPRICE       pic x(95) value """></td></tr> <tr><td class=
      -    """price"">".
       1 HTMLDISCOUNT      pic x(37) value "</td><td><span class=""disco
      -    "unt"">date ".
       1 HTMLPRODUCT       pic x(35) value "</span><br><span class=""pro
      -    "duct"">".
       1 HTMLOLDPRICE      pic x(12) value "<br> Solde: ".
       1 HTMLTABLEEND pic x(138) value "</span></td></tr></table><br>
      -    "</div>".
       1 HTMLFLYERFOOTER   pic x(20) value "<div id=""footer""><p>".
       1 HTMLFOOTER        pic x(24) value "</p></div></body></html>".


       PROCEDURE DIVISION.

           
           OPEN INPUT DATAS. 
           READ DATAS
              AT END SET END-OF-FILE TO TRUE
       
           END-READ
           MOVE INV-DATA  TO  DATAS-LINE  

           COMPUTE  TODAYS-DATE-INT  =
              FUNCTION INTEGER-OF-DATE(TODAYS-DATE )

           OPEN OUTPUT FLYER
           INITIALIZE  FLYER-FILE 
              STRING HTMLHEADER1  HTMLHEADER2  HTMLHEADER3
 
                 DELIMITED BY SIZE INTO FLYER-FILE 
           WRITE FLYER-FILE.
           READ DATAS
              AT END SET END-OF-FILE TO TRUE
           END-READ
           PERFORM UNTIL END-OF-FILE

                 MOVE D-LIBELLE-OP  TO LIBELLE-OP 
                 MOVE D-MONTANT-OP  TO MONTANT-OP 
                 MOVE D-COMPTE-SOLDE TO COMPTE-SOLDE
                 MOVE D-DATE-OP  TO DATE-OP      

                 INITIALIZE FLYER-FILE
                 
                    STRING
                      HTMLTABLESTART  HTMLPRODUCT LIBELLE-OP  HTMLPRICE
                      "$" MONTANT-OP HTMLDISCOUNT DATE-OP
                      HTMLTABLEEND 
                      DELIMITED BY SIZE
                  INTO FLYER-FILE
                 
                 WRITE FLYER-FILE
             
          
           READ DATAS
              AT END SET END-OF-FILE TO TRUE
           END-READ
           END-PERFORM

           COMPUTE SALE-END-DATE-INT  = TODAYS-DATE-INT + 7
           INITIALIZE FLYER-FILE 
           IF FLYERFORMAT  NOT = 'TEXT' THEN
              MOVE HTMLFLYERFOOTER  TO FLYER-FILE 
              WrITE FLYEr-FILE
           END-IF 

           STRING
              "Relevé de compte édité le  "
              FUNCTION FORMATTED-DATE ("YYYY-MM-DD" TODAYS-DATE-INT )
              DELIMITED BY SIZE
              INTO FLYER-FILE 
           WRITE FLYER-FILE

           STRING " VOTRE SOLDE EST DE $ "  COMPTE-SOLDE " A "
            DELIMITED BY 
           SIZE INTO FLYER-FILE 
           WRITE FLYER-FILE
           

           IF FLYERFORMAT  NOT = 'TEXT' THEN
              MOVE HTMLFOOTER  TO FLYER-FILE 
           WRITE FLYER-FILE 
           END-IF

           CLOSE FLYER, DATAS

           GOBACK.
       END PROGRAM relevehtml.
