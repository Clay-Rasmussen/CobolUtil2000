000100 IDENTIFICATION DIVISION.                                         00010099
000200 PROGRAM-ID. UTIL1000.                                            00020099
000300***************************************************************** 00030099
000400*  Programmer.:                                                   00040099
000500*  Date.......:                                                   00050099
000600*  GitHub URL.:                                                   00060099
000700*  Description:                                                   00070099
000800***************************************************************** 00080099
000900 DATA DIVISION.                                                   00090099
001000 WORKING-STORAGE SECTION.                                         00100099
001100                                                                  00110099
001200***************************************************************** 00120099
001300* CONSTANTS                                                       00130099
001400***************************************************************** 00140099
001500 01  WS-RATE-TIER1            PIC V99      VALUE .12.             00150099
001600 01  WS-RATE-TIER2            PIC V99      VALUE .15.             00160099
001700 01  WS-RATE-TIER3            PIC V99      VALUE .18.             00170099
001800 01  WS-TIER1-LIMIT           PIC 9(4)     VALUE 500.             00180099
001900 01  WS-TIER2-LIMIT           PIC 9(4)     VALUE 500.             00190099
002000                                                                  00200099
002100***************************************************************** 00210099
002200* 3 PREDEFINED CUSTOMERS (NO TABLES)                              00220099
002300***************************************************************** 00230099
002400 01  WS-CUST1.                                                    00240099
002500     05  WS-C1-NAME           PIC X(12)   VALUE 'CUST-ALPHA  '.   00250099
002600     05  WS-C1-KWH            PIC 9(5)    VALUE 350.              00260099
002700     05  WS-C1-FEE            PIC 9(3)V99 VALUE 14.95.            00270099
002800                                                                  00280099
002900***************************************************************** 00290099
003000* CURRENT "INPUT" FIELDS (LOADED PER CUSTOMER)                    00300099
003100***************************************************************** 00310099
003200 01  WS-CUST-NAME             PIC X(12)   VALUE SPACES.           00320099
003300 01  WS-KWH-USED              PIC 9(5)    VALUE 0.                00330099
003400 01  WS-SERVICE-FEE           PIC 9(3)V99 VALUE 0.                00340099
003500                                                                  00350099
003600***************************************************************** 00360099
003700* WORK AREAS                                                      00370099
003800***************************************************************** 00380099
003900 01  WS-TIER1-KWH             PIC 9(5)     VALUE 0.               00390099
004000 01  WS-TIER2-KWH             PIC 9(5)     VALUE 0.               00400099
004100 01  WS-TIER3-KWH             PIC 9(5)     VALUE 0.               00410099
004200                                                                  00420099
004300 01  WS-TIER1-CHARGE          PIC 9(5)V99  VALUE 0.               00430099
004400 01  WS-TIER2-CHARGE          PIC 9(5)V99  VALUE 0.               00440099
004500 01  WS-TIER3-CHARGE          PIC 9(5)V99  VALUE 0.               00450099
004600                                                                  00460099
004700 01  WS-SUBTOTAL              PIC 9(6)V99  VALUE 0.               00470099
004800 01  WS-TOTAL-BILL            PIC 9(6)V99  VALUE 0.               00480099
004900                                                                  00490099
005000***************************************************************** 00500099
005100* EDITED FIELDS FOR DISPLAY                                       00510099
005200***************************************************************** 00520099
005300 01  WS-KWH-USED-ED           PIC Z,ZZZ,ZZZ,ZZ9.                  00530099
005400 01  WS-MONEY-ED              PIC $$,$$$,$$9.99.                  00540099
005500 01  WS-MONEY-ED2             PIC $$,$$$,$$9.99.                  00550099
005600                                                                  00560099
005700***************************************************************** 00570099
005800* IT'S GO TIME!                                                   00580099
005900***************************************************************** 00590099
006000 PROCEDURE DIVISION.                                              00600099
006100                                                                  00610099
006200***************************************************************** 00620099
006300* MAINLINE - DISPLAY HEADING, LOAD CUSTOMER, RUN BILL, STOP       00630099
006400***************************************************************** 00640099
006500 000-MAIN.                                                        00650099
006600     DISPLAY '********************************'.                  00660099
006700     DISPLAY '*** UTIL1000 - CUSTOMER BILL ***'.                  00670099
006800     DISPLAY '********************************'.                  00680099
006900     DISPLAY ' '.                                                 00690099
007000                                                                  00700099
007100     PERFORM 500-LOAD-CUST.                                       00710099
007200     PERFORM 600-RUN-BILL.                                        00720099
007300                                                                  00730099
007400     STOP RUN.                                                    00740099
007500                                                                  00750099
007600***************************************************************** 00760099
007700* MOVE name/kwh/fee from CUST into current fields.                00770099
007800***************************************************************** 00780099
007900 500-LOAD-CUST.                                                   00790099
008000     MOVE WS-C1-NAME TO WS-CUST-NAME.                             00800099
008100     MOVE WS-C1-KWH  TO WS-KWH-USED.                              00810099
008200     MOVE WS-C1-FEE  TO WS-SERVICE-FEE.                           00820099
008300                                                                  00830099
008400***************************************************************** 00840099
008500* BILL ROUTINE                                                    00850099
008600***************************************************************** 00860099
008700 600-RUN-BILL.                                                    00870099
008800     PERFORM 100-INITIALIZE.                                      00880099
008900     PERFORM 200-CALC-TIERS.                                      00890099
009000     PERFORM 300-CALC-CHARGES.                                    00900099
009100     PERFORM 400-DISPLAY-RESULTS.                                 00910099
009200     DISPLAY ' '.                                                 00920099
009300                                                                  00930099
009400***************************************************************** 00940099
009500* Zero tier kWh, charges, subtotal, total                         00950099
009600***************************************************************** 00960099
009700 100-INITIALIZE.                                                  00970099
009800     MOVE 0 TO WS-TIER1-KWH                                       00980099
009900              WS-TIER2-KWH                                        00990099
010000              WS-TIER3-KWH                                        01000099
010100              WS-TIER1-CHARGE                                     01010099
010200              WS-TIER2-CHARGE                                     01020099
010300              WS-TIER3-CHARGE                                     01030099
010400              WS-SUBTOTAL                                         01040099
010500              WS-TOTAL-BILL.                                      01050099
010600                                                                  01060099
010700***************************************************************** 01070099
010800* Determine WS-TIER1-KWH, WS-TIER2-KWH, WS-TIER3-KWH              01080099
010900* based on WS-KWH-USED                                            01090099
011000*                                                                 01100099
011100* These are the per-kWh rates:                                    01110099
011200* - Tier 1: first 500 kWh at $0.12/kWh                            01120099
011300* - Tier 2: next 500 kWh (kWh 401 1000) at $0.15/kWh              01130099
011400* - Tier 3: any kWh above 1000 at $0.18/kWh                       01140099
011500***************************************************************** 01150099
011600 200-CALC-TIERS.                                                  01160099
011700     *> If amount used is less than 500 kWh, all goes in tier 1   01170099
011800     IF WS-KWH-USED <= WS-TIER1-LIMIT                             01180099
011900         MOVE WS-KWH-USED TO WS-TIER1-KWH                         01190099
012000         MOVE 0 TO WS-TIER2-KWH WS-TIER3-KWH                      01200099
012100     ELSE                                                         01210099
012200         MOVE WS-TIER1-LIMIT TO WS-TIER1-KWH                      01220099
012300                                                                  01230099
012400         *> If amount used is between 501 and 1000 kWh,           01240099
012500         *> tier 1 is full, remainder goes in tier 2              01250099
012600         IF WS-KWH-USED <= (WS-TIER1-LIMIT + WS-TIER2-LIMIT)      01260099
012700             COMPUTE WS-TIER2-KWH =                               01270099
012800                 WS-KWH-USED - WS-TIER1-LIMIT                     01280099
012900             MOVE 0 TO WS-TIER3-KWH                               01290099
013000                                                                  01300099
013100         *> If amount used is between 1001 and above,             01310099
013200         *> tier 1 and tier 2 are full, remainder goes in tier 3  01320099
013300         ELSE                                                     01330099
013400             MOVE WS-TIER2-LIMIT TO WS-TIER2-KWH                  01340099
013500             COMPUTE WS-TIER3-KWH =                               01350099
013600                 WS-KWH-USED - WS-TIER1-LIMIT - WS-TIER2-LIMIT    01360099
013700         END-IF                                                   01370099
013800     END-IF.                                                      01380099
013900                                                                  01390099
014000***************************************************************** 01400099
014100* COMPUTE charges using ROUNDED and compute totals.               01410099
014200***************************************************************** 01420099
014300 300-CALC-CHARGES.                                                01430099
014400     COMPUTE WS-TIER1-CHARGE ROUNDED =                            01440099
014500         WS-TIER1-KWH * WS-RATE-TIER1.                            01450099
014600                                                                  01460099
014700     COMPUTE WS-SUBTOTAL = WS-TIER1-CHARGE.                       01470099
014800                                                                  01480099
014900     COMPUTE WS-TOTAL-BILL =                                      01490099
015000         WS-SUBTOTAL + WS-SERVICE-FEE.                            01500099
015100                                                                  01510099
015200***************************************************************** 01520099
015300* Display report including customer name.                         01530099
015400***************************************************************** 01540099
015500 400-DISPLAY-RESULTS.                                             01550099
015600     MOVE WS-KWH-USED TO WS-KWH-USED-ED.                          01560099
015700                                                                  01570099
015800     DISPLAY '--------------------------------'.                  01580099
015900     DISPLAY 'CUSTOMER: ' WS-CUST-NAME.                           01590099
016000     DISPLAY '--------------------------------'.                  01600099
016100     DISPLAY 'KWH USED       : ' WS-KWH-USED-ED.                  01610099
016200                                                                  01620099
016300     MOVE WS-SERVICE-FEE TO WS-MONEY-ED.                          01630099
016400     DISPLAY 'SERVICE FEE    : ' WS-MONEY-ED.                     01640099
016500                                                                  01650099
016600     MOVE WS-TIER1-CHARGE TO WS-MONEY-ED.                         01660099
016700     DISPLAY 'TIER 1 CHARGE  : ' WS-MONEY-ED.                     01670099
016800                                                                  01680099
016900     MOVE WS-TIER2-CHARGE TO WS-MONEY-ED.                         01690099
017000     DISPLAY 'TIER 2 CHARGE  : ' WS-MONEY-ED.                     01700099
017100                                                                  01710099
017200     MOVE WS-TIER3-CHARGE TO WS-MONEY-ED.                         01720099
017300     DISPLAY 'TIER 3 CHARGE  : ' WS-MONEY-ED.                     01730099
017400                                                                  01740099
017500     MOVE WS-TOTAL-BILL TO WS-MONEY-ED2.                          01750099
017600     DISPLAY '--------------------------------'.                  01760099
017700     DISPLAY 'TOTAL BILL     : ' WS-MONEY-ED2.                    01770099
017800     DISPLAY '--------------------------------'.                  01780099
