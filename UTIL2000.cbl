000100 IDENTIFICATION DIVISION.                                         00010000
000200 PROGRAM-ID. UTIL2000.                                            00020001
000300***************************************************************** 00030000
000400*  Programmer.: Clay Rasmussen                                    00040001
000500*  Date.......: February 10, 2025                                 00050001
000600*  GitHub URL.: github.com/Clay-Rasmussen/CobolUtil2000           00060003
000700*  Description: This COBOL program is a utility billing system    00070003
000710*  that calculates and prints monthly bills for multiple          00071003
000720*  customers based on their kilowatt-hours (kWh) used.            00072003
000800***************************************************************** 00080000
000900 DATA DIVISION.                                                   00090000
001000 WORKING-STORAGE SECTION.                                         00100000
001100                                                                  00110000
001200***************************************************************** 00120000
001300* CONSTANTS                                                       00130000
001400***************************************************************** 00140000
001500 01  WS-RATE-TIER1            PIC V99      VALUE .12.             00150000
001600 01  WS-RATE-TIER2            PIC V99      VALUE .15.             00160000
001700 01  WS-RATE-TIER3            PIC V99      VALUE .18.             00170000
001800 01  WS-TIER1-LIMIT           PIC 9(4)     VALUE 500.             00180000
001900 01  WS-TIER2-LIMIT           PIC 9(4)     VALUE 500.             00190000
002000                                                                  00200000
002100***************************************************************** 00210000
002200* 3 PREDEFINED CUSTOMERS (NO TABLES)                              00220000
002300***************************************************************** 00230000
002400 01  WS-CUST1.                                                    00240000
002500     05  WS-C1-NAME           PIC X(12)   VALUE 'CUST-ALPHA  '.   00250000
002600     05  WS-C1-KWH            PIC 9(5)    VALUE 350.              00260000
002700     05  WS-C1-FEE            PIC 9(3)V99 VALUE 14.95.            00270000
002800                                                                  00280000
002900 02  WS-CUST2.                                                    00290001
003000     05  WS-C2-NAME           PIC X(12)   VALUE 'CUST-BRAVO  '.   00300001
003100     05  WS-C2-KWH            PIC 9(5)    VALUE 925.              00310001
003200     05  WS-C2-FEE            PIC 9(3)V99 VALUE 14.95.            00320001
003300                                                                  00330001
003400 02  WS-CUST3.                                                    00340001
003500     05  WS-C3-NAME           PIC X(12)   VALUE 'CUST-CHARLIE'.   00350001
003600     05  WS-C3-KHW            PIC 9(5)    VALUE 1350.             00360001
003700     05  WS-C3-FEE            PIC 9(3)V99 VALUE 14.95.            00370001
003800***************************************************************** 00380000
003900* CURRENT "INPUT" FIELDS (LOADED PER CUSTOMER)                    00390000
004000***************************************************************** 00400000
004100 01  WS-CUST-NAME             PIC X(12)   VALUE SPACES.           00410000
004200 01  WS-KWH-USED              PIC 9(5)    VALUE 0.                00420000
004300 01  WS-SERVICE-FEE           PIC 9(3)V99 VALUE 0.                00430000
004400                                                                  00440000
004500***************************************************************** 00450000
004600* WORK AREAS                                                      00460000
004700***************************************************************** 00470000
004800 01  WS-TIER1-KWH             PIC 9(5)     VALUE 0.               00480000
004900 01  WS-TIER2-KWH             PIC 9(5)     VALUE 0.               00490000
005000 01  WS-TIER3-KWH             PIC 9(5)     VALUE 0.               00500000
005100                                                                  00510000
005200 01  WS-TIER1-CHARGE          PIC 9(5)V99  VALUE 0.               00520000
005300 01  WS-TIER2-CHARGE          PIC 9(5)V99  VALUE 0.               00530000
005400 01  WS-TIER3-CHARGE          PIC 9(5)V99  VALUE 0.               00540000
005500                                                                  00550000
005600 01  WS-SUBTOTAL              PIC 9(6)V99  VALUE 0.               00560000
005700 01  WS-TOTAL-BILL            PIC 9(6)V99  VALUE 0.               00570000
005800                                                                  00580000
005900***************************************************************** 00590000
006000* EDITED FIELDS FOR DISPLAY                                       00600000
006100***************************************************************** 00610000
006200 01  WS-KWH-USED-ED           PIC Z,ZZZ,ZZZ,ZZ9.                  00620000
006300 01  WS-MONEY-ED              PIC $$,$$$,$$9.99.                  00630000
006400 01  WS-MONEY-ED2             PIC $$,$$$,$$9.99.                  00640000
006500                                                                  00650000
006600***************************************************************** 00660000
006700* IT'S GO TIME!                                                   00670000
006800***************************************************************** 00680000
006900 PROCEDURE DIVISION.                                              00690000
007000                                                                  00700000
007100***************************************************************** 00710000
007200* MAINLINE - DISPLAY HEADING, LOAD CUSTOMER, RUN BILL, STOP       00720000
007300***************************************************************** 00730000
007400 000-MAIN.                                                        00740000
007500     DISPLAY '************************************'.              00750001
007600     DISPLAY '*** UTIL2000 - ALL CUSTOMER BILL ***'.              00760001
007700     DISPLAY '************************************'.              00770001
007800     DISPLAY ' '.                                                 00780000
007900                                                                  00790000
008000*    ALPHA                                                        00800001
008100     PERFORM 510-LOAD-CUST-ALPHA.                                 00810001
008200     PERFORM 600-RUN-BILL.                                        00820001
008300                                                                  00830001
008400*    BRAVO                                                        00840001
008410     PERFORM 520-LOAD-CUST-BRAVO.                                 00841001
008420     PERFORM 600-RUN-BILL.                                        00842001
008421                                                                  00842101
008430*    CHARLIE                                                      00843001
008440     PERFORM 530-LOAD-CUST-CHARLIE.                               00844001
008450     PERFORM 600-RUN-BILL.                                        00845001
008460                                                                  00846001
008500     STOP RUN.                                                    00850000
008600                                                                  00860000
008700***************************************************************** 00870000
008800* MOVE name/kwh/fee from CUST into current fields.                00880000
008900***************************************************************** 00890000
009000 510-LOAD-CUST-ALPHA.                                             00900001
009100     MOVE WS-C1-NAME TO WS-CUST-NAME.                             00910000
009200     MOVE WS-C1-KWH  TO WS-KWH-USED.                              00920000
009300     MOVE WS-C1-FEE  TO WS-SERVICE-FEE.                           00930000
009400                                                                  00940000
009410 520-LOAD-CUST-BRAVO.                                             00941001
009420     MOVE WS-C2-NAME TO WS-CUST-NAME.                             00942001
009430     MOVE WS-C2-KWH  TO WS-KWH-USED.                              00943001
009440     MOVE WS-C2-FEE  TO WS-SERVICE-FEE.                           00944001
009441                                                                  00944101
009450 530-LOAD-CUST-CHARLIE.                                           00945001
009460     MOVE WS-C3-NAME TO WS-CUST-NAME.                             00946001
009470     MOVE WS-C3-KHW  TO WS-KWH-USED.                              00947001
009480     MOVE WS-C3-FEE  TO WS-SERVICE-FEE.                           00948001
009490                                                                  00949001
009500***************************************************************** 00950000
009600* BILL ROUTINE                                                    00960000
009700***************************************************************** 00970000
009800 600-RUN-BILL.                                                    00980000
009900     PERFORM 100-INITIALIZE.                                      00990000
010000     PERFORM 200-CALC-TIERS.                                      01000000
010100     PERFORM 300-CALC-CHARGES.                                    01010000
010200     PERFORM 400-DISPLAY-RESULTS.                                 01020000
010300     DISPLAY ' '.                                                 01030000
010400                                                                  01040000
010500***************************************************************** 01050000
010600* Zero tier kWh, charges, subtotal, total                         01060000
010700***************************************************************** 01070000
010800 100-INITIALIZE.                                                  01080000
010900     MOVE 0 TO WS-TIER1-KWH                                       01090000
011000              WS-TIER2-KWH                                        01100000
011100              WS-TIER3-KWH                                        01110000
011200              WS-TIER1-CHARGE                                     01120000
011300              WS-TIER2-CHARGE                                     01130000
011400              WS-TIER3-CHARGE                                     01140000
011500              WS-SUBTOTAL                                         01150000
011600              WS-TOTAL-BILL.                                      01160000
011700                                                                  01170000
011800***************************************************************** 01180000
011900* Determine WS-TIER1-KWH, WS-TIER2-KWH, WS-TIER3-KWH              01190000
012000* based on WS-KWH-USED                                            01200000
012100*                                                                 01210000
012200* These are the per-kWh rates:                                    01220000
012300* - Tier 1: first 500 kWh at $0.12/kWh                            01230000
012400* - Tier 2: next 500 kWh (kWh 401 1000) at $0.15/kWh              01240000
012500* - Tier 3: any kWh above 1000 at $0.18/kWh                       01250000
012600***************************************************************** 01260000
012700 200-CALC-TIERS.                                                  01270000
012800     *> If amount used is less than 500 kWh, all goes in tier 1   01280000
012900     IF WS-KWH-USED <= WS-TIER1-LIMIT                             01290000
013000         MOVE WS-KWH-USED TO WS-TIER1-KWH                         01300000
013100         MOVE 0 TO WS-TIER2-KWH WS-TIER3-KWH                      01310000
013200     ELSE                                                         01320000
013300         MOVE WS-TIER1-LIMIT TO WS-TIER1-KWH                      01330000
013400                                                                  01340000
013500         *> If amount used is between 501 and 1000 kWh,           01350000
013600         *> tier 1 is full, remainder goes in tier 2              01360000
013700         IF WS-KWH-USED <= (WS-TIER1-LIMIT + WS-TIER2-LIMIT)      01370000
013800             COMPUTE WS-TIER2-KWH =                               01380000
013900                 WS-KWH-USED - WS-TIER1-LIMIT                     01390000
014000             MOVE 0 TO WS-TIER3-KWH                               01400000
014100                                                                  01410000
014200         *> If amount used is between 1001 and above,             01420000
014300         *> tier 1 and tier 2 are full, remainder goes in tier 3  01430000
014400         ELSE                                                     01440000
014500             MOVE WS-TIER2-LIMIT TO WS-TIER2-KWH                  01450000
014600             COMPUTE WS-TIER3-KWH =                               01460000
014700                 WS-KWH-USED - WS-TIER1-LIMIT - WS-TIER2-LIMIT    01470000
014800         END-IF                                                   01480000
014900     END-IF.                                                      01490000
015000                                                                  01500000
015100***************************************************************** 01510000
015200* COMPUTE charges using ROUNDED and compute totals.               01520000
015300***************************************************************** 01530000
015400 300-CALC-CHARGES.                                                01540000
015500     COMPUTE WS-TIER1-CHARGE ROUNDED =                            01550000
015600         WS-TIER1-KWH * WS-RATE-TIER1.                            01560000
015700                                                                  01570000
015800     COMPUTE WS-TIER2-CHARGE ROUNDED =                            01580001
015900         WS-TIER2-KWH * WS-RATE-TIER2.                            01590001
016000                                                                  01600001
016100     COMPUTE WS-TIER3-CHARGE ROUNDED =                            01610001
016200         WS-TIER3-KWH * WS-RATE-TIER3.                            01620001
016300                                                                  01630001
016400     COMPUTE WS-SUBTOTAL = WS-TIER1-CHARGE +                      01640001
016500           WS-TIER2-CHARGE + WS-TIER3-CHARGE                      01650001
016600                                                                  01660001
016700     COMPUTE WS-TOTAL-BILL =                                      01670000
016800         WS-SUBTOTAL + WS-SERVICE-FEE.                            01680001
016900                                                                  01690001
017000                                                                  01700001
017100                                                                  01710000
017200***************************************************************** 01720000
017300* Display report including customer name.                         01730000
017400***************************************************************** 01740000
017500 400-DISPLAY-RESULTS.                                             01750000
017600     MOVE WS-KWH-USED TO WS-KWH-USED-ED.                          01760000
017700                                                                  01770000
017800     DISPLAY '--------------------------------'.                  01780000
017900     DISPLAY 'CUSTOMER: ' WS-CUST-NAME.                           01790000
018000     DISPLAY '--------------------------------'.                  01800000
018100     DISPLAY 'KWH USED       : ' WS-KWH-USED-ED.                  01810000
018200                                                                  01820000
018300     MOVE WS-SERVICE-FEE TO WS-MONEY-ED.                          01830000
018400     DISPLAY 'SERVICE FEE    : ' WS-MONEY-ED.                     01840000
018500                                                                  01850000
018600     MOVE WS-TIER1-CHARGE TO WS-MONEY-ED.                         01860000
018700     DISPLAY 'TIER 1 CHARGE  : ' WS-MONEY-ED.                     01870000
018800                                                                  01880000
018900     MOVE WS-TIER2-CHARGE TO WS-MONEY-ED.                         01890000
019000     DISPLAY 'TIER 2 CHARGE  : ' WS-MONEY-ED.                     01900000
019100                                                                  01910000
019200     MOVE WS-TIER3-CHARGE TO WS-MONEY-ED.                         01920000
019300     DISPLAY 'TIER 3 CHARGE  : ' WS-MONEY-ED.                     01930000
019400                                                                  01940000
019500     MOVE WS-TOTAL-BILL TO WS-MONEY-ED2.                          01950000
019600     DISPLAY '--------------------------------'.                  01960000
019700     DISPLAY 'TOTAL BILL     : ' WS-MONEY-ED2.                    01970000
019800     DISPLAY '--------------------------------'.                  01980000
