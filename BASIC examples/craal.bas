10 REM By Mike Bibby
20 REM (c) Computing With The Amstrad
30 REM Ported to LM80C BASIC by Leonardo Miliani (2020)
40 SCREEN 0
50 COLOR 14,1
70 GOTO 220
80 DIM D(10,4):CLEAR 3500
90 H$="":C$="":O$=""
100 OO$="":HH$=""
110 FOR I=1 TO 10:FOR J=1 TO 4
120 READ D(I,J)
130 NEXT :NEXT
140 N=12:T=7:M=51
150 DIM J$(N):DIM O(N):DIM M$(M)
160 FOR I=1 TO N:READ HH$:READ CC$:GOSUB 560:J$(I)=OO$
165 HH$=CC$:GOSUB 560:O(I)=VAL(OO$):NEXT I
170 FOR I=1 TO M:READ PL
172 FOR X=1 TO PL
174 READ TP$:M$(I)=M$(I)+TP$
176 NEXT X:NEXT I
180 H$="":C$="":O$=""
190 B=-1:D=-1:E=-1:S=0:L=0:A=-1:G=0:FF=-1
200 R=2:X=3
210 GOTO 270
220 CLS:PRINT:PRINT "On a visit to the palace of Craal, you"
222 PRINT "find the place in uproar. The king is"
224 PRINT "dead and his crown stolen by a wicked"
226 PRINT "wizard who's fled to his den in the":PRINT"palace dungeons."
230 PRINT "By paying rather too much attention to"
232 PRINT "the ex-king's daughter, you find your-"
234 PRINT "self volunteered to recover it."
240 PRINT "You are thrown into the dungeons and"
242 PRINT "told not to come back without the crown."
250 PRINT "Here begins the adventure...."
260 PRINT:PRINT "(please, wait a while..)":GOTO 80
270 REM WHILE NOT g
280 IF R<>X THEN GOSUB 640
290 X=R
300 C=0
305 GOSUB 410:IF C=0 THEN 305
310 ON C GOSUB 840,880,910,960,1120,640,1030,990,1060,1210
320 IF NOT G THEN 280
330 PRINT
340 IF O(8)=1 THEN 360
345 M=31:GOSUB 2200
350 M=32:GOSUB 2200:PRINT:GOTO 380
360 M=29:GOSUB 2200:PRINT
370 M=30:GOSUB 2200:END
380 PRINT:M=51:GOSUB 2200
390 A=INKEY(0)
395 A=INKEY(50) AND 223:IF A=0 THEN 395
400 IF A=ASC("N") THEN END
405 RUN
410 PRINT:PRINT"What now?"
420 C$=""
425 INPUT "===>";C$:IF C$="" THEN 425
428 A$=C$:GOSUB 2300:C$=A$
430 IF LEN(C$)<>1 THEN 450
440 C=INSTR("nsewil",C$):IF C<>0 THEN RETURN
444 PRINT "I don't recognise this single letter"
446 PRINT "command - only n,s,e,w,i,l.":RETURN
450 SX=INSTR(C$," "):IF SX<>0 THEN 460
453 PRINT "I don't understand - put a space between";
456 PRINT "command and object, please.":C=0:RETURN
460 VE$=LEFT$(C$,SX-1):O$=" "+MID$(C$,SX+1)
463 O$=RIGHT$(O$,LEN(O$)-1)
466 IF LEFT$(O$,1)=" " THEN 463
470 C=INSTR("droptakesayhit",VE$)
480 IF C<>1 AND C<>5 AND C<>9 AND C<>12 THEN 486
483 GOTO 490
486 PRINT "I don't understand your command.":C=0:RETURN
490 IF C=1 THEN C=7:GOTO 500
492 IF C=5 THEN C=8:GOTO 500
494 IF C=12 THEN C=10:GOTO 500
496 IF C=9 THEN RETURN
500 Z=0:I=1:M=0
510 IF LEFT$(O$,4)=LEFT$(J$(I),4) THEN M=1
520 I=I+1
530 IF M<>1 AND I<>N+1 THEN 510
540 IF M=1 THEN Z=I-1:RETURN
550 PRINT "I don't understand the object you mean.":C=0:RETURN
560 OO$=""
570 FOR JJ=1 TO LEN(HH$)
580 RR=ASC(MID$(HH$,JJ,1))-3
590 IF RR=38 OR RR=41 OR RR=34 THEN RR=RR+3
600 OO$=OO$+CHR$(RR)
610 NEXT
620 RETURN
630 END
640 IN=-1:M=R-1:GOSUB 2140
650 PRINT:PRINT:PRINT "You can see around you :-"
660 F=0
670 FOR I=1 TO N
680 IF O(I)<>R THEN GOTO 700
685 F=-1
690 GOSUB 1430
700 NEXT I
710 IF NOT F THEN PRINT "Nothing of interest."
720 PRINT
730 RETURN
740 DATA 0,0,0,0
750 DATA 3,9,0,0
760 DATA 0,2,4,0
770 DATA 0,0,5,3
780 DATA 0,0,0,4
790 DATA 0,7,0,0
800 DATA 6,0,0,8
810 DATA 0,0,7,9
820 DATA 2,0,8,0
830 DATA 0,0,0,0
840 IF D(R,1)=0 THEN PRINT" Not allowed":PRINT:RETURN
850 IF R=7 AND A THEN M=33:GOSUB 2200:PRINT:RETURN
860 R=D(R,1)
870 RETURN
880 IF D(R,2)=0 THEN PRINT" Not allowed":PRINT:RETURN
890 R=D(R,2)
900 RETURN
910 IF D(R,3)=0 THEN PRINT" Not allowed":PRINT:RETURN
920 IF D(R,3)=8 AND FF THEN M=34:GOSUB 2200:FF=0
930 IF D(R,3)=7 AND O(7)=0 THEN O(7)=8:M=35:GOSUB 2200:PRINT
940 R=D(R,3)
950 RETURN
960 IF D(R,4)=0 THEN PRINT " Not allowed":PRINT:RETURN
970 R=D(R,4)
980 RETURN
990 IF O(Z)=1 THEN PRINT "You already have it":RETURN
1000 IF O(Z)<>R THEN PRINT "It's not here":RETURN
1010 IF Z<T THEN PRINT "You can't take that":GOTO 1020
1015 O(Z)=1
1020 RETURN
1030 IF O(Z)=1 THEN O(Z)=R:GOTO 1040
1035 PRINT "You don't have it":RETURN
1040 ON (R-1) GOSUB 1280,1300,1290,1340,1290,1290,1360,1290,1290
1050 RETURN
1060 H$="":FOR I=LEN(O$) TO 1 STEP -1
1070 H$=H$+MID$(O$,I,1)
1080 NEXT I
1090 IF O$=H$ THEN G=-1:M=50:GOSUB 2200:RETURN
1100 PRINT"Okay, "+CHR$(34)+O$+CHR$(34)
1110 RETURN
1120 PRINT"Your inventory contains:-"
1130 F=0
1140 FOR I=T TO N
1150 IF O(I)<> 1 THEN GOTO 1170
1155 F=-1
1160 GOSUB 1430
1170 NEXT I
1180 IF NOT F THEN PRINT"Nothing at all."
1190 PRINT
1200 RETURN
1210 IF O(Z)<>R THEN PRINT"It wasn't here to hit":RETURN
1220 IF O(11)<>1 THEN M=36:GOSUB 2200:RETURN 
1230 IF Z<>6 AND Z<>3 THEN PRINT "This has no effect whatsoever."
1240 IF Z=6 AND A THEN A=0:M=37:GOSUB 2200:PRINT:O(6)=0:RETURN
1250 IF Z=3 AND S=0 THEN M=38:GOSUB 2200:S=-1:O(8)=2:RETURN
1260 IF Z=3 AND S=-1 THEN M=39:GOSUB 2200:RETURN
1270 RETURN
1280 IF Z=8 AND E THEN M=40:GOSUB 2200:O(Z)=0:E=NOT E:O(11)=1
1290 RETURN
1300 IF Z=7 AND NOT B AND L THEN M=41:GOSUB 2200:PRINT:L=0
1310 IF Z=9 THEN M=42:GOSUB 2200:O(Z)=0:O(12)=3
1320 IF Z=10 THEN D=0
1330 RETURN
1340 IF O(7)=5 AND O(12)=5 THEN M=43:GOSUB 2200:B=0:O(12)=0:O(7)=1
1350 RETURN
1360 IF Z<>10 THEN GOTO 1400
1370 IF D THEN M=44:GOSUB 2200:GOTO 380
1380 IF (NOT L) OR (O(7)<>8 AND O(7)<>1) THEN M=45:GOSUB 2200:GOTO 380
1390 IF O(2)=0 THEN M=46:GOSUB 2200:O(1)=0:O(2)=8:O(8)=8
1400 IF Z=7 AND NOT B THEN L=-1:M=47:GOSUB 2200:PRINT
1410 IF Z=9 OR Z=12 THEN M=48:GOSUB 2200:PRINT:M=49:GOSUB 2200:GOTO 380
1420 RETURN
1430 IF I=1 THEN M=10:GOSUB 2140
1440 IF I=2 THEN M=11:GOSUB 2140
1450 IF I=3 AND NOT S THEN M=12:GOSUB 2140
1460 IF I=3 AND S THEN M=14:GOSUB 2140
1470 IF I=4 THEN M=15:GOSUB 2140:M=16:GOSUB 2200:M=17:GOSUB 2200
1480 IF I=5 THEN M=18:GOSUB 2140
1490 IF I=6 THEN M=19:GOSUB 2140
1500 IF I=7 AND NOT B AND NOT L THEN M=21:GOSUB 2140
1510 IF I=7 AND NOT B AND L THEN M=22:GOSUB 2140
1520 IF I=7 AND B THEN M=23:GOSUB 2140
1530 IF I=8 THEN M=20:GOSUB 2140
1540 IF I=9 THEN M=24:GOSUB 2140
1550 IF I=10 AND D THEN M=25:GOSUB 2140
1560 IF I=10 AND NOT D THEN M=28:GOSUB 2140
1570 IF I=11 AND NOT A THEN M=26:GOSUB 2140
1580 IF I=11 AND A THEN CR$="A "+J$(I):PRINT CR$
1590 IF I=12 THEN M=27:GOSUB 2140
1600 PRINT
1610 RETURN
1620 DATA iluh,;,sloh#ri#dvkhv,3,yhqglqj#pdfklqh,5,pluuru,9,wuroo,8,gzdui,":"
1625 DATA odps,3,frlq,3,sdufkphqw,7,fxuwdlq,<,vzrug,3,yrxfkhu,3
1630 DATA 2,"d#udwkhu#vsduvh,#xqiulhqgo|##urrp#zlwk#wkh#dssduhqwo|#lqh{solfdeoh###"
1635 DATA "vljq#*Duprxu|*#rq#wkh#zdoo1"
1640 DATA 2,"d#ydvw#fdyhuq#zlwk#d#odujh###srro#ri#zdwhu1#D#sdwk#ohdgv#dorqj#"
1645 DATA "lwv###hgjh1"
1650 DATA 1,"dq#h{wuhpho|#wljkw,#Hdvw#0###Zhvw#wxqqho1"
1660 DATA 1,"d#vkrs1#Ryhu#wkh#frxqwhu#lv#dvljq=#Hehqhh}hu*v#Hqwhusulvhv1"
1670 DATA 1,"d#vpdoo#fkdpehu,#lwv#zdoov###lulghvfhqw#zlwk#vsdunolqj#fu|vwdov,vdyh#rqh1"
1680 DATA 1,"d#udwkhu#jorrp|#fdyh,########uhplqlvfhqw#ri#rog#plqh#zrunlqjv1"
1690 DATA 1,"zkdw#orrnv#olnh#d############khuplw*v#fhoo1"
1700 DATA 1,"dq#rog#gluw|#fkdpehu#wkdw####orrnv#wr#kdyh#ehhq#sloodjhg#orqj#djr1"
1710 DATA 2,"#wkh#zl}dug*v#fkdpehu1#Dv#qr#rqh#kdv#hyhu#vxuylyhg#wklv#h{shulhqfh,##"
1715 DATA "wkhuh#lv#qr#h{wdqw#ghvfulswlrq1111"
1720 DATA 1,"exuqlqj#eulooldqwo|#lq#d#fruqhu1"
1730 DATA 1,"qrz#txlwh#frro1"
1740 DATA 1,"zlwk#d#qrwlfh#*qr#krw#prqh|#ru#frxqwhuihlw#frlqv*1"
1750 DATA 1,"zlwk#d#qrwlfh#*hpsw|*#rq#lw1"
1760 DATA 1,"lq#elwv#rq#wkh#iorru1"
1770 DATA 1,"hqjudyhg="
1780 DATA 1,"*Vd|#pluuru#zrug#wr#zl}dug#phhw,"
1790 DATA 1,"Eh#vxuh#|rx#kdyh#wkh#jrog#wr#juhhw1*"
1800 DATA 1,"zlwk#d#exvlqhvv#olnh#dlu1"
1810 DATA 1,"zhdulqj#gdun#jodvvhv1"
1820 DATA 1,"zruwk#lwv#zhljkw#lq#jrog1"
1830 DATA 1,"qrw#|hw#olw1"
1840 DATA 1,"exuqlqj#euljkwo|1"
1850 DATA 1,"lq#d#udwkhu#edwwhuhg#frqglwlrq1"
1860 DATA 1,"gu|#dv#d#erqh#dqg#frpsohwho|eodqn1"
1870 DATA 1,"lq#txlwh#jrrg#frqglwlrq#0#dqg#qrw#dw#doo#gdps1"
1880 DATA 1,"fryhuhg#lq#eorrg1"
1890 DATA 2,"uhdglqj=#H{fkdqjh#wklv#yrxfkhuwrjhwkhu#zlwk#dq#rog#odps#dqg#|rx*oo#jhwd#"
1895 DATA "qhz#odps#iuhh$"
1900 DATA 1,",#zulqjlqj#zhw1"
1910 DATA 2,"Wkh#zl}dug*v#h|hv#oljkw#xs1#Kh#wdnhv####|rxu#frlq#wr#ex|#klpvhoi#d#"
1915 DATA "gulqn#dqg####glvdsshduv1"
1920 DATA 2,"\rx#sxw#rq#wkh#furzq#kh*v#ohiw#ehklqg###dqg#ilqg#|rxuvhoi#edfn#lq#"
1925 DATA "wkh#sdodfh,###Nlqj#ri#Fuddo,#dqg#kdss|#hyhu#diwhu1"
1930 DATA 2,"Rk#ghdu,#|rx*yh#jrw#qr#prqh|#0#dqg#wkh##zl}dug#zdv##krslqj##|rx*g#"
1935 DATA "ex|#klp#d#####slqw#ru#wzr1"
1940 DATA 1,"Qdwxudoo|#kh#nloov#|rx1"
1950 DATA 1,"Wkh#gzdui#uhixvhv#wr#ohw#|rx#sdvw1"
1960 DATA 2,"D#zrug#wr#wkh#zlvh=#\rx*uh#jrlqj#wr#wkh#rqo|#urrp#lq#wkh#sodfh#"
1965 DATA "zlwkrxw#lwv#rzq##qdwxudo#skrvskruhvfhqfh1"
1970 DATA 2,"Dq#ludwh#gzdui,#lqixuldwhg#e|#wkh#oljkw#|rx*yh#ohw#lq,#wkurzv#klv#"
1975 DATA "xqolw#odps#dw#|rx1#Lw#odqgv,#lq#wkh#urrp#|rx*yh#mxvw##ohiw,#vkdwwhuhg1"
1980 DATA 1,"\rx#kdyhq*w#jrw#dq|wklqj#ghfhqw#wr#klw##zlwk1#Jhw#d#vzrug1"
1990 DATA 1,"Wkh#gzdui#idoov#ghdg,#wkhq#glvdsshduv#lqiurqw#ri#|rxu#h|hv1"
2000 DATA 1,"Wkh#yhqglqj#pdfklqh#vkdwwhuv1#D#frlq####gursv#rxw1"
2010 DATA 1,"Judwxlwrxv#ylrohqfh#grhvq*w#khos#dq|rqh1"
2020 DATA 2,"Wkh#frlq#idoov#lqwr#wkh#vorw#lq#wkh#####yhqglqj#pdfklqh1#\rx#duh#"
2025 DATA "jlyhq#d#vzrug1#Wkdw*v#zk|#lw*v#fdoohg#wkh#Duprxu|$"
2030 DATA 1,"Xqiruwxqdwho|,#|rxu#odps#jrhv#rxw1"
2040 DATA 1,"Lqwhuhvwlqj$#Vrphwklqj*v#kdsshqlqj#wr###wkh#sdufkphqw1#Wdnh#d#orrn1111"
2050 DATA 1,"\rx*yh#jrw#d#qhz#odps$"
2060 DATA 1,"Wkh#fxuwdlq#ioduhv#xs1#\rx#glh#lq#wkh###eod}h1"
2070 DATA 2,"Wdnlqj#dgydqwdjh#ri#wkh#vxgghq#soxqjh###lqwr#gdunqhvv,#wkh#gzdui#iurp#"
2075 DATA "qh{w#grru#frphv#lq#dqg#srolvkhv#|rx#rii1"
2080 DATA 1,"Wkh#iluh#jrhv#rxw1#\rx#vhh#d#frlq#lq#lwvdvkhv1"
2090 DATA 1,"\rxu#odps#oljkwv1"
2100 DATA 1,"Lw#exuvwv#lqwr#iodph$"
2110 DATA 2,"\rx#vxgghqo|#uhdolvh#wkdw#lw#zdv#ylwdo##wr#wkh#jdph1#Ryhufrph#zlwk#"
2115 DATA "julhi,#|rx###nloo#|rxuvhoi1"
2120 DATA 2,"Wkh#pluuru#glvvroyhv#dqg#|rx#ilqg#######|rxuvhoi#sxoohg#wkurxjk#wkh#jds#"
2125 DATA "lw*v####ohiw#lqwr#wkh#zl}dug*v#urrp1"
2130 DATA 2,"Zkdw#d#vkdph#0#dqg#|rx#zhuh#grlqj#vr####zhoo#wrr1#Gr#|rx#zdqw#dqrwkhu#"
2135 DATA "jrB#+\2Q)"
2140 HH$=M$(M):GOSUB 560
2160 CR$="You are in "+OO$:IF IN THEN PRINT:PRINT CR$;
2170 IF NOT IN THEN CR$="A "+J$(I)+" "+OO$:PRINT CR$
2180 IN=0
2190 RETURN
2200 HH$=M$(M):GOSUB 560:PRINT:PRINT OO$
2210 RETURN
2300 IF A$="" THEN RETURN
2310 TP$="":FOR X=1 TO LEN(A$)
2320 AS=ASC(MID$(A$,X,1))
2330 IF AS>=97 AND AS<=122 THEN AS=AS-32
2340 TP$=TP$+CHR$(AS)
2350 NEXT:RETURN
