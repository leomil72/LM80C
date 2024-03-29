10 REM SUPER STARTREK - MAY 16,1978 - REQUIRES 24K MEMORY
30 REM
40 REM ****                 **** STAR TREK ****              ****
50 REM **** SIMULATION OF A MISSION OF THE STARSHIP ENTERPRISE,
60 REM **** AS SEEN ON THE STAR TREK TV SHOW.
70 REM **** ORIGINAL PROGRAM BY MIKE MAYFIELD, MODIFIED VERSION
80 REM **** PUBLISHED IN DEC'S "101 BASIC GAMES", BY DAVE AHL.
90 REM **** MODIFICATIONS TO THE LATTER (PLUS DEBUGGING) BY BOB
100 REM *** LEEDOM - APRIL & DECEMBER 1974,
110 REM *** WITH A LITTLE HELP FROM HIS FRIENDS . . .
120 REM *** COMMENTS, EPITHETS, AND SUGGESTIONS SOLICITED --
130 REM *** SEND TO:  R. C. LEEDOM
140 REM ***           WESTINGHOUSE DEFENSE & ELECTRONICS SYSTEMS CNTR.
150 REM ***           BOX 746, M.S. 338
160 REM ***           BALTIMORE, MD  21203
170 REM ***
180 REM *** CONVERTED TO MICROSOFT 8 K BASIC 3/16/78 BY JOHN GORDERS
190 REM *** LINE NUMBERS FROM VERSION STREK7 OF 1/12/75 PRESERVED AS
200 REM *** MUCH AS POSSIBLE WHILE USING MULTIPLE STATEMENTS PER LINE
205 REM *** SOME LINES ARE LONGER THAN 72 CHARACTERS; THIS WAS DONE
210 REM *** BY USING "?" INSTEAD OF "PRINT" WHEN ENTERING LINES
211 REM ***
212 REM *** PORTED TO LM80C BASIC ON 2019/09/15
213 REM *** BY LEONARDO MILIANI. REVISED ON 2023/12/12
215 REM ***
220 SCREEN 0:COLOR 2,1:GOSUB 20000
226 PRINT:PRINT:PRINT "     THE USS ENTERPRISE -- NCC-1701"
227 GOSUB 13000:LOCATE 0,20:GOSUB 10000
230 CLS:PRINT "Generating galaxy map"
240 PRINT "Please wait ..............";:LOCATE 12,1
260 CLEAR 600:PRINT " ";:REM MAKE ROOM FOR STRINGS BUFFER
270 Z$="                         "
280 A=RND(-ABS(TMR(0))):REM RANDOMIZE RNG
330 DIM G(8,8),C(9,2),K(3,3),N(3),Z(8,8),D(8)
335 PRINT " ";
370 T=INT(RND(1)*20+20)*100:T0=T:T9=25+INT(RND(1)*10):D0=0:E=3000:E0=E
440 P=10:P0=P:S9=200:S=0:B9=2:K9=0:X$="":X0$=" is "
445 PRINT " ";
470 DEF FND(D)=SQR((K(I,1)-S1)^2+(K(I,2)-S2)^2)
475 DEF FNR(R)=INT(RND(R)*7.98+1.01)
480 REM INITIALIZE ENTERPRISE'S POSITION
490 Q1=FNR(1):Q2=FNR(1):S1=FNR(1):S2=FNR(1)
495 PRINT " ";
530 FOR I=1 TO 9:C(I,1)=0:C(I,2)=0:NEXT I
540 C(3,1)=-1:C(2,1)=-1:C(4,1)=-1:C(4,2)=-1:C(5,2)=-1:C(6,2)=-1
600 C(1,2)=1:C(2,2)=1:C(6,1)=1:C(7,1)=1:C(8,1)=1:C(8,2)=1:C(9,2)=1
605 PRINT " ";
670 FOR I=1 TO 8:D(I)=0:NEXT I
710 A1$="NAVSRSLRSPHATORSHEDAMCOMXXX"
715 PRINT " ";
810 REM SETUP WHAT EXHISTS IN GALAXY . . .
815 REM K3= # KLINGONS  B3= # STARBASES  S3 = # STARS
820 FOR I=1 TO 8:FOR J=1 TO 8:K3=0:Z(I,J)=0:R1=RND(1)
850 IF R1>.98 THEN K3=3:K9=K9+3:GOTO 980
860 IF R1>.95 THEN K3=2:K9=K9+2:GOTO 980
870 IF R1>.80 THEN K3=1:K9=K9+1
980 B3=0:IF RND(1)>.96 THEN B3=1:B9=B9+1
1040 G(I,J)=K3*100+B3*10+FNR(1):NEXT J:PRINT " ";
1050 NEXT I:IF K9>T9 THEN T9=K9+1
1100 IF B9<>0 THEN 1200
1150 IF G(Q1,Q2)<200 THEN G(Q1,Q2)=G(Q1,Q2)+120:K9=K9+1
1160 B9=1:G(Q1,Q2)=G(Q1,Q2)+10:Q1=FNR(1):Q2=FNR(1)
1200 K7=K9:IF B9<>1 THEN X$="s":X0$=" are "
1230 CLS:PRINT "Your orders are as follows:":PRINT
1240 PRINT "destroy the";K9;"KLINGON warships which"
1245 PRINT "have invaded the galaxy before they can"
1250 PRINT "attack Federation Headquarters on"
1255 PRINT "stardate";T0+T9;"."
1260 PRINT "This gives you";T9;"days.":PRINT
1265 PRINT "There";X0$;B9;"Starbase";X$;" in the galaxy"
1270 PRINT "for resupplying your ship."
1280 PRINT
1290 GOSUB 12000
1300 I=RND(1)
1310 REM HERE ANY TIME NEW QUADRANT ENTERED
1320 Z4=Q1:Z5=Q2:K3=0:B3=0:S3=0:G5=0:D4=.5*RND(1):Z(Q1,Q2)=G(Q1,Q2)
1390 IF Q1<1 OR Q1>8 OR Q2<1 OR Q2>8 THEN 1600
1430 GOSUB 9030:PRINT:IF T0<>T THEN 1490
1460 PRINT "Your mission begins with your starship"
1470 PRINT "located in the galactic quadrant"
1480 PRINT "'";G2$;"'.":GOTO 1500
1490 PRINT "Now entering ";G2$;" quadrant...":PAUSE 100
1500 K3=INT(G(Q1,Q2)*.01):B3=INT(G(Q1,Q2)*.1)-10*K3
1540 S3=G(Q1,Q2)-100*K3-10*B3:IF K3=0 THEN 1590
1550 CLS:LOCATE 5,10
1560 PRINT "COMBAT AREA      CONDITION RED":IF S>200 THEN 1590
1580 PRINT:PRINT SPC(8);"SHIELDS DANGEROUSLY LOW"
1590 FOR I=1 TO 3:K(I,1)=0:K(I,2)=0:NEXT I
1600 FOR I=1 TO 3:K(I,3)=0:NEXT I:Q$=Z$+Z$+Z$+Z$+Z$+Z$+Z$+LEFT$(Z$,17)
1660 REM POSITION ENTERPRISE IN QUADRANT, THEN PLACE "K3" KLINGONS, &
1670 REM "B3" STARBASES, & "S3" STARS ELSEWHERE.
1680 A$="<E>":Z1=S1:Z2=S2:GOSUB 8670:IF K3<1THEN1820
1720 FOR I=1 TO K3:GOSUB 8590:A$="-K-":Z1=R1:Z2=R2
1780 GOSUB 8670:K(I,1)=R1:K(I,2)=R2:K(I,3)=S9*(0.5+RND(1)):NEXT I
1820 IF B3<1 THEN 1910
1880 GOSUB 8590:A$="(S)":Z1=R1:B4=R1:Z2=R2:B5=R2:GOSUB 8670
1910 FOR I=1 TO S3:GOSUB 8590:A$=" * ":Z1=R1:Z2=R2:GOSUB 8670:NEXT I
1980 GOSUB 6430
1990 IF S+E>10 THEN IF E>10 OR D(7)=0 THEN 2060
2020 CLS:PRINT "** FATAL ERROR **"
2025 PRINT "You've just stranded your ship in space."
2030 PRINT "You have insufficient maneuvering"
2040 PRINT "energy, and shield control is presently"
2045 PRINT "incapable of cross-circuiting to engine"
2050 PRINT "room!!":GOTO 6220
2060 A$="":INPUT "Command";A$:GOSUB 11200
2080 FOR I=1 TO 9:IF LEFT$(A$,3)<>MID$(A1$,3*I-2,3) THEN 2160
2140 ONIGOTO 2300,1980,4000,4260,4700,5530,5690,7290,6270
2160 NEXT I:CLS:PRINT "Enter one of the following:"
2180 PRINT "  NAV  - to set course"
2190 PRINT "  SRS  - for Short Range Sensor scan"
2200 PRINT "  LRS  - for Long Range Sensor scan"
2210 PRINT "  PHA  - to fire phasers"
2220 PRINT "  TOR  - to fire photon torpedoes"
2230 PRINT "  SHE  - to raise or lower shields"
2240 PRINT "  DAM  - for damage control reports"
2250 PRINT "  COM  - to call on Library-Computer"
2260 PRINT "  XXX  - to resign your command"
2270 GOSUB 12000:CLS:GOTO 1980
2290 REM COURSE CONTROL BEGINS HERE
2300 INPUT "Course (0-9)";C1:IF C1=9 THEN C1=1
2310 IF C1>=1 AND C1<9 THEN 2350
2330 CLS:PRINT "Lt. SULU reports:"
2340 PRINT "'Incorrect course data, Sir!'":GOSUB 12000:GOTO 1990
2350 X$="8":IF D(1)<0 THEN X$="0.2"
2360 PRINT "Warp factor (0-";X$;")";:INPUT W1
2370 IF D(1)<0 AND W1>.2 THEN 2470
2380 IF W1>0 AND W1<=8THEN2490
2390 IF W1=0 THEN 1990
2420 CLS:PRINT "Chief engineer SCOTT reports:"
2430 PRINT "'The engine won't take warp ";W1;"!'":GOTO 1990
2470 CLS:PRINT "Warp engines are damaged."
2480 PRINT "Maximum speed = Warp 0.2":GOTO 1990
2490 N=INT(W1*8+.5):IF E-N>=0 THEN 2590
2500 CLS:PRINT "Engineering reports:"
2505 PRINT "'Insufficient energy available for"
2510 PRINT "maneuvering at warp";W1;"!'"
2530 IF S<N-E OR D(7)<0 THEN 1990
2550 PRINT "Deflector control room acknowledges"
2555 PRINT S;"units of energy presently"
2560 PRINT "deployed to shields."
2570 GOTO 1990
2580 REM KLINGONS MOVE/FIRE ON MOVING STARSHIP . . .
2590 FOR I=1 TO K3:IF K(I,3)=0 THEN 2700
2610 A$="   ":Z1=K(I,1):Z2=K(I,2):GOSUB 8670:GOSUB 8590
2660 K(I,1)=Z1:K(I,2)=Z2:A$="-K-":GOSUB 8670
2700 NEXT I:GOSUB 6000:D1=0:D6=W1:IF W1>=1 THEN D6=1
2770 FOR I=1 TO 8:IF D(I)>=0 THEN 2880
2790 D(I)=D(I)+D6:IF D(I)>-.1 AND D(I)<0 THEN D(I)=-.1:GOTO 2880
2800 IF D(I)<0 THEN 2880
2810 IF D1<>1 THEN D1=1:PRINT "Damage control report:  "
2840 R1=I:GOSUB 8790:PRINT G2$;" REPAIR COMPLETED."
2880 NEXT I:IF RND(1)>.2 THEN 3070
2910 R1=FNR(1):IF RND(1)>=.6 THEN 3000
2930 D(R1)=D(R1)-(RND(1)*5+1):PRINT "Damage control report:  "
2960 GOSUB 8790:PRINT G2$;" DAMAGED":PRINT:GOTO 3070
3000 D(R1)=D(R1)+RND(1)*3+1:PRINT "Damage control report:  "
3030 GOSUB 8790:PRINT G2$;":":PRINT "state of repair IMPROVED":PRINT
3040 PAUSE 150
3060 REM BEGIN MOVING STARSHIP
3070 A$="   ":Z1=INT(S1):Z2=INT(S2):GOSUB 8670
3110 X1=C(C1,1)+(C(C1+1,1)-C(C1,1))*(C1-INT(C1)):X=S1:Y=S2
3140 X2=C(C1,2)+(C(C1+1,2)-C(C1,2))*(C1-INT(C1)):Q4=Q1:Q5=Q2
3170 FOR I=1 TO N:S1=S1+X1:S2=S2+X2
3180 IF S1<1 OR S1>=9 OR S2<1 OR S2>=9 THEN 3500
3240 S8=INT(S1)*24+INT(S2)*3-26:IF MID$(Q$,S8,2)="  " THEN 3360
3320 S1=INT(S1-X1):S2=INT(S2-X2)
3330 CLS
3340 PRINT "Warp engines shut down at sector";S1;",";S2
3350 PRINT "due to bad navigation":GOSUB 12000:GOTO 3370
3360 NEXT I:S1=INT(S1):S2=INT(S2)
3370 A$="<*>":Z1=INT(S1):Z2=INT(S2):GOSUB 8670:GOSUB 3910:T8=1
3430 IF W1<1 THEN T8=.1*INT(10*W1)
3450 T=T+T8:IF T>T0+T9 THEN 6220
3470 REM SEE IF DOCKED, THEN GET COMMAND
3480 GOTO 1980
3490 REM EXCEEDED QUADRANT LIMITS
3500 X=8*Q1+X+N*X1:Y=8*Q2+Y+N*X2:Q1=INT(X/8):Q2=INT(Y/8):S1=INT(X-Q1*8)
3550 S2=INT(Y-Q2*8):IF S1=0 THEN Q1=Q1-1:S1=8
3590 IF S2=0 THEN Q2=Q2-1:S2=8
3620 X5=0:IF Q1<1 THEN X5=1:Q1=1:S1=1
3670 IF Q1>8 THEN X5=1:Q1=8:S1=8
3710 IF Q2<1 THEN X5=1:Q2=1:S2=1
3750 IF Q2>8 THEN X5=1:Q2=8:S2=8
3790 IF X5=0 THEN 3860
3800 CLS:PRINT "Lt. UHURA reports message from Star-"
3805 PRINT "Fleet Command:":PRINT
3810 PRINT "Permission to attempt crossing of"
3815 PRINT "galactic perimeter is hereby *DENIED*."
3820 PRINT "Shut down your engines.":PRINT
3830 PRINT "Chief engineer SCOTT reports warp"
3840 PRINT "engines shut down at sector";S1;",";S2
3845 PRINT "of quadrant";Q1;",";Q2;".":GOSUB 12000
3850 IF T>T0+T9 THEN 6220
3860 IF 8*Q1+Q2=8*Q4+Q5 THEN 3370
3870 T=T+1:GOSUB 3910:GOTO 1320
3900 REM MANEUVER ENERGY S/R **
3910 E=E-N-10:IF E>=0 THEN RETURN
3930 PRINT "Shield control supplies energy to"
3935 PRINT "complete the maneuver."
3940 S=S+E:E=0:IF S<=0 THEN S=0
3980 RETURN
3990 REM LONG RANGE SENSOR SCAN CODE
4000 IF D(3)<0 THEN 4010
4005 GOTO 4030
4010 A$="Long Range Sensors are inoperable":GOSUB 14000
4015 GOTO 1990
4030 CLS:PRINT "Long Range Scan for quadrant";Q1;",";Q2
4040 O1$="-------------------":PRINT O1$
4060 FOR I=Q1-1 TO Q1+1:N(1)=-1:N(2)=-2:N(3)=-3:FOR J=Q2-1 TO Q2+1
4120 IF I>0 AND I<9 AND J>0 AND J<9 THEN N(J-Q2+2)=G(I,J):Z(I,J)=G(I,J)
4180 NEXT J:FOR L=1 TO 3:PRINT ": ";:IF N(L)<0 THEN PRINT "*** ";:GOTO 4230
4210 PRINT RIGHT$(STR$(N(L)+1000),3);" ";
4230 NEXT L:PRINT ":":PRINT O1$:NEXT I:GOTO 1990
4250 REM PHASER CONTROL CODE BEGINS HERE
4260 CLS:IF D(4)<0 THEN 4262
4261 GOTO 4265
4262 A$="Phasers inoperative":GOSUB 14000:GOTO 1990
4265 IF K3>0 THEN 4330
4270 PRINT "Science officer SPOCK reports:"
4275 PRINT "'Sensors show no enemy ships in this"
4280 PRINT "quadrant'":GOTO 1990
4330 IF D(8)<0 THEN PRINT "Computer failure hampers accuracy"
4350 PRINT "Phasers locked on target;  "
4360 PRINT "Energy available =";E;"units"
4370 INPUT "Number of units to fire";X:IF X<=0 THEN 1990
4400 IF E-X<0 THEN 4360
4410 E=E-X:IF D(7)<0 THEN X=X*RND(1)
4450 H1=INT(X/K3):FOR I=1 TO 3:IF K(I,3)<=0 THEN 4670
4480 H=INT((H1/FND(0))*(RND(1)+2)):IF H>.15*K(I,3) THEN 4530
4500 PRINT "Sensors show no damage to enemy"
4502 PRINT "at ";K(I,1);",";K(I,2)
4505 GOTO 4670
4530 K(I,3)=K(I,3)-H:PRINT
4540 PRINT H;"unit hit on KLINGON"
4550 PRINT "at sector";K(I,1);",";K(I,2)
4555 IF K(I,3)<=0 THEN PRINT "*** KLINGON DESTROYED ***":GOTO 4580
4560 PRINT "(Sensors show";K(I,3);"units remaining)":GOTO 4670
4580 K3=K3-1:K9=K9-1:Z1=K(I,1):Z2=K(I,2):A$="   ":GOSUB 8670
4650 K(I,3)=0:G(Q1,Q2)=G(Q1,Q2)-100:Z(Q1,Q2)=G(Q1,Q2):IF K9<=0 THEN 6370
4670 NEXT I:GOSUB 6000:GOTO 1990
4690 REM PHOTON TORPEDO CODE BEGINS HERE
4700 IF P<=0 THEN 4720
4710 GOTO 4730
4720 A$="All photon torpedoes expended":GOSUB 14000:GOTO 1990
4730 IF D(5)<0 THEN 4750
4740 GOTO 4760
4750 A$="Photon tubes are not operational":GOSUB 14000:GOTO 1990
4760 CLS:INPUT "Photon torpedo course (1-9)";C1:IF C1=9 THEN C1=1
4780 IF C1>=1 AND C1<9 THEN 4850
4790 PRINT "Ensign CHEKOV reports:"
4795 PRINT "'Incorrect course data, Sir!'"
4800 GOTO 1990
4850 X1=C(C1,1)+(C(C1+1,1)-C(C1,1))*(C1-INT(C1)):E=E-2:P=P-1
4860 X2=C(C1,2)+(C(C1+1,2)-C(C1,2))*(C1-INT(C1)):X=S1:Y=S2
4910 PRINT "Torpedo track:"
4920 X=X+X1:Y=Y+X2:X3=INT(X+.5):Y3=INT(Y+.5)
4960 IF X3<1 OR X3>8 OR Y3<1 OR Y3>8 THEN 5490
5000 PRINT "               ";X3;",";Y3:A$="   ":Z1=X:Z2=Y:GOSUB 8830
5050 IF Z3<>0 THEN 4920
5060 A$="-K-":Z1=X:Z2=Y:GOSUB 8830:IF Z3=0 THEN 5210
5110 PRINT "*** KLINGON DESTROYED ***":K3=K3-1:K9=K9-1:IF K9<=0 THEN 6370
5150 FOR I=1 TO 3:IF X3=K(I,1) AND Y3=K(I,2) THEN 5190
5180 NEXT I:I=3
5190 K(I,3)=0:GOTO 5430
5210 A$=" * ":Z1=X:Z2=Y:GOSUB 8830:IF Z3=0 THEN 5280
5260 PRINT "Star at";X3;",";Y3;"absorbed torpedo energy."
5265 GOSUB 6000:GOTO 1990
5280 A$="(S)":Z1=X:Z2=Y:GOSUB 8830:IF Z3=0 THEN 4760
5330 PRINT "*** STARBASE DESTROYED ***":B3=B3-1:B9=B9-1
5360 IF B9>0 OR K9>T-T0-T9 THEN 5400
5370 PRINT "That does it, Captain!! You are hereby"
5375 PRINT "relieved of command and sentenced to"
5380 PRINT "99 stardates at hard labor on Cygnus"
5385 PRINT "12!!":PAUSE 500
5390 GOTO 6270
5400 PRINT "Starfleet command reviewing your"
5405 PRINT "record to consider COURT MARTIAL!"
5410 D0=0
5430 Z1=X:Z2=Y:A$="   ":GOSUB 8670
5470 G(Q1,Q2)=K3*100+B3*10+S3:Z(Q1,Q2)=G(Q1,Q2):GOSUB 6000:GOTO 1990
5490 PRINT "Torpedo missed":GOSUB 6000:GOTO 1990
5520 REM SHIELD CONTROL
5530 IF D(7)<0 THEN 5550
5540 GOTO 5560
5550 A$="Shield control inoperable":GOSUB 14000:GOTO 1990
5560 CLS:PRINT "Energy available =";E+S
5570 INPUT "Number of units to shields";X
5580 IF X<0 OR S=X THEN PRINT "<SHIELDS UNCHANGED>":PAUSE 300:GOTO 1990
5590 IF X<=E+S THEN 5630
5600 PRINT "Shield control reports ";
5605 PRINT "'This is not the Federation Treasury.'"
5610 PRINT "<SHIELDS UNCHANGED>":PAUSE 300:GOTO 1990
5630 E=E+S-X:S=X:PRINT "Deflector control room report:"
5660 PRINT "'Shields now at";INT(S);"units per your"
5662 PRINT "command.'":PRINT
5665 GOTO 1990
5680 REM DAMAGE CONTROL
5690 IF D(6)>=0 THEN 5910
5700 A$="Damage control report not available":GOSUB 14000:IF D0=0 THEN 1880
5720 D3=0:FOR I=1 TO 8:IF D(I)<0 THEN D3=D3+.1
5760 NEXT I:IF D3=0 THEN 1880
5780 PRINT:D3=D3+D4:IF D3>=1 THEN D3=.9
5810 PRINT "Technicians standing by to effect"
5820 PRINT "repairs to your ship; estimated"
5830 PRINT "time to repair:";.01*INT(100*D3);"stardates"
5840 PRINT "Will you authorize the repair"
5850 PRINT "order (Y/N)?":I=INKEY(0)
5860 I=INKEY(10) AND 223:IF I<>ASC("Y")THEN 1880
5870 FOR I=1 TO 8:IF D(I)<0 THEN D(I)=0
5890 NEXT I:T=T+D3+.1
5910 CLS:PRINT "DEVICE             STATE OF REPAIR":FOR R1=1 TO 8
5920 GOSUB 8790:PRINT G2$;LEFT$(Z$,25-LEN(G2$));INT(D(R1)*100)*.01
5950 NEXT R1:PRINT:IF D0<>0 THEN 5720
5980 GOTO 1990
5990 REM KLINGONS SHOOTING
6000 IF K3<=0 THEN RETURN
6010 IF D0<>0 THEN 6030
6020 GOTO 6040
6030 A$="Starbase shields protect the ENTERPRISE":GOSUB 14000:RETURN
6040 FOR I=1 TO 3:IF K(I,3)<=0 THEN 6200
6060 H=INT((K(I,3)/FND(1))*(2+RND(1))):S=S-H:K(I,3)=K(I,3)/(3+RND(0))
6080 PRINT:PRINT H;"unit hit on ENTERPRISE from"
6085 PRINT "sector";K(I,1);",";K(I,2)
6090 IF S<=0 THEN 6240
6100 PRINT "<Shields down to";S;"units>":IF H<20 THEN 6200
6120 IF RND(1)>.6 OR H/S<=.02 THEN 6200
6140 R1=FNR(1):D(R1)=D(R1)-H/S-.5*RND(1):GOSUB 8790
6170 PRINT "Damage control reports"
6180 PRINT "<";G2$;"> damages by the hit"
6200 NEXT I:RETURN
6210 REM END OF GAME
6220 CLS
6230 PRINT "It is stardate";T:GOTO 6275
6240 PRINT "The ENTERPRISE has been DESTROYED. Then"
6250 PRINT "Federation will be conquered":GOTO 6230
6270 CLS
6275 PRINT "THere were";K9;"KLINGON battle cruisers"
6280 PRINT "left at the end of your mission."
6290 PRINT:PRINT:IF B9=0 THEN 6360
6310 PRINT "The Federation is in need of a new"
6315 PRINT "starship commander for a similar"
6320 PRINT "mission -- If there is a volunteer,"
6330 PRINT "let him step forward and enter"
6340 A$="":INPUT "'AYE'";A$:IF A$="AYE" OR A$="aye" THEN RUN
6360 END
6370 CLS:PRINT "Congratulation, Captain!  Then last"
6375 PRINT "KLINGON battle cruiser menacing the"
6380 PRINT "Federation has been destroyed.":PRINT
6400 PRINT "Your efficiency rating is";1000*(K7/(T-T0))^2:GOTO 6290
6420 REM SHORT RANGE SENSOR SCAN & STARTUP SUBROUTINE
6430 FOR I=S1-1 TO S1+1:FOR J=S2-1 TO S2+1
6450 IF INT(I+.5)<1 OR INT(I+.5)>8 OR INT(J+.5)<1 OR INT(J+.5)>8 THEN 6540
6490 A$="(S)":Z1=I:Z2=J:GOSUB 8830:IF Z3=1 THEN 6580
6540 NEXT J:NEXT I:D0=0:GOTO 6650
6580 D0=1:C$="DOCKED":E=E0:P=P0
6620 PRINT "Shields dropped for docking purposes":S=0:GOTO 6720
6650 IF K3>0 THEN C$="**RED**":GOTO 6720
6660 C$="GREEN":IF E<E0*.1 THEN C$=">YELLOW<"
6720 IF D(2)>=0 THEN 6770
6730 CLS:PRINT:PRINT "*** SHORT RANGE SENSORS ARE OUT ***":PRINT:RETURN
6770 CLS
6772 PRINT "   1  2  3  4  5  6  7  8"
6775 O1$="  ------------------------":PRINT O1$:FOR I=1 TO 8:CY=(I-1)*2
6820 LOCATE 0,CY+2:PRINT I;
6822 LOCATE 2,CY+2:FOR J=(I-1)*24+1 TO (I-1)*24+22STEP3
6825 PRINT MID$(Q$,J,3);:NEXT J
6828 LOCATE 28,CY+2
6830 ON I GOTO 6850,6900,6960,7020,7070,7120,7180,7240
6850 PRINT "Stardate ":LOCATE 28,CY+3:PRINT INT(T*10)*.1:GOTO 7260
6900 PRINT "Condition":LOCATE 28,CY+3:PRINT C$:GOTO 7260
6960 PRINT "Quadrant ":LOCATE 28,CY+3:PRINT Q1;",";Q2:GOTO 7260
7020 PRINT "Sector   ":LOCATE 28,CY+3:PRINT S1;",";S2:GOTO 7260
7070 PRINT "Torpedoes":LOCATE 28,CY+3:PRINT INT(P):GOTO 7260
7120 PRINT "Energy   ":LOCATE 28,CY+3:PRINT INT(E+S):GOTO 7260
7180 PRINT "Shield   ":LOCATE 28,CY+3:PRINT INT(S):GOTO 7260
7240 PRINT "Klingons ":LOCATE 28,CY+3:PRINT INT(K9)
7260 NEXT I:PRINT O1$:RETURN
7280 REM LIBRARY COMPUTER CODE
7290 IF D(8)<0 THEN PRINT "COMPUTER DISABLED":GOTO 1990
7320 CLS:PRINT:PRINT "COMPUTER ACTIVE"
7350 PRINT
7360 PRINT "FUNCTIONS AVAILABLE FROM COMPUTER:"
7370 PRINT " 0 = Cumulative galactic record"
7372 PRINT " 1 = Status report"
7374 PRINT " 2 = Photon torpedo data"
7376 PRINT " 3 = Starbase navigation data"
7378 PRINT " 4 = Direction/Distance calculator"
7380 PRINT " 5 = Galaxy 'Region name' map":PRINT
7382 INPUT "Awaiting command";A:IF A<0 THEN 1990
7384 PRINT:H8=1:ON A+1 GOTO 7540,7900,8070,8500,8150,7400
7385 GOTO 7320
7390 REM SETUP TO CHANGE CUM GAL RECORD TO GALAXY MAP
7400 H8=0:G5=1:CLS:PRINT SPC(15)"THE GALAXY":GOTO 7550
7530 REM CUM GALACTIC RECORD
7540 CLS:PRINT:REM PRINT "        ";
7544 PRINT "RECORD OF GALAXY FOR QUADRANT";Q1;",";Q2
7546 PRINT
7550 PRINT "     1   2   3   4   5   6   7   8"
7560 O1$="    --- --- --- --- --- --- --- ---"
7570 PRINT O1$:FOR I=1 TO 8:PRINT I;:IF H8=0 THEN 7740
7630 FOR J=1 TO 8:PRINT " ";:IF Z(I,J)=0 THEN PRINT "***";:GOTO 7720
7700 PRINT RIGHT$(STR$(Z(I,J)+1000),3);
7720 NEXT J:GOTO 7850
7740 Z4=I:Z5=1:GOSUB 9030:J0=INT(12-.5*LEN(G2$)):PRINT TAB(J0);G2$;
7800 Z5=5:GOSUB 9030:J0=INT(27-.5*LEN(G2$)):PRINT TAB(J0);G2$;
7850 PRINT:PRINT O1$:NEXT I:PRINT:GOTO 1990
7890 REM STATUS REPORT
7900 CLS:PRINT "STATUS REPORT:":PRINT:X$="":IF K9>1 THEN X$="s"
7940 PRINT:PRINT "Klingon";X$;" left: ";K9
7960 PRINT "Stardates left for mission:";
7965 PRINT .1*INT((T0+T9-T)*10)
7970 X$="s":IF B9<2 THEN X$="":IF B9<1 THEN 8010
7980 PRINT "Federation Starbase";X$;" in galaxy:";
7985 PRINT B9
7990 GOTO 5690
8010 PRINT "Your stupidity has left you on your on"
8020 PRINT "in the galaxy! You have no Starbases"
8030 PRINT "left!":GOTO 5690
8060 REM TORPEDO, BASE NAV, D/D CALCULATOR
8070 IF K3<=0 THEN 4270
8080 X$="":IF K3>1 THEN X$="s"
8090 PRINT "From ENTERPRISE to Klingon cruiser";X$
8100 H8=0:FOR I=1 TO 3:IF K(I,3)<=0 THEN 8480
8110 W1=K(I,1):X=K(I,2)
8120 C1=S1:A=S2:GOTO 8220
8150 CLS:PRINT "DIR./DIST. CALCULATOR:"
8160 PRINT "You are at quadrant ";Q1;",";Q2;" sector ";S1;",";S2
8170 PRINT "Please enter":INPUT "  INITIAL coordinates (X,Y)";C1,A
8200 INPUT "  FINAL coordinates (X,Y)";W1,X
8220 X=X-A:A=C1-W1:IF X<0 THEN 8350
8250 IF A<0 THEN 8410
8260 IF X>0 THEN 8280
8270 IF A=0 THEN C1=5:GOTO 8290
8280 C1=1
8290 IF ABS(A)<=ABS(X) THEN 8330
8310 PRINT "DIRECTION =";C1+(((ABS(A)-ABS(X))+ABS(A))/ABS(A)):GOTO 8460
8330 PRINT "DIRECTION =";C1+(ABS(A)/ABS(X)):GOTO 8460
8350 IF A>0 THEN C1=3:GOTO 8420
8360 IF X<>0 THEN C1=5:GOTO 8290
8410 C1=7
8420 IF ABS(A)>=ABS(X) THEN 8450
8430 PRINT "DIRECTION =";C1+(((ABS(X)-ABS(A))+ABS(X))/ABS(X)):GOTO 8460
8450 PRINT "DIRECTION =";C1+(ABS(X)/ABS(A))
8460 PRINT "DISTANCE =";SQR(X^2+A^2):IF H8=1 THEN 1990
8480 NEXT I:GOTO 1990
8500 IF B3<>0 THEN PRINT "From ENTERPRISE to STARBASE:":W1=B4:X=B5:GOTO 8120
8510 CLS:PRINT "Mr. SPOCK report:"
8515 PRINT "'Sensors show no Starbases in this"
8520 PRINT "quadrant.'":GOTO 1990
8580 REM FIND EMPTY PLACE IN QUADRANT (FOR THINGS)
8590 R1=FNR(1):R2=FNR(1):A$="   ":Z1=R1:Z2=R2:GOSUB 8830:IF Z3=0 THEN 8590
8600 RETURN
8660 REM INSERT IN STRING ARRAY FOR QUADRANT
8670 S8=INT(Z2-.5)*3+INT(Z1-.5)*24+1
8675 IF LEN(A$)<>3 THEN PRINT "ERROR":STOP
8680 IF S8=1 THEN Q$=A$+RIGHT$(Q$,189):RETURN
8690 IF S8=190 THEN Q$=LEFT$(Q$,189)+A$:RETURN
8700 Q$=LEFT$(Q$,S8-1)+A$+RIGHT$(Q$,190-S8):RETURN
8780 REM PRINTS DEVICE NAME
8790 ON R1 GOTO 8792,8794,8796,8798,8800,8802,8804,8806
8792 G2$="Warp Engines":RETURN
8794 G2$="Short Range Sensors":RETURN
8796 G2$="Long Range Sensors":RETURN
8798 G2$="Phaser Control":RETURN
8800 G2$="Photon Tubes":RETURN
8802 G2$="Damage Control":RETURN
8804 G2$="Shield Control":RETURN
8806 G2$="Library-Computer":RETURN
8820 REM STRING COMPARISON IN QUADRANT ARRAY
8830 Z1=INT(Z1+.5):Z2=INT(Z2+.5):S8=(Z2-1)*3+(Z1-1)*24+1:Z3=0
8890 IF MID$(Q$,S8,3)<>A$ THEN RETURN
8900 Z3=1:RETURN
9010 REM QUADRANT NAME IN G2$ FROM Z4,Z5 (=Q1,Q2)
9020 REM CALL WITH G5=1 TO GET REGION NAME ONLY
9030 IF Z5<=4 THEN ON Z4 GOTO 9040,9050,9060,9070,9080,9090,9100,9110
9035 GOTO 9120
9040 G2$="ANTARES":GOTO 9210
9050 G2$="RIGEL":GOTO 9210
9060 G2$="PROCYON":GOTO 9210
9070 G2$="VEGA":GOTO 9210
9080 G2$="CANOPUS":GOTO 9210
9090 G2$="ALTAIR":GOTO 9210
9100 G2$="SAGITTARIUS":GOTO 9210
9110 G2$="POLLUX":GOTO 9210
9120 ON Z4 GOTO 9130,9140,9150,9160,9170,9180,9190,9200
9130 G2$="SIRIUS":GOTO 9210
9140 G2$="DENEB":GOTO 9210
9150 G2$="CAPELLA":GOTO 9210
9160 G2$="BETELGEUSE":GOTO 9210
9170 G2$="ALDEBARAN":GOTO 9210
9180 G2$="REGULUS":GOTO 9210
9190 G2$="ARCTURUS":GOTO 9210
9200 G2$="SPICA"
9210 IF G5<>1 THEN ON Z5 GOTO 9230,9240,9250,9260,9230,9240,9250,9260
9220 RETURN
9230 G2$=G2$+" I":RETURN
9240 G2$=G2$+" II":RETURN
9250 G2$=G2$+" III":RETURN
9260 G2$=G2$+" IV":RETURN
10000 REM INSTRUCTIONS FOR "SUPER STARTREK"  MAR 5, 1978
10010 PRINT "Do you need instructions (Y/N)":I=INKEY(10)
10020 I=INKEY(10) AND 223:IF I=0 THEN 10020
10030 IF I<>ASC("Y") THEN 11160
10050 CLS
10090 PRINT "   INSTRUCTIONS FOR 'SUPER STAR TREK'  "
10100 PRINT
10110 PRINT "1. When you see <COMMAND ?> printed,"
10115 PRINT "enter one of the legal commands: NAV,"
10120 PRINT "SRS,LRS,PHA,TOR,SHE,DAM,COM, or XXX."
10125 PRINT
10130 PRINT "2. If you should type in an illegal"
10135 PRINT "command, you'll get a short list of"
10140 PRINT "the legal commands printed out."
10145 PRINT
10150 PRINT "3. Some commands require you to enter"
10155 PRINT "data: for example, the 'NAV' command"
10160 PRINT "comes back with 'COURSE (1-9) ?'.  If"
10170 PRINT "you type in illegal data (like negative"
10180 PRINT "numbers), than command will be aborted"
10190 PRINT
10200 PRINT "The galaxy is divided into an 8 x 8"
10210 PRINT "quadrant grid, and each quadrant is"
10220 PRINT "further divided into an 8 x 8 sector"
10230 PRINT "grid."
10240 GOSUB 12000:CLS
10250 PRINT "You will be assigned a starting point"
10260 PRINT "somewhere in the galaxy to begin a tour"
10310 PRINT "of duty as comander of the starship"
10320 PRINT "<ENTERPRISE>; your mission: to seek and"
10330 PRINT "destroy the fleet of Klingon warships"
10340 PRINT "which are menacing the United Federa-"
10350 PRINT "tion of Planets."
10360 PRINT
10370 PRINT "You have the following commands"
10375 PRINT "available to you as Captain of the"
10380 PRINT "starship ENTERPRISE:"
10385 GOSUB 12000:CLS
10390 PRINT "<NAV> command = WARP ENGINE CONTROL"
10395 PRINT
10400 PRINT "Course is in a circular       4  3  2"
10410 PRINT "numerical vector arrangement   . . ."
10420 PRINT "as shown. Integer and real      ..."
10430 PRINT "values may be used, thus    5 ---*--- 1"
10440 PRINT "course 1.5 is half-way          ..."
10450 PRINT "between 1 and 2.               . . ."
10460 PRINT "Values may approach 9.0,      6  7  8"
10470 PRINT "which itself is equivalent    COURSE"        
10480 PRINT "to 1.0."
10490 PRINT "One warp factor is the size of one"
10500 PRINT "quadrant. Therefore, to get from"
10510 PRINT "quadrant 6,5 to 5,5, you would use"
10520 PRINT "COURSE 3, WARP FACTOR 1."
10530 GOSUB 12000:CLS
10540 PRINT "<SRS> command = SHORT RANGE SENSOR SCAN"
10545 PRINT
10550 PRINT "Shows you a scan of your present"
10555 PRINT "quadrant."
10560 PRINT "Symbology on your sensor screen is as"
10565 PRINT "follows:"
10570 PRINT "<E> = your starship's position"
10580 PRINT "-K- = Klingon battle cruiser"
10590 PRINT "(S) = Federation Starbase (Refuel/"
10595 PRINT "      /Repair/Re-Arm here!)"
10600 PRINT " *  = Star"
10605 PRINT
10610 PRINT "A condensed 'STATUS REPORT' will also"
10615 PRINT "be presented."
10620 GOSUB 12000:CLS
10640 PRINT "<LRS> command = LONG RANGE SENSOR SCAN"
10645 PRINT
10650 PRINT "Shows conditions in space for one"
10655 PRINT "quadrant on each side of the"
10660 PRINT "ENTERPRISE (which is in the middle of"
10665 PRINT "the scan)"
10670 PRINT "The scan is coded in the form <###>,"
10675 PRINT "where the units digit is the number of"
10680 PRINT "stars, the tens digit is the number"
10690 PRINT "of starbases, and the hundreds digit"
10700 PRINT "is the number of Klingons."
10705 PRINT
10706 PRINT "Example: 207"
10708 PRINT "2 Klingons, NO Starbases, & 7 Stars."
10710 GOSUB 12000:CLS
10720 PRINT "<PHA> command = PHASER CONTROL."
10725 PRINT
10730 PRINT "Allows you to destroy the Klingon"
10735 PRINT "battle cruisers by zapping them with"
10740 PRINT "suitably large units of energy to"
10750 PRINT "deplete their shield power."
10760 PRINT "Remember Klingon have phasers too!"
10770 GOSUB 12000:CLS
10780 PRINT "<TOR> command = PHOTON TORPEDO CONTROL"
10785 PRINT
10790 PRINT "Torpedo course is the same as used in"
10795 PRINT "Warp engine control."
10800 PRINT "If you hit the Klingon vessel, it is"
10810 PRINT "destroyed and cannot fire back at you."
10815 PRINT "IIf you miss, you are subject to its"
10820 PRINT "phaser fire. In either case, you are"
10825 PRINT "also subject to the phaser fire of"
10830 PRINT "all other Klingons in the quadrant."
10835 PRINT
10840 PRINT "The Library-Computer (<COM> command)"
10845 PRINT "has an option to compute torpedo"
10850 PRINT "trajectory for you (option 2)"
10855 GOSUB 12000:CLS
10860 PRINT "<SHE> command = SHIELD CONTROL"
10865 PRINT
10870 PRINT "Defines the number of energy units to"
10875 PRINT "be assigned to the shields.  Energy is"
10880 PRINT "taken from total ship's energy. Note"
10890 PRINT "that the status display TOTAL ENERGY"
10895 PRINT "includes shield energy"
10900 GOSUB 12000:CLS
10910 PRINT "<DAM> command = DAMAGE CONTROL REPORT"
10915 PRINT
10920 PRINT "Gives the state of repair of all"
10925 PRINT "devices. Where a negative 'STATE OF"
10930 PRINT "REPAIR' shows that the device is"
10940 PRINT "temporarily damaged."
10950 GOSUB 12000:CLS
10960 PRINT "<COM> command = LIBRARY-COMPUTER"
10965 PRINT
10970 PRINT "The Library-Computer contains six"
10975 PRINT "options:"
10977 PRINT
10980 PRINT "Option 0 = CUMULATIVE GALACTIC RECORD"
10990 PRINT "This otpion showes computer memory of"
10995 PRINT "the results of all previous short and"
11000 PRINT "long range sensor scans"
11005 PRINT
11010 PRINT "Option 1 = STATUS REPORT"
11020 PRINT "This option shows the number of"
11025 PRINT "Klingons, Stardates, and Starbases"
11030 PRINT "remaining in the game."
11035 PRINT
11040 PRINT "Option 2 = PHOTON TORPEDO DATA"
11050 PRINT "This gives directions and distance"
11055 PRINT "from the ENTERPRISE to all Klingons"
11060 PRINT "in your quadrant"
11065 GOSUB 12000:CLS
11070 PRINT "Option 3 = STARBASE NAV DATA"
11080 PRINT "This option gives direction and"
11085 PRINT "distance to any Starbase within"
11090 PRINT "your quadrant"
11095 PRINT
11100 PRINT "Option 4 = DIRECTION/DISTANCE"
11105 PRINT "CALCULATOR"
11110 PRINT "This option allows you to enter"
11115 PRINT "coordinates for direction/"
11120 PRINT "distance calculations"
11125 PRINT
11130 PRINT "Option 5 = GALACTIC/ REGION NAME/ MAP"
11140 PRINT "This option prints the names of the"
11145 PRINT "sixteen major galactic regions"
11150 PRINT "referred to in the game."
11155 GOSUB 12000:CLS
11160 RETURN
11200 IF A$="" THEN RETURN:ELSE ZA$="":FOR I=1 TO LEN(A$)
11210 ZA$=ZA$+CHR$(ASC(MID$(A$,I,1)) AND 223)
11220 NEXT:A$=ZA$:RETURN
12000 PRINT:PRINT "Press ENTER to continue":I=INKEY(0)
12010 I=INKEY(10):IF I=0 THEN 12010
12020 RETURN
13000 REM PLAY THE START TREK INTRO THEME
13005 VOLUME 0,15
13010 SOUND 3,3810,40:PAUSE 40:SOUND 3,3930,50:PAUSE 50
13020 SOUND 3,3930,120:PAUSE 140:SOUND 3,3921,50:PAUSE 50
13030 SOUND 3,3899,50:PAUSE 50:SOUND 3,3875,50:PAUSE 50
13040 SOUND 3,3862,50:PAUSE 60:SOUND 3,3862,120:PAUSE 120
13050 SOUND 3,0,1
13080 RETURN
14000 REM PRINT A MESSAGE FOR A WHILE AND THEN REMOVE IT
14010 LOCATE0,22:PRINT A$;:PAUSE 200
14020 LOCATE0,22:PRINT SPC(40);
14030 RETURN
20000 RESTORE 21000
20010 FOR Y=4 TO 8:LOCATE 4,Y:FOR X=0 TO 31
20020 READ A:PRINT CHR$(A);
20030 NEXT:NEXT:RETURN
21000 DATA 32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32
21010 DATA 213,157,157,157,157,157,157,221,157,157,157,157,157,157,214,32
21020 DATA 213,157,157,157,157,157,157,157,157,157,157,157,157,214,32,32
21030 DATA 216,157,157,157,157,157,32,32,157,157,157,157,157,157,215,32
21040 DATA 216,157,157,157,157,157,157,157,157,157,150,149,157,215,32
21050 DATA 32,32,32,32,32,32,159,32,159,32,32,32,32,32,32,32,32
21060 DATA 32,32,32,32,32,213,157,157,157,157,151,152,157,157,157,157
21070 DATA 157,157,157,157,157,32,32,157,214,32,32,32,32,32,32,32
21080 DATA 32,32,32,32,32,216,157,157,157,157,157,157,157,157,157,157
21090 DATA 157,157,157,157,157,157,157,157,215,32,32,32,32,32,32,32
