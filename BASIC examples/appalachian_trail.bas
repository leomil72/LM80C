100 SCREEN0
110 LOCATE1,8:X$="APPALACHIAN TRAIL":GOSUB5590
120 LOCATE1,12:X$="(c) by David H. Ahl, 1986":GOSUB5590
125 LOCATE1,14:X$="Adaptation by Leonardo Miliani":GOSUB5590
130 LOCATE1,20:X$="Press any key to continue.":GOSUB5590:A=INKEY(10)
140 RN=RN+1:A=INKEY(10):IFA=0THEN140
150 IFRN>32767THENRN=RN-65535:GOTO150
155 A=RND(RN):CLS:GOSUB 5620
160 DIM HZ(25),DLC(25),LC$(25)
170 A=INKEY(10)
175 A=INKEY(10):IFA=0THENRN=RN+1:GOTO175
178 DB=300:REM New boot gives 300 extra mi
180 REM
190 REM *** Data Input Section ***
200 CLS:PRINT"First we need some data about you.":PRINT
210 INPUT"Your sex (male or female)";A$:GOSUB5540:PRINT
220 IFA$<>"M"ANDA$<>"F"THENPRINT"Answer 'M' or 'F' please.":GOTO210
230 INPUT"Your weight in pounds";WB
240 IFWB>79ANDWB<401THEN260
250 PRINT"Surely you jest. Let's input it again.":GOTO230
260 PRINT"What is your physical condition"
265 PRINT"(1=excellent, 2=good, 3=fair, 4=poor)"
270 INPUT PC$:PC=VAL(PC$)
280 IF PC<1 OR PC>4 THEN PRINT "Answer 1, 2, 3, or 4 please.":GOTO 260
290 PRINT: PRINT "Walking pace: You may change your pace"
295 PRINT"as the hike progresses. Remember, a"
300 PRINT"faster pace covers the distance more"
305 PRINT"quickly but burns more calories and"
310 PRINT"has a higher risk of injury."
320 PRINT"Slow and deliberate......1.7 mph"
330 PRINT"Moderate and vigorous......3 mph"
340 PRINT"Fast and very difficult....4 mph"
350 PRINT:PRINT"At what rate in mph do you wish to walk"
355 INPUT"(number & decimal okay)";RW$:RW=VAL(RW$)
360 IF RW<1 OR RW>4.2 THEN PRINT "A rate of" RW "mph is silly.":GOTO 350
370 PRINT:PRINT "Walking hours per day: You may change"
375 PRINT"this as time goes on."
380 GOSUB6000:CLS:PRINT"To start, how many hours do you wish to"
390 INPUT"walk per day";TW$:TW=VAL(TW$)
395 IF TW>14 THEN PRINT "That's just too ambitious.":GOTO 380
400 IF TW*RW<7 THEN PRINT "You won't even reach NJ by Christmas.":GOTO 380
410 PRINT:PRINT "Your sensitivity to poison ivy:"
420 PRINT " (1) Highly sensitive":PRINT " (2) Moderately sensitive"
430 PRINT " (3) Immune":PRINT " (4) Had series of desensitization shots"
440 INPUT "Which number describes you";IVY$:IVY=VAL(IVY$)
450 IF IVY<1 OR IVY>4 THEN PRINT "What's that? Let's try again.":GOTO 440
460 CLS:PRINT:PRINT "People who have hiked the Trail have"
465 PRINT"different feelings about rain:"
470 PRINT " (1) Let it pour, I love it."
480 PRINT " (2) No problem as long as the sun comes";
485 PRINT "     out every few days."
490 PRINT " (3) Five solid days of rain really gets";
495 PRINT "     me down."
500 PRINT " (4) If I foresee a long stretch of"
505 PRINT "     rain, I'll hole up in a shelter or"
510 PRINT "     motel and wait it out."
520 PRINT:PRINT "Which number most closely describes your";
525 INPUT "feeling";RAIN$:RAIN=VAL(RAIN$)
530 IF RAIN<1 OR RAIN>4 THEN PRINT "Not possible. Again please.":GOTO 520
540 REM 
550 REM Data on what to carry
560 CLS: PRINT:PRINT "You must make some decisions about what"
565 PRINT "to pack."
570 FOR I=1 TO 7:READ SP$(I):PRINT:PRINT SP$(I);":"
580 FOR J=1 TO 4:READ IM$(J),WHT(J),CS(J),VL(J):PRINT J;".. ";IM$(J);
590 WG=WHT(J):PRINT " ";:GOSUB 5430
600 IF VL(J)>0 THEN PRINT ",";VL(J);"cu in";
610 IF CS(J)>0 THEN PRINT ", price: $";CS(J)
620 NEXT J
630 PRINT:PRINT:INPUT "Which one do you want (number)";A
640 IF A<1 OR A>4 THEN PRINT "Come on now; answer 1, 2, 3, or 4":GOTO 630
650 IT$(I)=IM$(A):WT(I)=WHT(A):C0(I)=CS(A):V0(I)=VL(A):CLS:NEXT I
660 V1=V0(1)+V0(5)+V0(7):V1=V0(3)+V0(4):PRINT
670 IF C0(2)<=135 THEN 680
673 IF V0(2)>3000+V1 THEN 720
676 GOTO 690
680 IF V0(2)>3000+V1+V1 THEN 720:REM Internal pack hold stuff?
690 PRINT "Your pack is too small to hold all those";
695 PRINT "things plus clothes and food. You'll"
700 PRINT "have to take a larger pack or some"
705 PRINT "smaller items."
710 PRINT:PRINT "Let's try again...":RESTORE:GOTO 570
720 PRINT "How many changes of underwear do you"
725 INPUT "want to take";U$:U=VAL(U$)
730 IF U>6 THEN PRINT "This is not a picnic. Take fewer.":GOTO 720
740 PRINT "Do you want to take a walking stick"
745 INPUT "(Y or N)";A$:GOSUB 5540
750 SK=0:IF A$="Y" THEN SK=1
760 PRINT:PRINT "To summarize, here is what you have"
765 PRINT "chosen:"
770 FOR I=1 TO 7:PRINT SP$(I) ":";IT$(I):NEXT I
780 PRINT "Changes of underwear:";U
790 IF SK=1 THEN PRINT "Walking stick."
795 GOSUB 6000:CLS
800 PRINT " In addition, you must carry (or wear) a";
805 PRINT "hat, short-sleeve shirt, chamois shirt,"
810 PRINT "light jacket, long underwear, hiking"
815 PRINT "shorts, long pants, 3 pairs socks,"
820 PRINT "eating gear, water bottle, soap, toilet"
825 PRINT "tissue, toilet supplies, towel, first-"
830 PRINT "aid kit, snakebite kit, flashlight, a"
840 PRINT "100' nylon cord, watch, compass,"
845 PRINT "lighter, bandanna, sewing kit, insect"
850 PRINT "repellent, Swiss Army knife, water-"
855 PRINT "purifier tablets, notebook, maps,"
860 PRINT "guidebook, stuff sacks, moleskin,"
865 PRINT "camera, and money.":PRINT
870 FOR I=1 TO 7:WP=WP+WT(I):CS=CS+C0(I):NEXT I:REM Summarize weights
880 WP=WP+190+U*4:IF SK=1 THEN WP=WP+24
890 PRINT "If you bought everything new, the total"
895 PRINT "cost would be $";225+CS
900 PRINT "The total weight of what you are"
905 PRINT "wearing and carrying is";
910 WG=WP:GOSUB 5430:PRINT:PRINT ".... not including food or water."
920 REM 
930 REM Data on food
940 GOSUB 5390:CLS:PRINT "Common systems of food supply include:"
950 PRINT "(1) Caches buried along the trail. Pros:";
955 PRINT "    no wasted time leaving the Trail for";
960 PRINT "    food, heavy items can be buried."
970 PRINT "(2) Food sent to post offices along the"
975 PRINT "    way. Pros:more flexible than caches.";
980 PRINT "    Cons: P.O.s closed nights, Sat pm"
985 PRINT "    and Sun."
990 PRINT "(3) Grocery stores and restaurants.Pros:";
995 PRINT "    good variety, cheap. Cons: wasted"
1000 PRINT "    time leaving Trail, limited opening"
1005 PRINT "    hours."
1010 PRINT "Which will be your major method of food"
1015 INPUT "supply";FOOD$:FOOD=VAL(FOOD$)
1020 IF FOOD<1 OR FOOD>3 THEN PRINT "Sorry, try again.":GOTO 1010
1030 RT=.95:IF FOOD=1 THEN RT=1:REM Off-trail excursions reduce walking rate
1040 RM=RT:GOSUB 5510:REM Short pause before screen clears
1050 CLS
1060 PRINT "Obviously, you will carry your food in"
1065 PRINT "the most efficient form: dried, de-"
1070 PRINT "hydrated, concentrated, etc. However,"
1075 PRINT "you must specify the percentage of your"
1080 PRINT "diet accounted for by each of the"
1085 PRINT "following food groups (remember, all"
1090 PRINT "five must add up to 100):"
1100 PRINT "(1) Dairy foods, cheese, yogurt"
1110 PRINT "(2) Fruits and vegetables"
1120 PRINT "(3) Meat, poultry, fish, eggs"
1130 PRINT "(4) Bread, cereal, seeds, nuts,"
1140 PRINT "(5) Margarine, lard, oils, fats"
1150 CT=0:FOR I=1 TO 5:LOCATE 33,I+6:INPUT FD(I):CT=CT+FD(I):NEXT I
1160 LOCATE 33,13:PRINT CT;"%":PRINT
1170 IF CT=100 THEN PRINT "Very good.":GOTO 1220
1180 PRINT "Sorry, but your percentages add up to"
1185 PRINT CT;"rather than to 100%."
1190 PRINT:PRINT "Press any key to try again."
1200 A=INKEY(10)
1205 A=INKEY(10):IFA=0THEN1205
1207 GOTO 1050
1210 REM 
1220 REM Calculate calorie usage
1230 DM=RW*TW:IF DM>30 THEN DM=30:REM Desired distance=rate*time
1240 REM Calories=metabolism+walking+climbing+camp activities
1250 CD=INT(WB*11.5+WB*DM*.3+(WB+WP/16)*DM*.21+WB*(15-TW)*.22)
1260 PRINT:PRINT "Given your weight and that of your"
1265 PRINT "supplies, your walking speed, and your"
1270 PRINT "walking time per day, you can expect to"
1280 PRINT "burn at least";CD;"calories per day."
1290 PRINT:PRINT "How many calories worth of food do you"
1295 INPUT "want to eat"; CL
1300 IF CL>.6*CD THEN 1330
1310 PRINT "Your body will rebel against burning that"
1315 PRINT "much body fat."
1320 PRINT "Better eat a bit more...":GOTO 1290
1330 IF CL>1.5*CD THEN PRINT "No blimps allowed on the trail.":GOTO 1290
1340 FWT=INT(CL*3.2/(4*FD(1)+3*FD(2)+4*FD(3)+4*FD(4)+9*FD(5))):WG=FWT
1350 PRINT "That means eating an approx food weight"
1355 PRINT "per day of";:GOSUB 5430
1360 PRINT:PRINT:IF FOOD=3 THEN DY=2: GOTO1370
1365 DY=3:REM Avg days of food carried
1370 WADD=DY*FWT+17:CADD=WADD*DM*.21:WP=WP+WADD:CD=CD+CADD
1380 PRINT "Food and water add" WADD "oz. to your"
1385 PRINT "trail weight bringing your total weight"
1390 PRINT "(worn and carried) to";:WG=WP:GOSUB 5430:PRINT
1400 PRINT:X$="Preparations are finally complete!":GOSUB 5590:GOSUB 5510
1410 REM 
1420 REM Main Hiking Section
1430 REM Initial calculations, data, and messages
1440 WP=WP>16:GOSUB 1930:REM Calculate true hiking pace
1450 PRINT:PRINT:GOSUB 5100:REM Read locations
1460 PRINT " It is April 1 and you briskly step out"
1465 PRINT "on the approach trail at Amicalola"
1470 PRINT "Falls, Georgia. You hike the 6.9 miles"
1475 PRINT "to the peak of Springer Mountain and"
1480 PRINT "sign the trail log, the first of many"
1485 PRINT "that you intend to sign. Your hike will"
1490 PRINT "take you through 14 states as the Trail"
1495 PRINT "wanders 2007 miles along the Appalachian";
1500 PRINT "Mountains to Baxter Peak on Mt. Katahdin";
1510 PRINT "in Maine. It is a challenging trail with";
1520 PRINT "an average climb of 200 feet each mile."
1530 PRINT "Fewer than 2000 people have walked its"
1535 PRINT "entire length. Good luck!"
1540 PRINT:GOSUB 5390
1550 REM 
1560 REM Main loop starts here
1570 T=T+3:TD=INT(T+.5):PRINT:GOSUB 5200:REM Check on the hiker every 3 days
1580 DDAYS=3*RM*D1:D=D+DDAYS:IF D>1999 THEN 4560
1590 PRINT "You have walked";INT(D);"miles. ";
1600 FOR I=1 TO 21:IF D>DLC(I)-17 AND D<DLC(I)+17 THEN 1620:REM Near anyplace?
1610 NEXT I:PRINT:GOTO 1630
1620 PRINT "You are near" LC$(I)
1630 IF D>1466 THEN R(10)=.85:TO(10)=2007:REM Slow going in mountains
1640 GOSUB 1710:REM Any mishaps recently?
1650 IF T>12 THEN GOSUB 4030:REM Long stretch of rain?
1660 IF D>1545 AND T>166 THEN GOSUB 4260:REM Snow in New England
1670 IF D>1845 AND KEN=0 THEN GOSUB 4380:REM Kennebec River crossing
1680 IF D>(SV+1)*400 THEN GOSUB 1820:REM Allow user to reset input variables
1690 GOSUB 5390:GOTO 1570:REM Go back to start of hiking loop
1700 REM 
1710 REM Subroutine to determine which mishap, if any, occurs
1720 TM=0:RN=INT(1+40*RND(1)):IF RN>35 THEN RN=36:REM Mishap occurs
1730 IF RN>12 THEN 1760
1740 ON RN GOSUB 2070,2110,2150,2220,2260,2300,2320,2340,2390,2410,2430,2460
1750 GOTO 1800
1760 IF RN>24 THEN 1790
1770 ON RN-12 GOSUB 2530,2570,2600,2630,2670,2710,2750,2770,2790,2820,2870,2910
1780 GOTO 1800
1790 ON RN-24 GOSUB 2960,2990,3010,3040,3070,3100,3130,3170,3200,3220,3260,3340
1800 T=T+TM:GOSUB 2020:RETURN:REM Time delay resulting from mishap
1810 REM 
1820 REM Subroutine to let user reset input variables
1830 SV=SV+1:PRINT "Want to change walking pace or hours of"
1835 INPUT "walking"; A$
1840 GOSUB 5540:IF A$<>"Y "THEN 1910
1850 INPUT "New walking pace (mph)"; RW
1860 IF RW<1 OR RW>4.5 THEN PRINT "A rate of" RW "mph is silly.":GOTO 1850
1870 INPUT "New hours per day on the trail"; TW
1880 IF TW>14 THEN PRINT "Come now; that's just too ambitious.":GOTO 1870
1890 IF SK=1 THEN 1910
1895 PRINT "Want to change your mind and carry a";
1900 INPUT "walking stick";A$:GOSUB 5540:IF A$="Y" THEN SK=1
1910 PRINT:GOSUB 1930:RETURN
1920 REM 
1930 REM Subroutine to establish true hiking pace
1940 D1=RW*TW:REM Desired distance=walking rate*hours per day
1950 IF D>600 THEN PC=1:REM Under 600 miles physical condition limits mileage
1960 DX=6+6*(5-PC):IF D1>DX THEN D1=DX
1970 IF WB>WP>6 THEN 1990:REM Body weight to pack weight ratio under 6?
1980 D1=(.49+.086*WB>WP)*D1:REM Heavy pack cuts down speed
1990 IF SV=0 THEN RETURN:REM No chance to change diet at the start
2000 GOSUB 3820:RETURN:REM Chance to change diet as trip progresses
2010 REM 
2020 REM Subroutine to alter hiking rate due to mishaps
2030 RM=RT:FOR I=1 TO 10:IF TO(I)>T THEN RM=RM*R(I)
2040 NEXT I:RETURN
2050 REM 
2060 REM Subroutines for 35 assorted mishaps follow
2070 IF D>360 OR HZ(6)=1 THEN RETURN
2075 TM=.5:HZ(6)=1
2080 PRINT "You run into Rangers on military"
2085 PRINT "exercises who advise you to avoid the"
2090 PRINT "trail for the next few miles because"
2095 PRINT "of booby traps.":RETURN
2100 REM 
2110 IF D<800 OR HZ(12)=1 THEN RETURN
2115 TM=1:HZ(12)=1
2120 PRINT "The back-support strap on your backpack"
2125 PRINT "has worn through. You'll have to find a"
2130 PRINT "shoemaker to sew on a piece of heavy"
2135 PRINT "leather.":RETURN
2140 REM 
2150 IF D-DB<500 THEN RETURN
2153 IF HZ(15)=1 THEN 2180
2156 TM=1:HZ(15)=1
2160 PRINT "The soles of your boots have worn"
2165 PRINT "through. You'll have to get new soles"
2170 PRINT "at a shoemaker.":RETURN
2180 IF D-DB<800 THEN RETURN
2185 TM=1.5:DB=D:HZ(15)=0
2190 PRINT "Your repaired boot soles are going"
2195 PRINT "again. You'll have to buy a new pair"
2200 PRINT "of boots along the way.":RETURN
2210 REM 
2220 IF D-DB<700 THEN RETURN
2223 IF HZ(24)=1 THEN RETURN
2226 TM=1:HZ(24)=1
2230 PRINT "The uppers on your boots are starting"
2235 PRINT "to separate from the soles. You'll"
2240 PRINT "have to get them repaired by shoemaker.":RETURN
2250 REM 
2260 IF D<1000 OR HZ(23)=1 THEN RETURN
2265 TM=.4:HZ(23)=1
2270 PRINT "The seat of your pants has worn through.";
2275 PRINT "You can take off the pockets and sew"
2280 PRINT "them over the holes.":RETURN
2290 REM 
2300 TM=.3:PRINT "Mice got into your food last night."
2305 PRINT "Yuck.":RETURN
2310 REM 
2320 TM=.3:PRINT "Some Boy Scouts kept you awake 'til 1 am";
2325 PRINT "last night.":RETURN
2330 REM 
2340 IF D>900 THEN RETURN
2345 TM=.1
2350 PRINT "Curs attack you as you are walking"
2355 PRINT "through a small town."
2360 IF SK=1 THEN PRINT "You drive them off with your walking"
2365 PRINT "stick.":RETURN
2370 PRINT "They nip at your heels. You should"
2375 PRINT "really carry a stick.":RETURN
2380 REM 
2390 TM=.3:PRINT "Trail is poorly marked and you get"
2395 PRINT "temporarily lost.":RETURN
2400 REM 
2410 TM=.2:PRINT "Broken zipper on your pack. Lose time"
2415 PRINT "drying stuff.":RETURN
2420 REM 
2430 TM=.3:PRINT "Route marked on map is out of date."
2435 PRINT "You lose time trying to get back on the"
2440 PRINT "trail.":RETURN
2450 REM 
2460 IF HZ(1)>1 THEN RETURN
2465 TM=2:HZ(1)=HZ(1)+1:DOC=1
2470 PRINT "You forget to shake out your boot and"
2475 PRINT "a snake has curled up inside for the"
2480 PRINT "night. You're scared and he's mad.":GOSUB 5510
2490 IF RND(1)<.9 THEN PRINT "He slithers away and all is okay. Whew!":RETURN
2500 PRINT "It's a rattler and he bites you."
2505 PRINT "You'll have to get a doctor."
2510 GOTO 3360
2520 REM 
2530 IF D>165 OR HZ(7)=1 THEN RETURN
2535 HZ(7)=1:RN=INT(1+4*RND(1)):TM=RN
2540 PRINT "Late snow in the Smokies. The trail is"
2545 PRINT "unpassable for" RN "days."
2550 RETURN
2560 REM 
2570 IF HZ(8)>3 THEN RETURN
2575 HZ(8)=HZ(8)+1:R(1)=.9:TO(1)=T+14
2580 PRINT "You have some nasty blisters that will"
2585 PRINT "slow your pace.":RETURN
2590 REM 
2600 IF HZ(9)>2 THEN RETURN
2605 HZ(9)=HZ(9)+1:R(2)=.7:TO(2)=T+3
2610 PRINT "You have bad indigestion from an"
2615 PRINT "unbalanced diet.":RETURN
2620 REM 
2630 IF D>870 OR HZ(10)=1 THEN RETURN
2635 HZ(10)=1:TM=.5
2640 PRINT "A bear got into your food and ripped"
2645 PRINT "your pack last night. It's a good thing"
2650 PRINT "he wasn't hungry for human burgers.":RETURN
2660 REM 
2670 IF HZ(11)>1 THEN RETURN
2675 HZ(11)=HZ(11)+1:R(3)=.75:TO(3)=T+6
2680 PRINT "You twisted your ankle crossing a"
2685 PRINT "stream. That will slow your pace"
2690 PRINT "for a few days. Be more careful!":RETURN
2700 REM 
2710 IF HZ(2)=1 THEN RETURN
2715 HZ(2)=1:DOC=2
2720 PRINT "You slipped on some rocks on a ledge."
2725 PRINT "It's incredibly painful! Better see"
2730 PRINT "a doctor.":GOTO 3360
2740 REM 
2750 TM=.3:PRINT "A branch snaps in your eye. Lose time"
2755 PRINT "to treat it.":RETURN
2760 REM 
2770 TM=.3:PRINT "Bad case of constipation. Better change"
2775 PRINT "diet.":RETURN
2780 REM 
2790 TM=.5:PRINT "You fell in a stream and everything got"
2795 PRINT "wet. Lose time drying out your sleeping bag"
2800 PRINT "and clothes.":RETURN
2810 REM 
2820 IF HZ(13)=1 THEN RETURN
2825 HZ(13)=1:TM=1.5
2830 PRINT "Last night you saw an animal moving near"
2835 PRINT "you and swatted at it."
2840 GOSUB 5510:PRINT "Big mistake! It was a skunk. You'll have";
2845 PRINT "to wash what you can and replace the"
2850 PRINT "rest.":RETURN
2860 REM 
2870 IF HZ(14)=1 THEN RETURN
2875 HZ(14)=1:TM=2
2880 PRINT "Lowliest of the low! Someone stole your"
2885 PRINT "pack while you were taking a shower."
2890 PRINT "You'll have to replace everything.":RETURN
2900 REM 
2910 IF HZ(3)=1 THEN RETURN
2915 HZ(3)=1:TM=3:DOC=3
2920 PRINT "After five solid days of rain, every-"
2925 PRINT "thing is soaked and you just can't stop"
2930 PRINT "shivering. You feel so terrible that"
2940 PRINT "you'll have to see a doctor.":GOTO 3360
2950 REM 
2960 IF HZ(16)=1 THEN RETURN
2965 HZ(16)=1:TM=.4
2970 PRINT "You cut your hand badly with your knife.";
2975 PRINT "Be careful!":RETURN
2980 REM 
2990 TM=.3:PRINT "Socks worn through. You'll have to buy"
2995 PRINT "new ones.":RETURN
3000 REM 
3010 IF HZ(17)=1 THEN RETURN
3015 HZ(17)=1:TM=1
3020 PRINT "Bad toothache. You'll have to find a"
3025 PRINT "dentist soon.":RETURN
3030 REM 
3040 IF HZ(18)=2 OR WT(1)=0 THEN RETURN
3045 HZ(18)=HZ(18)+1:TM=.5
3050 PRINT "Tent ripped. You'll have to sew on a"
3055 PRINT "patch.":RETURN
3060 REM 
3070 IF D<1000 OR HZ(19)=1 THEN RETURN
3075 HZ(19)=1:TM=.3
3080 PRINT "Your groundcloth is in shreds. Must buy"
3085 PRINT "a new one.":RETURN
3090 REM 
3100 IF D<500 OR HZ(20)=1 OR WT(7)=0 THEN RETURN
3105 HZ(20)=1:TM=.4
3110 PRINT "Bad rip in raingear. Must get a repla-"
3115 PRINT "cement.":RETURN
3120 REM 
3130 IF D<600 OR HZ(21)=1 OR SK=0 THEN RETURN
3135 HZ(21)=1:R(4)=.9:TM=.4
3140 PRINT "Your walking stick breaks. You can get"
3145 PRINT "a new one in the next town. Maybe they"
3150 PRINT "make 'em better up here.":TO(4)=T+4:RETURN
3160 REM 
3170 IF HZ(22)=1 THEN RETURN
3175 HZ(22)=1:TM=.4
3180 PRINT "Your water bag springs a leak. Better"
3185 PRINT "get a new one soon!":RETURN
3190 REM 
3200 TM=.2:PRINT "You run out of toilet tissue. Yucko!":RETURN
3210 REM 
3220 PRINT "You run out of water and the springs"
3225 PRINT "marked on the map seem to have vanished"
3230 PRINT "or dried up. Better take it easy for a"
3235 PRINT "bit."
3240 HZ(5)=1:R(5)=.9:TO(5)=T+3:RETURN
3250 REM 
3260 IF HZ(4)=3 THEN RETURN
3265 HZ(4)=HZ(4)+1:DOC=4
3270 PRINT "Oh oh, you stumble into a thicket of"
3275 PRINT "poison ivy. Zowie!"
3280 IF IVY=1 OR (IVY=2 AND RND(1)>.5) THEN 3290
3285 GOTO 3300
3290 PRINT "You got it really bad. You'll have to"
3295 PRINT "see a doctor.":GOTO 3360
3300 PRINT "Like it or not, you got a mild case."
3305 PRINT "It itches like crazy but the calamine"
3310 PRINT "seems to have it under control."
3315 PRINT "It slows you down tho'."
3320 R(6)=.9:TO(6)=T+7:TM=.4:RETURN
3330 REM 
3340 PRINT "Walking...walking...walking...walking.":RETURN
3350 REM 
3360 REM Subroutine to deal with serious injuries and illnesses
3370 PRINT:PRINT "You're feeling horrible, but you found"
3375 PRINT "a nice country doctor."
3380 ON DOC GOTO 3410,3480,3570,3640,3700
3390 REM 
3400 REM Rattlesnake bite
3410 PRINT "He examines your swollen leg and says,":GOSUB 5510
3420 PRINT "'Good thing you got here so quickly."
3425 PRINT "I'll give you a shot of antivenin but"
3430 PRINT "you're going to be out of commission for";
3435 PRINT "a good 4 days-and even after that you'll"
3440 PRINT "have to take it easy for a while.'"
3450 TM=4:R(7)=.7:TO(7)=T+15:RETURN
3460 REM 
3470 REM Broken or sprained leg
3480 PRINT "He examines your leg and says,":GOSUB 5510:IF RND(1)>.7 THEN 3520
3490 PRINT "'That's a very nasty sprain. I'll tape"
3495 PRINT "it up, but you'll have to take it easy"
3500 PRINT "for at least a month.'"
3510 TM=1.5:R(8)=.6:TO(8)=T+30:RETURN
3520 PRINT "'Bad news, my young friend. Your leg is"
3525 PRINT "broken. I'm surprised you got here under"
3530 PRINT "your own power. But this is the end of"
3535 PRINT "your hike. Sorry, but maybe you can try"
3540 PRINT "again next year.'":GOTO 4610
3550 REM 
3560 REM Hypothermia
3570 PRINT "He examines you and says,":GOSUB 5510:TM=INT(5+6*RND(1))
3580 PRINT "'That prolonged rain and cold has put"
3585 PRINT "you in a condition that we call hypo-"
3590 PRINT "thermia. You can stay in town here at"
3595 PRINT "a motel and I'll keep an eye on you,"
3600 PRINT "but you can't go back on the Trail for"
3605 PRINT "at least" TM "days."
3610 PRINT "Sorry, but that's the way it is.'":RETURN
3620 REM 
3630 REM Poison Ivy
3640 PRINT "He only needs a glance to see that you"
3645 PRINT "are suffering from an extremely bad case";
3650 PRINT "of poison ivy. He puts you in a clinic"
3655 PRINT "for a few days and tells you what you"
3660 PRINT "already knew-that you'll just have to"
3665 PRINT "let it run its course.":TM=5:RETURN
3680 REM 
3690 REM Excessive weight loss
3700 PRINT "He examines you and says,":GOSUB 5510:IF WS>.33*WB THEN 3770
3710 PRINT "'You may want to lose some weight, but"
3715 PRINT "it's coming off far too quickly. Your"
3720 PRINT "body just can't cope. I'm going to keep"
3725 PRINT "you here for a week on a controlled"
3730 PRINT "diet-and then for the rest of the trip"
3740 PRINT "you'll have to go at a slower pace."
3745 PRINT "Also, I want you to consume at least as"
3750 PRINT "many calories per day as your body is"
3755 PRINT "using up.'"
3760 WB=1.18*WB:TM=7:R(9)=.8:TO(9)=T+30:RETURN
3770 PRINT "'Believe it or not, you are in an"
3775 PRINT "advanced stage of starvation. You're"
3780 PRINT "going to have to remain here for a few"
3785 PRINT "weeks on a controlled diet to stabilize"
3790 PRINT "your body chemistry. And then you will"
3795 PRINT "go home-yes, HOME, and not back to the"
3800 PRINT "Trail this year.'":GOTO 4610
3810 REM 
3820 REM Subroutine to examine weight loss
3830 CO=CD-1.03*CL:REM Augment diet by 3% with ice cream, etc.
3840 TS=T-TL:TL=T:REM Time (in days) of this trip segment
3850 WO=CO*TS>3500:WS=WS+WO:IF WO<1 THEN RETURN
3860 PRINT "Since the start of the trip, you have"
3865 PRINT "lost" INT(WS) "pounds."
3870 IF WS>.07*WB THEN 3900:REM Lost more than 7% of orig body weight?
3880 PRINT "Right now you are feeling fit as a"
3885 PRINT "fiddle, but remember, you still have"
3890 PRINT "a long way to go.":GOTO 3970
3900 IF WS>.15*WB THEN 3930:REM Lost more than 15% of orig body weight?
3910 PRINT "You occasionally feel a bit lightheaded"
3915 PRINT "and shaky. You really should eat a bit"
3920 PRINT "more.":GOTO 3970
3930 PRINT "That's far too much weight to lose in"
3935 PRINT "this short period of time."
3940 IF WS>.24*WB THEN HZ(5)=1:DOC=5:GOSUB 3360:GOTO 3970
3950 PRINT "You frequently feel lightheaded,"
3955 PRINT "nauseated, and sluggish. You'd better"
3960 PRINT "add to your diet...and soon!"
3970 WB=WB.WO:CD=INT(WB*11.5+WB*D1*.3+(WB+WP)*D1*.21+WB*(15-TW)*.22)
3980 PRINT "At your current pace, you are burning"
3985 PRINT CD "calories per day."
3990 PRINT:PRINT "How many calories worth of food do you"
3995 INPUT "want to eat";CL$:CL=VAL(CL$)
4000 IF CD-CL>400 THEN PRINT "Okay, suit yourself."
4010 RETURN
4020 REM 
4030 REM Subroutine to deal with a long period of rain
4040 IF RND(1)<.94 THEN HZ(5)=0:RETURN:REM 6% chance of heavy rain
4050 IF HZ(5)=1 THEN HZ(5)=0:RETURN:REM In a dry stretch?
4060 HZ(25)=HZ(25)+1:TM=RAIN*1.7:IF HZ(25)>4 THEN HZ(25)=4
4070 ON HZ(25) GOTO 4080,4130,4150,4180
4080 PRINT "It has been raining steadily for the"
4085 PRINT "past week and you are thoroughly soaked-";
4090 PRINT "your clothes, your sleeping bag, your"
4095 PRINT "food, and, yes, your spirits."
4100 PRINT "What a bummer.":GOSUB 5510
4110 PRINT "If it continues for another week, you're";
4115 PRINT "going to have fungus growing on your"
4120 PRINT "skin.":RETURN
4130 PRINT "Good grief! More rain-torrential,"
4135 PRINT "blustery, miserable rain. This is"
4140 PRINT "really beginning to get you down.":RETURN
4150 PRINT "Would you believe it? It is raining"
4155 PRINT "again. Not the pitter-patter of the"
4160 PRINT "songwriters, but steady, heavy, cold"
4165 PRINT "rain.":GOSUB 5510
4170 PRINT "...and more rain. Won't it ever stop?":RETURN
4180 PRINT "Unbelievable...it is raining again..."
4185 PRINT "and has been for the past week."
4190 IF (RAIN=2 OR RAIN=3) AND D<1900 THEN 4230
4195 RETURN
4200 IF RAIN=4 THEN PRINT "Okay, you resign yourself to wait it"
4205 PRINT "out.":RETURN
4210 PRINT "Even your cheerful attitude toward rain"
4215 PRINT "is taking a beating, but you keep"
4220 PRINT "slogging along, hoping for a letup.":RETURN
4230 GOSUB 5510:PRINT:PRINT "That's it. You can't take any more."
4235 PRINT "Maybe you'll try again next year, but"
4240 PRINT "that's it for now.":GOTO 4610
4250 REM 
4260 REM Subroutine to deal with snow in New England
4270 IF T>200 AND RND(1)>.5 THEN 4290:REM 50% chance of snow after Oct 15
4280 IF RND(1)>.2 THEN RETURN:REM 20% chance of snow
4290 SNO=SNO+1:PRINT "Oh oh, New England is getting some"
4295 PRINT "snow..."
4300 IF SNO=1 THEN PRINT "but you keep pushing on.":RETURN
4310 IF SNO>2 THEN 4350:REM 3 heavy snowfalls and you're out
4320 PRINT "You pushed through the last flurries"
4325 PRINT "but this looks more serious. You say to"
4340 PRINT "yourself, 'I've gone this far, I'm going";
4345 PRINT "to go all the way.' And on you go...":RETURN
4350 PRINT "You made a gallant attempt to get"
4355 PRINT "through, but the Park Rangers won't"
4360 PRINT "let you go on. Too bad.":GOTO 4610
4370 REM 
4380 REM Subroutine to deal with the Kennebec River
4390 KEN=1:PRINT:PRINT "You have arrived at the Kennebec River."
4400 PRINT "Did you make prior arrangements to get"
4405 INPUT "across";A$:GOSUB 5540
4410 IF A$<>"Y" THEN 4450
4415 RN=RND(1):IF RN>.5 THEN 4440
4420 PRINT "Fortunately the person you called showed";
4425 PRINT "up to meet you with a canoe. You get"
4430 PRINT "across in jig time.":TM=.5:GOTO 4540
4440 GOSUB 5510:PRINT "Too bad; the guy you called didn't show"
4445 PRINT "up.":GOTO 4460
4450 PRINT "That wasn't very sensible. What will you";
4455 PRINT "do now?":GOSUB 5510
4460 IF RND(1)>.7 THEN 4520:REM 30% chance you can ford the river
4470 PRINT "The river is running very high and the"
4475 PRINT "logs from the sawmill are very dange-"
4480 PRINT "rous. You'll have to hang around until"
4485 PRINT "another hiker (who, hopefully, has"
4490 PRINT "arranged for a canoe) shows up or hope"
4500 PRINT "that someone comes along.":GOSUB 5510:TM=INT(2+3*RND(1))
4510 PRINT "Finally...you're across, but it cost"
4515 PRINT "you" TM "days.":GOTO 4540
4520 PRINT "Fortunately the river isn't running too"
4525 PRINT "high and you can probably wade across"
4530 PRINT "downstream at the ford. Boy, were you"
4535 PRINT "lucky!":TM=.6
4540 T=T+TM:RETURN
4550 REM 
4560 REM Reached end of trail!
4570 PRINT "You reached the end of the trail at"
4575 PRINT "Baxter Peak on Mt. Katahdin!"
4580 FOR J=1 TO 3:GOSUB 5510:NEXT J:CLS:FOR J=1 TO 10
4590 X$="CONGRATULATIONS!":LOCATE 12,1:GOSUB 5590
4600 FOR I=1 TO 100:NEXT I:CLS:FOR I=1 TO 50:NEXT I:NEXT J:D=2007
4610 TD=INT(T+.5):D=INT(D):X=(INT(.5+10*D>TD))>10:REM End game statistics
4620 PRINT:PRINT "It is now ";:GOSUB 5220:PRINT "and you have been on the"
4630 PRINT "trail for" TD "days. You have covered"
4635 PRINT D;"miles. Your average speed,"
4640 PRINT "considering all the delays, was "
4650 PRINT X;" miles per day."
4660 WB=INT(WB+.5):WL=INT(WS+.5):IF WL>0 THEN X$="less":GOTO 4680
4670 X$="more":WL=-WL
4680 PRINT "You weighed" WB "pounds at the end,"
4685 PRINT WL;X$;" than at the start."
4690 PRINT "Nice going!"
4700 PRINT:INPUT "Would you like to try again (Y or N)"; A$:GOSUB 5540
4710 IF A$="Y" THEN RUN
4715 CLS:PRINT"Bye bye":END
4720 REM 
4730 REM Names of items that can be carried
4740 DATA "Tent"
4750 DATA "Sierra Designs Divine Light, 20 sq ft, max height 34 in.",35,135,214
4760 DATA "Eureka! Crescent Bike, 22 sq ft, height 43 in.",48,125,353
4770 DATA "Moss Starlet, 29 sq ft; with vestibule, 37; height 39 in.",78,250,334
4780 DATA "None. Use trail shelters and sleep in open.",0,0,0
4790 DATA "Pack"
4800 DATA "Kelty Mountaineer external-frame with Seneca pack",69,139,3975
4810 DATA "Jansport D2 external-frame pack",99,169,5520
4820 DATA "Camp Trails Grey Wolf internal-frame pack, large",82,119,5975
4830 DATA "Coleman Peak 1 model 680 internal-frame pack",58,115,4013
4840 DATA "Sleeping bag"
4850 DATA "North Face Blue Kazoo, mummy, goose down rated 25 deg",34,140,452
4860 DATA "Slumberjack Bike Lite, mummy, Quallofil, rated 30 deg",50,65,808
4870 DATA "REI 747 Wide Body, semi-rect, Quallofil, rated 30 deg",54,90,1884
4880 DATA "L.L. Bean Ultra-Lite, rectangular, Quallofil, rated 35 deg",58,80,804
4890 DATA "Sleeping pad/mattress"
4900 DATA "Sevylor Superlight air mattress",32,20,360
4910 DATA "Therm-A-Rest self-inflating ultra-lite pad",28,48,325
4920 DATA "Ensolite 1/2 in. pad",24,23,300
4930 DATA "None.",0,0,0
4940 DATA "Stove"
4950 DATA "MSR Whisperlite, uses white gas (available along trail)",18,37,120
4960 DATA "Gaz Bleuet, fuel: butane cartridge (light and reliable)",16,20,106
4970 DATA "Primus Grasshopper, fuel: propane cylinder (long life)",22,19,90
4980 DATA "None (incidentally, wood fires are PROHIBITED on the trail)",0,0,0
4990 DATA "Boots"
5000 DATA "Asolo Trail II S, mid-ankle leather boots",49,140,0
5010 DATA "Danner Featherlight Trail, mid-ankle leather & Gore-Tex boot",52,95,0
5020 DATA "Timberland Lightweight Hiker, mid-ankle fabric and Gore-Tex",46,50,0
5030 DATA "Raichle Montagnas, full-height leather boots",80,125,0
5040 DATA "Raingear"
5050 DATA "Early Winters Ultralight Gore-Tex rain parka",13,145,25
5060 DATA "Patagonia featherweight Gore-Tex shell",7,58,15
5070 DATA "Campmor nylon poncho",16,25,22
5080 DATA "None.",0,0,0
5090 REM 
5100 FOR I=1 TO 21:READ DLC(I),LC$(I):NEXT I:RETURN
5110 DATA 79,"Bly Gap, GA",165,"Doe Knob, NC",302,"Big Bald Mt., NC"
5120 DATA 384,"Wilbur Lake, TN",483,"Big Walker Lookout, VA",602,"Tinker Mt., VA"
5130 DATA 698,"Salt Log Gap, VA",800,"Fishers Gap, VA",889,"Potomac River, WV"
5140 DATA 966,"Piney Mt., PA",1099,"Baer Rocks, PA",1190,"High Point, NC"
5150 DATA 1272,"Shenandoah Mt., NY",1361,"Sages Ravine, CT"
5160 DATA 1435,"Mt. Greylock, MA",1554,"Killington, VT",1687,"Mt. Washington, NH"
5170 DATA 1776,"Bemis Pond, ME",1855,"Kennebec River, ME"
5180 DATA 1922,"Chairback Mt., ME",1992,"Penobscot West Branch, ME"
5190 REM
5200 REM Subroutine to print the date
5210 IF TT=TD THEN RETURN:REM Printed this date already?
5220 IF TD<31 THEN 5250
5223 IF TD<62 THEN 5260
5226 IF TD<90 THEN 5270
5230 IF TD<121 THEN 5280
5233 IF TD<152 THEN 5290
5236 IF TD<182 THEN 5300
5240 IF TD<213 THEN 5310
5243 IF TD<225 THEN 5320
5426 GOTO 5340
5250 MO$="April":MD=TD:GOTO 5330
5260 MO$="May":MD=TD-30:GOTO 5330
5270 MO$="June":MD=TD-61:GOTO 5330
5280 MO$="July":MD=TD-89:GOTO 5330
5290 MO$="August":MD=TD-120:GOTO 5330
5300 MO$="September":MD=TD-151:GOTO 5330
5310 MO$="October":MD=TD-181:GOTO 5330
5320 MO$="November":MD=TD-212
5330 PRINT MO$;MD;:TT=TD:RETURN
5340 PRINT:PRINT "It's November 12 and all the New England";
5345 PRINT"states are covered with snow. You have"
5450 PRINT"no chance of finishing the trail. Better";
5360 PRINT"luck next year. You have been out on the";
5370 TD=INT(T+.5):D=INT(D):X=(INT(.5+10*D>TD))>10:GOTO 4630
5380 REM
5390 REM Subroutine to temporarily break execution
5400 PRINT:X$="Press any key to continue":GOSUB 5590:A=INKEY(10)
5410 A=INKEY(10):IFA=0THEN5410
5415 PRINT:RETURN
5420 REM 
5430 REM Subroutine to print weights by pounds and ounces
5440 W1=INT(WG/16):WZ=WG-(16*W1)
5450 IF W1>1 THEN PRINT W1;"lb";:GOTO 5470
5460 IF W1=1 THEN PRINT " 1 lb";
5470 IF WZ>1 THEN PRINT WZ;"oz";:RETURN
5480 IF WZ=1 THEN PRINT " 1 oz";
5490 RETURN
5500 REM 
5510 REM Subroutine to make a short pause
5520 PAUSE50:RETURN
5530 REM 
5540 REM Subroutine to extract the first letter of an input answer
5550 IF A$="" THEN A$="Y":RETURN
5560 A$=LEFT$(A$,1):IF A$ >= "A" AND A$ <= "Z" THEN RETURN
5570 A$=CHR$(ASC(A$)-32):RETURN
5580 REM 
5590 REM Subroutine to print centered lines
5600 PRINT TAB((40-LEN(X$))/2);X$;:RETURN
5610 REM 
5620 REM Subroutine to print the instructions
5630 X$="Appalachian Trail":GOSUB 5590:PRINT:PRINT
5640 PRINT" You are a hiker whose goal is to walk"
5650 PRINT"the entire 2007 miles of the Appala-"
5660 PRINT"chian Trail from Springer Mt., GA, to"
5670 PRINT"Mt. Katahdin, Maine. You set out in"
5680 PRINT"April as soon as the Smokies are clear"
5690 PRINT"of snow, and you must reach the northern";
5700 PRINT"terminus before it is blocked by snow."
5710 PRINT" Your hike is divided into three-day"
5720 PRINT"segments. Along the way, you encounter"
5730 PRINT"natural hazards, difficulties with your"
5740 PRINT"equipment, and physical problems."
5750 PRINT" Careful planning for your hike is very"
5760 PRINT"important. In deciding What to pack,"
5770 PRINT"you have to make trade-offs-generally"
5780 PRINT"between weight and comfort. Of course,"
5790 PRINT"everything must fit in your pack."
5800 PRINT" You must decide how you will obtain"
5810 PRINT"food along the route, how much to eat in";
5820 PRINT"each food group, and how many calories"
5830 PRINT"to replenish."
5840 GOSUB 6000:CLS
5850 PRINT" You must decide at what pace you will"
5860 PRINT"walk, and how long to hike each day. Of"
5870 PRINT"course, a faster pace will cover mileage";
5880 PRINT"more quickly than a slower one, but it"
5890 PRINT"is much harder on your body."
5900 PRINT" You don't have many choices when"
5910 PRINT"dealing with mishaps. It is assumed that";
5920 PRINT"you are a sensible hiker, make repairs"
5930 PRINT"when necessary, replace things that wear"
5940 PRINT"out, and see a doctor if you get sick."
5950 PRINT"Nevertheless, mishaps cost you time, of"
5960 PRINT"which you have little to spare as you"
5970 PRINT"take another of the five million steps"
5980 PRINT"towards Maine."
5840 PRINT:X$="Press any key when you're ready to go.":GOSUB 5590:RETURN
6000 PRINT"Press a key to continue..."
6010 A=INKEY(10)
6020 A=INKEY(10):IF A=0 THEN 6020
6030 RETURN
