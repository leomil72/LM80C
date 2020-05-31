2 REM "ASTRONAVE FARMER (C) 1983 MARIO PETTENGHI & MCOMPUTER"
4 REM "CONVERTITO PER LM80C BASIC DA LEONARDO MILIANI"
6 CO=8:VE=11:SOG=20:OGG=10:DIM CO$(8),VE$(11),SOG$(20),OGG(10),OGG$(10),FL(9)
10 DX$=CHR$(13):SCREEN 0:COLOR11,1:VOLUME0,15:GOTO 500
20 INPUT "Cosa devo fare";FR$:X=0:Y=0
22 TT$="":FORJ=1TOLEN(FR$):TC$=MID$(FR$,J,1)
24 IF TC$>="a" AND TC$<="z" THEN TT$=TT$+CHR$(ASC(TC$)-32):GOTO28
26 TT$=TT$+TC$
28 NEXT:FR$=TT$
30 FOR J=1 TO CO:IF INSTR(FR$,CO$(J))<>0 THEN X=J
40 NEXT:ON X GOSUB 100,100,100,100,8000,9000,9500,9700
45 PAUSE200:IF X THEN RETURN
50 FOR J=1 TO VE:IF INSTR(FR$,VE$(J))<>0 THEN X=J
60 NEXT:FOR J=1 TO SOG:IF INSTR(FR$,SOG$(J))<>0 THEN Y=J
70 NEXT:ONXGOSUB1000,1100,1200,1300,1400,1500,1600,1700,1800,1900,1920
75 GOSUB7000:PAUSE200:IFXTHENRETURN
80 SOUND3,4000,5:PRINT DX$;"..saro' stupido ma non ti capisco!";DX$:GOTO 20
82 N=0:E=0:S=0:O=0:SOUND3,4000,5
84 CLS:IF LO>20 THEN ON LO-20 GOSUB 2210,2220,2230,2240,2250,2260:GOTO 94
86 IF LO>10 THEN 90
88 ON LO GOSUB 2000,2010,2020,2030,2040,2050,2060,2070,2080,2090:GOTO94
90 LO=LO-10:ON LO GOSUB 2100,2110,2120,2130,2140,2150,2160,2170,2180,2190,2200
94 PRINT:PRINT "Direzioni: ";:IF N THEN PRINT "NORD ";
96 IF E THEN PRINT "EST ";
97 IF S THEN PRINT "SUD ";
98 IF O THEN PRINT "OVEST ";
99 RETURN                                             
100 IF LO=2 AND X=3 AND FL(1)=0 THEN PRINT "La porta di sicurezza e' bloccata!":RETURN
101 IF LO=10 AND X=2 AND OGG(3) THEN PRINT "Solo gli ufficiali possono entrare!":RETURN
102 IF LO=17 AND FL(6)=0 AND X=4 THEN PRINT "Il portellone non si apre!!!":RETURN
103 IF LO=17 AND X=4 AND FL(4)=0 THEN 106
104 GOTO 108
106 PRINT "Vuoi uscire dalla nave mentre e' in":PRINT"volo? Idiota.":RETURN
108 ON X GOTO 110,120,130,140
110 IF N=0 THEN 150
111 LO=N:RETURN
120 IF E=0 THEN 150
121 LO=E:RETURN
130 IF S=0 THEN 150
131 LO=S:RETURN
140 IF O=0 THEN 150
141 LO=O:RETURN           
150 PRINT "Non puoi andare in quella direzione...":PRINT:RETURN
200 PRINT "Oggetti visibili: ";
210 FL=0:FOR J=1 TO OGG:IF OGG(J)=LO THEN PRINT TAB(19);OGG$(J):FL=1
220 NEXT:IF FL=0 THEN PRINT TAB(19);"NESSUNO"
230 IF LO=8 AND FL(2)=0 THEN GOTO 9100
300 PRINT "--------------------------------------":RETURN
500 FOR J=1 TO CO:READ CO$(J):NEXT
540 FOR J=1 TO VE:READ VE$(J):NEXT
550 FOR J=1 TO SOG:READ SOG$(J):NEXT
560 FOR J=1 TO OGG:READ OGG(J),OGG$(J):NEXT
590 LO=1:CLS
600 PRINT "--------------------------------------":GOSUB 82
610 PRINT :GOSUB 200
620 PRINT :GOSUB 20
630 GOTO 600
1000 IF Y=0 THEN PRINT "Non ci trovi nulla di":PRINT"interessante!":RETURN
1010 IF LO=2 AND Y=10 THEN 1014
1012 GOTO 1020
1014 PRINT "E' una porta di sicurezza. Puoi":PRINT"aprirla con la sua scheda di"
1016 PRINT "riconoscimento.":RETURN
1020 IF LO=4 AND Y=11 THEN 1024
1022 GOTO 1030
1024 PRINT PRINT "E' di metallo, ma lo sportello non":PRINT"ha serrature.":RETURN
1030 IF LO=6 AND Y=13 AND OGG(3)=69 THEN 1034
1032 GOTO 1040
1034 PRINT "Era il comandante C.P.Ustinov.":PRINT"Grand'uomo. Non ha soldi ";
1036 PRINT "in tasca,":PRINT"ma ha una tessera con foto sul petto"
1038 PRINT "e una brutta cera.":OGG(3)=6:RETURN
1040 IF LO=18 AND Y=16 AND FL(3)=0 THEN 1044
1042 GOTO 1050
1044 PRINT "C'e' scritto qualcosa in un vecchio":PRINT"linguaggio. Un appassionato di"
1046 PRINT "computer saprebbe certo decifrarlo...":RETURN
1050 IF LO=18 AND Y=16 AND FL(3) THEN 1054
1052 GOTO 1060
1054 PRINT "Le tue conoscenze di computer ti":PRINT"consentono di leggere il codice di"
1056 PRINT "uscita":FL(6)=1 :RETURN
1060 IF LO=21 AND Y=18 AND OGG(8)<>0 THEN 1064
1062 GOTO 1090
1064 PRINT "E' di ottone. C'e' una torcia su di":PRINT"esso!":OGG(8)=21:RETURN
1090 PRINT "Mi bruciano gli occhi dallo sforzo.":RETURN
1100 FL=0:IF Y=0 THEN  1115
1102 IF LO=6 AND Y=13 THEN PRINT "No. amico, chiedilo a Frankenstein!":RETURN
1103 IF LO=4 AND Y=2 THEN 1105
1104 GOTO 1106
1105 PRINT "No, non vale la pena, non e' nemmemo":PRINT"digitale.":RETURN
1106 IF LO=5 AND Y=3 THEN PRINT "E' bloccato con una catenella!":RETURN
1109 FOR J=1 TO OGG:IF INSTR(OGG$(J),SOG$(Y))<>0 THEN FL=J
1110 NEXT
1115 IF FL=0 OR LO<>OGG(FL) THEN PRINT "Andiamo, cerca di essere serio!!":RETURN
1120 OGG(FL)=0:PRINT "OK, eseguito.":RETURN
1200 FOR J=1 TO OGG:IF SOG$(Y)=OGG$(J) THEN FL=J
1210 NEXT
1220 IF FL=0 OR OGG(FL)<>0 THEN 1224
1222 GOTO 1230
1224 PRINT "Come posso lasciare quello che non ho,":PRINT"totano!":RETURN
1230 OGG(FL)=LO:PRINT "OK... come vuoi tu!":RETURN
1300 IF Y=0 THEN 1390
1310 IF LO=4 AND Y=2 THEN PRINT "E' spento...":RETURN
1320 IF LO=11 AND Y=7 AND FL(5) AND OGG(7)=0 THEN 1324
1322 GOTO 1330
1324 LO=14:PRINT "OK, funziona, ma non c'e' piu' energia"
1326 PRINT "per un secondo utilizzo":RETURN
1330 IF LO=11 AND Y=7 AND FL(5)=0 THEN 1334
1332 GOTO 1340
1334 PRINT "Per farlo devi attivare prima il motore.";:RETURN
1340 IF LO=11 AND Y=7 AND OGG(7) THEN 1344
1342 GOTO 1390
1344 PRINT "E' necessaria la chiave del quadro":PRINT"comandi!":RETURN
1390 PRINT "Prova a usare il cervello, se non e'":PRINT"un optional!":RETURN
1400 IF Y=17 AND OGG(8)=0 THEN OGG(8)=-1:PRINT "OK....":RETURN
1408 IF Y<>2 THEN PRINT "Accenditi la barba, ippogrifo!":RETURN
1410 IF LO=4 AND OGG(6) THEN 1414
1412 GOTO 1420
1414 PRINT "Dovrei avere anche una cassetta per":PRINT"farlo!":RETURN
1420 IF LO<>4 THEN PRINT "Dove lo trovo? Su Audio Review?":RETURN
1440 PRINT "OK.. C'e' un messaggio:"
1441 PRINT "SEI STATO RISVEGLIATO DALL'IBERNAZIONE"
1442 PRINT "PER UNA MISSIONE DI EMERGENZA!"
1443 PRINT "L'ASTRONAVE 'FARMER' HA AVUTO UN'AVARIA."
1444 PRINT "DEVI RIPROGRAMMARE IL COMPUTER DI BORDO"
1445 PRINT "PER UN ATTERRAGGIO DI FORTUNA. PER FARLO";
1446 PRINT "TI SERVE UN FLOPPY DISC CHE CONTIENE I "
1447 PRINT "DATI NECESSARI. TROVALO E INSERISCILO "
1448 PRINT "NEL LETTORE DEL COMPUTER DI BORDO."
1449 PRINT "E' IN GIOCO ANCHE LA TUA VITA. AUGURI."
1471 RETURN
1500 IF LO=13 AND Y=15 THEN 1520
1510 GOTO 1590
1520 PRINT "Le luci vacillano per un istante.":PRINT"Tutto OK.":FL(5)=1:RETURN
1590 PRINT "Forse una petunia ha un Q.I.":PRINT"superiore al tuo...":RETURN
1600 IF Y=0 THEN 1604
1602 GOTO 1610
1604 PRINT "Ora ti apriro' il cranio se continui":PRINT"a dire scemate!":RETURN
1610 IF LO=2 AND OGG(1)=0 AND Y=10 THEN 1614
1612 GOTO 1620
1614 PRINT "La porta si apre lentamente...":FL(1)=1:RETURN
1620 IF LO=4 AND Y=11 THEN PRINT "C'e' una cassetta qui dentro!":OGG(6)=4:RETURN
1690 PRINT "Non ci riesco!!!":RETURN
1700 IF LO=5 AND Y=3 THEN FL(3)=1: PRINT "Ora sai tutto di computer!":RETURN
1710 PRINT "E' meglio un buon libro di":PRINT"fantascienza":RETURN
1800 IF Y<>6 THEN 1830
1810 IF OGG(5)<>0 THEN PRINT "Non mi sembra di averne una...":RETURN
1820 FL(2)=1:PRINT "Non e' proprio la tua taglia, ma":PRINT"puo' andare!":RETURN
1830 PRINT "L'unica cosa che potrei mettere e'"
1835 PRINT "la tua testa in un tritatutto!":RETURN
1900 CLS:RETURN
1920 IF LO<>16 OR Y<>5 THEN 1970
1930 IF OGG(4)<>0THENPRINT "Buona idea! Peccato che non ne abbia":PRINT"uno...":RETURN
1940 PRINT"Complimenti! Il calcolatore e'":PRINT"riprogrammato. Stiamo atterrando!"
1941 PRINT"Ora devi uscire dall'astronave e":PRINT"lanciare l'SOS alla Terra! Fai"
1942 PRINT"presto!":FL(4)=1:RETURN
1970 PRINT "Potrei dire una volgarita'... ":RETURN
2000 PRINT "Ora sono nella mia cabina. La mia":PRINT"capsula criogenica e' spenta."
2002 PRINT "L'arredamento e' ridotto":PRINT"all'essenziale. Posso uscire solo"
2004 PRINT "dalla porta sud."
2005 S=2:RETURN
2010 PRINT "Sono in una stanza dalle pareti di me-"
2011 PRINT"tallo. Ci sono uscite in ogni direzione.";
2012 PRINT"La porta a sud ha una serratura magneti-";
2013 PRINT"ca e sembra molto robusta."
2015 N=1:E=3:S=6:O=4:RETURN
2020 PRINT "Sono in un corridoio del settore ri-"
2022 PRINT "creativo. L'illuminazione e' scarsa, ma"
2023 PRINT"riesco a orientarmi. Nessun segno di"
2024 PRINT "vita, mi sento in pericolo."
2025 N=5:E=24:O=2:RETURN
2030 PRINT "Questa e' la sala riunioni. C'e' un"
2031 PRINT "grande tavolo con molte sedie, un arma-"
2032 PRINT "dio. Sul tavolo vedo un registratore a"
2033 PRINT "cassette, sembra possa funzionare anco-"
2034 PRINT "ra."
2035 E=2:RETURN
2040 PRINT "Sono nella sala di lettura. Ci sono nu-"
2041 PRINT "merosi volumi e in bella vista c'e' una"
2042 PRINT "copia recente di MComputer. Non c'e'"
2044 PRINT "anima viva..."
2045 S=3:RETURN
2050 PRINT "Mi trovo in un lungo corridoio. Per ter-";
2052 PRINT "ra di fronte a me c'e' un cadavere."
2053 PRINT "Riesco a sentire il ronzio dei motori"
2054 PRINT "dell'astronave."
2055 N=2:S=7:RETURN
2060 PRINT "Eccomi in una grande stanza. Ci sono"
2061 PRINT "porte dappertutto. Sono vicino al cuore"
2062 PRINT "della nave. La moquette non e' di mio"
2064 PRINT "gusto ma sopporto stoicamente il fatto."
2065 N=6:E=9:S=10:O=8:RETURN
2070 PRINT "Sono nella cabina del comandante."
2071 PRINT "L'oblo' di fronte a me e' incrinato."
2072 PRINT "La temperatura e' di 70 gradi, le"
2073 PRINT "radiazioni sono mortali."
2075 E=7:RETURN
2080 PRINT "Questa e' l'armeria. Gli scaffali sono"
2081 PRINT "vuoti, siamo in missione pacifica. La"
2082 PRINT "porta a est e' chiusa e non vedo come"
2084 PRINT "aprirla..."
2085 O=7:RETURN
2090 PRINT "A ovest c'e' un corridoio bloccato da un";
2091 PRINT "robot in avaria. A sud vedo lo splendore";
2092 PRINT "della galassia da un oblo' Trinitron!"
2095 E=11:N=7 :RETURN
2100 PRINT "Ecco la sala ufficiali. C'e' un ascen-"
2102 PRINT "sore con le porte spalancate. Un tavolo"
2103 PRINT "pieno di bicchieri (vuoti). Vedo a est"
2104 PRINT "una sala con pannelli di controllo."
2105 E=13:O=10:RETURN
2110 RETURN: REM PARDON!
2120 PRINT "E' la sala controllo traffico. Di fronte";
2121 PRINT "a me c'e' un pulsante con la scritta"
2122 PRINT"'INTERRUTTORE ASCENSORE', mi sembra trop-";
2123 PRINT "po bello..."
2125 O=11:RETURN
2130 PRINT "Vedo l'ascensore, ora inutilizzabile."
2131 PRIN T"Sono in un lungo corridoio. A ovest c'e'"
2132 PRINT"la sala di sbarco, a est la sala del"
2134 PRINT "computer."
2135 E=15:O=17:RETURN
2140 PRINT "Sono nella sala del computer. Vedo i"
2141 PRINT "giganteschi banchi di memoria della"
2142 PRINT "Sinclair, e le tastiere a sfioramento."
2143 PRINT "Sono vicino alla sala controllo."
2145 N=18:E=16:O=14:RETURN
2150 PRINT "La sala controllo. Di fronte a me c'e'"
2151 PRINT "il drive per floppy disc, ma c'e'"
2152 PRINT "anche una telecamera dall'aspetto"
2154 PRINT "sinistro..."
2155 O=15:IF OGG(3)=0 THEN 9110
2156 RETURN
2160 PRINT "Ecco la sala per lo sbarco. A ovest"
2161 PRINT "c'e' il portellone elettronico, a est"
2162 PRINT"un lungo corridoio ben illuminato."
2165 O=19:E=14:RETURN
2170 PRINT "Questa e' la sala controllo sbarco,"
2171 PRINT "c'e' un terminale acceso, un vecchio"
2172 PRINT "Quantum Beef, con un monitor HP a"
2174 PRINT "cristalli tossicodipendenti!"
2175 S=15:RETURN
2180 PRINT "Sono sbarcato. Il cielo e' verde chiaro."
2181 PRINT "Spira un dolce vento. A ovest vedo una"
2182 PRINT "costruzione. A nord una caverna. Un"
2183 PRINT "sentiero porta a sud."
2185 N=20:S=22:E=17:O=21:RETURN
2190 IF OGG(8)=-1 THEN 9200
2191 PRINT "L'interno della caverna e' piu' buio"
2192 PRINT "che in un adventure... I rumori sono"
2193 PRINT "altrettanto ripugnanti. Anima di Scott"
2194 PRINT "Adams, aiutami tu!"
2195 S=19:RETURN
2200 REM
2210 PRINT "Sono all'interno di un tempio alieno."
2211 PRINT "C'e' un grande altare con una statua"
2212 PRINT "di uno scarafaggio a trenta braccia,"
2214 PRINT "c'e' odore di insetticida!"
2215 E=19:S=23:RETURN
2220 PRINT "Sono su una collina. il panorama sotto"
2221 PRINT "di me e' entusiasmante. Vedo villaggi"
2222 PRINT "alieni, carri trainati da strani"
2224 PRINT "animali e la reclame della COCA-COLA."
2225 N=19:RETURN
2230 PRINT "Sono in una grande sala con dipinti"
2231 PRINT "alle pareti, gioielli e armi. Dalla"
2232 PRINT "porta a est scorgo il resto del"
2233 PRINT "villaggio, ma una forza misteriosa"
2234 PRINT "non mi permette di proseguire."
2235 N=21:RETURN
2240 PRINT "Questa e' la sala addestramento colo-"
2241 PRINT "nizzatori. Non riesco a orientarmi. Ci"
2242 PRINT "sono strane attrezzature in ogni punto."
2245 E=25:S=24:N=25:O=24:RETURN
2250 PRINT "Sono sempre nei locali di addestra-"
2251 PRINT "mento. Non conosco quest'area della"
2252 PRINT "nave!"
2255 S=26:N=24:E=24:O=25:RETURN
2260 PRINT "Questa e' la sala addestramento colo-"
2261 PRINT "nizzatori. Non riesco a orientarmi. Ci"
2262 PRINT "sono strane attrezzature in ogni punto."
2265 N=3:S=24:O=25:E=25:RETURN
6990 REM BEEP
7000 SOUND1,3500,5:PAUSE5:RETURN
8000 PRINT :IF LO=1 THEN PRINT "Sei solo all'inizio.":RETURN
8010 IF LO=2 THEN PRINT "Stai in guardia e cerca con pazienza.":RETURN
8020 IF LO<27 AND LO>23 THEN PRINT "Studia i punti....":RETURN
8030 IF LO=17 THEN PRINT "Ci vorrebbe il codice computerizzato...":RETURN
8040 IF LO=18 AND FL(3)=0 THEN PRINT "Se ti fossi preparato prima, magari"
8042 PRINT "leggendo qualcosa di utile...":RETURN
8050 IF LO=18 THEN PRINT "Eppure qualcosa ci deve essere...":RETURN
8060 IF LO=21 THEN PRINT "Insisti, vedo qualcosa...":RETURN
8070 IF LO=20 THEN PRINT "Se ci fosse meno buio...":RETURN
8900 IF RND(0)>.5 THEN PRINT "Sono nei guai come te.":PRINT:RETURN
8901 PRINT "Mi hai scambiato per l'hobbit?":PRINT:RETURN
9000 CLS:PRINT"INVENTARIO:":PRINT "In questo momento ho con me:";DX$:FL=0
9010 FOR J=1 TO OGG:IF OGG(J)=0 THEN PRINT OGG$(J):FL=1
9020 NEXT:IF FL=0 THEN PRINT "Un bel niente..."
9030 GOSUB 9800:RETURN
9100 PRINT :IF LO=8 THEN PRINT "La mia pelle sta prendendo fuoco. Ho"
9102 PRINT "pochi secondi di vita e non mi sto"
9104 PRINT "divertento per niente. AAAARGH!":GOTO 9150
9110 PRINT DX$;DX$;"*** ATTENZIONE ****":PRINT"IDENTIFICAZIONE NEGATIVA."
9112 PRINT "Il tuo tracciato metabolico non"
9114 PRINT "corrisponde al campione.":PRINT "NON SEI IL COMANDANTE USTINOV!"
9116 PRINT DX$;"Un raggio laser mi trapassa da parte a"
9118 PRINT "parte, con poca discrezione. MUOIO!":GOTO 9150
9150 PRINT DX$;"Hai fallito miseramente.":PRINT"Vuoi ritentare? (S/N)"
9160 AA=INKEY(0):IFAA=0THEN 9160
9170 IF AA=83 OR AA=115 THEN RUN
9171 IF AA=78 OR AA=110 THEN CLS:PRINT"Addio...":CLR:END
9180 GOTO 9160
9200 SOUND3,4000,5:PRINT DX$;DX$:PRINT "Hai terminato questa avventura...";DX$
9205 PRINT "Di fronte a me c'e' una lastra di pietra";
9206 PRINT "alta venti metri. Reca una scritta:";DX$
9210 READ X:IF X=255 THEN 9230
9220 PRINT CHR$(X-OGG(8)-LO);:GOTO 9210
9230 PRINT:PRINT DX$;"***COMPLIMENTI***";DX$:END
9500 SOUND3,4000,5:CLS:LOCATE3,1
9502 PRINTCHR$(135);:FORI=1TO32:PRINTCHR$(143);:NEXT:PRINTCHR$(136)
9504 LOCATE3,2:PRINTCHR$(144);" ASTRONAVE FARMER (AVVENTURA 1) ";CHR$(144)
9506 LOCATE3,3:PRINTCHR$(138);:FORI=1TO32:PRINTCHR$(143);:NEXT:PRINTCHR$(137)
9510 LOCATE8,6:PRINT "(C) 1984 MARIO PETTENGHI"
9512 LOCATE7,7:PRINT "CONVERTITO PER LM80C BASIC"
9514 LOCATE7,8:PRINT "DA LEONARDO MILIANI (2020)"
9520 LOCATE0,10:PRINT "Comandi riconosciuti:":TX=0:TY=12
9525 FOR J=1 TO VE:LOCATETX,TY:PRINTVE$(J);:TX=TX+13
9530 IF TX>26 THEN TX=0:TY=TY+1
9535 NEXT:FOR J=1 TO CO:LOCATETX,TY:PRINT CO$(J):TX=TX+13
9540 IF TX>26 THEN TX=0:TY=TY+1
9545 NEXT
9550 GOSUB 9800
9560 CLS:RETURN
9700 PRINT DX$;"*** Sei sicuro di voler terminare? ***":AA=INKEY(0)
9710 AA=INKEY(10):IF AA=0 THEN 9710
9720 IF AA=78 OR AA=110 THEN PRINT DX$;"Hai bisogno di una lunga vacanza...":RETURN
9730 IF AA=83 OR AA=115 THEN CLS:END
9740 GOTO 9710
9800 PRINT DX$;DX$;">Premi un tasto per riprendere il gioco":AA=INKEY(0)
9810 AA=INKEY(10):IF AA=0 THEN 9810
9820 RETURN
10000 DATA NORD,EST,SUD,OVEST,AIUTO,INVENTARIO,VOCABOLARIO,FINE
10010 DATA ESAMINA,PRENDI,LASCIA,USA,ACCENDI,PREMI
10015 DATA APRI,LEGGI,METTI,GUARDA,INSERISCI
10020 DATA SCHEDA,REGISTRATORE,MC,TESSERA,FLOPPY DISC,TUTA
10025 DATA ASCENSORE,CODICE,PORTELLO
10030 DATA PORTA,ARMADIO,CASSETTA,CADAVERE,CHIAVE,PULSANTE
10035 DATA MONITOR,TORCIA,ALTARE,LASER,PORTACHIAVI
10040 DATA 4,SCHEDA,5,MC,69,TESSERA,8,FLOPPY DISC
10045 DATA 9,TUTA,32,CASSETTA,25,CHIAVE,29,TORCIA,4,LASER,7,PORTACHIAVI
10050 DATA 89,92,97,88,51,87,88,95,95,84,51,99
10055 DATA 101,92,96,84,51,99,84,101,103,88,65,51,97
10060 DATA 98,97,51,99,88,101,87,88,101,88,51,58,92
10065 DATA 95,51,103,88,96,99,92,98,51,84,95,92
10070 DATA 88,97,98,58,52,65,65,65,255
