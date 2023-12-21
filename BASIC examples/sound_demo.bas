10 REM SOUND DEMO
20 SREG 7,&B11101000:REM Enable Ch.A,B,& C + noise
30 SREG 0,100:REM Ch.A tone
40 SREG 2,200:REM Ch.B tone
50 SREG 4,250:REM Ch.C tone
60 SREG 11,0:SREG 12,25:REM Set envelopre frq. for Ch.C
70 SREG 13,12:REM Set envelope mode for Ch.C
80 SREG 8,15:REM Ch.A volume
90 SREG 9,15:REM Ch.B volume
100 SREG 10,16:REM Ch.C volume & envelope mode on
