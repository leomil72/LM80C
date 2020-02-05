10  REM *************************
20  REM *      DRAW  TEST       *
30  REM *  BY LEONARDO MILIANI  *
40  REM *                       *
50  REM *   WORKS WITH LM80C    *
60  REM *    COLOR COMPUTER     *
70  REM *   WITH FIRMARE 2.11   *
80  REM *       AND ABOVE       *
90  REM *************************
100 SCREEN 2
110 X1=0:Y1=96
120 FOR X=0 TO 255
130 Y=INT(96+95*SIN(X/16))
140 DRAW X1,Y1,X,Y:X1=X:Y1=Y
150 NEXT
