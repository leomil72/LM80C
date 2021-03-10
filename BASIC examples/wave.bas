1 rem by Antonino Porcino
2 rem 06/03/2021
10 screen 1:color 1,14,14
20 s=97*8
30 for t=0 to 64
40 vpoke s+t,0
50 next
60 s=s+7
70 for t=0 to 7
80 vpoke s,255
90 s=s+7
100 next
110 print "aabcdefghhgfedcba";
120 goto 110
