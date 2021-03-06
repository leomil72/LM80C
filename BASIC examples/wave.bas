3 rem by Antonino Porcino, 06/03/2021
4 screen 1:color 1,14,14
5 s=97*8
10 for t=0 to 64
20 vpoke s+t,0
30 next
40 s=s+7
50 for t=0 to 7
60 vpoke s,255
70 s=s+7
80 next
100 print "aabcdefghhgfedcba";
110 goto 100
