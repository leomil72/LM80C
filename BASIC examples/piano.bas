90 print "wait..."
100 dim f(32)
110 for i=0 to 31
120 x=440*2^((i-12)/12)
140 f(i)=4096-(115206.1875/x)
150 next
160 cls
161 print "*** LM80C Piano ***"
162 print
163 print "by Antonino Porcino, 2020"
164 print:print
165 print "  2 3   5 6 7   9 0
166 print " Q W E R T Y U I O P
167 print
168 print "  S D   G H J
169 print " Z X C V B N M
174 volume 1,15
175 a$="zsxdcvgbhnjmq2w3er5t6y7ui9o0p"
180 x=inkey(10):if x=0 then 180
190 x=instr(a$,chr$(x))
200 if x=0 then 180
210 sound 1,f(x),50
220 goto 180
