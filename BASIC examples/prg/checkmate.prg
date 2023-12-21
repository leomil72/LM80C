1V
 � *************************** SV � *        CHECK MATE       * uV � *                         * �V( � * Originally published as * �V2 � *     "Chess Program"     * �V< � *  by Dieter Steinwender  * �VF � *  Ported to LM80C BASIC  * WP � *   by Leonardo Miliani   * AWZ � *************************** dWd �:� 30000:� Show splash screen �Wn � B(119),S(10,4),M(10),A$(10),U(10) �Wx � R1(4),R2(4),R3(4),R4(4),R5(4),R6(4),R7(4),G1(10) �W� � W(10),P(10),O(15),OA(6),OE(6),L(6),Z(200,6) X� � ZT(9,8),BV(8),BL(2,9),TL(2,9) 7X� � T7(2),BA(2),KR(2),KL(2) AX� � 350 VX� � Initialisation fX� I�0 � 119 sXB(I)�100 {X"� I �X,� I�9 � 2 � �1 �X6� J�1 � 8 �X@� B(I�10�J) �XJ� J �XT� I �XV� Initial displacement of pieces on board YW� 1=pawn/2=rook/3=bishop/4=knight/5=queen/6=king OYX� Pos. numbers=whites / Neg. numbers=blacks mY^� -2,-4,-3,-5,-6,-3,-4,-2 �Yh� -1,-1,-1,-1,-1,-1,-1,-1 �Yr� 0,0,0,0,0,0,0,0 �Y|� 0,0,0,0,0,0,0,0 �Y�� 0,0,0,0,0,0,0,0 �Y�� 0,0,0,0,0,0,0,0 �Y�� 1,1,1,1,1,1,1,1 Z�� 2,4,3,5,6,3,4,2 Z�F�1 %Z�� I�1 � 4 2Z�S(0,I)�1 :Z�� I GZ�S(0,0)�0 RZ�M(0)�0 `Z�� I�0 � 6 wZ� A$(I),U(I),CH(I) Z� I �Z� FIRST ONE IS 'NO PIECE' �Z� "_",0,228,"P",100,148,"R",500,128,"B",350,136 �Z&� "N",325,132,"Q",900,144,"K",20000,140 
[0M0�48100 [�� I�0 � 15 $[�� O(I) ,[�� I d[�� -9,-11,9,11,-1,10,1,-10,19,21,12,-8,-19,-21,-12,8 r[�� I�1 � 6 �[�� OA(I),OE(I),L(I) �[�� I �[�� 0,3,0,4,7,1,0,3,1,8,15,0,0,7,1,0,7,0 �[�� I�1 � 4 �[�� R1(I),R2(I),R3(I),R4(I),R5(I),R6(I),R7(I) \� I \� 96,97,95,97,95,97,98 8\� 92,94,93,95,95,93,91 S\ � 26,27,25,27,25,27,28 n\*� 22,24,23,25,25,23,21 |\H� J�1 � 8 �\R� I�2 � 9 �\\ZT(I,J)�12�4�(�(5.5�I)��(4.5�J)) �\f� I �\p� BV(J) �\z� J �\�� 0,0,4,6,7,2,0,0 �\�MZ�0 �\�G1(0)�1 ]T�0:HN�0:MV�0 4]�:� 31000:� 4000:� First print on video F]� TR�0 � 3500 Z]�� Player's move �]��0,22:��31);:�0,22:� "  Your move      ";:� 13000 �]�� 17,22:� �12); �]�� E$��"" � 2050 �]�� 32000:� 1810 �]� E$��"R" � 2070 �]� 200:� Restart 	^� E$��"Q" � 2120 ^ � 15000:� Quit 1^H� E$��"C" � 2150 n^R� 5000:� Set a specific configuration of pieces on board y^\� 2000 �^f� E$��"X" � 2170 �^pTR�TR � 1:� 31000:� 3500:� computer gets the current side �^z� E$��"M" � 2210 _�� 2,23:� "Multiple move: "; 1_�� MZ�0 � MZ�1:� 17,23:� "Beginning";:� 2000 X_�MZ�0:� 17,23:� "End      ";:� 2000 m_�� E$��"H" � 2260 |_�Z0�Z1:HN�1 �_��:� " Calculating moves...":�:� 6500:� Show legal moves �_�Z1�Z0:HN�0 �_�� 13500:� 1810 �_�� E$��"B" � 2330  `�� T�1 � 2300 &`�� 0,23:� "  Sorry, not possible"; 1`�� 2000 ]`�� 9600:� Roll back move AND CHANGE SIDE r`	� 0,23:� "  OK"; }`	� 2000 �`	� E$��"D" � 3000 �`$	� 0,23:� "  Analysing depth=";T0; �`.	� T0 �`8	T0��(T0) �`=	� 0,23:� �31); �`B	� 2000 �`�� a�� �(E$)�4 � 2000 &a�� 17,22:� ": checking..."; Ka�V1��(E$)�64�10�(�(�(E$,2,1))�47) wa�N1��(�(E$,3,1))�64�10�(�(�(E$,4,1))�47) �a�� 8800 �a�� 7000 �a�� Z1�1 � G�1 �a�� Z(Z1,1)��V1 � 3090 �a� Z(Z1,2)�N1 � 3120 �a� Z1 b� 0,22:� "          Illegal move          ";:�100 b&� 2000 0b0� Z(Z1,4)�0 � 3170 LbD� �(E$,1)�"N" � Z1�Z1�1 hbN� �(E$,1)�"B" � Z1�Z1�2 �bX� �(E$,1)�"R" � Z1�Z1�3 �bb� 0,22:� "  Your move      ="; �bl� 6000:� 20,9�(5�TR):� A$; �bqMV�MV�1:� 25,3:� MV �bv� 9000 �b�� 7000 c�� MT�0 � 3300 c�� 9600 c�� 3100 7c�� MZ�1 � � 4000:� 2000 Mc�� Computer's move Xc�� 4000 �c�� 0,22:��31);:� 0,22:�"  Evaluating     :"; �c�� 8800 �c�� 10000 �c�� Z2�0 � 3650 �c�� W�1 � 3660 �c�� W��32766 � 3630 �c�Z1�Z2 d�� 0,22:� "  My move        = "; &d�� 6000:� 20,14�(5�TR):� A$; >dMV�MV�1:� 25,3:� MV Id� 9000 [d� W��2 � 3660 pd� W�32765 � 3670 �d� 0,22:� "  CHECK MATE!"; �d$� 3670 �d.� 0,22:� "  Damn, you won!"; �d8� 3670 �dB� T0�0 � 3670 �dL� 0,22:� "  Stalemate: draw!"; eV� 0,23:� �31); =eX� 0,23:� "  Val=";W;"Pos. comp.=";C1;:� 200 Ve[� 4000:� 0,23:��31); ae`� 2000 �e�� Displaying board and pieces �e�X�2:Y�0 �e�� I�9 � 2 � �1:CL�(I�1) � 1 �e�� J�1 � 8:A1�B(I�10�J):F1��(A1)�1:A1��(A1) �e�CR�CH(A1):� CR��228 � 4080 
f�CR�CR�CL +f�� X�(J�1)�2,Y:� �(CR);�(CR); Uf�� X�(J�1)�2,Y�1:� �(CR);�(CR);:� 4100 �f�CR�CR�(F1�24)�(CL�24):� X�(J�1)�2,Y:� �(CR�2);�(CR�3); �f�� X�(J�1)�2,Y�1:� �(CR);�(CR�1); �fCL�CL � 1:� J �fY�Y�2 �f� I �fJ� Print Game statistics  gT� 0,18  g^� "  Material value =";M(T) Dgh� "  En passant sqr.= ";:� 6700 {gr� "  Castle status  =";S(T,1);S(T,2);S(T,3);S(T,4) �g|� "  Now move       = "; �g�� F�1 � � "White":� 4250 �g�� "Black" �g�� �g�� Input move �g�T�0 h�� 0,22:� "Empty the board (Y/N)"; h�� 13000 (h�� E$�"N" � 5130 =h�� E$��"Y" � 5030 Kh�� I�2 � 9 Yh�� J�1 � 8 ih�B(I�10�J)�0 qh�� J yh�� I �h M(0)�0 �h
� "White "; �hF�1 �h� 5500 �h(� "Black "; �h2F��1 �h<� 5500 �hF� "Side to move (W/B) "; �hP� 13000 iZ� E$�"B" � 5240 id� E$��"W" � 5190 %inF�1 Lix� "Change E.P./Castle sts. (Y/N)"; Xi�� 13000 li�� E$�"N" � 5320 �i�� E$��"Y" � 5240 �i�� "E.P. square (0 to reset)";:� 13000 �i�� E$�"0" � S(0,0)�0:� 5300 �i�S(0,0)�F�15��(E$)�9 j�� "Castle status (N,N,N,N)"; %j�� S(0,1),S(0,2),S(0,3),S(0,4) 0j�� 4000 6j�� ^j|� Setting up the pieces on one side �j�� Input is: Xxy - X is piece (R/N/B/Q/K/P), x is A/H, Y is 1/8 �j�� to finish, enter "." �j�� 13000 �j�� E$�"." � 5660 �j�� �(E$)��3 � 5590  k�� I�0 � 6 !k�� �(E$,1)�A$(I) � A�I:� 5600 )k�� I Ck�� "Wrong data":� 5520 Ik�� tk�N��(�(E$,2,1))�64�10�(�(�(E$,3,1))�47) �k�M(0)�M(0)��(B(N))�U(�(B(N))) �k�B(N)�F�A �kM(0)�M(0)�F�U(A) �k� 5520 �k� �kp� Show move �k�A$�A$(�(B(Z(Z1,1)))) �k�� J�1 � 2 l�RE��(Z(Z1,J)�10) )l�LI�Z(Z1,J)�10�RE El�A$�A$��(64�LI)��(47�RE) Ml�� J dl�� Z(Z1,4)�0 � 6100 ~l�A$�A$�"="�A$(Z(Z1,4)) �l�� HN�0 � � 19,22:� A$;�7);:� 6130 �l�� " ";A$;:HN�HN�1 �l�� HN�5 � HN�1:� �l�� �ld� Show legal moves �lx� 7000 m�� MT�0 � 6560 $m�� "  King may be taken" *m�� Pm�� G�G1(T);" Pseudo-legal moves":� em�� Z1�G1(T) � G�1 pm�� 6000 ym�� Z1 m�� �m,� Show an e.p. square �m6� S(T,0)�0 � A$�"0 ":� 6750 �m@RE��(S(T,0)�10) �mJLI�S(T,0)�10�RE �mTA$��(64�LI)��(47�RE) n^� A$ 	nh� nX� Move generation (nlMT�0 4nvG�G1(T) Dn�� V�21 � 98 On�A�B(V) bn�� A�100 � 7490 wn�� �(A)��F � 7490 �n�A��(A) �n�� A��1 � 7360 �n�N�V�(F�10) �n�� B(N)��0 � 7210 �n�� 7900 �n�� (�(V�10)�5.5)�F���2.5 � 7210 �n�N�V�(F�20) 
o� B(N)��0 � 7210 o� 8500 +oZ(G�1,6)�(V�N)�.5 1o*� ?o4� I�1 � 2 No>N�V�O(F�I) doH� B(N)�100 � 7330 zoR� N�S(T,0) � 7300 �o\� �(B(N))���F � 7330 �of� B(N)��F�6 � MT�1:� 7810 �op� 7900 �oz� 7330 �o�� 8500 �o�Z(G�1,3)�1 �o�Z(G�1,6)�N�10�F �o�� I p�� 7490 p�� $p�� I�OA(A) � OE(A) 0p�LA�L(A) 8p�N�V Ep�N�N�O(I) [p�� B(N)�100 � 7480 rp�� �(B(N))�F � 7480 �p� B(N)�0 � 7460 �p� B(N)��F�6 � MT�1:� 7810 �pLA�0 �p$� 8500 �p.� LA�1 � 7400 �p8� I �pB� V �p`� I�F�2 � F�3 qj� S(T,I)�0 � 7790 qt� J�R1(I) � R2(I) -q~� B(J)��0 � 7790 5q�� J Kq�� J�R3(I) � R4(I) Yq�� K�0 � 7 aq�N�J nq�N�N�O(K) �q�� B(N)�100 � 7700 �q�� �(B(N))�F � 7700 �q�� B(N)�0 � 7610 �q�� OA(�(B(N)))�K � 7700 �q�� OA(�(B(N)))�K � 7700 �q�� N�J�O(K) � 7790 r � L(�(B(N)))��1 � 7700 !r
� 7790 )r� K 8r� K�8 � 15 Tr(� B(J�O(K))��F�4 � 7790 \r2� K dr<� J prFV�R5(I) |rPN�R6(I) �rZ� 8500 �rdZ(G�1,5)�I �rn� I �rxG1(T�1)�G �r�� �r�� Note the moves of pawn �r�� (�(N�10)�5.5)�F��3.5 � � 8500:� 7980 s� I2�5 � 2 � �1 s� 8500 )sZ(G�1,4)�I2 2s"� I2 8s,� ns4!� Place the generated moves into the moves' stack {sH!Z(G,1)�V �sR!Z(G,2)�N �s\!Z(G,3)��(B(N)) �sf!� I1�4 � 6 �sp!Z(G,I1)�0 �sz!� I1 �s�!� G�200 � 8600 �s�!G�G�1 �s�!� t`"� Tree moves initialisation tt"� T�0 � 8880 #t~"� I�0 � 4 5t�"S(0,I)�S(1,I) =t�"� I Kt�"M(0)�M(1) St�"T�0 Yt�"� kt(#� Make a move ut<#T�T�1 �tF#S(T,0)�0 �tP#� I�1 � 4 �tZ#S(T,I)�S(T�1,I) �td#� I �tn#M(T)�M(T�1) �tx#V�Z(Z1,1) �t�#N�Z(Z1,2) �t�#� Z(Z1,6)�0 � 9160 u�#� Z(Z1,3)�0 � 9140 u�#B(Z(Z1,6))�0 "u�#� 9430 5u�#S(T,0)�Z(Z1,6) @u�#� 9430 Fu�#� ^u�#� V��R5(F�2) � 9210 mu�#S(T,F�2)�0 |u�#S(T,F�3)�0 �u�#� 9360 �u�#� V��R7(F�2) � 9240 �u$S(T,F�2)�0 �u$� 9260 �u$� V��R7(F�3) � 9260 �u"$S(T,F�3)�0 �u,$� N��R7(�F�2) � 9290 	v6$S(T,�F�2)�0 v@$� 9310 -vJ$� N��R7(�F�3) � 9310 =vT$S(T,�F�3)�0 Cv^$� Zvh$� Z(Z1,4)�0 � 9430 mvr$B(V)�Z(Z1,4)�F �v|$M(T)�M(T)�F�(U(Z(Z1,4))�100) �v�$� 9430 �v�$� �v�$RO�Z(Z1,5) �v�$� RO�0 � 9430 �v�$VO�R7(RO) �v�$B(VO)�0 �v�$NA�(R5(RO)�R6(RO))�.5 w�$B(NA)�2�F w�$� w�$B(N)�B(V) !w�$B(V)�0 <w�$M(T)�M(T)�F�U(Z(Z1,3)) Ew�$F��F Kw%� aw�%� Take back moves jw�%F��F xw�%V�Z(Z1,1) �w�%N�Z(Z1,2) �w�%� Z(Z1,6)�0 � 9710 �w�%� Z(Z1,3)�0 � 9820 �w�%B(Z(Z1,6))��F �w�%B(V)�F �w�%B(N)�0 �w�%� 9840 �w�%� �w�%RO�Z(Z1,5) x&� RO�0 � 9790 (x&NA�(R5(RO)�R6(RO))�.5 4x&B(NA)�0 Bx &VO�R7(RO) Px*&B(VO)�2�F [x4&� 9820 ax>&� xxH&� Z(Z1,4)�0 � 9820 �xR&B(N)�F �x\&B(V)�B(N) �xf&B(N)��F�Z(Z1,3) �xp&T�T�1 �xz&� �x'� Alpha-beta search �x$'Z2�0 �x.'C1�0 �x8'W(0)��32767 �xB'W(1)��32767 yV'� T�T0 � 10110 y`'� 12000 -yj'W(T�2)�W�F 9yt'� 10380 Dy~'� 7000 Wy�'� MT�0 � 10150 jy�'W(T�2)�32767�T vy�'� 10380 �y�'� G�G1(T) � 10190 �y�'W(T�2)�0 �y�'� 10380 �y�'� �y�'P(T)�G1(T) �y�'W(T�2)�W(T) �y�'Z1�P(T) �y�'� T��0 � 10250 �y (� 6000 �y
(� 9000 z(C1�C1�1 z(� 10070 5z2(� �W(T�3)��W(T�2) � 10350 Hz<(W(T�2)��W(T�3) ZzF(� T�0 � 10340 fzP(Z2�P(T) yzU(� 23,23:� �7); �zZ(� 0,23:� "  Best move ";A$;": Val=";W(2); �zd(� W(T�2)���W(T�1) � 10380 �zn(P(T)�P(T)�1 �zx(� P(T)�G1(T�1) � 10220 {�(� W(T�2)���32766�T � 10380 {�(F��F #{�(� 7000 ,{�(F��F ?{�(� MT�1 � 10380 N{�(W(T�2)�1�T `{�(� T�0 � 10430 n{�(Z1�P(T�1) y{�(� 9600 �{�(� 10290 �{�(W�W(2) �{�(� �{�.� Eval function �{�.� W=0:RETURN:REM:*** uncomment for zero eval function �{�.M�0 �{/W�0 |/� I�0 � 2 |/T7(I)�0 |&/BA(I)�0 (|0/� J�0 � 9 6|:/BL(I,J)�0 D|D/TL(I,J)�0 L|N/� J T|X/� I b|l/� I�2 � 9 p|v/� J�1 � 8 }|�/V�I�10�J �|�/A��(B(V)) �|�/� A�0 � 12420 �|�/FA��(B(V)) �|�/M�M�U(A) �|�/� A � 12220,12270,12320,12360,12420,12390 �|�/� }�/BA(FA�1)�BA(FA�1)�1 !}�/BL(FA�1,J)�BL(FA�1,J)�1 C}�/W�W�FA�BV(J)�(3.5�FA�(5.5�I)) O}�/� 12420 U}�/� s}�/� (I�5.5)�FA��2.5 � 12300 �}0T7(FA�1)�T7(FA�1)�1 �}0TL(FA�1,J)�TL(FA�1,J)�1 �}0� 12420 �} 0� �}*0� (I�5.5)�FA���3.5 � 12420 �}40W�W�FA�10 �}>0� 12420 �}H0� ~R0W�W�FA�ZT(I,J) ~\0� 12420 ~f0� ,~p0KR(FA�1)�I ;~z0KL(FA�1)�J C~�0� J K~�0� I Q~�0� `~�0FA��(M(T)) s~�0� FA�0 � 12500 �~�0W�W�M(T)��(M(T)�BA(FA�1)�(BA(FA�1)�1)�(M0�M)�.0001) �~�0� �~�0W�W��(ZT(KR(2),KL(2))�(43000�M�M(T))�.001) �0W�W��(ZT(KR(0),KL(0))�(43000�M�M(T))�.001) &1W�W�T7(2)�T7(2)�12 =1W�W�T7(0)�T7(0)�12 K.1� I�1 � 8 Y81� J�0 � 2 dB1FA�J�1 wL1� FA�0 � 12880 �V1� BL(J,I)�0 � 12830 �`1W�W�FA�(BL(J,I)�1)�8 �j1IIS�0 �t1� BL(J,I�1)�0 � 12710 �~1� BL(J,I�1)�0 � 12710 ��1W�W�FA�20 ��1IIS�1 ��1� BL(2�J,I)�0 � 12880 9��1W�W�FA�TL(2�J,I)�TL(2�J,I)�3 U��1� BL(2�J,I�1)�0 � 12790 q��1� BL(2�J,I�1)�0 � 12790 ��1W�W�FA�18 ���1� 12890 ���1� �� 2� IIS�0 � 12890 ��
2W�W�FA�10 ��2� 12890 ـ2� BL(2�J,I)�0 � 12880 �22W�W�TL(2,I)�TL(2,I)�8 �<2W�W�TL(0,I)�TL(0,I)�8 �F2� 12890 !�P2� J )�Z2� I /�d2� d��2� Get an input and change it to upper cases only ���2E$�"":� Z9$:� Z9$�"" � � ���2� I9�1 � �(Z9$) ���2A9��(�(Z9$,I9,1)) ́�2� A9��97 � A9��122 � A9�A9�32 ݁�2E$�E$��(A9) �3�:� ��4� Wait for a key to continue 3��4�:� " Press any key to continue":A��(10) M��4A��(50):� A�0 � 13510 S��4� {�0u� Display instructions and settings ��:u� 1:� 15,2,12:� 40000:�I�4 � 6:� 4,I ÂDu� J�1 � 24:� A:� �(A);:�:� �Nu� 6,8:� "by  Leonardo Miliani" �Xu�:� " Enter a move using the format" 5�bu� " 'lnLN': 'ln' is the starting" \�lu� " square and 'LN' is the target" ��vu� " while 'n/N' are numbers (1-8)." ���u� " Example: C2C3 moves the piece" ���u� " in C2 to C3." ꃔu�:� " Just press RETURN to get help" ��u� " during the game." ��u� 13500 =��u�:� 0,2:D9�1:� "Difficult (1-8, def.1)"; i��u� D9$:� �(D9$)�0 � �(D9$)�9 � D9��(D9$) ���uT0�D9:� "Difficult:";T0 ���u� 0,5:� "Choose color to play with, white"; ؄�u� "moves first (def.). (W/B)"; ��u� 13000:� E$�"" � E$�"W" � TR�1:� 30220:� Player chose white >��u� E$��"B"� � 0,8:� �10);:� 30170 \�vTR�0:� Player chose black ��v� "You'll start with ";:� TR�0 � � "BLACK":� 30270 ��v� "WHITE" �� v� 10,13:� "Please  wait" �*v� 6,15: � "Initializing game..."; �4v� Load graphic chars into VRAM �>v� 40070 =�Hv� I�0 � 191:� X:� 1024�I,X:� 1216�I,X Y�Rv� 1408�I,X:� 1600�I,X:� ��\v� I�0 � 15:� X:� 1824�I,X:�:� EMPTY SQUARES ��fv� I�8208 � 8210:� I,&H15:� ǆpv� I�8211 � 8213:� I,&H14:� �zv� I�8214 � 8216:� I,&HF5:� ��v� I�8217 � 8219:� I,&HF4:� ��v� 8220,&H45 ��v� >��v� 128/151=WHITES ON LIGHT BLUE `��v� 152/175=WHITES ON DARK BLUE ���v� 128/152=ROOK-132/156=KNIGHT-136/162=BISHOP ���v� 140/166=QUEEN-144/170=KING-148/174=PAWN ۇ�v� EACH PIECE IS 4 CHARS ���v� 176/199=BLACKS ON LIGHT BLUE  ��v� 200/223=BLACKS ON DARK BLUE Q��v� 176/200=ROOK-180/204=KNIGHT-184/208=BISHOP ��v� 188/216=QUEEN-192/220=KING-196/224=PAWN ���v� EACH PIECE IS 4 CHARS Ɉw� EMPTY SQUARE IS 228/229 FOR WHITE/BLACK ۈy� Display GUI �yY�0:� I�8 � 1 � �1:� 0,Y:� �(I);:Y�Y�2:� -�"y� 2,Y:� I�1 � 8:� �(64�I);" ";:� H�,y� 20,1:� "CHECK MATE"; a�6y� 20,3:� "Mv. :";MV; z�@y� 20,4:� "Dft.:";T0; ��Jy� 20,7:� "BLACK: "; ��Ty� TR�0 � � "You":� 31080 ��^y� "CPU" Չhy� 20,8:� "Last move"; �ry� 20,12:� "WHITE: "; �|y� TR�1 � � "You":� 31120 ��y� "CPU" 2��y� 20,13:� "Last move"; 8��y� H� }� Show help `�
}�:� "INSTRUCTIONS:" ��}� "To enter a move, just insert co-"; ��}� "ordinates of start and end: C2C3"; ي(}� "moves a piece from C2 to C3." ��2}�:� "Q: quit the program" �<}� "R: restart the game" 8�F}� "C: let place pieces on board" \�P}� "   to make a specific setup" ��Z}� "X: the player and CPU exchange" ��d}� "   their sides, then CPU moves" Ћn}� "M: insert multiply moves, for" ��x}� "   player & CPU, alternatively,"  ��}� "   another 'M' before last move" D��}� "H: show hints (legal moves)" b��}� "B: roll back one move" ���}� "D: set difficult (def. 1)" ���}� 13500 ���}� ��6�� FOR SPLASH SCREEN �@�� 217,143,143,143,143,143,143,143,143,143,143,143 �J�� 143,143,143,143,143,143,143,143,143,143,143,218 N�T�� 144,32,67,32,72,32,69,32,67,32,75,32,32,32,32 p�^�� 77,32,65,32,84,32,69,32,144 ��h�� 220,143,143,143,143,143,143,143,143,143,143,143 ܍r�� 143,143,143,143,143,143,143,143,143,143,143,219 �|�� ROOK ���� 15,10,10,15,24,63,63,0 %���� 240,16,16,240,24,252,252,0 @���� 0,25,25,25,31,31,8,8 b���� 0,152,152,152,248,248,80,80 o���� KNIGHT ����� 115,99,103,15,24,63,63,0 ��� 176,176,176,240,24,252,252,0 Ў̜� 0,10,10,31,55,54,127,121 ��֜� 0,0,0,128,192,224,112,176 ����� BISHOP �꜃ 30,14,15,7,4,31,60,0 :���� 120,112,240,224,32,248,60,0 T���� 0,1,3,7,14,28,28,30 v��� 0,128,192,224,112,56,56,120 ���� KING ���� 49,63,31,8,15,8,15,0 ��&�� 140,252,248,16,240,16,240,0 ׏0�� 0,1,3,3,1,13,31,49 ��:�� 0,128,192,192,128,176,248,140 �D�� QUEEN "�N�� 54,63,31,8,15,8,15,0 D�X�� 108,248,240,16,240,16,240,0 c�b�� 0,12,12,100,100,38,54,54 ��l�� 0,48,48,38,38,100,108,108 ��v�� PAWN ����� 3,7,3,3,7,31,31,0 ʐ��� 192,224,192,192,224,248,248,0 ����� 0,0,0,3,7,7,3,1  ���� 0,0,0,192,224,224,192,128 ���� BLANK SQUARE 6���� 0,0,0,0,0,0,0,0:� LIGHT BLUE h���� 255,255,255,255,255,255,255,255:� DARK BLUE   