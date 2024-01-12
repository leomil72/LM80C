8V
 � compactflash demo, www.smbaker.com �V � This was intended to be a basic program that could read and write �V � the compactflash card. Unfortunately, doing a sector write on a fuji W( � card using basic appears to be too slow and aborts.  !W2 � 0:� 8000:� configure QWd �:� "1 - dump drive ID":� "2 - read sector" wWn � "3 - write sector":� "0 - EXIT" �W� � "Selection "; �W� � A$:� A$�"0" � � �W� � A$�"1" � � 8400 �W� � A$�"2" � � 1000 �W� � A$�"3" � � 2000  �W,� 100 �W�� "Sector "; X�� S X�� 8500 X� )X�� "Sector "; 1X�� S @X�� "Data "; IX�� D$ TX�� 9000 ZX� �XN� wait until CF has executed the requested job �XX� (�(I7) � 128)��0 � 7000:� wait for not busy �Xb� �X@� setup constants for IDE port addresses �XJI0�80 YTI1�I0�1 Y^I2�I0�2  YhI3�I0�3 ,YrI4�I0�4 8Y|I5�I0�5 DY�I6�I0�6 PY�I7�I0�7 oY�� 7000:� wait for not busy �Y�� I1,1:� 7000:� 8-bit mode �Y�� I7,239:� 7000:� set features command �Y�� I1,130:� 7000:� no-cache Z�� I7,239:� 7000:� set features command  
Z�� Z � wait-for-ready +Z X��(I7) VZ � (X � 64)�0 � 8210:� wait for ready=1 �Z& � (X � 128)��0 � 8210:� wait for busy=0 �Z0 � �Zl � wait-for-drq �Zv X��(I7) �Z� � (X � 8)�0 � 8310:� wait for drq=1 �Z� � (X � 128)��0 � 8310:� wait for busy=0 [� � [� � read drive status 5[� � 8200:� wait for ready Z[� � I6,224:� 7000:� select master  �[� � I7,236:� 7000:� send drive id command �[� � 8300:� wait for drq �[!CT�512 �[!� 8600 �[!� �[4!� read sector �[>!� 8200:� wait for ready �[C!� I2,1:� 7000 \H!� I3,S�(�(S�256)�256):� 7000 6\R!� I4,�(S�256):� 7000 H\W!� I5,0:� 7000 l\\!� I6,224:� 7000:� select master �\f!� I7,32:� 7000:� send drive read command �\p!� 8300:� wait for drq �\z!CT�512 �\�!� 8600 �\�!� �\�!� dump data from disk to screen ]�!�:� I�0 � (CT�16)�1 3]�!X�I�16:� 8850:� " ";:� print offset >]�!AS$�"" M]�!� J�1 � 16 Y]�!X��(I0) d]�!� 8800 o]�!� " "; �]�!� (X�32) � AS$�AS$��(X):� 8700 �]�!AS$�AS$�"." �]�!� J �]"� AS$ �]"� I �]"� �]`"� print two hex digits ^j"XH$��(X):� �(XH$)�2 � XH$�"0"�XH$ ^t"� XH$;:� ,^�"� print four hex digits 9^�"ZH$��(X) O^�"� �(ZH$)�4 � 8880 y^�"LZ��(ZH$):� HH�1 � 4�LZ:ZH$�"0"�ZH$:� �^�"� ZH$;:�  �^(#� write sector �^2#� 8200:� wait for ready �^7#� I2,1:� 7000 �^<#� I3,S�(�(S�256)�256):� 7000 _F#� I4,�(S�256):� 7000 _K#� I5,0:� 7000 8_P#� I6,224:� 7000:� select master f_Z#� I7,48:� 7000:� send drive write command �_d#� 8300:� wait for drq �_n#� I�1 � �(D$) �_x#� I0,�(�(D$,I,1)):� 7000 �_�#� I �_�#� I�1 � 512��(D$) �_�#� I0,0:� 7000 �_�#� I �_�#�   