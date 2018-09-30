1     ! ROBCO TermLink Emulator for the HP9816
2     ! By Travis Rosenbaum (Twitter/GH: @Zagrophyte)
3     ! Now you, too, can connect to the ROBCO TermLink from
4     ! your very own HP9816 Terminal!
5     !
10    RANDOMIZE
20    DIM Console$(0:12)[25]
30    DIM Password$[25]
40    DIM Words$(0:12)[25]
50    !--------------------Word Lists-----------------------!
60    DATA "TIDINGS","PININGS","APPROVE","FRONTED","FINDING","BRAIDED","PARADES","CABLERS","STABLED","ORIGINS","CONTROL","STAGGER","WAGERED"
70    DATA "THOROUGH","MINOTAUR","SANDWICH","FRIGHTEN","RIGOROUS","STAGNATE","ORIGINAL","BREAKING","SNOWPLOW","GLOWBUGS","PAPERING","PROTEINS","PADLOCKS"
80    DATA "ENCOURAGE","FRAGILITY","STALEMATE","BOUNDLESS","XENOPHOBE","THANKLESS","STEADFAST","AGREEMENT","VICTORIES","GRANDIOSE","UNGUARDED","DESIGNERS"
90    DATA "SIGNALERS" ! 13th entry for above line (line size limit)
100   !-----------------------------------------------------!
110   CONTROL 1,12;1 ! Turn off softkeys
120   CLEAR SCREEN
130   WAIT 1
140   CALL Typeprint("SET TERMINAL/INQUIRE")
150   CALL Typeprint("SET FILE/PROTECTION=OWNER:RWED ACCOUNTS.F")
160   CALL Typeprint("SET HALT RESTART/MAINT")
170   CALL Typeprint("RUN DEBUG/ACCOUNTS.F")
180   WAIT 1
190   CLEAR SCREEN
200   PRINT "WELCOME TO ROBCO INDUSTRIES (TM) TERMLINK"
210   WAIT .25
220   PRINT "RIT-V300"
230   WAIT .25
240   PRINT "Initializing RobCo Industries(TM) MF Boot Agent v2.3.0"
250   PRINT "RETROS BIOS"
260   PRINT "RBIOS-4.02.08.00 52EE5.E7.E8"
270   PRINT "Copyright 2075-2077 RobCo Ind."
280   WAIT .5
290   STATUS 1,1;Y
300   STATUS 1,0;X
310   FOR I=1 TO 64
320     CONTROL 1,1;Y
330     CONTROL 1,0;X
340     PRINT "Uppermem: ";I;"KB"
350     WAIT .005
360   NEXT I
370   PRINT "Root (5A8)"
380   WAIT .25
390   PRINT "Maintenance Mode"
400   WAIT 1
410   PRINT "" ! BOOT Beep
420   CLEAR SCREEN
430   PRINT "ROBCO INDUSTRIES (TM) TERMLINK PROTOCOL"
440   WAIT 1
450   PRINT "ENTER PASSWORD NOW"
460   PRINT ""
470   PRINT ""
480   RANDOMIZE
490   Attempts=4
500   READ Words$(*)
510   Password$=Words$(RND*12)
520   CALL Drawgame(Words$(*))
530   CALL Showattempts(Attempts)
540   CALL Resetconsole(Console$(*))
550 Tryguess: !
560   Guess$=" "
570   INPUT ">",Guess$
580   Similarity=FNTryguess(Password$,Guess$)
590   Attempts=Attempts-1
600   CALL Showattempts(Attempts)
605   CALL Addconsole(Console$(*),"")
610   CALL Addconsole(Console$(*),">"&UPC$(Guess$))
620   IF Similarity=-1 THEN
630     CALL Addconsole(Console$(*),">Access granted")
640     CALL Addconsole(Console$(*),">Loading...")
650     BEEP 300,.1
660     BEEP 800,.25
670     WAIT 1
680     Granted=Granted+1
690     IF Granted=3 THEN
700       CALL Terminal(Password$)
710     ELSE
720       GOTO 410
730     END IF
740   ELSE
750     CALL Addconsole(Console$(*),">Entry denied")
760     BEEP 300,.1
770     CALL Addconsole(Console$(*),">"&VAL$(Similarity)&"/"&VAL$(LEN(Password$))&" correct.")
780     IF Attempts=0 THEN
790       CALL Addconsole(Console$(*),">Attempts exceeded")
800       CALL Addconsole(Console$(*),">Term lockdown.")
810       BEEP 100,.15
820       WAIT .01
830       BEEP 100,.15
840       WAIT .01
850       BEEP 100,.15
860       WAIT 1
870       CALL Lockdown
880     END IF
890   END IF
900   GOTO Tryguess
910   END
920   SUB Showconsole(C$(*))
930     FOR I=0 TO 12
940       CONTROL 1,1;I+6
950       CONTROL 1,0;54
960       PRINT C$(I)&RPT$(" ",25-LEN(C$(I)))
970     NEXT I
980   SUBEND
990   SUB Addconsole(Console$(*),Line$)
1000    FOR I=0 TO 11
1010      Console$(I)=Console$(I+1)
1020    NEXT I
1030    Console$(12)=Line$
1040    CALL Showconsole(Console$(*))
1050  SUBEND
1060  DEF FNTryguess(Pass$,G$)
1070    Charsalike=0
1080    FOR I=1 TO MIN(LEN(Pass$),LEN(G$))
1090      IF LWC$(G$[I;1])=LWC$(Pass$[I;1]) THEN
1100        Charsalike=Charsalike+1
1110      END IF
1120    NEXT I
1130    IF LWC$(Pass$)=LWC$(G$) THEN
1140      RETURN -1
1150    ELSE
1160      RETURN Charsalike
1170    END IF
1180  FNEND
1190  SUB Drawgame(Words$(*))
1200    FOR I=6 TO 18
1210      Word$=Words$(I-6)
1220      Isleft=INT(RND*2)
1230      Memstart=62704
1240      Mem1$=DVAL$(Memstart+((I-6)*12),16)
1250      Mem2$=DVAL$(Memstart+((I+11)*12),16)
1260      CONTROL 1,1;I
1270      CONTROL 1,0;1!Set X = 1
1280      PRINT "0x";Mem1$[5;4];" ";FNGarbage$(16)
1290      IF Isleft THEN
1300        CONTROL 1,1;I
1310        CONTROL 1,0;(RND*(16-LEN(Word$))+8)
1320        PRINT Word$
1330      END IF
1340      CONTROL 1,1;I
1350      CONTROL 1,0;28
1360      PRINT "0x";Mem2$[5;4];" ";FNGarbage$(16)
1370      IF NOT Isleft THEN
1380        CONTROL 1,1;I
1390        CONTROL 1,0;(RND*(16-LEN(Word$))+28+7)
1400        PRINT Word$
1410      END IF
1420    NEXT I
1430  SUBEND
1440  SUB Showattempts(Attempts)
1450    CONTROL 1,1;4
1460    CONTROL 1,0;1
1470    PRINT VAL$(Attempts);" ATTEMPT(S) LEFT: "&RPT$(CHR$(133)&" "&CHR$(128)&" ",Attempts)&RPT$("  ",4-Attempts)
1480  SUBEND
1490  SUB Lockdown
1500    CLEAR SCREEN
1510    CONTROL 1,0;33
1520    CONTROL 1,1;11
1530    PRINT "TERMINAL LOCKED"
1540    PRINT ""
1550    CONTROL 1,0;30
1560    PRINT "PLEASE SEE SUPERVISOR"
1570    WAIT 30
1580    CONTROL 1,12;0
1590    STOP
1600  SUBEND
1610  SUB Typeprint(Text$)
1620    STATUS 1,0;X ! Save X Pos
1630    STATUS 1,1;Y ! Save Y Pos
1640    FOR I=1 TO LEN(Text$)
1650      CONTROL 1,0;X+I-1
1660      CONTROL 1,1;Y
1670      STATUS 1,0;Lastx
1680      PRINT Text$[I;1]&CHR$(133)&" "&CHR$(128)
1690      WAIT MIN(.07,RND*.07)
1700      BEEP 1000,.00001
1710    NEXT I
1720    CONTROL 1,1;Y
1730    CONTROL 1,0;Lastx+1
1740    PRINT " "
1750    BEEP 1000,.00001
1760    CONTROL 1,1;Y+1
1770    PRINT CHR$(133);" ";CHR$(128)
1780    CONTROL 1,1;Y+1
1790    WAIT .5
1800    CONTROL 1,1;Y+1
1810    PRINT CHR$(128);" "
1820    CONTROL 1,1;Y+1
1830  SUBEND
1840  SUB Resetconsole(Console$(*))
1850    FOR I=0 TO 11
1860      Console$(I)=""
1870    NEXT I
1880    Console$(12)=">Ready."
1890    CALL Showconsole(Console$(*))
1900  SUBEND
1910  DEF FNGarbage$(INTEGER Numchars)
1920    DIM Garbage$[80],Garbagechars$[80]
1930    Garbagechars$="!@#$%^&*()[]{};:',<>/?................"
1940    FOR I=1 TO Numchars
1950      Index=INT(RND*(LEN(Garbagechars$))+1)
1960      Garbage$[I;1]=Garbagechars$[Index;1]
1970    NEXT I
1980    RETURN Garbage$
1990  FNEND
2000  SUB Terminal(Password$)
2010    CLEAR SCREEN
2020    PRINT "ROBCO INDUSTRIES (TM) TERMLINK PROTOCOL"
2030    PRINT ">"
2040    CONTROL 1,1;2
2050    CALL Typeprint(">LOGIN ROOT")
2060    WAIT .25
2070    PRINT "ENTER PASSWORD NOW"
2080    WAIT .5
2090    PRINT ">"
2100    CONTROL 1,1;4
2110    CALL Typeprint(">"&RPT$("*",LEN(Password$)))
2120    BEEP
2130    WAIT 1
2140    CALL Terminalhome
2150  SUBEND
2160  SUB Terminalmail
2170    CLEAR SCREEN
2180    PRINT "ROBCO INDUSTRIES UNIFIED OPERATING SYSTEM"
2190    PRINT "   COPYRIGHT 2075-2077 ROBCO INDUSTRIES"
2200    PRINT "                -Server 9-"
2210    PRINT ""
2220    PRINT "RE: Strange Network Activity"
2230    PRINT "____________________________"
2240    PRINT ""
2250    PRINT "I need you to look into some strange activity that has recently cropped up"
2260    PRINT "on the termlinks this week. Several systems have been observed to have"
2270    PRINT "halted or started up in maintenance mode. Do we have any scheduled"
2280    PRINT "maintenance this week?"
2290    PRINT ""
2300    PRINT "I'm sure it's probably nothing, but better to be safe than sorry."
2310    PRINT ""
2320    PRINT "J. Schumacher"
2330    INPUT "Press ENTER to Continue",X
2340  SUBEND
2350  SUB Terminalhome
2360 Display:   !
2370    CLEAR SCREEN
2380    PRINT "ROBCO INDUSTRIES UNIFIED OPERATING SYSTEM"
2390    PRINT "   COPYRIGHT 2075-2077 ROBCO INDUSTRIES"
2400    PRINT "                -Server 9-"
2410    PRINT ""
2420    PRINT "1> Remote Mail"
2430    PRINT "2> Disable Security"
2440    PRINT "3> Unlock Safe"
2450    PRINT "4> Logout"
2460    PRINT ""
2470    INPUT ">",Selection
2480    SELECT Selection
2490    CASE 1
2500      CALL Terminalmail
2510    CASE 2
2520      PRINT "SECURITY PROTOCOL DEACTIVATED"
2530      INPUT "Press ENTER to Continue",X
2540    CASE 3
2550      BEEP 10000,.0001
2560      BEEP 10000,.0001
2570      PRINT "SAFE UNLOCKED"
2580      INPUT "Press ENTER to Continue",X
2590    CASE 4
2600      CLEAR SCREEN
2610      PRINT "User ROOT Session Ended"
2620      CONTROL 1,12;0
2630      STOP
2640    CASE ELSE
2650      DISP "Not a valid selection"
2660      BEEP 100,.25
2670      WAIT .5
2680    END SELECT
2690    GOTO Display
2700  SUBEND
