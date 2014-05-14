'Programm: ConvertFd - erstellt 9. Aug 1985

' Dieses Programm konvertiert .fd-Dateien wie z.B.
' graphics_lib.fd  in .bmap-Dateien wie  graphics.bmap.
' Damit können AmigaBasic-Programme Amiga-Systemroutinen,
' die in diesen Bibliotheken enthalten sind, ueber den
' LIBRARY-Befehl mit ihrem Namen aufrufen. 
'
' Modifiziert 01/86 von Carolyn Scheppner  CBM USA:
' Allen Funktionsnamen, die mit AmigaBasic-Schluessel-
'  wörtern kollidieren, wird ein x vorangestellt.
'  DATA-Zeilen am Programmende enthalten bekannte
'  Namenskonflikte. Bei Aufruf dieser Funktionen ist
'  ebenfalls ein x voranzustellen (Bsp. xRead).
' Erzeugte .bmap-Datei wird jetzt im aktuellen bzw.
'  angegebenen Verzeichnis gespeichert (bisher in
'  LIBS:). Greift Ihr Programm auf eine .bmap-Datei
'  mit dem LIBRARY-Befehl zu, muß es im aktuellen
'  Verzeichnis stehen oder in LIBS: .
' Soweit bekannt, MÜSSEN Sie die .bmap-Datei genau
'  wie die LIBRARY-Datei benennen, also deren 
'  Dateinamen bis zum _ 
'  (Bsp.:   dos.bmap  aus  dos_lib.fd) .

  DEFINT a-Z    ' Alle Variablen als Integer voreingestellt

  REM ******** Fuer Namenskonflikte **********
  READ cnt   ' Anzahl bekannte Namenskonflikte
  DIM con$(cnt)
  FOR k = 0 TO cnt-1: READ con$(k): NEXT
  REM ****************************************

  PRINT "Bitte eingeben:":PRINT 
  INPUT "Name der zu lesenden      .fd-Datei = ",fdFilename$
  OPEN fdFilename$ FOR INPUT AS #1
  INPUT "Name der zu erzeugenden .bmap-Datei = ",bmapFilename$
  OPEN bmapFilename$ FOR OUTPUT AS #2
  INPUT "Prefix bei funktionen               = ",prefix$
  nam$="0"
  WHILE NOT EOF(1)
    GetLine
    IF char$ = "#" THEN
      ' Zeilen mit "#" am Anfang sind Kommandozeilen 
      GOSUB GotCommand
    ELSEIF char$ = "*" THEN
      ' Zeilen mit "*" am Anfang sind Kommentarzeilen 
    ELSEIF char$ = CHR$(10) OR char$ = CHR$(13) THEN
      ' Leerzeile
    ELSE
      ' alle anderen Zeilen definieren eine LIBRARY-Funktion    
      GOSUB GotFunction
    END IF
  WEND
  CLOSE
  END

GotCommand:
  GetChar  '  erstes "#" ueberlesen
  GetChar  ' zweites "#" ueberlesen
  gettoken
  IF token$ = "bias" THEN
    GetNum
    offset = -num
  ELSEIF token$="base" THEN
    SkipWhiteSpace
    libbase$=MID$(buf$,column)
    PRINT #2,libbase$+CHR$(9)+"dc.l 0"
  ELSEIF token$="private" THEN
    public=0:BEEP
  ELSEIF token$="public" THEN
    public=1
  ELSE
    PRINT token$
  END IF
 ram: RETURN

GotFunction:
  funcoffset=offset
  offset=offset-6
  IF public=0 THEN RETURN
  gettoken  'token$=Funktionsname

  REM **** Konfliktnamen ein 'x' voranstellen ****
  k$ = token$
  REM **********************************************
PRINT #2,"w_"+k$+": dc.l ",nam$
PRINT #2," dc.l 0"
PRINT #2," dc.b WF_BINARY|WF_SMUDGE"
PRINT #2," dc.b "+MID$(STR$(LEN(token$)+LEN(prefix$)),2)+","+CHR$(34)+UCASE$(prefix$+token$)+CHR$(34)
PRINT #2," even"
PRINT #2," move.l "+libbase$+"(pc),a6"
contin=0:nam$="w_"+token$
PRINT k$,
  parms$=""
  SkipTill "(": IF char$="" THEN BadFileFormat
  SkipTill ")": IF char$="" THEN BadFileFormat
  GetChar
  IF char$<>"" THEN
    SkipTill "(": IF char$="" THEN BadFileFormat
    char$=""
    WHILE char$ <> ")"
      GetChar ' , oder / ueberlesen
      IF char$<>")" THEN
        GOSUB GetRegister
        IF register=0 THEN BadFileFormat
        IF register=-1 THEN
          PRINT "Warnung: Funktion ";token$;" nicht mitgenommen, weil"
          PRINT " ihr Parameter in einem Register zu uebergeben sind,"
          PRINT " das AmigaBasic nicht verwenden kann.
          PRINT #2,"    *****";
        END IF
        IF char$<>"/" THEN
          IF contin=0 THEN PRINT #2," move.l (a5)+,";
          PRINT #2,reg$ : contin=0
        ELSE
          IF contin=0 THEN
            contin=1
            PRINT #2," movem.l (a5)+,";
          END IF
          PRINT #2,reg$+"/";
        END IF
      END IF
    WEND
  END IF
INPUT noparams
IF noparams=0 THEN
  PRINT #2," jmp "+STR$(funcoffset)+"(a6)"
ELSE
  PRINT #2," jsr "+STR$(funcoffset)+"(a6)"
  PRINT #2," move.l d0,-(a5)
  PRINT #2," rts"
END IF
  RETURN

BadFileFormat:
  PRINT "Fehler   : ";fdFilename$;" hat einen Format-Fehler"
  PRINT "In Zeile :";lineNum;" ";buf$
  PRINT "In Spalte:";column
  CLOSE
  STOP


' {d0,d1,d2,d3,d4,d5,d6,d7,a0,a1,a2 ,a3,a4,a5} zuordnen zu {1,..,14}
GetRegister:
  uchar$=UCASE$(char$)
  IF uchar$="D" THEN
    register=1
  ELSEIF uchar$="A" THEN
    register = 9
  ELSE
    register=0  ' Fehler
    RETURN
  END IF
  GetChar  ' a oder d ueberspringen
  reg$=uchar$+char$
  i=ASC(char$)-48
  IF i<0 OR i>7 THEN register=0: RETURN  'Fehler
  GetChar  'Ziffer ueberspringen
  register=register+i
  IF register>11 THEN register=-1  'Fehler
  RETURN

SUB GetLine STATIC
  SHARED buf$,column,lineNum
  LINE INPUT #1,buf$
  column = 0
  GetChar
  lineNum = lineNum+1
  END SUB

SUB GetNum STATIC
  SHARED num,token$
  gettoken
  num = VAL(token$)
  END SUB

SUB gettoken STATIC
  SHARED buf$,char$,token$
  SkipWhiteSpace
  token$=""
  uchar$=UCASE$(char$)
  WHILE ((uchar$>="A") AND (uchar$<="Z")) OR ((uchar$>="0") AND (uchar$<="9")) OR (uchar$="-")
    token$=token$+char$
    GetChar
    uchar$ = UCASE$(char$)
  WEND
  END SUB

SUB SkipTill(stopChar$) STATIC
  SHARED char$
  WHILE (char$ <> stopChar$) AND (char$ <> "")
    GetChar
  WEND
  END SUB

SUB SkipWhiteSpace STATIC
  SHARED char$
  WHILE (char$=" ") OR (char$=CHR$(9))
    GetChar
  WEND
  END SUB

SUB GetChar STATIC
  SHARED column,char$,buf$
  column = column + 1
  char$ = MID$(buf$,column,1)
  END SUB
       
REM **** Konfliktnamen, Anzahl vorweg ****                
DATA 11                
DATA abs, Close, Exit, Input, Open, Output
DATA Read, tan, Translate, Wait, Write
