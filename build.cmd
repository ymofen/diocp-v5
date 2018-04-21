ECHO OFF
SET BDSROOT=D:\Program Files (x86)\Embarcadero\Studio\19.0
SET DIOCPV5=%cd%


"%BDSROOT%\bin\dcciosarm64.exe"  -DNDEBUG -JPHNE -AGenerics.Collections=System.Generics.Collections;Generics.Defaults=System.Generics.Defaults;WinTypes=Winapi.Windows;WinProcs=Winapi.Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE -NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell -I"%DIOCPV5%\source";"%BDSROOT%\lib\iosDevice64\release" -O"%DIOCPV5%\outputs\iOS64";"%BDSROOT%\lib\iosDevice64\release" -R"%DIOCPV5%\outputs\iOS64";"%BDSROOT%\lib\iosDevice64\release" -U"%DIOCPV5%\outputs\iOS64";"%BDSROOT%\lib\iosDevice64\release" -E"%DIOCPV5%\outputs\iOS64" -LE"%DIOCPV5%\outputs\iOS64" -LN"%DIOCPV5%\outputs\iOS64" -NU"%DIOCPV5%\outputs\iOS64" -NB"%DIOCPV5%\outputs\iOS64" -NO"%DIOCPV5%\outputs\iOS64" -NH"%DIOCPV5%\outputs\iOS64" -B "%DIOCPV5%\source\utils_strings.pas"

pause