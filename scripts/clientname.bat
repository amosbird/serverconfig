@echo off
:: set The RDP port here usually 3389:
set RDPort=3389

for /f "tokens=3" %%a in ('netstat -n ^| find /i "%RDPort%" ^| find /i "established"') do set IP=%%a
IF "%IP%"=="" goto :Cancela

for /f "delims=:" %%a in ("%IP%") do set IP=%%a
reg add "HKCU\Environment" /v "CLIENTNAME" /d "%IP%:22" /t reg_sz /f
exit

:Cancela
exit
