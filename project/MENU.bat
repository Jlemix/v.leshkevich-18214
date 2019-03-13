cls
@ECHO OFF
title MENU
:DECISION
echo =Tic-Tac-Toe Menu======================================================
echo =======================================================================
echo.                                                                        
echo.  Please, make a decision from a list below. 
echo.
echo.   With GUI:
echo.     'F' - To play against friend.
echo.     'B' - To play against bot.
echo.
echo.   W/O GUI:
echo.     'W' - To play Tic-Tac-Toe in console.
echo.                                                  'E' - Exit.
echo.=======================================================================
echo.=======================================================================
set/p "cho=>"
if %cho%==F goto FRIEND
if %cho%==f goto FRIEND
if %cho%==b goto BOT
if %cho%==B goto BOT
if %cho%==W goto CONSOLE
if %cho%==w goto CONSOLE
if %cho%==E goto EXIT
if %cho%==e goto EXIT
echo Invalid choice.
goto DECISION
:FRIEND
start s.exe
:BOT
start bot.exe
:CONSOLE
start ETOKRESTI.exe
:EXIT