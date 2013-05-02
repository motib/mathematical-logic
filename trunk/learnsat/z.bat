call clean
set back=learnsat
7z u -r -tzip c:\%back%\%back%.zip @zlist.bat
copy %back%.zip \zip
copy %back%.zip n:\zip
set back=
pause
