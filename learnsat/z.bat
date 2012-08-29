set back=learnsat
7z u -r -tzip c:\%back%\%back%.zip *.pro *.tex *.bib docs\copyright.txt *.html docs\*.png
copy %back%.zip \zip
copy %back%.zip n:\zip
set back=
pause
