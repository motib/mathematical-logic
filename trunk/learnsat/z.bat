set back=learnsat
7z u -r -tzip -x!gpl.txt c:\%back%\%back%.zip *.pro *.tex *.bib *.txt *.html *.png
copy %back%.zip \zip
copy %back%.zip n:\zip
set back=
pause
