call clean
set back=learnsat
7z u -r -tzip c:\%back%\%back%.zip src\*.pro src\all1.txt docs\*.tex docs\*.bib docs\copyright.txt readme.txt docs\*.html docs\*.png *.bat
copy %back%.zip \zip
copy %back%.zip n:\zip
set back=
pause
