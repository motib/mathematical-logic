set back=learnsat
7z u -r -tzip c:\%back%\%back%.zip *.pro copyright.txt learnsat.tex learnsat-release.html graph.png graph.eps
copy %back%.zip \zip
copy %back%.zip n:\zip
set back=
pause
