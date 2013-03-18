7z u -r -tzip c:\learnsat\learnsat-%1.zip src\*.pro docs\*.tex docs\*.bib docs\*.pdf readme.txt docs\*.txt docs\*.html docs\*.png
move learnsat-%1.zip dist
pause
