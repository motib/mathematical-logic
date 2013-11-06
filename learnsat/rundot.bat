cd src
mkdir %1-dot
move %1*.dot %1-dot
cd %1-dot
for %%F in (%1-*.dot) do c:\"program files"\"graphviz"\bin\dot -Tpng %%F > %%~nF.png
cd ..
cd ..
