cd src
mkdir %1-dots
move %1*.dot %1-dots
cd %1-dots
for %%F in (%1-*.dot) do c:\"program files"\graphviz\bin\dot -Tpng %%F > %%~nF.png
cd ..
cd ..
