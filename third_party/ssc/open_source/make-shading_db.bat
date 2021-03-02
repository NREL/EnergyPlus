@echo off
:Ask
echo Would you like to use create the shading database open source package?(Y/N)
set /P INPUT= %=%
If /I "%INPUT%"=="y" goto yes 
If /I "%INPUT%"=="n" goto no

:yes
del /Q shading_db\DB8_vmpp_impp_uint8_bin.h
del /Q shading_db\lib_pv_shade_loss_mpp.h
del /Q shading_db\lib_pv_shade_loss_mpp.cpp
del /Q shading_db\lib_miniz.cpp
del /Q shading_db\lib_miniz.h

copy ..\shared\DB8_vmpp_impp_uint8_bin.h shading_db
copy ..\shared\lib_pv_shade_loss_mpp.h shading_db
copy ..\shared\lib_pv_shade_loss_mpp.cpp shading_db
copy ..\shared\lib_miniz.cpp shading_db
copy ..\shared\lib_miniz.h shading_db

echo
echo Zip up the shading_db folder and distribute with disclaimer
:no
