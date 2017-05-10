****''
/*

local files : dir "D:\Dropbox\Dropbox\ENCUESTA_SAL\ENCUESTA_SAL\REPORTES_SEMANA\SINCHI\20160411_10AM" files "*.do"
display `files'

foreach file in `files' {
do `files'
export excel using `files' .xls, sheet("sheet1") firstrow(variables) replace
save `files', replace

}

**end

cd "D:\Dropbox\Dropbox\ENCUESTA_SAL\ENCUESTA_SAL\REPORTES_SEMANA\SINCHI\20160411_10AM"
local files : dir "D:\Dropbox\Dropbox\ENCUESTA_SAL\ENCUESTA_SAL\REPORTES_SEMANA\SINCHI\20160411_10AM" files "*.do"
foreach file in `files' {
do `files'
export excel using `files'.xls,  replace
save `files', replace
//clear
}
*end

*/

//set excelxlsxlargefile on



// CARGAR MODULOS Y GUARDAR COMO XLS (NO TABLAS)


//cd "D:\Dropbox\Dropbox\ENCUESTA_SAL\ENCUESTA_SAL\REPORTES_SEMANA\SINCHI\20160411_10AM"

cd "D:\Dropbox\Dropbox\ENCUESTA_SAL\ENCUESTA_PER\REPORTES_SEMANA\SUP_4\2016_11_28"

set more off
set max_memory 5g
set maxvar 32767

//local path D:\Dropbox\Dropbox\ENCUESTA_SAL\ENCUESTA_SAL\REPORTES_SEMANA\SINCHI\20160411_10AM\
local path D:\Dropbox\Dropbox\ENCUESTA_SAL\ENCUESTA_PER\REPORTES_SEMANA\SUP_4\2016_11_28\

foreach name_n in MODULO_A MODULO_C MODULO_D  MODULO_F MODULO_G3 MODULO_H4 MODULO_I C MODULO_M MODULO_N1 MODULOD_N4 MODULO_N8 MODULO_Q OBSERVACIONES{
do "`path'\`name_n'.do"
save "`name_n'.dta",replace
use "`name_n'.dta",clear
export excel using "`name_n'.xls",  sheet("sheet1") sheetreplace firstrow(varlabels)
//export delimited using "`name_n'.csv",  delimiter("|")  varnames(1) replace

clear
//do `files'
}

*end

// CARGAR MODULOS Y GUARDAR COMO CSV (TABLAS) 



set more off
set max_memory 5g
set maxvar 32767


//local path D:\Dropbox\Dropbox\ENCUESTA_SAL\ENCUESTA_SAL\REPORTES_SEMANA\SINCHI\20160411_10AM\
local path D:\Dropbox\Dropbox\ENCUESTA_SAL\ENCUESTA_PER\REPORTES_SEMANA\SUP_4\2016_11_28\

foreach name_n in  MODULO_B MODULO_E MODULO_G  MODULO_G2 MODULO_H MODULO_H2 MODULO_H3 MODULO_I2 MODULO_I3 MODULO_J MODULO_J2 MODULO_J3 MODULO_K MODULO_K2 MODULO_L1 MODULO_L2 MODULO_L3 MODULO_L4 MODULO_L5_1 MODULO_L5_2 MODULO_M2 MODULO_M3 MODULO_N3 MODULO_N5 MODULO_N6 MODULO_N7{
do "`path'\`name_n'.do"
save "`name_n'.dta",replace
use "`name_n'.dta",clear
//export excel using "`name_n'.xls",  sheet("sheet1") sheetreplace firstrow(varlabels)
export delimited using "`name_n'.csv",  delimiter("|") replace

clear
//do `files'
}

*end
