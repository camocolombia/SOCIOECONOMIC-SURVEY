######################################################
######################################################
#########CCSA 2016####################################
######################################################
######################################################
####SOURCES PATH####

#src.dir<-"E:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/SCRIPTS"

# src.dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/SCRIPTS"
# source(paste0(src.dir,"/","STATA_TO_EXCEL_F.R"))
# source(paste0(src.dir,"/","ZIP_ORGANIZING_F.R"))

####REPORTS PATH####

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#rep_dir<-"E:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"

####REPORTS PER INSTITUTE PATH####

#ins<-c("CIPAV","SINCHI","UNIAMAZ")
#ins_dir<-paste0(rep_dir,"/",ins[[2]]) 

####DATE TO MAKE REPORTS PER INSTITUTE PATH####

#date_S<-"20160411_10AM"
#date_S<-"20160420"

####DATE TO MAKE REPORTS PER INSTITUTE PATH####

#date_dir<-paste0(ins_dir,"/",date_S)


######RUNNING FUNCTIONS########################

# xx<-STATA_TO_EXCEL(rep_dir,ins_dir,date_S,src.dir)
# xx<-FINAL_STEPS(rep_dir,ins_dir,date_S)


######BATCH RUN ########################

options(java.home="C:\\Program Files\\Java\\jre1.8.0_131")



src.dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_PER/SCRIPTS_PER"
source(paste0(src.dir,"/","STATA_TO_EXCEL_F.R"))
source(paste0(src.dir,"/","ZIP_ORGANIZING_F.R"))

#ins<-c("SUP_1","SUP_2","SUP_3","SUP_4")
#ins<-c("SUP_2","SUP_3","SUP_4")#"SUP_1",
#ins<-c("SUP_1")#"SUP_1",
#ins<-c("SUP_4")#"SUP_1",
#ins<-"JOIN_2017_02_02"
#ins<-"SUP_3"
ins<-""

for(i in 1:length(ins)){ 
  #rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_PER/REPORTES_SEMANA"
  rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_PER/SS"
  
  
  #ins_dir<-paste0(rep_dir,"/",ins[[i]]) 
  ins_dir<-""
  
  #date_S<-"2016_05_18"
  #date_S<-"2016_09_16"
  #date_S<-"2016_10_04"
  #date_S<-"2016_11_28"
  #date_S<-"2017_01_31"
  #date_S<-"2017_02_01"
  #date_S<-""
 
  # date_dir<-paste0(ins_dir,"/",date_S)
  date_dir<-rep_dir
  
  
  
  cat("processing ",date_dir," ...TRASPOSING MATRICES ...","\n")
  
  STATA_TO_EXCEL(rep_dir,ins_dir,date_S,src.dir)
  
  cat("processing ",date_dir," ...COMPRESSING FILES...","\n")
  
 # FINAL_STEPS(rep_dir,ins_dir,date_S)
  
  cat(ins_dir, " DONE! ","\n")
  gc()
 };rm(i) 


