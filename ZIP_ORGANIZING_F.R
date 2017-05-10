FINAL_STEPS<-function(rep_dir,ins_dir,date_S){
  library(xlsx)
  cat("                               ","\n")
  cat("################################################################","\n")
  cat(" Organizing files in ZIP FILES and joining Module L in one file ","\n")
  cat("################################################################","\n")
  cat("                               ","\n")
  
  output_Dir<-paste0(date_dir,"/","EXCEL_FILES");if(!dir.exists(output_Dir)){dir.create(output_Dir)}
  output_STATA_Dir<-paste0(date_dir,"/","STATA_OUTPUTS");if(!dir.exists(output_STATA_Dir)){dir.create(output_STATA_Dir)}
  output_DTA_Dir<-paste0(date_dir,"/","DTA_OUTPUTS");if(!dir.exists(output_DTA_Dir)){dir.create(output_DTA_Dir)}
  output_CSPRO_FILES<-paste0(date_dir,"/","output_CSPRO_FILES");if(!dir.exists(output_CSPRO_FILES)){dir.create(output_CSPRO_FILES)}
  
  
  ####################################################
  out_EXCEL<-list.files(date_dir,pattern = ".xls*",full.names = T);out_EXCEL2<-list.files(date_dir,pattern = ".xls*",full.names = F)
  file.rename(out_EXCEL,paste0(output_Dir,"/",out_EXCEL2));
  
  
  MODULO_L<-list.files(date_dir,pattern = ".RData*",full.names = T)
  
  MODULO_L2<-lapply(1:length(MODULO_L),function(i){
    
    x<-load(MODULO_L[[i]]);gc()
    x<-M_B
    return(x)
  })
  
  M_B<-do.call(cbind,MODULO_L2)
  write.xlsx (M_B,paste0(output_Dir,"/","MODULO_L_COMPLETE",".xls"),row.names=F,showNA = F);gc()
  file.remove(MODULO_L)
  rm(M_B,MODULO_L,MODULO_L2)
  
  
  zip(paste0(date_dir,"/","out_EXCEL_USE_THIS.ZIP"),output_Dir)
  unlink(output_Dir,recursive = T)
  
  
  if(length(list.files(date_dir,pattern = ".csv*",full.names = T))!=0){
    file.remove(list.files(date_dir,pattern = ".csv*",full.names = T))
  }else{
    cat("There were not CSV files, SKIPPING","\n")
    
  }
  
  ####################################################
  if(length(list.files(date_dir,pattern = ".DO",full.names = T))!=0){
    file.rename(list.files(date_dir,pattern = ".DO",full.names = T),paste0(output_STATA_Dir,"/",list.files(date_dir,pattern = ".DO",full.names = F)))
    file.rename(list.files(date_dir,pattern = ".DCT",full.names = T),paste0(output_STATA_Dir,"/",list.files(date_dir,pattern = ".DCT",full.names = F)))
    file.rename(list.files(date_dir,pattern = ".DAT",full.names = T),paste0(output_STATA_Dir,"/",list.files(date_dir,pattern = ".DAT",full.names = F)))
    
    zip(paste0(date_dir,"/","STATA_OUTPUTS_NO_TO_USE.ZIP"),output_STATA_Dir)
    unlink(output_STATA_Dir,recursive = T)
    
    
  }else{
    cat("There were not DAT, DO and DCT files, SKIPPING","\n")
  }
  
  
  ####################################################
  
  out_DTA<-list.files(date_dir,pattern = ".dta*",full.names = T);out_DTA2<-list.files(date_dir,pattern = ".dta*",full.names = F)
  file.rename(out_DTA,paste0(output_DTA_Dir,"/",out_DTA2));
  
  
  if(length(list.files(date_dir,pattern = ".dta*",full.names = T))!=0){
    file.remove(list.files(date_dir,pattern = ".dta*",full.names = T))
  }else{
    cat("There were not DTA files, SKIPPING","\n")
    
  }
  
  zip(paste0(date_dir,"/","output_DTA_Dir_NO_TO_USE.ZIP"),output_DTA_Dir)
  unlink(output_DTA_Dir,recursive = T)
  
  #####################################################
  
  rest<-list.files(date_dir,full.names = T);rest_N<-list.files(date_dir,full.names = F)
  rest<-rest[1];rest_N<-rest_N[1]
  
  file.rename(rest,paste0(output_CSPRO_FILES,"/",rest_N));
  zip(paste0(date_dir,"/","output_CSPRO_FILES_NO_TO_USE.ZIP"),output_CSPRO_FILES)
  unlink(output_CSPRO_FILES,recursive = T)
  
  
  
  
  #####################################################
  gc()
  
  cat("          ","\n")
  cat("##########","\n")
  cat("!ZIP DONE!","\n")
  cat("##########","\n")
  cat("          ","\n")
  
}

# rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
# #rep_dir<-"E:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
# ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
# #date_S<-"20160411_10AM"
# date_S<-"20160420"
# 
# date_dir<-paste0(ins_dir,"/",date_S)
# 
# xx<-FINAL_STEPS(rep_dir,ins_dir,date_S)
# rm(xx)
