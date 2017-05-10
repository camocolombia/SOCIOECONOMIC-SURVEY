
M2_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_M2.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  

  #ii=1
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"m5_1")))));ID<-t(t(ID[1:length(Subset(ss,"m5_1")),]))
    a<-t(Subset(ss[ii,],"m5_1"))
    b<-t(Subset(ss[ii,],"m5_2"))
    c<-t(Subset(ss[ii,],"m5_3"))
   
    
    
    to_check<-list(ID,a,b,c)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c)
    
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "M.5  Nombre",
                   "M.5 Asigne numeros de 1 al cuatro donde 1 es mas productivo y 4 es el menos productivo",
                   "M.5 Proporción de pasturas en su chacra"
                   
  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_M2",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-M2_FUNCTION(rep_dir,ins_dir,date_S)