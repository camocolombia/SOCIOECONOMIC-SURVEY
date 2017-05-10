
J2_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }

  ss<-read.dta13(paste0(date_dir,"/","MODULO_J2.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #length(Subset(ss,"g_1_1_"))
  
  
#ii=1  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"j2_1_1_")))))
    aa<-t(Subset(ss[ii,],"j2_1_1_"))
    a<-t(Subset(ss[ii,],"j2_1_2_"))
    b<-t(Subset(ss[ii,],"j2_1_3_"))
    c<-t(Subset(ss[ii,],"j2_1_3a_"))
    d<-t(Subset(ss[ii,],"j2_1_4_"))
    e<-t(Subset(ss[ii,],"j2_1_5_"))
    f<-t(Subset(ss[ii,],"j2_1_6_"))
    g<-t(Subset(ss[ii,],"j2_1_6a_"))
    h<-t(Subset(ss[ii,],"j2_1_7_"))
    
    to_check<-list(ID,aa,a,b,c,d,e,f,g,h)
    to_check<-do.call(cbind,to_check)
    rm(ID,aa,a,b,c,d,e,f,g,h)
    
   M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "J.2.1  Credito numero",
                   "J.2.1 ¿En que año pidió el crédito?",
                   "J.2.1 ¿Cuál fue la razón de solicitar el crédito?",
                   "J.2.1 ¿Cuál fue la razón de solicitar el crédito? (Especifique)",
                   "J.2.1 ¿Quién del hogar solicitó el crédito?",
                   "J.2.1 ¿Cuánto fue el monto conseguido?",
                   "J.2.1 ¿Quién (o qué entidad) le hizo el préstamo?",
                   "J.2.1 ¿Quién (o qué entidad) le hizo el préstamo? (Especifique)",
                   "J.2.1 Quien decidió en que gastar el dinero?"
                  
  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_J2",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-J2_FUNCTION(rep_dir,ins_dir,date_S)