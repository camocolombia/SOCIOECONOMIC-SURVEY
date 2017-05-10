
N3_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_N3.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #ii=1
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"n4_s1_")))));ID<-t(t(ID[1:length(Subset(ss,"n4_s1a_")),]))
    a<-t(Subset(ss[ii,],"n4_s1_"));a<-t(t(a[1:length(Subset(ss,"n4_s1a_")),]))
    b<-t(Subset(ss[ii,],"n4_s1a_"))
    c<-t(Subset(ss[ii,],"n4_s1_1_"))
    d<-t(Subset(ss[ii,],"n4_s2_"))
    e<-t(Subset(ss[ii,],"n4_s3_"))
    f<-t(Subset(ss[ii,],"n4_s4_"))
    g<-t(Subset(ss[ii,],"n4_s7_"))
    h<-t(Subset(ss[ii,],"n4_s8_"))
    
    
    
    
    to_check<-list(ID,a,b,c,d,e,f,g,h)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,d,e,f,g,h)
    
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "N.4 Animal",
                   "N.4 Animal (Especifique)",
                   "Tiene este tipo de animales",
                   "N.4 ¿Durante los ultimos 12 meses, cuantos animales en total tenia?",
                   "N.4 ¿De estos animales cuántos son propios?",
                   "N.4 ¿Cual es el Peso promedio del animal (kg) ?",
                   "N.4 ¿Cuál es el número total de animales nacidos en ultimo año?",
                   "N.4 ¿Cuál es el número total de animales muertos durante los ultimos 12 meses?"
                  
  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_N3",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-N3_FUNCTION(rep_dir,ins_dir,date_S)