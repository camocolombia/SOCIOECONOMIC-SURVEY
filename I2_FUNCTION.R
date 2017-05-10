I2_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  
  
  
  
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_I2.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #length(Subset(ss,"g_1_1_"))
  
  
  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"i1_1_")))))
    a<-t(Subset(ss[ii,],"i1_1_"));a<-t(t(a[1:length(Subset(ss,"i1_1_")),]))
    b<-t(Subset(ss[ii,],"i1_2_"))
    c<-t(Subset(ss[ii,],"i1_3_"));c<-t(t(c[1:length(Subset(ss,"i1_1_")),]))
    d<-t(Subset(ss[ii,],"i1_3_1_"))
    e<-t(Subset(ss[ii,],"i1_3_2_"))
    f<-t(Subset(ss[ii,],"i1_3_3_"))
    g<-t(Subset(ss[ii,],"i1_3_4_"))
    h<-t(Subset(ss[ii,],"i1_3_5_"))
    i<-t(Subset(ss[ii,],"i1_3a_"))
    
    to_check<-list(ID,a,b,c,d,e,f,g,h,i)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,d,e,f,g,h,i)
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "I.1.1 ID de la persona",
                   "I.1.1 Nombre de la organización",
                   "I.1.1 ¿Qué beneficios recibe por pertenecer a la asociación?",
                   "I.1.1 Mercadeo",
                   "I.1.1 Credito",
                   "I.1.1 Capacitación",
                   "I.1.1 Insumos",
                   "I.1.1 Otro",
                   "I.1.1 ¿Qué beneficios recibe por pertenecer a la asociación? (opcional)"
  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_I2",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-I2_FUNCTION(rep_dir,ins_dir,date_S)