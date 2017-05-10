I3_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  
  
  
  
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_I3.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #length(Subset(ss,"g_1_1_"))
  
  
#ii=1  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"i2_1_1_")))))
    a<-t(Subset(ss[ii,],"i2_1_1_"))
    b<-t(Subset(ss[ii,],"i2_1_1a"))
    c<-t(Subset(ss[ii,],"i2_1_2"))
    d<-t(Subset(ss[ii,],"i2_1_3"))
    
        to_check<-list(ID,a,b,c,d)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,d)
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "I.2.1  Tipo de información",
                   "I.2.1  Tipo de información (Especifique) ",
                   "I.2.1 Los servicios de información / extensión o capacitaciones fueron de [.]?",
                   "I.2.1 ¿Han puesto en práctica lo aprendido en su finca"
                   
  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_I3",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-I3_FUNCTION(rep_dir,ins_dir,date_S)