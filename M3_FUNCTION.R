
M3_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_M3.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #ii=1
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"m6_1")))));ID<-t(t(ID[1:length(Subset(ss,"m6_1_1_")),]))
    a<-t(Subset(ss[ii,],"m6_1"));a<-t(t(a[1:length(Subset(ss,"m6_1_1_")),]))
    b<-t(Subset(ss[ii,],"m6_1_1_"))
    c<-t(Subset(ss[ii,],"m6_1_2_"))
    d<-t(Subset(ss[ii,],"m6_1_3_"))
    e<-t(Subset(ss[ii,],"m6_1_4_"))
    f<-t(Subset(ss[ii,],"m6_2_"))
    g<-t(Subset(ss[ii,],"m6_3_"))
    h<-t(Subset(ss[ii,],"m6_4_"));h<-t(t(h[1:length(Subset(ss,"m6_1_1_")),]))
    i<-t(Subset(ss[ii,],"m6_4_1_"))
    j<-t(Subset(ss[ii,],"m6_5_"))
    k<-t(Subset(ss[ii,],"m6_6_"))
    
    
    
    to_check<-list(ID,a,b,c,d,e,f,g,h,i,j,k)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,d,e,f,g,h,i,j,k)
    
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "M.6 Control de plagas [Opcion multiple]",
                   "Químico",
                   "Mecánico",
                   "Manual",
                   "No hace control de plagas",
                   "M.6 Si el control es quimico, especifique el nombre del producto",
                   "M.6 Si el control es quimico, especifique la cantidad del producto",
                   "M.6 Si el control es quimico, especifique las unidades del producto",
                   "M.6 Si el control es quimica. especifique costo por unidad",
                   "M.6 ¿Usó maquinaria? (si/no)"  ,                 
                   "M.6 ¿Si usó maquinaria, Cuantas(Horas/año)?"
                   )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_M3",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-M3_FUNCTION(rep_dir,ins_dir,date_S)