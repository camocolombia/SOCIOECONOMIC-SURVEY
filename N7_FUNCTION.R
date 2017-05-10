
N7_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_N7.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #ii=1
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"n12_1a_")))))
    a<-t(Subset(ss[ii,],"n12_1_"));a<-t(t(a[1:length(Subset(ss,"n12_1_1_")),]))
    b<-t(Subset(ss[ii,],"n12_1a_"))
    c<-t(Subset(ss[ii,],"n12_1_1_"))
    d<-t(Subset(ss[ii,],"n12_2_"))
    e<-t(Subset(ss[ii,],"n12_3_"))#;e<-t(t(e[1:length(Subset(ss,"n10_2_")),]))
    f<-t(Subset(ss[ii,],"n12_4_"))
    g<-t(Subset(ss[ii,],"n12_5_"))
    h<-t(Subset(ss[ii,],"n12_6_"))#;h<-t(t(h[1:length(Subset(ss,"n10_2_")),]))
    i<-t(Subset(ss[ii,],"n12_a_"))
    j<-t(Subset(ss[ii,],"n12_b_"))
    
    to_check<-list(ID,a,b,c,d,e,f,g,h,i,j)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,d,e,f,g,h,i,j)
    
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "Actividad",
                   "Actividad (Especifique)",
                   "¿Realiza la actividad?",
                   "N.12 Total de jornales por hombres (trabajo familiar)",
                   "N.12 Total de jornales por mujeres (trabajo familiar)",
                   "N.12 Total de jornales por niños (trabajo familiar)",
                   "N.12 Total de jornales por hombres (trabajo contratado)",
                   "N.12 Total de jornales por mujeres (trabajo contratado)",
                   "N.12 Costo total (Dinero y especie) por dia (Pesos) por hombres (trabajo contratado)",
                   "N.12 Costo total (Dinero y especie) por dia (Pesos) por mujeres (trabajo contratado)"
  )
  
                   
  
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_N7",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-N7_FUNCTION(rep_dir,ins_dir,date_S)