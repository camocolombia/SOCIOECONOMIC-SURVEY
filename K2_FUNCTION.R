
K2_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_K2.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #length(Subset(ss,"g_1_1_"))
  
#ii=1  
  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"k2_")))));ID<-t(t(ID[1:length(Subset(ss,"k2_0_")),]))
    a<-t(Subset(ss[ii,],"k2_"));a<-t(t(a[1:length(Subset(ss,"k2_0_")),]))
    b<-t(Subset(ss[ii,],"k2_0_"))
    c<-t(Subset(ss[ii,],"k2_1_"))
    d<-t(Subset(ss[ii,],"k2_2_"))
    e<-t(Subset(ss[ii,],"k2_3_"))
    f<-t(Subset(ss[ii,],"k2_4_"))
    g<-t(Subset(ss[ii,],"k2_5_"))
    h<-t(Subset(ss[ii,],"k2_6_"))
    i<-t(Subset(ss[ii,],"k2_7_"))
    j<-t(Subset(ss[ii,],"k2_8_"))
    k<-t(Subset(ss[ii,],"k2_9_"))
    l<-t(Subset(ss[ii,],"k2_10_"))
    m<-t(Subset(ss[ii,],"k2_11_"))
    n<-t(Subset(ss[ii,],"k2_12_0_"))
    o<-t(Subset(ss[ii,],"k2_12_"));o<-t(t(o[11:20,]))
    p<-t(Subset(ss[ii,],"k2_12b"))   
    q<-t(Subset(ss[ii,],"k2_12_c_"))
  
    
    
    to_check<-list(ID,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
    
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "K.2  ID_CHACRA",
                   "K.2 Área total de la chacra",
                   "K.2 Área en cultivos permanentes (incluyendo pasto de corte o banco forrajero)",
                   "K.2 Área en cultivos temporales",
                   "K.2 Área en pasturas (sin incluir las que estan en sistemas silvopastoriles)",
                   "K.2 Área en huertas, huerto, solar, patio",
                   "K.2 Área en descanso/purma",
                   "K.2 Silvopastoril (sin incluir pasto de corte o banco forrajero)",
                   "K.2 Área agroforestal",
                   "K.2 Área no cultivables (ríos, quebradas,etc.)",
                   "K.2 Área en bosque",
                   "K.2 Área en humedales",
                   "K.2 Área en cananguachales o aguajales",
                   "K.2 ¿Con cuantos nacimientos de agua cuenta la chacra",
                   "K.2 Otras áreas no incluidas (si/no)",
                   "K.2 Otro (Especifique)",
                   "K.2 Área de otros"
                  
  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_K2",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-K2_FUNCTION(rep_dir,ins_dir,date_S)