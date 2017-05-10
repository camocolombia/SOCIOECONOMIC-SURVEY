
L4_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_L4.dta"))#,generate.factors=T,convert.factors = T,convert.dates = T,missing.type = F,replace.strl=T);gc()
  
  #ii=1
  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"l2_1_")))))#;ID<-t(t(ID[1:length(Subset(ss,"l2_1_")),]))
    a<-t(Subset(ss[ii,],"l2_1_"))
    b<-t(Subset(ss[ii,],"l2_2_"));#b<-t(t(b[1:length(Subset(ss,"l2_1_")),]))
    c<-t(Subset(ss[ii,],"l2_2a_"))
    d<-t(Subset(ss[ii,],"l4_1_"))
    e<-t(Subset(ss[ii,],"l4_2_"))
    f<-t(Subset(ss[ii,],"l4_3_"))
    g<-t(Subset(ss[ii,],"l4_a_"))
    h<-t(Subset(ss[ii,],"l4_5_"))
    i<-t(Subset(ss[ii,],"l4_6_"))
    j<-t(Subset(ss[ii,],"l4_7_"))
    k<-t(Subset(ss[ii,],"l4_8_"))
    l<-t(Subset(ss[ii,],"l4_9_"))
    m<-t(Subset(ss[ii,],"l4_10_"))
    n<-t(Subset(ss[ii,],"l4_11_"))
    o<-t(Subset(ss[ii,],"l4_12_"))
    q<-t(Subset(ss[ii,],"l4_13_"))
    r<-t(Subset(ss[ii,],"l4_14_"))
    s<-t(Subset(ss[ii,],"l4_15_"))
    t<-t(Subset(ss[ii,],"l4_16_"))
    u<-t(Subset(ss[ii,],"l4_17_"))
    v<-t(Subset(ss[ii,],"l4_18_"))
    w<-t(Subset(ss[ii,],"l4_19_"))
    x<-t(Subset(ss[ii,],"l4_20_"))
    y<-t(Subset(ss[ii,],"l4_21_"))
    z<-t(Subset(ss[ii,],"l4_22_"))
    
    
    to_check<-list(ID,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r,s,t,u,v,w,x,y,z)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,q,r,s,t,u,v,w,x,y,z)
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "L.4  ID_Chacra",
                   "L.4 Cultivo",
                   "L.4 Cultivo (Especifique)",
                   "L.4 Preparacion de la tierra/Siembra (Hombres)",
                   "L.4 Preparacion de la tierra/Siembra (Mujeres)",
                   "L.4 Preparacion de la tierra/Siembra (Niños)",
                   "L.4 Manejo de podas, sombras, control de malezas y plagas (Hombres)",
                   "L.4 Manejo de podas, sombras, control de malezas y plagas (Mujeres)",
                   "L.4 Manejo de podas, sombras, control de malezas y plagas (Niños)",
                   "L.4 Cosecha (Hombres)",
                   "L.4 Cosecha (Mujeres)",
                   "L.4 Cosecha (Niños)",
                   "L.4 Aplicación de fertilizantes, elaboración de fertilizantes organicos (Hombres)",
                   "L.4 Aplicación de fertilizantes, elaboración de fertilizantes organicos (Mujeres)",
                   "L.4 Aplicación de fertilizantes, elaboración de fertilizantes organicos (Niños)",
                   "L.4 Preparacion de la tierra/Siembra (Hombres)",
                   "L.4 Preparacion de la tierra/Siembra (Mujeres)",
                   "L.4 Manejo de podas, sombras, control de malezas y plagas (Hombres)",
                   "L.4 Manejo de podas, sombras, control de malezas y plagas (Mujeres)",
                   "L.4 Cosecha (Hombres)",
                   "L.4 Cosecha (Mujeres)",
                   "L.4 Aplicación de fertilizantes, elaboración de fertilizantes organicos (Hombres)",
                   "L.4 Aplicación de fertilizantes, elaboración de fertilizantes organicos (Mujeres)",
                   "L.4 Costo total (Dinero y especie) por dia (Pesos) (Hombres)",
                   "L.4 Costo total (Dinero y especie) por dia (Pesos) (Mujeres)"
                   )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_L4",".xls"),row.names=F,showNA = F)
  save(M_B,file=paste0(date_dir,"/","MODULO_L4",".RData"))
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-L4_FUNCTION(rep_dir,ins_dir,date_S)
