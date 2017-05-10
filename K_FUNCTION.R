
K_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_K.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #length(Subset(ss,"g_1_1_"))
  
  
#ii=1  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"k1_")))));ID<-t(t(ID[1:length(Subset(ss,"k1_1_2_")),]))
    a<-t(Subset(ss[ii,],"k1_"));a<-t(t(a[1:length(Subset(ss,"k1_1_2_")),]))
    b<-t(Subset(ss[ii,],"k1_1_"));b<-t(t(b[1:length(Subset(ss,"k1_1_2_")),]))
    c<-t(Subset(ss[ii,],"k1_1_1_"))
    d<-t(Subset(ss[ii,],"k1_1_2_"))
    e<-t(Subset(ss[ii,],"k1_2_"))
    f<-t(Subset(ss[ii,],"k1_2a_"))
    g<-t(Subset(ss[ii,],"k1_3_"));g<-t(t(g[1:length(Subset(ss,"k1_1_2_")),]))
    h<-t(Subset(ss[ii,],"k1_3_1_"))
    i<-t(Subset(ss[ii,],"k1_3_2_"))
    j<-t(Subset(ss[ii,],"k1_3_3_"))
    k<-t(Subset(ss[ii,],"k1_3_4_"))
    l<-t(Subset(ss[ii,],"k1_3_5_"))
    m<-t(Subset(ss[ii,],"k1_3_6_"))
    mm<-t(Subset(ss[ii,],"k1_3a_"))
    n<-t(Subset(ss[ii,],"k1_4_"))
    o<-t(Subset(ss[ii,],"k1_5_"));o<-t(t(o[1:length(Subset(ss,"k1_1_2_")),]))
    p<-t(Subset(ss[ii,],"k1_5_1_"))   
    q<-t(Subset(ss[ii,],"k1_5_2_"))
    r<-t(Subset(ss[ii,],"k1_5_3_"))
    s<-t(Subset(ss[ii,],"k1_5_4_"))
    t<-t(Subset(ss[ii,],"k1_5_5_"))
    u<-t(Subset(ss[ii,],"k1_5a_"))
    v<-t(Subset(ss[ii,],"j1_cvf"))
    w<-t(Subset(ss[ii,],"k1_6_"))
    x<-t(Subset(ss[ii,],"k1_6a_"))
    y<-t(Subset(ss[ii,],"k1_7_"))
   
    
    to_check<-list(ID,a,b,c,d,e,f,g,h,i,j,k,l,m,mm,n,o,p,q,r,s,t,u,v,w,x,y)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,d,e,f,g,h,i,j,k,l,m,mm,n,o,p,q,r,s,t,u,v,w,x,y)
    
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "K.1.1  ID_chacra",
                   "K.1.1 Nombre",
                   "K.1.1 Latitud",
                   "K.1.1 Longitud",
                   "K.1.1 ¿Que tipo de posesión de tierra tiene?",
                   "K.1.1 ¿Que tipo de posesión de tierra tiene? (Especifique)",
                   "K.1.1 Si contestó Propietario ¿Que tipo de documento de tenencia tiene? [Opción múltiple]",
                   "K.1.1 Escritura publica",
                   "K.1.1 Titulo de propiedad",
                   "K.1.1 Documento compraventa",
                   "K.1.1 Ninguno",
                   "K.1.1 Constancia de posesión",                   
                   "K.1.1 Otro, cual?",
                   "K.1.1 Si contestó Propietario ¿Que tipo de documento de tenencia tiene? (Especifique)",
                   "K.1.1 Si contestó Propietario ¿Quién(es) es el dueño? Escribir el ID",
                   "K.1.1 ¿En que paisaje donde se encuentra? [opción múltiple]",
                   "K.1.1 Lomerio",
                   "K.1.1 Vega/aluviales",
                   "K.1.1 Colinas bajas estructurales",
                   "K.1.1 Terrazas altas",
                   "K.1.1 Otro, ¿Cuál?",
                   "K.1.1 ¿En que paisaje donde se encuentra? (Especificar)",
                   "K.1.1 ¿Vive en la chacra?",
                   "K.1.1 Si no vive en la chacra, ¿Cual es el medio de tranporte usado con más frecuencia?",
                   "K.1.1 Si no vive en la chacra, ¿Cual es el medio de tranporte usado con más frecuencia? (Opcional)",
                   "K.1.1 ¿Que tiempo promedio en Horas requiere?"
                    )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_K",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-K_FUNCTION(rep_dir,ins_dir,date_S)