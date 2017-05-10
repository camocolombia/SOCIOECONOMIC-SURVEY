
N5_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_N5.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #ii=1
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"n10_1_")))));ID<-t(t(ID[1:length(Subset(ss,"n10_2_")),]))
    a<-t(Subset(ss[ii,],"n10_1_"));a<-t(t(a[1:length(Subset(ss,"n10_2_")),]))
    b<-t(Subset(ss[ii,],"n10_2_"))
    c<-t(Subset(ss[ii,],"n10_c0_"))
    d<-t(Subset(ss[ii,],"n10_3_"))
    e<-t(Subset(ss[ii,],"n10_4_"));e<-t(t(e[1:length(Subset(ss,"n10_2_")),]))
    ee<-t(Subset(ss[ii,],"n10_4_1_"));ee<-t(t(ee[1:length(Subset(ss,"n10_2_")),]))
    f<-t(Subset(ss[ii,],"n10_5_"))
    g<-t(Subset(ss[ii,],"n10_6_"))
    h<-t(Subset(ss[ii,],"n10_7_"));h<-t(t(h[1:length(Subset(ss,"n10_2_")),]))
    i<-t(Subset(ss[ii,],"n10_7_1_"))
    j<-t(Subset(ss[ii,],"n10_7a_"))
    k<-t(Subset(ss[ii,],"n10_8_"))
    l<-t(Subset(ss[ii,],"n10_8a_"))
    m<-t(Subset(ss[ii,],"n10_9_"))
    n<-t(Subset(ss[ii,],"n10_9a_"))
    o<-t(Subset(ss[ii,],"n10_10_"))
    p<-t(Subset(ss[ii,],"n10_10a_"))
    q<-t(Subset(ss[ii,],"n10_11_"));q<-t(t(q[1:length(Subset(ss,"n10_2_")),]))
    r<-t(Subset(ss[ii,],"n10_11_1_"))
    s<-t(Subset(ss[ii,],"n10_12_"))
    
    
    
    to_check<-list(ID,a,b,c,d,e,ee,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,d,e,ee,f,g,h,i,j,k,l,m,n,o,p,q,r,s)
    
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "N.10 Animal",
                   "N.10 Animal (Especifique)",
                   "¿Posee este tipo de animales?",
                   "N.10 Del numero total de animales que tenia en los ultimos 12 meses,¿Cuántos fueron para autoconsumo?",
                   "N.10 Del numero total de animales que tenia en los ultimos 12 meses,¿Cuántos fueron vendidas?",
                   "¿Vendió?",
                   "N.10 Quien decide cuándo vender un animal? [opción múltiple] ID persona",
                   "N.10 Razón de venta (edad animal, problemas, otros (espectos)",
                   "N.10 ¿Cuál fue el Precio de venta durante los ultimos 12 meses?",
                   "N.10 Unidad de venta",
                   "N.10 Unidad de venta (Otros)",
                   "N.10 ¿A quien le vendio?",
                   "N.10 ¿A quien le vendio? (especifique)",
                   "N.10 ¿Donde realizo la venta?",
                   "N.10 ¿Donde realizo la venta? (especifique)",
                   "N.10 Si lo vendio fuera de su finca, ¿utilizo algún medio de transporte para realizar la venta?",
                   "N.10 Si lo vendio fuera de su finca, ¿utilizo algún medio de transporte para realizar la venta? (Especifique)",
                   "N.10 ¿Cuanto le cuesta en promedio cada viaje? ($Soles)?",
                   "N.10 ¿Que número de viajes necesito para vender el total del producto(#)?",
                   "N.10 ¿Quién decide en qué gastar el dinero que se genera con...? (Ver ID)"
                   
  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_N5",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-N5_FUNCTION(rep_dir,ins_dir,date_S)