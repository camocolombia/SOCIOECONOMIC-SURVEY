
N6_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_N6.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #ii=1
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"n11_1_")))))
    aa<-t(Subset(ss[ii,],"n11_1_"))
    a<-t(Subset(ss[ii,],"n11_1a_"))
    aaa<-t(Subset(ss[ii,],"n11_c1_"))
    b<-t(Subset(ss[ii,],"n11_2_"))
    c<-t(Subset(ss[ii,],"n11_3_"))
    d<-t(Subset(ss[ii,],"n11_4_"))
    e<-t(Subset(ss[ii,],"n11_5_"))#;e<-t(t(e[1:length(Subset(ss,"n10_2_")),]))
    f<-t(Subset(ss[ii,],"n11_6_"))
    g<-t(Subset(ss[ii,],"n11_7_"))
    h<-t(Subset(ss[ii,],"n11_7a_"))#;h<-t(t(h[1:length(Subset(ss,"n10_2_")),]))
    i<-t(Subset(ss[ii,],"n11_8_"))
    j<-t(Subset(ss[ii,],"n11_9_"))
    k<-t(Subset(ss[ii,],"n11_10_"))
    l<-t(Subset(ss[ii,],"n11_10a_"))
    m<-t(Subset(ss[ii,],"n11_11_"))
    n<-t(Subset(ss[ii,],"n11_12_"));n<-t(t(n[1:length(Subset(ss,"n11_1_")),]))
    o<-t(Subset(ss[ii,],"n11_12u_"))
    p<-t(Subset(ss[ii,],"n11_12_1_"))
    q<-t(Subset(ss[ii,],"n11_13_"))#;q<-t(t(q[1:length(Subset(ss,"n10_2_")),]))
    r<-t(Subset(ss[ii,],"n11_13a_"))
    s<-t(Subset(ss[ii,],"n11_14_"))
    t<-t(Subset(ss[ii,],"n11_14a_"))
    u<-t(Subset(ss[ii,],"n11_15_"))
    v<-t(Subset(ss[ii,],"n11_15a_"))
    w<-t(Subset(ss[ii,],"n11_16_"))
    x<-t(Subset(ss[ii,],"n11_17_"))
    y<-t(Subset(ss[ii,],"n11_18_"))
  
    
    to_check<-list(ID,aa,a,aaa,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y)
    to_check<-do.call(cbind,to_check)
    rm(ID,aa,a,aaa,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y)
    
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "N.11 Subproducto",
                   "N.11 Subproducto (Especifique)",
                   "¿Realiza la actividad?",
                   "N.11 ¿Cuál es el número de vacas en producción de leche?",
                   "N.11 ¿Cuál es el número de ordeños diarios(Promedio)",
                   "N.11 ¿Que tipo de ordeño ha utilizado durante los ultimos 12 meses?",
                   "N.11 ¿Cuál fue la producción diaria (leche ) (Del total de vacas)?'",
                   "N.11 ¿Cuál fue la producción Anual total (leche) ultimos 12 meses?",
                   "N.11 Unidad",
                   "N.11 Unidad (Otros)",
                   "N.11 Dividir la producción según su uso(debe quedar expresado en las mismas unidades de medida de la producción anual) (Autoconsumo)",
                   "N.11 Dividir la producción según su uso(debe quedar expresado en las mismas unidades de medida de la producción anual) (Venta)",
                   "N.11 Dividir la producción según su uso(debe quedar expresado en las mismas unidades de medida de la producción anual) (Otro)",
                   "N.11 Dividir la producción según su uso(debe quedar expresado en las mismas unidades de medida de la producción anual) (Especifique)",
                   "N.11 ¿Quien realizó la venta? [opción múltiple] ID persona",
                   "N.11 ¿Cuál fue el Precio promedio de venta por unidad durante los ultimos 12 meses ?",
                   "N.11 ¿Cuál fue el Precio promedio de venta por unidad durante los ultimos 12 meses ?  (Unidades)",
                   "¿Vendió?",
                   "N.11 ¿A quien le vendio?",
                   "N.11 ¿A quien le vendio? (Especifique)",
                   "N.11 ¿Donde realizo la venta?",
                   "N.11 ¿Donde realizo la venta? (Especifique)",
                   "N.11 Si lo vendio fuera de su finca, ¿utilizó algún medio de transporte para realizar la venta?",
                   "N.11 Si lo vendio fuera de su finca, ¿utilizó algún medio de transporte para realizar la venta? (Especifique)",
                   "N.11 Cuanto le cuesta en promedio cada viaje? ($Soles)",
                   "N.11 ¿Que número de viajes necesito para vender el total del producto?",
                   "N.11 ¿Quién decide en qué gastar el dinero que se genera con la venta? (Ver ID)"
  )
  
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_N6",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-N6_FUNCTION(rep_dir,ins_dir,date_S)