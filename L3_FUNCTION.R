
L3_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_L3.dta"))#,generate.factors=T,convert.factors = T,convert.dates = T,missing.type = F,replace.strl=T);gc()
  
  #ii=1
  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"l2_1_")))))#;ID<-t(t(ID[1:length(Subset(ss,"l2_1_")),]))
    a<-t(Subset(ss[ii,],"l2_1_"))
    b<-t(Subset(ss[ii,],"l2_2_"));#b<-t(t(b[1:length(Subset(ss,"l2_1_")),]))
    c<-t(Subset(ss[ii,],"l2_2a_"))
    cc<-t(Subset(ss[ii,],"l3_c2_"))
    d<-t(Subset(ss[ii,],"l3_1_"))
    e<-t(Subset(ss[ii,],"l3_2_"))
    f<-t(Subset(ss[ii,],"l3_2a_"))
    g<-t(Subset(ss[ii,],"l3_3_"))
    h<-t(Subset(ss[ii,],"l3_4_"));h<-t(t(h[1:length(Subset(ss,"l2_1_")),]))
    i<-t(Subset(ss[ii,],"l3_4_1_"))
    iii<-t(Subset(ss[ii,],"l3_5_"))
    j<-t(Subset(ss[ii,],"l3_cf_"))
    k<-t(Subset(ss[ii,],"l3_6_"))
    l<-t(Subset(ss[ii,],"l3_7_"));l<-t(t(l[1:length(Subset(ss,"l2_1_")),]))
    m<-t(Subset(ss[ii,],"l3_7a_"))
    n<-t(Subset(ss[ii,],"l3_7_1_"))
    o<-t(Subset(ss[ii,],"l3_8_"));o<-t(t(o[1:length(Subset(ss,"l2_1_")),]))
    p<-t(Subset(ss[ii,],"l3_8_1_"))   
    q<-t(Subset(ss[ii,],"l3_9_"))
    r<-t(Subset(ss[ii,],"l3_10_"))
    s<-t(Subset(ss[ii,],"l3_11_"));s<-t(t(s[1:length(Subset(ss,"l2_1_")),]))
    t<-t(Subset(ss[ii,],"l3_11_1_"))
    u<-t(Subset(ss[ii,],"l3_11_2_"))
    v<-t(Subset(ss[ii,],"l3_11_3_"))
    w<-t(Subset(ss[ii,],"l3_11_4_"))
    x<-t(Subset(ss[ii,],"l3_11_5_"))
    y<-t(Subset(ss[ii,],"l3_11_6_"))
    z<-t(Subset(ss[ii,],"l3_11_7_"))
    
    za<-t(Subset(ss[ii,],"l3_11_8_"))
    zb<-t(Subset(ss[ii,],"l3_11_9_"))
    zc<-t(Subset(ss[ii,],"l3_11_10_"))
    zd<-t(Subset(ss[ii,],"l3_11_11_"))
    ze<-t(Subset(ss[ii,],"l3_11_12_"))
    zf<-t(Subset(ss[ii,],"l3_11_13_"))

    aa<-t(Subset(ss[ii,],"l3_11a_"))
    ab<-t(Subset(ss[ii,],"l3_12_"))
    ac<-t(Subset(ss[ii,],"l13_13_"))
    ad<-t(Subset(ss[ii,],"l13_13a_"))
    ae<-t(Subset(ss[ii,],"l3_14_"))
    af<-t(Subset(ss[ii,],"l3_15_"))
    ag<-t(Subset(ss[ii,],"l3_16_"))
    ah<-t(Subset(ss[ii,],"l3_17_"))
    ai<-t(Subset(ss[ii,],"l3_cp_"))
    aj<-t(Subset(ss[ii,],"l3_18_"))
    ak<-t(Subset(ss[ii,],"l13_19_"))
    al<-t(Subset(ss[ii,],"l13_19a_"))
    am<-t(Subset(ss[ii,],"l3_20_"))
    an<-t(Subset(ss[ii,],"l3_21_"))
    ao<-t(Subset(ss[ii,],"l3_22_"))
    ap<-t(Subset(ss[ii,],"l3_23_"))
    aq<-t(Subset(ss[ii,],"l3_24_"))
    ar<-t(Subset(ss[ii,],"l3_25_"))
    as<-t(Subset(ss[ii,],"l3_25a_"))
    at<-t(Subset(ss[ii,],"l3_26_"))
    au<-t(Subset(ss[ii,],"l3_27_"))
    av<-t(Subset(ss[ii,],"l3_28_"))
    aw<-t(Subset(ss[ii,],"l3_29_"))
    ax<-t(Subset(ss[ii,],"l3_ch_"))
    ay<-t(Subset(ss[ii,],"l3_30_"))
    az<-t(Subset(ss[ii,],"l3_30a_"))
    ba<-t(Subset(ss[ii,],"l3_31_"))
    bb<-t(Subset(ss[ii,],"l3_32_"))
    bc<-t(Subset(ss[ii,],"l3_33_"))
    bd<-t(Subset(ss[ii,],"l3_34_"))
   
    
    to_check<-list(ID,a,b,c,cc,d,e,f,g,h,i,iii,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,za,zb,zc,zd,ze,zf,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az,ba,bb,bc,bd)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,cc,d,e,f,g,h,i,iii,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,za,zb,zc,zd,ze,zf,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az,ba,bb,bc,bd)
  
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "L.3  ID_Chacra",
                   "L.3 Cultivo",
                   "L.3 Cultivo (Especifique)",
                   "¿Usa fertilizantes?",
                   "L.3 Nombre del fertilizante 1  (si es organico especificar si es estiercol, residuo organico. Si es quimico nombre del producto)",
                   "L.3 Tipo de fertilizante 1",
                   "L.3 Tipo de fertilizante 1 (Especifique)",
                   "L.3 Cantidad por mes del fertilzante 1",
                   "L.3 Unidad",
                   "L.3 Costo por Unidad del fertilizante 1 (Pesos)",
                   "L.3 En los ultimos 12 meses cuantas veces aplico el fertilizante 1",
                   "¿Usa un segundo fertilizante?",
                   "L.3 Nombre del fertilizante 2  (si es organico especificar si es estiercol, residuo organico. Si es quimico nombre del producto)",
                   "L.3 Tipo de fertilizante 2",
                   "L.3 Tipo de fertilizante 2 (Especifique)",
                   "L.3 Cantidad por mes del fertilzante 2",
                   "L.3 Unidad",
                   "L.3 Costo por Unidad del fertilizante 2 (Pesos)",
                   "L.3 En los ultimos 12 meses cuantas veces aplico el fertilizante 2",
                   "L.3 ¿Tuvo plagas?",
                   "L.3 ¿Cuáles plagas?",
                   "Cogollero",
                   "Moniliasis",
                   "Escoba de bruja",
                   "Chinches",
                   "Mosca blanca",
                   "Pulgones",
                   "Racha",
                   "Mancha foliar de palmito",
                   "Acaros",
                   "Antracnosis",
                   "Fusarium",
                   "Nematodos",
                   "Otro (especificar)",
                   "L.3 ¿Cuáles plagas? (Especifique)",
                   "L.3 Nombre del plaguicida 1",
                   "L.3 Tipo del plaguicida 1",
                   "L.3 Tipo del plaguicida 1 (Especifique)",
                   "L.3 Cantidad por mes del plaguicida 1",
                   "L.3 Unidad",
                   "L.3 Costo por Unidad del plaguicida 1 (Pesos)",
                   "L.3 En los ultimos 12 meses cuantas veces aplico el plaguicida 1",
                   "¿Usa un segundo plaguicida?",
                   "L.3 Nombre del plaguicida 2",
                   "L.3 Tipo del plaguicida 2",
                   "L.3 Tipo del plaguicida 2 (Especifique)",
                   "L.3 Cantidad por mes del plaguicida 2",
                   "L.3 Unidad",
                   "L.3 Costo por Unidad del plaguicida 2 (Pesos)",
                   "L.3 En los ultimos 12 meses cuantas veces aplico el plaguicida 2",
                   "L.3 ¿Realiza control de malezas?",
                   "L.3 Tipo del herbicida 1",
                   "L.3 Tipo del herbicida 1 (Especifique)",
                   "L.3 Cantidad por mes del herbicida 1",
                   "L.3 Unidad",
                   "L.3 Costo por Unidad del  herbicida 1 (Pesos)",
                   "L.3 En los ultimos 12 meses cuantas veces aplico el herbicida 1",
                   "¿Usa un segundo herbicida?",
                   "L.3 Tipo del herbicida 2",
                   "L.3 Tipo del herbicida 2 (Especifique)",
                   "L.3 Cantidad por mes del herbicida 2",
                   "L.3 Unidad",
                   "L.3 Costo por Unidad del  herbicida 2 (Pesos)",
                   "L.3 En los ultimos 12 meses cuantas veces aplico el herbicida 2"
                  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_L3",".xls"),row.names=F,showNA = F)
  save(M_B,file=paste0(date_dir,"/","MODULO_L3",".RData"))
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-L3_FUNCTION(rep_dir,ins_dir,date_S)
