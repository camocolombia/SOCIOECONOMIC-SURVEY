
L5_2_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_L5_2.dta"))#,generate.factors=T,convert.factors = T,convert.dates = T,missing.type = F,replace.strl=T);gc()
  
  #ii=1
  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"l1_")))));ID<-t(t(ID[1:length(Subset(ss,"l1_1_")),]))
    aaa<-t(Subset(ss[ii,],"l1_"));aaa<-t(t(aaa[1:length(Subset(ss,"l1_1_")),]))
    a<-t(Subset(ss[ii,],"l1_1_"))
    b<-t(Subset(ss[ii,],"l1_1a_"))
#     c<-t(Subset(ss[ii,],"l1_2_"))
#     d<-t(Subset(ss[ii,],"l1_3_"))
#     e<-t(Subset(ss[ii,],"l1_4_1_"))
#     f<-t(Subset(ss[ii,],"l1_4_2_"))
#     g<-t(Subset(ss[ii,],"l1_5_"))
#     h<-t(Subset(ss[ii,],"l1_6_"))
#     i<-t(Subset(ss[ii,],"l1_7_"))
#     j<-t(Subset(ss[ii,],"l1_8_"))
#     k<-t(Subset(ss[ii,],"l1_9_"))
#     kk<-t(Subset(ss[ii,],"l1_9a_"))
#     l<-t(Subset(ss[ii,],"l1_10_"))
#     m<-t(Subset(ss[ii,],"l1_10a_"))
#     n<-t(Subset(ss[ii,],"l1_11_"))
#     o<-t(Subset(ss[ii,],"l1_11a_"))
#     p<-t(Subset(ss[ii,],"l1_12_"))   
#     q<-t(Subset(ss[ii,],"l1_13_"))
#     qq<-t(Subset(ss[ii,],"l1_13a_"))
#     r<-t(Subset(ss[ii,],"l1_14_"))
#     s<-t(Subset(ss[ii,],"l1_15_"))
#     t<-t(Subset(ss[ii,],"l1_16_"))   
#     u<-t(Subset(ss[ii,],"l1_17_"))
#     v<-t(Subset(ss[ii,],"l1_18_")) 
#     w<-t(Subset(ss[ii,],"l1_19_")) ;w<-t(t(w[1:length(Subset(ss,"l1_1_")),]))
#     x<-t(Subset(ss[ii,],"l1_19_1_"))
#     y<-t(Subset(ss[ii,],"l1_19_2_"))
#     z<-t(Subset(ss[ii,],"l1_19_3_"))
#     aa<-t(Subset(ss[ii,],"l1_19_4_"))
#     ab<-t(Subset(ss[ii,],"l1_19_5_"))
#     ac<-t(Subset(ss[ii,],"l1_19_6_"))
#     ad<-t(Subset(ss[ii,],"l1_19_7_"))
#     ae<-t(Subset(ss[ii,],"l1_19_8_"))
#     af<-t(Subset(ss[ii,],"l1_19_9_"))
#     ag<-t(Subset(ss[ii,],"l1_19_10_"))
#     ah<-t(Subset(ss[ii,],"l1_19_10a_"))
#     ai<-t(Subset(ss[ii,],"l5_1_"))
#     aj<-t(Subset(ss[ii,],"l5_2_"))
#     ak<-t(Subset(ss[ii,],"l5_3_"))
#     al<-t(Subset(ss[ii,],"l5_4_")) ;al<-t(t(al[1:length(Subset(ss,"l1_1_")),]))
#     am<-t(Subset(ss[ii,],"l5_4a_"))
#     an<-t(Subset(ss[ii,],"l5_4_1_"))
#     ao<-t(Subset(ss[ii,],"l5_4_2_"))
#     ap<-t(Subset(ss[ii,],"l5_4_2a_"))
#     ar<-t(Subset(ss[ii,],"l5_5_"))
#     as<-t(Subset(ss[ii,],"l5_6_"))
#     at<-t(Subset(ss[ii,],"l5_6a_"))
#     au<-t(Subset(ss[ii,],"l5_7_"))
#     av<-t(Subset(ss[ii,],"l5_8_"))
#     aw<-t(Subset(ss[ii,],"l5_8a_"))
#     aww<-t(Subset(ss[ii,],"l5_c_c2_"))#;aww<-t(t(aww[1:length(Subset(ss,"l1_1_")),]))
#     ax<-t(Subset(ss[ii,],"l5_9_"))
#     ay<-t(Subset(ss[ii,],"l5_10_"));ay<-t(t(ay[1:length(Subset(ss,"l1_1_")),]))
#     az<-t(Subset(ss[ii,],"l5_10a_"))
#     ba<-t(Subset(ss[ii,],"l5_10_1_"))
#     bb<-t(Subset(ss[ii,],"l5_10_2_"))
#     bc<-t(Subset(ss[ii,],"l5_10_2a_"))
#     bd<-t(Subset(ss[ii,],"l5_11_"))
#     be<-t(Subset(ss[ii,],"l5_12_"))
#     bf<-t(Subset(ss[ii,],"l5_12a_"))
#     bg<-t(Subset(ss[ii,],"l5_13_"))
#     bh<-t(Subset(ss[ii,],"l5_14_"))
#     bi<-t(Subset(ss[ii,],"l5_14a_"))
    bj<-t(Subset(ss[ii,],"l5_c1_"))
    bk<-t(Subset(ss[ii,],"l5_15_"))
    bl<-t(Subset(ss[ii,],"l5_16_"))
    bm<-t(Subset(ss[ii,],"l5_17_"))
    bn<-t(Subset(ss[ii,],"l5_17a_"))
    bo<-t(Subset(ss[ii,],"l5_18_"))
    bp<-t(Subset(ss[ii,],"l5_18a_"))
    bq<-t(Subset(ss[ii,],"l5_19_"))
    br<-t(Subset(ss[ii,],"l5_19a_"))
    bs<-t(Subset(ss[ii,],"l5_20_"))
    bt<-t(Subset(ss[ii,],"l520a_"))
    bu<-t(Subset(ss[ii,],"l5_21_"))
    bv<-t(Subset(ss[ii,],"l5_22_"))
    bw<-t(Subset(ss[ii,],"l5_23_"))
    bx<-t(Subset(ss[ii,],"l5_24_"))
    by<-t(Subset(ss[ii,],"l5_25_"));by<-t(t(by[1:length(Subset(ss,"l1_1_")),]))
    bz<-t(Subset(ss[ii,],"l5_25_1_"))
    ca<-t(Subset(ss[ii,],"l5_25_2_"))
    cb<-t(Subset(ss[ii,],"l5_25_3_"))
    cc<-t(Subset(ss[ii,],"l5_25_4_"))
    cd<-t(Subset(ss[ii,],"l5_25_5_"))
    ce<-t(Subset(ss[ii,],"l5_25_6_"))
    cf<-t(Subset(ss[ii,],"l5_25a_"))
    
    to_check<-list(ID,aaa,a,b,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bw,bx,by,bz,ca,cb,cc,cd,ce,cf)
                   #,ai,aj,ak,al,am,an,ao,ap,ar,as,at,au,av,aw,aww,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,
                   
    
                   #c,d,e,f,g,h,i,j,k,kk,l,m,n,o,p,q,qq,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,af,ag,ah  
    to_check<-do.call(cbind,to_check)
    rm(ID,aaa,a,b,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bw,bx,by,bz,ca,cb,cc,cd,ce,cf)
       #ai,aj,ak,al,am,an,ao,ap,ar,as,at,au,av,aw,aww,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,
      
    
    
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "L.1  ID_Chacra",
                   "L.1 Cultivo",
                   "L.1 Cultivo (Especifique)",
#                    "L.1 Área dedica al cultivo (Ha)",
#                    "L.1 ¿Quién decide sembrar.cultivo ID? [opción múltiple]     ID persona",
#                    "L.1 ¿En que mes empezó la siembra?",
#                    "L.1 ¿En que año empezó la siembra?",
#                    "L.1 ¿Cuantas plantas tiene sembradas?",
#                    "L.1 ¿Cuantas plantas estan en etapa productiva?",
#                    "L.1 ¿Cuál es la distancia entre cada planta?",
#                    "L.1 ¿Cuál es la distancia entre los surcos?",
#                    "L.1 Unidad",
#                    "L.1 Unidad (Especificar)",
#                    "L.1 ¿Cuál fue la fuente del agua para el riego?",
#                    "L.1 ¿Cuál fue la fuente del agua para el riego? (especificar)",
#                    "L.1 ¿Cuál fue el tipo de riego?",
#                    "L.1 ¿Cuál fue el tipo de riego? (especificar)",
#                    "L.1 ¿Cuál variedad sembró o tiene establecida?",
#                    "L.1 ¿Cómo obtuvo esa variedad?",
#                    "L.1 ¿Cómo obtuvo esa variedad? (especificar)",
#                    "L.1 ¿Si es COMPRADA,Qué cantidad de semillas compro? (Cantidad)",
#                    "L.1 ¿Si es COMPRADA,Qué cantidad de semillas compro? (Unidad)",
#                    "L.1 ¿Si es COMPRADA,Qué cantidad de semillas compro? (Precio por Unidad)",
#                    "L.1 ¿La variedad de semilla usada es resistente a las sequias, inundaciones, plagas o enfermedades?  1.Si 2No",
#                    "L.1 ¿El cultivo esta asociado con otros?",
#                    "L.1 ¿Que cultivos sembrados en asociación con el cultivo? [Opción múltiple]",
#                    "Yuca",
#                    "Platano",
#                    "Frijol",
#                    "Maiz",
#                    "Cacao",
#                    "Palma Africana",
#                    "Caña",
#                    "Caucho",
#                    "Piña",
#                    "Otro (especificar)",
#                    "L.1 ¿Que cultivos sembrados en asociación con el cultivo? (Especifique)",
#                    "L.5 ¿Cuántas cosechas (Principales) tuvo durante los ultimos 12 meses?",
#                    "L.5 ¿Quién decide cuándo cosechar? [opción múltiple]    ID persona",
#                    "L.5 Total de la cosecha 1",
#                    "L.5 Total de la cosecha 1 (Unidad)",
#                    "L.5 Total de la cosecha 1 (Unidad) (Especifique)",
#                    "L.5 Venta de la cosecha 1",
#                    "L.5 Venta de la cosecha 1 (Unidad)",
#                    "L.5 Venta de la cosecha 1 (Unidad) (Especifique)",
#                    "L.5 Auto-consumo de la cosecha 1",
#                    "L.5 Auto-consumo de la cosecha 1 (Unidad)",
#                    "L.5 Auto-consumo de la cosecha 1 (Unidad) (Especifique)",
#                    "L.5 Otros(semilla,etc.) de la cosecha 1",
#                    "L.5 Otros(semilla,etc.) de la cosecha 1 (Unidad)",
#                    "L.5 Otros(semilla,etc.) de la cosecha 1 (Unidad) (Especifique)",
#                    "Tuvo una cosecha 2",
#                    "L.5 Total de la cosecha 2",
#                    "L.5 Total de la cosecha 2 (Unidad)",
#                    "L.5 Total de la cosecha 2 (Unidad) (Especifique)",
#                    "L.5 Venta de la cosecha 2",
#                    "L.5 Venta de la cosecha 2 (Unidad)",
#                    "L.5 Venta de la cosecha 2 (Unidad) (Especifique)",
#                    "L.5 Auto-consumo de la cosecha 2",
#                    "L.5 Auto-consumo de la cosecha 2 (Unidad)",
#                    "L.5 Auto-consumo de la cosecha 2 (Unidad) (Especifique)",
#                    "L.5 Otros(semilla,etc.) de la cosecha 2",
#                    "L.5 Otros(semilla,etc.) de la cosecha 2 (Unidad)",
#                    "L.5 Otros(semilla,etc.) de la cosecha 2 (Unidad) (Especifique)",
                   "¿Vendió el producto",
                   "L.5 ¿Quién realizó la venta? [opción múltiple]    ID persona",
                   "L.5 ¿Cuál fue el Precio de venta durante los ultimos 12 meses?  (Precio promedio)",
                   "L.5 ¿Cuál fue el Precio de venta durante los ultimos 12 meses? (Unidad)",
                   "L.5 ¿Cuál fue el Precio de venta durante los ultimos 12 meses? (Especifique)",
                   "L.5 ¿A quien le vendio?",
                   "L.5 ¿A quien le vendio? (especifique)",
                   "L.5 ¿Donde realizo la venta?",
                   "L.5 ¿Donde realizo la venta? (especifique)",
                   "L.5 ¿Si lo vendio fuera de su finca, utilizo algún medio de transporte para realizar la venta?",
                   "L.5 ¿Si lo vendio fuera de su finca, utilizo algún medio de transporte para realizar la venta? (especifique)",
                   "L.5 ¿Cuanto le cuesta en promedio cada viaje? ($Pesos)",
                   "L.5 ¿Que número de viajes necesito para vender el total del producto(#)",
                   "L.5 ¿Quién decide en qué gastar el dinero que se genera con...? (Ver ID)",
                   "L.5 ¿Considera que este producto presenta mayores obstaculos para su comercializacion?",
                   "L.5 ¿Cuales? [opción múltiple]",
                   "Falta de vias de acceso para sacar los productos",
                   "Falta de compradores",
                   "Precio",
                   "Calidad",
                   "Se produce demasiado en la zona",
                   "Otro (especificar)",
                  "L.5 ¿Cuales (Especificar)?" 
  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_L5_2",".xls"),row.names=F,showNA = F)
  save(M_B,file=paste0(date_dir,"/","MODULO_L5_2",".RData"))
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-L5_FUNCTION(rep_dir,ins_dir,date_S)
