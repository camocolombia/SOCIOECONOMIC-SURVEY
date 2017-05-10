H2_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  
  
  
  
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_H2.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #length(Subset(ss,"g_1_1_"))
  #ii=1
  
  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"h2_2")))))
    a<-t(Subset(ss[ii,],"h2_1_"))#;a<-a[1:length(Subset(ss,"h1a_")),]
    b<-t(Subset(ss[ii,],"h2_2_"))
    c<-t(Subset(ss[ii,],"h2_3_1_"))
    d<-t(Subset(ss[ii,],"h2_3_2_"))
    e<-t(Subset(ss[ii,],"h2_3_3_"))
    f<-t(Subset(ss[ii,],"h2_3_4_"))
    g<-t(Subset(ss[ii,],"h2_3_5_"))
    h<-t(Subset(ss[ii,],"h2_3_6_"))
    i<-t(Subset(ss[ii,],"h2_3_6a_"))
    j<-t(Subset(ss[ii,],"h2_4_1_"));j<-t(t(j[1:length(Subset(ss,"h2_2_")),]))
    k<-t(Subset(ss[ii,],"h2_4_1_1_"))
    l<-t(Subset(ss[ii,],"h2_4_1_2_"))
    m<-t(Subset(ss[ii,],"h2_4_1_3_"))
    n<-t(Subset(ss[ii,],"h2_4_1_4_"))
    o<-t(Subset(ss[ii,],"h2_4_1_5_"))
    p<-t(Subset(ss[ii,],"h2_4_1_6_"))
    q<-t(Subset(ss[ii,],"h2_4_1_7_"))
    r<-t(Subset(ss[ii,],"h2_4_1_8_"))
    s<-t(Subset(ss[ii,],"h2_4_1_9_"))
    t<-t(Subset(ss[ii,],"h2_4_1_10_"))
    u<-t(Subset(ss[ii,],"h2_4_1a_"))
    v<-t(Subset(ss[ii,],"h2_4_2_"));v<-t(t(v[1:length(Subset(ss,"h2_3_1_")),]))
    w<-t(Subset(ss[ii,],"h2_4_2_1_"))
    x<-t(Subset(ss[ii,],"h2_4_2_2_"))
    y<-t(Subset(ss[ii,],"h2_4_2_3_"))
    z<-t(Subset(ss[ii,],"h2_4_2_4_"))
    aa<-t(Subset(ss[ii,],"h2_4_2_5_"))
    ab<-t(Subset(ss[ii,],"h2_4_2_6_"))
    ac<-t(Subset(ss[ii,],"h2_4_2_7_"))
    ad<-t(Subset(ss[ii,],"h2_4_2_8_"))
    ae<-t(Subset(ss[ii,],"h2_4_2_9_"))
#     af<-t(Subset(ss[ii,],"h2_4_2_10_"))
#     ag<-t(Subset(ss[ii,],"h2_4_2_11_"))
    ah<-t(Subset(ss[ii,],"h2_4_2a_"))
    ai<-t(Subset(ss[ii,],"h2_4_3_"));ai<-t(t(v[1:length(Subset(ss,"h2_3_1_")),]))
    aj<-t(Subset(ss[ii,],"h2_4_3_1_"))
    ak<-t(Subset(ss[ii,],"h2_4_3_2_"))
    al<-t(Subset(ss[ii,],"h2_4_3_3_"))
    am<-t(Subset(ss[ii,],"h2_4_3_4_"))
    an<-t(Subset(ss[ii,],"h2_4_3_5_"))
    ao<-t(Subset(ss[ii,],"h2_4_3_6_"))
    ap<-t(Subset(ss[ii,],"h2_4_3_7_"))
    aq<-t(Subset(ss[ii,],"h2_4_3_8_"))
    ar<-t(Subset(ss[ii,],"h2_4_3_9_"))
    as<-t(Subset(ss[ii,],"h2_4_3_10_"))
    at<-t(Subset(ss[ii,],"h2_4_3_11_"))
    au<-t(Subset(ss[ii,],"h2_4_3a_"))
    av<-t(Subset(ss[ii,],"h2_4_4_"))
    aw<-t(Subset(ss[ii,],"h2_4_5_"));aw<-t(t(aw[1:length(Subset(ss,"h2_3_1_")),]))
    ax<-t(Subset(ss[ii,],"h2_4_5_1_"))
    ay<-t(Subset(ss[ii,],"h2_4_5_2_"))
    az<-t(Subset(ss[ii,],"h2_4_5_3_"))
    ba<-t(Subset(ss[ii,],"h2_4_5_4_"))
    bb<-t(Subset(ss[ii,],"h2_4_5_5_"))
    bc<-t(Subset(ss[ii,],"h2_4_5_6_"))
    bd<-t(Subset(ss[ii,],"h2_4_5_7_"))
    be<-t(Subset(ss[ii,],"h2_4_5_8_"))
    bf<-t(Subset(ss[ii,],"h2_4_5_9_"))
    bg<-t(Subset(ss[ii,],"h2_4_5_10_"))
    bh<-t(Subset(ss[ii,],"h2_4_5a_"))
    bi<-t(Subset(ss[ii,],"h2_4_6_"));bi<-t(t(v[1:length(Subset(ss,"h2_3_1_")),]))
    bj<-t(Subset(ss[ii,],"h2_4_6_1_"))
    bk<-t(Subset(ss[ii,],"h2_4_6_2_"))
    bl<-t(Subset(ss[ii,],"h2_4_6_3_"))
    bm<-t(Subset(ss[ii,],"h2_4_6_4_"))
    bn<-t(Subset(ss[ii,],"h2_4_6_5_"))
    bo<-t(Subset(ss[ii,],"h2_4_6_6_"))
    bp<-t(Subset(ss[ii,],"h2_4_6_7_"))
    bq<-t(Subset(ss[ii,],"h2_4_6_8_"))
    br<-t(Subset(ss[ii,],"h2_4_6_9_"))
    
    bra<-t(Subset(ss[ii,],"h2_4_6_10_"))
    brb<-t(Subset(ss[ii,],"h2_4_6_11_"))
    brc<-t(Subset(ss[ii,],"h2_4_6_12_"))
    brd<-t(Subset(ss[ii,],"h2_4_6_13_"))
    bre<-t(Subset(ss[ii,],"h2_4_6_14_"))
    
    bu<-t(Subset(ss[ii,],"h2_4_6a_"))
    bv<-t(Subset(ss[ii,],"h2_4_7_"));bv<-t(t(bv[1:length(Subset(ss,"h2_3_1_")),]))
    bw<-t(Subset(ss[ii,],"h2_4_7_1_"))
    bx<-t(Subset(ss[ii,],"h2_4_7_2_"))
    by<-t(Subset(ss[ii,],"h2_4_7_3_"))
    
    bya<-t(Subset(ss[ii,],"h2_4_7_4_"))    
    byb<-t(Subset(ss[ii,],"h2_4_7_5_"))    
    byc<-t(Subset(ss[ii,],"h2_4_7_6_")) 
    
    bz<-t(Subset(ss[ii,],"h2_4_7a_"))
    ca<-t(Subset(ss[ii,],"h2_4_8_"));ca<-t(t(ca[1:length(Subset(ss,"h2_3_1_")),]))
    cb<-t(Subset(ss[ii,],"h2_4_8_1_"))
    cc<-t(Subset(ss[ii,],"h2_4_8_2_"))
    cd<-t(Subset(ss[ii,],"h2_4_8_3_"))
    ce<-t(Subset(ss[ii,],"h2_4_8a_"))
    cf<-t(Subset(ss[ii,],"h2_5_"));cf<-t(t(cf[1:length(Subset(ss,"h2_3_1_")),]))
    cg<-t(Subset(ss[ii,],"h2_5a_"))
    ch<-t(Subset(ss[ii,],"h2_6_"))
    ci<-t(Subset(ss[ii,],"h2_7_"))
    
    
    
    
    
    
    to_check<-list(ID,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,bra,brb,brc,brd,bre,bu,bv,bw,bx,by,bya,byb,byc,bz,ca,cb,cc,cd,ce,cf,cg,ch,ci)
    
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,aa,ab,ac,ad,ae,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,as,at,au,av,aw,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,bra,brb,brc,brd,bre,bu,bv,bw,bx,by,bya,byb,byc,bz,ca,cb,cc,cd,ce,cf,cg,ch,ci)
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "Actividades relacionadas al uso de recursos naturales",
                   "¿Realiza la actividad?",
                   "Solo si realiza la actividad: Indicar frecuencia (Bosque)",
                   "Solo si realiza la actividad: Indicar frecuencia (Rastrojo)",
                   "Solo si realiza la actividad: Indicar frecuencia (Área de cultivos)",
                   "Solo si realiza la actividad: Indicar frecuencia (Huerta)",
                   "Solo si realiza la actividad: Indicar frecuencia (Área de pasturas)",
                   "Solo si realiza la actividad: Indicar frecuencia (Otros)",
                   "Solo si realiza la actividad: Indicar frecuencia (Otros (Especifique))",
                   "¿Qué especies? [opción multiple] Pregunta 1",
                   "¿Qué especies?  Pregunta 1 (Capirona)",
                   "¿Qué especies?  Pregunta 1 (Guaba)",
                   "¿Qué especies?  Pregunta 1 (Ocuera)",
                   "¿Qué especies?  Pregunta 1 (Palisangre)",
                   "¿Qué especies?  Pregunta 1 (Papelillo)",
                   "¿Qué especies?  Pregunta 1 (Quillobordón)",
                   "¿Qué especies?  Pregunta 1 (Yanavara)",
                   "¿Qué especies?  Pregunta 1 (Shimbillo)",
                   "¿Qué especies?  Pregunta 1 (Tahuari)",
                   "¿Qué especies?  Pregunta 1 (Otros)",
                   "¿Qué especies?  Pregunta 1 (Otros (Especifique))",
                   "¿Qué especies? [opción multiple] Pregunta 2",
                   "¿Qué especies?  Pregunta 2 (Boquichico)",
                   "¿Qué especies?  Pregunta 2 (Palometa)",
                   "¿Qué especies?  Pregunta 2 (Bujurqui)",
                   "¿Qué especies?  Pregunta 2 (Fasaco)",
                   "¿Qué especies?  Pregunta 2 (Lisa)",
                   "¿Qué especies?  Pregunta 2 (Paco)",
                   "¿Qué especies?  Pregunta 2 (Gamitama)",
                   "¿Qué especies?  Pregunta 2 (Doncella)",
                   "¿Qué especies?  Pregunta 2 (Otros)",				   
                   "¿Qué especies?  Pregunta 2 (Otros (Especifique))",
                   "¿Qué especies? [opción multiple] Pregunta 3",
                   "¿Qué especies?  Pregunta 3 (Malva)",
                   "¿Qué especies?  Pregunta 3 (Paico)",
                   "¿Qué especies?  Pregunta 3 (Ajo sacha)",
                   "¿Qué especies?  Pregunta 3 (Piñon)",
                   "¿Qué especies?  Pregunta 3 (Uña de gato)",
                   "¿Qué especies?  Pregunta 3 (Verbena)",
                   "¿Qué especies?  Pregunta 3 (Chiricsanango)",
                   "¿Qué especies?  Pregunta 3 (Chuchuhuasi)",
                   "¿Qué especies?  Pregunta 3 (Hierba Luisa)",
                   "¿Qué especies?  Pregunta 3 (Llanten)",
                   "¿Qué especies?  Pregunta 3 (Otros)",				   
                   "¿Qué especies?  Pregunta 3 (Otros (Especifique))",
                   "¿Qué especies?  Preguntas (4,9-15)",
                   "¿Qué especies? [opción multiple] Pregunta 5",
                   "¿Qué especies?  Pregunta 5 (Majaz)",
                   "¿Qué especies?  Pregunta 5 (Añuje)",
                   "¿Qué especies?  Pregunta 5 (Perdiz)",
                   "¿Qué especies?  Pregunta 5 (Panguana)",
                   "¿Qué especies?  Pregunta 5 (Sajino)",
                   "¿Qué especies?  Pregunta 5 (Lagarto)",
                   "¿Qué especies?  Pregunta 5 (Venado)",
                   "¿Qué especies?  Pregunta 5 (Paujil)",
                   "¿Qué especies?  Pregunta 5 (Otros)",				   
                   "¿Qué especies?  Pregunta 5 (Otros (Especifique))",                 
                   "¿Qué especies? [opción multiple] Pregunta 6",
                   "¿Qué especies?  Pregunta 6 (Zapote)",
                   "¿Qué especies?  Pregunta 6 (Mango)",
                   "¿Qué especies?  Pregunta 6 (Guaba)",
                   "¿Qué especies?  Pregunta 6 (Mandarina)",
                   "¿Qué especies?  Pregunta 6 (Palta)",
                   "¿Qué especies?  Pregunta 6 (Pijuayo)",
                   "¿Qué especies?  Pregunta 6 (Taperiba)",
                   "¿Qué especies?  Pregunta 6 (Aguaje)",
                   "¿Qué especies?  Pregunta 6 (Huito)",				   
                   "¿Qué especies?  Pregunta 6 (Ungurahui)",
                   "¿Qué especies?  Pregunta 6 (Pan del árbol)",
                   "¿Qué especies?  Pregunta 6 (Umari)",				   
                   "¿Qué especies?  Pregunta 6 (Caimito)",
                   "¿Qué especies?  Pregunta 6 (Otros)",				   
                   "¿Qué especies?  Pregunta 6 (Otros (Especifique))",
                   "¿Qué especies? [opción multiple] Pregunta 7",
                   "¿Qué especies?  Pregunta 7 (Guisador)",
                   "¿Qué especies?  Pregunta 7 (Sacha culantro)",
                   "¿Qué especies?  Pregunta 7 (Caihua)",
                   "¿Qué especies?  Pregunta 7 (Oregano)",
                   "¿Qué especies?  Pregunta 7 (Achiote)",
                   "¿Qué especies?  Pregunta 7 (Otros)",				   
                   "¿Qué especies?  Pregunta 7 (Otros (Especifique))", 
                   "¿Qué especies? [opción multiple] Pregunta 8",
                   "¿Qué especies?  Pregunta 8 (Suri)",
                   "¿Qué especies?  Pregunta 8 (Hormiga siquisapas)",
                   "¿Qué especies?  Pregunta 8 (Otros)",				   
                   "¿Qué especies?  Pregunta 8 (Otros (Especifique))",				 
                   "Propósito",
                   "Propósito (Otro, especificar)",
                   "Quiénes son los principales responsables de llevar a cabo las actividades? [Opcion multiple] [ID_persona]",
                   "Indicar si  es una actividad importante para la generación de ingresos de la familia"
                   
  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_H2",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-H2_FUNCTION(rep_dir,ins_dir,date_S)