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
                   "�Realiza la actividad?",
                   "Solo si realiza la actividad: Indicar frecuencia (Bosque)",
                   "Solo si realiza la actividad: Indicar frecuencia (Rastrojo)",
                   "Solo si realiza la actividad: Indicar frecuencia (�rea de cultivos)",
                   "Solo si realiza la actividad: Indicar frecuencia (Huerta)",
                   "Solo si realiza la actividad: Indicar frecuencia (�rea de pasturas)",
                   "Solo si realiza la actividad: Indicar frecuencia (Otros)",
                   "Solo si realiza la actividad: Indicar frecuencia (Otros (Especifique))",
                   "�Qu� especies? [opci�n multiple] Pregunta 1",
                   "�Qu� especies?  Pregunta 1 (Capirona)",
                   "�Qu� especies?  Pregunta 1 (Guaba)",
                   "�Qu� especies?  Pregunta 1 (Ocuera)",
                   "�Qu� especies?  Pregunta 1 (Palisangre)",
                   "�Qu� especies?  Pregunta 1 (Papelillo)",
                   "�Qu� especies?  Pregunta 1 (Quillobord�n)",
                   "�Qu� especies?  Pregunta 1 (Yanavara)",
                   "�Qu� especies?  Pregunta 1 (Shimbillo)",
                   "�Qu� especies?  Pregunta 1 (Tahuari)",
                   "�Qu� especies?  Pregunta 1 (Otros)",
                   "�Qu� especies?  Pregunta 1 (Otros (Especifique))",
                   "�Qu� especies? [opci�n multiple] Pregunta 2",
                   "�Qu� especies?  Pregunta 2 (Boquichico)",
                   "�Qu� especies?  Pregunta 2 (Palometa)",
                   "�Qu� especies?  Pregunta 2 (Bujurqui)",
                   "�Qu� especies?  Pregunta 2 (Fasaco)",
                   "�Qu� especies?  Pregunta 2 (Lisa)",
                   "�Qu� especies?  Pregunta 2 (Paco)",
                   "�Qu� especies?  Pregunta 2 (Gamitama)",
                   "�Qu� especies?  Pregunta 2 (Doncella)",
                   "�Qu� especies?  Pregunta 2 (Otros)",				   
                   "�Qu� especies?  Pregunta 2 (Otros (Especifique))",
                   "�Qu� especies? [opci�n multiple] Pregunta 3",
                   "�Qu� especies?  Pregunta 3 (Malva)",
                   "�Qu� especies?  Pregunta 3 (Paico)",
                   "�Qu� especies?  Pregunta 3 (Ajo sacha)",
                   "�Qu� especies?  Pregunta 3 (Pi�on)",
                   "�Qu� especies?  Pregunta 3 (U�a de gato)",
                   "�Qu� especies?  Pregunta 3 (Verbena)",
                   "�Qu� especies?  Pregunta 3 (Chiricsanango)",
                   "�Qu� especies?  Pregunta 3 (Chuchuhuasi)",
                   "�Qu� especies?  Pregunta 3 (Hierba Luisa)",
                   "�Qu� especies?  Pregunta 3 (Llanten)",
                   "�Qu� especies?  Pregunta 3 (Otros)",				   
                   "�Qu� especies?  Pregunta 3 (Otros (Especifique))",
                   "�Qu� especies?  Preguntas (4,9-15)",
                   "�Qu� especies? [opci�n multiple] Pregunta 5",
                   "�Qu� especies?  Pregunta 5 (Majaz)",
                   "�Qu� especies?  Pregunta 5 (A�uje)",
                   "�Qu� especies?  Pregunta 5 (Perdiz)",
                   "�Qu� especies?  Pregunta 5 (Panguana)",
                   "�Qu� especies?  Pregunta 5 (Sajino)",
                   "�Qu� especies?  Pregunta 5 (Lagarto)",
                   "�Qu� especies?  Pregunta 5 (Venado)",
                   "�Qu� especies?  Pregunta 5 (Paujil)",
                   "�Qu� especies?  Pregunta 5 (Otros)",				   
                   "�Qu� especies?  Pregunta 5 (Otros (Especifique))",                 
                   "�Qu� especies? [opci�n multiple] Pregunta 6",
                   "�Qu� especies?  Pregunta 6 (Zapote)",
                   "�Qu� especies?  Pregunta 6 (Mango)",
                   "�Qu� especies?  Pregunta 6 (Guaba)",
                   "�Qu� especies?  Pregunta 6 (Mandarina)",
                   "�Qu� especies?  Pregunta 6 (Palta)",
                   "�Qu� especies?  Pregunta 6 (Pijuayo)",
                   "�Qu� especies?  Pregunta 6 (Taperiba)",
                   "�Qu� especies?  Pregunta 6 (Aguaje)",
                   "�Qu� especies?  Pregunta 6 (Huito)",				   
                   "�Qu� especies?  Pregunta 6 (Ungurahui)",
                   "�Qu� especies?  Pregunta 6 (Pan del �rbol)",
                   "�Qu� especies?  Pregunta 6 (Umari)",				   
                   "�Qu� especies?  Pregunta 6 (Caimito)",
                   "�Qu� especies?  Pregunta 6 (Otros)",				   
                   "�Qu� especies?  Pregunta 6 (Otros (Especifique))",
                   "�Qu� especies? [opci�n multiple] Pregunta 7",
                   "�Qu� especies?  Pregunta 7 (Guisador)",
                   "�Qu� especies?  Pregunta 7 (Sacha culantro)",
                   "�Qu� especies?  Pregunta 7 (Caihua)",
                   "�Qu� especies?  Pregunta 7 (Oregano)",
                   "�Qu� especies?  Pregunta 7 (Achiote)",
                   "�Qu� especies?  Pregunta 7 (Otros)",				   
                   "�Qu� especies?  Pregunta 7 (Otros (Especifique))", 
                   "�Qu� especies? [opci�n multiple] Pregunta 8",
                   "�Qu� especies?  Pregunta 8 (Suri)",
                   "�Qu� especies?  Pregunta 8 (Hormiga siquisapas)",
                   "�Qu� especies?  Pregunta 8 (Otros)",				   
                   "�Qu� especies?  Pregunta 8 (Otros (Especifique))",				 
                   "Prop�sito",
                   "Prop�sito (Otro, especificar)",
                   "Qui�nes son los principales responsables de llevar a cabo las actividades? [Opcion multiple] [ID_persona]",
                   "Indicar si  es una actividad importante para la generaci�n de ingresos de la familia"
                   
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