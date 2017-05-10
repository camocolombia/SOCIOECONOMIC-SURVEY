
L2_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_L2.dta"))#,generate.factors=T,convert.factors = T,convert.dates = T,missing.type = F,replace.strl=T);gc()
  
  #ii=1
  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"l2_1_")))))#;ID<-t(t(ID[1:length(Subset(ss,"l2_1_")),]))
    
        a<-t(Subset(ss[ii,],"l2_1_"))
        b<-t(Subset(ss[ii,],"l2_2_"));b<-t(t(b[1:length(Subset(ss,"l2_1_")),]))
        c<-t(Subset(ss[ii,],"l2_2a_"))
        cc<-t(Subset(ss[ii,],"l2_2_1_"))
        d<-t(Subset(ss[ii,],"l2_3_"))
        e<-t(Subset(ss[ii,],"l2_4_"))
        f<-t(Subset(ss[ii,],"l2_5_"))
        g<-t(Subset(ss[ii,],"l2_6_"))
        h<-t(Subset(ss[ii,],"l2_7_"))
        i<-t(Subset(ss[ii,],"l2_8_"));i<-t(t(i[1:length(Subset(ss,"l2_1_")),]))
        j<-t(Subset(ss[ii,],"l2_8_1_"))
        k<-t(Subset(ss[ii,],"l2_9_"))
        kk<-t(Subset(ss[ii,],"l2_10_"))
        l<-t(Subset(ss[ii,],"l2_11_"));l<-t(t(l[1:length(Subset(ss,"l2_1_")),]))
        m<-t(Subset(ss[ii,],"l2_11_1_"))
        n<-t(Subset(ss[ii,],"l2_11_2_"))
        o<-t(Subset(ss[ii,],"l2_11_3_"))
        p<-t(Subset(ss[ii,],"l2_11_4_"))   
        q<-t(Subset(ss[ii,],"l2_11a_"))
        r<-t(Subset(ss[ii,],"l2_12_"))
        s<-t(Subset(ss[ii,],"l2_13_"))
    to_check<-list(ID,a,b,c,cc,d,e,f,g,h,i,j,k,kk,l,m,n,o,p,q,r,s)
    to_check<-do.call(cbind,to_check)
    rm(ID,a,b,c,cc,d,e,f,g,h,i,j,k,kk,l,m,n,o,p,q,r,s)
    
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "L.2  ID_Chacra",
                   "L.2 Cultivo",
                   "L.2 Cultivo (Especifique)",
                   "L.2 ¿Realiza Tumba y quema? 1.Si 2.No",
                   "L.2 ¿Cual es la frecuencia de quemas para preparar terreno? (# veces)",
                   "L.2 ¿Rota cultivos?",
                   "L.2 Si respondio que si, ¿con cuál cultivo?",
                   "L.2 ¿Hace trazados en los lotes agricolas?",
                   "L.2 ¿Hace barbecho? (Dejar descansar la tierra sin sembrar)",
                   "L.2 ¿Cuánto tiempo deja descansar la tierra? (meses)",
                   "L.2 ¿Tiene cultivos de cobertura?",
                   "L.2 Si respondio que si, ¿con cuál cúltivo de cobertura?",
                   "L.2 ¿Realiza arados?",
                   "L.2 ¿Con que realiza el arado? (Opción multiple)",
                   "Tractor",
                   "Manual",
                   "Animal",
                   "Otro (especificar)",
                   "L.2 ¿Con que realiza el arado? (Especifique)",
                   "L.2 Si usa tractor, (Horas trabajadas/año)",
                   "L.2 ¿Encala el terreno?"
                   )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_L2",".xls"),row.names=F,showNA = F)
  save(M_B,file=paste0(date_dir,"/","MODULO_L2",".RData"))
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-L2_FUNCTION(rep_dir,ins_dir,date_S)
