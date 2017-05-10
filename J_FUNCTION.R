
J_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  
  OPTIONS<-toupper(letters)

  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_J.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #length(Subset(ss,"g_1_1_"))
  
#ii=1  
  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"j1_0")))))
    aa<-t(Subset(ss[ii,],"j1_0_"))
    a<-t(Subset(ss[ii,],"j1_1_"))
    b<-t(Subset(ss[ii,],"j1_2_"))
    c<-t(Subset(ss[ii,],"j1_3_"));c<-t(t(c[1:length(Subset(ss,"j1_0")),]))
    d<-t(Subset(ss[ii,],"j1_3_1_"))
    e<-t(Subset(ss[ii,],"j1_3_2_"))
    f<-t(Subset(ss[ii,],"j1_3_3_"))
    g<-t(Subset(ss[ii,],"j1_3_4_"))
    h<-t(Subset(ss[ii,],"j1_3_5_"))
    i<-t(Subset(ss[ii,],"j1_3_6_"))
    j<-t(Subset(ss[ii,],"j1_3_7_"))
    l<-t(Subset(ss[ii,],"j1_3_8_"))
    m<-t(Subset(ss[ii,],"j1_3_9_"))
    n<-t(Subset(ss[ii,],"j1_3_10_"))
    o<-t(Subset(ss[ii,],"j1_3_11_"))
    p<-t(Subset(ss[ii,],"j1_3_12_"))
    q<-t(Subset(ss[ii,],"j1_4_"))
    r<-t(Subset(ss[ii,],"j1_5_"))
    
    to_check<-list(ID,aa,a,b,c,d,e,f,g,h,i,j,l,m,n,o,p,q,r)
    to_check<-do.call(cbind,to_check)
    rm(ID,aa,a,b,c,d)
    
    
    for(i in 1:nrow(to_check)){
      
      kkk<-strsplit(to_check[i,5],"")
      if (length(grep("A", kkk[[1]])>0)){
        to_check[i,6]<-"Si"
      }else{to_check[i,6]<-"No"}
      
      if (length(grep("B",kkk[[1]])>0)){
        to_check[i,7]<-"Si"
      }else{to_check[i,7]<-"No"}
      if (length(grep("C",kkk[[1]])>0)){
        to_check[i,8]<-"Si"
      }else{to_check[i,8]<-"No"}
      if (length(grep("D",kkk[[1]])>0)){
        to_check[i,9]<-"Si"
      }else{to_check[i,9]<-"No"}
      if (length(grep("E",kkk[[1]])>0)){
        to_check[i,10]<-"Si"
      }else{to_check[i,10]<-"No"}
      if (length(grep("F",kkk[[1]])>0)){
        to_check[i,11]<-"Si"
      }else{to_check[i,11]<-"No"}
      if (length(grep("G",kkk[[1]])>0)){
        to_check[i,12]<-"Si"
      }else{to_check[i,12]<-"No"}
      if (length(grep("H",kkk[[1]])>0)){
        to_check[i,13]<-"Si"
      }else{to_check[i,13]<-"No"}
      if (length(grep("I",kkk[[1]])>0)){
        to_check[i,14]<-"Si"
      }else{to_check[i,14]<-"No"}
      if (length(grep("J",kkk[[1]])>0)){
        to_check[i,15]<-"Si"
      }else{to_check[i,15]<-"No"}
      if (length(grep("K",kkk[[1]])>0)){
        to_check[i,16]<-"Si"
      }else{to_check[i,16]<-"No"}
      if (length(grep("L",kkk[[1]])>0)){
        to_check[i,17]<-"Si"
      }else{to_check[i,17]<-"No"}
    };rm(i)
    
   ####
    
    
      ###
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "J.1  ¿Quién? ID persona",
                   "J.1 Tipo de ingreso",
                   "J.1 Ingreso mensual",
                   "J.1 En que meses recibio el ingreso",
                   "J.1. Enero",
                   "J.1. Febrero",
                   "J.1. Marzo",
                   "J.1. Abril",
                   "J.1. Mayo",
                   "J.1. Junio",
                   "J.1. Julio",
                   "J.1. Agosto",
                   "J.1. Septiembre",
                   "J.1. Octubre",
                   "J.1. Noviembre",
                   "J.1. Diciembre",
                   "J.1. ¿Recibió pago en especie?",
                   "J.1. ¿En cuánto estima el valor de ese pago en especie"
  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_J",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-J_FUNCTION(rep_dir,ins_dir,date_S)