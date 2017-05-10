
J3_FUNCTION<-function(rep_dir,ins_dir,date_S){
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  Subset <- function(df, pattern) {
    ind <- grepl(pattern, names(df))
    df[, ind]
  }
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_J3.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()
  
  
  
  #length(Subset(ss,"g_1_1_"))
  
  
#ii=1  
  M_B<-list()
  
  for(ii in 1:nrow(ss)){
    
    
    ID<-t(t(rep(ss[ii,1],length(Subset(ss,"j3_1_")))))
    aa<-t(Subset(ss[ii,],"j3_1_"))
    a<-t(Subset(ss[ii,],"j3_2_"));a<-t(t(a[1:length(Subset(ss,"j3_1_")),]))
    b<-t(Subset(ss[ii,],"j3_2_1_"))
    c<-t(Subset(ss[ii,],"j3_2_2_"))
    d<-t(Subset(ss[ii,],"j3_2_3_"))
    e<-t(Subset(ss[ii,],"j3_2_4_"))
    f<-t(Subset(ss[ii,],"j3_2_5_"))
    g<-t(Subset(ss[ii,],"j3_2_6_"))
    h<-t(Subset(ss[ii,],"j3_2_7_"))
    i<-t(Subset(ss[ii,],"j3_2_8_"))
    j<-t(Subset(ss[ii,],"j3_2_9_"))
    k<-t(Subset(ss[ii,],"j3_2_10_"))
    l<-t(Subset(ss[ii,],"j3_2_11_"))
    m<-t(Subset(ss[ii,],"j3_2_12_"))
    n<-t(Subset(ss[ii,],"j3_3_"))
    o<-t(Subset(ss[ii,],"j3_4_"))
    p<-t(Subset(ss[ii,],"j3_4a_"))   
    q<-t(Subset(ss[ii,],"j3_5_"))
   # r<-t(Subset(ss[ii,],"j3_c1_"))
    
    
    to_check<-list(ID,aa,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
    to_check<-do.call(cbind,to_check)
    rm(ID,aa,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q)
    
    
    
    ######################
    for(i in 1:nrow(to_check)){
      
      kkk<-strsplit(to_check[i,3],"")
      
      if (length(grep("A", kkk[[1]])>0)){
        to_check[i,4]<-"Si"
      }else{to_check[i,4]<-"No"}
      
      if (length(grep("B",kkk[[1]])>0)){
        to_check[i,5]<-"Si"
      }else{to_check[i,5]<-"No"}
      
      if (length(grep("C",kkk[[1]])>0)){
        to_check[i,6]<-"Si"
      }else{to_check[i,6]<-"No"}
      
      if (length(grep("D",kkk[[1]])>0)){
        to_check[i,7]<-"Si"
      }else{to_check[i,7]<-"No"}
      
      if (length(grep("E",kkk[[1]])>0)){
        to_check[i,8]<-"Si"
      }else{to_check[i,8]<-"No"}
      
      if (length(grep("F",kkk[[1]])>0)){
        to_check[i,9]<-"Si"
      }else{to_check[i,9]<-"No"}
      
      if (length(grep("G",kkk[[1]])>0)){
        to_check[i,10]<-"Si"
      }else{to_check[i,10]<-"No"}
      
      if (length(grep("H",kkk[[1]])>0)){
        to_check[i,11]<-"Si"
      }else{to_check[i,11]<-"No"}
      
      if (length(grep("I",kkk[[1]])>0)){
        to_check[i,12]<-"Si"
      }else{to_check[i,12]<-"No"}
      
      if (length(grep("J",kkk[[1]])>0)){
        to_check[i,13]<-"Si"
      }else{to_check[i,13]<-"No"}
      
      if (length(grep("K",kkk[[1]])>0)){
        to_check[i,14]<-"Si"
      }else{to_check[i,14]<-"No"}
      
      if (length(grep("L",kkk[[1]])>0)){
        to_check[i,15]<-"Si"
      }else{to_check[i,15]<-"No"}
    };rm(i)
    
    
    
    
    #########################
    
    
    M_B[[ii]]<-to_check
    
  };rm(ii)
  
  
  M_B<-do.call(rbind,M_B)
  
  
  
  colnames(M_B)<-c("ID_HOUSE",
                   "J.3  Actividad",
                   "J.3. En que meses lo recibio",
                   "J.3. Enero",
                   "J.3. Febrero",
                   "J.3. Marzo",
                   "J.3. Abril",
                   "J.3. Mayo",
                   "J.3. Junio",
                   "J.3. Julio",
                   "J.3. Agosto",
                   "J.3. Septiembre",
                   "J.3. Octubre",
                   "J.3. Noviembre",
                   "J.3. Diciembre",
                   "¿Cuál fue el monto concedido total? Para subsidios en especies estime el valor monetario? [pesos]",
                   "¿Quién o que entidad se lo otorgo?",
                   "¿Quién o que entidad se lo otorgo? (Especifique)",
                   "¿Quién lo recibio? ID Persona"
                   #"¿Desea agregar un nuevo registro?"
                   
  )
  
  # write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
  
  write.xlsx (M_B,paste0(date_dir,"/","MODULO_J3",".xls"),row.names=F,showNA = F)
  
  rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-J3_FUNCTION(rep_dir,ins_dir,date_S)