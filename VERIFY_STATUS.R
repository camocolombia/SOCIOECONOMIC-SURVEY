options(java.home="C:\\Program Files\\Java\\jre1.8.0_101")

INDEX_FUNCTION<-function(VOL,rep_dir,INS,date_S,out_dir){
  require(xlsx);require(dplyr)

  cat("Processing ",as.character(INS),"\n")
#####################################################################################
#CALLING NA REPLACE FUNCTION 
#####################################################################################  
  
  empty_as_na <- function(x){
    if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
    ifelse(as.character(x)!="", x, NA)
  }
  
 
  
#####################################################################################
#CALLING PATHS 
#####################################################################################
  
rep_dir2<-paste0(VOL,"/",rep_dir)
ins_dir<-paste0(rep_dir2,"/",INS) 
date_dir<-paste0(ins_dir,"/",date_S)

EXCEL_FILES_DIR<-paste0(date_dir,"/",rep_dir,"/",INS,"/",date_S,"/","EXCEL_FILES")
EXCEL_FILES<-list.files(EXCEL_FILES_DIR,full.names = T)
EXCEL_FILES_NAME<-list.files(EXCEL_FILES_DIR,full.names = F);
EXCEL_FILES<-EXCEL_FILES[-23];EXCEL_FILES_NAME<-EXCEL_FILES_NAME[-23];sub(".xls","",EXCEL_FILES_NAME)


setwd(date_dir);unzip(paste0(date_dir,"/","out_EXCEL_USE_THIS.zip"))

##########################################################################################################################################################################
##########################################################################################################################################################################
####SECCION A
##########################################################################################################################################################################
##########################################################################################################################################################################
cat("checking section A","\n")

#####################################################################################
#CHECK MODULE A (MANDATORY)
#####################################################################################
cat("checking Module A","\n")
a<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_A.xls"),sheetIndex=1,header=T)
a<-a %>% mutate_each(funs(empty_as_na)) 

a$INDEX<-1
a2<-data.frame(matrix(nrow=nrow(a),ncol=3))
a2[,1]<-a$ID_HOUSE;a2[,2]<-a2[,2]<-1
a2[,3]<-"Aceptable";rm(a)

a2<-a2[which(!duplicated(a2[,1])),]
row.names(a2)<-1:nrow(a2)
#####################################################################################
#CHECK MODULE B (MANDATORY)
#####################################################################################
cat("checking Module B","\n")
b<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_B.xls"),sheetIndex=1,header=T)
b<-b %>% mutate_each(funs(empty_as_na)) 
b<-b[which(!duplicated(b)),]
#b$INDEX<-NA
b2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
b2[,1]<-a2$X1;

for(i in 1:length(a2$X1)){
bdummy<-b[which(b$ID_HOUSE==as.character(a2$X1[[i]])),]
bdummy <- bdummy[!is.na(bdummy),]

if(sum(bdummy$B.1..Estado.civil =="Casado",na.rm=T)>1 | (sum(bdummy$B.1..Estado.civil =="Conviviente o en uniÃ³n libre",na.rm=T)>1) |  (sum(bdummy$B.1..Estado.civil =="Soltero",na.rm=T)!=0)|(sum(bdummy$B.1..Estado.civil =="Viudo",na.rm=T)!=0)|(sum(bdummy$B.1..Estado.civil =="Separado",na.rm=T)!=0)|(sum(bdummy$B.1..Estado.civil =="Otro (especificar)",na.rm=T)!=0)){

  b2[i,2]<-1  
  b2[i,3]<-"Aceptable"
      }else{
b2[i,2]<-0.5 ##si el estado marital esta incorrecto (ej: dice casado o union libre y solo aparece un registro)  
b2[i,3]<-"Chequear estado marital"
  }
};rm(i,bdummy);gc()

#####################################################################################
#CHECK MODULE C (MANDATORY)
#####################################################################################
cat("checking Module C","\n")
c<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_C.xls"),sheetIndex=1,header=T)
c<-c %>% mutate_each(funs(empty_as_na)) 
c<-c[which(!duplicated(c)),]

#c4<-replace(c[,4],"",NA)
c2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
c2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  
if(sum(is.na(c[i,c(2,3,5,11,12)]))!=0){
  c2[i,2]<-0.5
  c2[i,3]<-"Revisar variables"
  
}else{
  c2[i,2]<-1
  c2[i,3]<-"Aceptable"
    }
};rm(i,c);gc()

#####################################################################################
#CHECK MODULE D (MANDATORY)
#####################################################################################
cat("checking Module D","\n")
d<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_D.xls"),sheetIndex=1,header=T)
d<-d %>% mutate_each(funs(empty_as_na)) 
d<-d[which(!duplicated(d)),]

d2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
d2[,1]<-a2$X1
for(i in 1:length(a2$X1)){
  
if(sum(!is.na(d[i,-c(1,3,8,10,12,20,28,39,46,60)]))<49){
  d2[i,2]<-0.5
  d2[i,3]<-"Revisar variables"
      }else{
  d2[i,2]<-1
  d2[i,3]<-"Aceptable"
    }
  };rm(i,d)


#####################################################################################
#CHECK MODULE E
#####################################################################################
cat("checking Module E","\n")
e<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_E.xls"),sheetIndex=1,header=T)
e<-e %>% mutate_each(funs(empty_as_na)) 
e<-e[which(!duplicated(e)),]

e2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
e2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  edummy<-e[which(e$ID_HOUSE==as.character(a2$X1[[i]])),]
  edummy[,3]<-as.numeric(edummy[,3])
  
  if(sum(edummy[,3]!=0,na.rm=T)==sum(!is.na(edummy[,4]))){
    
    e2[i,2]<-1  
    e2[i,3]<-"Aceptable"
    }else{
      e2[i,2]<-0.75 
      e2[i,3]<-"El numero de objetos y dueños no coinciden"
  }
};rm(i,edummy,e);gc()

#####################################################################################
#CHECK MODULE F
#####################################################################################
cat("checking Module F","\n")
f<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_F.xls"),sheetIndex=1,header=T)
f<-f%>% mutate_each(funs(empty_as_na)) 
f<-f[which(!duplicated(f)),]

f2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
f2[,1]<-a2$X1


for(i in 1:length(a2$X1)){
if(f[,2][[i]]<15 | sum(!is.na(f[i,]))==17){
  f2[i,2]<-1 
  f2[i,3]<-"Aceptable"
  } else{
    f2[i,2]<-0.75 
    f2[i,3]<-"Verificar"
  }
};rm(i,f)

#####################################################################################
#CHECK MODULE G (MANDATORY)
#####################################################################################
cat("checking Module G","\n")
g<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_G.xls"),sheetIndex=1,header=T)
g<-g%>% mutate_each(funs(empty_as_na)) 
g<-g[which(!duplicated(g)),]

g2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
g2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  gdummy<-g[which(g$ID_HOUSE==as.character(a2$X1[[i]])),]
  gdummy<-gdummy[!is.na(gdummy$Tipo.de.comida),]
  if(sum(!is.na(unique(gdummy$Tipo.de.comida)))==4){
    g2[i,2]<-1  
    g2[i,3]<-"Aceptable"
      }else{
    g2[i,2]<-0.5 
    g2[i,3]<-"El numero de comidas o de donde viene la comida debe verificarse"
  }
};rm(i,gdummy,g);gc()

#####################################################################################
#CHECK MODULE G2 (MANDATORY)
#####################################################################################
cat("checking Module G2","\n")
g_1<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_G2.xls"),sheetIndex=1,header=T)
g_1<-g_1%>% mutate_each(funs(empty_as_na)) 
g_1<-g_1[which(!duplicated(g_1)),]

g2_1<-data.frame(matrix(nrow=nrow(a2),ncol=3))
g2_1[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  gdummy<-g_1[which(g_1$ID_HOUSE==as.character(a2$X1[[i]])),]
  bdum<-b[which(b$ID_HOUSE==as.character(a2$X1[[i]])),]
  bdum[,7]<-as.numeric(bdum[,7])
  gdummy_2<-gdummy[11:17,]
  
  if(any(bdum[,7]<18,na.rm=T)){
    
    if(sum(gdummy_2$Intensidad=="No aplica")<7){
       g2_1[i,2]<-1 
      g2_1[i,3]<-"Aceptable"
      
  }else{
     g2_1[i,2]<-0.5  
     g2_1[i,3]<-"Valores extraños, revisar"
      }
  }else{
    g2_1[i,2]<-0.75 
    g2_1[i,3]<-"Aceptable" 
    
    }
  };rm(i,gdummy,g_1,bdum,gdummy_2);gc()

#####################################################################################
#CHECK MODULE G3 (MANDATORY)
#####################################################################################
cat("checking Module G3","\n")
g_3<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_G3.xls"),sheetIndex=1,header=T)
g_3<-g_3%>% mutate_each(funs(empty_as_na)) 
g_3<-g_3[which(!duplicated(g_3)),]


g3_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
g3_2[,1]<-a2$X1


for(i in 1:length(a2$X1)){
  g3_2dum<-g_3[which(g_3$ID_HOUSE==as.character(a2$X1[[i]])),]

  if(g3_2dum[,2]=="No"){
    if(sum(g3_2dum[,4:15]=="Si",na.rm=T)!=0){
      
      g3_2[i,2]<-0.5
      g3_2[i,3]<-"Esta marcado G.6. ¿Hubo algún mes dentro de los últimos doce en los que no tuvieron suficient como NO, pero hay valores en los meses, verificar"
    }else{
      g3_2[i,2]<-1
      g3_2[i,3]<-"Aceptable"
    }
  }else{
    g3_2[i,2]<-1
    g3_2[i,3]<-"Aceptable"
      }
    };rm(i,g3_2dum,g_3);gc()

#####################################################################################
#CHECK MODULE H
#####################################################################################
cat("checking Module H","\n")
h<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_H.xls"),sheetIndex=1,header=T)
h<-h%>% mutate_each(funs(empty_as_na)) 
h<-h[which(!duplicated(h)),]

h_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
h_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  hdum<-h[which(h$ID_HOUSE==as.character(a2$X1[[i]])),]
  hdum2<-hdum[which(hdum[,4]=="No"),]
  hdum3<-hdum[which(hdum[,6]=="No lleva a cabo la actividad"),]
  
  if(sum(!is.na(hdum2[,5]),na.rm=T)>0 | (sum(hdum3[,4]=="Si",na.rm=T)>0)){
    h_2[i,2]<-0.75
    h_2[i,3]<-"Hay algun dato que dice que no realiza la actividad y aparece ID de persona o realiza la actividad pero en proposito dice no realizarla (Verificar)"
    
  }else{
    h_2[i,2]<-1
    h_2[i,3]<-"Aceptable"
    
  }
};rm(i,hdum,hdum2,hdum3,h);gc()

#####################################################################################
#CHECK MODULE H2
#####################################################################################
cat("checking Module H2 =1","\n")
h_2_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
h_2_2[,1]<-a2$X1
h_2_2[,2]<-1
h_2_2[,3]<-"Aceptable"

#####################################################################################
#CHECK MODULE H3
#####################################################################################
cat("checking Module H3 =1","\n")
h_3_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
h_3_2[,1]<-a2$X1
h_3_2[,2]<-1
h_3_2[,3]<-"Aceptable"

#####################################################################################
#CHECK MODULE H4
#####################################################################################
cat("checking Module H4 =1","\n")
h_4_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
h_4_2[,1]<-a2$X1
h_4_2[,2]<-1
h_4_2[,3]<-"Aceptable"


#####################################################################################
#CHECK MODULE I
#####################################################################################
cat("checking Module I =1","\n")
i_D<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_I.xls"),sheetIndex=1,header=T)
i_D<-i_D%>% mutate_each(funs(empty_as_na)) 
i_D<-i_D[which(!duplicated(i_D)),]


i_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
i_2[,1]<-a2$X1
i_2[,2]<-1
i_2[,3]<-"Aceptable"

#####################################################################################
#CHECK MODULE I2
#####################################################################################
cat("checking Module I2","\n")
i_2a<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_I2.xls"),sheetIndex=1,header=T)
i_2a<-i_2a%>% mutate_each(funs(empty_as_na)) 
i_2a<-i_2a[which(!duplicated(i_2a)),]

i_2_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
i_2_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){

  i2_dum<-i_2a[which(i_2a$ID_HOUSE==as.character(a2$X1[[i]])),]
if(sum(!is.na(i2_dum[,-1]))>0){
  i_2_2[i,2]<-1
  i_2_2[i,3]<-"Aceptable"
}else{
  i_2_2[i,2]<-1
  i_2_2[i,3]<-"Aceptable (Salto el modulo)" 
  }
};rm(i_2a,i2_dum,i)  
#####################################################################################
#CHECK MODULE I3
#####################################################################################
cat("checking Module I3","\n")
i_3<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_I3.xls"),sheetIndex=1,header=T)
i_3<-i_3%>% mutate_each(funs(empty_as_na)) 
i_3<-i_3[which(!duplicated(i_3)),]


i_3_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
i_3_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  
  i3_dum<-i_3[which(i_3$ID_HOUSE==as.character(a2$X1[[i]])),]
  
  if(sum(!is.na(i3_dum[,-1]))>0){
    i_3_2[i,2]<-1
    i_3_2[i,3]<-"Aceptable"
  }else{
    i_3_2[i,2]<-1
    i_3_2[i,3]<-"Aceptable (Salto el modulo)" 
  }
};rm(i_3,i3_dum,i);gc()  

#####################################################################################
#CHECK MODULE J1
#####################################################################################
cat("checking Module J1","\n")

j<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_J.xls"),sheetIndex=1,header=T)
j<-j%>% mutate_each(funs(empty_as_na)) 
j<-j[which(!duplicated(j)),]


j_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
j_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  
  j_dum<-j[which(j$ID_HOUSE==as.character(a2$X1[[i]])),]
  
  if(sum(j_dum[,6:17]=="No",na.rm = T)<36){
    j_2[i,2]<-1
    j_2[i,3]<-"Aceptable"
  }else{
    j_2[i,2]<-1
    j_2[i,3]<-"Aceptable (Salto el modulo)" 
  }
};rm(j,j_dum,i);gc()  

#####################################################################################
#CHECK MODULE J2 (MANDATORY)
#####################################################################################
cat("checking Module J2","\n")

j2<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_J2.xls"),sheetIndex=1,header=T)
j2<-j2%>% mutate_each(funs(empty_as_na)) 
j2<-j2[which(!duplicated(j2)),]

j_2_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
j_2_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  
  
  
  j2_dum<-j2[which(j2$ID_HOUSE==as.character(a2$X1[[i]])),]
  
  if(sum(!is.na(j2_dum[,-1])>0)){
    j_2_2[i,2]<-1
    j_2_2[i,3]<-"Aceptable"
  }else{
    j_2_2[i,2]<-1
    j_2_2[i,3]<-"Aceptable (Salto el modulo)" 
  }
};rm(j2,j2_dum,i);gc()

#####################################################################################
#CHECK MODULE J3 (MANDATORY)
#####################################################################################
cat("checking Module J3","\n")

j3<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_J3.xls"),sheetIndex=1,header=T)
j3<-j3%>% mutate_each(funs(empty_as_na)) 
j3<-j3[which(!duplicated(j3)),]


j_3_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
j_3_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){

  j3_dum<-j3[which(j3$ID_HOUSE==as.character(a2$X1[[i]])),]
  
  if(sum(j3_dum[,c(4:15)]=="No")<36){
    
    if(any(j3_dum[,16]==0,na.rm=T)){
      
        j_3_2[i,2]<-0.5
        j_3_2[i,3]<-"Hay un dato de monto concedido en 0, revisar"
        
      }else{
        j_3_2[i,2]<-1
        j_3_2[i,3]<-"Aceptable (Salto el modulo)" 
      
  }
    }else{
    
    if(any(j3_dum[,16]==0,na.rm=T)){
    
      j_3_2[i,2]<-0.5
      j_3_2[i,3]<-"Hay un dato de monto concedido en 0, revisar"
      
    }else{
      j_3_2[i,2]<-1
      j_3_2[i,3]<-"Aceptable (Salto el modulo)" 
      }
    }
  };rm(j3,j3_dum,i);gc()

#####################################################################################
#CHECK MODULE C 
#####################################################################################
cat("checking C =1","\n")

c2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
c2[,1]<-a2$X1
c2[,2]<-1
c2[,3]<-"Aceptable"

#####################################################################################
#CALCULATING SECTION A SCORES
#####################################################################################
cat("Calculating Indexes for SECTION A","\n")

SECT_A_SCORES<-cbind(a2[,1:2],b2[,2],c2[,2],d2[,2],e2[,2],f2[,2],g2[,2],g2_1[,2],g3_2[,2],h_2[,2],
              h_2_2[,2],h_3_2[,2],h_4_2[,2],i_2[,2],i_2_2[,2],i_3_2[,2],j_2[,2],j_2_2[,2],j_3_2[,2],c2[,2])
SECT_A_STATUS<-cbind(a2[,1:2],b2[,3],c2[,3],d2[,3],e2[,3],f2[,3],g2[,3],g2_1[,3],g3_2[,3],h_2[,3],
              h_2_2[,3],h_3_2[,3],h_4_2[,3],i_2[,3],i_2_2[,3],i_3_2[,3],j_2[,3],j_2_2[,3],j_3_2[,3],c2[,3])

colnames(SECT_A_SCORES)<-c("ID_HOUSE","MODULO_A","MODULO_B","MODULO_C","MODULO_D","MODULO_E","MODULO_F","MODULO_G","MODULO_G2","MODULO_G3","MODULO_H","MODULO_H2","MODULO_H3","MODULO_H4","MODULO_I","MODULO_I2","MODULO_I3","MODULO_J","MODULO_J2","MODULO_J3","C")
colnames(SECT_A_STATUS)<-colnames(SECT_A_SCORES)

MAND_A_SCORES<-SECT_A_SCORES[,c("ID_HOUSE","MODULO_A","MODULO_B","MODULO_C","MODULO_D","MODULO_G","MODULO_G2","MODULO_G3","MODULO_J2","MODULO_J3")]
MAND_A_STATUS<-SECT_A_STATUS[,c("ID_HOUSE","MODULO_A","MODULO_B","MODULO_C","MODULO_D","MODULO_G","MODULO_G2","MODULO_G3","MODULO_J2","MODULO_J3")]

OPT_A_SCORES<-SECT_A_SCORES[,c("ID_HOUSE","MODULO_E","MODULO_F","MODULO_H","MODULO_H2","MODULO_H3","MODULO_H4","MODULO_I","MODULO_I2","MODULO_I3","MODULO_J","MODULO_C")]
OPT_A_STATUS<-SECT_A_STATUS[,c("ID_HOUSE","MODULO_E","MODULO_F","MODULO_H","MODULO_H2","MODULO_H3","MODULO_H4","MODULO_I","MODULO_I2","MODULO_I3","MODULO_J","MODULO_C")]

IND_A<-data.frame(cbind(MAND_A_SCORES[,1],MAND_A_SCORES[,2],MAND_A_SCORES[,3]));
IND_A[,2]<-NA;IND_A[,3]<-NA;IND_A[,4]<-NA;IND_A[,5]<-NA;IND_A[,6]<-NA;IND_A[,7]<-NA;IND_A[,8]<-NA;IND_A[,9]<-NA;IND_A[,10]<-NA

for(i in 1:nrow(IND_A)){
  IND_A[i,2]<-(sum(MAND_A_SCORES[i,-c(1)]))/sum(MAND_A_SCORES[i,-c(1)]>0)*100 #MANDATORIO SCORE
  IND_A[i,3]<-sum(MAND_A_SCORES[i,-c(1)]>0)#MANDATORIO
  IND_A[i,4]<-(sum(OPT_A_SCORES[i,-c(1)]))/sum(OPT_A_SCORES[i,-c(1)]>0)*100 #OPCIONAL SCORE
  IND_A[i,5]<-sum(OPT_A_SCORES[i,-c(1)]>0)  #OPCIONAL
  IND_A[i,6]<-(sum(SECT_A_SCORES[i,-c(1)]))/sum(SECT_A_SCORES[i,-c(1)]>0)*100  #TOTAL SCORE
  IND_A[i,7]<-sum(SECT_A_SCORES[i,-c(1)]>0) #TOTAL
  IND_A[i,8]<-((sum(MAND_A_SCORES[i,-c(1)]))/sum(SECT_A_SCORES[i,-c(1)]>0)*100) #MAND/TOTAL SCORE
  IND_A[i,9]<-((sum(OPT_A_SCORES[i,-c(1)]))/sum(SECT_A_SCORES[i,-c(1)]>0)*100) #OPT /TOTAL SCORE
  
    if(IND_A[i,8]>=40){
    IND_A[i,10]<-"LISTA"
    }else{
      IND_A[i,10]<-"REVISAR"
    }
};rm(i)

colnames(IND_A)<-c("ID_HOUSE","SCORE_MAND","TOTAL_MAND","SCORE_OPT","TOTAL_OPT","SCORE_TOTAL","TOTAL_MODULOS","SCORE_MAND/TOTAL_MODULOS","SCORE_OPT/TOTAL_MODULOS","STATUS")
gc()

##########################################################################################################################################################################
##########################################################################################################################################################################
####SECCION B
##########################################################################################################################################################################
##########################################################################################################################################################################
cat("checking section B","\n")

#####################################################################################
#CHECK MODULE K (MANDATORY)
#####################################################################################
cat("checking Module K","\n")

k<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_K.xls"),sheetIndex=1,header=T)
k<-k%>% mutate_each(funs(empty_as_na)) 
k_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
k_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){

  
  
  k_dum<-k[which(k$ID_HOUSE==as.character(a2$X1[[i]])),]
  k_dum<-k_dum[which(rowSums(is.na(k_dum))!=26),]
  k_dum<-k_dum[which(!duplicated(k_dum[,2])),]
  
  if(any(is.na(k_dum[,3]))|nrow(k_dum)==0){
    k_2[i,2]<-0.5
    k_2[i,3]<-"CHACRA SIN DATOS (REVISAR)"  
  }else if(any(k_dum[,3]=="NO TIENE CHACRA")){
    k_2[i,2]<-1
    k_2[i,3]<-"Aceptable (No hay chacra) o no hay chacra"  
    
  }else{

lat_check<-list();lon_check<-list()
for(j in 1:nrow(k_dum)){
  cat(as.character(a2$X1[[i]]),"and ",j,"\n")
  #if(as.vector(regexpr("°",k_dum[,4]))!=-1 |is.na(k_dum[,4])){
  if(!is.na(k_dum[,4])){
    
    lat_check[[j]]<-1
    lon_check[[j]]<-1
  }else{
    lat_check[[j]]<-0
    lon_check[[j]]<-0  
    
#       lat_check[[j]]<-pmatch (c("N","W","°"," ","° "," °"),k_dum[,4])
#     lon_check[[j]]<-pmatch (c("N","W","°"," ","° "," °"),k_dum[,5])     
        }  

};rm(j)

if(sum((unlist(lon_check)))!=0){
  k_2[i,2]<-1
  k_2[i,3]<-"Aceptable"   
  
}else{

  
  k_2[i,2]<-0.5
  k_2[i,3]<-"No hay coordenadas" 
    }
  }
};rm(i,k_dum,lat_check,lon_check)

#####################################################################################
#CHECK MODULE K2 (MANDATORY)
#####################################################################################
cat("checking Module K2","\n")

k_ma<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_K.xls"),sheetIndex=1,header=T)
k_ma<-k_ma%>% mutate_each(funs(empty_as_na)) 


k2<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_K2.xls"),sheetIndex=1,header=T)
k2<-k2%>% mutate_each(funs(empty_as_na)) 
k_2_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
k_2_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  cat(as.character(a2$X1[[i]]),"\n")
  
  k_dum<-k_ma[which(k_ma$ID_HOUSE==as.character(a2$X1[[i]])),]
  k_dum<-k_dum[which(rowSums(is.na(k_dum))!=26),]
  k_dum<-k_dum[which(!duplicated(k_dum[,2])),]
  
  
  if(any(is.na(k_dum[,3]))|nrow(k_dum)==0){
    k_2_2[i,2]<-0.5
    k_2_2[i,3]<-"CHACRA SIN DATOS (REVISAR)"  
  }else if(any(k_dum[,3]=="NO TIENE CHACRA")){    
    k_2_2[i,2] <-1
    k_2_2[i,3] <-"Aceptable (No hay chacra) o no hay nombre de chacra"  
    
  }else{
    k2_dum<-k2[which(k2$ID_HOUSE==as.character(a2$X1[[i]])),]
    k2_dum<-k2_dum[which(!duplicated(k2_dum[,2])),]
    k2_dum<-k2_dum[which(rowSums(is.na(k2_dum))!=17),]   
    
    if(nrow(k2_dum)==0){  #length(rowSums(k2_dum,na.rm=T)
      k_2_2[i,2] <-0.5
      k_2_2[i,3] <-"ERROR, DEBEN HABER AREAS, SI HAY CHACRA!" 
      
      }else{  
  k2_dum<-k2[which(k2$ID_HOUSE==as.character(a2$X1[[i]])),]
  k2_dum<-k2_dum[which(!duplicated(k2_dum[,2])),]
  k2_dum<-k2_dum[which(rowSums(is.na(k2_dum))!=17),]
  
    for(k in 2:16){
    
    k2_dum[,k]<-as.numeric(as.character(k2_dum[,k]))
          };rm(k)
  
  k2_dum[,18]<-as.numeric(as.character(k2_dum[,18]))
  
  
  k2_dum[k2_dum==-999]<-NA
  k2_dum[k2_dum==-998]<-NA
  k2_dum[k2_dum==-997]<-NA
  
  if(nrow(k2_dum)>1){
    
    k2_list<-list()
    for(j in 1:nrow(k2_dum)){
      
      k2_list[[j]]<-sum(k2_dum[j,c(4,5,6,7,8,9,10,11,12,13,14,16,18)],na.rm=T)!=0
      
    };rm(j)
    k2_list<-as.matrix((unlist(k2_list)))
    if((sum(k2_list[,1]==TRUE)/length(k2_list))==0){
      
      k_2_2[i,2] <-1
      k_2_2[i,3] <-"Aceptable"  
    }else{
      k_2_2[i,2] <-0.5
      k_2_2[i,3] <-"No hay areas descritas para una o varias de las fincas" 
      
    }
  }else{
  
  if(sum(k2_dum[,c(4,5,6,7,8,9,10,11,12,13,14,16,18)],na.rm=T)==0){
    k_2_2[i,2] <-0.5
    k_2_2[i,3] <-"No hay areas descritas"
      }else{
        k_2_2[i,2] <-1
        k_2_2[i,3] <-"Aceptable"    
      }
    }
   }
  }  
};rm(i)#,k2_dum,k2)
gc()
#####################################################################################
#CHECK MODULE L (MANDATORY)
#####################################################################################
cat("checking Module L1,L5","\n")

cat("loading MODULO_L1.xls...","\n")

l1<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_L1.xls"),sheetIndex=1,header=T)
l1<-l1%>% mutate_each(funs(empty_as_na)) 
l1<-l1[which(rowSums(is.na(l1))!=37),]

cat("loading MODULO_L5_1.xls..","\n")
l5_1<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_L5_1.xls"),sheetIndex=1,header=T)
l5_1<-l5_1%>% mutate_each(funs(empty_as_na)) 
l5_1<-l5_1[which(rowSums(is.na(l5_1))!=30),]

cat("loading MODULO_L5_2.xls..","\n")

l5_2<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_L5_2.xls"),sheetIndex=1,header=T)
l5_2<-l5_2%>% mutate_each(funs(empty_as_na)) 
l5_2<-l5_1[which(rowSums(is.na(l5_2))!=26),]

cat("Joining  MODULO_L5..","\n")
l1_5<-cbind(l1,l5_1[,-c(1,2,3,4)],l5_2[,-c(1,2,3,4)])
l1_5<-l1_5[which(rowSums(is.na(l1_5))!=87),]


l1_5_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
l1_5_2[,1]<-a2$X1
gc()


cat("Beginning to process MODULO_L1-L5 routine...","\n")
for(i in 1:length(a2$X1)){
  
  l1_5_dum<-l1_5[which(l1_5$ID_HOUSE==as.character(a2$X1[[i]])),]
  
  l1_5_dum<-l1_5_dum[which(!duplicated(l1_5_dum)),]
  
  if(nrow(l1_5_dum)==0){
    
    l1_5_2[i,2]<-1
    l1_5_2[i,3]<-"Aceptable (Salto el modulo)"  
  }else{
    
l1_5_ot<-subset(l1_5_dum,l1_5_dum[,3]=="Otro (especificar)")
    if(nrow(l1_5_ot)!=0 & sum(as.matrix(any(is.na(l1_5_ot[,4]))))!=0){
      l1_5_2[i,2]<-0.5
      l1_5_2[i,3]<-"Otros especificar esta activado pero no tiene el cultivo (verificar)" 
      
    }else if(sum(as.numeric(l1_5_dum[,5]),na.rm=T)==0){
      l1_5_2[i,2]<-0.5
      l1_5_2[i,3]<-"NO hay area cultivada" 
    }else{
        
      l1_5_2[i,2]<-1
      l1_5_2[i,3]<-"Aceptable"
      }
  }      

};rm(i,l1_5_dum)

gc()
#####################################################################################
#CHECK MODULE L2 (MANDATORY)
#####################################################################################
cat("checking Module L2-L4","\n")

cat("loading MODULO_L2.xls..","\n")

l2<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_L2.xls"),sheetIndex=1,header=T)
l2<-l2%>% mutate_each(funs(empty_as_na)) 
l2<-l2[which(rowSums(is.na(l2))!=21),];gc()

cat("loading MODULO_L3.xls..","\n")
l3<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_L3.xls"),sheetIndex=1,header=T)
l3<-l3%>% mutate_each(funs(empty_as_na)) 
l3<-l3[which(rowSums(is.na(l3))!=64),];gc()

cat("loading MODULO_L4.xls..","\n")
l4<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_L4.xls"),sheetIndex=1,header=T)
l4<-l4%>% mutate_each(funs(empty_as_na)) 
l4<-l4[which(rowSums(is.na(l4))!=25),];gc()


l2_4_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
l2_4_2[,1]<-a2$X1

c11<-nrow(l2)==nrow(l3);c22<-nrow(l3)==nrow(l4);c33<-nrow(l2)==nrow(l4);ct<-c(c11,c22,c33)

if(any(ct==T)){
  
  cat("Joining MODULO_L2, L3, L4 ...","\n")
  
test<-merge(l2,l3,by.x=c("ID_HOUSE","L.2..ID_Chacra","L.2.Cultivo","L.2.Cultivo..Especifique."),by.y =c("ID_HOUSE","L.3..ID_Chacra","L.3.Cultivo","L.3.Cultivo..Especifique.") ,all= T)
l2_4<-merge(test,l4,by.x=c("ID_HOUSE","L.2..ID_Chacra","L.2.Cultivo","L.2.Cultivo..Especifique."),by.y =c("ID_HOUSE","L.4..ID_Chacra","L.4.Cultivo","L.4.Cultivo..Especifique.") ,all= T)

}else{
  cat("Joining MODULO_L2, L3, L4 ...","\n")
  
  l2_4<-cbind(l2,l3,l4)
  
}

cat("Beginning to process MODULO_L2, L3, L4 routine...","\n")

for(i in 1:length(a2$X1)){
  cat(as.character(a2$X1[[i]]),"\n")
  l1_dum<-l1[which(l1$ID_HOUSE==as.character(a2$X1[[i]])),]
  l1_dum<-l1_dum[which(!duplicated(l1_dum)),]
  l1_dum<-l1_dum[which(rowSums(is.na(l1_dum))!=37),]
  
  l2_4_dum<-l2_4[which(l2_4$ID_HOUSE==as.character(a2$X1[[i]])),]
  l2_4_dum<-l2_4_dum[which(!duplicated(l2_4_dum)),]
  
  if(nrow(l1_dum)>nrow(l2_4_dum)){
    
    if(nrow(l2_4_dum)==0){
      
      l2_4_2[i,2]<-1
      l2_4_2[i,3]<-"Aceptable (salto el modulo)"
    }else{
      l2_4_2[i,2]<-0.5
      l2_4_2[i,3]<-"El numero de cultivos reportado es menor que el modulo L"
     }
    }else{
    
      if(nrow(l2_4_dum)==0){
        l2_4_2[i,2]<-1
        l2_4_2[i,3]<-"Aceptable (salto el modulo)"
      }else{
               l2_4_2[i,2]<-1
       l2_4_2[i,3]<-"Aceptable"
      }
    }
  };rm(i,l1_dum,l2_4_dum)

gc()
#rm(l2,l3,l4,l5)
#####################################################################################
#CHECK MODULE M (MANDATORY)
#####################################################################################
cat("checking Module M","\n")

m<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_M.xls"),sheetIndex=1,header=T)
m<-m%>% mutate_each(funs(empty_as_na)) 
m<-m[which(!duplicated(m)),]

m_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
m_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  
  m_dum<-m[which(m$ID_HOUSE==as.character(a2$X1[[i]])),]
  m_dum_2<-m_dum[which(m_dum[,2]=="Saltar"),]
  
  if(nrow(m_dum_2)==0){
    m_2[i,2]<-1
    m_2[i,3]<-"Aceptable"
  }else{
    if(sum(!is.na(m_dum_2[,3:44]))>0){
      
      m_2[i,2]<-0.5
      m_2[i,3]<-"Saltar el modulo de pastos esta activado y hay informacion, verificar"
    }else{
    m_2[i,2]<-1
    m_2[i,3]<-"Aceptable (Salto el modulo)"
     }
   }
  };rm(i,m_dum,m_dum_2,m)

#####################################################################################
#CHECK MODULE M2
#####################################################################################
cat("checking Module M2","\n")

m2<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_M2.xls"),sheetIndex=1,header=T)
m2<-m2%>% mutate_each(funs(empty_as_na)) 
m2<-m2[which(!duplicated(m2)),]

m_2_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
m_2_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  
  m2_dum<-m2[which(m2$ID_HOUSE==as.character(a2$X1[[i]])),]
  m2_dum<-m2_dum[which(rowSums(is.na(m2_dum))!=3),]
  
  
  if(nrow(m2_dum)==0){
    m_2_2[i,2]<-1
    m_2_2[i,3]<-"Aceptable (Salto el modulo)"
    
  }else{
    m_2_2[i,2]<-1
    m_2_2[i,3]<-"Aceptable"
    
  }
  
};rm(i,m2_dum)

#####################################################################################
#CHECK MODULE M3
#####################################################################################
cat("checking Module M3","\n")

m3<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_M3.xls"),sheetIndex=1,header=T)
m3<-m3%>% mutate_each(funs(empty_as_na)) 
m3<-m3[which(!duplicated(m3)),]

m_3_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
m_3_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  
  m3_dum<-m3[which(m3$ID_HOUSE==as.character(a2$X1[[i]])),]
  m3_dum<-m3_dum[which(rowSums(is.na(m3_dum))!=11),]
  
  if(nrow(m3_dum)==0){
    m_3_2[i,2]<-1
    m_3_2[i,3]<-"Aceptable (Salto el modulo)"
    
  }else{
    m_3_2[i,2]<-1
    m_3_2[i,3]<-"Aceptable"
    
  }
  
};rm(i,m3_dum)

#####################################################################################
#CHECK MODULE N1 (MANDATORY)
#####################################################################################
cat("checking Module N1","\n")

n1<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_N1.xls"),sheetIndex=1,header=T)
n1<-n1%>% mutate_each(funs(empty_as_na)) 
n1<-n1[which(!duplicated(n1)),]

n1_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
n1_2[,1]<-a2$X1



for(i in 1:length(a2$X1)){
  
  n1_dum<-n1[which(n1$ID_HOUSE==as.character(a2$X1[[i]])),]
  
  if(nrow(n1_dum)==0){
    n1_2[i,2]<-1
    n1_2[i,3]<-"Aceptable (Salto el modulo)"
    
  }else{
    n1_2[i,2]<-1
    n1_2[i,3]<-"Aceptable"
    
  }
  
};rm(i,n1_dum)

#####################################################################################
#CHECK MODULE N3 (MANDATORY)
#####################################################################################
cat("checking Module N3","\n")

n3<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_N3.xls"),sheetIndex=1,header=T)
n3<-n3%>% mutate_each(funs(empty_as_na)) 
n3<-n3[which(!duplicated(n3)),]

n3_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
n3_2[,1]<-a2$X1


for(i in 1:length(a2$X1)){
  
  n3_dum<-n3[which(n3$ID_HOUSE==as.character(a2$X1[[i]])),]
  n3_dum<-n3_dum[which(rowSums(is.na(n3_dum))!=8),]
  
  n3_dum_2<-n3_dum[which(n3_dum[,4]=="Si" & n3_dum[,7]==0),]
  
  if(nrow(n3_dum)==0){
    
    n3_2[i,2]<-1
    n3_2[i,3]<-"Aceptable (Salto el modulo)"
    
  }else{
    
    if(nrow(n3_dum_2)>0){
      n3_2[i,2]<-0.5
      n3_2[i,3]<-"Eligio que si tenia un animal y el peso promedio es 0 (verificar)" 
      
    }else{
      
      n3_2[i,2]<-1
      n3_2[i,3]<-"Aceptable"  
    }
  }
  
};rm(i,n3_dum_2,n3_dum)

#####################################################################################
#CHECK MODULE N4
#####################################################################################


cat("checking Module N4","\n")

n4<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULOD_N4.xls"),sheetIndex=1,header=T)
n4<-n4%>% mutate_each(funs(empty_as_na)) 
n4<-n4[which(!duplicated(n4)),]

n4_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
n4_2[,1]<-a2$X1


for(i in 1:length(a2$X1)){
  
  n4_dum<-n4[which(n4$ID_HOUSE==as.character(a2$X1[[i]])),]
  n4_dum<-n4_dum[which(rowSums(is.na(n4_dum))!=13),]
  
  if(nrow(n4_dum)==0){
    
    n4_2[i,2]<-1
    n4_2[i,3]<-"Aceptable (Salto el modulo)"
    
  }else{
    
    n4_2[i,2]<-1
    n4_2[i,3]<-"Aceptable"  
    }
};rm(i,n4_dum,n4)

#####################################################################################
#CHECK MODULE N5
#####################################################################################

cat("checking Module N5","\n")

n5<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_N5.xls"),sheetIndex=1,header=T)
n5<-n5%>% mutate_each(funs(empty_as_na)) 
n5<-n5[which(!duplicated(n5)),]

n5_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
n5_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  
  n5_dum<-n5[which(n5$ID_HOUSE==as.character(a2$X1[[i]])),]
  n5_dum<-n5_dum[which(rowSums(is.na(n5_dum))!=20),]
  
  
  if(nrow(n5_dum)==0){
    
    n5_2[i,2]<-1
    n5_2[i,3]<-"Aceptable (Salto el modulo)"
    
  }else{
    
    n5_2[i,2]<-1
    n5_2[i,3]<-"Aceptable"  
  }
};rm(i,n5_dum,n5)


#####################################################################################
#CHECK MODULE N6
#####################################################################################
cat("checking Module N6","\n")

n6<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_N6.xls"),sheetIndex=1,header=T)
n6<-n6%>% mutate_each(funs(empty_as_na)) 
n6<-n6[which(!duplicated(n6)),]


n6_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
n6_2[,1]<-a2$X1

for(i in 1:length(a2$X1)){
  
  n6_dum<-n6[which(n6$ID_HOUSE==as.character(a2$X1[[i]])),]
  n6_dum<-n6_dum[which(rowSums(is.na(n6_dum))!=27),]
  
  if(nrow(n6_dum)==0){
    
    n6_2[i,2]<-1
    n6_2[i,3]<-"Aceptable (Salto el modulo)"
    
  }else{
    
    n6_2[i,2]<-1
    n6_2[i,3]<-"Aceptable"  
  }
};rm(i,n6_dum,n6)

#####################################################################################
#CHECK MODULE N7
#####################################################################################
cat("checking Module N7","\n")

n7<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_N7.xls"),sheetIndex=1,header=T)
n7<-n7%>% mutate_each(funs(empty_as_na)) 
n7<-n7[which(!duplicated(n7)),]


n7_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
n7_2[,1]<-a2$X1


for(i in 1:length(a2$X1)){
  
  n7_dum<-n7[which(n7$ID_HOUSE==as.character(a2$X1[[i]])),]
  n7_dum<-n7_dum[which(rowSums(is.na(n7_dum))!=10),]
  
  if(nrow(n7_dum)==0){
    
    n7_2[i,2]<-1
    n7_2[i,3]<-"Aceptable (Salto el modulo)"
    
  }else{
    
    n7_2[i,2]<-1
    n7_2[i,3]<-"Aceptable"  
  }
};rm(i,n7_dum,n7)


#####################################################################################
#CHECK MODULE N8
#####################################################################################
cat("checking Module N8","\n")

n8<-read.xlsx2(file=paste0(EXCEL_FILES_DIR,"/","MODULO_N8.xls"),sheetIndex=1,header=T)
n8<-n8%>% mutate_each(funs(empty_as_na)) 
n8<-n8[which(!duplicated(n8)),]


n8_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
n8_2[,1]<-a2$X1


for(i in 1:length(a2$X1)){
  
  n8_dum<-n8[which(n8$ID_HOUSE==as.character(a2$X1[[i]])),]
  n8_dum<-n8_dum[which(rowSums(is.na(n8_dum))!=125),]
  
  if(nrow(n8_dum)==0){
    
    n8_2[i,2]<-1
    n8_2[i,3]<-"Aceptable (Salto el modulo)"
    
  }else{
    
    n8_2[i,2]<-1
    n8_2[i,3]<-"Aceptable"  
  }
};rm(i,n8_dum,n8)

#####################################################################################
#CHECK MODULE Q
#####################################################################################
cat("checking Module Q =1","\n")


q_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
q_2[,1]<-a2$X1
q_2[,2]<-1
q_2[,3]<-"Aceptable"

#####################################################################################
#CHECK MODULE OBSERVATIONES
#####################################################################################

cat("checking Module OBSERVACIONES =1","\n")

OBS_2<-data.frame(matrix(nrow=nrow(a2),ncol=3))
OBS_2[,1]<-a2$X1
OBS_2[,2]<-1
OBS_2[,3]<-"Aceptable"

#####################################################################################
#CALCULATING SECTION B SCORES
#####################################################################################

cat("Calculating Indexes for SECTION B","\n")

SECT_B_SCORES<-(cbind(a2[,1],k_2[,2],k_2_2[,2],l1_5_2[,2],l2_4_2[,2],m_2[,2],m_2_2[,2],m_3_2[,2],n1_2[,2],n3_2[,2],
                     n4_2[,2],n5_2[,2],n6_2[,2],n4_2[,2],n8_2[,2],q_2[,2],OBS_2[,2]))
SECT_B_SCORES<-as.data.frame(SECT_B_SCORES)

for(i in 2:17){
  
  SECT_B_SCORES[,i]<-as.numeric(as.character(SECT_B_SCORES[,i]))
  
};rm(i)

SECT_B_STATUS<-(cbind(a2[,1],k_2[,3],k_2_2[,3],l1_5_2[,3],l2_4_2[,3],m_2[,3],m_2_2[,3],m_3_2[,3],n1_2[,3],n3_2[,3],
                     n4_2[,3],n5_2[,3],n6_2[,3],n4_2[,3],n8_2[,3],q_2[,3],OBS_2[,3]))

SECT_B_SCORES<-as.data.frame(SECT_B_SCORES)
SECT_B_STATUS<-as.data.frame(SECT_B_STATUS)


colnames(SECT_B_SCORES)<-c("ID_HOUSE","MODULO_K","MODULO_K2","MODULO_L","MODULO_L2","MODULO_M","MODULO_M2","MODULO_M3","MODULO_N1","MODULO_N3","MODULO_N4","MODULO_N5","MODULO_N6","MODULO_N7","MODULO_N8","MODULO_Q","OBSERVACIONES")
colnames(SECT_B_STATUS)<-colnames(SECT_B_SCORES)
  
MAND_B_SCORES<-SECT_B_SCORES[,c("ID_HOUSE","MODULO_K","MODULO_K2","MODULO_L","MODULO_L2","MODULO_M","MODULO_N1","MODULO_N3","MODULO_Q","OBSERVACIONES")]
MAND_B_STATUS<-SECT_B_STATUS[,c("ID_HOUSE","MODULO_K","MODULO_K2","MODULO_L","MODULO_L2","MODULO_M","MODULO_N1","MODULO_N3","MODULO_Q","OBSERVACIONES")]

MAND_B_SCORES<-as.data.frame(MAND_B_SCORES)
MAND_B_STATUS<-as.data.frame(MAND_B_STATUS)

for(i in 2:ncol(MAND_B_SCORES)){
    
    MAND_B_SCORES[,i]<-as.numeric(as.character(MAND_B_SCORES[,i]))
};rm(i)


OPT_B_SCORES<-SECT_B_SCORES[,c("ID_HOUSE","MODULO_M2","MODULO_M3","MODULO_N4","MODULO_N5","MODULO_N6","MODULO_N7","MODULO_N8")]
OPT_B_STATUS<-SECT_B_STATUS[,c("ID_HOUSE","MODULO_M2","MODULO_M3","MODULO_N4","MODULO_N5","MODULO_N6","MODULO_N7","MODULO_N8")]

OPT_B_SCORES<-as.data.frame(OPT_B_SCORES)

for(i in 2:ncol(OPT_B_SCORES)){
  OPT_B_SCORES[,i]<-as.numeric(as.character(OPT_B_SCORES[,i]))

};rm(i)


IND_B<-data.frame(cbind(MAND_B_SCORES[,1],MAND_B_SCORES[,2],MAND_B_SCORES[,3]));
IND_B[,1]<-a2[,1]
IND_B[,2]<-NA;IND_B[,3]<-NA;IND_B[,4]<-NA;IND_B[,5]<-NA;IND_B[,6]<-NA;IND_B[,7]<-NA;IND_B[,8]<-NA;IND_B[,9]<-NA;IND_B[,10]<-NA

for(i in 1:nrow(IND_B)){
  IND_B[i,2]<-(sum(MAND_B_SCORES[i,-c(1)]))/sum(MAND_B_SCORES[i,-c(1)]>0)*100 #MANDATORIO SCORE
  IND_B[i,3]<-sum(MAND_B_SCORES[i,-c(1)]>0)#MANDATORIO
  IND_B[i,4]<-(sum(OPT_B_SCORES[i,-c(1)]))/sum(OPT_B_SCORES[i,-c(1)]>0)*100 #OPCIONAL SCORE
  IND_B[i,5]<-sum(OPT_B_SCORES[i,-c(1)]>0)  #OPCIONAL
  IND_B[i,6]<-(sum(SECT_B_SCORES[i,-c(1)]))/sum(SECT_B_SCORES[i,-c(1)]>0)*100  #TOTAL SCORE
  IND_B[i,7]<-sum(SECT_B_SCORES[i,-c(1)]>0) #TOTAL
  IND_B[i,8]<-((sum(MAND_B_SCORES[i,-c(1)]))/sum(SECT_B_SCORES[i,-c(1)]>0)*100) #MAND/TOTAL SCORE
  IND_B[i,9]<-((sum(OPT_B_SCORES[i,-c(1)]))/sum(SECT_B_SCORES[i,-c(1)]>0)*100) #OPT /TOTAL SCORE
  
  if(IND_B[i,8]>50.1){
    IND_B[i,10]<-"LISTA"
  }else{
    IND_B[i,10]<-"REVISAR"
  }
};rm(i)

colnames(IND_B)<-c("ID_HOUSE","SCORE_MAND","TOTAL_MAND","SCORE_OPT","TOTAL_OPT","SCORE_TOTAL","TOTAL_MODULOS","SCORE_MAND/TOTAL_MODULOS","SCORE_OPT/TOTAL_MODULOS","STATUS")


#####################################################################################
#CALCULATING OVERALL SCORES
#####################################################################################

cat("Calculating Indexes for whole Survey","\n")


SCORES<-cbind(a2[,1:2],b2[,2],c2[,2],d2[,2],e2[,2],f2[,2],g2[,2],g2_1[,2],g3_2[,2],h_2[,2],
              h_2_2[,2],h_3_2[,2],h_4_2[,2],i_2[,2],i_2_2[,2],i_3_2[,2],j_2[,2],j_2_2[,2],j_3_2[,2],c2[,2],
              k_2[,2],k_2_2[,2],l1_5_2[,2],l2_4_2[,2],m_2[,2],m_2_2[,2],m_3_2[,2],n1_2[,2],n3_2[,2],
              n4_2[,2],n5_2[,2],n6_2[,2],n4_2[,2],n8_2[,2],q_2[,2],OBS_2[,2])

SCORES<-as.data.frame(SCORES)

colnames(SCORES)<-c("ID_HOUSE","MODULO_A","MODULO_B","MODULO_C","MODULO_D","MODULO_E","MODULO_F","MODULO_G","MODULO_G2","MODULO_G3","MODULO_H","MODULO_H2","MODULO_H3","MODULO_H4","MODULO_I","MODULO_I2","MODULO_I3","MODULO_J","MODULO_J2","MODULO_J3","C",
                    "MODULO_K","MODULO_K2","MODULO_L","MODULO_L2","MODULO_M","MODULO_M2","MODULO_M3","MODULO_N1","MODULO_N3","MODULO_N4","MODULO_N5","MODULO_N6","MODULO_N7","MODULO_N8","MODULO_Q","OBSERVACIONES")

SCORES<-as.data.frame(SCORES)

for(i in 2:ncol(SCORES)){
  
  SCORES[,i]<-as.numeric(as.character(SCORES[,i]))
  
};rm(i)

STATUS<-cbind(a2[,1],a2[,3],b2[,3],c2[,3],d2[,3],e2[,3],f2[,3],g2[,3],g2_1[,3],g3_2[,3],h_2[,3],
              h_2_2[,3],h_3_2[,3],h_4_2[,3],i_2[,3],i_2_2[,3],i_3_2[,3],j_2[,3],j_2_2[,3],j_3_2[,3],c2[,3],
              k_2[,3],k_2_2[,3],l1_5_2[,3],l2_4_2[,3],m_2[,3],m_2_2[,3],m_3_2[,3],n1_2[,3],n3_2[,3],
              n4_2[,3],n5_2[,3],n6_2[,3],n4_2[,3],n8_2[,3],q_2[,3],OBS_2[,3])
colnames(STATUS)<-colnames(SCORES)

MAND_SCORES<-SCORES[,c("ID_HOUSE","MODULO_A","MODULO_B","MODULO_C","MODULO_D","MODULO_G","MODULO_G2","MODULO_G3","MODULO_J2","MODULO_J3","MODULO_K","MODULO_K2","MODULO_L","MODULO_L2","MODULO_M","MODULO_N1","MODULO_N3","MODULO_Q","OBSERVACIONES")]
MAND_STATUS<-STATUS[,c("ID_HOUSE","MODULO_A","MODULO_B","MODULO_C","MODULO_D","MODULO_G","MODULO_G2","MODULO_G3","MODULO_J2","MODULO_J3","MODULO_K","MODULO_K2","MODULO_L","MODULO_L2","MODULO_M","MODULO_N1","MODULO_N3","MODULO_Q","OBSERVACIONES")]

MAND_SCORES<-as.data.frame(MAND_SCORES)

for(i in 2:ncol(MAND_SCORES)){
  
  MAND_SCORES[,i]<-as.numeric(as.character(MAND_SCORES[,i]))
  
};rm(i)

OPT_SCORES<-SCORES[,c("ID_HOUSE","MODULO_E","MODULO_F","MODULO_H","MODULO_H2","MODULO_H3","MODULO_H4","MODULO_I","MODULO_I2","MODULO_I3","MODULO_J","MODULO_C","MODULO_M2","MODULO_M3","MODULO_N4","MODULO_N5","MODULO_N6","MODULO_N7","MODULO_N8")]
OPT_STATUS<-STATUS[,c("ID_HOUSE","MODULO_E","MODULO_F","MODULO_H","MODULO_H2","MODULO_H3","MODULO_H4","MODULO_I","MODULO_I2","MODULO_I3","MODULO_J","MODULO_C","MODULO_M2","MODULO_M3","MODULO_N4","MODULO_N5","MODULO_N6","MODULO_N7","MODULO_N8")]

OPT_SCORES<-as.data.frame(OPT_SCORES)

for(i in 2:ncol(OPT_SCORES)){
  
  OPT_SCORES[,i]<-as.numeric(as.character(OPT_SCORES[,i]))
  
};rm(i)


IND_TOTAL<-data.frame(cbind(MAND_SCORES[,1],MAND_SCORES[,2],MAND_SCORES[,3]));
IND_TOTAL[,1]<-a2[,1]
IND_TOTAL[,2]<-NA;IND_TOTAL[,3]<-NA;IND_TOTAL[,4]<-NA;IND_TOTAL[,5]<-NA;IND_TOTAL[,6]<-NA;IND_TOTAL[,7]<-NA;IND_TOTAL[,8]<-NA;IND_TOTAL[,9]<-NA;IND_TOTAL[,10]<-NA

for(i in 1:nrow(IND_TOTAL)){
  IND_TOTAL[i,2]<-(sum(MAND_SCORES[i,-c(1)]))/sum(MAND_SCORES[i,-c(1)]>0)*100 #MANDATORIO SCORE
  IND_TOTAL[i,3]<-sum(MAND_SCORES[i,-c(1)]>0)#MANDATORIO
  IND_TOTAL[i,4]<-(sum(OPT_SCORES[i,-c(1)]))/sum(OPT_SCORES[i,-c(1)]>0)*100 #OPCIONAL SCORE
  IND_TOTAL[i,5]<-sum(OPT_SCORES[i,-c(1)]>0)  #OPCIONAL
  IND_TOTAL[i,6]<-(sum(SCORES[i,-c(1)]))/sum(SCORES[i,-c(1)]>0)*100  #TOTAL SCORE
  IND_TOTAL[i,7]<-sum(SCORES[i,-c(1)]>0) #TOTAL
  IND_TOTAL[i,8]<-((sum(MAND_SCORES[i,-c(1)]))/sum(SCORES[i,-c(1)]>0)*100) #MAND/TOTAL SCORE
  IND_TOTAL[i,9]<-((sum(OPT_SCORES[i,-c(1)]))/sum(SCORES[i,-c(1)]>0)*100) #OPT /TOTAL SCORE
  
  if(IND_TOTAL[i,8]>=47){
    IND_TOTAL[i,10]<-"LISTA"
  }else{
    IND_TOTAL[i,10]<-"REVISAR"
  }
};rm(i)

colnames(IND_TOTAL)<-c("ID_HOUSE","SCORE_MAND","TOTAL_MAND","SCORE_OPT","TOTAL_OPT","SCORE_TOTAL","TOTAL_MODULOS","SCORE_MAND/TOTAL_MODULOS","SCORE_OPT/TOTAL_MODULOS","STATUS")

#####################################################################################
#SAVING CSV
#####################################################################################

cat("Saving EXCEL files for SECTION A","\n")


write.xlsx (IND_A,paste0(out_dir,"/","INDEXES_SECTION_A.xls"),row.names=F,showNA = F,sheetName="INDEX_SECTION_A")
write.xlsx (SECT_A_STATUS,paste0(out_dir,"/","INDEXES_SECTION_A.xls"),row.names=F,showNA = F,sheetName="STATUS_SECTION_A", append=TRUE)
write.xlsx (MAND_A_STATUS,paste0(out_dir,"/","INDEXES_SECTION_A.xls"),row.names=F,showNA = F,sheetName="STATUS_MANDATORY_SECTION_A", append=TRUE)
write.xlsx (OPT_A_STATUS,paste0(out_dir,"/","INDEXES_SECTION_A.xls"),row.names=F,showNA = F,sheetName="STATUS_OPTIONAL_SECTION_A", append=TRUE)
write.xlsx (SECT_A_SCORES,paste0(out_dir,"/","INDEXES_SECTION_A.xls"),row.names=F,showNA = F,sheetName="SCORES_SECTION_A", append=TRUE)
write.xlsx (MAND_A_SCORES,paste0(out_dir,"/","INDEXES_SECTION_A.xls"),row.names=F,showNA = F,sheetName="SCORES_MANDATORY_SECTION_A", append=TRUE)
write.xlsx (OPT_A_SCORES,paste0(out_dir,"/","INDEXES_SECTION_A.xls"),row.names=F,showNA = F,sheetName="SCORES_OPTIONAL_SECTION_A", append=TRUE)


cat("Saving EXCEL files for SECTION B","\n")


write.xlsx (IND_B,paste0(out_dir,"/","INDEXES_SECTION_B.xls"),row.names=F,showNA = F,sheetName="INDEX_SECTION_B")
write.xlsx (SECT_B_STATUS,paste0(out_dir,"/","INDEXES_SECTION_B.xls"),row.names=F,showNA = F,sheetName="STATUS_SECTION_B", append=TRUE)
write.xlsx (MAND_B_STATUS,paste0(out_dir,"/","INDEXES_SECTION_B.xls"),row.names=F,showNA = F,sheetName="STATUS_MANDATORY_SECTION_B", append=TRUE)
write.xlsx (OPT_B_STATUS,paste0(out_dir,"/","INDEXES_SECTION_B.xls"),row.names=F,showNA = F,sheetName="STATUS_OPTIONAL_SECTION_B", append=TRUE)
write.xlsx (SECT_B_SCORES,paste0(out_dir,"/","INDEXES_SECTION_B.xls"),row.names=F,showNA = F,sheetName="SCORES_SECTION_B", append=TRUE)
write.xlsx (MAND_B_SCORES,paste0(out_dir,"/","INDEXES_SECTION_B.xls"),row.names=F,showNA = F,sheetName="SCORES_MANDATORY_SECTION_B", append=TRUE)
write.xlsx (OPT_B_SCORES,paste0(out_dir,"/","INDEXES_SECTION_B.xls"),row.names=F,showNA = F,sheetName="SCORES_OPTIONAL_SECTION_B", append=TRUE)

cat("Saving EXCEL files for whole survey","\n")


write.xlsx (IND_TOTAL,paste0(out_dir,"/","INDEXES_TOTAL.xls"),row.names=F,showNA = F,sheetName="INDEX_TOTAL")
write.xlsx (SCORES,paste0(out_dir,"/","INDEXES_TOTAL.xls"),row.names=F,showNA = F,sheetName="SCORES_TOTAL", append=TRUE)
write.xlsx (STATUS,paste0(out_dir,"/","INDEXES_TOTAL.xls"),row.names=F,showNA = F,sheetName="STATUS_TOTAL", append=TRUE)
write.xlsx (MAND_STATUS,paste0(out_dir,"/","INDEXES_TOTAL.xls"),row.names=F,showNA = F,sheetName="STATUS_MANDATORY_TOTAL", append=TRUE)
write.xlsx (OPT_STATUS,paste0(out_dir,"/","INDEXES_TOTAL.xls"),row.names=F,showNA = F,sheetName="STATUS_OPTIONAL_TOTAL", append=TRUE)
write.xlsx (MAND_SCORES,paste0(out_dir,"/","INDEXES_TOTAL.xls"),row.names=F,showNA = F,sheetName="SCORES_MANDATORY_TOTAL", append=TRUE)
write.xlsx (OPT_SCORES,paste0(out_dir,"/","INDEXES_TOTAL.xls"),row.names=F,showNA = F,sheetName="SCORES_OPTIONAL_TOTAL", append=TRUE)


unlink(paste0(date_dir,"/","Dropbox"),recursive = T)

cat("##############","\n")
cat("    DONE!     ","\n")
cat("##############","\n")

gc()
}

#VOL<-"D:"
VOL<-"D:"
rep_dir<-"Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_PER/REPORTES_SEMANA"

#INS<-c("SUP_1","SUP_2","SUP_3","SUP_4")
#INS<-c("SUP_1","SUP_3","SUP_4")
INS<-"SUP_4"
#INS<-"SUP_2"

     # date_S<-"2016_05_18"
#date_S<-"2016_09_16"
#date_S<-"2016_11_28"
date_S<-"2017_01_31"
  #"2016_10_04"
       #out_dir<-"E:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA/SINCHI/2016_04_20"
       #out_dir<-paste0(rep_dir,"/",INS,"/",date_S)
       



lapply(1:length(INS),function(i){
  
  x<-INDEX_FUNCTION(VOL=VOL,rep_dir=rep_dir,INS=INS[[i]],date_S=date_S,out_dir=paste0(VOL,"/",rep_dir,"/",INS[[i]],"/",date_S))
})

# i=1
# VOL=VOL
# INS=INS[[i]]
# date_S=date_S
# out_dir=paste0(VOL,"/",rep_dir,"/",INS[[i]],"/",date_S)
