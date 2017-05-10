B_FUNCTION<-function(rep_dir,ins_dir,date_S){
  
  require(foreign);require(readstata13);library(car);library(xlsx)
  
  ss<-read.dta13(paste0(date_dir,"/","MODULO_B.dta"),generate.factors=T,convert.factors = T,convert.dates = T,missing.type = T,replace.strl=T);gc()

  #ii=1
  M_B<-list()
  
 for(ii in 1:nrow(ss)){
   
   
   
     
              ID<-t(t(rep(ss[ii,1],15)))
               a<-t(ss[ii,c(2:16)])
               b<-t(ss[ii,c(17:31)])
               c<-t(ss[ii,c(32:46)])
               d<-t(ss[ii,c(47:61)])
               e<-t(ss[ii,c(62:76)])
               f<-t(ss[ii,c(77:91)])
               g<-t(ss[ii,c(92:106)])
               h<-t(ss[ii,c(107:121)])
               i<-t(ss[ii,c(122:136)])
               j<-t(ss[ii,c(137:151)])
               k<-t(ss[ii,c(152:166)])
               l<-t(ss[ii,c(167:181)])
               m<-t(ss[ii,c(182:196)])
               n<-t(ss[ii,c(197:211)])
               o<-t(ss[ii,c(212:226)])
               p<-t(ss[ii,c(227:241)])
               q<-t(ss[ii,c(242:256)])
               r<-t(ss[ii,c(257:271)])
               s<-t(ss[ii,c(272:286)])
               t<-t(ss[ii,c(287:301)])
               u<-t(ss[ii,c(302:316)])
               v<-t(ss[ii,c(317:331)])
               w<-t(ss[ii,c(332:346)])
               y<-t(ss[ii,c(347:361)])
               z<-t(ss[ii,c(362:376)])
               aa<-t(ss[ii,c(377:391)])
               ab<-t(ss[ii,c(392:406)])  
               ac<-t(ss[ii,c(407:421)])  
               ad<-t(ss[ii,c(422:436)])  
     to_check<-list(ID,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,y,z,aa,ab,ac,ad)
     
     to_check<-do.call(cbind,to_check)
     rm(ID,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,y,z,aa,ab,ac,ad);gc()  
     
          M_B[[ii]]<-to_check
     
   };rm(ii)
       
 
  M_B<-do.call(rbind,M_B)
 

 
 colnames(M_B)<-c("ID_HOUSE",
                  "B.1. Id de persona",
                  "B.1. Nombre",
                  "B.1. ¿Cual es el parentezco con el jefe de la familia?",
                  "B.1. ¿Cual es el parentezco con el jefe de la familia? (Especifique)",
                  "B.1. Sexo",
                  "B.1. Edad (años)",
                  "B.1. ¿Ha migrado durante el último año?",
                  "B.1.  Razon de migracion [opción multiple]",
                  "Falta de tierra",
                  "Búsqueda de tierra más fértil",
                  "Falta de empleo",
                  "Matrimonio",
                  "Para estudiar",
                  "Estudio de los hijos",
                  "Salud",
                  "Violencia de grupos armados",
                  "Otro (especificar)",
                  "Razón de migración (Especifique)",
                  "B.1. ¿Sabe leer?",
                  "B.1. ¿Sabe escribir?",
                  "B.1.  Número total de años de estudio cursados",
                  "B.1. Para niños entre 6 y 18 años, estuvo en la escuela los últimos 12 meses?",
                  "B.1. Ocupación principal (la que realiza la mayor parte del tiempo)",
                  "B.1. Ocupación principal (opcional)",
                  "B.1. Ocupación secundaria (eventual o temporal)",
                  "B.1. Ocupación secundaria (Especifique)",
                  "B.1. Estado civil",
                  "B.1. Estado civil (Especifique)",
                  "B.1. Presencia de enfermedades cronicas"
 )
 
# write.table(M_B,paste0(date_dir,"/","MODULO_B_",Sys.Date(),".csv"),row.names=F,quote=T,sep=">",na="")
 
 write.xlsx (M_B,paste0(date_dir,"/","MODULO_B",".xls"),row.names=F,showNA = F)
 
 rm(ss,M_B,to_check);gc()
}

#rep_dir<-"D:/Dropbox/Dropbox/ENCUESTA_SAL/ENCUESTA_SAL/REPORTES_SEMANA"
#ins_dir<-paste0(rep_dir,"/","SINCHI") #"SINCHI", "UNIAMAZ"
#date_S<-"20160411_10AM"
#date_dir<-paste0(ins_dir,"/",date_S)

#x<-B_FUNCTION(rep_dir,ins_dir,date_S)