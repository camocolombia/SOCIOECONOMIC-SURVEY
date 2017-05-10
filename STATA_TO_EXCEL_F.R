STATA_TO_EXCEL<-function(rep_dir,ins_dir,date_S,src.dir){
  
  cat("                                                            ","\n")
  cat("############################################################","\n")
  cat("converting survey from DTA to Excel files. Please be patient","\n")
  cat("############################################################","\n")
  cat("                                                            ","\n")
  
  
  sources_functions<-list.files(src.dir,pattern = "FUNCTION.R*",full.names = T)
  for(i in 1:length(sources_functions)){
    source(sources_functions[[i]])
  };rm(i)
  
  
  B_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO B from DTA to Excel format","\n");gc()
  E_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO E from DTA to Excel format","\n");gc()
  G_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO G from DTA to Excel format","\n");gc()
  G2_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO G2 from DTA to Excel format","\n");gc() 
  H_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO H from DTA to Excel format","\n");gc()
  H2_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO H2 from DTA to Excel format","\n");gc()
  H3_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO H3 from DTA to Excel format","\n");gc()
  I2_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO I2 from DTA to Excel format","\n");gc()
  I3_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO I3 from DTA to Excel format","\n");gc()
  J_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO J from DTA to Excel format","\n");gc()
  J2_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO J2 from DTA to Excel format","\n");gc()
  J3_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO J3 from DTA to Excel format","\n");gc()
  K_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO K from DTA to Excel format","\n");gc()
  K2_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO K2 from DTA to Excel format","\n");gc()
  L1_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO L1 from DTA to Excel format","\n");gc()
  L2_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO L2 from DTA to Excel format","\n");gc()
  L3_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO L3 from DTA to Excel format","\n");gc()
  L4_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO L4 from DTA to Excel format","\n");gc()
  #L5_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO L5 from DTA to Excel format","\n");gc()
  L5_1_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO L5 PART 1 from DTA to Excel format","\n");gc()
  L5_2_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO L5 PART 2 from DTA to Excel format","\n");gc()
  M2_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO M2 from DTA to Excel format","\n");gc()
  M3_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO M3 from DTA to Excel format","\n");gc()
  N3_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO N3 from DTA to Excel format","\n");gc()
  N5_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO N5 from DTA to Excel format","\n");gc()
  N6_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO N6 from DTA to Excel format","\n");gc()
  N7_FUNCTION(rep_dir,ins_dir,date_S);cat("converting MODULO N7 from DTA to Excel format","\n");gc()
  
  cat("          ","\n")
  cat("##########","\n")
  cat("  DONE!   ","\n")
  cat("##########","\n")
  cat("          ","\n")
}