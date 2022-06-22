name_outcome_vaccs=function(type,outcome,codes){
  
  
name_outcome=codes$Code[which(codes$Outcome==outcome)]
  
  
  tp=NA
  tp=ifelse(type=="RNA based vaccine","rna",tp)
  tp=ifelse(type=="Non replicating viral vector","norep",tp)
  tp=ifelse(type=="Inactivated virus","inac",tp)
  tp=ifelse(type=="Protein subunit","prot",tp)
  tp=ifelse(type=="Virus-Like particle","vlp",tp)
  


  
  final_name=paste("20000",name_outcome,"vacc_rct",tp,sep = "_")
  
  final_name=paste(final_name,"jpeg",sep=".")
  
  final_name
  
}


