name_outcome=function(comp,outcome,codes){
  
  name_outcome=codes$Code[which(codes$Outcome==outcome)]
  
  
  d14_28=grep("D28",outcome)
  d7=grep("D7",outcome)
  d60=grep("D60",outcome)
  d_AE=grep("Adverse events",outcome)
  d_SAE=grep("Serious adverse events",outcome)
  
  day=NA
  
  day=ifelse(length(d14_28)>0,"14",day)
  day=ifelse(length(d60)>0,"60",day)
  day=ifelse(length(d7)>0,"07",day)
  day=ifelse(length(d_AE)>0,"14",day)
  day=ifelse(length(d_SAE)>0,"14",day)
  day=ifelse(sum(length(d7),length(d14_28),length(d60),length(d_AE),length(d_SAE))==0,"",day)

  
  
  final_name=ifelse(day>0,paste(comp,name_outcome,day,"pharma_rct",sep="_"),paste(comp,name_outcome,"pharma_rct",sep="_"))
  
  final_name=paste(final_name,"jpeg",sep=".")
  
  final_name
  
}


