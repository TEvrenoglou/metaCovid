create_table=function(data,dat,comp.num){
  data$stats_name=trimws(data$stats_name)
  
  data$First_author=ifelse(is.na(data$First_author),data$name_study,data$First_author)
  data$First_author=trimws(data$First_author)
  
  
  data$Conflict_of_interest_category=trimws(data$Conflict_of_interest_category)
  data$Type_publication=ifelse(data$Type_publication=="published paper","Published paper",data$Type_publication)
  data$Type_publication=ifelse(data$Type_publication=="preprint","Preprint",data$Type_publication)
  
  data$Type_publication=trimws(data$Type_publication)
  
  
  E=which(dat$comp.num==comp.num)
  
  
  t1=unique(dat$treat1[E])
  t2=unique(dat$treat2[E])
  t1=trimws(t1)
  t2=trimws(t2)
  
  
  e1=data %>%
    filter(stats_name %in% t1)
  
  e2=data %>%
    filter(stats_name %in% t2)
  
  common <- intersect(e1$Trial_ID, e2$Trial_ID)  
  final_data=data[data$Trial_ID %in% common,]
  
  final_data1=final_data %>%
    filter((stats_name %in% t1) | (stats_name %in% t2))
  
  final_data1$events=rep(50,nrow(final_data1))
  final_data1$nn=rep(100,nrow(final_data1))
  
  dat_new=pairwise_old(treat=stats_name,event=events,n=nn,studlab =Trial_ID,data = final_data1)
  E1=which(class(dat_new)=="pairwise") 
  class(dat_new)=class(dat_new)[-E1]
  dat_new$First_author=paste(dat_new$First_author,dat_new$Journal_abbrv,dat_new$Year,sep=", ")
  
  if((is.null(dat_new$n_Randomized1)) & (is.null(dat_new$n_Randomized2))){

    dat_new$n_Randomized1=dat_new$n_Randomized
    dat_new$n_Randomized2=dat_new$n_Randomized
  }else{
    dat_new$n_Randomized1=dat_new$n_Randomized1
    dat_new$n_Randomized2=dat_new$n_Randomized2
  }
  
  if(is.null(dat_new$Treat_desc_summary)){
    dat_new$Treat_desc_summary=dat_new$Treat_desc_summary1
  }else{
    dat_new$Treat_desc_summary=  dat_new$Treat_desc_summary
  }
      
  dat_new$n_Randomized1=ifelse(is.na(dat_new$n_Randomized1),dat_new$n_Analyzed_1,dat_new$n_Randomized1)
  dat_new$n_Randomized2=ifelse(is.na(dat_new$n_Randomized2),dat_new$n_Analyzed_2,dat_new$n_Randomized2)
  dat_new$sample_size=dat_new$n_Randomized1+dat_new$n_Randomized2
  

  
  # dat_final<- dat_new %>%
  #   select(First_author,stats_name1,stats_name2,Treat_desc_summary,Funding,Countries,ROB_1_randomization,ROB_2_Deviations_from_intervention,ROB3_FOR_WEBSITE,ROB4_FOR_WEBSITE,
  #          ROB5_FOR_WEBSITE,ROB_6_for_website)
  
  
  dat_final<- dat_new %>%
    select(First_author,stats_name1,stats_name2,sample_size,Treat_desc_summary,Research_question,Type_publication,Conflict_of_interest_category,Funding,Countries,ROB_6_for_website)
  
  names(dat_final)=c("First author","Intervention 1","Intervention 2","Sample Size","Treatment description summary","Type of patients","Publication status","Conflict of interest","Funding",
                     "Country","Overall Risk of bias")
  
  return(dat_final)
}
