create_table_vaccs=function(data,type){
  
  data$comp_type=ifelse(is.na(data$comp_type),"regular",data$comp_type)
  
  data=data %>%
    filter(data$comp_type=="regular")
  
  data$stats_name=trimws(data$stats_name)
  
  data$First_author=ifelse(is.na(data$First_author),data$Trial_name,data$First_author)
  data$First_author=trimws(data$First_author)
  
  
  data$Conflict_of_interest_category=trimws(data$Conflict_of_interest_category)
  data$Type_publication=ifelse(data$Type_publication=="published paper","Published paper",data$Type_publication)
  data$Type_publication=ifelse(data$Type_publication=="preprint","Preprint",data$Type_publication)
  
  data$Type_publication=trimws(data$Type_publication)
  
  data$pse=rep(10,nrow(data))
  data$pn=rep(50,nrow(data))
  
  dat=pairwise_old(studlab =Trial_ID,treat = stats_name,n=pn,event = pse, data = data)
  
  
  E=which(dat$Treat_Type1==type)
  
  E1=which(class(dat)=="pairwise") 
  class(dat)=class(dat)[-E1]

  
  ID=dat$Trial_ID[E]
  
  dat=dat %>% 
  filter(Trial_ID %in% ID)
  
  t1=unique(dat$treat1)
  t2=unique(dat$treat2)
  t1=trimws(t1)
  t2=trimws(t2)
  
  dat$First_author=paste(dat$First_author,dat$Journal_abbrv,dat$Year,sep=", ")

  
  
  dat_final<- dat %>%
    select(First_author,Developer_name,stats_name1,stats_name2,
           Study_phase,Type_of_participants,Type_publication,
           Conflict_of_interest_category,Funding,Countries)
  
  names(dat_final)=c("First author","Developer","Intervention 1","Intervention 2","Phase","Type of participants","Publication status","Conflict of interest","Funding",
                     "Country")
  
  return(dat_final)
}
