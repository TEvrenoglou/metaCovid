 select_new=function(data,comp.num,dat){
  
  data$stats_name=trimws(data$stats_name)
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
  
  
  ########################## Fixing events in terms of days ########################################
  
  final_data1$All_cause_mortality_events_D28=ifelse(is.na(final_data1$All_cause_mortality_events_D28),
                                                    final_data1$All_cause_mortality_events_D14,final_data1$All_cause_mortality_events_D28)
  
  
  final_data1$All_cause_mortality_events_D28=ifelse(is.na(final_data1$All_cause_mortality_events_D28),
                                                    final_data1$All_cause_mortality_events_D7,final_data1$All_cause_mortality_events_D28)
  
  
  final_data1$All_cause_mortality_events_D60=ifelse(is.na(final_data1$All_cause_mortality_events_D90),
                                                    final_data1$All_cause_mortality_events_D60,final_data1$All_cause_mortality_events_D90)
  
  
  final_data1$Clinical_improvement_D28_n=ifelse(is.na(final_data1$Clinical_improvement_D28_n),
                                                final_data1$Clinical_improvement_D14_n,final_data1$Clinical_improvement_D28_n)
  
  
  final_data1$Clinical_improvement_D28_n=ifelse(is.na(final_data1$Clinical_improvement_D28_n),
                                                    final_data1$Clinical_improvement_D7_n,final_data1$Clinical_improvement_D28_n)
  
  
  final_data1$Clinical_improvement_D60_n=ifelse(is.na(final_data1$Clinical_improvement_D90_n),
                                                    final_data1$Clinical_improvement_D60_n,final_data1$Clinical_improvement_D90_n)
  
  
  final_data1$SCORE_7_and_above_D28=ifelse(is.na(final_data1$SCORE_7_and_above_D28),
                                                    final_data1$SCORE_7_and_above_D14,final_data1$SCORE_7_and_above_D28)
  
  
  final_data1$SCORE_7_and_above_D28=ifelse(is.na(final_data1$SCORE_7_and_above_D28),
                                                    final_data1$SCORE_7_and_above_D7,final_data1$SCORE_7_and_above_D28)
  
  
  final_data1$NEG_conv_n_D7=ifelse(is.na(final_data1$NEG_conv_n_D7),
                                           final_data1$NEG_conv_n_D3,final_data1$NEG_conv_n_D7)

  #############################################################################
  
  
  
  #################################################################
  clininc_impr28=na.omit(final_data1$Clinical_improvement_D28_n)
  clininc_impr60=na.omit(final_data1$Clinical_improvement_D60_n)
  
  mort28=na.omit(final_data1$All_cause_mortality_events_D28)
  mort60=na.omit(final_data1$All_cause_mortality_events_D60)
  
  who7_28=na.omit(final_data1$SCORE_7_and_above_D28)
  who7_90=na.omit(final_data1$SCORE_7_and_above_D90)
  
  neg_conv=na.omit(final_data1$NEG_conv_n_D7)
  
  adverse=na.omit(final_data1$Total_AE)
  sae=na.omit(final_data1$Serious_AE)
  
  time_to_clinic=na.omit(final_data1$Time_to_clinical_improvement_HR)
  time_to_death=na.omit(final_data1$time_to_death_HR)
  time_to_score7=na.omit(final_data1$Time_to_score_7_and_above_HR)
  time_to_neg_conv=na.omit(final_data1$Time_to_NEG_conv_HR)
  
  
  hosp=na.omit(final_data1$Hosp)
  if(comp.num %in% c(29,253)){
    time_to_clinic=NULL
    time_to_neg_conv=NULL
  }
  #################################################################
  Clinical_improvement28=ifelse(length(clininc_impr28)>1,"Clinical improvement D28",NA)
  Clinical_improvement60=ifelse(length(clininc_impr60)>1,"Clinical improvement D60",NA)
  
  Mortality28=ifelse(length(mort28)>1,"Mortality D28",NA)
  Mortality60=ifelse(length(mort60)>1,"Mortality D60",NA)
  
  
  WHO_28=ifelse(length(who7_28)>1,"WHO Score 7 D28",NA)
  WHO_90=ifelse(length(who7_90)>1,"WHO Score 7 D60",NA)
  
  
  AE=ifelse(length(adverse)>1,"Adverse events",NA)
  SAE=ifelse(length(sae)>1,"Serious adverse events",NA)
  
  Time_to_Clinical=ifelse(length(time_to_clinic)>0,"Time to clinical improvement",NA)
  Time_to_Death=ifelse(length(time_to_death)>0,"Time to death",NA)
  Time_to_Score7=ifelse(length(time_to_score7)>0,"Time to WHO Score 7",NA)
  Time_to_Negconv=ifelse(length(time_to_neg_conv)>0,"Time to Negative Conversion",NA)
  
  
  Hospitalized=ifelse(length(hosp)>1,"Hospitalization or death",NA)
  
  Negative=ifelse(length(neg_conv)>1,"Negative conversion D7",NA)
  
  ################################################################
  
  outcomes=c(Hospitalized,Clinical_improvement28,Clinical_improvement60,WHO_28,WHO_90,Mortality28,Mortality60,
             Negative,
             AE,SAE,Time_to_Clinical,Time_to_Score7,Time_to_Death,Time_to_Negconv)
  
  outcomes=outcomes[complete.cases(outcomes)]
  
  return(outcomes)
 }
 