select_vaccines=function(data,type,comparison="regular"){
  
  
  
  data$comp_type=ifelse(is.na(data$comp_type),"regular",data$comp_type)
  data$comp_type=trimws(data$comp_type)
  
  data<- data %>%
    filter(comp_type==comparison)
  
  data$Treat_Type<- trimws(data$Treat_Type)
  E<- which(data$Treat_Type==type)

  A=data$Trial_ID[E]  

  dat<- data %>%
    filter(Trial_ID %in% A)
  

#################################################
  
E_conf=na.omit(dat$Any_Vaccine_efficacy_perc)

conf=E_conf

#################################################
#################################################

E_conf_symp=na.omit(dat$Sympt_Vaccine_efficacy_perc)

conf_symp=E_conf_symp

#################################################
#################################################

E_severe=na.omit(dat$Vaccine_efficacy_severity)

severe=E_severe

#################################################
#################################################

#################################################
E_systemic=na.omit(dat$AE_syst_d14)

systemic=E_systemic

#################################################
#################################################

#################################################
E_local=na.omit(dat$AE_Local_d7)

local=E_local

#################################################
#################################################

E_mortality=na.omit(dat$All_cause_mortality_events)

mortality=E_mortality

#################################################
#################################################

E_serious=na.omit(dat$Serious_AE)

serious=E_serious


#################################################
#################################################

adverse1=ifelse(is.na(dat$Total_AE),dat$Solicited_AE,dat$Total_AE)
adverse=ifelse(is.na(adverse1),dat$unsolicited_AE,adverse1)

E_adverse=na.omit(adverse)

adverse=E_adverse

#################################################
#################################################

AE=ifelse(length(adverse)>1,"Any adverse events",NA)

SAE=ifelse(length(serious)>1,"Serious adverse events",NA)

Systemic=ifelse(length(systemic)>1,"Systemic adverse events",NA)

Local=ifelse(length(local)>1,"Local adverse events",NA)

Mortality=ifelse(length(mortality)>1,"All-cause mortality",NA)

Severe=ifelse(length(severe)>0,"Confirmed severe or critical disease due to Covid-19",NA)

Conf_Symp=ifelse(length(conf_symp)>0,"Symptomatic Covid-19",NA)

Confirmed=ifelse(length(conf)>0,"SARS-CoV-2 infection",NA)

outcomes=c(Severe,Conf_Symp,Confirmed,Mortality,SAE,AE,Systemic,Local)

outcomes=outcomes[complete.cases(outcomes)]


return(outcomes)
}