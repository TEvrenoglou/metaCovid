#library(tidyverse)
#library(metafor)
#library(stringr)
#library(netmeta)



#data=read.csv("C:\\Users\\Theodoros Evrenoglou\\Desktop\\Covid-19 NMA\\Database\\December\\24_12_2020\\rct__updated_database_24_12.csv", na=c("*","NA"))

select=function(data,comp.num){
  # ncores=detectCores()
  # options(mc.cores=ncores)
  # 
  
  data=data[,c("Trial_ID","ID_stats","name_study","First_author","Year","Study_design","Funding","Conflict_of_interest_category",
               "Research_question","Countries","F_U_days","Treatment_control","Treat_Name","stats_name","Treat_desc_summary",
               "n_Randomized","n_Analyzed_",
               "All_cause_mortality_events_D7","All_cause_mortality_events_D14","All_cause_mortality_events_D28",
               "All_cause_mortality_events_D60","All_cause_mortality_events_D90","time_to_death_HR","time_to_death_lci","time_to_death_uci",
               "NEG_conv_n_D3","NEG_conv_n_D7","NEG_conv_denominator_d3","NEG_conv_denominator_d7","Time_to_NEG_conv_HR","Time_to_NEG_conv_HR_LowerCI",
               "Time_to_NEG_conv_HR_UpperCI","Clinical_improvement_D7_n","Clinical_improvement_D14_n","Clinical_improvement_D28_n","Clinical_improvement_D60_n",
               "Clinical_improvement_D90_n","Time_to_clinical_improvement_HR","Time_to_clinical_improvement_HR_CI_Lower","Time_to_clinical_improvement_HR_CI_Upper",
               "SCORE_7_and_above_D7","SCORE_7_and_above_D14","SCORE_7_and_above_D28","SCORE_7_and_above_D90","Time_to_score_7_and_above_HR",
               "Time_to_score_7_and_above_LCI","Time_to_score_7_and_above_UCI","Total_AE","Serious_AE"
               )]
#data$F_U_days=as.numeric(data$F_U_days)
data$F_U_days=as.numeric(data$F_U_days)
data$n_Randomized=as.numeric(data$n_Randomized)
# data$n_without_consent=as.numeric(data$n_without_consent)
# data$n_without_consent=ifelse(is.na(data$n_without_consent),0,data$n_without_consent)
# data$n_Randomized=data$n_Randomized-data$n_without_consent
data$n_Randomized=ifelse(is.na(data$n_Randomized),data$n_Analyzed_,data$n_Randomized)
#data$n_in_the_as_treated_analysis=ifelse(is.na(data$n_in_the_as_treated_analysis),data$n_Randomized,data$n_in_the_as_treated_analysis)
#data$NEG_conv_denominator_d3=ifelse(is.na(data$NEG_conv_denominator_d3),data$n_Randomized,data$NEG_conv_denominator_d3)

data$NEG_conv_denominator_d7=ifelse(is.na(data$NEG_conv_denominator_d7),data$NEG_conv_denominator_d3,data$NEG_conv_denominator_d7)
data$NEG_conv_denominator_d7=ifelse(is.na(data$NEG_conv_denominator_d7),data$n_Randomized,data$NEG_conv_denominator_d7)

data$Treat_Name=trimws(data$Treat_Name)
data$stats_name=trimws(data$stats_name)
data$Treat_Name=data$stats_name
data$Trial_ID=trimws(data$Trial_ID)
data$First_author=trimws(data$First_author)
data$name_study=trimws(data$name_study)
data$First_author=ifelse(is.na(data$name_study),as.character(data$First_author),paste(as.character(data$First_author),as.character(data$name_study),sep = ","))
data$Year=trimws(data$Year)
data$Research_question=trimws(data$Research_question)
data$Research_question=str_to_sentence(data$Research_question)

# data$Treat_Name[data$Treat_Name=="Corticosteroids (HC, MPS, DM)"]="Corticosteroids"
# data$Treat_Name[data$Treat_Name=="Corticosteroids (MPS or Pred or HC or Dex)"]="Corticosteroids"
# data$Treat_Name[data$Treat_Name=="IFN alpha2b+IFN gamma"]="INF a-2b+INF gamma"
# data$Treat_Name[data$Treat_Name=="Interferon alpha2b"]="Interferon a-2b"
#data$Treat_Dose_desc <- rename(data$Treat_Dose_desc, replace = c("at prescribers' discretion " = "-"))
#data$Treat_Dose_desc[data$Treat_Dose_desc=="at prescribers' discretion "]="-"
#data$Treat_Dose1[data$Treat_Dose1=="at prescribers' discretion "]=" "
#data$Treat_desc_summary[data$Treat_desc_summary=="at prescribers' discretion "]="-"
data$Treat_Dose_desc=as.character(data$Treat_desc_summary)


data$NEG_conv_n_D7=ifelse(!is.na(data$NEG_conv_n_D7),data$NEG_conv_n_D7,data$NEG_conv_n_D3)
data$All_cause_mortality_events_D14=ifelse(!is.na(data$All_cause_mortality_events_D14),data$All_cause_mortality_events_D14,data$All_cause_mortality_events_D7)
data$All_cause_mortality_events_D1428=ifelse(!is.na(data$All_cause_mortality_events_D28),data$All_cause_mortality_events_D28,data$All_cause_mortality_events_D14)
data$All_cause_mortality_events_D60=ifelse(!is.na(data$All_cause_mortality_events_D90),data$All_cause_mortality_events_D90,data$All_cause_mortality_events_D60)

data$Clinical_improvement_D14_n=ifelse(!is.na(data$Clinical_improvement_D14_n),data$Clinical_improvement_D14_n,data$Clinical_improvement_D7_n)
data$Clinical_improvement_D1428_n=ifelse(!is.na(data$Clinical_improvement_D28_n),data$Clinical_improvement_D28_n,data$Clinical_improvement_D14_n)

#data$SCORE_6_and_above_D1428=ifelse(!is.na(data$SCORE_6_and_above_D28),data$SCORE_6_and_above_D28,data$SCORE_6_and_above_D14)

data$SCORE_7_and_above_D14=ifelse(!is.na(data$SCORE_7_and_above_D14),data$SCORE_7_and_above_D14,data$SCORE_7_and_above_D7)
data$SCORE_7_and_above_D1428=ifelse(!is.na(data$SCORE_7_and_above_D28),data$SCORE_7_and_above_D28,data$SCORE_7_and_above_D14)
#data$Serious_AE_D1428=ifelse(data$F_U_days>1,data$Serious_AE,NA)
#data$Total_AE_D1428=ifelse(data$F_U_days>1,data$Total_AE,NA)

rct_data=subset(data,data$Study_design=="RCT" & Trial_ID!=9 & Trial_ID!=139)






#transform data per comparison#
 # rct_data$Clinical_improvement_D7_n=as.numeric(rct_data$Clinical_improvement_D7_n)
 # pair_data_01_07=pairwise(studlab=Trial_ID,treat=Treat_Name,event=Clinical_improvement_D7_n,n=n_Randomized,
 #                          data=rct_data,measure="RR",ref="Standard care")
 # ncomp=length(pair_data_01_07$treat1)
 # pair_data_01_07$F_U_days1=pair_data_01_07$F_U_days
 # pair_data_01_07$First_author=pair_data_01_07$First_author


rct_data$Clinical_improvement_D1428_n=as.numeric(rct_data$Clinical_improvement_D1428_n)
pair_data_01_14=pairwise(studlab=Trial_ID,treat=Treat_Name,event=Clinical_improvement_D1428_n,n=n_Randomized,
                         data=rct_data,measure="RR",ref="Standard care")
ncomp=length(pair_data_01_14$treat1)
pair_data_01_14$F_U_days1=pair_data_01_14$F_U_days
pair_data_01_14$First_author=pair_data_01_14$First_author



rct_data$Clinical_improvement_D60_n=as.numeric(rct_data$Clinical_improvement_D60_n)
pair_data_01_60=pairwise(studlab=Trial_ID,treat=Treat_Name,event=Clinical_improvement_D60_n,n=n_Randomized,
                         data=rct_data,measure="RR",ref="Standard care")
pair_data_01_60$F_U_days1=pair_data_01_60$F_U_days
pair_data_01_60$First_author=pair_data_01_60$First_author


#rct_data$Recovery_D7_n=as.numeric(rct_data$Recovery_D7_n)
#pair_data_02_07=pairwise(studlab=Trial_ID,treat=Treat_Name,event=Recovery_D7_n,n=n_Randomized,
#                        data=rct_data,measure="RR",ref="Standard care")
#pair_data_02_07$F_U_days=pair_data_02_07$F_U_days1

#rct_data$Recovery_D1428_n=as.numeric(rct_data$Recovery_D1428_n)
#pair_data_02_14=pairwise(studlab=Trial_ID,treat=Treat_Name,event=Recovery_D1428_n,n=n_Randomized,
#                        data=rct_data,measure="RR",ref="Standard care")
#pair_data_02_14$F_U_days=pair_data_02_14$F_U_days1

# rct_data$SCORE_6_and_above_D7=as.numeric(rct_data$SCORE_6_and_above_D7)
# pair_data_03_07=pairwise(studlab=Trial_ID,treat=Treat_Name,event=SCORE_6_and_above_D7,n=n_Randomized,
#                          data=rct_data,measure="RR",ref="Standard care")
# pair_data_03_07$F_U_days1=pair_data_03_07$F_U_days
# 
# 
# rct_data$SCORE_6_and_above_D1428=as.numeric(rct_data$SCORE_6_and_above_D1428)
# pair_data_03_14=pairwise(studlab=Trial_ID,treat=Treat_Name,event=SCORE_6_and_above_D1428,n=n_Randomized,
#                          data=rct_data,measure="RR",ref="Standard care")
# pair_data_03_14$F_U_days1=pair_data_03_14$F_U_days

# rct_data$SCORE_7_and_above_D7=as.numeric(rct_data$SCORE_7_and_above_D7)
# pair_data_04_07=pairwise(studlab=Trial_ID,treat=Treat_Name,event=SCORE_7_and_above_D7,n=n_Randomized,
#                          data=rct_data,measure="RR",ref="Standard care")
# pair_data_04_07$F_U_days1=pair_data_04_07$F_U_days

rct_data$SCORE_7_and_above_D1428=as.numeric(rct_data$SCORE_7_and_above_D1428)
pair_data_04_14=pairwise(studlab=Trial_ID,treat=Treat_Name,event=SCORE_7_and_above_D1428,n=n_Randomized,
                         data=rct_data,measure="RR",ref="Standard care")
pair_data_04_14$F_U_days1=pair_data_04_14$F_U_days

pair_data_04_14$First_author=pair_data_04_14$First_author


# rct_data$All_cause_mortality_events_D7=as.numeric(rct_data$All_cause_mortality_events_D7)
# pair_data_05_07=pairwise(studlab=Trial_ID,treat=Treat_Name,event=All_cause_mortality_events_D7,n=n_Randomized,
#                          data=rct_data,measure="RR",ref="Standard care")
# pair_data_05_07$F_U_days1=pair_data_05_07$F_U_days


rct_data$All_cause_mortality_events_D1428=as.numeric(rct_data$All_cause_mortality_events_D1428)
pair_data_05_14=pairwise(studlab=Trial_ID,treat=Treat_Name,event=All_cause_mortality_events_D1428,n=n_Randomized,
                         data=rct_data,measure="RR",ref="Standard care")
pair_data_05_14$F_U_days1=pair_data_05_14$F_U_days
pair_data_05_14$First_author=pair_data_05_14$First_author



rct_data$All_cause_mortality_events_D60=as.numeric(rct_data$All_cause_mortality_events_D60)
pair_data_05_60=pairwise(studlab=Trial_ID,treat=Treat_Name,event=All_cause_mortality_events_D60,n=n_Randomized,
                         data=rct_data,measure="RR",ref="Standard care")
pair_data_05_60$F_U_days1=pair_data_05_60$F_U_days
pair_data_05_60$First_author=pair_data_05_60$First_author

rct_data$All_cause_mortality_events_D90=as.numeric(rct_data$All_cause_mortality_events_D90)
pair_data_05_90=pairwise(studlab=Trial_ID,treat=Treat_Name,event=All_cause_mortality_events_D90,n=n_Randomized,
                         data=rct_data,measure="RR",ref="Standard care")
pair_data_05_90$F_U_days1=pair_data_05_90$F_U_days
pair_data_05_90$First_author=pair_data_05_90$First_author


# rct_data$NEG_conv_n_D3=as.numeric(rct_data$NEG_conv_n_D3)
# pair_data_06_03=pairwise(studlab=Trial_ID,treat=Treat_Name,event=NEG_conv_n_D3,n=NEG_conv_denominator_d3,
#                          data=rct_data,measure="RR",ref="Standard care")
# pair_data_06_03$F_U_days1=pair_data_06_03$F_U_days

rct_data$NEG_conv_n_D7=as.numeric(rct_data$NEG_conv_n_D7)
pair_data_06_07=pairwise(studlab=Trial_ID,treat=Treat_Name,event=NEG_conv_n_D7,n=NEG_conv_denominator_d7,
                         data=rct_data,measure="RR",ref="Standard care")
pair_data_06_07$F_U_days1=pair_data_06_07$F_U_days
pair_data_06_07$First_author=pair_data_06_07$First_author



rct_data$Total_AE_D1428=as.numeric(rct_data$Total_AE)
pair_data_07_14=pairwise(studlab=Trial_ID,treat=Treat_Name,event=Total_AE_D1428,n=n_Randomized,
                         data=rct_data,measure="RR",ref="Standard care")
pair_data_07_14$F_U_days1=pair_data_07_14$F_U_days
pair_data_07_14$First_author=pair_data_07_14$First_author



rct_data$Serious_AE=as.numeric(rct_data$Serious_AE)
pair_data_08_14=pairwise(studlab=Trial_ID,treat=Treat_Name,event=Serious_AE,n=n_Randomized,
                         data=rct_data,measure="RR",ref="Standard care")
pair_data_08_14$F_U_days1=pair_data_08_14$F_U_days
pair_data_08_14$First_author=pair_data_08_14$First_author



#make treatment and control as indicated#
Intervention1=rep("NA",ncomp)
Intervention2=rep("NA",ncomp)     
Intervention1=ifelse(pair_data_01_14$Treatment_control1=="Treatment" | pair_data_01_14$Treatment_control1=="Treatment/control",pair_data_01_14$treat1,Intervention1)
Intervention1=ifelse(pair_data_01_14$Treatment_control1=="Control",pair_data_01_14$treat2,Intervention1)
Intervention2=ifelse(Intervention1==pair_data_01_14$treat1,pair_data_01_14$treat2,Intervention2)
Intervention2=ifelse(Intervention1==pair_data_01_14$treat2,pair_data_01_14$treat1,Intervention2)
Comparison=str_c(Intervention1, Intervention2, sep = " vs ", collapse = NULL)

# r1_01_07=rep(NA,ncomp)
# r2_01_07=rep(NA,ncomp)
# N1_01_07=rep(NA,ncomp)
# N2_01_07=rep(NA,ncomp)
# r1_01_07=ifelse(pair_data_01_07$treat1==Intervention1,pair_data_01_07$event1,r1_01_07)
# r1_01_07=ifelse(pair_data_01_07$treat2==Intervention1,pair_data_01_07$event2,r1_01_07)
# N1_01_07=ifelse(pair_data_01_07$treat1==Intervention1,pair_data_01_07$n1,N1_01_07)
# N1_01_07=ifelse(pair_data_01_07$treat2==Intervention1,pair_data_01_07$n2,N1_01_07)
# r2_01_07=ifelse(pair_data_01_07$treat2==Intervention2,pair_data_01_07$event2,r2_01_07)
# r2_01_07=ifelse(pair_data_01_07$treat1==Intervention2,pair_data_01_07$event1,r2_01_07)
# N2_01_07=ifelse(pair_data_01_07$treat2==Intervention2,pair_data_01_07$n2,N2_01_07)
# N2_01_07=ifelse(pair_data_01_07$treat1==Intervention2,pair_data_01_07$n1,N2_01_07)
# pair_data_01_07$Intervention1=Intervention1
# pair_data_01_07$Intervention2=Intervention2
# pair_data_01_07$Comparison=Comparison

r1_01_14=rep(NA,ncomp)
r2_01_14=rep(NA,ncomp)
N1_01_14=rep(NA,ncomp)
N2_01_14=rep(NA,ncomp)
r1_01_14=ifelse(pair_data_01_14$treat1==Intervention1,pair_data_01_14$event1,r1_01_14)
r1_01_14=ifelse(pair_data_01_14$treat2==Intervention1,pair_data_01_14$event2,r1_01_14)
N1_01_14=ifelse(pair_data_01_14$treat1==Intervention1,pair_data_01_14$n1,N1_01_14)
N1_01_14=ifelse(pair_data_01_14$treat2==Intervention1,pair_data_01_14$n2,N1_01_14)
r2_01_14=ifelse(pair_data_01_14$treat2==Intervention2,pair_data_01_14$event2,r2_01_14)
r2_01_14=ifelse(pair_data_01_14$treat1==Intervention2,pair_data_01_14$event1,r2_01_14)
N2_01_14=ifelse(pair_data_01_14$treat2==Intervention2,pair_data_01_14$n2,N2_01_14)
N2_01_14=ifelse(pair_data_01_14$treat1==Intervention2,pair_data_01_14$n1,N2_01_14)
pair_data_01_14$Intervention1=Intervention1
pair_data_01_14$Intervention2=Intervention2
pair_data_01_14$Comparison=Comparison


r1_01_60=rep(NA,ncomp)
r2_01_60=rep(NA,ncomp)
N1_01_60=rep(NA,ncomp)
N2_01_60=rep(NA,ncomp)
r1_01_60=ifelse(pair_data_01_60$treat1==Intervention1,pair_data_01_60$event1,r1_01_60)
r1_01_60=ifelse(pair_data_01_60$treat2==Intervention1,pair_data_01_60$event2,r1_01_60)
N1_01_60=ifelse(pair_data_01_60$treat1==Intervention1,pair_data_01_60$n1,N1_01_60)
N1_01_60=ifelse(pair_data_01_60$treat2==Intervention1,pair_data_01_60$n2,N1_01_60)
r2_01_60=ifelse(pair_data_01_60$treat2==Intervention2,pair_data_01_60$event2,r2_01_60)
r2_01_60=ifelse(pair_data_01_60$treat1==Intervention2,pair_data_01_60$event1,r2_01_60)
N2_01_60=ifelse(pair_data_01_60$treat2==Intervention2,pair_data_01_60$n2,N2_01_60)
N2_01_60=ifelse(pair_data_01_60$treat1==Intervention2,pair_data_01_60$n1,N2_01_60)
pair_data_01_60$Intervention1=Intervention1
pair_data_01_60$Intervention2=Intervention2
pair_data_01_60$Comparison=Comparison





# r1_03_07=rep(NA,ncomp)
# r2_03_07=rep(NA,ncomp)
# N1_03_07=rep(NA,ncomp)
# N2_03_07=rep(NA,ncomp)
# r1_03_07=ifelse(pair_data_03_07$treat1==Intervention1,pair_data_03_07$event1,r1_03_07)
# r1_03_07=ifelse(pair_data_03_07$treat2==Intervention1,pair_data_03_07$event2,r1_03_07)
# N1_03_07=ifelse(pair_data_03_07$treat1==Intervention1,pair_data_03_07$n1,N1_03_07)
# N1_03_07=ifelse(pair_data_03_07$treat2==Intervention1,pair_data_03_07$n2,N1_03_07)
# r2_03_07=ifelse(pair_data_03_07$treat2==Intervention2,pair_data_03_07$event2,r2_03_07)
# r2_03_07=ifelse(pair_data_03_07$treat1==Intervention2,pair_data_03_07$event1,r2_03_07)
# N2_03_07=ifelse(pair_data_03_07$treat2==Intervention2,pair_data_03_07$n2,N2_03_07)
# N2_03_07=ifelse(pair_data_03_07$treat1==Intervention2,pair_data_03_07$n1,N2_03_07)
# pair_data_03_07$Intervention1=Intervention1
# pair_data_03_07$Intervention2=Intervention2
# pair_data_03_07$Comparison=Comparison

# r1_03_14=rep(NA,ncomp)
# r2_03_14=rep(NA,ncomp)
# N1_03_14=rep(NA,ncomp)
# N2_03_14=rep(NA,ncomp)
# r1_03_14=ifelse(pair_data_03_14$treat1==Intervention1,pair_data_03_14$event1,r1_03_14)
# r1_03_14=ifelse(pair_data_03_14$treat2==Intervention1,pair_data_03_14$event2,r1_03_14)
# N1_03_14=ifelse(pair_data_03_14$treat1==Intervention1,pair_data_03_14$n1,N1_03_14)
# N1_03_14=ifelse(pair_data_03_14$treat2==Intervention1,pair_data_03_14$n2,N1_03_14)
# r2_03_14=ifelse(pair_data_03_14$treat2==Intervention2,pair_data_03_14$event2,r2_03_14)
# r2_03_14=ifelse(pair_data_03_14$treat1==Intervention2,pair_data_03_14$event1,r2_03_14)
# N2_03_14=ifelse(pair_data_03_14$treat2==Intervention2,pair_data_03_14$n2,N2_03_14)
# N2_03_14=ifelse(pair_data_03_14$treat1==Intervention2,pair_data_03_14$n1,N2_03_14)
# pair_data_03_14$Intervention1=Intervention1
# pair_data_03_14$Intervention2=Intervention2
# pair_data_03_14$Comparison=Comparison

# r1_04_07=rep(NA,ncomp)
# r2_04_07=rep(NA,ncomp)
# N1_04_07=rep(NA,ncomp)
# N2_04_07=rep(NA,ncomp)
# r1_04_07=ifelse(pair_data_04_07$treat1==Intervention1,pair_data_04_07$event1,r1_04_07)
# r1_04_07=ifelse(pair_data_04_07$treat2==Intervention1,pair_data_04_07$event2,r1_04_07)
# N1_04_07=ifelse(pair_data_04_07$treat1==Intervention1,pair_data_04_07$n1,N1_04_07)
# N1_04_07=ifelse(pair_data_04_07$treat2==Intervention1,pair_data_04_07$n2,N1_04_07)
# r2_04_07=ifelse(pair_data_04_07$treat2==Intervention2,pair_data_04_07$event2,r2_04_07)
# r2_04_07=ifelse(pair_data_04_07$treat1==Intervention2,pair_data_04_07$event1,r2_04_07)
# N2_04_07=ifelse(pair_data_04_07$treat2==Intervention2,pair_data_04_07$n2,N2_04_07)
# N2_04_07=ifelse(pair_data_04_07$treat1==Intervention2,pair_data_04_07$n1,N2_04_07)
# pair_data_04_07$Intervention1=Intervention1
# pair_data_04_07$Intervention2=Intervention2
# pair_data_04_07$Comparison=Comparison

r1_04_14=rep(NA,ncomp)
r2_04_14=rep(NA,ncomp)
N1_04_14=rep(NA,ncomp)
N2_04_14=rep(NA,ncomp)
r1_04_14=ifelse(pair_data_04_14$treat1==Intervention1,pair_data_04_14$event1,r1_04_14)
r1_04_14=ifelse(pair_data_04_14$treat2==Intervention1,pair_data_04_14$event2,r1_04_14)
N1_04_14=ifelse(pair_data_04_14$treat1==Intervention1,pair_data_04_14$n1,N1_04_14)
N1_04_14=ifelse(pair_data_04_14$treat2==Intervention1,pair_data_04_14$n2,N1_04_14)
r2_04_14=ifelse(pair_data_04_14$treat2==Intervention2,pair_data_04_14$event2,r2_04_14)
r2_04_14=ifelse(pair_data_04_14$treat1==Intervention2,pair_data_04_14$event1,r2_04_14)
N2_04_14=ifelse(pair_data_04_14$treat2==Intervention2,pair_data_04_14$n2,N2_04_14)
N2_04_14=ifelse(pair_data_04_14$treat1==Intervention2,pair_data_04_14$n1,N2_04_14)
pair_data_04_14$Intervention1=Intervention1
pair_data_04_14$Intervention2=Intervention2
pair_data_04_14$Comparison=Comparison

# r1_05_07=rep(NA,ncomp)
# r2_05_07=rep(NA,ncomp)
# N1_05_07=rep(NA,ncomp)
# N2_05_07=rep(NA,ncomp)
# r1_05_07=ifelse(pair_data_05_07$treat1==Intervention1,pair_data_05_07$event1,r1_05_07)
# r1_05_07=ifelse(pair_data_05_07$treat2==Intervention1,pair_data_05_07$event2,r1_05_07)
# N1_05_07=ifelse(pair_data_05_07$treat1==Intervention1,pair_data_05_07$n1,N1_05_07)
# N1_05_07=ifelse(pair_data_05_07$treat2==Intervention1,pair_data_05_07$n2,N1_05_07)
# r2_05_07=ifelse(pair_data_05_07$treat2==Intervention2,pair_data_05_07$event2,r2_05_07)
# r2_05_07=ifelse(pair_data_05_07$treat1==Intervention2,pair_data_05_07$event1,r2_05_07)
# N2_05_07=ifelse(pair_data_05_07$treat2==Intervention2,pair_data_05_07$n2,N2_05_07)
# N2_05_07=ifelse(pair_data_05_07$treat1==Intervention2,pair_data_05_07$n1,N2_05_07)
# pair_data_05_07$Intervention1=Intervention1
# pair_data_05_07$Intervention2=Intervention2
# pair_data_05_07$Comparison=Comparison

r1_05_14=rep(NA,ncomp)
r2_05_14=rep(NA,ncomp)
N1_05_14=rep(NA,ncomp)
N2_05_14=rep(NA,ncomp)
r1_05_14=ifelse(pair_data_05_14$treat1==Intervention1,pair_data_05_14$event1,r1_05_14)
r1_05_14=ifelse(pair_data_05_14$treat2==Intervention1,pair_data_05_14$event2,r1_05_14)
N1_05_14=ifelse(pair_data_05_14$treat1==Intervention1,pair_data_05_14$n1,N1_05_14)
N1_05_14=ifelse(pair_data_05_14$treat2==Intervention1,pair_data_05_14$n2,N1_05_14)
r2_05_14=ifelse(pair_data_05_14$treat2==Intervention2,pair_data_05_14$event2,r2_05_14)
r2_05_14=ifelse(pair_data_05_14$treat1==Intervention2,pair_data_05_14$event1,r2_05_14)
N2_05_14=ifelse(pair_data_05_14$treat2==Intervention2,pair_data_05_14$n2,N2_05_14)
N2_05_14=ifelse(pair_data_05_14$treat1==Intervention2,pair_data_05_14$n1,N2_05_14)
pair_data_05_14$Intervention1=Intervention1
pair_data_05_14$Intervention2=Intervention2
pair_data_05_14$Comparison=Comparison


r1_05_60=rep(NA,ncomp)
r2_05_60=rep(NA,ncomp)
N1_05_60=rep(NA,ncomp)
N2_05_60=rep(NA,ncomp)
r1_05_60=ifelse(pair_data_05_60$treat1==Intervention1,pair_data_05_60$event1,r1_05_60)
r1_05_60=ifelse(pair_data_05_60$treat2==Intervention1,pair_data_05_60$event2,r1_05_60)
N1_05_60=ifelse(pair_data_05_60$treat1==Intervention1,pair_data_05_60$n1,N1_05_60)
N1_05_60=ifelse(pair_data_05_60$treat2==Intervention1,pair_data_05_60$n2,N1_05_60)
r2_05_60=ifelse(pair_data_05_60$treat2==Intervention2,pair_data_05_60$event2,r2_05_60)
r2_05_60=ifelse(pair_data_05_60$treat1==Intervention2,pair_data_05_60$event1,r2_05_60)
N2_05_60=ifelse(pair_data_05_60$treat2==Intervention2,pair_data_05_60$n2,N2_05_60)
N2_05_60=ifelse(pair_data_05_60$treat1==Intervention2,pair_data_05_60$n1,N2_05_60)
pair_data_05_60$Intervention1=Intervention1
pair_data_05_60$Intervention2=Intervention2
pair_data_05_60$Comparison=Comparison



r1_05_90=rep(NA,ncomp)
r2_05_90=rep(NA,ncomp)
N1_05_90=rep(NA,ncomp)
N2_05_90=rep(NA,ncomp)
r1_05_90=ifelse(pair_data_05_90$treat1==Intervention1,pair_data_05_90$event1,r1_05_90)
r1_05_90=ifelse(pair_data_05_90$treat2==Intervention1,pair_data_05_90$event2,r1_05_90)
N1_05_90=ifelse(pair_data_05_90$treat1==Intervention1,pair_data_05_90$n1,N1_05_90)
N1_05_90=ifelse(pair_data_05_90$treat2==Intervention1,pair_data_05_90$n2,N1_05_90)
r2_05_90=ifelse(pair_data_05_90$treat2==Intervention2,pair_data_05_90$event2,r2_05_90)
r2_05_90=ifelse(pair_data_05_90$treat1==Intervention2,pair_data_05_90$event1,r2_05_90)
N2_05_90=ifelse(pair_data_05_90$treat2==Intervention2,pair_data_05_90$n2,N2_05_90)
N2_05_90=ifelse(pair_data_05_90$treat1==Intervention2,pair_data_05_90$n1,N2_05_90)
pair_data_05_90$Intervention1=Intervention1
pair_data_05_90$Intervention2=Intervention2
pair_data_05_90$Comparison=Comparison




# r1_06_03=rep(NA,ncomp)
# r2_06_03=rep(NA,ncomp)
# N1_06_03=rep(NA,ncomp)
# N2_06_03=rep(NA,ncomp)
# r1_06_03=ifelse(pair_data_06_03$treat1==Intervention1,pair_data_06_03$event1,r1_06_03)
# r1_06_03=ifelse(pair_data_06_03$treat2==Intervention1,pair_data_06_03$event2,r1_06_03)
# N1_06_03=ifelse(pair_data_06_03$treat1==Intervention1,pair_data_06_03$n1,N1_06_03)
# N1_06_03=ifelse(pair_data_06_03$treat2==Intervention1,pair_data_06_03$n2,N1_06_03)
# r2_06_03=ifelse(pair_data_06_03$treat2==Intervention2,pair_data_06_03$event2,r2_06_03)
# r2_06_03=ifelse(pair_data_06_03$treat1==Intervention2,pair_data_06_03$event1,r2_06_03)
# N2_06_03=ifelse(pair_data_06_03$treat2==Intervention2,pair_data_06_03$n2,N2_06_03)
# N2_06_03=ifelse(pair_data_06_03$treat1==Intervention2,pair_data_06_03$n1,N2_06_03)
# pair_data_06_03$Intervention1=Intervention1
# pair_data_06_03$Intervention2=Intervention2
# pair_data_06_03$Comparison=Comparison

r1_06_07=rep(NA,ncomp)
r2_06_07=rep(NA,ncomp)
N1_06_07=rep(NA,ncomp)
N2_06_07=rep(NA,ncomp)
r1_06_07=ifelse(pair_data_06_07$treat1==Intervention1,pair_data_06_07$event1,r1_06_07)
r1_06_07=ifelse(pair_data_06_07$treat2==Intervention1,pair_data_06_07$event2,r1_06_07)
N1_06_07=ifelse(pair_data_06_07$treat1==Intervention1,pair_data_06_07$n1,N1_06_07)
N1_06_07=ifelse(pair_data_06_07$treat2==Intervention1,pair_data_06_07$n2,N1_06_07)
r2_06_07=ifelse(pair_data_06_07$treat2==Intervention2,pair_data_06_07$event2,r2_06_07)
r2_06_07=ifelse(pair_data_06_07$treat1==Intervention2,pair_data_06_07$event1,r2_06_07)
N2_06_07=ifelse(pair_data_06_07$treat2==Intervention2,pair_data_06_07$n2,N2_06_07)
N2_06_07=ifelse(pair_data_06_07$treat1==Intervention2,pair_data_06_07$n1,N2_06_07)
pair_data_06_07$Intervention1=Intervention1
pair_data_06_07$Intervention2=Intervention2
pair_data_06_07$Comparison=Comparison



r1_07_14=rep(NA,ncomp)
r2_07_14=rep(NA,ncomp)
N1_07_14=rep(NA,ncomp)
N2_07_14=rep(NA,ncomp)
r1_07_14=ifelse(pair_data_07_14$treat1==Intervention1,pair_data_07_14$event1,r1_07_14)
r1_07_14=ifelse(pair_data_07_14$treat2==Intervention1,pair_data_07_14$event2,r1_07_14)
N1_07_14=ifelse(pair_data_07_14$treat1==Intervention1,pair_data_07_14$n1,N1_07_14)
N1_07_14=ifelse(pair_data_07_14$treat2==Intervention1,pair_data_07_14$n2,N1_07_14)
r2_07_14=ifelse(pair_data_07_14$treat2==Intervention2,pair_data_07_14$event2,r2_07_14)
r2_07_14=ifelse(pair_data_07_14$treat1==Intervention2,pair_data_07_14$event1,r2_07_14)
N2_07_14=ifelse(pair_data_07_14$treat2==Intervention2,pair_data_07_14$n2,N2_07_14)
N2_07_14=ifelse(pair_data_07_14$treat1==Intervention2,pair_data_07_14$n1,N2_07_14)
pair_data_07_14$Intervention1=Intervention1
pair_data_07_14$Intervention2=Intervention2
pair_data_07_14$Comparison=Comparison


r1_08_14=rep(NA,ncomp)
r2_08_14=rep(NA,ncomp)
N1_08_14=rep(NA,ncomp)
N2_08_14=rep(NA,ncomp)
r1_08_14=ifelse(pair_data_08_14$treat1==Intervention1,pair_data_08_14$event1,r1_08_14)
r1_08_14=ifelse(pair_data_08_14$treat2==Intervention1,pair_data_08_14$event2,r1_08_14)
N1_08_14=ifelse(pair_data_08_14$treat1==Intervention1,pair_data_08_14$n1,N1_08_14)
N1_08_14=ifelse(pair_data_08_14$treat2==Intervention1,pair_data_08_14$n2,N1_08_14)
r2_08_14=ifelse(pair_data_08_14$treat2==Intervention2,pair_data_08_14$event2,r2_08_14)
r2_08_14=ifelse(pair_data_08_14$treat1==Intervention2,pair_data_08_14$event1,r2_08_14)
N2_08_14=ifelse(pair_data_08_14$treat2==Intervention2,pair_data_08_14$n2,N2_08_14)
N2_08_14=ifelse(pair_data_08_14$treat1==Intervention2,pair_data_08_14$n1,N2_08_14)
pair_data_08_14$Intervention1=Intervention1
pair_data_08_14$Intervention2=Intervention2
pair_data_08_14$Comparison=Comparison



################################ Time to event outcomes ####################

rct_data$Clinical_improvement_D7_n=as.numeric(rct_data$Clinical_improvement_D7_n)
pair_data=pairwise(studlab=Trial_ID,treat=Treat_Name,event=Clinical_improvement_D7_n,n=n_Randomized,
                   data=rct_data,measure="RR",ref="Standard care")
pair_data$F_U_days1=pair_data$F_U_days
pair_data$First_author=pair_data$First_author
ncomp=length(pair_data$treat1)


pair_data$Time_to_clinical_improvement_HR=as.numeric(pair_data$Time_to_clinical_improvement_HR1)
pair_data$Time_to_clinical_improvement_HR_CI_Lower=as.numeric(pair_data$Time_to_clinical_improvement_HR_CI_Lower1)
pair_data$Time_to_clinical_improvement_HR_CI_Upper=as.numeric(pair_data$Time_to_clinical_improvement_HR_CI_Upper1)


pair_data$time_to_death_HR=as.numeric(pair_data$time_to_death_HR1)
pair_data$time_to_death_lci=as.numeric(pair_data$time_to_death_lci1)
pair_data$time_to_death_uci=as.numeric(pair_data$time_to_death_uci1)


pair_data$Time_to_NEG_conv_HR=rep(NA,ncomp)
pair_data$Time_to_NEG_conv_HR_LowerCI=rep(NA,ncomp)
pair_data$Time_to_NEG_conv_HR_UpperCI=rep(NA,ncomp)
pair_data$Time_to_NEG_conv_HR=ifelse(is.na(pair_data$Time_to_NEG_conv_HR2),pair_data$Time_to_NEG_conv_HR1,pair_data$Time_to_NEG_conv_HR)
pair_data$Time_to_NEG_conv_HR_LowerCI=ifelse(is.na(pair_data$Time_to_NEG_conv_HR_LowerCI2),pair_data$Time_to_NEG_conv_HR_LowerCI1,pair_data$Time_to_NEG_conv_HR_LowerCI)
pair_data$Time_to_NEG_conv_HR_UpperCI=ifelse(is.na(pair_data$Time_to_NEG_conv_HR_UpperCI2),pair_data$Time_to_NEG_conv_HR_UpperCI1,pair_data$Time_to_NEG_conv_HR_UpperCI)


pair_data$Time_to_NEG_conv_HR=as.numeric(pair_data$Time_to_NEG_conv_HR)
pair_data$Time_to_NEG_conv_HR_LowerCI=as.numeric(pair_data$Time_to_NEG_conv_HR_LowerCI)
pair_data$Time_to_NEG_conv_HR_UpperCI=as.numeric(pair_data$Time_to_NEG_conv_HR_UpperCI)


# pair_data$Time_to_score_6_and_above_HR=as.numeric(pair_data$Time_to_score_6_and_above_HR)
# pair_data$Time_to_score_6_and_above_LCI=as.numeric(pair_data$Time_to_score_6_and_above_LCI)
# pair_data$Time_to_score_6_and_above_UCI=as.numeric(pair_data$Time_to_score_6_and_above_UCI)


pair_data$Time_to_score_7_and_above_HR=as.numeric(pair_data$Time_to_score_7_and_above_HR1)
pair_data$Time_to_score_7_and_above_LCI=as.numeric(pair_data$Time_to_score_7_and_above_LCI1)
pair_data$Time_to_score_7_and_above_UCI=as.numeric(pair_data$Time_to_score_7_and_above_UCI1)


Intervention1=rep("NA",ncomp)
Intervention2=rep("NA",ncomp)     
Intervention1=ifelse(pair_data$Treatment_control1=="Treatment" | pair_data$Treatment_control1=="Treatment/control",pair_data$treat1,Intervention1)
Intervention1=ifelse(pair_data$Treatment_control1=="Control",pair_data$treat2,Intervention1)
Intervention2=ifelse(Intervention1==pair_data$treat1,pair_data$treat2,Intervention2)
Intervention2=ifelse(Intervention1==pair_data$treat2,pair_data$treat1,Intervention2)
Intervention1=trimws(Intervention1)
Intervention2=trimws(Intervention2)
pair_data$treat1=trimws(pair_data$treat1)
pair_data$treat2=trimws(pair_data$treat2)
Comparison=str_c(Intervention1, Intervention2, sep = " vs ", collapse = NULL)


N1=rep(NA,ncomp)
N2=rep(NA,ncomp)
N1=ifelse(pair_data$treat1==Intervention1,pair_data$n1,N1)
N1=ifelse(pair_data$treat2==Intervention1,pair_data$n2,N1)
N2=ifelse(pair_data$treat2==Intervention2,pair_data$n2,N2)
N2=ifelse(pair_data$treat1==Intervention2,pair_data$n1,N2)
pair_data$Intervention1=Intervention1
pair_data$Intervention2=Intervention2
pair_data$Comparison=Comparison



##########################################################################


##########################################################################


#codes for comparisons#

#### comp
comp=rep(0,ncomp)
comp=ifelse(Comparison=="Lopinavir-Ritonavir vs Standard care",1,comp)
comp=ifelse(Comparison=="Lopinavir-Ritonavir vs Placebo",1,comp)
comp=ifelse(Comparison=="Umifenovir vs Standard care",2,comp)
comp=ifelse(Comparison=="Hydroxychloroquine vs Standard care",3,comp)
comp=ifelse(Comparison=="Hydroxychloroquine vs Placebo",3,comp)
comp=ifelse(Comparison=="Lopinavir-Ritonavir vs Umifenovir",4,comp) # CHECK # CHECK AGAIN WITH ELISE.
comp=ifelse(Comparison=="Favipiravir vs Umifenovir",5,comp)
comp=ifelse(Comparison=="HFNO vs Mask",6,comp)
comp=ifelse(Comparison=="video training vs live instructor-led training",7,comp)
comp=ifelse(Comparison=="Chloroquine 600mg vs Chloroquine 450mg",8,comp)
comp=ifelse(Comparison=="Chloroquine vs Lopinavir/Ritonavir",9,comp)
comp=ifelse(Comparison=="a-Lipoic acid vs Placebo",10,comp)
comp=ifelse(Comparison=="Novaferon+LPV/r vs Lopinavir-Ritonavir",11,comp)
comp=ifelse(Comparison=="Novaferon+LPV/r vs Novaferon",12,comp)
comp=ifelse(Comparison=="Novaferon vs Lopinavir-Ritonavir",13,comp)
comp=ifelse(Comparison=="Remdesivir vs Placebo",14,comp) 
comp=ifelse(Comparison=="Baloxavir marboxil vs Standard care",15,comp)
comp=ifelse(Comparison=="Favipiravir vs Baloxavir marboxil",16,comp) 
comp=ifelse(Comparison=="Favipiravir vs LPV/r/dar/cob+umif+int-a",17,comp) 
comp=ifelse(Comparison=="LPV/r+RBV+IFN beta1 vs Lopinavir/Ritonavir",18,comp)
comp=ifelse(Comparison=="Chloroquine vs Standard care",19,comp)
comp=ifelse(Comparison=="CQ+AZM/CLM vs Standard care",20,comp)
comp=ifelse(Comparison=="HCQ+AZI/CLM vs Standard care",21,comp)
comp=ifelse(Comparison=="Convalescent plasma vs Standard care",22,comp)
comp=ifelse(Comparison=="Convalescent plasma vs Placebo",22,comp)
comp=ifelse(Comparison=="Convalescent plasma vs Fresh frozen plasma",22,comp) # ex 133
comp=ifelse(Comparison=="Convalescent plasma vs Control plasma",22,comp) # ex 133
comp=ifelse(Comparison=="Convalescent plasma vs Standard plasma",22,comp)
comp=ifelse(Comparison=="Famotidine vs Standard care",23,comp)
comp=ifelse(Comparison=="Remdesivir 5 days vs Remdesivir 10 days",24,comp)
#comp=ifelse(Comparison=="Ruxolitinib vs Placebo",25,comp)
comp=ifelse(Comparison=="Ruxolitinib vs Vitamin C",25,comp)
comp=ifelse(Comparison=="Interferon beta-1a vs Standard care",26,comp)
comp=ifelse(Comparison=="Interferon beta-1b vs Standard care",26,comp)
comp=ifelse(Comparison=="INF-beta-1a+INF-beta-1b vs Standard care",26,comp)
comp=ifelse(Comparison=="INF a-2b+INF gamma vs Interferon a-2b",27,comp)
comp=ifelse(Comparison=="Tocilizumab vs Standard care",28,comp)
comp=ifelse(Comparison=="Tocilizumab vs Placebo",28,comp)
comp=ifelse(Comparison=="HCQ+AZM vs Hydroxychloroquine",29,comp)
comp=ifelse(Comparison=="Colchicine vs Standard care",30,comp)
comp=ifelse(Comparison=="Colchicine vs Placebo",30,comp) #NEW
comp=ifelse(Comparison=="Azithromycin vs Standard care",31,comp)
comp=ifelse(Comparison=="Social distancing at sports vs No access to sports",32,comp)
comp=ifelse(Comparison=="HCQ+AZM vs Standard care",33,comp)
comp=ifelse(Comparison=="HCQ+AZM vs Placebo",33,comp)
#comp=ifelse(Comparison=="Dexamethasone vs Standard care",34,comp) #It's 40
comp=ifelse(Comparison=="IV Ig vs Standard care",35,comp) 
comp=ifelse(Comparison=="Ivermectin vs Standard care",36,comp)
comp=ifelse(Comparison=="Ivermectin vs Placebo",36,comp)
comp=ifelse(Comparison=="Ivermectin vs Standard care-Placebo",36,comp)
comp=ifelse(Comparison=="Hydroxychloroquine vs Chloroquine",37,comp)
comp=ifelse(Comparison=="Respiratory rehabilitation vs No respiratory rehabilitation",38,comp)
comp=ifelse(Comparison=="Corticosteroids vs Standard care",39,comp)
comp=ifelse(Comparison=="Methylprednisolone vs Standard care",39,comp)
comp=ifelse(Comparison=="Methylprednisolone vs Placebo",39,comp)#Added by Thodoris
comp=ifelse(Comparison=="Dexamethasone vs Standard care",39,comp) # Added by Thodoris
comp=ifelse(Comparison=="Hydrocortisone vs Placebo",39,comp)
comp=ifelse(Comparison=="Hydrocortisone vs Standard care",39,comp)
comp=ifelse(Comparison=="Fixed-dose hydrocortisone vs Standard care",39,comp)
comp=ifelse(Comparison=="IVMP Pulse vs Standard care",39,comp)
comp=ifelse(Comparison=="IV pulse MPS vs Standard care",39,comp)

#comp=ifelse(Comparison=="Shock-dependent hydrocortisone vs Standard care",39,comp)
comp=ifelse(Comparison=="Darunavir+cobicistat vs Standard care",41,comp)
comp=ifelse(Comparison=="Favipiravir 1800/800mg vs Favipiravir 1600/600mg",42,comp)

#### ADDED BY THODORIS 

comp=ifelse(Comparison=="Favipiravir vs Standard care",43,comp)
#comp=ifelse(Comparison=="Favipiravir 1600/600mg vs Standard care",43,comp)
#comp=ifelse(Comparison=="Methylprednisolone vs Placebo",44,comp)
comp=ifelse(Comparison=="Azvudine vs Standard care",45,comp)
comp=ifelse(Comparison=="IFX-1 vs Standard care",46,comp)
comp=ifelse(Comparison=="Telmisartan vs Standard care",47,comp)
comp=ifelse(Comparison=="RBV+LPV/r+IFN alpha vs RBV+IFN alpha",48,comp) #
comp=ifelse(Comparison=="LPV/r+IFN alpha vs RBV+IFN alpha",49,comp) #
comp=ifelse(Comparison=="RBV+LPV/r+IFN alpha vs LPV/r+IFN alpha",50,comp) #
comp=ifelse(Comparison=="Aprepitant+Dex vs Dexamethasone",51,comp)
comp=ifelse(Comparison=="Remdesivir 5 days vs Standard care",52,comp)
comp=ifelse(Comparison=="Remdesivir vs Standard care",14,comp) ### initially this was 53
#comp=ifelse(Comparison=="Remdesivir 10 days vs Standard care",14,comp)
comp=ifelse(Comparison=="Icatibant vs Standard care",54,comp)
comp=ifelse(Comparison=="iC1e/K vs Standard care",55,comp)
comp=ifelse(Comparison=="Icatibant vs iC1e/K",56,comp)
comp=ifelse(Comparison=="Auxora vs Standard care",57,comp)
comp=ifelse(Comparison=="Sofosbuvir-Daclatasvir vs Standard care",58,comp)
comp=ifelse(Comparison=="Sofosbuvir-Daclatasvir vs Placebo",58,comp)
comp=ifelse(Comparison=="hUC-MSC vs Standard care",60,comp)
comp=ifelse(Comparison=="hUC-MSC vs Placebo",60,comp)
comp=ifelse(Comparison=="UC-MSC vs Standard care",60,comp)
comp=ifelse(Comparison=="UC-MSC vs Placebo",60,comp)
comp=ifelse(Comparison=="SOF/DCV+RBV vs Standard care",61,comp)
comp=ifelse(Comparison=="Vitamin C vs Placebo",63,comp)
comp=ifelse(Comparison=="Vitamin C vs Standard care",63,comp)
#comp=ifelse(Comparison=="Calcifediol vs Standard care",83,comp)
comp=ifelse(Comparison=="Bromhexine vs Standard care",84,comp)
comp=ifelse(Comparison=="Bromhexine vs Placebo",84,comp)
comp=ifelse(Comparison=="IVM+DX vs HCQ+AZM",85,comp)
comp=ifelse(Comparison=="N-electrolyzed saline vs Standard care",86,comp)
comp=ifelse(Comparison=="rhG-CSF vs Standard care",87,comp)
comp=ifelse(Comparison=="Lincomycin vs Azithromycin",91,comp)
comp=ifelse(Comparison=="HCQ+LPV/r vs HCQ+Umif",4,comp)
comp=ifelse(Comparison=="Favipiravir Early vs Favipiravir Late",92,comp)
comp=ifelse(Comparison=="LEF+IFN alpha2a vs IFN alpha2a",93,comp)
comp=ifelse(Comparison=="CIGB-325 vs Standard care",94,comp)
comp=ifelse(Comparison=="rSIFN-co vs Interferon alpha",95,comp)
comp=ifelse(Comparison=="Triazavirin vs Placebo",96,comp)
comp=ifelse(Comparison=="Therap EX vs Prophylactic AC",97,comp)
comp=ifelse(Comparison=="Therap UFH vs Prophylactic AC",97,comp)
comp=ifelse(Comparison=="Early CP vs Late CP",98,comp)
comp=ifelse(Comparison=="IFN-k+TFF2 vs Standard care",101,comp)
comp=ifelse(Comparison=="CMCS vs Placebo",102,comp)
comp=ifelse(Comparison=="Favipiravir vs Favipiravir+Tocilizumab",104,comp)
comp=ifelse(Comparison=="Tocilizumab vs Favipiravir+Tocilizumab",105,comp)
comp=ifelse(Comparison=="Favipiravir vs Tocilizumab",106,comp)
comp=ifelse(Comparison=="Nitazoxanide vs Placebo",118,comp)
comp=ifelse(Comparison=="Nitazoxanide vs Standard care",118,comp)
comp=ifelse(Comparison=="IVM+DX vs Placebo",134,comp)
comp=ifelse(Comparison=="IVM+DX vs Standard care",134,comp)
comp=ifelse(Comparison=="Febuxostat vs Hydroxychloroquine",135,comp)
comp=ifelse(Comparison=="Oxygen-Ozone+Probiotic vs Standard care",137,comp)
comp=ifelse(Comparison=="LY-CoV555 vs Placebo",138,comp)
comp=ifelse(Comparison=="FAV+IFN beta-1b vs Hydroxychloroquine",144,comp)
comp=ifelse(Comparison=="Fluvoxamine vs Placebo",155,comp)
comp=ifelse(Comparison=="Hydroxychloroquine vs Azithromycin",157,comp)
comp=ifelse(Comparison=="Dutasteride vs Placebo",159,comp)
comp=ifelse(Comparison=="Peginterferon Lambda-1a vs Placebo",160,comp)
comp=ifelse(Comparison=="Peginterferon Lambda-1 vs Placebo",160,comp)
comp=ifelse(Comparison=="Hydroxychloroquine+Zinc vs Hydroxychloroquine",162,comp)
comp=ifelse(Comparison=="SNG001 vs Placebo",163,comp)
comp=ifelse(Comparison=="Calcifediol vs Standard care",164,comp)
comp=ifelse(Comparison=="Cholecalciferol vs Placebo",164,comp)
comp=ifelse(Comparison=="t-He/O2 vs Standard care",165,comp)
comp=ifelse(Comparison=="Itolizumab vs Standard care",167,comp)
comp=ifelse(Comparison=="IVM+DX vs Ivermectin",168,comp)
comp=ifelse(Comparison=="Prolectin-M vs Standard care",169,comp)
comp=ifelse(Comparison=="Sulodexide vs Placebo",175,comp)
comp=ifelse(Comparison=="Baricitinib+Remdesivir vs Placebo+Remdesivir",176,comp)
comp=ifelse(Comparison=="PBMT-sMF vs Standard care",177,comp)
comp=ifelse(Comparison=="PBMT-sMF vs Placebo",177,comp)
comp=ifelse(Comparison=="Tocilizumab vs Sarilumab",178,comp)
comp=ifelse(Comparison=="Sarilumab vs Standard care",179,comp)
comp=ifelse(Comparison=="Sarilumab vs Placebo",179,comp)
comp=ifelse(Comparison=="REGN-COV2 vs Placebo",192,comp)
comp=ifelse(Comparison=="Itraconazole vs Standard care",193,comp)
comp=ifelse(Comparison=="Progesterone vs Standard care",195,comp)
comp=ifelse(Comparison=="Ivermectin 6mg vs Ivermectin 12mg",198,comp)
comp=ifelse(Comparison=="Ivermectin vs Lopinavir-Ritonavir",199,comp)
comp=ifelse(Comparison=="Levamisole vs Placebo",211,comp)
comp=ifelse(Comparison=="ARB/ACEI vs Discontinue ARB/ACEI",212,comp)
comp=ifelse(Comparison=="Proxalutamide vs Placebo",214,comp)
comp=ifelse(Comparison=="IFN-beta-1a High vs IFN-beta-1a Low",215,comp)
comp=ifelse(Comparison=="Ivermectin vs Hydroxychloroquine",222,comp)
comp=ifelse(Comparison=="Anakinra vs Standard care",223,comp)
comp=ifelse(Comparison=="Lopinavir-Ritonavir vs LPV/r+IFN beta-1a",224,comp)
comp=ifelse(Comparison=="Lopinavir-Ritonavir vs Hydroxychloroquine",225,comp)
comp=ifelse(Comparison=="LPV/r+IFN beta-1a vs Hydroxychloroquine",226,comp)
comp=ifelse(Comparison=="LPV/r+IFN beta-1a vs Standard care",227,comp)
comp=ifelse(Comparison=="Favipiravir vs Chloroquine",228,comp)
comp=ifelse(Comparison=="Compound 21 vs Placebo",229,comp)
comp=ifelse(Comparison=="Methylprednisolone vs Dexamethasone",231,comp)
comp=ifelse(Comparison=="SOF/LDP vs Standard care",243,comp)
comp=ifelse(Comparison=="HCQ+SOF vs HCQ+LPV/r",244,comp)
comp=ifelse(Comparison=="Budesonide vs Standard care",245,comp)
comp=ifelse(Comparison=="Budesonide vs Placebo",245,comp)
comp=ifelse(Comparison=="Oxygen-Ozone vs Standard care",249,comp)
comp=ifelse(Comparison=="Methisoprinol vs Standard care",251,comp)
comp=ifelse(Comparison=="INM005 vs Standard care",252,comp)
comp=ifelse(Comparison=="INM005 vs Placebo",252,comp)
comp=ifelse(Comparison=="LY-CoV555+LY-CoV016 vs Placebo",254,comp)
comp=ifelse(Comparison=="Chloroquine (nasal drops) vs Standard care",261,comp)
comp=ifelse(Comparison=="Zinc vs Standard care",277,comp)
comp=ifelse(Comparison=="Zinc vs Placebo",277,comp)
comp=ifelse(Comparison=="Interferon beta-1b vs Interferon beta-1a",280,comp)
comp=ifelse(Comparison=="Losartan vs Amlodipine",287,comp)
comp=ifelse(Comparison=="Vitamin C vs Zinc",289,comp)
comp=ifelse(Comparison=="Vitamin C vs Vitamin C+Zinc",290,comp)
comp=ifelse(Comparison=="Zinc vs Vitamin C+Zinc",291,comp)
comp=ifelse(Comparison=="Vitamin C+Zinc vs Standard care",292,comp)
comp=ifelse(Comparison=="Azithromycin vs Clarithromycin",293,comp)
comp=ifelse(Comparison=="Clarithromycin vs Standard care",294,comp)
comp=ifelse(Comparison=="Favipiravir vs Lopinavir-Ritonavir",295,comp)
comp=ifelse(Comparison=="Interferon alpha-2b vs Standard care",297,comp)
comp=ifelse(Comparison=="Intermediate dose vs Standard dose",298,comp)
comp=ifelse(Comparison=="Mavrilimumab vs Placebo",299,comp)
comp=ifelse(Comparison=="Sofosbuvir vs LPV/r+IFN beta-1a",303,comp)
comp=ifelse(Comparison=="Sofosbuvir vs LPV/r+IFN beta-1a",303,comp)
comp=ifelse(Comparison=="SOF/DCV vs LPV/r",305,comp)
comp=ifelse(Comparison=="TD-0903 vs Placebo",307,comp)
comp=ifelse(Comparison=="TD-0903 vs Standard care",307,comp)
comp=ifelse(Comparison=="Helmet NIV vs HFNO",314,comp)
comp=ifelse(Comparison=="KB109 vs Standard care",324,comp)
comp=ifelse(Comparison=="KB109 vs Placebo",324,comp)
comp=ifelse(Comparison=="Aviptadil vs Placebo",327,comp)
comp=ifelse(Comparison=="Aviptadil vs Standard care",327,comp)
comp=ifelse(Comparison=="Thalidomide vs Standard care",328,comp)
comp=ifelse(Comparison=="HCQ+Umif vs Hydroxychloroquine",329,comp)
comp=ifelse(Comparison=="Ivermectin 100 mcg/kg vs Ivermectin 200 mcg/kg",331,comp)
comp=ifelse(Comparison=="Ivermectin 100 mcg/kg vs Ivermectin 400 mcg/kg",332,comp)
comp=ifelse(Comparison=="Ivermectin 200 mcg/kg vs Ivermectin 400 mcg/kg",334,comp)
comp=ifelse(Comparison=="CERC-002 vs Placebo",338,comp)
comp=ifelse(Comparison=="CERC-002 vs Standard care",338,comp)
comp=ifelse(Comparison=="HBOT vs Placebo",343,comp)
comp=ifelse(Comparison=="HBOT vs Standard care",343,comp)
comp=ifelse(Comparison=="TD-0903 1 mg vs TD-0903 3 mg",344,comp)
comp=ifelse(Comparison=="TD-0903 1 mg vs TD-0903 10 mg",345,comp)
comp=ifelse(Comparison=="TD-0903 3 mg vs TD-0903 10 mg",347,comp)
comp=ifelse(Comparison=="CT-P59 40mg/kg vs CT-P59 80mg/kg",350,comp)
comp=ifelse(Comparison=="CT-P59 vs Placebo",351,comp)
comp=ifelse(Comparison=="Canakinumab 600mg vs Canakinumab 300mg",354,comp)
comp=ifelse(Comparison=="Canakinumab vs Placebo",357,comp)
comp=ifelse(Comparison=="Otilimab vs Placebo",359,comp)
comp=ifelse(Comparison=="Otilimab vs Standard care",359,comp)
comp=ifelse(Comparison=="Ammonium chloride vs Placebo",361,comp)
comp=ifelse(Comparison=="Ammonium chloride vs Standard care",361,comp)
comp=ifelse(Comparison=="Atorvastatin vs Standard care",368,comp)
comp=ifelse(Comparison=="Atorvastatin vs Aspirin",369,comp)
comp=ifelse(Comparison=="Atorvastatin vs AT+ASP",370,comp)
comp=ifelse(Comparison=="Aspirin vs Standard care",371,comp)
comp=ifelse(Comparison=="Aspirin vs AT+ASP",372,comp)
comp=ifelse(Comparison=="AT+ASP vs Standard care",373,comp)










#codes for severity#
sev=rep(0,ncomp)
sev=ifelse(pair_data_01_14$Research_question=="Mild",1,sev)
sev=ifelse(pair_data_01_14$Research_question=="Mild/moderate",2,sev)
sev=ifelse(pair_data_01_14$Research_question=="Moderate",3,sev)
sev=ifelse(pair_data_01_14$Research_question=="Mild to severe",4,sev)
sev=ifelse(pair_data_01_14$Research_question=="Moderate/severe",5,sev)
sev=ifelse(pair_data_01_14$Research_question=="Mild to critical",6,sev)
sev=ifelse(pair_data_01_14$Research_question=="Moderate to severe",7,sev)
sev=ifelse(pair_data_01_14$Research_question=="Moderate to critical",8,sev)
sev=ifelse(pair_data_01_14$Research_question=="Severe",9,sev)
sev=ifelse(pair_data_01_14$Research_question=="Severe/critical",10,sev)
sev=ifelse(pair_data_01_14$Research_question=="Unclear severity",11,sev)
sev=ifelse(pair_data_01_14$Research_question=="Critical",12,sev)

#subgroups#
subgroup=rep(0,ncomp)
subgroup=ifelse(sev>1 & sev<12,1,subgroup)
subgroup=ifelse(sev==12,2,subgroup)

# pair_data_01_07$comp=comp
# pair_data_01_07$r1=r1_01_07
# pair_data_01_07$r2=r2_01_07
# pair_data_01_07$N1=N1_01_07
# pair_data_01_07$N2=N2_01_07
# pair_data_01_07$sev=sev
# pair_data_01_07$subgroup=subgroup

pair_data_01_14$comp=comp
pair_data_01_14$r1=r1_01_14
pair_data_01_14$r2=r2_01_14
pair_data_01_14$N1=N1_01_14
pair_data_01_14$N2=N2_01_14
pair_data_01_14$sev=sev
pair_data_01_14$subgroup=subgroup

pair_data_01_60$comp=comp
pair_data_01_60$r1=r1_01_60
pair_data_01_60$r2=r2_01_60
pair_data_01_60$N1=N1_01_60
pair_data_01_60$N2=N2_01_60
pair_data_01_60$sev=sev
pair_data_01_60$subgroup=subgroup


# pair_data_03_07$comp=comp
# pair_data_03_07$r1=r1_03_07
# pair_data_03_07$r2=r2_03_07
# pair_data_03_07$N1=N1_03_07
# pair_data_03_07$N2=N2_03_07
# pair_data_03_07$sev=sev
# pair_data_03_07$subgroup=subgroup

# pair_data_03_14$comp=comp
# pair_data_03_14$r1=r1_03_14
# pair_data_03_14$r2=r2_03_14
# pair_data_03_14$N1=N1_03_14
# pair_data_03_14$N2=N2_03_14
# pair_data_03_14$sev=sev
# pair_data_03_14$subgroup=subgroup

# pair_data_04_07$comp=comp
# pair_data_04_07$r1=r1_04_07
# pair_data_04_07$r2=r2_04_07
# pair_data_04_07$N1=N1_04_07
# pair_data_04_07$N2=N2_04_07
# pair_data_04_07$sev=sev
# pair_data_04_07$subgroup=subgroup

pair_data_04_14$comp=comp
pair_data_04_14$r1=r1_04_14
pair_data_04_14$r2=r2_04_14
pair_data_04_14$N1=N1_04_14
pair_data_04_14$N2=N2_04_14
pair_data_04_14$sev=sev
pair_data_04_14$subgroup=subgroup

# pair_data_05_07$comp=comp
# pair_data_05_07$r1=r1_05_07
# pair_data_05_07$r2=r2_05_07
# pair_data_05_07$N1=N1_05_07
# pair_data_05_07$N2=N2_05_07
# pair_data_05_07$sev=sev
# pair_data_05_07$subgroup=subgroup

pair_data_05_14$comp=comp
pair_data_05_14$r1=r1_05_14
pair_data_05_14$r2=r2_05_14
pair_data_05_14$N1=N1_05_14
pair_data_05_14$N2=N2_05_14
pair_data_05_14$sev=sev
pair_data_05_14$subgroup=subgroup


pair_data_05_60$comp=comp
pair_data_05_60$r1=r1_05_60
pair_data_05_60$r2=r2_05_60
pair_data_05_60$N1=N1_05_60
pair_data_05_60$N2=N2_05_60
pair_data_05_60$sev=sev
pair_data_05_60$subgroup=subgroup


pair_data_05_90$comp=comp
pair_data_05_90$r1=r1_05_90
pair_data_05_90$r2=r2_05_90
pair_data_05_90$N1=N1_05_90
pair_data_05_90$N2=N2_05_90
pair_data_05_90$sev=sev
pair_data_05_90$subgroup=subgroup



# pair_data_06_03$comp=comp
# pair_data_06_03$r1=r1_06_03
# pair_data_06_03$r2=r2_06_03
# pair_data_06_03$N1=N1_06_03
# pair_data_06_03$N2=N2_06_03
# pair_data_06_03$sev=sev
# pair_data_06_03$subgroup=subgroup

pair_data_06_07$comp=comp
pair_data_06_07$r1=r1_06_07
pair_data_06_07$r2=r2_06_07
pair_data_06_07$N1=N1_06_07
pair_data_06_07$N2=N2_06_07
pair_data_06_07$sev=sev
pair_data_06_07$subgroup=subgroup


pair_data_07_14$comp=comp
pair_data_07_14$r1=r1_07_14
pair_data_07_14$r2=r2_07_14
pair_data_07_14$N1=N1_07_14
pair_data_07_14$N2=N2_07_14
pair_data_07_14$sev=sev
pair_data_07_14$subgroup=subgroup


pair_data_08_14$comp=comp
pair_data_08_14$r1=r1_08_14
pair_data_08_14$r2=r2_08_14
pair_data_08_14$N1=N1_08_14
pair_data_08_14$N2=N2_08_14
pair_data_08_14$sev=sev
pair_data_08_14$subgroup=subgroup

#### Time to event

pair_data$comp=comp
pair_data$N1=N1
pair_data$N2=N2
pair_data$sev=sev
pair_data$subgroup=subgroup

###############################

#keep desired data#
#new_data_01_07=subset(pair_data_01_07, comp>0)
new_data_01_14=subset(pair_data_01_14, comp>0)
new_data_01_60=subset(pair_data_01_60, comp>0)
#new_data_02_07=subset(pair_data_02_07, comp>0)
#new_data_02_14=subset(pair_data_02_14, comp>0)
#new_data_03_07=subset(pair_data_03_07, comp>0)
#new_data_03_14=subset(pair_data_03_14, comp>0)
#new_data_04_07=subset(pair_data_04_07, comp>0)
new_data_04_14=subset(pair_data_04_14, comp>0)
#new_data_05_07=subset(pair_data_05_07, comp>0)
new_data_05_14=subset(pair_data_05_14, comp>0)
new_data_05_60=subset(pair_data_05_60, comp>0)
new_data_05_90=subset(pair_data_05_90, comp>0)
#new_data_06_03=subset(pair_data_06_03, comp>0)
new_data_06_07=subset(pair_data_06_07, comp>0)
new_data_07_14=subset(pair_data_07_14, comp>0)
new_data_08_14=subset(pair_data_08_14, comp>0)

##### Time to event ####
new_data=subset(pair_data, comp>0)


comp.num=comp.num
#######################################################################################################
#######################################################################################################
#######################################################################################################

############################ Adverse events ###########################################################

analysis_data_07_14=cbind.data.frame(Trial_ID=new_data_07_14$Trial_ID,First_author=new_data_07_14$First_author,
                                     F_U_days=new_data_07_14$F_U_days,Intervention1=new_data_07_14$Intervention1,
                                     Intervention2=new_data_07_14$Intervention2,r1=new_data_07_14$r1,N1=new_data_07_14$N1,
                                     r2=new_data_07_14$r2,N2=new_data_07_14$N2,Comparison=new_data_07_14$Comparison,
                                     severity=new_data_07_14$Research_question,comp=new_data_07_14$comp,
                                     sev=new_data_07_14$sev,subgroup=new_data_07_14$subgroup,Year=new_data_07_14$Year
                                     ,dose=as.character(new_data_07_14$Treat_Dose_desc1))
                                     # A=as.character(new_data_07_14$ROB_1_randomization),
                                     # B=as.character(new_data_07_14$ROB2_AE),
                                     # C=as.character(new_data_07_14$ROB_3_Adverse_events),
                                     # D=as.character(new_data_07_14$ROB_4_Adverse_event),
                                     # E=as.character(new_data_07_14$ROB_5_Adverse_events),
                                     # Overall=as.character(new_data_07_14$ROB_6_Adverse_events))


analysis_data_07_14=as.data.frame(analysis_data_07_14)


analysis_data_07_14$f1=analysis_data_07_14$N1-analysis_data_07_14$r1
analysis_data_07_14$f2=analysis_data_07_14$N2-analysis_data_07_14$r2


analysis_data_07_14$counts1=str_c(analysis_data_07_14$r1, analysis_data_07_14$N1, sep = "/", collapse = NULL)
analysis_data_07_14$counts2=str_c(analysis_data_07_14$r2, analysis_data_07_14$N2, sep = "/", collapse = NULL)

analysis_data_07_14= analysis_data_07_14 %>%
  filter(analysis_data_07_14$comp==comp.num)

analysis_data_07_14=analysis_data_07_14[complete.cases(analysis_data_07_14$counts1), ]

analysis_data_07_14=as.data.frame(analysis_data_07_14)

rows_AE=nrow(analysis_data_07_14)


#######################################################################################################
#######################################################################################################
#######################################################################################################

##### Serious AE ######################################################################################

analysis_data_08_14=cbind.data.frame(Trial_ID=new_data_08_14$Trial_ID,First_author=new_data_08_14$First_author,
                                     F_U_days=new_data_08_14$F_U_days,Intervention1=new_data_08_14$Intervention1,
                                     Intervention2=new_data_08_14$Intervention2,r1=new_data_08_14$r1,N1=new_data_08_14$N1,
                                     r2=new_data_08_14$r2,N2=new_data_08_14$N2,Comparison=new_data_08_14$Comparison,
                                     severity=new_data_08_14$Research_question,comp=new_data_08_14$comp,
                                     sev=new_data_08_14$sev,subgroup=new_data_08_14$subgroup,Year=new_data_08_14$Year
                                     ,dose=as.character(new_data_08_14$Treat_Dose_desc1))
                                     # A=as.character(new_data_08_14$ROB_1_randomization),
                                     # B=as.character(new_data_08_14$ROB2_SAE),
                                     # C=as.character(new_data_08_14$ROB_3_Serious_adverse_events),
                                     # D=as.character(new_data_08_14$ROB_4_Serious_adverse_events),
                                     # E=as.character(new_data_08_14$ROB_5_Serious_adverse_events),
                                     # Overall=as.character(new_data_08_14$ROB_6_Serious_adverse_events))

analysis_data_08_14=as.data.frame(analysis_data_08_14)


analysis_data_08_14$f1=analysis_data_08_14$N1-analysis_data_08_14$r1
analysis_data_08_14$f2=analysis_data_08_14$N2-analysis_data_08_14$r2


analysis_data_08_14$counts1=str_c(analysis_data_08_14$r1, analysis_data_08_14$N1, sep = "/", collapse = NULL)
analysis_data_08_14$counts2=str_c(analysis_data_08_14$r2, analysis_data_08_14$N2, sep = "/", collapse = NULL)

analysis_data_08_14= analysis_data_08_14 %>%
  filter(analysis_data_08_14$comp==comp.num)

analysis_data_08_14=analysis_data_08_14[complete.cases(analysis_data_08_14$counts1), ]


analysis_data_08_14=as.data.frame(analysis_data_08_14)



rows_SAE=nrow(analysis_data_08_14)



#######################################################################################################
#######################################################################################################
#######################################################################################################

###### Clinical improvement D7 ########################################################################

# analysis_data_01_07=cbind.data.frame(Trial_ID=new_data_01_07$Trial_ID,First_author=new_data_01_07$First_author,
#                                      F_U_days=new_data_01_07$F_U_days,Intervention1=new_data_01_07$Intervention1,
#                                      Intervention2=new_data_01_07$Intervention2,r1=as.numeric(new_data_01_07$r1),N1=new_data_01_07$N1,
#                                      r2=new_data_01_07$r2,N2=new_data_01_07$N2,Comparison=new_data_01_07$Comparison,
#                                      severity=new_data_01_07$Research_question,comp=new_data_01_07$comp,
#                                      sev=new_data_01_07$sev,subgroup=new_data_01_07$subgroup,Year=new_data_01_07$Year
#                                      ,dose=as.character(new_data_01_07$Treat_Dose_desc1),
#                                      A=as.character(new_data_01_07$ROB_1_randomization),
#                                      B=as.character(new_data_01_07$ROB_2_Deviations_from_intervention),
#                                      C=as.character(new_data_01_07$ROB_3__Clinical_improvement_n),
#                                      D=as.character(new_data_01_07$ROB_4_Clinical_improvement),
#                                      E=as.character(new_data_01_07$ROB_5_Clinical_improvement),
#                                      Overall=as.character(new_data_01_07$ROB_6_Clinical_improvement))
# 
# 
# 
# analysis_data_01_07=as.data.frame(analysis_data_01_07)
# 
# analysis_data_01_07$f1=analysis_data_01_07$N1-analysis_data_01_07$r1
# analysis_data_01_07$f2=analysis_data_01_07$N2-analysis_data_01_07$r2
# 
# analysis_data_01_07$counts1=str_c(analysis_data_01_07$r1, analysis_data_01_07$N1, sep = "/", collapse = NULL)
# analysis_data_01_07$counts2=str_c(analysis_data_01_07$r2, analysis_data_01_07$N2, sep = "/", collapse = NULL)
# 
# analysis_data_01_07= analysis_data_01_07 %>%
#   filter(analysis_data_01_07$comp==comp.num)
# 
# analysis_data_01_07=analysis_data_01_07[complete.cases(analysis_data_01_07$counts1), ]
# 
# analysis_data_01_07=as.data.frame(analysis_data_01_07)
# 
# 
# rows_Clinical_improvement_D7=nrow(analysis_data_01_07)



#######################################################################################################
#######################################################################################################
#######################################################################################################

###### Clinical improvement D14-D28

analysis_data_01_14=cbind.data.frame(Trial_ID=new_data_01_14$Trial_ID,First_author=new_data_01_14$First_author,
                                     F_U_days=new_data_01_14$F_U_days,Intervention1=new_data_01_14$Intervention1,
                                     Intervention2=new_data_01_14$Intervention2,r1=new_data_01_14$r1,N1=new_data_01_14$N1,
                                     r2=new_data_01_14$r2,N2=new_data_01_14$N2,Comparison=new_data_01_14$Comparison,
                                     severity=new_data_01_14$Research_question,comp=new_data_01_14$comp,
                                     sev=new_data_01_14$sev,subgroup=new_data_01_14$subgroup,Year=new_data_01_14$Year
                                     ,dose=as.character(new_data_01_14$Treat_Dose_desc1))
                                     # A=as.character(new_data_01_14$ROB_1_randomization),
                                     # B=as.character(new_data_01_14$ROB2_clinic_imp_D28),
                                     # C=as.character(new_data_01_14$ROB_3__Clinical_improvement_D28),
                                     # D=as.character(new_data_01_14$ROB_4_Clinical_improvement),
                                     # E=as.character(new_data_01_14$ROB_5_Clinical_improvement_D28),
                                     # Overall=as.character(new_data_01_14$ROB_6_Clinical_improvement_D28)
                                     
                                     
# )



analysis_data_01_14=as.data.frame(analysis_data_01_14)
analysis_data_01_14$f1=analysis_data_01_14$N1-analysis_data_01_14$r1
analysis_data_01_14$f2=analysis_data_01_14$N2-analysis_data_01_14$r2
analysis_data_01_14$counts1=str_c(analysis_data_01_14$r1, analysis_data_01_14$N1, sep = "/", collapse = NULL)
analysis_data_01_14$counts2=str_c(analysis_data_01_14$r2, analysis_data_01_14$N2, sep = "/", collapse = NULL)


analysis_data_01_14= analysis_data_01_14 %>%
  filter(analysis_data_01_14$comp==comp.num)

analysis_data_01_14=analysis_data_01_14[complete.cases(analysis_data_01_14$counts1), ]

analysis_data_01_14=as.data.frame(analysis_data_01_14)

rows_Clinical_improvement_D14_28=nrow(analysis_data_01_14)


#######################################################################################################
#######################################################################################################
#######################################################################################################

analysis_data_01_60=cbind.data.frame(Trial_ID=new_data_01_60$Trial_ID,First_author=new_data_01_60$First_author,
                                     F_U_days=new_data_01_60$F_U_days,Intervention1=new_data_01_60$Intervention1,
                                     Intervention2=new_data_01_60$Intervention2,r1=new_data_01_60$r1,N1=new_data_01_60$N1,
                                     r2=new_data_01_60$r2,N2=new_data_01_60$N2,Comparison=new_data_01_60$Comparison,
                                     severity=new_data_01_60$Research_question,comp=new_data_01_60$comp,
                                     sev=new_data_01_60$sev,subgroup=new_data_01_60$subgroup,Year=new_data_01_60$Year
                                     ,dose=as.character(new_data_01_60$Treat_Dose_desc1))
                                     # A=as.character(new_data_01_60$ROB_1_randomization),
                                     # B=as.character(new_data_01_60$ROB2_clinic_imp_D60),
                                     # C=as.character(new_data_01_60$ROB_3__Clinical_improvement_D60),
                                     # D=as.character(new_data_01_60$ROB_4_Clinical_improvement),
                                     # E=as.character(new_data_01_60$ROB_5_Clinical_improvement_D60),
                                     # Overall=as.character(new_data_01_60$ROB_6_Clinical_improvement_D60))

analysis_data_01_60=as.data.frame(analysis_data_01_60)


analysis_data_01_60$f1=analysis_data_01_60$N1-analysis_data_01_60$r1
analysis_data_01_60$f2=analysis_data_01_60$N2-analysis_data_01_60$r2
analysis_data_01_60$counts1=str_c(analysis_data_01_60$r1, analysis_data_01_60$N1, sep = "/", collapse = NULL)
analysis_data_01_60$counts2=str_c(analysis_data_01_60$r2, analysis_data_01_60$N2, sep = "/", collapse = NULL)

analysis_data_01_60= analysis_data_01_60 %>%
  filter(analysis_data_01_60$comp==comp.num)

analysis_data_01_60=analysis_data_01_60[complete.cases(analysis_data_01_60$counts1), ]

analysis_data_01_60=as.data.frame(analysis_data_01_60)


rows_Clinical_improvement_D60=nrow(analysis_data_01_60)


#######################################################################################################
#######################################################################################################
#######################################################################################################

################### Mortality D7 ####################

# analysis_data_05_07=cbind.data.frame(Trial_ID=new_data_05_07$Trial_ID,First_author=new_data_05_07$First_author,
#                                      F_U_days=new_data_05_07$F_U_days,Intervention1=new_data_05_07$Intervention1,
#                                      Intervention2=new_data_05_07$Intervention2,r1=new_data_05_07$r1,N1=new_data_05_07$N1,
#                                      r2=new_data_05_07$r2,N2=new_data_05_07$N2,Comparison=new_data_05_07$Comparison,
#                                      severity=new_data_05_07$Research_question,comp=new_data_05_07$comp,
#                                      sev=new_data_05_07$sev,subgroup=new_data_05_07$subgroup,Year=new_data_05_07$Year
#                                      ,dose=as.character(new_data_05_07$Treat_Dose_desc1),
#                                      A=as.character(new_data_05_07$ROB_1_randomization),
#                                      B=as.character(new_data_05_07$ROB_2_Deviations_from_intervention),
#                                      C=as.character(new_data_05_07$ROB_3_mortality_),
#                                      D=as.character(new_data_05_07$ROB_4_mortality_OR_time_to_death),
#                                      E=as.character(new_data_05_07$ROB_5_mortality_),
#                                      Overall=as.character(new_data_05_07$ROB_6_mortality)
#                                      
# )
# 
# analysis_data_05_07=as.data.frame(analysis_data_05_07)
# 
# analysis_data_05_07$f1=analysis_data_05_07$N1-analysis_data_05_07$r1
# analysis_data_05_07$f2=analysis_data_05_07$N2-analysis_data_05_07$r2
# 
# 
# analysis_data_05_07$counts1=str_c(analysis_data_05_07$r1, analysis_data_05_07$N1, sep = "/", collapse = NULL)
# analysis_data_05_07$counts2=str_c(analysis_data_05_07$r2, analysis_data_05_07$N2, sep = "/", collapse = NULL)
# 
# analysis_data_05_07= analysis_data_05_07 %>%
#   filter(analysis_data_05_07$comp==comp.num)
# 
# analysis_data_05_07=analysis_data_05_07[complete.cases(analysis_data_05_07$counts1), ]
# 
# analysis_data_05_07=as.data.frame(analysis_data_05_07)
# 
# 
# rows_Mortality_D7=nrow(analysis_data_05_07)




#######################################################################################################
#######################################################################################################
#######################################################################################################

################### Mortality D14_D28 #####################


analysis_data_05_14=cbind.data.frame(Trial_ID=new_data_05_14$Trial_ID,First_author=new_data_05_14$First_author,
                                     F_U_days=new_data_05_14$F_U_days,Intervention1=new_data_05_14$Intervention1,
                                     Intervention2=new_data_05_14$Intervention2,r1=new_data_05_14$r1,N1=new_data_05_14$N1,
                                     r2=new_data_05_14$r2,N2=new_data_05_14$N2,Comparison=new_data_05_14$Comparison,
                                     severity=new_data_05_14$Research_question,comp=new_data_05_14$comp,
                                     sev=new_data_05_14$sev,subgroup=new_data_05_14$subgroup,Year=new_data_05_14$Year
                                     ,dose=as.character(new_data_05_14$Treat_Dose_desc1))
                                     # A=as.character(new_data_05_14$ROB_1_randomization)
                                     # B=as.character(new_data_05_14$ROB2_Mort_D28),
                                     # C=as.character(new_data_05_14$ROB_3_mortality_D28),
                                     # D=as.character(new_data_05_14$ROB_4_mortality_OR_time_to_death),
                                     # E=as.character(new_data_05_14$ROB_5_mortality_D28),
#                                      # Overall=as.character(new_data_05_14$ROB_6_mortality_D28)
# )


analysis_data_05_14=as.data.frame(analysis_data_05_14)


analysis_data_05_14$f1=analysis_data_05_14$N1-analysis_data_05_14$r1
analysis_data_05_14$f2=analysis_data_05_14$N2-analysis_data_05_14$r2


analysis_data_05_14$counts1=str_c(analysis_data_05_14$r1, analysis_data_05_14$N1, sep = "/", collapse = NULL)
analysis_data_05_14$counts2=str_c(analysis_data_05_14$r2, analysis_data_05_14$N2, sep = "/", collapse = NULL)



analysis_data_05_14= analysis_data_05_14 %>%
  filter(analysis_data_05_14$comp==comp.num)

analysis_data_05_14=analysis_data_05_14[complete.cases(analysis_data_05_14$counts1), ]

analysis_data_05_14=as.data.frame(analysis_data_05_14)


rows_Mortality_D14_D28=nrow(analysis_data_05_14)



#######################################################################################################
#######################################################################################################
#######################################################################################################

############### Mortality D60 #############################3

analysis_data_05_60=cbind.data.frame(Trial_ID=new_data_05_60$Trial_ID,First_author=new_data_05_60$First_author,
                                     F_U_days=new_data_05_60$F_U_days,Intervention1=new_data_05_60$Intervention1,
                                     Intervention2=new_data_05_60$Intervention2,r1=new_data_05_60$r1,N1=new_data_05_60$N1,
                                     r2=new_data_05_60$r2,N2=new_data_05_60$N2,Comparison=new_data_05_60$Comparison,
                                     severity=new_data_05_60$Research_question,comp=new_data_05_60$comp,
                                     sev=new_data_05_60$sev,subgroup=new_data_05_60$subgroup,Year=new_data_05_60$Year
                                     ,dose=as.character(new_data_05_60$Treat_Dose_desc1))
                                     # A=as.character(new_data_05_60$ROB_1_randomization)
                                     # B=as.character(new_data_05_60$ROB2_Mort_D60),
                                     # C=as.character(new_data_05_60$ROB_3_mortality_60),
                                     # D=as.character(new_data_05_60$ROB_4_mortality_OR_time_to_death),
                                     # E=as.character(new_data_05_60$ROB_5_mortality_D60),
                                     # Overall=as.character(new_data_05_60$ROB_6_mortality_D60))

analysis_data_05_60=as.data.frame(analysis_data_05_60)


analysis_data_05_60$f1=analysis_data_05_60$N1-analysis_data_05_60$r1
analysis_data_05_60$f2=analysis_data_05_60$N2-analysis_data_05_60$r2


analysis_data_05_60$counts1=str_c(analysis_data_05_60$r1, analysis_data_05_60$N1, sep = "/", collapse = NULL)
analysis_data_05_60$counts2=str_c(analysis_data_05_60$r2, analysis_data_05_60$N2, sep = "/", collapse = NULL)


analysis_data_05_60= analysis_data_05_60 %>%
  filter(analysis_data_05_60$comp==comp.num)

analysis_data_05_60=analysis_data_05_60[complete.cases(analysis_data_05_60$counts1), ]

analysis_data_05_60=as.data.frame(analysis_data_05_60)


rows_Mortality_D60=nrow(analysis_data_05_60)

#######################################################################################################
#######################################################################################################
#######################################################################################################

################## Mortality D90 ##################################

# analysis_data_05_90=cbind.data.frame(Trial_ID=new_data_05_90$Trial_ID,First_author=new_data_05_90$First_author,
#                                      F_U_days=new_data_05_90$F_U_days,Intervention1=new_data_05_90$Intervention1,
#                                      Intervention2=new_data_05_90$Intervention2,r1=new_data_05_90$r1,N1=new_data_05_90$N1,
#                                      r2=new_data_05_90$r2,N2=new_data_05_90$N2,Comparison=new_data_05_90$Comparison,
#                                      severity=new_data_05_90$Research_question,comp=new_data_05_90$comp,
#                                      sev=new_data_05_90$sev,subgroup=new_data_05_90$subgroup,Year=new_data_05_90$Year
#                                      ,dose=as.character(new_data_05_90$Treat_Dose_desc1),
#                                      A=as.character(new_data_05_90$ROB_1_randomization),
#                                      B=as.character(new_data_05_90$ROB_2_Deviations_from_intervention),
#                                      C=as.character(new_data_05_90$ROB_3_mortality_),
#                                      D=as.character(new_data_05_90$ROB_4_mortality_OR_time_to_death),
#                                      E=as.character(new_data_05_90$ROB_5_mortality_),
#                                      Overall=as.character(new_data_05_90$ROB_6_mortality))
# 
# analysis_data_05_90=as.data.frame(analysis_data_05_90)
# 
# analysis_data_05_90$f1=analysis_data_05_90$N1-analysis_data_05_90$r1
# analysis_data_05_90$f2=analysis_data_05_90$N2-analysis_data_05_90$r2
# 
# 
# analysis_data_05_90$counts1=str_c(analysis_data_05_90$r1, analysis_data_05_90$N1, sep = "/", collapse = NULL)
# analysis_data_05_90$counts2=str_c(analysis_data_05_90$r2, analysis_data_05_90$N2, sep = "/", collapse = NULL)
# 
# 
# analysis_data_05_90= analysis_data_05_90 %>%
#   filter(analysis_data_05_90$comp==comp.num)
# 
# analysis_data_05_90=analysis_data_05_90[complete.cases(analysis_data_05_90$counts1), ]
# 
# analysis_data_05_90=as.data.frame(analysis_data_05_90)
# 
# 
# rows_Mortality_D90=nrow(analysis_data_05_90)

#######################################################################################################
#######################################################################################################
#######################################################################################################

################# Score6 D7 #############################

# analysis_data_03_07=cbind.data.frame(Trial_ID=new_data_03_07$Trial_ID,First_author=new_data_03_07$First_author,
#                                      F_U_days=new_data_03_07$F_U_days,Intervention1=new_data_03_07$Intervention1,
#                                      Intervention2=new_data_03_07$Intervention2,r1=new_data_03_07$r1,N1=new_data_03_07$N1,
#                                      r2=new_data_03_07$r2,N2=new_data_03_07$N2,Comparison=new_data_03_07$Comparison,
#                                      severity=new_data_03_07$Research_question,comp=new_data_03_07$comp,
#                                      sev=new_data_03_07$sev,subgroup=new_data_03_07$subgroup,Year=new_data_03_07$Year
#                                      ,dose=as.character(new_data_03_07$Treat_Dose_desc1),
#                                      A=as.character(new_data_03_07$ROB_1_randomization),
#                                      B=as.character(new_data_03_07$ROB_2_Deviations_from_intervention),
#                                      C=as.character(new_data_03_07$ROB_3_SCORE_6_and_above_WHO),
#                                      D=as.character(new_data_03_07$ROB_4_SCORE_6_and_above_WHO),
#                                      E=as.character(new_data_03_07$ROB_5_SCORE_6_and_above_WHO),
#                                      Overall=as.character(new_data_03_07$ROB_6_SCORE_6_and_above_WHO))
# 
# analysis_data_03_07=as.data.frame(analysis_data_03_07)
# 
# 
# analysis_data_03_07$f1=analysis_data_03_07$N1-analysis_data_03_07$r1
# analysis_data_03_07$f2=analysis_data_03_07$N2-analysis_data_03_07$r2
# 
# 
# analysis_data_03_07$counts1=str_c(analysis_data_03_07$r1, analysis_data_03_07$N1, sep = "/", collapse = NULL)
# analysis_data_03_07$counts2=str_c(analysis_data_03_07$r2, analysis_data_03_07$N2, sep = "/", collapse = NULL)
# 
# 
# analysis_data_03_07= analysis_data_03_07 %>%
#   filter(analysis_data_03_07$comp==comp.num)
# 
# 
# 
# analysis_data_03_07=analysis_data_03_07[complete.cases(analysis_data_03_07$counts1), ]
# 
# analysis_data_03_07=as.data.frame(analysis_data_03_07)
# 
# 
# rows_Score6_D7=nrow(analysis_data_03_07)



#######################################################################################################
#######################################################################################################
#######################################################################################################

# analysis_data_03_14=cbind.data.frame(Trial_ID=new_data_03_14$Trial_ID,First_author=new_data_03_14$First_author,
#                                      F_U_days=new_data_03_14$F_U_days,Intervention1=new_data_03_14$Intervention1,
#                                      Intervention2=new_data_03_14$Intervention2,r1=new_data_03_14$r1,N1=new_data_03_14$N1,
#                                      r2=new_data_03_14$r2,N2=new_data_03_14$N2,Comparison=new_data_03_14$Comparison,
#                                      severity=new_data_03_14$Research_question,comp=new_data_03_14$comp,
#                                      sev=new_data_03_14$sev,subgroup=new_data_03_14$subgroup,Year=new_data_03_14$Year
#                                      ,dose=as.character(new_data_03_14$Treat_Dose_desc1),
#                                      A=as.character(new_data_03_14$ROB_1_randomization),
#                                      B=as.character(new_data_03_14$ROB_2_Deviations_from_intervention),
#                                      C=as.character(new_data_03_14$ROB_3_SCORE_6_and_above_WHO),
#                                      D=as.character(new_data_03_14$ROB_4_SCORE_6_and_above_WHO),
#                                      E=as.character(new_data_03_14$ROB_5_SCORE_6_and_above_WHO),
#                                      Overall=as.character(new_data_03_14$ROB_6_SCORE_6_and_above_WHO))
# analysis_data_03_14=as.data.frame(analysis_data_03_14)
# 
# 
# analysis_data_03_14$f1=analysis_data_03_14$N1-analysis_data_03_14$r1
# analysis_data_03_14$f2=analysis_data_03_14$N2-analysis_data_03_14$r2
# 
# 
# analysis_data_03_14$counts1=str_c(analysis_data_03_14$r1, analysis_data_03_14$N1, sep = "/", collapse = NULL)
# analysis_data_03_14$counts2=str_c(analysis_data_03_14$r2, analysis_data_03_14$N2, sep = "/", collapse = NULL)
# 
# analysis_data_03_14= analysis_data_03_14 %>%
#   filter(analysis_data_03_14$comp==comp.num)
# 
# analysis_data_03_14=analysis_data_03_14[complete.cases(analysis_data_03_14$counts1), ]
# 
# analysis_data_03_14=as.data.frame(analysis_data_03_14)
# 
# 
# rows_Score6_D14_D28=nrow(analysis_data_03_14)



#######################################################################################################
#######################################################################################################
#######################################################################################################

##### Score 7 D7 #####

# analysis_data_04_07=cbind.data.frame(Trial_ID=new_data_04_07$Trial_ID,First_author=new_data_04_07$First_author,
#                                      F_U_days=new_data_04_07$F_U_days,Intervention1=new_data_04_07$Intervention1,
#                                      Intervention2=new_data_04_07$Intervention2,r1=new_data_04_07$r1,N1=new_data_04_07$N1,
#                                      r2=new_data_04_07$r2,N2=new_data_04_07$N2,Comparison=new_data_04_07$Comparison,
#                                      severity=new_data_04_07$Research_question,comp=new_data_04_07$comp,
#                                      sev=new_data_04_07$sev,subgroup=new_data_04_07$subgroup,Year=new_data_04_07$Year
#                                      ,dose=as.character(new_data_04_07$Treat_Dose_desc1),
#                                      A=as.character(new_data_04_07$ROB_1_randomization),
#                                      B=as.character(new_data_04_07$ROB_2_Deviations_from_intervention),
#                                      C=as.character(new_data_04_07$ROB_3_SCORE_7_and_above_WHO),
#                                      D=as.character(new_data_04_07$ROB_4_SCORE_7_and_above_WHO),
#                                      E=as.character(new_data_04_07$ROB_5_SCORE_7_and_above_WHO),
#                                      Overall=as.character(new_data_04_07$ROB_6_SCORE_7_and_above_WHO))
# 
# analysis_data_04_07=as.data.frame(analysis_data_04_07)
# 
# 
# analysis_data_04_07$f1=analysis_data_04_07$N1-analysis_data_04_07$r1
# analysis_data_04_07$f2=analysis_data_04_07$N2-analysis_data_04_07$r2
# 
# 
# analysis_data_04_07$counts1=str_c(analysis_data_04_07$r1, analysis_data_04_07$N1, sep = "/", collapse = NULL)
# analysis_data_04_07$counts2=str_c(analysis_data_04_07$r2, analysis_data_04_07$N2, sep = "/", collapse = NULL)
# 
# 
# analysis_data_04_07= analysis_data_04_07 %>%
#   filter(analysis_data_04_07$comp==comp.num)
# 
# analysis_data_04_07=analysis_data_04_07[complete.cases(analysis_data_04_07$counts1), ]
# 
# 
# 
# analysis_data_04_07=as.data.frame(analysis_data_04_07)
# 
# 
# rows_Score7_D7=nrow(analysis_data_04_07)


#######################################################################################################
#######################################################################################################
#######################################################################################################

##### Score 7 D14-D28 #####################

analysis_data_04_14=cbind.data.frame(Trial_ID=new_data_04_14$Trial_ID,First_author=new_data_04_14$First_author,
                                     F_U_days=new_data_04_14$F_U_days,Intervention1=new_data_04_14$Intervention1,
                                     Intervention2=new_data_04_14$Intervention2,r1=new_data_04_14$r1,N1=new_data_04_14$N1,
                                     r2=new_data_04_14$r2,N2=new_data_04_14$N2,Comparison=new_data_04_14$Comparison,
                                     severity=new_data_04_14$Research_question,comp=new_data_04_14$comp,
                                     sev=new_data_04_14$sev,subgroup=new_data_04_14$subgroup,Year=new_data_04_14$Year
                                     ,dose=as.character(new_data_04_14$Treat_Dose_desc1))
                                     # A=as.character(new_data_04_14$ROB_1_randomization)
                                     # B=as.character(new_data_04_14$ROB2_score7_D28),
                                     # C=as.character(new_data_04_14$ROB_3_SCORE_7_and_above_WHO_D28),
                                     # D=as.character(new_data_04_14$ROB_4_SCORE_7_and_above_WHO),
                                     # E=as.character(new_data_04_14$ROB_5_SCORE_7_and_above_WHO_D28),
                                     # Overall=as.character(new_data_04_14$ROB_6_SCORE_7_and_above_WHO_D28)
# )
analysis_data_04_14=as.data.frame(analysis_data_04_14)

analysis_data_04_14$f1=analysis_data_04_14$N1-analysis_data_04_14$r1
analysis_data_04_14$f2=analysis_data_04_14$N2-analysis_data_04_14$r2



analysis_data_04_14$counts1=str_c(analysis_data_04_14$r1, analysis_data_04_14$N1, sep = "/", collapse = NULL)
analysis_data_04_14$counts2=str_c(analysis_data_04_14$r2, analysis_data_04_14$N2, sep = "/", collapse = NULL)


analysis_data_04_14= analysis_data_04_14 %>%
  filter(analysis_data_04_14$comp==comp.num)

analysis_data_04_14=analysis_data_04_14[complete.cases(analysis_data_04_14$counts1), ]

analysis_data_04_14=as.data.frame(analysis_data_04_14)

rows_Score7_D14_28=nrow(analysis_data_04_14)


#######################################################################################################
#######################################################################################################
#######################################################################################################

################## Negative conversion D3
# 
# analysis_data_06_03=cbind.data.frame(Trial_ID=new_data_06_03$Trial_ID,First_author=new_data_06_03$First_author,
#                                      F_U_days=new_data_06_03$F_U_days,Intervention1=new_data_06_03$Intervention1,
#                                      Intervention2=new_data_06_03$Intervention2,r1=new_data_06_03$r1,N1=new_data_06_03$N1,
#                                      r2=new_data_06_03$r2,N2=new_data_06_03$N2,Comparison=new_data_06_03$Comparison,
#                                      severity=new_data_06_03$Research_question,comp=new_data_06_03$comp,
#                                      sev=new_data_06_03$sev,subgroup=new_data_06_03$subgroup,Year=new_data_06_03$Year
#                                      ,dose=as.character(new_data_06_03$Treat_Dose_desc1),
#                                      A=as.character(new_data_06_03$ROB_1_randomization),
#                                      B=as.character(new_data_06_03$ROB_2_Deviations_from_intervention),
#                                      C=as.character(new_data_06_03$ROB_3__Viral_Neg_Conv_events),
#                                      D=as.character(new_data_06_03$ROB_4__Viral_Neg_Conv_events),
#                                      E=as.character(new_data_06_03$ROB_5__Viral_Neg_Conv_events),
#                                      Overall=as.character(new_data_06_03$ROB_6__Viral_Neg_Conv_events))
# 
# 
# analysis_data_06_03=as.data.frame(analysis_data_06_03)
# 
# 
# analysis_data_06_03$f1=analysis_data_06_03$N1-analysis_data_06_03$r1
# analysis_data_06_03$f2=analysis_data_06_03$N2-analysis_data_06_03$r2
# 
# 
# 
# analysis_data_06_03$counts1=str_c(analysis_data_06_03$r1, analysis_data_06_03$N1, sep = "/", collapse = NULL)
# analysis_data_06_03$counts2=str_c(analysis_data_06_03$r2, analysis_data_06_03$N2, sep = "/", collapse = NULL)
# 
# analysis_data_06_03= analysis_data_06_03 %>%
#   filter(analysis_data_06_03$comp==comp.num)
# 
# analysis_data_06_03=analysis_data_06_03[complete.cases(analysis_data_06_03$counts1), ]
# 
# 
# analysis_data_06_03=as.data.frame(analysis_data_06_03)
# 
# rows_Neg_conv_D3=nrow(analysis_data_06_03)


#######################################################################################################
#######################################################################################################
#######################################################################################################

################## Negative conversion D7 ###################

analysis_data_06_07=cbind.data.frame(Trial_ID=new_data_06_07$Trial_ID,First_author=new_data_06_07$First_author,
                                     F_U_days=new_data_06_07$F_U_days,Intervention1=new_data_06_07$Intervention1,
                                     Intervention2=new_data_06_07$Intervention2,r1=new_data_06_07$r1,N1=new_data_06_07$N1,
                                     r2=new_data_06_07$r2,N2=new_data_06_07$N2,Comparison=new_data_06_07$Comparison,
                                     severity=new_data_06_07$Research_question,comp=new_data_06_07$comp,
                                     sev=new_data_06_07$sev,subgroup=new_data_06_07$subgroup,Year=new_data_06_07$Year
                                     ,dose=as.character(new_data_06_07$Treat_Dose_desc1))
                                     # A=as.character(new_data_06_07$ROB_1_randomization)
                                     # B=as.character(new_data_06_07$ROB2_Neg_conv),
                                     # C=as.character(new_data_06_07$ROB_3__Viral_Neg_Conv_events),
                                     # D=as.character(new_data_06_07$ROB_4__Viral_Neg_Conv_events),
                                     # E=as.character(new_data_06_07$ROB_5__Viral_Neg_Conv_events),
                                     # Overall=as.character(new_data_06_07$ROB_6__Viral_Neg_Conv_events))
analysis_data_06_07=as.data.frame(analysis_data_06_07)


analysis_data_06_07$f1=analysis_data_06_07$N1-analysis_data_06_07$r1
analysis_data_06_07$f2=analysis_data_06_07$N2-analysis_data_06_07$r2


analysis_data_06_07$counts1=str_c(analysis_data_06_07$r1, analysis_data_06_07$N1, sep = "/", collapse = NULL)
analysis_data_06_07$counts2=str_c(analysis_data_06_07$r2, analysis_data_06_07$N2, sep = "/", collapse = NULL)

analysis_data_06_07= analysis_data_06_07 %>%
  filter(analysis_data_06_07$comp==comp.num)

analysis_data_06_07=analysis_data_06_07[complete.cases(analysis_data_06_07$counts1), ]

analysis_data_06_07=as.data.frame(analysis_data_06_07)

rows_Neg_conv_D7=nrow(analysis_data_06_07)


#######################################################################################################
#######################################################################################################
#######################################################################################################

##### Time to clinical improvement ###############


analysis_data_09=cbind.data.frame(Trial_ID=new_data$Trial_ID,First_author=new_data$First_author,
                                  F_U_days=new_data$F_U_days,Intervention1=new_data$Intervention1,
                                  Intervention2=new_data$Intervention2,N1=new_data$N1,
                                  N2=new_data$N2,Comparison=new_data$Comparison,
                                  severity=new_data$Research_question,comp=new_data$comp,
                                  HR=new_data$Time_to_clinical_improvement_HR,LCI=new_data$Time_to_clinical_improvement_HR_CI_Lower,
                                  UCI=new_data$Time_to_clinical_improvement_HR_CI_Upper,
                                  sev=new_data$sev,subgroup=new_data$subgroup,
                                  Year=new_data$Year,dose=as.character(new_data$Treat_Dose_desc1))
                                  # A=as.character(new_data$ROB_1_randomization),
                                  # B=as.character(new_data$ROB2_Time_clinic_imp),
                                  # C=as.character(new_data$ROB_3__Time_to_clinical_improvement),
                                  # D=as.character(new_data$ROB_4_Clinical_improvement),
                                  # E=as.character(new_data$ROB_5_Time_to_clinical_improvement),
                                  # Overall=as.character(new_data$ROB_6__Time_to_clinical_improvement))

analysis_data_09=as.data.frame(analysis_data_09)

analysis_data_09$logHR=log(analysis_data_09$HR)
analysis_data_09$se=(log(analysis_data_09$UCI)-log(analysis_data_09$LCI))/3.92

analysis_data_09= analysis_data_09 %>%
  filter(analysis_data_09$comp==comp.num)

analysis_data_09=analysis_data_09[complete.cases(analysis_data_09$UCI), ]


analysis_data_09=as.data.frame(analysis_data_09)


rows_Time_to_clinical_improvement=nrow(analysis_data_09)

#######################################################################################################
#######################################################################################################
#######################################################################################################

############### Time to Score 6 #####

# analysis_data_11a=cbind.data.frame(Trial_ID=new_data$Trial_ID,First_author=new_data$First_author,
#                                    F_U_days=new_data$F_U_days,Intervention1=new_data$Intervention1,
#                                    Intervention2=new_data$Intervention2,N1=new_data$N1,
#                                    N2=new_data$N2,Comparison=new_data$Comparison,
#                                    severity=new_data$Research_question,comp=new_data$comp,
#                                    HR=new_data$Time_to_score_6_and_above_HR,LCI=new_data$Time_to_score_6_and_above_LCI,
#                                    UCI=new_data$Time_to_score_6_and_above_UCI,
#                                    sev=new_data$sev,subgroup=new_data$subgroup,
#                                    Year=new_data$Year,dose=as.character(new_data$Treat_Dose_desc1),
#                                    A=as.character(new_data$ROB_1_randomization),
#                                    B=as.character(new_data$ROB_2_Deviations_from_intervention),
#                                    C=as.character(new_data$ROB_3_SCORE_6_and_above_WHO),
#                                    D=as.character(new_data$ROB_4_SCORE_6_and_above_WHO),
#                                    E=as.character(new_data$ROB_5_SCORE_6_and_above_WHO),
#                                    Overall=as.character(new_data$ROB_6_SCORE_6_and_above_WHO))
# 
# analysis_data_11a=as.data.frame(analysis_data_11a)
# 
# 
# analysis_data_11a$logHR=log(analysis_data_11a$HR)
# analysis_data_11a$se=(log(analysis_data_11a$UCI)-log(analysis_data_11a$LCI))/3.92
# 
# 
# analysis_data_11a= analysis_data_11a %>%
#   filter(analysis_data_11a$comp==comp.num)
# 
# analysis_data_11a=analysis_data_11a[complete.cases(analysis_data_11a$UCI), ]
# 
# analysis_data_11a=as.data.frame(analysis_data_11a)
# 
# 
# rows_Time_to_Score6=nrow(analysis_data_11a)

#######################################################################################################
#######################################################################################################
#######################################################################################################

analysis_data_11b=cbind.data.frame(Trial_ID=new_data$Trial_ID,First_author=new_data$First_author,
                                   F_U_days=new_data$F_U_days,Intervention1=new_data$Intervention1,
                                   Intervention2=new_data$Intervention2,N1=new_data$N1,
                                   N2=new_data$N2,Comparison=new_data$Comparison,
                                   severity=new_data$Research_question,comp=new_data$comp,
                                   HR=new_data$Time_to_score_7_and_above_HR,LCI=new_data$Time_to_score_7_and_above_LCI,
                                   UCI=new_data$Time_to_score_7_and_above_UCI,
                                   sev=new_data$sev,subgroup=new_data$subgroup,
                                   Year=new_data$Year,dose=as.character(new_data$Treat_Dose_desc1))

                                   # A=as.character(new_data$ROB_1_randomization),
                                   # B=as.character(new_data$ROB2_Time_score7),
                                   # C=as.character(new_data$ROB_3_Time_to_SCORE_7_and_above_WHO),
                                   # D=as.character(new_data$ROB_4_SCORE_7_and_above_WHO),
                                   # E=as.character(new_data$ROB_5_Time_to_SCORE_7_and_above_WHO),
                                   # Overall=as.character(new_data$ROB_6_Time_to_SCORE_7_and_above_WHO))


analysis_data_11b=as.data.frame(analysis_data_11b)

analysis_data_11b$logHR=log(analysis_data_11b$HR)
analysis_data_11b$se=(log(analysis_data_11b$UCI)-log(analysis_data_11b$LCI))/3.92



analysis_data_11b= analysis_data_11b %>%
  filter(analysis_data_11b$comp==comp.num)

analysis_data_11b=analysis_data_11b[complete.cases(analysis_data_11b$UCI), ]

analysis_data_11b=as.data.frame(analysis_data_11b)


rows_Time_to_Score7=nrow(analysis_data_11b)


#######################################################################################################
#######################################################################################################
#######################################################################################################

################## Time to death #######
analysis_data_12=cbind.data.frame(Trial_ID=new_data$Trial_ID,First_author=new_data$First_author,
                                  F_U_days=new_data$F_U_days,Intervention1=new_data$Intervention1,
                                  Intervention2=new_data$Intervention2,N1=new_data$N1,
                                  N2=new_data$N2,Comparison=new_data$Comparison,
                                  severity=new_data$Research_question,comp=new_data$comp,
                                  HR=new_data$time_to_death_HR,LCI=new_data$time_to_death_lci,
                                  UCI=new_data$time_to_death_uci,
                                  sev=new_data$sev,subgroup=new_data$subgroup,
                                  Year=new_data$Year,dose=as.character(new_data$Treat_Dose_desc1))

                                  # A=as.character(new_data$ROB_1_randomization),
                                  # B=as.character(new_data$ROB2_Time_death),
                                  # C=as.character(new_data$ROB_3_time_to_death),
                                  # D=as.character(new_data$ROB_4_mortality_OR_time_to_death),
                                  # E=as.character(new_data$ROB_5_time_to_death),
                                  # Overall=as.character(new_data$ROB_6_time_to_death))

analysis_data_12=as.data.frame(analysis_data_12)


analysis_data_12$logHR=log(analysis_data_12$HR)

analysis_data_12$se=(log(analysis_data_12$UCI)-log(analysis_data_12$LCI))/3.92



analysis_data_12= analysis_data_12 %>%
  filter(analysis_data_12$comp==comp.num)

analysis_data_12=analysis_data_12[complete.cases(analysis_data_12$UCI), ]

analysis_data_12=as.data.frame(analysis_data_12)


rows_Time_to_death=nrow(analysis_data_12)


#######################################################################################################
#######################################################################################################
#######################################################################################################

############## Time to negative conversion ##########

analysis_data_13=cbind.data.frame(Trial_ID=new_data$Trial_ID,First_author=new_data$First_author,
                                  F_U_days=new_data$F_U_days,Intervention1=new_data$Intervention1,
                                  Intervention2=new_data$Intervention2,N1=new_data$N1,
                                  N2=new_data$N2,Comparison=new_data$Comparison,
                                  severity=new_data$Research_question,comp=new_data$comp,
                                  HR=new_data$Time_to_NEG_conv_HR,LCI=new_data$Time_to_NEG_conv_HR_LowerCI,
                                  UCI=new_data$Time_to_NEG_conv_HR_UpperCI,
                                  sev=new_data$sev,subgroup=new_data$subgroup,
                                  Year=new_data$Year,dose=as.character(new_data$Treat_Dose_desc1))

#                                   A=as.character(new_data$ROB_1_randomization),
#                                   B=as.character(new_data$ROB2_Time_neg_conv),
#                                   C=as.character(new_data$ROB_3_Time_to_NEG_conv),
#                                   D=as.character(new_data$ROB_4__Viral_Neg_Conv_events),
#                                   E=as.character(new_data$ROB_5_Time_to_NEG_conv),
#                                   Overall=as.character(new_data$ROB_6_Time_to_NEG_conv)
#                                   
# )

analysis_data_13=as.data.frame(analysis_data_13)

analysis_data_13$logHR=log(analysis_data_13$HR)
analysis_data_13$se=(log(analysis_data_13$UCI)-log(analysis_data_13$LCI))/3.92

analysis_data_13= analysis_data_13 %>%
  filter(analysis_data_13$comp==comp.num)

analysis_data_13=analysis_data_13[complete.cases(analysis_data_13$UCI), ]


analysis_data_13=as.data.frame(analysis_data_13)

rows_Time_to_NEG_conv=nrow(analysis_data_13)


#######################################################################################################
#######################################################################################################
#######################################################################################################


final_AE=ifelse(rows_AE>0,"Adverse events",NA)
final_SAE=ifelse(rows_SAE>0,"Serious adverse events",NA)

#final_Clinical_improvement_D7=ifelse(rows_Clinical_improvement_D7>0,"Clinical improvement D7",NA)
final_Clinical_improvement_D14_28=ifelse(rows_Clinical_improvement_D14_28>0,"Clinical improvement D28",NA)
final_Clinical_improvement_D60=ifelse(rows_Clinical_improvement_D60>0,"Clinical improvement D60",NA)


#final_Score6_D7=ifelse(rows_Score6_D7>0,"WHO Score 6 D7",NA)
#final_Score6_D14_28=ifelse(rows_Score6_D14_D28>0,"WHO Score 6 D14-D28",NA)

#final_Score7_D7=ifelse(rows_Score7_D7>0,"WHO Score 7 D7",NA)
final_Score7_D14_28=ifelse(rows_Score7_D14_28>0,"WHO Score 7 D28",NA)

#final_Mortality_D7=ifelse(rows_Mortality_D7>0,"Mortality D7",NA)
final_Mortality_D14_28=ifelse(rows_Mortality_D14_D28>0,"Mortality D28",NA)
final_Mortality_D60=ifelse(rows_Mortality_D60>0,"Mortality D60",NA)
#final_Mortality_D90=ifelse(rows_Mortality_D90>0,"Mortality D90",NA)

#final_Neg_conv_D3=ifelse(rows_Neg_conv_D3>0,"Negative conversion D3",NA)
final_Neg_conv_D7=ifelse(rows_Neg_conv_D7>0,"Negative conversion D7",NA)

##### Time to event ###############
final_Time_to_clinical_improvement=ifelse(rows_Time_to_clinical_improvement>0,"Time to clinincal improvement",NA)
#final_Time_to_Score6=ifelse(rows_Time_to_Score6>0,"Time to WHO score 6",NA)
final_Time_to_Score7=ifelse(rows_Time_to_Score7>0,"Time to WHO score 7",NA)
final_Time_to_death=ifelse(rows_Time_to_death>0,"Time to death",NA)
final_Time_to_NEG_conv=ifelse(rows_Time_to_NEG_conv>0,"Time to Negative Conversion",NA)



# final=c(final_Clinical_improvement_D7,final_Clinical_improvement_D14_28,final_Clinical_improvement_D60,
#         final_Score6_D7,final_Score6_D14_28,final_Score7_D7,final_Score7_D14_28,
#         final_Mortality_D7,final_Mortality_D14_28,final_Mortality_D60,final_Mortality_D90,
#         final_Neg_conv_D3,final_Neg_conv_D7,
#         final_AE,final_SAE,
#         final_Time_to_clinical_improvement,final_Time_to_Score6,final_Time_to_Score7,final_Time_to_death,final_Time_to_NEG_conv)


# final=c(final_Clinical_improvement_D7,final_Clinical_improvement_D14_28,final_Clinical_improvement_D60,
#         final_Score6_D7,final_Score6_D14_28,final_Score7_D7,final_Score7_D14_28,
#         final_Mortality_D7,final_Mortality_D14_28,final_Mortality_D60,final_Mortality_D90,
#         #final_Neg_conv_D3,
#         final_Neg_conv_D7,
#         final_AE,final_SAE,
#         final_Time_to_clinical_improvement,final_Time_to_Score6,final_Time_to_Score7,final_Time_to_death,final_Time_to_NEG_conv)

final=c(final_Clinical_improvement_D14_28,final_Clinical_improvement_D60,
       final_Score7_D14_28,
        final_Mortality_D14_28,final_Mortality_D60,
        #final_Neg_conv_D3,
        final_Neg_conv_D7,
        final_AE,final_SAE,
        final_Time_to_clinical_improvement,final_Time_to_Score7,final_Time_to_death,final_Time_to_NEG_conv)

final=final[complete.cases(final)]



return(final)



}




