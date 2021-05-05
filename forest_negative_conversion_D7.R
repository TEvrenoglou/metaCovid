#library(metafor)
#library(readxl)
#library(netmeta)
#library(meta)
#library(stringr)
#library(tidyverse)
#import and prepare data#
#data=read.csv("C:\\Users\\Theodoros Evrenoglou\\Desktop\\Covid-19 NMA\\Database\\October\\29-10-2020\\rct__updated_database_29_10_2020_new_merged.csv", na=c("*","NA"))

forest_NEG_D7=function(data,comp.num,sliderRob=0,sliderDose=0,high_RoB=FALSE,keep_only="All population",model="Random-effects",
                       sub="Severity",den="Randomized",hide_dose=FALSE,hide_severity=FALSE){
  
  
  data=data[,c("Trial_ID","ID_stats","name_study","First_author","Year","Study_design","Funding","Conflict_of_interest_category",
               "Research_question","Countries","F_U_days","Treatment_control","Treat_Name","stats_name","Treat_desc_summary",
               "n_Randomized","n_Analyzed_",
               "NEG_conv_n_D7","NEG_conv_n_D3","NEG_conv_denominator_d3","NEG_conv_denominator_d7",
               "Neg_conv_D7_TP",
               "ROB_1_randomization","ROB2_Neg_conv","ROB_2_Deviations_from_intervention","ROB_3__Viral_Neg_Conv_events",
               "ROB_4__Viral_Neg_Conv_events","ROB_5__Viral_Neg_Conv_events","ROB_6__Viral_Neg_Conv_events"
               
  )]
  
  
  #data$F_U_days=as.numeric(data$F_U_days)
  data$F_U_days=as.numeric(data$F_U_days)
  data$Neg_conv_D7_TP=as.numeric(data$Neg_conv_D7_TP)
  data$F_U_days=ifelse(is.na(data$Neg_conv_D7_TP),data$F_U_days,data$Neg_conv_D7_TP)
  
  data$n_Randomized=as.numeric(data$n_Randomized)
  # data$n_without_consent=as.numeric(data$n_without_consent)
  # data$n_without_consent=ifelse(is.na(data$n_without_consent),0,data$n_without_consent)
  #data$n_Randomized=data$n_Randomized-data$n_without_consent
  data$n_Randomized=ifelse(is.na(data$n_Randomized),data$n_Analyzed_,data$n_Randomized)
  
  # data$n_in_the_as_treated_analysis=ifelse(is.na(data$n_in_the_as_treated_analysis),data$n_Randomized,data$n_in_the_as_treated_analysis)
  data$NEG_conv_denominator_d7=ifelse(is.na(data$NEG_conv_denominator_d7),data$NEG_conv_denominator_d3,data$NEG_conv_denominator_d7)
  data$NEG_conv_denominator_d7=ifelse(is.na(data$NEG_conv_denominator_d7),data$n_Randomized,data$NEG_conv_denominator_d7)
  
  data$n_Analyzed_=ifelse(is.na(data$n_Analyzed_),data$n_Randomized,data$n_Analyzed_)
  
  if(den=="Randomized"){
    
    data$NEG_conv_denominator_d7=data$NEG_conv_denominator_d7
  }
  if(den!="Randomized"){
    data$NEG_conv_denominator_d7=data$n_Analyzed_
  }
  
  #### Conflicts ###
  data$Conflict_of_interest_category=trimws(data$Conflict_of_interest_category)
  
  ############### Funding #######
  data$Funding=trimws(data$Funding)
  data$Funding=ifelse(data$Funding==" ","Not reported/unclear",data$Funding)
  data$Funding=ifelse(data$Funding=="","Not reported/unclear",data$Funding)
  data$Funding=ifelse(data$Funding=="5.Not reported/unclear","Not reported/unclear",data$Funding)
  data$Funding=ifelse(data$Funding=="Private","Mixed/Private",data$Funding)
  data$Funding=ifelse(data$Funding=="Mixed","Mixed/Private",data$Funding)
  
  ########################
  
  
  ############### Countries #######
  
  data$Countries=trimws(data$Countries)
  data$Countries=ifelse(1*(str_detect(data$Countries,","))==1,"Multinational",data$Countries)
  data$Countries=ifelse(data$Countries=="Multinational (30)","Multinational",data$Countries)
  data$Countries=ifelse(data$Countries!="Multinational","National",data$Countries)
  
  ########################
  
  data$Treat_Name=trimws(data$Treat_Name)
  data$stats_name=trimws(data$stats_name)
  data$Treat_Name=data$stats_name
  data$Trial_ID=trimws(data$Trial_ID)
  data$First_author=trimws(data$First_author)
  data$name_study=trimws(data$name_study)
  #data$First_author=ifelse(is.na(data$name_study),as.character(data$First_author),paste(as.character(data$First_author),as.character(data$name_study),sep = ","))
  #data$First_author=ifelse(is.na(data$name_study),as.character(data$First_author),as.character(data$name_study))
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
  # data$All_cause_mortality_events_D1428=ifelse(!is.na(data$All_cause_mortality_events_D28),data$All_cause_mortality_events_D28,data$All_cause_mortality_events_D14)
  # #data$Recovery_D1428_n=ifelse(!is.na(data$Recovery_D28_n),data$Recovery_D28_n,data$Recovery_D14_n)
  # data$Clinical_improvement_D1428_n=ifelse(!is.na(data$Clinical_improvement_D28_n),data$Clinical_improvement_D28_n,data$Clinical_improvement_D14_n)
  # #data$SCORE_6_and_above_D1428=ifelse(!is.na(data$SCORE_6_and_above_D28),data$SCORE_6_and_above_D28,data$SCORE_6_and_above_D14)
  # data$SCORE_7_and_above_D1428=ifelse(!is.na(data$SCORE_7_and_above_D28),data$SCORE_7_and_above_D28,data$SCORE_7_and_above_D14)
  # data$Serious_AE_D7=ifelse(data$F_U_days<=10,data$Serious_AE,NA)
  # data$Serious_AE_D1428=ifelse(data$F_U_days>10 & data$F_U_days<=30,data$Serious_AE,NA)
  # data$Total_AE_D7=ifelse(data$F_U_days<=10,data$Total_AE,NA)
  # data$Total_AE_D1428=ifelse(data$F_U_days>10 & data$F_U_days<=30,data$Total_AE,NA)
  
  #rct_data=subset(data,data$Study_design=="RCT" & Trial_ID!=23 & Trial_ID!=9)
  rct_data=subset(data,data$Study_design=="RCT" & Trial_ID!=9)
  #rct_data=subset(data,data$Study_design=="RCT" & Trial_ID!=9 & Trial_ID!=139)
  
  
  rct_data$NEG_conv_n_D7=as.numeric(rct_data$NEG_conv_n_D7)
  pair_data_06_07=pairwise(studlab=Trial_ID,treat=Treat_Name,event=NEG_conv_n_D7,n=NEG_conv_denominator_d7,
                           data=rct_data,measure="RR",ref="Standard care")
  pair_data_06_07$F_U_days1=pair_data_06_07$F_U_days
  pair_data_06_07$First_author=pair_data_06_07$First_author
  
  ncomp=length(pair_data_06_07$treat1)
  
  
  Intervention1=rep("NA",ncomp)
  Intervention2=rep("NA",ncomp)     
  Intervention1=ifelse(pair_data_06_07$Treatment_control1=="Treatment" | pair_data_06_07$Treatment_control1=="Treatment/control",pair_data_06_07$treat1,Intervention1)
  Intervention1=ifelse(pair_data_06_07$Treatment_control1=="Control",pair_data_06_07$treat2,Intervention1)
  Intervention2=ifelse(Intervention1==pair_data_06_07$treat1,pair_data_06_07$treat2,Intervention2)
  Intervention2=ifelse(Intervention1==pair_data_06_07$treat2,pair_data_06_07$treat1,Intervention2)
  Comparison=str_c(Intervention1, Intervention2, sep = " vs ", collapse = NULL)
  
  
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
  
  
  
  
  
  
  sev=rep(0,ncomp)
  if(sub=="Severity"){
    #codes for severity#
    sev=ifelse(pair_data_06_07$Research_question=="Mild",1,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Mild outpatients",1,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Mild/moderate",2,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Moderate",3,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Mild to severe",4,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Moderate/severe",5,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Mild to critical",6,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Moderate to severe",7,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Moderate to critical",8,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Severe",9,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Severe/critical",10,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Unclear severity",11,sev)
    sev=ifelse(pair_data_06_07$Research_question=="Critical",12,sev)
  }
  
  if(sub=="Conflicts of interest"){
    sev=ifelse(pair_data_06_07$Conflict_of_interest_category=="no COI",1,sev)
    sev=ifelse(pair_data_06_07$Conflict_of_interest_category=="COI",2,sev)
    sev=ifelse(pair_data_06_07$Conflict_of_interest_category=="Unclear conflicts of interest",11,sev)
  }
  
  if(sub=="Funding"){
    sev=ifelse(pair_data_06_07$Funding=="Mixed/Private",1,sev)
    sev=ifelse(pair_data_06_07$Funding=="Public/non profit",2,sev)
    sev=ifelse(pair_data_06_07$Funding=="Not reported/unclear",11,sev)
    sev=ifelse(pair_data_06_07$Funding=="No specific funding",12,sev)
  }
  
  if(sub=="Location"){
    sev=ifelse(pair_data_06_07$Countries=="Multinational",1,sev)
    sev=ifelse(pair_data_06_07$Countries=="National",2,sev)
    # sev=ifelse(pair_data_01_14$Funding=="Not reported/unclear",11,sev)
    # sev=ifelse(pair_data_01_14$Funding=="No specific funding",12,sev)
  }
  
  
  #subgroups#
  subgroup=rep(0,ncomp)
  subgroup=ifelse(sev>1 & sev<12,1,subgroup)
  subgroup=ifelse(sev==12,2,subgroup)
  
  
  pair_data_06_07$comp=comp
  pair_data_06_07$r1=r1_06_07
  pair_data_06_07$r2=r2_06_07
  pair_data_06_07$N1=N1_06_07
  pair_data_06_07$N2=N2_06_07
  pair_data_06_07$sev=sev
  pair_data_06_07$subgroup=subgroup
  
  
  new_data_06_07=subset(pair_data_06_07, comp>0)
  
  
  
  analysis_data_06_07=cbind.data.frame(Trial_ID=new_data_06_07$Trial_ID,First_author=new_data_06_07$First_author,
                            F_U_days=new_data_06_07$F_U_days,Intervention1=new_data_06_07$Intervention1,
                            Intervention2=new_data_06_07$Intervention2,r1=new_data_06_07$r1,N1=new_data_06_07$N1,
                            r2=new_data_06_07$r2,N2=new_data_06_07$N2,Comparison=new_data_06_07$Comparison,
                            severity=new_data_06_07$Research_question,comp=new_data_06_07$comp,
                            sev=new_data_06_07$sev,subgroup=new_data_06_07$subgroup,Year=new_data_06_07$Year
                            ,dose=as.character(new_data_06_07$Treat_Dose_desc1),
                            A=as.character(new_data_06_07$ROB_1_randomization),
                            B=as.character(new_data_06_07$ROB2_Neg_conv),
                            C=as.character(new_data_06_07$ROB_3__Viral_Neg_Conv_events),
                            D=as.character(new_data_06_07$ROB_4__Viral_Neg_Conv_events),
                            E=as.character(new_data_06_07$ROB_5__Viral_Neg_Conv_events),
                            Overall=as.character(new_data_06_07$ROB_6__Viral_Neg_Conv_events),
                            Conflicts_of_interest=new_data_06_07$Conflict_of_interest_category,
                            Funding=new_data_06_07$Funding,
                            Countries=new_data_06_07$Countries
                            )
  analysis_data_06_07=as.data.frame(analysis_data_06_07)
  
  #analysis_data_06_07$N1=as.numeric(levels(analysis_data_06_07$N1))[analysis_data_06_07$N1]
  #analysis_data_06_07$N2=as.numeric(levels(analysis_data_06_07$N2))[analysis_data_06_07$N2]
  #analysis_data_06_07$r1=as.numeric(levels(analysis_data_06_07$r1))[analysis_data_06_07$r1]
  #analysis_data_06_07$r2=as.numeric(levels(analysis_data_06_07$r2))[analysis_data_06_07$r2]
  
  #analysis_data_06_07$sev=as.numeric(levels(analysis_data_06_07$sev))[analysis_data_06_07$sev]
  #analysis_data_06_07$subgroup=as.numeric(levels(analysis_data_06_07$subgroup))[analysis_data_06_07$subgroup]
  
  analysis_data_06_07$f1=analysis_data_06_07$N1-analysis_data_06_07$r1
  analysis_data_06_07$f2=analysis_data_06_07$N2-analysis_data_06_07$r2
  
  #analysis_data_06_07$comp=as.numeric(levels(analysis_data_06_07$comp))[analysis_data_06_07$comp]
  
  analysis_data_06_07$counts1=str_c(analysis_data_06_07$r1, analysis_data_06_07$N1, sep = "/", collapse = NULL)
  analysis_data_06_07$counts2=str_c(analysis_data_06_07$r2, analysis_data_06_07$N2, sep = "/", collapse = NULL)
  
  
  
  if(keep_only=="All population"){
    analysis_data_06_07=analysis_data_06_07
  }
  
  if(keep_only=="Mild population"){
    data_sev<- analysis_data_06_07 %>%
      filter(analysis_data_06_07$sev==1)
    analysis_data_06_07=data_sev
  }
  
  if(keep_only=="Mixed population"){
    
    data_sev<-analysis_data_06_07 %>%
      filter(analysis_data_06_07$sev %in% c(2:11))
    
    analysis_data_06_07=data_sev
  }
  
  if(keep_only=="Critical population"){
    
    data_sev<-analysis_data_06_07 %>%
      filter(analysis_data_06_07$sev==12)
    
    analysis_data_06_07=data_sev
  }
  
  
  
  comp.num=comp.num
  #file_name="comp.num_06_07_pharma_rct.jpeg"
  #file_name=paste(comp.num,"_06_07_pharma_rct.jpeg",sep="")
  #jpeg(file=file_name,width=2431,height=1346,res=200)
  #meta-analysis#
  #rm(result_06_07)
  #rm(result_06_07.3)
  
  #analysis_data_06_07=subset(analysis_data_06_07_all,comp==comp.num & !is.na(counts1) & !is.na(counts2))
  
  analysis_data_06_07= analysis_data_06_07 %>%
    filter(analysis_data_06_07$comp==comp.num)
  
  analysis_data_06_07=analysis_data_06_07[complete.cases(analysis_data_06_07$counts1), ]
  
  analysis_data_06_07=as.data.frame(analysis_data_06_07)
  
  if(nrow(analysis_data_06_07)>0){
  
  ################################################################
  ################################################################
  exclude_rob<- analysis_data_06_07 %>%
    filter(analysis_data_06_07$Overall=="High")
  
  w=setdiff(analysis_data_06_07$First_author,exclude_rob$First_author)
  
  if(high_RoB==TRUE){
    analysis_data_06_07<-subset(analysis_data_06_07,analysis_data_06_07$First_author %in% w)}
  
  if(high_RoB==FALSE){
    analysis_data_06_07=analysis_data_06_07}
  
  #################################################################
  #################################################################
  
  analysis_data_06_07=analysis_data_06_07[order(analysis_data_06_07$sev,decreasing=TRUE),]
  
  ilab=cbind(F_U_days=as.character(analysis_data_06_07$F_U_days),Intervention1=as.character(analysis_data_06_07$Intervention1),
             Intervention2=as.character(analysis_data_06_07$Intervention2),
             counts1=analysis_data_06_07$counts1,counts2=analysis_data_06_07$counts2)
  ilab=as.data.frame(ilab)
  
  ########################################################################
  analysis_data_06_07_0<-subset(analysis_data_06_07,analysis_data_06_07$comp==comp.num & analysis_data_06_07$subgroup==0)
  analysis_data_06_07_1<-subset(analysis_data_06_07,analysis_data_06_07$comp==comp.num & analysis_data_06_07$subgroup==1)
  analysis_data_06_07_2<-subset(analysis_data_06_07,analysis_data_06_07$comp==comp.num & analysis_data_06_07$subgroup==2)
  ##########################################################################
  
  result_06_07=rma(measure="RR",ai=analysis_data_06_07$r1,bi=analysis_data_06_07$f1,ci=analysis_data_06_07$r2,
                   di=analysis_data_06_07$f2,slab=paste(analysis_data_06_07$First_author,analysis_data_06_07$Year,sep=", "),
                   subset=(analysis_data_06_07$comp==comp.num),add=1/2, to="only0",drop00=FALSE)
  
  if(model=="Fixed-effects"){
    result_06_07=update(result_06_07,tau2=0)
    
  }
  
  ########### Incorporate correct weights in the model ####
  
  
  calc_var=escalc(measure = "RR",ai=analysis_data_06_07$r1,bi=analysis_data_06_07$f1,
                  ci=analysis_data_06_07$r2,di=analysis_data_06_07$f2,drop00 = T)
  
  analysis_data_06_07$var_rr=calc_var$vi
  
  if(sum(analysis_data_06_07$r1)>0 | sum(analysis_data_06_07$r2)>0){
    
    if(sum(analysis_data_06_07$f1)>0 | sum(analysis_data_06_07$f2)>0){
      weights_model=update(result_06_07,drop00=T)
    }
    
    if(sum(analysis_data_06_07$f1)==0 & sum(analysis_data_06_07$f2)==0){
      weights_model=result_06_07
      
    }
  }
  
  if(sum(analysis_data_06_07$r1)==0 & sum(analysis_data_06_07$r2)==0){
    weights_model=result_06_07
  }
  
  
  analysis_data_06_07$weights=1/(analysis_data_06_07$var_rr+weights_model$tau2)
  
  analysis_data_06_07$relative_weights=(analysis_data_06_07$weights/sum(analysis_data_06_07$weights,na.rm = T))*100
  
  analysis_data_06_07$relative_weights=format(round(analysis_data_06_07$relative_weights,digits = 2),nsmall=2)
  analysis_data_06_07$relative_weights=trimws(analysis_data_06_07$relative_weights)
  
  analysis_data_06_07$relative_weights=ifelse(analysis_data_06_07$relative_weights=="NA","",analysis_data_06_07$relative_weights)
  #analysis_data_06_07$relative_weights=ifelse(analysis_data_06_07$relative_weights==100,"100.00",analysis_data_06_07$relative_weights)
  
  analysis_data_06_07$relative_weights=ifelse(analysis_data_06_07$relative_weights!="",
                                              paste(analysis_data_06_07$relative_weights,"%",sep = ""),analysis_data_06_07$relative_weights)
  
  ilab$weights=analysis_data_06_07$relative_weights
  ilab$weights=ifelse(ilab$weights!=" 0.00%",ilab$weights," ")
  #######################################################################################################
  
  
  result_06_071=metabin(event.e = analysis_data_06_07$r1,n.e = analysis_data_06_07$N1,event.c = analysis_data_06_07$r2,
                        n.c = analysis_data_06_07$N2,sm="RR",byvar = analysis_data_06_07$subgroup,allstudies = T)
  

  
  ks0=0
  ks1=0
  ks2=0
  if (result_06_07$k>1 & nrow(analysis_data_06_07_0)>0){
    result_06_07.0=rma(measure="RR",ai=analysis_data_06_07_0$r1,bi=analysis_data_06_07_0$f1,ci=analysis_data_06_07_0$r2,
                       di=analysis_data_06_07_0$f2,slab=paste(analysis_data_06_07_0$First_author,analysis_data_06_07_0$Year,sep=", "),
                       add=1/2, 
                       to="only0",drop00=FALSE)
    
    if(model=="Fixed-effects"){
      result_06_07.0=update(result_06_07.0,tau2=0)
      
    }
    
    ks0=result_06_07.0$k
    
    if(ks0>=1 & sum(analysis_data_06_07_0$r1)>0 | sum(analysis_data_06_07_0$r2)>0){
      if(sum(analysis_data_06_07_0$f1)>0 | sum(analysis_data_06_07_0$f2)>0){
        result_06_07.0eff=rma(measure="RR",ai=analysis_data_06_07_0$r1,bi=analysis_data_06_07_0$f1,ci=analysis_data_06_07_0$r2,
                              di=analysis_data_06_07_0$f2,slab=paste(analysis_data_06_07_0$First_author,analysis_data_06_07_0$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=TRUE)
        
        if(model=="Fixed-effects"){
          result_06_07.0eff=update(result_06_07.0eff,tau2=0)
          
        }
        ks0=result_06_07.0$k
      }
      
      if(sum(analysis_data_06_07_0$f1)==0 & sum(analysis_data_06_07_0$f2)==0){
        result_06_07.0eff=rma(measure="RR",ai=analysis_data_06_07_0$r1,bi=analysis_data_06_07_0$f1,ci=analysis_data_06_07_0$r2,
                              di=analysis_data_06_07_0$f2,slab=paste(analysis_data_06_07_0$First_author,analysis_data_06_07_0$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=FALSE)
        
        if(model=="Fixed-effects"){
          result_06_07.0eff=update(result_06_07.0eff,tau2=0)
          
        }
        ks0=0
      }
      
    }
    
    if(ks0>=1 & sum(analysis_data_06_07_0$r1)==0 | sum(analysis_data_06_07_0$r2)==0){
      result_06_07.0eff=rma(measure="RR",ai=analysis_data_06_07_0$r1,bi=analysis_data_06_07_0$f1,ci=analysis_data_06_07_0$r2,
                            di=analysis_data_06_07_0$f2,slab=paste(analysis_data_06_07_0$First_author,analysis_data_06_07_0$Year,sep=", "),
                            add=1/2, 
                            to="only0",drop00=FALSE)
      
      if(model=="Fixed-effects"){
        result_06_07.0eff=update(result_06_07.0eff,tau2=0)
        
      }
      ks0=0
    }
  }
  
  
  if (result_06_07$k>1 & nrow(analysis_data_06_07_1)>0){
    result_06_07.1=rma(measure="RR",ai=analysis_data_06_07_1$r1,bi=analysis_data_06_07_1$f1,ci=analysis_data_06_07_1$r2,
                       di=analysis_data_06_07_1$f2,slab=paste(analysis_data_06_07_1$First_author,analysis_data_06_07_1$Year,sep=", "),
                       add=1/2, 
                       to="only0",drop00=FALSE)
    
    if(model=="Fixed-effects"){
      
      result_06_07.1=update(result_06_07.1,tau2=0)
    }
    
    ks1=result_06_07.1$k
    
    if(ks1>=1 & sum(analysis_data_06_07_1$r1)>0 | sum(analysis_data_06_07_1$r2)>0){
      if(sum(analysis_data_06_07_1$f1)>0 |sum(analysis_data_06_07_1$f2)>0){
        result_06_07.1eff=rma(measure="RR",ai=analysis_data_06_07_1$r1,bi=analysis_data_06_07_1$f1,ci=analysis_data_06_07_1$r2,
                              di=analysis_data_06_07_1$f2,slab=paste(analysis_data_06_07_1$First_author,analysis_data_06_07_1$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=TRUE)
      if(model=="Fixed-effects"){
        result_06_07.1eff=update(result_06_07.1eff,tau2=0)
        
      }
      ks1=result_06_07.1$k
      }
      
      if(sum(analysis_data_06_07_1$f1)==0 & sum(analysis_data_06_07_1$f2)==0){
        
        result_06_07.1eff=rma(measure="RR",ai=analysis_data_06_07_1$r1,bi=analysis_data_06_07_1$f1,ci=analysis_data_06_07_1$r2,
                              di=analysis_data_06_07_1$f2,slab=paste(analysis_data_06_07_1$First_author,analysis_data_06_07_1$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=FALSE)
        if(model=="Fixed-effects"){
          result_06_07.1eff=update(result_06_07.1eff,tau2=0)
          
        }
        ks1=0
      }
    }
    
    if(ks1>=1 & sum(analysis_data_06_07_1$r1)==0 & sum(analysis_data_06_07_1$r2)==0){
      
      result_06_07.1eff=rma(measure="RR",ai=analysis_data_06_07_1$r1,bi=analysis_data_06_07_1$f1,ci=analysis_data_06_07_1$r2,
                            di=analysis_data_06_07_1$f2,slab=paste(analysis_data_06_07_1$First_author,analysis_data_06_07_1$Year,sep=", "),
                            add=1/2, 
                            to="only0",drop00=FALSE)
      if(model=="Fixed-effects"){
        result_06_07.1eff=update(result_06_07.1eff,tau2=0)
        
      }
      ks1=0
    }
    
  }
  
  if (result_06_07$k>1 & nrow(analysis_data_06_07_2)>0){
    result_06_07.2=rma(measure="RR",ai=analysis_data_06_07_2$r1,bi=analysis_data_06_07_2$f1,ci=analysis_data_06_07_2$r2,
                       di=analysis_data_06_07_2$f2,slab=paste(analysis_data_06_07_2$First_author,analysis_data_06_07_2$Year,sep=", "),
                       add=1/2, 
                       to="only0",drop00=FALSE)
    
    if(model=="Fixed-effects"){
      result_06_07.2=update(result_06_07.2,tau2=0)
      
    }
    
    ks2=result_06_07.2$k
    
    if(ks2>=1 & sum(analysis_data_06_07_2$r1)>0 | sum(analysis_data_06_07_2$r2)>0){
      if(sum(analysis_data_06_07_2$f1)>0 & sum(analysis_data_06_07_2$f2)>0){
        result_06_07.2eff=rma(measure="RR",ai=analysis_data_06_07_2$r1,bi=analysis_data_06_07_2$f1,ci=analysis_data_06_07_2$r2,
                              di=analysis_data_06_07_2$f2,slab=paste(analysis_data_06_07_2$First_author,analysis_data_06_07_2$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=TRUE)
        ks2=result_06_07.2$k
        
      }
      if(sum(analysis_data_06_07_2$f1)==0 & sum(analysis_data_06_07_2$f2)==0){
        
        result_06_07.2eff=rma(measure="RR",ai=analysis_data_06_07_2$r1,bi=analysis_data_06_07_2$f1,ci=analysis_data_06_07_2$r2,
                              di=analysis_data_06_07_2$f2,slab=paste(analysis_data_06_07_2$First_author,analysis_data_06_07_2$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=FALSE)
        ks2=0
        
      }
      
      if(model=="Fixed-effects"){
        result_06_07.2eff=update(result_06_07.2eff,tau2=0)
      }
    }
    
    if(ks2>=1 & sum(analysis_data_06_07_2$r1)==0 & sum(analysis_data_06_07_2$r2)==0){
      
      result_06_07.2eff=rma(measure="RR",ai=analysis_data_06_07_2$r1,bi=analysis_data_06_07_2$f1,ci=analysis_data_06_07_2$r2,
                            di=analysis_data_06_07_2$f2,slab=paste(analysis_data_06_07_2$First_author,analysis_data_06_07_2$Year,sep=", "),
                            add=1/2, 
                            to="only0",drop00=FALSE)
      
      if(model=="Fixed-effects"){
        result_06_07.2eff=update(result_06_07.2eff,tau2=0)
      }
      
      ks2=0
    }
    
  }
  
  k00=0
  
  
  if(sum(analysis_data_06_07$r1)>0 | sum(analysis_data_06_07$r2)>0){
    
    if(sum(analysis_data_06_07$f1)>0 | sum(analysis_data_06_07$f2)>0){
      
      result_06_07.3=rma(measure="RR",ai=analysis_data_06_07$r1,bi=analysis_data_06_07$f1,ci=analysis_data_06_07$r2,
                         di=analysis_data_06_07$f2,slab=paste(analysis_data_06_07$First_author,analysis_data_06_07$Year,sep=", "),
                         subset=(analysis_data_06_07$comp==comp.num),add=1/2, 
                         to="only0",drop00=TRUE)
      k00=result_06_07.3$k
    }
    
    if(sum(analysis_data_06_07$f1)==0 & sum(analysis_data_06_07$f2)==0){
      result_06_07.3=rma(measure="RR",ai=analysis_data_06_07$r1,bi=analysis_data_06_07$f1,ci=analysis_data_06_07$r2,
                         di=analysis_data_06_07$f2,slab=paste(analysis_data_06_07$First_author,analysis_data_06_07$Year,sep=", "),
                         subset=(analysis_data_06_07$comp==comp.num),add=1/2, 
                         to="only0",drop00=FALSE)
      k00=0
    }
    
    
    if(model=="Fixed-effects"){
      result_06_07.3=update(result_06_07.3,tau2=0)
      
    }
    
  }
  
  if(sum(analysis_data_06_07$r1)==0 & sum(analysis_data_06_07$r2)==0){
    result_06_07.3=rma(measure="RR",ai=analysis_data_06_07$r1,bi=analysis_data_06_07$f1,ci=analysis_data_06_07$r2,
                       di=analysis_data_06_07$f2,slab=paste(analysis_data_06_07$First_author,analysis_data_06_07$Year,sep=", "),
                       subset=(analysis_data_06_07$comp==comp.num),add=1/2, 
                       to="only0",drop00=FALSE)
    
    if(model=="Fixed-effects"){
      result_06_07.3=update(result_06_07.3,tau2=0)
      
    }
    
    
    k00=0
  }
  
    
  if (k00==0){
    result_06_07.3=result_06_07
  }
  if (result_06_07$k==1){
    allrows=1
  }
  if (result_06_07$k>1){
    if ((ks0==0 & ks2==0) | (ks0==0 & ks1==0) | (ks1==0 & ks2==0)){
      allrows=c(1:result_06_07$k)    
    }
    if (ks0>0 & ks1>0 & ks2==0){
      allrows=c(2:(ks1+1),(ks1+3):(ks1+3+ks0-1))     
    }
    if (ks0>0 & ks1==0 & ks2>0){
      allrows=c(2:(ks2+1),(ks2+3):(ks2+3+ks0-1))    
    }
    if (ks0==0 & ks1>0 & ks2>0){
      allrows=c(2:(ks2+1),(ks2+3):(ks2+3+ks1-1))    
    }
    if (ks0>0 & ks1>0 & ks2>0){
      allrows=c(2:(ks2+1),(ks2+3):(ks2+3+ks1-1),(ks2+5+ks1):(ks2+5+ks1+ks0-1))    
    }
  }
  if(k00==0 & result_06_07$k==1){
    metafor::forest(result_06_07,atransf=exp,showweights=F, xlab="Risk Ratio",header=c("Study                                  Follow up                   Intervention 1         Intervention 2      r1/N1     r2/N2                                                                      Risk of Bias
                                                 days                                                                                                                                                                 A       B       C       D       E       Overall"),cex=0.73,mlab="",
                    col="white",border="white",xlim=c(-28,20),alim=c(-0.5,0.5),steps=4,
                    ilab=ilab,ilab.xpos=c(-20.2,-15,-10.5,-7.2,-5.3,14.7),rows=allrows,ylim=c(-1.5,3+max(allrows)),refline=NA,annotate=FALSE)       
  }
  if(k00!=0 | result_06_07$k!=1){
    metafor::forest(result_06_07,atransf=exp,showweights=F, xlab="Risk Ratio",header=c("Study                                  Follow up                   Intervention 1         Intervention 2      r1/N1     r2/N2                                                                      Risk of Bias
                                                 days                                                                                                                                                                 A       B       C       D       E       Overall"),cex=0.73,mlab="",
                    col="white",border="white",xlim=c(-28,20),alim=c(floor(result_06_07$ci.lb)-2,ceiling(result_06_07$ci.ub))+1,steps=4,
                    ilab=ilab,ilab.xpos=c(-20.2,-15,-10.5,-7.2,-5.3,14.7),rows=allrows,ylim=c(-1.5,3+max(allrows)),refline=NA)  
  }
  
  
  legend(10, -0.8, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
  if(k00!=0 | result_06_07$k!=1){
    addpoly.rma(result_06_07.3,row=-1,mlab="",col="darkblue",border="darkblue",atransf=exp,cex=0.9)
  }
  if(k00==0 & result_06_07$k==1){
    addpoly.rma(result_06_07.3,row=-1,mlab="",col="white",border="white",annotate=FALSE)
  }
  if (result_06_07$k>1){
    if (ks0>0 & ks1>0 & ks2==0){
      
      if(sub=="Severity"){
      addpoly.rma(result_06_07.1eff,row=1,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9)
      addpoly.rma(result_06_07.0eff,row=ks1+2,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      
      if(sub=="Conflicts of interest"){
        addpoly.rma(result_06_07.1eff,row=1,mlab="                    Studies with conflicts of interest",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_06_07.0eff,row=ks1+2,mlab="                    Studies without conflicts of interest",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      
      if(sub=="Funding"){
        addpoly.rma(result_06_07.1eff,row=1,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_06_07.0eff,row=ks1+2,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9)
        
      }
      
      if(sub=="Location"){
        addpoly.rma(result_06_07.1eff,row=1,mlab="                    National studies",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_06_07.0eff,row=ks1+2,mlab="                    Multinational studies",col="blue",border="blue",atransf=exp,cex=0.9)
        
      }
    }
    
    if (ks0>0 & ks1==0 & ks2>0){
      if(sub=="Severity"){
      addpoly.rma(result_06_07.2eff,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9)
      addpoly.rma(result_06_07.0eff,row=ks2+2,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      
      if(sub=="Funding"){
        addpoly.rma(result_06_07.2eff,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_06_07.0eff,row=ks2+2,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9)
        
      }
    }
    if (ks0==0 & ks1>0 & ks2>0){
      if(sub=="Severity"){
      addpoly.rma(result_06_07.2eff,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9)
      addpoly.rma(result_06_07.1eff,row=ks2+2,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      
      if(sub=="Funding"){
        addpoly.rma(result_06_07.2eff,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_06_07.1eff,row=ks2+2,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9)
        
      }
    }
    
    if (ks0>0 & ks1>0 & ks2>0){
      if(sub=="Severity"){
      addpoly.rma(result_06_07.2eff,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9)
      addpoly.rma(result_06_07.1eff,row=ks2+2,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9)
      addpoly.rma(result_06_07.0eff,row=ks2+4+ks1,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      if(sub=="Funding"){
        addpoly.rma(result_06_07.2eff,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_06_07.1eff,row=ks2+2,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_06_07.0eff,row=ks2+4+ks1,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9)
        
      }
    }        
  }
  
  par(xpd=TRUE)
  legend(-27.5,-0.7,adj=c(0, 0.5),legend=c("Low Risk of Bias","Some Concerns","High Risk of Bias"),ncol=1,
         col=c("chartreuse3","gold","red"),fill=c("chartreuse3","gold","red")
         ,title = "Risk of bias ratings:",border = c("chartreuse3","gold","red"), cex=0.6
  )
  legend(-21.6, -0.7,legend=c("A: Bias due to randomization","B: Bias due to deviation from intended intervention",
                              "C: Bias due to missing data","D: Bias due to outcome measurement","E: Bias due to selection of reported result"),
         ncol=1,cex=0.6,title = "Risk of Bias Domains:",box.lty = 2)
  text(-6,3.5+max(allrows),"Pharmacological treatments",
       pos=4,
       cex=1,
       col = "darkblue")
  text(-7,3+max(allrows),"Incidence of viral negative conversion D7",
       pos=4,
       cex=1)
  
  #get rob data#
  rob_data_06_07=cbind(Trial_ID=data$Trial_ID,A=as.character(data$ROB_1_randomization),
                       B=ifelse(is.na(data$ROB2_Neg_conv),as.character(data$ROB_2_Deviations_from_intervention),as.character(data$ROB2_Neg_conv)),
                       C=as.character(data$ROB_3__Viral_Neg_Conv_events),D=as.character(data$ROB_4__Viral_Neg_Conv_events),E=as.character(data$ROB_5__Viral_Neg_Conv_events),
                       Overall=as.character(data$ROB_6__Viral_Neg_Conv_events))
  rob_data_06_07=as.data.frame(rob_data_06_07)
  ncomp=length(analysis_data_06_07$Trial_ID)
  A=rep("NA",ncomp)
  B=rep("NA",ncomp)
  C=rep("NA",ncomp)
  D=rep("NA",ncomp)
  E=rep("NA",ncomp)
  Overall=rep("NA",ncomp)
  nst=length(rob_data_06_07$Trial_ID)
  for(i in 1:ncomp){
    for(j in 1:nst){
      if(rob_data_06_07$Trial_ID[j]==analysis_data_06_07$Trial_ID[i]){
        A[i]=as.character(rob_data_06_07$A[j])
        B[i]=as.character(rob_data_06_07$B[j])
        C[i]=as.character(rob_data_06_07$C[j])
        D[i]=as.character(rob_data_06_07$D[j])
        E[i]=as.character(rob_data_06_07$E[j])
        Overall[i]=as.character(rob_data_06_07$Overall[j])
      }   
    }
  }
  A=trimws(A)
  A=str_to_sentence(A)
  B=trimws(B)
  B=str_to_sentence(B)
  C=trimws(C)
  C=str_to_sentence(C)
  D=trimws(D)
  D=str_to_sentence(D)
  E=trimws(E)
  E=str_to_sentence(E)
  Overall=trimws(Overall)
  Overall=str_to_sentence(Overall)
  analysis_data_06_07$A=A
  analysis_data_06_07$B=B
  analysis_data_06_07$C=C
  analysis_data_06_07$D=D
  analysis_data_06_07$E=E
  analysis_data_06_07$Overall=Overall
  analysis_data_06_07$A[analysis_data_06_07$A=="High"]="red"
  analysis_data_06_07$A[analysis_data_06_07$A=="Low"]="chartreuse3"
  analysis_data_06_07$A[analysis_data_06_07$A=="Some concerns"]="gold"
  analysis_data_06_07$B[analysis_data_06_07$B=="High"]="red"
  analysis_data_06_07$B[analysis_data_06_07$B=="Low"]="chartreuse3"
  analysis_data_06_07$B[analysis_data_06_07$B=="Some concerns"]="gold"
  analysis_data_06_07$C[analysis_data_06_07$C=="High"]="red"
  analysis_data_06_07$C[analysis_data_06_07$C=="Low"]="chartreuse3"
  analysis_data_06_07$C[analysis_data_06_07$C=="Some concerns"]="gold"
  analysis_data_06_07$D[analysis_data_06_07$D=="High"]="red"
  analysis_data_06_07$D[analysis_data_06_07$D=="Low"]="chartreuse3"
  analysis_data_06_07$D[analysis_data_06_07$D=="Some concerns"]="gold"
  analysis_data_06_07$E[analysis_data_06_07$E=="High"]="red"
  analysis_data_06_07$E[analysis_data_06_07$E=="Low"]="chartreuse3"
  analysis_data_06_07$E[analysis_data_06_07$E=="Some concerns"]="gold"
  analysis_data_06_07$Overall[analysis_data_06_07$Overall=="High"]="red"
  analysis_data_06_07$Overall[analysis_data_06_07$Overall=="Low"]="chartreuse3"
  analysis_data_06_07$Overall[analysis_data_06_07$Overall=="Some concerns"]="gold"
  
  #analysis_rob_06_07=subset(analysis_data_06_07,comp==comp.num & !is.na(counts1) & !is.na(counts2))
  analysis_rob_06_07= analysis_data_06_07 %>%
    filter(analysis_data_06_07$comp==comp.num)
  
  analysis_rob_06_07=analysis_rob_06_07[complete.cases(analysis_rob_06_07$counts1), ]
  
  # if(den=="Randomized"){
    
  if ((ks0==0 & ks2==0) | (ks0==0 & ks1==0) | (ks1==0 & ks2==0)){
    for(i in 1:result_06_07$k){
      legend(x=3.2,y=0.41+i+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
  }
  if (ks0>0 & ks1>0 & ks2==0){
    for(i in 1:ks1){
      legend(x=3.2,y=0.8+i+1+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
    for(i in (ks1+1):(ks0+ks1)){
      legend(x=3.2,y=0.8+i+2+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
  }
  if (ks0>0 & ks1==0 & ks2>0){
    for(i in 1:ks2){
      legend(x=3.2,y=0.8+i+1+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
    for(i in (ks2+1):(ks0+ks2)){
      legend(x=3.2,y=0.8+i+2+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
  }
  if (ks0==0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      legend(x=3.2,y=0.8+i+1+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
    for(i in (ks2+1):(ks1+ks2)){
      legend(x=3.2,y=0.8+i+2+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }        
  }
  if (ks0>0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      legend(x=3.2,y=0.8+i+1+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
    for(i in (ks2+1):(ks1+ks2)){
      legend(x=3.2,y=0.8+i+2+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
    for(i in (ks1+ks2+1):(ks1+ks2+ks0)){
      legend(x=3.2,y=0.8+i+4+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_06_07$A[i],analysis_rob_06_07$B[i],analysis_rob_06_07$C[i],analysis_rob_06_07$D[i],analysis_rob_06_07$E[i],analysis_rob_06_07$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
  }
    
  # }
  
  if(den!="Randomized"){
    par(font=2)
    text(7.8, -0.25, "Risk of bias is assessed only for randomized patients",cex=0.6)
    par(font=1)
  }
  
  if(hide_dose=="FALSE"){
  if(comp.num==comp.num){
    if ((ks0==0 & ks2==0) | (ks0==0 & ks1==0) | (ks1==0 & ks2==0)){
      for(i in 1:result_06_07$k){
        text(-16.5, -0.3+i+sliderDose, pos=4, analysis_rob_06_07$dose[i],cex=0.7)
      }
    }
    if (ks0>0 & ks1>0 & ks2==0){
      for(i in 1:ks1){
        text(-16.5, -0.3+i+1+sliderDose, pos=4, analysis_rob_06_07$dose[i],cex=0.7)
      }
      for(i in (ks1+1):(ks0+ks1)){
        text(-16.5, -0.3+i+2+sliderDose, pos=4, analysis_rob_06_07$dose[i],cex=0.7)
      }
    }
    if (ks0>0 & ks1==0 & ks2>0){
      for(i in 1:ks2){
        text(-16.5, -0.3+i+1+sliderDose, pos=4, analysis_rob_06_07$dose[i],cex=0.7)
      }
      for(i in (ks2+1):(ks0+ks2)){
        text(-16.5, -0.3+i+2+sliderDose, pos=4, analysis_rob_06_07$dose[i],cex=0.7)
      }  
    }
    if (ks0==0 & ks1>0 & ks2>0){
      for(i in 1:ks2){
        text(-16.5, -0.3+i+1+sliderDose, pos=4, analysis_rob_06_07$dose[i],cex=0.7)
      }
      for(i in (ks2+1):(ks1+ks2)){
        text(-16.5, -0.3+i+2+sliderDose, pos=4, analysis_rob_06_07$dose[i],cex=0.7)
      }    
    }
    if (ks0>0 & ks1>0 & ks2>0){
      for(i in 1:ks2){
        text(-16.5, -0.3+i+1+sliderDose, pos=4, analysis_rob_06_07$dose[i],cex=0.7)
      }
      for(i in (ks2+1):(ks1+ks2)){
        text(-16.5, -0.3+i+2+sliderDose, pos=4, analysis_rob_06_07$dose[i],cex=0.7)
      }  
      for(i in (ks1+ks2+1):(ks1+ks2+ks0)){
        text(-16.5, -0.3+i+4+sliderDose, pos=4, analysis_rob_06_07$dose[i],cex=0.7)
      }
    }
  }
  
  
  count=sum(str_count(analysis_rob_06_07$dose, "\\*"),na.rm = TRUE)
  
  if(is.na(count)==TRUE){
    count=0}
  
  if(count>0){
    text(-17.5, -0.2, "(*different loading dose)", pos=4, cex=0.6)}
  
  else if(count==0) {
    text(-17.5, -0.2, "", pos=4, cex=0.6)}
  }
  
  par(font=2,cex=0.7)
  if ((ks0==0 & ks2==0) | (ks0==0 & ks1==0) | (ks1==0 & ks2==0)){
    for(i in 1:result_06_07$k){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i, pos=4, analysis_rob_06_07$severity[i])}
      
      if(analysis_rob_06_07$r1[i]==0 & analysis_rob_06_07$r2[i]==0){
        legend(12.5, 0.2+i, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
      if(analysis_rob_06_07$f1[i]==0 & analysis_rob_06_07$f2[i]==0){
        legend(12.5, 0.2+i, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
    }
  }
  
  if (ks0>0 & ks1>0 & ks2==0){
    for(i in 1:ks1){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+1, pos=4, analysis_rob_06_07$severity[i])}
      
      if(analysis_rob_06_07$r1[i]==0 & analysis_rob_06_07$r2[i]==0){
        legend(12.5, 0.2+i+1, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+1, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
      if(analysis_rob_06_07$f1[i]==0 & analysis_rob_06_07$f2[i]==0){
        legend(12.5, 0.2+i+1, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+1, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
    }
    for(i in (ks1+1):(ks0+ks1)){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+2, pos=4, analysis_rob_06_07$severity[i])}
      
      if(analysis_rob_06_07$r1[i]==0 & analysis_rob_06_07$r2[i]==0){
        legend(12.5, 0.2+i+2, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+2, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
      if(analysis_rob_06_07$f1[i]==0 & analysis_rob_06_07$f2[i]==0){
        legend(12.5, 0.2+i+2, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+2, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
    }
  }
  
  if (ks0>0 & ks1==0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+1, pos=4, analysis_rob_06_07$severity[i])}
      
      if(analysis_rob_06_07$r1[i]==0 & analysis_rob_06_07$r2[i]==0){
        legend(12.5, 0.2+i+1, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+1,"                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
      if(analysis_rob_06_07$f1[i]==0 & analysis_rob_06_07$f2[i]==0){
        legend(12.5, 0.2+i+1, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+1,"                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
    }
    for(i in (ks2+1):(ks0+ks2)){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+2, pos=4, analysis_rob_06_07$severity[i])}
      
      if(analysis_rob_06_07$r1[i]==0 & analysis_rob_06_07$r2[i]==0){
        legend(12.5, 0.2+i+2, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+2, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
      if(analysis_rob_06_07$f1[i]==0 & analysis_rob_06_07$f2[i]==0){
        legend(12.5, 0.2+i+2, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+2, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
    }  
  }
  
  if (ks0==0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+1, pos=4, analysis_rob_06_07$severity[i])}
      
      if(analysis_rob_06_07$r1[i]==0 & analysis_rob_06_07$r2[i]==0){
        legend(12.5, 0.2+i+1, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+1, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
      if(analysis_rob_06_07$f1[i]==0 & analysis_rob_06_07$f2[i]==0){
        legend(12.5, 0.2+i+1, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+1, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
    }
    
    for(i in (ks2+1):(ks1+ks2)){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+2, pos=4, analysis_rob_06_07$severity[i])}
      
      if(analysis_rob_06_07$r1[i]==0 & analysis_rob_06_07$r2[i]==0){
        legend(12.5, 0.2+i+2, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+2, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
      if(analysis_rob_06_07$f1[i]==0 & analysis_rob_06_07$f2[i]==0){
        legend(12.5, 0.2+i+2, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+2, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
    }    
  }
  
  if (ks0>0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+1, pos=4, analysis_rob_06_07$severity[i])}
      
      if(analysis_rob_06_07$r1[i]==0 & analysis_rob_06_07$r2[i]==0){
        legend(12.5, 0.2+i+1, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+1, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
      if(analysis_rob_06_07$f1[i]==0 & analysis_rob_06_07$f2[i]==0){
        legend(12.5, 0.2+i+1, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+1, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
    }
    for(i in (ks2+1):(ks1+ks2)){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+2, pos=4, analysis_rob_06_07$severity[i])}
      
      if(analysis_rob_06_07$r1[i]==0 & analysis_rob_06_07$r2[i]==0){
        legend(12.5, 0.2+i+2, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+2, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
      if(analysis_rob_06_07$f1[i]==0 & analysis_rob_06_07$f2[i]==0){
        legend(12.5, 0.2+i+2, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+2, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
    }
    
    for(i in (ks1+ks2+1):(ks1+ks2+ks0)){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+4, pos=4, analysis_rob_06_07$severity[i])}
      
      if(analysis_rob_06_07$r1[i]==0 & analysis_rob_06_07$r2[i]==0){
        legend(12.5, 0.2+i+3, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+3, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
      if(analysis_rob_06_07$f1[i]==0 & analysis_rob_06_07$f2[i]==0){
        legend(12.5, 0.2+i+3, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
        legend(-4, 0.2+i+3, "                                                ", box.col = "white", bg = "white", adj = 0.5)
      }
      
    }
  }
  
  lines(c(0,0),c(-2,(max(allrows)+1)),lty=3)
  
  text(-6.5,-1.5,"Intervention 2 better",
       pos=4,
       cex=0.9)
  text(1,-1.5,"Intervention 1 better",
       pos=4,
       cex=0.9)
  
   # today <- Sys.Date()
   # today=format(today, format="%m %d %Y")
   # legend("bottomright",legend=paste("Forest plot was updated on:"," ",today,sep=""),
   #        ncol=1,cex=0.85,box.lty = 0)
  
  legend("bottomright",legend=paste("Data source: the COVID-NMA initiative (https://covid-nma.com/)"),
         ncol=1,cex=0.85,box.lty = 0,text.col = "blue")
   
   par(font=2)
   N1_total=sum(analysis_data_06_07$N1)
   N2_total=sum(analysis_data_06_07$N2)
   r1_total=sum(analysis_data_06_07$r1)
   r2_total=sum(analysis_data_06_07$r2)
   
   I1=paste(r1_total,N1_total,sep = "/")
   I2=paste(r2_total,N2_total,sep="/")
   
   text(-7.5,0.65,I1,cex=1)
   text(-5.1,0.65,I2,cex=1)
   text(-9.7,0.65,"Total:",cex=1)
   
   par(font=1)
  
  if(result_06_07$k>1){
    if(length(unique(analysis_data_06_07$subgroup))==1){
    
    text(-28, 0.2, pos=4, cex=0.9, bquote(paste("Heterogeneity: Q = ",
                                                .(formatC(result_06_07$QE, digits=2, format="f")),
                                                ", p = ", .(formatC(result_06_07$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                .(formatC(result_06_07$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                .(formatC(result_06_07$tau2, digits=2, format="f")))))
    }
    
    if(length(unique(analysis_data_06_07$subgroup))>1){
      
      
      
      text(-28, 0.2, pos=4, cex=0.9, bquote(paste("Heterogeneity: Q = ",
                                                  .(formatC(result_06_07$QE, digits=2, format="f")),
                                                  ", p = ", .(formatC(result_06_07$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                  .(formatC(result_06_07$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                  .(formatC(result_06_07$tau2, digits=2, format="f")), "; ", "Test for subgroup differences: Q = ",
                                                  .(formatC(result_06_071$Q.b.random, digits=2, format="f")),"; ", "p = ",
                                                  .(formatC(result_06_071$pval.Q.b.random, digits=2, format="f"))
                                                  
      )))
      
    }
    
    
  }
  }
  
  else if (nrow(analysis_data_06_07)==0){
    stop("No data available. Please, make another choice.")
  }
  #dev.off()
  }

  
  