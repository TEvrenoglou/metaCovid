#library(metafor)
#library(readxl)
#library(netmeta)
#library(meta)
#library(stringr)
#library(tidyverse)
#import and prepare data#
#data=read.csv("C:\\Users\\Theodoros Evrenoglou\\Desktop\\Covid-19 NMA\\Database\\October\\20-10-2020\\rct__updated_database_20_10_2020_new_merged.csv", na=c("*","NA"))

forest_Time_to_death=function(data,comp.num,sliderRob=0,sliderDose=0,high_RoB=FALSE,keep_only="All population",model="Random-effects",
                              sub="Severity",den="Randomized",hide_dose=FALSE,hide_severity=FALSE){
  
  
  data=data[,c("Trial_ID","ID_stats","name_study","First_author","Year","Study_design","Funding","Conflict_of_interest_category",
               "Research_question","Countries","F_U_days","Treatment_control","Treat_Name","stats_name","Treat_desc_summary",
               "n_Randomized","n_Analyzed_",
               "Clinical_improvement_D7_n",
               "time_to_death_HR","time_to_death_lci","time_to_death_uci",
               "ROB_1_randomization","ROB2_Time_death","ROB_2_Deviations_from_intervention","ROB_3_time_to_death",
               "ROB_4_mortality_OR_time_to_death","ROB_5_time_to_death","ROB_6_time_to_death"
               
  )]
  
  
  data$F_U_days=as.numeric(data$F_U_days)
  data$n_Randomized=as.numeric(data$n_Randomized)
  # data$n_without_consent=as.numeric(data$n_without_consent)
  # data$n_without_consent=ifelse(is.na(data$n_without_consent),0,data$n_without_consent)
  #data$n_Randomized=as.numeric(data$n_Analyzed_)
  #data$n_Randomized=data$n_Randomized-data$n_without_consent
  data$n_Randomized=ifelse(is.na(data$n_Randomized),data$n_Analyzed_,data$n_Randomized)
  # data$n_in_the_as_treated_analysis=ifelse(is.na(data$n_in_the_as_treated_analysis),data$n_Randomized,data$n_in_the_as_treated_analysis)
  #data$NEG_conv_denominator=ifelse(is.na(data$NEG_conv_denominator),data$n_Randomized,data$NEG_conv_denominator)
  data$n_Analyzed_=ifelse(is.na(data$n_Analyzed_),data$n_Randomized,data$n_Analyzed_)
  
  if(den=="Randomized"){
    
    data$n_Randomized=data$n_Randomized
  }
  if(den!="Randomized"){
    data$n_Randomized=data$n_Analyzed_
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
  # data$First_author=ifelse(is.na(data$name_study),as.character(data$First_author),paste(as.character(data$First_author),as.character(data$name_study),sep = ","))
  #data$First_author=ifelse(is.na(data$name_study),as.character(data$First_author),as.character(data$name_study))
  data$Year=trimws(data$Year)
  data$Research_question=trimws(data$Research_question)
  data$Research_question=str_to_sentence(data$Research_question)
  
  # data$Treat_Name[data$Treat_Name=="Corticosteroids (HC, MPS, DM)"]="Corticosteroids"
  # data$Treat_Name[data$Treat_Name=="Corticosteroids (MPS or Pred or HC or Dex)"]="Corticosteroids"
  # data$Treat_Name[data$Treat_Name=="IFN alpha2b+IFN gamma"]="INF a-2b+INF gamma"
  # data$Treat_Name[data$Treat_Name=="Interferon alpha2b"]="Interferon a-2b"
  #data$Treat_desc_summary[data$Treat_desc_summary=="at prescribers' discretion "]="-"
  data$Treat_Dose_desc=as.character(data$Treat_desc_summary)
  
  
  rct_data=subset(data,data$Study_design=="RCT" & Trial_ID!=9 & Trial_ID!=139)
  
  rct_data$Clinical_improvement_D7_n=as.numeric(rct_data$Clinical_improvement_D7_n)
  pair_data=pairwise(studlab=Trial_ID,treat=Treat_Name,event=Clinical_improvement_D7_n,n=n_Randomized,
                     data=rct_data,measure="RR",ref="Standard care")
  pair_data$F_U_days1=pair_data$F_U_days
  pair_data$First_author=pair_data$First_author
  
  ncomp=length(pair_data$treat1) 
  
  pair_data$time_to_death_HR=as.numeric(pair_data$time_to_death_HR1)
  pair_data$time_to_death_lci=as.numeric(pair_data$time_to_death_lci1)
  pair_data$time_to_death_uci=as.numeric(pair_data$time_to_death_uci1)
  
  
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
    sev=ifelse(pair_data$Research_question=="Mild",1,sev)
    sev=ifelse(pair_data$Research_question=="Mild outpatients",1,sev)
    sev=ifelse(pair_data$Research_question=="Mild/moderate",2,sev)
    sev=ifelse(pair_data$Research_question=="Moderate",3,sev)
    sev=ifelse(pair_data$Research_question=="Mild to severe",4,sev)
    sev=ifelse(pair_data$Research_question=="Moderate/severe",5,sev)
    sev=ifelse(pair_data$Research_question=="Mild to critical",6,sev)
    sev=ifelse(pair_data$Research_question=="Moderate to severe",7,sev)
    sev=ifelse(pair_data$Research_question=="Moderate to critical",8,sev)
    sev=ifelse(pair_data$Research_question=="Severe",9,sev)
    sev=ifelse(pair_data$Research_question=="Severe/critical",10,sev)
    sev=ifelse(pair_data$Research_question=="Unclear severity",11,sev)
    sev=ifelse(pair_data$Research_question=="Critical",12,sev)
  }
  
  if(sub=="Conflicts of interest"){
    sev=ifelse(pair_data$Conflict_of_interest_category=="no COI",1,sev)
    sev=ifelse(pair_data$Conflict_of_interest_category=="COI",2,sev)
    sev=ifelse(pair_data$Conflict_of_interest_category=="Unclear conflicts of interest",11,sev)
  }
  
  if(sub=="Funding"){
    sev=ifelse(pair_data$Funding=="Mixed/Private",1,sev)
    sev=ifelse(pair_data$Funding=="Public/non profit",2,sev)
    sev=ifelse(pair_data$Funding=="Not reported/unclear",11,sev)
    sev=ifelse(pair_data$Funding=="No specific funding",12,sev)
  }
  
  if(sub=="Location"){
    sev=ifelse(pair_data$Countries=="Multinational",1,sev)
    sev=ifelse(pair_data$Countries=="National",2,sev)
    # sev=ifelse(pair_data_01_14$Funding=="Not reported/unclear",11,sev)
    # sev=ifelse(pair_data_01_14$Funding=="No specific funding",12,sev)
  }
  
  #subgroups#
  subgroup=rep(0,ncomp)
  subgroup=ifelse(sev>1 & sev<12,1,subgroup)
  subgroup=ifelse(sev==12,2,subgroup)
  
  pair_data$comp=comp
  pair_data$N1=N1
  pair_data$N2=N2
  pair_data$sev=sev
  pair_data$subgroup=subgroup
  
  new_data=subset(pair_data, comp>0)
  
  
  
  analysis_data_12=cbind.data.frame(Trial_ID=new_data$Trial_ID,First_author=new_data$First_author,
                         F_U_days=new_data$F_U_days,Intervention1=new_data$Intervention1,
                         Intervention2=new_data$Intervention2,N1=new_data$N1,
                         N2=new_data$N2,Comparison=new_data$Comparison,
                         severity=new_data$Research_question,comp=new_data$comp,
                         HR=new_data$time_to_death_HR,LCI=new_data$time_to_death_lci,
                         UCI=new_data$time_to_death_uci,
                         sev=new_data$sev,subgroup=new_data$subgroup,
                         Year=new_data$Year,dose=as.character(new_data$Treat_Dose_desc1),
                         A=as.character(new_data$ROB_1_randomization),
                         B=as.character(new_data$ROB2_Time_death),
                         C=as.character(new_data$ROB_3_time_to_death),
                         D=as.character(new_data$ROB_4_mortality_OR_time_to_death),
                         E=as.character(new_data$ROB_5_time_to_death),
                         Overall=as.character(new_data$ROB_6_time_to_death),
                         Conflicts_of_interest=new_data$Conflict_of_interest_category,
                         Funding=new_data$Funding,
                         Countries=new_data$Countries)
  
  analysis_data_12=as.data.frame(analysis_data_12)
  
  #analysis_data_12$N1=as.numeric(levels(analysis_data_12$N1))[analysis_data_12$N1]
  #analysis_data_12$N2=as.numeric(levels(analysis_data_12$N2))[analysis_data_12$N2]
  #analysis_data_12$comp=as.numeric(levels(analysis_data_12$comp))[analysis_data_12$comp]
  
  #analysis_data_12$LCI=as.numeric(levels(analysis_data_12$LCI))[analysis_data_12$LCI]
  #analysis_data_12$UCI=as.numeric(levels(analysis_data_12$UCI))[analysis_data_12$UCI]
  
  #analysis_data_12$HR=as.numeric(levels(analysis_data_12$HR))[analysis_data_12$HR]
  
  analysis_data_12$logHR=log(analysis_data_12$HR)
  
  analysis_data_12$se=(log(analysis_data_12$UCI)-log(analysis_data_12$LCI))/3.92
  
  #analysis_data_12$sev=as.numeric(levels(analysis_data_12$sev))[analysis_data_12$sev]
  #analysis_data_12$subgroup=as.numeric(levels(analysis_data_12$subgroup))[analysis_data_12$subgroup]
  
  if(keep_only=="All population"){
    analysis_data_12=analysis_data_12
  }
  
  if(keep_only=="Mild population"){
    data_sev<- analysis_data_12 %>%
      filter(analysis_data_12$sev==1)
    analysis_data_12=data_sev
  }
  
  if(keep_only=="Mixed population"){
    
    data_sev<-analysis_data_12 %>%
      filter(analysis_data_12$sev %in% c(2:11))
    
    analysis_data_12=data_sev
  }
  
  if(keep_only=="Critical population"){
    
    data_sev<-analysis_data_12 %>%
      filter(analysis_data_12$sev==12)
    
    analysis_data_12=data_sev
  }
  
  
  
  
  comp.num=comp.num
  #file_name="comp.num_12_pharma_rct.jpeg"
  #file_name=paste(comp.num,"_12_pharma_rct.jpeg",sep="")
  #jpeg(file=file_name,width=2431,height=1346,res=200)
  #meta-analysis#
  #rm(result_12)
  #rm(result_12.3)
  
  #analysis_data_12=subset(analysis_data_12_all,comp==comp.num & !is.na(logHR) & !is.na(se))
  
  analysis_data_12= analysis_data_12 %>%
    filter(analysis_data_12$comp==comp.num)
  
  analysis_data_12=analysis_data_12[complete.cases(analysis_data_12$UCI), ]
  
  analysis_data_12=as.data.frame(analysis_data_12)
  
  if(nrow(analysis_data_12)>0){
  
  ################################################################
  ################################################################
  exclude_rob<- analysis_data_12 %>%
    filter(analysis_data_12$Overall=="High")
  
  w=setdiff(analysis_data_12$First_author,exclude_rob$First_author)
  
  if(high_RoB==TRUE){
    analysis_data_12<-subset(analysis_data_12,analysis_data_12$First_author %in% w)}
  
  if(high_RoB==FALSE){
    analysis_data_12=analysis_data_12}
  
  
  analysis_data_12=analysis_data_12[order(analysis_data_12$sev,decreasing=TRUE),]
  
  ilab=cbind(F_U_days=as.character(analysis_data_12$F_U_days),Intervention1=as.character(analysis_data_12$Intervention1),
             Intervention2=as.character(analysis_data_12$Intervention2),
             counts1=analysis_data_12$N1,counts2=analysis_data_12$N2)
  ilab=as.data.frame(ilab)
  
  ########################################################################
  analysis_data_12_0<-subset(analysis_data_12,analysis_data_12$comp==comp.num & analysis_data_12$subgroup==0)
  analysis_data_12_1<-subset(analysis_data_12,analysis_data_12$comp==comp.num & analysis_data_12$subgroup==1)
  analysis_data_12_2<-subset(analysis_data_12,analysis_data_12$comp==comp.num & analysis_data_12$subgroup==2)
  ##########################################################################
  
  
  result_12=rma(measure="GEN",yi=analysis_data_12$logHR,sei=analysis_data_12$se,
                slab=paste(analysis_data_12$First_author,analysis_data_12$Year,sep=", "),
                subset=(analysis_data_12$comp==comp.num))
  
  result_121=metagen(TE=analysis_data_12$logHR,seTE=analysis_data_12$se,byvar = analysis_data_12$subgroup)
  
  if(model=="Fixed-effects"){
    
    result_12=update(result_12,tau2=0)
  }
  
  
  ks0=0
  ks1=0
  ks2=0
  if (result_12$k>1 & nrow(analysis_data_12_0)>0){
    result_12.0=rma(measure="GEN",yi=analysis_data_12_0$logHR,sei=analysis_data_12_0$se,
                    slab=paste(analysis_data_12_0$First_author,analysis_data_12_0$Year,sep=", "))
    
    if(model=="Fixed-effects"){
      result_12.0=update(result_12.0,tau2=0)
      
    }
    
    ks0=result_12.0$k
  }
  if (result_12$k>1 & nrow(analysis_data_12_1)>0){
    result_12.1=rma(measure="GEN",yi=analysis_data_12_1$logHR,sei=analysis_data_12_1$se,
                    slab=paste(analysis_data_12_1$First_author,analysis_data_12_1$Year,sep=", "))
    
    if(model=="Fixed-effects"){
      result_12.1=update(result_12.1,tau2=0)
      
    }
    
    ks1=result_12.1$k
  }
  if (result_12$k>1 & nrow(analysis_data_12_2)>0 ){
    result_12.2=rma(measure="GEN",yi=analysis_data_12_2$logHR,sei=analysis_data_12_2$se,
                    slab=paste(analysis_data_12_2$First_author,analysis_data_12_2$Year,sep=", "))
    
    if(model=="Fixed-effects"){
      
      result_12.2=update(result_12.2,tau2=0)
    }
    
    
    ks2=result_12.2$k
  }
  result_12.3=result_12
  if (result_12$k==1){
    allrows=1
  }
  if (result_12$k>1){
    if ((ks0==0 & ks2==0) | (ks0==0 & ks1==0) | (ks1==0 & ks2==0)){
      allrows=c(1:result_12$k)    
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
  metafor::forest(result_12,atransf=exp,showweights=TRUE, xlab="Hazard Ratio",header=c("Study                                   Study Duration         Intervention 1               Intervention 2          N1       N2                                                                 Risk of Bias
                                                    days                                                                                                                                                                A       B       C       D       E       Overall"),cex=0.73,mlab="",
                  col="white",border="white",xlim=c(-28,20),alim=c(floor(result_12$ci.lb)-2,ceiling(result_12$ci.ub))+1,steps=4,
                  ilab=ilab,ilab.xpos=c(-19.5,-15,-9.5,-6,-3.5),rows=allrows,ylim=c(-1.5,3+max(allrows)))  
  legend(10, -0.8, "                                                  ", box.col = "white", bg = "white", adj = 0.5)
  addpoly.rma(result_12.3,row=-1,mlab="",col="darkblue",border="darkblue",atransf=exp,cex=0.9)
  if (result_12$k>1){
    if (ks0>0 & ks1>0 & ks2==0){
      if(sub=="Severity"){
      addpoly.rma(result_12.1,row=1,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9)
      addpoly.rma(result_12.0,row=ks1+2,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      
      if(sub=="Conflicts of interest"){
        addpoly.rma(result_12.1,row=1,mlab="                    Studies with conflicts of interest",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_12.0,row=ks1+2,mlab="                    Studies without conflicts of interest",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      
      if(sub=="Funding"){
        addpoly.rma(result_12.1,row=1,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_12.0,row=ks1+2,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      
      if(sub=="Location"){
        addpoly.rma(result_12.1,row=1,mlab="                    National studies",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_12.0,row=ks1+2,mlab="                    Multinational studies",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      
      
    }
    
    if (ks0>0 & ks1==0 & ks2>0){
      if(sub=="Severity"){
      addpoly.rma(result_12.2,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9)
      addpoly.rma(result_12.0,row=ks2+2,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      
      if(sub=="Funding"){
        addpoly.rma(result_12.2,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_12.0,row=ks2+2,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9)
        
      }
    }
    
    if (ks0==0 & ks1>0 & ks2>0){
      if(sub=="Severity"){
      addpoly.rma(result_12.2,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9)
      addpoly.rma(result_12.1,row=ks2+2,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      if(sub=="Funding"){
        addpoly.rma(result_12.2,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_12.1,row=ks2+2,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      
    }
    if (ks0>0 & ks1>0 & ks2>0){
      if(sub=="Severity"){
      addpoly.rma(result_12.2,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9)
      addpoly.rma(result_12.1,row=ks2+2,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9)
      addpoly.rma(result_12.0,row=ks2+4+ks1,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9)
      }
      if(sub=="Funding"){
        addpoly.rma(result_12.2,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_12.1,row=ks2+2,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9)
        addpoly.rma(result_12.0,row=ks2+4+ks1,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9)
        
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
  text(-4.5,3+max(allrows),"Time to death",
       pos=4,
       cex=1)
  
  #get rob data#
  rob_data_12=cbind(Trial_ID=data$Trial_ID,A=as.character(data$ROB_1_randomization),
                    B=ifelse(is.na(data$ROB2_Time_death),as.character(data$ROB_2_Deviations_from_intervention),data$ROB2_Time_death),
                    C=as.character(data$ROB_3_time_to_death),D=as.character(data$ROB_4_mortality_OR_time_to_death),E=as.character(data$ROB_5_time_to_death),
                    Overall=as.character(data$ROB_6_time_to_death))
  rob_data_12=as.data.frame(rob_data_12)
  ncomp=length(analysis_data_12$Trial_ID)
  A=rep("NA",ncomp)
  B=rep("NA",ncomp)
  C=rep("NA",ncomp)
  D=rep("NA",ncomp)
  E=rep("NA",ncomp)
  Overall=rep("NA",ncomp)
  nst=length(rob_data_12$Trial_ID)
  for(i in 1:ncomp){
    for(j in 1:nst){
      if(rob_data_12$Trial_ID[j]==analysis_data_12$Trial_ID[i]){
        A[i]=as.character(rob_data_12$A[j])
        B[i]=as.character(rob_data_12$B[j])
        C[i]=as.character(rob_data_12$C[j])
        D[i]=as.character(rob_data_12$D[j])
        E[i]=as.character(rob_data_12$E[j])
        Overall[i]=as.character(rob_data_12$Overall[j])
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
  analysis_data_12$A=A
  analysis_data_12$B=B
  analysis_data_12$C=C
  analysis_data_12$D=D
  analysis_data_12$E=E
  analysis_data_12$Overall=Overall
  analysis_data_12$A[analysis_data_12$A=="High"]="red"
  analysis_data_12$A[analysis_data_12$A=="Low"]="chartreuse3"
  analysis_data_12$A[analysis_data_12$A=="Some concerns"]="gold"
  analysis_data_12$B[analysis_data_12$B=="High"]="red"
  analysis_data_12$B[analysis_data_12$B=="Low"]="chartreuse3"
  analysis_data_12$B[analysis_data_12$B=="Some concerns"]="gold"
  analysis_data_12$C[analysis_data_12$C=="High"]="red"
  analysis_data_12$C[analysis_data_12$C=="Low"]="chartreuse3"
  analysis_data_12$C[analysis_data_12$C=="Some concerns"]="gold"
  analysis_data_12$D[analysis_data_12$D=="High"]="red"
  analysis_data_12$D[analysis_data_12$D=="Low"]="chartreuse3"
  analysis_data_12$D[analysis_data_12$D=="Some concerns"]="gold"
  analysis_data_12$E[analysis_data_12$E=="High"]="red"
  analysis_data_12$E[analysis_data_12$E=="Low"]="chartreuse3"
  analysis_data_12$E[analysis_data_12$E=="Some concerns"]="gold"
  analysis_data_12$Overall[analysis_data_12$Overall=="High"]="red"
  analysis_data_12$Overall[analysis_data_12$Overall=="Low"]="chartreuse3"
  analysis_data_12$Overall[analysis_data_12$Overall=="Some concerns"]="gold"
  
  #analysis_rob_12=subset(analysis_data_12,comp==comp.num & !is.na(logHR) & !is.na(se))
  
  analysis_rob_12= analysis_data_12 %>%
    filter(analysis_data_12$comp==comp.num)
  
  analysis_rob_12=analysis_rob_12[complete.cases(analysis_rob_12$UCI), ]
  
  # if(den=="Randomized"){
  
  if ((ks0==0 & ks2==0) | (ks0==0 & ks1==0) | (ks1==0 & ks2==0)){
    for(i in 1:result_12$k){
      legend(x=3.2,y=0.41+i+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
  }
  if (ks0>0 & ks1>0 & ks2==0){
    for(i in 1:ks1){
      legend(x=3.2,y=0.8+i+1+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
    for(i in (ks1+1):(ks0+ks1)){
      legend(x=3.2,y=0.8+i+2+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
  }
  if (ks0>0 & ks1==0 & ks2>0){
    for(i in 1:ks2){
      legend(x=3.2,y=0.8+i+1+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
    for(i in (ks2+1):(ks0+ks2)){
      legend(x=3.2,y=0.8+i+2+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
  }
  if (ks0==0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      legend(x=3.2,y=0.8+i+1+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
    for(i in (ks2+1):(ks1+ks2)){
      legend(x=3.2,y=0.8+i+2+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }        
  }
  if (ks0>0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      legend(x=3.2,y=0.8+i+1+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
    for(i in (ks2+1):(ks1+ks2)){
      legend(x=3.2,y=0.8+i+2+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),
             text.width = c(0.22,0.22,0.22,0.22,0.22,0.22),
      )
    }
    for(i in (ks1+ks2+1):(ks1+ks2+ks0)){
      legend(x=3.2,y=0.8+i+4+sliderRob,legend=c("", "","","","",""),
             col=c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]), 
             cex=0.75,inset=0.1,bg="white",
             box.lty=0,title = "",fill = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),pt.cex = 5
             ,x.intersp=0.5,
             horiz = TRUE,
             border = c(analysis_rob_12$A[i],analysis_rob_12$B[i],analysis_rob_12$C[i],analysis_rob_12$D[i],analysis_rob_12$E[i],analysis_rob_12$Overall[i]),
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
      for(i in 1:result_12$k){
        text(-16.5, -0.3+i+sliderDose, pos=4, analysis_rob_12$dose[i],cex=0.7)
      }
    }
    if (ks0>0 & ks1>0 & ks2==0){
      for(i in 1:ks1){
        text(-16.5, -0.3+i+1+sliderDose, pos=4, analysis_rob_12$dose[i],cex=0.7)
      }
      for(i in (ks1+1):(ks0+ks1)){
        text(-16.5, -0.3+i+2+sliderDose, pos=4, analysis_rob_12$dose[i],cex=0.7)
      }
    }
    if (ks0>0 & ks1==0 & ks2>0){
      for(i in 1:ks2){
        text(-16.5, -0.3+i+1+sliderDose, pos=4, analysis_rob_12$dose[i],cex=0.7)
      }
      for(i in (ks2+1):(ks0+ks2)){
        text(-16.5, -0.3+i+2+sliderDose, pos=4, analysis_rob_12$dose[i],cex=0.7)
      }  
    }
    if (ks0==0 & ks1>0 & ks2>0){
      for(i in 1:ks2){
        text(-16.5, -0.3+i+1+sliderDose, pos=4, analysis_rob_12$dose[i],cex=0.7)
      }
      for(i in (ks2+1):(ks1+ks2)){
        text(-16.5, -0.3+i+2+sliderDose, pos=4, analysis_rob_12$dose[i],cex=0.7)
      }    
    }
    if (ks0>0 & ks1>0 & ks2>0){
      for(i in 1:ks2){
        text(-16.5, -0.3+i+1+sliderDose, pos=4, analysis_rob_12$dose[i],cex=0.7)
      }
      for(i in (ks2+1):(ks1+ks2)){
        text(-16.5, -0.3+i+2+sliderDose, pos=4, analysis_rob_12$dose[i],cex=0.7)
      }  
      for(i in (ks1+ks2+1):(ks1+ks2+ks0)){
        text(-16.5, -0.3+i+4+sliderDose, pos=4, analysis_rob_12$dose[i],cex=0.7)
      }
    }
  }
  
    
  count=sum(str_count(analysis_rob_12$dose, "\\*"),na.rm = TRUE)
  
  if(is.na(count)==TRUE){
    count=0}
  
  if(count>0){
    text(-17.5, -0.2, "(*different loading dose)", pos=4, cex=0.6)}
  
  else if(count==0) {
    text(-17.5, -0.2, "", pos=4, cex=0.6)}
  }
  
  par(font=2,cex=0.7)
  if ((ks0==0 & ks2==0) | (ks0==0 & ks1==0) | (ks1==0 & ks2==0)){
    for(i in 1:result_12$k){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i, pos=4, analysis_rob_12$severity[i])
      }
    }
  }
  if (ks0>0 & ks1>0 & ks2==0){
    for(i in 1:ks1){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+1, pos=4, analysis_rob_12$severity[i])
      }
    }
    for(i in (ks1+1):(ks0+ks1)){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+2, pos=4, analysis_rob_12$severity[i])
      }
    }
  }
  if (ks0>0 & ks1==0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+1, pos=4, analysis_rob_12$severity[i])
        
      }
    }
    for(i in (ks2+1):(ks0+ks2)){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+2, pos=4, analysis_rob_12$severity[i])
      }
    }  
  }
  if (ks0==0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+1, pos=4, analysis_rob_12$severity[i])
      }
    }
    for(i in (ks2+1):(ks1+ks2)){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+2, pos=4, analysis_rob_12$severity[i])
      }
    }    
  }
  if (ks0>0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+1, pos=4, analysis_rob_12$severity[i])
      }
    }
    for(i in (ks2+1):(ks1+ks2)){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+2, pos=4, analysis_rob_12$severity[i])
      }
    }  
    for(i in (ks1+ks2+1):(ks1+ks2+ks0)){
      if(hide_severity=="FALSE"){
      text(-28, 0.5+i+4, pos=4, analysis_rob_12$severity[i])
      }
    }
  }
  
  text(-6.5,-1.5,"Intervention 1 better",
       pos=4,
       cex=0.9)
  text(1,-1.5,"Intervention 2 better",
       pos=4,
       cex=0.9)
  
      # today <- Sys.Date()
      # today=format(today, format="%m %d %Y")
      # legend("bottomright",legend=paste("Forest plot was updated on:"," ",today,sep=""),
      #        ncol=1,cex=0.85,box.lty = 0)
  
  legend("bottomright",legend=paste("Data source: the COVID-NMA initiative (https://covid-nma.com/)"),
         ncol=1,cex=0.85,box.lty = 0,text.col = "blue")
  
  if(result_12$k>1){
    if(length(unique(analysis_data_12$subgroup))==1){
    text(-28, 0.2, pos=4, cex=0.9, bquote(paste("Heterogeneity: Q = ",
                                                .(formatC(result_12$QE, digits=2, format="f")),
                                                ", p = ", .(formatC(result_12$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                .(formatC(result_12$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                .(formatC(result_12$tau2, digits=2, format="f")))))
    }
    if(length(unique(analysis_data_12$subgroup))>1){
      
      text(-28, 0.2, pos=4, cex=0.9, bquote(paste("Heterogeneity: Q = ",
                                                  .(formatC(result_12$QE, digits=2, format="f")),
                                                  ", p = ", .(formatC(result_12$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                  .(formatC(result_12$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                  .(formatC(result_12$tau2, digits=2, format="f")), "; ", "Test for subgroup differences: Q = ",
                                                  .(formatC(result_121$Q.b.random, digits=2, format="f")),"; ", "p = ",
                                                  .(formatC(result_121$pval.Q.b.random, digits=2, format="f"))
                                                  
      )))
      
      
    }
    
  }
  }
  
  else if (nrow(analysis_data_12)==0){
    stop("No data available. Please, make another choice.")
  }
 #dev.off()
}
  
  