#library(metafor)
#library(readxl)
#library(netmeta)
#library(meta)
#library(stringr)
#library(tidyverse)
#import and prepare data#
#data=read.csv("C:\\Users\\Theodoros Evrenoglou\\Desktop\\Covid-19 NMA\\Database\\October\\29-10-2020\\rct__updated_database_29_10_2020_new_merged.csv", na=c("*","NA"))

forest_NEG_D7=function(data,dat,comp.num,sliderRob=0,sliderDose=0,high_RoB=FALSE,keep_only="All population",model="Random-effects",
                       sub="Severity",den="Randomized",hide_dose=FALSE,hide_severity=FALSE,
                       Publication=FALSE,het_estimate="REML"){
  
  
  data=data[,c("Trial_ID","ID_stats","name_study","First_author","Year","Study_design","Funding","Conflict_of_interest_category",
               "Research_question","Countries","F_U_days","Treatment_control","Treat_Name","stats_name","Treat_desc_summary",
               "n_Randomized","n_Analyzed_",
               "NEG_conv_n_D7","NEG_conv_n_D3","NEG_conv_denominator_d3","NEG_conv_denominator_d7",
               "Neg_conv_D7_TP",
               "ROB_1_randomization","ROB2_Neg_conv","ROB_2_Deviations_from_intervention","ROB_3__Viral_Neg_Conv_events",
               "ROB_4__Viral_Neg_Conv_events","ROB_5__Viral_Neg_Conv_events","ROB_6__Viral_Neg_Conv_events","Type_publication"
               
  )]
  
  
  data$F_U_days=as.numeric(data$F_U_days)
  data$Neg_conv_D7_TP=as.numeric(data$Neg_conv_D7_TP)
  data$F_U_days=ifelse(is.na(data$Neg_conv_D7_TP),data$F_U_days,data$Neg_conv_D7_TP)
  data$Type_publication=trimws(data$Type_publication)
  data$n_Randomized=as.numeric(data$n_Randomized)
  data$n_Randomized=ifelse(is.na(data$n_Randomized),data$n_Analyzed_,data$n_Randomized)
  
  
  data$ROB2_Neg_conv=ifelse(is.na(data$ROB2_Neg_conv),data$ROB_2_Deviations_from_intervention,data$ROB2_Neg_conv)
  
  data$NEG_conv_denominator_d7=as.numeric(data$NEG_conv_denominator_d7)
  data$NEG_conv_denominator_d3=as.numeric(data$NEG_conv_denominator_d3)
  
  data$NEG_conv_denominator_d7=ifelse(is.na(data$NEG_conv_denominator_d7),data$NEG_conv_denominator_d3,data$NEG_conv_denominator_d7)
  data$NEG_conv_denominator_d7=ifelse(is.na(data$NEG_conv_denominator_d7),data$n_Randomized,data$NEG_conv_denominator_d7)
  data$NEG_conv_denominator_d7=as.numeric(data$NEG_conv_denominator_d7)
  
  
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
  data$First_author=ifelse(is.na(data$First_author),data$name_study,data$First_author)
  data$Year=trimws(data$Year)
  data$Research_question=trimws(data$Research_question)
  data$Research_question=str_to_sentence(data$Research_question)
  
  data$Treat_Dose_desc=as.character(data$Treat_desc_summary)
  
  data$NEG_conv_n_D7=ifelse(!is.na(data$NEG_conv_n_D7),data$NEG_conv_n_D7,data$NEG_conv_n_D3)

  rct_data=subset(data,data$Study_design=="RCT" & Trial_ID!=9)

  
  
  rct_data$NEG_conv_n_D7=as.numeric(rct_data$NEG_conv_n_D7)
  
  ##############################################
  E=which(dat$comp.num==comp.num)
  
  dat=dat[E,]
  dat$treat1=trimws(dat$treat1)
  dat$treat2=trimws(dat$treat2)
  
  
  id1<- rct_data %>%
    filter(stats_name %in% dat$treat1)
  id1=id1$Trial_ID
  
  id2<- rct_data %>%
    filter(stats_name %in% dat$treat2)
  id2=id2$Trial_ID
  
  id=intersect(id1,id2)
  
  
  rct_data<- rct_data %>%
    filter(Trial_ID %in% id)
  
  rct_data<- rct_data %>%
    filter(stats_name %in% c(dat$treat1,dat$treat2))
  
  
  
  comp=unique(dat$comp.num)
  ####################################################
  
  
  
  pair_data_06_07=pairwise_old(studlab=Trial_ID,treat=Treat_Name,event=NEG_conv_n_D7,n=NEG_conv_denominator_d7,
                           data=rct_data,measure="RR",ref="Standard care")
  pair_data_06_07$F_U_days1=pair_data_06_07$F_U_days
  pair_data_06_07$First_author=pair_data_06_07$First_author
  
  pair_data_06_07$Treatment_control1=ifelse(is.null(pair_data_06_07$Treatment_control1),pair_data_06_07$Treatment_control,pair_data_06_07$Treatment_control1)
  
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
  
  
  
  
  sev1=rep(0,ncomp)
  sev1=ifelse(pair_data_06_07$Research_question=="Mild",1,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Mild outpatients",1,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Outpatients",1,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Mild/moderate",2,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Moderate",3,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Mild to severe",4,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Moderate/severe",5,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Mild to critical",6,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Moderate to severe",7,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Moderate to critical",8,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Severe",9,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Severe/critical",10,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Unclear severity",11,sev1)
  sev1=ifelse(pair_data_06_07$Research_question=="Critical",12,sev1)
  
  
  
  
  
  sev=rep(0,ncomp)
  if(sub=="Severity"){
    #codes for severity#
sev=sev1
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
    # sev=ifelse(pair_data_06_07$Funding=="Not reported/unclear",11,sev)
    # sev=ifelse(pair_data_06_07$Funding=="No specific funding",12,sev)
  }
  if(sub=="Separate Standard care/Placebo"){
    sev=ifelse(pair_data_06_07$treat2=="Standard care",1,sev)
    sev=ifelse(pair_data_06_07$treat2=="Placebo",2,sev)
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
  pair_data_06_07$sev1=sev1
  pair_data_06_07$subgroup=subgroup
  
  
  new_data_06_07=subset(pair_data_06_07, comp>0)
  
  if(is.null(new_data_06_07$Treat_Dose_desc)){
    new_data_06_07$Treat_Dose_desc=new_data_06_07$Treat_Dose_desc1
  }else{
    new_data_06_07$Treat_Dose_desc=new_data_06_07$Treat_Dose_desc
  }
  
  analysis_data_06_07=cbind.data.frame(Trial_ID=new_data_06_07$Trial_ID,First_author=new_data_06_07$First_author,
                            F_U_days=new_data_06_07$F_U_days,Intervention1=new_data_06_07$Intervention1,
                            Intervention2=new_data_06_07$Intervention2,r1=new_data_06_07$r1,N1=new_data_06_07$N1,
                            r2=new_data_06_07$r2,N2=new_data_06_07$N2,Comparison=new_data_06_07$Comparison,
                            severity=new_data_06_07$Research_question,comp=new_data_06_07$comp,
                            sev=new_data_06_07$sev,sev1=new_data_06_07$sev1,subgroup=new_data_06_07$subgroup,Year=new_data_06_07$Year
                            ,dose=as.character(new_data_06_07$Treat_Dose_desc),
                            D1=as.character(new_data_06_07$ROB_1_randomization),
                            D2=as.character(new_data_06_07$ROB2_Neg_conv),
                            D3=as.character(new_data_06_07$ROB_3__Viral_Neg_Conv_events),
                            D4=as.character(new_data_06_07$ROB_4__Viral_Neg_Conv_events),
                            D5=as.character(new_data_06_07$ROB_5__Viral_Neg_Conv_events),
                            Overall=as.character(new_data_06_07$ROB_6__Viral_Neg_Conv_events),
                            Conflicts_of_interest=new_data_06_07$Conflict_of_interest_category,
                            Funding=new_data_06_07$Funding,
                            Countries=new_data_06_07$Countries,
                            Publication=new_data_06_07$Type_publication,
                            RR=trimws(format(round(exp(new_data_06_07$TE),digits = 2),nsmall=2)),
                            seTE=new_data_06_07$seTE,
                            L=trimws(format(round(exp(new_data_06_07$TE-1.96*new_data_06_07$seTE),digits = 2),nsmall=2)),
                            U=trimws(format(round(exp(new_data_06_07$TE+1.96*new_data_06_07$seTE),digits = 2),nsmall=2))
                            )
  analysis_data_06_07=as.data.frame(analysis_data_06_07)
  
  analysis_data_06_07$RR=ifelse(analysis_data_06_07$RR=="NA",NA,analysis_data_06_07$RR)
  analysis_data_06_07$L=ifelse(analysis_data_06_07$L=="NA",NA,analysis_data_06_07$L)
  analysis_data_06_07$U=ifelse(analysis_data_06_07$U=="NA",NA,analysis_data_06_07$U)
  
  interval=paste("[",analysis_data_06_07$L,", ",analysis_data_06_07$U,"]",sep="")
  analysis_data_06_07$ress=paste(analysis_data_06_07$RR,interval,sep=" ")
  analysis_data_06_07$ress=ifelse(analysis_data_06_07$ress=="NA [NA, NA]","",analysis_data_06_07$ress)
  
  
  analysis_data_06_07$f1=analysis_data_06_07$N1-analysis_data_06_07$r1
  analysis_data_06_07$f2=analysis_data_06_07$N2-analysis_data_06_07$r2
  

  
  analysis_data_06_07$counts1=str_c(analysis_data_06_07$r1, analysis_data_06_07$N1, sep = "/", collapse = NULL)
  analysis_data_06_07$counts2=str_c(analysis_data_06_07$r2, analysis_data_06_07$N2, sep = "/", collapse = NULL)
  
  
  analysis_data_06_07$status=ifelse(analysis_data_06_07$r1+analysis_data_06_07$r2==0,"events=0","events>0")
  analysis_data_06_07$status=ifelse(analysis_data_06_07$f1+analysis_data_06_07$f2==0,"events=0",analysis_data_06_07$status)
  
  
  if(keep_only=="All population"){
    analysis_data_06_07=analysis_data_06_07
  }
  
  if(keep_only=="Mild population"){
    data_sev<- analysis_data_06_07 %>%
      filter(analysis_data_06_07$sev1==1)
    analysis_data_06_07=data_sev
  }
  
  if(keep_only=="Mixed population"){
    
    data_sev<-analysis_data_06_07 %>%
      filter(analysis_data_06_07$sev1 %in% c(2:11))
    
    analysis_data_06_07=data_sev
  }
  
  if(keep_only=="Critical population"){
    
    data_sev<-analysis_data_06_07 %>%
      filter(analysis_data_06_07$sev1==12)
    
    analysis_data_06_07=data_sev
  }
  
  
  exclude_preprint<- analysis_data_06_07 %>%
    filter(analysis_data_06_07$Publication=="published paper")
  
  
  
  if(Publication==TRUE){
    analysis_data_06_07<-exclude_preprint}
  
  
  if(Publication==FALSE){
    analysis_data_06_07<-analysis_data_06_07}
  
  comp.num=comp.num
  
  analysis_data_06_07= analysis_data_06_07 %>%
    filter(analysis_data_06_07$comp==comp.num)
  
  analysis_data_06_07=analysis_data_06_07[complete.cases(analysis_data_06_07$counts1), ]
  
  analysis_data_06_07=as.data.frame(analysis_data_06_07)
  
  if(nrow(analysis_data_06_07)>0){
  
  ################################################################
  ################################################################
    exclude_rob<- analysis_data_06_07 %>%
      filter(analysis_data_06_07$Overall=="High")
    
    exclude_rob1<- analysis_data_06_07 %>%
      filter(analysis_data_06_07$Overall %in% c("High","Some concerns") )
    
    w=setdiff(analysis_data_06_07$First_author,exclude_rob$First_author)
    
    w1=setdiff(analysis_data_06_07$First_author,exclude_rob1$First_author)
    
    if(high_RoB=="High RoB"){
      analysis_data_06_07<-subset(analysis_data_06_07,analysis_data_06_07$First_author %in% w)}
    
    
    if(high_RoB=="High RoB/Some concerns"){
      analysis_data_06_07<-subset(analysis_data_06_07,analysis_data_06_07$First_author %in% w1)}
    
    
    if(high_RoB=="No exclusion"){
      analysis_data_06_07<-analysis_data_06_07}
  
  if(high_RoB==TRUE){
    analysis_data_06_07<-subset(analysis_data_06_07,analysis_data_06_07$First_author %in% w)}
  
  if(high_RoB==FALSE){
    analysis_data_06_07=analysis_data_06_07}
  
  #################################################################
  #################################################################
  
  analysis_data_06_07=analysis_data_06_07[order(analysis_data_06_07$sev,decreasing=TRUE),]
  
  ilab=cbind(F_U_days=as.character(analysis_data_06_07$F_U_days),Intervention1=as.character(analysis_data_06_07$Intervention1),
             Intervention2=as.character(analysis_data_06_07$Intervention2),
             counts1=analysis_data_06_07$counts1,counts2=analysis_data_06_07$counts2,analysis_data_06_07$ress)
  ilab=as.data.frame(ilab)
  
  ########################################################################
  analysis_data_06_07_0<-subset(analysis_data_06_07,analysis_data_06_07$comp==comp.num & analysis_data_06_07$subgroup==0)
  analysis_data_06_07_1<-subset(analysis_data_06_07,analysis_data_06_07$comp==comp.num & analysis_data_06_07$subgroup==1)
  analysis_data_06_07_2<-subset(analysis_data_06_07,analysis_data_06_07$comp==comp.num & analysis_data_06_07$subgroup==2)
  ##########################################################################
  
  result_06_07=rma(measure="RR",ai=analysis_data_06_07$r1,bi=analysis_data_06_07$f1,ci=analysis_data_06_07$r2,
                   di=analysis_data_06_07$f2,slab=paste(analysis_data_06_07$First_author,analysis_data_06_07$Year,sep=", "),
                   subset=(analysis_data_06_07$comp==comp.num),add=1/2, to="only0",drop00=FALSE,method = het_estimate)
  
  if(model=="Fixed-effects"){
    result_06_07=update(result_06_07,method = "FE")
    
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
                        n.c = analysis_data_06_07$N2,sm="RR",byvar = analysis_data_06_07$subgroup,allstudies = T,method.tau = het_estimate)
  

  
  ks0=0
  ks1=0
  ks2=0
  
  
  if (result_06_07$k>1 & nrow(analysis_data_06_07_0)>0){
    result_06_07.0=rma(measure="RR",ai=analysis_data_06_07_0$r1,bi=analysis_data_06_07_0$f1,ci=analysis_data_06_07_0$r2,
                       di=analysis_data_06_07_0$f2,slab=paste(analysis_data_06_07_0$First_author,analysis_data_06_07_0$Year,sep=", ")
                       ,add=1/2, 
                       to="only0",drop00=FALSE,method = het_estimate)
    
    if(model=="Fixed-effects"){
      result_06_07.0=update(result_06_07.0,method = "FE")
    }
    
    pred_06_07.0<- predict(result_06_07.0, transf=exp, digits=2)
    pred_06_07.0 <- formatC(c(pred_06_07.0$pred, pred_06_07.0$ci.lb, pred_06_07.0$ci.ub), format="f", digits=2)
    
    
    ks0=result_06_07.0$k
    
    if(ks0>=1 & sum(analysis_data_06_07_0$r1)>0 | sum(analysis_data_06_07_0$r2)>0){
      
      if(sum(analysis_data_06_07_0$f1)>0 | sum(analysis_data_06_07_0$f2)>0){
        result_06_07.0eff=rma(measure="RR",ai=analysis_data_06_07_0$r1,bi=analysis_data_06_07_0$f1,ci=analysis_data_06_07_0$r2,
                              di=analysis_data_06_07_0$f2,slab=paste(analysis_data_06_07_0$First_author,analysis_data_06_07_0$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=TRUE,method = het_estimate)
        
        pred_06_07.0eff<- predict(result_06_07.0eff, transf=exp, digits=2)
        pred_06_07.0eff<- formatC(c(pred_06_07.0eff$pred, pred_06_07.0eff$ci.lb, pred_06_07.0eff$ci.ub), format="f", digits=2)
        
        
      }
      
      if(sum(analysis_data_06_07_0$f1)==0 & sum(analysis_data_06_07_0$f2)==0){
        result_06_07.0eff=rma(measure="RR",ai=analysis_data_06_07_0$r1,bi=analysis_data_06_07_0$f1,ci=analysis_data_06_07_0$r2,
                              di=analysis_data_06_07_0$f2,slab=paste(analysis_data_06_07_0$First_author,analysis_data_06_07_0$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=TRUE,method = het_estimate)
        
        pred_06_07.0eff<- predict(result_06_07.0eff, transf=exp, digits=2)
        pred_06_07.0eff<- formatC(c(pred_06_07.0eff$pred, pred_06_07.0eff$ci.lb, pred_06_07.0eff$ci.ub), format="f", digits=2)
        
        
      }
      
      if(model=="Fixed-effects"){
        result_06_07.0eff=update(result_06_07.0eff,method = "FE")
        pred_06_07.0eff<- predict(result_06_07.0eff, transf=exp, digits=2)
        pred_06_07.0eff<- formatC(c(pred_06_07.0eff$pred, pred_06_07.0eff$ci.lb, pred_06_07.0eff$ci.ub), format="f", digits=2)
        
      }
      
    }
    
    if (ks0>1 & sum(analysis_data_06_07_0$r1)==0 & sum(analysis_data_06_07_0$r2)==0){
      result_06_07.0eff=rma(measure="RR",ai=analysis_data_06_07_0$r1,bi=analysis_data_06_07_0$f1,ci=analysis_data_06_07_0$r2,
                            di=analysis_data_06_07_0$f2,slab=paste(analysis_data_06_07_0$First_author,analysis_data_06_07_0$Year,sep=", "),
                            add=1/2, 
                            to="only0",drop00=FALSE,method = het_estimate)
      
      if(model=="Fixed-effects"){
        result_06_07.0eff=update(result_06_07.0eff,method = "FE")}
      
      ks0=0
    }
    
    pred_06_07.0eff<- predict(result_06_07.0eff, transf=exp, digits=2)
    pred_06_07.0eff<- formatC(c(pred_06_07.0eff$pred, pred_06_07.0eff$ci.lb, pred_06_07.0eff$ci.ub), format="f", digits=2)
    
  }
  
  
  if (result_06_07$k>1 & nrow(analysis_data_06_07_1)>0){
    
    result_06_07.1=rma(measure="RR",ai=analysis_data_06_07_1$r1,bi=analysis_data_06_07_1$f1,ci=analysis_data_06_07_1$r2,
                       di=analysis_data_06_07_1$f2,slab=paste(analysis_data_06_07_1$First_author,analysis_data_06_07_1$Year,sep=", "),
                       add=1/2, 
                       to="only0",drop00=FALSE,method = het_estimate)
    
    if(model=="Fixed-effects"){
      result_06_07.1=update(result_06_07.1,method = "FE")
    }
    
    pred_06_07.1<- predict(result_06_07.1, transf=exp, digits=2)
    pred_06_07.1<- formatC(c(pred_06_07.1$pred, pred_06_07.1$ci.lb, pred_06_07.1$ci.ub), format="f", digits=2)
    
    ks1=result_06_07.1$k
    
    if(ks1>=1 & sum(analysis_data_06_07_1$r1)>0 | sum(analysis_data_06_07_1$r2)>0){
      
      if(sum(analysis_data_06_07_1$f1)>0 | sum(analysis_data_06_07_1$f2)>0){
        result_06_07.1eff=rma(measure="RR",ai=analysis_data_06_07_1$r1,bi=analysis_data_06_07_1$f1,ci=analysis_data_06_07_1$r2,
                              di=analysis_data_06_07_1$f2,slab=paste(analysis_data_06_07_1$First_author,analysis_data_06_07_1$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=TRUE,method = het_estimate)
      }
      if(ks1>=1 & sum(analysis_data_06_07_1$f1)==0 | sum(analysis_data_06_07_1$f2)==0){
        
        result_06_07.1eff=rma(measure="RR",ai=analysis_data_06_07_1$r1,bi=analysis_data_06_07_1$f1,ci=analysis_data_06_07_1$r2,
                              di=analysis_data_06_07_1$f2,slab=paste(analysis_data_06_07_1$First_author,analysis_data_06_07_1$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=FALSE,method = het_estimate) 
        ks1=0
      }
      
      if(model=="Fixed-effects"){
        
        result_06_07.1eff=update(result_06_07.1eff,method = "FE")
      }
      
      pred_06_07.1eff<- predict(result_06_07.1eff, transf=exp, digits=2)
      pred_06_07.1eff<- formatC(c(pred_06_07.1eff$pred, pred_06_07.1eff$ci.lb, pred_06_07.1eff$ci.ub), format="f", digits=2)
      
    }
    
    if(ks1>=1 & sum(analysis_data_06_07_1$r1)==0 & sum(analysis_data_06_07_1$r2)>0){
      
      result_06_07.1eff=rma(measure="RR",ai=analysis_data_06_07_1$r1,bi=analysis_data_06_07_1$f1,ci=analysis_data_06_07_1$r2,
                            di=analysis_data_06_07_1$f2,slab=paste(analysis_data_06_07_1$First_author,analysis_data_06_07_1$Year,sep=", "),
                            add=1/2, 
                            to="only0",drop00=FALSE,method = het_estimate) 
      
      
      
      if(model=="Fixed-effects"){
        
        result_06_07.1eff=update(result_06_07.1eff,method = "FE")
      }
      ks1=0
    }
    
    pred_06_07.1eff<- predict(result_06_07.1eff, transf=exp, digits=2)
    pred_06_07.1eff<- formatC(c(pred_06_07.1eff$pred, pred_06_07.1eff$ci.lb, pred_06_07.1eff$ci.ub), format="f", digits=2)
    
  }
  
  
  if (result_06_07$k>1 & nrow(analysis_data_06_07_2)>0){
    result_06_07.2=rma(measure="RR",ai=analysis_data_06_07_2$r1,bi=analysis_data_06_07_2$f1,ci=analysis_data_06_07_2$r2,
                       di=analysis_data_06_07_2$f2,slab=paste(analysis_data_06_07_2$First_author,analysis_data_06_07_2$Year,sep=", "),
                       add=1/2, 
                       to="only0",drop00=FALSE,method = het_estimate)
    if(model=="Fixed-effects"){
      result_06_07.2=update(result_06_07.2,method = "FE")
    }
    
    pred_06_07.2<- predict(result_06_07.2, transf=exp, digits=2)
    pred_06_07.2<- formatC(c(pred_06_07.2$pred, pred_06_07.2$ci.lb, pred_06_07.2$ci.ub), format="f", digits=2)
    
    ks2=result_06_07.2$k
    
    if(ks2>=1 & sum(analysis_data_06_07_2$r1)>0 | sum(analysis_data_06_07_2$r2)>0){
      
      if(sum(analysis_data_06_07_2$f1)>0 | sum(analysis_data_06_07_2$f2)>0){
        result_06_07.2eff=rma(measure="RR",ai=analysis_data_06_07_2$r1,bi=analysis_data_06_07_2$f1,ci=analysis_data_06_07_2$r2,
                              di=analysis_data_06_07_2$f2,slab=paste(analysis_data_06_07_2$First_author,analysis_data_06_07_2$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=TRUE,method = het_estimate)
      }
      
      if(sum(analysis_data_06_07_2$f1)==0 & sum(analysis_data_06_07_2$f2)==0){
        result_06_07.2eff=rma(measure="RR",ai=analysis_data_06_07_2$r1,bi=analysis_data_06_07_2$f1,ci=analysis_data_06_07_2$r2,
                              di=analysis_data_06_07_2$f2,slab=paste(analysis_data_06_07_2$First_author,analysis_data_06_07_2$Year,sep=", "),
                              add=1/2, 
                              to="only0",drop00=FALSE,method = het_estimate)
        
        
        if(model=="Fixed-effects"){
          result_06_07.2eff=update(result_06_07.2eff,method = "FE")
        }
        
        ks2=0
      }
      pred_06_07.2eff<- predict(result_06_07.2eff, transf=exp, digits=2)
      pred_06_07.2eff<- formatC(c(pred_06_07.2eff$pred, pred_06_07.2eff$ci.lb, pred_06_07.2eff$ci.ub), format="f", digits=2)
      
    }
    
    if(ks2>=1 & sum(analysis_data_06_07_2$r1)==0 | sum(analysis_data_06_07_2$r2)==0){
      
      result_06_07.2eff=rma(measure="RR",ai=analysis_data_06_07_2$r1,bi=analysis_data_06_07_2$f1,ci=analysis_data_06_07_2$r2,
                            di=analysis_data_06_07_2$f2,slab=paste(analysis_data_06_07_2$First_author,analysis_data_06_07_2$Year,sep=", "),
                            add=1/2, 
                            to="only0",drop00=FALSE,method = het_estimate)
      
      if(model=="Fixed-effects"){
        result_06_07.2eff=update(result_06_07.2eff,method = "FE")
      }
      ks2=0
      
    }
    
    pred_06_07.2<- predict(result_06_07.2, transf=exp, digits=2)
    pred_06_07.2<- formatC(c(pred_06_07.2$pred, pred_06_07.2$ci.lb, pred_06_07.2$ci.ub), format="f", digits=2)
  }
  
  k00=0
  if((sum(analysis_data_06_07$r1)>0 | sum(analysis_data_06_07$r2)>0)){
    
    if((sum(analysis_data_06_07$f1)>0 | sum(analysis_data_06_07$f2)>0)){
      
      
      result_06_07.3=rma(measure="RR",ai=analysis_data_06_07$r1,bi=analysis_data_06_07$f1,ci=analysis_data_06_07$r2,
                         di=analysis_data_06_07$f2,slab=paste(analysis_data_06_07$First_author,analysis_data_06_07$Year,sep=", "),
                         subset=(analysis_data_06_07$comp==comp.num),add=1/2, 
                         to="only0",drop00=TRUE,method = het_estimate)
      
      
      if(model=="Fixed-effects"){
        result_06_07.3=update(result_06_07.3,method = "FE")}
      
      
      k00=result_06_07.3$k}
    
    if((sum(analysis_data_06_07$f1)==0 & sum(analysis_data_06_07$f2)==0)){
      
      result_06_07.3=rma(measure="RR",ai=analysis_data_06_07$r1,bi=analysis_data_06_07$f1,ci=analysis_data_06_07$r2,
                         di=analysis_data_06_07$f2,slab=paste(analysis_data_06_07$First_author,analysis_data_06_07$Year,sep=", "),
                         subset=(analysis_data_06_07$comp==comp.num),add=1/2, 
                         to="only0",drop00=FALSE,method = het_estimate)
      
      
      if(model=="Fixed-effects"){
        result_06_07.3=update(result_06_07.3,method = "FE")
      }
      
      
      k00=0}
    
    pred_06_07.3<- predict(result_06_07.3, transf=exp, digits=2)
    pred_06_07.3<- formatC(c(pred_06_07.3$pred, pred_06_07.3$ci.lb, pred_06_07.3$ci.ub), format="f", digits=2)
  }
  
  
  if ((sum(analysis_data_06_07$r1)==0 & sum(analysis_data_06_07$r2)==0)
      
  ){
    
    if(model=="Random-effects"){
      result_06_07.3=rma(measure="RR",ai=analysis_data_06_07$r1,bi=analysis_data_06_07$f1,ci=analysis_data_06_07$r2,
                         di=analysis_data_06_07$f2,slab=paste(analysis_data_06_07$First_author,analysis_data_06_07$Year,sep=", "),
                         subset=(analysis_data_06_07$comp==comp.num),add=1/2, 
                         to="only0",drop00=FALSE,method = het_estimate)
    }
    if(model=="Fixed-effects"){
      result_06_07.3=update(result_06_07.3,method = "FE")}
    k00=0
    pred_06_07.3<- predict(result_06_07.3, transf=exp, digits=2)
    pred_06_07.3<- formatC(c(pred_06_07.3$pred, pred_06_07.3$ci.lb, pred_06_07.3$ci.ub), format="f", digits=2)
    
  }
  
  cex1=0.9
  
  par(cex=cex1)
  
  analysis_data_06_07$cols=ifelse(analysis_data_06_07$status=="events=0","white","black")
  
  data_rob=analysis_data_06_07 %>%
    select(First_author,D1,D2,D3,D4,D5,Overall)
  
  par(mar=c(11.5,0,3,1.5), mgp=c(3,0.2,0), tcl=-0.2)
  
  if(sub!="No subgroup analysis"){
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
  }else{
    allrows=c(1:result_06_07$k)   
  }
  
  
  if(k00==0 & result_06_07$k==1){
    if(sum(analysis_data_06_07$r1)>0 | sum(analysis_data_06_07$r2)>0){
      if(sum(analysis_data_06_07$f1)>0 | sum(analysis_data_06_07$f2)>0){
        forest=rob_append_to_forest(result_06_07,atransf=exp,showweights=F, xlab="Risk Ratio",cex=0.8,mlab="",
                                    col="white",border="white",xlim=c(-15,4),alim=c(-0.5,0.5),steps=4,
                                    ilab=ilab,ilab.xpos=c(-12.35,-11.2,-9.3,-8,-7,-5.1,-6.2),
                                    rows=allrows,ylim=c(-1.5,3+max(allrows)),refline=NA,annotate=FALSE,
                                    rob_data = data_rob ,at=log(c(0.1,1,5))
        )
      }
      
      
      if(sum(analysis_data_06_07$f1)==0 & sum(analysis_data_06_07$f2)==0){
        forest=rob_append_to_forest(result_06_07,atransf=exp,showweights=F, xlab="Risk Ratio",cex=0.8,mlab="",
                                    col="white",border="white",xlim=c(-15,4),
                                    #alim=c(-0.5,0.5),
                                    steps=4,
                                    ilab=ilab,ilab.xpos=c(-12.35,-11.2,-9.3,-8,-7,-5.1,-6.2),rows=allrows,ylim=c(-1.5,3+max(allrows)),refline=NA,annotate=FALSE,
                                    efac=c(0,0),psize=0,lty="blank",rob_data = data_rob,at=log(c(0.1,1,5))
        )
      }
      
    }
    
    if(sum(analysis_data_06_07$r1)==0 & sum(analysis_data_06_07$r2)==0){
      forest=rob_append_to_forest(result_06_07,atransf=exp,showweights=F, xlab="Risk Ratio",cex=0.8,mlab="",
                                  col="white",border="white",xlim=c(-15,4),
                                  #alim=c(-0.5,0.5),
                                  steps=4,
                                  ilab=ilab,ilab.xpos=c(-12.35,-11.2,-9.3,-8,-7,-5.1,-6.2),rows=allrows,ylim=c(-1.5,3+max(allrows)),refline=NA,annotate=FALSE,
                                  efac=c(0,0),psize=0,lty="blank",rob_data = data_rob,annotate=F,at=log(c(0.1,1,5)))  
      
    }
    
  }
  
  
  if(k00!=0 | result_06_07$k!=1){
    forest=rob_append_to_forest(result_06_07,atransf=exp,showweights=F, xlab="Risk Ratio",cex=0.8,mlab="",
                                col="white",border="white",xlim=c(-15,4),
                                #alim=c(floor(result_06_07$ci.lb)-2,ceiling(result_06_07$ci.ub))+1,
                                steps=4,
                                ilab=ilab,ilab.xpos=c(-12.35,-11.2,-9.3,-8,-7,-5.1,-6.2),rows=allrows,ylim=c(-1.5,3+max(allrows)),refline=NA,
                                rob_data = data_rob,annotate=F,at=log(c(0.1,1,5)))  
  }
  
  
  
  header=c("Follow up\ndays","Intervention 1","Intervention 2","r1/N1","r2/N2","Weights","RR [95% CI]")

  
  par(font=2)
  
  rob_top<-3
  
  
  text(c(-12.35,-11.2,-9.3,-8,-7,-6.2,-5.1),forest$ylim[2]-(rob_top-1) + 1,header,cex=cex1-0.1)
  
  par(font=1)
  
  
  
  legend(10, -0.8, "                                                                            ", box.col = "white", bg = "white", adj = 2.2)
  
  
  if(sub!="No subgroup analysis"){
  if(k00!=0 | result_06_07$k!=1){
    addpoly.rma(result_06_07.3,row=-1,mlab="",col="darkblue",border="darkblue",atransf=exp,cex=0.9,annotate = F)
    
    par(font=2)
    text(-5.1,-1,paste(pred_06_07.3[1], " [", pred_06_07.3[2], ",  ", pred_06_07.3[3], "]",sep = ""),cex = cex1)
    par(font=1)
  }
  
  
  if(k00==0 & result_06_07$k==1){
    addpoly.rma(result_06_07.3,row=-1,mlab="",col="white",border="white",annotate=FALSE)
  }
  if (result_06_07$k>1){
    if (ks0>0 & ks1>0 & ks2==0)
    {
      if(sub=="Severity"){
        
        addpoly.rma(result_06_07.1eff,row=1,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.0eff,row=ks1+2,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      if(sub=="Conflicts of interest"){
        addpoly.rma(result_06_07.1eff,row=1,mlab="                    Studies with conflicts of interest",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.0eff,row=ks1+2,mlab="                    Studies without conflicts of interest",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      
      if(sub=="Funding"){
        addpoly.rma(result_06_07.1eff,row=1,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.0eff,row=ks1+2,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      if(ks0>0 & ks1>0 & ks2==0 & sub=="Location"){
        addpoly.rma(result_06_07.1eff,row=1,mlab="                    National studies",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.0eff,row=ks1+2,mlab="                    Multinational studies",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        
      }
      
      if(sub=="Separate Standard care/Placebo"){
        addpoly.rma(result_06_07.1eff,row=1,mlab="                    Placebo summary result",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.0eff,row=ks1+1.8,mlab="                    Standard care summary result",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        
      }
      
      par(font=2)
      text(-5.1,1,paste(pred_06_07.1eff[1], " [", pred_06_07.1eff[2], ",  ",pred_06_07.1eff[3], "]",sep = ""),cex = cex1)
      text(-5.1,ks1+1.8,paste(pred_06_07.0eff[1], " [", pred_06_07.0eff[2], ",  ", pred_06_07.0eff[3], "]",sep = ""),cex = cex1)
      par(font=1)
      
    }
    
    
    if (ks0>0 & ks1==0 & ks2>0){
      if(sub=="Severity"){
        addpoly.rma(result_06_07.2eff,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.0eff,row=ks2+2,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      
      if(sub=="Funding"){
        addpoly.rma(result_06_07.1eff,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.0eff,row=ks1+2,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      text(-5.1,1,paste(pred_06_07.2eff[1], " [", pred_06_07.2eff[2], ",  ", pred_06_07.2eff[3], "]",sep = ""),cex = cex1)
      text(-5.1,ks2+2,paste(pred_06_07.0eff[1], " [", pred_06_07.0eff[2], ",  ", pred_06_07.0eff[3], "]",sep = ""),cex = cex1)
      
    }
    
    
    if (ks0==0 & ks1>0 & ks2>0){
      if(sub=="Severity"){
        addpoly.rma(result_06_07.2eff,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.1eff,row=ks2+2,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      if(sub=="Funding"){
        addpoly.rma(result_06_07.2eff,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.1eff,row=ks2+2,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      text(-5.1,1,paste(pred_06_07.2eff[1], " [", pred_06_07.2eff[2], ",  ", pred_06_07.2eff[3], "]",sep = ""),cex = cex1)
      text(-5.1,ks2+2,paste(pred_06_07.1eff[1], " [", pred_06_07.1eff[2], ",  ", pred_06_07.1eff[3], "]",sep = ""),cex = cex1)
      
    }
    if (ks0>0 & ks1>0 & ks2>0){
      if(sub=="Severity"){
        addpoly.rma(result_06_07.2eff,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.1eff,row=ks2+2,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.0eff,row=ks2+4+ks1,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)}
      
      if(sub=="Funding"){
        addpoly.rma(result_06_07.2eff,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.1eff,row=ks2+2,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_06_07.0eff,row=ks2+4+ks1,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        
      }
      
      text(-5.1,1,paste(pred_06_07.2eff[1], " [", pred_06_07.2eff[2], ",  ", pred_06_07.2eff[3], "]",sep = ""),cex = cex1)
      text(-5.1,ks2+2,paste(pred_06_07.1eff[1], " [", pred_06_07.0eff[2], ",  ", pred_06_07.0eff[3], "]",sep = ""),cex = cex1)
      text(-5.1,ks2+4,paste(pred_06_07.0eff[1], " [", pred_06_07.0eff[2], ",  ", pred_06_07.0eff[3], "]",sep = ""),cex = cex1)
      
    }
    
    
  }
  }else{
    if(k00!=0 | result_06_07$k!=1){
      addpoly.rma(result_06_07.3,row=-1,mlab="",col="darkblue",border="darkblue",atransf=exp,cex=0.9,annotate = F)
      
      par(font=2)
      text(-5.1,-1,paste(pred_06_07.3[1], " [", pred_06_07.3[2], ",  ", pred_06_07.3[3], "]",sep = ""),cex = cex1)
      par(font=1)
    }
    
    
    if(k00==0 & result_06_07$k==1){
      addpoly.rma(result_06_07.3,row=-1,mlab="",col="white",border="white",annotate=FALSE)
    }
    
    
  }
  
  par(font=1)  
  par(xpd=TRUE)
  if(nrow(analysis_data_06_07)>8){
    legend(-15,-5,adj=c(0, 0.5),legend=c("Low Risk of Bias","Some Concerns","High Risk of Bias"),ncol=1,
           col=c("chartreuse3","gold","red"),fill=c("chartreuse3","gold","red")
           ,title = "Risk of bias ratings:",border = c("chartreuse3","gold","red"), cex=0.6
    )
    legend(-13, -5,legend=c("A: Bias due to randomization","B: Bias due to deviation from intended intervention",
                            "C: Bias due to missing data","D: Bias due to outcome measurement","E: Bias due to selection of reported result"),
           ncol=1,cex=0.6,title = "Risk of Bias Domains:",box.lty = 2)
    
  }else{
    legend(-15,-2.5,adj=c(0, 0.5),legend=c("Low Risk of Bias","Some Concerns","High Risk of Bias"),ncol=1,
           col=c("chartreuse3","gold","red"),fill=c("chartreuse3","gold","red")
           ,title = "Risk of bias ratings:",border = c("chartreuse3","gold","red"), cex=0.6
    )
    legend(-13, -2.5,legend=c("A: Bias due to randomization","B: Bias due to deviation from intended intervention",
                              "C: Bias due to missing data","D: Bias due to outcome measurement","E: Bias due to selection of reported result"),
           ncol=1,cex=0.6,title = "Risk of Bias Domains:",box.lty = 2)
    
    
  }
  
  
  if(result_06_07$k>8){
    text(-6.2,3.9+max(allrows),"Incidence of viral negative conversion D7",
         pos=4,
         cex=0.95)
  }
  
  if(result_06_07$k<8 | result_06_07$k==8){
    text(-6.2,3.3+max(allrows),"Incidence of viral negative conversion D7",
         pos=4,
         cex=0.95)
  }
  
  if(den!="Randomized"){
    par(font=2)
    text(7.8, -0.25, "Risk of bias is assessed only for randomized patients",cex=0.6)
    par(font=1)
  }
  
  if(hide_dose=="FALSE"){
    if(comp.num==comp.num){
      if ((ks0==0 & ks2==0) | (ks0==0 & ks1==0) | (ks1==0 & ks2==0)){
        for(i in 1:result_06_07$k){
          text(-11.5, -0.3+i+sliderDose, pos=4, analysis_data_06_07$dose[i],cex=cex1-0.3)
        }
      }
      if (ks0>0 & ks1>0 & ks2==0){
        for(i in 1:ks1){
          text(-11.5, -0.3+i+1+sliderDose, pos=4, analysis_data_06_07$dose[i],cex=cex1-0.3)
        }
        for(i in (ks1+1):(ks0+ks1)){
          text(-11.5, -0.3+i+2+sliderDose, pos=4, analysis_data_06_07$dose[i],cex=cex1-0.3)
        }
      }
      if (ks0>0 & ks1==0 & ks2>0){
        for(i in 1:ks2){
          text(-11.5, -0.3+i+1+sliderDose, pos=4, analysis_data_06_07$dose[i],cex=cex1-0.2)
        }
        for(i in (ks2+1):(ks0+ks2)){
          text(-11.5, -0.3+i+2+sliderDose, pos=4, analysis_data_06_07$dose[i],cex=cex1-0.2)
        }  
      }
      if (ks0==0 & ks1>0 & ks2>0){
        for(i in 1:ks2){
          text(-11.5, -0.3+i+1+sliderDose, pos=4, analysis_data_06_07$dose[i],cex=cex1-0.2)
        }
        for(i in (ks2+1):(ks1+ks2)){
          text(-11.5, -0.3+i+2+sliderDose, pos=4, analysis_data_06_07$dose[i],cex=cex1-0.2)
        }    
      }
      if (ks0>0 & ks1>0 & ks2>0){
        for(i in 1:ks2){
          text(-11.5, -0.3+i+1+sliderDose, pos=4, analysis_data_06_07$dose[i],cex=cex1-0.2)
        }
        for(i in (ks2+1):(ks1+ks2)){
          text(-11.5, -0.3+i+2+sliderDose, pos=4, analysis_data_06_07$dose[i],cex=cex1-0.2)
        }  
        for(i in (ks1+ks2+1):(ks1+ks2+ks0)){
          text(-11.5, -0.3+i+4+sliderDose, pos=4, analysis_data_06_07$dose[i],cex=cex1-0.2)
        }
      }
    }
    
    
    count=sum(str_count(analysis_data_06_07$dose, "\\*"),na.rm = TRUE)
    
    if(is.na(count)==TRUE){
      count=0}
    
    if(count>0){
      text(-9.5, -0.2, "(*different loading dose)", pos=4, cex=0.6)}
    
    else if(count==0) {
      text(-9, -0.2, "", pos=4, cex=0.6)}
    
  }
  
  par(font=2,cex=0.7)
  if ((ks0==0 & ks2==0) | (ks0==0 & ks1==0) | (ks1==0 & ks2==0)){
    for(i in 1:result_06_07$k){
      if(hide_severity=="FALSE"){
        text(-15, 0.5+i, pos=4, analysis_data_06_07$severity[i],cex=cex1-0.1)}
      
      if(analysis_data_06_07$r1[i]==0 & analysis_data_06_07$r2[i]==0){
        legend(-2.3, 0.2+i, 
               "                                                                                                                                                               ",
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
      if(analysis_data_06_07$f1[i]==0 & analysis_data_06_07$f2[i]==0){
        legend(-2.3, 0.2+i, 
               "                                                                                                                                                               ",
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
    }
  }
  if (ks0>0 & ks1>0 & ks2==0){
    for(i in 1:ks1){
      if(hide_severity=="FALSE"){
        text(-15, 0.5+i+1, pos=4, analysis_data_06_07$severity[i],cex=cex1-0.1)}
      
      if(analysis_data_06_07$r1[i]==0 & analysis_data_06_07$r2[i]==0){
        legend(-2.3, 0.2+i+1.5,
               "                                                                                                                                                               ",
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
      if(analysis_data_06_07$f1[i]==0 & analysis_data_06_07$f2[i]==0){
        legend(-3, 0.2+i+1.5, 
               "                                                                                                                                                               ",               
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
    }
    for(i in (ks1+1):(ks0+ks1)){
      if(hide_severity=="FALSE"){
        text(-15, 0.5+i+2, pos=4, analysis_data_06_07$severity[i],cex=cex1-0.1)}
      
      if(analysis_data_06_07$r1[i]==0 & analysis_data_06_07$r2[i]==0){
        legend(-2.3, 0.2+i+2.5, 
               "                                                                                                                                                               ",               
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
      
      if(analysis_data_06_07$f1[i]==0 & analysis_data_06_07$f2[i]==0){
        legend(-2.3, 0.2+i+2.5,
               "                                                                                                                                                                                  ", 
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
    }
  }
  if (ks0>0 & ks1==0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
        text(-15, 0.5+i+1, pos=4, analysis_data_06_07$severity[i],cex=cex1-0.1)}
      
      if(analysis_data_06_07$r1[i]==0 & analysis_data_06_07$r2[i]==0){
        legend(-2.3, 0.2+i+1.5, 
               "                                                                                                                                                               ",              
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
      if(analysis_data_06_07$f1[i]==0 & analysis_data_06_07$f2[i]==0){
        legend(-2.3, 0.2+i+1.5,
               "                                                                                                                                                               ",               
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
    }
    for(i in (ks2+1):(ks0+ks2)){
      if(hide_severity=="FALSE"){
        text(-15, 0.5+i+2, pos=4, analysis_data_06_07$severity[i],cex=cex1-0.1)}
      
      if(analysis_data_06_07$r1[i]==0 & analysis_data_06_07$r2[i]==0){
        legend(-2.3, 0.2+i+2.5,
               "                                                                                                                                                               ",               
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
      
      if(analysis_data_06_07$f1[i]==0 & analysis_data_06_07$f2[i]==0){
        legend(-2.3, 0.2+i+2.5,
               "                                                                                                                                                               ",               
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
    }  
  }
  if (ks0==0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
        text(-15, 0.5+i+1, pos=4, analysis_data_06_07$severity[i],cex=cex1-0.1)}
      
      if(analysis_data_06_07$r1[i]==0 & analysis_data_06_07$r2[i]==0){
        legend(-2.3, 0.2+i+1.5, 
               "                                                                                                                                                               ",               
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
      
      if(analysis_data_06_07$f1[i]==0 & analysis_data_06_07$f2[i]==0){
        legend(-2.3, 0.2+i+1.5, 
               "                                                                                                                                                               ",               
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
    }
    for(i in (ks2+1):(ks1+ks2)){
      if(hide_severity=="FALSE"){
        text(-15, 0.5+i+2, pos=4, analysis_data_06_07$severity[i],cex=cex1-0.1)}
      
      if(analysis_data_06_07$r1[i]==0 & analysis_data_06_07$r2[i]==0){
        legend(-2.3, 0.2+i+2.5,
               "                                                                                                                                                               ",
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
      
      if(analysis_data_06_07$f1[i]==0 & analysis_data_06_07$f2[i]==0){
        legend(-2.3, 0.2+i+2.5, 
               "                                                                                                                                                               ",
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
    }    
  }
  if (ks0>0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
        text(-15, 0.5+i+1, pos=4, analysis_data_06_07$severity[i],cex=cex1-0.1)}
      
      if(analysis_data_06_07$r1[i]==0 & analysis_data_06_07$r2[i]==0){
        legend(-3, 0.2+i+1.5, 
               "                                                                                                                                                                                  ", 
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
      if(analysis_data_06_07$f1[i]==0 & analysis_data_06_07$f2[i]==0){
        legend(-3, 0.2+i+1.5,
               "                                                                                                                                                                                  ", 
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
    }
    for(i in (ks2+1):(ks1+ks2)){
      if(hide_severity=="FALSE"){
        text(-15, 0.5+i+2, pos=4, analysis_data_06_07$severity[i],cex=cex1-0.1)}
      
      if(analysis_data_06_07$r1[i]==0 & analysis_data_06_07$r2[i]==0){
        legend(-3, 0.2+i+2.5,
               "                                                                                                                                                                                  ", 
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
      if(analysis_data_06_07$f1[i]==0 & analysis_data_06_07$f2[i]==0){
        legend(-3, 0.2+i+2.5,
               "                                                                                                                                                                                  ", 
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
    }  
    for(i in (ks1+ks2+1):(ks1+ks2+ks0)){
      if(hide_severity=="FALSE"){
        text(-15, 0.5+i+4, pos=4, analysis_data_06_07$severity[i],cex=cex1-0.1)}
      
      if(analysis_data_06_07$r1[i]==0 & analysis_data_06_07$r2[i]==0){
        legend(-3, 0.2+i+3.5, 
               "                                                                                                                                                                                  ", 
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
      
      if(analysis_data_06_07$f1[i]==0 & analysis_data_06_07$f2[i]==0){
        legend(-3, 0.2+i+3.5, 
               "                                                                                                                                                                                  ", 
               box.col = "white", bg = "white", adj = 0.5,cex = 0.5)
      }
    }
  }
  
  if ((ks0==0) & (ks1==0 ) & (ks2==0)){
    for(i in 1:result_06_07$k){
      if(hide_severity=="FALSE"){
        text(-15, 0.5+i, pos=4, analysis_data_06_07$severity[i],cex=cex1-0.1)
      }
      if(analysis_data_06_07$status[i]=="events=0"){
        legend(-2.5, 0.18+i,"                                                                                                                                                                                  ", 
               box.col = "white", bg = "white", adj = 0.5,cex=0.5)
      }
    }
  }
  
  
  
  lines(c(0,0),c(-2,(max(allrows)+1)),lty=3)
  
  text(-2.5,-1.4,"Intervention 2 better",
       pos=4,
       cex=cex1-0.1)
  text(0.5,-1.4,"Intervention 1 better",
       pos=4,
       cex=cex1-0.1)
  
  
  today <- Sys.Date()
  today=format(today, format="%m %d %Y")
  
  
  a1=paste("Forest plot produced at:"," ",today,sep="")
  a2=paste("Data source: the COVID-NMA initiative (covid-nma.com)")
  
  
  
  af=paste(a1,a2,sep ="\n")
  if(nrow(analysis_data_06_07)>8.5){
    legend(0.2,-6,legend=af,
           ncol=1,cex=0.75,box.lty = 0,text.col = "blue")
    
    
  }else{
    legend(0.2,-2.5,legend=af,
           ncol=1,cex=0.75,box.lty = 0,text.col = "blue")}
   
  par(font=2)
  # totals=analysis_data_06_07 %>% 
  #   filter(status=="events>0")
  
  totals=analysis_data_06_07
  
  N1_total=sum(totals$N1)
  N2_total=sum(totals$N2)
  r1_total=sum(totals$r1)
  r2_total=sum(totals$r2)
  
  I1=paste(r1_total,N1_total,sep = "/")
  I2=paste(r2_total,N2_total,sep = "/")
  
  if(nrow(totals)>1){
    text(-9.7,-1,pos = 4,cex = cex1,"Totals")
    text(-8.4,-1,pos = 4,cex=cex1,I1)
    text(-7.4,-1,pos = 4,cex = cex1,I2)
  }
  
  par(font=1)
   
  
  if(result_06_07$k>8.5){
    if(length(unique(analysis_data_06_07$subgroup))==1){
      par(font=2)
      text(-15,-1.8 , pos=4, cex=cex1, bquote(paste("Heterogeneity results: Q = ",
                                                    .(formatC(result_06_07$QE, digits=2, format="f")),
                                                    ", p = ", .(formatC(result_06_07$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                    .(formatC(result_06_07$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                    .(formatC(result_06_07$tau2, digits=2, format="f")))))  
    }
    if(length(unique(analysis_data_06_07$subgroup))>1){
      par(font=2)
      text(-15, -1.8, pos=4, cex=cex1, bquote(paste("Heterogeneity results (overall analysis): Q = ",
                                                    .(formatC(result_06_07$QE, digits=2, format="f")),
                                                    ", p = ", .(formatC(result_06_07$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                    .(formatC(result_06_07$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                    .(formatC(result_06_07$tau2, digits=2, format="f"))))) 
      
      text(-15, -2.8, pos=4, cex=cex1, bquote(paste("Test for subgroup differences: Q = ",
                                                    .(formatC(result_06_071$Q.b.random, digits=2, format="f")),"; ", "p = ",
                                                    .(formatC(result_06_071$pval.Q.b.random, digits=2, format="f"))
                                                    
      )))
      
      
    }
    
  }
  
  
  if((result_06_07$k>1) & (result_06_07$k<9)){
    if(length(unique(analysis_data_06_07$subgroup))==1){
      par(font=2)
      text(-15,-1.8 , pos=4, cex=cex1, bquote(paste("Heterogeneity results: Q = ",
                                                    .(formatC(result_06_07$QE, digits=2, format="f")),
                                                    ", p = ", .(formatC(result_06_07$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                    .(formatC(result_06_07$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                    .(formatC(result_06_07$tau2, digits=2, format="f")))))  
    }
    if(length(unique(analysis_data_06_07$subgroup))>1){
      par(font=2)
      text(-15, -1.8, pos=4, cex=cex1, bquote(paste("Heterogeneity results (overall analysis): Q = ",
                                                    .(formatC(result_06_07$QE, digits=2, format="f")),
                                                    ", p = ", .(formatC(result_06_07$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                    .(formatC(result_06_07$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                    .(formatC(result_06_07$tau2, digits=2, format="f"))))) 
      
      text(-15, -2.3, pos=4, cex=cex1, bquote(paste("Test for subgroup differences: Q = ",
                                                  .(formatC(result_06_071$Q.b.random, digits=2, format="f")),"; ", "p = ",
                                                  .(formatC(result_06_071$pval.Q.b.random, digits=2, format="f"))
                                                  
      )))
      
      
    }
    
  }
  }
  
  if (nrow(analysis_data_06_07)==0){
    stop("No data available. Please, make another choice.")
  }
  #dev.off()
  }

  
  