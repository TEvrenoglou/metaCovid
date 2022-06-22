forest_Time_to_death=function(data,dat,comp.num,sliderRob=0,sliderDose=0,high_RoB=FALSE,keep_only="All population",model="Random-effects",
                              sub="Severity",den="Randomized",hide_dose=FALSE,hide_severity=FALSE,Publication=FALSE,het_estimate="REML"){
  
  
  data=data[,c("Trial_ID","ID_stats","name_study","First_author","Year","Study_design","Funding","Conflict_of_interest_category",
               "Research_question","Countries","F_U_days","Treatment_control","Treat_Name","stats_name","Treat_desc_summary",
               "n_Randomized","n_Analyzed_",
               "Clinical_improvement_D7_n",
               "time_to_death_HR","time_to_death_lci","time_to_death_uci",
               "ROB_1_randomization","ROB2_Time_death","ROB_2_Deviations_from_intervention","ROB_3_time_to_death",
               "ROB_4_mortality_OR_time_to_death","ROB_5_time_to_death","ROB_6_time_to_death","Type_publication"
               
  )]
  
  
  data$F_U_days=as.numeric(data$F_U_days)
  data$n_Randomized=as.numeric(data$n_Randomized)

  data$n_Randomized=ifelse(is.na(data$n_Randomized),data$n_Analyzed_,data$n_Randomized)
  
  data$n_Analyzed_=ifelse(is.na(data$n_Analyzed_),data$n_Randomized,data$n_Analyzed_)
  
  data$ROB2_Time_death=ifelse(is.na(data$ROB2_Time_death),data$ROB_2_Deviations_from_intervention,data$ROB2_Time_death)
  
  data$Type_publication=trimws(data$Type_publication)
  
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
  data$First_author=ifelse(is.na(data$First_author),data$name_study,data$First_author)

  data$Year=trimws(data$Year)
  data$Research_question=trimws(data$Research_question)
  data$Research_question=str_to_sentence(data$Research_question)
  
  data$Treat_Dose_desc=as.character(data$Treat_desc_summary)
  
  
  rct_data=subset(data,data$Study_design=="RCT" & Trial_ID!=9 & Trial_ID!=139 & Trial_ID!=232)
  
  rct_data$Clinical_improvement_D7_n=as.numeric(rct_data$Clinical_improvement_D7_n)
  
  
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
  
  
  pair_data=pairwise_old(studlab=Trial_ID,treat=Treat_Name,event=Clinical_improvement_D7_n,n=n_Randomized,
                     data=rct_data,measure="RR",ref="Standard care")
  pair_data$F_U_days1=pair_data$F_U_days
  pair_data$First_author=pair_data$First_author
  
  ncomp=length(pair_data$treat1) 
  
  
  if(
    (is.null(pair_data$time_to_death_HR)) & (is.null(pair_data$time_to_death_lci)) &
    (is.null(pair_data$time_to_death_uci))
  ){
    
    pair_data$time_to_death_HR=pair_data$time_to_death_HR1
    pair_data$time_to_death_lci=pair_data$time_to_death_lci1
    pair_data$time_to_death_uci=pair_data$time_to_death_uci1
  }
  
  
  
  pair_data$time_to_death_HR=as.numeric(pair_data$time_to_death_HR)
  pair_data$time_to_death_lci=as.numeric(pair_data$time_to_death_lci)
  pair_data$time_to_death_uci=as.numeric(pair_data$time_to_death_uci)
  
  
  pair_data$Treatment_control1=ifelse(is.null(pair_data$Treatment_control1),pair_data$Treatment_control,pair_data$Treatment_control1)
  
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
  

  sev1=rep(0,ncomp)
  sev1=ifelse(pair_data$Research_question=="Mild",1,sev1)
  sev1=ifelse(pair_data$Research_question=="Mild outpatients",1,sev1)
  sev1=ifelse(pair_data$Research_question=="Outpatients",1,sev1)
  sev1=ifelse(pair_data$Research_question=="Mild/moderate",2,sev1)
  sev1=ifelse(pair_data$Research_question=="Moderate",3,sev1)
  sev1=ifelse(pair_data$Research_question=="Mild to severe",4,sev1)
  sev1=ifelse(pair_data$Research_question=="Moderate/severe",5,sev1)
  sev1=ifelse(pair_data$Research_question=="Mild to critical",6,sev1)
  sev1=ifelse(pair_data$Research_question=="Moderate to severe",7,sev1)
  sev1=ifelse(pair_data$Research_question=="Moderate to critical",8,sev1)
  sev1=ifelse(pair_data$Research_question=="Severe",9,sev1)
  sev1=ifelse(pair_data$Research_question=="Severe/critical",10,sev1)
  sev1=ifelse(pair_data$Research_question=="Unclear severity",11,sev1)
  sev1=ifelse(pair_data$Research_question=="Critical",12,sev1)
  
  
  
  sev=rep(0,ncomp)
  if(sub=="Severity"){
    #codes for severity#
    sev=sev1
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
    # sev=ifelse(pair_data$Funding=="Not reported/unclear",11,sev)
    # sev=ifelse(pair_data$Funding=="No specific funding",12,sev)
  }
  
  if(sub=="Separate Standard care/Placebo"){
    sev=ifelse(pair_data$treat2=="Standard care",1,sev)
    sev=ifelse(pair_data$treat2=="Placebo",2,sev)
  }
  
  #subgroups#
  subgroup=rep(0,ncomp)
  subgroup=ifelse(sev>1 & sev<12,1,subgroup)
  subgroup=ifelse(sev==12,2,subgroup)
  
  pair_data$comp=comp
  pair_data$N1=N1
  pair_data$N2=N2
  pair_data$sev=sev
  pair_data$sev1=sev1
  pair_data$subgroup=subgroup
  
  new_data=subset(pair_data, comp>0)
  
  
  if(is.null(new_data$Treat_Dose_desc)){
    new_data$Treat_Dose_desc=new_data$Treat_Dose_desc1
  }else{
    new_data$Treat_Dose_desc=new_data$Treat_Dose_desc
  }
  
  analysis_data_12=cbind.data.frame(Trial_ID=new_data$Trial_ID,First_author=new_data$First_author,
                         F_U_days=new_data$F_U_days,Intervention1=new_data$Intervention1,
                         Intervention2=new_data$Intervention2,N1=new_data$N1,
                         N2=new_data$N2,Comparison=new_data$Comparison,
                         severity=new_data$Research_question,comp=new_data$comp,
                         HR=new_data$time_to_death_HR,LCI=new_data$time_to_death_lci,
                         UCI=new_data$time_to_death_uci,
                         sev=new_data$sev,sev1=new_data$sev1,subgroup=new_data$subgroup,
                         Year=new_data$Year,dose=as.character(new_data$Treat_Dose_desc),
                         D1=as.character(new_data$ROB_1_randomization),
                         D2=as.character(new_data$ROB2_Time_death),
                         D3=as.character(new_data$ROB_3_time_to_death),
                         D4=as.character(new_data$ROB_4_mortality_OR_time_to_death),
                         D5=as.character(new_data$ROB_5_time_to_death),
                         Overall=as.character(new_data$ROB_6_time_to_death),
                         Conflicts_of_interest=new_data$Conflict_of_interest_category,
                         Funding=new_data$Funding,
                         Countries=new_data$Countries,
                         Publication=new_data$Type_publication)
  
  analysis_data_12=as.data.frame(analysis_data_12)
  
  interval=paste("[",format(round(analysis_data_12$LCI,digits = 2),nsmall = 2),", ",format(round(analysis_data_12$UCI,digits = 2),nsmall = 2),"]",sep="")
  analysis_data_12$ress=paste(format(round(analysis_data_12$HR,digits = 2),nsmall = 2),interval,sep=" ")
  analysis_data_12$ress=ifelse(analysis_data_12$ress=="NA [NA, NA]","",analysis_data_12$ress)

  
  analysis_data_12$logHR=log(analysis_data_12$HR)
  
  analysis_data_12$se=(log(analysis_data_12$UCI)-log(analysis_data_12$LCI))/3.92
  
  
  if(keep_only=="All population"){
    analysis_data_12=analysis_data_12
  }
  
  if(keep_only=="Mild population"){
    data_sev<- analysis_data_12 %>%
      filter(analysis_data_12$sev1==1)
    analysis_data_12=data_sev
  }
  
  if(keep_only=="Mixed population"){
    
    data_sev<-analysis_data_12 %>%
      filter(analysis_data_12$sev1 %in% c(2:11))
    
    analysis_data_12=data_sev
  }
  
  if(keep_only=="Critical population"){
    
    data_sev<-analysis_data_12 %>%
      filter(analysis_data_12$sev1==12)
    
    analysis_data_12=data_sev
  }
  
  
  exclude_preprint<- analysis_data_12 %>%
    filter(analysis_data_12$Publication=="published paper")
  
  
  
  if(Publication==TRUE){
    analysis_data_12<-exclude_preprint}
  
  
  if(Publication==FALSE){
    analysis_data_12<-analysis_data_12}
  
  
  
  
  comp.num=comp.num

  analysis_data_12= analysis_data_12 %>%
    filter(analysis_data_12$comp==comp.num)
  
  analysis_data_12=analysis_data_12[complete.cases(analysis_data_12$UCI), ]
  
  analysis_data_12=as.data.frame(analysis_data_12)
  
  if(nrow(analysis_data_12)>0){
  
  ################################################################
  ################################################################
    exclude_rob<- analysis_data_12 %>%
      filter(analysis_data_12$Overall=="High")
    
    exclude_rob1<- analysis_data_12 %>%
      filter(analysis_data_12$Overall %in% c("High","Some concerns") )
    
    w=setdiff(analysis_data_12$First_author,exclude_rob$First_author)
    
    w1=setdiff(analysis_data_12$First_author,exclude_rob1$First_author)
    
    if(high_RoB=="High RoB"){
      analysis_data_12<-subset(analysis_data_12,analysis_data_12$First_author %in% w)}
    
    
    if(high_RoB=="High RoB/Some concerns"){
      analysis_data_12<-subset(analysis_data_12,analysis_data_12$First_author %in% w1)}
    
    
    if(high_RoB=="No exclusion"){
      analysis_data_12<-analysis_data_12}
  
  if(high_RoB==TRUE){
    analysis_data_12<-subset(analysis_data_12,analysis_data_12$First_author %in% w)}
  
  if(high_RoB==FALSE){
    analysis_data_12=analysis_data_12}
  
  
  analysis_data_12=analysis_data_12[order(analysis_data_12$sev,decreasing=TRUE),]
  
  ilab=cbind(F_U_days=as.character(analysis_data_12$F_U_days),Intervention1=as.character(analysis_data_12$Intervention1),
             Intervention2=as.character(analysis_data_12$Intervention2),
             counts1=analysis_data_12$N1,counts2=analysis_data_12$N2,analysis_data_12$ress)
  ilab=as.data.frame(ilab)
  
  ########################################################################
  analysis_data_12_0<-subset(analysis_data_12,analysis_data_12$comp==comp.num & analysis_data_12$subgroup==0)
  analysis_data_12_1<-subset(analysis_data_12,analysis_data_12$comp==comp.num & analysis_data_12$subgroup==1)
  analysis_data_12_2<-subset(analysis_data_12,analysis_data_12$comp==comp.num & analysis_data_12$subgroup==2)
  ##########################################################################
  
  
  result_12=rma(measure="GEN",yi=analysis_data_12$logHR,sei=analysis_data_12$se,
                slab=paste(analysis_data_12$First_author,analysis_data_12$Year,sep=", "),
                subset=(analysis_data_12$comp==comp.num),method = het_estimate)
  
  
  
  
  result_121=metagen(TE=analysis_data_12$logHR,seTE=analysis_data_12$se,byvar = analysis_data_12$subgroup,method.tau = het_estimate)
  
  if(model=="Fixed-effects"){
    
    result_12=update(result_12,method = "FE")
  }
  
  pred_12<- predict(result_12, transf=exp, digits=2)
  pred_12 <- formatC(c(pred_12$pred, pred_12$ci.lb, pred_12$ci.ub), format="f", digits=2)
  
  
  ks0=0
  ks1=0
  ks2=0
  if (result_12$k>1 & nrow(analysis_data_12_0)>0){
    result_12.0=rma(measure="GEN",yi=analysis_data_12_0$logHR,sei=analysis_data_12_0$se,
                    slab=paste(analysis_data_12_0$First_author,analysis_data_12_0$Year,sep=", "),method = het_estimate)
    
    if(model=="Fixed-effects"){
      result_12.0=update(result_12.0,method = "FE")
      
    }
    
    ks0=result_12.0$k
    
    pred_12.0<- predict(result_12.0, transf=exp, digits=2)
    pred_12.0 <- formatC(c(pred_12.0$pred, pred_12.0$ci.lb, pred_12.0$ci.ub), format="f", digits=2)
    
  }
  if (result_12$k>1 & nrow(analysis_data_12_1)>0){
    result_12.1=rma(measure="GEN",yi=analysis_data_12_1$logHR,sei=analysis_data_12_1$se,
                    slab=paste(analysis_data_12_1$First_author,analysis_data_12_1$Year,sep=", "),method = het_estimate)
    
    if(model=="Fixed-effects"){
      result_12.1=update(result_12.1,method = "FE")
      
    }
    
    ks1=result_12.1$k
    
    pred_12.1<- predict(result_12.1, transf=exp, digits=2)
    pred_12.1<- formatC(c(pred_12.1$pred, pred_12.1$ci.lb, pred_12.1$ci.ub), format="f", digits=2)
    
  }
  if (result_12$k>1 & nrow(analysis_data_12_2)>0 ){
    result_12.2=rma(measure="GEN",yi=analysis_data_12_2$logHR,sei=analysis_data_12_2$se,
                    slab=paste(analysis_data_12_2$First_author,analysis_data_12_2$Year,sep=", "),method = het_estimate)
    
    if(model=="Fixed-effects"){
      
      result_12.2=update(result_12.2,method = "FE")
    }
    
    
    ks2=result_12.2$k
    
    pred_12.2<- predict(result_12.2, transf=exp, digits=2)
    pred_12.2<- formatC(c(pred_12.2$pred, pred_12.2$ci.lb, pred_12.2$ci.ub), format="f", digits=2)
    
  }
  
  
  cex1=0.9
  par(cex=cex1)
  
  result_12.3=result_12
  
  data_rob=analysis_data_12 %>%
    select(First_author,D1,D2,D3,D4,D5,Overall)
  
  par(mar=c(11.5,0,3,1.5), mgp=c(3,0.2,0), tcl=-0.2)
  
  if(sub!="No subgroup analysis"){
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
  }else{
    allrows=c(1:result_12$k)  
  }
  
  analysis_data_12$var_hr=analysis_data_12$se*analysis_data_12$se
  analysis_data_12$weights=1/(analysis_data_12$var_hr+result_12$tau2)
  
  analysis_data_12$relative_weights=(analysis_data_12$weights/sum(analysis_data_12$weights,na.rm = T))*100
  
  analysis_data_12$relative_weights=format(round(analysis_data_12$relative_weights,digits = 2),nsmall=2)
  analysis_data_12$relative_weights=trimws(analysis_data_12$relative_weights)
  
  analysis_data_12$relative_weights=ifelse(analysis_data_12$relative_weights=="NA","",analysis_data_12$relative_weights)
  
  analysis_data_12$relative_weights=ifelse(analysis_data_12$relative_weights!="",
                                           paste(analysis_data_12$relative_weights,"%",sep = ""),analysis_data_12$relative_weights)
  
  ilab$weights=analysis_data_12$relative_weights
  ilab$weights=ifelse(ilab$weights!=" 0.00%",ilab$weights," ")
  
  forest=rob_append_to_forest(result_12,atransf=exp,showweights=F, xlab="Hazard Ratio",cex=0.8,mlab="",
                  col="white",border="white",xlim=c(-15,4),
                  steps=4,
                  ilab=ilab,ilab.xpos=c(-12.35,-11.2,-9.3,-8,-7,-5.1,-6.2),
                  rows=allrows,ylim=c(-1.5,3+max(allrows)),
                  rob_data = data_rob,annotate=F,at=log(c(0.1,1,5))
                  
                  )  
                    

  
  
  header=c("Follow up\ndays","Intervention 1","Intervention 2","N1","N2","Weights","HR [95% CI]")

  par(font=2)
  
  rob_top<-3
  
  
  text(c(-12.35,-11.2,-9.3,-8,-7,-6.2,-5.1),forest$ylim[2]-(rob_top-1) + 1,header,cex=cex1-0.1)
  
  par(font=1)
  
  legend(10, -0.8, "                                                                            ", box.col = "white", bg = "white", adj = 2.2)
  
  

  

  
  
  addpoly.rma(result_12.3,row=-1,mlab="",col="darkblue",border="darkblue",atransf=exp,cex=0.9,annotate = F)
  
  par(font=2)
  text(-5.1,-1,paste(pred_12[1], " [", pred_12[2], ",  ", pred_12[3], "]",sep = ""),cex = cex1)
  par(font=1)
  
  if(sub!="No subgroup analysis"){
  if (result_12$k>1){
    if (ks0>0 & ks1>0 & ks2==0){
      if(sub=="Severity"){
      addpoly.rma(result_12.1,row=1,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      addpoly.rma(result_12.0,row=ks1+2,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      if(sub=="Conflicts of interest"){
        addpoly.rma(result_12.1,row=1,mlab="                    Studies with conflicts of interest",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_12.0,row=ks1+2,mlab="                    Studies without conflicts of interest",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      if(sub=="Funding"){
        addpoly.rma(result_12.1,row=1,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_12.0,row=ks1+2,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      if(sub=="Location"){
        addpoly.rma(result_12.1,row=1,mlab="                    National studies",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_12.0,row=ks1+2,mlab="                    Multinational studies",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      if(sub=="Separate Standard care/Placebo"){
        addpoly.rma(result_12.1,row=1,mlab="                    Placebo summary result",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_12.0,row=ks1+1.8,mlab="                    Standard care summary result",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        
      }
      
      
      par(font=2)
      text(-5.1,1,paste(pred_12.1[1], " [", pred_12.1[2], ",  ",pred_12.1[3], "]",sep = ""),cex = cex1)
      text(-5.1,ks1+1.8,paste(pred_12.0[1], " [", pred_12.0[2], ",  ", pred_12.0[3], "]",sep = ""),cex = cex1)
      par(font=1)
      
    }
    
    if (ks0>0 & ks1==0 & ks2>0){
      if(sub=="Severity"){
      addpoly.rma(result_12.2,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      addpoly.rma(result_12.0,row=ks2+2,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      if(sub=="Funding"){
        addpoly.rma(result_12.2,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_12.0,row=ks2+2,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        
      }
    }
    
    if (ks0==0 & ks1>0 & ks2>0){
      if(sub=="Severity"){
      addpoly.rma(result_12.2,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      addpoly.rma(result_12.1,row=ks2+2,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      if(sub=="Funding"){
        addpoly.rma(result_12.2,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_12.1,row=ks2+2,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      
      text(-5.1,1,paste(pred_12.2[1], " [", pred_12.2[2], ",  ", pred_12.2[3], "]",sep = ""),cex = cex1)
      text(-5.1,ks2+2,paste(pred_12.1[1], " [", pred_12.1[2], ",  ", pred_12.1[3], "]",sep = ""),cex = cex1)
      
      
    }
    if (ks0>0 & ks1>0 & ks2>0){
      if(sub=="Severity"){
      addpoly.rma(result_12.2,row=1,mlab="                    Critical population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      addpoly.rma(result_12.1,row=ks2+2,mlab="                    Mixed population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      addpoly.rma(result_12.0,row=ks2+4+ks1,mlab="                    Mild population",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
      }
      if(sub=="Funding"){
        addpoly.rma(result_12.2,row=1,mlab="                    Studies without specific funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_12.1,row=ks2+2,mlab="                    Studies with public/non profit funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        addpoly.rma(result_12.0,row=ks2+4+ks1,mlab="                    Studies with mixed/private funding",col="blue",border="blue",atransf=exp,cex=0.9,annotate = F)
        
      }
      
      text(-5.1,1,paste(pred_12.2[1], " [", pred_12.2[2], ",  ", pred_12.2[3], "]",sep = ""),cex = cex1)
      text(-5.1,ks2+2,paste(pred_12.1[1], " [", pred_12.1[2], ",  ", pred_12.1[3], "]",sep = ""),cex = cex1)
      
      text(-5.1,1,paste(pred_12.2[1], " [", pred_12.2[2], ",  ", pred_12.2[3], "]",sep = ""),cex = cex1)
      text(-5.1,ks2+2,paste(pred_12.1[1], " [", pred_12.1[2], ",  ", pred_12.1[3], "]",sep = ""),cex = cex1)
      text(-5.1,ks2+4,paste(pred_12.0[1], " [", pred_12.0[2], ",  ", pred_12.0[3], "]",sep = ""),cex = cex1)
        
    }
  }
  }
  
  par(font=1)  
  par(xpd=TRUE)
  if(nrow(analysis_data_12)>8){
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
  
  
  text(-4.5,3.5+max(allrows),"Time to death",
       pos=4,
       cex=1)
  
    
   if(den!="Randomized"){
     par(font=2)
     text(7.8, -0.25, "Risk of bias is assessed only for randomized patients",cex=0.6)
     par(font=1)
   }
   
  if(hide_dose=="FALSE"){
    if(comp.num==comp.num){
      if ((ks0==0 & ks2==0) | (ks0==0 & ks1==0) | (ks1==0 & ks2==0)){
        for(i in 1:result_12$k){
          text(-11.5, -0.3+i+sliderDose, pos=4, analysis_data_12$dose[i],cex=cex1-0.3)
        }
      }
      if (ks0>0 & ks1>0 & ks2==0){
        for(i in 1:ks1){
          text(-11.5, -0.3+i+1+sliderDose, pos=4, analysis_data_12$dose[i],cex=cex1-0.3)
        }
        for(i in (ks1+1):(ks0+ks1)){
          text(-11.5, -0.3+i+2+sliderDose, pos=4, analysis_data_12$dose[i],cex=cex1-0.3)
        }
      }
      if (ks0>0 & ks1==0 & ks2>0){
        for(i in 1:ks2){
          text(-11.5, -0.3+i+1+sliderDose, pos=4, analysis_data_12$dose[i],cex=cex1-0.2)
        }
        for(i in (ks2+1):(ks0+ks2)){
          text(-11.5, -0.3+i+2+sliderDose, pos=4, analysis_data_12$dose[i],cex=cex1-0.2)
        }  
      }
      if (ks0==0 & ks1>0 & ks2>0){
        for(i in 1:ks2){
          text(-11.5, -0.3+i+1+sliderDose, pos=4, analysis_data_12$dose[i],cex=cex1-0.2)
        }
        for(i in (ks2+1):(ks1+ks2)){
          text(-11.5, -0.3+i+2+sliderDose, pos=4, analysis_data_12$dose[i],cex=cex1-0.2)
        }    
      }
      if (ks0>0 & ks1>0 & ks2>0){
        for(i in 1:ks2){
          text(-11.5, -0.3+i+1+sliderDose, pos=4, analysis_data_12$dose[i],cex=cex1-0.2)
        }
        for(i in (ks2+1):(ks1+ks2)){
          text(-11.5, -0.3+i+2+sliderDose, pos=4, analysis_data_12$dose[i],cex=cex1-0.2)
        }  
        for(i in (ks1+ks2+1):(ks1+ks2+ks0)){
          text(-11.5, -0.3+i+4+sliderDose, pos=4, analysis_data_12$dose[i],cex=cex1-0.2)
        }
      }
    }
    
    
    count=sum(str_count(analysis_data_12$dose, "\\*"),na.rm = TRUE)
    
    if(is.na(count)==TRUE){
      count=0}
    
    if(count>0){
      text(-9.5, -0.2, "(*different loading dose)", pos=4, cex=0.6)}
    
    else if(count==0) {
      text(-9, -0.2, "", pos=4, cex=0.6)}
    
  }
  
  par(font=2,cex=0.7)
  if ((ks0==0 & ks2==0) | (ks0==0 & ks1==0) | (ks1==0 & ks2==0)){
    for(i in 1:result_12$k){
      if(hide_severity=="FALSE"){
      text(-15, 0.5+i, pos=4, analysis_data_12$severity[i],cex=cex1-0.1)
      }
    }
  }
  if (ks0>0 & ks1>0 & ks2==0){
    for(i in 1:ks1){
      if(hide_severity=="FALSE"){
      text(-15, 0.5+i+1, pos=4, analysis_data_12$severity[i],cex=cex1-0.1)
      }
    }
    for(i in (ks1+1):(ks0+ks1)){
      if(hide_severity=="FALSE"){
      text(-15, 0.5+i+2, pos=4, analysis_data_12$severity[i],cex=cex1-0.1)
      }
    }
  }
  if (ks0>0 & ks1==0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
      text(-15, 0.5+i+1, pos=4, analysis_data_12$severity[i],cex=cex1-0.1)
        
      }
    }
    for(i in (ks2+1):(ks0+ks2)){
      if(hide_severity=="FALSE"){
      text(-15, 0.5+i+2, pos=4, analysis_data_12$severity[i],cex=cex1-0.1)
      }
    }  
  }
  if (ks0==0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
      text(-15, 0.5+i+1, pos=4, analysis_data_12$severity[i],cex=cex1-0.1)
      }
    }
    for(i in (ks2+1):(ks1+ks2)){
      if(hide_severity=="FALSE"){
      text(-15, 0.5+i+2, pos=4, analysis_data_12$severity[i],cex=cex1-0.1)
      }
    }    
  }
  if (ks0>0 & ks1>0 & ks2>0){
    for(i in 1:ks2){
      if(hide_severity=="FALSE"){
      text(-15, 0.5+i+1, pos=4, analysis_data_12$severity[i],cex=cex1-0.1)
      }
    }
    for(i in (ks2+1):(ks1+ks2)){
      if(hide_severity=="FALSE"){
      text(-15, 0.5+i+2, pos=4, analysis_data_12$severity[i],cex=cex1-0.1)
      }
    }  
    for(i in (ks1+ks2+1):(ks1+ks2+ks0)){
      if(hide_severity=="FALSE"){
      text(-15, 0.5+i+4, pos=4, analysis_data_12$severity[i],cex=cex1-0.1)
      }
    }
  }
  par(font=2)

  
  if(nrow(analysis_data_12)>1){
    
  
    N1_total=sum(analysis_data_12$N1)
    N2_total=sum(analysis_data_12$N2)
    I1=c(N1_total,N2_total)
    text(-9.7,-1,"Totals:",cex=cex1)
    text(c(-8,-7),-1,I1,cex=cex1)
    
  }
  
  text(-2.5,-1.5,"Intervention 1 better",
       pos=4,
       cex=cex1-0.1)
  text(0.5,-1.5,"Intervention 2 better",
       pos=4,
       cex=cex1-0.1)
  
  today <- Sys.Date()
  today=format(today, format="%m %d %Y")
  
  
  a1=paste("Forest plot produced at:"," ",today,sep="")
  a2=paste("Data source: the COVID-NMA initiative (covid-nma.com)")
  
  
  
  af=paste(a1,a2,sep ="\n")
  if(nrow(analysis_data_12)>7.5){
    legend(0.5,-4,legend=af,
           ncol=1,cex=0.75,box.lty = 0,text.col = "blue")
    
    
  }else{
    legend(0.5,-3,legend=af,
           ncol=1,cex=0.75,box.lty = 0,text.col = "blue")}
  
  
  
  if(result_12$k>8.5){
    if(length(unique(analysis_data_12$subgroup))==1){
      par(font=2)
      text(-15,-1.5 , pos=4, cex=cex1, bquote(paste("Heterogeneity results: Q = ",
                                                    .(formatC(result_12$QE, digits=2, format="f")),
                                                    ", p = ", .(formatC(result_12$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                    .(formatC(result_12$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                    .(formatC(result_12$tau2, digits=2, format="f")))))  
    }
    if(length(unique(analysis_data_12$subgroup))>1){
      par(font=2)
      text(-15, -1.5, pos=4, cex=cex1, bquote(paste("Heterogeneity results (overall analysis): Q = ",
                                                    .(formatC(result_12$QE, digits=2, format="f")),
                                                    ", p = ", .(formatC(result_12$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                    .(formatC(result_12$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                    .(formatC(result_12$tau2, digits=2, format="f"))))) 
      
      text(-15, -2.5, pos=4, cex=cex1, bquote(paste("Test for subgroup differences: Q = ",
                                                    .(formatC(result_121$Q.b.random, digits=2, format="f")),"; ", "p = ",
                                                    .(formatC(result_121$pval.Q.b.random, digits=2, format="f"))
                                                    
      )))
      
      
    }
    
  }
  
  
  if((result_12$k>1) & (result_12$k<9)){
    if(length(unique(analysis_data_12$subgroup))==1){
      par(font=2)
      text(-15,-1.5 , pos=4, cex=cex1, bquote(paste("Heterogeneity results: Q = ",
                                                    .(formatC(result_12$QE, digits=2, format="f")),
                                                    ", p = ", .(formatC(result_12$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                    .(formatC(result_12$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                    .(formatC(result_12$tau2, digits=2, format="f")))))  
    }
    if(length(unique(analysis_data_12$subgroup))>1){
      par(font=2)
      text(-15, -1.5, pos=4, cex=cex1, bquote(paste("Heterogeneity results (overall analysis): Q = ",
                                                    .(formatC(result_12$QE, digits=2, format="f")),
                                                    ", p = ", .(formatC(result_12$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                                    .(formatC(result_12$I2, digits=1, format="f")), "%; ",tau^2," = ",
                                                    .(formatC(result_12$tau2, digits=2, format="f"))))) 
      
      text(-15, -2, pos=4, cex=cex1, bquote(paste("Test for subgroup differences: Q = ",
                                                  .(formatC(result_121$Q.b.random, digits=2, format="f")),"; ", "p = ",
                                                  .(formatC(result_121$pval.Q.b.random, digits=2, format="f"))
                                                  
      )))
      
      
    }
    
  }
  
  }
  
   if (nrow(analysis_data_12)==0){
    stop("No data available. Please, make another choice.")
  }
 #dev.off()
}
  
  