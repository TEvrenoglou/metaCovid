models=function(data,comp.num,outcome,dat){
  
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
  if(outcome=="Serious adverse events"){
  new_data=final_data1 %>%
    select(Trial_ID,ID_stats,First_author,name_study,stats_name,n_Randomized,n_Analyzed_,Serious_AE)
  nc=ncol(new_data)
  new_data$First_author=ifelse(is.na(new_data$First_author),new_data$name_study,new_data$First_author)
  new_data=new_data[complete.cases(new_data[ , nc]),]
  }
  
  if(outcome=="Adverse events"){
    new_data=final_data1 %>%
      select(Trial_ID,ID_stats,First_author,name_study,stats_name,n_Randomized,n_Analyzed_,Total_AE)
    nc=ncol(new_data)
    new_data$First_author=ifelse(is.na(new_data$First_author),new_data$name_study,new_data$First_author)
    new_data=new_data[complete.cases(new_data[ , nc]),]
  }
  
  if(outcome=="Mortality D28"){
    new_data=final_data1 %>%
      select(Trial_ID,ID_stats,First_author,name_study,stats_name,n_Randomized,n_Analyzed_,All_cause_mortality_events_D28)
    nc=ncol(new_data)
    new_data$First_author=ifelse(is.na(new_data$First_author),new_data$name_study,new_data$First_author)
    new_data=new_data[complete.cases(new_data[ , nc]),]
  }
  
  
  if(outcome=="Mortality D60"){
    new_data=final_data1 %>%
      select(Trial_ID,ID_stats,First_author,name_study,stats_name,n_Randomized,n_Analyzed_,All_cause_mortality_events_D60)
    nc=ncol(new_data)
    new_data$First_author=ifelse(is.na(new_data$First_author),new_data$name_study,new_data$First_author)
    new_data=new_data[complete.cases(new_data[ , nc]),]
  }
  
  
  if(outcome=="Clinical improvement D28"){
    new_data=final_data1 %>%
      select(Trial_ID,ID_stats,First_author,name_study,stats_name,n_Randomized,n_Analyzed_,Clinical_improvement_D28_n)
    nc=ncol(new_data)
    new_data$First_author=ifelse(is.na(new_data$First_author),new_data$name_study,new_data$First_author)
    new_data=new_data[complete.cases(new_data[ , nc]),]
  }
  
  
  if(outcome=="Clinical improvement D60"){
    new_data=final_data1 %>%
      select(Trial_ID,ID_stats,First_author,name_study,stats_name,n_Randomized,n_Analyzed_,Clinical_improvement_D60_n)
    nc=ncol(new_data)
    new_data$First_author=ifelse(is.na(new_data$First_author),new_data$name_study,new_data$First_author)
    new_data=new_data[complete.cases(new_data[ , nc]),]
  }
  
  if(outcome=="WHO Score 7 D28"){
    new_data=final_data1 %>%
      select(Trial_ID,ID_stats,First_author,name_study,stats_name,n_Randomized,n_Analyzed_,SCORE_7_and_above_D28)
    nc=ncol(new_data)
    new_data$First_author=ifelse(is.na(new_data$First_author),new_data$name_study,new_data$First_author)
    new_data=new_data[complete.cases(new_data[ , nc]),]
  }
  
  
  if(outcome=="WHO Score 7 D90"){
    new_data=final_data1 %>%
      select(Trial_ID,ID_stats,First_author,name_study,stats_name,n_Randomized,n_Analyzed_,SCORE_7_and_above_D90)
    nc=ncol(new_data)
    new_data$First_author=ifelse(is.na(new_data$First_author),new_data$name_study,new_data$First_author)
    new_data=new_data[complete.cases(new_data[ , nc]),]
  }
  
  if(outcome=="Negative conversion D7"){
    new_data=final_data1 %>%
      select(Trial_ID,ID_stats,First_author,name_study,stats_name,n_Randomized,n_Analyzed_,NEG_conv_n_D7)
    nc=ncol(new_data)
    new_data$First_author=ifelse(is.na(new_data$First_author),new_data$name_study,new_data$First_author)
    new_data=new_data[complete.cases(new_data[ , nc]),]
  }
  
  if(outcome=="Hospitalized patients"){
    new_data=final_data1 %>%
      select(Trial_ID,ID_stats,First_author,name_study,stats_name,n_Randomized,n_Analyzed_,Hosp)
    nc=ncol(new_data)
    new_data$First_author=ifelse(is.na(new_data$First_author),new_data$name_study,new_data$First_author)
    new_data=new_data[complete.cases(new_data[ , nc]),]
  }
  
  
t=as.data.frame(table(new_data$Trial_ID))
E=which(t$Freq==1)
single_arm=as.character(t$Var1[E])

new_data=new_data %>%
  filter(Trial_ID %!in% single_arm)

new_data$n_Randomized=ifelse(is.na(new_data$n_Randomized),new_data$n_Analyzed_,new_data$n_Randomized)
  
n=length(names(new_data))
names(new_data)[n]="events"
new_data$treat=rep(c(2,1),nrow(new_data)/2)

new_data$events=as.numeric(new_data$events)
new_data$n_Randomized=as.numeric(new_data$n_Analyzed_)
  
#################################################################

if(length(unique(new_data$Trial_ID))>1){

# Meta-analysis using penalized logistic regression 
  
penalized_model=brglm(cbind(events,n_Randomized-events)~as.factor(Trial_ID)+as.factor(treat),data = new_data)

penalized_ests=summary(penalized_model)$coefficients
penalized_ests=penalized_ests[grep("treat",rownames(penalized_ests)),]
logOR_pen=unname(penalized_ests[1])
selogOR_pen=unname(penalized_ests[2])
L_logOR_pen=logOR_pen-1.96*selogOR_pen
U_logOR_pen=logOR_pen+1.96*selogOR_pen

OR_pen=format(round(exp(logOR_pen),digits = 2),nsmall=2)
L_OR_pen=format(round(exp(L_logOR_pen),digits = 2),nsmall=2)
U_OR_pen=format(round(exp(U_logOR_pen),digits = 2),nsmall=2)

pen_interval=paste("[",L_OR_pen,", ",U_OR_pen,"]",sep = "")
pen_ests=paste(OR_pen,pen_interval,sep=" ")


#### preparing data for metafor ###

p=pairwise(treat = treat,studlab = Trial_ID,event = events,n=n_Randomized,data = new_data)

# Meta-analysis using common-effect inverse variance for the estimation of OR   

iv_common_model=rma(measure="OR", ai=events1, bi=n1-events1, ci=events2, di=n2-events2, data=p,drop00 = F,method = "FE")
OR_IV_common=format(round(exp(iv_common_model$b),digits = 2),nsmall=2)
L_IV_common=format(round(exp(iv_common_model$ci.lb),digits=2),nsmall=2)
U_IV_common=format(round(exp(iv_common_model$ci.ub),digits=2),nsmall=2)

IV_common_interval=paste("[",L_IV_common,", ",U_IV_common,"]",sep = "")
IV_common_ests=paste(OR_IV_common,IV_common_interval,sep = " ")

# Meta-analysis using random-effects inverse variance for the estimation of OR   

iv_random_model=rma(measure="OR", ai=events1, bi=n1-events1, ci=events2, di=n2-events2, data=p,drop00 = F)
OR_IV_random=format(round(exp(iv_random_model$b),digits = 2),nsmall=2)
L_IV_random=format(round(exp(iv_random_model$ci.lb),digits=2),nsmall=2)
U_IV_random=format(round(exp(iv_random_model$ci.ub),digits=2),nsmall=2)

IV_random_interval=paste("[",L_IV_random,", ",U_IV_random,"]",sep = "")
IV_random_ests=paste(OR_IV_random,IV_random_interval,sep = " ")

# Meta-analysis using MH OR           
mh_model=rma.mh(measure="OR", ai=events1, bi=n1-events1, ci=events2, di=n2-events2, data=p,drop00 = F)
OR_MH=format(round(exp(mh_model$b),digits = 2),nsmall=2)
L_MH=format(round(exp(mh_model$ci.lb),digits=2),nsmall=2)
U_MH=format(round(exp(mh_model$ci.ub),digits=2),nsmall=2)

MH_interval=paste("[",L_MH,", ",U_MH,"]",sep = "")
MH_ests=paste(OR_MH,MH_interval,sep = " ")

#################################################
# Meta-analysis using Peto's OR
peto_model=rma.peto(ai=events1, bi=n1-events1, ci=events2, di=n2-events2, data=p,drop00 = F)

OR_peto=format(round(exp(peto_model$b),digits = 2),nsmall=2)
L_peto=format(round(exp(peto_model$ci.lb),digits=2),nsmall=2)
U_peto=format(round(exp(peto_model$ci.ub),digits=2),nsmall=2)

peto_interval=paste("[",L_peto,", ",U_peto,"]",sep = "")
peto_ests=paste(OR_peto,peto_interval,sep = " ")



########## Collect the results ####
results=rbind.data.frame(IV_random_ests,IV_common_ests,pen_ests,MH_ests,peto_ests)
results$type=c("Random-effects",rep("Common/Fixed-effect",4))
results$model=c("Inverse-Variance","Inverse-Variance","Penalized likelihood method","Mantel-Haenszel method","Peto method")
names(results)=c("OR","Type","Model")
results=results %>%
  select(Model,Type,OR)
names(results)=c("Model","Type","OR [95% CI]")
}else{
  stop("The number of the available randomized controlled trials should be greater than 1 for pooling to make sense")
}

return(results)
}

