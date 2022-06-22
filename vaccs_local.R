forest_local=function(data,
                         type,
                         slider=0,
                         subgroup=T,
                         overall=F,
                         expand=c(0,0),
                         showweights=F,  
                         model="Random-effects",
                         het_estimate="REML",
                         high_RoB=FALSE,
                         Publication=FALSE
)
{
  
  data=subset(data,
              Trial_ID!=66 & Trial_ID!=5015 & Trial_ID!= 47 & Trial_ID!= 67 & Trial_ID!= 5025 & Trial_ID!= 78 & Trial_ID!= 1012
              & Trial_ID!= 97  & Trial_ID!= 70
              & Trial_ID!= 104 & Trial_ID!= 5002 & Trial_ID!= 105)
  
  
  data$comp_type=ifelse(is.na(data$comp_type),"regular",data$comp_type)
  
  data=data %>%
    filter(data$comp_type=="regular")
  
  
  data$Type_publication=trimws(data$Type_publication)
  data$Safety_median_followup=ifelse(!is.na(data$Safety_median_followup),
                                     paste(data$Safety_median_followup,"*",sep=""),"")
  data$complement=trimws(data$complement)
  
  data$F_U_months=as.character(data$Safety_median_followup)
  data$F_U_days=ifelse(is.na(data$local_AE_timepoint),data$F_U_months,as.numeric(data$local_AE_timepoint))
  
  data$Treat_Type=trimws(data$Treat_Type)
  data$Developer_name=trimws(data$Developer_name)
  
  data$Developer_name=ifelse(data$Developer_name=="BioNTech SE, Pfizer","Pfizer/BioNTech+Fosun Pharma",
                             data$Developer_name)
  
  data$Developer_name=ifelse(startsWith(data$Developer_name,"EpiVac"),"EpiVac LLC",data$Developer_name)
  
  data$Developer_name=ifelse(data$Developer_name=="Gamaleya Research Institute of Epidemiology and Microbiology",
                             "Gamaleya Research Institute (Sputnik V)",data$Developer_name)
  
  
  data$Developer_name=ifelse(data$Developer_name=="AstraZeneca+University of Oxford",
                             "University of Oxford+AstraZeneca",data$Developer_name)
  
  
  data$Developer_name=ifelse(data$Developer_name=="Sinovac Research and Development Co., Ltd",
                             "Sinovac",data$Developer_name)
  
  data$Developer_name=ifelse(data$Developer_name=="Sinopharm - China National Biotec Group Company Limited-Wuhan",
                             "Sinopharm-Wuhan",data$Developer_name)
  
  data$Developer_name=ifelse(data$Developer_name=="Sinopharm - China National Biotec Group Company Limited-Beijing",
                             "Sinopharm-Beijing",data$Developer_name)
  
  
  
  
  data$n_Randomized=as.numeric(data$AE_Local_d7)
  
  
  
  data$Treat_Name=trimws(data$stats_name)
  data$stats_name=trimws(data$stats_name)
  data$Treat_Name=data$stats_name
  data$Trial_ID=trimws(data$Trial_ID)
  data$First_author=trimws(data$First_author)
  data$name_study=trimws(data$Trial_name)
  data$First_author=ifelse(is.na(data$First_author),data$name_study,data$First_author)
  data$Year=trimws(data$Year)
  data$Research_question=trimws(data$Treat_Type)
  
  
  
  data$AE_syst_d14=data$AE_Local_d7
  
  rct_data=data
  
  
  
  rct_data$AE_syst_d14=as.numeric(rct_data$AE_syst_d14)
  pair_data_05_14=pairwise_old(studlab=Trial_ID,treat=Treat_Name,event=AE_syst_d14,n=AE_Local_d7_denominator,
                               data=rct_data,measure="RR",ref="Standard care")
  pair_data_05_14$F_U_days1=pair_data_05_14$F_U_days
  pair_data_05_14$First_author=pair_data_05_14$First_author
  
  
  ncomp=length(pair_data_05_14$treat1)
  
  
  Intervention1=rep("NA",ncomp)
  Intervention2=rep("NA",ncomp)     
  Intervention1=ifelse(pair_data_05_14$Intervention_control1=="Intervention" | pair_data_05_14$Intervention_control1=="Intervention/control",pair_data_05_14$treat1,Intervention1)
  Intervention1=ifelse(pair_data_05_14$Intervention_control1=="Control",pair_data_05_14$treat2,Intervention1)
  Intervention2=ifelse(Intervention1==pair_data_05_14$treat1,pair_data_05_14$treat2,Intervention2)
  Intervention2=ifelse(Intervention1==pair_data_05_14$treat2,pair_data_05_14$treat1,Intervention2)
  Comparison=str_c(Intervention1, Intervention2, sep = " vs ", collapse = NULL)
  
  
  
  
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
  
  
  
  ##########################################################################
  
  
  
  #codes for comparisons#
  
  ##### comp
  comp=rep(0,ncomp)
  
  
  
  pair_data_05_14$comp=comp
  pair_data_05_14$r1=r1_05_14
  pair_data_05_14$r2=r2_05_14
  pair_data_05_14$N1=N1_05_14
  pair_data_05_14$N2=N2_05_14
  
  
  pair_data_05_14_1=as.data.frame(pair_data_05_14)
  
  class(pair_data_05_14_1)=class(pair_data_05_14_1)[-2]
  
  if(is.null(pair_data_05_14_1$Treat_Type1)){
    pair_data_05_14_1$Treat_Type1=pair_data_05_14_1$Treat_Type
  }else{
    pair_data_05_14_1$Treat_Type1=pair_data_05_14_1$Treat_Type1
  }
  
  if(is.null(pair_data_05_14_1$Research_question1)){
    pair_data_05_14_1$Research_question1=pair_data_05_14_1$Research_question
  }else{
    pair_data_05_14_1$Research_question1=pair_data_05_14_1$Research_question1
  }
  
  new_data_05_14=pair_data_05_14_1 %>%
    filter(Treat_Type1==type)
  
  
  
  analysis_data_05_14=cbind.data.frame(Trial_ID=new_data_05_14$Trial_ID,First_author=new_data_05_14$First_author,
                                       F_U_days=new_data_05_14$F_U_days,Intervention1=new_data_05_14$Intervention1,
                                       Intervention2=new_data_05_14$Intervention2,r1=new_data_05_14$r1,N1=new_data_05_14$N1,
                                       r2=new_data_05_14$r2,N2=new_data_05_14$N2,Comparison=new_data_05_14$Comparison,
                                       
                                       severity=new_data_05_14$Research_question1,comp=new_data_05_14$comp,
                                       
                                       Year=new_data_05_14$Year,
                                       A=as.character(new_data_05_14$ROB_1_randomization),
                                       B=as.character(new_data_05_14$ROB2_AE_local),
                                       C=as.character(new_data_05_14$ROB_3_AE_local),
                                       D=as.character(new_data_05_14$ROB_4_AE_local),
                                       E=as.character(new_data_05_14$ROB_5_AE_local),
                                       Overall=as.character(new_data_05_14$ROB_6_AE_local),
                                       Conflicts_of_interest=new_data_05_14$Conflict_of_interest_category,
                                       Funding=new_data_05_14$Funding,
                                       Countries=new_data_05_14$Countries,
                                       Developer_name=new_data_05_14$Developer_name,
                                       Type=new_data_05_14$Treat_Type1,
                                       Publication=new_data_05_14$Type_publication,
                                       RR=trimws(format(round(exp(new_data_05_14$TE),digits = 2),nsmall=2)),
                                       seTE=new_data_05_14$seTE,
                                       L=trimws(format(round(exp(new_data_05_14$TE-1.96*new_data_05_14$seTE),digits = 2),nsmall=2)),
                                       U=trimws(format(round(exp(new_data_05_14$TE+1.96*new_data_05_14$seTE),digits = 2),nsmall=2)),
                                       complement=new_data_05_14$complement
  )
  
  analysis_data_05_14=as.data.frame(analysis_data_05_14)
  
  
  
  analysis_data_05_14$RR=ifelse(analysis_data_05_14$RR=="NA",NA,analysis_data_05_14$RR)
  analysis_data_05_14$L=ifelse(analysis_data_05_14$L=="NA",NA,analysis_data_05_14$L)
  analysis_data_05_14$U=ifelse(analysis_data_05_14$U=="NA",NA,analysis_data_05_14$U)
  
  interval=paste("[",analysis_data_05_14$L,", ",analysis_data_05_14$U,"]",sep="")
  analysis_data_05_14$ress=paste(analysis_data_05_14$RR,interval,sep=" ")
  analysis_data_05_14$ress=ifelse(analysis_data_05_14$ress=="NA [NA, NA]","",analysis_data_05_14$ress)
  
  year_comp=ifelse(is.na(analysis_data_05_14$complement),analysis_data_05_14$Year,
                   paste(analysis_data_05_14$Year,analysis_data_05_14$complement,sep=""))
  
  analysis_data_05_14$year_comp=year_comp
  
  analysis_data_05_14$f1=analysis_data_05_14$N1-analysis_data_05_14$r1
  analysis_data_05_14$f2=analysis_data_05_14$N2-analysis_data_05_14$r2
  
  
  analysis_data_05_14$counts1=str_c(analysis_data_05_14$r1, analysis_data_05_14$N1, sep = "/", collapse = NULL)
  analysis_data_05_14$counts2=str_c(analysis_data_05_14$r2, analysis_data_05_14$N2, sep = "/", collapse = NULL)
  
  oo=ifelse(analysis_data_05_14$r1+analysis_data_05_14$r2==0,"0-0","not 0-0")
  
  analysis_data_05_14$oo=oo
  
  
  
  analysis_data_05_14= analysis_data_05_14 %>%
    group_by(Developer_name) %>%
    arrange(Developer_name,.by_group=T)
  
  slab=paste(analysis_data_05_14$First_author,analysis_data_05_14$year_comp,sep=", ")
  
  analysis_data_05_14$slab=slab
  
  p1=escalc(ai=analysis_data_05_14$r1,bi=analysis_data_05_14$f1,ci=analysis_data_05_14$r2,
            di=analysis_data_05_14$f2,slab=analysis_data_05_14$slab,
            add=1/2, to="only0",drop00=F,measure="RR")
  
  ##################################################################
  
  
  analysis_data_05_14=analysis_data_05_14[complete.cases(analysis_data_05_14$counts1), ]
  
  analysis_data_05_14=as.data.frame(analysis_data_05_14)
  
  exclude_preprint<- analysis_data_05_14 %>%
    filter(analysis_data_05_14$Publication=="Published")
  
  
  if(Publication==TRUE){
    analysis_data_05_14<-exclude_preprint}
  
  
  if(Publication==FALSE){
    analysis_data_05_14<-analysis_data_05_14}
  
  
  exclude_rob<- analysis_data_05_14 %>%
    filter(analysis_data_05_14$Overall=="High")
  
  exclude_rob1<- analysis_data_05_14 %>%
    filter(analysis_data_05_14$Overall %in% c("High","Some concerns") )
  
  rob=setdiff(analysis_data_05_14$slab,exclude_rob$slab)
  
  rob1=setdiff(analysis_data_05_14$slab,exclude_rob1$slab)
  
  if(high_RoB=="High RoB"){
    analysis_data_05_14<-subset(analysis_data_05_14,analysis_data_05_14$slab %in% rob)}
  
  
  if(high_RoB=="High RoB/Some concerns"){
    analysis_data_05_14<-subset(analysis_data_05_14,analysis_data_05_14$slab %in% rob1)}
  
  
  if(high_RoB=="No exclusion"){
    analysis_data_05_14<-analysis_data_05_14}
  
  
  analysis_data_05_14=analysis_data_05_14 %>%
    group_by(Developer_name) %>%
    arrange(Developer_name,.by_group=T)
  
  
  
  
  ilab=cbind(F_U_days=as.character(analysis_data_05_14$F_U_days),Intervention1=as.character(analysis_data_05_14$Intervention1),
             Intervention2=as.character(analysis_data_05_14$Intervention2),
             counts1=analysis_data_05_14$counts1,counts2=analysis_data_05_14$counts2)
  ilab=as.data.frame(ilab)
  
  
  #slab=paste(analysis_data_05_14$First_author,analysis_data_05_14$Year,sep=", ")
  
  p=escalc(ai=analysis_data_05_14$r1,bi=analysis_data_05_14$f1,ci=analysis_data_05_14$r2,
           di=analysis_data_05_14$f2,slab=analysis_data_05_14$slab,
           add=1/2, to="only0",drop00=FALSE,measure="RR")
  
  analysis_data_05_14$yi=log(as.numeric(analysis_data_05_14$RR))
  analysis_data_05_14$vi=as.numeric((analysis_data_05_14$seTE*analysis_data_05_14$seTE))
  
  names(p1)=c("yi1","vi1")
  
  if(model!="Fixed-effects"){
    meta=metagen(TE=yi,seTE=sqrt(vi),byvar = Developer_name,data = analysis_data_05_14,method.tau = het_estimate)
  }else{
    meta=metagen(TE=yi,seTE=sqrt(vi),byvar = Developer_name,data = analysis_data_05_14,comb.fixed = T,comb.random = F)
  }
  
  
  analysis_data_05_14$estimates = analysis_data_05_14$ress
  # ilab$weights=analysis_data_05_14$rel_wi
  ilab$ests=analysis_data_05_14$estimates
  
  
  
  
  ###### New code #########
  
  id=seq(1:nrow(analysis_data_05_14))
  analysis_data_05_14$id=id
  
  u=unique(analysis_data_05_14$Developer_name)
  sub=NA
  
  
  for(i in 1:length(u)){
    for(k in 1:nrow(analysis_data_05_14)){
      
      sub[k]=ifelse(analysis_data_05_14$Developer_name[k]==u[i],i,sub[k])
      
    }
    
  }
  
  
  
  analysis_data_05_14$sub=as.numeric(sub)
  
  p$sub=analysis_data_05_14$sub
  p$slab=analysis_data_05_14$slab
  
  
  if(nrow(analysis_data_05_14)>1){
    res <- rma.mv(yi, vi, random = ~ 1 | as.factor(sub), data =p,
                  slab=analysis_data_05_14$slab)
    
  }
  
  
  if(nrow(analysis_data_05_14)==1){
    result_05_14=rma(p,drop00 = F)
    res=result_05_14
  }
  
  dd <- c(0,diff(analysis_data_05_14$sub))
  
  
  dd[dd > 0] <- 1
  rows <- (1:res$k) + cumsum(dd)
  
  
  #slab=paste(analysis_data_05_14$First_author,analysis_data_05_14$Year,sep=", ")
  
  
  I2=meta$I2.w
  I2=format(round(I2,digits = 2)*100,nsmall=2)
  I2=trimws(I2)
  I2=ifelse(I2!="NA",paste(I2,"%",sep=""),NA)
  tau=meta$tau2.w
  tau=format(round(tau,digits = 2),nsmall=2)
  tau=trimws(tau)
  tau=ifelse(tau=="NA",NA,tau)
  
  Q=meta$Q.w
  Q=format(round(Q,digits = 2),nsmall=2)
  Q=trimws(Q)
  Q=ifelse(Q=="NA",NA,Q)
  
  p=meta$pval.Q.w
  p=format(round(p,digits = 2),nsmall=2)
  p=trimws(p)
  p=ifelse(p=="NA",NA,p)
  
  by_var=unique(meta$byvar)
  
  het=c()
  
  ind_het=NA
  
  ind_het=ifelse(is.na(I2),0,1)
  
  for(i in 1: length(by_var)){
    if(ind_het[i]==1){
      het[i]=paste("Heterogeneity: Q = ",Q[i],
                   ", p = ", p[i], "; ", "I2", " = ", 
                   I2[i], "; ","tau2"," = ",
                   tau[i])
      
    }else{
      het[i]=paste(" ")
    }
  }
  
  het_f=cbind.data.frame(by_var,het)
  names(het_f)=c("Developer","Heterogeneity")
  
  
  data_rob=data.frame(Study=analysis_data_05_14$slab,
                      D1=analysis_data_05_14$A,
                      D2=analysis_data_05_14$B,
                      D3=analysis_data_05_14$C,
                      D4=analysis_data_05_14$D,
                      D5=analysis_data_05_14$E,
                      Overall=analysis_data_05_14$Overall
  )
  
  
  
  
  analysis_data_05_14$cols=ifelse(analysis_data_05_14$oo=="not 0-0","black","white")
  
  par(mar=c(11.5,0,3,1.5), mgp=c(3,0.2,0), tcl=-0.2)
  
  cex1=0.9
  forest=rob_append_to_forest(res,atransf=exp,
                              rob_data = data_rob,
                              showweights=F, xlab="Risk ratio",
                              cex=0.8,mlab="",
                              col="white",border="white",xlim=c(-15,4),
                              at=log(c(0.1,1,6)),
                              steps=4,
                              ilab=ilab,ilab.xpos=c(-12.35,-11.2,-9.3,-8,-7,
                                                    #-6.1,
                                                    -5.1),rows=rows,ylim=c(-1.5,3+max(rows)),
                              psize=1.1,
                              colout=analysis_data_05_14$cols,
                              annotate=F,
                              refline=NA
  )
  
  
  
  
  for(i in 1:nrow(analysis_data_05_14)){
    if(analysis_data_05_14$oo[i]=="0-0"){
      legend(-2.3, rows[i]+0.3,"                                                                                                                          ",
             box.col = "white", bg = "white", adj = 0.5,cex=0.5)
    }
  }
  
  if(subgroup==T){
    header=c("Follow up\ndays","Intervention 1","Intervention 2","r1/N1","r2/N2","Weights","RR [95% CI]")
  }else{
    header=c("Follow up\ndays","Intervention 1","Intervention 2","r1/N1","r2/N2","RR [95% CI]")  
  }
  
  par(font=2)
  
  rob_top<-3
  
  
  if(subgroup==T){
    text(c(-12.35,-11.2,-9.3,-8,-7,-6.1,-5.1),forest$ylim[2]-(rob_top-1) + 1,header,cex=cex1-0.1)
  }else{
    text(c(-12.35,-11.2,-9.3,-8,-7,-5.1),forest$ylim[2]-(rob_top-1) + 1,header,cex=cex1-0.1)  
  }
  
  
  
  
  par(font=1)  
  par(xpd=TRUE)
  if(nrow(analysis_data_05_14)>8){
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
  
  
  
  
  par(font=1)
  text(-2.5,3.7+max(rows),"Local reactogenicity events",
       pos=4,
       cex=1)
  # 
  
  par(font=1)
  text(-2.7,2.6+max(rows),paste("Type:",type,sep=" "),
       pos=4,
       cex=1)
  
  
  yy=list()
  
  yy1=list()
  
  u=unique(analysis_data_05_14$sub)
  fake_list=list()
  
  r=list()
  
  ff=which(diff(rows)>1)+slider
  uu=list()
  weights=list()
  rel_weights=list()
  diamond_ci.lb=list()
  diamond_ci.ub=list()
  diamond=list()
  diamond_row=list()
  
  events.dev=list()
  col.diamond=list()
  
  g=list()
  w=list()
  g1=list()
  
  par(font=2)
  par(xpd=T)
  analysis_data_05_14$rows=rows
  
  text_diamond=list()
  text_diamond_lb=list()
  text_diamond_ub=list()
  text_interval=list()
  text_estimates=list()
  
  Heterogeneity=c()
  for(k in 1:nrow(het_f)){
    for(i in 1:nrow(analysis_data_05_14)){
      
      if(het_f$Developer[k]==analysis_data_05_14$Developer_name[i]){
        Heterogeneity[i]=het_f$Heterogeneity[k]  
        
      }
      
      
    }
    
    
  }
  
  
  analysis_data_05_14$events=analysis_data_05_14$r1+analysis_data_05_14$r2
  analysis_data_05_14$Heterogeneity=Heterogeneity
  
  for(i in 1:max(u)){
    yy[[i]]<- analysis_data_05_14 %>%
      filter(sub==i)
    
    
    g[[i]]=max(yy[[i]]$rows)
    g1[[i]]=min(yy[[i]]$rows)
    
    events.dev[[i]] <- with(yy[[i]], tapply(events, Developer_name, sum))
    
    
    col.diamond[[i]]=ifelse(events.dev[[i]]==0,
                            "white","blue")
    
    
    
    yy1[[i]]=yy[[i]]
    
    
    if(length(yy[[i]])>1 & events.dev[[i]]!=0){
      r[[i]]=rma(ai=r1,bi=N1-r1,ci=r2,di=N2-r2,
                 data =yy[[i]],
                 drop00 = T,measure = "RR",method = het_estimate)
      
      if(model=="Fixed-effects"){
        r[[i]]=update(r[[i]],method="FE")
        
        
        
      }
      
      weights[[i]]=1/(yy[[i]]$vi+r[[i]]$tau2)
      weights[[i]]=ifelse(is.na(weights[[i]]),0.0001,weights[[i]])
      rel_weights[[i]]=weights[[i]]/sum(weights[[i]],na.rm = T)*100
      rel_weights[[i]]=format(round(rel_weights[[i]],digits = 2),nsmall=2)
      rel_weights[[i]]=paste(rel_weights[[i]],"%",sep = "")
      yy[[i]]$rel_weights=rel_weights[[i]]
      
      uu[[i]]=cbind.data.frame(yy[[i]]$oo,yy[[i]]$rel_weights)
      
    }
    
    if(length(yy[[i]])>1 & events.dev[[i]]==0){
      r[[i]]=rma(ai=r1,bi=N1-r1,ci=r2,di=N2-r2,
                 data =yy[[i]],
                 drop00 = F,measure = "RR",method = het_estimate)
      
      
      if(model=="Fixed-effects"){
        r[[i]]=update(r[[i]],method="FE")
        
        
        
      }
      
      weights[[i]]=1/(yy[[i]]$vi+r[[i]]$tau2)
      weights[[i]]=ifelse(is.na(weights[[i]]),0.0001,weights[[i]])
      rel_weights[[i]]=weights[[i]]/sum(weights[[i]],na.rm = T)*100
      rel_weights[[i]]=format(round(rel_weights[[i]],digits = 2),nsmall=2)
      rel_weights[[i]]=paste(rel_weights[[i]],"%",sep = "")
      yy[[i]]$rel_weights=rel_weights[[i]]
      
      uu[[i]]=cbind.data.frame(yy[[i]]$oo,yy[[i]]$rel_weights)
    }
    
    
    if(length(yy[[i]])==1 & events.dev[[i]]==0){
      r[[i]]=rma(ai=r1,bi=N1-r1,ci=r2,di=N2-r2,
                 data =yy[[i]],
                 drop00 = F,measure = "RR",method = het_estimate)
      
      
      if(model=="Fixed-effects"){
        r[[i]]=update(r[[i]],method="FE")
        
        
        
      }
      
      weights[[i]]=1/(yy[[i]]$vi+r[[i]]$tau2)
      weights[[i]]=ifelse(is.na(weights[[i]]),0.0001,weights[[i]])
      rel_weights[[i]]=weights[[i]]/sum(weights[[i]],na.rm = T)*100
      rel_weights[[i]]=format(round(rel_weights[[i]],digits = 2),nsmall=2)
      rel_weights[[i]]=paste(rel_weights[[i]],"%",sep = "")
      yy[[i]]$rel_weights=rel_weights[[i]]
      
      uu[[i]]=cbind.data.frame(yy[[i]]$oo,yy[[i]]$rel_weights)
    }
    
    
    if(length(yy[[i]])==1 & events.dev[[i]]>0){
      r[[i]]=rma(ai=r1,bi=N1-r1,ci=r2,di=N2-r2,
                 data =yy[[i]],
                 drop00 = T,measure = "RR",method = het_estimate)
      
      
      if(model=="Fixed-effects"){
        r[[i]]=update(r[[i]],method="FE")
        
        
        
      }
      
      weights[[i]]=1/(yy[[i]]$vi+r[[i]]$tau2)
      weights[[i]]=ifelse(is.na(weights[[i]]),0.0001,weights[[i]])
      rel_weights[[i]]=weights[[i]]/sum(weights[[i]],na.rm = T)*100
      rel_weights[[i]]=format(round(rel_weights[[i]],digits = 2),nsmall=2)
      rel_weights[[i]]=paste(rel_weights[[i]],"%",sep = "")
      yy[[i]]$rel_weights=rel_weights[[i]]
      
      uu[[i]]=cbind.data.frame(yy[[i]]$oo,yy[[i]]$rel_weights)
    }
    
    diamond_row[[i]]=min(yy1[[i]]$rows)-0.5
    
    
    par(font=2)
    text(x=-15,y=g[[i]]+0.5,yy[[i]]$Developer_name[1],cex = cex1-0.2,adj = 0)
    
    yy[[i]]=yy[[i]][complete.cases(yy[[i]]$L),] 
    
    diamond[[i]]<- ifelse(r[[i]]$k>1,r[[i]]$b,r[[i]]$yi)
    
    
    diamond_ci.lb[[i]]<- ifelse(r[[i]]$k>1,r[[i]]$ci.lb,log(as.numeric(yy[[i]]$L)))
    
    
    diamond_ci.ub[[i]]<- ifelse(r[[i]]$k>1,r[[i]]$ci.ub,log(as.numeric(yy[[i]]$U)))
    
    
    if(subgroup==T){
     # if(yy[[i]]$Developer_name!="Sinopharm-Beijing"){
      addpoly(x=diamond[[i]],
              ci.lb=diamond_ci.lb[[i]],
              ci.ub=diamond_ci.ub[[i]],
              rows=diamond_row[[i]],
              cex=1,
              col = col.diamond[[i]],
              border=col.diamond[[i]],
              annotate = F,
              atransf = exp,
              mlab = ""
      )
      
      
      text_diamond[[i]]=trimws(format(round(exp(diamond[[i]]),digits = 2),nsmall=2))  
      text_diamond_lb[[i]]=
        trimws(paste("[",format(round(exp(diamond_ci.lb[[i]]),digits = 2),nsmall=2),sep=""))  
      
      text_diamond_ub[[i]]=
        trimws(paste(format(round(exp(diamond_ci.ub[[i]]),digits = 2),nsmall=2),"]",sep="")) 
      
      text_interval[[i]]=paste(text_diamond_lb[[i]],text_diamond_ub[[i]],sep = ",")
      
      text_estimates[[i]]=paste(text_diamond[[i]],text_interval[[i]],sep=" ")
      
      text_estimates[[i]]=ifelse(events.dev[[i]]>0,text_estimates[[i]],"")
      
      
      
      diamond_row[[i]]=min(yy1[[i]]$rows)-0.7
      par(font=2)
      text(-5.1,diamond_row[[i]],text_estimates[[i]],cex =cex1)
      
      
      }
    #}
    par(font=1)
    
    
    par(font=1)
    if(model!="Fixed-effects"){
      text(x=-12.5,y=g1[[i]]-0.8,yy[[i]]$Heterogeneity[1],cex = cex1-0.15 )
      
    }
    
    
    
    
  }
  
  
  weights_all=as.data.frame(do.call(rbind, uu))
  names(weights_all)=c("oo","weights")
  weights_all$weights=ifelse(weights_all$oo=="0-0"," ",weights_all$weights)
  weights_all=weights_all$weights
  weights_all=trimws(weights_all)
  
  if(subgroup==T){
    for(k in 1:length(rows)){
      
      text(-6.1,rows[k],weights_all[k],cex = 0.8)
      
    }
  }
  
  
  today <- Sys.Date()
  today=format(today, format="%m %d %Y")
  
  
  a1=paste("Forest plot produced at:"," ",today,sep="")
  a2=paste("Data source: the COVID-NMA initiative (covid-nma.com)")
  
  
  
  af=paste(a1,a2,sep ="\n")
  if(nrow(analysis_data_05_14)>8.5){
    legend(-0.1,-6,legend=af,
           ncol=1,cex=0.75,box.lty = 0,text.col = "blue")
    
    
  }else{
    legend(-0.1,-4,legend=af,
           ncol=1,cex=0.75,box.lty = 0,text.col = "blue")}
  
  
  par(font=2)
  text(-2.5,-1.4,"Vaccine better",
       pos=4,
       cex=cex1-0.1)
  text(0.5,-1.4,"No vaccine better",
       pos=4,
       cex=cex1-0.1)
  
  
  
  
  count=sum(str_count(analysis_data_05_14$F_U_days, "\\*"),na.rm = TRUE)
  
  if(is.na(count)==TRUE){
    count=0}
  
  if(count>0){
    text(-6.5, -0.2, "(* Median follow-up)", pos=4, cex=0.75)}
  
  if(count==0) {
    text(-4, -0.2, "", pos=4, cex=0.6)
  }
  
  if(type=="Non replicating viral vector"){
    text(-15, -0.2, "Sadoff J, 2021b: Results from the FDA briefing report", pos=4, cex=0.7)
    
  }
  
  if(type=="RNA based vaccine"){
    text(-15, -0.2, "Speich B, 2022: Immunocompromised participants (PLWH and SOTR)", pos=4, cex=0.7)
    
  }
  
  lines(c(0,0),c(-2,(max(rows)+1)),lty=3)
  # dev.off()
}
