forest_Conf_Sympt=function(data,type,slider=0,subgroup=T,model="Random-effects",het_estimate="REML",
                           high_RoB=FALSE,Publication=FALSE){
                    
  data=subset(data,
              Trial_ID!=66 & Trial_ID!=5015 & Trial_ID!= 47 & Trial_ID!= 67 & Trial_ID!= 73 
              & Trial_ID!= 78)
  
  data$complement=trimws(data$complement)
  
  
  data$comp_type=ifelse(is.na(data$comp_type),"regular",data$comp_type)

    data=data %>%
    filter(data$comp_type=="regular")
  
  
    data$Type_publication=trimws(data$Type_publication)
data$Efficacy_median_followup=ifelse(!is.na(data$Efficacy_median_followup),
                                     paste(data$Efficacy_median_followup,"*",sep=""),"")

data$F_U_months=as.character(data$Efficacy_median_followup)
data$F_U_days=ifelse(is.na(data$Conf_sympt_timepoint),data$F_U_months,as.numeric(data$Conf_sympt_timepoint))
data$Start_F_U_days=as.character(data$Conf_sympt_time_zero)
data$n_Randomized=as.numeric(data$Conf_sympt_denom)
data$Treat_Type=trimws(data$Treat_Type)
data$Developer_name=trimws(data$Developer_name)

data$Developer_name=ifelse(data$Developer_name=="BioNTech SE, Pfizer","Pfizer/BioNTech+Fosun Pharma",
                           data$Developer_name)

data$Developer_name=ifelse(startsWith(data$Developer_name,"EpiVac"),"EpiVac",data$Developer_name)

data$Developer_name=ifelse(data$Developer_name=="Gamaleya Research Institute of Epidemiology and Microbiology",
                           "Gamaleya Research Institute (Sputnik V)",data$Developer_name)


data$Developer_name=ifelse(data$Developer_name=="AstraZeneca + University of Oxford",
                           "University of Oxford/AstraZeneca",data$Developer_name)


data$Developer_name=ifelse(data$Developer_name=="Sinovac Research and Development Co., Ltd",
                           "Sinovac",data$Developer_name)

data$Developer_name=ifelse(data$Developer_name=="Sinopharm - China National Biotec Group Company Limited-Wuhan",
                           "Sinopharm-Wuhan",data$Developer_name)

data$Developer_name=ifelse(data$Developer_name=="Sinopharm - China National Biotec Group Company Limited-Beijing",
                           "Sinopharm-Beijing",data$Developer_name)


data$status=rep(NA,nrow(data))

data$status=ifelse(is.na(data$Sympt_Vaccine_efficacy_LCI),"Incomplete data",data$status)

data$status=ifelse(is.na(data$Sympt_Vaccine_efficacy_UCI),"Incomplete data",data$status)

data$status=ifelse(is.na(data$status),"Complete data",data$status)



data$Treat_Name=trimws(data$stats_name)
data$stats_name=trimws(data$stats_name)
data$Treat_Name=data$stats_name
data$Trial_ID=trimws(data$Trial_ID)
data$First_author=trimws(data$First_author)
data$First_author=ifelse(is.na(data$First_author),"",data$First_author)
data$name_study=trimws(data$Trial_name)
data$First_author=ifelse(is.na(data$First_author),data$Trial_name,data$First_author)
data$Year=trimws(data$Year)
data$Research_question=trimws(data$Treat_Type)



rct_data=data

rct_data$Confirmed_symptomatic=as.numeric(rct_data$Confirmed_symptomatic)
pair_data=pairwise_old(studlab=Trial_ID,treat=Treat_Name,event=Confirmed_symptomatic,n=Conf_sympt_denom,
                   data=rct_data,measure="RR",ref="Standard care")
pair_data$F_U_days1=pair_data$F_U_days
pair_data$First_author=pair_data$First_author

ncomp=length(pair_data$treat1) 


if(type %!in% c("Inactivated virus","all","Protein subunit","RNA based vaccine","Non replicating viral vector","Virus-Like particle")){
pair_data$Conf_sympt1=as.numeric(pair_data$Sympt_Vaccine_efficacy_perc1)
pair_data$Conf_sympt_CI_Lower1=as.numeric(pair_data$Sympt_Vaccine_efficacy_LCI1)
pair_data$Conf_sympt_CI_Upper1=as.numeric(pair_data$Sympt_Vaccine_efficacy_UCI1)

}

if(type %in% c("all","Protein subunit","RNA based vaccine","Non replicating viral vector","Inactivated virus","Virus-Like particle")){
  pair_data$Conf_sympt1=as.numeric(pair_data$Sympt_Vaccine_efficacy_perc)
  pair_data$Conf_sympt_CI_Lower1=as.numeric(pair_data$Sympt_Vaccine_efficacy_LCI)
  pair_data$Conf_sympt_CI_Upper1=as.numeric(pair_data$Sympt_Vaccine_efficacy_UCI)
  
}


Intervention1=rep("NA",ncomp)
Intervention2=rep("NA",ncomp)     
Intervention1=ifelse(pair_data$Intervention_control1=="Intervention" | pair_data$Intervention_control1=="Intervention/control",pair_data$treat1,Intervention1)
Intervention1=ifelse(pair_data$Intervention_control1=="Control",pair_data$treat2,Intervention1)
Intervention2=ifelse(Intervention1==pair_data$treat1,pair_data$treat2,Intervention2)
Intervention2=ifelse(Intervention1==pair_data$treat2,pair_data$treat1,Intervention2)
Intervention1=trimws(Intervention1)
Intervention2=trimws(Intervention2)
pair_data$treat1=trimws(pair_data$treat1)
pair_data$treat2=trimws(pair_data$treat2)
Comparison=str_c(Intervention1, Intervention2, sep = " vs ", collapse = NULL)

r1=rep(NA,ncomp)
r2=rep(NA,ncomp)
N1=rep(NA,ncomp)
N2=rep(NA,ncomp)
N1=ifelse(pair_data$treat1==Intervention1,pair_data$n1,N1)
N1=ifelse(pair_data$treat2==Intervention1,pair_data$n2,N1)
r1=ifelse(pair_data$treat1==Intervention1,pair_data$event1,r1)
r1=ifelse(pair_data$treat2==Intervention1,pair_data$event2,r1)
N2=ifelse(pair_data$treat2==Intervention2,pair_data$n2,N2)
N2=ifelse(pair_data$treat1==Intervention2,pair_data$n1,N2)
r2=ifelse(pair_data$treat2==Intervention2,pair_data$event2,r2)
r2=ifelse(pair_data$treat1==Intervention2,pair_data$event1,r2)
pair_data$Intervention1=Intervention1
pair_data$Intervention2=Intervention2
pair_data$Comparison=Comparison




##########################################################################


#codes for comparisons#

##### comp
comp=rep(0,ncomp)

sev=rep(0,ncomp)


pair_data$comp=comp
pair_data$N1=N1
pair_data$r1=r1
pair_data$N2=N2
pair_data$r2=r2


pair_data1=as.data.frame(pair_data)

class(pair_data1)=class(pair_data1)[-2]
#new_data=subset(pair_data, comp>0)
if(type %!in% c("all","booster")){
new_data=pair_data1 %>%
  filter(Treat_Type1==type)
}

if(type %in% c("all","booster")){
  new_data=pair_data1 
}

if(type %in% c("all","Protein subunit","booster","RNA based vaccine","Non replicating viral vector","Inactivated virus","Virus-Like particle")){
  
  new_data$Sympt_Vaccine_efficacy_UCI1=new_data$Sympt_Vaccine_efficacy_UCI
  new_data$Sympt_Vaccine_efficacy_LCI1=new_data$Sympt_Vaccine_efficacy_LCI
  new_data$Sympt_Vaccine_efficacy_perc1=new_data$Sympt_Vaccine_efficacy_perc
  
}

if(type %in% c("Inactivated virus","Protein subunit","RNA based vaccine","booster","Non replicating viral vector")){
  
  new_data$Start_F_U_days1=new_data$Start_F_U_days
  
}


analysis_data_09=cbind.data.frame(Trial_ID=new_data$Trial_ID,First_author=new_data$First_author,
                                  F_U_days=new_data$F_U_days,F_U_months=new_data$F_U_months,Start=new_data$Start_F_U_days,
                                  Intervention1=new_data$Intervention1,
                                  Intervention2=new_data$Intervention2,N1=new_data$N1,r1=new_data$r1,
                                  N2=new_data$N2,r2=new_data$r2,
                                  Comparison=new_data$Comparison,
                                  severity=new_data$Research_question1,comp=new_data$comp,
                                  percentage=new_data$Sympt_Vaccine_efficacy_perc1,LCI=new_data$Sympt_Vaccine_efficacy_LCI1,
                                  UCI=new_data$Sympt_Vaccine_efficacy_UCI1,
                                  Year=new_data$Year,
                                  A=as.character(new_data$ROB_1_randomization),
                                  B=as.character(new_data$ROB2_Confirmed_symptomatic),
                                  C=as.character(new_data$ROB3_Confirmed_symptomatic),
                                  D=as.character(new_data$ROB4_Confirmed_symptomatic),
                                  E=as.character(new_data$ROB5_Confirmed_symptomatic),
                                  Overall=as.character(new_data$ROB6_Confirmed_symptomatic),
                                  Conflicts_of_interest=new_data$Conflict_of_interest_category,
                                  Funding=new_data$Funding,
                                  Countries=new_data$Countries,
                                  Developer_name=new_data$Developer_name,
                                  Type=new_data$Treat_Type1,
                                  status=new_data$status1,
                                  Publication=new_data$Type_publication,
                                  complement=new_data$complement
                               
)

analysis_data_09=as.data.frame(analysis_data_09)

exclude_preprint<- analysis_data_09 %>%
  filter(Publication=="Published")

if(Publication==TRUE){
  analysis_data_09<-exclude_preprint
  }


if(Publication==FALSE){
  analysis_data_09<-analysis_data_09}


exclude_rob<- analysis_data_09 %>%
  filter(analysis_data_09$Overall=="High")

exclude_rob1<- analysis_data_09 %>%
  filter(analysis_data_09$Overall %in% c("High","Some concerns") )

rob=setdiff(analysis_data_09$First_author,exclude_rob$First_author)

rob1=setdiff(analysis_data_09$First_author,exclude_rob1$First_author)

if(high_RoB=="High RoB"){
  analysis_data_09<-subset(analysis_data_09,analysis_data_09$First_author %in% rob)}


if(high_RoB=="High RoB/Some concerns"){
  analysis_data_09<-subset(analysis_data_09,analysis_data_09$First_author %in% rob1)}


if(high_RoB=="No exclusion"){
  analysis_data_09<-analysis_data_09}


non_arrange=NA




if(type!="Non replicating viral vector"){
analysis_data_09=analysis_data_09 %>%
  group_by(Developer_name) %>%
  arrange(Developer_name,.by_group=T)
}else{
  
  non_arrange=ifelse(analysis_data_09$Developer_name=="AstraZeneca+University of Oxford",1,non_arrange)
  non_arrange=ifelse(analysis_data_09$Developer_name=="Janssen Pharmaceutical Companies",2,non_arrange)
  non_arrange=ifelse(analysis_data_09$Developer_name=="Gamaleya Research Institute (Sputnik V)",3,non_arrange)
  analysis_data_09$non_arrange=non_arrange
  
  analysis_data_09=analysis_data_09 %>%
    arrange(desc(non_arrange),.by_group=T)
  
}





analysis_data_09$rate=analysis_data_09$percentage/100

analysis_data_09$rate1=analysis_data_09$rate


analysis_data_09$UCI_rate=(analysis_data_09$UCI/100)


analysis_data_09$LCI_rate=(analysis_data_09$LCI/100)



analysis_data_09$LCI_rate1=analysis_data_09$LCI_rate


analysis_data_09$rate=analysis_data_09$rate1
analysis_data_09$se=(analysis_data_09$UCI_rate-analysis_data_09$LCI_rate1)/3.92

analysis_data_09=analysis_data_09[complete.cases(analysis_data_09$UCI), ]


analysis_data_09=as.data.frame(analysis_data_09)





################# New code
analysis_data_09$vi=analysis_data_09$se*analysis_data_09$se

analysis_data_09$yi=analysis_data_09$rate
id=seq(1:nrow(analysis_data_09))
analysis_data_09$id=id

u=unique(analysis_data_09$Developer_name)
sub=NA


for(i in 1:length(u)){
  for(k in 1:nrow(analysis_data_09)){
    
    sub[k]=ifelse(analysis_data_09$Developer_name[k]==u[i],i,sub[k])
    
  }
  
}

analysis_data_09$sub=as.numeric(sub)



###############################

analysis_data_09$counts1=paste(analysis_data_09$r1,analysis_data_09$N1,sep = "/")
analysis_data_09$counts2=paste(analysis_data_09$r2,analysis_data_09$N2,sep = "/")

#analysis_data_09=analysis_data_09[order(analysis_data_09$sev,decreasing=TRUE),]

Ef=paste(format(round(analysis_data_09$percentage,digits = 2),
                nsmall=2),"% ",sep="")
Ef1=paste("[",format(round(analysis_data_09$LCI,digits = 2),
                     nsmall=2),"%",sep = "") 
Ef2=paste(",",format(round(analysis_data_09$UCI,digits = 2),
                     nsmall=2),"%","]",sep="") 
Ef_final=paste(Ef,Ef1,Ef2,sep="")
analysis_data_09$Ef_final=Ef_final



ilab=cbind(Start=analysis_data_09$Start,F_U_days=as.character(analysis_data_09$F_U_days),Intervention1=as.character(analysis_data_09$Intervention1),
           Intervention2=as.character(analysis_data_09$Intervention2),
           counts1=analysis_data_09$counts1,counts2=analysis_data_09$counts2,Efficacy=analysis_data_09$Ef_final)
ilab=as.data.frame(ilab)

if(model!="Fixed-effects"){
  meta=metagen(TE=yi,seTE=sqrt(vi),byvar = Developer_name,data = analysis_data_09,method.tau = het_estimate)
}else{
  meta=metagen(TE=yi,seTE=sqrt(vi),byvar = Developer_name,data = analysis_data_09,comb.fixed = T,comb.random = F)
}

slab=paste(analysis_data_09$First_author,analysis_data_09$Year,sep=", ")

slab=ifelse(is.na(analysis_data_09$complement),slab,paste(slab,analysis_data_09$complement,sep = ""))

#######################
if(nrow(analysis_data_09)>1){
res <- rma.mv(yi, vi, random = ~ 1 | as.factor(sub), data =analysis_data_09,
              slab=slab)



dd <- c(0,diff(analysis_data_09$sub))


dd[dd > 0] <- 1
rows <- (1:res$k) + cumsum(dd)
}else{

  res=rma(yi, vi, data=analysis_data_09,
                slab=slab)
  rows=c(1:res$k)
  
}



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

ind_het=NA

ind_het=ifelse(is.na(I2),0,1)

het=c()
g1=list()
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



data_rob=data.frame(Study=slab,
                    D1=analysis_data_09$A,
                    D2=analysis_data_09$B,
                    D3=analysis_data_09$C,
                    D4=analysis_data_09$D,
                    D5=analysis_data_09$E,
                    Overall=analysis_data_09$Overall
)

result_09=rma(yi, vi, data=analysis_data_09,
              slab=slab)

analysis_data_09$rate_ratio=1-analysis_data_09$yi
analysis_data_09$rate_ratio=ifelse(analysis_data_09$rate_ratio==0,0.003,analysis_data_09$rate_ratio)
analysis_data_09$rate_ratio=log(analysis_data_09$rate_ratio)

analysis_data_09$rr_LCI=1-analysis_data_09$UCI_rate
analysis_data_09$rr_LCI=ifelse(analysis_data_09$rr_LCI==0,0.001,analysis_data_09$rr_LCI)
analysis_data_09$rr_LCI=log(analysis_data_09$rr_LCI)

analysis_data_09$rr_UCI=1-analysis_data_09$LCI_rate
analysis_data_09$rr_UCI=log(analysis_data_09$rr_UCI)


analysis_data_09$se_rr=(analysis_data_09$rr_UCI-analysis_data_09$rr_LCI)/3.92
analysis_data_09$vi_rr=analysis_data_09$se_rr*analysis_data_09$se_rr


result_091=rma(rate_ratio,vi_rr, data=analysis_data_09,
              slab=slab)

if(subgroup==F){
ilab.xpos=c(-6.5,-5.8,-4.8,-3.9,-3.1,-2.5,-1.2)
}else{
  ilab.xpos=c(-6.5,-5.8,-4.8,-3.9,-3.1,-2.5,-0.9)
}
#metafor::forest
rob_append_to_forest(res,#atransf=exp,
                     rob_data = data_rob,
                     type="Efficacy",
                     rob_caption = F,
                     rob_legend = F,
                     rob_psize = 1.5,
                     showweights=F, xlab="",
                     # header=c(),
                     cex=0.8,mlab="",
                     col="white",border="white",xlim=c(-8,3.5),alim=c(floor(result_09$ci.lb)-2,ceiling(result_09$ci.ub))+1,steps=4,
                     
                     ilab=ilab,ilab.xpos=ilab.xpos,rows=rows,ylim=c(-1.5,3+max(rows)),
                     psize=1.1,
                     xaxt="n",
                     colout="white",
                     annotate=F,
                     olim=c(0,1),
                     refline=NA)




axis(
  1,at = c(0,0.5,1),
  labels = c(0, 50, 100),
  cex.axis = 0.7
)



par.usr <- par("usr")



segments(x0=1, y0=par.usr[3],
         #  -2.7,
         x1 = 1, y1 = max(rows)+1,
         lty="dotted")

segments(x0=0, y0=par.usr[3],
         #  -2.7,
         x1 = 0, y1 = max(rows)+1,
         lty="dotted")

par(font=2)
xlab="Vaccine Efficacy"
lmtext    <- function(..., textpos, clim, rowadj) mtext(...)

lmtext(xlab, side=1, at=0.5, line=par("mgp")[1]-0.5, cex=0.6)
par(font=1)

if(subgroup==F){
header1=c("N-days\nafter dose","Follow-up\nmonths","Intervention 1","Intervention 2","r1/N1","r2/N2","Vaccine Efficacy [95% CI]")
}else{
  header1=c( "N-days\nafter dose","Follow-up\nmonths","Intervention 1","Intervention 2","r1/N1","r2/N2","Weights(%)","Vaccine Efficacy [95% CI]")
}

header=header1

header3=c("(days-dose)")


Heterogeneity=c()

for(k in 1:nrow(het_f)){
  for(i in 1:nrow(analysis_data_09)){
    
    if(het_f$Developer[k]==analysis_data_09$Developer_name[i]){
      Heterogeneity[i]=het_f$Heterogeneity[k]  
      
    }
    
    
  }
  
  
}


analysis_data_09$Heterogeneity=Heterogeneity


yy=list()


u=unique(analysis_data_09$sub)
fake_list=list()

r=list()

ff=which(diff(rows)>1)+c(1.3,2)



diamond_row=list()

diamond_ci.lb=list()
diamond_ci.ub=list()
diamond=list()

new_diamond=list()
new_diamond_lb=list()
new_diamond_ub=list()
per1=list()
lower1=list()
upper1=list()
int=list()
estimates=list()
seg=list()
lower_segments=list()
upper_segments=list()

w=list()

g=list()
g1=list()


par(font=2)
par(xpd=T)
analysis_data_09$rows=rows

for(i in 1:max(u)){
yy[[i]]<- analysis_data_09 %>%
  filter(sub==i)


r[[i]]=rma(yi=rate_ratio,vi=vi_rr,data =yy[[i]],method = het_estimate)

if(model=="Fixed-effects"){
  r[[i]]=update(r[[i]],method="FE")
}

w[[i]]=format(round(weights(r[[i]]),digits = 2),nsmall=2)
w[[i]]=paste(w[[i]],"%",sep = "")


diamond[[i]]<- ifelse(r[[i]]$k>1,1-exp(r[[i]]$b),yy[[i]]$rate)

diamond_ci.lb[[i]]<- ifelse(r[[i]]$k>1,1-exp(r[[i]]$ci.ub),yy[[i]]$LCI_rate)
diamond_ci.ub[[i]]<- ifelse(r[[i]]$k>1,1-exp(r[[i]]$ci.lb),yy[[i]]$UCI_rate)

diamond[[i]]=ifelse(diamond[[i]]>1,1,diamond[[i]])
diamond_ci.ub[[i]]=ifelse(diamond_ci.ub[[i]]>1,1,diamond_ci.ub[[i]])
diamond_ci.lb[[i]]=ifelse(diamond_ci.lb[[i]]<0,0,diamond_ci.lb[[i]])

diamond_row[[i]]=min(yy[[i]]$rows)-0.5+slider

if(subgroup==T){
addpoly(x=diamond[[i]],
        ci.lb=diamond_ci.lb[[i]],
        ci.ub=diamond_ci.ub[[i]],
        rows=diamond_row[[i]],
        cex=0.8,
        col = "blue",
        border="blue",
        annotate = F
)
  
  
  diamond[[i]]=ifelse(diamond[[i]]>1,1,diamond[[i]])
  diamond_ci.ub[[i]]=ifelse(diamond_ci.ub[[i]]>1,1,diamond_ci.ub[[i]])
  diamond_ci.lb[[i]]=ifelse(diamond_ci.lb[[i]]<0,0,diamond_ci.lb[[i]])

new_diamond[[i]]=trimws(format(round(diamond[[i]]*100,digits = 2),nsmall=2))
new_diamond_lb[[i]]=trimws(format(round(diamond_ci.lb[[i]]*100,digits = 2),nsmall=2))
new_diamond_ub[[i]]=trimws(format(round(diamond_ci.ub[[i]]*100,digits = 2),nsmall=2))

per1[[i]]=paste(new_diamond[[i]],"%",sep="")

lower1[[i]]=paste("[",new_diamond_lb[[i]],"%",sep = "")

upper1[[i]]=paste(new_diamond_ub[[i]],"%","]",sep = "")

int[[i]]=paste(lower1[[i]],upper1[[i]],sep=", ")




estimates[[i]]=paste(per1[[i]],int[[i]],sep=" ")


text(x=-2.1,y=yy[[i]]$rows,w[[i]],pos = 4,cex=0.75)
if(subgroup==F){
  text(x=-1.2,y=diamond_row[i],estimates[[i]],cex=0.85)
}else{
  text(x=-0.9,y=diamond_row[i],estimates[[i]],cex=0.85)
}

}
g[[i]]=max(yy[[i]]$rows)
g1[[i]]=min(yy[[i]]$rows)
par(font=2)
text(x=-8,y=g[[i]]+0.5,yy[[i]]$Developer_name[1],cex = 0.8,adj = 0)
par(font=1)
if(model!="Fixed-effects"){
  text(x=-6.4,y=g1[[i]]-0.8,yy[[i]]$Heterogeneity[1],cex = 0.8)
}

arrows(x0=yy[[i]]$LCI_rate,x1=yy[[i]]$UCI_rate,
       y0=yy[[i]]$rows,
       yi=yy[[i]]$rows,code = 3,
       angle=90,length=0.02)

points(x=yy[[i]]$rate,y=yy[[i]]$rows,pch=15)



}



par(font=2)
if(subgroup==F){
text(c(-6.5,-5.8,-4.8,-3.8,-3.1,-2.6,-1.2),max(rows)+2,header,cex=0.75)
}else{
  text(c(-6.5,-5.8,-4.8,-3.8,-3.1,-2.6,-1.9,-0.8),max(rows)+2,header,cex=0.75)
}

par(font=1)

legend(-8,-0.1,adj=c(0, 0.5),legend=c("Low Risk of Bias","Some Concerns","High Risk of Bias"),ncol=1,
       col=c("chartreuse3","gold","red"),fill=c("chartreuse3","gold","red")
       ,title = "Risk of bias ratings:",border = c("chartreuse3","gold","red"), cex=0.6
)
legend(-6.5, -0.1,legend=c("A: Bias due to randomization","B: Bias due to deviation from intended intervention",
                           "C: Bias due to missing data","D: Bias due to outcome measurement","E: Bias due to selection of reported result"),
       ncol=1,cex=0.6,title = "Risk of Bias Domains:",box.lty = 2)



text(-3.4,3.5+max(rows),"Confirmed symptomatic COVID-19 after complete vaccination",
     pos=4,
     cex=1)


par(font=1)

text(-2.7,3+max(rows),paste("Type:",type,sep=" "),
     pos=4,
     cex=1)


today <- Sys.Date()
today=format(today, format="%m %d %Y")


a1=paste("Forest plot produced at:"," ",today,sep="")
a2=paste("Data source: the COVID-NMA initiative (covid-nma.com)")

af=paste(a1,a2,sep ="\n")
legend("bottomright",legend=af,
       ncol=1,cex=0.7,box.lty = 0,text.col = "blue")




count=sum(str_count(analysis_data_09$F_U_days, "\\*"),na.rm = TRUE)

if(is.na(count)==TRUE){
  count=0}

if(count>0){
  text(-2, -0.2, "(* Median follow-up)", pos=4, cex=0.75)}

if(count==0) {
  text(-2, -0.2, "", pos=4, cex=0.6)
}

if(type=="Inactivated virus"){
  
  text(-3, -0.6, "Tanriover M, 2021: Hospitalized (moderate or severe)", pos=4, cex=0.75)
}

if(type=="Non replicating viral vector"){
  
  text(-3, -0.6, "Logunov D, 2021: Vaccine efficacy reported as (1-OR)x100%", pos=4, cex=0.75)
  
  text(-3, -1, "MenACWY: Quadrivalent meningococcal conjugate vaccine", pos=4, cex=0.75)
  
}

lines(c(0.5,0.5),c(-2,(max(rows)+1)),lty=3)

}

