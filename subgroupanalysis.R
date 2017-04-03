#Subgroup analyses
require(Rmisc)

#function to reduce the amount of coding to create dataframes
df.function<-function(df, treat){
  df$treat[ecogain==1] <- "ecogain"
  df$treat[ecoloss==1] <- "ecoloss"
  df$treat[econgain==1] <- "econgain"
  df$treat[econloss==1] <- "econloss"
  df$treat[control==1] <- "control"
  
  return(df)
}

#Party ID
reps_df <- subset(kuni_df, partyid==1)
attach(reps_df)
reps_df<-df.function(reps_df, treat=treat_reps)
support_reps <- as.numeric(project_support==1)
chisq.test(support_reps, treat_reps) #not significant
p.adjust(.3825, method="bonferroni", n=3)


dems_df <- subset(kuni_df, partyid==2)
attach(dems_df)
dems_df<-df.function(dems_df, treat=treat_dems)
support_dems <- as.numeric(project_support==1)
chisq.test(support_dems, treat_dems) 
p.adjust(.00001291, method="bonferroni", n=3)


inds_df <- subset(kuni_df, partyid==3)
attach(inds_df)
inds_df<-df.function(inds_df, treat=treat_inds)
support_inds <- as.numeric(inds_df$project_support==1)
chisq.test(support_inds, treat_inds)
summary(project_support)


summary(logit_reps <- glm(support_reps ~ as.factor(treat_reps), data=reps_df, family=binomial(link="logit"))) 
summary(logit_dems <- glm(support_dems ~ as.factor(treat_dems), data=dems_df, family=binomial(link="logit"))) 
summary(logit_inds <- glm(support_inds ~ as.factor(treat_inds), data=inds_df, family=binomial(link="logit"))) 


#FOR ECOLOGICAL
ecosupport_func<-function(support, treat){
  ecosupport<-mean(support[treat=="ecogain" | treat=="ecoloss"])
  control<-mean(support[treat=="control"])
  N<-length(support[treat=="ecogain" | treat=="ecoloss"])
  se<-(sd(support[treat=="ecogain" | treat=="ecoloss"]))/sqrt(N) 
  ATE<-ecosupport-control
  ci<-ATE + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/N)*ATE*(1-ATE))
  
  return(list(ecosupport=ecosupport, control=control, N=N, se=se, ATE=ATE, ci=ci))
}

dem_ecosupport<-ecosupport_func(support=support_dems, treat=treat_dems)
dem_ecosupport

rep_ecosupport<-ecosupport_func(support=support_reps, treat=treat_reps)
rep_ecosupport

ind_ecosupport<-ecosupport_func(support=support_inds, treat=treat_inds)
ind_ecosupport

#FOR ECONOMIC
econsupport_func<-function(support, treat){
  econsupport<-mean(support[treat=="econgain" | treat=="econloss"])
  control<-mean(support[treat=="control"])
  N<-length(support[treat=="econgain" | treat=="econloss"])
  se<-(sd(support[treat=="econgain" | treat=="econloss"]))/sqrt(N) 
  ATE<-econsupport-control
  ci<-ATE + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/N)*ATE*(1-ATE))
  
  return(list(econsupport=econsupport, control=control, N=N, se=se, ATE=ATE, ci=ci))
}

dem_econsupport<-econsupport_func(support=support_dems, treat=treat_dems)
rep_econsupport<-econsupport_func(support=support_reps, treat=treat_reps)
ind_econsupport<-econsupport_func(support=support_inds, treat=treat_inds)


#Environmentalism
quantile(NEP, na.rm=T) #25%=2.6, 75%=3.8
enviro_df <- subset(kuni_df, NEP>=3.8)
nonenviro_df <- subset(kuni_df, NEP<=2.6)

attach(enviro_df)
enviro_df$treat_enviro[ecogain==1] <- "ecogain"
enviro_df$treat_enviro[ecoloss==1] <- "ecoloss"
enviro_df$treat_enviro[econgain==1] <- "econgain"
enviro_df$treat_enviro[econloss==1] <- "econloss"
enviro_df$treat_enviro[control==1] <- "control"
attach(enviro_df)
support_enviro <- as.numeric(project_support==1)
summary(project_support)
summary(logit_enviro <- glm(support_enviro ~ as.factor(treat_enviro), data=enviro_df, family=binomial(link="logit"))) 
#eco-loss positive, sig
#eco-gain positive but marginal
#econ are both negative but non-sig

#FOR ENVIRO/ECONOMIC
mean(support_enviro[treat_enviro=="econgain" | treat_enviro=="econloss"])
mean(support_enviro[treat_enviro=="control"])
length(support_enviro[treat_enviro=="econgain" | treat_enviro=="econloss"])
.002 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/146)*.002*(1-.002))
#ATE=-.002, N=214, 95% CI is -.009 - .005

mean(support_nonenviro[treat_nonenviro=="econgain" | treat_reps=="econloss"], na.rm=T)
mean(support_nonenviro[treat_nonenviro=="control"], na.rm=T)
length(support_nonenviro[treat_nonenviro=="econgain" | treat_nonenviro=="econloss"])
.161 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/108)*.161*(1-.161))
#ATE=.161, N=108, 95% CI is .092 - .230

#FOR ENVIRO/ECOLOGICAL
mean(support_enviro[treat_enviro=="ecogain" | treat_enviro=="ecoloss"])
mean(support_enviro[treat_enviro=="control"])
length(support_enviro[treat_enviro=="ecogain" | treat_enviro=="ecoloss"])
.176 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/124)*.176*(1-.176))
#ATE=.176, N=124, 95% CI is .109 - .243

mean(support_nonenviro[treat_nonenviro=="ecogain" | treat_reps=="ecoloss"], na.rm=T)
mean(support_nonenviro[treat_nonenviro=="control"], na.rm=T)
length(support_nonenviro[treat_nonenviro=="ecogain" | treat_nonenviro=="ecoloss"])
.230 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/131)*.230*(1-.230))
#ATE=.230, N=108, 95% CI is .158 - .302


mean(support_inds[treat_inds=="econgain" | treat_inds=="econloss"])
mean(support_inds[treat_inds=="control"])
length(support_inds[treat_inds=="econgain" | treat_inds=="econloss"])
.113 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/85)*(.113)*(1-.113))
#ATE=-.113, N=85, 95% CI is -.1803 to -.0457


##GAIN AND LOSS##

#PARTY ID

#FOR GAIN
mean(support_dems[treat_dems=="ecogain" | treat_dems=="econgain"]) - mean(support_dems[treat_dems=="control"])
length(support_dems[treat_dems=="ecogain" | treat_dems=="econgain"])
.14155 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/208)*.14155*(1-.14155))
#ATE=.142, N=208, 95% CI is .094 - .189

mean(support_reps[treat_reps=="ecogain" | treat_reps=="econgain"]) - mean(support_reps[treat_reps=="control"])
length(support_reps[treat_reps=="ecogain" | treat_reps=="econgain"])
.075 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/120)*.075*(1-.075))
#ATE=.075, N=120, 95% CI is .028 - .122

mean(support_inds[treat_inds=="ecogain" | treat_inds=="econgain"]) - mean(support_inds[treat_inds=="control"])
length(support_inds[treat_inds=="ecogain" | treat_inds=="econgain"])
.04276 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/93)*.04276*(1-.04276))
#ATE=-.043, N=93, 95% CI is -.084 to -.002

#FOR LOSS
mean(support_dems[treat_dems=="ecoloss" | treat_dems=="econloss"]) - mean(support_dems[treat_dems=="control"])
length(support_dems[treat_dems=="ecoloss" | treat_dems=="econloss"])
(sd(support_dems[treat_dems=="ecogain" | treat_dems=="ecoloss"]))/sqrt(193) #SE=.0284
.24344 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/199)*.24344*(1-.24344))
#ATE=.243, N=199, 95% CI is .184 - .303

mean(support_reps[treat_reps=="ecoloss" | treat_reps=="econloss"]) - mean(support_reps[treat_reps=="control"])
length(support_reps[treat_reps=="ecoloss" | treat_reps=="econloss"])
.136612 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/122)*.136612*(1-.136612))
#ATE=.137, N=122, 95% CI is .076 - .198

mean(support_inds[treat_inds=="ecoloss" | treat_inds=="econloss"]) - mean(support_inds[treat_inds=="control"])
length(support_inds[treat_inds=="ecoloss" | treat_inds=="econloss"])
.02371 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/92)*(.02371)*(1-.02371))
#ATE=-.024, N=92, 95% CI is -.055 to -.007

#Environmentalism
quantile(NEP, na.rm=T) #25%=2.6, 75%=3.8
enviro_df <- subset(kuni_df, NEP>=3.8)
nonenviro_df <- subset(kuni_df, NEP<=2.6)

attach(enviro_df)
enviro_df$treat_enviro[ecogain==1] <- "ecogain"
enviro_df$treat_enviro[ecoloss==1] <- "ecoloss"
enviro_df$treat_enviro[econgain==1] <- "econgain"
enviro_df$treat_enviro[econloss==1] <- "econloss"
enviro_df$treat_enviro[control==1] <- "control"
attach(enviro_df)
support_enviro <- as.numeric(project_support==1)
summary(project_support)
summary(logit_enviro <- glm(support_enviro ~ as.factor(treat_enviro), data=enviro_df, family=binomial(link="logit"))) 
#eco-loss positive, sig
#eco-gain positive but marginal
#econ are both negative but non-sig

#FOR GAIN
mean(support_enviro[treat_enviro=="ecogain" | treat_enviro=="econgain"]) - mean(support_enviro[treat_enviro=="control"])
length(support_enviro[treat_enviro=="ecogain" | treat_enviro=="econgain"])
.045 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/136)*.045*(1-.045))
#ATE=.045, N=136, 95% CI is .010 - .080

mean(support_nonenviro[treat_nonenviro=="ecogain" | treat_reps=="econgain"], na.rm=T) - mean(support_nonenviro[treat_nonenviro=="control"], na.rm=T)
length(support_nonenviro[treat_nonenviro=="ecogain" | treat_nonenviro=="econgain"])
.19627 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/124)*.19627*(1-.19627))
#ATE=.196, N=124, 95% CI is .126 - .266

#FOR LOSS
mean(support_enviro[treat_enviro=="ecoloss" | treat_enviro=="econloss"]) - mean(support_enviro[treat_enviro=="control"])
length(support_enviro[treat_enviro=="ecoloss" | treat_enviro=="econloss"])
.115 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/134)*.115*(1-.115))
#ATE=.115, N=134, 95% CI is .061 - .169

mean(support_nonenviro[treat_nonenviro=="econloss" | treat_reps=="ecoloss"], na.rm=T) - mean(support_nonenviro[treat_nonenviro=="control"], na.rm=T)
length(support_nonenviro[treat_nonenviro=="econloss" | treat_nonenviro=="ecoloss"])
.2502 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/115)*.2502*(1-.2502))
#ATE=.250, N=115, 95% CI is .171 - .329


mean(support_inds[treat_inds=="econgain" | treat_inds=="econloss"])
mean(support_inds[treat_inds=="control"])
length(support_inds[treat_inds=="econgain" | treat_inds=="econloss"])
.113 + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/85)*(.113)*(1-.113))
#ATE=-.113, N=85, 95% CI is -.1803 to -.0457








attach(nonenviro_df)
nonenviro_df$treat_nonenviro[ecogain==1] <- "ecogain"
nonenviro_df$treat_nonenviro[ecoloss==1] <- "ecoloss"
nonenviro_df$treat_nonenviro[econgain==1] <- "econgain"
nonenviro_df$treat_nonenviro[econloss==1] <- "econloss"
nonenviro_df$treat_nonenviro[control==1] <- "control"
attach(nonenviro_df)
support_nonenviro <- as.numeric(project_support==1)
summary(project_support)
summary(logit_nonenviro <- glm(support_nonenviro ~ as.factor(treat_nonenviro), data=nonenviro_df, family=binomial(link="logit"))) 

#Concern for Animals
quantile(animalrights, na.rm=T) #25%=2.75, 75%=4.00
animalcare_df <- subset(kuni_df, animalrights>=4.0)
noanimalcare_df <- subset(kuni_df, animalrights<=2.6)

#Urban/Rural
urban_df <- subset(kuni_df, urban_rural<=2)
rural_df <- subset(kuni_df, urban_rural==3)

#Gender
male_df <- subset(kuni_df, gender==1)
female_df <- subet(kuni_df, gender==2)


#probably don't need these
#Age
young_df <- subset(kuni_df, age<=35)
old_df <- subset(kuni_df, age>=65)

#Education
college_df <- subet(kuni_df, )