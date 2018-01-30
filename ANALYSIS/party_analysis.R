#Subgroup analyses
require(Rmisc)

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

#Among Republicans
summary(rep_overall <- support_binary[republican==1], na.rm=T) #0.764 is overall number
summary(rep_ecogain <- support_binary[treat_cat=="ecogain" & republican==1], na.rm=T) #0.793
summary(rep_ecoloss <- support_binary[treat_cat=="ecoloss" & republican==1], na.rm=T) #0.833
summary(rep_econgain <- support_binary[treat_cat=="econgain" & republican==1], na.rm=T) #0.732
summary(rep_econloss <- support_binary[treat_cat=="econloss" & republican==1], na.rm=T) #0.793
summary(rep_control <- support_binary[treat_cat=="control" & republican==1], na.rm=T) #0.660
#Ordering: eco-loss, econ-loss & eco-gain (tied), econ-gain, control

#republican data frame
m = c(mean(rep_econgain), mean(rep_econloss), mean(rep_ecogain), mean(rep_ecoloss))
ATE=c(mean(rep_econgain)-mean(rep_control), mean(rep_econloss)-mean(rep_control), 
      mean(rep_ecogain)-mean(rep_control), mean(rep_ecoloss)-mean(rep_control))
treat_cat_rep = c("Econ Gain", "Econ Loss", "Ecol Gain", "Ecol Loss")
se = c(sd(rep_econgain)/sqrt(length(rep_econgain)), 
       sd(rep_econloss)/sqrt(length(rep_econloss)), sd(rep_ecogain)/sqrt(length(rep_ecogain)),
       sd(rep_ecoloss)/sqrt(length(rep_ecoloss)))
#ci<-ATE + c(-qnorm(0.975),qnorm(0.975))*sqrt((1/N)*ATE*(1-ATE))
party_rep<-as.factor(rep(1, each=length(m)))

rep.df<-data.frame(treat_cat_rep, m, se, ATE, party_rep)
rep.df<-setNames(rep.df, c("treat_cat", "mean", "se", "ATE", "partyid"))

#Among Democrats
summary(dem_overall <- support_binary[democrat==1], na.rm=T) #0.697 is overall number
summary(dem_ecogain <- support_binary[treat_cat=="ecogain" & democrat==1], na.rm=T) #0.733
summary(dem_ecoloss <- support_binary[treat_cat=="ecoloss" & democrat==1], na.rm=T) #0.885
summary(dem_econgain <- support_binary[treat_cat=="econgain" & democrat==1], na.rm=T) #0.643
summary(dem_econloss <- support_binary[treat_cat=="econloss" & democrat==1], na.rm=T) #0.702
summary(dem_control <- support_binary[treat_cat=="control" & democrat==1], na.rm=T) #0.558
#Ordering: eco-loss, eco-gain, econ-loss, econ-gain, control

#democrat data frame
m = c(mean(dem_econgain), mean(dem_econloss), mean(dem_ecogain), mean(dem_ecoloss))
ATE=c(mean(dem_econgain)-mean(dem_control), mean(dem_econloss)-mean(dem_control), 
      mean(dem_ecogain)-mean(dem_control), mean(dem_ecoloss)-mean(dem_control))
treat_cat_dem = c("Econ Gain", "Econ Loss", "Ecol Gain", "Ecol Loss")
se = c(sd(dem_econgain)/sqrt(length(dem_econgain)), sd(dem_econloss)/sqrt(length(dem_econloss)),
       sd(dem_ecogain)/sqrt(length(dem_ecogain)),
       sd(dem_ecoloss)/sqrt(length(dem_ecoloss)))
party_dem<-as.factor(rep(2, length(m)))

dem.df<-data.frame(treat_cat_dem, m, se, ATE, party_dem)
dem.df<-setNames(dem.df, c("treat_cat", "mean", "se", "ATE", "partyid"))


#Among Unaffiliated
summary(ind_overall <- support_binary[independent==1], na.rm=T) #0.746 is overall number
summary(ind_ecogain <- support_binary[treat_cat=="ecogain" & independent==1], na.rm=T) #0.818
summary(ind_ecoloss <- support_binary[treat_cat=="ecoloss" & independent==1], na.rm=T) #0.837
summary(ind_econgain <- support_binary[treat_cat=="econgain" & independent==1], na.rm=T) #0.600
summary(ind_econloss <- support_binary[treat_cat=="econloss" & independent==1], na.rm=T) #0.720
summary(ind_control <- support_binary[treat_cat=="control" & independent==1], na.rm=T) #0.714
#Ordering: eco-loss, eco-gain, econ-loss, control, econ-gain

#independent data frame
m = c(mean(ind_econgain), mean(ind_econloss), mean(ind_ecogain), mean(ind_ecoloss))
ATE=c(mean(ind_econgain)-mean(ind_control), mean(ind_econloss)-mean(ind_control), 
      mean(ind_ecogain)-mean(ind_control), mean(ind_ecoloss)-mean(ind_control))
treat_cat_ind = c("Econ Gain", "Econ Loss", "Ecol Gain", "Ecol Loss")
se = c(sd(ind_econgain)/sqrt(length(ind_econgain)), 
       sd(ind_econloss)/sqrt(length(ind_econloss)), sd(ind_ecogain)/sqrt(length(ind_ecogain)),
       sd(ind_ecoloss)/sqrt(length(ind_ecoloss)))
party_ind<-as.factor(rep(3, length(m)))

ind.df<-data.frame(treat_cat_ind, m, se, ATE, party_ind)
ind.df<-setNames(ind.df, c("treat_cat", "mean", "se", "ATE", "partyid"))

#plotting dems and reps together
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))

party.df<-rbind.data.frame(rep.df, dem.df, ind.df)
ggplot(party.df, aes(x=treat_cat, y=ATE, fill=partyid)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=ATE-se, ymax=ATE+se),
                width=.2, position=position_dodge(.9)) +
  xlab("Treatment Category") + ylab("Average Treatment Effect")+
  scale_fill_hue(name="Party", labels=c("Republican", "Democrat", "Unaffiliated")) +
  ggtitle("Treatment Effect by Party ID")

#using boxplot instead
ggplot(party.df, aes(x=partyid, y=ATE, fill=treat_cat)) +
  geom_boxplot() +
  geom_errorbar(aes(ymin=ATE-se, ymax=ATE+se),
                width=.2, position=position_dodge(.9)) + ggtitle("Framing Effects by Party ID") +
  xlab("Party ID") + ylab("Average Treatment Effect") +
  scale_x_discrete(
                   labels=c("Rep", "Dem", "Ind")) +
  facet_grid(.~treat_cat)

#potential major heterogeneous treatment effects: eco-loss for dems, eco-gain for reps, both econ for reps
#republicans seem much more responsive to gain frames, dems somewhat more responsive to loss frames.
#dems also much less responsive to control frame as compared to republicans
#independents seems somewhere in between
#FOR ECOLOGICAL POOLED
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
rep_ecosupport<-ecosupport_func(support=support_reps, treat=treat_reps)
ind_ecosupport<-ecosupport_func(support=support_inds, treat=treat_inds)

#FOR POOLED ECONOMIC
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

