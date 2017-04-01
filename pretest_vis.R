#kuni pretest visualization#

library(dotwhisker)

#First, Bar Plots#

#CONTROL, ECON, ECOLOGICAL
support_control = na.omit(project_support[treat_cat=="control"])
support_econ = na.omit(project_support[treat_cat=="econ_gain" | treat_cat=="econ_loss"])
support_eco = na.omit(project_support[treat_cat=="eco_gain" | treat_cat=="eco_loss"])

m = c(mean(support_control), mean(support_econ), mean(support_eco))
names(m) = c("Control", "Econ Frame", "Ecological Frame")
se = c(sd(support_control)/sqrt(length(support_control)), sd(support_econ)/sqrt(length(support_econ)), sd(support_eco)/sqrt(length(support_eco)))
bp = barplot(m, ylim=c(0,1.0), xpd=FALSE, col=c("gray", "lightsteelblue3","darkolivegreen2"))
box()
title(ylab = "Support for Pig Management Program", main="Framing Effects on Pig Management, 1", font.lab = 2)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

#ADD GAIN/LOSS FRAMES
support_control = na.omit(project_support[treat_cat=="control"])
support_econ_gain = na.omit(project_support[treat_cat=="econ_gain"])
support_econ_loss = na.omit(project_support[treat_cat=="econ_loss"])
support_eco_gain = na.omit(project_support[treat_cat=="eco_gain"])

m = c(mean(support_control), mean(support_econ_gain), mean(support_econ_loss), mean(support_eco_gain))
names(m) = c("Control", "Econ Gain", "Econ Loss", "Ecol Gain")
se = c(sd(support_control)/sqrt(length(support_control)), sd(support_econ_gain)/sqrt(length(support_econ_gain)), 
       sd(support_econ_loss)/sqrt(length(support_econ_loss)), sd(support_eco_gain)/sqrt(length(support_eco_gain)))
bp = barplot(m, ylim=c(0,1.0), xpd=FALSE, col=c("gray", "lightsteelblue3", "lightsteelblue4", "darkolivegreen2"))
box()
title(ylab = "Support for Pig Management Program", main="Framing Effects on Pig Management, 2", font.lab = 2)
arrows(x0=bp, y0=m-se, x1=bp, y1=m+se, code=3, angle=90)

#Party ID
summary(republican_mean <- mean(pretest_df$project_support[partyid==1], na.rm=T)) #0.675, N=163 (53 no, 110 yes)
summary(dem_mean <- mean(pretest_df$project_support[partyid==2], na.rm=T)) #0.553, N=372 (166 no, 205 yes)
summary(ind_mean <- mean(pretest_df$project_support[partyid==3 | partyid==4], na.rm=T)) #0.536, N=266 (123 no, 142 yes;include both independents and unaffiliated)
print(partyid_test <- prop.test(x = c(110, 205), n = c(163, 372), correct = FALSE)) #p<0.05. reps more likely to support than dems

partyid<-pretest_df$partyid
support_rep = na.omit(project_support[partyid==1])
support_dem = na.omit(project_support[partyid==2])
support_ind = na.omit(project_support[partyid==3])

m = c(mean(support_rep), mean(support_dem), mean(support_ind))
names(m) = c("Republican", "Democrat", "Independent")
se = c(sd(support_rep)/sqrt(length(support_rep)), sd(support_dem)/sqrt(length(support_dem)), 
       sd(support_ind)/sqrt(length(support_ind)))
windows()
bp = barplot(m, ylim=c(0,1.0), xpd=FALSE)
box()
title(ylab = "Support for Pig Management Program", main="Support for Pig Management, By Party ID", font.lab = 2)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

#Environmentalist
support_enviro = na.omit(project_support[enviro_responsibility==5 | enviro_responsibility==4])
support_nonenviro = na.omit(project_support[enviro_responsibility==3 | enviro_responsibility==2 | enviro_responsibility==1])
summary(as.factor(enviro_responsibility))

m = c(mean(support_enviro), mean(support_nonenviro))
names(m) = c("Environmentalist", "Non-Environmentalist")
se = c(sd(support_enviro)/sqrt(length(support_enviro)), sd(support_nonenviro)/sqrt(length(support_nonenviro)))
windows()
bp = barplot(m, ylim=c(0,1.0), xpd=FALSE)
box()
title(ylab = "Support for Pig Management Program", main="Support for Pig Management, Environmentalism", font.lab = 2)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

summary(enviro_ecogain <- mean(pretest_df$project_support[treat_cat=="econ_gain"], na.rm=T)) #0.5641
summary(enviro_ecogain <- na.omit(pretest_df$project_support[treat_cat=="econ_gain"]))
summary(econloss_mean <- mean(pretest_df$project_support[treat_cat=="econ_loss"], na.rm=T)) #0.5432
summary(ecogain_mean <- mean(pretest_df$project_support[treat_cat=="eco_gain"], na.rm=T)) #0.7055
summary(ecoloss_mean <- mean(pretest_df$project_support[treat_cat=="eco_loss"], na.rm=T)) #0.5723
summary(control_mean <- mean(pretest_df$project_support[treat_cat=="control"], na.rm=T)) #0.472


#heterogeneous treatment effects
enviro_dummy <- as.numeric(enviro_responsibility==5 | enviro_responsibility==4) #743 of these
nonenviro_dummy <- as.numeric(enviro_responsibility==3 | enviro_responsibility==2 | enviro_responsibility==1) #59 of these

enviro_dummy2 <- as.numeric(enviro_responsibility==5) #543 of these, 268 of 1 through 4

#among environmentalists (onlyi enviro_responsibility==5)
summary(enviro_econgain <- na.omit(pretest_df$project_support[treat_cat=="econ_gain" & enviro_dummy2==1])) #0.551
summary(enviro_econloss <- mean(pretest_df$project_support[treat_cat=="econ_loss" & enviro_dummy2==1], na.rm=T)) #0.5
summary(enviro_ecogain <- mean(pretest_df$project_support[treat_cat=="eco_gain" & enviro_dummy2==1], na.rm=T)) #0.6481
summary(enviro_control <- mean(pretest_df$project_support[treat_cat=="control" & enviro_dummy2==1], na.rm=T)) #0.3894

summary(non_enviro_econgain <- mean(pretest_df$project_support[treat_cat=="econ_gain" & nonenviro_dummy==1], na.rm=T)) #0.5789
summary(non_enviro_econloss <- mean(pretest_df$project_support[treat_cat=="econ_loss" & nonenviro_dummy==1], na.rm=T)) #0.6346
summary(non_enviro_ecogain <- mean(pretest_df$project_support[treat_cat=="eco_gain" & nonenviro_dummy==1], na.rm=T)) #0.8182
summary(non_enviro_control <- mean(pretest_df$project_support[treat_cat=="control" & nonenviro_dummy==1], na.rm=T)) #0.6667

summary(as.factor(enviro_responsibility))

#non-enviro
m = c(mean(non_enviro_control), mean(non_enviro_econgain), mean(non_enviro_econloss), mean(non_enviro_ecogain))
names(m) = c("Control", "Econ Gain", "Econ Loss", "Ecol Gain")
se = c(sd(non_enviro_control)/sqrt(length(non_enviro_control)), sd(non_enviro_econgain)/sqrt(length(non_enviro_econgain)), 
       sd(non_enviro_econloss)/sqrt(length(non_enviro_econloss)), sd(non_enviro_ecogain)/sqrt(length(non_enviro_ecogain)))
windows()
bp = barplot(m, ylim=c(0,1), xpd=FALSE)
box()
title(ylab = "Support for Pig Management Program", main="Framing Effects Among Non-Environmentalists", font.lab = 2)
arrows(x0=bp, y0=m-se, x1=bp, y1=m+se, code=3, angle=90)

#enviros
m = c(mean(enviro_control), mean(enviro_econgain), mean(enviro_econloss), mean(enviro_ecogain))
names(m) = c("Control", "Econ Gain", "Econ Loss", "Ecol Gain")
se = c(sd(enviro_control)/sqrt(length(enviro_control)), sd(enviro_econgain)/sqrt(length(enviro_econgain)), 
       sd(enviro_econloss)/sqrt(length(enviro_econloss)), sd(enviro_ecogain)/sqrt(length(enviro_ecogain)))
windows()
bp = barplot(m, ylim=c(0,1), xpd=FALSE)
box()
title(ylab = "Support for Pig Management Program", main="Framing Effects, Among Strong Environmentalists", font.lab = 2)
arrows(x0=bp, y0=m-se, x1=bp, y1=m+se)

#Party ID
summary(republican_mean <- mean(pretest_df$project_support[partyid==1], na.rm=T)) #0.675, N=163 (53 no, 110 yes)
summary(dem_mean <- mean(pretest_df$project_support[partyid==2], na.rm=T)) #0.553, N=372 (166 no, 205 yes)
summary(ind_mean <- mean(pretest_df$project_support[partyid==3 | partyid==4], na.rm=T)) #0.536, N=266 (123 no, 142 yes;include both independents and unaffiliated)
print(partyid_test <- prop.test(x = c(110, 205), n = c(163, 372), correct = FALSE)) #p<0.05. reps more likely to support than dems

support_rep = na.omit(project_support[partyid==1])
support_dem = na.omit(project_support[partyid==2])
support_ind = na.omit(project_support[partyid==3])
summary(as.factor(partyid)) #163 republicans, 372 dems, 231 independent

m = c(mean(support_rep), mean(support_dem), mean(support_ind))
names(m) = c("Republican", "Democrat", "Independent")
se = c(sd(support_rep)/sqrt(length(support_rep)), sd(support_dem)/sqrt(length(support_dem)), 
       sd(support_ind)/sqrt(length(support_ind)))
windows()
bp = barplot(m, ylim=c(.25,.75), xpd=FALSE)
box()
title(ylab = "Support for Pig Management Program", main="Support for Pig Management, By Party ID", font.lab = 2)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

#dems
summary(dem_econgain <- mean(pretest_df$project_support[treat_cat=="econ_gain" & partyid==2], na.rm=T)) #0.5574
summary(dem_econloss <- mean(pretest_df$project_support[treat_cat=="econ_loss" & partyid==2], na.rm=T)) #0.5797
summary(dem_ecogain <- mean(pretest_df$project_support[treat_cat=="eco_gain" & partyid==2], na.rm=T)) #0.6875
summary(dem_control <- mean(pretest_df$project_support[treat_cat=="control" & partyid==2], na.rm=T)) #0.3816

#reps
summary(rep_econgain <- mean(pretest_df$project_support[treat_cat=="econ_gain" & partyid==1], na.rm=T)) #0.5526
summary(rep_econloss <- mean(pretest_df$project_support[treat_cat=="econ_loss" & partyid==1], na.rm=T)) #0.6857
summary(rep_ecogain <- mean(pretest_df$project_support[treat_cat=="eco_gain" & partyid==1], na.rm=T)) #0.8276
summary(rep_control <- mean(pretest_df$project_support[treat_cat=="control" & partyid==1], na.rm=T)) #0.6552

#dems
m = c(mean(dem_control), mean(dem_econgain), mean(dem_econloss), mean(dem_ecogain))
names(m) = c("Control", "Econ Gain", "Econ Loss", "Ecol Gain")
se = c(sd(dem_control)/sqrt(length(dem_control)), sd(dem_econgain)/sqrt(length(dem_econgain)), 
       sd(dem_econloss)/sqrt(length(dem_econloss)), sd(dem_ecogain)/sqrt(length(dem_ecogain)))
windows()
bp = barplot(m, ylim=c(0,1.0), xpd=FALSE)
box()
title(ylab = "Support for Pig Management Program", main="Framing Effects Among Democrats", font.lab = 2)
arrows(x0=bp, y0=m-se, x1=bp, y1=m+se, code=3, angle=90)

#reps
m1 = c(mean(rep_control), mean(rep_econgain), mean(rep_econloss), mean(rep_ecogain))
names(m) = c("Control", "Econ Gain", "Econ Loss", "Ecol Gain")
se = c(sd(rep_control)/sqrt(length(rep_control)), sd(rep_econgain)/sqrt(length(rep_econgain)), 
       sd(rep_econloss)/sqrt(length(rep_econloss)), sd(rep_ecogain)/sqrt(length(rep_ecogain)))
windows()
bp1 = barplot(m1, ylim=c(0,1.0), xpd=FALSE)
box()
title(ylab = "Support for Pig Management Program", main="Framing Effects Among Republicans", font.lab = 2)
arrows(x0=bp1, y0=m-se, x1=bp1, y1=m+se, code=3, angle=90)


#gender
summary(male_mean <- mean(pretest_df$project_support[gender==1], na.rm=T)) #0.630, N=350 (165 no, 281 yes)
summary(female_mean <- mean(pretest_df$project_support[gender==0], na.rm=T)) #0.499, N=351 (176 no,  175 yes)
summary(anova_1 <- aov(project_support ~ gender, data=pretest_df)) #sig difference




##ggplot2 to make it prettier##
library(ggplot2)
library(gridExtra)

#Bar Plots#
summary.project_support <- data.frame(
  treat=levels(as.factor(pretest_df$treat_cat)),
  support=c(mean(na.omit(econ_gain_support)), mean(econ_loss_support), mean(eco_gain_support), mean(na.omit(eco_loss_support)), 
            mean(control_support)),
  sd=c(sd(na.omit(econ_gain_support))/sqrt(length(na.omit(econ_gain_support)))), sd(econ_loss_support)/sqrt(length(econ_loss_support)), 
        sd(eco_gain_support)/sqrt(length(eco_gain_support)), sd(na.omit(eco_loss_support))/sqrt(length(na.omit(eco_loss_support))), 
        sd(control_support)/sqrt(length(control_support)))
summary.project_support

mean(na.omit(eco_loss_support))

ggplot(data = summary.vote15, aes(x = factor(treat), y = vote)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin=vote-sd, ymax=vote+sd), width=0.1, col="darkblue") +
  coord_flip() +
  geom_text(aes(y=vote, ymax=vote, label=round(vote,2)), position= position_dodge(width=0.9), vjust=-2, color="black") +
  scale_y_continuous("Probability of Voting", limits=c(0.15,.25),breaks=seq(0.15, .25, .05)) + 
  scale_x_discrete("Control vs. Two Years of Treatment")


summary.vote15b <- data.frame(
  treat=levels(as.factor(ooc_data$treatyear)),
  vote=c(mean(vote_control) - mean(vote15), mean(vote_treatment15) - mean(vote15), mean(vote_treatboth) - mean(vote15)),
  sd=c(sd(vote_control)/sqrt(length(vote_control)), sd(vote_treatment15)/sqrt(length(vote_treatment15)),
       sd(vote_treatboth)/sqrt(length(vote_treatboth))))
summary.vote15b

ggplot(data = summary.vote15b, aes(x = factor(treat), y = vote)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin=vote-sd, ymax=vote+sd), width=0.1, col="darkblue") +
  coord_flip() +
  geom_text(aes(y=vote, ymax=vote, label=round(vote,2)), position= position_dodge(width=0.9), vjust=-2, color="black") +
  scale_y_continuous("Increased Probability of Voting", limits=c(-0.05,.05),
                     breaks=seq(-0.05, .05, .05)) + 
  scale_x_discrete("Control vs. Two Years of Treatment")

#Just comparing control to treatment in 2015
summary.vote <- data.frame(
  treat=levels(as.factor(ooc_data$treatment15)),
  vote=tapply(ooc_data$vote15, ooc_data$treatment15, mean),
  sd=tapply(ooc_data$vote15, ooc_data$treatment15, sd))
summary.vote

summary(vote15)