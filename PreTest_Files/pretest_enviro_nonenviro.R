#Environmentalist
support_enviro = na.omit(project_support[enviro_responsibility==5 | enviro_responsibility==4])
support_nonenviro = na.omit(project_support[enviro_responsibility==3 | enviro_responsibility==2 | enviro_responsibility==1])
summary(as.factor(enviro_responsibility))

m = c(mean(support_enviro), mean(support_nonenviro))
names(m) = c("Environmentalist", "Non-Environmentalist")
se = c(sd(support_enviro)/sqrt(length(support_enviro)), sd(support_nonenviro)/sqrt(length(support_nonenviro)))
bp = barplot(m, ylim=c(0,1.0), xpd=FALSE, col=c("darkseagreen3", "mistyrose2"))
box()
title(ylab = "Support for Pig Management Program", main="Support for Pig Management, Environmentalism", font.lab = 2)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)

#heterogeneous treatment effects
enviro_dummy <- as.numeric(enviro_responsibility==5 | enviro_responsibility==4) #743 of these
nonenviro_dummy <- as.numeric(enviro_responsibility==3 | enviro_responsibility==2 | enviro_responsibility==1) #59 of these
enviro_dummy2 <- as.numeric(enviro_responsibility==5) #543 of these, 268 of 1 through 4

#among environmentalists (onlyi enviro_responsibility==5)
enviro_econgain <- na.omit(pretest_df$project_support[treat_cat=="econ_gain" & enviro_dummy2==1]) #0.551
enviro_econloss <- na.omit(pretest_df$project_support[treat_cat=="econ_loss" & enviro_dummy2==1]) #0.5
enviro_ecogain <- na.omit(pretest_df$project_support[treat_cat=="eco_gain" & enviro_dummy2==1]) #0.6481
enviro_control <- na.omit(pretest_df$project_support[treat_cat=="control" & enviro_dummy2==1]) #0.3894

m = c(mean(enviro_control), mean(enviro_econgain), mean(enviro_econloss), mean(enviro_ecogain))
treat_cat_enviro = c("Control", "Econ Gain", "Econ Loss", "Ecol Gain")
se = c(sd(enviro_control)/sqrt(length(enviro_control)), sd(enviro_econgain)/sqrt(length(enviro_econgain)), 
       sd(enviro_econloss)/sqrt(length(enviro_econloss)), sd(enviro_ecogain)/sqrt(length(enviro_ecogain)))
enviro<-as.factor(rep(5, each=length(m)))
enviro.df<-data.frame(treat_cat_enviro, m, se, enviro)
enviro.df<-setNames(enviro.df, c("treat_cat", "mean", "se", "enviro"))



# bp = barplot(m, ylim=c(0,1), xpd=FALSE, col=c("gray", "lightsteelblue3", "lightsteelblue4", "darkolivegreen2"))
# box()
# title(ylab = "Support for Pig Management Program", main="Framing Effects, Among Strong Environmentalists", font.lab = 2)
# arrows(x0=bp, y0=m-se, x1=bp, y1=m+se, code=3, angle=90)


#non-enviro
non_enviro_econgain <- na.omit(pretest_df$project_support[treat_cat=="econ_gain" & nonenviro_dummy==1]) #0.5789
non_enviro_econloss <- na.omit(pretest_df$project_support[treat_cat=="econ_loss" & nonenviro_dummy==1]) #0.6346
non_enviro_ecogain <- na.omit(pretest_df$project_support[treat_cat=="eco_gain" & nonenviro_dummy==1]) #0.8182
non_enviro_control <- na.omit(pretest_df$project_support[treat_cat=="control" & nonenviro_dummy==1]) #0.6667

summary(as.factor(enviro_responsibility))

m = c(mean(non_enviro_control), mean(non_enviro_econgain), mean(non_enviro_econloss), mean(non_enviro_ecogain))
treat_cat_nonenviro = c("Control", "Econ Gain", "Econ Loss", "Ecol Gain")
se = c(sd(non_enviro_control)/sqrt(length(non_enviro_control)), sd(non_enviro_econgain)/sqrt(length(non_enviro_econgain)), 
       sd(non_enviro_econloss)/sqrt(length(non_enviro_econloss)), sd(non_enviro_ecogain)/sqrt(length(non_enviro_ecogain)))
nonenviro<-as.factor(rep(1, each=length(m)))
nonenviro.df<-data.frame(treat_cat_nonenviro, m, se, nonenviro)
nonenviro.df<-setNames(nonenviro.df, c("treat_cat", "mean", "se", "enviro"))

enviro.tot.df<-rbind.data.frame(enviro.df, nonenviro.df)
ggplot(enviro.tot.df, aes(x=treat_cat, y=mean, fill=enviro)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2, position=position_dodge(.9)) +
  xlab("Treatment Category") + ylab("Mean")+
  scale_fill_hue(name="Environmentalist", # Legend label, use darker colors
                 labels=c("Strong Environmentalist", "Non-Environmentalist")) +
  ggtitle("Environmentalists vs. Non-Environmentalists")


# bp = barplot(m, ylim=c(0,1), xpd=FALSE, col=c("gray", "lightsteelblue3", "lightsteelblue4", "darkolivegreen2"))
# box()
# title(ylab = "Support for Pig Management Program", main="Framing Effects Among Non-Environmentalists", font.lab = 2)
# arrows(x0=bp, y0=m-se, x1=bp, y1=m+se, code=3, angle=90)


