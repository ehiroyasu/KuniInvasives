#kuni pretest visualization#

library(dotwhisker)
library(ggplot2)
pretest_df <- read.csv("pretest_dec16.csv", h=T, stringsAsFactors=FALSE)

#First, Bar Plots#
##Party ID
republican<- as.integer(na.omit(pretest_df$project_support[partyid==1])) #0.675, N=163 (53 no, 110 yes)
dem <- as.integer(na.omit(pretest_df$project_support[partyid==2])) #0.553, N=372 (166 no, 205 yes)
ind <- as.integer(na.omit(pretest_df$project_support[partyid==3 | partyid==4])) #0.536, N=266 (123 no, 142 yes;include both independents and unaffiliated)
#print(partyid_test <- prop.test(x = c(110, 205), n = c(163, 372), correct = FALSE)) #p<0.05. reps more likely to support than dems

partyid<-pretest_df$partyid
support_rep = na.omit(project_support[partyid==1])
support_dem = na.omit(project_support[partyid==2])
support_ind = na.omit(project_support[partyid==3])

m = c(mean(support_rep), mean(support_dem), mean(support_ind))
names(m) = c("Republican", "Democrat", "Independent")
se = c(sd(support_rep)/sqrt(length(support_rep)), sd(support_dem)/sqrt(length(support_dem)), 
       sd(support_ind)/sqrt(length(support_ind)))
bp = barplot(m, ylim=c(0,1.0), xpd=FALSE, col=c("darkred", "darkblue", "grey"))
box()
title(ylab = "Support for Pig Management Program", main="Support for Pig Management, By Party ID", font.lab = 2)
arrows(x0=bp, y0=m-se, y1=m+se, code=3, angle=90)


#dems
dem_econgain <- na.omit(pretest_df$project_support[treat_cat=="econ_gain" & partyid==2]) #0.5574
dem_econloss <- na.omit(pretest_df$project_support[treat_cat=="econ_loss" & partyid==2]) #0.5797
dem_ecogain <- na.omit(pretest_df$project_support[treat_cat=="eco_gain" & partyid==2]) #0.6875
dem_control <- na.omit(pretest_df$project_support[treat_cat=="control" & partyid==2]) #0.3816

m = (c(mean(dem_control), mean(dem_econgain), mean(dem_econloss), mean(dem_ecogain)))
treat_cat_dem = c("Control", "Econ Gain", "Econ Loss", "Ecol Gain")
se =c(sd(dem_control)/sqrt(length(dem_control)), sd(dem_econgain)/sqrt(length(dem_econgain)), 
       sd(dem_econloss)/sqrt(length(dem_econloss)), sd(dem_ecogain)/sqrt(length(dem_ecogain)))
party_dem<-as.factor(rep(2, each=length(m)))
dem.df<-data.frame(treat_cat_dem, m, se, party_dem)#, stringsAsFactors = FALSE)
dem.df<-setNames(dem.df, c("treat_cat", "mean", "se", "partyid"))


# bp = barplot(m, ylim=c(0,1.0), xpd=FALSE, col=c("gray", "lightsteelblue3", "lightsteelblue4", "darkolivegreen2"))
# box()
# title(ylab = "Support for Pig Management Program", main="Framing Effects Among Democrats", font.lab = 2)
# arrows(x0=bp, y0=m-se, x1=bp, y1=m+se, code=3, angle=90)


#reps
rep_econgain <- na.omit(pretest_df$project_support[treat_cat=="econ_gain" & partyid==1]) #0.5526
rep_econloss <- na.omit(pretest_df$project_support[treat_cat=="econ_loss" & partyid==1]) #0.6857
rep_ecogain <- na.omit(pretest_df$project_support[treat_cat=="eco_gain" & partyid==1]) #0.8276
rep_control <- na.omit(pretest_df$project_support[treat_cat=="control" & partyid==1]) #0.6552

m = c(mean(rep_control), mean(rep_econgain), mean(rep_econloss), mean(rep_ecogain))
treat_cat_rep = c("Control", "Econ Gain", "Econ Loss", "Ecol Gain")
se = c(sd(rep_control)/sqrt(length(rep_control)), sd(rep_econgain)/sqrt(length(rep_econgain)), 
       sd(rep_econloss)/sqrt(length(rep_econloss)), sd(rep_ecogain)/sqrt(length(rep_ecogain)))
party_rep<-as.factor(rep(1, each=length(m)))
rep.df<-data.frame(treat_cat_rep, m, se, party_rep)
rep.df<-setNames(rep.df, c("treat_cat", "mean", "se", "partyid"))
# 
# bp1 = barplot(m1, ylim=c(0,1.0), xpd=FALSE, col=c("gray", "lightsteelblue3", "lightsteelblue4", "darkolivegreen2"))
# box()
# title(ylab = "Support for Pig Management Program", main="Framing Effects Among Republicans", font.lab = 2)
# arrows(x0=bp1, y0=m1-se, x1=bp1, y1=m1+se, code=3, angle=90)

party.df<-rbind.data.frame(rep.df, dem.df)
ggplot(party.df, aes(x=treat_cat, y=mean, fill=partyid)) + 
  geom_bar(position=position_dodge(), stat="identity") +
geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
              width=.2, position=position_dodge(.9)) +
          xlab("Treatment Category") + ylab("Mean")+
                scale_fill_hue(name="Party", # Legend label, use darker colors
                    labels=c("Republican", "Democrat")) +
                ggtitle("Republicans vs Democrats")
