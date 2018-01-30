#Initial Analysis

attach(kuni_df)

#Means support across the five conditions
summary(ecogain_mean <- mean(support_binary[treat_cat=="ecogain"], na.rm=T)) #0.773
summary(ecoloss_mean <- mean(support_binary[treat_cat=="ecoloss"], na.rm=T)) #0.857
summary(econgain_mean <- mean(support_binary[treat_cat=="econgain"], na.rm=T)) #0.660
summary(econloss_mean <- mean(support_binary[treat_cat=="econloss"], na.rm=T)) #0.731
summary(control_mean <- mean(support_binary[treat_cat=="control"], na.rm=T)) #0.618
#1. Eco-Loss, 2. Eco-Gain, 3. Econ-Loss, 4. Econ-Gain, 5. Control
#Very large effects of eco-loss and eco-gain, plus econ-loss as well

#strength of support across five conditions
summary(ecogain_mean_strength <- mean(support_oppose[treat_cat=="ecogain"], na.rm=T)) #4.17
summary(ecoloss_mean_strength <- mean(support_oppose[treat_cat=="ecoloss"], na.rm=T)) #3.88
summary(econgain_mean_strength <- mean(support_oppose[treat_cat=="econgain"], na.rm=T)) #4.16
summary(econloss_mean_strength <- mean(support_oppose[treat_cat=="econloss"], na.rm=T)) #4.14
summary(control_mean_strength <- mean(support_oppose[treat_cat=="control"], na.rm=T)) #4.00

#HETEROGENEOUS TREATMENT EFFECTS

#BY PARTY ID#

#Among Republicans
summary(rep_overall <- support_binary[republican==1], na.rm=T) #0.764 is overall number
summary(rep_ecogain <- support_binary[treat_cat=="ecogain" & republican==1], na.rm=T) #0.793
summary(rep_ecoloss <- support_binary[treat_cat=="ecoloss" & republican==1], na.rm=T) #0.833
summary(rep_econgain <- support_binary[treat_cat=="econgain" & republican==1], na.rm=T) #0.732
summary(rep_econloss <- support_binary[treat_cat=="econloss" & republican==1], na.rm=T) #0.793
summary(rep_control <- support_binary[treat_cat=="control" & republican==1], na.rm=T) #0.660
#Ordering: eco-loss, econ-loss & eco-gain (tied), econ-gain, control

#plotting republicans
m = c(mean(rep_control), mean(rep_econgain), mean(rep_econloss), mean(rep_ecogain), mean(rep_ecoloss))
treat_cat_rep = c("Control", "Econ Gain", "Econ Loss", "Ecol Gain", "Ecol Loss")
se = c(sd(rep_control)/sqrt(length(rep_control)), sd(rep_econgain)/sqrt(length(rep_econgain)), 
       sd(rep_econloss)/sqrt(length(rep_econloss)), sd(rep_ecogain)/sqrt(length(rep_ecogain)),
       sd(rep_ecoloss)/sqrt(length(rep_ecoloss)))
party_rep<-as.factor(rep(1, each=length(m)))
#creating a separate data frame with republican data
rep.df<-data.frame(treat_cat_rep, m, se, party_rep)
rep.df<-setNames(rep.df, c("treat_cat", "mean", "se", "partyid"))


#Among Democrats
summary(dem_overall <- support_binary[democrat==1], na.rm=T) #0.697 is overall number
summary(dem_ecogain <- support_binary[treat_cat=="ecogain" & democrat==1], na.rm=T) #0.733
summary(dem_ecoloss <- support_binary[treat_cat=="ecoloss" & democrat==1], na.rm=T) #0.885
summary(dem_econgain <- support_binary[treat_cat=="econgain" & democrat==1], na.rm=T) #0.643
summary(dem_econloss <- support_binary[treat_cat=="econloss" & democrat==1], na.rm=T) #0.702
summary(dem_control <- support_binary[treat_cat=="control" & democrat==1], na.rm=T) #0.558
#Ordering: eco-loss, eco-gain, econ-loss, econ-gain, control

m = c(mean(dem_control), mean(dem_econgain), mean(dem_econloss), mean(dem_ecogain), mean(dem_ecoloss))
treat_cat_dem = c("Control", "Econ Gain", "Econ Loss", "Ecol Gain", "Ecol Loss")
se = c(sd(dem_control)/sqrt(length(dem_control)), sd(dem_econgain)/sqrt(length(dem_econgain)), 
       sd(dem_econloss)/sqrt(length(dem_econloss)), sd(dem_ecogain)/sqrt(length(dem_ecogain)),
       sd(dem_ecoloss)/sqrt(length(dem_ecoloss)))
party_dem<-as.factor(rep(2, length(m)))
#creating a separate data frame with republican data
dem.df<-data.frame(treat_cat_dem, m, se, party_dem)
dem.df<-setNames(dem.df, c("treat_cat", "mean", "se", "partyid"))


#plotting dems and reps together
library(ggplot2)
theme_update(plot.title = element_text(hjust = 0.5))

party.df<-rbind.data.frame(rep.df, dem.df)
ggplot(party.df, aes(x=treat_cat, y=mean, fill=partyid)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se),
                width=.2, position=position_dodge(.9)) +
  ylim(0,1)+xlab("Treatment Category") + ylab("Mean")+
  scale_fill_hue(name="Party", labels=c("Republican", "Democrat")) +
  ggtitle("Republicans vs Democrats")


#Among Unaffiliated
summary(overall_mean <- mean(support_binary[independent==1], na.rm=T)) #0.746 is overall number
summary(ecogain_mean <- mean(support_binary[treat_cat=="ecogain" & independent==1], na.rm=T)) #0.818
summary(ecoloss_mean <- mean(support_binary[treat_cat=="ecoloss" & independent==1], na.rm=T)) #0.837
summary(econgain_mean <- mean(support_binary[treat_cat=="econgain" & independent==1], na.rm=T)) #0.600
summary(econloss_mean <- mean(support_binary[treat_cat=="econloss" & independent==1], na.rm=T)) #0.720
summary(control_mean <- mean(support_binary[treat_cat=="control" & independent==1], na.rm=T)) #0.714
#Ordering: eco-loss, eco-gain, econ-loss, control, econ-gain

#potential major heterogeneous treatment effects: eco-loss for dems, eco-gain for reps, both econ for reps
#republicans seem much more responsive to gain frames, dems somewhat more responsive to loss frames.
#dems also much less responsive to control frame as compared to republicans
#independents seems somewhere in between

#BY GENDER#

#male
summary(overall_mean <- mean(support_binary[gender==1], na.rm=T)) #0.830
summary(ecogain_mean <- mean(support_binary[treat_cat=="ecogain" & gender==1], na.rm=T)) #0.859
summary(ecoloss_mean <- mean(support_binary[treat_cat=="ecoloss" & gender==1], na.rm=T)) #0.914
summary(econgain_mean <- mean(support_binary[treat_cat=="econgain" & gender==1], na.rm=T)) #0.757
summary(econloss_mean <- mean(support_binary[treat_cat=="econloss" & gender==1], na.rm=T)) #0.823
summary(control_mean <- mean(support_binary[treat_cat=="control" & gender==1], na.rm=T)) #0.786
#Ordering: Eco-Loss, Eco-Gain, Econ-Loss, Control, Econ-Gain

#female
summary(overall_mean <- mean(support_binary[gender==2], na.rm=T)) #0.665
summary(ecogain_mean <- mean(support_binary[treat_cat=="ecogain" & gender==2], na.rm=T)) #0.724
summary(ecoloss_mean <- mean(support_binary[treat_cat=="ecoloss" & gender==2], na.rm=T)) #0.817
summary(econgain_mean <- mean(support_binary[treat_cat=="econgain" & gender==2], na.rm=T)) #0.605
summary(econloss_mean <- mean(support_binary[treat_cat=="econloss" & gender==2], na.rm=T)) #0.670
summary(control_mean <- mean(support_binary[treat_cat=="control" & gender==2], na.rm=T)) #0.527
#Ordering: Eco-Loss, Eco-Gain, Econ-Loss, Econ-Gain, Control

#Men much more supportive overall. Women maybe more responsive to eco-loss and eco-gain.

#BY URBAN-RURAL#

#Urban
summary(overall_mean <- mean(support_binary[urban_rural==1], na.rm=T)) #0.711
summary(ecogain_mean <- mean(support_binary[treat_cat=="ecogain" & urban_rural==1], na.rm=T)) #0.738
summary(ecoloss_mean <- mean(support_binary[treat_cat=="ecoloss" & urban_rural==1], na.rm=T)) #0.879
summary(econgain_mean <- mean(support_binary[treat_cat=="econgain" & urban_rural==1], na.rm=T)) #0.648
summary(econloss_mean <- mean(support_binary[treat_cat=="econloss" & urban_rural==1], na.rm=T)) #0.708
summary(control_mean <- mean(support_binary[treat_cat=="control" & urban_rural==1], na.rm=T)) #0.591
#Ordering: Eco-Loss, Eco-Gain, Econ-Loss, Econ-Gain, Control

#Suburban
summary(overall_mean <- mean(support_binary[urban_rural==2], na.rm=T)) #0.751
summary(ecogain_mean <- mean(support_binary[treat_cat=="ecogain" & urban_rural==2], na.rm=T)) #0.788
summary(ecoloss_mean <- mean(support_binary[treat_cat=="ecoloss" & urban_rural==2], na.rm=T)) #0.866
summary(econgain_mean <- mean(support_binary[treat_cat=="econgain" & urban_rural==2], na.rm=T)) #0.640
summary(econloss_mean <- mean(support_binary[treat_cat=="econloss" & urban_rural==2], na.rm=T)) #0.732
summary(control_mean <- mean(support_binary[treat_cat=="control" & urban_rural==2], na.rm=T)) #0.718
#Ordering: Eco-Loss, Eco-Gain, Econ-Loss, Control, Econ-Gain

#Rural
summary(overall_mean <- mean(support_binary[urban_rural==3], na.rm=T)) #0.713
summary(ecogain_mean <- mean(support_binary[treat_cat=="ecogain" & urban_rural==3], na.rm=T)) #0.793
summary(ecoloss_mean <- mean(support_binary[treat_cat=="ecoloss" & urban_rural==3], na.rm=T)) #0.813
summary(econgain_mean <- mean(support_binary[treat_cat=="econgain" & urban_rural==3], na.rm=T)) #0.708
summary(econloss_mean <- mean(support_binary[treat_cat=="econloss" & urban_rural==3], na.rm=T)) #0.760
summary(control_mean <- mean(support_binary[treat_cat=="control" & urban_rural==3], na.rm=T)) #0.509
#Ordering: Eco-Loss, Eco-Gain, Econ-Loss, Econ-Gain, Control



