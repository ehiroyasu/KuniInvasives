#Initial Analysis of Kuni Pre-Test#

pretest_df <- read.csv("pretest_dec16.csv", h=T, stringsAsFactors=FALSE)
View(pretest_df)

#Meta-Data#

#gender: male=1, female=0
#MSV & FSV: 5=very much like me, 1=not at all like me
#enviro_responsibility: 5=strongly agree; 1=strongly disagree
#treatments: need to recode
#project_support: 1=support; 0=oppose
#oppose_strength: 1=strongly oppose, 2=moderately oppose, 3=only slightly oppose
#support_strength: 6=strongly support, 5=moderately support, 4=only slightly oppose
#grizzly_support: 7=strongly support, 1=strongly oppose
#protect_endangered: 5=extremely important, 1=not at all important
#right_devproperty (right to develop property even if endangered species on it): 5=strongly support; 1=strongly oppose
#property_endanger (should not have right to use property if endangers species): 5=strongly support; 1=strongly oppose
#takings_paid (if owner can't develop land, should be paid for it): 5=strongly support; 1=strongly oppose
#vote2016: 1=sure they voted; 0=did not vote (all excuses)
#candidate_choice: 1=Donald Trump; 2=Hillary Clinton; 3=Other
#partyID: 1=Republican; 2=Democrat; 3=Independent; 4=Other
#ind_choice: 3=closer to republicans; 4=neither; 5=closer to dems
#dem_strength: 6=not very strong; 7=strong
#rep_strength: 2=not very strong; 1=strong
#ideology: 1=extremely liberal; 7=extremely conservative
#rural_urban: 1=urban; 2=suburban; 3=rural
#education: 1=primary school; 2=HS graduate; 3=some college; 4=BA; 5=advanced degree
#income: 1=<20k; 2=20-40k; 3=40-65k; 4=65-100k; 5=100-250k; 6=>250k
#race: 1=nonwhite (all); 0=white

#Grizzly-Related Questions
#grizzly_support = likert scale from 1 (least supportive) to 7 (most supportive)
#grizz_treatment = treatment language participants received. Q43 is the generic message, Q44 is national parks.

#Endangered Species Questions
#protect_endangered = response from 1 (not at all important) to 5 (extremely important): "How important do you believe it is for society to protect wildlife that are in danger of going extinct?"

#(next three questions are all Likert scale from 1 (strongly disagree) to 5 (strongly agree))
#right_devproperty = measuring response to "Endangered wildlife protection should not interfere with a landowner's right to develop property."
#property_endanger = measuring response to "Landowners should not have the right to use their property in ways that endanger wildlife."
#takings_paid = measuring response to "Landowners prevented from developing their property because of endangered wildlife laws should be paid for any lost income by the public."

#Code Different Treatment Conditions#
attach(pretest_df)
pretest_df$treat_cat[timing_1>=0] <- "econ_gain"
pretest_df$treat_cat[timing_2>=0] <- "econ_loss"
pretest_df$treat_cat[timing_3>=0] <- "eco_gain"
pretest_df$treat_cat[timing_4>=0] <- "eco_loss"
pretest_df$treat_cat[timing_5>=0] <- "control"
summary(as.factor(pretest_df$treat_cat))
detach(pretest_df)

#get means for each treatment condition
attach(pretest_df)
summary(econgain_mean <- mean(pretest_df$project_support[treat_cat=="econ_gain"], na.rm=T)) #0.5641
summary(econloss_mean <- mean(pretest_df$project_support[treat_cat=="econ_loss"], na.rm=T)) #0.5432
summary(ecogain_mean <- mean(pretest_df$project_support[treat_cat=="eco_gain"], na.rm=T)) #0.7055
summary(ecoloss_mean <- mean(pretest_df$project_support[treat_cat=="eco_loss"], na.rm=T)) #0.5723
summary(control_mean <- mean(pretest_df$project_support[treat_cat=="control"], na.rm=T)) #0.472

summary(anova_1 <- aov(project_support ~ treat_cat, data=pretest_df))
#significant difference across treatment conditions

#all five treatment conditions
summary(as.factor(econ_gain_support <- project_support[treat_cat=="econ_gain"])) #68 no, 88 yes
summary(as.factor(econ_loss_support <- project_support[treat_cat=="econ_loss"])) #74 no, 88 yes
summary(as.factor(eco_gain_support <- project_support[treat_cat=="eco_gain"])) #48 no, 115 yes
summary(as.factor(eco_loss_support <- project_support[treat_cat=="eco_loss"])) #68 no, 91 yes
summary(as.factor(control_support <- project_support[treat_cat=="control"])) #85 no, 76 yes

#separate into econ and ecological frames
summary(as.factor(econ_both_support <- project_support[treat_cat=="econ_gain" | treat_cat=="econ_loss"])) #142 no, 176 yes
summary(as.factor(eco_both_support <- project_support[treat_cat=="eco_gain" | treat_cat=="eco_loss"])) #116 no, 206 yes

#separate into gain and loss frames
summary(as.factor(gain_both_support <- project_support[treat_cat=="econ_gain" | treat_cat=="eco_gain"])) #116 no, 203 yes
summary(as.factor(loss_both_support <- project_support[treat_cat=="econ_loss" | treat_cat=="eco_loss"])) #142 no, 179 yes

#simple diff of proportions tests
print(econ_test <- prop.test(x = c(88, 88), n = c(156, 162), correct = FALSE)) #not sig. this is comparing to econ frames to one another
print(eco_test <- prop.test(x = c(115, 91), n = c(163, 159), correct = FALSE)) #p=0.01. this is comparing eco frames to one another
print(econ_control_test <- prop.test(x = c(176, 76), n = c(318, 161), correct = FALSE)) #marginal, p=0.09. comparing both econ frames to control
print(eco_control_test <- prop.test(x = c(206, 76), n = c(322, 161), correct = FALSE)) #p<0.001. comparing both ecological frames to control
print(gain_control_test <- prop.test(x = c(203, 76), n = c(319, 161), correct = FALSE)) #p<0.001. comparing gain frames to control
print(loss_control_test <- prop.test(x = c(179, 76), n = c(321, 161), correct = FALSE)) #maginal. p=0.08. comparing loss frames to control
print(gain_loss_test <- prop.test(x = c(203, 179), n = c(319, 321), correct = FALSE)) #p<0.05. comparing gain frames to loss frames
print(econ_eco_test <- prop.test(x = c(206, 176), n = c(322, 318), correct = FALSE)) #p<0.05. comparing ecological frames to economic frames

#urban-rural divide
summary(urban_mean <- mean(pretest_df$project_support[urban_rural==1], na.rm=T)) #0.6026, N=231
summary(suburban_mean <- mean(pretest_df$project_support[urban_rural==2], na.rm=T)) #0.549, N=408
summary(rural_mean <- mean(pretest_df$project_support[urban_rural==3], na.rm=T)) #0.585, N=164
summary(anova_1 <- aov(project_support ~ urban_rural, data=pretest_df)) #not a significant difference
summary(as.factor(urban_rural))

#partisan divide
summary(republican_mean <- mean(pretest_df$project_support[partyid==1], na.rm=T)) #0.675, N=163 (53 no, 110 yes)
summary(dem_mean <- mean(pretest_df$project_support[partyid==2], na.rm=T)) #0.553, N=372 (166 no, 205 yes)
summary(ind_mean <- mean(pretest_df$project_support[partyid==3 | partyid==4], na.rm=T)) #0.536, N=266 (123 no, 142 yes;include both independents and unaffiliated)
print(partyid_test <- prop.test(x = c(110, 205), n = c(163, 372), correct = FALSE)) #p<0.05. reps more likely to support than dems

#gender
summary(male_mean <- mean(pretest_df$project_support[gender==1], na.rm=T)) #0.630, N=350 (165 no, 281 yes)
summary(female_mean <- mean(pretest_df$project_support[gender==0], na.rm=T)) #0.499, N=351 (176 no,  175 yes)
summary(anova_1 <- aov(project_support ~ gender, data=pretest_df)) #sig difference

#age (binned)
pretest_df$agecat[age >=55] <- 4
pretest_df$agecat[age<55 & age>=40] <- 3
pretest_df$agecat[age<40 & age>=30] <- 2
pretest_df$agecat[age<30 & age>=18] <- 1
summary(young_mean <- mean(pretest_df$project_support[pretest_df$agecat==1], na.rm=T)) #0.618, N=293 (112 no, 181 yes)
summary(youngish_mean <- mean(pretest_df$project_support[pretest_df$agecat==2], na.rm=T)) #0.571, N=254 (109 no, 145 yes)
summary(middle_mean <- mean(pretest_df$project_support[pretest_df$agecat==3], na.rm=T)) #0.527, N=169 (80 no, 89 yes)
summary(old_mean <- mean(pretest_df$project_support[pretest_df$agecat==4], na.rm=T)) #0.506, N=85 (42 no, 43 yes)

summary(as.factor(pretest_df$project_support[pretest_df$agecat==4]))

#Grizzly Questions
attach(pretest_df)

hist(grizzly_support)

summary(grizz_general <- mean(pretest_df$grizzly_support[grizz_treatment=="Q43"], na.rm=T)) #mean=4.55, sd=1.45
summary(grizz_parks <- mean(pretest_df$grizzly_support[grizz_treatment=="Q44"], na.rm=T)) #mean=4.73, sd=1.51
grizz_parks <- as.numeric(grizz_treatment=="Q44")
t.test(grizzly_support, grizz_parks)

summary(anova_2 <- aov(grizzly_support ~ urban_rural, data=pretest_df)) #significant difference
summary(grizz_general <- mean(pretest_df$grizzly_support[urban_rural==1], na.rm=T)) #mean=4.86
summary(grizz_general <- mean(pretest_df$grizzly_support[urban_rural==2], na.rm=T)) #mean=4.62
summary(grizz_general <- mean(pretest_df$grizzly_support[urban_rural==3], na.rm=T)) #mean=4.38

summary(grizz_general <- mean(pretest_df$project_support[grizz_treatment=="Q43"], na.rm=T)) #mean=.57
summary(grizz_parks <- mean(pretest_df$project_support[grizz_treatment=="Q44"], na.rm=T)) #mean=.58
t.test(project_support, grizz_parks)
