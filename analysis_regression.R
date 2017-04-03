#Regression analysis#

#Primary Analysis Using the Following:
#Demographic variables: race, gender, age (continuous), age (squared, for non-linear effects), rural/urban, occupation
#Resource variables: education, HH income
#Political variables: party ID, ideology
#Environmental variables: NEP, animal rights 

#Binary Support for project - Logit

#just with treatments
summary(support_logit <- glm(support_binary ~ as.factor(treat_cat) + NEP + animalrights + partyid + ideology + college_degree + hhincome 
                             + nonwhite + male + nonwhite + age + age2 + rural, data=kuni_df, family=binomial(link="logit"))) #control is ref factor
#not currently included: occupation (need to code them)

#CURRENT RESULTS (March 15)
#eco-loss has largest effect (p<.01)
#eco-gain second largest effect (p<.01)
#econ-loss also positive effect (p<.05)
#econ-gain treatment does not have significant effect
#controls: animal rights (negative), male (positive)

#Strength of support - ologit
supportstrength_ologit <- zelig(as.factor(support_oppose) ~ as.factor(treat_cat) + NEP + animalrights + partyid + ideology + college_degree + hhincome 
                                + nonwhite + male + nonwhite + age + rural, model="ologit", data = kuni_df)
summary(supportstrength_ologit)
#eco-gain, eco-loss both have positive effects on strength of support. 
#econ-gain and econ-loss have no sig effect
#control variables: animalrights (negative effect); nothing else has sig effect
