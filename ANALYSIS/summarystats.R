#Summary Statistics for Kuni Survey#
library(stargazer)

kuni_df <- read.csv("kunisurvey_coded_march8.csv", h=T)
View(kuni_df)

attach(kuni_df)

#Primary Analysis Using the Following:
#Demographic variables: race, gender, age (continuous), age (squared, for non-linear effects), rural/urban, occupation
#Resource variables: education, HH income
#Political variables: party ID, ideology
#Environmental variables: NEP, animal rights

demographics <- data.frame(white, nonwhite, male, age, rural, college_degree, hhincome, democrat, republican, independent, ideology)
stargazer(demographics, summary=T)

dvs <- data.frame(support_binary, support_oppose)
stargazer()