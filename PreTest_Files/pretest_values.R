#Regressions for support of management plans#

altruism <- (MSV1 + MSV2 + MSV6 + FSV1 + FSV2 + FSV6)/3
egoism <- (MSV3 + MSV4 + MSV5 + FSV3 + FSV4 + FSV5)/3

summary(lm_values <- lm(project_support ~ altruism + egoism, data=pretest_df))
#R2=.02
#altruism has significant negative effect
#egoism has non-significant effect
ideology2 <- ideology^2
project_strength <- support_strength + oppose_strength
summary(lm_values <- lm(project_support ~ altruism + egoism + ideology + ideology2 + as.factor(partyid) + education + 
                          income_cat + race + gender + age, data=pretest_df))
#altruists less likely to support project
#conservatives less likely to support project (strange?)

summary(lm_values <- lm(project_strength ~ altruism + egoism + ideology + ideology2 + as.factor(partyid) + education + 
                          income_cat + race + gender + age, data=pretest_df))
summary(as.factor(project_strength))
hist(project_strength)
hist(ideology)
