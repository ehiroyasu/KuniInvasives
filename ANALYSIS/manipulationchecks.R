#Manipulation Checks

#Variables Used for Manipulation Checks#

#1. pigsproblem (everyone): 1=bad economically; 2=bad ecologically; 3=other/dk
#2. eco_test (eco treated): 1=prevent declines; 2=allow increases; 3=other/dk
#3. econ_test (econ treated): 1=prevent economic damage; 2=allow economic benefits; 3=other/dk

#why_eco1: 
#supportwhy_eco2
#supportwhy_econ1
#supportwhy_econ2
#supportwhy_econ3

attach(kuni_df)
kuni_df$treat_cat[ecogain==1] <- "ecogain"
kuni_df$treat_cat[ecoloss==1] <- "ecoloss"
kuni_df$treat_cat[econgain==1] <- "econgain"
kuni_df$treat_cat[econloss==1] <- "econloss"
kuni_df$treat_cat[control==1] <- "control"
summary(as.factor(treat_cat)) 

#pigsproblem question
summary(as.factor(pigsproblem)) #1=291, 2=542, 3=143
pigsproblem_table <- table(treat_cat, pigsproblem)
print(pigsproblem)
chisq.test(pigsproblem, treat_cat) #sig difference in responses

#eco_test question: 1=prevent declines; 2=allow increases; 3=other/dk
attach(kuni_df)
kuni_df$treat_eco[ecogain==1] <- "ecogain"
kuni_df$treat_eco[ecoloss==1] <- "ecoloss"
summary(as.factor(treat_eco)) 
ecotest_table <- table(eco_test, treat_eco)
print(ecotest_table)
chisq.test(eco_test, treat_cat) #sig difference among groups

#econ_test question: 1=prevent economic damage; 2=allow economic benefits; 3=other/dk
attach(kuni_df)
kuni_df$treat_econ[econgain==1] <- "econgain"
kuni_df$treat_econ[econloss==1] <- "econloss"
attach(kuni_df)
summary(as.factor(treat_econ))
econtest_table <- table(econ_test, treat_econ)
print(econtest_table)
chisq.test(econ_test, treat_cat) #not sig differences. suggests that econ gain and loss not distinguished from one another
