#Create Variables

kuni_df <- read.csv("kunisurvey_coded_march16.csv", h=T)
View(kuni_df)

#Independent Variables

#Primary Analysis Using the Following:
#Demographic variables: race, gender, age (continuous), age (squared, for non-linear effects), rural/urban, occupation
#Resource variables: education, HH income
#Political variables: party ID, ideology
#Environmental variables: NEP, animal rights

#TREATMENT#

#Categorical Variable for Treatment
attach(kuni_df)
kuni_df$treat_cat[ecogain==1] <- "ecogain"
kuni_df$treat_cat[ecoloss==1] <- "ecoloss"
kuni_df$treat_cat[econgain==1] <- "econgain"
kuni_df$treat_cat[econloss==1] <- "econloss"
kuni_df$treat_cat[control==1] <- "control"
attach(kuni_df)
summary(as.factor(treat_cat)) 
#Breakdown of treatments: 218 control, 217 ego-gain, 215 eco-loss, 216 econ-gain, 211 econ-loss

#Pooled
attach(kuni_df)
kuni_df$pooled[ecogain==1 | ecoloss==1] <- "eco"
kuni_df$pooled[econloss==1 | econgain==1] <- "econ"
kuni_df$pooled[control==1] <- "control"
attach(kuni_df)
summary(as.factor(pooled)) #432 eco, 427 econ, 218 control

attach(kuni_df)
kuni_df$pooled2[ecogain==1 | econgain==1] <- "gain"
kuni_df$pooled2[ecoloss==1 | econloss==1] <- "loss"
kuni_df$pooled2[control==1] <- "control"
attach(kuni_df)
summary(as.factor(pooled2)) #433 gain, 426 loss, 218 control

#reference level
treat_factor <- as.factor(treat_cat)
kuni_df <- within(kuni_df, treat_factor <- relevel(treat_factor, ref = "control"))

#Demographic Variables

#Race
attach(kuni_df)

white[is.na(white)] <- 0
black[is.na(black)] <- 0
am_indian[is.na(am_indian)] <- 0
asian[is.na(asian)] <- 0
pac_islander[is.na(pac_islander)] <- 0
nonwhite <- as.numeric(black==1 | am_indian==1 | asian==1 | pac_islander==1 | latino==1)
summary(pac_islander) #469 non-white or latino, 601 non-latino white
#translates to 56.2% white, 20.1% hispanic/latino, 17.3% asian, 5.9% black, 3.7% american indian, 2% pacific islander (combined 43.8% non-white or latino)
#CA population: 38% non-hispanic white, 39% hispanic, 13% asian, 6% black, <1% each pacific islander & american indian, 3% mixed race
#US population: 76% white (incl hispanic), 17% hispanic (any race), 14% black, 6% asian, 2% american indian, <1% pacific islander

#Rural
rural <- as.numeric(urban_rural==3)
summary(rural) # 367 urban, 450 suburban, 260 rural; 24.1% rural, 75.9% urban/suburban

#Age
summary(age) #mean=43.59, min=18, max=92
summary(age>25 & age<=35)
summary(age>50 & age<=65)
#157 NAs for age
sd(na.omit(age))

#Age^2 for non-linear age effects (unlikely)
age2 <- age**2
summary(age2)

#Occupation
summary(as.factor(occupation))
summary(as.factor(employment_status)) 
#604 working, 116 unemployed, 162 retired, 67 disabled, 113 other
employed <- as.numeric(employment_status==1 | employment_status==2)
unemployed <- as.numeric(employment_status==3 | employment_status==4)

#gender
male <- as.numeric(gender==1)
summary(as.factor(male)) #687 females, 390 males; 63.8% women

#Resource Variables

#Education
summary(as.factor(highest_degree)) #only 10 people not HS grads
college_degree <- as.numeric(highest_degree>=5)
summary(as.factor(college_degree)) #585 people no college degree, 491 BA or higher

#Income
summary(as.factor(hhincome))

length(hhincome)
#Political Variables

#Party ID
#specify independent as reference level
kuni_df <- within(kuni_df, partyid <- relevel(as.factor(partyid), ref=3))
republican <- as.numeric(partyid==1) #302 republicans, 28%
democrat <- as.numeric(partyid==2) #515 dems, 47.8%
independent <- as.numeric(partyid==3 | partyid==4) #260 independent/other, 24.1%

#Ideology
summary(ideology) #mean=3.68, min=1, max=7; #64 NAs ("Haven't thought about this much")
quantile(ideology, na.rm=T) #25%=2, 75%=5
sd(na.omit(ideology))
#moving toward "conservative"

#Voting Behavior
trump_dummy <- as.numeric(votechoice==1)
clinton_dummy <- as.numeric(votechoice==2)
voted <- as.numeric(vote2016==4)

#Immigration
immigration_decrease <- as.numeric(immigration_change==3)

#Environmental Variables#

#New Ecological Paradigm
NEP <- (as.numeric(nep1) + (6-as.numeric(nep2_rev)) + (6-as.numeric(nep3_rev))
        + as.numeric(nep4) + as.numeric(nep5))/5
summary(NEP) #mean=3.30, median=3.2; 25%=2.6, 75%=3.8
hist(NEP) #pretty close to normally distributed
sd(na.omit(NEP))
nep2 <- 6-nep2_rev
nep3 <- 6-nep3_rev
NEP_table <- cbind(nep1, nep2, nep3, nep4, nep5)
alpha(NEP_table) #alpha=.64, not great. about as good as it can be, though.

#Animal Rights
animalrights <- (as.numeric(animals_suffering) + as.numeric(animals_displace) + as.numeric(animals_veg) + (6-as.numeric(animals_humanuse_rev)))/4
summary(animals_suffering) #mean=4.07, median=4.0; 25%=4, 75%=5; min=1, max=5
library(psych)
human_rev <- 6-animals_humanuse_rev
animals_table <- cbind(animals_suffering, animals_displace, animals_veg, human_rev)
alpha(animals_table) #alpha=.61, pretty poor internal reliability when all four included
sd(animalrights, na.rm=T)
#Dependent Variables#

#Primary Analysis Using:
#1. Project Support (binary)
#2. Strength of Support from 1 (strongly oppose) to 6 (strongly support)
#3. Write letter to CDFW (still need to figure out coding for this)

#binary support
support_binary <- as.numeric(project_support==1)
summary(as.factor(support_binary)) #786 support, 291 oppose; 72.98% of people support

#Political Action in Support
support_letter_binary[is.na(support_letter_binary)] <- 0
support_action <- as.numeric(support_letter_binary==1)
summary(as.factor(support_action)) #134 yes, 943 no
summary(as.factor(support_action[project_support==1])) #134 yes, 652 no


#strength of support/opposition
kuni_df$treat_cat[ecogain==1] <- "ecogain"

attach(kuni_df)
kuni_df$support_oppose[support_strength==3] <- 6
kuni_df$support_oppose[support_strength==2] <- 5
kuni_df$support_oppose[support_strength==1] <- 4
kuni_df$support_oppose[oppose_strength==3] <- 1
kuni_df$support_oppose[oppose_strength==2] <- 2
kuni_df$support_oppose[oppose_strength==1] <- 3
summary(support_oppose) #mean=4.08
summary(as.factor(support_oppose)) #Breakdown: 1=118, 2=128, 3=45, 4=213, 5=399, 6=173
hist(support_oppose) #skew toward positive end. could transform.
support_oppose2 <- support_oppose**2 #transform for normality
hist(support_oppose2)
