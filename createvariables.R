#Create Variables

kuni_df <- read.csv("kunisurvey_coded_march16.csv", h=T)
View(kuni_df)

#Independent Variables

#Primary Analysis Using the Following:
#Demographic variables: race, gender, age (continuous), age (squared, for non-linear effects), rural/urban, occupation
#Resource variables: education, HH income
#Political variables: party ID, ideology
#Environmental variables: NEP, animal rights

#Demographic Variables
attach(kuni_df)

#Race
white[is.na(white)] <- 0
black[is.na(black)] <- 0
am_indian[is.na(am_indian)] <- 0
asian[is.na(asian)] <- 0
pac_islander[is.na(pac_islander)] <- 0
nonwhite <- as.numeric(black==1 | am_indian==1 | asian==1 | pac_islander==1 | latino==1)
summary(as.factor(nonwhite)) #420 non-white or latino, 553 non-latino white
#translates to 57% white, 21% hispanic/latino, 16% asian, 6% black, 4% american indian, 2% pacific islander (combined 43% non-white or latino)
#CA population: 38% non-hispanic white, 39% hispanic, 13% asian, 6% black, <1% each pacific islander & american indian, 3% mixed race
#US population: 76% white (incl hispanic), 17% hispanic (any race), 14% black, 6% asian, 2% american indian, <1% pacific islander

#Rural
rural <- as.numeric(urban_rural==3)
summary(as.factor(rural)) #329 urban, 397 suburban, 254 rural; 26% rural, 74% urban/suburban

#Age
summary(age) #mean=43.53, min=18, max=92
#157 NAs for age, which is a problem

#Age^2 for non-linear age effects (unlikely)
age2 <- age**2
summary(age2)

#Occupation
summary(as.factor(occupation)) #156 retired, 228 unemployed
summary(as.factor(employment_status)) 
#563 working (employed or self-employed), 103 unemployed, 142 retired, 63 disabled, 97 other
employed <- as.numeric(employment_status==1 | employment_status==2)
unemployed <- as.numeric(employment_status==3 | employment_status==4)

#gender
male <- as.numeric(gender==1)
summary(as.factor(male)) #609 females, 371 males; 62% women

#Resource Variables

#Educaiton
summary(as.factor(highest_degree)) #only 9 people not HS grads
college_degree <- as.numeric(highest_degree>=5)
summary(as.factor(college_degree)) #537 people no college degree, 442 BA or higher

#Income
summary(as.factor(hhincome))

#Political Variables

#Party ID
#specify independent as reference level
kuni_df <- within(kuni_df, partyid <- relevel(as.factor(partyid), ref=3))
republican <- as.numeric(partyid==1) #275 republicans
democrat <- as.numeric(partyid==2) #469 dems
independent <- as.numeric(partyid==3 | partyid==4) #236 independent/other

#Ideology
summary(ideology) #mean=3.68, min=1, max=7
#56 NAs

#Environmental Variables#

#New Ecological Paradigm
NEP <- (as.numeric(nep1) + (6-as.numeric(nep2_rev)) + (6-as.numeric(nep3_rev))
        + as.numeric(nep4) + as.numeric(nep5))/5
summary(NEP) #mean=3.30, median=3.20
hist(NEP) #pretty close to normally distributed

#Animal Rights
animalrights <- (as.numeric(animals_suffering) + as.numeric(animals_displace) + as.numeric(animals_veg) + (6-as.numeric(animals_humanuse_rev)))/4
summary(animalrights) #mean=3.30, median=3.25

#Replace blanks with zeros in treatments#
#attach(kuni_df)
kuni_df$ecogain[kuni_df$ecogain==""] <- 0
kuni_df$ecoloss[kuni_df$ecoloss==""] <- 0

#Categorical Variable for Treatment
attach(kuni_df)
kuni_df$treat_cat[ecogain==1] <- "ecogain"
kuni_df$treat_cat[ecoloss==1] <- "ecoloss"
kuni_df$treat_cat[econgain==1] <- "econgain"
kuni_df$treat_cat[econloss==1] <- "econloss"
kuni_df$treat_cat[control==1] <- "control"
summary(as.factor(treat_cat)) 

#Breakdown of treatments: 199 control, 194 ego-gain, 196 eco-loss, 194 econ-gain, 197 econ-loss

#reference level
treat_factor <- as.factor(treat_cat)
kuni_df <- within(kuni_df, treat_factor <- relevel(treat_factor, ref = "control"))

#Voting Behavior
trump_dummy <- as.numeric(votechoice==1)
clinton_dummy <- as.numeric(votechoice==2)
voted <- as.numeric(vote2016==4)

#Immigration
immigration_decrease <- as.numeric(immigration_change==3)

#Dependent Variables#

#Primary Analysis Using:
#1. Project Support (binary)
#2. Strength of Support from 1 (strongly oppose) to 6 (strongly support)
#3. Write letter to CDFW (still need to figure out coding for this)

#binary support
support_binary <- as.numeric(project_support==1)
summary(support_binary) #N=980, 72.76% of people support

#strength of support/opposition
attach(kuni_df)
kuni_df$support_oppose[support_strength==3] <- 6
kuni_df$support_oppose[support_strength==2] <- 5
kuni_df$support_oppose[support_strength==1] <- 4
kuni_df$support_oppose[oppose_strength==3] <- 1
kuni_df$support_oppose[oppose_strength==2] <- 2
kuni_df$support_oppose[oppose_strength==1] <- 3
support_oppose <- kuni_df$support_oppose
summary(support_oppose) #mean=4.07
summary(as.factor(support_oppose)) #Breakdown: 1=110, 2=115, 3=42, 4=194, 5=367, 6=151
hist(support_oppose) #skew toward positive end. could transform.
