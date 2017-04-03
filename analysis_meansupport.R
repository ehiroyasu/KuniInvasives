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
summary(ecogain_mean <- mean(support_binary[republican==1], na.rm=T)) #0.764 is overall number
summary(ecogain_mean <- mean(support_binary[treat_cat=="ecogain" & republican==1], na.rm=T)) #0.793
summary(ecoloss_mean <- mean(support_binary[treat_cat=="ecoloss" & republican==1], na.rm=T)) #0.833
summary(econgain_mean <- mean(support_binary[treat_cat=="econgain" & republican==1], na.rm=T)) #0.732
summary(econloss_mean <- mean(support_binary[treat_cat=="econloss" & republican==1], na.rm=T)) #0.793
summary(control_mean <- mean(support_binary[treat_cat=="control" & republican==1], na.rm=T)) #0.660
#Ordering: eco-loss, econ-loss & eco-gain (tied), econ-gain, control

#Among Democrats
summary(overall_mean <- mean(support_binary[democrat==1], na.rm=T)) #0.697 is overall number
summary(ecogain_mean <- mean(support_binary[treat_cat=="ecogain" & democrat==1], na.rm=T)) #0.733
summary(ecoloss_mean <- mean(support_binary[treat_cat=="ecoloss" & democrat==1], na.rm=T)) #0.885
summary(econgain_mean <- mean(support_binary[treat_cat=="econgain" & democrat==1], na.rm=T)) #0.643
summary(econloss_mean <- mean(support_binary[treat_cat=="econloss" & democrat==1], na.rm=T)) #0.702
summary(control_mean <- mean(support_binary[treat_cat=="control" & democrat==1], na.rm=T)) #0.558
#Ordering: eco-loss, eco-gain, econ-loss, econ-gain, control

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



