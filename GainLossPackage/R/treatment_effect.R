#'treatment_effect
#'Calculating the mean, average treatment effect, and standard errors for a subset of the kuni data
#'
#'@param df is the dataframe that contains the kuni data or a subset of the kuni data, that indicates the support for 
#'the management policy. It must contain a column that specifies the treatment category of each observation.
#'
#'@author Elizabeth Hiroyasu
#'


treatment_effect<-function(df){
  ecogain<-df[treat=="ecogain"]
  ecoloss<-df[treat=="ecoloss"]
  econgain<-df[treat=="econgain"]
  econloss<-df[treat=="econloss"]
  control<-df[treat=="control"]
  
  m = c(mean(econgain, na.rm=TRUE), mean(econloss, na.rm=TRUE), mean(ecogain, na.rm=TRUE), mean(ecoloss, na.rm=TRUE))
  ATE=c(mean(econgain, na.rm=TRUE)-mean(control, na.rm=TRUE), mean(econloss, na.rm=TRUE)-mean(control, na.rm=TRUE), 
        mean(ecogain, na.rm=TRUE)-mean(control, na.rm=TRUE), mean(ecoloss, na.rm=TRUE)-mean(control, na.rm=TRUE))
  treat_cat_rep = c("Econ Gain", "Econ Loss", "Ecol Gain", "Ecol Loss")
  se = c(sd(econgain, na.rm=TRUE)/sqrt(length(econgain)), 
         sd(econloss, na.rm=TRUE)/sqrt(length(econloss)), 
         sd(ecogain, na.rm=TRUE)/sqrt(length(ecogain)),
         sd(ecoloss, na.rm=TRUE)/sqrt(length(ecoloss)))
  
  df<-data.frame(treat_cat_rep, m, se, ATE)
  df<-setNames(df, c("treat_cat", "mean", "se", "ATE"))
  
  return(list("ecogain"=ecogain, "ecoloss"=ecoloss, "econgain"=econgain, "econloss"=econloss, "control"=control, "df"=df))
}

