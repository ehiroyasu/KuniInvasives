#'treatment_effect
#'Calculating the mean, average treatment effect, and standard errors for a subset of the kuni data
#'
#'
#'@param df is the dataframe that contains the kuni data or a subset of the kuni data, that indicates the support for 
#'the management policy. It must contain a column that specifies the treatment category of each observation.
#'
#'@author Elizabeth Hiroyasu
#'


treatment_effect<-function(df){

  support_vec <- as.numeric(df$project_support==1)
  
  ecogain<-support_vec[df$treat=="ecogain"]
  ecoloss<-support_vec[df$treat=="ecoloss"]
  econgain<-support_vec[df$treat=="econgain"]
  econloss<-support_vec[df$treat=="econloss"]
  control<-support_vec[df$treat=="control"]
  
  m = c(mean(econgain, na.rm=TRUE), mean(econloss, na.rm=TRUE), mean(ecogain, na.rm=TRUE), mean(ecoloss, na.rm=TRUE), mean(control, na.rm=TRUE))
  ATE=c(m[1]-m[5], m[2]-m[5], m[3]-m[5], m[4]-m[5])
  treat_cat_rep = c("Econ Gain", "Econ Loss", "Ecol Gain", "Ecol Loss")
  se = c(sd(econgain, na.rm=TRUE)/sqrt(length(econgain)), 
         sd(econloss, na.rm=TRUE)/sqrt(length(econloss)), 
         sd(ecogain, na.rm=TRUE)/sqrt(length(ecogain)),
         sd(ecoloss, na.rm=TRUE)/sqrt(length(ecoloss)))
  
  te.df<-data.frame(treat_cat_rep, m[-5], se, ATE)
  te.df<-setNames(te.df, c("treat_cat", "mean", "se", "ATE"))
  
  return(list("support_vec"=support_vec, "ecogain"=ecogain, "ecoloss"=ecoloss, "econgain"=econgain, "econloss"=econloss, "control"=control, "te.df"=te.df))
}

