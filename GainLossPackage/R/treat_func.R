#'individual treatment function
#'
#'splits the data into dataframes for each treatment category then calculates the mean, 
#'average treatement effect, and standard error.
#'
#'@param df is the dataframe that contains the kuni data
#'
#'@author Elizabeth Hiroyasu
#'


treat_func<-function(df){
  attach(df)
  
  summary(ecogain <- support_binary[treat_cat=="ecogain"], na.rm=T)
  summary(ecoloss <- support_binary[treat_cat=="ecoloss"], na.rm=T)
  summary(econgain <- support_binary[treat_cat=="econgain"], na.rm=T)
  summary(econloss <- support_binary[treat_cat=="econloss"], na.rm=T)
  summary(control <- support_binary[treat_cat=="control"], na.rm=T)
  
  m = c(mean(econgain), mean(econloss), mean(ecogain), mean(ecoloss))
  ATE=c(mean(econgain)-mean(control), mean(econloss)-mean(control), 
        mean(ecogain)-mean(control), mean(ecoloss)-mean(control))
  treat_cat = c("Econ Gain", "Econ Loss", "Ecol Gain", "Ecol Loss")
  se = c(sd(econgain)/sqrt(length(econgain)), 
         sd(econloss)/sqrt(length(econloss)),
         sd(ecogain)/sqrt(length(ecogain)),
         sd(ecoloss)/sqrt(length(ecoloss)))
  
  treat.df<-data.frame(treat_cat, m, se, ATE)
  treat.df<-setNames(treat.df, c("treat_cat", "mean", "se", "ATE"))
  
  return(treat.df)
  
}