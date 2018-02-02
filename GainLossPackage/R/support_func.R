#'Support_func
#'
#'A function to calculated the mean support, mean control, sample size, standard error, average treatment effect, and 
#'confidence intervals for a treatment group.
#'
#'@param df is the dataframe that contains the kuni data or a subset of the kuni data, that indicates the support for 
#'the management policy. It must contain a column that specifies the treatment category of each observation.
#'@param support.df is a subset of the kuni data where support.df <- as.numeric(df$project_support==1)
#'@param treat.type vector of treatments that you are interested in pooling, note that the third element should always be "control"
#'
#'@author Elizabeth Hiroyasu
#'


support_func<-function(support.df, treat.type, df){

  support<-mean(support.df[df$treat==treat.type[1] | df$treat==treat.type[2]], na.rm=TRUE)
  control<-mean(support.df[df$treat==treat.type[3]], na.rm=TRUE)
  N<-length(support.df[df$treat==treat.type[1] | df$treat==treat.type[2]])
  se<-(sd(support.df[df$treat==treat.type[1] | df$treat==treat.type[2]]))/sqrt(N) 
  ATE<-support-control
  ci<-ATE + c(-qnorm(0.975),qnorm(0.975))*sqrt(abs((1/N)*ATE*(1-ATE)))
  
  return(list(support=support, control=control, N=N, se=se, ATE=ATE, ci=ci))
}