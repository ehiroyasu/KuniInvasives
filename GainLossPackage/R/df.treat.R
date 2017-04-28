#'Data Frame Function
#'
#'splits the data into dataframes for each treatment category. Treatments are ecogain, ecoloss, econgain, econloss, and control
#'
#'@param df is the dataframe that contains the kuni data
#'@param treat is the treatment categories to split the dataframe into, dataframes for each treatment
#'
#'@author Elizabeth Hiroyasu
#'




df.treat<-function(df, treat){
  df$treat[ecogain==1] <- "ecogain"
  df$treat[ecoloss==1] <- "ecoloss"
  df$treat[econgain==1] <- "econgain"
  df$treat[econloss==1] <- "econloss"
  df$treat[control==1] <- "control"
  
  return(df)
}