#'Data Frame Function
#'
#'splits the data into dataframes for each treatment category. Treatments are ecogain, ecoloss, econgain, econloss, and control
#'
#'@param df is the dataframe that contains the kuni data
#'
#'@author Elizabeth Hiroyasu
#'


df.treat<-function(df){

  #separating by each treatment type
  df$treat[ecogain==1] <- "ecogain"
  df$treat[ecoloss==1] <- "ecoloss"
  df$treat[econgain==1] <- "econgain"
  df$treat[econloss==1] <- "econloss"
  df$treat[control==1] <- "control"
  
  #Separating by the ecology vs econ pooled treatments
  df$ecoecon[ecogain==1 | ecoloss==1] <- "eco"
  df$ecoecon[econloss==1 | econgain==1] <- "econ"
  df$ecoecon[control==1] <- "control"
  
  #Separating by the pooled gain vs loss treatments
  df$gainloss[ecogain==1 | econgain==1] <- "gain"
  df$gainloss[ecoloss==1 | econloss==1] <- "loss"
  df$gainloss[control==1] <- "control"

  return(df)
}