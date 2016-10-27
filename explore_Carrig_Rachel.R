#before beginning, I require the ggplot2 so that we can use the diamonds dataset
require(ggplot2)

#we import the data from the diamonds dataframe
data("diamonds")

#we create the variable df, which can be used with any dataframe. 
#here, we have used the dataframe diamonds, to show that the codes work
#for all df = diamonds, but diamonds can be replaced by any dataframe.
#additionally, df could be replaced within the code directly with the name of
#a different dataset

df = data.frame(diamonds)

#--------------
#PART B - please note I've done the second part of the assignment first, since we need these functions to be used
#in our overarching explore function.
#---
#1: A frequency table for every categorical and logical variable
freqtable <- function(df){
  #here, we create a function where the parameters are the data frame and the output is a frequency table of the logical
  #and characteristic variables

  logicdf <- df[sapply(df, is.logical)] #first, we create a dataframe with the logical varaibles
  factordf <- df[sapply(df, is.factor)] #then, we create a dataframe with the categorical variables
  logicfactordf <- data.frame(logicdf,factordf) #we then combine those two dataframes into one dataframe
  return(table(logicfactordf)) #we return a frequency table of the new dataframe we created

}
freqtable(df) #here, we test the function

#--------------
#2A: A summary statistics table for each numerical variable  

summarytable <- function(df){
  #we create a function freqtable that has parameters of a dataframe and returns the summary statistics of the numerical
  #columns within that dataframe
  
  numdflist <- sapply(df, is.numeric)
  #first, we create the variable numdflist, which will be the numeric columns in the given dataframe
  
  sumstats = sapply(df[,numdflist], summary) #then, we apply the summary function to the columns of the dataframe
  
  return(sumstats) #this returns the table we have created
}
summarytable(df) #here, we test the function

#--------------
#2B: data frame containing each pair of column names in the 1st column and the associated r-square value in the 2nd column.

rsquarefunc <- function(df){

numdf <- df[sapply(df, is.numeric)]
#we create the numdf variable dataframe, taking all columns from the initial df and keeping only the numeric ones

  x <- combn(colnames(numdf), 2) #finds all combinations of two pairs of column names
  colpairs <- paste(x[1,],x[2,],sep = '~')
  #we put them together separating them with a ~, so that we can use the next function
  
  rsquare <- unname(sapply(colpairs, function(l) summary(lm(l, data = numdf))$r.squared))
  #we create rsquare, which is the a new function taking the linear model of the data from our column pairs and taking the
  #r squared value of those pairs (which is why we need the ~ above)
  
  colpairs <- paste(x[1,],x[2,],sep = '-')
  #here, we re-make colpairs separating them with a '-' as desired
  
  rsquared <- data.frame(colpairs,rsquare) #now, we make a data frame using the column pairs and r square values we made
  colnames(rsquared) <- c("Variable Pairs", "R-Square") #we rename the columns as desired

  return(rsquared) #this returns the desired dataframe
}

rsquarefunc(df) #here, we test the function
#--------------
#2C: A data framethat contains eachpair of column names in the first column and correlation coefficient (Pearson) 
#for all coefficients whose absolute value is greater than the correlation threshold in the second column

pearsoncoef <- function(df){
  #our function here is pearsoncoef.  the parameters are a dataframe df, and the return is a new dataframe that takes
  #each pair of numeric columns in df and finds the absolute value of their pearson correlation, printing the pairs 
  #and correlations as columns
  
  numdf <- df[sapply(df, is.numeric)]
  #we create the numdf variable dataframe, taking all columns from the initial df and keeping only the numeric ones
  
  if(ncol(numdf) >= 2) { #we only want the number of columns to be 2
    
    x <- combn(colnames(numdf), 2) #finds all combinations of two pairs of column names
    colpairs <- paste(x[1,],x[2,],sep = '-')
    #this is the same colpairs variable as before, pasted here so the function can run by itself if needed
  
    z <- abs(cor(numdf, method = "pearson")) #here, we create variable z that takes the absolute value of the pearson
    #correlation and creates a matrix of those values
    
    correlation <- z[which(lower.tri(z))] #here, we create variable correlation which takes the values in the lower triangular
    #matrix of z, because they match the column pairs
    
    cordf <- data.frame(colpairs, correlation) #now, we create our new dataframe with the pairs desired 
    colnames(cordf) <- c("Variable Pairs","Pearson Exceeds Threshold") #we rename the columns within the dataframe as desired
  
    return(cordf) #this returns the new dataframe we have created

  }
}
pearsoncoef(df) #here, we test the function


#--------------
#PART A
explore <- function(data.frame,plotswitch,threshold,binsize){
  #our explore function takes the parameters of a dataframe, a plotswitch, a threshhold cutoff, and an optional vector
  #representing the numbers of bins to use for a histogram. since i'm not in groups a,b,c, or d, no defensive 
  #programming is needed here
  
  x = list(freqtable(df),summarytable(df),rsquarefunc(df),pearsoncoef(df)) #we create a list x here, that takes all the
  #functions we created earlier and puts them in a list
  
  return(x) #we return the list we created

}
explore(df) #here, we test the function