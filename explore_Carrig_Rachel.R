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

#---
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

#---
#2B: data frame containing each pair of column names in the 1st column and the associated r-square value in the 2nd column.

rsquarefunc <- function(df){

numdf <- df[sapply(df, is.numeric)]
#we create the numdf variable dataframe, taking all columns from the initial df and keeping only the numeric ones

  x <- combn(colnames(numdf), 2) #finds all combinations of two pairs of column names
  colpairs <- paste(x[1,],x[2,],sep = '-')
  
  rsquare = 0
  for (i in 1:(length(numdf)-1)) {
    for (j in (i+1):length(numdf)) {
      rsquare <-c(rsquare, summary(lm(numdf[[i]]~numdf[[j]]))$r.squared)
      rsquare <- rsquare[-1]
    }
  }

  rsquared = data.frame(colpairs,rsquare)
  return(rsquared)
}

  ###GEORGE'S ANSWER#####
        square=0#set the initial value of square as 0
        for (i in 1:(length(numdf)-1))#for loop for i, because we are taking pairs of each variables therefore the for loop can only be defined over 1:length(data_num)-1
        {#the begin of the for loop of i
          for (j in (i+1):length(numdf))#the inner loop of j in each i, because we are taking pairs so only consider the variables after i, so the loop of j is defined over (i+1):length(data_num)
          {#the begin of the j loop
            square <- c(square, 
                        summary(lm(numdf[[i]]~numdf[[j]]))$r.squared)#square<-c(square,...) will keep the value of each time of loop. lm(data_num[[i]]~data_num[[j]]) will return a linear model of the ith and jth variable fo data_num. summary()will return a basic information fo this model and r.squared is one of those information. we use $ to extract that value.
          }#the end of the inner loop of j which will return the r-square value of i and the variables after i.
        }#the end of the loop of i which will return the r-square value for every pair of numerical variables.
        square <- square[-1]#delete the initial value
        rsquare<-data.frame(string, square)#

#colpairs.lm <- lm(carat ~ cut, data=numdf)
##summary(colpairs.lm)$r.squared  


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



#---
#PART A
explore <- function(data.frame,plotswitch,threshold,binsize){
  #our explore function takes the parameters of a dataframe, a plotswitch, a threshhold cutoff, and an optional vector
  #representing the numbers of bins to use for a histogram
  
  x = list(freqtable(df),summarytable(df),rsquarefunc(df),pearsoncoef(df)) #we create a list x here, that takes all the
  #functions we created earlier and puts them in a list
  
  return(x) #we return the list we created

}
explore(df) #here, we test the function