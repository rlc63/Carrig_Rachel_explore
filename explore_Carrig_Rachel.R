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

explore <--function(data.frame){}


#PART B
#---
#1: A frequency table for every categorical and logical variable
freqdf <- sapply(df, is.logical)
print(freqdf)

freqdf <- sapply(freqdf, is.c)

#---
#2A: A summary statistics table for each numerical variable  

freqtable <- function(df){
  #we create a function freqtable that has parameters of a dataframe and returns the summary statistics of the numerical
  #columns within that dataframe
  
  numdflist <- sapply(df, is.numeric)
  #first, we create the variable numdflist, which will be the numeric columns in the given dataframe
  
  sumstats = sapply(df[,numdflist], summary) #then, we apply the summary function to the columns of the dataframe
  
  return(sumstats) #this returns the table we have created
}

freqtable(df) #here, we test the function
#---
#2B:
numdf <- df[sapply(df, is.numeric)]
#we create the numdf variable dataframe, taking all columns from the initial df and keeping only the numeric ones

if(ncol(numdf) >= 2) {
  x <- combn(colnames(numdf), 2) #finds all combinations of two pairs of column names
  colpairs <- paste(x[1,],x[2,],sep = '-')
  
  y <- combn(col(numdf), 2)
  
  
  colpairs.lm <- lm(.^2 ~ .^2, data = numdf)
  summary(colpairs.lm)$r.squared 
}


#-----
if(ncol(a) >= 2) {
  b <- combn(colnames(a), 2) #finds all combinations of the name pairs
  
  pairs <- paste(b[1,], b[2, ], sep = "-") #makes sure that the column names
  #are seperated by - using paste function to paste the columns
  
  c <- cor(a, method = "pearson")#finds the pearson correlation using the cor 
  #function and creates of matrix of the values
  
  correlation <- c[which(lower.tri(c))] #gets the correclation values of the lower 
  #triangular matrix since those match the column pairs 
  
  newdf <- data.frame(pairs, correlation) #create a new data frame with our pairs 
  return(newdf)
  
}
else  #print this message if we can't find Pearson correlation
  print("Pearson Correlation cannot be computted because there are not enough numeric columns")
}
corCoef(diamonds)#test case 



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
x = list(freqtable(df),pearsoncoef(df))

print(x)