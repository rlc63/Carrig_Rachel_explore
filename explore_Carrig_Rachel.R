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
#ADDED FOR HOMEWORK 8

#1 - If the plot switch parameter is "on" or "grid", then plot a pair of blue histograms with a vertical red line at the 
#mean (one using counts and the other density) for every numerical variable at each number of bins integer specified in
#the bin vector parameter. If the plot switch is set to “grid”, there should be a gridfor each count- bin combination 
#and a separate gridfor each density-  bin size combination.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {#here we create the function multiplot
  plots <- c(list(...), plotlist) #we create the list plots combining ... and a plotlist
  numPlots = length(plots) #we create a variable numplots that is the length of the list plots
  
  if (is.null(layout)) { #if the layout is null, we use the columns to create the layout
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols)) #here we create the layout, with the number of columns
    #in the plot and number of rows calculated from the number of columns
  }
  
  if (numPlots==1) {
    print(plots[[1]]) #if the length of the plots is 1, print plots[[1]]
    
  } else { #if the length of the plots is not 1
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout)))) #set up the new page with each plot in the
    #correct location
  
    for (x in 1:numPlots) { #for each x in numplots, get the matrix positions of the subplot
      matchidx <- as.data.frame(which(layout == x, arr.ind = TRUE))
      
      print(plots[[x]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col)) #print the plots
    }
  }
}


numplot <- function(df, plotswitch, binvector) {
  #here we create a new function, where if the plot switch parameter is "on" or "grid", numplot plots blue histograms
  #with a vertical red line at the mean for every numerical variable at each number of bins integer specified in the
  #bin vector parameter. If plot switch is grid, numplot prints a grid for each count-bin combo and a separate one for
  #each density-bin combo
  #parameters - df=dataframe, plotswitch=string, binvector=vector
  #returns - grid plots with a count and density histogram

  num <- df[sapply(df,is.numeric)] #we make a new dataframe with the numeric columns of the dataset
  
  for(name in colnames(df)) { #We loop through the columns of the dataset and use if-else statements for each case

    if(plotswitch == "on"){ #when the plotswitch is on
      grid.newpage()          
      m <- lapply(df[name], mean) #find the mean of the column so we can plot our vertical line
      
      plot1 <- ggplot(df, aes_string(name)) + geom_histogram(fill="blue") + 
        geom_vline(xintercept = m[[1]], colour="red") 
      plot2 <- ggplot(df, aes_string(name)) + geom_histogram(aes(y= ..density..), fill="blue") + 
        geom_vline(xintercept = m[[1]], colour="red")
      #we create two histograms, adding our vertical line at the mean in red
      
      multiplot(plot1, plot2, cols = 1) #we use the multiplot to plot the histograms into our grid
    }
    
    if(plotswitch == "grid"){    #if the plotswitch is grid
      m <- lapply(df[name], mean) #find the mean of the column so we can plot our vertical line
      
      count_plots <- list() #Create list to store histogram subplots of each bin size
      density_plots <- list() #Create list to store the density histograms subplots of each bin size
      
      if(missing(binvector)){ #if there is no binvector inputted, we use the default of 30 bins
        print(ggplot(df, aes_string(name), color = "blue") + geom_histogram(fill="blue")+ 
                labs(title= "default bins")) + geom_vline(xintercept = m[[1]], colour="red") 
        print(ggplot(df, aes_string(name), color = "blue") + 
                geom_histogram(aes(y= ..density..), fill="blue")+ labs(title= "default bins")) + 
          geom_vline(xintercept = m[[1]], colour="red") 
        #again, print out two histograms (one for density and one for count)
        
      }else{ #for any other situation              
        for(y in 1:length(binvector)) {    #here, we loop through each bin size in binvector and create a subplot
          k <- ggplot(df, aes_string(name), color = "blue") + 
            geom_histogram(fill="blue", bins = binvector[y])+ labs(title= paste(binvector[i], "bins"))+ 
            geom_vline(xintercept = m[[1]], colour="red") 
          count_plots[[y]] <- k  #this puts the subplots into a list
        }
        multiplot(plotlist = count_plots, cols = 2)  #here, we use the multiplot function to plot the graphs
        
        for(z in 1:length(binvector)) { #we repeat the process for the second histogram here
          k <- ggplot(df, aes_string(name), color = "blue") + 
            geom_histogram(aes(y= ..density..), fill="blue", bins = binvector[z])+ 
            labs(title= paste(binvector[z], "bins"))+ geom_vline(xintercept = m[[1]], colour="red") 
          density_plots[[z]] <- k       
        }
        multiplot(plotlist = density_plots, cols = 2)
        
      }
    }
  }
}


#2 If the plot switch parameter is "on" or "grid", plot a gray bar graph for every categorical and binary variable
catbinaryplot <-function(df, plotswitch){
  #here, we create a function to plot the gray bar graph as desired when plot switch parameter is "on" or "grid"
  #paraameters - df - dataframe, plotswitch - a string, and returns bar graphs

  #here, we go through the different cases
  catbinary <- sapply(df, function(x) (is.factor(x) || is.logical(x)))    
  catbinarydata <- df[catbinary] #here, we have extracted the categorical and binary columns 
  
  if(plotswitch == "on" || plotswitch == "grid") { #if the plotswitch is on or grid
    
    for(name in colnames(catbinarydata)) { #for each name in the new dataframe with categorical and binary variables
      #we plot bar graphs for each column
      catplot <- ggplot(catbinarydata, aes_string(name), color = "grey") + geom_bar(fill="grey")
      #we create the graph and plot it using ggplot
      print(catplot) #then we print the result
    }
  }
}


#--------------
#PART A
#UPDATED FOR HW 8
explore <- function(data.frame,plotswitch,threshold,binvector){
  #our explore function takes the parameters of a dataframe (data.frame), a plotswitch (accepting on, off, or grid),
  #a threshhold cutoff(between 0 and 1 for correlations), and an optional bin vector representing the numbers of bins 
  #to use for a histogram, and returns an r list
  
  outputplot <- numplot(data,plotswitch,binvector)
  +  outputplot2 <- catbinaryplot(data,plotswitch)
  
  x = list(freqtable(df),summarytable(df),rsquarefunc(df),pearsoncoef(df)) #we create a list x here, that takes all the
  #functions we created earlier and puts them in a list
 

  return(x) #we return the list we created

  
  
}
explore(df) #here, we test the function
