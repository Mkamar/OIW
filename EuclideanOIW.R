#invariance function
#d is the sequence being passed, window is the invariance widow
invariateWindow <- function(d,window){
  firstpass <- TRUE
  # cutting the time series into chuncks of size window
  splitted <- split(d, ceiling(seq_along(d)/window))
  
  for (winvec in splitted)
  {
    #sorting the windows
    winvec <- sort(winvec)
    if (firstpass == FALSE)
    {
      #linking sorted chuncks
      newd <- c(newd,winvec)
      
    }
    else
    {
      
      newd <- winvec
      firstpass = FALSE
    }
    
  }
  
  newd
  
}

#Euclidean OIW
Q_oiw <- function(v1,v2,w=0){
  oiwindow <- w
  v1 <- invariateWindow(v1,oiwindow)
  v2 <- invariateWindow(v2,oiwindow)
  dist(rbind(v1,v2))[1]
}
# calculating 1NN error rate
Calculate_Error_Rate <- function (w=0)
{
  
  wrong <- 0
  counter <-0
  for (i in 1:nrow(test))
  {
    counter <- counter +1
    nearest <- ''
    nearestVector <- ''
    for (j in  1:nrow(train))
    {
      d <- Q_oiw(as.numeric(as.vector(test[i,][-1])),as.numeric(as.vector(train[j,][-1])),w)
      if (nearest == '')
      {
        nearest <- d
        nearestVector <- j
      }
      else
      {
        
        if (d <= nearest)
        {
         
          nearest <- d
          nearestVector <-  j
        }
      }
      
    }
    
    if (test[i,][1] != train[nearestVector,][1])
    {
      wrong <- wrong +1
    }
    
  }

  c(0,w,wrong/allRecords)
}

results <- NULL
#loading training Dataset
train <- as.matrix(read.csv("C:/Users/t738277/Downloads/UCR_TS_Archive_2015/UCR_TS_Archive_2015/StarLightCurves/StarLightCurves_TRAIN" , header = FALSE ))
#Loading Test Dataset
test <- as.matrix(read.csv("C:/Users/t738277/Downloads/UCR_TS_Archive_2015/UCR_TS_Archive_2015/StarLightCurves/StarLightCurves_TEST", header = FALSE))
#Getting number of test set
allRecords <- nrow(test)
#getting length of time-series
timeSeriesLength <- ncol(test) -1
for ( i in 1:timeSeriesLength)
{
  results <<- rbind(results ,Calculate_Error_Rate(w=i))
  print(i)
}