#' Calculate baseflow peak statistics
#' 
#' This function finds the start, middle, end, and duration of the baseflow
#' peak based on percent of the total annual baseflow volume.
#' A value of 0 is returned for years with no flow. Hydrologic 
#' years with fewer than normal observations (outliers) are excluded from the 
#' analysis, and for stations with seasonal flow records, additional seasonal 
#' subsetting is done to include only days with observations in all years.
#' @param TS output from \code{\link{create.ts}} containing a data.frame of flow
#'   time series
#' @param bfpct numeric vector of percentages used to define the 
#'   start, middle, and end of the baseflow peak. Default is c(25, 50, 75)
#' @details This function calculates metrics intended to focus on snowmelt-related
#'   streamflow occuring in spring and summer. For catchments in cold climates, 
#'   the baseflow peak can be interpreted as snowmelt-induced. Baseflow is estimated with 
#'   \code{\link{bf_eckhardt}}. If total annual flow is equal to 0, returns NA for that
#'   year.
#' @return Returns a data.frame with the following columns:
#'   \itemize{
#'     \item Start - day of year defining the start of the baseflow peak
#'     \item Mid - day of year defining the middle of the baseflow peak
#'     \item End - day of year defining the end of the baseflow peak
#'     \item Dur - duration of the baseflow peak, in days
#'   }
#' @author Jennifer Dierauer
#' @export
#' @examples
#' data(cania.sub.ts)
#' res1 <- pk.bf.stats(cania.sub.ts)
#' 
#' # trend and changepoint plot for baseflow peak start doy
#' res2 <- screen.metric(res1[,1], "Day of Year")


pk.bf.stats <- function(TS, bfpct=c(25,50,75)) {
    
  NumRecords <- tapply(TS$Flow, TS$hyear, length)
  outliers <- grDevices::boxplot.stats(NumRecords)$out
  if (length(outliers) > 0) {
    outliers <- outliers[outliers < stats::median(NumRecords)]
    if (length(outliers) > 0) {
      YearList <- attr(NumRecords, "dimnames")[[1]][NumRecords > max(outliers)]
      TS <- TS[TS$hyear %in% YearList, ]
    }
  }
  
  ##### use only doys that are in every year
  doy.list <- tapply(TS$Flow, TS$doy, length)
  doy.list <- attr(doy.list, "dimnames")[[1]][doy.list >= stats::median(doy.list)]
  TS <- subset(TS, TS$doy %in% doy.list)
    
    ### Set parameter values for Eckhardt RDF
    BFindex <- 0.8
    alpha <- 0.970 ##based on values suggested by Eckhardt 2012 for perrennial stream
    
    ## calculate daily BF and BFI
    temp <- TS
    temp$ro <- temp$Flow - bf_eckhardt(temp$Flow, alpha, BFindex)
    
    ## calculate 10%, 50%, and 90% of annual baseflow volume
    BFVy <- array(data=NA, c(length(unique(temp$hyear)), length(bfpct) + 1))
    colnames(BFVy) <- c("Sum", bfpct)
    BFVy[,1] <- tapply(temp$ro, temp$hyear, sum)

    for (i in 1:length(bfpct)) {
        BFVy[,i+1] <- BFVy[,1] * (bfpct[i]/100)
    }
    
    output <- data.frame(BFVy[,c(2:ncol(BFVy))])
    colnames(output) <- bfpct
    
    YearStack <- split(temp, temp$hyear)  #split into dataframes by year for looping
    
    for (i in 1:length(YearStack)) { #loop through years
        
      if (BFVy[i, 1] == 0) { # if no baseflow in hyear, return 0
        output[i, ] <- 0
      } else {
        Have10 <- FALSE
        Have50 <- FALSE
        Have90 <- FALSE
        mysum <- 0
        temp.sub <- YearStack[[i]]
        j <- 0
        
        while (Have90 == FALSE) { #loop through days until Q90 value is reached
          j <- j + 1
          
          mysum <- mysum + temp.sub$ro[j]
          
          if (mysum > BFVy[i,2] & Have10 == FALSE) {
            Have10 <- TRUE
            output[i,1] <- as.numeric(format(temp.sub$Date[j], "%j"))
          } else if (mysum > BFVy[i,3] & Have50 == FALSE) {
            Have50 <- TRUE
            output[i,2] <- as.numeric(format(temp.sub$Date[j], "%j"))
          } else if (mysum > BFVy[i,4] & Have90 == FALSE) {
            Have90 <- TRUE
            output[i,3] <- as.numeric(format(temp.sub$Date[j], "%j"))
          }
          
        }
      }
    }
    
    #calculate spring flood duration
    output$Dur <- output[,3] - output[,1]
    
    for (i in 1:4) {
        attr(output[,i], "times") <- as.character(unique(temp$hyear))
    }
    
    colnames(output) <- c("Start", "Mid", "End", "Dur")
    return(output)
    
}