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
    TS$base <- bf_eckhardt(TS$Flow, alpha, BFindex)
    
    TS$cumsum <- NA
    year_list <- unique(TS$hyear)
    
    # setup output data.frame
    out <- as.data.frame(array(data = NA, dim = c(length(year_list), length(bfpct))))
    colnames(out) <- bfpct
    
    # convert percent to fraction for searching
    bfpct <- bfpct / 100
    
    
    # add cumulative sum column to time series

    
    for (i in 1:length(year_list)) { #loop through years
      
      # only look for day of each percent if baseflow > 0 for the year
      if (sum(TS$base[TS$hyear == year_list[i]]) > 0) {
        
        TS$cumsum[TS$hyear == year_list[i]] <- cumsum(TS$base[TS$hyear == year_list[i]]) / sum(TS$base[TS$hyear == year_list[i]])
        
        for (b in 1:length(bfpct)) {
          out[i, b] <- min(TS$doy[(TS$hyear == year_list[i]) & (TS$cumsum >= bfpct[b])])
        }

      } else {
        out[i,c(1:length(bfpct))] <- 0
      }
      
    } # end of year loop
    

    #calculate spring flood duration
    out$Dur <- out[, which.max(bfpct)] - out[, which.min(bfpct)]
    
    for (i in 1:4) {
        attr(out[,i], "times") <- as.character(unique(TS$hyear))
    }
    
    colnames(out) <- c("Start", "Mid", "End", "Dur")
    return(out)
    
}