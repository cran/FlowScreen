#' Center of Volume
#' 
#' This function calculates center of volume metrics, including the day of the 
#' hydrologic year that 25 percent, 50 percent, and 75 percent of the total annual 
#' streamflow is reached. A value of 0 is returned for years with no flow. Hydrologic 
#' years with fewer than normal observations (outliers) are excluded from the 
#' analysis, and for stations with seasonal flow records, additional seasonal 
#' subsetting is done to include only days with observations in all years.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param threshold.missing Numeric value indicating the fraction of data that can 
#'   be missing in a single year. Years with a missing data above this threshold will have
#'   NA values returned. Default is 0.5 (max of 50\% missing data allowed). 
#' @return Returns a data.frame with the following columns:
#'   \itemize{
#'     \item hYear - Hydrologic Years
#'     \item Q25 - day of hydrologic year for 25 percent of the total annual streamflow
#'     \item Q50 - day of hydrologic year for 50 percent of the total annual streamflow, i.e. Center of Volume
#'     \item Q75 - day of hydrologic year for 75 percent of the total annual streamflow
#'     \item Dur - duration of between the 25 percent and 75 percent day of year, in days
#'   }
#' @author Jennifer Dierauer
#' @importFrom stats median
#' @export
#' @examples
#' data(cania.sub.ts)
#' cania.sub.ts <- set.plot.titles(cania.sub.ts, 
#' title.elements = c("StationID", "StnName", "StateProv"))
#' res1 <- pk.cov(cania.sub.ts)
#' 
#' # trend and changepoint plot for baseflow peak start doy
#' res2 <- screen.metric(res1[,2], "Day of Year")
#' res2 <- screen.metric(res1[,2], "Day of Year", title = TRUE)

pk.cov <- function(TS, threshold.missing = 0.5) {
  
  plot_title <- attr(TS, 'plot title')
  title_size <- attr(TS, 'title size')
  
  ##### use only doys that are in every year
  doy.list <- tapply(TS$Flow, TS$doy, length)
  doy.list <- attr(doy.list, "dimnames")[[1]][doy.list >= stats::median(doy.list)]
  TS <- subset(TS, TS$doy %in% doy.list)

  year_list <- unique(TS$hyear)
  
  # set up data.frame to fill and return
  out <- data.frame(hYear=year_list, Q25=NA, Q50=NA, Q75=NA, Dur=NA)
  
  # add cumulative sum column to time series
  TS$cumsum <- NA
  
  for (i in 1:length(year_list)) { #loop through years
    
    TS_sub <- TS[(TS$hyear == year_list[i]) & (!is.na(TS$Flow)),]
    
    if (nrow(TS_sub) == 0) {
      
      out[i,2] <- NA
      out[i,3] <- NA
      out[i,4] <- NA
      
    } else {
      
      # only look for 25, 50, and 75% of flow if year has flow > 0
      if (sum(TS$Flow[TS$hyear == year_list[i]], na.rm = T) > 0) {
        
        if (nrow(TS_sub) < (floor(0.5*365))) {
          
          out[i,2] <- NA
          out[i,3] <- NA
          out[i,4] <- NA
          
        } else {
          
          TS$cumsum[TS$hyear == year_list[i]] <- cumsum(TS$Flow[TS$hyear == year_list[i]]) / 
            sum(TS$Flow[TS$hyear == year_list[i]])
          
          doy.25 <- min(TS$hdoy[(TS$hyear == year_list[i]) & (TS$cumsum >= 0.25)])
          doy.50 <- min(TS$hdoy[(TS$hyear == year_list[i]) & (TS$cumsum >= 0.50)])
          doy.75 <- min(TS$hdoy[(TS$hyear == year_list[i]) & (TS$cumsum >= 0.75)])
          
          out[i,2] <- as.numeric(doy.25)
          out[i,3] <- as.numeric(doy.50)
          out[i,4] <- as.numeric(doy.75)
          
        }
        
      } else {
        out[i,c(2:4)] <- 0
      }
      
    }
      
  }# end of year loop
  
  out$Dur <- out[,4] - out[,2]
  
  for (i in 2:5) {
      attr(out[,i], "times") <- as.character(year_list)
      
      attr(out[,i], 'StationID') <- TS$ID[1]
      attr(out[,i], 'Agency') <- TS$Agency[1]
      
      # carry-over the plot title if it has been set
      if (!is.null(plot_title)) {
        attr(out[,i], 'plot title') <- plot_title
        attr(out[,i], 'title size') <- title_size
      }
  }
  
  return(out)
    
}


