#' Center of Volume
#' 
#' This function calculates center of volume metrics, including the day of the 
#' hydrologic year that 25 percent, 50 percent, and 75 percent of the total annual 
#' streamflow is reached. A value of 0 is returned for years with no flow. Hydrologic 
#' years with fewer than normal observations (outliers) are excluded from the 
#' analysis, and for stations with seasonal flow records, additional seasonal 
#' subsetting is done to include only days with observations in all years.
#' @param TS output from \code{\link{create.ts}} containing a data.frame of flow
#'   time series
#' @return Returns a data.frame with the following columns:
#'   \itemize{
#'     \item hYear - Hydrologic Years
#'     \item Q25 - day of hydrologic year for 25 percent of the total annual streamflow
#'     \item Q50 - day of hydrologic year for 50 percent of the total annual streamflow, i.e. Center of Volume
#'     \item Q75 - day of hydrologic year for 75 percent of the total annual streamflow
#'     \item Dur - duration of between the 25 percent and 75 percent day of year, in days
#'   }
#' @author Jennifer Dierauer
#' @export
#' @examples
#' data(cania.sub.ts)
#' res1 <- pk.cov(cania.sub.ts)
#' 
#' # trend and changepoint plot for baseflow peak start doy
#' res2 <- screen.metric(res1[,2], "Day of Year")

pk.cov <- function(TS) {
    
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
    
    #create factors for year
    Year <- as.factor(TS$hyear)
    
    year_list <- unique(Year)
    
    out <- data.frame(hYear=year_list, Q25=NA, Q50=NA, Q75=NA, Dur=NA)
    
    YearStack <- split(TS, Year)  #split into dataframes by year for looping
    
    for (i in 1:length(YearStack)) { #loop through years
        
        Have25 <- FALSE
        Have50 <- FALSE
        Have75 <- FALSE
        mysum <- 0
        temp <- YearStack[[i]]
        j <- 0
        
        total.flow <- sum(temp$Flow)
        q25 <- total.flow * 0.25
        q50 <- total.flow * 0.5
        q75 <- total.flow * 0.75
        
        if (total.flow > 0) {
        
            while (Have75 == FALSE) { #loop through days until 75% is reached
                
                j <- j+1      
                mysum <- mysum + temp$Flow[j]
                
                if ((mysum > q25) & (Have25 == FALSE)) {
                    Have25 <- TRUE
                    out[i,2] <- as.numeric(temp$hdoy[j])
                }
                
                else if ((mysum > q50) & (Have50 == FALSE)) {
                    Have50 <- TRUE
                    out[i,3] <- as.numeric(temp$hdoy[j])
                }
                
                else if ((mysum > q75 & Have75 == FALSE)) {
                    Have75 <- TRUE
                    out[i,4] <- as.numeric(temp$hdoy[j])
                }
            }
                
        } else {
            out[i,c(2:4)] <- 0
        }
    }
    
    out$Dur <- out[,4] - out[,2]
    
    for (i in 2:5) {
        attr(out[,i], "times") <- as.character(year_list)
    }
    
    return(out)
    
}