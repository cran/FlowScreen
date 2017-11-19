#' Add hydrologic Year, month, and doy columns to a daily time series
#' 
#' @param TS Output from \code{\link{create.ts}} function.
#' @param hyrstart define start month of hydrologic year. Defaults to 10 (October).
#' @return Returns a data.frame with hyear, hmonth, and hdoy columns 
#'   appended to the original input data.frame.
#' @author Jennifer Dierauer


hyear.internal <- function(TS, hyrstart=1) {
    
    TS$hyear <- TS$year
    TS$hmonth <- TS$month
    TS$hdoy <- TS$doy
    
    if (hyrstart > 6.5) {
        
        ## define hydrologic year based on start month
        MonthsUp <- c(hyrstart:12)
        month.hyr <- c(c(1:(hyrstart - 1)) + length(MonthsUp), c(1:(13 - hyrstart)))
        
        TS$hyear[TS$month %in% MonthsUp] <- TS$year[TS$month %in% MonthsUp] + 1
        TS$hmonth <- month.hyr[as.numeric(TS$month)]
    } else if (hyrstart < 6.5) {
        
        MonthsDown <- c(1:(hyrstart-1))
        month.hyr <- c((MonthsDown + (12 - length(MonthsDown))), 1:(13-hyrstart))
        
        TS$hyear[TS$month %in% MonthsDown] <- TS$year[TS$month %in% MonthsDown] + 1
        TS$hmonth <- month.hyr[as.numeric(TS$month)]
        
    }
    
    yr.list <- unique(TS$hyear) 
    
    for (y in 1:length(yr.list)) {
        TS$hdoy[TS$hyear == yr.list[y]] <- c(1:366)[1:length(TS$hdoy[TS$hyear == yr.list[y]])]
    }
 
    
    return(TS)
}
