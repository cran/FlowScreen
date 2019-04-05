#' Add hydrologic Year, month, and doy columns to a daily time series
#' 
#' @param TS Output from \code{\link{create.ts}} function.
#' @param hyrstart define start month of hydrologic year. Defaults to 10 (October).
#' @return Returns a data.frame with hyear, hmonth, and hdoy columns 
#'   appended to the original input data.frame.
#' @author Jennifer Dierauer


hyear.internal <- function(TS, hyrstart=10) {
    
    TS$hyear <- TS$year
    TS$hmonth <- TS$month
    TS$hdoy <- TS$doy
    
    if (hyrstart > 6.5) {
        
        ## define hydrologic year based on start month
        MonthsUp <- c(hyrstart:12)
        month.hyr <- c(c(1:(hyrstart - 1)) + length(MonthsUp), c(1:(13 - hyrstart)))
        
        TS$hyear[TS$month %in% MonthsUp] <- TS$year[TS$month %in% MonthsUp] + 1
        TS$hmonth <- month.hyr[as.numeric(TS$month)]
        
    } else if (hyrstart == 1) {
        
        TS$hyear <- TS$month
        
    } else if (hyrstart < 6.5) {
        
        MonthsDown <- c(1:(hyrstart-1))
        month.hyr <- c((MonthsDown + (12 - length(MonthsDown))), 1:(13-hyrstart))
        
        TS$hyear[TS$month %in% MonthsDown] <- TS$year[TS$month %in% MonthsDown] - 1
        TS$hmonth <- month.hyr[as.numeric(TS$month)]
        
    }
    
    yr.list <- unique(TS$hyear) 
    
    if (hyrstart != 1) {
        for (y in 1:length(yr.list)) {
            
            if (y > 1) {
                mlen <- nrow(TS[TS$hyear == yr.list[y], ])
                TS$hdoy[TS$hyear %in% yr.list[y]] <- c(1:366)[1:length(TS$hdoy[TS$hyear %in% yr.list[y]])]
            } else {
                mlen <- nrow(TS[TS$hyear %in% yr.list[y], ])
                if (length(seq(from = as.Date(paste(yr.list[y], "-01-01", sep = ""), format = "%Y-%m-%d"), 
                               to = as.Date(paste(yr.list[y], "-12-31", sep = ""), format = "%Y-%m-%d"), by = 1)) > 365) {
                    TS$hdoy[TS$hyear == yr.list[y]] <- c((367-mlen):366)
                } else {
                    TS$hdoy[TS$hyear == yr.list[y]] <- c((366-mlen):365)
                }
            }
            
        }
    }
    
    return(TS)
}
