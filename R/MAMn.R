#' Calculate mean annual minimum n-day flows
#' 
#' This function calculates the calculates the mean annual minimum n-day flow by
#' calendar year or by hydrologic year. This function can also be used to find
#' the annual minimum series by setting n=1.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param n Numeric value for the number of days in the n-day flow period. Default is 7.
#' @param by Character string indicating whether to use hydrologic years or 
#'   calendar years. Default is "hyear". Other option is "year".
#' @param threshold.missing Numeric value indicating the fraction of data that can 
#'   be missing in a single year. Years with a missing data above this threshold will have
#'   NA values returned. Default is 0.5 (max of 50\% missing data allowed). 
#' @return Returns a numeric vector containing the calculated MAM n-day flow for each 
#'   year in the input time series. The "times" attribute provides the corresponding
#'   year for each calculated value. Note: a partial start year or end year in the time
#'   series that exceeds the threshold set by 'threshold.missing' will be automatically
#'   truncated from the output.
#' @author Jennifer Dierauer
#' @seealso \code{\link{screen.metric}}
#' @export
#' @examples
#' data(caniapiscau)
#' cania.ts <- create.ts(caniapiscau, hyrstart = 4)
#' cania.ts <- drop.years(cania.ts)
#' cania.ts <- set.plot.titles(cania.ts, 
#' title.elements = c("StationID", "StnName", "StateProv"))
#' 
#' # find the annual minimum series and plot 
#' res <- MAMn(cania.ts, n=1)
#' res2 <- screen.metric(res, ylabel = "Q (m3/s)", title = TRUE)
#' 
#' # do the same with MAM 7-day flow instead of annual minimum
#' res <- MAMn(cania.ts, n=7)
#' res2 <- screen.metric(res, ylabel = "Q (m3/s)", title = TRUE)

MAMn <- function(TS, n=7, by="hyear", threshold.missing=0.5) {
    
    ts_attributes <- attributes(TS)
    
    if (!is.numeric(n) | !(n %in% c(1:90))) {
        stop("Number of days (n) must be an integer between 1 and 90.")
    }
    
    if (!is.numeric(n) | (threshold.missing > 0.9) | (threshold.missing < 0)) {
        stop('Threshold for amount of missing data (threshold.missing) must be 
             a numeric value between 0 and 0.9.')
    }
    
    if ((threshold.missing * 365) < (2*n)) {
        stop('Threshold for missing data must allow for at least two times the length of 
             the desired n-day flow window.')
    }
    
    ifelse(by == "hyear", Year <- as.factor(TS$hyear), Year <- as.factor(TS$year))
    
    YearList <- as.character(unique(Year))
    out <- (1:length(unique(Year)))
    
    for (i in 1:length(YearList)) {
        
        temp <- TS[Year %in% YearList[i],]
        temp <- temp[!is.na(temp$Flow),]
        
        if (nrow(temp) == 0) {
            out[i] <- NA
            next
        }
        
        out[i] <- max(temp$Flow, na.rm=TRUE)
        
        if (length(temp$Flow) < (threshold.missing * 365)) {
            out[i] <- NA
            next
        }
        
        for (j in 1:(length(temp$Flow)-n)) {
            checkmin <- mean(temp$Flow[j:(j + (n - 1))], na.rm=TRUE)
            if (!is.na(checkmin) & !is.infinite(checkmin)) {
                    if (checkmin < out[i]) {out[i] <- checkmin}
            }
        }
    }
    
    # Remove leading NA values
    first_non_na <- which(!is.na(out))[1]
    if (!is.na(first_non_na)) {
        out <- out[first_non_na:length(out)]
        YearList <- YearList[first_non_na:length(YearList)]
    }
    
    # Remove trailing NA values
    last_non_na <- which(!is.na(out))[length(which(!is.na(out)))]
    if (!is.na(last_non_na)) {
        out <- out[1:last_non_na]
        YearList <- YearList[1:last_non_na]
    }
    
        
    attr(out, "times") <- YearList
    attr(out, 'StationID') <- TS$ID[1]
    attr(out, 'Agency') <- TS$Agency[1]
    
    # carry-over the plot title if it has been set
    if ('plot title' %in% names(ts_attributes)) {
        attr(out, 'plot title') <- ts_attributes$`plot title`
        attr(out, 'title size') <- ts_attributes$`title size`
    }
    
    return(out)
}
