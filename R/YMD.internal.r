#' Add calendar year, month, and day of year columns
#' 
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @return Returns a data.frame with year, month, and doy columns appended.
#' @author Jennifer Dierauer

YMD.internal <- function(TS) {
    TS$year <- as.numeric(format(TS$Date, "%Y"))
    TS$month <- as.numeric(format(TS$Date, "%m"))
    TS$doy <- as.numeric(format(TS$Date, "%j"))
    
    return(TS)
}