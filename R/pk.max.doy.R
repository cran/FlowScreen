#' Day of year for annual maximum series
#' 
#' This function returns the day of the hydrologic year for each annual maximum flow.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @return Returns a numeric vector containing the day of the (hydrologic) year for 
#'   each annual maximum flow. The "times" attribute contains the 
#'   hydrologic year for each element in the vector.
#' @author Jennifer Dierauer
#' @seealso See \code{\link{create.ts}} to format the input flow series.
#' 
#'   See \code{\link{pk.max}} for the annual maximum flow series.
#' @export
#' @examples
#' data(caniapiscau)
#' cania.ts <- create.ts(caniapiscau, hyrstart = 4)
#' cania.ts <- drop.years(cania.ts)
#' cania.ts <- set.plot.titles(cania.ts, 
#' title.elements = c("StationID", "StnName", "StateProv"))
#' 
#' res <- pk.max.doy(cania.ts)
#' res2 <- screen.metric(res, ylabel = "Day of Year", title = TRUE)


pk.max.doy <- function(TS) {
    
    # Input validation
    if (!is.data.frame(TS) || !all(c("Flow", "hyear", "hdoy") %in% names(TS))) {
        stop("TS must be a data frame with 'Flow', 'hyear', and 'hdoy' columns.")
    }
    
    # Carry-over the plot title if it has been set
    ts_attributes <- attributes(TS)
    
    # Remove rows with NA values in Flow
    TS <- TS[!is.na(TS$Flow), ]
    
    year <- as.factor(TS$hyear)
    year_list <- unique(year)
    max.doy <- numeric(length(year_list))
    
    for (i in seq_along(year_list)) {
        temp <- subset(TS, hyear == year_list[i])
        max_flow <- max(temp$Flow)
        temp <- subset(temp, Flow == max_flow)
        
        # If more than one day with the same max flow value, take the first
        max.doy[i] <- as.numeric(temp$hdoy[1])
    }
    
    attr(max.doy, "times") <- as.numeric(as.character(year_list))
    attr(max.doy, 'StationID') <- TS$ID[1]
    attr(max.doy, 'Agency') <- TS$Agency[1]
    
    # Carry-over the plot title if it has been set
    if ('plot title' %in% names(ts_attributes)) {
        attr(max.doy, 'plot title') <- ts_attributes$`plot title`
        attr(max.doy, 'title size') <- ts_attributes$`title size`
    }
    
    return(max.doy)
}