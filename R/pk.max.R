#' Annual maximum series
#' 
#' This function returns the annual maximum series from a daily streamflow time
#' series.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @return Returns a numeric vector containing the annual maximum flow (m3/s) 
#'   series, by hydrologic year. The "times" attribute contains the hydrologic year 
#'   for each element in the vector.
#' @author Jennifer Dierauer
#' @seealso See \code{\link{create.ts}} to format the input flow series.
#' 
#'   See \code{\link{pk.max.doy}} to find the day of year for each annual
#'   maximum flow event.
#' @export
#' @examples
#' data(caniapiscau)
#' cania.ts <- create.ts(caniapiscau, hyrstart = 4)
#' cania.ts <- drop.years(cania.ts)
#' cania.ts <- set.plot.titles(cania.ts, 
#' title.elements = c("StationID", "StnName", "StateProv"))
#' 
#' res <- pk.max(cania.ts)
#' res2 <- screen.metric(res, ylabel = "Q (m3/s)", title = TRUE)

pk.max <- function(TS) {
  # Input validation
  if (!is.data.frame(TS) || !all(c("Flow", "hyear") %in% names(TS))) {
    stop("TS must be a data frame with 'Flow' and 'hyear' columns.")
  }
  
  # Carry-over the plot title if it has been set
  ts_attributes <- attributes(TS)
  
  # Remove rows with NA values in Flow
  TS <- TS[!is.na(TS$Flow), ]
  
  # Determine the maximum flow for each hydrological year
  year <- as.factor(TS$hyear)
  maxseries <- tapply(TS$Flow, year, max, na.rm = TRUE)
  attr(maxseries, "times") <- as.numeric(as.character(unique(TS$hyear)))
  
  # Add additional attributes
  attr(maxseries, 'StationID') <- TS$ID[1]
  attr(maxseries, 'Agency') <- TS$Agency[1]
  
  # Carry-over the plot title if it has been set
  if ('plot title' %in% names(ts_attributes)) {
    attr(maxseries, 'plot title') <- ts_attributes$`plot title`
    attr(maxseries, 'title size') <- ts_attributes$`title size`
  }
  
  return(maxseries)
}