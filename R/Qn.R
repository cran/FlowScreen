#' Calculate flow quantiles
#' 
#' This function calculates flow quantiles by hydrologic year, calendar
#' year, month, or doy.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param n Numeric value of the quantile.  Default is 0.1.
#' @param by Character string indicating time unit to summarize by.  Default is
#'   "hyear" for hydrologic year, see \code{\link{create.ts}}.  Other options 
#'   are "year" for calendar year, "month", or "doy" for day of year. 
#' @return Returns a numeric vector of the calculated flow quantile for the time
#'   periods indicated with the "by" argument.  The "times" attribute contains the
#'   hydrologic year, calendar year, month, or day of year for each data point.
#' @author Jennifer Dierauer
#' @importFrom stats quantile
#' @export
#' @examples
#' data(caniapiscau)
#' cania.ts <- create.ts(caniapiscau, hyrstart = 4)
#' cania.ts <- drop.years(cania.ts)
#' cania.ts <- set.plot.titles(cania.ts, 
#' title.elements = c("StationID", "StnName", "StateProv"))
#' 
#' # 50% quantile, i.e. mean, by calendar year
#' res <- Qn(cania.ts, n=0.5, by="year")
#' res2 <- screen.metric(res, ylabel = "Q (m3/s)")
#' 
#' # Default 10% quantile, by hydrologic year
#' res <- Qn(cania.ts)
#' res2 <- screen.metric(res, ylabel = "Q (m3/s)")


Qn <- function(TS, n=0.1, by="hyear") {

  # Input validation
  if (!is.data.frame(TS) || !all(c("Flow", "hyear", "year", "month", "doy") %in% names(TS))) {
    stop("TS must be a data frame with 'Flow', 'hyear', 'year', 'month', and 'doy' columns.")
  }
  
  if (!is.numeric(n) || n < 0 || n > 1) {
    stop("n must be a numeric value between 0 and 1 (inclusive).")
  }
  
  if (!by %in% c("hyear", "year", "month", "doy")) {
    stop("by must be one of 'hyear', 'year', 'month', or 'doy'.")
  }
  
  # Carry-over the plot title if it has been set
  ts_attributes <- attributes(TS)
  
  # Remove rows with NA values in Flow
  TS <- TS[!is.na(TS$Flow), ]
  
  # Determine the grouping variable
  if (by == "hyear") {
    SumBy <- TS$hyear
  } else if (by == "year") {
    SumBy <- TS$year
  } else if (by == "month") {
    SumBy <- TS$month
  } else if (by == "doy") {
    SumBy <- TS$doy
  }
  
  mList <- as.character(unique(SumBy))
  
  # Calculate quantiles using tapply
  out <- tapply(TS$Flow, SumBy, function(x) stats::quantile(x, n, na.rm = TRUE))
  
  # Convert output to numeric vector
  out <- unlist(out)
  
  # Add attributes to output
  attr(out, "times") <- mList
  attr(out, "dimnames") <- NULL
  attr(out, 'StationID') <- TS$ID[1]
  attr(out, 'Agency') <- TS$Agency[1]
  
  # Carry-over the plot title if it has been set
  if ('plot title' %in% names(ts_attributes)) {
    attr(out, 'plot title') <- ts_attributes$`plot title`
    attr(out, 'title size') <- ts_attributes$`title size`
  }
  
  return(out)
  
}