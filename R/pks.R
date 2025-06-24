#' Get the flow peaks over a threshold
#' 
#' This function finds the flow peaks over a user defined threshold and declusters to remove
#' dependent peaks.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param Dur numeric value of the minimum number of days between peaks
#' @param Qmax numeric value for peaks over threshold quantile.  
#'   Default is 0.95.
#' @details Peaks Over Threshold values are calcuated as mean daily streamflow (m3/s)
#'   minus the threshold streamflow value (m3/s) defined by the 
#'   input quantile (Qmax). Peaks are identified with \code{\link[evir]{pot}} and 
#'   the minimum inter-event duration (Dur) is applied by 
#'   \code{\link[evir]{decluster}}.
#' @return Returns a numeric vector of peaks of threshold values in m3/s. The "times"
#'   attribute contains the date by calendar year, and the "names" attribute contains
#'   the hydrologic year and hydrologic day of year, e.g., 2012 55.
#' @author Jennifer Dierauer
#' @importFrom evir pot
#' @importFrom evir decluster
#' @importFrom stats quantile
#' @export
#' @examples
#' data(caniapiscau)
#' cania.ts <- create.ts(caniapiscau, hyrstart = 4)
#' cania.ts <- drop.years(cania.ts)
#' cania.ts <- set.plot.titles(cania.ts, 
#' title.elements = c("StationID", "StnName", "StateProv"))
#' 
#' res <- pks(cania.ts)
#' res2 <- screen.metric(res, ylabel = "Peak Over Threshold (m3/s)", title = TRUE)

pks <- function(TS, Dur=5, Qmax=0.95) {
    
  # Input validation
  if (!is.data.frame(TS) || !all(c("Flow", "Date", "hyear", "hdoy", "ID", "Agency") %in% names(TS))) {
    stop("TS must be a data frame with 'Flow', 'Date', 'hyear', 'hdoy', 'ID', and 'Agency' columns.")
  }
  
  if (!is.numeric(Dur) || Dur <= 0) {
    stop("Dur must be a positive numeric value.")
  }
  
  if (!is.numeric(Qmax) || Qmax <= 0 || Qmax >= 1) {
    stop("Qmax must be a numeric value between 0 and 1 (exclusive).")
  }
  
  # Carry-over the plot title if it has been set
  ts_attributes <- attributes(TS)
  
  # Remove rows with NA values in Flow
  Flow <- TS$Flow[!is.na(TS$Flow)]
  
  # Calculate the threshold
  MyThreshold <- stats::quantile(Flow, Qmax, na.rm = TRUE)
  
  # Assign attributes to Flow
  attr(Flow, "times") <- TS$Date[!is.na(TS$Flow)]
  attr(Flow, "names") <- paste(TS$hyear[!is.na(TS$Flow)], TS$hdoy[!is.na(TS$Flow)], sep = " ")
  
  # Find Peaks Over Threshold
  Peaks <- tryCatch(pot(Flow, MyThreshold, picture = FALSE), 
                    error = function(e) NA)
  
  if (inherits(Peaks, "potd")) {
    # Remove minor peaks using input "duration" between peaks
    Peaks <- tryCatch(decluster(Peaks$data, Dur, picture = FALSE), 
                      error = function(e) NA)

    Peaks <- Peaks - MyThreshold
  } else {
    Peaks <- NA
  }
  
  # Add attributes to Peaks
  attr(Peaks, 'StationID') <- TS$ID[1]
  attr(Peaks, 'Agency') <- TS$Agency[1]
  
  # Carry-over the plot title if it has been set
  if ('plot title' %in% names(ts_attributes)) {
    attr(Peaks, 'plot title') <- ts_attributes$`plot title`
    attr(Peaks, 'title size') <- ts_attributes$`title size`
  }
  
  return(Peaks)
}
