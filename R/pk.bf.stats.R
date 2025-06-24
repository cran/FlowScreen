#' Calculate baseflow peak statistics
#' 
#' This function finds the start, middle, end, and duration of the baseflow
#' peak based on percent of the total annual baseflow volume.
#' A value of 0 is returned for years with no flow. Hydrologic 
#' years with fewer than normal observations (outliers) are excluded from the 
#' analysis, and for stations with seasonal flow records, additional seasonal 
#' subsetting is done to include only days with observations in all years.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param bfpct numeric vector of percentages used to define the 
#'   start, middle, and end of the baseflow peak. Default is c(25, 50, 75)
#' @details This function calculates metrics intended to focus on snowmelt-related
#'   streamflow occuring in spring and summer. For catchments in cold climates, 
#'   the baseflow peak can be interpreted as snowmelt-induced. Baseflow is estimated with 
#'   \code{\link{bf_eckhardt}}. If total annual flow is equal to 0, returns NA for that
#'   year.
#' @return Returns a data.frame with the following columns:
#'   \itemize{
#'     \item Start - day of year defining the start of the baseflow peak
#'     \item Mid - day of year defining the middle of the baseflow peak
#'     \item End - day of year defining the end of the baseflow peak
#'     \item Dur - duration of the baseflow peak, in days
#'   }
#' @author Jennifer Dierauer
#' @importFrom stats median
#' @export
#' @examples
#' data(caniapiscau)
#' cania.ts <- create.ts(caniapiscau, hyrstart = 4)
#' cania.ts <- drop.years(cania.ts)
#' cania.ts <- set.plot.titles(cania.ts, 
#' title.elements = c("StationID", "StnName", "StateProv"))
#' res1 <- pk.bf.stats(cania.ts)
#' 
#' # trend and changepoint plot for baseflow peak start doy
#' res2 <- screen.metric(res1[,1], ylabel = "Day of Year")


pk.bf.stats <- function(TS, bfpct=c(25,50,75)) {
  
  # Input validation
  if (!is.data.frame(TS) || !all(c("Flow", "doy", "hyear", "hdoy") %in% names(TS))) {
    stop("TS must be a data frame with 'Flow', 'doy', 'hyear', and 'hdoy' columns.")
  }
  
  if (!is.numeric(bfpct) || any(bfpct < 0) || any(bfpct > 100)) {
    stop("bfpct must be a numeric vector with values between 0 and 100.")
  }
  
  ts_attributes <- attributes(TS)
  
  # Remove rows with NA values in Flow
  TS <- TS[!is.na(TS$Flow), ]
  
  # Use only doys that are in every year
  doy.list <- tapply(TS$Flow, TS$doy, length)
  doy.list <- as.numeric(names(doy.list[doy.list >= stats::median(doy.list)]))
  TS <- subset(TS, TS$doy %in% doy.list)
  
  # Set parameter values for Eckhardt RDF
  BFindex <- 0.8
  alpha <- 0.970 # based on values suggested by Eckhardt 2012 for perennial stream
  
  # Calculate daily BF and BFI
  TS$base <- bf_eckhardt(TS$Flow, alpha, BFindex)
  
  # Initialize cumsum column
  TS$cumsum <- NA
  year_list <- unique(TS$hyear)
  
  # Setup output data frame
  out <- data.frame(matrix(NA, nrow = length(year_list), ncol = length(bfpct) + 1))
  colnames(out) <- c(paste0("P", bfpct), "Dur")
  
  # Convert percent to fraction for searching
  bfpct <- bfpct / 100
  
  # Calculate cumulative sum and find the day of each percent
  for (i in seq_along(year_list)) {
    year_data <- subset(TS, hyear == year_list[i])
    
    if (sum(year_data$base) > 0) {
      year_data$cumsum <- cumsum(year_data$base) / sum(year_data$base)
      
      for (b in seq_along(bfpct)) {
        out[i, b] <- min(year_data$hdoy[year_data$cumsum >= bfpct[b]], na.rm = TRUE)
      }
    } else {
      out[i, 1:length(bfpct)] <- 0
    }
  }
  
  # Calculate spring flood duration
  out$Dur <- apply(out[, 1:length(bfpct)], 1, function(row) row[which.max(bfpct)] - row[which.min(bfpct)])
  
  # Add attributes to the output
  for (i in 1:ncol(out)) {
    attr(out[, i], "times") <- as.character(year_list)
    attr(out[, i], 'StationID') <- TS$ID[1]
    attr(out[, i], 'Agency') <- TS$Agency[1]
    
    if ('plot title' %in% names(ts_attributes)) {
      attr(out[, i], 'plot title') <- ts_attributes$`plot title`
      attr(out[, i], 'title size') <- ts_attributes$`title size`
    }
  }
  
  return(out)
    
}