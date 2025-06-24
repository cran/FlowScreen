#' Drop hydrologic years 
#'
#' Removesthose hydrologic years where the fraction of missing data is above the 
#' defined threshold.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param NAthresh Numeric value indicating the threshold for missing data points
#'   in any one year.  Default is 0.80, indicating that years with more than 80 percent missing data
#'   will be omitted from the metric calculations. This value should always be set to 
#'   greater than 0.1, as years with fewer observations than approximately 1 month will
#'   cause errors.
#' @return Returns TS data.frame with hydrologic years with % of missing data 
#'   above the user-defined threshold dropped.
#' @author Jennifer Dierauer
#' @export 
#' @examples
#' data(caniapiscau)
#' cania.ts <- create.ts(caniapiscau, hyrstart = 4)
#' cania.ts <- drop.years(cania.ts, NAthresh = 0.75)

drop.years <- function(TS, NAthresh = 0.8) {
  
  # Calculate the minimum number of observations needed to keep a year
  MinObs <- NAthresh * 365
  
  # Calculate the number of valid records for each year, excluding NA Flow values
  NumRecords <- table(TS$hyear[!is.na(TS$Flow)])
  
  # Identify the years to keep based on the threshold
  keepers <- as.numeric(names(NumRecords)[NumRecords >= MinObs])
  
  # Identify all unique years in the dataset
  all_years <- unique(TS$hyear)
  
  # Identify the years to omit
  YearTrim <- setdiff(all_years, keepers)
  
  if (length(YearTrim) > 0) {
    # Create a data frame of omitted years
    OmitYears <- data.frame(
      Years = YearTrim,
      Observations = sapply(YearTrim, function(year) {
        if (year %in% names(NumRecords)) {
          return(NumRecords[as.character(year)])
        } else {
          return(0)
        }
      })
    )
    
    # Remove row names from the data frame
    rownames(OmitYears) <- NULL
    
    print("The following hydrologic years were omitted from analysis due to insufficient data points:")
    print(OmitYears)
    
    # Subset the original data frame to keep only the years that meet the threshold
    TS.sub <- TS[TS$hyear %in% keepers,]
  } else {
    print("No hydrologic years were omitted from analysis.")
    TS.sub <- TS
  }
  
  return(TS.sub)
}