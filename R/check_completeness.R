#' Check Completeness
#'
#' Determine if the desired completeness criteria are being met. This considers if the
#' date range of interest is complete, and whether internal gaps are longer than the
#' criteria. There is an option for allowing the data set to be padded with an optional number
#' of years and then tested using the same criteria. Function returns TRUE if the
#' criteria are met and FALSE if not, and a numeric code that indicates the mode of failure
#' or success.
#'
#' @param data Result from \code{\link{missingness}}.
#' @param st_yr Starting year of the desired period.
#' @param nd_yr Ending year of the desired period.
#' @param allowed Maximum number of years allowed to be missing in the period.
#' @param period Period of years that cannot exceed an allowed gap.
#' @param pad Optional number of years to pad the data set.
#' @param my Optional maximum number of years allowed to be missing with padding.
#'
#' @return A list containing:
#'   \itemize{
#'     \item TRUE or FALSE indicating conditions were met or not
#'     \item code: 0 "met", 1 "not long enough period", 2 "gaps longer than allowed",
#' and 3 "more gaps in period than allowed", 10 "met with pad",
#' 11 "not long enough period with pad", 12 "gaps longer than allowed with pad",
#' and 13 "more gaps in period than allowed with pad"
#'   }
#' @return Messages indicating the reason for failure
#'
#' @examples
#' robin_path <- system.file("extdata", "ROBIN_example.csv", package = "FlowScreen")
#' 
#' TS <- read.flows(robin_path)
#' res <- missingness(TS, title = TRUE)
#' check_completeness(res, st_yr = 1985, nd_yr = 2014, allowed = 3, period = 10)
#' 
#' @author Paul Whitfield
#' @export
check_completeness <- function(data, st_yr, nd_yr, allowed = 3, period = 10, pad = NULL, my = NULL) {
  
  if (!("complete_years" %in% names(data)) | !("years_total" %in% names(data))) {
    return("Invalid Input. Input should be result of the missingness function.")
  }
  
  if (is.null(pad)) {
    if (!(data$years_total >= (nd_yr - st_yr + 1))) {
      message(paste("code 1: Insufficient data: available", data$years_total, "<", (nd_yr - st_yr + 1), "desired"))
      result <- list(FALSE, 1)
      return(result)
    }
    
    complete_years <- as.numeric(data$complete_years)
    sample_years <- complete_years[complete_years >= st_yr & complete_years <= nd_yr]
    sample_years <- unique(c(st_yr, sample_years, nd_yr))
    counter <- diff(sample_years)
    
    if (max(counter) > allowed) {
      message(paste("code 2: Data contains larger gaps than allowed:", allowed, "years"))
      result <- list(FALSE, 2)
      return(result)
    }
    
    for (kk in 1:length(counter)) {
      for (jj in 2:allowed) {
        if (counter[kk] == jj) {
          ntest <- period - jj
          test_sum <- sum(counter[(kk+1):(kk+ntest)])
          
          if (!is.na(test_sum) && test_sum > ntest) {
            message(paste("code 3: Data contains more gaps than allowed:", allowed, "in period", period))
            result <- list(FALSE, 3)
            return(result)
          }
        }
      }
    }
    
    message(paste("code 0: Completeness conditions met, from:", st_yr, "to:", nd_yr, "no more than", allowed, "in", period, "years"))
    result <- list(TRUE, 0)
    return(result)
  }
  
  if (!is.null(pad)) {
    if (!(data$years_total >= (nd_yr - st_yr + 1 - pad))) {
      message(paste("code 11: With padding insufficient data: available", data$years_total, "<", (nd_yr - st_yr + 1 - pad), "desired"))
      result <- list(FALSE, 11)
      return(result)
    }
    
    if (!(data$years_total <= (nd_yr - st_yr + 1 - pad - my))) {
      message(paste("code 11: With padding too many missing years: more than", my, "<", (nd_yr - st_yr + 1 - pad), "desired"))
      result <- list(FALSE, 11)
      return(result)
    }
    
    complete_years <- as.numeric(data$complete_years)
    sample_years <- complete_years[complete_years >= st_yr & complete_years <= nd_yr]
    sample_years <- c(st_yr, sample_years, nd_yr)
    counter <- diff(sample_years)
    
    if (max(counter) > allowed) {
      message(paste("code 12: With padding data contains larger gaps than allowed:", allowed, "years"))
      result <- list(FALSE, 12)
      return(result)
    }
    
    for (kk in 1:length(counter)) {
      for (jj in 2:allowed) {
        if (counter[kk] == jj) {
          ntest <- period - jj
          test_sum <- sum(counter[(kk+1):(kk+ntest)])
          
          if (!is.na(test_sum) && test_sum > ntest) {
            message(paste("code 13: With padding data contains more gaps than allowed:", allowed, "in period", period))
            result <- list(FALSE, 13)
            return(result)
          }
        }
      }
    }
    
    message(paste("code 10: With padding completeness conditions met, from:", st_yr, "to:", nd_yr, "no more than", allowed, "in", period, "years"))
    result <- list(TRUE, 10)
    return(result)
  }
}
