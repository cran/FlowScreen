#' Create a Time Series of daily streamflow observations
#' 
#' This function creates a daily time series formatted
#' for use with the functions in this package. This function is executed within the
#' \code{\link{read.flows}} function. To use separately, the 'Flows' input data.frame must have 
#' include the columns: ID, PARAM, date, Flow, SYM, Agency, and FlowUnits. This 
#' function would be used in the case the user has data files containing 
#' dates and flows and this function would convert the original data into the 
#' the form used by the FlowScreen functions.
#' @param Flows Data.frame containing daily streamflow time series loaded 
#'   with the \code{\link{read.flows}} function.
#' @param hyrstart define start month of hydrologic year. Defaults to 10 (October).
#' @return Returns a data.frame with year, month, doy, and hyear columns 
#'   appended to the original input data.frame.
#' @author Jennifer Dierauer
#' @export 
#' @examples
#' data(caniapiscau)
#' # subset flow series for shorter example run time
#' # first, drop the rows with missing streamflow
#' caniapiscau <- caniapiscau[!is.na(caniapiscau$Flow),]
#' caniapiscau.sub <- caniapiscau[300:1800,]
#' caniapiscau.sub.ts <- create.ts(caniapiscau.sub, hyrstart = 3)


create.ts <- function(Flows, hyrstart=10) {
  
  # 
  if (!('Date' %in% colnames(Flows)) |
      !('Flow' %in% colnames(Flows)) | 
      !('Agency' %in% colnames(Flows)) | 
      !('SYM' %in% colnames(Flows)) | 
      !('ID' %in% colnames(Flows)) | 
      !('FlowUnits' %in% colnames(Flows))) {
    stop('input data.frame must have the following columns (as created by the read.flows function: ID, Date, Flow, SYM, Agency, FlowUnits.')
  }
  
  
  # remove any rows with NA values for the date
  Flows <- Flows[!is.na(Flows$Date),]
  # put into chronological order by date
  Flows <- Flows[order(Flows$Date),]
  
  numobs <- nrow(Flows)
  mseq <- seq(from=Flows$Date[1], to=max(Flows$Date), by=1)
  mseq <- data.frame(Date=mseq, Flow=NA)
  
  
  # create time series with no missing dates
  if (nrow(mseq) > numobs) {
      out <- merge(mseq, Flows, by = "Date", all.x=T, all.y=T)
      out <- data.frame(ID=rep(out$ID[1], nrow(out)), Date=out$Date, Flow=out$Flow.y, 
                        Code=out$SYM, Agency=out$Agency[1], FlowUnits = out$FlowUnits[1])
      
  } else {
    out <- data.frame(ID=Flows$ID, Date=Flows$Date, Flow=Flows$Flow, 
                            Code=Flows$SYM, Agency=Flows$Agency, FlowUnits = Flows$FlowUnits)
  }
  
  out <- YMD.internal(out)
  out <- hyear.internal(out, hyrstart)
  
  # quantify missing data by year
  year_count <- length(unique(out$hyear))
  year_start <- min(out$hyear) 
  year_end <- max(out$hyear)
  na_count <- sum(is.na(out$Flow))
  series_length <- nrow(out)
  obs_count <- series_length - na_count
  
  pct_missing <- round(100 * (na_count / series_length), 2)
  
  message(paste0('Streamflow time series contains ', year_count, 
                 ' hydrologic years (',
                 year_start, "-", year_end, ") and ",
                 obs_count, ' daily observations. 
    ', pct_missing, '% of the data are missing.'))
  
  
  if (year_count < 10) {
    message('Warning: Some of the package functionality is not possible with less than 10 years of data.')
  }
  
  
  #out <- out[!is.na(out$Flow),]
  
  return(out)
    
}