#' Find the start, middle, end, and duration of seasonal droughts
#' 
#' This function returns the day of year for the start, middle, and end of 
#' seasonal droughts.  It also returns the duration and severity of each drought
#' event. The function allows for seasonal analysis by defining a season 
#' argument which lists months during which droughts of interest may start.  
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param Qdr Numeric value for drought quantile.  Default is 0.2.
#' @param WinSize Numeric value for moving window size in days.
#'   Default is 30.
#' @param IntEventDur Numeric value for the minimum inter-event duration in 
#'   days. Drought events with less than the specified number of days between will 
#'   be pooled and considered as one event. Default is 10.
#' @param EventDur Numeric value for the minimum drought duration in days. Default
#'   is 15.
#' @param Season Numeric vector of months during which droughts start. Default 
#'   is c(4:9) for non-frost season droughts.
#' @details This function calls \code{\link{dr.events}} which calls 
#'   \code{\link{dr.pds}} and \code{\link{mqt}}
#' @return Returns a data.frame of drought event metrics; the columns are:
#'   \itemize{
#'     \item StartDay - day of year that the drought event started on
#'     \item MidDay - day of year for the middle of the drought event, which is
#'       defined as the day when the cumulative drought deficit reached 50% of the
#'       total cumulative daily streamflow deficit.  Total cumulative streamflow 
#'       deficit is also referred to as drought severity in this package.
#'     \item EndDay - day of year that the drought ended on
#'     \item Duration - length of the drought event, in days
#'     \item Severity - severity of the drought event, calculated as the total
#'       cumulative daily streamflow deficit
#'   }
#'   
#'   The "times" attribute provides the start date to preserve year information
#'   and aid in plotting the time series.
#' @author Jennifer Dierauer
#' @seealso See \code{\link{create.ts}} to format the input flow series. \cr
#'   See \code{\link{dr.events}} and \code{\link{mqt}} for details on how drought 
#'   events are defined.
#' @export
#' @examples
#' data(cania.sub.ts)
#' res <- dr.seas(cania.sub.ts)
#' res2 <- screen.metric(res[,1], "Day of Year")

dr.seas <- function(TS, Qdr=0.2, WinSize=30, IntEventDur=10, EventDur=15,
                         Season=c(4:9)) {
  
  ts_attributes <- attributes(TS)
  
  # Get drought events using the dr.events function
  res <- dr.events(TS, Qdr, WinSize, IntEventDur, EventDur)
  
  if (is.na(res$DroughtEvents[1, 1])) {
    output <- data.frame(StartDay = NA, MidDay = NA, EndDay = NA, Duration = NA, Severity = NA)
  } else {
    DroughtEvents <- res$DroughtEvents
    DroughtPDS <- res$DroughtPDS
    
    # Add additional columns for seasonal analysis
    DroughtEvents$StartMonth <- as.numeric(format(DroughtEvents$Start, "%m"))
    DroughtEvents$StartDay <- as.numeric(format(DroughtEvents$Start, "%j"))
    DroughtEvents$EndDay <- as.numeric(format(DroughtEvents$End, "%j"))
    DroughtEvents$MidDay <- NA
    
    # Subset by events starting in user-defined months
    DroughtEvents.season <- subset(DroughtEvents, StartMonth %in% Season)
    EventList <- unique(DroughtEvents.season$Event)
    PDS.season <- subset(DroughtPDS, Event %in% EventList)
    
    if (length(EventList) > 0) {
      for (i in seq_along(EventList)) {
        MidPoint <- 0.5 * DroughtEvents.season$Severity[i]
        temp <- subset(PDS.season, Event == EventList[i])
        mDef <- 0
        HaveMid <- FALSE
        j <- 1
        
        while (!HaveMid && j <= nrow(temp)) {
          mDef <- mDef + temp$Def[j]
          if (mDef >= MidPoint) {
            HaveMid <- TRUE
            DroughtEvents.season$MidDay[i] <- as.numeric(format(temp$Date[j], "%j"))
          }
          j <- j + 1
        }
      }
    }
    
    output <- data.frame(
      StartDay = DroughtEvents.season$StartDay,
      MidDay = DroughtEvents.season$MidDay,
      EndDay = DroughtEvents.season$EndDay,
      Duration = DroughtEvents.season$Duration,
      Severity = DroughtEvents.season$Severity
    )
    
    # Add attributes to the output
    for (k in seq_along(output)) {
      attr(output[, k], "times") <- DroughtEvents.season$Start
      attr(output[, k], 'StationID') <- TS$ID[1]
      attr(output[, k], 'Agency') <- TS$Agency[1]
      
      # Carry-over the plot title if it has been set
      if (!is.null(ts_attributes$`plot title`)) {
        attr(output[, k], 'plot title') <- ts_attributes$`plot title`
        attr(output[, k], 'title size') <- ts_attributes$`title size`
      }
    }
    
    attr(output[, 5], "dimnames") <- NULL
  }
  
  return(output)
}


