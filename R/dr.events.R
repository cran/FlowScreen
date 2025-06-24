#' Partial Duration Series and Event Statistics for streamflow droughts
#' 
#' This function extracts the partial duration series for all streamflow droughts based
#' on a moving window quantile threshold.  Also returns summary information (start date,
#' end date, duration, deficit volume) for each drought event.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param Qdr Numeric value of the drought threshold quantile.  Default is 0.2.
#' @param WinSize Numeric value specifying the size of the moving window in
#'   days.  Default is 30. 
#' @param IntEventDur Numeric value for the minimum inter-event duration in days. Drought
#'   events with less than the specified number of days between will be pooled 
#'   and considered as one event.
#' @param EventDur Numeric value for the minimum drought duration in days. Default
#'   is 15.
#' @return Returns a list with the following elements:
#' 
#'   DroughtEvents: A data.frame with the following columns:
#'   \itemize{
#'     \item Event - Integer indicating the original event number assigned before
#'       minor drought events were removed.
#'     \item Start - Date of the start of the drought event.
#'     \item End - Date of the end of the drought event
#'     \item maxDef - Numeric value of the maximum streamflow deficit.
#'     \item Severity - Numeric value indicating the drought severity, 
#'       calculated as the cumulative daily streamflow deficit in m3/s.
#'     \item Duration - Numeric value of the drought duration in days.
#'     \item Magnitude - Numeric value indicating the drought magnitude, which 
#'       is calculated as the mean daily streamflow deficit in m3/s.
#'     \item stdtotDef - Numeric value indicating the standardized cumulative
#'       streamflow deficit, calculated as the drought severity divided by
#'       the mean annual daily streamflow.
#'   }
#'   
#'   DroughtPDS: A data.frame of the original input TS that has been subset to 
#'   include only the days on which the streamflow was below the drought threshold.
#'   The data.frame also has the following columns appended:
#'   \itemize{
#'     \item Thresh - Numeric value indicating the streamflow drought threshold,
#'       as calculated by \code{\link{mqt}}
#'     \item BelowThresh - Logical indicating whether the observed streamflow 
#'       was below the streamflow drought threshold.
#'     \item Def - Numeric value of the streamflow defict, calculated as the 
#'       streamflow drought threshold (m3/s) minus the observed streamflow (m3/s).
#'   }
#' @author Jennifer Dierauer
#' @seealso See \code{\link{dr.seas}} to calculate metrics for droughts 
#'   occurring in a user-defined season.
#'   
#'   This function calls \code{\link{dr.pds}} which calls \code{\link{mqt}}.
#' @importFrom graphics par
#' @export
#' @examples
#' data(cania.sub.ts)
#' res1 <- dr.events(cania.sub.ts)
#' events <- res1$DroughtEvents
#' 
#' opar <- graphics::par(no.readonly = TRUE)
#' par(mar=c(5,6,2,2))
#' plot(events$Start, events$Duration, pch=19, ylab="Drought Duration (days)", xlab="")
#' graphics::par(opar)

dr.events <- function(TS, Qdr=0.2, WinSize=30, IntEventDur=10, EventDur=15) {
    
  
  ts_attributes <- attributes(TS)
  
  ## Create PDS from original data
  myPDS <- dr.pds(TS, Qdr, WinSize)
  
  ### Subset to keep only dates when streamflow was below threshold
  myPDS <- subset(myPDS, BelowThresh == TRUE)
  
  if (nrow(myPDS) > 0) {
    
    ### Pool drought events based on IntEventDur
    eventNo <- 1
    myPDS$Event <- 1
    
    for (i in 2:nrow(myPDS)) {
      meventdur <- as.numeric(myPDS$Date[i] - myPDS$Date[i-1])
      if (meventdur >= IntEventDur) {
        eventNo <- eventNo + 1
      }
      myPDS$Event[i] <- eventNo
    }
    
    myPDS$Def <- as.numeric(myPDS$Thresh - myPDS$Flow)
    
    ### Set up output data.frame 
    DroughtEvents <- data.frame(Event = numeric(), 
                                Start = as.Date(character()),
                                End = as.Date(character()),
                                maxDef = numeric(),
                                totDef = numeric(),
                                Duration = numeric(),
                                Magnitude = numeric(),
                                stdtotDef = numeric())
    
    ### Calculate event stats, ignoring drought events with duration < EventDur
    for (j in 1:eventNo) {
      PDS.sub <- subset(myPDS, Event == j)
      dur <- as.numeric(difftime(max(PDS.sub$Date), min(PDS.sub$Date), units = "days"))
      if (dur >= EventDur) {
        dstats <- data.frame(Event = j, 
                             Start = min(PDS.sub$Date),
                             End = max(PDS.sub$Date),
                             maxDef = max(PDS.sub$Def, na.rm = TRUE), 
                             Severity = sum(PDS.sub$Def, na.rm = TRUE),
                             Duration = dur,
                             Magnitude = mean(PDS.sub$Def, na.rm = TRUE),
                             stdtotDef = sum(PDS.sub$Def, na.rm = TRUE) / mean(TS$Flow, na.rm = TRUE))
        
        DroughtEvents <- rbind(DroughtEvents, dstats)
      }
    }
    
    ### Subset PDS to remove minor droughts
    Events <- unique(DroughtEvents$Event)
    myPDS <- subset(myPDS, Event %in% Events)
    
    # Carry StationID and Agency as attributes
    attr(myPDS, 'StationID') <- TS$ID[1]
    attr(myPDS, 'Agency') <- TS$Agency[1]
    
    # Carry-over the plot title if it has been set
    if ('plot title' %in% names(ts_attributes)) {
      attr(myPDS, 'plot title') <- ts_attributes$'plot title'
      attr(myPDS, 'title size') <- ts_attributes$'title size'
    }
    
    attr(DroughtEvents, 'StationID') <- TS$ID[1]
    attr(DroughtEvents, 'Agency') <- TS$Agency[1]
    
    # Carry-over the plot title if it has been set
    if ('plot title' %in% names(ts_attributes)) {
      attr(DroughtEvents, 'plot title') <- ts_attributes$'plot title'
      attr(DroughtEvents, 'title size') <- ts_attributes$'title size'
    }
    
    output <- list(DroughtEvents = DroughtEvents, DroughtPDS = myPDS)
    
  } else {
    dstats <- data.frame(Event = NA, 
                         Start = NA,
                         End = NA,
                         maxDef = NA, 
                         Severity = NA,
                         Duration = NA,
                         Magnitude = NA,
                         stdtotDef = NA)
    
    myPDS <- TS[1,]
    myPDS[,] <- NA
    
    attr(myPDS, 'StationID') <- TS$ID[1]
    attr(myPDS, 'Agency') <- TS$Agency[1]
    
    # Carry-over the plot title if it has been set
    if ('plot title' %in% names(ts_attributes)) {
      attr(myPDS, 'plot title') <- ts_attributes$'plot title'
      attr(myPDS, 'title size') <- ts_attributes$'title size'
    }
    
    attr(dstats, 'StationID') <- TS$ID[1]
    attr(dstats, 'Agency') <- TS$Agency[1]
    
    # Carry-over the plot title if it has been set
    if ('plot title' %in% names(ts_attributes)) {
      attr(dstats, 'plot title') <- ts_attributes$'plot title'
      attr(dstats, 'title size') <- ts_attributes$'title size'
    }
    
    output <- list(DroughtEvents = dstats, DroughtPDS = myPDS)
  }
  
  return(output)
}
