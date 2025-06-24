#' Calculate the inter-event duration
#' 
#' This function calculates duration (in days) between flow peaks.
#' @param Peaks Output from \code{\link{pks}}.
#' @author Jennifer Dierauer
#' @return Returns a numeric vector containing the duration (in days) between peaks
#'   over threshold from \code{\link{pks}}. The "times" attribute contains the calendar 
#'   year date of the earlier peak. The "names" attribute contains the hydrologic year and
#'   the day (1-366) of the hydrologic year.
#' @export
#' @examples
#' data(caniapiscau)
#' cania.ts <- create.ts(caniapiscau, hyrstart = 4)
#' cania.ts <- drop.years(cania.ts)
#' cania.ts <- set.plot.titles(cania.ts, 
#' title.elements = c("StationID", "StnName", "StateProv"))
#' 
#' res1 <- pks(cania.ts)
#' res2 <- pks.dur(res1)
#' res3 <- screen.metric(res2, ylabel = "Inter-Event Duration (days)")

pks.dur <- function(Peaks) {
    
    if ((length(Peaks) == 1) & (is.na(Peaks[1]))) {
        return(NA)
    }
    
    # Input validation
    if (!is.numeric(Peaks) | is.null(names(Peaks))) {
        stop("Peaks must be a named numeric vector, as returned from the pks().")
    }
    
    # Carry-over the plot title if it has been set
    ts_attributes <- attributes(Peaks)
    
    # Ensure 'times' attribute is present and valid
    MyDates <- as.Date(attr(Peaks, "times"))
    if (is.null(MyDates) | any(is.na(MyDates))) {
        stop("The 'times' attribute must be a valid Date vector.")
    }
    
    # Ensure 'names' attribute is present and valid
    hyr.hdoy <- attr(Peaks, "names")
    if (is.null(hyr.hdoy)) {
        stop("The 'names' attribute must be present.")
    }
    
    # Calculate inter-event durations
    Durations <- diff(MyDates)
    
    # Attach time attribute
    attr(Durations, "times") <- MyDates[-1]
    attr(Durations, "names") <- hyr.hdoy[-1]
    
    # Add additional attributes
    attr(Durations, 'StationID') <- attr(Peaks, 'StationID')
    attr(Durations, 'Agency') <- attr(Peaks, 'Agency')
    
    # Carry-over the plot title if it has been set
    if ('plot title' %in% names(ts_attributes)) {
        attr(Durations, 'plot title') <- ts_attributes$`plot title`
        attr(Durations, 'title size') <- ts_attributes$`title size`
    }
    
    return(Durations)
}
