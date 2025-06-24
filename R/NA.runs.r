#' Missing data runs for daily time series.
#' 
#' This function takes a data.frame from create.ts and returns a data.frame of 
#' missing data runs.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param quiet A boolean to indicate message printing. 
#' @author Jennifer Dierauer
#' @return Returns a data.frame with the following columns:
#'   \itemize{
#'     \item Start - Date of the start of the missing data period
#'     \item End - Date of the end of the missing data period
#'     \item Duration - number of days in the missing data period
#'   }
#' @seealso \code{\link{create.ts}} to create input, \code{\link{NA.count.runs}} to sum the
#'   the missing data occurrences by year or month.
#' @export
#' @examples
#' data(caniapiscau)
#' cania.sub <- caniapiscau[300:1800,]
#' cania.ts <- create.ts(cania.sub)
#' res <- NA.runs(cania.ts)
#' print(res)


NA.runs <- function(TS, quiet=FALSE) {
    # Check if 'Flow' column exists
    if (!"Flow" %in% colnames(TS)) {
        stop("The data.frame does not contain a 'Flow' column.")
    }
    
    # Ensure the data.frame has a 'Date' column in Date format
    if (!"Date" %in% colnames(TS) || !inherits(TS$Date, "Date")) {
        stop("The data.frame must contain a 'Date' column of class Date.")
    }
    
    # Sort the data frame by Date
    TS <- TS[order(TS$Date), ]
    
    # Create a complete sequence of dates from the first to the last date in the dataset
    full_dates <- seq(min(TS$Date), max(TS$Date), by = "day")
    
    # Merge the full date sequence with the original data frame to identify missing dates
    full_TS <- data.frame(Date = full_dates)
    full_TS <- merge(full_TS, TS, by = "Date", all.x = TRUE)
    
    # Identify positions of missing values in the full date sequence
    is_na <- is.na(full_TS$Flow)
    
    # Initialize vectors to store start and end dates of missing data periods
    starts <- c()
    ends <- c()
    
    # Track missing data periods
    in_na_run <- FALSE
    for (i in 1:nrow(full_TS)) {
        if (is_na[i]) {
            if (!in_na_run) {
                # Start of a new missing data period
                starts <- c(starts, full_TS$Date[i])
                in_na_run <- TRUE
            }
        } else {
            if (in_na_run) {
                # End of the current missing data period
                ends <- c(ends, full_TS$Date[i-1])
                in_na_run <- FALSE
            }
        }
    }
    
    # If the series ends with a missing data run, close the period
    if (in_na_run) {
        ends <- c(ends, full_TS$Date[nrow(full_TS)])
    }
    
    # Create data frame with results
    miss <- data.frame(Start = as.Date(starts, origin = "1970-01-01"),
                       End = as.Date(ends, origin = "1970-01-01"),
                       Duration = as.numeric(ends - starts + 1))
    
    if (nrow(miss) == 0) {
        if (!quiet) {message('No missing data.')}
        return(NULL)
    } else {
        return(miss)
    }
    
    
}
