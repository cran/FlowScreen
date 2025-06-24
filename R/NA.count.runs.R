#' Sum missing data points from a daily time series
#' 
#' Counts the number of missing data points by calendar year, hydrologic year, or month
#' @param input output from \code{\link{NA.runs}}
#' @param by character string identifying the time period to summarize by.  
#'   Defaults is hydrologic year ("hyear"), other choices are "year" and "month".
#'   The "month" option will return the number of missing data points for each
#'   month in the time series.
#' @param hyrstart optional argument, define start month of hydrologic year
#' @return Returns a numeric vector of the number of missing observations per
#'   summary period.  The "times" attribute of the returned vector provides the
#'   corresponding year, hyear, or month.
#' @author Jennifer Dierauer
#' @seealso \code{\link{NA.runs}}
#' @export
#' @examples
#' data(caniapiscau)
#' cania.ts <- create.ts(caniapiscau)
#' res <- NA.runs(cania.ts)
#' print(res)
#' res2 <- NA.count.runs(res)
#' print(res2)


NA.count.runs <- function(input, by="hyear", hyrstart=1) {
    
    # Check if input is NULL or an empty dataframe
    if (is.null(input) || nrow(input) == 0) {
        message("No missing values or input is empty")
        mcount <- 0
        return(mcount)
    }
    
    # Initialize NAruns
    NAruns <- as.Date(NA)
    
    # Create a sequence of dates for each run of missing values
    for (i in seq_along(input$Start)) {
        mseq <- seq(from = input$Start[i], to = input$End[i], by = 1)
        NAruns <- c(NAruns, mseq)
    }
    
    # Remove the initial NA value
    NAruns <- NAruns[-1]
    
    # Create a dataframe from NAruns
    NAruns <- data.frame(Date = NAruns)
    NAruns <- YMD.internal(NAruns)
    
    # Initialize mcount
    mcount <- NULL
    
    # Calculate the count of missing values by the specified time period
    if (by == "year") {
        mcount <- tapply(NAruns$Date, NAruns$year, length)
        labs <- unique(NAruns$year)
        attr(mcount, "times") <- labs
    } else if (by == "hyear") {
        NAruns <- hyear.internal(NAruns, hyrstart)
        mcount <- tapply(NAruns$Date, NAruns$hyear, length)
        labs <- unique(NAruns$hyear)
        attr(mcount, "times") <- labs
    } else if (by == "month") {
        NAruns$yrm <- paste(NAruns$year, NAruns$month, sep = "-")
        mcount <- tapply(NAruns$Date, NAruns$yrm, length)
        labs <- unique(NAruns$yrm)
        attr(mcount, "times") <- labs
    } else {
        stop("Invalid value for 'by'. Must be one of 'year', 'hyear', or 'month'.")
    }
    
    return(mcount)
}


