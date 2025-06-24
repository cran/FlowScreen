#' Seasonal baseflow percentage
#' 
#' This function estimates the percentage of baseflow in a given period relative to the total
#' annual baseflow.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param seas Integers representing months of the year. Default is c(6:8), i.e. June-August.
#' @details This function calls \code{\link{bf_eckhardt}} to complete the 
#'   baseflow separation.
#' @return Returns a vector containing the calculated percentage for each year
#'   in the input time series.  The "times" attribute provides the corresponding
#'   year for each calculated value.
#' @author Jennifer Dierauer
#' @seealso See \code{\link{bf.stats}} to calculate additional baseflow metrics.
#' @export
#' @examples
#' data(cania.sub.ts)
#' cania.sub.ts <- set.plot.titles(cania.sub.ts, title.elements = c("StationID", "Country"))
#' res <- bf.seas(cania.sub.ts)
#' res2 <- screen.metric(res, "Percent Annual Baseflow in Jun-Aug", title = TRUE)

bf.seas <- function(TS, seas=c(6:8)) {
    
    plot_title <- attr(TS, 'plot title')
    title_size <- attr(TS, 'title size')
    
    ### Set parameter values for Eckhardt RDF
    BFindex <- 0.8
    alpha <- 0.970 ##based on values suggested by Eckhardt 2012 for perennial stream
    
    ## calculate daily BF
    TS <- subset(TS, !is.na(TS$Flow))
    TS$bf <- bf_eckhardt(TS$Flow, alpha, BFindex) 
    
    ## loop through years and calculate seasonal baseflow percentage
    years <- unique(TS$year)
    BFpct <- numeric()
    for (i in 1:length(years)) {
        
        TS.sub <- subset(TS, TS$year == years[i])
        BFvol <- sum(TS.sub$bf)
        TS.sub <- subset(TS, TS$month %in% seas)
        
        if (length(TS.sub) == 1) {
            BFpct.sub <- 0
        } else {
            BFpct.sub <- (sum(TS.sub$bf)/BFvol)
        }
        
        BFpct <- c(BFpct, BFpct.sub)
    }
    
    BFpct[is.infinite(BFpct)] <- NA
    
    attr(BFpct, "times") <- as.numeric(years)
    attr(BFpct, "dimnames") <- NULL
    attr(BFpct, 'StationID') <- TS$ID[1]
    attr(BFpct, 'Agency') <- TS$Agency[1]
    
    # carry-over the plot title if it has been set
    if (!is.null(plot_title)) {
        attr(BFpct, 'plot title') <- plot_title
        attr(BFpct, 'title size') <- title_size
    }
    
    return(BFpct)
    
}