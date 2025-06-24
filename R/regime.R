#' Plot flow regime
#' 
#' This function plots the min, max, mean, and two user-defined quantiles of 
#' daily streamflow to provide visual summary of the flow regime. Flow record must 
#' have at least 10 years of data to produce a plot. A visual summary is not shown for 
#' any days of the year that are missing >80% of the flow record years. 
#' Area between the upper and lower quantile is shaded grey, the dark blue line represents
#' the mean daily discharge, gray line represents the median daily discharge, 
#' and the period of record daily maximum and minimum are
#' shown with the blue points.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param q Numeric vector of the upper and lower quantile values.  Default
#'   is c(0.9, 0.1).
#' @param hyear.start Integer indicating the start month for the regime plot. 
#'   Default is 10 (October).
#' @param title optional plot title. Default is FALSE indicating no plot title is wanted. 
#'   Set to TRUE to use the the default plot title, which will 
#'   look for 'plot title' attribute of the data.frame set by 
#'   \code{\link{set.plot.titles}}. All values other values 
#'   will be used as a custom plot title.
#' @param legend TRUE or FALSE to indicate whether a legend should be included. 
#'   Default is TRUE.
#' @param change.margins TRUE or FALSE to indicate whether the user's current 
#'   margin settings should be used, or if the margins should be set within the 
#'   function. Default is TRUE, to set margins to the minimal amount. 
#' @param y.lims optional user-defined y-axis minimum and maximum. e.g. c(0, 500)
#' @importFrom stats quantile
#' @importFrom stats median
#' @import graphics
#' @author Jennifer Dierauer
#' @export
#' @examples
#' 
#' # Load example ROBIN streamflow data
#' robin_path <- system.file("extdata", "ROBIN_example.csv", package = "FlowScreen")
#' 
#' TS <- read.flows(robin_path)
#' TS <- set.plot.titles(TS)
#' regime(TS, title = TRUE)

regime <- function(TS, q = c(0.9, 0.1), title = FALSE, hyear.start = 10, 
                   y.lims = NA, legend = TRUE, change.margins = TRUE) {
    
    plot_title <-  attr(TS, 'plot title')
    title_size <- attr(TS, 'title size')
    
    opar <- graphics::par(no.readonly = TRUE)
    
    if (!inherits(TS, "data.frame")) {
        stop("Invalid Input")
    }
    
    # Use a custom hydrologic year start for the plot
    TS <- hyear.internal(TS, hyear.start)
    
    year_count <- length(unique(TS$hyear))
    if (year_count < 10) {
        message('Streamflow time series must contain at least 10 years of data.')
        return()
    }
    
    # If less than 20% of years have data on a particular day, remove it from the regime plot
    hdoy_counts <- tapply(TS$Flow, TS$hdoy, length)
    hdoy_counts[hdoy_counts < floor(0.20 * year_count)] <- NA
    
    # Initialize array to be filled
    Qdoy <- array(NA, c(366, 6))  # Use 366 to cover leap years
    colnames(Qdoy) <- c("MaxQ", "MinQ", "MeanQ", "Q90", "Q10", "Median")
    
    # Calculate statistics
    Qdoy[as.numeric(names(tapply(TS$Flow, TS$hdoy, max, na.rm = TRUE))), 1] <- tapply(TS$Flow, TS$hdoy, max, na.rm = TRUE)
    Qdoy[as.numeric(names(tapply(TS$Flow, TS$hdoy, min, na.rm = TRUE))), 2] <- tapply(TS$Flow, TS$hdoy, min, na.rm = TRUE)
    Qdoy[as.numeric(names(tapply(TS$Flow, TS$hdoy, mean, na.rm = TRUE))), 3] <- tapply(TS$Flow, TS$hdoy, mean, na.rm = TRUE)
    Qdoy[as.numeric(names(tapply(TS$Flow, TS$hdoy, stats::quantile, q[1], na.rm = TRUE))), 4] <- tapply(TS$Flow, TS$hdoy, stats::quantile, q[1], na.rm = TRUE)
    Qdoy[as.numeric(names(tapply(TS$Flow, TS$hdoy, stats::quantile, q[2], na.rm = TRUE))), 5] <- tapply(TS$Flow, TS$hdoy, stats::quantile, q[2], na.rm = TRUE)
    Qdoy[as.numeric(names(tapply(TS$Flow, TS$hdoy, stats::median, na.rm = TRUE))), 6] <- tapply(TS$Flow, TS$hdoy, stats::median, na.rm = TRUE)
    
    # Fill NAs where more than 80% of the data is missing
    Qdoy[is.na(hdoy_counts), ] <- NA
    
    # Set up polygon for inter-quantile shading
    xx <- c(1:366, 366:1)
    yy <- c(Qdoy[, 4], rev(Qdoy[, 5]))
    
    na_inds <- c(1:366)
    na_inds[is.na(hdoy_counts)] <- NA
    na_inds <- c(na_inds, rev(na_inds))
    yy[is.na(na_inds)] <- NA
    
    # Create plot
    if (change.margins == TRUE) {
        graphics::par(mar = c(4, 6, 2, 2))
    }
    
    if (title != FALSE) {
        if (change.margins == TRUE) {
            graphics::par(oma = c(0, 0, 1, 0))
        }
    }
    
    if (TS$FlowUnits[1] == 'm3/s') {
        yl1 <- expression(paste("Discharge (m" ^ {3}, "/s)"))
    } else if (TS$FlowUnits[1] == 'ft3/s') {
        yl1 <- expression(paste("Discharge (ft" ^ {3}, "/s)"))
    } else {
        yl1 <- 'unknown units'
    }
    
    if (!is.na(y.lims[1])) {
        graphics::plot(Qdoy[, 1], col = "#6BAED6", type = "p", pch = 19, cex = 0.5, 
                       xlab = "", ylab = "", xaxt = "n", ylim = y.lims, xlim = c(1, 366), las = 1)
    } else {
        y.lims <- range(pretty(c(0, TS$Flow)))
        y.lims[2] <- y.lims[2] * 1.025
        graphics::plot(Qdoy[, 1], col = "#6BAED6", type = "p", pch = 19, cex = 0.5, 
                       xlab = "", ylab = "", xaxt = "n", ylim = y.lims, las = 1)
    }
    
    graphics::title(ylab = yl1, line = 3.5)
    graphics::points(Qdoy[, 2], col = "#6BAED6", type = "p", pch = 19, cex = 0.5)
    graphics::polygon(xx[!is.na(yy)], yy[!is.na(yy)], col = "gray", border = "#3182BD")
    graphics::points(Qdoy[, 3], col = "#08519C", type = "l", lwd = 2)
    graphics::points(Qdoy[, 6], col = "gray50", type = "l", lwd = 2)
    
    axis_doy.internal(hyear.start)
    
    if (legend == TRUE) {
        SYMnames <- c("maximum, minimum    ", 
                      paste0(as.character(min(q)), ", ", as.character(max(q)), " quantile    "), 
                      "mean", "median")
        SYMcol <- c("#6BAED6", "#3182BD", "#08519C", "gray50")
        
        graphics::legend("topleft", legend = SYMnames, lwd = c(NA, 1, 2, 2, 1, NA), 
                         lty = c(NA, 1, 1, 1, 1, NA),
                         pch = c(19, NA, NA, NA, NA, 19), bty = "n",
                         pt.cex = 0.5, col = SYMcol, cex = 0.65, ncol = 2)
    }
    
    
    if (title != FALSE) {
        
        if (title == TRUE) {
            if (!is.null(plot_title)) {
                title.text <- plot_title
            } else {title.text <- NULL}
        } else {
            title.text <- title
        }
        
        if (!is.null(title_size)) {
            mcex <- title_size
        } else {mcex <- 1}
        
        graphics::mtext(title.text, side = 3, line = -1, outer = TRUE, cex = mcex)
    }
        
    
    
    on.exit(suppressWarnings(graphics::par(opar)))
}
