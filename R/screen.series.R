#' Create a plot of the daily streamflow time series
#'
#' Plots the daily streamflow time series and color codes points by data quality codes if 
#' the data are from Water Survey Canada. Also highlights date ranges with missing
#' observations.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param title optional plot title. Default is FALSE indicating no plot title is wanted. 
#'   Set to TRUE to use the the default plot title, which will 
#'   look for 'plot title' attribute of the data.frame set by 
#'   \code{\link{set.plot.titles}}. All values other values 
#'   will be used as a custom plot title.
#' @param change.margins TRUE or FALSE to indicate whether the user's current 
#' margin settings should be used, or if the margins should be set within the 
#' function. Default is TRUE, to set margins to the minimal amount. 
#' @author Jennifer Dierauer and Paul Whitfield
#' @importFrom grDevices heat.colors
#' @import graphics
#' @export
#' @examples
#' # load flow time series for the Caniapiscau River
#' data(cania.sub.ts)
#' 
#' # plot daily time series with default margin text
#' screen.series(cania.sub.ts)

screen.series <- function (TS, title = FALSE, change.margins = TRUE) {
    
    opar <- graphics::par(no.readonly = TRUE)

    if (TS$FlowUnits[1] == 'm3/s') {
        y1 = expression(paste("Discharge (m" ^{3}, "/s)"))
    } else if (TS$FlowUnits[1] == 'ft3/s') {
        y1 = expression(paste("Discharge (ft" ^{3}, "/s)"))
    } else {
        y1 = 'unknown units'
    }
    
    
    plot_title <-  attr(TS, 'plot title')
    title_size <- attr(TS, 'title size')
    
    
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
    } else {title.text <- NULL}
    
    
    if (!is.null(title.text)) {graphics::par(oma = c(0, 0, 3, 0))}
    
    stdata <- station.info(Agency = TS$Agency[1], StationID = TS$ID[1])
    Country <- stdata$Country
    Agency <- stdata$Agency
    
    if (Agency == "WSC") {
        SYMs <- c("", "E", "A", "B", "D", "R")
        SYMnames <- c("No Code", "Estimate", "Partial Day", "Ice Conditions", 
                      "Dry", "Revised")
        SYMcol <- c("black", "#E41A1C", "#4DAF4A", "#377EB8", 
                    "#FF7F00", "#984EA3")
        codes <- as.factor(TS$Code)
        codes <- match(codes, SYMs)
        if (change.margins == TRUE) {graphics::par(mar = c(4, 4.5, 0, 0.5))}
        mYlims <- c(0, 1.2 * max(TS$Flow))
        graphics::plot(TS$Date, TS$Flow, pch = 19, col = SYMcol[codes], 
             cex = 0.5, xlab = "", ylab = "", ylim = mYlims, las = 1)
        graphics::title(ylab = y1, line = 3)
        graphics::legend(TS$Date[1], 1.15 * max(TS$Flow), SYMnames, pch = 19, 
               pt.cex = 0.9, cex = 0.9, col = SYMcol, bty = "n", 
               xjust = 0, x.intersp = 0.5, yjust = 0.5, ncol = 3)
    } else {
        if (change.margins == TRUE) {graphics::par(mar = c(3, 4, 0, 0.5))}
        
        mYlims <- c(0, 1.2 * max(TS$Flow))
        graphics::plot(TS$Date, TS$Flow, pch = 19, cex = 0.5, xlab = "", ylab="", ylim = mYlims)
        graphics::title(ylab = y1, line = 2)
    }
    
    MissingDays <- NA.runs(TS)
    polycol <- grDevices::heat.colors(1, alpha = 0.2)
    if (length(MissingDays$Start) != 0) {
        for (i in 1:length(MissingDays$Start)) {
            xx <- c(MissingDays$Start[i], MissingDays$Start[i], 
                    MissingDays$End[i], MissingDays$End[i])
            yy <- c(-100, 1.5 * max(TS$Flow), 1.5 * max(TS$Flow), 
                    -100)
            graphics::polygon(xx, yy, col = polycol, border = NA)
        }
    }
    
    if (!is.null(title.text)) {
        
        graphics::mtext(title.text, side=3, line=1, outer=T, cex=mcex)
    }
    
    on.exit(suppressWarnings(graphics::par(opar)))
    
}