#' Change point time series plot
#'
#' Compiles change point information for all metrics and outputs a daily flow
#' time series plot overlain with a bar plot of changepoint counts by year.
#' @param metrics output from \code{\link{metrics.all}}
#' @param type character indicating which type of metric to compile change points for.
#'   Options are "h" for high flow metrics, "l" for low flow metrics, "b" for baseflow 
#'   metrics, or "a" for all 30 metrics (10 high, 10 low, 10 baseflow).
#' @param title optional plot title. Default is FALSE indicating no plot title is wanted. 
#'   Set to TRUE to use the the default plot title, which will 
#'   look for 'plot title' attribute of the data.frame set by 
#'   \code{\link{set.plot.titles}}. All values other values 
#'   will be used as a custom plot title.
#' @param change.margins TRUE or FALSE to indicate whether the user's current 
#' margin settings should be used, or if the margins should be set within the 
#' function. Default is TRUE, to set margins to the minimal amount. 
#' @return When type="a", returns a data.frame of changepoint counts by metric 
#'   type and year.
#' @author Jennifer Dierauer
#' @seealso \code{\link{metrics.all}}
#' @importFrom grDevices cm.colors
#' @import graphics
#' @export
#' @examples
#' # load results from metrics.all function for the Caniapiscau River
#' data(caniapiscau.res)
#' 
#' # plot changepoints for all groups of metrics
#' screen.cpts(caniapiscau.res, type="l")
#' screen.cpts(caniapiscau.res, type="h")
#' screen.cpts(caniapiscau.res, type="b")

screen.cpts <- function(metrics, type="a", title = FALSE, change.margins = TRUE) {
    
    if (!(is.list(metrics))) {
        if ((length(metrics) == 1)) {
            if (metrics == "Flow screening not completed. Streamflow record must contain at least 10 hydrologic years of data.") {
                message(metrics)
                return(NULL)
            } else {stop("Invalid Input")}
        } else {stop("Invalid Input")}
    }
    
    if (!("tcpRes" %in% names(metrics))) {
        stop("Invalid Input")
    }
    if (!("indata" %in% names(metrics))) {
        stop("Invalid Input")
    }
    
    opar <- graphics::par(no.readonly = TRUE)
    plot_title <- metrics$plot.title[[1]][1]
    title_size <- metrics$plot.title[[2]][1]
    
    res <- metrics$tcpRes
    TS <- metrics$indata
    cptsh <- list() 
    cptsl <- list()
    cptsb <- list()
    
    cmcolors <- grDevices::cm.colors(20, alpha=0.5)
    colh <- cmcolors[1]
    coll <- cmcolors[20]
    colb <- cmcolors[10]
    
    if (type == "h") {
        mtitle <- "Changepoints in High Flow Metrics"
        rylab <- "Number of Changepoints (max = 10)"
        types <- "Change Point Count"
        mcol <- colh
    }
    
    if (type == "l") {
        mtitle <- "Changepoints in Low Flow Metrics"
        rylab <- "Number of Changepoints (max = 10)"
        types <- "Change Point Count"
        mcol <- coll
    }
    
    if (type == "b") {
        mtitle <- "Changepoints in Baseflow Metrics"
        rylab <- "Number of Changepoints (max = 10)"
        types <- "Change Point Count"
        mcol <- colb
    }
    
    if (type == "a") {
        mtitle <- "Changepoints in All Metrics"
        rylab <- "Number of Changepoints (max = 30)"
        types <- c("Baseflow Changepoints", "Low Flow Changepoints", 
                   "High Flow Changepoints")
        mcol <- c(colb, coll, colh)
    }
    
    # put all change points in a list

    for (i in 1:10) {
        
        metname <- names(res[i])
        cptsh[[metname]] <- res[[i]]$cpts
        
    }
    for (i in 11:20) {
        
        metname <- names(res[i])
        cptsl[[metname]] <- res[[i]]$cpts
        
    }
    for (i in 21:30) {
        
        metname <- names(res[i])
        cptsb[[metname]] <- res[[i]]$cpts
        
    }
    
    ## identify start and end dates for plot, set axis limits
    Year1 <- TS$year[1]
    Start <- as.Date(paste(Year1, "-01-01", sep=""))
    YearLast <- TS$year[length(TS$year)]
    End <- as.Date(paste(YearLast, "-12-31", sep=""))
    myxlims <- c(Start, End)
    myylims <- c(0, 1.1 * max(TS$Flow))
    NumYears <- as.numeric(YearLast) - as.numeric(Year1) + 1
    
    Year_List <- seq(from=Year1, to=YearLast, by=1)
    
    CountsH <- c(numeric(0))
    CountsL <- c(numeric(0))
    CountsB <- c(numeric(0))
    
    for (i in 1:length(cptsh)) {
        cpts.sub <- cptsh[[i]]

        if (!is.na(cpts.sub[1])) {
            cpts.sub <- attr(cpts.sub, "times")
            for (j in 1:length(cpts.sub)) {
                ind <- as.numeric(substr(as.character(cpts.sub[j]), 1, 4))
                CountsH <- c(CountsH, ind)
            }      
        }
    }
    
    for (i in 1:length(cptsl)) {
        cpts.sub <- cptsl[[i]]
 
        if (!is.na(cpts.sub[1])) {
            cpts.sub <- attr(cpts.sub, "times")
            for (j in 1:length(cpts.sub)) {
                ind <- as.numeric(substr(as.character(cpts.sub[j]), 1, 4))
                CountsL <- c(CountsL, ind)
            }      
        }
    }
    
    for (i in 1:length(cptsb)) {
        cpts.sub <- cptsb[[i]]
 
        if (!is.na(cpts.sub[1])) {
            cpts.sub <- attr(cpts.sub, "times")
            
            for (j in 1:length(cpts.sub)) {
                ind <- as.numeric(substr(as.character(cpts.sub[j]), 1, 4))
                CountsB <- c(CountsB, ind)
            }      
        }
    }

    y1 <- expression(paste("Discharge (m" ^{3}, "/s)"))
    
    if (change.margins == TRUE) {graphics::par(mar = c(3,5,2,5))}
    if (title == TRUE) {
        if (change.margins == TRUE) {graphics::par(oma=c(0,0,1,0))}
    }
    
    graphics::plot(TS$Date, TS$Flow, pch=19, cex=0.3, ylab=y1, xlab="",
         xlim=myxlims, ylim=myylims, col="grey50")
    graphics::title(main=mtitle)
    
    # add optional margin text
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
        
        graphics::mtext(title.text, side=3, line=0, outer=T, cex=mcex)
    }
    

    ### add shaded polygons covering missing data periods
    MissingDays <- suppressMessages(NA.runs(TS))
    polycol <- grDevices::gray(0.5, alpha=0.5)
    if (length(MissingDays$Start) != 0) {
        for (i in 1:length(MissingDays$Start)) {
            xx <- c(MissingDays$Start[i], MissingDays$Start[i],
                    MissingDays$End[i], MissingDays$End[i])
            yy <- c(-100, 1.5*max(TS$Flow), 1.5*max(TS$Flow), -100)
            graphics::polygon(xx,yy,col=polycol, border=NA)
        }
    }
    
    CountsH <- CountsH[CountsH < YearLast - 1]
    CountsB <- CountsB[CountsB < YearLast - 1]
    CountsL <- CountsL[CountsL < YearLast - 1]
    
    hv1 <- rep(0, length(Year_List))
    hv2 <- rep(0, length(Year_List))
    hv3 <- rep(0, length(Year_List))
    
    for (i in 1:length(Year_List)) {
        hv1[i] <- length(CountsH[CountsH == Year_List[i]])
        hv2[i] <- length(CountsL[CountsL == Year_List[i]])
        hv3[i] <- length(CountsB[CountsB == Year_List[i]])
    }
    
    
    ## add histogram of changepoints
    if (type == "a") {
        graphics::par(new = T)
        graphics::barplot(rbind(hv3, hv2, hv1), col=mcol, xlab="", ylim=c(0, 30), 
                xaxt="n", yaxt="n")
    }
    if (type == "h") {
        graphics::par(new = T)
        graphics::barplot(hv1, col=mcol, xlab="", ylim=c(0, 10), 
                xaxt="n", yaxt="n")
    }
    if (type == "l") {
        graphics::par(new = T)
        graphics::barplot(hv2, col=mcol, xlab="", ylim=c(0, 10), 
                xaxt="n", yaxt="n")
    }
    if (type == "b") {
        graphics::par(new = T)
        graphics::barplot(hv3, col=mcol, xlab="", ylim=c(0, 10), 
                xaxt="n", yaxt="n")
    }
    
    graphics::axis(side=4)
    graphics::mtext(side = 4, line=3, rylab)
    
    
    graphics::legend("topright", legend = c("Missing Data Periods", types), col=c(polycol, mcol)
           , fill=c(polycol, mcol), bty="n")

    res <- data.frame(Year=Year_List, HighFlow=hv1, LowFlow=hv2, Baseflow=hv3)
    
    if (type == "a") {return(res)}
    
    # restore settings on exit
    on.exit(suppressWarnings(graphics::par(opar)))
    
}


