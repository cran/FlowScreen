#' Plot a metric with trend and change points
#' 
#' This function plots a time series of a streamflow metric with the prewhitened
#' linear trend and any detected changepoints in mean and variance. 
#' @param y Numeric vector with "times" attribute, and, optionally, a 'StationID'
#'   and a 'Agency' attribute if you want the function to auto-generate a default 
#'   plot title.
#' @param ylabel Character string for the y-axis label
#' @param title optional plot title. Default is FALSE indicating no plot title is wanted. 
#'   Set to TRUE to use the the default plot title, which will 
#'   look for 'plot title' attribute of the data.frame set by 
#'   \code{\link{set.plot.titles}}. All values other values 
#'   will be used as a custom plot title. Set to FALSE to use this 
#'   function in a multi-plot layout.
#' @param change.margins TRUE or FALSE to indicate whether the user's current 
#'   margin settings should be used, or if the margins should be set within the 
#'   function. Default is TRUE, to set margins to the minimal amount. 
#' @details This function plots detected changepoints as a vertical dashed line.
#'   The means on either side of a changepoint are plotted as solid black lines.
#'   If the temporal trend is significant (p-value < 0.1), the trend is plotted as 
#'   a blue or red line for an increasing or decreasing trend, respectively.
#'   The upper and lower 95% confidence bounds for the trend are represented by the
#'   dotted red or blue lines. If a trend is not significant, it is not plotted.
#' @return Returns a list containing results from the trend and changepoint
#'   analysis. This list has the following elements:
#'   \itemize{
#'     \item slope - Numeric vector containing the intercept and slope of the 
#'       prewhitened linear trend computed with \code{\link[zyp]{zyp.trend.vector}}
#'       using Yue Pilon's method
#'     \item ci1 - numeric vector containing the intercept and slope of the
#'       upper confidence bound. See \code{\link[zyp]{confint.zyp}}
#'     \item ci2 - numeric vector of length 2 containing the intercept and slope
#'       of the lower confidence bound. See \code{\link[zyp]{confint.zyp}}
#'     \item pval - numeric value indicatng the significance value of the detected
#'       trend, Kendall test computed within \code{\link[zyp]{zyp.trend.vector}}
#'     \item cpts - numeric vector of changepoints if any are found, computed 
#'       with \code{\link[changepoint]{cpt.meanvar}}. Will be NULL if changepoint 
#'       analysis was not run due to insufficient data.
#'     \item means - numeric vector of means computed with 
#'       \code{\link[changepoint]{cpt.meanvar}}. Will be NULL if changepoint 
#'       analysis was not run due to insufficient data.
#'   }
#' @author Jennifer Dierauer
#' @seealso See \code{\link{screen.summary}} to create a summary screening plot of 
#'   high flow, low flow, or baseflow metrics.
#'   
#'   See \code{\link{metrics.all}} to calculate 30 different streamflow metrics at once.
#'   The \code{\link{screen.metric}} function could then be used to loop through the metrics and 
#'   create an individual plot for each.
#' @import graphics
#' @importFrom zyp zyp.trend.vector
#' @importFrom changepoint cpt.meanvar
#' @export
#' @examples
#' data(cania.sub.ts)
#' 
#' # calculate and plot the annual maximum series
#' cania.sub.ts <- set.plot.titles(cania.sub.ts)
#' res <- pk.max(cania.sub.ts)
#' res1 <- screen.metric(res, ylabel=cania.sub.ts$FlowUnit[1], 
#' title = TRUE)
#' 
#' # calculate and plot the annual minimum series
#' res <- MAMn(cania.sub.ts, n=1)
#' res1 <- screen.metric(res, ylabel="Discharge (m3/s)", 
#' title = TRUE)

screen.metric <- function(y, ylabel="", title=FALSE, change.margins = TRUE) {
    
    opar <- graphics::par(no.readonly = TRUE)
    
    if ((sum(!is.na(y)) < 10) | (length(y) < 10)) {
        message('Not enough data. Must have at least 10 data points.')
        return('Not enough data. Must have at least 10 data points.')
    }
    
    if ((sum(is.na(y)) > 0) | (length(y) < 10)) {
        calc_cp <- FALSE
        if (length(y) < 10) {
            warning('Changepoint analysis not completed due to insufficient data. 
                    Must have at least 10 data points.')
        } else {
            warning('Changepoint analysis not completed due to missing data.')
        }
        
    } else {
        calc_cp <- TRUE
    }
    
    MyY <- y
    MyX <- attr(y, "times")
    
    
    ### set y axis limits
    MyYlims <- c(0, ceiling(1.2*max(MyY, na.rm=TRUE)))
    
    ### format x values to work with plotting of sen slopes and change points
    if(length(MyX[1]) > 5) { # if a date and not a year, do this
        Year <- as.numeric(format(MyX, "%Y"))
        Year1 <- min(Year)
        YearEnd <- max(Year)
        Start <- as.Date(paste(Year1, "-01-01", sep=""))
        MyX.mod <- c(1:length(MyX))
        for (j in 1:length(MyX)) {MyX.mod[j] <- (MyX[j]-Start)}
    } else { # if it's yearly data points, do this
        MyX.mod <- c(1:length(MyX))
        for (j in 2:length(MyX)) {MyX.mod[j] <- (as.numeric(MyX[j])-as.numeric(MyX[1]) + 1)}
    }

    #plot time series
    if (title != FALSE) {
        if (change.margins == TRUE) {graphics::par(oma=c(0,0,1,0))}
    } else {
        if (change.margins == TRUE) {graphics::par(oma=c(0,0,0,0))}
    }
    
    if (change.margins == TRUE) {graphics::par(mar=c(4,4,2,2))}
    graphics::par(xpd = FALSE)
        
    graphics::plot(MyX.mod, MyY, ylab=ylabel, xaxt="n", xlab="", type="p",
         pch=19, ylim=MyYlims, las = 1)
    
    # add optional margin text
    plot_title <- attr(y, 'plot title')
    title_size <- attr(y, 'title size')
    
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
        
        graphics::mtext(title.text, side=3, line=-1, outer=T, cex=mcex)
        
    } else {title.text <- NULL}
    
    
    # add x-axis ticks and labels
    if(nchar(as.character(MyX[1]), type="chars") > 4) {
        Years <- substr(MyX, 1, 4)
        Year1 <- as.numeric(Years[1])
        YearEnd <- as.numeric(Years[length(Years)])
        myticks <- seq(from=0, to=365*(YearEnd-Year1), by=365)
        graphics::axis(1, at=myticks, labels=c(Year1:YearEnd), las = 3)
    } else {
        graphics::axis(1, at=1:((max(MyX.mod))), labels=c(min(MyX):max(MyX)))
    }
    
    # used zyp.sen and confint.zyp to get the intercept and slope of the 
    # upper and lower confidence intervals
    mrange <- max(MyY, na.rm=T) - min(MyY, na.rm=T)
    if (mrange > 0) {
        
        # trend intercept, slope, and p-value are calculated as the prewhitened
        # linear trend with the Yue Pilon method.
        res <- zyp::zyp.trend.vector(MyY, x=MyX.mod, method="yuepilon", 
                                     conf.intervals = T,
                                     preserve.range.for.sig.test = T)
        slope <- c(res[[11]], res[[2]])
        
        ci1 <- c(res[[12]], res[[1]])
        
        ci2 <- c(res[[13]], res[[4]])
        
        names(slope) <- c('intercept', 'slope')
        
        names(ci1) <- c('intercept_lbound', 'slope_lbound')
        names(ci2) <- c('intercept_ubound', 'slope_ubound')
        pval <- res[[6]]
        
    } else {
        slope <- NA
        ci1 <- NA
        ci2 <- NA
        pval <- NA
    }

    #add trend line if p-value is less than 0.1
    if (!is.na(pval) & (pval <= 0.1)) {
        mcol <- ifelse(slope[2] < 0, "darkred", "darkblue")
        mlwd <- ifelse(pval <= 0.05, ifelse(pval <= 0.01, 3, 2), 1)
        graphics::abline(coef=slope, col=mcol, lwd=mlwd)
        graphics::abline(coef=ci1, lty=3, col=mcol)
        graphics::abline(coef=ci2, lty=3, col=mcol)
        
        ypos <- ifelse(mean(MyY[1:3]) <= 0.5*MyYlims[2], 0.95*MyYlims[2], 1.1*MyYlims[1])
 
        mypvalue <- round(as.numeric(pval), digits=2)
        
        if (mypvalue == 0) {
            graphics::text(MyX.mod[1], ypos, paste("Trend p-value < 0.01"),col=mcol, 
                 adj=c(0,0))
        } else {
            graphics::text(MyX.mod[1], ypos, paste("Trend p-value =", mypvalue),col=mcol, 
                adj=c(0,0))
        }
    }
    
    # find change points if there is no missing data
    if (calc_cp == T) {
        
        out <- suppressWarnings(changepoint::cpt.meanvar(as.numeric(MyY), 
                                                         penalty="Asymptotic",
                                                         pen.value=0.05,
                                                         method="BinSeg"))
        MyCpts <- out@cpts
        MyMeans <- out@param.est$mean
        
        #add vertical lines for change points and horizontal lines for means
        NumPoints <- length(MyX.mod)
        MyCpts <- MyCpts[(MyCpts > 3) & (MyCpts < (NumPoints-3))] ## remove cpts at end and beginning
        
        if (length(MyCpts) > 0) {
            
            for (j in 1:length(MyCpts)) {
                graphics::abline(v=MyX.mod[MyCpts[j]], lwd=2, lty=5)
                ifelse(j==1, xpts <- c(-10, MyX.mod[MyCpts[j]]),
                       xpts <- c(MyX.mod[MyCpts[j-1]], MyX.mod[MyCpts[j]]))
                ypts <- c(MyMeans[j], MyMeans[j])
                graphics::points(xpts, ypts, type="l", lwd=2)
            }
            
            xpts <- c(xpts[2], 1.1*max(MyX.mod))
            ypts <- c(MyMeans[length(MyMeans)], MyMeans[length(MyMeans)])
            graphics::points(xpts, ypts, type="l", lwd=2)
            
            MyCpts <- attr(y, 'times')[MyCpts]
            
        }
        
    } else {
        MyCpts <- NULL
        MyMeans <- NULL
    }
    
    #compile results for return
    out <- list(Slope=slope, ci1=ci1, ci2=ci2, pval=pval, cpts=MyCpts, means=MyMeans)
    return(out)
    
    on.exit(suppressWarnings(graphics::par(opar)))
}

