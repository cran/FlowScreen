#' Plot one or more frames from the summary screening plot
#'
#' This function plots one or more frames (i.e. time series plot) from any of the three 
#' plot.screening summary plots at a time. It can be used to create custom
#' summary plots - see the example code.
#' @param metrics output from \code{\link{metrics.all}}
#' @param type Character string indicating the set of metrics to plot.  Options
#'   are "h" for high flow metrics, "l" for low flow metrics, or "b" for baseflow
#'   metrics.
#' @param element Numeric index(es) (1-10) of the frame(s) to plot, see details of this function
#'   for the list of metrics for each category (high, low, baseflow). Each category has
#'   ten different metrics that can be plotted individually. Default is NULL, which creates
#'   individual plots for all ten metrics. A list of elements c(1, 5, 10) can be specified
#'   or a range c(1:5).
#' @param language Language for plot labels.  Choice of either "English" or
#'   "French". Default is "English".
#' @param mmar Numeric vector specifying plot margins. Default is c(3,4,0.5,0.5)
#' @param title optional plot title. Default is FALSE indicating no plot title is wanted. 
#'   Set to TRUE to use the the default plot title, which will 
#'   look for 'plot title' attribute of the data.frame set by 
#'   \code{\link{set.plot.titles}}. All values other values 
#'   will be used as a custom plot title. Set to FALSE to use this 
#'   function in a multi-plot layout.
#' @param multi Boolean indicating whether the function is being used to create one plot
#'   in a multi-plot layout. Default is F. If T, suppresses the reset of plot parameter settings.
#'   This plot function will only work for a multi-plot layout if text=F
#' @param xaxis Boolean indicating whether to plot an x-axis. Default = T.
#' @details High flow metrics include:
#'   \enumerate{
#'     \item Annual Maximum Series
#'     \item Annual Maximum Day of Year
#'     \item Peaks Over Threshold (Qmax)
#'     \item Inter-Event Duration
#'     \item Q80
#'     \item Q90
#'     \item Day of Year 25 percent Annual Flow
#'     \item Center of Volume
#'     \item Day of Year 75 percent Annual Flow
#'     \item Duration between 25 percent and 75 percent Annual Flow
#'   }
#'   
#'   Low flow metrics include:
#'   \enumerate{
#'     \item Q10
#'     \item Q25
#'     \item Drought Start
#'     \item Drought Center
#'     \item Drought End
#'     \item Drought Duration
#'     \item Drought Severity
#'     \item Annual Minimum Flow
#'     \item Mean Annual Minimum 7-day Flow
#'     \item Mean Annual Minimum 10-day Flow
#'   }
#'   
#'   Baseflow metrics include:
#'   \enumerate{
#'     \item Mean Daily Discharge
#'     \item Annual Baseflow Volume
#'     \item Annual Mean Baseflow
#'     \item Annual Maximum Baseflow
#'     \item Annual Minimum Baseflow
#'     \item Mean Annual Baseflow Index
#'     \item Day of Year 25 percent Baseflow Volume
#'     \item Center of Volume Baseflow
#'     \item Day of Year 75 percent Baseflow Volume
#'     \item Duration between 25 percent and 75 percent Baseflow Volume
#'   }
#' @author Jennifer Dierauer and Paul Whitfield
#' @importFrom graphics par
#' @export
#' @examples
#' # load results from metrics.all function for the Caniapiscau River
#' data(caniapiscau.res)
#' caniapiscau.ts <- caniapiscau.res$indata
#' 
#' # plot one frame from the baseflow screening plot
#' screen.frames(caniapiscau.res, type="b", element=1)
#' 
#' # plot three frames from the low flow screening plot
#' screen.frames(caniapiscau.res, type="l", element=c(1:3))
#' 
#' # create a custom summary plot
#' opar <- par(no.readonly = TRUE)
#' layout(matrix(c(1,2,3,4), 2, 2, byrow=TRUE))
#' par(oma=c(0,0,3,0))
#' stninfo <- station.info(caniapiscau.ts$Agency[1], caniapiscau.ts$ID[1], Plot=TRUE)
#' screen.frames(caniapiscau.res, type="h", element=1, multi=TRUE)
#' screen.frames(caniapiscau.res, type="l", element=1, multi=TRUE)
#' screen.frames(caniapiscau.res, type="b", element=1, multi=TRUE)
#' 
#' par <- opar
#' layout(1,1,1)
#' 
#' # or plot everything!
#' opar <- par(no.readonly = TRUE)
#' layout(matrix(c(1:30), 5, 6, byrow=TRUE))
#' screen.frames(caniapiscau.res, type="h", multi=TRUE)
#' screen.frames(caniapiscau.res, type="l", multi=TRUE)
#' screen.frames(caniapiscau.res, type="b", multi=TRUE)
#' par <- opar
#' layout(1,1,1)


screen.frames <- function(metrics, type = "h", element=NULL, language = "English", 
                          mmar=c(3,4,0.5,0.5), title=FALSE, multi=F, xaxis=T){
    
    if (multi==F) {opar <- graphics::par(no.readonly = TRUE)}
    
    TS <- metrics[[3]]
    inmetrics <- metrics[[1]]
    inparams <- metrics[[2]]
    
    flow.units <- TS$FlowUnits[1]
    plot_title <- metrics$plot.title[[1]][1]
    title_size <- metrics$plot.title[[2]][1]
    
    Year1 <- min(c(as.numeric(TS$hyear[1]), as.numeric(TS$year[1])))
    YearEnd <- max(c(max(as.numeric(TS$year)), max(as.numeric(TS$hyear))))
    hyrstart <- as.numeric(subset(TS, TS$hmonth==1)$month[1])
    
    # calculate mean annual flow correlation with time for cov plots
    maf <- inmetrics[[21]]
    
    if (type == "l") {
        DataType <- c(1, 1, 2, 2, 2, 3, 5, 1, 1, 1)
        start <- 11
        end <- 20
    }
    if (type == "h") {
        DataType <- c(1, 2, 1, 3, 1, 1, 2, 2, 2, 3)
        start <- 1
        end <- 10
    }
    if (type == "b") {
        DataType <- c(1, 5, 1, 1, 1, 4, 2, 2, 2, 3)
        start <- 21
        end <- 30
    }
    
    inmetrics <- inmetrics[start:end]
    inparams <- inparams[start:end]
    Qmax <- 0.95
    
    if (type == "h") {
        temp <- inparams[[3]]$MetricName
        msplit <- strsplit(temp, " ", fixed = T)[[1]]
        Qmax <- substr(msplit[4], 3, 4)
        Qmax <- paste("0.", Qmax, sep = "")
    }
    
    MyXlabs <- get.titles.internal(type, flow.units, language, Qmax)$Xlabs
    MyYlabs <- get.titles.internal(type, flow.units, language, Qmax)$Ylabs
    
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
        
    } else {title.text <- NULL}
    
    if (is.null(element)){
        
        for (i in 1:10) {
            screen.frames.internal(inmetrics[[i]], inparams[[i]], 
                          MyYlabs[i + 3], DataType[i], maf, mmar, title.text, xaxis, 
                          Year1, YearEnd, hyrstart)
        }
        
    } else if (length(element) > 1) {
        
        for (i in 1:length(element)) {
            
            dtype <- DataType[element[i]]
            mylab <- MyYlabs[element[i] + 3]
            screen.frames.internal(inmetrics[[element[i]]], inparams[[element[i]]], 
                          mylab, dtype, maf, mmar, title.text, xaxis, Year1, YearEnd, hyrstart)
        }
        
    } else {
        
        dtype <- DataType[element]
        mylab <- MyYlabs[element + 3]
        screen.frames.internal(inmetrics[[element]], inparams[[element]], 
                                      mylab, dtype, maf, mmar, title.text, xaxis, Year1, YearEnd,
                                      hyrstart)
    }
    
    if (multi==F) {on.exit(suppressWarnings(graphics::par(opar)))}
    
}