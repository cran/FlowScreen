#' Streamflow metrics
#'
#' Calculates 30 different flow metrics, 10 each for high flows, low flows, and baseflow.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param Season Numeric vector of months during which droughts start. Default 
#'   is c(4:9) for non-frost season droughts.
#' @param Qmax Numeric value for peaks over threshold quantile.
#'   Default is 0.95.
#' @param Dur Numeric value for minimum number of days between flood peaks.
#'   Default is 5.
#' @param Qdr Numeric value for drought quantile.  Default is 0.2, i.e. the 80th percentile
#'   of the flow duration curve.
#' @param WinSize Numeric value for moving window size (in days) for the moving
#'   window quantile drought threshold. See \code{\link{mqt}}. Default is 30.
#' @param NAthresh Numeric value indicating the threshold for missing data points
#'   in any one year.  Default is 0.50, indicating that years with more than 50 percent missing data
#'   will be omitted from the metric calculations. This value should always be set to 
#'   greater than 0.1, as years with fewer observations than approximately 1 month will
#'   cause errors.
#' @param language Character string indicating the language to be used for naming
#'   the different plot metrics. These names are used in \code{\link{screen.summary}}
#'   to label individual plots. Options are "English" or "French".  Default is "English".
#' @details This function calculates streamflow metrics and calculates the 
#'   prewhitened trend using \code{\link[zyp]{zyp.trend.vector}} and looks
#'   for changepoints in mean and variance using \code{\link[changepoint]{cpt.meanvar}}
#'   This function is intended for use as a data quality screening tool aimed 
#'   at identifying streamflow records with anthropogenic impacts and should not be used
#'   to complete a temporal trend analysis, as the calculated metrics may not be 
#'   appropriate for all catchments. See the functions linked in the following section
#'   for details on how each metric is calculated.
#' @return Returns a list with the following elements:
#' 
#'   metricTS: a list containing a vector of each metric calculated. Each
#'   vector has a times attribute providing either the year for metrics with 
#'   one observation per year or a date for metrics that may have more than one 
#'   observation per year (e.g., Peaks Over Threshold).  This 
#'   list has the following elements:
#'   \itemize{
#'     \item Annual Maximum Series - calculated with \code{\link{pk.max}}
#'     \item Day of Annual Maximum - calculated with \code{\link{pk.max.doy}}
#'     \item Peaks Over Threshold (Qmax) - calculated with \code{\link{pks}}
#'     \item Inter-Event Duration - calculated with \code{\link{pks.dur}}
#'     \item Q80 - calculated with \code{\link{Qn}}
#'     \item Q90 - calculated with \code{\link{Qn}}
#'     \item Day of Year 25 percent Annual Volume - calculated with \code{\link{pk.cov}}
#'     \item Center of Volume - calculated with \code{\link{pk.cov}}
#'     \item Day of Year 75 percent Annual Volume - calculated with \code{\link{pk.cov}}
#'     \item Duration between 25 percent and 75 percent Annual Volume - calculated with \code{\link{pk.cov}}
#'     \item Q10 - calculated with \code{\link{Qn}}
#'     \item Q25 - calculated with \code{\link{Qn}}
#'     \item Drought Start - calculated with \code{\link{dr.seas}}
#'     \item Drought Center - calculated with \code{\link{dr.seas}}
#'     \item Drought End - calculated with \code{\link{dr.seas}}
#'     \item Drought Duration - calculated with \code{\link{dr.seas}}
#'     \item Drought Severity - calculated with \code{\link{dr.seas}}
#'     \item Annual Minimum Flow - calculated with \code{\link{MAMn}}
#'     \item Mean Annual Minimum 7-day Flow - calculated with \code{\link{MAMn}}
#'     \item Mean Annual Minimum 10-day Flow - calculated with \code{\link{MAMn}}
#'     \item Mean Daily Discharge - calculated with \code{\link{bf.stats}}
#'     \item Annual Baseflow Volume - calculated with \code{\link{bf.stats}}
#'     \item Annual Mean Baseflow - calculated with \code{\link{bf.stats}}
#'     \item Annual Maximum Baseflow - calculated with \code{\link{bf.stats}}
#'     \item Annual Minimum Baseflow - calculated with \code{\link{bf.stats}}
#'     \item Mean Annual Baseflow Index - calculated with \code{\link{bf.stats}}
#'     \item Day of Year 25  percent Baseflow Volume - calculated with \code{\link{pk.bf.stats}}
#'     \item Center of Volume Baseflow - calculated with \code{\link{pk.bf.stats}}
#'     \item Day of Year 75  percent Baseflow Volume - calculated with \code{\link{pk.bf.stats}}
#'     \item Duration between 25  percent and 75  percent Baseflow Volume - calculated with \code{\link{pk.bf.stats}}
#'   }
#'   
#'   tcpRes: this list contains the results of the trend and changepoint analysis
#'   for each of the metrics in the metricTS list described above. Each list 
#'   element is a list containing the following elements:
#'   \itemize{
#'     \item MetricID - integer used to identify the metric
#'     \item MetricName - Name of the metric.
#'     \item Slope - numeric vector containing the intercept and slope of the 
#'       prewhitened linear trend calculated using the Yue Pilon method. See
#'       \code{\link[zyp]{zyp.trend.vector}}
#'     \item ci1 - upper bound of the trend's 95 percent confidence interval
#'     \item ci2 - lower bound of the trend's 95 percent conficence interval
#'     \item pval - Kendall's P-value computed for the detrended time series
#'     \item cpts - Most probable location of a changepoint, if one is detected. 
#'     \item means - Mean before and after the changepoint
#'     \item NumObs - The number of data points for the metric
#'   }
#'   
#'   inData: A data.frame of the original input daily streamflow time series.
#'   
#'   OmitYrs: A data.frame containing the years and the number of observations for
#'   any years omitted from the analysis due to insufficient data.  If no years were
#'   omitted, NA is returned.
#' @author Jennifer Dierauer
#' @seealso See the documentation for individual functions linked in the 
#'   output description for details on methods.
#'   
#'   See \code{\link{screen.metric}} to create individual plots for each metric.
#' @importFrom changepoint cpt.meanvar
#' @importFrom zyp zyp.trend.vector
#' @export
#' @examples
#' # load subset of daily streamflow time series for the Caniapiscau River
#' data(cania.sub.ts)
#' 
#' \dontrun{
#' # calculate low flow, high flow, and baseflow metrics
#' res <- metrics.all(cania.sub.ts)
#' }


metrics.all <- function(TS, Qmax=0.95, Dur=5,
                    Qdr=0.2, WinSize=30, Season=c(4:9), NAthresh=0.5,
                    language="English") {
    
    plot_title = attr(TS, 'plot title')
    title_size = attr(TS, 'title size')
    
    if (length(TS) != 12) {return('Invalid Input')}
    TS <- subset(TS, !is.na(TS$Flow))
    
    ## only run data analysis if there are more than 10 years in record
    if (length(unique(TS$hyear)) >= 10) {
        
        # Remove years with more than the threshold for missing data
        MinObs <- NAthresh * 365
        Year <- as.factor(TS$hyear)
        NumRecords <- tapply(TS$Flow, Year, length)
        YearList <- c(min(TS$hyear):max(TS$hyear))
        keepers <- as.numeric(attr(NumRecords, "dimnames")[[1]][NumRecords >= MinObs])

        if (length(keepers) != length(YearList)) {
            YearTrim <- YearList[!(YearList %in% keepers)]
            
            OmitYears <- data.frame(Years=YearTrim, Observations=0)
            for (m in 1:length(YearTrim)) {
                OmitYears$Observations[m] <- length(TS$Flow[TS$hyear == YearTrim[m]])
            }
            print("The following hydrologic years were omitted from analysis due to insufficient data points:")
            print(OmitYears)
            TS.sub <- TS[TS$hyear %in% keepers,]
        } else {
            OmitYears <- data.frame(Years="N/A", Observations="N/A")
            TS.sub <- TS
        }
        
        ### High Flow Metrics ###
        p1 <- pk.max(TS.sub) # max annual series
        p2 <- pk.max.doy(TS.sub) # doy of annual max
        
        p3 <- pks(TS.sub, Dur, Qmax) # peaks over threshold
        
        # if peaks over threshold returned NA - peaks over threshold duration also = NA
        if (!is.na(p3[1])) {
          p4 <- pks.dur(p3) #inter event duration
        } else {
          p4 <- NA
        }

        p5 <- Qn(TS.sub, n=0.80) #Q80
        p6 <- Qn(TS.sub, n=0.90) #Q90
        
        cov.stats <- pk.cov(TS.sub)
        p7 <- cov.stats[,2] # 25% annual volume doy
        p8 <- cov.stats[,3] # 50% annual volume doy
        p9 <- cov.stats[,4] # 75% annual volume doy
        p10 <- cov.stats[,5] # Duration between 25% and 75% doys
        
        ### Low Flow Metrics ###
        p11 <- Qn(TS.sub, n=0.1) #Q10
        p12 <- Qn(TS.sub, n=0.25) #Q25
        
        DrStats <- dr.seas(TS.sub, Qdr, WinSize, Season=Season)
        p13 <- DrStats[,1] #Drought Start doy
        p14 <- DrStats[,2] #Drought Center doy
        p15 <- DrStats[,3] #Drought End
        p16 <- DrStats[,4] #Drought Duration
        p17 <- DrStats[,5] #Drought Severity
        
        p18 <- MAMn(TS.sub, n=1) #annual minimum
        p19 <- MAMn(TS.sub, n=7) #mean annual minimum 7-day
        p20 <- MAMn(TS.sub, n=10) #mean annual minimum 10-day
        
        ### Baseflow Metrics ###
        mBFstats <- bf.stats(TS.sub)
        bfcov <- pk.bf.stats(TS.sub)
        
        p21 <- mBFstats[,2] # Mean Annual Q
        p22 <- mBFstats[,6] # BF volume
        p23 <- mBFstats[,3] # Average BF
        p24 <- mBFstats[,4] # Max BF
        p25 <- mBFstats[,5] # Min BF
        p26 <- mBFstats[,7] # mean bfi
        p27 <- bfcov[,1] #BF peak start
        p28 <- bfcov[,2] #BF peak center
        p29 <- bfcov[,3] #BF peak end
        p30 <- bfcov[,4] #BF peak duration
            
        
        ## Compile as a list
        Pdata <- list(p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7,
                      p8=p8, p9=p9, p10=p10, p11=p11, p12=p12, p13=p13,
                      p14=p14, p15=p15, p16=p16, p17=p17, p18=p18, p19=p19,
                      p20=p20, p21=p21, p22=p22, p23=p23, p24=p24,
                      p25=p25, p26=p26, p27=p27, p28=p28, p29=p29, p30=p30)
        
        ## Give list elements meaningful names
        MyTitles <- get.titles.internal("h", TS$FlowUnits[1], language, Qmax)$Titles[4:13]
        MyTitles <- c(MyTitles, get.titles.internal("l", TS$FlowUnits[1], language, Qmax)$Titles[4:13])
        MyTitles <- c(MyTitles, get.titles.internal("b", TS$FlowUnits[1], language, Qmax)$Titles[4:13])
        
        names(Pdata) <- MyTitles
        
        Year1 <- min(TS.sub$hyear)
        YearEnd <- max(TS.sub$hyear)
        
        ## Calculate slopes, p-values, changepoints, etc.
        params <- list()
        
        for (i in 1:length(Pdata)) {
            
            MyY <- Pdata[[i]]
            
            if (!is.na(MyY[1])) {
                
                # don't calculate change points if there's missing data, 
                # or if there are less than 10 data points
                if ((sum(is.na(MyY)) > 0) | (length(MyY) < 10)) {
                    calc_cp <- FALSE
                } else {
                    calc_cp <- TRUE
                }
                
              MyX <- attr(MyY, "times")
              
              ### format x values to work with plotting of sen slopes and change points
              if (nchar(MyX[1]) > 5) {
                
                Start <- as.Date(paste(Year1, "-01-01", sep=""))
                MyX.mod <- c(1:length(MyX))
                
                for (j in 1:length(MyX)) {MyX.mod[j] <- (MyX[j]-Start)}
                
              } else {
                
                Start <- as.numeric(min(MyX))
                MyX.mod <- c(1:length(MyX))
                for (j in 1:length(MyX)) {MyX.mod[j] <- (as.numeric(MyX[j]) - Year1) + 1}
              }
              
              mrange <- max(MyY, na.rm=T) - min(MyY, na.rm=T)
              
              if (mrange > 0) {
                  
                  tryCatch({
                      res <- zyp.trend.vector(MyY, x=MyX.mod, method="yuepilon", 
                                                   conf.intervals = TRUE,
                                                   preserve.range.for.sig.test = TRUE)
                      slope <- c(res[[11]], res[[2]])
                      ci1 <- c(res[[12]], res[[1]])
                      ci2 <- c(res[[13]], res[[4]])
                      pval <- res[[6]]
                  }, error = function(e) {
                      slope <- NA
                      ci1 <- NA
                      ci2 <- NA
                      pval <- NA
                  })
                  
                  names(slope) <- c('intercept', 'slope')
                  names(ci1) <- c('intercept_lbound', 'slope_lbound')
                  names(ci2) <- c('intercept_ubound', 'slope_ubound')
                  
              } else {
                slope <- NA
                ci1 <- NA
                ci2 <- NA
                pval <- NA
              }
              
              if (calc_cp == T) {
                
                out <- tryCatch(suppressWarnings(cpt.meanvar(as.numeric(MyY), 
                                                                   penalty="Asymptotic",
                                                                   pen.value=0.05,
                                                                   method="BinSeg")), 
                         error = function(e) NA)
                
                if (inherits(out, "cpt.range")) {
                  MyCpts <- out@cpts
                  attr(MyCpts, "times") <- MyX[MyCpts]
                  MyMeans <- out@param.est$mean
                } else {
                  MyCpts <- NA
                  MyMeans <- NA
                }

              } else {
                MyCpts <- NA
                MyMeans <- NA
              }
              
              NumObs <- length(MyX.mod)
              
            } else {
              slope <- NA
              ci1 <- NA
              ci2 <- NA
              pval <- NA
              MyCpts <- NA
              MyMeans <- NA
              NumObs <- NA
            }
            
            params.sub <- list(MetricID=i, MetricName=MyTitles[i], Slope=slope, ci1=ci1, ci2=ci2,
                               pval=pval, cpts=MyCpts, means=MyMeans, NumObs=NumObs)
            
            name <- paste(MyTitles[i])
            params[[name]] <- params.sub
        }
        
        output <- list(metricTS=Pdata, tcpRes=params, indata=TS.sub, OmitYrs=OmitYears, 
                       plot.title=list(plot_title, title_size))
        
        return(output)
    
    } else {
    message('Flow screening not completed.')
    return("Flow screening not completed. Streamflow record must contain at least 10 hydrologic years of data.")
    }
}
    
    