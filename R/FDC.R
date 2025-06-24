#' Flow Duration Curve
#'
#' Produces a flow duration curve plot with optional Gustard type-curves that 
#' can be used to estimate catchment permeability.
#' @param TS A data frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param normalize.flow Boolean to indicate whether or not the streamflow should 
#' be normalized by dividing by the mean. Default is TRUE. Gustard's Type Curves 
#' can only be included when this is TRUE.
#' @param ylog Boolean indicating whether or not to plot the y-axis as a 
#' logarithmic scale. Default is TRUE.
#' @param title optional plot title. Default is FALSE indicating no plot title is wanted. 
#'   Set to TRUE to use the the default plot title, which will 
#'   look for 'plot title' attribute of the data.frame set by 
#'   \code{\link{set.plot.titles}}. All values other values 
#'   will be used as a custom plot title.
#' @param normal A logical indicating whether to use normal probability x-axis 
#' (normal=TRUE) or a linear probability x-axis (default, normal=FALSE).
#' @param gust A logical indicating whether to include Gustard's Type Curves 
#' (default is TRUE). Type curves can only be plotted when normalize.flow is TRUE.
#' @param ylimits A numeric vector of length 2 to set y-axis limits (default is NULL).
#' @references Gustard, A., Bullock, A., and Dixon, J.M. (1992). Report No. 108:
#'   Low flow estimation in the United Kingdom. 
#'   Oxfordshire, United Kingdom: Institute of Hydrology.
#' @author Paul Whitfield
#' @import graphics
#' @import stats
#' @export
#' @examples
#' data(caniapiscau)
#' FDC(caniapiscau, title="Caniapiscau River")

FDC <- function(TS, normalize.flow = TRUE, ylog = TRUE, title = FALSE, 
                normal=FALSE, gust=TRUE, ylimits=NULL) {
    
    # Error checkingx
    if (!is.data.frame(TS)) stop("TS must be a data frame.")
    if (!"Flow" %in% colnames(TS)) stop("TS must contain a 'Flow' column.")
    if (!is.logical(normal)) stop("normal must be a logical value.")
    if (!is.logical(gust)) stop("gust must be a logical value.")
    if (!is.null(ylimits) & (!is.numeric(ylimits))) stop("ylimits must be a numeric vector of length 2.")
    if (!is.null(ylimits) & (length(ylimits) != 2)) stop("ylimits must be a numeric vector of length 2.")
    
    plot_title <- attr(TS, "plot title")
    
    flow <- TS$Flow
    flow <- flow[!is.na(flow)]
    
    if (ylog == TRUE) {
        flow <- flow[flow > 0]
    }
    
    # Gustard's Type Curves Values
    g <- matrix(c(
        975.70, 577.26, 20.49, 3.70, 1.73, 1.00, 0.38,
        904.17, 534.08, 22.69, 4.42, 2.13, 1.26, 0.51,
        838.77, 511.37, 25.10, 5.27, 2.62, 1.58, 0.67,
        776.04, 480.48, 27.86, 6.33, 3.25, 2.00, 0.88,
        719.91, 452.42, 30.82, 7.54, 3.99, 2.51, 1.16,
        667.48, 425.82, 34.11, 9.00, 4.92, 3.16, 1.53,
        618.22, 400.44, 37.81, 10.77, 6.07, 3.98, 2.02,
        572.53, 376.64, 41.82, 12.86, 7.47, 5.01, 2.65,
        520.00, 350.65, 45.10, 15.20, 9.16, 6.30, 3.46,
        472.29, 326.46, 48.64, 17.98, 11.22, 7.94, 4.52,
        428.96, 303.93, 52.46, 21.25, 13.75, 10.00, 5.89,
        389.60, 282.96, 56.57, 25.13, 16.86, 12.57, 7.69,
        353.86, 263.44, 61.01, 29.71, 20.66, 15.83, 10.03,
        321.39, 245.26, 65.79, 35.12, 25.32, 19.93, 13.08,
        291.65, 228.19, 71.00, 41.58, 31.09, 25.13, 17.11,
        264.89, 212.45, 76.57, 49.16, 38.10, 31.64, 22.32,
        240.09, 197.49, 82.60, 58.08, 46.67, 39.81, 29.13,
        206.89, 176.99, 89.91, 67.82, 56.95, 50.13, 39.00,
        178.28, 158.62, 97.86, 79.21, 69.50, 63.12, 52.22,
        153.69, 142.20, 106.49, 92.46, 84.77, 79.43, 69.85), 
        nrow=20, byrow=TRUE)
    
    p <- c(0.02, 0.05, 0.50, 0.80, 0.90, 0.95, 0.99)
    rank <- rank(flow, ties.method="max")
    rank <- max(rank) - rank
    exceedtime <- rank / (length(flow) + 1)
    
    if (normalize.flow) {
        q <- sort(100 * (flow / mean(flow)), decreasing=FALSE)
        yl <- "Percent of Mean Discharge"
    } else {
        q <- sort(flow, decreasing=FALSE)
        
        if (TS$FlowUnits[1] == 'm3/s') {
            yl <- expression(paste("Discharge (m" ^ {3}, "/s)"))
        } else if (TS$FlowUnits[1] == 'ft3/s') {
            yl <- expression(paste("Discharge (ft" ^ {3}, "/s)"))
        } else {
            yl <- 'unknown units'
        }
    }
    
    exceed <- sort(exceedtime, decreasing=TRUE)
    
    xl <- "Exceedance probability (%)"
    
    # Determine y-axis limits and breaks
    if (is.null(ylimits)) {
        ymin <- 10^floor(log10(min(q[q > 0])))
        ymax <- 10^ceiling(log10(max(q[q > 0])))
        
        if (normalize.flow) {
            if (ylog) {
                ybreaks <- c(0.1, 1, 10, 100, 1000)
            } else {
                ybreaks <- pretty(q)
            }
            
        } else {
            ybreaks <- pretty(q)
            if (ylog) {
                ybreaks <- ybreaks[ybreaks > 0]
            }
        }
        
        if (ylog == TRUE) {
            if (ybreaks[1] > ymin) {
                ybreaks <- c(ymin, ybreaks)
            }
            if (ybreaks[length(ybreaks)] < ymax) {
                ybreaks <- c(ybreaks, ymax)
            }
            ylims <- range(ybreaks)
        } else {
            ylims <- range(pretty(q))
        }
        
    } else {
        ylims <- ylimits
        ybreaks <- pretty(q)
    }
    
    # Function to format labels
    format_labels <- function(x) {
        if (x >= 1000) {
            formatted_x <- sprintf("%.0e", x)
            return(sub("e\\+0", "e+", formatted_x))
        } else if (x >= 10) {
            return(round(x, 0))
        } else if (x >= 1) {
            return(format(round(x, 1), nsmall = 1, trim = TRUE))
        } else if (x >= 0.01) {
            return(format(round(x, 2), nsmall = 2, trim = TRUE))
        } else {
            formatted_x <- sprintf("%.0e", x)
            return(sub("e\\+0", "e+", formatted_x))
        }
    }
    
    # Set up plot margins
    opar <- graphics::par(no.readonly = TRUE)
    on.exit(suppressWarnings(graphics::par(opar)), add = TRUE)
    
    graphics::par(mar=c(4, 5, 2, 2))
    
    if (normal) {
        exceed.z <- stats::qnorm(exceed)
        p.z <- stats::qnorm(p)
        
        xlims <- c(-3, 3)
        if (ylog == FALSE) {
            graphics::plot(exceed.z, q, type='l', lwd=2, col='blue', 
                           xaxt="n", yaxt='n',
                           ylim=ylims, xlim=xlims, xlab=xl, ylab=yl, las=1)
        } else {
            graphics::plot(exceed.z, q, 
                           type='l', lwd=2, col='blue', log="y", 
                           xaxt="n", yaxt='n',
                           ylim=ylims, xlim=xlims, xlab=xl, ylab=yl, las=1)
        }
        
        
        # add optional margin text
        if (title != FALSE) {
            
            if (title == TRUE) {
                if (!is.null(plot_title)) {
                    title.text <- plot_title
                } else {title.text <- NULL}
            } else {
                title.text <- title
            }
            
            graphics::mtext(title.text)
        }
        
        probs <- c(0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5,
                   0.6, 0.7, 0.8, 0.9, 0.95, 0.98, 0.99, 0.995, 0.998, 0.999)
        
        z.vals <- stats::qnorm(probs)
        graphics::axis(side=1, at=z.vals, labels=probs, line=0, tck=-0.025, xlab=xl)
        graphics::axis(side=2, at=ybreaks, 
                       labels=sapply(ybreaks, format_labels), 
                       line=0, tck=-0.025, xlab=xl, las = 1)
        
        if ((normalize.flow == TRUE) & (gust == TRUE)) {
            for (k in 1:20) {
                graphics::points(p.z, g[k, ], type='l', col='gray50')
                graphics::text(p.z[7], g[k, 7], k-1, col='gray50', pos=4, cex=0.7)
            }
            graphics::points(exceed.z, q, type='l', lwd=2, col='blue')
            graphics::legend("bottomleft", 
                             legend="Flow Duration Curve with Gustard's Type Curves",
                             pch="", text.col="gray50", bty="n")
            graphics::text(2.5, g[20, 7], "permeable", col='gray50', pos=3, cex=0.7)
            graphics::text(2.5, g[1, 7], "impermeable", col='gray50', pos=1, cex=0.7)
        }
        
        graphics::abline(v=stats::qnorm(0.50), lty=2, col='red')
        
        if (normalize.flow == TRUE) {
            graphics::abline(h=100, lty=2, col='red')
        }
        
        
    } else {
        
        xlims <- c(0, 1)
        
        if (ylog == FALSE) {
            graphics::plot(exceed, q, type='l', lwd=2, col='blue', 
                           ylim=ylims, xlim=xlims, xlab=xl, ylab=yl, las=1, 
                           yaxt='n')
        } else {
            
        graphics::plot(exceed, q, type='l', lwd=2, col='blue', log="y", 
                       ylim=ylims, xlim=xlims, xlab=xl, ylab=yl, las=1, 
                       yaxt='n')
        }
        
        
        graphics::axis(side=2, at=ybreaks, 
                       labels=sapply(ybreaks, format_labels), 
                       line=0, tck=-0.025, xlab=xl, las = 1)
        
        
        # add optional margin text
        if (title != FALSE) {
            
            if (title == TRUE) {
                if (!is.null(plot_title)) {
                    title.text <- plot_title
                } else {title.text <- NULL}
            } else {
                title.text <- title
            }
            
            graphics::mtext(title.text)
        }
        
        if ((normalize.flow == TRUE) & (gust == TRUE)) {
            for (k in 1:20) {
                graphics::points(p, g[k, ], type='l', col='gray50')
                graphics::text(p[7], g[k, 7], k, col='gray50', pos=4, cex=0.7)
            }
            graphics::points(exceed, q, type='l', lwd=2, col='blue')
            graphics::legend("bottomleft", 
                             legend="Flow Duration Curve with Gustard's Type Curves",
                             pch="", text.col="gray50", bty="n")
            graphics::text(p[7], g[20, 7], "permeable", col='gray50', pos=3, cex=0.7)
            graphics::text(p[7], g[1, 7], "impermeable", col='gray50', pos=1, cex=0.7)
        }
        
        graphics::abline(v=0.50, lty=2, col='red')
        
        if (normalize.flow == TRUE) {
            graphics::abline(h=100, lty=2, col='red')
        }
        
    }
}