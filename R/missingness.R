#' Determine the Annual Amount of Missing Data and Generate a Missingness Plot
#'
#' @title missingness test
#' @description Determine the annual amount of missing data and generate an
#' optional missingness plot.
#'
#' @param TS A data frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param title optional plot title. Default is FALSE indicating no plot title is wanted. 
#'   Set to TRUE to use the the default plot title, which will 
#'   look for 'plot title' attribute of the data.frame set by 
#'   \code{\link{set.plot.titles}}. All values other values 
#'   will be used as a custom plot title.
#' @param plot Logical, default is TRUE. If FALSE, does not produce a plot.
#' @param increasing Logical, default is TRUE. If FALSE, years are ordered from top to bottom.
#' @param cols Plot colors, default is white and blue. White always corresponds to NA. Only observed color can be changed.
#' @param omar Vector of length 4, outer margins for the plot.
#' @param mar Vector of length 4, margins for the plot.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{years_total}: Number of years from start to ending year.
#'     \item \code{years_with_obs}: Number of years with observations.
#'     \item \code{years_no_missing_obs}: Number of years with no missing days.
#'     \item \code{complete_years}: Individual years with no missing data.
#'     \item \code{partial_years}: Individual years with some observations.
#'     \item \code{longest_common_period_years}: Number of sequential years with complete data.
#'     \item \code{lcperiod_st}: Starting year of the sequence of years with complete data.
#'     \item \code{lcperiod_nd}: Ending year of the sequence of years with complete data.
#'     \item \code{table}: A dataframe with years, and counts and fractions of missing data.
#'     \item \code{missingness plot}: A plot showing the missingness data (if \code{plot = TRUE}).
#'   }
#'
#' @examples
#' robin_path <- system.file("extdata", "ROBIN_example.csv", package = "FlowScreen")
#' 
#' TS <- read.flows(robin_path)
#' res <- missingness(TS, cols = c("white", "red"), increasing = FALSE)
#'
#' @author Paul Whitfield
#' @export
missingness <- function(TS, title = FALSE, plot = TRUE, increasing = TRUE,
                        cols = c("white", "blue"), omar = c(2, 2, 2, 2), mar = c(3, 5, 3, 2)) {
  
  if (!inherits(TS, "data.frame")) {
    stop("Invalid Input")
  }
  
  if (!("year" %in% names(TS)) | !("Flow" %in% names(TS))) {
    stop("Invalid Input. Input should be a data frame of streamflow observations loaded with the read.flows function..")
  }
  
  opar <- graphics::par(no.readonly = TRUE)
  
  
  plot_title <-  attr(TS, 'plot title')
  title_size <- attr(TS, 'title size')
  
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
    
  }
  
  is_leap_year <- function(year) {
    (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
  }
  
  find_longest_sequence <- function(years) {
    if (length(years) == 0) return(c(NA, NA, 0))
    
    diffs <- diff(years)
    seq_ends <- which(diffs != 1)
    
    if (length(seq_ends) == 0) {
      # If there are no breaks in the sequence, return the entire range
      return(c(min(years), max(years), length(years)))
    }
    
    # Add the start and end for calculating lengths correctly
    seq_ends <- c(seq_ends, length(years))
    seq_starts <- c(1, seq_ends[-length(seq_ends)] + 1)
    seq_lengths <- seq_ends - seq_starts + 1
    
    # Find the longest sequence
    max_idx <- which.max(seq_lengths)
    start_idx <- seq_starts[max_idx]
    end_idx <- seq_ends[max_idx]
    
    c(years[start_idx], years[end_idx], seq_lengths[max_idx])
  }
  
  Year <- unique(TS$year)
  
  if (!increasing) Year <- rev(Year)
  
  # Set locations and text for month axis
  m_doy <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 336, 366)
  m_mon <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan")
  
  # Set legend text
  l_text <- c("missing", "observed")
  l_col <- cols
  
  # Make an array of year by day of year
  result <- array(NA, dim = c(length(Year), 366))
  
  # Make variables for by year summary
  result0 <- array(NA, dim = c(length(Year), 5))
  result0[, 2] <- 365
  
  for (kk in 1:length(Year)) {
    result0[kk, 1] <- Year[kk]
    if (is_leap_year(Year[kk])) result0[kk, 2] <- 366
    case <- TS[TS$year == Year[kk], ]
    result0[kk, 3] <- length(case$Flow[!is.na(case$Flow)])
    result0[kk, 4] <- result0[kk, 2] - result0[kk, 3]
    result0[kk, 5] <- result0[kk, 3] / result0[kk, 2]
  }
  
  for (kk in 1:length(Year)) {
    case <- TS[TS$year == Year[kk], ]
    for (jj in 1:length(case$Flow)) {
      result[kk, case$doy[jj]] <- ifelse(is.na(case$Flow[jj]), 0, 1)
    }
  }
  
  result[is.na(result)] <- 0
  
  if (plot) {
    par(oma = omar)
    par(mar = mar)
    
    result <- t(result)
    
    par(mar = c(7, 3, 3, 3), xpd = TRUE)
    
    image(1:366, 1:length(Year), result, axes = FALSE,
          col = cols, zlim = c(0, 1), xlab = "", ylab = "")
    box()
    
    ys <- seq(1:length(Year))
    m_at <- ys
    axis(2, at = m_at, labels = Year, cex.axis = 0.7, las = 1)
    axis(1, at = m_doy, labels = m_mon, cex.axis = 0.6,
         tck = -0.02, padj = -1.5, las = 1) 
    if (title != FALSE) {title(title.text, cex.main = mcex)}
    legend("bottom", pch = 22, horiz = TRUE, legend = l_text,
           pt.bg = l_col, bty = "n", inset = c(0, -1), col = cols[2]) # Centered horizontally and moved down
  }
  
  result0 <- data.frame(result0)
  colnames(result0) <- c("year", "days_yr", "days_obs", "missing", "fraction")
  
  # Separate all years from partial or completely missing years, by count
  all_year <- c(min(result0$year):max(result0$year))
  nyears <- length(result0$year)
  lyears <- max(result0$year) - min(result0$year) 
  lyears <- max(result0$year) - min(result0$year) + 1
  result0a <- result0[result0$missing == 0, ] # complete years
  dyear <- result0a$year
  myear <- all_year[!all_year %in% dyear]
  
  # Find the longest sequence of complete years
  lcs <- find_longest_sequence(dyear)
  lcs_st <- lcs[1]
  lcs_nd <- lcs[2]
  lcp <- lcs[3]
  
  # Determine longest sequence of incomplete years
  miss_seq <- rle(diff(as.numeric(myear)))
  miss_seq_m <- ifelse(length(miss_seq$lengths) != 0, max(miss_seq$lengths) + 1, 1)
  
  nmyears <- length(dyear)
  results <- list(
    TS[1, 1], lyears, nyears, nmyears, dyear, myear, lcp, lcs_st, lcs_nd,
    miss_seq_m, result0
  )
  names(results) <- c(
    "stnID", "years_total", "years_with_obs", "years_no_missing_obs",
    "complete_years", "partial_years", "longest_common_period_years",
    "lcperiod_st", "lcperiod_nd", "years_longest_incomplete_sequence",
    "table"
  )
  return(results)
  
  on.exit(suppressWarnings(graphics::par(opar)))
}