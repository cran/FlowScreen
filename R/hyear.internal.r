#' Add hydrologic Year, month, and doy columns to a daily time series
#' 
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param hyrstart define start month of hydrologic year. Defaults to 10 (October).
#' @return Returns a data.frame with hyear, hmonth, and hdoy columns 
#'   appended to the original input data.frame.
#' @author Jennifer Dierauer


hyear.internal <- function(TS, hyrstart = 10) {
    
    # Create new columns for hydrological year, month, and day of year
    TS$hyear <- TS$year
    TS$hmonth <- TS$month
    
    # Remove 'hdoy' column if it exists
    if ("hdoy" %in% colnames(TS)) {
        TS$hdoy <- NULL
    }
    
    # Re-number hmonth and hyear based on start of hydrologic year
    if (hyrstart != 1) {
        # Define hydrologic year based on start month
        MonthsUp <- hyrstart:12
        MonthsDown <- 1:(hyrstart - 1)
        
        TS$hyear <- ifelse(TS$month %in% MonthsUp, TS$year + 1, TS$year)
        TS$hmonth <- ifelse(TS$month %in% MonthsUp, TS$month - hyrstart + 1, TS$month + (12 - hyrstart + 1))
        
        # Create a full sequence of dates for each hydrologic year
        unique_hyears <- unique(TS$hyear)
        full_dates_list <- lapply(unique_hyears, function(hyr) {
            start_date <- as.Date(paste(hyr - 1, hyrstart, "01", sep = "-"))
            end_date <- as.Date(paste(hyr, hyrstart - 1, "01", sep = "-"))
            end_date <- seq.Date(end_date, by = "month", length.out = 2)[2] - 1
            data.frame(Date = seq.Date(start_date, end_date, by = "day"), hyear = hyr)
        })
        full_dates <- do.call(rbind, full_dates_list)
        
        # Merge with the input data frame to ensure all dates are included
        TSb <- merge(TS, full_dates, by = c("Date", "hyear"), all = TRUE)
        
        # Split the data by hydrologic year
        split_data <- split(TSb, TSb$hyear)
        
        # Assign hdoy within each hydrologic year
        split_data <- lapply(split_data, function(df) {
            df$hdoy <- seq_len(nrow(df))
            df
        })
        
        # Combine the split data back into a single data frame
        TSb <- do.call(rbind, split_data)
        
        # Merge the hdoy column back to the original data frame
        TS <- merge(TS, TSb[, c("Date", "hdoy")], by = "Date", all.x = TRUE)
    }
    
    return(TS)
}
