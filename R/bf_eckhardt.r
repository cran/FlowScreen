#' Eckhardt two parameter recursive digital filter 
#' 
#' This function takes vector of discharge data and estimates the baseflow
#' @param discharge vector of daily discharge observations
#' @param a Numeric value.
#' @param BFI Numeric value.
#' @return Returns
#' @author Paul Whitfield
#' @references Eckhardt, K. 2012. Technical note: Analytical sensitivity analysis 
#'   of two parameter recursive digital baseflow separation filter. Hydrology and
#'   Earth System Sciences 16: 451-455.
#' @export
#' @examples
#' data(cania.sub.ts)
#' bf <- bf_eckhardt(cania.sub.ts$Flow, 0.97, 0.8)
#' plot(cania.sub.ts$Date, cania.sub.ts$Flow, type="l")
#' points(cania.sub.ts$Date, bf, type="l", col="blue")

bf_eckhardt <- function(discharge, a, BFI){
    
    # Remove NA values and store the indices
    na_indices <- is.na(discharge)
    discharge_sub <- discharge[!na_indices]
    
    # Pre-allocate the output vector
    bf <- numeric(length(discharge_sub))
    
    # Initialize the first value
    bf[1] <- discharge_sub[1]
    
    # Calculate the base flow using vectorized operations
    for (i in 2:length(discharge_sub)) {
        bf[i] <- (((1 - BFI) * a * bf[i-1]) + ((1 - a) * BFI * discharge_sub[i])) / (1 - a * BFI)
        if (bf[i] > discharge_sub[i]) bf[i] <- discharge_sub[i]
    }
    
    # Create the final output vector with NAs in the original positions
    bf_out <- rep(NA, length(discharge))
    bf_out[!na_indices] <- bf
    
    return(bf_out)
}