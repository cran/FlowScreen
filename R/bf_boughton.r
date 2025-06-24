#' Boughton recursive digital filter
#' 
#' This function estimates baseflow
#' @param discharge Nnumeric vector of daily flow data
#' @param k Numeric value of the recession constant (dimensionless).
#' @param C Numeric value of the partitioning factor (dimensionless).
#' @return Returns a numeric vector of the estimated baseflow.
#' @references Boughton, WC. 1993. A hydrograph-based model for estimating the 
#'   water yield of ungauged catchments.In Hydrology and Water Resources Symposium,
#'   Institution of Engineers Australia, Newcastle, NSW; 317-324.
#' @author Paul H. Whitfield
#' @export
#' @examples
#' data(cania.sub.ts)
#' res <- bf_boughton(cania.sub.ts$Flow, k=0.9, C=0.1)
#' plot(cania.sub.ts$Date, cania.sub.ts$Flow, xlab="", ylab="Q (m3/s)", type="l")
#' points(cania.sub.ts$Date, res, type="l", col="blue")

bf_boughton <- function(discharge, k, C){
    
    # Remove NA values and store the indices
    na_indices <- is.na(discharge)
    discharge_sub <- discharge[!na_indices]
    
    # Pre-allocate the output vector
    bf <- numeric(length(discharge_sub))
    
    # Initialize the first value
    bf[1] <- discharge_sub[1]
    
    # Calculate the base flow using vectorized operations
    for (i in 2:length(discharge_sub)) {
        bf[i] <- (k * bf[i-1] / (1 + C)) + (C * discharge_sub[i] / (1 + C))
        if (bf[i] > discharge_sub[i]) bf[i] <- discharge_sub[i]
    }
    
    # Create the final output vector with NAs in the original positions
    bf_out <- rep(NA, length(discharge))
    bf_out[!na_indices] <- bf
    
    return(bf_out)
}