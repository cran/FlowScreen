#' One parameter recursive digital filter
#' 
#' This function estimates baseflow.
#' @param discharge Numeric vector of daily flow data
#' @param k Numeric value for the recession constant (dimensionless).
#' @return Returns a numeric vector of the estimated baseflow.
#' @references Eckhardt, K. 2005. How to construct recursive digital filters
#'   for baseflow separation methods. Journal of Hydrology 352: 168-173.
#' @author Paul H. Whitfield
#' @export
#' @examples
#' data(cania.sub.ts)
#' res <- bf_oneparam(cania.sub.ts$Flow, k=0.9)
#' plot(cania.sub.ts$Date, cania.sub.ts$Flow, xlab="", ylab="Q (m3/s)", type="l")
#' points(cania.sub.ts$Date, res, type="l", col="blue")

bf_oneparam <- function(discharge, k){
    # Remove NA values and store the indices
    na_indices <- is.na(discharge)
    discharge_sub <- discharge[!na_indices]
    
    # Pre-allocate the output vector
    bf <- numeric(length(discharge_sub))
    
    # Initialize the first value
    bf[1] <- discharge_sub[1]
    
    # Calculate the base flow using vectorized operations
    for (i in 2:length(discharge_sub)) {
        bf[i] <- (k * bf[i-1] / (2 - k)) + ((1 - k) * discharge_sub[i] / (2 - k))
        if (bf[i] > discharge_sub[i]) bf[i] <- discharge_sub[i]
    }
    
    # Create the final output vector with NAs in the original positions
    bf_out <- rep(NA, length(discharge))
    bf_out[!na_indices] <- bf
    
    return(bf_out)
}