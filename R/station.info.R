#' Retrieve Station Info
#'
#' Returns station information from metadata included in the package data files. If
#' there is no metadata match for the StationID AND Agency, returns NA values for all other columns.
#' @param Agency String indicating Agency where streamflow data. 
#' @param StationID String of the Station ID
#' @param Plot Boolean indicating whether a plot of station information should be created.
#'   Default is FALSE. Plot is intended for use as the upper-left panel of the plot produced 
#'   by \code{\link{screen.summary}}.
#' @param Language Language for plotting when Plot = T.  Choice of either "English" or
#'   "French". Default is "English".
#' @return Returns a data.frame of the following station information:
#'   \itemize{
#'     \item $Agency - Name of Agency from which the record came
#'     \item $StationID
#'     \item $StnName
#'     \item $StateProv - State, Province, or Territory where the station is located
#'     \item $Country - Country in which the station is located
#'     \item $Lat - Latitude of the station, numeric
#'     \item $Lon - Longitude of the station, numeric
#'     \item $CatchmentArea_km2 - total drainage area in square kilometers
#'     \item $RHN - Indication whether the station is part of a reference hydrologic network, TRUE/FALSE
#'     \item $MetadataSource - Indication of where the metadata came from, e.g. WSC Hydat, USGS, user-supplied.
#'     \item $StationID_Alternate - Alternate station ID, e.g. original station ID versus ROBIN database
#'   }
#' @author Jennifer Dierauer
#' @import graphics
#' @export
#' @examples
#' data(cania.sub.ts)
#' stn_metdat <- station.info(cania.sub.ts$Agency[1], cania.sub.ts$ID[1])
#' print(stn_metdat)

station.info <- function(Agency, StationID, Plot = FALSE, Language = "English") {
    
    # Input validation
    if (!is.character(Agency) || length(Agency) != 1) {
        stop("Agency must be a single character string.")
    }
    
    if (!is.character(StationID) || length(StationID) != 1) {
        stop("StationID must be a single character string.")
    }
    
    if (!is.logical(Plot) || length(Plot) != 1) {
        stop("Plot must be a single logical value (TRUE or FALSE).")
    }
    
    if (!(Language %in% c('English', 'French'))) {
        stop("Language parameter must be either 'English' or 'French'.")
    }
    
    if (Plot) {
        graphics::par(mar = c(0, 0.5, 0, 0)) # set margins
        graphics::plot(1:10, 1:10, pch = "", axes = FALSE, ylab = "", xlab = "") # and plot area
    }
    
    # Set language of text
    if (Language == "English") {
        tname <- "Name: "
        tprovince <- "Province: "
        tcatcharea <- "Catchment Area (km2): "
        tperiod <- "Period: "
        tRHBN <- "RHBN Station"
        tRHN <- "RHN Station"
        tProvState <- "Prov/State: "
        tincreasing <- "Blue Trend Line = Increasing Trend"
        tdecreasing <- "Red Trend Line = Decreasing Trend"
        tnoline <- "No Line: p-value > 0.1"
        tthinline <- "0.05 < p-value <= 0.1"
        tmedline <- "0.01 < p-value <= 0.05"
        tthickline <- "p-value <= 0.01"
    } else if (Language == "French") {
        tname <- "Nom: "
        tprovince <- "Province: "
        tcatcharea <- "Bassin versant (km2): "
        tperiod <- "Periode d'obseration: "
        tRHBN <- "RHBN Station"
        tRHN <- "RHN Station"
        tProvState <- "Province/Etat: "
        tincreasing <- "ligne bleue = tendance a la hausse"
        tdecreasing <- "ligne rouge = tendance a la baisse"
        tnoline <- "pas de ligne: p > 0.1"
        tthinline <- "0.05 < p <= 0.1"
        tmedline <- "0.01 < p <= 0.05"
        tthickline <- "p <= 0.01"
    }
    
    if (is.na(Agency)) {
        Agency <- "Unknown"
    }
    
    # Pull station info from metadata
    station_info_df <- get.station.internal(Agency, StationID)
    
    # If no station data, fill with Agency and station ID, all else NA
    if (is.null(station_info_df)) {
        station_info_df <- data.frame(
            Agency = Agency,
            StationID = StationID,
            StnName = NA,
            StateProv = NA,
            Country = NA,
            Lat = NA,
            Lon = NA,
            CatchmentArea_km2 = NA,
            RHN = NA,
            MetadataSource = NA,
            StationID_Alternate = NA
        )
    }
    
    StnName <- station_info_df$StnName[1]
    Country <- station_info_df$Country
    Lat <- round(as.numeric(station_info_df$Lat), 2)
    Long <- round(as.numeric(station_info_df$Lon), 2)
    
    if (!is.na(station_info_df$RHN)) {
        RHN <- ifelse(station_info_df$RHN[1] == "TRUE", TRUE, FALSE)
    } else {
        RHN <- FALSE
    }
    
    out <- station_info_df
    
    # Add relevant station info to plot if Plot == TRUE
    if (Plot) {
        graphics::text(1, 9.5, paste(tname, StnName, sep = ""), adj = c(0, 0))
        graphics::text(1, 8.5, paste("Latitude: ", Lat, " ", "Longitude: ", Long, sep = ""), adj = c(0, 0))
        
        if (RHN) {
            graphics::text(1, 7, tRHBN, adj = c(0, 0))
        }
        
        # Add legend for plot trend lines
        graphics::text(1, 6, tincreasing, col = "darkblue", adj = c(0, 0))
        graphics::text(1, 5.5, tdecreasing, col = "darkred", adj = c(0, 0))
        graphics::text(3, 4.5, tnoline, adj = c(0, 0.5))
        graphics::text(3, 4, tthinline, adj = c(0, 0.5))
        graphics::text(3, 3.5, tmedline, adj = c(0, 0.5))
        graphics::text(3, 3, tthickline, adj = c(0, 0.5))
        graphics::points(c(1, 2), c(4, 4), type = "l", lwd = 1)
        graphics::points(c(1, 2), c(3.5, 3.5), type = "l", lwd = 2)
        graphics::points(c(1, 2), c(3, 3), type = "l", lwd = 3)
    }
    
    return(out)
}