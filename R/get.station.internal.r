#' Get station information for hydrometric stations
#' 
#' Get station information for ROBIN, USGS, or WSC hydrometric stations.
#' @param Agency Character string of Agency. 
#' @param StationID Character string of station ID.
#' @return Returns a data.frame of station information
#' @author Jennifer Dierauer

## uses 'station_metadata', included in the external package data.
get.station.internal <- function(Agency, StationID) {
  
  # Define the path to the station metadata file
  metadata_path <- system.file("extdata", "station_metadata.rds", package = "FlowScreen")
  
  # Check if the metadata file exists
  if (!file.exists(metadata_path)) {
    stop("Metadata file not found.")
  }
  
  # Read the station metadata
  station_metadata <- readRDS(metadata_path)
  
  # Check if the Agency is present in the metadata
  if (sum(station_metadata$Agency == Agency) == 0) {
    message('Agency not found in metadata.')
    return(NULL)
  }
  
  # Retrieve the station information
  st_info <- station_metadata[(station_metadata$StationID == StationID) & 
                                (station_metadata$Agency == Agency), ]
  
  # If the station is not found and the agency is "USGS"
  if (nrow(st_info) == 0 && Agency == "USGS" && substr(StationID, 1, 1) != "0") {
    # Add a "0" to the beginning of the StationID and check again
    StationID <- paste0("0", StationID)
    st_info <- station_metadata[(station_metadata$StationID == StationID) & 
                                  (station_metadata$Agency == Agency), ]
  }
  
  # Check if the station information is found
  if (nrow(st_info) == 1) {
    return(st_info)
  } else {
    message('Station not found in metadata file.')
    return(NULL)
  }
}
