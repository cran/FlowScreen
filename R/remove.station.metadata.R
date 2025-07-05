#' Remove MetaData for one station from database
#'
#' Removes a station's metadata from the package database based on the Agency and StationID.
#' If the agency is "USGS" and the station is not found, it will also check by adding a "0" 
#' to the beginning of the StationID. Used to remove a case added in error.
#' @param Agency string indicating the source of the streamflow data, e.g. USGS, WSC, etc. Cannot be NA.
#' @param StationID string, cannot be NA.
#' @return The metadata of the removed station if found and removed, or NULL if not found.
#' @export
#' @examples
#' \dontrun{
#' # Add station metadata
#' met_added <- add.station.metadata(
#'   Agency = "Foo Bar",
#'   StationID = "01234",
#'   StnName = "Example Station",
#'   StateProv = "Example State",
#'   Country = "Example Country",
#'   Lat = 40.0,
#'   Lon = -89.0,
#'   CatchmentArea_km2 = 500,
#'   RHN = TRUE,
#'   StationID_Alternate = "01234A",
#'   Overwrite = FALSE
#' )
#' 
#' 
#' # Remove the added station metadata
#' met_removed <- remove.station.metadata(
#'   Agency = "Foo Bar",
#'   StationID = "01234"
#' )
#' }

remove.station.metadata <- function(Agency, StationID) {
  
  if (is.na(Agency)) {stop("Must supply the originating Agency for the streamflow record.")}
  if (is.na(StationID)) {stop("Must supply the Station ID.")}
  
  # check input formats
  Agency <- as.character(Agency)
  StationID <- as.character(StationID)
  
  # Load the metadata file
  metadata_path <- system.file("extdata", "station_metadata.rds", package = "FlowScreen")
  if (!file.exists(metadata_path)) {
    stop("Metadata file not found.")
  }
  station_metadata <- readRDS(metadata_path)
  
  # Check if the Agency and StationID exist in the metadata
  met_dat <- station_metadata[(station_metadata$Agency == Agency) & 
                                (station_metadata$StationID == StationID),]
  
  if (nrow(met_dat) == 0) {
    message('No matching record found in the metadata.')
    return(NULL)
  }
  
  # Remove the specified entry
  station_metadata <- station_metadata[!(station_metadata$Agency == Agency & 
                                           station_metadata$StationID == StationID),]
  
  # Special case for USGS: Check if StationID starts with "0" and handle accordingly
  if (Agency == "USGS" && substr(StationID, 1, 1) != "0") {
    StationID_with_zero <- paste0("0", StationID)
    station_metadata <- station_metadata[!(station_metadata$Agency == Agency & 
                                             station_metadata$StationID == StationID_with_zero),]
  }
  
  # Save the updated metadata back to the RDS file
  saveRDS(station_metadata, metadata_path)
  
  message('Metadata entry removed.')
  
  return(met_dat)
}
