#' Add MetaData to Database
#'
#' Adds user-supplied station metadata to package database. Can also be used to update
#' metadata for a station that is already present in the metadata.
#' @param Agency string indicating the source of the streamflow data, e.g. USGS, 
#' WSC, etc. Cannot be NA, but can be any user-specified string, e.g. "Agency A".
#' @param StationID string, cannot be NA.
#' @param StnName string, cannot be NA.
#' @param StateProv string, State, Province, or Territory. Can be NA.
#' @param Country string, can be an abbreviation, e.g. USA, CA, or full name. 
#' @param Lat numeric indicating latitude (in decimal degrees) for the gauge location. Can be NA.
#' @param Lon numeric indicating longitude (in decimal degrees) for the gauge location. Can be NA.
#' @param CatchmentArea_km2 numeric, the total drainage area in square kilometers.
#' @param RHN TRUE or FALSE indication of whether the station is part of a reference hydrologic network, 
#' representing a catchment that has minimal human impacts. Default is FALSE.
#' @param StationID_Alternate Optional alternate station ID, default is NA.
#' @param Overwrite Indication of whether a record in the metadata should be overwritten if a match is found. 
#' Match is based on the Agency AND StationID. Default is FALSE.
#' @author Jennifer Dierauer
#' @export
#' @examples
#' 
#' met_added <- add.station.metadata(Agency = "Foo Bar", StationID = "01234", 
#' StnName = "Example Station", Country = NA, Lat = 40.0, Lon = -89.0)
#' print(met_added)

add.station.metadata <- function(Agency, StationID, StnName, StateProv = NA, Country, 
                                 Lat, Lon, 
                                 CatchmentArea_km2 = NA,
                                 RHN = FALSE, 
                                 StationID_Alternate = NA, 
                                 Overwrite = FALSE) {
  
  if (is.na(Agency)) {stop("Must supply the originating Agency for the streamflow record.")}
  if (is.na(StationID)) {stop("Must supply the Station ID.")}
  if (is.na(StnName)) {stop("Must supply a station name.")}
  
  # check input formats
  if (!is.na(Lat)) {
    if (!is.numeric(Lat)) {Lat <- as.numeric(Lat)}
    if (abs(Lat) > 90) {stop("Invalid Latitude.")}
  }
  if (!is.na(Lon)) {
    if (!is.numeric(Lon)) {Lon <- as.numeric(Lon)}
    if (abs(Lon) > 360) {stop("Invalid Longitude.")}
  }
  
  if (!is.logical(RHN)) {stop("RHN must be TRUE or FALSE.")}
  if (!is.logical(Overwrite)) {stop("Overwrite must be TRUE or FALSE.")}
  
  # append to metadata file if doesn't exist in file already
  metadata_path <- system.file("extdata", "station_metadata.rds", package = "FlowScreen")
  station_metadata <- readRDS(metadata_path)
  
  met_dat <- station_metadata[(station_metadata$Agency == Agency) & 
                                (station_metadata$StationID == StationID),]
  
  if (nrow(met_dat) == 0) {
    
    meta_added <- data.frame(Agency = Agency, 
                             StationID = StationID, 
                             StnName = StnName,
                             StateProv = StateProv,
                             Country = Country, 
                             Lat = Lat, 
                             Lon = Lon, 
                             CatchmentArea_km2 = CatchmentArea_km2,
                             RHN = RHN, 
                             MetadataSource = "User-Supplied", 
                             StationID_Alternate = StationID_Alternate)
    
    station_metadata <- rbind(station_metadata, meta_added)
    saveRDS(station_metadata, metadata_path)
    
    message('Metadata added.')
    
  } else if (Overwrite == TRUE) {
    
    station_metadata$StnName[(station_metadata$Agency == Agency) & 
                               (station_metadata$StationID == StationID)] <- StnName
    station_metadata$StateProv[(station_metadata$Agency == Agency) & 
                                 (station_metadata$StationID == StationID)] <- StateProv
    station_metadata$Country[(station_metadata$Agency == Agency) & 
                               (station_metadata$StationID == StationID)] <- Country
    station_metadata$Lat[(station_metadata$Agency == Agency) & 
                           (station_metadata$StationID == StationID)] <- Lat
    station_metadata$Lon[(station_metadata$Agency == Agency) & 
                           (station_metadata$StationID == StationID)] <- Lon
    station_metadata$CatchmentArea_km2[(station_metadata$Agency == Agency) & 
                                         (station_metadata$StationID == StationID)] <- CatchmentArea_km2
    station_metadata$RHN[(station_metadata$Agency == Agency) & 
                           (station_metadata$StationID == StationID)] <- RHN
    station_metadata$StationID_Alternate[(station_metadata$Agency == Agency) & 
                                           (station_metadata$StationID == StationID)] <- StationID_Alternate
    met_source_orig <- station_metadata$MetadataSource[(station_metadata$Agency == Agency) & 
                                                         (station_metadata$StationID == StationID)]
    
    # if MetadataSource does not already indicate it is User-Modified or User-Supplied, update
    if (!grepl("User", met_source_orig)) {
      station_metadata$MetadataSource[(station_metadata$Agency == Agency) & 
                                        (station_metadata$StationID == StationID)] <- paste(met_source_orig, "User-Modified", sep = ", ")
      
    }
    meta_added <- station_metadata[(station_metadata$Agency == Agency) & 
                                     (station_metadata$StationID == StationID),]
    message('Metadata match found. Record was over-written with user-supplied input.')
    
  } else if (Overwrite == FALSE) {
    meta_added <- met_dat  # Assign the existing metadata to meta_added
    message('Metadata match found. Record was not over-written.')
  }
  
  invisible(meta_added)
}
