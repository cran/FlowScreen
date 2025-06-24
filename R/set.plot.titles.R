#' Set plot titles
#'
#' Sets the title to be used for all plots.
#' @param TS data.frame of streamflow time series loaded with \code{\link{read.flows}}.
#' @param title.elements A character vector with the title elements you want to 
#' include in the plot title, in the desired order. Possible values are: Agency, 
#' StationID, StnName, StateProv, Country, Lat, Lon, CatchmentArea_km2, 
#' MetadataSource. Default is \code{c("StationID", "StnName", "Country")}. 
#' Additional examples: \code{c("StnName", "StateProv")}, 
#' \code{c("StnName", "StationID")}, etc.
#' @param delimeter separator for title elements, default is " - "
#' @param custom.title String of a custom plot title. Default is NULL. Will
#'   supersede title.format if not NULL.
#' @param title.size parameter cex for the base::plot function. Number 
#'   indicating the amount by which plotting text and symbols should be scaled 
#'   relative to the default. 1=default, 1.5 is 50 percent larger, 0.5 is 50 percent smaller, etc.
#' @return Returns the input TS data.frame with a 'plot title' attribute added. 
#'   This attribute will be the default option used for all plot titles unless an 
#'   alternative title is passed to the plotting function, e.g. with \code{\link{regime}}.
#' @author Jennifer Dierauer
#' @export
#' @examples
#' 
#' # Load example ROBIN streamflow data
#' robin_path <- system.file("extdata", "ROBIN_example.csv", package = "FlowScreen")
#' TS <- read.flows(robin_path)
#' TS <- set.plot.titles(TS, title.elements = c("StationID", "StnName"))
#' regime(TS, title = TRUE)
#' 
#' TS <- set.plot.titles(TS, custom.title = "My Custom Plot Title")
#' regime(TS, title = TRUE)

set.plot.titles <- function(TS, title.elements = c("StationID", "StnName", "Country"), 
                            delimeter = " - ", custom.title = NULL, title.size = 1) {
  
  attr(TS, 'title size') <- title.size
  
  if (!is.null(custom.title)) {
    
    attr(TS, 'plot title') <- custom.title
    
  } else {
    
    StationID = TS$ID[1]
    Agency = TS$Agency[1]
    
    # load station metadata
    metadata_path <- system.file("extdata", "station_metadata.rds", package = "FlowScreen")
    station_metadata <- readRDS(metadata_path)
    
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
    
    if (nrow(st_info) == 1) {
      
      # Extract the specified columns from the data.frame
      title_parts <- st_info[1, title.elements, drop = FALSE]
      
      # Concatenate the values into a single string
      title <- paste(title_parts, collapse = delimeter)
      
      attr(TS, 'plot title') <- title
    } else {
      attr(TS, "plot title") <- NULL
      message("Station not found in metadata.")
    }
    
    
      
  }
    


  return(TS)
}

