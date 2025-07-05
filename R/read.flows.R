#' Read file of streamflows 
#' 
#' Reads .csv, .Rdata, or .rds files of daily streamflow time series.  Recognizes 
#' several formats, including those used by Water Survey Canada (WSC), United 
#' States Geological Survey (USGS), and ROBIN. Reads fixed width .txt files in 
#' GRDC format only. Uses read.csv(), load(), readRDS(), read.fwf() functions 
#' from base package and returns data frame with ID, Date, Flow, Agency,  and, if available,
#' associated quality codes and source agency. Replaces negative values that are 
#' sometimes used to denote missing data with NAs. 
#' 
#' Streamflow records in .csv, .Rdata, or .rds format that are not from the 
#' USGS, WSC, ROBIN, or GRDC can be read by read.flows() if they contain the 
#' following required columns. Date format is auto-detected as long as it is some 
#' version of YYYY/mm/dd, mm/dd/YYYY, mm-dd-yy, etc. The file-to-be-read must 
#' contain, at a minimum, columns containing a partial matches to the following 
#' (not case sensitive):
#'    \itemize{
#'     \item flow | val | value for the daily streamflow discharge (Flow column)
#'     \item id | site for the ID column
#'     \item date for the Date column  
#'   }
#'   
#' Optional columns names for partial matching include:  
#'    \itemize{
#'     \item sym | code | flag for the SYM column (quality codes)
#'     \item agency for the Agency column
#'   }
#' 
#' @param filename name of .csv, .txt, .rds, or .rdata file to be read. 
#' Filename should contain the file type extension, e.g. "station1.csv"
#' @param flow.units Character string indicating the units for streamflow values, one of either
#' 'ft3/s' or 'm3/s'. If the streamflow is in different units, it must be converted prior to use of 
#' the package functions, or the units will be labeled as 'Unknown'. Default is 'm3/s'.
#' @param convert.to Character string indicating desired flow units (if different 
#' from original flow units). Options are 'm3/s' or 'ft3/s'. Default is \code{NULL}, 
#' indicating no unit conversion. If input matches the \code{flow.units} parameter, nothing will be done. 
#' @param hyrstart integer used to define start month of hydrologic year. Defaults to 10 (October).
#' @author Jennifer Dierauer
#' @importFrom tools file_ext
#' @importFrom utils read.csv
#' @importFrom utils read.fwf
#' @export 
#' @examples 
#' 
#' # example code to read a file, not run
#' # my_file_path <- "/Project/file1.csv"
#' # dat1 <- read.flows(my_file_path)
#' 
#' # Example code using external files included with the package
#' wsc_path <- system.file("extdata", "WSC_example.csv", package = "FlowScreen")
#' wsc_dat <- read.flows(wsc_path)
#' 
#' usgs_path <- system.file("extdata", "USGS_example.csv", package = "FlowScreen")
#' usgs_dat <- read.flows(usgs_path, flow.units = 'ft3/s', convert.to = 'm3/s')
#' 
#' robin_path <- system.file("extdata", "ROBIN_example.csv", package = "FlowScreen")
#' robin_dat <- read.flows(robin_path)
#' 
#' \dontrun{
#' grdc_path <- system.file("extdata", "GRDC_example.txt", package = "FlowScreen")
#' grdc_dat <- read.flows(grdc_path)
#' }

read.flows <- function(filename, flow.units = 'm3/s', convert.to = NULL, hyrstart = 10) {
    
    # Load station metadata
    metadata_path <- system.file("extdata", "station_metadata.rds", package = "FlowScreen")
    station_metadata <- readRDS(metadata_path)
    
    # Get file extension
    file_ext <- tolower(tools::file_ext(filename))
    
    # Error handling for hyrstart
    if (!is.numeric(hyrstart) | !(hyrstart %in% 1:12)) {
        stop('hyrstart must be an integer between 1 and 12, representing a month.')
    }
    
    # Error handling for convert.to
    if (!is.null(convert.to)) {
        if (!(convert.to %in% c('m3/s', 'ft3/s'))) {
            stop("convert.to parameter must be either NULL, or one of: 'm3/s', 'ft3/s'.")
        }
    }
    
    # Error handling for file extension
    if (!file_ext %in% c("rdata", "csv", "rds", "txt")) {
        stop("File must be a .rdata, .rds, .csv, or .txt file")
    }
    
    # Function to read CSV files, including handling for WSC files
    read_csv_file <- function(filename) {
        check_header <- tryCatch(utils::read.csv(filename, nrows = 1, stringsAsFactors = FALSE), 
                                 error = function(e) NA)
        
        if ((length(check_header) == 1) & (is.na(check_header[1]))) {
            ddata <- utils::read.csv(filename, header = FALSE, skip = 1, 
                                     col.names = c('ID', 'PARAM', 'Date', 'Flow', 'SYM'))
            
        } else {
            ddata <- utils::read.csv(filename, check.names = FALSE, stringsAsFactors = FALSE)
            colnames(ddata) <- trimws(colnames(ddata))
        }
        
        # trim off footer if present
        mID <- grep("id|site", tolower(colnames(ddata)))
        
        if (length(mID) == 0) stop("Station ID column could not be auto-detected.")
        
        if (ddata[nrow(ddata), mID] != ddata[1, mID]) {
            cut <- length(ddata[,1])-2
            ddata <- ddata[1:cut,]
        }
        
        return(ddata)
    }
    
    # Function to get metadata value
    get_meta_value <- function(met_dat, key) {
        gsub("^\\s+|\\s+$", "", unlist(strsplit(met_dat[grepl(key, met_dat)], ":"))[2])
    }
    
    # Function to update metadata
    update_metadata <- function(grdc_metadata, station_metadata, metadata_path) {
        st_info <- subset(station_metadata, StationID == grdc_metadata$StationID & Agency == grdc_metadata$Agency)
        if (nrow(st_info) == 0) {
            station_metadata <- rbind(station_metadata, grdc_metadata)
            saveRDS(station_metadata, metadata_path)
        }
    }
    
    # Function to handle GRDC .txt files
    read_grdc_txt <- function(filename) {
        line1 <- readLines(filename, n = 1)
        if (!grepl("GRDC", line1)) {
            stop("Unknown streamflow record source and format. The read.flows function 
           can only read .txt files in the GRDC format.")
        }
        ddata <- read.fwf(filename, skip = 37, widths = c(10, -7, 12), header = FALSE, stringsAsFactors = FALSE)
        colnames(ddata) <- c('Date', 'Flow')
        ddata$SYM <- NA
        ddata$Date <- as.Date(ddata$Date, "%Y-%m-%d")
        ddata <- extract_grdc_metadata(filename, ddata)
        return(ddata)
    }
    
    # Function to extract GRDC metadata
    extract_grdc_metadata <- function(filename, ddata) {
        met_dat <- readLines(filename, n = 19, encoding = "ISO-8859-1")
        grdc_metadata <- data.frame(Agency = "GRDC", 
                                    StationID = get_meta_value(met_dat, "GRDC-No"), 
                                    StnName = paste(get_meta_value(met_dat, "Station"), 
                                                    get_meta_value(met_dat, "River"), sep = ", "), 
                                    StateProv = NA,
                                    Country = get_meta_value(met_dat, "Country"), 
                                    RHN = NA,
                                    Lat = as.numeric(get_meta_value(met_dat, "Latitude")), 
                                    Lon = as.numeric(get_meta_value(met_dat, "Longitude")), 
                                    CatchmentArea_km2 = NA,
                                    MetadataSource = "GRDC file header", 
                                    StationID_Alternate = NA)
        
        grdc_metadata$StnName <- iconv(grdc_metadata$StnName, from = "ISO-8859-1", to = "UTF-8")
        grdc_metadata$Country <- iconv(grdc_metadata$Country, from = "ISO-8859-1", to = "UTF-8")
        
        missing_data_value <- gsub("^\\s+|\\s+$", "", 
                                   unlist(strsplit(met_dat[grepl("missing values", met_dat)], " "))) 
        missing_data_value <- as.numeric(missing_data_value[length(missing_data_value)])
        
        ddata$ID <- grdc_metadata$StationID
        ddata$Agency <- grdc_metadata$Agency
        ddata$Flow[ddata$Flow == missing_data_value] <- NA
        update_metadata(grdc_metadata, station_metadata, metadata_path)
        return(ddata)
    }
    
    contains_time <- function(date_vector) {
        # Regular expression to match time elements (HH:MM:SS or HH:MM:SS.SSS)
        time_pattern <- "\\b\\d{2}:\\d{2}:\\d{2}(\\.\\d{3})?\\b"
        
        # Check if any element in the vector matches the time pattern
        contains_time <- grepl(time_pattern, date_vector)
        
        return(contains_time)
    }
    
    remove_time <- function(date_vector) {
        # Regular expression to match time elements (HH:MM:SS or HH:MM:SS.SSS) and any spaces before it
        time_pattern <- "\\s*\\d{2}:\\d{2}:\\d{2}(\\.\\d{3})?"
        
        # Use sub to remove the time element
        date_only_vector <- sub(time_pattern, "", date_vector)
        
        # Trim any trailing spaces that might result from the substitution
        date_only_vector <- trimws(date_only_vector)
        
        return(date_only_vector)
    }
    
    
    convert_dates <- function(date_vector) {
        if (length(date_vector) == 0) {
            return(as.Date(character(0)))  # return an empty Date vector if input is empty
        }
        
        date_sample <- date_vector[1]
        
        if (contains_time(date_sample)) {
            date_vector <- remove_time(date_vector)
        }
        
        if (grepl("/", date_sample)) {
            split_char <- "/"
        } else if (grepl("-", date_sample)) {
            split_char <- "-"
        } else {
            stop("Unknown date format.")
        }
        
        
        split_dates <- strsplit(date_vector, split_char)
        
        # Create new columns 'Part1', 'Part2', and 'Part3' by extracting elements from the list
        dates_df <- data.frame(
            Part1 = as.numeric(sapply(split_dates, `[`, 1)),
            Part2 = as.numeric(sapply(split_dates, `[`, 2)),
            Part3 = as.numeric(sapply(split_dates, `[`, 3))
        )
        
        day_column <- rep(NA, 3)
        month_column <- rep(NA, 3)
        for (i in 1:3) {
            day_column[i] <- sum(c(1:31) %in% unique(dates_df[,i]))
            month_column[i] <- ((length(unique(dates_df[,i])) <= 12) & 
                                    (max(dates_df[,i]) <= 12))
        }
        
        day_column <- which.max(day_column)
        month_column <- which(month_column == TRUE)
        if (length(month_column) > 1) {
            check_seq <- rep(NA, length(month_column))
            for (i in 1:length(month_column)) {
                check_seq[i] <- length(unique(dates_df[c(1:365),i]))
            }
            month_column <- month_column[which.max(check_seq)]
        }
        
        year_column <- c(1:3)[which(!(c(1:3) %in% c(day_column, month_column)))]
        
        # Check if the year column has 4 digits or 2 digits
        year_length <- max(nchar(dates_df[, year_column]))
        
        if (year_length == 4) {
            date_format <- "Y"
        } else if (year_length == 2) {
            date_format <- "y"
        } else {
            stop("Unknown date format.")
        }
        
        # Create the date format string
        date_format <- paste0("%", c("d", "m", date_format))
        date_format <- date_format[c(day_column, month_column, year_column)]
        date_format <- paste(date_format, collapse = split_char)
        
        # Convert to Date
        return(as.Date(date_vector, format = date_format))
    }
    
    # Process data based on file source
    process_data <- function(ddata) {
        ddata <- as.data.frame(ddata)
        colnames(ddata) <- tolower(colnames(ddata))
        mID <- grep("id|site", colnames(ddata))
        mdatec <- grep("date", colnames(ddata))
        mFlow <- grep("flow|value|val|00060_00003(?!.*cd)", colnames(ddata), perl = TRUE)
        msym <- grep("sym|code|flag|00060_00003_cd", colnames(ddata))
        magency <- grep("agency", colnames(ddata))
        
        if (length(mID) == 0) stop("Station ID column could not be auto-detected.")
        if (length(mFlow) == 0) stop("Flow column could not be auto-detected.")
        
        if (length(mdatec) == 0) {
            if (length(grep("^day$", tolower(colnames(ddata)))) != 0) {
                mday <- grep("^day$", tolower(colnames(ddata)))
                mmonth <- grep("^month$", tolower(colnames(ddata)))
                myr <- grep("^year$", tolower(colnames(ddata)))
                
                if (length(mmonth) == 0) {stop("Date column could not be auto-detected.")}
                if (length(myr) == 0) {stop("Date column could not be auto-detected.")}
                
                mdates <- as.Date(paste(ddata[,myr], ddata[,mmonth], ddata[,mday], sep="/"),
                                  format="%Y/%m/%d")
            } else {stop("Date column could not be auto-detected.")}
        }
        
        dates <- as.character(ddata[, mdatec])
        
        if (!grepl("/", dates[1]) & !(grepl("-", dates[1]))) {
            message('Date not in expected format.')
            return('Date not in expected format.')
        }
        
        converted_dates <- convert_dates(dates)
        ddata <- data.frame(ID = as.character(ddata[, mID]), 
                            Date = converted_dates, 
                            Flow = as.numeric(ddata[, mFlow]), 
                            SYM = ifelse(length(msym) == 0, NA, ddata[, msym]), 
                            Agency = ifelse(length(magency) == 0, NA, ddata[, magency]))
        return(ddata)
    }
    
    
    
    # File reading based on extension
    ddata <- switch(file_ext,
                    csv = read_csv_file(filename),
                    rdata = {
                        load(filename)
                        get(ls()[1])
                    },
                    rds = readRDS(filename),
                    txt = read_grdc_txt(filename),
                    stop("Unknown file type.")
    )
    
    # Processing raw data
    ddata <- process_data(ddata)
    
    # Replace negative values with NA
    ddata$Flow[ddata$Flow < 0] <- NA
    
    if (!(flow.units %in% c('m3/s', 'ft3/s'))) {
        ddata$FlowUnits <- 'unknown'
    } else {
        ddata$FlowUnits <- ifelse(flow.units == 'm3/s', 
                                  'm3/s', 'ft3/s')
    }
    
    # Convert flow units if needed
    if (!is.null(convert.to)) {
        if (flow.units != convert.to) {
            if (convert.to %in% c('m3/s', 'ft3/s')) {
                if (convert.to  == 'm3/s') {
                    ddata$Flow <- ddata$Flow / 35.3147
                    ddata$FlowUnits <- 'm3/s'
                } else if (convert.to == 'ft3/s') {
                    ddata$Flow <- ddata$Flow * 35.3147
                    ddata$FlowUnits <- 'ft3/s'
                }
            } else {
                stop('convert.to parameter must be one of: m3/s or ft3/s')
            }
        }
    }
    
    # Handle Agency information
    Station <- ddata$ID[1]
    if (is.na(ddata$Agency[1]) & (Station %in% station_metadata$StationID)) {
        ddata$Agency <- station_metadata$Agency[station_metadata$StationID == Station]
    }
    
    # Fix dropped "0" at start of USGS station names
    if (!is.na(ddata$Agency[1])) {
        if ((ddata$Agency[1] == "USGS") & 
            (!(Station %in% station_metadata$StationID)) & 
            (paste0("0", Station) %in% station_metadata$StationID)) {
            ddata$ID <- rep(paste0("0", Station), nrow(ddata))
        }
        
        # Warnings for unexpected units and agency
        if ((ddata$Agency[1] == 'USGS') & (flow.units == 'm3/s')) {
            warning('Streamflow data is from USGS, but flow.units parameter in the read.flows function was set as cubic meters per second (m3/s). Ensure streamflow units are set as intended.')
        }
        if ((ddata$Agency[1] %in% c('ROBIN', 'WSC', 'GRDC')) & (flow.units == 'ft3/s')) {
            warning(paste0('Streamflow data is from ', ddata$Agency[1], ', but flow.units parameter in the read.flows function was set as cubic feet per second (ft3/s). Ensure streamflow units are set as intended.'))
        }
    }

    
    # Pass to function that creates the hydrologic year
    ddata <- create.ts(ddata, hyrstart)
    
    return(ddata)
}
