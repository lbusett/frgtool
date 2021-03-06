#'@title frg_download_modis
#'@description Function used to download, mosaic and reproject MODIS data, using
#' `MODIStsp`` functionalities
#'@details The function leverages
#' functionality form `MODIStsp` package (https://github.com/lbusett/MODIStsp/)
#' **Download and Mosaicing**: 
#' Images covering Europe for DOYS 209 and 225 of each selected year are downloaded, 
#' mosaiced and reprojected in LAEA projection, with 250m pixels size
#' Resulting images are saved in TIFF format in the Originals subfolder of the 
#' output folder selected by the user to store MODIS imafes. Separate subfolders are used
#' to store images related to  NDVI, QA (Quality Assurance) and Pixel Reliability.
#' Original HDF files are stored in the "hdf" folder, while METADATA files to 
#' access the full time series in `ENVI` and `R` are stored in the `time_series`
#' subfolder
#'
#' @param opts `list` of options passed from `frg_fullprocessing()`
#' @param yy Year for which images are to be downloaded and processed (NOTE: 
#'   within `frgtool`, different years are processed in a for cycle.)
#' @return The function is called for its side effects
#' @rdname frg_download_modis
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom RJSONIO fromJSON toJSON
#' @importFrom MODIStsp MODIStsp
#' @importFrom magrittr "%>%"

frg_download_modis <- function(opts, 
                               yy) {
  
  # Update the processing year and some other options on the   ----
  # MODIStsp json file
  mstp_opts_file <- system.file("ExtData/frg_modistsp_opts_test.json",
                           package = "frgtool")
  mstp_opts                 <- RJSONIO::fromJSON(mstp_opts_file) 
  mstp_opts$start_date      <- paste(yy, 07, 15, sep = "-")
  mstp_opts$end_date        <- paste(yy, 08, 15, sep = "-")
  mstp_opts$out_folder_mod  <- file.path(opts$out_origpath, "hdfs")
  mstp_opts$out_folder      <- file.path(opts$out_origpath, "time_series")
  mstp_opts$download_server <- "http"
  mstp_opts$reprocess       <- ifelse(opts$redown == 1, "Yes", "No")
  # mstp_opts$start_x         <- 17
  # mstp_opts$end_x           <- 18
  # mstp_opts$start_y         <- 4
  # mstp_opts$end_y           <- 4
  mstp_opts$use_aria        <- TRUE
  RJSONIO::toJSON(mstp_opts) %>% 
    write(mstp_opts_file)
  # Launch MODIStsp   ----
  print(mstp_opts)
 MODIStsp(options_file = mstp_opts_file,
           gui = FALSE)
  
  message("--- Download, mosaicing and reprojection for ", yy,  "complete ! ---")
  
  return("DONE")
}