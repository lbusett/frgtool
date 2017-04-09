#'frg_moddownload
#'@description Function used to download, mosaic and reproject MODIS data
#'@details The function leverages
#' functionality form `MODIStsp` package [https://github.com/lbusett/MODIStsp/]
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
#' @param OutOrig_Path string Main folder where the original MODIS mosaics 
#'     (i.e., one pan european image for each data and data type) will be stored
#' @param ReProc Flag. If = 1, existing preporocessed images will be recomputed 
#' and overwritten
#' @param yy Year for which images are to be downloaded and processed
#'
#' @return NULL
#' @author Lorenzo Busetto, PhD (2017) - email lbusett@gmail.com
#' @import MODIStsp 
#' @import RJSONIO
#' @importFrom magrittr %>% 
#' @export

frg_moddownload <- function(OutOrig_Path, 
                            ReDown, 
                            yy) {
  
  # Update the processing year and some other options on the   ----
  # MODIStsp json file
  
  opts                 <- fromJSON("inst/ExtData/frg_modistsp_opts_test.json") 
  opts$start_date      <- paste(yy, 07, 15, sep = "-")
  opts$end_date        <- paste(yy, 08, 15, sep = "-")
  opts$end_date        <- paste(yy, 08, 15, sep = "-")
  opts$out_folder_mod  <- file.path(OutOrig_Path, "hdfs")
  opts$out_folder      <- file.path(OutOrig_Path, "time_series")
  opts$download_server <- "http"
  opts$reprocess       <- ifelse(ReDown == 1, "Yes", "No")
  toJSON(opts) %>% 
    write("inst/ExtData/frg_modistsp_opts_test.json")
  
  # Launch MODIStsp   ----
  MODIStsp(options_file = "inst/ExtData/frg_modistsp_opts_test.json", gui = FALSE)
  
  message("--- Download, mosaicing and reprojection for ", yy,  "complete ! ---")
  
  return("DONE")
}