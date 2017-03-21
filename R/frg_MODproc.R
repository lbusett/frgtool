#' frg_modproc
#'@description Function used to download, mosaic and preprocess MODIS data
#'@details Function used to download, mosaic and preprocess MODIS data.
#'1. **Download and Mosaicing**:
#'               Images covering Europe for end July - mid august  of each selected year are downloaded, mosaiced and reprojected
#'               Resulting images are saved in TIFF format in the Originals subfolder of the output folder selected by the user. Separate subfolders are used\cr
#'               to store images related to  NDVI, RED and NIR reflectance, QA (Quality Assurance) and Pixel Reliability.
#'2. **Preprocessing**:
#'               The preprocessing steps conducted are the following:
#'               a. MODIS UI (Usefulness Index) values are extracted from the MODIS QA (Quality Assurance) files
#'               c. The average yearly values for DVI and NDVI are computed starting from the images of DOY 209 and 225. NODATA and bad quality data\cr
#'                   are automatically removed
#' @param MOD_Dir string Folder where the original and preprocessed image will be stored
#' @param Start_Year numeric Starting year of the analysis
#' @param End_Year numeric Ending Year of the analysis
#' @param ReProcIm numeric if = 1, already existing MODIS mosaics will be reprocessed
#' @param ReDown numeric if = 1, MODIS images needed to create already existing mosaic files will be redownloaded, and
#'               already existing mosaics will be overwritten
#' @param max_UI Max UI value kept when averaging MODIS data. default = 5
#' @param UI_check 

#' @author Lorenzo Busetto (2016)
#' email: lbusett@gmail.com
#'
#' @export

frg_modproc <- function(MOD_Dir, 
                        Start_Year, 
                        End_Year, 
                        ReProcIm, 
                        ReDown, 
                        UI_check = TRUE, 
                        max_UI = 5) {
  # Prevent MAIN_GUI from closing
  addHandlerUnrealize(Main_GUI, handler = function(h, ...) {return(TRUE)})  
  
  # Create output folder and Initialize processing variables
  dir.create(MOD_Dir, recursive = TRUE, showWarnings = FALSE)
  OutOrig_Path <- file.path(MOD_Dir, "Originals")
  
  # Messages ----
  message("----------------------------------------------------------")
  message("---------- MODIS Download and PreProcessing --------------")
  message("----------------------------------------------------------")
  message("-> Main MODIS Folder: ", MOD_Dir)
  message("--------------------------------------------------------")
  message("--- Downloading, Mosaicing and Reprojecting Files ---> RUNNING <--- ")
  
  # Download MODIS images using MODIStsp - oprions file is saved in
  # inst/Ext/frg_modistsp_opts.json -----
  
  er <- frg_moddownload(OutOrig_Path = OutOrig_Path, 
                        ReDown       = ReDown, 
                        yy           = yy)
  
  message("--- Downloading, Mosaicing and Reprojecting Files ---> DONE <--- ")
  
  # Compute yearly average values from 209 and 225 images ----
  message("----------- Computing averages of summer values --------- ")
  message("  ---  Computing Mean Indexes --> ONGOING <---")
  nodata_in <- FRG_Options$No_Data_Out_Rast
  er <- (FRG_MOD_Comp_MeanInd(OutOrig_Path = OutOrig_Path, ReProc = ReProcIm, 
                              yy = yy, UI_check = UI_check, max_UI = max_UI, nodata_in = nodata_in))
  message("  ---  Computing Mean Indexes --> DONE <---")
  
  # End Cylcle on years
  
  print(" ---- Download and Preprocessing Complete ! ---- ")
  
  return("DONE")  # Return processing variables
  
}
