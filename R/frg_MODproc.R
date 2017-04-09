#' frg_modproc
#'@description Function used to download, mosaic and preprocess MODIS data
#'@details Function used to download, mosaic and preprocess MODIS data.
#'1. **Download and Mosaicing**:
#'   Images covering Europe for end July - mid august  of each selected
#'   year are downloaded, mosaiced and reprojected
#'   Resulting images are saved in TIFF format in the Originals subfolder 
#'   of the output folder selected by the user. Separate subfolders are used
#'   to store images related to  NDVI, RED and NIR reflectance, QA 
#'   (Quality Assurance) and Pixel Reliability.
#'2. **Preprocessing**:
#'   The preprocessing steps conducted are the following:
#'   a. MODIS UI (Usefulness Index) values are extracted from the MODIS QA 
#'   (Quality Assurance) files
#'   c. The average yearly values for DVI and NDVI are computed starting from the
#'   images of DOY 209 and 225. NODATA and bad quality data#'   are automatically removed
#'                   
#' @param MOD_Dir    string Folder where the original and preprocessed image will be stored
#' @param Start_Year numeric Starting year of the analysis
#' @param End_Year   numeric Ending Year of the analysis
#' @param ReProcIm   numeric if = 1, already existing MODIS mosaics will be reprocessed
#' @param ReDown     numeric if = 1, MODIS images needed to create already existing mosaic 
#'                   files will be redownloaded, and already existing mosaics will be overwritten
#' @param UI_check   flag If = 1 then a chack on pixel usefulness index is done while
#'                   determining pixels to be used to compute average NDVI. default = 1
#' @param max_UI     Max UI value kept when averaging MODIS data. default = 5
#'
#' @author Lorenzo Busetto (2016)
#' email: lbusett@gmail.com
#'
#' @export
#' 
frg_modproc <- function(OutOrig_Path, 
                        Start_Year, 
                        End_Year, 
                        ReProcIm, 
                        ReDown, 
                        UI_check = TRUE, 
                        max_UI = 5) {
  
  # Create output folder and Initialize processing variables ----
  # Print Messages ----
  message("-------------------------------------------")
  message("---- MODIS Download and PreProcessing -----")
  message("-------------------------------------------")
  message("----> Main MODIS Folder: ", OutOrig_Path)
  message("-------------------------------------------")
  message("---- Downloading, and Preprocessing of MODIS data ---> RUNNING <--- ")
  
  # Download MODIS images using MODIStsp for each selected year ------ 
  # options file is saved in inst/Ext/frg_modistsp_opts.json 
  
  for (yy in seq(Start_Year, End_Year, 1)) {
    
    er <- frg_moddownload(OutOrig_Path = OutOrig_Path, 
                          ReDown       = ReDown, 
                          yy           = yy)
    
    # Compute yearly average values from 209 and 225 images ----
    
    message("----  Computing Average of summer values for ", yy, "-----")
    
    er <- frg_compmean(OutOrig_Path, ReProcIm, yy, UI_check, max_UI)
    
  } # End Cycle on years
  
  message("---- Downloading, and Preprocessing of MODIS data ---> COMPLETED <--- ")
  return("DONE")  # Return processing variables
  
}