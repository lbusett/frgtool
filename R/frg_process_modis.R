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
#' @param opts    string Folder where the original and preprocessed image will be stored
#' @param UI_check   flag If = 1 then a chack on pixel usefulness index is done while
#'                   determining pixels to be used to compute average NDVI. default = 1
#' @param max_UI     Max UI value kept when averaging MODIS data. default = 5
#' @param force_update DESCRIPTION , Default: FALSE
#'
#' @author Lorenzo Busetto (2016)
#' email: lbusett@gmail.com
#'
#' @export
#' 
frg_process_modis <- function(opts, 
                              UI_check,
                              max_UI, 
                              force_update) {
  
  # Create output folder and Initialize processing variables ----
  # Print Messages ----
  message("-------------------------------------------")
  message("---- MODIS Download and PreProcessing -----")
  message("-------------------------------------------")
  message("----> Main MODIS Folder: ", opts$out_origpath)
  message("-------------------------------------------")
  message("---- Downloading, and Preprocessing of MODIS data ---> RUNNING <--- ")
  
  # Download MODIS images using MODIStsp for each selected year ------ 
  # options file is saved in inst/Ext/frg_modistsp_opts.json 
  
  for (yy in seq(opts$start_year, opts$end_year, 1)) {
    
    er <- frg_download_modis(opts, yy)
    
    # Compute yearly average values from 209 and 225 images ----
    
    message("----  Computing Average of summer values for ", yy, "-----")
    
    er <- frg_compmean(opts,
                       yy,
                       UI_check,
                       max_UI, 
                       force_update)
    
  } # End Cycle on years
  
  message("---- Downloading, and Preprocessing of MODIS data ---> COMPLETED <--- ")
  return("DONE")  # Return processing variables
  
}