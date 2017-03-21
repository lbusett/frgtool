#'@title FRG_MOD_Proc
#'@description Function used to download, mosaic and preprocess MODIS data
#'@details Function used to download, mosaic and preprocess MODIS data. \cr
#' 	\itemize{
#' 		\item Download and Mosaicing: \cr
#'               Images covering Europe for end July - mid august  of each selected year are downloaded, mosaiced and reprojected \cr
#'               Resulting images are saved in TIFF format in the Originals subfolder of the output folder selected by the user. Separate subfolders are used\cr
#'               to store images related to  NDVI, RED and NIR reflectance, QA (Quality Assurance) and Pixel Reliability.
#'		\item Preprocessing: \cr
#'               The preprocessing steps conducted are the following: \cr
#'               1) MODIS UI (Usefulness Index) values are extracted from the MODIS QA (Quality Assurance) files
#'               2) The DVI (Difference Vegetation Index) is computed for each date starting from the RED and NIR MODIS images
#'               3) The average yearly values for DVI and NDVI are computed starting from the images of DOY 209 and 225. NODATA and bad quality data\cr
#'                   are automatically removed
#'}
#' @param MOD_Dir string Folder where the original and preprocessed image will be stored
#' @param Start_Year numeric Starting year of the analysis
#' @param End_Year numeric Ending Year of the analysis
#' @param ReProcIm numeric if = 1, already existing MODIS mosaics will be reprocessed
#' @param ReDown numeric if = 1, MODIS images needed to create already existing mosaic files will be redownloaded, and
#' 				 already existing mosaics will be overwritten
#' @param UI_Check flag If 1 , then check on usefulness index is performed when averaging MODIS data. default = 1
#' @param max_UI Max UI value kept when averaging MODIS data. default = 5
#'
#' @return
#'
#'
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Oct 29, 2012
#' @export

FRG_MOD_Proc = function(MOD_Dir= MOD_Dir, Start_Year= Start_Year, End_Year= End_Year,
                        ReProcIm = ReProcIm, ReDown = ReDown,UI_check = 1,max_UI = 5) {


  addHandlerUnrealize(Main_GUI, handler = function(h,...) {return(TRUE)})			# Prevent MAIN_GUI from closing
  # Initialize processing variables
  OutOrig_Path = file.path(MOD_Dir,'Originals')

  # ---------------------------------- #
  # Start Cycle on selected years
  # ---------------------------------- #
  for (yy in Start_Year:End_Year) {

    print('--------------------------------------------------------')
    print(paste('MODIS Processing - Year ',yy,'Completion Status'))
    print('--------------------------------------------------------')
    print( '--- Downloading, Mosaicing and Reprojecting Files ---> ONGOING <--- ')
    #browser()
    # ---------------------------------- #
    # Download MODIS images
    # ---------------------------------- #
    # Create string representing the dates to be processed (From 15 July to 15 August of the year - DOYS composite 209 and 225)
    dates = c(paste(as.character(yy),'.07.15'),paste(as.character(yy),'.08.15'))
    er = (FRG_MOD_Download(dates=dates,OutOrig_Path = OutOrig_Path,  ReDown = ReDown, yy = yy))		# Download the images for the year

    # ---------------------------------- #
    # Pre Process MODIS images
    # ---------------------------------- #
    print( '--- Downloading, Mosaicing and Reprojecting Files ---> DONE <--- ')

    # Call accessory function to compute UI from QA bit fields images
    print( '  ---  Computing UI from QA data --> ONGOING <---')
    er = (FRG_MOD_Comp_UI(OutOrig_Path = OutOrig_Path, ReProc = ReProcIm, yy = yy))		#	Convert the MODIS QA bit field s to Quality Assurance and Usefulness Index values (IDL)
    print( '  ---  Computing UI from QA data --> DONE <---')

    print( '  ---  Computing RDVI --> ONGOING <---')
    er = (FRG_MOD_Comp_RDVI (OutOrig_Path= OutOrig_Path, ReProc = ReProcIm, yy = yy))			#	Compute DVI from RED and NIR data
    print( '  ---  Computing RDVI --> DONE <---')

    # Compute yearly average values from 209 and 225 images
    print( '  ---  Computing Mean Indexes --> ONGOING <---')
    nodata_in = FRG_Options$No_Data_Out_Rast
    er <- (FRG_MOD_Comp_MeanInd (OutOrig_Path= OutOrig_Path, ReProc = ReProcIm, yy = yy,UI_check = UI_check,
                                 max_UI = max_UI, nodata_in = nodata_in))
    print( '  ---  Computing Mean Indexes --> DONE <---')

  }	# End Cylcle on years

  print(' ---- Download and Preprocessing Complete ! ---- ')

  return ('DONE')				# Return processing variables

}
