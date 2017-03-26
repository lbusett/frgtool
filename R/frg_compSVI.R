#' frg_compSVI 
#'@description Function used to call the IDL functiosn used for computation of 
#'             SRDVI and SNDVI yearly images (_**FRG_Compute_MedScaled_VI.pro**_)
#'@details 
#' This function is used to call the IDL function used for computation of SDVI and 
#' SNDVI yearly images. It also calls the IDL routines needed to create the ENVI burnt 
#' areas ROI, and the ENVI burnt areas MASKS (both total and eroded!). 
#' Yearly SRDVI (or SNDVI) images are computed starting from the corresponding Yearly
#' Average mosaics. Computation of the scaled indexes for each pixel is done as follows:
#' 1. The pixels belonging to the same class of the target included in a window of
#'   width specified by the user centred on the target are identified on the 
#'   basis of the CLC00 recoded map;
#' 2. Pixels showing NODATA values in the input VI image, or corresponding to areas 
#'    classified as 'burned' on the input ROI files are removed from the above selection
#' 3. Remaining pixels (i.e., same class, unburned and with good data) are used 
#'    to compute the MOMENTS of the distribution of VI values of the target 
#'    CLC class in the window centred on the target pixel
#' 4. Computed MOMENTS are use to determine an ESTIMATE of the 5th, 50th (median) 
#'    95th percentile of the distribution, using the Cornish-Fisher fourth-order Expansion 
#'    method (["http://www.nematrian.com/R.aspx?p=CornishFisherDerivation])
#' 5. Scaled indexes for the target pixel is computed exploiting this method:
#'    - 'Percentage Deviation to Kernel Median': For each pixel, the algorithm 
#'    identifies all pixels of the image corresponding to the same CLC class 
#'    contained in a window of width NKer then computes the median of the distribution 
#'    of values of the analyzed Vegetation Index in the selecteds pixels. 
#'    - Scaled Index for the pixel are then computed as: sVI = 100 * (VI_pix - VI_median)/(VI_median).
#'
#'(For further details, see the documentation for the IDL function _FRG_Compute_MedScaled_VI.pro_)
#' @inheritParams frg_fullprocessing 
#' @param  Intermediate_Folder string Folder for storing intermediate processing results
#' @return 'DONE' if all went OK, otherwise error message to be treated by 'try.catch'
#' @author Lorenzo Busetto (2016)
#'         email: lbusett@8gmail.com
#' @export
frg_compSVI <- function(MOD_Dir, Shape_File, CLC_File_00, Scaled_Folder, Start_Year, 
                        End_Year, NKer, Method, SRDVI, SNDVI, nodata_out, ReProcIm, 
                        Intermediate_Folder) {
  
  message("----------------------------------------------------------")
  message("------------- Computation of Scaled Indexes --------------")
  message("----------------------------------------------------------")
  message(c("-> Scaled Indexes Output Folder: ", Scaled_Folder))
  
  # Initialize processing variables ----
  
  
  # Compute number of pixels in Kernel on the basis of the extent in KM of the kenel
  NKer         <- (NKer * 1000/250) + 1  
  # String to Expand IDL path to the folder containing the scripts to be executed
  exp_path_str <- paste0(
    "!PATH = Expand_Path('", "+", 
    FRG_Options$IDL_Dir, 
    "') +' ;' + !PATH"
  )  
  
  # Message window
  message("--- Checking and building ROI and Mask files ---")
  
  # Ceate a ROI file on the basis of the ORIGINAL SHAPEFILE specified -----
  # by the user and of the INPUT CLC_00_File (used to determine extent !)
  
  ROI_File <- frg_buildroi(Shape_File, 
                           CLC_File_00, 
                           exp_path_str, 
                           ROI_file)
  
  # Create a 'Mask' envi file using the ROI File created from the burned areas ----
  # shapefile specified by the user. 
  # (The mask file is successively used to exclude burned areas from
  # computation of statistics needed for the calculation of the scaled
  # indexes.
  
  FireMask_File <- frg_createmask(ROI_File, 
                                  CLC_File_00, 
                                  exp_path_str,
                                  Intermediate_Folder, 
                                  FireMask_File)
  
  # Create an ERODED 'Mask' envi file the ROI File created from the burned areas ----
  # shapefile specified by the user. The mask file is successively used to determine
  # which of the ROI pixels are CORE pixels (i.e., not on the borders)
  
  FireMask_File_Eroded <- frg_createmask_eroded(ROI_File, 
                                                FireMask_File, 
                                                exp_path_str, 
                                                FireMask_File_Eroded)
  
  
  out_files    <- NULL  
  in_avg_dir   <- file.path(MOD_Dir, "Originals", Index, "Averages")  # Folder containing the Mean yearly VI images
  in_avg_files <- list.files(in_avg_dir, pattern = "*.tif$")         # Get List of ENVI header files    
  
  # Start Cycling on selected years ----
  
  for (yy in Start_Year:End_Year) {
    
    # Find average file for the selected year
    in_avg_file   <- file.path(in_avg_dir, in_avg_files[grep(yy, in_avg_files)])
    
    # ---- Define output folder and file name ----
    
    out_dir  <- file.path(Scaled_Folder, "Med_SNDVI", "Yearly_Images",yy) 
    out_file <- file.path(out_dir, paste0("Med_SNDVI_", yy))
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)        
    
    # If output file for selected year and index doesn't exist, or ReProc =
    # Yes, start the processing
    if (file.exists(out_file) == FALSE | ReProcIm == 1) {
      
      # Update status bar
      message("---- Computing Med_SNDVI for year ", yy," ----")
      
      # Build string to call the FRG_Compute_MedScaled_VI.pro IDL script ----
      str_idl <- paste0("res = FRG_Compute_MedScaled_VI(", 
                        "CLC_File_00 = '",  CLC_File_00,   "' , $ \n",
                        "In_File = '",      in_avg_file,   "' , $ \n",
                        "FireMask_File= '", FireMask_File, "' , $ \n",
                        "Out_File = '",     out_file,      "' , $ \n",
                        "nodata_out = '",   nodata_out,    "' , $ \n",
                        "N_Ker = '",        NKer,          "' , $ \n", 
                        "Index = '",        Index,         "' , $ \n", 
                        "Year = '",         yy,            "' )"
      )
      
      # Build an IDL batch file using the string defined above ----
      
      batch_file <- file.path(FRG_Options$src_dir_idl, 
                              "/batch_files/FRG_Compute_Med_SVI_batch.pro")
      fileConn <- file(batch_file)
      writeLines(c(exp_path_str, 
                   "envi, /restore_base_save_files  ", 
                   "ENVI_batch_init", 
                   str_idl
                   , "exit"), 
                 fileConn)
      close(fileConn)
      
      # Launch computation in IDL ----
      out <- system2("idl.exe", args = batch_file)  
      if (!is.null(attributes(out)$status)) {
        stop("An error occurred while computing scaled indexes ! Processing stopped. 
                Check 'FRG_Compute_Med_SVI_batch.pro'. Manually compiling and running
                it from IDL allows debugging ! ")
      }
      
      if (file.exists(out_file) == TRUE) {
        out_files <- c(out_files, out_file)  # Update list of available sVI files to be used for META file creation
      } else {
        stop("An error occurred while computing scaled indexes ! Processing stopped. 
                Check 'FRG_Compute_Med_SVI_batch.pro'. Manually compiling and running
                it from IDL allows debugging ! ")
      }
    } else {
      out_files <- c(out_files, out_file) 
      message("---- Scaled VI file already existing for year ", yy, " - skipping ----")
    } # End of 'if-else' condition on file existence
    
  } # End of Cycle on Years
  
  # End of 'if'-else' condition on file existence' on method
  
  # Write the ENVI META text file ----
  # It lately allow to access the time series of SVI files as a single time 
  # series. 
  
  frg_createmeta(Index, Start_Year, End_Year, 
                 Method, Scaled_Folder, 
                 out_files)
  
  # End cycle SNDVI vs SRDVI
  
  return("DONE")  
}