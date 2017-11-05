#'@title frg_compSVI 
#'@description Function used to call the IDL functiosn used for computation of 
#' SNDVI yearly images (_**frg_compute_med_SVI.pro**_)
#'@details This function is used to call the IDL function used for computation of SDVI and 
#' opts$SNDVI yearly images. It also calls the IDL routines needed to create the ENVI burnt 
#' areas ROI, and the ENVI burnt areas MASKS (both total and eroded!). 
#' Yearly opts$SRDVI (or opts$SNDVI) images are computed starting from the corresponding Yearly
#' Average mosaics. Computation of the scaled opts$indexes for each pixel is done as follows:
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
#'    opts$method ("http://www.nematrian.com/R.aspx?p=CornishFisherDerivation)
#' 5. Scaled opts$indexes for the target pixel is computed exploiting this opts$method:
#'    - 'Percentage Deviation to Kernel Median': For each pixel, the algorithm 
#'    identifies all pixels of the image corresponding to the same CLC class 
#'    contained in a window of width opts$nker then computes the median of the distribution 
#'    of values of the analyzed Vegetation opts$index in the selecteds pixels. 
#'    - Scaled opts$index for the pixel are then computed as: sVI = 100 * (VI_pix - VI_median)/(VI_median).
#'
#'(For further details, see the documentation for the IDL function _frg_compute_med_SVI_batch_)
#' @param opts `list` of options passed from `frg_fullprocessing()`
#' @param force_update `logical` If TRUE, recreate the ROI file even if it already
#'  exist, default: FALSE
#' @rdname frg_compSVI
#' @return 'DONE' if all went OK, otherwise error message to be treated by
#' 'try.catch'
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @export
#' 
frg_compSVI <- function(opts,  
                        force_update) {
  
  message("- ------------------------------------------------------ -")
  message("- Computation of Scaled Indexes                          -")
  message("- ------------------------------------------------------ -")
  
  # Initialize processing variables ----
  
  # Compute number of pixels in Kernel on the basis of the extent in KM of the kenel
  opts$nker         <- (opts$nker * 1000/250) + 1  
  # String to Expand IDL path to the folder containing the scripts to be executed
  
  exp_path_str <- paste0(
    "!PATH = Expand_Path('", "+", 
    opts$src_dir_idl, 
    "') +' ;' + !PATH"
  )  
  
  # Message window
  message("--- Checking and building ROI and Mask files ---")
  
  # Ceate a ROI file on the basis of the ORIGINAL SHAPEFILE specified -----
  # by the user and of the INPUT CLC_00_File (used to determine extent !)
  
  opts$roi_file <- frg_buildroi(opts, 
                                exp_path_str, 
                                force_update)
  
  # Create a 'Mask' envi file using the ROI File created from the burned areas ----
  # shapefile specified by the user. 
  # (The mask file is successively used to exclude burned areas from
  # computation of statistics needed for the calculation of the scaled
  # opts$indexes.
  
  frg_createmask(opts, 
                 exp_path_str,
                 force_update)
  
  # Create an ERODED 'Mask' envi file the ROI File created from the burned areas ----
  # shapefile specified by the user. The mask file is successively used to determine
  # which of the ROI pixels are CORE pixels (i.e., not on the borders)
  
  frg_createmask_eroded(opts,
                        exp_path_str,
                        force_update)
  
  
  out_files    <- NULL  
  # Set Folder containing the Mean yearly VI images
  in_avg_dir   <- file.path(opts$mod_dir, "Originals", opts$index, "Averages")  
  
  # Get List of ENVI header files    
  in_avg_files <- list.files(in_avg_dir, pattern = "*.tif$")
  
  # Start Cycling on selected years ----
  
  for (yy in opts$start_year:opts$end_year) {
    
    # Find average file for the selected year
    in_avg_file   <- file.path(in_avg_dir, in_avg_files[grep(yy, in_avg_files)])
    
    # ---- Define output folder and file name ----
    
    out_dir  <- file.path(opts$scaled_dir, "Med_SNDVI", "Yearly_Images",yy) 
    out_file <- file.path(out_dir, paste0("Med_SNDVI_", yy))
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)        
    
    # If output file for selected year and opts$index doesn't exist, or ReProc =
    # Yes, start the processing
    if (file.exists(out_file) == FALSE | force_update) {
      
      # Update status bar
      message("---- Computing Med_SNDVI for year ", yy," ----")
      
      # Build string to call the FRG_Compute_MedScaled_VI.pro IDL script ----
      str_idl <- paste0("res = frg_compute_med_SVI(", 
                        "CLC_file_00 = '",   opts$clc_file,   "' , $ \n",
                        "in_file = '",       in_avg_file,   "' , $ \n",
                        "firemask_file = '", opts$firemask_file, "' , $ \n",
                        "out_file = '",      out_file,      "' , $ \n",
                        "nodata_out = '",    opts$nodata_out,    "' , $ \n",
                        "n_ker = '",         opts$nker,          "' , $ \n", 
                        "index = '",         opts$index,         "' , $ \n", 
                        "year = '",          yy,            "' )"
      )
      
      # Build an IDL batch file using the string defined above ----
      # browser()  
      batch_file <- file.path(opts$src_dir_idl, 
                              "/batch_files/frg_compute_med_SVI_batch.pro")
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
      
      if (!is.null(attributes(out)$status) | !file.exists(out_file)) {
        stop("An error occurred while computing scaled opts$indexes ! Processing stopped. 
                Check 'frg_compute_med_SVI_batch.pro'. Manually compiling and running
                it from IDL allows debugging ! ")
      } else {
        # Update list of available sVI files to be used for META file creation
        out_files <- c(out_files, out_file)  
      }
      
    } else {
      out_files <- c(out_files, out_file) 
      message("- -> Scaled VI file already existing for year ", yy, " - skipping")
    } # End of 'if-else' condition on file existences
    
  } # End of Cycle on Years
  
  # End of 'if'-else' condition on file existence' on opts$method
  
  # Write the ENVI META text file ----
  # It lately allow to access the time series of SVI files as a single time 
  # series. 
  
  frg_createmeta(opts, 
                 out_files, 
                 force_update)
  
  # End cycle opts$SNDVI vs opts$SRDVI
  
  return("DONE")  
}