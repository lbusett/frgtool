#' @title frg_buildroi
#' @description Accessory function to call the IDL routine for ROI creation. 
#' @details Function used to create a batch file (`frg_create_ROI_batch.pro`) which 
#'  is then used to call the `frg_create_ROI.pro` IDL unction from a command shell.  
#'  `frg_create_ROI.pro` is used to create a ROI file starting from the shapefile
#'  of burned areas. See `frg_create_ROI.pro` in `/IDL/Build_ROIS` for further 
#'  documentation
#' @param opts `list` of options passed from `frg_fullprocessing()`
#' @param exp_path_str `character` string to be used to expand the IDL path
#'  in IDL batch files
#' @param force_update `logical` If TRUE, recreate the ROI file even if it already
#'  exist, default: FALSE
#' @return `character` path of the created ROI file
#' @rdname frg_buildroi
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

frg_buildroi <- function(opts, 
                         exp_path_str, 
                         force_update = FALSE) {
  
  # Check if ROI already existing, If not, create it 
  
  # for debugging - set to "yes" to rebuild rois even if already existing
  if (!file.exists(opts$roi_file) | force_update) {
    message("---- IDL-> Creating ROI File: ", opts$roi_file, " Please Wait ! ----")
    
    # Build the command to run the frg_create_ROI.pro IDL funtion ----
    str_idl <- paste0(
      "res = frg_create_ROI(shape_file    = '", opts$orig_shapefile,  "' , $ \n",
      "                     CLC_00_file   = '", opts$clc_file,       "' , $ \n",
      "                     roi_file      = '", opts$roi_file,       "')"
    )
    
    # Create the batch file needed to run the frg_create_ROI.pro IDL funtion ----
    # from a command shell
    
    batch_file <- file.path(opts$src_dir_idl,
                            "batch_files/frg_create_ROI_batch.pro")
    fileConn   <- file(batch_file)
    writeLines(c(exp_path_str, 
                 "envi, /restore_base_save_files  ", 
                 "ENVI_batch_init", 
                 str_idl, 
                 "exit"), fileConn)
    close(fileConn)
    
    # Execute frg_create_ROI_batch.pro  ----
    
    # out <- system2("idl.exe", args = batch_file)
    
    out <- system2("idl", args = batch_file)
    
    # Error message on problems in execution of frg_create_ROI_batch.pro
    if (!is.null(attributes(out)$status) | !file.exists(opts$roi_file)) {
      stop("An error occurred while creating ROIs. Check '/IDL/frg_create_ROI_batch.pro'.
            Manually compiling and running it from IDL allows debugging ! ")
    }
  } else {
    message("- -> ROI file already existing - skipping ")
  }
  return(opts$roi_file)
}
