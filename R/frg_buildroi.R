#' frg_buildroi
#' @description Accessory function to call IDL routine for ROI creation. 
#' @details 
#' Function used to create a batch file (`FRG_Create_ROI_batch.pro`) which 
#' is then used to call the `FRG_CREATE_ROI.pro` IDL unction from a command shell.  
#' 
#' `FRG_CREATE_ROI.pro` is used to create a ROI file starting from the shapefile
#' of burned areas. See `FRG_CREATE_ROI.pro` in `/IDL/Build_ROIS` for further 
#' documentation
#' @inheritParams frg_compSVI
#' @inheritParams frg_fullprocessing
#' @importFrom tools file_path_sans_ext
#' @return NULL
#' @export

frg_buildroi <- function(Shape_File, CLC_File_00, 
                         exp_path_str, ROI_File, 
                         force_update) {
  
  # Check if ROI already existing, If not, create it 
  
  # for debugging - set to "yes" to rebuild rois even if already existing
  if (!file.exists(ROI_File) | force_update) {
    message("---- IDL-> Creating ROI File: ", ROI_File, " Please Wait ! ----")
    
    # Build the command to run the FRG_Create_ROI.pro IDL funtion ----
    str_idl <- paste0(
      "res = FRG_Create_ROI(Shape_File = '",  Shape_File,  "' , $ \n",
      "                     CLC_00_File = '", CLC_File_00, "' , $ \n",
      "                     ROI_File = '",    ROI_File,    "')"
    )
    
    # Create the batch file needed to run the FRG_Create_ROI.pro IDL funtion ----
    # from a command shell
    
    batch_file <- file.path(FRG_Options$src_dir_idl,
                            "batch_files/FRG_Create_ROI_batch.pro")
    fileConn   <- file(batch_file)
    writeLines(c(exp_path_str, 
                 "envi, /restore_base_save_files  ", 
                 "ENVI_batch_init", 
                 str_idl, 
                 "exit"), fileConn)
    close(fileConn)
    
    # Execute FRG_Create_ROI_batch.pro  ----
    
    out <- system2("idl.exe", args = batch_file, stdout = "log_IDL")
    
    # Error message on problems in execution of FRG_Create_ROI_batch.pro
    if (!is.null(attributes(out)$status)) {
      stop("An error occurred while creating ROIs. Check '/IDL/FRG_Create_ROI_batch.pro'.
            Manually compiling and running it from IDL allows debugging ! ")
    }
  } else {
    message("---- ROI file already existing - skipping ----")
  }
  return(ROI_File)
}
