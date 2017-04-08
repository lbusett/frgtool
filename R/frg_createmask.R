#' frg_createmask
#' @description Accessory function to call IDL routine for Mask creation.
#' @details 
#' Function used to create a batch file (`FRG_CreateMask_Batch.pro`) which 
#' is then used to call the `FRG_Create_Mask.pro` IDL function from a command shell.  
#' 
#' `FRG_Create_Mask.pro` is used to create a binary mask to exclude burned areas
#' from statistics computaitons. See `FRG_Create_Mask.pro` in `/IDL/VI_Elaborations`
#' for further documentation
#' @param ROI_file 
#' @inheritParams frg_compSVI
#' @inheritParams frg_fullprocessing
#' @importFrom tools file_path_sans_ext
#' @return NULL
#' @export

frg_createmask <- function(ROI_File, CLC_File_00, 
                           exp_path_str, FireMask_File) {
  
  
  # Check if mask already existing, If not, create it 
  selection <- "no"  # for debugging - set to "yes" to rebuild mask even if already existing
  
  if (!file.exists(FireMask_File) | selection == "yes") {
    
    message("---- IDL-> Creating Burnt Areas Mask File: ", FireMask_File, " ----") 
    
    # Build the command to run the FRG_Create_Mask.pro IDL funtion ----
    
    str_idl <- paste0(
      "res = FRG_Create_Mask(ROI_File = '",   ROI_File,      "' , $ \n",
      "                     CLC_00_File = '", CLC_File_00,   "' , $ \n",
      "                     Mask_File = '",   FireMask_File, "')"
    )
    
    # Create the batch file needed to run the FRG_Create_Mask.pro IDL funtion ----
    # from a command shell
    
    batch_file <- file.path(FRG_Options$src_dir_idl, "/batch_files/FRG_CreateMask_Batch.pro")
    fileConn   <- file(batch_file)
    writeLines(c(exp_path_str, 
                 "envi, /restore_base_save_files  ", 
                 "ENVI_batch_init", str_idl, 
                 "exit"), fileConn)
    close(fileConn)
    
    # Execute FRG_Create_Mask.pro  ----
    out <- system2("idl.exe", args = batch_file)
    
    if (!is.null(attributes(out)$status)) {
      stop("An error occurred while creating the burned areas mask. 
            Check '/IDL/batch_files/FRG_CreateMask_Batch.pro'.
            Manually compiling and running it from IDL allows debugging ! ")
    }
  } else {
    message("---- Mask file already existing - skipping ----")
  }
  return(FireMask_File)
}