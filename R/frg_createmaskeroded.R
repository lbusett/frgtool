#' frg_createmask
#' @description Accessory function to call IDL routine for Mask creation.
#' @details 
#' Function used to create a batch file (`FRG_CreateMask_Eroded_Batch.pro`) which 
#' is then used to call the `FRG_Create_Mask_Eroded.pro` IDL function from a command shell.  
#' 
#' `FRG_Create_Mask_Eroded.pro` is used to create an eroded version of the binary mask 
#' of burned areas, which allows to identify non-border (CORE) burnt pixels.
#' See `FRG_Create_Mask_Eroded.pro` in `/IDL/VI_Elaborations` for further documentation
#' @param ROI_file 
#' @param force_update
#' @inheritParams frg_compSVI
#' @inheritParams frg_fullprocessing
#' @importFrom tools file_path_sans_ext
#' @return NULL
#' @export

frg_createmask_eroded <- function(ROI_File, FireMask_File, exp_path_str, 
                                  FireMask_File_Eroded, 
                                  force_update) {
  

  # Check if mask already existing, If yes, do not recreate it unless 
  # force_update == TRUE
  
  if (!file.exists(FireMask_File_Eroded) | force_update) {
    
    # Update status bar
    message("---- Creating Eroded Burnt Areas Mask File: ", FireMask_File_Eroded, " ----")  
    
    # Build the command to run the FRG_Create_Mask_Eroded IDL funtion ----
    str_idl <- paste0("res = FRG_Create_Mask_Eroded(", 
                      "Mask_File = '", FireMask_File, "' , $ \n ", 
                      "Eroded_Mask_File = '", FireMask_File_Eroded, "' )"
    ) 
    
    # Create the batch file needed to run the FRG_Create_Mask_Eroded.pro IDL function ----
    # from a command shell
    batch_file <- file.path(FRG_Options$src_dir_idl, 
                            "/batch_files/FRG_CreateMask_Eroded_Batch.pro")
    fileConn   <- file(batch_file)
    writeLines(c(exp_path_str, "envi, /restore_base_save_files  ", 
                 "ENVI_batch_init", str_idl, "exit"), fileConn)
    close(fileConn)
    
    # Execute FRG_Create_Mask_Eroded.pro  ----
    out <- system2("idl.exe", args = batch_file) 
    
    if (!is.null(attributes(out)$status)) {
      stop("An error occurred while creating the burned areas eroded mask. 
            Check '/IDL/batch_files/FRG_CreateMask_Eroded_Batch.pro'.
            Manually compiling and running it from IDL allows debugging ! ")
    }
  } else {
    message("---- Eroded Mask file already existing - skipping ----")
  }
  return(FireMask_File_Eroded)
}