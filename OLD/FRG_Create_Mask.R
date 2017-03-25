#' frg_createmask
#' @description Accessory function for Mask creation. Calls the FRG_CREATE_MASK.pro
#' @details 
#' @inheritParams frg_comp_SVI
#' @inheritParams frg_full_processing
#' @param ROI_File 
#' @param CLC_File_00 
#' @param exp_path_str 
#' @param Intermediate_Folder 
#'
#' @return
#' @export
#'
#' @examples
frg_createmask <- function(ROI_File, CLC_File_00, 
                            exp_path_str, Intermediate_Folder) {
  
  dir.create(file.path(Intermediate_Folder, "ENVI_Mask"), showWarnings = FALSE, 
             recursive = TRUE)
  FireMask_File <- file.path(Intermediate_Folder, "ENVI_Mask", paste(sub("[.][^.]*$", 
                                                                         "", basename(ROI_File)), "_ENVI_Mask", sep = ""))
  selection <- "no"
  # if (file.exists(FireMask_File)) { selection = tk_messageBox(caption =
  # 'Overwrite Warning', type = c ('yesno'), message = 'A Mask file for
  # the selected ROI file already exists !\n Do you want to overwrite it
  # ? ', default = 'no')}
  if (!file.exists(FireMask_File) | selection == "yes") {
    
    print(paste("--- Creating Burnt Areas Mask File: ", FireMask_File, 
                " ---"))  # Update status bar
    str_idl <- paste("res = FRG_Create_Mask(", "ROI_File = '", ROI_File, 
                     "' , ", "CLC_00_File = '", CLC_File_00, "' , ", "Mask_File= '", 
                     FireMask_File, "' )", sep = "")
    batch_file <- file.path(FRG_Options$IDL_Dir, "FRG_CreateMask_Batch.pro")
    fileConn <- file(batch_file)
    writeLines(c(exp_path_str, "envi, /restore_base_save_files  ", 
                 "ENVI_batch_init", str_idl, "exit"), fileConn)
    close(fileConn)
    out <- system((paste(FRG_Options$idl_exe, batch_file, sep = " ")), 
                  invisible = TRUE, show.output.on.console = TRUE)  # Launch computation in IDL 
    if (out != 0) {
      print("An error occurred while creating Envi ROIS mask  ! Processing stopped")
      stop()
    }
  }
  return(FireMask_File)
}