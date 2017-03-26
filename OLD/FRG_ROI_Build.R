
#' frg_buildroi_old
#' @description # - Accessory function for ROI creation. Calls the FRG_CREATE_ROI.pro
# IDL function. See it for further documentation
#' @inheritParams frg_compSVI
#' @param Shape_File 
#' @param CLC_File_00 
#' @param exp_path_str 
#' @param Intermediate_Folder 
#'
#' @return
#' @export
#'
#' @examples
frg_buildroi_old <- function(Shape_File = Shape_File, CLC_File_00 = CLC_File_00, 
                          exp_path_str = exp_path_str, Intermediate_Folder = Intermediate_Folder) {
  
  dir.create(file.path(Intermediate_Folder, "ENVI_ROI"), showWarnings = FALSE, 
             recursive = TRUE)  # Create folder for ROI storing if needed
  ROI_File <- file.path(Intermediate_Folder, "ENVI_ROI", paste(sub("[.][^.]*$", 
                                                                   "", basename(Shape_File)), ".ROI", sep = ""))  # Define ROI file name
  selection <- "no"
  
  # If ROI already existing, ask user if he wants to re-create it !  if
  # (file.exists(ROI_File)) { selection = tk_messageBox(caption =
  # 'Overwrite Warning', type = c ('yesno'), message = 'A ROI file with
  # appropriate file name already exists !\n Do you want to re-build it
  # ?\n(Needed only if you made changes on the shape file,or on the
  # input files extent !) ', default = 'no')}
  if (!file.exists(ROI_File) | selection == "yes") {
    print(paste("--- Creating ROI File: ", ROI_File, " ---"))  # Update status bar
    str_idl <- paste("res = FRG_Create_ROI(", "Shape_File = '", Shape_File, 
                     "' , ", "CLC_00_File = '", CLC_File_00, "' , ", "ROI_File = '", 
                     ROI_File, "' )", sep = "")
    batch_file <- file.path(FRG_Options$idl_exe_dir, "FRG_Create_ROI_batch.pro")
    fileConn <- file(batch_file)
    writeLines(c(exp_path_str, "envi, /restore_base_save_files  ", 
                 "ENVI_batch_init", str_idl, "exit"), fileConn)
    close(fileConn)
    out <- system((paste(FRG_Options$idl_exe, batch_file, sep = " ")), 
                  invisible = TRUE, show.output.on.console = TRUE)  # Launch ROI creation in IDL 
    if (out != 0) {
      print("An error occurred while creating ROIs  ! Processing stopped")
      stop()
    }
  }
  return(ROI_File)
}
