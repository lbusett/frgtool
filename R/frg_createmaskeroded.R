#' @title frg_createmask_eroded
#' @description Accessory function to call IDL routine for Mask creation - eroded.
#' @details Function used to create a batch file (`frg_createmask_eroded_batch.pro`) which 
#'  is then used to call the `frg_createmask_eroded.pro` IDL function from a command shell.  
#'  `frg_createmask_eroded.pro` is used to create an eroded version of the binary mask 
#'  of burned areas, which allows to identify non-border (CORE) burnt pixels.
#'  See `frg_createmask_eroded.pro` in `/IDL_scripts/VI_Elaborations` for 
#'  further documentation
#' @inheritParams frg_buildroi
#' @return `character` path to the created mask
#' @rdname frg_createmask_eroded
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>

frg_createmask_eroded <- function(opts,
                                  exp_path_str, 
                                  force_update = FALSE) {
  

  # Check if mask already existing, If yes, do not recreate it unless 
  # force_update == TRUE
  
  if (!file.exists(opts$firemask_file_er) | force_update) {
    
    # Update status bar
    message("---- Creating Eroded Burnt Areas Mask File: ",
            opts$firemask_file_er, " ----")  
    
    # Build the command to run the frg_createmask_eroded IDL funtion ----
    str_idl <- paste0("res = frg_createmask_eroded(", 
                      "mask_file = '",        opts$firemask_file, "' , $ \n ", 
                      "eroded_mask_file = '", opts$firemask_file_er, "' )"
    ) 
    
    # Create the batch file needed to run the frg_createmask_eroded.pro IDL function ----
    # from a command shell
    batch_file <- file.path(opts$src_dir_idl, 
                            "/batch_files/frg_createmask_eroded_batch.pro")
    fileConn   <- file(batch_file)
    writeLines(c(exp_path_str, "envi, /restore_base_save_files  ", 
                 "ENVI_batch_init", str_idl, "exit"), fileConn)
    close(fileConn)
    
    # Execute frg_createmask_eroded.pro  ----
    # out <- system2("idl.exe", args = batch_file, stdout = T) 
    
    out <- system2("idl", args = batch_file, stdout = T) 
    
    if (!is.null(attributes(out)$status) | !file.exists(opts$firemask_file_er)) {
      stop("An error occurred while creating the burned areas eroded mask. 
            Check '/IDL/batch_files/frg_createmask_eroded_batch.pro'.
            Manually compiling and running it from IDL allows debugging ! ")
    }
  } else {
    message("- -> Eroded Mask file already existing - skipping")
  }
  return(opts$firemask_file_er)
}