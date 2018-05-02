#' @title frg_createmask
#' @description Accessory function to call IDL routine for Mask creation.
#' @details Function used to create a batch file (`frg_createmask_batch.pro`) which 
#'  is then used to call the `frg_createmask.pro` IDL function from a command shell.  
#'  `frg_createmask.pro` is used to create a binary mask to exclude burned areas
#'  from statistics computaitons. See `frg_createmask.pro` in `/IDL/VI_Elaborations`
#'  for further documentation
#' @inheritParams frg_buildroi
#' @return `character` path to the created mask
#' @rdname frg_createmask
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
frg_createmask <- function(opts, 
                           exp_path_str,
                           force_update = FALSE) {

  # Check if mask already existing, If yes, do not recreate it unless
  # force_update == T
  
  if (!file.exists(opts$firemask_file) | force_update) {
    
    message("---- IDL-> Creating Burnt Areas Mask File: ", opts$firemask_file, " ----") 
    
    # Build the command to run the frg_createmask.pro IDL funtion ----
    
    str_idl <- paste0(
      "res = frg_createmask(roi_file = '",    opts$roi_file,      "' , $ \n",
      "                     CLC_00_file = '", opts$clc_file,      "' , $ \n",
      "                     mask_file = '",   opts$firemask_file, "')"
    )
    
    # Create the batch file needed to run the frg_createmask.pro IDL funtion ----
    # from a command shell
    
    batch_file <- file.path(opts$src_dir_idl, "/batch_files/frg_createmask_batch.pro")
    fileConn   <- file(batch_file)
    writeLines(c(exp_path_str, 
                 "envi, /restore_base_save_files  ", 
                 "ENVI_batch_init", str_idl, 
                 "exit"), fileConn)
    close(fileConn)
    
    # Execute frg_createmask.pro  ----
    # out <- system2("idl.exe", args = batch_file)
    
    out <- system2("idl", args = batch_file)
    
    if (!is.null(attributes(out)$status)) {
      stop("An error occurred while creating the burned areas mask. 
            Check '/IDL/batch_files/frg_createmask_batch.pro'.
            Manually compiling and running it from IDL allows debugging ! ")
    }
  } else {
    message("- -> Mask file already existing - skipping")
  }
  
}