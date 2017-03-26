#' frg_def_files
#' @description 
#  Helper function used to define required file names and folders
#' @param Out_Dir 
#' @param Start_Year 
#' @param End_Year 
#' @param MOD_Dir 
#' @param Shape_File 
#'
#' @return ff list contains all main file names and folders
#' @export

frg_def_files <- function(Out_Dir, Start_Year, End_Year, MOD_Dir, Shape_File) {
  
  ff <- list()
  
  # Set Main Results Dir
  Out_Dir <- file.path(Out_Dir, paste("Results", Start_Year, End_Year, 
                                as.character(Sys.Date()), 
                                sep = "_"))
  ff[["Out_Dir"]]      <- file.path(Out_Dir)
  ff[["OutFile_Conn"]] <- file.path(Out_Dir, paste("Process_Summary", ".txt", sep = ""))  # Set Processing log file
  ff[["Intermed_Dir"]] <- file.path(Out_Dir, "Intermed_Proc")  # Set Dir for storing intermediate processing results
  ff[["Summary_Dir"]]  <- file.path(Out_Dir, "Summaries_for_EFFIS")  # Set Dir for storing EFFIS summaries results
  ff[["Scaled_Dir"]]   <- file.path(Out_Dir, "Scaled_Indexes")  # Define Dir were Scaled Indexes time series will be saved
  ff[["OutOrig_Path"]] <- file.path(MOD_Dir, "Originals")
  
  ff[["ROI_File"]]     <- file.path(ff$Intermed_Dir, "ENVI_ROI", 
                     paste0(file_path_sans_ext(basename(Shape_File)), ".ROI"))
  
  ff[["FireMask_File"]] <- file.path(ff$Intermed_Dir, "ENVI_Mask", # Define Mask file name and create Dir for mask storing if needed ----
                                  paste0(file_path_sans_ext(basename(ff$ROI_File)), 
                                         "_ENVI_Mask"))
  
  ff[["FireMask_File_Er"]] <- file.path(ff$Intermed_Dir, "ENVI_Mask",  # Define Mask file name and create Dir for mask storing if needed ----
                                  paste0(file_path_sans_ext(basename(ff$ROI_File)), 
                                         "_ENVI_Mask_Eroded"))
  
  ff[["out_dir_extrsvi"]]     <- file.path(ff$Out_Dir, "Med_SNDVI", "TS_Extraction")
  
  ff[["ExtTS_File_Single"]]   <- file.path(ff$out_dir_extrsvi, 
                                        paste0("Med_SNDVI_ts_single"))
  ff[["ExtTS_File_Multiple"]] <- file.path(ff$out_dir_extrsvi, 
                                        paste0("Med_SNDVI_ts_multiple"))
  
  ff[["out_dir_stats"]] <- file.path(ff$Out_Dir, "Med_SNDVI", "Stat_Analysis")
  
  ff[["TS_filename"]] <- file.path(ff$Scaled_Dir, "Med_SNDVI", 
                                   paste0("Med_SNDVI", "_", Start_Year, "_", End_Year, "_META")) 
  
  
  # Create all required Dirs (if needed) ----
  
  dir.create(ff$Scaled_Dir,                recursive = TRUE, showWarnings = FALSE)
  dir.create(ff$Intermed_Dir,              recursive = TRUE, showWarnings = FALSE)
  dir.create(ff$Summary_Dir,               recursive = TRUE, showWarnings = FALSE)
  dir.create(ff$OutOrig_Path,              recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(ff$ROI_File),         recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(ff$FireMask_File),    recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(ff$FireMask_File_Er), recursive = TRUE, showWarnings = FALSE)
  dir.create(ff$out_dir_extrsvi,           recursive = TRUE, showWarnings = FALSE)
  dir.create(ff$out_dir_stats,             recursive = TRUE, showWarnings = FALSE)

  
  # Set Basename for statistics output file
  # ff[["Stats_File_Single"]] <- file.path(ff$Out_Stats_Dir, 
  #                                            file_path_sans_ext(basename(ExtTS_RData_File_Single)))  
  # 
  # ff[["Stats_Dir_Multiple"]] <- file.path(dirname(dirname(dirname(ExtTS_RData_File_Multiple))), 
  #                                     "Stat_Analysis", "Burned_Multiple", sep = "")
  # ff[["Stats_File_Multiple"]] <- file.path(Out_Stats_Dir_Multiple, 
  #                                              file_path_sans_ext(basename(ExtTS_RData_File_Single)))  # Set Basename for statistics file
  # dir.create(Out_Stats_Dir_Multiple, recursive = T, 
  #            showWarnings = F)
  # 
  # 
  # 
  # dir.create(ff$Out_Stats_Dir_Single, recursive = T, showWarnings = F)  
  
  #Send out the `ff` list
  return(ff)
  
}
