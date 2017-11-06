#' @title frg_def_files
#' @description Helper function used to define required file names and folders  
#' @param opts initial options list created by frg_main
#' @return `opts` list containing all main file names and folders
#' @rdname frg_def_files
#' @export 
#' @author Lorenzo Busetto, phD (2017) <lbusett@gmail.com>
#' @importFrom tools file_path_sans_ext
#' 
frg_def_files                   <- function(opts) {
  
  opts[["orig_shapefile"]] <- opts$orig_shapefile
  # Set Processing log file
  opts[["outfile_conn"]]   <- file.path(opts$out_dir,
                                        paste("Process_Summary", ".txt", sep = ""))  
  # Set Dir for storing intermediate processing results
  opts[["intermed_dir"]]   <- file.path(opts$out_dir, "Intermed_Proc")  
  # Set Dir for storing EFFIS summaries results
  opts[["summary_dir"]]    <- file.path(opts$out_dir, "Summaries_for_EFFIS") 
  # Define Dir were Scaled Indexes time series will be saved
  opts[["scaled_dir"]]     <- file.path(opts$out_dir, "Scaled_Indexes")  
  opts[["out_origpath"]]   <- file.path(opts$mod_dir, "Originals")
  
  opts[["roi_file"]]       <- file.path(
    opts$intermed_dir,
    "ENVI_ROI", 
    paste0(file_path_sans_ext(basename(opts$orig_shapefile)), ".ROI")
  )
  
  # Define Mask file name and create Dir for mask storing if needed ----
  opts[["firemask_file"]] <- file.path(
    opts$intermed_dir, "ENVI_Mask", 
    paste0(file_path_sans_ext(basename(opts$roi_file)), 
           "_ENVI_Mask"))
  
  # Define Mask file name and create Dir for mask storing if needed ----
  opts[["firemask_file_er"]] <- file.path(
    opts$intermed_dir, "ENVI_Mask",  
    paste0(file_path_sans_ext(basename(opts$roi_file)), 
           "_ENVI_Mask_Eroded"))
  
  opts[["out_dir_extrsvi"]]  <- file.path(opts$out_dir,
                                          "Med_SNDVI/TS_Extraction")
  
  opts[["ts_file_single"]]   <- file.path(opts$out_dir_extrsvi, 
                                          paste0("Med_SNDVI_ts_single"))
  opts[["ts_file_multiple"]] <- file.path(opts$out_dir_extrsvi, 
                                          paste0("Med_SNDVI_ts_multiple"))
  
  opts[["out_dir_stats"]]    <- file.path(opts$out_dir, "Med_SNDVI/Stat_Analysis")
  
  opts[["ts_filename"]]      <- file.path(
    opts$scaled_dir, "Med_SNDVI", 
    paste0("Med_SNDVI", "_", opts$start_year, "_", opts$end_year, "_META")
  ) 
  
  # Set Basename for statistics output file
  opts[["stats_file_single"]]   <- file.path(opts$out_dir_stats,
                                             "Burned_Once",
                                             basename(opts$ts_file_single))
  opts[["stats_file_multiple"]] <- file.path(opts$out_dir_stats,
                                             "Burned_Multiple",
                                             basename(opts$ts_file_multiple))
  
  opts[["intermed_shapes_dir"]] <- file.path(opts$intermed_dir, "Shapefiles/")
  opts[["shapefile_single"]] = file.path(
    opts$intermed_dir, "Shapefiles/",
    paste0(basename(tools::file_path_sans_ext(opts$orig_shapefile)), 
           "_Single_Fires.shp")
  )
  opts[["shapefile_multiple"]] <-  file.path(
    opts$intermed_dir, "Shapefiles/",
    paste0(basename(tools::file_path_sans_ext(opts$orig_shapefile)), 
           "_Multiple_Fires.shp")
  )
  opts[["lut_file_multiple"]] <- file.path(
    opts$intermed_dir, "Shapefiles/",
    paste0(basename(tools::file_path_sans_ext(opts$orig_shapefile)), 
           "_Intersect_LUT_csv.csv")
  )
  opts[["out_shape_dir"]] <- file.path(opts$summary_dir, "Shapefiles")
  
  # Create all required Dirs (if needed) ----
  
  dir.create(opts$scaled_dir,                recursive = TRUE, showWarnings = FALSE)
  dir.create(opts$intermed_dir,              recursive = TRUE, showWarnings = FALSE)
  dir.create(opts$summary_dir,               recursive = TRUE, showWarnings = FALSE)
  dir.create(opts$out_origpath,              recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(opts$roi_file),         recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(opts$firemask_file),    recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(opts$firemask_file_er), recursive = TRUE, showWarnings = FALSE)
  dir.create(opts$out_dir_extrsvi,           recursive = TRUE, showWarnings = FALSE)
  dir.create(opts$out_dir_stats,             recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(opts$ts_file_single),   recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(opts$ts_file_multiple), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(opts$intermed_shapes_dir), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(opts[["stats_file_single"]]), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(opts[["stats_file_multiple"]]), recursive = TRUE, showWarnings = FALSE)
  
  #Send back the updated `opts` list ----
  return(opts)
  
}
