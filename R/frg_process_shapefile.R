#' @title frg_process_shapefile
#' @description Process the Burnt area shapefile to create the shapefiles of areas
#'  burnt once and that of areas burnt multiple times and the corresponding CSV
#'  LUT by calling the "FRG_Burnt_Areas_Processing.py" python script
#' @param opts `list` of options passed from `frg_fullprocessing()`
#' @return The function is called for its side effects
#' @rdname frg_process_shapefile
#' @export 
#' @author Lorenzo Busetto, phD <lbusett@gmail.com>,
#'   Giovanni Caudullo
#' @importFrom tools file_path_sans_ext

# ------------------------------------------------------------------- #
# --- Process the Burnt area shapefile to create the shapefile of area burnt once
# --- and that of areas burnt multiple times and the corresponding ROIs
# ------------------------------------------------------------------- #  
frg_process_shapefile = function(opts) {
  
  
  # - create the two shapefiles starting from the original "full" shapefile by
  #  calling the "FRG_Burnt_Areas_Processing.py" python function
  
  message('-> Creating single and multiple fires shapefiles from : ', 
          basename(opts$orig_shapefile))
  
  py_script <- file.path(opts$src_dir_python,
                         "FRG_Burnt_Areas_Processing.py")
  
  str_python <- paste(opts$arcpython, py_script,
                      '--Burned_Areas_Full_shp', opts$orig_shapefile,
                      '--Intermediate_Folder', opts$intermed_shapes_dir, 
                      sep = ' ')
  out <- system(str_python,  invisible = FALSE, show.output.on.console = TRUE)
  if (out != 0) {
    stop('An error occurred while processing shapefiles using the python script!", 
         Processing aborted!')
  }
  
  Shape_File_Single   <- file.path(opts$intermed_dir, "Shapefiles/",
                                   paste0(tools::file_path_sans_ext(opts$orig_shapefile), 
                                          "_Single_Fires.shp"))
  
  Shape_File_Multiple <- file.path(opts$intermed_dir, "Shapefiles/",
                                   paste0(tools::file_path_sans_ext(opts$orig_shapefile), 
                                          "_Multiple_Fires.shp"))
  
  LUT_File_Multiple   <- file.path(opts$intermed_dir, "Shapefiles/",
                                   paste0(tools::file_path_sans_ext(opts$orig_shapefile), 
                                          "_Intersect_LUT_csv.csv"))
  
  Shape_File_Inter    <- data.frame(Shape_File_Single = Shape_File_Single,
                                    Shape_File_Multiple = Shape_File_Multiple,
                                    LUT_File_Multiple = LUT_File_Multiple)
  return(Shape_File_Inter)
  
}