#' @title FRG_Process_Shapefile
#' @description Function used Process the Burnt area shapefile and create the shapefile of area burnt once \cr
#'              and that of areas burnt multiple times and the corresponding ROIs
#'
#' @param Shape_File string Input Shapefile of BAs
#' @param Intermediate_Folder string Folder where the new shapefiles will be placed
#' @param CLC_File_00 string ENVI file containing the CORINE land Cover map 2000, recoded to the EFFIS legend

FRG_Process_Shapefile = function(Shape_File = Shape_File, Intermediate_Folder = Intermediate_Folder,CLC_File_00 = CLC_File_00) {
  
# ------------------------------------------------------------------- #
# - create the two shapefiles starting from the original "full" shapefile by calling the "FRG_Burnt_Areas_Processing.py" python function
# ------------------------------------------------------------------- #
  
  print('------------------------------------------------------')
  print(paste('-> Creating single and multiple fires shapefiles from : ',basename(Shape_File)))
  print('------------------------------------------------------')
  py_script = file.path(FRG_Options$Main_Dir,'Python_FRG','FRG_Burnt_Areas_Processing.py')
  Shapes_Folder = file.path(Intermediate_Folder,'Shapefiles')
  dir.create(Shapes_Folder, recursive = T, showWarnings = FALSE)
  str_python = paste(FRG_Options$arcpython,py_script, '--Burned_Areas_Full_shp',Shape_File, '--Intermediate_Folder', Shapes_Folder,
                  sep = ' ')
  
  out = system (str_python,  invisible = FALSE, show.output.on.console = TRUE)
  if (out != 0) {
    print('An error occurred while processing shapefiles using python ! Processing stopped')
    stop()
  }
  Shape_File_Single =  file.path(Intermediate_Folder,'Shapefiles', paste(sub("[.][^.]*$", "", basename(Shape_File)),'_Single_Fires.shp', sep = '')) # Retrieve name of created shape
  Shape_File_Multiple = file.path(Intermediate_Folder,'Shapefiles', paste(sub("[.][^.]*$", "", basename(Shape_File)),'_Multiple_Fires.shp', sep = '')) # Retrieve name of created shape
  LUT_File_Multiple = file.path(Intermediate_Folder,'Shapefiles', paste(sub("[.][^.]*$", "", basename(Shape_File)),'_Intersect_LUT_csv.csv', sep = '')) # Retrieve name of created LUT
  Shape_File_Inter = data.frame(Shape_File_Single = Shape_File_Single, Shape_File_Multiple = Shape_File_Multiple, LUT_File_Multiple = LUT_File_Multiple)
  
  
#   Create ROIs for single fires and multiple fires
  exp_path_str = paste('!PATH = Expand_Path(\'','+',FRG_Options$IDL_Dir,'\') +\' ;\' + !PATH', sep = '')
  ROI_File = FRG_ROI_Build(Shape_File = Shape_File_Single, CLC_File_00 = CLC_File_00,exp_path_str = exp_path_str, Intermediate_Folder = Intermediate_Folder)
  ROI_File = FRG_ROI_Build(Shape_File = Shape_File_Multiple, CLC_File_00 = CLC_File_00,exp_path_str = exp_path_str, Intermediate_Folder = Intermediate_Folder)
  return(Shape_File_Inter)
  
}