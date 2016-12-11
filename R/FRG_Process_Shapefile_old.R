

# ------------------------------------------------------------------- #
# --- Process the Burnt area shapefile to create the shapefile of area burnt once
# --- and that of areas burnt multiple times and the corresponding ROIs
# ------------------------------------------------------------------- #  
FRG_Process_Shapefile = function(Shape_File = Shape_File, Intermediate_Folder = Intermediate_Folder) {
  
  # - create the two shapefiles starting from the original "full" shapefile by calling the "FRG_Burnt_Areas_Processing.py" python function
  
  print('------------------------------------------------------')
  print(paste('-> Creating single and multiple fires shapefiles from : ',basename(Shape_File)))
  print('------------------------------------------------------')
  py_script = file.path(FRG_Options$Main_Dir,'Python_FRG','FRG_Burnt_Areas_Processing.py')
  Shapes_Folder = file.path(Intermediate_Folder,'Shapefiles')
  dir.create(Shapes_Folder, recursive = T, showWarnings = FALSE)
  str_python = paste(FRG_Options$arcpython,py_script, '--Burned_Areas_Full_shp',Shape_File, '--Intermediate_Folder', Shapes_Folder, sep = ' ')
  out = system (str_python,  invisible = FALSE, show.output.on.console = TRUE)
  if (out != 0) {
    print('An error occurred while processing shapefiles using python ! Processing stopped')
    # browser()
  }
  Shape_File_Single =  file.path(Intermediate_Folder,'Shapefiles', paste(sub("[.][^.]*$", "", basename(Shape_File)),'_Single_Fires.shp', sep = '')) # Define ROI file name
  Shape_File_Multiple = file.path(Intermediate_Folder,'Shapefiles', paste(sub("[.][^.]*$", "", basename(Shape_File)),'_Multiple_Fires.shp', sep = '')) # Define ROI file name
  LUT_File_Multiple = file.path(Intermediate_Folder,'Shapefiles', paste(sub("[.][^.]*$", "", basename(Shape_File)),'_Intersect_LUT_csv.csv', sep = '')) # Define ROI file name
  Shape_File_Inter = data.frame(Shape_File_Single = Shape_File_Single, Shape_File_Multiple = Shape_File_Multiple, LUT_File_Multiple = LUT_File_Multiple)
  return(Shape_File_Inter)
  
}