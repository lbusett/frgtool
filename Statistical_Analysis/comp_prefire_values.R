# Pre_Fire values computation on burnt pixels 
 
Shape_File_TS = 'H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Intermediate_Processing/Shapefiles/Burned_Areas_00_12_Single_Fires.shp'
Shape_File_Orig = 'H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Ancillary_Data/ArcGIS_Shape/Burned_Areas_00_12.shp'
Intermediate_Folder = 'H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Intermediate_Processing/'
CLC_File_00 = 'H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Ancillary_Data/CLC_00/CLC_00_250_ENVI'
ENV_Zones_File = 'H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Ancillary_Data/ENV_Zones/ENV_Zones.tif'

Files_in = c('E:/Projects_Data/Fire_Regeneration_Data/Temp_Files/Temp_NDVI/Yearly_Images/Mosaic_NDVI_2000_2012_Meta',
             'E:/Projects_Data/Fire_Regeneration_Data/Temp_Files/Temp_RDVI/Yearly_Images/Mosaic_RDVI_2000_2012_Meta')

Out_Files = c('H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Prefire_Analysis/NDVI/TS_Extraction_NDVI_2000_2012',
              'H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Prefire_Analysis/RDVI/TS_Extraction_RDVI_2000_2012')
erode_file = 'H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Intermediate_Processing/ENVI_Mask/Burned_Areas_00_12_ENVI_Mask_Eroded'
erode = 1

FRG_Options$Use_Temp_Folder = 0
mess = gwindow(title =paste('Extraction of sVI time series for burnt areas'), container = TRUE, width = 400, height = 40)

for (file in 1:2){
  SVI_File = Files_in [file]
  print(SVI_File)
  ExtTS_File = Out_Files [file]
  dir.create(dirname(ExtTS_File), recursive = T)
  TS_filename =Files_in[file] 
  
  er = FRG_Extr_Stats( SVI_File = SVI_File, Shape_File = Shape_File_TS, CLC_File_00 = CLC_File_00,
                          ENV_Zones_File = ENV_Zones_File, Out_File = ExtTS_File,erode = erode, erode_file = erode_file,
                          Intermediate_Folder = Intermediate_Folder, Overlap = 'Single', Shape_File_Orig = Shape_File_Orig, LUT_File_Multiple = '')
  
#   er = FRG_Extr_Stats(SVI_File = TS_filename, Shape_File = Shape_File_TS, CLC_File_00 = CLC_File_00,ENV_Zones_File = ENV_Zones_File, 
#                       Out_File = ExtTS_File,erode = erode, erode_file =erode_file,Intermediate_Folder = Intermediate_Folder, Overlap = Overlap, 
#                       Shape_File_Orig = '', LUT_File_Multiple = ''  )  	# Call the processing routine
  

  
}

# 
# in_file = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/NDVI/TS_Extraction_NDVI_2000_2012_RData.RData'
# load(in_file)