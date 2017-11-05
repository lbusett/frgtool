!PATH = Expand_Path('+E:/Fire_Regeneration_Tool/IDL-FRG') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_MEAN_Indexes(File_1 = 'F:\Fire_Regeneration\MODIS_Data\/Originals/NDVI/Single_Dates/Mosaic_2015209_NDVI_LAEA.tif' , File_2 = 'F:\Fire_Regeneration\MODIS_Data\/Originals/NDVI/Single_Dates/Mosaic_2015225_NDVI_LAEA.tif' , Out_File= 'F:\Fire_Regeneration\MODIS_Data\/Originals/NDVI/Average/Mosaic_NDVI_Average_2015' , UI_File1 = 'F:\Fire_Regeneration\MODIS_Data\/Originals/UI/Single_Dates/Mosaic_2015209_UI_LAEA.hdr' , UI_File2 = 'F:\Fire_Regeneration\MODIS_Data\/Originals/UI/Single_Dates/Mosaic_2015225_UI_LAEA.hdr' , RELY_File1 = 'F:\Fire_Regeneration\MODIS_Data\/Originals/RELY/Single_Dates/Mosaic_2015209_RELY_LAEA.tif' , RELY_File2 = 'F:\Fire_Regeneration\MODIS_Data\/Originals/RELY/Single_Dates/Mosaic_2015225_RELY_LAEA.tif' , nodata_in = '-999' , UI_check = '1' , max_UI = '5' )
exit
