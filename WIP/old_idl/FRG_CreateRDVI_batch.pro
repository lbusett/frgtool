!PATH = Expand_Path('+E:/Fire_Regeneration_Tool/IDL-FRG') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_COMPUTE_RDVI(NIR_file = 'F:\Fire_Regeneration\MODIS_Data\/Originals/NIR/Single_Dates/Mosaic_2015225_NIR_LAEA.tif' , RED_File ='F:\Fire_Regeneration\MODIS_Data\/Originals/Red/Single_Dates/Mosaic_2015225_Red_LAEA.tif' ,Out_File='F:\Fire_Regeneration\MODIS_Data\/Originals/RDVI/Single_Dates/Mosaic_2015225_RDVI_LAEA.tif' ,nodata_in='-999' ,nodata_out  ='-999' )
exit
