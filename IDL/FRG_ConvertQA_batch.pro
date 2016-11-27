!PATH = Expand_Path('+E:/Fire_Regeneration_Tool/IDL-FRG') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_CONVERT_QA(file_in = 'F:\Fire_Regeneration\MODIS_Data\/Originals/QA/Single_Dates/Mosaic_2015225_QA_LAEA.tif' , file_out ='F:\Fire_Regeneration\MODIS_Data\/Originals/UI/Single_Dates/Mosaic_2015225_UI_LAEA' ,nodata_in='-999' ,nodata_out  ='-999' )
exit
