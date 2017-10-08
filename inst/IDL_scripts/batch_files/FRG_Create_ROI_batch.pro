!PATH = Expand_Path('+') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Create_ROI(Shape_File = 'D:\frg\Input_Shapefiles\Burned_Areas_00_16.shp' , $ 
                     CLC_00_File = 'D:\frg\Ancillary_Data\CLC_00\CLC_00_250_ENVI' , $ 
                     roi_file = 'D:\frg\output/Intermed_Proc/ENVI_ROI/Burned_Areas_00_16.ROI')
exit
