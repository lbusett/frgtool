!PATH = Expand_Path('+') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Create_ROI(Shape_File = 'D:\Temp\tempfrg\Input_Shapefiles\Burned_Areas_00_15.shp' , $ 
                     CLC_00_File = 'D:\Temp\tempfrg\Ancillary_Data\CLC_00\CLC_00_250_ENVI' , $ 
                     ROI_File = 'D:\Temp\tempfrg/Intermed_Proc/ENVI_ROI/Burned_Areas_00_15.ROI')
exit
