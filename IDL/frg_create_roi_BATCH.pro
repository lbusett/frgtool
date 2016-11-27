!PATH = Expand_Path('+E:/Fire_Regeneration_Tool/IDL-FRG') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Create_ROI(Shape_File = 'F:\Fire_Regeneration\Output_2015/Results_2000_2015_Ker_50_Perc_Diff_9.5/Intermediate_Processing/Shapefiles/Burned_Areas_00_15_Multiple_Fires.shp' , CLC_00_File = 'E:\Fire_Regeneration_Tool\Ancillary_Data\CLC_00\CLC_00_250_ENVI' , ROI_File = 'F:\Fire_Regeneration\Output_2015/Results_2000_2015_Ker_50_Perc_Diff_9.5/Intermediate_Processing/ENVI_ROI/Burned_Areas_00_15_Multiple_Fires.ROI' )
exit
