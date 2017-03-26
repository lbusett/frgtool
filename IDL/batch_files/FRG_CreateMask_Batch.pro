!PATH = Expand_Path('+') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Create_Mask(ROI_File = 'D:\Temp\tempfrg/Results_2000_2016_Ker_200_Perc_Diff_9.5/Intermediate_Processing/ENVI_ROI/Burned_Areas_00_15.ROI' , $ 
                     CLC_00_File = 'D:\Temp\tempfrg\Ancillary_Data\CLC_00\CLC_00_250_ENVI' , $ 
                     Mask_File = 'D:\Temp\tempfrg/Results_2000_2016_Ker_200_Perc_Diff_9.5/Intermediate_Processing/ENVI_Mask/Burned_Areas_00_15_ENVI_Mask')
exit
