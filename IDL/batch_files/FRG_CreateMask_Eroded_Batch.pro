!PATH = Expand_Path('+') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Create_Mask_Eroded(Mask_File = 'D:\Temp\tempfrg/Results_2000_2016_Ker_200_Perc_Diff_9.5/Intermediate_Processing/ENVI_Mask/Burned_Areas_00_15_ENVI_Mask' , $ 
 Eroded_Mask_File = 'D:\Temp\tempfrg/Results_2000_2016_Ker_200_Perc_Diff_9.5/Intermediate_Processing/ENVI_Mask/Burned_Areas_00_15_ENVI_Mask_Eroded' )
exit
