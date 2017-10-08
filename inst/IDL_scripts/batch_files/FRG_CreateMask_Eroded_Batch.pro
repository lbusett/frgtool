!PATH = Expand_Path('+') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Create_Mask_Eroded(Mask_File = '' , $ 
 Eroded_Mask_File = 'D:\frg\output/Intermed_Proc/ENVI_Mask/Burned_Areas_00_16_ENVI_Mask_Eroded' )
exit
