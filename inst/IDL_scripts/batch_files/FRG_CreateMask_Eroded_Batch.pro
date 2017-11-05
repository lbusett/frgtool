!PATH = Expand_Path('+D:/Documents/Source/git/frgtool/inst/IDL_Scripts') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = frg_createmask_eroded(mask_file = 'D:\frg\output3/Intermed_Proc/ENVI_Mask/Burned_Areas_00_16_ENVI_Mask' , $ 
 eroded_mask_file = 'D:\frg\output3/Intermed_Proc/ENVI_Mask/Burned_Areas_00_16_ENVI_Mask_Eroded' )
exit
