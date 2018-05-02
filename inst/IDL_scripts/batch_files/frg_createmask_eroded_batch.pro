!PATH = Expand_Path('+/home/lb/Source/git/frgtool/inst/IDL_scripts') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = frg_createmask_eroded(mask_file = '/home/lb/tmp/frgtest_new/Outputs/Intermed_Proc/ENVI_Mask/Burned_Areas_00_16_ENVI_Mask' , $ 
 eroded_mask_file = '/home/lb/tmp/frgtest_new/Outputs/Intermed_Proc/ENVI_Mask/Burned_Areas_00_16_ENVI_Mask_Eroded' )
exit
