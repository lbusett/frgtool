!PATH = Expand_Path('+/home/lb/Source/git/frgtool/inst/IDL_scripts') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = frg_createmask(roi_file = '/home/lb/tmp/frgtest_new/Outputs/Intermed_Proc/ENVI_ROI/Burned_Areas_00_16.ROI' , $ 
                     CLC_00_file = '/home/lb/tmp/frgtest_new/inputs/Ancillary_Data/CLC_00/CLC_00_250_ENVI' , $ 
                     mask_file = '/home/lb/tmp/frgtest_new/Outputs/Intermed_Proc/ENVI_Mask/Burned_Areas_00_16_ENVI_Mask')
exit
