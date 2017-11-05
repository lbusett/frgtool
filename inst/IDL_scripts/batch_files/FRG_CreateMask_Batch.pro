!PATH = Expand_Path('+D:/Documents/Source/git/frgtool/inst/IDL_Scripts') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = frg_createmask(roi_file = 'D:\frg\output3/Intermed_Proc/ENVI_ROI/Burned_Areas_00_16.ROI' , $ 
                     CLC_00_file = 'D:\frg\Ancillary_Data\CLC_00\CLC_00_250_ENVI' , $ 
                     mask_file = 'D:\frg\output3/Intermed_Proc/ENVI_Mask/Burned_Areas_00_16_ENVI_Mask')
exit
