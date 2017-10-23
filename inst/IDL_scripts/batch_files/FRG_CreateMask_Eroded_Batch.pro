!PATH = Expand_Path('+D:/Documents/Source/git/frgtool/inst/IDL_Scripts') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Create_Mask_Eroded(Mask_File = 'D:\frg\output/Intermed_Proc/ENVI_Mask/Burned_Areas_00_16_ENVI_Mask' , $ 
 Eroded_Mask_File = 'D:\frg\output/Intermed_Proc/ENVI_Mask/Burned_Areas_00_16_ENVI_Mask_Eroded' )
exit
