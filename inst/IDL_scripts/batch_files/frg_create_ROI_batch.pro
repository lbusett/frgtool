!PATH = Expand_Path('+/home/lb/Source/git/frgtool/inst/IDL_scripts') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = frg_create_ROI(shape_file    = '/home/lb/tmp/frgtest_new/inputs/Input_Shapefiles/Burned_Areas_00_16/Burned_Areas_00_16.shp' , $ 
                     CLC_00_file   = '/home/lb/tmp/frgtest_new/inputs/Ancillary_Data/CLC_00/CLC_00_250_ENVI' , $ 
                     roi_file      = '/home/lb/tmp/frgtest_new/Outputs/Intermed_Proc/ENVI_ROI/Burned_Areas_00_16.ROI')
exit
