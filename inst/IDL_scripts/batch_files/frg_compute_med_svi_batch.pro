!PATH = Expand_Path('+E:/Source/git/frgtool/inst/IDL_scripts') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = frg_compute_med_svi(CLC_file_00 = 'E:\tmp\frgtest_new\inputs\Ancillary_Data\CLC_00\CLC_00_250_ENVI' , $ 
in_file = 'E:\tmp\frgtest_new\MODIS/Originals/NDVI/Averages/NDVI_Average_2016.tif' , $ 
firemask_file = 'E:\tmp\frgtest_new\Outputs/Intermed_Proc/ENVI_Mask/Burned_Areas_00_16_ENVI_Mask' , $ 
out_file = 'E:\tmp\frgtest_new\Outputs/Scaled_Indexes/Med_SNDVI/Yearly_Images/2016/Med_SNDVI_2016' , $ 
nodata_out = '32767' , $ 
n_ker = '801' , $ 
index = 'NDVI' , $ 
year = '2016' )
exit
