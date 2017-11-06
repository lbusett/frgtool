!PATH = Expand_Path('+D:/Documents/Source/git/frgtool/inst/IDL_scripts') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = frg_compute_med_SVI(CLC_file_00 = 'D:\frg\Ancillary_Data\CLC_00\CLC_00_250_ENVI' , $ 
in_file = 'D:\frg\MODIS/Originals/NDVI/Averages/NDVI_Average_2001.tif' , $ 
firemask_file = 'D:\frg\output3/Intermed_Proc/ENVI_Mask/Burned_Areas_00_16_ENVI_Mask' , $ 
out_file = 'D:\frg\output3/Scaled_Indexes/Med_SNDVI/Yearly_Images/2001/Med_SNDVI_2001' , $ 
nodata_out = '32767' , $ 
n_ker = '801' , $ 
index = 'NDVI' , $ 
year = '2001' )
exit
