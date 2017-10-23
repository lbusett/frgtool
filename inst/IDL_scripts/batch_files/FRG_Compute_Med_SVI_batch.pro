!PATH = Expand_Path('+D:/Documents/Source/git/frgtool/inst/IDL_Scripts') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Compute_MedScaled_VI(CLC_File_00 = 'D:\frg\Ancillary_Data\CLC_00\CLC_00_250_ENVI' , $ 
In_File = 'D:\frg\MODIS/Originals/NDVI/Averages/NDVI_Average_2016.tif' , $ 
FireMask_File = '' , $ 
Out_File = 'D:\frg\output/Scaled_Indexes/Med_SNDVI/Yearly_Images/2016/Med_SNDVI_2016' , $ 
nodata_out = '32767' , $ 
N_Ker = '801' , $ 
index = 'NDVI' , $ 
Year = '2016' )
exit
