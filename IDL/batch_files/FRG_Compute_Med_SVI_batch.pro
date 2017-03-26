!PATH = Expand_Path('+') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Compute_MedScaled_VI(CLC_File_00 = 'D:\Temp\tempfrg\Ancillary_Data\CLC_00\CLC_00_250_ENVI' , $ 
In_File = 'D:\Temp\tempfrg\MODIS_data\/Originals/NDVI/Averages/NDVI_Average_2016.tif' , $ 
FireMask_File= 'D:\Temp\tempfrg/Results_2000_2016_Ker_200_Perc_Diff_9.5/Intermediate_Processing/ENVI_Mask/Burned_Areas_00_15_ENVI_Mask' , $ 
Out_File = 'D:\Temp\tempfrg/Results_2000_2016_Ker_200_Perc_Diff_9.5/Scaled_Indexes/Med_SNDVI/Yearly_Images/2016/Med_SNDVI_2016' , $ 
nodata_out = '32767' , $ 
N_Ker = '801' , $ 
Index = 'NDVI' , $ 
Year = '2016' )
exit
