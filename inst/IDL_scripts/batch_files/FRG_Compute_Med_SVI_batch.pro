!PATH = Expand_Path('+') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Compute_MedScaled_VI(CLC_File_00 = 'D:\frg\Ancillary_Data\CLC_00\CLC_00_250_ENVI' , $ 
In_File = '' , $ 
FireMask_File= '' , $ 
Out_File = 'D:\frg\output/Scaled_Indexes/Med_SNDVI/Yearly_Images/2001/Med_SNDVI_2001' , $ 
nodata_out = '32767' , $ 
N_Ker = '801' , $ 
Index = 'NDVI' , $ 
Year = '2001' )
exit
