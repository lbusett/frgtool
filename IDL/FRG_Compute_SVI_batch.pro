!PATH = Expand_Path('+E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/IDL-FRG') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Compute_Scaled_VI(CLC_File_00 = 'Z:\WORKING\Fire_Regeneration\Data\Ancillary\CLC_00\CLC_00_250_ENVI_recoded_cropped' , In_File = 'Z:\WORKING\Fire_Regeneration\Data\MODIS_Data/Originals/RDVI/Average/Mosaic_RDVI_Average_2012.hdr' , FireMask_File= 'Z:/WORKING/Fire_Regeneration/Data/Burned_Areas/ENVI_Mask/Burned_Areas_00_12_ENVI_Mask' , Out_File = 'Z:\WORKING\Fire_Regeneration\Data\MODIS_Data/Scaled_Indexes/100/SRDVI/Yearly_Images/SRDVI_2012' , nodata_out = '-999' , N_Ker = '401' , Index = 'RDVI' , Year = '2012' , low_perc = '0.05' , high_perc = '0.95' , out_5th = '0' , out_95th = '0' , out_NPix = '0' , lowperc_zero = '0' )
exit
