!PATH = Expand_Path('+E:/Fire_Regeneration_Tool/IDL-FRG') +' ;' + !PATH
envi, /restore_base_save_files  
ENVI_batch_init
res = FRG_Compute_MedScaled_VI(CLC_File_00 = 'E:\Fire_Regeneration_Tool\Ancillary_Data\CLC_00\CLC_00_250_ENVI' , In_File = 'F:\Fire_Regeneration\MODIS_Data\/Originals/NDVI/Average/Mosaic_NDVI_Average_2015.hdr' , FireMask_File= 'F:\Fire_Regeneration\Output_2015/Results_2000_2015_Ker_50_Perc_Diff_9.5/Intermediate_Processing/ENVI_Mask/Burned_Areas_00_15_ENVI_Mask' , Out_File = 'F:\Fire_Regeneration\Output_2015/Results_2000_2015_Ker_50_Perc_Diff_9.5/Scaled_Indexes/Med_SNDVI/Yearly_Images/Med_SNDVI_2015' , nodata_out = '-999' , N_Ker = '201' , Index = 'NDVI' , Year = '2015' )
exit
