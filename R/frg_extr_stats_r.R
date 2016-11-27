#' frg_extr_roi_stats_r
#'
#' @description function to extract time series data on burnt areas from multitemporal
#'   SVI files
#' @param ROI_File 
#' @param Erode_File 
#' @param sVI_File 
#' @param CLC_File_00 
#' @param ENV_Zones_File 
#' @param Out_File 
#' @param no_data_in 
#' @param Start_Year 
#' @param End_Year 
#'
#' @return
#' @export
#'
#' @examples
frg_extr_roi_stats_r <- function(
                          ROI_File       = ROI_File, 
                          Erode_File     = Erode_File, 
                          sVI_File       = sVI_File,
                          CLC_File_00    = CLC_File_00,  
                          ENV_Zones_File = ENV_Zones_File, 
                          Out_File       = Out_File, 
                          no_data_in     = no_data_in, 
                          Start_Year     = Start_Year, 
                          End_Year       = End_Year)
  { 
  
  End_Year = as.numeric(End_Year)      
  Start_Year = as.numeric(Start_Year)
  
  
  
  
  
  
  
  
  
  
  
  
  
}

  ROI_File       = "D:/Documents/temp/frgdata/output_2015/Intermediate_Processing/ENVI_ROI/Burned_Areas_00_15.ROI" 
  Erode_File     = "D:/Documents/temp/frgdata/output_2015/Intermediate_Processing/ENVI_Mask/Burned_Areas_00_15_ENVI_Mask_Eroded" 
  sVI_Folder     = "D:/Documents/temp/frgdata/output_2015/Scaled_Indexes/Med_SNDVI/Yearly_Images/"
  CLC_File_00    = "data/CLC_00/CLC_00_250_ENVI"  
  ENV_Zones_File = "data/ENV_Zones/data/ENV_Zones/ENV_Zones.tif" 
  Out_Folder     = "D:/Documents/temp/frgdata/output_2015/Med_SNDVI/TS_Extraction/" 
  no_data_in     = "-999"
  Start_Year     = 2003 
  End_Year       = 2016

