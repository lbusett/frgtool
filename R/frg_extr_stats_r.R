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
#' @return NULL
#' @importFrom tools file_path_sans_ext
#' @importFrom raster stack
#' @export
#'

frg_extr_roi_stats_r <- function(
                          burned_shape   = burned_shape, 
                          Erode_File     = Erode_File, 
                          sVI_Folder     = sVI_Folder,
                          CLC_File_00    = CLC_File_00,  
                          ENV_Zones_File = ENV_Zones_File, 
                          Out_File       = Out_File, 
                          no_data_in     = no_data_in, 
                          Start_Year     = Start_Year, 
                          End_Year       = End_Year)
  { 
  
  End_Year    <- as.numeric(End_Year)      
  Start_Year  <- as.numeric(Start_Year)
  no_data_out <- no_data_in
  
  # Open required raster files
  svi_files   <- tools::file_path_sans_ext(list.files(sVI_Folder, pattern = ".hdr$", full.names = TRUE))
  svi_stack   <- raster::stack(svi_files)
  clc_rast    <- raster::raster(CLC_File_00)
  erode_rast  <- raster::raster(Erode_File)  
  envzon_rast <- raster::raster(ENV_Zones_File)
  
  # set nodatavalue for input SVI file
  raster::NAvalue(svi_stack) <- -999
  dates <- ymd(paste0(str_split(names(svi_stack),"_", simplify = TRUE)[,3],"-01-01"))
  svi_stack <- setZ(svi_stack, dates,"time")
  
  
  # Open required shape files
  totburn_shp <- as(sf::st_read(burned_shape),"Spatial")
  nfires      <- dim(totburn_shp)[1]
  
  tempraster = tempfile(tmpdir = tempdir(), fileext = ".tiff")
  gdal_rasterize(burned_shape, tempraster, tr = raster::res(svi_stack), te = extent(svi_stack)[c(1, 3, 2, 4)], 
                 a = "ID", ot = "Int32")
       
  ts_data <- frg_fastzonal(in_rts         = svi_stack, 
                           zone_object    = tempraster, 
                           mask_object    = erode_rast, 
                           clc_object     = clc_rast,
                           envzone_object = envzon_rast,
                           id_field = "ID", 
                           small = "FALSE", 
                           verbose = TRUE, 
                           start_band = 1 , end_band = 5)
  write.csv(ts_data, file = csv_file_single)
  save(ts_data, file = RData_file_single)
  
   out = FRG_Comp_Sig_Matrix(In_File = RData_file_single , 
                             out_file_pmat = out_file_pmat,
                             min_pix = min_pix, 
                             perc_diff = perc_diff , 
                             MedWdt = MedWdt ,
                             sub_zones = sub_zones, 
                             erode = 1)    # Compute p-values matrix


}

  burned_shape   <- "D:/Documents/temp/frgdata/output_2015/Intermediate_Processing/Shapefiles/Burned_Areas_00_15_Single_Fires.shp" 
  Erode_File     <- "D:/Documents/temp/frgdata/output_2015/Intermediate_Processing/ENVI_Mask/Burned_Areas_00_15_ENVI_Mask_Eroded" 
  sVI_Folder     <- "D:/Documents/temp/frgdata/output_2015/Scaled_Indexes/Med_SNDVI/Yearly_Images/"
  CLC_File_00    <- "data/CLC_00/CLC_00_250_ENVI"  
  ENV_Zones_File <- "data/ENV_Zones/ENV_Zones.tif" 
  Out_Folder     <- "D:/Documents/temp/frgdata/output_2015/Med_SNDVI/TS_Extraction/" 
  no_data_in     <- -999
  Start_Year     <- 2003 
  End_Year       <- 2016
  csv_file_single <- "D:/Documents/temp/frgdata/output_2015/Med_SNDVI/TS_Extraction/Burned_Once/TS_Extraction_Med_SNDVI_2000_2015_META_IDL_Matrix.csv" 
  RData_file_single <- "D:/Documents/temp/frgdata/output_2015/Med_SNDVI/TS_Extraction/Burned_Once/TS_Extraction_Med_SNDVI_2000_2015_META_IDL_Matrix.RData" 
  perc_diff      <- 9.5
  min_pix        <- 20
  MedWdt         <- 3
  
  out_dir = dirname("D:/Documents/temp/frgdata/output_2015")
  out_dir_p_matrix = file.path(out_dir,'p_matrix')                                    # Filename for output p-values matrix
  dir.create(out_dir_p_matrix, recursive = T)
  out_file_pmat = file.path(out_dir_p_matrix,'p_matrix')
  print(paste('--- Creating p-values matrix for significance analysis : ',out_file_pmat))
  
  
  
  
