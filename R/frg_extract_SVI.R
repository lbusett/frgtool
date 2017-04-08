#'frg_extract_svi
#'@description Function used to extract Scaled VIs time series of the different pixels of analyzed burnt areas, 
#'  associate info form the burnt areas shapefile and save as an RData file to be used for statistical analysis
#'@details This function is used to extract Scaled VIs time series of the different pixels of analyzed burnt 
#'  areas from the raster images created by 'FRG_Compute_SVI.R, associate ancillary info derived form the burnt 
#'  areas shapefile (e.g., Area, FireYear, ...) and save results as an RData file (in the '/TS_Extraction subfolder) 
#'  to be used later for statistical analysis
#'
#' @param SVI_File string ENVI Meta File associated to time serie to be analyzed
#' @param Shape_File string Burnt Areas Shapefile (Must Correspond to the 'burned_once' shafile if Overlap == 'Single'
#'                                                 Must Correspond to the 'burned_once' shafile if Overlap == 'Multiple')
#' @param CLC_File_00 string ENVI file containing the CORINE land Cover map 2000, recoded to the EFFIS legend
#' @param ENV_Zones_File string Tiff file containing EcoZones (No longer used...)
#' @param Out_File numeric BaseName of the output RData and CSV files
#' @param erode flag if 1, then perfom analysis on eroded ROIS (default to 1)
#' @param erode_file string Filename of the ENVI mask containing the ERODED ROIS (generated automatically in FRG_Compute_SVI.R)
#' @param Intermed_Dir string name of the Folder containing the intermediate processing results
#' @param Overlap string If == 'Single', extract time series for areas burned once.
#'        if =='Multiple, extract time series for areas burned multiple times
#' @param Shape_File_Orig string Filename of the original input BAs shapefile
#' @param LUT_File_Multiple string Filename of the LUT table created by FRG_Process_Shapefile.py
#'
#' @return 'DONE' if all went OK, otherwise error message
#'
#' @importFrom tools file_path_sans_ext
#' @importFrom sf st_read
#' @importFrom lubridate ymd
#' @importFrom raster raster stack setZ extent
#' @importFrom gdalUtils gdal_rasterize
#' @importFrom stringr str_split
#' @author Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Nov 8, 2012
#' @export
#'
frg_extract_svi <- function(SVI_File, Shape_File, CLC_File_00, ENV_Zones_File, 
                            Out_File, erode, FireMask_File_Er, Intermed_Dir, Overlap, Shape_File_Orig, 
                            LUT_File_Multiple) {
  
  message("----------------------------------------------------------")
  message("---- Extraction of sVI time series for burnt areas -", Overlap, " fires ----")
  message("----------------------------------------------------------")
  message(paste("-> In File for TS extraction: ", SVI_File))
  message(paste("-> Out File for TS extraction: ", Out_File))
  
  # Define output file names (csv and RData) ----
  if (Overlap == "Single") {
    Out_RData   <- paste(Out_File, "RData.RData", sep = "_")  # Filenames used for output saving RData Matrix
    Out_IDL_CSV <- paste(Out_File, "IDL_Matrix.csv", sep = "_")
  } else {
    Out_RData   <- paste(Out_File, "RData.RData", sep = "_")  # Filenames used for output saving RData Matrix
    Out_IDL_CSV <- paste(Out_File, "IDL_Matrix.csv", sep = "_")
  }
  
  selection <- "yes"
  
  if (!(file.exists(Out_RData)) | selection == "yes") {
    svi_folder  <- file.path(dirname(SVI_File), "Yearly_Images")
    # Open required raster files ----
    svi_files   <- file_path_sans_ext(list.files(svi_folder, pattern = ".hdr$",
                                                 recursive = TRUE, include.dirs = TRUE, 
                                                 full.names = TRUE)) 
    svi_stack   <- raster::stack(svi_files) 
    clc_rast    <- raster::raster(CLC_File_00) 
    erode_rast  <- raster::raster(FireMask_File_Er)
    # envzon_rast <- raster::raster(ENV_Zones_File) 
    
    raster::NAvalue(svi_stack) <- 32767 # set nodatavalue for input SVI file  
    # set Dates as attribute to input svi time series
    dates     <- ymd(paste0(str_split(names(svi_stack),'_', simplify = TRUE)[,3],'-01-01')) 
    svi_stack <- setZ(svi_stack, dates,'time') 
    
    #Open required shape file ----
    totburn_shp <- as(sf::st_read(Shape_File, quiet = TRUE),'Spatial') 
    nfires      <- dim(totburn_shp)[1]
    tempraster  <- tempfile(tmpdir = tempdir(), fileext = ".tiff") 
    id_field    <- ifelse(Overlap == 'Single', 'OBJECTID','OVERLAP_ID') 
    
    #Rasterize the shapefile to allow a fast extraction of SVI data ----
    gdal_rasterize(Shape_File, tempraster, tr = raster::res(svi_stack), 
                   te = extent(svi_stack)[c(1, 3, 2, 4)], a = id_field, 
                   ot = 'Int32') 
    
    ts_data <- frg_fastzonal(in_rts = svi_stack, zone_object = tempraster, 
                             mask_object = erode_rast, clc_object = clc_rast, 
                             # envzone_object = envzon_rast, 
                             id_field = "ID", 
                             small = FALSE,
                             verbose = TRUE) 
    write.csv(ts_data, file = Out_IDL_CSV)
    save(ts_data, file = Out_RData) 
    
    Data <- ts_data  # Data now contains time series for all pixels of each burnt 
                     #area
    
    message("---- Creating RData file joining MODIS and Shapefile info ----")
   
    # Load the burned Areas shapefile and save the attribute table as a ----
    # data frame
    BAreas_Name <- strsplit(basename(Shape_File_Orig), ".shp")[[1]]
    BAreas_Dir  <- dirname(Shape_File_Orig)
    BAreas_shp  <- opens(BAreas_Dir, BAreas_Name, stringsAsFactors = FALSE)
    Data_Shape  <- BAreas_shp@data  # Get attributes data from the shp

    #----------------------------------------------------------------------------------------------------------#
    # Processing conducted when analyzing areas burned only once
    #----------------------------------------------------------------------------------------------------------#
    
    if (Overlap == "Single") {
      # Load the statistics file computed by IDL (Derived from Application of
      # FRG_ROI_STAT_ERODE.pro)
      
      # Data = read.csv(Out_IDL_CSV,header = TRUE, na.strings =
      # FRG_Options$No_Data_Out_Rast, stringsAsFactors = FALSE) Data[Data ==
      # FRG_Options$No_Data_Out_Rast] = NA # Put the NODATA to 'R' NA
      
      Data <- read.csv(Out_IDL_CSV)
      Data <- select(Data, -X)
      n_Years <- length(unique(Data$Year))
      names(Data)[1] <- "OBJECTID"
      Data$OBJECTID <- as.factor(Data$OBJECTID)
      # names(Data)[5:(5+n_Years-1)] =
      # as.character(seq(as.numeric(Start_Year),as.numeric(End_Year))) #
      # Correct columns names names(Data)[(5+n_Years):(5+2*n_Years-1)] =
      # paste('erode',as.character(seq(as.numeric(Start_Year),as.numeric(End_Year))),
      # sep = '_') # Correct columns names
      
      # Remove from Data_Shape records not found in the MODIS ROI data
      
      Data_Shape <- Data_Shape[which(Data_Shape$OBJECTID %in% unique(Data$OBJECTID)), 
                               ]
      Data_Shape$Area_HA <- as.numeric(as.character(Data_Shape$Area_HA))
      Data_Shape <- arrange(Data_Shape, desc(Area_HA))
      # Compute the FireYear and Area variable for each fire (From the
      # sahpefile data) and add it to the DataFrame
      
      FireYear <- numeric(length(Data$OBJECTID))
      Area_All <- numeric(length(Data$OBJECTID))
      Area_Forest <- numeric(length(Data$OBJECTID))
      IDS <- unique(Data_Shape$OBJECTID)
      
      estimate_Area <- function(Data_tmp) {
        # Accessory function used to compute Burnt area for each CLC class from
        # the number of pixels in the ROI Used only if Info is not already
        # present in the shapefile
        full_Area <- 250 * 250 * (length(Data_tmp$N_PIX))/10000
        tot_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                               c(2, 3, 4, 5, 6))]))/10000
        bro_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                               c(2))]))/10000
        con_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                               c(3))]))/10000
        mix_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                               c(4))]))/10000
        scler_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                                 c(5))]))/10000
        trans_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                                 c(6))]))/10000
        est_Area <- data.frame(tot_Area = tot_Area, bro_Area = bro_Area, 
                               con_Area = con_Area, mix_Area = mix_Area, trans_Area = trans_Area, 
                               scler_Area = scler_Area, full_Area = full_Area)
      }
      Data <- data.table(Data, key = c("OBJECTID", "Year"))
      Data_Shape <- data.table(Data_Shape, key = "OBJECTID")
      for (FireID in 1:length(IDS)) {
        if (FireID %in% seq(1, 20000, 100)) 
          (message(FireID, "  ", length(IDS)))
        ID <- as.numeric(as.character(IDS[FireID]))
        
        FireRows <- which(Data$OBJECTID == ID)  # Rows of the dataframe corresponding to the selected fire
        where_id <- which(Data_Shape$OBJECTID == ID)
        YY <- Data_Shape$YearSeason[where_id][1]  # Retrieve Fire Year
        # Retrieve Total Area burned in considered CLC_Classes
        Area_For <- Data_Shape[where_id, "BroadLeave"] + Data_Shape[where_id, 
                                                                    "Coniferous"] + Data_Shape[where_id, "MixedFores"] + 
          Data_Shape[where_id, "Sclerophyl"] + Data_Shape[where_id, 
                                                          "Transition"]
        if (Area_For == 0) {
          # If Area_Forest = 0 then, noinfo in shape and area estimated from
          # number of pixels.
          est_Area <- estimate_Area(Data[FireRows, ])/n_Years
          Area_For <- est_Area$tot_Area
          Data_Shape$BroadLeave[where_id] <- est_Area$bro_Area
          Data_Shape$Coniferous[where_id] <- est_Area$con_Area
          Data_Shape$MixedFores[where_id] <- est_Area$mix_Area
          Data_Shape$Sclerophyl[where_id] <- est_Area$scler_Area
          Data_Shape$Transition[where_id] <- est_Area$trans_Area
        }
        Area_Tot <- Data_Shape$Area_HA[where_id][1]  # Retrieve Total Area burned
        if (Area_Tot == 0) 
        {
          Area_All <- est_Area$full_Area
        }  # If Area_Tot = 0 then, noinfo in shape and area estimated from number of pixels.
        FireYear[FireRows] <- YY  # Assign FireYear
        Area_Forest[FireRows] <- Area_For  # Assign total area burnt in forest land cover types
        Area_All[FireRows] <- Area_Tot  # Assign total area burnt in forest land cover types
        
      }
      Data$FireYear <- FireYear  # Assign FireYear
      Data$Area_All <- Area_All  # Assign total area of the intersectr
      Data$Area_Forest <- Area_Forest  # Assign total area burnt in forest land cover types
      
      # Data <- Data[,c(1:4,ncol(Data)-2, ncol(Data)-1,
      # ncol(Data),5:(ncol(Data)-3))] #Reorder Columns
      
      # Reclass the values of CORINE using the same scheme used in the EFFIS
      # data base
      
      Data$CLC_Class <- factor(Data$CLC_Class, levels = c(0:10), 
                               labels = c("Artificial Surfaces", "Agricultural Areas", 
                                          "Broadleaved Forests", "Coniferous Forests", "Mixed Forests", 
                                          "Schlerophyllus Vegetation", "Transitional Vegetation", 
                                          "Other Natural Land", "Wetlands", "Water Bodies", "Other"))
      
      Data$ENV_ZONE <- factor(Data$ENV_ZONE, levels = c(1:13), labels = c("ALN", 
                                                                          "BOR", "NEM", "ATN", "ALS", "CON", "ATC", "PAN", "LUS", 
                                                                          "ANA", "MDM", "MDN", "MDS"))
      
      # recode(Data$ENV_ZONE , # Recode ENV_ZONE according to legend '1 =
      # 'ALN'; 2 = 'BOR'; 3 = 'NEM'; 4 = 'ATN'; 5 = 'ALS'; 6 = 'CON'; 7 =
      # 'ATC'; 8 = 'PAN'; 9 = 'LUS'; 10 = 'ANA'; 11 = 'MDM'; 12 = 'MDN'; 13 =
      # 'MDS'' , as.factor.result = TRUE)
      
      # Add attributes to the Data Frame (Useful to keep track of processing
      # options!)
      
      # attr(Data, 'SVI_File') = SVI_File ;
      attr(Data, "Shape_File") <- Shape_File  #; \tattr(Data, 'CSV_File') = Out_IDL_CSV
      attr(Data, "CLC_File") <- CLC_File_00
      attr(Data, "Processing Date") <- Sys.Date()
      attr(Data, "Start_Year") <- Start_Year
      attr(Data, "End_Year") <- End_Year
      attr(Data, "Index") <- "Med_SNDVI"
      # if (length(grep( 'RDVI',SVI_File)) > 0) {attr(Data, 'Index') ='RDVI'}
      # # to be removed !  if (length(grep( 'NDVI',SVI_File)) > 0)
      # {attr(Data, 'Index') ='NDVI'} # to be removed !  if (length(grep(
      # 'SNDVI',SVI_File) > 0)) {attr(Data, 'Index') ='SNDVI'} if
      # (length(grep( 'SRDVI',SVI_File) > 0)) {attr(Data, 'Index') ='SRDVI'}
      # if (length(grep( 'Med_SNDVI',SVI_File) > 0)) {attr(Data, 'Index')
      # ='Med_SNDVI'} if (length(grep( 'Med_SRDVI',SVI_File)) > 0)
      # {attr(Data, 'Index') ='Med_SRDVI'}
      
      # Save the computed Data Frames
      
      print(paste("-> Saving TS info RData file"))
      save(Data, Data_Shape, file = Out_RData)
      
      # Finished Processing. Close message box and perform garbage collection
      gc()
      rm(Data, BAreas_shp)
      gc()
    } else {
      
      #----------------------------------------------------------------------------------------------------------#
      # Processing conducted when analyzing areas burned multiple times
      #----------------------------------------------------------------------------------------------------------#
      
      BAreas_Name <- strsplit(basename(Shape_File), ".shp")[[1]]  # Open shapefile of areas burned multiple times
      BAreas_Dir <- dirname(Shape_File)  # (Used to determine burnt surface in the overlaps )
      BAreas_shp_mult <- readOGR(BAreas_Dir, BAreas_Name)
      Data_Shape_mult <- BAreas_shp_mult@data  # Get attributes data from the shp
      
      # Load the statistics file computed by IDL (Derived from Application of
      # FRG_ROI_STAT_ERODE.pro) Data = read.csv(Out_IDL_CSV,header = TRUE,
      # na.strings = FRG_Options$No_Data_Out_Rast, stringsAsFactors = FALSE)
      # Data[Data == FRG_Options$No_Data_Out_Rast] = NA # Put the NODATA to
      # 'R' NA n_Years = (length(names(Data)) - 4)/2
      Data <- read.csv(Out_IDL_CSV)
      Data <- select(Data, -X)
      n_Years <- length(unique(Data$Year))
      names(Data)[1] <- "OVERLAP_ID"
      Data$OVERLAP_ID <- as.factor(Data$OVERLAP_ID)
      
      
      # names(Data)[5:(5+n_Years-1)] =
      # as.character(seq(as.numeric(Start_Year),as.numeric(End_Year))) #
      # Correct columns names names(Data)[(5+n_Years):(5+2*n_Years-1)] =
      # paste('erode',as.character(seq(as.numeric(Start_Year),as.numeric(End_Year))),
      # sep = '_') # Correct columns names
      
      # Load the LUT data relating each overlap with the corresponding
      # original BAs
      Data_LUT <- read.csv2(file = LUT_File_Multiple, stringsAsFactors = FALSE, 
                            header = TRUE, sep = ";")  # Restore the LUT
      # Create a new empty Data_Shape object. This will be filled using info
      # from the ROIS
      Data_Shape <- droplevels(Data_Shape[0, ])
      names(Data_Shape)[1] <- "OVERLAP_ID"
      names <- names(Data_Shape)
      n_recs <- length(unique(Data$OVERLAP_ID))
      tmp_df <- as.data.frame(array(NA, dim = c(n_recs, length(names))))
      names(tmp_df) <- names
      Data_Shape <- tmp_df
      # Set the Data_Shape OVERLAPID to the values derived from the ROIS
      Data_Shape$OVERLAP_ID <- unique(Data$OVERLAP_ID)
      Data_Shape$FireYear <- "Multiple"
      Data_Shape$YearSeason <- "Multiple"
      Data_Shape$FireDate <- "Multiple"
      # Compute the FireYear and Area variable for each fire (From the
      # sahpefile data) and add it to the DataFrame FireYear corresponds to
      # the FIreYear of the earlier fire that originated the overlap
      FireYear <- numeric(length(Data$OVERLAP_ID))
      Area_All <- numeric(length(Data$OVERLAP_ID))
      Area_Forest <- numeric(length(Data$OVERLAP_ID))
      IDS <- unique(Data$OVERLAP_ID)
      Data <- data.table(Data, key = "OVERLAP_ID")
      Data_Shape <- data.table(Data_Shape, key = "OVERLAP_ID")
      
      estimate_Area <- function(Data_tmp) {
        # Accessory function used to compute Burnt area for each CLC class from
        # the number of pixels in the ROI Used ALWAYS for areas burned multiple
        # times, since no Info is already available regarding the area burned
        # in different classes within an overlap
        
        full_Area <- 250 * 250 * (length(Data_tmp$N_PIX))/10000
        Area_For <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                               c(2, 3, 4, 5, 6))]))/10000
        bro_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                               c(2))]))/10000
        con_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                               c(3))]))/10000
        mix_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                               c(4))]))/10000
        scler_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                                 c(5))]))/10000
        trans_Area <- 250 * 250 * (length(Data_tmp$N_PIX[which(Data_tmp$CLC_Class %in% 
                                                                 c(6))]))/10000
        est_Area <- data.frame(Area_For = Area_For, bro_Area = bro_Area, 
                               con_Area = con_Area, mix_Area = mix_Area, trans_Area = trans_Area, 
                               scler_Area = scler_Area, full_Area = full_Area)
      }
      
      for (FireID in 1:length(IDS)) {
        if (FireID %in% seq(1, 20000, 100)) 
          (message(FireID, "  ", length(IDS)))
        FireRows <- which(Data$OVERLAP_ID == IDS[FireID])  # Rows of the dataframe corresponding to the selected fire
        min_FireYear <- min(Data_LUT[which(Data_LUT$OVERLAP_ID == 
                                             IDS[FireID]), ]$YearSeason)  # Get fire year of the first fire originating the overlap
        YY <- min_FireYear  # Set Fire Year to the year of the first fire
        est_Area <- estimate_Area(Data[FireRows, ])/n_Years  # Estimate burned area in the different LC classes, as the product of the number of pixel*250*250
        where_id <- which(Data_Shape$OVERLAP_ID == IDS[FireID])
        Data_Shape$Area_HA[where_id] <- Data_Shape_mult$Area_Int[BAreas_shp_mult$OVERLAP_ID == 
                                                                   IDS[FireID]][1]  # Area burned in all land cover types
        Data_Shape$BroadLeave[where_id] <- est_Area$bro_Area
        Data_Shape$Coniferous[where_id] <- est_Area$con_Area
        Data_Shape$MixedFores[where_id] <- est_Area$mix_Area
        Data_Shape$Sclerophyl[where_id] <- est_Area$scler_Area
        Data_Shape$Transition[where_id] <- est_Area$trans_Area
        FireYear[FireRows] <- YY  # Assign FireYear
        Area_Forest[FireRows] <- est_Area$Area_For  # Assign total area burnt in forest land cover types
        Area_All[FireRows] <- Data_Shape_mult$Area_Int[BAreas_shp_mult$OVERLAP_ID == 
                                                         IDS[FireID]][1]  # Assign total area burnt in forest land cover types
      }
      
      Data$FireYear <- FireYear  # Assign FireYear
      Data$Area_All <- Area_All  # Assign total area of the intersect
      Data$Area_Forest <- Area_Forest  # Assign total area burnt in forest land cover types
      
      # Data <- Data[,c(1:4,ncol(Data)-2, ncol(Data)-1,
      # ncol(Data),5:(ncol(Data)-3))] # Reorder Columns
      
      # Reclass the values of CORINE using the same scheme used in the EFFIS
      # data base
      
      Data$CLC_Class <- factor(Data$CLC_Class, levels = c(0:10), 
                               labels = c("Artificial Surfaces", "Agricultural Areas", 
                                          "Broadleaved Forests", "Coniferous Forests", "Mixed Forests", 
                                          "Schlerophyllus Vegetation", "Transitional Vegetation", 
                                          "Other Natural Land", "Wetlands", "Water Bodies", "Other"))
      
      Data$ENV_ZONE <- factor(Data$ENV_ZONE, levels = c(1:13), labels = c("ALN", 
                                                                          "BOR", "NEM", "ATN", "ALS", "CON", "ATC", "PAN", "LUS", 
                                                                          "ANA", "MDM", "MDN", "MDS"))
      
      # Add attributes to the Data Frame (Useful to keep track of processing
      # options!)
      
      attr(Data, "SVI_File") <- SVI_File
      attr(Data, "Shape_File") <- Shape_File
      attr(Data, "CSV_File") <- Out_IDL_CSV
      attr(Data, "CLC_File") <- CLC_File_00
      attr(Data, "Processing Date") <- Sys.Date()
      attr(Data, "Start_Year") <- Start_Year
      attr(Data, "End_Year") <- End_Year
      if (length(grep("RDVI", SVI_File)) > 0) 
      {
        attr(Data, "Index") <- "RDVI"
      }  # to be removed !
      if (length(grep("NDVI", SVI_File)) > 0) 
      {
        attr(Data, "Index") <- "NDVI"
      }  # to be removed !
      if (length(grep("SNDVI", SVI_File) > 0)) {
        attr(Data, "Index") <- "SNDVI"
      }
      if (length(grep("SRDVI", SVI_File) > 0)) {
        attr(Data, "Index") <- "SRDVI"
      }
      if (length(grep("Med_SNDVI", SVI_File) > 0)) {
        attr(Data, "Index") <- "Med_SNDVI"
      }
      if (length(grep("Med_SRDVI", SVI_File)) > 0) {
        attr(Data, "Index") <- "Med_SRDVI"
      }
      
      # Save the computed Data Frames
      
      print(paste("-> Saving TS info RData file"))
      save(Data, Data_Shape, file = Out_RData)
      
      # Finished Processing. Close message box and perform garbage collection
      gc()
      rm(Data, BAreas_shp)
      gc()
      
    }
  } else {
    print(paste("-> RData file joining MODIS and Shapefile info already existing: ", 
                basename(Out_RData)))
  }
  
  # Completed
  return("DONE")
}