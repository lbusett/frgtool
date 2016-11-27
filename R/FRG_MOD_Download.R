#'@title FRG_MOD_Proc
#'@description Function used to download, mosaic and reproject MODIS data
#'@details Function used to download, mosaic and reproject MODIS data  \cr
#'   \itemize{
#' 		\item Download and Mosaicing: \cr
#'               Images covering Europe for DOYS 209 and 225 of each selected year are downloaded, mosaiced and reprojected \cr
#'               Resulting images are saved in TIFF format in the Originals subfolder of the output folder selected by the user. Separate subfolders are used\cr
#'               to store images related to  NDVI, RED and NIR reflectance, QA (Quality Assurance) and Pixel Reliability.}
#' @param dates string array of dates to be downloaded
#' @param OutOrig_Path string Main folder where the original MODIS mosaics (i.e., one pan european image for each data and data type) will be stored
#' @param ReDown numeric if = 1, MODIS images needed to create already existing mosaic files will be redownloaded, and
#' 				 already existing mosaics will be overwritten
#' @param yy numeric Year considered
#'
#' @return
#'
#'
#' @author Babak Naimi (naimi@r-gis.net)
# (Thanks Tomislav Hengl as his script made the main core of this function [spatial-analyst.net]) Modified by Lorenzo Busetto (2012)
#' email: lorenzo.busetto@@jrc.ec.europa.eu
#'
#' Created Date: Oct 29, 2012
#' @export

FRG_MOD_Download <- function(dates=dates,OutOrig_Path = OutOrig_Path,  ReDown = ReDown, yy = yy) {

  dir.create(OutOrig_Path, showWarnings = FALSE)
  # Set tiles to be downloaded (All combinations will be downloaded)
  h=c(17,18,19,20)
  v=c(1,2,3,4,5)

  FTP =  FRG_Options$MOD_FTP			# Set LDAAC ftp site name
  MRTpath =  FRG_Options$MRTpath				# Set Path to MRT bin folder (Needed for mosaicing)

  # Set bands to be processed - MOD13Q1
  bandnames = 	c('NDVI','EVI',	'QA',	'Red',	'NIR',	'Blue','MIR','Zen',	'Azi',	'RelAz',	'DOY','RELY')				#Band names (from https://lpdaac.usgs.gov/products/modis_products_table)
  bands_subset = c(1,       0,     1,     1,     1,     0,     0,     0,     0,     0,        0,      1)						#Selection of desired bands
  nodatas =         c('-3000','-3000','65535','-1000','-1000','-1000','-1000','-10000','-10000','-4000','-1','-1')	# Set nodata values
  nodata_out = FRG_Options$No_Data_Out_Rast

  # Set info on projection, pixel size and bounding box for reprojected images
  MOD_prj_str = '+proj=sinu +R=6371007.181 +nadgrids=@null +wktext'		# MODIS ISIN projection
  LAEA_prj_str = "+init=epsg:3035 +towgs84=0,0,0,0,0,0,0"						# LAEA projection string
  pixel_size = 250																						# Desired output pixel size
  bbox = c(2600000,1400000,7300000,5450000)										# Bounding box for output mosaic (LAEA coordinates; Xmin, Ymin, Xmax, Ymax)
  ovr = FALSE	; 		sep_mos = TRUE		;	reproj = TRUE	;		mosaic = TRUE   ;  	del = FALSE

  # Check if Mosaic tiff images already exist for the selected year

  chk_files = 0
  for (bandname in bandnames [ bands_subset ==1]) {
    outtiff1 = file.exists( file.path(OutOrig_Path, bandname, 'Single_Dates', paste('Mosaic_',yy,'209','_',bandname,'_LAEA.tif',sep = '')))	# Check name for the 209 TIFF reprojected  mosaic
    outtiff2 = file.exists( file.path(OutOrig_Path, bandname, 'Single_Dates', paste('Mosaic_',yy,'225','_',bandname,'_LAEA.tif',sep = '')))	# Check name for the 225  TIFF reprojected  mosaic
    if (outtiff1 & outtiff2) { chk_files = chk_files + 0  }	else {chk_files = chk_files +1}
  }

  if  (ReDown == 1 | chk_files != 0) {		# Start downloading if not all mosaic files are already present for the year, or ReDownload was selected

    print('--- Checking Files ---')

    # Create a list of the folders containing images to be downloaded (Corresponding to the selected dates)
    dirs <- get_MOD_dirs(FTP = FTP, .Platform = .Platform)
    dirs = get_MOD_dirs_dates(dates = dates, dirs = dirs)
    dir.create(OutOrig_Path, showWarnings = FALSE)
    setwd(OutOrig_Path)				# Change working dir, so to allow easy assignation of filename
    if (length(dirs) < 1) stop("No available data for selected dates")

    # Start Cycling on directories containing images to be downloaded
    for (i in 1:length(dirs)) {

      # Create vector of image names corresponding to the selected tiles
      Modislist = get_MOD_names(FTP = FTP, dirs = dirs, i = i, v = v, h = h)
      # Get the date
      date_name <- sub(sub(pattern="\\.", replacement="_", dirs[i]), pattern="\\.", replacement="_", dirs[i])	#Create the date string
      # Start cycle for downloading the images in Modislist vector
      if (length(Modislist) > 0) {
        for (ModisName in Modislist) {
          if (file.exists(ModisName)  == FALSE | ovr == TRUE ) {
            er <- 5		; 	class(er) <- "try-error" ;	ce <- 0
            while(er != 0) {
              print(paste('Downloading File: ', ModisName ))
              print(paste('--- Downloading Files for date', date_name, ':' ,which(Modislist == ModisName),' of ', length(Modislist),' ---'))    # Update progress window

              er <- tryCatch(download.file(url=paste(FTP,dirs[i], "/",ModisName,sep=''),destfile=ModisName,mode='wb',quiet=T, cacheOK=FALSE),  # Download
                             warning=function(war) {
                               print(war)
                               return (1)
                             }, error =function(err) {
                               print(err)
                               return (1)
                             } )
#               down_str = paste("c:/aria32/aria2c.exe -x10 -s10 --min-split-size=1048576 -d",OutOrig_Path, paste(FTP,dirs[i], "/",ModisName,sep=''),sep = " " )
#               er <- system(down_str)

              # On download error, wait 10 seconds and retry for a maximum of 21 times
              if (er != 0) {
                Sys.sleep(10)
                ce <- ce + 1
                print(paste('retries: ', ce))
                if (ce == 21) stop("Error: FTP server is down!!")			# Stop after 21 failed attempts
              }
            }

          }
        }  # End cycle for downloading the images in Modislist vector
        print(paste(length(Modislist)," files for date of ",dirs[i]," were successfully downloaded!",sep=''))

        # Create the parameter file for MRT mosaic function

        if (length(Modislist) > 1){
          mosaicname = file(paste(MRTpath, "/TmpMosaic.prm", sep=""), open="wt")
          write(paste(OutOrig_Path,"/",Modislist[1], sep=""), mosaicname)
          for (j in 2:length(Modislist)) write(paste(getwd(),"/",Modislist[j], sep=""),mosaicname,append=T)
          close(mosaicname)

          # Run the MRT tool to generate the mosaic HDFs. One separate HDF is generated for each selected band
          # Added by L.Busetto --- Instead of a single mosaic one different mosaic for each selected band will be created.
          # This is useful in the case of very large mosaics !

          for (band in 1:length(bands_subset)) {														# Cycle on MODIS Bands
            bands = numeric(length(bands_subset))													# Create vector with length = bands, filled with zeroes
            er_mos = 1    ; er_rep = 1
            if (bands_subset[band] == 1) {

              print(paste('--- Mosaicing ', bandnames[band],'files for date: ',date_name ,' ---'))
              bands[band]=1																					# IF band selected for processing, put its value to 1
              dir.create(file.path(OutOrig_Path, bandnames[band], 'Single_Dates'), showWarnings = F, recursive = T)
              bands = paste(as.character(bands), collapse = '', sep = ' ')					# Convert to character
              outfile = paste(OutOrig_Path, '/Mosaic_',substr(Modislist[1],10,16),'_',bandnames[band],'.hdf', sep = '')		# Create name for the HDF mosaic
              outtiff = file.path(OutOrig_Path, bandnames[band], 'Single_Dates',paste(sub("[.][^.]*$", "", basename(outfile), perl=TRUE),'_LAEA.tif',sep = ''))  # Create name for the TIFF reprojected  mosaic
              er_mos <- system(paste(MRTpath, '/mrtmosaic -i ', MRTpath, '/TmpMosaic.prm' ,' -o ', outfile,' -s ',bands, sep=""))	# Launche MRT to create the mosaic

              if (er_mos != 0) {stop("Error occurred while mosaicing !!")		}

              # If reprojection requested, convert to output projection using gdalwarp
              if (reproj == T) {

                print (paste('--- Reprojecting ', bandnames[band],'files for date: ',date_name ))
                if (file.exists(outtiff) == T) {unlink(outtiff)}

                # If bounding box was passed, the output reproject file will satisfy the bbox
                if (length(bbox) == 4) {
                  er_rep = 	system(paste('gdalwarp -s_srs "',MOD_prj_str, 	# Launch GDAL to crete the reprojected TIFF
                                         '" -t_srs "', LAEA_prj_str, '" -of GTiff -r near -co compress=lzw',
                                         '-te',bbox[1],bbox[2],bbox[3],bbox[4], '-tr ',pixel_size, pixel_size,
                                         '-wo "INIT_DEST=NO_DATA"', '-wm 500 ',
                                         '-srcnodata ', nodatas[band],'-dstnodata ', nodata_out,
                                         outfile,  outtiff,
                                         sep = ' '))
                if (er_rep != 0) {
                    print('An error occurred while reprojecting MODIS ! Processing stopped')
                  stop()
                }

                }
                else {						# If bounding box was not passed, keep the original extent when creating the TIFF
                  er_rep = 	system(paste('gdalwarp -s_srs "',MOD_prj_str,
                                         '" -t_srs "', LAEA_prj_str,
                                         '" -of GTiff -r near -co compress=lzw',
                                         '-tr ',pixel_size, pixel_size,
                                         '-wo "INIT_DEST=NO_DATA"','-wm 500 ',
                                         '-srcnodata ',  nodata_out,
                                         '-dstnodata -999',
                                         outfile,  outtiff,
                                         sep = ' '))
                  if (er_rep != 0) {
                    print('An error occurred while reprojecting MODIS ! Processing stopped')
                  stop()
                }
                }
              }
              if (er_rep != 0) stop("An Error occurred while mosaicing !!")
              unlink(outfile)							# Delete un-reprojected mosaiced HDF file
            }  # ENDIF band selected for processing
          }	# END Cycle on selected MODIS Bands
        }
        if (del == T) {for (ModisName in Modislist) unlink(paste(getwd(), '/', ModisName, sep=""))}		# Delete original downloaded HDFs

      } # ENDIF on lenght of Modislist

      else {print(paste("No available image for selected Tiles in ",dirs[i], sep=""))}

    }	# End Cycling on directories containing images to be downloaded (i.e., dates)

  }  # End on IF for existing files - ReDownload

  print('--- Download, mosaicing and reprojection complete ! ---')

  return('DONE')
}

# ----- End of Main Function -------------#

# Accessory function to get the full list of directories on the http site (modified from Barry Rowlingson functions):
get_MOD_dirs <- function(FTP, .Platform) {

  print('Retrieving MODIS folders')
  if (strsplit(FTP,'')[[1]][length(strsplit(FTP,'')[[1]])] != "/") FTP <- paste(FTP,"/",sep="")
  if (.Platform$OS.type=="unix") options('download.file.method'='wget')	else options('download.file.method'='auto')
  items <- 0
  class(items) <- "try-error"
  ce <- 0
  while(class(items) == "try-error") {
    items <- try(strsplit(getURL(FTP), "\r*\n")[[1]],silent=TRUE)
    if (class(items) == "try-error") {
      Sys.sleep(10)
      ce <- ce + 1
      if (ce == 200) stop("Error: FTP server is down!!")
    }
  }
  items <- items[-1]
  # get the directory names (available dates)
  dirs <- unlist(lapply(strsplit(items, ">"), function(x){x[length(x)-1]}))
  dirs = dirs[seq(3,length(dirs)-2)]
  dirs <- unlist(lapply(strsplit(dirs, "/"), function(x){x[1]}))
  return(dirs)
}

# Accessory function to find the folders corresponding to the selected dates period
get_MOD_dirs_dates <- function(dates, dirs) {

  if (length(dates) > 1) {
    start.date <- strsplit(dates[1],'\\.')[[1]]
    end.date <- strsplit(dates[2],'\\.')[[1]]
    wr <- c()
    for (i in 1:length(dirs)) {
      d <- unlist(strsplit(dirs[i],"\\."))
      if (length(d) == 3)
        if (as.numeric(d[1]) >= as.numeric(start.date[1]) & as.numeric(d[1]) <= as.numeric(end.date[1]) ) wr <- c(wr,i)
    }
    if (length(wr) > 0) dirs <- dirs[wr]
    wr <- c()
    for (i in 1:length(dirs)) {
      d <- unlist(strsplit(dirs[i],"\\."))
      if (as.numeric(d[2]) < as.numeric(start.date[2]) & as.numeric(d[1]) == as.numeric(start.date[1])) wr <- c(wr,i)
      if (as.numeric(d[2]) > as.numeric(end.date[2]) & as.numeric(d[1]) == as.numeric(end.date[1])) wr <- c(wr,i)
    }
    if (length(wr) > 0) dirs <- dirs[-wr]
    wr <- c()
    for (i in 1:length(dirs)) {
      d <- unlist(strsplit(dirs[i],"\\."))
      if (as.numeric(d[3]) < as.numeric(start.date[3]) & as.numeric(d[1]) == as.numeric(start.date[1]) & as.numeric(d[2]) == as.numeric(start.date[2])) wr <- c(wr,i)
      if (as.numeric(d[3]) > as.numeric(end.date[3]) & as.numeric(d[1]) == as.numeric(end.date[1]) & as.numeric(d[2]) == as.numeric(end.date[2])) wr <- c(wr,i)
    }
    if (length(wr) > 0) dirs <- dirs[-wr]
  } else dirs <- dirs[which(dirs == dates[1])]
  print('Retrieving MODIS folders: DONE')
  return(dirs)
}

# Accessory function to find the image names corresponding to the selected dates period and tiles
get_MOD_names <- function(FTP, dirs, i, v, h) {
  print('Retrieving MODIS Names ')
  getlist <- 0
  class(getlist) <- "try-error"
  ce <- 0
  while(class(getlist) == "try-error") {
    getlist <- try(strsplit(getURL(paste(FTP,dirs[i], "/", sep="")), "\r*\n")[[1]],silent=TRUE)
    if (class(getlist) == "try-error") {
      Sys.sleep(5)
      ce <- ce + 1
      if (ce == 21) stop("Error: FTP server is down!!")
    }
  }
  getlist <- getlist[-1]
  getlist <- unlist(lapply(strsplit(getlist, ">"), function(x){x[length(x)-1]}))
  getlist = getlist[seq(3,length(getlist)-2)]
  getlist <- unlist(lapply(strsplit(getlist, "<"), function(x){x[1]}))
  # 	getlist <- unlist(lapply(strsplit(getlist, " "), function(x){x[length(x)]}))
  Modislist <- c()
  for (vv in v) {
    for (hh in h) {
      if (vv < 10) vc <- paste('0',as.character(vv),sep='')
      else vc <- as.character(vv)
      if (hh < 10) hc <- paste('0',as.character(hh),sep='')
      else hc <- as.character(hh)
      ModisName <- grep(".hdf$",grep(paste('h',hc,'v',vc,sep=''),getlist,value=TRUE),value=TRUE)
      if (length(ModisName) == 1) Modislist <- c(Modislist,ModisName)
    }
  }

  print('Retrieving MODIS Names : DONE')
  return(Modislist)
}
