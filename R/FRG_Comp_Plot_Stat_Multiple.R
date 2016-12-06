#' FRG_Comp_Plot_Stat_Multiple 
#' 
#' @description Function used extract the info necessary to plot the NDVIRtime series (i.e., boxplots, overlapid of BA, fireyears, ecc) \ cr
#' for multiple-fire areas
#' 
#' @details
#'
#' @param In_File string File of scaled VI Time Series extracted for multiple-fire areas
#' @param Out_File string Basename for output files
#' @param min_pix numeric MInimum number of CORE pixels for a bunt area to be considered
#' @param sub_zones flag If 1 , consider ENV_ZONES differences when analyzing
#' @param erode flag If 1, perform analysis only on NON CORE burnt pixels 
#'
#' @return Saves a .RData file containing data needed to plot NDVIR time series for the different multiple-fire BAs
#'
#' @author Lorenzo Busetto - email: lorenzo.busetto@@jrc.ec.europa.eu
#' Created Date: Feb 16, 2012
#' @export
#' 


FRG_Comp_Plot_Stat_Multiple = function (In_File = In_File , Out_File = Out_File ,min_pix = min_pix, sub_zones = sub_zones, erode = erode) {
  
  # Load data and define processing options.
  
  Out_File_R = Out_File
    
  #- ---------------------------------------------------- - 
  # Get time series data from the input file ----
  #- ---------------------------------------------------- - 
  
  print(paste('-> Extracting time series statistics from file: ', basename(In_File)))   # Status Message
  load(In_File)
  Index = attributes(Data)$Index  			;        SVI_File = attributes(Data)$SVI_File   ;    Shape_File = attributes(Data)$Shape_File
  CSV_File = attributes(Data)$CSV_File     ;    		CLC_File = attributes(Data)$CLC_File
  N_Years = as.numeric(attributes(Data)$End_Year)-as.numeric(attributes(Data)$Start_Year)
  
  # Retrieve time series data
  # if(erode == 0) { #  Get data in the case that erode = 0
  #   Data = Data [,1:(8+N_Years)]
  # } else { #  Get data in the case that erode = 1
  #   names = names(Data)[1:(8+N_Years)]    
  #   Data = Data [,c(1:7,(8+N_Years+1):(8+2*N_Years+1))]
  #   names(Data)= names
  # }
  
  # Initialization and Preliminary pre elaborations
  
  ptm <- proc.time()
  # Select "Interesting" CLC Classes
  sel_levels = c('Schlerophyllus Vegetation', 'Broadleaved Forests', 'Coniferous Forests',	'Mixed Forests','Transitional Vegetation')  
  Data = drop.levels(subset(Data,CLC_Class %in% sel_levels))		# Remove unneeded CLC classes
  Data$N_PIX = as.factor(paste('p', Data$N_PIX, sep = '_'))	  	# Convert N_PIX to factor and add a "p" before the number
  n_fires = length(unique(Data$OVERLAP_ID))												# Number of records to be analyzed (total number of FIRES in both burned areas shapefile and MODIS ROI )
  st_ind = 8   ;    end_ind = length(names(Data))		;  								# Start and end indexes of "years" columns
  
  # Get ancillary data regarding fires from the EFFIS shape file
  
  Data_Shape  = droplevels(subset(Data_Shape,OVERLAP_ID %in% unique(Data$OVERLAP_ID)))			# Remove fires not "present" in the MODIS dataset					
  recs = unique(Data_Shape$OVERLAP_ID)							# Count the remaining fires
  n_recs =length(recs)														# Number of fires to process
  
  # Reshape data structure to facilitate the analyis (melting) and get info about the data (e.g., number of years, ecc)
  Data = Data[complete.cases(Data),]
  Data_Melted = melt(Data, id.vars = c(1:(st_ind-1)))
  names(Data_Melted)[st_ind:(st_ind+1)] = c('Year', 'Index')
  attributes(Data_Melted)$Index = Index
  # 	Data_Melted = drop.levels(subset(Data_Melted,Year != '2000'))			# If present, remove the data regarding year 2000 (Strong uncertainties in MODIS data of this year !!!!
  
  Avail_Years = as.numeric(levels(Data_Melted$Year))				# Get list of available years
  n_Years = length(Avail_Years)															# Number of years in the time series	
  Start_Year = min(Avail_Years)															# Starting year of the time serie
  End_Year = max(Avail_Years)  	                            # Ending year of the time serie
  
  #- --------------------------------------------------------------------------- -#
  #- #Initialize output matrixes
  #- --------------------------------------------------------------------------- -#
  
  plot_stat_names = c('CASE_ID','OVERLAP_ID', 'FireYear', 'YearFromFire','Area_All','Area_Forest','Area_CLC','ENV_ZONE','CLC_Class','N_PIX','Time_Signif','Year','YearDiff','low_ci', 'mean','hi_ci','std_dev','low_box', 'X25th','median','X75th','up_box')
  plot_stat = NULL    # Initialize output variables
  #- ---------------------------------------------------- - #
  # Start Analysis on single fires. FirePix is a counter on fires. ? Fires are analyzed ordered by total area according to the EFFIS shapefile ?
  #- ---------------------------------------------------- - #
  
  case_ID = 0 
  
  Data_Melted$YearDiff = (as.numeric(as.character(Data_Melted$Year)) - as.numeric(Data_Melted$FireYear))		# Compute the "Years from fire" variable.
  Data_Melted = data.table(Data_Melted)																														# cONVERT TO A DATA.TABLE to improve speed !!!
  setkey(Data_Melted ,OVERLAP_ID,CLC_Class,ENV_ZONE)																									# Set the "keys" for the table. 
  
  for (FirePix in 1:n_fires){
    
    FireCode = recs[FirePix]																				    # Getrr OBJECTID of the fire to be analyzed
    Data_Fire =droplevels(Data_Melted[J(FireCode)])												# Get Data of the selected fire
    Data_Fire_Shape = subset(Data_Shape, OVERLAP_ID == FireCode)
    
    YearDiffs = (unique(Data_Fire$YearDiff)	)															# get levels of "Difference" with fire year - full and before fire
    YearDiffs_Before =YearDiffs[which(YearDiffs < 0)]
    
    if (length(Data_Fire$FireYear) != 0 & is.finite(Data_Fire$FireYear[1])) {   #control on number of records for selected fire
      FireYear =  Data_Fire$FireYear[1]																	# Get year of occurrence of the selected fire		
      Area_All = Data_Fire$Area_All[1]                                   # Get total burnt Area
      Area_Forest =  Data_Fire$Area_Forest[1]                           # Get total in forest CLC types
      YearFromFire = Start_Year - FireYear														  # Compute years from fire at the beginning of the time serie for the selected fire 	
      
      # Limit the analysis to fires occurred at least three years later than the start of the time series AND at least one year before its end 
      if ((FireYear - Start_Year >= 3) & (FireYear < End_Year )) {		
        
        CLC_classes = c(levels(Data_Fire$CLC_Class),'All')						# Get array of CLC classes present in the selected fire Fire
        ENV_Zones = c(levels(Data_Fire$ENV_ZONE), 'All')					# Get Array of ENV_ZONES present in the selected fire
        
        if (sub_zones == 0) {ENV_Zones = 'All'} else {ENV_Zones = c(levels(Data_Fire$ENV_ZONE), 'All')}
        # Start cyclying on ENV_Zones "available" in the selected fire
        
        for (Zone in ENV_Zones) {	
          if (Zone != 'All') {Data_Zone =droplevels(Data_Fire[ENV_ZONE == Zone])	} else  {Data_Zone = Data_Fire}
          
          # Start cyclying on CLC classes "available" in the selected fire 
          
          for (Class in CLC_classes) {
            
            if (Class != 'All') {Data_Class =droplevels(Data_Zone[CLC_Class == Class])	} else  {Data_Class = Data_Zone} 
            
            n_pix= length(unique(Data_Class$N_PIX))		# Check to see if at least min_pix pixels are "available" for the selected CLC class and Zone in the selected fire
            case_ID = case_ID + 1
            if (n_pix >= min_pix) { # if number of pixels grater than minimum , perform analysis 
              
              # Compute the "Area_CLC" class, i.e., the area burnt in each CLC class for the analyzed fire
              if (Class =='All' ) {Area_CLC = Area_Forest}
              if (Class =='Broadleaved Forests' ) {Area_CLC = Data_Fire_Shape[['BroadLeave']]}
              if (Class =='Coniferous Forests' )  {Area_CLC = Data_Fire_Shape[['Coniferous']]}
              if (Class =='Mixed Forests' )       {Area_CLC = Data_Fire_Shape[['MixedFores']]}
              if (Class =='Transitional Vegetation' ) {Area_CLC = Data_Fire_Shape[['Transition']]}
              if (Class =='Schlerophyllus Vegetation' ) {Area_CLC = Data_Fire_Shape[['Sclerophyl']]}
              
              #- --------------------------------------------------------------------------- -#
              #- Compute data to be saved to allow plotting (CIs + boxplot thresholds
              #- --------------------------------------------------------------------------- -#
              
              # Compute confidence intervals using the method from Morey (2008) (http://wiki.stdout.org/rcookbook/Graphs/Plotting%20means%20and%20error%20bars%20%28ggplot2%29/)
              #							ci_data = summarySEwithin(Data_Class, measurevar ='Index', withinvars = 'Year', idvar = 'N_PIX', na.rm = FALSE, conf.interval = .95)
              #							ci_data = c(NA,NA,NA)
              #							int_data = cbind(ci_data$Index - ci_data$ci,ci_data$Index, ci_data$Index+ci_data$ci) 
              int_data = array(999, dim =c(length(Avail_Years),3) )
              # Compute quantiles of distribution for different years
              
              box_data = t(boxplot(Index~YearDiff, data = Data_Class, plot = FALSE)$stats)
              sdev_data = Data_Class[, list(sd= sd(Index, na.rm=TRUE)), by = list(YearDiff)]
              
              # Fill-in the output matrix for the analyzed fire.
              
              plot_stat_tmp= as.data.frame(cbind(case_ID, FireCode,FireYear, YearFromFire,Area_All,Area_Forest,Area_CLC, as.character(Zone),as.character(Class),n_pix,  Time_Signif = 'Yes', Avail_Years,YearDiffs, int_data,sdev_data$sd, box_data), stringsAsFactors = FALSE)
              names(plot_stat_tmp) = plot_stat_names
              plot_stat = rbind(plot_stat, plot_stat_tmp) # Add the results for the selected fire to the full output matrix
            } 	else {	# If n_pix too low, the fire is skipped. Neither "plot_stat" nor the p-values matrixes are filled
              
            } 
            
          }   # End of cycle on ENV_Zones  for a single fire
          
        }   # End of cycle on CLC classes for a single fire
        
      } else case_ID = case_ID + 1	# End of check for sufficient number of years before and after fire
    }		
    
    if ((FirePix/50)-floor(FirePix/50) == 0) {print(paste('-> Analysing Burnt Area: ',  FirePix, ' of: ', n_fires, sep = ''))}
    
  }   # End of Cycle on Fires
  
  #Convert the columns to the correct formats
  for (cc_df in c(5,6,7,10, 14:22)) plot_stat [,cc_df] = as.numeric(plot_stat [,cc_df])
  for (cc_df in c(2,3,8,9,11)) plot_stat [,cc_df] = as.factor(plot_stat [,cc_df])
  for (cc_df in c(1,4,12,13)) plot_stat [,cc_df] = as.ordered(as.numeric(plot_stat [,cc_df]))				  
  
  # Add attributes to the output Data Frame (Useful to keep track of processing !)
  
  attr(plot_stat, "RData_File") = In_File
  attr(plot_stat, "SVI_File") =SVI_File
  attr(plot_stat, "Shape_File") = Shape_File	
  attr(plot_stat, "CSV_File") = CSV_File
  attr(plot_stat, "CLC_File") = CLC_File
  attr(plot_stat, "Index") = Index
  attr(plot_stat, "Min_Pix") = min_pix   
  attr(plot_stat, "Processing_Date") = Sys.Date()   
  attr(plot_stat, "Eroded") = erode  
  
  #save the output in RData format	
  save(plot_stat, file = Out_File_R)
  gc(verbose = F)				#Garbage collection
  
  return('DONE')
  
}
