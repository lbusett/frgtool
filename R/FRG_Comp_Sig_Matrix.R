#' @title FRG_Comp_Sig_Matrix
#' 
#' @description Function used to perform the Wilcoxon test of significant differences, using differnt number of "before fire years" as reference \cr
#' The function creates and saves matrixes of p-values obtained using differnt number of before fire years. Matrix is saved within the "p-matrixes" subfolder \cr
#' of the folder containing the final output file. Additional info necessary to plot the time series (i.e., boxplots, objectids of fires, fireyears, ecc) \ cr
#' are also saved in the same file (plot_stat data frame). 
#' 
#' @details
#'
#' @param In_File string File of scaled VI Time Series extracted for single-fire areas
#' @param out_file_pmat string Output file name where the p-values matrix is saved
#' @param min_pix numeric Minimum number of CORE pixels for a bunt area to be considered
#' @param perc_diff numeric Minimum percentage difference checked for significant reduction 
#' @param MedWdt numeric Number of pre-fire years considered as reference
#' @param sub_zones flag If 1 , consider ENV_ZONES differences when analyzing
#' @param sig_level numeric Significance level used for Wilcoxon test 
#' @param erode flag If 1, perform analysis only on NON CORE burnt pixels 
#'
#' @return Saves a p_matrix.RData file containing the p-values matrix and the "plot_stats" data frame in the "p_matrixes" subfolder
#'
#' @author Lorenzo Busetto - email: lorenzo.busetto@@jrc.ec.europa.eu
#' Created Date: Feb 16, 2012
#' @export
#' @importFrom data.table rbindlist


FRG_Comp_Sig_Matrix = function (In_File = In_File , out_file_pmat = out_file_pmat ,min_pix = min_pix, 
                                perc_diff = perc_diff , MedWdt = MedWdt ,sub_zones = sub_zones, erode = erode) {
  
  median_analysis = T
  perc_difference = perc_diff
  
  Out_File = out_file_pmat
  # Load data and define processing options.
  
  Out_File_R = paste(Out_File,'.RData', sep = '')
  Out_File_csv = paste(Out_File,'.csv', sep = '')
  
  #- ---------------------------------------------------- - 
  # Get time series data from the input file ----
  #- ---------------------------------------------------- - 
  
  print(paste('-> Performing Statistical Analysis on Post-Fire SVI Time Series from: ', basename(In_File)))   # Status Message
  load(In_File)
  Index    = attributes(Data)$Index				 ;        SVI_File = attributes(Data)$SVI_File   ;    Shape_File = attributes(Data)$Shape_File
  CSV_File = attributes(Data)$CSV_File     ;    		CLC_File = attributes(Data)$CLC_File
  
  
  # Retrieve time series data
  # Retrieve time series data
  # if(erode == 0) { #  Get data in the case that erode = 0
  #   Data = Data [,1:(8+N_Years)]
  # } else { #  Get data in the case that erode = 1
  #   names = names(Data)[1:(8+N_Years)]    
  #   Data = Data [,c(1:7,(8+N_Years+1):(8+2*N_Years+1))]
  #   names(Data)= names
  # }
  
  # Initialization and Preliminary pre elaborations
  counter = 1
  ptm <- proc.time()
  # Select "Interesting" CLC Classes
  sel_levels = c('Schlerophyllus Vegetation', 'Broadleaved Forests', 'Coniferous Forests',	'Mixed Forests','Transitional Vegetation')  
  Data = drop.levels(subset(Data,CLC_Class %in% sel_levels))		# Remove unneeded CLC classes
  Data$N_PIX = as.factor(paste('p', Data$N_PIX, sep = '_'))	  	# Convert N_PIX to factor and add a "p" before the number
  n_fires = length(unique(Data$OBJECTID))												# Number of records to be analyzed (total number of FIRES in both burned areas shapefile and MODIS ROI )
  
  st_ind = 8   ;    end_ind = length(names(Data))		;  								# Start and end indexes of "years" columns
  
  # Get ancillary data regarding fires from the EFFIS shape file
  
  Data_Shape  = droplevels(subset(Data_Shape,OBJECTID %in% unique(Data$OBJECTID)))			# Remove fires not "present" in the MODIS dataset					
  recs = unique(Data_Shape$OBJECTID)							# Count the remaining fires
  n_recs =length(recs)														# Number of fires to process
  
  # Reshape data structure to facilitate the analyis (melting) and get info about the data (e.g., number of years, ecc)
  Data = Data[complete.cases(Data),]
  Data_Melted = Data
  # Data_Melted = melt(Data, id.vars = c(1:(st_ind-1)))
  # names(Data_Melted)[st_ind:(st_ind+1)] = c('Year', 'Index')
  attributes(Data_Melted)$Index = Index
  # 	Data_Melted = drop.levels(subset(Data_Melted,Year != '2000'))			# If present, remove the data regarding year 2000 (Strong uncertainties in MODIS data of this year !!!!
  
  # Avail_Years = as.numeric(levels(Data_Melted$Year))				# Get list of available years
  # Number of years in the time series	
  Start_Year = min(Data_Melted$Year)															# Starting year of the time serie
  End_Year = max(Data_Melted$Year)  	                            # Ending year of the time serie
  N_Years  =  1 + as.numeric(End_Year) - as.numeric(Start_Year)
  Avail_Years = seq(Start_Year, End_Year, 1)
  #- --------------------------------------------------------------------------- -#
  #- #Initialize output matrixes
  #- --------------------------------------------------------------------------- -#
  
  plot_stat_names = c('CASE_ID','OBJECTID', 'FireYear', 'YearFromFire','Area_All','Area_Forest','Area_CLC','ENV_ZONE','CLC_Class','N_PIX','Time_Signif','Year','YearDiff','low_ci', 'mean','hi_ci','std_dev','low_box', 'X25th','median','X75th','up_box')
  
  # Define the output matrixes that will contain information on p-values of wilcoxon rank sum tests. 
  
  max_plus_years = N_Years - 3						#To compute the limits of the p-values full matrixes: 
  max_minus_years =-(N_Years - 1)				#To compute the limits of the p-values full matrixes
  
  p_matrix_median = NULL ; plot_stat = NULL    # Initialize output variables
  
  p_matrix_median_empty =  array(data = NA, dim = c(max_plus_years-max_minus_years, max_plus_years-max_minus_years))		#Define an empty matrix with correct dimensions to store the results of 
  colnames(p_matrix_median_empty) = seq(max_minus_years,max_plus_years-1, 1 )																		    #the wilcoxon comparisons - using medians 
  rownames(p_matrix_median_empty) = seq(max_minus_years + 1,max_plus_years, 1 )		
  
  #- ---------------------------------------------------- - #
  # Start Analysis on single fires. FirePix is a counter on fires. ? Fires are analyzed ordered by total area according to the EFFIS shapefile ?
  #- ---------------------------------------------------- - #
  
  case_ID = 0 
  
  Data_Melted$YearDiff = (as.numeric(as.character(Data_Melted$Year)) - as.numeric(Data_Melted$FireYear))		# Compute the "Years from fire" variable.
  Data_Melted = data.table(Data_Melted)																														# cONVERT TO A DATA.TABLE to improve speed !!!
  setkey(Data_Melted ,OBJECTID,CLC_Class,ENV_ZONE)																									# Set the "keys" for the table. 
  
  for (FirePix in 1:n_fires){
    
    FireCode = recs[FirePix]																				    # Getrr OBJECTID of the fire to be analyzed
    Data_Fire =droplevels(Data_Melted[J(FireCode)])												# Get Data of the selected fire
    Data_Fire_Shape = subset(Data_Shape, OBJECTID == FireCode)
    
    YearDiffs = (unique(Data_Fire$YearDiff)	)															# get levels of "Difference" with fire year - full and before fire
    YearDiffs_Before = YearDiffs[which(YearDiffs < 0)]
    
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
            
            Data_Class_check <- Data_Class %>% 
              group_by(N_PIX) %>% 
              summarise(lgtyy = length(Index)) %>% 
              filter(lgtyy == N_Years) %>% 
              select(N_PIX)
            Data_Class = subset(Data_Class, Data_Class$N_PIX %in% Data_Class_check$N_PIX) 
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
              
              if (median_analysis == T) {
                
                #- ----------------------------------------------
                # - Creation of the "means" matrix. Replace data of the "before" years with medians computed considering 1 year, 2 years, etcetera. 
                #- ----------------------------------------------	
                
                Data_Class_med = Data_Class
                
                for (Before_Year in YearDiffs_Before) {				# Start of cycle on: 
                  
                  Data_Before = droplevels(Data_Class[YearDiff >= Before_Year & YearDiff < 0])								# Get before fire data up to "Before_Year" CHECK !!!!!!!!!!!
                  mean_before = as.integer(Data_Before[,list(mean=mean(Index, na.rm = T)), by = list(N_PIX)]$mean)      # Compute means per pixels of the before years
                  Data_Class_med[YearDiff == Before_Year]$Index = mean_before    														# Replace the original values with the means of before years
                }    # End of cycle on: 
                
                #- ----------------------------------------------
                # - Statistical analysis - apply wilcoxon test to compute the p-values matrixes - check differences with respect to means of before years
                #- ----------------------------------------------
                p_matrix_median_tmp = p_matrix_median_empty		#Define an empty matrix with correct dimensions to store the results
     
                
                
                paired_wtest_median = try(pairwise.wilcox.test(Data_Class_med$Index, Data_Class_med$YearDiff, 
                                                               paired = TRUE, alternative = 'less', 
                                                               p.adjust.method = 'none', 
                                                               mu = -perc_diff))  # Compute p-values
           
                if(class(paired_wtest_median) == "try-error") {
                  browser()
                } 
                
                #- ----------------------------------------------
                # - Then put results in the cumulated matrix. Assign the "CASE_ID" as dimname of the 3rd dimension 
                # for easy referencing
                #- ----------------------------------------------
                
                p_matrix_median_tmp[attributes(paired_wtest_median$p.value)[2]$dimnames[[1]], 
                                    attributes(paired_wtest_median$p.value)[2]$dimnames[[2]]] = paired_wtest_median$p.value
                
                p_matrix_median = try(abind(p_matrix_median, p_matrix_median_tmp, along = 3))
                if(class(p_matrix_median) == "try-error") {
                  browser()
                } 
                dimnames(p_matrix_median)[[3]][dim(p_matrix_median)[3]] = case_ID
                
              }
              
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
              plot_stat [[counter]]= plot_stat_tmp # Add the results for the selected fire to the full output matrix
              counter = counter + 1
            } 	else {	# If n_pix too low, the fire is skipped. Neither "plot_stat" nor the p-values matrixes are filled
              
            } 
            
          }   # End of cycle on ENV_Zones  for a single fire
          
        }   # End of cycle on CLC classes for a single fire
        
      } else case_ID = case_ID + 1	# End of check for sufficient number of years before and after fire
    }		
    
    if ((FirePix/100)-floor(FirePix/100) == 0) {message('-> Analysing Burnt Area: ',  FirePix, ' of: ', n_fires)}
    
  }   # End of Cycle on Fires
  
  plot_stat <- rbindlist(plot_stat)
  browser()
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
  attr(plot_stat, "Min_Percentage") = perc_diff   
  attr(plot_stat, "Processing_Date") = Sys.Date()   
  attr(plot_stat, "Eroded") = erode  
  
  if (median_analysis == T) {
    # Add attributes to the output Data Frame (Useful to keep track of processing !)
    attr(p_matrix_median, "RData_File") = In_File
    attr(p_matrix_median, "SVI_File") =SVI_File
    attr(p_matrix_median, "Shape_File") = Shape_File	
    attr(p_matrix_median, "CSV_File") = CSV_File
    attr(p_matrix_median, "CLC_File") = CLC_File
    attr(p_matrix_median, "Index") = Index
    attr(p_matrix_median, "Min_Pix") = min_pix   
    attr(p_matrix_median, "Min_Percentage") =perc_diff
    attr(p_matrix_median, "Processing_Date") = Sys.Date()  
    attr(p_matrix_median, "Eroded") = erode  
  }
  
  #save the output in RData format	
  save(plot_stat, p_matrix_median, file = Out_File_R)
  gc(verbose = F)				#Garbage collection
  
  return('DONE')
  
}

#- ----------------------------------------------
#- END OF Function 
#- ----------------------------------------------
