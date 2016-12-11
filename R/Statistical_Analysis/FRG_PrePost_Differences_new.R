
#- ------------------------------------------------------------------------------- #
# - Start of Main Function for computation of pre-post differences for each burnt area
#- ------------------------------------------------------------------------------- #
# #
# 
# RData_Files = c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/NDVI/TS_Extraction_NDVI_2000_2012_erode_RData.RData',
#                 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/RDVI/TS_Extraction_RDVI_2000_2012_erode_RData.RData',
#                 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Tests/50/Med_SNDVI/TS_Extraction/TS_Extraction_Med_SNDVI_2000_2012_META_RData.RData',
#                 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Tests/50/Med_SRDVI/TS_Extraction/TS_Extraction_Med_SRDVI_2000_2012_META_RData.RData')
# 
# Out_Files = c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/NDVI/NDVI_PrePost_Differences_new.RData',
#               'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/RDVI/RDVI_PrePost_Differences_new.RData',
#               'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/NDVI/SNDVI_PrePost_Differences_new.RData',
#               'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/RDVI/SRDVI_PrePost_Differences_new.RData')

RData_Files = c('H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Prefire_Analysis/NDVI/TS_Extraction_NDVI_2000_2012_RData.RData',
                'H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Med_SNDVI/TS_Extraction/Burned_Once/TS_Extraction_Med_SNDVI_2000_2012_META_RData.RData'
                )

Out_Files = c('H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Prefire_Analysis/NDVI/NDVI_PrePost_Difference.RData',
              'H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Prefire_Analysis/NDVI/SNDVI_PrePost_Differences.RData'
              )
              

# Data_Shape_File  = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012_old/Stability_Analysis/Data_Shape.RData'

# Out_Files = c('E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/R-FRG/Results/Med_SNDVI',
# 		'E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/R-FRG/Results/Med_SDVI'
# 
# #	'E:/busetlo/Documents/Projects/Fire_Regeneration/Source_Code/R-FRG/Results/temp_matrix_10%_median_prova'
# 
for  (file in seq(1,2)) {
  In_File = RData_Files[file]
  Out_File = Out_Files[file]
  out = FRG_Comp_DeltaIndexes(In_File, Out_File,10,10,3,0,1)	
}


FRG_Comp_DeltaIndexes = function (In_File, Out_File,min_pix, perc_diff,MedWdt, sub_zones,erode  ) {
  
  median_analysis = T
  before_analysis = F
  standard_analysis = F
  # 	Full_Years_Analysis = T
  perc_difference = perc_diff
  
  
  # Load data and define processing options.
  
  # 	Out_File_R = paste(Out_File,'.RData', sep = '')
  
  #	mess_lab = glabel(paste('Performing Statistical Analysis on Post-Fire SVI Time Series from: ', basename(In_File)), editable = FALSE, container = mess)   # Status Message
  
  print(paste('-> Performing Statistical Analysis on Post-Fire SVI Time Series from: ', basename(In_File)))   # Status Message
  # 	browser()
  load(In_File)
  Index = attributes(Data)$Index				;        SVI_File = attributes(Data)$SVI_File   ;    Shape_File = attributes(Data)$Shape_File
  CSV_File = attributes(Data)$CSV_File     ;    		CLC_File = attributes(Data)$CLC_File
  N_Years = as.numeric(attributes(Data)$End_Year)-as.numeric(attributes(Data)$Start_Year)
  N_Years = 12
  if(erode == 0) {
    Data = Data [,1:(7+N_Years)]
  } else {
    #   browser()
    names = names(Data)[1:(8+N_Years)]  
    Data = Data [,c(1:7,(8+N_Years+1):(8+2*N_Years+1))]
    names(Data)= names
  }
  # 	    Data_Eroded = Data [,c(1:4,18:30)]
  
  #   Data = Data [,1:(7+N_Years)]    # SNDVI and SRDVI
  
  # Initialization and Preliminary pre elaborations
  
  ptm <- proc.time()
  
  # 	sel_levels = c('Schlerophyllus Vegetation', 'Broadleaved Forests', 'Coniferous Forests',	'Mixed Forests','Transitional Vegetation','Other Natural Land')  
  
  sel_levels = c('Schlerophyllus Vegetation', 'Broadleaved Forests', 'Coniferous Forests',	'Mixed Forests','Transitional Vegetation')  
  
  Data = drop.levels(subset(Data,CLC_Class %in% sel_levels))						# Remove unneeded CLC classes
  Data$N_PIX = as.factor(paste('p', Data$N_PIX, sep = '_'))		# Convert N_PIX to factor and add a "p" before the number
  n_fires = length(unique(Data$OBJECTID))												# Number of records to be analyzed (total number of FIRES in both burned areas shapefile and MODIS ROI )
  
  # 	Data <- Data[,c(1:4,ncol(Data)-1, ncol(Data),5:(ncol(Data)-2))]																	# Reorder Columns
  # 	st_ind = 5   ;    end_ind = length(names(Data))		;  								# Start and end indexes of "years" columns
  
  st_ind =8   ;    end_ind = length(names(Data))  	;  								# Start and end indexes of "years" columns
  
#   browser()
  
  # Get ancillary data regarding fires from the EFFIS shape file
  
  Data_Shape  = droplevels(subset(Data_Shape,OBJECTID %in% unique(Data$OBJECTID)))			# Remove fires not "present" in the MODIS dataset					
  recs = unique(Data_Shape$OBJECTID)							# Count the remaining fires
  n_recs =length(recs)														# Number of fires to process
#   browser()
  # Reshape data structure to facilitate the analyis (melting) and get info about the data (e.g., number of years, ecc)
  Data = Data[complete.cases(Data),]
  # 	Data_Melted = melt(Data, id.vars = c(1:4,18))
  
  Data_Melted = melt(Data, id.vars = c(1:7))
  # 	names(Data_Melted)[6:7] = c('Year', 'Index')
  names(Data_Melted)[8:9] = c('Year', 'Index')
  attributes(Data_Melted)$Index = Index
  # 	Data_Melted = drop.levels(subset(Data_Melted,Year != '2000'))			# If present, remove the data regarding year 2000 (Strong uncertainties in MODIS data of this year !!!!
  
  Avail_Years = as.numeric(levels(Data_Melted$Year))							# Get list of available years
  n_Years = length(Avail_Years)															# Number of years in the time series	
  Start_Year = min(Avail_Years)															# Starting year of the time serie
  End_Year = max(Avail_Years)
  #   print(End_Year)
  #   browser()
  
  #- --------------------------------------------------------------------------- -#
  #- #Initialize output matrixes
  #- --------------------------------------------------------------------------- -#
  
  out_diff_names = c('CASE_ID','OBJECTID', 'FireYear', 'YearFromFire','ENV_ZONE','CLC_Class','N_PIX',
                     'Bef_100','Bef_90','Bef_75','Bef_50','Bef_25','Bef_10','Bef_0','Aft_100','Aft_90','Aft_75','Aft_50','Aft_25','Aft_10','Aft_0',
                     'Dif_100','Dif_90','Dif_75','Dif_50','Dif_25','Dif_10','Dif_0')
  
  out_diff = NULL
  
  #- ---------------------------------------------------- - #
  # Start Analysis on single fires. FirePix is a counter on fires. ? Fires are analyzed ordered by total area according to the EFFIS shapefile ?
  #- ---------------------------------------------------- - #
  
  case_ID = 0 
  
  Data_Melted$YearDiff = (as.numeric(as.character(Data_Melted$Year)) - as.numeric(Data_Melted$FireYear))		# Compute the "Years from fire" variable.
  #	browser()
  Data_Melted = data.table(Data_Melted)																														# cONVERT TO A DATA.TABLE to improve speed !!!
  setkey(Data_Melted ,OBJECTID,CLC_Class,ENV_ZONE)																									# Set the "keys" for the table. 
  
  for (FirePix in 1:n_fires){
    #  		for (FirePix in 1:100){		
    
    FireCode = recs[FirePix]																				    # Getrr OBJECTID of the fire to be analyzed
    Data_Fire =droplevels(Data_Melted[J(FireCode)])												# Get Data of the selected fire
    Data_Fire_Shape = subset(Data_Shape, OBJECTID == FireCode)
    
    YearDiffs = (unique(Data_Fire$YearDiff)	)															# get levels of "Difference" with fire year - full and before fire
    
    if (length(Data_Fire$FireYear) != 0 & is.finite(Data_Fire$FireYear[1])) {   #control on number of records for selected fire
      FireYear =  Data_Fire$FireYear[1]																	# Get year of occurrence of the selected fire		
      YearFromFire = Start_Year - FireYear														    # Compute years from fire at the beginning of the time serie for the selected fire 	
      
      # Limit the analysis to fires occurred at least three years later than the start of the time series AND at least one year before its end 
      if ((FireYear - Start_Year >= 3) & (FireYear < End_Year )) {		
        
        CLC_classes = c(levels(Data_Fire$CLC_Class),'All')						# Get array of CLC classes present in the selected fire Fire
        ENV_Zones = c(levels(Data_Fire$ENV_ZONE), 'All')					# Get Array of ENV_ZONES present in the selected fire
        
        if (sub_zones == 0) {ENV_Zones = 'All'} else {ENV_Zones = c(levels(Data_Fire$ENV_ZONE), 'All')}
        # Start cyclying on ENV_Zones "available" in the selected fire
        
        for (Zone in ENV_Zones) {	
          if (Zone != 'All') {Data_Zone =droplevels(Data_Fire[ENV_ZONE == Zone])	} else  {Data_Zone = Data_Fire}
          # 					browser()
          # Start cyclying on CLC classes "available" in the selected fire 
          
          for (Class in CLC_classes) {
            
            if (Class != 'All') {Data_Class =droplevels(Data_Zone[CLC_Class == Class])	} else  {Data_Class = Data_Zone} 
            
            n_pix= length(unique(Data_Class$N_PIX))		# Check to see if at least min_pix pixels are "available" for the selected CLC class and Zone in the selected fire
            case_ID = case_ID + 1
            if (n_pix >= min_pix) {
              
#                 Data_Before = quantile(subset(Data_Class,YearDiff<0 & YearDiff >=-3)$Index, na.rm = T, probs = c(0,0.1,0.25,0.5,0.75,0.9,1.0))
#               Data_After = min(median(subset(Data_Class,YearDiff==0)$Index, na.rm = T), median(subset(Data_Class,YearDiff==1)$Index, na.rm = T))
#               Data_Diff = Data_After-Data_Before
#               
#               Data_Before = median(subset(Data_Class,YearDiff<0 & YearDiff >=-3)$Index, na.rm = T)
#               Data_After = min(median(subset(Data_Class,YearDiff==0)$Index, na.rm = T), mean(subset(Data_Class,YearDiff==1)$Index, na.rm = T))
#               Data_Diff = Data_After-Data_Before
#                 
                
              # Compute values: Data_Before = mean of the three years before fire; Data_After: minimum between FireYear and FireYear +1
              # Data_Diff: Difference between the two. 
              
              mean_before = ddply(subset(Data_Class,YearDiff<0 & YearDiff >=-3), .(N_PIX), summarise, mean = mean(Index,na.rm = T))
              Data_Before = matrix(quantile(mean_before$mean, na.rm = T, probs = c(0,0.1,0.25,0.5,0.75,0.9,1.0)),nrow=1)
              
#               Data_After = matrix(pmin(quantile(subset(Data_Class,YearDiff== 0)$Index, na.rm = T, probs = c(0,0.1,0.25,0.5,0.75,0.9,1.0)), 
#                                quantile(subset(Data_Class,YearDiff== 1)$Index, na.rm = T, probs = c(0,0.1,0.25,0.5,0.75,0.9,1.0))
#                                ),nrow=1)
              mean_0 = median (subset(Data_Class,YearDiff== 0)$Index, na.rm = T)    ;  mean_1 = median (subset(Data_Class,YearDiff== 1)$Index, na.rm = T)
              
              # Old formulation (Yf or Yf +1)
#               if (mean_0 <mean_1) {
#                 Data_After = matrix(quantile(subset(Data_Class,YearDiff== 0)$Index, na.rm = T, probs = c(0,0.1,0.25,0.5,0.75,0.9,1.0)),nrow =1)
#                 diff = subset(Data_Class,YearDiff== 0)$Index-mean_before$mean
#                 } else {
#                 Data_After = matrix(quantile(subset(Data_Class,YearDiff== 1)$Index, na.rm = T, probs = c(0,0.1,0.25,0.5,0.75,0.9,1.0)),nrow =1)
#                 diff = subset(Data_Class,YearDiff== 1)$Index-mean_before$mean
#                 }
              
              # New formulation (Always Yf +1)
              
               Data_After = matrix(quantile(subset(Data_Class,YearDiff== 1)$Index, na.rm = T, probs = c(0,0.1,0.25,0.5,0.75,0.9,1.0)),nrow =1)
               diff = subset(Data_Class,YearDiff== 1)$Index-mean_before$mean
              
              
              Data_Diff = matrix(quantile(diff, probs = c(0,0.1,0.25,0.5,0.75,0.9,1.0)),nrow =1)
              colnames(Data_Before)=c('Bef_100','Bef_90','Bef_75','Bef_50','Bef_25','Bef_10','Bef_0')
              colnames(Data_After)=c('Aft_100','Aft_90','Aft_75','Aft_50','Aft_25','Aft_10','Aft_0')
              colnames(Data_Diff)=c('Dif_100','Dif_90','Dif_75','Dif_50','Dif_25','Dif_10','Dif_0')
#               browser()
              # Put data in the output
              out_diff_tmp= as.data.frame(cbind(case_ID,FireCode,FireYear,YearFromFire,as.character(Zone),as.character(Class),n_pix, 
                                       Data_Before,Data_After, Data_Diff), stringsAsFactors = FALSE)
              
              names(out_diff_tmp) = out_diff_names
              out_diff = rbind(out_diff, out_diff_tmp) # Add the results for the selected fire to the full output matrix
            } 	else {	# If n_pix too low, the fire is skipped. Neither "plot_stat" nor the p-values matrixes are filled
              
            } 
            
          }   # End of cycle on ENV_Zones  for a single fire
          
        }   # End of cycle on CLC classes for a single fire
        
      } else case_ID = case_ID + 1	# End of check for sufficient number of years before and after fire
    }		
    
    if ((FirePix/50)-floor(FirePix/50) == 0) {print(paste('-> Analysing Burnt Area: ',  FirePix, ' of: ', n_fires, sep = ''))}
    
  }   # End of Cycle on Fires
  
  #Convert the columns to the correct formats
  
#   browser()
   	for (cc_df in c(1,2,5,6)) out_diff [,cc_df] = as.factor(out_diff [,cc_df])
    for (cc_df in c(3,4)) out_diff [,cc_df] = as.ordered(out_diff [,cc_df])
    for (cc_df in c(7:28)) out_diff [,cc_df] = as.numeric(out_diff [,cc_df])
  
  
  # 	for (cc_df in c(2,3,7,8,10)) plot_stat [,cc_df] = as.factor(plot_stat [,cc_df])
  # 	for (cc_df in c(1,4,11,12)) plot_stat [,cc_df] = as.ordered(as.numeric(plot_stat [,cc_df]))				  
  # 	
  # Add attributes to the output Data Frame (Useful to keep track of processing !)
  
  attr(out_diff, "RData_File") = In_File
  attr(out_diff, "SVI_File") =SVI_File
  attr(out_diff, "Shape_File") = Shape_File	
  attr(out_diff, "CSV_File") = CSV_File
  attr(out_diff, "CLC_File") = CLC_File
  attr(out_diff, "Index") = Index
  attr(out_diff, "Min_Pix") = min_pix   
  attr(out_diff, "Min_Percentage") = perc_diff   
  attr(out_diff, "Processing_Date") = Sys.Date()   
  
  #save the output in CSV and RData format	
  save(out_diff,  file = Out_File)
  #	write.csv(FRG_PairedTest_Out_Matrix, file = Out_File_csv, row.names = FALSE)
  gc(verbose = F)				#Garbage collection
  # 	print(proc.time() - ptm)
  return('DONE')
  
}

#- ----------------------------------------------
#- END OF Function 
#- ----------------------------------------------
