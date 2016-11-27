#- --------------------------------------------------------------------------- -#
#- Funzioni di plotting accessorie. 
#- --------------------------------------------------------------------------- -#

  library(ggplot2)
  library(plyr)
  library(gridExtra)
  library(reshape2)
  
#- ----------------------------------------- - 
# Plot  time series of single fires, with significances
#- ----------------------------------------- - 

  my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 14, vjust =2, hjust = 0.5),
                                  axis.text.x = element_text( size = 9, angle = 45, hjust = 1), axis.text.y = element_text(size =9), 
                                  axis.title.x = element_text( vjust = 0, size =10, angle = 0 ), 
                                  axis.title.y = element_text(hjust = 0.5, vjust = 0.3,size = 10, angle = 90))
  
  plot_folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Plots/Time_Series'   # out folder for plots
  dir.create(plot_folder, recursive = T)
  
  max_N_Years_after = 9
  
  # Define and load files to be analyzed
  SNDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SNDVI/Stat_Analysis/'
  files_SNDVI = c('10/9.5%/Eroded/Stat_Analysis_Med_SNDVI_2000_2012_META_RData.RData')
  In_File_SNDVI = file.path(SNDVI_Folder,files_SNDVI) 

  
  # File needed to get the shapefile information
  In_File_x_Shape = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SNDVI/TS_Extraction/TS_Extraction_Med_SNDVI_2000_2012_META_RData.RData'  
  
  medwdt = 3
  Fire = 1
  
  load(In_File_x_Shape)
  str(Data_Shape)
 
  Start_Fire = 1
  End_Fire = 100
  x_var = 'Year'
  res = 600
  # x_var = 'YearDiff'
  wdt =5.514
  ht = 7.0642
  leg_size =0.50
  leg_fonts = 6.54
  for (Fire in c(1,45,46)) a = plot_single(Fire,In_File_SNDVI,In_File_SRDVI,medwdt,wdt,ht,leg_size, leg_fonts)
  
  plot_single = function(Fire,In_File_SNDVI,In_File_SRDVI,medwdt,wdt,ht,leg_size,leg_fonts ) {
    
    # Build the plot for SNDVI
    out_file = file.path(plot_folder, paste('Time_Series_NDVI',Fire,'.tif', sep = ''))
#     pdf(out_file,wdt,ht, pointsize = 4)
   tiff(out_file, width = wdt, height = ht, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = res )
    
    load(In_File_SNDVI)
    Data = plot_stat
    levels(Data$CLC_Class) = c('All Classes','Broadleaved Forests','Coniferous Forests','Mixed Forests','Schleropyllus Vegetation','Transitional W/S')
    levels(recov_stat$CLC_Class) = c('All Classes','Broadleaved Forests','Coniferous Forests','Mixed Forests','Schleropyllus Vegetation','Transitional W/S')
    
    OBID = unique(Data$OBJECTID)[Fire]
    Data_Fire_Rec = droplevels(subset(Data, OBJECTID == OBID & ENV_ZONE == 'All' & CLC_Class != "Other Natural"   ))
    recov_stat_data = droplevels(subset(recov_stat,OBJECTID == OBID & ENV_ZONE == 'All' & Comp_N == medwdt & CLC_Class != "Other Natural"))
    N_Years = length(levels(Data$Year))
    
    Fire_Name = paste(paste(' \n',paste(droplevels(subset(Data_Shape, OBJECTID == OBID))$Place_Name, 
                                        droplevels(subset(Data_Shape, OBJECTID == OBID))$YearSeason, 
                                        paste(droplevels(subset(Data_Shape, OBJECTID == OBID))$Area_HA,'ha',sep = ' '), 
                                        sep = ' - '),sep = ''))
    
    Compute_sig_arrays = function(df,N_Years) {
      sig_array = array(0, dim = N_Years)
      if (df$N_Signif != 0) {
      if (df$Signif_FireYear == 1 ) { Start_sig = abs(as.numeric(as.character(df$YearFromFire)))+1}
      if (df$Signif_FireYear == 0 ) { Start_sig = abs(as.numeric(as.character(df$YearFromFire)))+2}
      N_sig = as.numeric(as.character(df$N_Signif))
      sig_array[Start_sig:(Start_sig+N_sig-1)] = 1
      if ((Start_sig+N_sig-1) < N_Years ) {sig_array[(Start_sig+N_sig):N_Years] = 2}
      }
      sig_array
      #     browser()
    }
    
    sig_arrays = ddply (recov_stat_data, .(CLC_Class), function(df) Compute_sig_arrays (df,N_Years) , .progress = 'text')
    names(sig_arrays)[seq(2,(N_Years+1),1)] = levels(Data$Year)
    sig_arrays = melt(sig_arrays)
    names(sig_arrays)[c(2,3)] = c('Year', 'DiffSignif')
    Data_Fire_Rec = join(Data_Fire_Rec,sig_arrays)
    Data_Fire_Rec$DiffSignif = as.factor(Data_Fire_Rec$DiffSignif)
    Data_Fire_Rec$xintercept = which(levels(Data_Fire_Rec$Year) == Data_Fire_Rec$FireYear[1])
    
    Rec_Years = ddply(Data_Fire_Rec, .(CLC_Class), summarize, Rec_Year = min(which(DiffSignif ==2)),
                      RT = min(which(DiffSignif ==2))-min(which(DiffSignif ==1)))
    Rec_Years$Text = (paste('RT = ',Rec_Years$RT,sep=''))
    Rec_Years$Text [which(Rec_Years$RT == 'Inf')] = (paste('RT = ','NR',sep=''))
    
    p1 = ggplot(Data_Fire_Rec)+ my_theme_bw 
    p1 = p1 + geom_vline(aes_string(x =x_var, xintercept = 'xintercept'), linetype = 2, color = 'red')
    p1 = p1 + geom_vline(data = Rec_Years, aes(xintercept=Rec_Year), linetype = 2, color = 'forestgreen')
    p1 = p1 + geom_boxplot(aes_string(x =x_var, ymin = 'low_box', lower = 'X25th', middle = 'median', upper = 'X75th', ymax = 'up_box',fill = 'DiffSignif'), 
                           width = 0.8,stat = 'identity', position='dodge', size = 0.3)+facet_wrap(~CLC_Class, ncol = 2)
    p1 = p1 + scale_fill_manual(name = "", values = c('white','grey50','grey80'), labels =c('Pre-Fire','UnRecovered','Recovered'))
    p1 = p1 + xlab('Year') + ylab(expression(paste(NDVI^R,' [%]')))+coord_cartesian(ylim = c(-80,80))
    p1 = p1 + theme(legend.position = 'bottom')
    p1 = p1 + geom_text(data = Rec_Years, aes(x= '2012',y =70 ,label=Text), size = 2.5,hjust = 1, legend = FALSE)
    p1 = p1 + theme(legend.key.size =  unit(leg_size, "cm"), legend.text=theme_text(size=leg_fonts))
    p1 = p1 + theme(legend.background = element_rect(colour = 'black', size = 0.3))
    p1 = p1 + theme(legend.key = element_rect(colour = 'white', fill = 'white', size = 0.5, linetype='dashed'))
    p1 = p1 + ggtitle(Fire_Name)
    
    print(p1, main=textGrob(Fire_Name, gp = gpar(fontsize = 12, font = 2)))
    dev.off()
#     # Build the plot for SRDVI
#     
#     load(In_File_SRDVI)
#     Data = plot_stat
#     levels(Data$CLC_Class) = c('All Classes','Broadleaved Forests','Coniferous Forests','Mixed Forests','Schleropyllus Vegetation','Transitional W/S')
#     levels(recov_stat$CLC_Class) = c('All Classes','Broadleaved Forests','Coniferous Forests','Mixed Forests','Schleropyllus Vegetation','Transitional W/S')
#     
#     OBID = unique(Data$OBJECTID)[Fire]
#     Data_Fire_Rec = droplevels(subset(Data, OBJECTID == OBID & ENV_ZONE == 'All' & CLC_Class != "Other Natural"   ))
#     recov_stat_data = droplevels(subset(recov_stat,OBJECTID == OBID & ENV_ZONE == 'All' & Comp_N == medwdt & CLC_Class != "Other Natural"))
#     N_Years = length(levels(Data$Year))
#     
#     sig_arrays = ddply (recov_stat_data, .(CLC_Class), function(df) Compute_sig_arrays (df,N_Years) , .progress = 'text')
#     names(sig_arrays)[seq(2,(N_Years+1),1)] = levels(Data$Year)
#     sig_arrays = melt(sig_arrays)
#     names(sig_arrays)[c(2,3)] = c('Year', 'DiffSignif')
#     Data_Fire_Rec = join(Data_Fire_Rec,sig_arrays)
#     Data_Fire_Rec$DiffSignif = as.factor(Data_Fire_Rec$DiffSignif)
#     Data_Fire_Rec$xintercept = which(levels(Data_Fire_Rec$Year) == Data_Fire_Rec$FireYear[1])
#     
#     Rec_Years = ddply(Data_Fire_Rec, .(CLC_Class), summarize, Rec_Year = min(which(DiffSignif ==2)),
#                       RT = min(which(DiffSignif ==2))-min(which(DiffSignif ==1)))
#     Rec_Years$Text = (paste('RT = ',Rec_Years$RT,sep=''))
#     Rec_Years$Text [which(Rec_Years$RT == 'Inf')] = (paste('RT = ','NR',sep=''))
#     
#     p2 = ggplot(Data_Fire_Rec)+ my_theme_bw 
#     p2 = p2 + geom_vline(aes_string(x =x_var, xintercept = 'xintercept'), linetype = 2, color = 'red')
#     p2 = p2 + geom_vline(data = Rec_Years, aes(xintercept=Rec_Year), linetype = 2, color = 'forestgreen')
#     p2 = p2 + geom_boxplot(aes_string(x =x_var, ymin = 'low_box', lower = 'X25th', middle = 'median', upper = 'X75th', ymax = 'up_box',fill = 'DiffSignif'), 
#                            width = 0.8,stat = 'identity', position='dodge', size = 0.3)+facet_wrap(~CLC_Class, ncol = 1)
#  
#     p2 = p2 + scale_fill_manual(name = "", values = c('white','grey50','grey80'), labels =c('Pre-Fire','UnRecovered','Recovered'))
#     p2 = p2 + xlab('Year') + ylab(expression(paste(RDVI^R,' [%]')))+coord_cartesian(ylim = c(-80,80))
#     p2 = p2 +  theme(legend.position = 'bottom')
#     p2 = p2 + geom_text(data = Rec_Years, aes(x= '2012',y =70 ,label=Text), size = 2.5,hjust = 1, legend = FALSE)
#     p2 = p2 + theme(legend.key.size =  unit(leg_size, "cm"), legend.text=theme_text(size=leg_fonts))
#     p2 = p2 + theme(legend.background = element_rect(colour = 'black', size = 0.3))
#     p2 = p2 + theme(legend.key = element_rect(colour = 'white', fill = 'white', size = 0.5, linetype='dashed'))
#       
#     
    
  }

