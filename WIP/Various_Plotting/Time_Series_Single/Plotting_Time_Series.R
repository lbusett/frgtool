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

my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 13, vjust =2, hjust = 0.5),
                                axis.title.x = element_text(face = "bold",vjust = 0, size = 10, angle = 0 ), 
                                axis.title.y = element_text(face = "bold",hjust = 0.5, vjust = 0.3,size = 10, angle = 90))
plot_folder = 'E:/busetlo/Documents/Articles/DRAFTS/Fire_Regeneration/Figures/Time_Series_6_7.5'
dir.create(plot_folder, recursive = T)

medwdt = 3
Fire = 1

# In_File_SNDVI = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SNDVI/Stat_Analysis/9.5%/Stat_Analysis_Med_SNDVI_2000_2012_META_RData.RData'
# In_File_SRDVI = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SRDVI/Stat_Analysis/11.5%/Stat_Analysis_Med_SRDVI_2000_2012_META_RData.RData'

In_File_SNDVI = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SNDVI/Stat_Analysis/ENV/6%/Stat_Analysis_Med_SNDVI_2000_2012_META_RData.RData'
In_File_SRDVI = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SRDVI/Stat_Analysis/ENV/7.5%/Stat_Analysis_Med_SRDVI_2000_2012_META_RData.RData'

In_File_x_Shape = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Files_4_Final_Plots/TS_Extraction_Med_SNDVI_2000_2012_META_RData_ker50.RData'

load(In_File_x_Shape)
str(Data_Shape)

for (Fire in 10:100) {
  a = plot_single(Fire)  
}


# 
# a2 = plot_single_nofacet(Fire)
# 
 plot_single = function(Fire) {
#   x11()
  # Build the plot for SNDVI
   out_file = file.path(plot_folder, paste('Time_Series_',Fire,'.tif', sep = ''))
   tiff(out_file, width = 6.35, height = 8, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  load(In_File_SNDVI)
  Data = plot_stat
  levels(Data$CLC_Class) = c('All LC Classes','Broadleaved Forests','Coniferous Forests','Mixed Forests','Other Natural','Schleropyllus Vegetation','Transitional W/S')
  levels(recov_stat$CLC_Class) = c('All LC Classes','Broadleaved Forests','Coniferous Forests','Mixed Forests','Other Natural','Schleropyllus Vegetation','Transitional W/S')
  
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
    if (df$Signif_FireYear == 1 ) { Start_sig = abs(as.numeric(as.character(df$YearFromFire)))+1}
    if (df$Signif_FireYear == 0 ) { Start_sig = abs(as.numeric(as.character(df$YearFromFire)))+2}
    N_sig = as.numeric(as.character(df$N_Signif))
    sig_array[Start_sig:(Start_sig+N_sig-1)] = 1
    if ((Start_sig+N_sig-1) < N_Years ) {sig_array[(Start_sig+N_sig):N_Years] = 2}
    sig_array
    #     browser()
  }
  
  sig_arrays = ddply (recov_stat_data, .(CLC_Class), function(df) Compute_sig_arrays (df,N_Years) , .progress = 'text')
  names(sig_arrays)[seq(2,(N_Years+1),1)] = levels(Data$Year)
  sig_arrays = melt(sig_arrays)
  names(sig_arrays)[c(2,3)] = c('Year', 'DiffSignif')
  Data_Fire_Rec = join(Data_Fire_Rec,sig_arrays)
  
  
  p1 = ggplot(Data_Fire_Rec, aes(x =as.factor (YearDiff), ymin = low_box, lower = X25th, middle = median, upper = X75th, ymax = up_box))+ my_theme_bw 
  p1 = p1 + geom_boxplot(aes(fill = as.factor(DiffSignif), width = 0.8),stat = 'identity', position='dodge')+facet_wrap(~CLC_Class, ncol = 1)
  p1 = p1 + geom_vline(aes(xintercept=which(levels(Year) == FireYear[1])), linetype = 2)
  p1 = p1 + scale_fill_manual(name = "", values = c('white','grey50','grey80'), labels =c('Pre-Fire','UnRecovered','Recovered'))
  p1 = p1 + xlab('Years from Fire') + ylab(expression(bold(paste(NDVI^R,' [%]'))))+coord_cartesian(ylim = c(-80,80))
  p1 = p1 +  opts(  legend.position = 'bottom')
  
  
  
  # Build the plot for SRDVI
  
  load(In_File_SRDVI)
  Data = plot_stat
  levels(Data$CLC_Class) = c('All LC Classes','Broadleaved Forests','Coniferous Forests','Mixed Forests','Other Natural','Schleropyllus Vegetation','Transitional W/S')
  levels(recov_stat$CLC_Class) = c('All LC Classes','Broadleaved Forests','Coniferous Forests','Mixed Forests','Other Natural','Schleropyllus Vegetation','Transitional W/S')
  
  OBID = unique(Data$OBJECTID)[Fire]
  Data_Fire_Rec = droplevels(subset(Data, OBJECTID == OBID & ENV_ZONE == 'All' & CLC_Class != "Other Natural"   ))
  recov_stat_data = droplevels(subset(recov_stat,OBJECTID == OBID & ENV_ZONE == 'All' & Comp_N == medwdt & CLC_Class != "Other Natural"))
  N_Years = length(levels(Data$Year))
  
  sig_arrays = ddply (recov_stat_data, .(CLC_Class), function(df) Compute_sig_arrays (df,N_Years) , .progress = 'text')
  names(sig_arrays)[seq(2,(N_Years+1),1)] = levels(Data$Year)
  sig_arrays = melt(sig_arrays)
  names(sig_arrays)[c(2,3)] = c('Year', 'DiffSignif')
  Data_Fire_Rec = join(Data_Fire_Rec,sig_arrays)
   
  
  p2 = ggplot(Data_Fire_Rec, aes(x =as.factor (YearDiff), ymin = low_box, lower = X25th, middle = median, upper = X75th, ymax = up_box))+ my_theme_bw 
  p2 = p2 +facet_wrap(~CLC_Class, ncol = 1)+ geom_boxplot(aes(fill = as.factor(DiffSignif), width = 0.8),stat = 'identity', position='dodge')
  p2 = p2 + geom_vline(aes(xintercept=which(levels(Year)== FireYear[1])), linetype = 2)
  p2 = p2 + scale_fill_manual(name = "", values = c('white','grey50','grey80'), labels =c('Pre-Fire','UnRecovered','Recovered'))
  p2 = p2 + xlab('Years from Fire') + ylab(expression(bold(paste(RDVI^R,' [%]'))))+coord_cartesian(ylim = c(-80,80))
  p2 = p2 +  opts(  legend.position = 'bottom')
  
  
  print(grid.arrange(p1, p2, ncol=2, main=textGrob(Fire_Name, gp = gpar(fontsize = 16, font = 2))))
dev.off()
}

