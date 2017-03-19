#- --------------------------------------------------------------------------- -#
#- Funzioni di plotting accessorie. 
#- --------------------------------------------------------------------------- -#
  
  plot_folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Plots/Number'
  dir.create(plot_folder)
  library(ggplot2)
  library(plyr)
  library(gridExtra)
  library(reshape2)
  
  #- --------------------------------------------------------------------------- -#
  # Plot number of fires above 40 ha vs Year - Figure 1 of manuscript
  #- --------------------------------------------------------------------------- -#
  my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 12, vjust =2, hjust = 0.5),
                                  axis.title.x = element_text( face = "bold", vjust = 0, size = 10, angle = 0 ), 
                                  axis.title.y = element_text( face = "bold", hjust = 0.5, vjust = 0.3,size = 10, angle = 90))
  
  SNDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SNDVI/Stat_Analysis/'
  files_SNDVI = c('10/9.5%/Eroded/Percentages/Percentages_Area9.5%.RData')
  file = file.path(SNDVI_Folder,files_SNDVI) 
  
  load(file)
  Data = droplevels(subset(recov_stat,  Comp_N ==3 & CLC_Class =='All' & N_PIX >10 & as.numeric(as.character(FireYear)) < 2012 & as.numeric(as.character(FireYear)) >2002 ))
  
  p1 = ggplot(Data,aes(x = FireYear))
  p1 = p1 + geom_bar(stat = 'bin', colour="black", fill="white")
  p1 = p1 + xlab('Year') + ylab ('Number of Fires')+my_theme_bw
  
  p2 = ggplot(Data,aes(x = Area_CLC))+my_theme_bw
  p2 = p2 + geom_bar(binwidth = 0.1, colour="black", fill="white")+scale_x_log10(breaks=c(12,100,1000,10000,50000),labels=c(1,100,1000,10000,50000))
  p2 = p2 + xlab('Burnt Area (ha)') + ylab ('Number of Fires')
  
  # Bar plot - stacked using burnt area surface
  Data$cut_Area = cut(Data$Area_HA, breaks = c(0,500,1000,5000,100000), labels = c('< 500', '500 - 1000', '1000 - 5000', ' > 5000'))
  p1 = ggplot(Data,aes(x = as.factor(FireYear),fill = cut_Area))+my_theme_bw
  p1 = p1 + geom_bar( stat = 'bin')+ geom_bar(colour="black", show_guide=FALSE)
  p1 = p1 +scale_fill_grey('Burnt Area (ha)', end = 0.2, start = 0.8 )+theme(legend.position = c(0.92,0.89), legend.background = element_rect(colour = 'black'))
  p1 = p1 + xlab(expression(Year~of~Fire)) + ylab (expression('Number of Burnt Areas'))
    
  out_file = file.path(plot_folder, 'N_Fires_vs_Year_and_Area.tif')
  tiff(out_file, width = 8.15, height = 5.61, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
  dev.off()
  
   # Bar plot - stacked using CLC Class
  Data = droplevels(subset(recov_stat,  Comp_N ==3 & CLC_Class !='All' & N_PIX >10 & as.numeric(as.character(FireYear)) < 2012 & as.numeric(as.character(FireYear)) >2002 ))
  Data$cut_Area = cut(Data$Area_HA, breaks = c(0,500,1000,5000,100000), labels = c('< 500', '500 - 1000', '1000 - 5000', ' > 5000'))
  p1 = ggplot(Data,aes(x = as.factor(FireYear),fill = CLC_Class))+my_theme_bw
  p1 = p1 + geom_bar( stat = 'bin')+ geom_bar(colour="black", show_guide=FALSE)
  p1 = p1 +scale_fill_grey('CLC Class', end = 0.1, start = 0.9 )+theme(legend.position = c(0.87,0.87), legend.background = element_rect(colour = 'black'))
  p1 = p1 + xlab(expression(Year~of~Fire)) + ylab (expression('Number of Burnt Areas'))+guides(fill = guide_legend(reverse = TRUE))
    
  out_file = file.path(plot_folder, 'N_Fires_vs_Year_and_CLC.tif')
  tiff(out_file, width = 8.15, height = 5.61, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
  dev.off()
