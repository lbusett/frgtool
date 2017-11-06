#- --------------------------------------------------------------------------- -#
#- Funzioni di plotting accessorie. 
#- --------------------------------------------------------------------------- -#

plot_folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Plots/Stability'
dir.create(plot_folder, recursive =T) 
library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape2)

#- --------------------------------------------------------------------------- -#
# Plot stability analysis of time series - undisturbed pixels
#- --------------------------------------------------------------------------- -#

# Load stability data
load('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Stability_Analysis/Stability_Analysis_SRDVI.RData')
Data = out_std
load('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Stability_Analysis/Stability_Analysis_SNDVI.RData')
Data = rbind(Data, out_std)

Data$ker = as.factor(Data$ker)

# Do the plots !

my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 13, vjust =2, hjust = 0.5),
                                axis.title.x = element_text( face = "bold", vjust = 0, size = 12, angle = 0 ), 
                                axis.title.y = element_text( hjust = 0.5, vjust = 0.3,size = 12, angle = 90),
                                 plot.background = element_rect(colour = 'black', fill = 'white'),
                                 plot.margin= unit(c(1, 1, 2, 1), "lines"))
#                                 ,
# 		plot.background=element_rect(color ="black"))

# Boxplot of stdevs - NDVI 
x11()
Data_NDVI = droplevels(subset(Data, Index == 'SNDVI'))
p1 = ggplot(data = Data_NDVI)+scale_shape(solid = FALSE)
percs = ddply(Data_NDVI, .(ker),summarize, quant = quantile(Standard_Deviation, c(0.90), na.rm = T))
p1 = p1 + geom_boxplot(aes(x = as.factor(ker), y = Standard_Deviation),outlier.colour = "transparent")+coord_cartesian(ylim = c(0,20))
# p1 = p1 + geom_point(data = percs, aes(x = as.factor(ker), y = quant), pch = 5, size = 3)
p1 = p1+ xlab(NULL) +ylab('Standard Deviation [%]')+my_theme_bw
p1 = p1+ scale_x_discrete(labels=c('0'=paste(expression(NDVI),'',sep = ''),
                                   '10'=expression(paste(NDVI['10']^R, sep = '')),
                                   '25'=expression(paste(NDVI['25']^R, sep = '')),
                                   '50'=expression(paste(NDVI['50']^R, sep = '')),
                                   '100'=expression(paste(NDVI['100']^R, sep = '')),
                                   '150'=expression(paste(NDVI['150']^R, sep = '')),
                                   '200'=expression(paste(NDVI['200']^R, sep = ''))))

# Boxplot of stdevs - RDVI 
Data_RDVI = droplevels(subset(Data, Index == 'SRDVI'))
p2 = ggplot(data = Data_RDVI)+scale_shape(solid = FALSE)
percs = ddply(Data_RDVI, .(ker),summarize, quant = quantile(Standard_Deviation, c(0.90), na.rm = T))
p2 = p2 + geom_boxplot(aes(x = as.factor(ker), y = Standard_Deviation),outlier.colour = "transparent")+coord_cartesian(ylim = c(0,20))
# p2 = p2 + geom_point(data = percs, aes(x = as.factor(ker), y = quant), pch = 5, size = 3)
p2 = p2+xlab(NULL) + ylab('Standard Deviation [%]')+my_theme_bw
p2 = p2+ scale_x_discrete(labels=c('0'=expression(paste(RDVI['']^' ', sep = '')),
                                   '10'=expression(paste(RDVI['10']^R, sep = '')),
                                   '25'=expression(paste(RDVI['25']^R, sep = '')),
                                   '50'=expression(paste(RDVI['50']^R, sep = '')),
                                   '100'=expression(paste(RDVI['100']^R, sep = '')),
                                   '150'=expression(paste(RDVI['150']^R, sep = '')),
                                   '200'=expression(paste(RDVI['200']^R, sep = ''))))

#- --------------------------------------------------------------------------- -#
# Plot stability analysis of time series - prepost on burned
#- --------------------------------------------------------------------------- -#

load('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Stability_Analysis/Stability_Analysis_SNDVI_Prepost.RData')
Data = out_diff
load('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Stability_Analysis/Stability_Analysis_SRDVI_Prepost.RData')
Data = rbind(Data,out_diff)
Data$ker = as.factor(Data$ker)
# Do the plots !
# 
# my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 13, vjust =2, hjust = 0.5),
#                                 axis.title.x = element_text( face = "bold", vjust = 0, size = 12, angle = 0 ), 
#                                 axis.title.y = element_text( face = "bold", hjust = 0.5, vjust = 0.3,size = 12, angle = 90))
#                                 ,
# 		plot.background=element_rect(color ="black"))

# Boxplot of stdevs - NDVI 
x11()
Data_NDVI = droplevels(subset(Data, Index == 'NDVI'|Index == 'SNDVI'))
p3 = ggplot(data = Data_NDVI)
p3 = p3 + geom_boxplot(aes(x = as.factor(ker), y = Mean_Difference), outlier.colour = 'transparent')
p3 = p3 +coord_cartesian(ylim= c(-80,40)) + geom_hline(yintercept=0, linetype = 2)
p3 = p3 + scale_x_discrete(labels=c('0'=paste(expression(NDVI),'',sep = ''),
                                   '10'=expression(paste(NDVI['10']^R, sep = '')),
                                   '25'=expression(paste(NDVI['25']^R, sep = '')),
                                   '50'=expression(paste(NDVI['50']^R, sep = '')),
                                   '100'=expression(paste(NDVI['100']^R, sep = '')),
                                   '150'=expression(paste(NDVI['150']^R, sep = '')),
                                   '200'=expression(paste(NDVI['200']^R, sep = ''))))
p3 = p3 + xlab(NULL)+ylab(expression(paste(VI['Yf+1'] - VI['Yf-1'],' [%]', sep = ' ')))+my_theme_bw


Data_RDVI = droplevels(subset(Data, Index == 'RDVI'|Index == 'SRDVI'))
p4 = ggplot(data = Data_RDVI)
p4 = p4 + geom_boxplot(aes(x = as.factor(ker), y = Mean_Difference), outlier.colour = 'transparent')
p4 = p4 +coord_cartesian(ylim= c(-80,40)) + geom_hline(yintercept=0, linetype = 2)
p4 = p4+ scale_x_discrete(labels=c('0'=paste(expression(RDVI),'',sep = ''),
                                   '10'=expression(paste(RDVI['10']^R, sep = '')),
                                   '25'=expression(paste(RDVI['25']^R, sep = '')),
                                   '50'=expression(paste(RDVI['50']^R, sep = '')),
                                   '100'=expression(paste(RDVI['100']^R, sep = '')),
                                   '150'=expression(paste(RDVI['150']^R, sep = '')),
                                   '200'=expression(paste(RDVI['200']^R, sep = ''))))
p4 = p4 + xlab(NULL)+ylab(expression(paste(VI['Yf+1'] - VI['Yf-1'],' [%]', sep = ' ')))+my_theme_bw

out_file = file.path(plot_folder, 'Stability_Boxplots_NDVI.tif')
tiff(out_file, width = 4.25, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
print(p1) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
dev.off()

out_file = file.path(plot_folder, 'Stability_Boxplots_RDVI.tif')
tiff(out_file, width = 4.25, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
print(p2) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
dev.off()


out_file = file.path(plot_folder, 'PrePost_Boxplots_NDVI.tif')
tiff(out_file, width = 4.25, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
print(p3) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
dev.off()

out_file = file.path(plot_folder, 'PrePost_Boxplots_RDVI.tif')
tiff(out_file, width = 4.25, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
print(p4) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
dev.off()



out_file = file.path(plot_folder, 'Figure_x_Stability_Boxplots.tif')
tiff(out_file, width = 8.5, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
print(grid.arrange(p1, p2, ncol=2)) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
dev.off()


out_file = file.path(plot_folder, 'Figure_x_Stability_Boxplots_prepost2.tif')
tiff(out_file, width = 8.5, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
print(grid.arrange(p3, p4, ncol=2)) 
dev.off()


# Plot stability and prepost for NDVI only
out_file = file.path(plot_folder, 'Figure_x_Stability_Prepost_NDVI.tif')
tiff(out_file, width = 8.5, height = 4, units = 'in', pointsize = 6,compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
print(grid.arrange(p1, p3, ncol=2)) 
dev.off()

# Plot stability and prepost for RDVI only
out_file = file.path(plot_folder, 'Figure_x_Stability_Prepost_RDVI.tif')
tiff(out_file, width = 8.5, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
print(grid.arrange(p2, p4, ncol=2)) 
dev.off()


# Plot stability and prepost for NDVI and RDVI 
out_file = file.path(plot_folder, 'Figure_x_Stability_Prepost_Both.tif')
tiff(out_file, width = 8.5, height = 8, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
print(grid.arrange(p1, p2,p3,p4, ncol=2)) 
dev.off()

out_file = file.path(plot_folder, 'Figure_x_Stability_Prepost_Both_bis.tif')
tiff(out_file, width = 8.5, height = 8, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
print(grid.arrange(grid.arrange(p1, p2, ncol = 2),grid.arrange(p3,p4, ncol = 2))) 
dev.off()



# Plot only the averages

Data_NDVI = droplevels(subset(Data, Index == 'NDVI'|Index == 'SNDVI'))
Data_NDVI = Data_NDVI[is.finite(Data_NDVI$Mean_Difference),]
Data_means_NDVI = ddply(Data_NDVI,.(ker),summarize, mean = mean(Mean_Difference, na.rm = T) )

p1 = ggplot(Data_means_NDVI, aes(x = as.factor(ker), y = mean))
p1 = p1 + geom_line() + geom_point()
p1 = p1+ scale_x_discrete(labels=c('0'=paste(expression(NDVI),'',sep = ''),
                                   '10'=expression(paste(NDVI['10']^R, sep = '')),
                                   '25'=expression(paste(NDVI['25']^R, sep = '')),
                                   '50'=expression(paste(NDVI['50']^R, sep = '')),
                                   '100'=expression(paste(NDVI['100']^R, sep = '')),
                                   '150'=expression(paste(NDVI['150']^R, sep = '')),
                                   '200'=expression(paste(NDVI['200']^R, sep = ''))))
p1 = p1 + xlab(NULL)+ylab(expression(paste('Average ', VI['Yf+1'] - VI['Yf-1'],' [%]', sep = ' ')))+my_theme_bw+ylim(-25,0)

Data_RDVI = droplevels(subset(Data, Index == 'RDVI'|Index == 'SRDVI'))
Data_RDVI = Data_RDVI[is.finite(Data_RDVI$Mean_Difference),]
Data_means_RDVI = ddply(Data_RDVI,.(ker),summarize, mean = mean(Mean_Difference, na.rm = T) )

p2 = ggplot(Data_means_RDVI, aes(x = as.factor(ker), y = mean))
p2 = p2 + geom_line() + geom_point()
p2 = p2+ scale_x_discrete(labels=c('0'=paste(expression(NDVI),'',sep = ''),
                                   '10'=expression(paste(NDVI['10']^R, sep = '')),
                                   '25'=expression(paste(NDVI['25']^R, sep = '')),
                                   '50'=expression(paste(NDVI['50']^R, sep = '')),
                                   '100'=expression(paste(NDVI['100']^R, sep = '')),
                                   '150'=expression(paste(NDVI['150']^R, sep = '')),
                                   '200'=expression(paste(NDVI['200']^R, sep = ''))))
p2 = p2 + xlab(NULL)+ylab(expression(paste('Average ', VI['Yf+1'] - VI['Yf-1'],' [%]', sep = ' ')))+my_theme_bw+ylim(-25,0)

out_file = file.path(plot_folder, 'Figure_x_Stability_Boxplots_prepost2_onlymeans.tif')
tiff(out_file, width = 8.5, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
print(grid.arrange(p1, p2, ncol=2)) 
dev.off()