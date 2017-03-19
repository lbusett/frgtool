#- --------------------------------------------------------------------------- -#
#- Funzioni di plotting accessorie. 
#- --------------------------------------------------------------------------- -#

plot_folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Plots/N_Before'
dir.create(plot_folder, recursive =T) 

my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 15, vjust =2, hjust = 0.5),
                                axis.text.x = element_text( size = 10), axis.text.y = element_text(size =10), 
                                axis.title.x = element_text( vjust = 0, size = 12, angle = 0 ), 
                                axis.title.y = element_text(hjust = 0.5, vjust = 0.3,size = 12, angle = 90))

library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape2)

#- ----------------------------------------- - 
# Plot results analysis variable N before. 
#- ----------------------------------------- - 
  
  In_File_SNDVI = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SNDVI/Stat_Analysis/10/9.5%/Eroded/Percentages_Subset/Percentages_Subset_Area9.5%.RData'
  In_File_SRDVI = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SRDVI/Stat_Analysis/10/11.5%/Eroded/Percentages_Subset/Percentages_Subset_Area11.5%.RData'
  
  medwdt = 3
  
  load(In_File_SNDVI)
  Data_Perc = Perc_Recovered
  load(In_File_SRDVI)
  Data_Perc = rbind(Data_Perc, Perc_Recovered)
  
  levels(Data_Perc$CLC_Class) = c('All LC Classes','Broadleaved Forests','Coniferous Forests','Mixed Forests','Other Natural','Schleropyllus Vegetation','Transitional W/S')
  Data_Perc = droplevels(subset(Data_Perc, CLC_Class != 'Other Natural'& N_Years!= 7 & ENV_ZONE =='All'))
  Data_Perc$N_Years = as.factor(Data_Perc$N_Years)
  levels(Data_Perc$N_Years)[8] = 'NR'
  
  
  diff_func = function(Data) {
    
    Data_diffs_perc =  Data$Percentage - Data$Percentage[1]
    Data_diffs_Number = Data$N_Rec_YY - Data$N_Rec_YY[1]
    Data_diffs_Number_p = (Data$N_Rec_YY - Data$N_Rec_YY[1])/Data$N_Rec_YY[1]
    #       browser()
    Comp_N = seq(1,5,1)
    out = data.frame(Comp_N = Comp_N, Data_diffs_perc = Data_diffs_perc,Data_diffs_Number=Data_diffs_Number, Data_diffs_Number_p=Data_diffs_Number_p)
    
  }
  # Compute the differences in percentages of recovered fires and number of recovered fires when extending the width of the median
  Perc_Rec_Diff = ddply(Data_Perc, .(CLC_Class,N_Years,Index), function(df) diff_func (df),.progress = "text")
  Perc_Rec_sub = join(Data_Perc,Perc_Rec_Diff)
  levels(Perc_Rec_sub$Index) =c(expression(NDVI^R),expression(RDVI^R))
  Perc_Rec_sub = droplevels(subset(Perc_Rec_sub, CLC_Class =='All LC Classes' & Area_Cat =='All'))


  # Plot Numbers - no facetes
  p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), y = N_Rec_YY, group = N_Years, pch = N_Years))  + my_theme_bw 
  p = p + geom_line(lty = 2, color = 'grey50') #+ coord_cartesian(ylim=c(0,50))
  p = p + geom_point(aes(color = N_Years), size = 2.5)   #+theme(legend.position = 'none')
  p = p + facet_grid(~Index,  labeller=label_parsed) #+ theme(legend.position="none")
  p = p + xlab('N [Years]') + ylab ('Number of Cases')
  p = p + theme(legend.position="top", legend.direction="horizontal")
  p = p +scale_shape_manual(name = 'Recovery Time\n(Years)',values=(0:7), breaks = levels(Perc_Rec_sub$N_Years), labels = levels(Perc_Rec_sub$N_Years))
  p = p + scale_colour_brewer(name = 'Recovery Time\n(Years)', palette = "Dark2", breaks = levels(Perc_Rec_sub$N_Years), labels = levels(Perc_Rec_sub$N_Years)) #,breaks = seq(0,8))
  p = p +ylim(0,300)
  
  out_file = file.path(plot_folder, 'Width Effect.tif')
  tiff(out_file, width = 5.5, height = 4, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()

# Only NDVI
  p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), y = N_Rec_YY, group = N_Years, pch = N_Years))  + my_theme_bw 
  p = p + geom_line(lty = 2, color = 'grey50') #+ coord_cartesian(ylim=c(0,50))
  p = p + geom_point(aes(color = N_Years), size = 2.5)   #+theme(legend.position = 'none')
#   p = p + facet_grid(~Index,  labeller=label_parsed) #+ theme(legend.position="none")
  p = p + xlab('N') + ylab ('Number of Cases')
  p = p + theme(legend.position="top", legend.direction="horizontal", 
                legend.text = element_text(size = 6), 
                legend.title = element_text(size = 7),
                legend.key.size = unit(0.3, "cm"),
                legend.key = element_rect(size = 0.1))
  p = p +scale_shape_manual(name = 'Recovery Time\n(Years)',values=(0:7), breaks = levels(Perc_Rec_sub$N_Years), labels = levels(Perc_Rec_sub$N_Years))
  p = p + scale_colour_brewer(name = 'Recovery Time\n(Years)', palette = "Dark2", breaks = levels(Perc_Rec_sub$N_Years), labels = levels(Perc_Rec_sub$N_Years)) #,breaks = seq(0,8))
  p = p +ylim(0,300)
  
  out_file = file.path(plot_folder, 'NDVI_Width Effect.tif')
  tiff(out_file, width = 3, height =3.2, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
  print(p) # , main=textGrob('/nTitle', gp = gpar(fontsize = 16, font = 2))))
  dev.off()

# Other Possibilities For Plots ----------------------
  # Plot percentages - no facetes

  p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), y = 100*Percentage, group = N_Years, pch = N_Years))  + my_theme_bw 
  p = p + geom_line(lty = 2, color = 'grey50') #+ coord_cartesian(ylim=c(0,50))
  p = p + geom_point(aes(color = N_Years), size = 2.5)   #+theme(legend.position = 'none')
  p = p + facet_grid(~Index,  labeller=label_parsed) #+ theme(legend.position="none")
  p = p + xlab('N [Years]') + ylab ('Number of Cases')
  p = p + theme(legend.position="top", legend.direction="horizontal")
  p = p +scale_shape_manual(name = 'Recovery Time\n(Years)',values=(0:7), breaks = levels(Perc_Rec_sub$N_Years), labels = levels(Perc_Rec_sub$N_Years))
  p = p + scale_colour_brewer(name = 'Recovery Time\n(Years)', palette = "Dark2", breaks = levels(Perc_Rec_sub$N_Years), labels = levels(Perc_Rec_sub$N_Years)) #,breaks = seq(0,8))
  p = p +ylim(0,50)

  p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = N_Years, y = 100*Percentage))  + my_theme_bw 
  p = p + geom_point()   #+theme(legend.position = 'none')
  p = p + geom_line(lty = 2,) #+ coord_cartesian(ylim=c(0,50))
  p = p + facet_wrap(~Index) #+ theme(legend.position="none")
  p = p + xlab('Median Width (Years)') + ylab ('Probability')
  p = p + scale_colour_brewer(name = 'Recovery Time', palette = "Dark2") # ,breaks = seq(0,7))
  p = p + scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))+ylim(0,60)+theme(legend.position = 'right')
  p = p + scale_shape_manual(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))+ylim(0,60)+theme(legend.position = 'right')
  print(p +opts(axis.text.x = theme_text(size = 10))+opts(axis.text.y = theme_text(size = 10)))

  # Plot Number
  p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = CLC_Class))  + my_theme_bw 
  p = p + geom_line(aes(y = N_Rec_YY),  stat=  'identity',size = 0.8) #+ coord_cartesian(ylim=c(0,50))
  p = p + facet_grid(Index~N_Years) #+ theme(legend.position="none")
  p = p + xlab('Median Width (Years)') + ylab ('Number of Cases')
  p = p +scale_colour_brewer(palette = "Set2")+ scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))
  print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
  
  # Plot percentages
  p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = CLC_Class))  + my_theme_bw 
  p = p + geom_line(aes(y = Percentage),  stat=  'identity',size = 0.8) #+ coord_cartesian(ylim=c(0,50))
  p = p + facet_grid(Index~N_Years) #+ theme(legend.position="none")
  p = p + xlab('Median Width (Years)') + ylab ('% of Cases')
  p = p +scale_colour_brewer(palette = "Set2")+ scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))
  print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
  
  
 
  
  p = ggplot(data = Perc_Rec_sub , aes(x = as.numeric(Comp_N), color = CLC_Class))  + my_theme_bw 
  p = p + geom_line(aes(y =Data_diffs_Number),  stat=  'identity',size = 0.8) + coord_cartesian(ylim = c(-30,30))
  p = p + facet_wrap(Index~N_Years)
  p = p + xlab('Median Width (Years)') + ylab ('Variation of % of cases')
  p = p + ggtitle(paste(Index, 'Variation of % of cases vs Median Width, for different Recovery Times',sep = ' '))
  p = p + scale_x_continuous(breaks = seq(1,max(as.numeric(Perc_Rec_sub$Comp_N))))+scale_colour_brewer(palette = "Set2")
  print(p +opts(axis.text.x = theme_text(size = 6))+opts(axis.text.y = theme_text(size = 8)))
  
