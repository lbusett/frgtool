#- --------------------------------------------------------------------------- -#
#- Funzioni di plotting accessorie. 
#- --------------------------------------------------------------------------- -#

library(ggplot2)
library(plyr)
library(gridExtra)
library(reshape2)

library(lattice)
library(taRifx)

#- --------------------------------------------------------------------------- -#
#- Functions to plot the percentages of fires recovered after k years
#- --------------------------------------------------------------------------- -#
my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 15, vjust =2, hjust = 0.5),
                                axis.text.x = element_text( size = 12), axis.text.y = element_text(size =12), 
                                axis.title.x = element_text( vjust = 0, size = 14, angle = 0 ), 
                                axis.title.y = element_text(hjust = 0.5, vjust = 0.3,size = 14, angle = 90))
max_N_Years_after = 9

# In_File_SNDVI = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/10/Med_SNDVI/Stat_Analysis/9.5%/Percentages/Percentages_9.5%.RData'

SNDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Tests/50/Med_SNDVI/Stat_Analysis/'
SRDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Tests/50/Med_SRDVI/Stat_Analysis/'

files_SNDVI = c('10/6%/Normal/Percentages/Percentages_Area6%.RData',
                '10/6%/Normal/Percentages/Percentages_Area_Common6%.RData',
                '10/6%/Eroded/Percentages/Percentages_Area6%.RData',
                '20/6%/Normal/Percentages/Percentages_Area6%.RData',
                '20/6%/Normal/Percentages/Percentages_Area_Common6%.RData',
                '20/6%/Eroded/Percentages/Percentages_Area6%.RData',
                '10/9.5%/Normal/Percentages/Percentages_Area9.5%.RData',
                '10/9.5%/Normal/Percentages/Percentages_Area_Common9.5%.RData',
                '10/9.5%/Eroded/Percentages/Percentages_Area9.5%.RData',
                '20/9.5%/Normal/Percentages/Percentages_Area9.5%.RData',
                '20/9.5%/Normal/Percentages/Percentages_Area_Common9.5%.RData',
                '20/9.5%/Eroded/Percentages/Percentages_Area9.5%.RData'
)

files_SRDVI = c('10/7.5%/Normal/Percentages/Percentages_Area7.5%.RData',
                '10/7.5%/Normal/Percentages/Percentages_Area_Common7.5%.RData',
                '10/7.5%/Eroded/Percentages/Percentages_Area7.5%.RData',
                '20/7.5%/Normal/Percentages/Percentages_Area7.5%.RData',
                '20/7.5%/Normal/Percentages/Percentages_Area_Common7.5%.RData',
                '20/7.5%/Eroded/Percentages/Percentages_Area7.5%.RData',
                '10/11.5%/Normal/Percentages/Percentages_Area11.5%.RData',
                '10/11.5%/Normal/Percentages/Percentages_Area_Common11.5%.RData',
                '10/11.5%/Eroded/Percentages/Percentages_Area11.5%.RData',
                '20/11.5%/Normal/Percentages/Percentages_Area11.5%.RData',
                '20/11.5%/Normal/Percentages/Percentages_Area_Common11.5%.RData',
                '20/11.5%/Eroded/Percentages/Percentages_Area11.5%.RData'
)


In_Files_SNDVI = file.path(SNDVI_Folder,files_SNDVI) 
In_Files_SRDVI = file.path(SRDVI_Folder,files_SRDVI)


out_plot_folder_main = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Tests/50/plots'
out_plot_folders= file.path(out_plot_folder_main,c('6_7.5','6_7.5','6_7.5','6_7.5','6_7.5','6_7.5','9.5_11.5','9.5_11.5','9.5_11.5','9.5_11.5','9.5_11.5','9.5_11.5'))
out_pdf_names = c('plots_10_6_7.5_Normal.pdf',
                  'plots_10_6_7.5_Normal_Common.pdf',
                  'plots_10_6_7.5_Eroded.pdf',
                  'plots_20_6_7.5_Normal.pdf',
                  'plots_20_6_7.5_Normal_Common.pdf',
                  'plots_20_6_7.5_Eroded.pdf',
                  'plots_10_9_11.5_Normal.pdf',
                  'plots_10_9_11.5_Normal_Common.pdf',
                  'plots_10_9_11.5_Eroded.pdf',
                  'plots_20_9_11.5_Normal.pdf',
                  'plots_20_9_11.5_Normal_Common.pdf',
                  'plots_20_9_11.5_Eroded.pdf')

str_options = c('Normal - N_PIX = 10 - Percs = 6-7.5',
                'Normal - N_PIX = 10 - Percs = 6-7.5 - Common Fires',
                'Eroded - N_PIX = 10 - Percs = 6-7.5',
                'Normal - N_PIX = 20 - Percs = 6-7.5',
                'Normal - N_PIX = 20 - Percs = 6-7.5 - Common Fires',
                'Eroded - N_PIX = 20 - Percs = 6-7.5',
                'Normal - N_PIX = 10 - Percs = 9.5-11.5',
                'Normal - N_PIX = 10 - Percs = 9.5-11.5 - Common Fires',
                'Eroded - N_PIX = 10 - Percs = 9.5-11.5',
                'Normal - N_PIX = 20 - Percs = 9.5-11.5',
                'Normal - N_PIX = 20 - Percs = 9.5-11.5 - Common Fires',
                'Eroded - N_PIX = 20 - Percs = 9.5-11.5')


type_codes = c('Normal','Eroded','Normal','Eroded')

for(cy in (1:12)) {
  In_File_SNDVI = In_Files_SNDVI[cy]   ; In_File_SRDVI = In_Files_SRDVI[cy]   ; out_plot_folder = out_plot_folders[cy]
  type_code = type_codes[cy]  ; out_pdf = out_pdf_names[cy]; str_opt = str_options[cy]
  
  common_fires_flag = F
  
  plot_probabilities(In_File_SNDVI,In_File_SRDVI, out_plot_folder, type_code, common_fires_flag,str_opt)
}

plot_probabilities = function (In_File_SNDVI,In_File_SRDVI, out_plot_folder, type_code, common_fires_flag,str_opt){
  
  dir.create(out_plot_folder, recursive = T)
  pdf_out = file.path(out_plot_folder,out_pdf)
#   pdf(pdf_out,width = 11.69, height = 8.27, pointsize = 9)
  
  load(In_File_SNDVI)             # Load SNDVI file
  Data_Perc = Perc_Recovered      # Reassign data names for ease of use
  Data_YY = Perc_Recovered_YY
  
  load(In_File_SRDVI)            # Load SNDVI file
  Data_Perc = rbind(Data_Perc,Perc_Recovered)       # Join SNDVI and SRDVI data
  Data_YY = rbind(Data_YY,Perc_Recovered_YY)
  medwdt = 3                                        # Selected medwdt (To be modified ! Must be taken from GUI call)
  
  # Some prelimnary stuff to get only interesting data and set correct names for factor levels
  
  
  Data_Perc = droplevels(subset(Data_Perc, Comp_N == medwdt & CLC_Class!= 'Other Natural Land'))
  Data_Perc$N_Years = ordered(Data_Perc$N_Years, labels = c('0', '1','2','3','4','5','6','7','8','9','NR'))
  Data_Perc$Comp_N = ordered(as.numeric(as.character(Data_Perc$Comp_N)))
  levels(Data_Perc$CLC_Class)=c('All~LC~Classes','Broadleaved~Forests','Coniferous~Forests','Mixed~Forests','Schleropyllus','Transitional~W/S')
  levels(Data_Perc$Index) = c('NDVI^R','RDVI^R')
  Data_Perc$Area_Cat = factor(Data_Perc$Area_Cat, levels = unique(Data_Perc$Area_Cat))
  levels(Data_Perc$Area_Cat) = c('All','0-250','250-500','500-1000','1000-5000','5000-10000','10000-50000')
  
  Data_YY = droplevels(subset(Data_YY, Comp_N == medwdt & CLC_Class!= 'Other Natural Land'))
  Data_YY$N_Years = ordered(Data_YY$N_Years, labels = c('0', '1','2','3','4','5','6','7','8','9','NR'))
  Data_YY$Comp_N = ordered(as.numeric(as.character(Data_YY$Comp_N)))
  levels(Data_YY$CLC_Class)=c('All~LC~Classes','Broadleaved~Forests','Coniferous~Forests','Mixed~Forests','Schleropyllus','Transitional~W/S')
  levels(Data_YY$Index) = c('NDVI^R','RDVI^R')
  Data_YY$Area_Cat = factor(Data_YY$Area_Cat, levels = unique(Data_YY$Area_Cat))
  levels(Data_YY$Area_Cat) = c('All','0-250','250-500','500-1000','1000-5000','5000-10000','10000-50000')
  
  
  browser()
  
  #- ----------------------------------------- - 
  # Number of analysed fires, by Area_Cat
  #- ----------------------------------------- - 
  
  df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Years == 0  & CLC_Class =='All~LC~Classes' & Index == 'NDVI^R' & Area_Cat !='All') )
  p1 = ggplot(df,aes(x = Area_Cat,y =N_Tot_YY,fill = Area_Cat))
  p1 = p1 + stat_summary(fun.y="sum", geom="bar")
  p1 = p1 + xlab('Year') + ylab ('Number of Fires')+my_theme_bw+scale_fill_grey('Burnt Area (ha)', end = 0.2, start = 0.8 )
  p1 = p1 + coord_cartesian(ylim = c(0,1700))
  title = paste("Number of analyzed Burnt Areas, by Total Woody burnt area\n\n",str_opt, sep = '')
  p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
  out_file = file.path(out_plot_folder,paste('Barplot_Number_Fires_AreaCat_',type_code,'.tif', sep = ''))
#   tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
   print(p1)
#   dev.off()
  
  #- ----------------------------------------- - 
  # Number of analysed fires, by Area_Cat, separated by Year
  #- ----------------------------------------- - 
  df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Years == 0  & CLC_Class =='All~LC~Classes' & Index == 'NDVI^R' & Area_Cat !='All') )
  #   df$Area_Cat <- factor(df$Area_Cat, levels = rev(levels(df$Area_Cat)))
  p1 = ggplot(df,aes(x = Area_Cat,y =N_Tot_YY,fill = Area_Cat))
  p1 = p1+facet_grid(~FireYear, drop = FALSE)
  p1 = p1 + geom_bar( stat = 'identity', show_guide=FALSE)
  p1 = p1 + xlab('Year') + ylab ('Number of Fires')+my_theme_bw+scale_fill_grey('Burnt Area (ha)', end = 0.2, start = 0.8 )
  p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust =1,size = 12))
  p1 = p1+ coord_cartesian(ylim = c(0,350))
  title = paste("Number of analyzed Burnt Areas, by Total Woody burnt area and FireYear\n\n",str_opt, sep = '')
  p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  out_file = file.path(out_plot_folder,paste('Barplot_Number_Fires_AreaCat_YY',type_code,'.tif', sep = ''))
#   tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
  
   #- ----------------------------------------- - 
  # Number of analysed fires, by Area_Cat, separated by CLC_Class
  #- ----------------------------------------- - 
  df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Years == 0   & Index == 'NDVI^R' & Area_Cat !='All') )
  #   df$Area_Cat <- factor(df$Area_Cat, levels = rev(levels(df$Area_Cat)))
  p1 = ggplot(df,aes(x = Area_Cat,y =N_Tot_YY,fill = Area_Cat))
  p1 = p1+facet_wrap(~CLC_Class, drop = FALSE)
  p1 = p1 + geom_bar( stat = 'identity', show_guide=FALSE)
  p1 = p1 + xlab('Year') + ylab ('Number of Fires')+my_theme_bw+scale_fill_grey('Burnt Area (ha)', end = 0.2, start = 0.8 )
  p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust =1,size = 12))
   p1 = p1 + coord_cartesian(ylim = c(0,1700))
  title = paste("Number of analyzed Burnt Areas, by Total Woody burnt area and FireYear\n\n",str_opt, sep = '')
  p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  out_file = file.path(out_plot_folder,paste('Barplot_Number_Fires_AreaCat_YY',type_code,'.tif', sep = ''))
#   tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
 
  
  #- ----------------------------------------- - 
  # Plot Percentage of fires vs recovery time, as boxplots showing variance of % in != years
  #- ----------------------------------------- - 
  
  # Boxplots of percentages, by Class and index. Values of different years are the different points in the bplot
  df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0 & N_Years != 'NR' & Area_Cat =='All') )
  df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0  & Area_Cat =='All') )
  p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage,color = CLC_Class ))+ my_theme_bw 
  p1 = p1 + geom_boxplot(size = 0.2, outlier.colour = 'transparent', color = 'grey75')#  + geom_jitter(size =2, color = 'blue')
  p1 = p1 + geom_point(alpha = 0.6, position = position_jitter(height = 0 , width = 0.1)) 

  p1 = p1 + facet_grid(CLC_Class~Index, labeller=label_parsed, drop = FALSE) + theme(legend.position="none")
  p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]') + theme(strip.text.y = element_text(size = 8, angle = 90))
  p1 = p1 + coord_cartesian(ylim =c(0,100))
  title = paste("Distributions of Recovery Time Probability recorded in different Years, by CLC Class (All Areas)\n\n",str_opt, sep = '')
  p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  out_file = file.path(out_plot_folder,paste('Boxplot_Perc_CLC_Class_AllArea_', type_code,'.tif', sep = ''))
#   tiff(out_file, width = 5.5, height = 7.5, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
#   dev.off()
  
  # Boxplots of percentages, by Area_Cat and index for the "ALL" CLCL_Class. Values of different years are the different points in the bplot
  
  df = (subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0 & N_Years != 'NR' & CLC_Class =='All~LC~Classes') )
  df = (subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0  & CLC_Class =='All~LC~Classes') )
  p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage,color = Area_Cat ))+ my_theme_bw 
  p1 = p1 + geom_boxplot(size = 0.2, outlier.colour = 'transparent', color = 'grey75')#  + geom_jitter(size =2, color = 'blue')
  p1 = p1 + geom_point(alpha = 0.6, position = position_jitter(height = 0 , width = 0.1)) 

  p1 = p1 + facet_grid(Area_Cat~Index, labeller=label_parsed, drop = F) + theme(legend.position="none")
  p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]') + theme(strip.text.y = element_text(size = 8, angle = 90))
  p1 = p1 + coord_cartesian(ylim =c(0,100))
  title = paste("Distributions of Recovery Time Probability recorded in different Years, by Area Classes (All LC)\n\n",str_opt, sep = '')
  p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
   p1 = p1 + coord_cartesian(ylim =c(0,100))
  out_file = file.path(out_plot_folder,paste('Boxplot_Perc_Area_Cat_AllCLC_', type_code,'.tif', sep = ''))
#   tiff(out_file, width = 5.5, height = 7.5, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
#   dev.off()
  
  #- ----------------------------------------- - 
  # Plot Percentage of fires vs recovery time, separated by year
  #- ----------------------------------------- - 
  
  
  for (Ind in c('NDVI^R','RDVI^R')) {
    # Barplots, percentages, different for each index - vs CLC_Class for all area
    df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0 & Area_Cat =='All' & Index == Ind) )
    p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage,color = CLC_Class, group = CLC_Class ))  + my_theme_bw 
    p1 = p1 + geom_bar(stat = 'identity',size = 0.2)
    p1 = p1 + facet_grid(CLC_Class~FireYear, labeller=label_parsed, drop = FALSE) + theme(legend.position="none")
    p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]')+ theme(strip.text.y = element_text(size = 8, angle = 90))
    
    title = paste("Recovery Time Probability for different FireYears, by CLC Classes (All Area)\n\n",Ind, ' - ', str_opt, sep = '')
    p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
    p1 = p1 + coord_cartesian(ylim =c(0,100))
    out_file = file.path(out_plot_folder,paste('Barplot_Perc_CLC_Class_YY_',Ind,'_', type_code,'.tif', sep = ''))
#     tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
    print(p1)
#     dev.off()
    
    # Barplots, different for each index - vs Area for all CLC_Class
    df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0  & CLC_Class =='All~LC~Classes' & Index == Ind) )
    p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage,color = Area_Cat, group = Area_Cat ))  + my_theme_bw 
    p1 = p1 + geom_bar(stat = 'identity',size = 0.2)
    p1 = p1 + facet_grid(Area_Cat~FireYear, labeller=label_parsed, drop = FALSE) + theme(legend.position="none")
    p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]')+ theme(strip.text.y = element_text(size = 8, angle = 90))
    title = paste("Recovery Time Probability for different FireYears, by Area Classes (All LC Classes)\n\n",Ind, ' - ', str_opt, sep = '')
    p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
     p1 = p1 + coord_cartesian(ylim =c(0,100))
    out_file = file.path(out_plot_folder,paste('Barplot_Perc_Area_YY_',Ind,'_', type_code,'.tif', sep = ''))
#     tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
    print(p1)
    
    # NEW STUFF NEW STUFF NEW STUFF NEW STUFF NEW STUFF NEW STUFF NEW STUFF NEW STUFF NEW STUFF NEW STUFF 
    
#     df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0  & Index == Ind & Percentage >=0.0000001) )
#        title = paste("Recovery Time - Original vs Eroded Burnt Areas\n\n",Index,' - ',str_opt, sep = '')  
#     df$Percentage_cut = cut(df$Percentage, breaks = c(0.01,0.20,0.40,0.60,0.80,1.00))
#     p = ggplot(df)+ my_theme_bw
#     p = p + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
#     p = p + xlab('Recovery Time - On Original Burnt Areas')+ ylab ('Recovery Time - On Eroded Burnt Areas')
# #     p = p + scale_x_discrete(limits = levels(summary$N_Signif_Rec_Er),
# #                              breaks=levels(summary$N_Signif_Rec_Er), drop = FALSE,
# #                              labels =c(seq(0,(length(levels(summary$N_Signif_Rec_Er))-2),1),'NR' ))
# #     p = p + scale_y_discrete( limits = levels(summary$N_Signif_Rec_Er),
# #                               breaks=levels(summary$N_Signif_Rec_Er), drop = FALSE, 
# #                               labels =c(seq(0,length(levels(summary$N_Signif_Rec_Er))-2,1),'NR' ))
# #     
#     df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0  & Index == Ind & Area_Cat =='All') )
#      df$Percentage_cut = cut(df$Percentage, breaks = c(0.01,0.20,0.40,0.60,0.80,1.00))
#    p  = ggplot(df)+ my_theme_bw
#     p = p + geom_point(aes(x =as.numeric(N_Years) , y = (FireYear), 
#                            size = Percentage_cut, fill = Percentage_cut),pch = 21, na.rm = T) #+geom_abline(intercept = 0, slope = 1)
# #     p = p +  scale_fill_hue()
#      p = p + scale_size_discrete(range = c(2, 6))
#     p = p + facet_wrap(~CLC_Class,drop = FALSE)
# #     p = p + geom_text(aes(x=6, y=1, label=Text), data = summary_changes, size = 4,hjust=0.5)
#     p
#     
#         
#     compareplot(~as.numeric(N_Years)| CLC_Class *FireYear*Area_Cat, 
#       data.frame=df , 
#       main = "Chick Weights",
#       box.show.mean=FALSE,
#       box.show.whiskers=FALSE,
#       box.show.box=FALSE
#       )
# 
# cw2 = melt(cw, id = 'weight')
# 
# # create a data frame with boxplot stats
#   df2 = ddply(recov_stat, .(CLC_Class,Area_Cat, FireYear), function(df) boxplot.stats(df$N_Signif_Rec)$stats)
#   
#   # generate the plot
#   ggplot(df, aes(Area_Cat, Percentage)) +
#     geom_boxplot(fill = 'gray90', colour = 'gray90', alpha = 0) +      
#     geom_segment(data = df2, aes(xend = value, y = V1, yend = V2)) + 
#     geom_segment(data = cw3, aes(xend = value, y = V4, yend = V5)) + 
#     geom_point(data = cw3, aes(y = V3), size = 3) + 
#     facet_wrap(~ variable, scales = 'free_x', nrow = 1)      
#     
    # NEW STUFF NEW STU?ordinFF NEW STUFF NEW STUFF NEW STUFF NEW STUFF NEW STUFF NEW STUFF NEW STUFF NEW STUFF 
    
#     dev.off()
    
    df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0  & CLC_Class =='All~LC~Classes' & Index == Ind) )
    p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = N_Rec_YY,color = Area_Cat, group = Area_Cat ))  + my_theme_bw 
    p1 = p1 + geom_bar(stat = 'identity',size = 0.2)
    p1 = p1 + facet_grid(Area_Cat~FireYear, labeller=label_parsed, scales = 'free_y', drop = FALSE) + theme(legend.position="none")
    p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Number of cases')+ theme(strip.text.y = element_text(size = 8, angle = 90))
    out_file = file.path(out_plot_folder,paste('Barplot_Number_Area_YY_',Ind,'_', type_code,'.tif', sep = ''))
    
    title = paste("Number of fires recovered after YY years, for different FireYears, by Area Classes (All LC Classes)\n\n",Ind, ' - ', str_opt, sep = '')
    p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
    
#     tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
    print(p1)
#     dev.off()
    
  }
  
  # lineplots, percentages - vs CLC_Class for all area
  df = (subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0 & Area_Cat =='All') )
  p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage,color = Index, group = Index ))  + my_theme_bw 
  p1 = p1 + geom_line(size = 0.2, linetype = 1) + geom_point ( size = 2)
  p1 = p1 + facet_grid(CLC_Class~FireYear, labeller=label_parsed, drop = FALSE) + theme(legend.position="bottom")
  p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]')+ theme(strip.text.y = element_text(size = 8, angle = 90))+
    scale_colour_manual(name = '',values = c('black','grey50'), labels =c(expression(NDVI^R),expression(RDVI^R)))
  p1 = p1 #+ coord_cartesian(ylim = c(0,80))
   title = paste("Recovery Time Probability for different FireYears, by CLC Classes (All Area)\n\n", str_opt, sep = '')
    p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
   p1 = p1 + coord_cartesian(ylim =c(0,100))   
  out_file = file.path(out_plot_folder,paste('Lineplot_Perc_CLC_Class_YY_',type_code,'.tif', sep = ''))
#   tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
  
   # lineplots, percentages - vs Area for all CLC
  df = (subset(Data_YY, ENV_ZONE == 'All'  & CLC_Class =='All~LC~Classes' ) )
  p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage,color = Index, group = Index ))  + my_theme_bw 
  p1 = p1 + geom_line(size = 0.2, linetype = 1) + geom_point ( size = 2)
  p1 = p1 + facet_grid(Area_Cat~FireYear, labeller=label_parsed, drop = FALSE) + theme(legend.position="bottom")
  p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]')+ theme(strip.text.y = element_text(size = 8, angle = 90))+
    scale_colour_manual(name = '',values = c('black','grey50'), labels =c(expression(NDVI^R),expression(RDVI^R)))
  p1 = p1 #+ coord_cartesian(ylim = c(0,80))
  title = paste("Recovery Time Probability for different FireYears, by Area Classes (All LC Classes)\n\n", str_opt, sep = '')
  p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
   p1 = p1 + coord_cartesian(ylim =c(0,100))  
  out_file = file.path(out_plot_folder,paste('Lineplot_Perc_Area_YY_',type_code,'.tif', sep = ''))
#   tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
  

  dev.off()
  
}     
# 
# 
# # Line plots of percentages, by year, index and Class
# df = droplevels(subset(Data_YY, CLC_Class != 'Other Natural Land' & ENV_ZONE == 'All' & Comp_N ==3 & N_Tot_YY != 0 & N_Years != 999) )
# p1 = ggplot(data =df 
#             , aes(x =as.factor(N_Years),y = 100*Percentage,color = Index, group = Index ))  + my_theme_bw 
# p1 = p1 + geom_line(size = 0.6)  + geom_point(size = 1)
# p1 = p1 + facet_wrap(FireYear~Index, ncol = 2) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')+
#   scale_colour_manual(name = '',values = c('black','grey50'), labels =c(expression(NDVI^R),expression(RDVI^R)))
# 
# # bar plots of percentages, by year, index and Class
# df = droplevels(subset(Data_YY, CLC_Class == 'All' & ENV_ZONE == 'All' & Comp_N ==3 & N_Tot_YY != 0 ) )
# p1 = ggplot(data =df , aes(x =as.factor(N_Years),y = 100*Percentage,color = CLC_Class, group = CLC_Class ))  + my_theme_bw 
# p1 = p1 + geom_bar(stat = 'identity',size = 0.6)
# p1 = p1 + facet_wrap(FireYear~Index, ncol = 2) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# 
# 
# # Only NDVI
# df = droplevels(subset(Data_YY, Index  == 'Med_SNDVI' & ENV_ZONE == 'All' & CLC_Class != 'Other Natural Land' & Comp_N ==3 & N_Tot_YY != 0 ) )
# p1 = ggplot(data =df 
#             , aes(x =as.factor(N_Years),y = 100*Percentage,color = CLC_Class, group = CLC_Class ))  + my_theme_bw 
# p1 = p1 + geom_bar(stat = 'identity',size = 0.6)
# p1 = p1 + facet_grid(CLC_Class~FireYear) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# 
# 
# 
# # No unrecovered
# df = droplevels(subset(Data_YY, Index  == 'Med_SNDVI' & ENV_ZONE == 'All' & CLC_Class != 'Other Natural Land' & Comp_N ==3 & N_Tot_YY != 0 & N_Years != 999) )
# p1 = ggplot(data =df 
#             , aes(x =as.factor(N_Years),y = 100*Percentage,color = CLC_Class, group = CLC_Class ))  + my_theme_bw 
# p1 = p1 + geom_bar(stat = 'identity',size = 0.6)
# p1 = p1 + facet_grid(CLC_Class~FireYear) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# 
# # BY ENV_ZONE, for 1 CLC
# df = droplevels(subset(Data_YY, Index  == 'Med_SNDVI' &  CLC_Class == 'Transitional Vegetation' & Comp_N ==3 & N_Tot_YY != 0 ) )
# p1 = ggplot(data =df 
#             , aes(x =as.factor(N_Years),y = 100*Percentage,color = CLC_Class, group = CLC_Class ))  + my_theme_bw 
# p1 = p1 + geom_bar(stat = 'identity',size = 0.6)
# p1 = p1 + facet_grid(ENV_ZONE~FireYear) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# 
# # Only RDVI
# df = droplevels(subset(Data_YY, Index  == 'Med_SRDVI' & ENV_ZONE == 'All' & CLC_Class != 'Other Natural Land' & Comp_N ==3 & N_Tot_YY != 0 ) )
# p1 = ggplot(data =df 
#             , aes(x =as.factor(N_Years),y = 100*Percentage,color = CLC_Class, group = CLC_Class ))  + my_theme_bw 
# p1 = p1 + geom_bar(stat = 'identity',size = 0.6)
# p1 = p1 + facet_grid(CLC_Class~FireYear) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # No unrecovered
# df = droplevels(subset(Data_YY, Index  == 'Med_SRDVI' & ENV_ZONE == 'All' & CLC_Class != 'Other Natural Land' & Comp_N ==3 & N_Tot_YY != 0 & N_Years != 999) )
# p1 = ggplot(data =df 
#             , aes(x =as.factor(N_Years),y = 100*Percentage,color = CLC_Class, group = CLC_Class ))  + my_theme_bw 
# p1 = p1 + geom_bar(stat = 'identity',size = 0.6)
# p1 = p1 + facet_grid(CLC_Class~FireYear) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# 
# 
# # Bar plots of percentages, by year and index for a Class
# 
# df = droplevels(subset(Data_YY, CLC_Class == 'All' & ENV_ZONE == 'All' & Comp_N ==3 & N_Tot_YY != 0 & N_Years != 999) )
# p1 = ggplot(data =df 
#             , aes(x =as.factor(N_Years),y = 100*Percentage,color = CLC_Class, group = CLC_Class ))  + my_theme_bw 
# p1 = p1 + geom_line(size = 0.6)  + geom_point(size = 0.5)
# p1 = p1 + facet_wrap(FireYear~Index, ncol = 2) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# 
# 
# }  
# 
# 
# 
# 
# 
# 
# #                             
# # Data_NDVI = droplevels(subset(Data_Perc, Index == 'Med_SNDVI' & ENV_ZONE == 'All'))
# # p1 = ggplot(data = Data_NDVI , aes(x = as.factor(N_Years)))  + my_theme_bw 
# # p1 = p1 + geom_bar(aes(y = 100*Percentage, width = .85),  stat=  'identity',  color = 'grey90')
# # p1 = p1 + facet_wrap(~CLC_Class, ncol = 1) + theme(legend.position="none")
# # p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # p1 = p1 + ggtitle(expression(NDVI^R))
# # 
# # Data_RDVI = droplevels(subset(Data_Perc, Index == 'Med_SRDVI' & ENV_ZONE == 'All'))
# # p2 = ggplot(data = Data_RDVI , aes(x = as.factor(N_Years)))  + my_theme_bw 
# # p2 = p2 + geom_bar(aes(y = 100*Percentage, width = .85),  stat=  'identity', color = 'grey90') 
# # p2 = p2 + facet_wrap(~CLC_Class, ncol = 1) + theme(legend.position="none")
# # p2 = p2 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # p2 = p2 + ggtitle(expression(RDVI^R))
# # 
# # out_file = file.path(plot_folder, 'Figure_Percentages_Recovered.tif')
# # tiff(out_file, width = 4.8, height = 8.5, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
# # print(grid.arrange(p1, p2, ncol=2)) 
# # dev.off()
# # 
# # #- ----------------------------------------- - 
# # # Plot probability of fires vs recovery time, - no UNRECOVERED
# # #- ----------------------------------------- - 
# # Data_NDVI = droplevels(subset(Data_Perc, Index == 'Med_SNDVI' & N_Years !='NR' & ENV_ZONE == 'All'))
# # p1 = ggplot(data = Data_NDVI , aes(x = as.factor(N_Years)))  + my_theme_bw 
# # p1 = p1 + geom_bar(aes(y = 100*Percentage, width = .85),  stat=  'identity',  color = 'grey90')
# # p1 = p1 + facet_wrap(~CLC_Class, ncol = 1) + theme(legend.position="none")
# # p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # p1 = p1 + ggtitle(expression(NDVI^R))
# # 
# # Data_RDVI = droplevels(subset(Data_Perc, Index == 'Med_SRDVI'& N_Years !='NR' & ENV_ZONE == 'All'))
# # p2 = ggplot(data = Data_RDVI , aes(x = as.factor(N_Years)))  + my_theme_bw 
# # p2 = p2 + geom_bar(aes(y = 100*Percentage, width = .85),  stat=  'identity', color = 'grey90') 
# # p2 = p2 + facet_wrap(~CLC_Class, ncol = 1) + theme(legend.position="none")
# # p2 = p2 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # p2 = p2 + ggtitle(expression(RDVI^R))
# # 
# # out_file = file.path(plot_folder, 'Figure_Percentages_Recovered_No_Unrecovered.tif')
# # tiff(out_file, width = 4.8, height = 8.5, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
# # print(grid.arrange(p1, p2, ncol=2)) 
# # dev.off()
# # 
# # # As lines - no facet
# # Data = droplevels(subset(Data_Perc,N_Years !='NR' & ENV_ZONE == 'All'))
# # p1 = ggplot(data = Data , aes(x = as.numeric(N_Years)-1,y = 100*Percentage, group = Index, color = Index))  + my_theme_bw 
# # p1 = p1 + geom_line()+geom_point()
# # p1 = p1 + facet_wrap(~CLC_Class, ncol = 1) + theme(legend.position="bottom")
# # p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # p1 = p1 + scale_x_continuous(breaks = seq(min(as.numeric(Data$N_Years))-1,max(as.numeric(Data$N_Years))-1))+
# #   scale_colour_manual(name = '',values = c('black','grey50'), labels =c(expression(NDVI^R),expression(RDVI^R)))
# # 
# # out_file = file.path(plot_folder, 'Figure_Percentages_Recovered_No_Unrecovered_lines.tif')
# # tiff(out_file, width = 4.8, height = 8.5, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
# # print(p1) 
# # dev.off()
# # 
# # #- ----------------------------------------- - 
# # # Plot percentage of unrecovered fires vs time from fire,
# # #- ----------------------------------------- - 
# # levels(Data_YY$N_Years) = c('0', '1','2','3','4','5','6','7','8','9','NR')
# # Data_YY = droplevels(subset(Data_YY, Comp_N == medwdt & CLC_Class!= 'Other Natural Land' & ENV_ZONE == 'All'))
# # Data_YY$N_Years = ordered(Data_YY$N_Years)
# # levels(Data_YY$N_Years) = c('0', '1','2','3','4','5','6','7','8','9','NR')
# # Data_YY$Comp_N = ordered(as.numeric(as.character(Data_YY$Comp_N)))
# # levels(Data_YY$CLC_Class) = c('All LC Classes','Broadleaved Forests','Coniferous Forests','Mixed Forests','Schleropyllus Vegetation','Transitional W/S')
# # Data_YY = droplevels(subset(Data_YY, N_Years =='NR'))
# # 
# # # As bar plots
# # 
# # Data_NDVI = droplevels(subset(Data_YY, Index == 'Med_SNDVI' & N_Years =='NR' & ENV_ZONE == 'All'))
# # 
# # p1 = ggplot(data = Data_NDVI , aes(x = as.factor(FireYear)))  + my_theme_bw +
# #   opts(axis.text.x = element_text( hjust = 1,vjust = 1, size = 10, angle = 45 ))
# # p1 = p1 + geom_bar(aes(y = 100*Percentage, width = .85),  stat=  'identity',  color = 'grey90')
# # p1 = p1 + facet_wrap(~CLC_Class, ncol = 1) + theme(legend.position="none")
# # p1 = p1 + xlab('Fire Year') + ylab ('% of Burnt Areas UnRecovered as of 2012')
# # p1 = p1 + ggtitle(expression(NDVI^R))+  scale_colour_brewer(palette = "Set2")+coord_cartesian(ylim=c(0,90))
# # 
# # 
# # Data_RDVI = droplevels(subset(Data_YY, Index == 'Med_SRDVI' & N_Years =='NR' & ENV_ZONE == 'All'))
# # 
# # p2 = ggplot(data = Data_RDVI , aes(x = as.factor(FireYear)))  + my_theme_bw +opts(axis.text.x = element_text( hjust = 1,vjust = 1, size = 10, angle = 45 ))
# # p2 = p2 + geom_bar(aes(y = 100*Percentage, width = .85),  stat=  'identity',  color = 'grey90')
# # p2 = p2 + facet_wrap(~CLC_Class, ncol = 1) + theme(legend.position="none")
# # p2 = p2 + xlab('Fire Year') + ylab ('% of Burnt Areas UnRecovered as of 2012')
# # p2 = p2 + ggtitle(expression(RDVI^R))+  scale_colour_brewer(palette = "Set2")+coord_cartesian(ylim=c(0,90))
# # 
# # out_file = file.path(plot_folder, 'Figure_Percentages_UnRecovered_vs_FireYear_barplot.tif')
# # tiff(out_file, width = 4.8, height = 8.5, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
# # print(grid.arrange(p1, p2, ncol=2)) 
# # dev.off()
# # 
# # 
# # # As line plots
# # 
# # p1 = ggplot(data = Data_NDVI , aes(x = as.numeric(FireYear)))  + my_theme_bw 
# # p1 = p1 + geom_line(aes(y = 100*Percentage, color = CLC_Class))
# # p1 = p1 + facet_wrap(~CLC_Class, ncol = 1) + theme(legend.position="none")
# # p1 = p1 + xlab('Fire Year') + ylab ('% of Burnt Areas UnRecovered as of 2012')
# # p1 = p1 + ggtitle(expression(NDVI^R))+coord_cartesian(ylim=c(0,90))
# # p1 = p1 + scale_x_continuous(breaks = seq(min(as.numeric(Data_NDVI$FireYear)),max(as.numeric(Data_NDVI$FireYear))))+
# #   scale_colour_brewer(palette = "Set2")
# # 
# # p2 = ggplot(data = Data_RDVI , aes(x = as.numeric(FireYear)))  + my_theme_bw 
# # p2 = p2 + geom_line(aes(y = 100*Percentage, color = CLC_Class))
# # p2 = p2 + facet_wrap(~CLC_Class, ncol = 1) + theme(legend.position="none")
# # p2 = p2 + xlab('Fire Year') + ylab ('% of Burnt Areas UnRecovered as of 2012')
# # p2 = p2 + ggtitle(expression(RDVI^R))+coord_cartesian(ylim=c(0,90))
# # p2 = p2 + scale_x_continuous(breaks = seq(min(as.numeric(Data_NDVI$FireYear)),max(as.numeric(Data_NDVI$FireYear))))+
# #   scale_colour_brewer(palette = "Set2")
# # 
# # # As line plots no  faceted
# # 
# # p1 = ggplot(data = Data_YY , aes(x = as.numeric(FireYear),y = 100*Percentage, group = Index, color = Index))  + my_theme_bw 
# # p1 = p1 + geom_line()+geom_point()+opts(axis.text.x = element_text( hjust = 1,vjust = 1, size = 10, angle = 45 ))
# # p1 = p1 + facet_wrap(~CLC_Class, ncol = 1) + theme(legend.position="bottom")
# # p1 = p1 + xlab('Fire Year') + ylab ('% of Burnt Areas UnRecovered as of 2012')+coord_cartesian(ylim=c(0,90))
# # p1 = p1 + scale_x_continuous(breaks = seq(min(as.numeric(Data_YY$FireYear)),max(as.numeric(Data_YY$FireYear))))+
# #   scale_colour_manual(name = '',values = c('black','grey50'), labels =c(expression(NDVI^R),expression(RDVI^R)))
# # 
# # out_file = file.path(plot_folder, 'Figure_Percentages_UnRecovered_vs_FireYear.tif')
# # tiff(out_file, width = 4.8, height = 8.5, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 600 )
# # print(p1) 
# # dev.off()
# # 
# # 
# # 
# # 
# # 
# # p1 = ggplot(data = Data_NDVI , aes(x = as.numeric(FireYear)))  + my_theme_bw 
# # p1 = p1 + geom_line(aes(y = 100*Percentage, color = CLC_Class))
# # p1 = p1 + facet_wrap(~Index, ncol = 1) + theme(legend.position="none")
# # p1 = p1 + xlab('Fire Year') + ylab ('% of Burnt Areas UnRecovered as of 2012')
# # p1 = p1 + ggtitle(expression(NDVI^R))+coord_cartesian(ylim=c(0,90))
# # p1 = p1 + scale_x_continuous(breaks = seq(min(as.numeric(Data_NDVI$FireYear)),max(as.numeric(Data_NDVI$FireYear))))+
# #   scale_colour_brewer(palette = "Set2")
# # 
# # 
# # Data = droplevels(subset(Data_Perc,N_Years !='NR' & ENV_ZONE == 'All'))
# # Data = droplevels(subset(Data_Perc,N_Years !='NR' ))
# # cumprob = ddply(Data, .(Index,CLC_Class, ENV_ZONE), summarize, prob = Percentage, cumprob = cumsum(Percentage), N_Years = N_Years)
# # 
# # p1 = ggplot(data = cumprob , aes(x = as.numeric(N_Years)-1,y = 100*prob, group = CLC_Class, color = CLC_Class))  + my_theme_bw 
# # p1 = p1 + geom_line()+geom_point()
# # p1 = p1 + facet_wrap(~Index, ncol = 1) + theme(legend.position="bottom")
# # p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # p1 = p1 + scale_x_continuous(breaks = seq(min(as.numeric(Data$N_Years))-1,max(as.numeric(Data$N_Years))-1))+
# #   scale_colour_manual(name = '',values = c('black','grey50'), labels =c(expression(NDVI^R),expression(RDVI^R)))
# # 
# # 
# # Data = droplevels(subset(Data_Perc,N_Years !='NR' & ENV_ZONE == 'All'))
# # p1 = ggplot(data = Data , aes(x = as.numeric(N_Years)-1,y = 100*Percentage, group = CLC_Class, color = CLC_Class))  + my_theme_bw 
# # p1 = p1 + geom_line()+geom_point()
# # p1 = p1 + facet_wrap(~Index, ncol = 1) + theme(legend.position="bottom")
# # p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # p1 = p1 + scale_x_continuous(breaks = seq(min(as.numeric(Data$N_Years))-1,max(as.numeric(Data$N_Years))-1))+
# #   scale_colour_manual(name = '',values = c('black','grey50'), labels =c(expression(NDVI^R),expression(RDVI^R)))
# # 
# # 
# # # Plot  probability vs index and envzone - old method
# # p1 = ggplot(data = subset(cumprob, CLC_Class != 'Other Natural Land') , aes(x = as.numeric(N_Years),y = 100*prob, group = CLC_Class, color = CLC_Class))  + my_theme_bw 
# # p1 = p1 + geom_line()+geom_point()
# # p1 = p1 + facet_wrap(Index~ENV_ZONE) + theme(legend.position="bottom")
# # p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # 
# # # Plot cumulated probability vs index and envzone - old method
# # 
# # p1 = ggplot(data = subset(cumprob, CLC_Class != 'Other Natural Land') , aes(x = as.numeric(N_Years),y = 100*cumprob, group = CLC_Class, color = CLC_Class))  + my_theme_bw 
# # p1 = p1 + geom_line()+geom_point()
# # p1 = p1 + facet_wrap(Index~ENV_ZONE) + theme(legend.position="bottom")
# # p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # 
# # 
# # Data_YY = droplevels(subset(Data_YY, Comp_N ==3))
# # Data_YY$Percentage[which(is.finite(Data_YY$Percentage) == F)] = 0
# # 
# # 
# # #  New method for probability calculation
# # 
# # Newprobs = ddply(Data_YY, .(ENV_ZONE,CLC_Class, Index) , function(df) comp_probs(df),.progress = "text" )
# # 
# # comp_probs = function(df) {
# # # df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & CLC_Class == 'All' & Comp_N ==3 & Index == 'Med_SRDVI'))
# # Probs_greater = array(data = NA, dim = 1+length(levels(df$FireYear)))
# # Probs_greater [1] = 1
# # for (k in 0:max_N_Years_after) {
# #   
# #   df_sub = subset(df, N_Years > k)
# #   Recovered_sum = sum(subset(df_sub, FireYear < (2012-k) & N_Years != 999)$N_Rec_YY)
# #   Unrecovered_sum = sum(subset(df_sub, FireYear < (2012-k+1) & N_Years == 999)$N_Rec_YY)
# #   Tot_sum = sum(unique(subset(df_sub, FireYear < (2012-k+1) )$N_Tot))
# #   Probs_greater[k+2] = (Recovered_sum+Unrecovered_sum)/(Tot_sum)
# #   
# # }
# # Probs_greater[which(is.finite(Probs_greater) == F)] = 0
# # Probs = c(-diff(Probs_greater),Probs_greater[length(Probs_greater)])
# # CumProbs = cumsum(Probs)
# # out = data.frame(Probs = Probs, CumProbs = CumProbs, N_Years = seq(0,10))
# # # if (min(Probs) <0 ) browser()
# # out
# # }
# # 
# # 
# # p1 = ggplot(data = subset(Newprobs, ENV_ZONE == 'All'& CLC_Class != 'Other Natural Land') , aes(x = as.numeric(N_Years)-1,y = 100*CumProbs, group = CLC_Class, color = CLC_Class))  + my_theme_bw 
# # p1 = p1 + geom_line()+geom_point()
# # p1 = p1 + facet_wrap(~Index, ncol = 1) + theme(legend.position="bottom")
# # p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # 
# # Data = droplevels(subset(Newprobs,N_Years !='NR' & ENV_ZONE == 'All'& CLC_Class != 'Other Natural Land'))
# # p1 = ggplot(data = Data , aes(x = as.numeric(N_Years),y = 100*Probs, group = Index, color = Index))  + my_theme_bw 
# # p1 = p1 + geom_line()+geom_point()
# # p1 = p1 + facet_wrap(~CLC_Class, ncol = 1) + theme(legend.position="bottom")
# # p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # p1 = p1 + scale_x_continuous(breaks = seq(min(as.numeric(Data$N_Years)),max(as.numeric(Data$N_Years))))+
# #   scale_colour_manual(name = '',values = c('black','grey50'), labels =c(expression(NDVI^R),expression(RDVI^R)))
# # 
# # p1 = ggplot(data = subset(Newprobs, CLC_Class != 'Other Natural Land') , aes(x = as.numeric(N_Years),y = 100*CumProbs, group = CLC_Class, color = CLC_Class))  + my_theme_bw 
# # p1 = p1 + geom_line()+geom_point()
# # p1 = p1 + facet_wrap(Index~ENV_ZONE) + theme(legend.position="bottom")
# # p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # 
# # p1 = ggplot(data = subset(Newprobs, CLC_Class != 'Other Natural Land') , aes(x = as.numeric(N_Years),y = 100*Probs, group = CLC_Class, color = CLC_Class))  + my_theme_bw 
# # p1 = p1 + geom_line()+geom_point()
# # p1 = p1 + facet_wrap(Index~ENV_ZONE) + theme(legend.position="bottom")
# # p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# # 
# # 
# # 
# # 
# # 
# # 
# # df = droplevels(subset(Data_YY, CLC_Class != 'Other Natural Land' & ENV_ZONE == 'All' & Comp_N ==3 & Percentage != 0 & N_Years != 999) )
# # p1 = ggplot(data =subset(df , CLC_Class == 'All')
# #             , aes(x =as.factor(N_Years),y = 100*Percentage,color = FireYear, group = FireYear ))  + my_theme_bw 
# # p1 = p1 + geom_line(size = 0.6)  + geom_point(size = 0.5)
# p1 = p1 + facet_wrap(FireYear~Index, ncol = 2) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# 
# 
# # Plot of results separated by fire year
# 
# 
# 
# 
# 
# 
# fin = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/50/Med_SNDVI/Stat_Analysis/ENV/6%/Stat_Analysis_Med_SNDVI_2000_2012_META_RData.RData'
# load(fin)
# 
# data = droplevels(subset(recov_stat, CLC_Class == 'All' & ENV_ZONE == 'MDN' & Comp_N == 3 ))
# p1 = ggplot(data =data
#             , aes(x = as.numeric(N_Signif_Rec),color = FireYear, group = FireYear ))  + my_theme_bw 
# p1 = p1 + geom_histogram(aes(y = ..frequency..), binwidth = 1)
# p1 = p1 + geom_freqpoly( binwidth = 1)
# p1 = p1 + facet_wrap(FireYear~Index, ncol = 1) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# 
# p1 = ggplot(data =data
#             , aes(x = as.factor(FireYear),color = FireYear, group = FireYear ))  + my_theme_bw 
# p1 = p1 + geom_boxplot(aes(y=N_Signif_Rec))+geom_jitter(aes(y=N_Signif_Rec))
# p1 = p1 + facet_wrap(Index, ncol = 1) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# 
# 
# 
# dfc <- summarySE(df, measurevar="len", groupvars=c("FireYear","CLC_Class", "ENV_ZONE"))
# 
# a = ddply(df,.(Index, CLC_Class, ENV_ZONE, N_Years) , function(df) ci = comp_ci(df))
# comp_ci= function(df) {
#   # df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & CLC_Class == 'All' & Comp_N ==3 & Index == 'Med_SRDVI'))
#   #    browser()
#   ci = ci (df$Percentage)
#   ci$FireYear = df$N_Years[1]
#   ci
# }
# p1 = ggplot(a, aes(x=N_Years, y=mean))
# # p1 = p1 +  geom_bar(position=position_dodge(), stat="identity")+ my_theme_bw
# p1 = p1 +  geom_point()+ my_theme_bw
# p1 = p1 + geom_errorbar(aes(ymin=lower95ci, ymax=upper95ci), width=.1, color = 'red') #+ geom_point()
# p1 = p1 + facet_wrap(CLC_Class~Index, ncol = 2) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')+coord_cartesian(ylim = c(0,1))
# 
# p1 = ggplot(a, aes(x=N_Years, y=mean))
# # p1 = p1 +  geom_bar(position=position_dodge(), stat="identity")+ my_theme_bw
# p1 = p1 +  geom_point()+ my_theme_bw
# p1 = p1 + geom_errorbar(aes(ymin=mean-sd, ymax=mean + sd), width=.1, color = 'red') #+ geom_point()
# p1 = p1 + facet_wrap(CLC_Class~Index, ncol = 2) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')+coord_cartesian(ylim = c(0,0.5))
# 
# 
# 
# comp_ci_boot= function(df) {
#   # df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & CLC_Class == 'All' & Comp_N ==3 & Index == 'Med_SRDVI'))
#   #        browser()
#   bootmed=apply(matrix(sample(df$Percentage,rep=TRUE,10^4*length(df$Percentage)),nrow=10^4),1,median)
#   limits = quantile(bootmed,c(.05,0.95))
#   ci = data.frame(N_Years = df$N_Years[1], Perc =mean(df$Percentage, na.rm = T), ll = limits[1], ul = limits[2])    
#   
# }
# 
# a = ddply(df,.(Index, CLC_Class, ENV_ZONE, N_Years) , function(df) ci = comp_ci_boot(df), .progress = "text")
# p1 = ggplot(a, aes(x=N_Years, y=Perc))
# # p1 = p1 +  geom_bar(position=position_dodge(), stat="identity")+ my_theme_bw
# p1 = p1 +  geom_point()+ my_theme_bw
# p1 = p1 + geom_errorbar(aes(ymin=Perc-ll, ymax=Perc+ul), width=.1, color = 'red') #+ geom_point()
# p1 = p1 + facet_wrap(CLC_Class~Index, ncol = 2) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# 
# 
# 
# p1 = ggplot(data =df 
#             , aes(x =as.factor(N_Years),y = 100*Percentage,color = CLC_Class ))  + my_theme_bw 
# p1 = p1 + geom_boxplot(size = 0.6)  + geom_jitter(aes(label = FireYear),size = 0.6, color = 'black')
# p1 = p1 + facet_wrap(CLC_Class~Index, ncol = 2) + theme(legend.position="bottom")
# p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Probability [%]')
# 
# pippo = ddply(subset(Data_YY, Comp_N ==3), .(CLC_Class,ENV_ZONE, FireYear, Index), summarize, sum = sum(Percentage))
# head(pippo     )
