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
                                axis.text.x = element_text( size = 9), axis.text.y = element_text(size =9), 
                                axis.title.x = element_text( vjust = 0, size = 10, angle = 0 ), 
                                axis.title.y = element_text(hjust = 0.5, vjust = 0.3,size = 10, angle = 90))
max_N_Years_after = 9

# In_File_SNDVI = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/10/Med_SNDVI/Stat_Analysis/9.5%/Percentages/Percentages_9.5%.RData'

# Define files to be analyzed
SNDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SNDVI/Stat_Analysis/'
SRDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SRDVI/Stat_Analysis/'

files_SNDVI = c('10/9.5%/Eroded/Percentages/Percentages_Area9.5%.RData')
files_SRDVI = c('10/11.5%/Eroded/Percentages/Percentages_Area11.5%.RData')

In_Files_SNDVI = file.path(SNDVI_Folder,files_SNDVI) 
In_Files_SRDVI = file.path(SRDVI_Folder,files_SRDVI)

out_plot_folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Plots/Probabilities'
dir.create (out_plot_folder)


type_codes = c('Eroded','Eroded')

for(cy in (1)) {
  In_File_SNDVI = In_Files_SNDVI[cy]   ; In_File_SRDVI = In_Files_SRDVI[cy]   ; out_plot_folder = out_plot_folders[cy]
  type_code = type_codes[cy]  ; out_pdf = out_pdf_names[cy]; str_opt = str_options[cy]
  
  plot_probabilities(In_File_SNDVI,In_File_SRDVI, out_plot_folder, type_code)
}

plot_probabilities = function (In_File_SNDVI,In_File_SRDVI, out_plot_folder, type_code, common_fires_flag,str_opt){
#   
#   dir.create(out_plot_folder, recursive = T)
#   pdf_out = file.path(out_plot_folder,out_pdf)
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
  levels(Data_Perc$CLC_Class)=c('All~Classes','Broadleaved~For.','Coniferous~For.','Mixed~For.','Schleropyllus','Transitional~W/S')
  levels(Data_Perc$Index) = c('NDVI^R','RDVI^R')
  Data_Perc$Area_Cat = factor(Data_Perc$Area_Cat, levels = unique(Data_Perc$Area_Cat))
  levels(Data_Perc$Area_Cat) = c('All','0-250','250-500','500-1000','1000-5000','5000-10000','10000-50000')
  
  Data_YY = droplevels(subset(Data_YY, Comp_N == medwdt & CLC_Class!= 'Other Natural Land'))
  Data_YY$N_Years = ordered(Data_YY$N_Years, labels = c('0', '1','2','3','4','5','6','7','8','9','NR'))
  Data_YY$Comp_N = ordered(as.numeric(as.character(Data_YY$Comp_N)))
  levels(Data_YY$CLC_Class)=c('All~Classes','Broadleaved~For.','Coniferous~For.','Mixed~For.','Schleropyllus','Transitional~W/S')
  levels(Data_YY$Index) = c('NDVI^R','RDVI^R')
  Data_YY$Area_Cat = factor(Data_YY$Area_Cat, levels = unique(Data_YY$Area_Cat))
  levels(Data_YY$Area_Cat) = c('All','0-250','250-500','500-1000','1000-5000','5000-10000','10000-50000')
 
  
  Data_YY <- within(Data_YY, Area_Cat_New <- recode(Area_Cat, 'c("0-250", "250-500")="< 500";
                                                               c("500-1000")="500 - 1000";
                                                               c("1000-5000")="1000 - 5000";
                                                               c("5000-10000")="5000 - 10000";
                                                               c("10000-50000")="> 10000"'))
    
  Data_YY$Area_Cat_New = factor(Data_YY$Area_Cat_New, levels = c("< 500", "500 - 1000","1000 - 5000","5000 - 10000","> 10000","All"))
  
  levels(Data_YY$Area_Cat_New) =  c("'<'~500", "500~-~1000","1000~-~5000","5000~-~10000","'>'~10000","All")

  #- ----------------------------------------- - 
  # Plot Percentage of fires vs recovery time, as boxplots showing variance of % in != years
  #- ----------------------------------------- - 
  
  # Boxplots of percentages, by Class and index. Values of different years are the different points in the bplot
  df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0 & N_Years != 'NR' & Area_Cat =='All') )
  df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0  & Area_Cat =='All') )
  p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage))+ my_theme_bw 
  p1 = p1 + geom_boxplot(size = 0.2, outlier.colour = 'transparent', color = 'black')#  + geom_jitter(size =2, color = 'blue')
  p1 = p1 + geom_point(pch = 1,size = 1.4, color = 'grey50', position = position_jitter(height = 0 , width = 0.2)) 

  p1 = p1 + facet_grid(CLC_Class~Index, labeller=label_parsed, drop = FALSE) + theme(legend.position="none")
  p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]') + theme(strip.text.y = element_text(size = 8, angle = 90))
  p1 = p1 + coord_cartesian(ylim =c(0,110))
  p1 = p1 + theme( strip.background= element_rect(colour = 'black'),
                   panel.border = element_rect(colour = "black"))
#   title = paste("Distributions of Recovery Time Probability recorded in different Years, by CLC Class (All Areas)\n\n",str_opt, sep = '')
#   p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
  
  out_file = file.path(out_plot_folder,paste('Boxplot_Perc_CLC_Class_AllArea_', type_code,'.tif', sep = ''))
  tiff(out_file, width = 5.5, height = 7.5, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
  dev.off()
  
  # Boxplots of percentages, by Area_Cat and index for the "ALL" CLCL_Class. Values of different years are the different points in the bplot
  
  df = (subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0 & N_Years != 'NR' & CLC_Class =='All~Classes') )
  df = (subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0  & CLC_Class =='All~Classes') )
  p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage,color = Area_Cat_New ))+ my_theme_bw 
  p1 = p1 + geom_boxplot(size = 0.2, outlier.colour = 'transparent', color = 'black')#  + geom_jitter(size =2, color = 'blue')
  p1 = p1 + geom_point(pch = 1,size = 1.4, color = 'grey50', position = position_jitter(height = 0 , width = 0.2)) 

  p1 = p1 + facet_grid(Area_Cat_New~Index, labeller=label_parsed, drop = F) + theme(legend.position="none")
  p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]') + theme(strip.text.y = element_text(size = 8, angle = 90))
  p1 = p1 + coord_cartesian(ylim =c(0,110))
  p1 = p1 + theme( strip.background= element_rect(colour = 'black'),
                   panel.border = element_rect(colour = "black"))
 
  out_file = file.path(out_plot_folder,paste('Boxplot_Perc_Area_Cat_AllCLC_', type_code,'.tif', sep = ''))
  tiff(out_file, width = 5.5, height = 7.5, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
   dev.off()
  
  #- ----------------------------------------- - 
  # Plot Percentage of fires vs recovery time, separated by year an Index
  #- ----------------------------------------- - 
  
  
  for (Ind in c('NDVI^R','RDVI^R')) {
    # Barplots, percentages, different for each index - vs CLC_Class for all area
    
    # Select a subset of fire years 
    
    df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0 & Area_Cat =='All' & Index == Ind) )
    p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage, group = CLC_Class ))  + my_theme_bw 
    p1 = p1 + geom_bar(stat = 'identity',size = 0.2, color = 'black', fill = 'grey50', width = 0.80)
    p1 = p1 + facet_grid(CLC_Class~FireYear, labeller=label_parsed, drop = FALSE) + theme(legend.position="none")
    p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]')+ theme(strip.text.y = element_text(size = 7, angle = 90))
    p1 = p1 + coord_cartesian(ylim =c(0,110))
    p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
    p1 = p1 + theme( strip.background= element_rect(colour = 'black'),
                   panel.border = element_rect(colour = "black"))
    out_file = file.path(out_plot_folder,paste('Barplot_Perc_CLC_Class_YY_',Ind,'_', type_code,'.tif', sep = ''))
    tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
    print(p1)
    dev.off()
    
    # Same plot, but on  a subset of available fire years 
    
    max_y = 200
    
    sub_years = c('2003','2007')    ; n_years = 2
    rects = data.frame(FireYear = rep(sub_years,6), CLC_Class = rep(levels(df$CLC_Class), each = n_years),
                       xmin = rep(c(9,6), 6), xmax = rep(c(10,10),6), ymin = -10, ymax = rep(c(-5,max_y),6))
    
    sub_2003 = droplevels(subset(df, FireYear == '2003' & N_Years >5))
    compsum = function(df) {sum = sum(df$Percentage, na.rm = T)}
    sums_2003 =  ddply(sub_2003, .(CLC_Class) , function(df) compsum(df))
    rects2 = rects
    rects2$xmin = 10.5 ; rects2$xmax = 11.5; rects2$ymin = -10; rects2$ymax = c(-5,100*sums_2003$V1[1], -5,100*sums_2003$V1[2],
                                                                                -5,100*sums_2003$V1[3], -5,100*sums_2003$V1[4],
                                                                                -5,100*sums_2003$V1[5],-5,100*sums_2003$V1[6])
    
    df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0 & Area_Cat =='All' & Index == Ind & FireYear %in% sub_years) )
    p1 = ggplot()  + my_theme_bw 
    
    p1 = p1 + geom_bar(data =df, aes(x =as.factor(N_Years),y = 100*Percentage, group = CLC_Class ),stat = 'identity',size = 0.2, color = 'black', fill = 'grey50', width = 0.80)
    p1 = p1 +geom_rect(data = rects2, aes(xmin = xmin+0.13, xmax = xmax-0.13, ymin = ymin, ymax = ymax), 
                       fill = 'transparent', colour = 'grey75', linetype = 2)
    p1 = p1 +geom_rect(data = rects, aes(xmin = xmin+0.5, xmax = xmax+0.5, ymin = ymin, ymax = ymax), fill = 'grey90')
    p1 = p1 + facet_grid(CLC_Class~FireYear, labeller=label_parsed, drop = FALSE) + theme(legend.position="none")
    p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]')+ theme(strip.text.y = element_text(size = 7, angle = 90))
    p1 = p1 + coord_cartesian(ylim =c(0,55))
    p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
    p1 = p1 + theme( strip.background= element_rect(colour = 'black'),
                   panel.border = element_rect(colour = "black"))
    
  
#     p1
#     
#     
#     
#     p1 + geom_rect(xmin = 9.5, xmax = 10.5, ymin = 0, ymax = 30)
#     
#     
#     sub_years = c('2003','2005','2007','2008')
#     sub_years = c('2003','2005','2007')    ; n_years = 3
#     rects = data.frame(FireYear = rep(sub_years,6), CLC_Class = rep(levels(df$CLC_Class), each = n_years),
#                        xmin = rep(c(9,7,5), 6), xmax = rep(c(10,10,10),6), ymin = -10, ymax = rep(c(-5,max_y,max_y),6))
#     
#     
#     out_file = file.path(out_plot_folder,paste('Barplot_Perc_CLC_Class_YY_',Ind,'_', type_code,'.tif', sep = ''))
#     tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
#     print(p1)
#     dev.off()
    
    
    # Barplots, Number, different for each index - vs CLC_Class for all area
    
    df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0 & Area_Cat =='All' & Index == Ind) )
    p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = N_Rec_YY,color = Area_Cat, group = Area_Cat ))  + my_theme_bw 
    p1 = p1 + geom_bar(stat = 'identity',size = 0.2, color = 'black', fill = 'grey50', width = 0.80)
    p1 = p1 + facet_grid(CLC_Class~FireYear, labeller=label_parsed, scales = 'free_y', drop = FALSE) + theme(legend.position="none")
    p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Number of Burnt Areas')+ theme(strip.text.y = element_text(size = 8, angle = 90))
   
#     p1 = p1 + coord_cartesian(ylim =c(0,110))
    p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
    p1 = p1 + theme( strip.background= element_rect(colour = 'black'),
                   panel.border = element_rect(colour = "black"))
    out_file = file.path(out_plot_folder,paste('Barplot_Number_Area_YY_',Ind,'_', type_code,'.tif', sep = ''))
    tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
    print(p1)
    dev.off()
   
    
  }
  
  # Barplots - Percentages two indexes in same figure
  df = (subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0 & Area_Cat =='All') )
  p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage,color = Index, group = Index, fill = Index))  + my_theme_bw 
  p1 = p1+geom_bar(stat = 'identity', position = position_dodge(),width = 0.8)
  p1 = p1 + facet_grid(CLC_Class~FireYear, labeller=label_parsed, drop = FALSE) + theme(legend.position="bottom")
  p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]')+ theme(strip.text.y = element_text(size = 8, angle = 90))
  p1 = p1+ scale_fill_manual(name = '',values = c('grey75','grey50'), labels =c(expression(NDVI^R),expression(RDVI^R)))
  p1 = p1+ scale_color_manual(name = '',values = c('black','black'), labels =c(expression(NDVI^R),expression(RDVI^R)))
  p1 = p1 + theme( strip.background= element_rect(colour = 'black'),
                   panel.border = element_rect(colour = "black"))
  p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  p1 = p1 + coord_cartesian(ylim =c(0,110))   
  out_file = file.path(out_plot_folder,paste('Lineplot_Perc_CLC_Class_YY_bothIndexes',type_code,'.tif', sep = ''))
  tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
  dev.off()
  
  df = (subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0 & Area_Cat =='All') )
  p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage,color = Index, group = Index, fill = Index))  + my_theme_bw 
  p1 = p1+geom_point(stat = 'identity', position = position_dodge(width = 1.2),pch = 21,size = 1, col = 'black',lwd=0.1)
  p1 = p1 + facet_grid(CLC_Class~FireYear, labeller=label_parsed, drop = FALSE) + theme(legend.position="bottom")
  p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]')+ theme(strip.text.y = element_text(size = 8, angle = 90))
  p1 = p1+ scale_fill_manual(name = '',values = c('black','grey75'), labels =c(expression(NDVI^R),expression(RDVI^R)))
#   p1 = p1+ scale_color_manual(name = '',values = c('grey75','black'), labels =c(expression(NDVI^R),expression(RDVI^R)))
  p1 = p1 + theme( strip.background= element_rect(colour = 'black'),
                   panel.border = element_rect(colour = "black"))
  p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
  p1 = p1 + coord_cartesian(ylim =c(0,110))   
  out_file = file.path(out_plot_folder,paste('Pointplot_Perc_CLC_Class_YY_bothIndexes',type_code,'.tif', sep = ''))
  tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
  print(p1)
  dev.off()
}



# Use this to compute and plot "probabilities" of recover after YY years. First retrieve correct ">9" percentages from Data_YY
# and join to Data_Perc. Then remove incorrect previous percentages for YY=NR and replace. Then plot - Substantially 
# Abandoned approach. 
# 
# Data_YY_sub = droplevels(subset(Data_YY, FireYear == 2003 &  N_Years =='NR' & Index == Ind))
# Data_YY_sub [5] =NULL
# Data_YY_sub[5] = 'NR_Ok'
# 
# pro =droplevels(subset(Data_Perc, Index ==Ind & N_Years !='NR'))
# pro = rbind(pro,Data_YY_sub )
# pro_all = droplevels(subset(pro, Area_Cat =='All'))
# 
# p = ggplot(pro_all, aes(x = (N_Years), y = Percentage, color = CLC_Class, shape = CLC_Class, group = CLC_Class))
# p = p +geom_point()+geom_line()+my_theme_bw
# p = p + facet_wrap(~CLC_Class)
# p
# 
# p = ggplot(pro_all, aes(x = (N_Years), y = Percentage, color = CLC_Class, shape = CLC_Class, group = CLC_Class))
# p = p +geom_bar(stat= 'identity')+geom_line()+my_theme_bw
# p = p + facet_wrap(~CLC_Class)
# p
# 
# cumsum = ddply(pro_sub_all, .(CLC_Class), summarise, cumsum = cumsum(Percentage), N_Years = levels(N_Years))
# p = ggplot(cumsum, aes(x = (N_Years), y = cumsum, color = CLC_Class, shape = CLC_Class, group = CLC_Class))
# p = p +geom_point()+geom_line()+my_theme_bw
# p = p + facet_wrap(~CLC_Class)
# p
# 
# 
# 
# pro_sub = droplevels(subset(pro, N_Years != 'NR'))
# levels(pro_sub$N_Years)[11] = 'NR'
# pro_sub_all = droplevels(subset(pro_sub, Area_Cat =='All'))
# tot = ddply(pro_sub_all, .(CLC_Class), summarise, tot = sum(Percentage))
# 
# pro_all = droplevels(subset(pro_sub, N_Years !='NR' & Area_Cat =='All'))
# tot1 = ddply(pro_all, .(CLC_Class), summarise, tot = sum(Percentage))
# 
# p = ggplot(pro_sub_all, aes(x = as.factor(N_Years), y = Percentage))
# p = p +geom_bar(stat = 'identity')+geom_line()+my_theme_bw
# p = p + facet_wrap(~CLC_Class, ncol = 1)
# p
# 
# p = ggplot(pro_sub_all, aes(x = (N_Years), y = Percentage, color = CLC_Class, shape = CLC_Class, group = CLC_Class))
# p = p +geom_point()+geom_line()+my_theme_bw
# p = p + facet_wrap(~CLC_Class, ncol = 1)
# p
# 
# Data_YY2 = subset(Data_YY, Index = Ind)
# Data_YY2$N_Years_num = as.numeric(Data_YY2$N_Years)-1
# Data_YY2$FireYear_num = as.numeric(as.character(Data_YY2$FireYear))
# Data_YY2$Percentage[which(is.finite(Data_YY$Percentage) == F)] = 0
# 
# Data_YY3 = droplevels(subset(Data_YY2, Area_Cat == 'All' ))
# Newprobs2 = ddply(Data_YY3, .(CLC_Class) , function(df) comp_probs(df),.progress = "text" )
# 
# comp_probs = function(df) {
# # df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & CLC_Class == 'All' & Comp_N ==3 & Index == 'Med_SRDVI'))
# Probs_greater = array(data = NA, dim = 1+length(levels(df$FireYear)))
# 
# Probs_greater [1] = 1
# # browser()
# for (k in 0:max_N_Years_after) {
# #    browser()  
#   df_sub = subset(df, N_Years > k)
#   Recovered_sum = sum(subset(df_sub, FireYear_num < (2012-k) & N_Years_num != 10)$N_Rec_YY)
#   Unrecovered_sum = sum(subset(df_sub, FireYear_num < (2012-k+1) & N_Years_num == 10)$N_Rec_YY)
#   Tot_sum = sum(subset(df_sub, FireYear_num < (2012-k+1) &  N_Years_num == 10)$N_Tot)
#   Probs_greater[k+2] = (Recovered_sum+Unrecovered_sum)/(Tot_sum)
# #   browser()
# }
# Probs_greater[which(is.finite(Probs_greater) == F)] = 0
# Probs = c(-diff(Probs_greater),Probs_greater[length(Probs_greater)])
# CumProbs = cumsum(Probs)
# out = data.frame(Probs = Probs,Probs_greater,  CumProbs = CumProbs, N_Years = seq(0,10))
# # if (min(Probs) <0 ) browser()
# out
# }
# 
# p = ggplot(Newprobs2, aes(x = N_Years, y = Probs_greater, color = CLC_Class, group = CLC_Class))
# p = p +geom_line(stat = 'identity')+my_theme_bw
# p = p + facet_wrap(~CLC_Class)
# p
# 
# p = ggplot(Newprobs2, aes(x = N_Years, y = CumProbs))
# p = p +geom_bar(stat = 'identity')+my_theme_bw
# p = p + facet_wrap(~CLC_Class)
# p
# 
# p = ggplot(Newprobs2, aes(x = N_Years, y = Probs, color = CLC_Class, group = CLC_Class))
# p = p +geom_bar(stat = 'identity')+my_theme_bw
# p = p + facet_wrap(~CLC_Class)+ylim(0,0.60)
# p
# 
# p = ggplot(Newprobs2, aes(x = N_Years, y = Probs, color = CLC_Class, group = CLC_Class))
# p = p +geom_line(stat = 'identity')+my_theme_bw
# p = p + facet_wrap(~CLC_Class)+ylim(0,0.60)
# p
# 
# data = droplevels(subset(recov_stat, N_Signif_Rec != 'NR'))
# dd = ggplot(data, aes (x = CLC_Class, y = as.numeric(N_Signif_Rec)-1))
# dd = dd + geom_boxplot()  +facet_wrap(~FireYear, scales = 'free_y')
# dd
# 
# 



      # Barplots, different for each index - vs Area for all CLC_Class
#     df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Tot_YY != 0  & CLC_Class =='All~LC~Classes' & Index == Ind) )
#     p1 = ggplot(data =df, aes(x =as.factor(N_Years),y = 100*Percentage,color = Area_Cat, group = Area_Cat ))  + my_theme_bw 
#     p1 = p1 + geom_bar(stat = 'identity',size = 0.2, width = 0.9)
#     p1 = p1 + facet_grid(Area_Cat~FireYear, labeller=label_parsed, drop = FALSE) + theme(legend.position="none")
#     p1 = p1 + xlab('Recovery Time [Years]') + ylab ('Percentage [%]')+ theme(strip.text.y = element_text(size = 8, angle = 90))
#     title = paste("Recovery Time Probability for different FireYears, by Area Classes (All LC Classes)\n\n",Ind, ' - ', str_opt, sep = '')
#     p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
#      p1 = p1 + coord_cartesian(ylim =c(0,100))
#     out_file = file.path(out_plot_folder,paste('Barplot_Perc_Area_YY_',Ind,'_', type_code,'.tif', sep = ''))
# #     tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
#     print(p1)
#     
   
#     dev.off()
  
  
    #- ----------------------------------------- - 
  # Number of analysed fires, by Area_Cat
  #- ----------------------------------------- - 
  
# #   df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Years == 0  & CLC_Class =='All~LC~Classes' & Index == 'NDVI^R' & Area_Cat !='All') )
# #   p1 = ggplot(df,aes(x = Area_Cat,y =N_Tot_YY,fill = Area_Cat))
# #   p1 = p1 + stat_summary(fun.y="sum", geom="bar")
# #   p1 = p1 + xlab('Year') + ylab ('Number of Fires')+my_theme_bw+scale_fill_grey('Burnt Area (ha)', end = 0.2, start = 0.8 )
# #   p1 = p1 + coord_cartesian(ylim = c(0,1700))
# #   title = paste("Number of analyzed Burnt Areas, by Total Woody burnt area\n\n",str_opt, sep = '')
# #   p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
# #   out_file = file.path(out_plot_folder,paste('Barplot_Number_Fires_AreaCat_',type_code,'.tif', sep = ''))
# # #   tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
# #    print(p1)
# # #   dev.off()
# #   
#   #- ----------------------------------------- - 
#   # Number of analysed fires, by Area_Cat, separated by Year
#   #- ----------------------------------------- - 
#   df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Years == 0  & CLC_Class =='All~LC~Classes' & Index == 'NDVI^R' & Area_Cat !='All') )
#   #   df$Area_Cat <- factor(df$Area_Cat, levels = rev(levels(df$Area_Cat)))
#   p1 = ggplot(df,aes(x = Area_Cat,y =N_Tot_YY,fill = Area_Cat))
#   p1 = p1+facet_grid(~FireYear, drop = FALSE)
#   p1 = p1 + geom_bar( stat = 'identity', show_guide=FALSE)
#   p1 = p1 + xlab('Year') + ylab ('Number of Fires')+my_theme_bw+scale_fill_grey('Burnt Area (ha)', end = 0.2, start = 0.8 )
#   p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust =1,size = 12))
#   p1 = p1+ coord_cartesian(ylim = c(0,350))
#   title = paste("Number of analyzed Burnt Areas, by Total Woody burnt area and FireYear\n\n",str_opt, sep = '')
#   p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
#   
#   out_file = file.path(out_plot_folder,paste('Barplot_Number_Fires_AreaCat_YY',type_code,'.tif', sep = ''))
# #   tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
#   print(p1)
#   
#    #- ----------------------------------------- - 
#   # Number of analysed fires, by Area_Cat, separated by CLC_Class
#   #- ----------------------------------------- - 
#   df = droplevels(subset(Data_YY, ENV_ZONE == 'All' & N_Years == 0   & Index == 'NDVI^R' & Area_Cat !='All') )
#   #   df$Area_Cat <- factor(df$Area_Cat, levels = rev(levels(df$Area_Cat)))
#   p1 = ggplot(df,aes(x = Area_Cat,y =N_Tot_YY,fill = Area_Cat))
#   p1 = p1+facet_wrap(~CLC_Class, drop = FALSE)
#   p1 = p1 + geom_bar( stat = 'identity', show_guide=FALSE)
#   p1 = p1 + xlab('Year') + ylab ('Number of Fires')+my_theme_bw+scale_fill_grey('Burnt Area (ha)', end = 0.2, start = 0.8 )
#   p1 = p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1,vjust =1,size = 12))
#    p1 = p1 + coord_cartesian(ylim = c(0,1700))
#   title = paste("Number of analyzed Burnt Areas, by Total Woody burnt area and FireYear\n\n",str_opt, sep = '')
#   p1 = p1 + ggtitle(title) + theme(plot.title = element_text(lineheight=.8, face="bold"))
#   
#   out_file = file.path(out_plot_folder,paste('Barplot_Number_Fires_AreaCat_YY',type_code,'.tif', sep = ''))
# #   tiff(out_file, width = 8.5, height = 6, units = 'in', pointsize = 9, compression = 'none', type = 'cairo', antialias = 'subpixel', res = 300 )
#   print(p1)
#  


