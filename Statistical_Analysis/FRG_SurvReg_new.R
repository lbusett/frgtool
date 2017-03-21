# Load RT Analysis Results ---------------
   library(ggplot2)
  library(plyr)
  library(gridExtra)
  library(reshape2)
  library(ordinal)
  library(MASS)
  library(car)
  library(survival)
  library(rms)
  library(caret)
  library(corrplot)
  library(rgdal)
  library(rms)
  
  
 

  SNDVI_Folder = 'H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Med_SNDVI/Stat_Analysis/Burned_Once/'
  files_SNDVI = c('Stat_Analysis_Med_SNDVI_2000_2012_META_RData.RData')
  In_Files_SNDVI = file.path(SNDVI_Folder,files_SNDVI) 
    
  # Define and Load file containing the Ancillary Meteo information (MeteoClim)
  Meteo_data_File = "E:/Projects_Data/Fire_Regeneration_Data/Climate_Data/Meteo_Data.RData"
  
  # Define prepost datasewts
#   
#   Pre_Fire_SNDVI = c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/NDVI/SNDVI_PrePost_Differences.RData')
#   Pre_Fire_NDVI = c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/NDVI/NDVI_PrePost_Differences.RData')
#   Pre_Fire_SNDVI_new = c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/NDVI/SNDVI_PrePost_Differences_new.RData')
#   Pre_Fire_NDVI_new = c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/NDVI/NDVI_PrePost_Differences_new.RData') 


  Pre_Fire_SNDVI = c('H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Prefire_Analysis/NDVI/SNDVI_PrePost_Differences.RData')
  Pre_Fire_NDVI = c('H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Prefire_Analysis/NDVI/NDVI_PrePost_Differences.RData')

  Results_File = In_Files_SNDVI ;Pre_Fire_File_Orig = Pre_Fire_NDVI ; Pre_Fire_File_Res = Pre_Fire_SNDVI
#   Pre_Fire_File_Orig_new = Pre_Fire_NDVI_new ; Pre_Fire_File_Res_new = Pre_Fire_SNDVI_new
   
  #- -------------------------------------------------------- -#
  # Load the data and do necessary pre-elaborations
  #- -------------------------------------------------------- -#

  
# Load Data
  
  load(Results_File)
  recov_stat = droplevels(subset(recov_stat, Comp_N == 3 & Area_CLC !=0 &     #  Get only results obtained with MedWdt = 3, separated CLC_Class
                                   CLC_Class != 'All' ))    # and Recover =1
  recov_stat$numFireYear = as.numeric(as.character(recov_stat$FireYear))
  data_in = recov_stat
  data_in$N_Signif_ord = ordered(data_in$N_Signif_Rec)                                  # Convert RT to ordinal (Ordered) response variable)
  data_in$N_Signif_num = as.numeric(data_in$N_Signif_ord)-1                               # Convert RT to ordinal (Ordered) response variable)
  data_in$Area_rec = log10(data_in$Area_CLC)                                            # Compute logarithm of the Area       

  load(Meteo_data_File)
  names(Meteo_Data)[4:28] = substr(names(Meteo_Data)[4:28], 1, nchar(names(Meteo_Data))[4:28]-1)
  data_in = join(data_in, Meteo_Data, type= 'left')
  data_in$Aridit = data_in$Aridit/10000

# Load shapefile and jon the date field to data 
  Shape_File = 'Z:/WORKING/Fire_Regeneration/Data/Burned_Areas/Burned_Areas_00_12.shp'
  BAreas_Name = strsplit(basename(Shape_File),'.shp')[[1]]
  BAreas_Dir = dirname(Shape_File)
  BAreas_shp = readOGR(BAreas_Dir, BAreas_Name)
  Data_Shape =BAreas_shp@data        						# Get attributes data from the shp
  Data_Shape = arrange(Data_Shape,desc(Area_HA))
    
    # Reassign FireSeason according to FireDate: If FireDate exists, check if the fire happened after DOY 235: in that case reassign FireSeason 
    # as FireSeason + 1 (needed because images used to compute SDVI are of DOYS 209 and 225: if the fire happened after this date, the "effect" will be visible only on the next
    # year !
    
  doys = as.POSIXlt(Data_Shape$FireDate)$yday
  Data_Shape$doy  = doys
  Data_Shape$YearSeason_rec = Data_Shape$YearSeason
  Data_Shape$YearSeason_rec[which(doys >= 225 )] =Data_Shape$YearSeason[which(doys >= 225)] + 1  
  Data_join = Data_Shape[,c("OBJECTID","FireDate","YearSeason", "YearSeason_rec","doy")] 


  load(Pre_Fire_File_Orig_new)
  out_diff_Or = out_diff
  names(out_diff_Or) = c('CASE_ID','OBJECTID', 'FireYear', 'YearFromFire','ENV_ZONE','CLC_Class','N_PIX',
                         'VI_Bef_100','VI_Bef_90','VI_Bef_75','VI_Bef_50','VI_Bef_25','VI_Bef_10','VI_Bef_0',
                         'VI_Aft_100','VI_Aft_90','VI_Aft_75','VI_Aft_50','VI_Aft_25','VI_Aft_10','VI_Aft_0',
                         'VI_Dif_100','VI_Dif_90','VI_Dif_75','VI_Dif_50','VI_Dif_25','VI_Dif_10','VI_Dif_0')
  load(Pre_Fire_File_Res_new)
  names(out_diff) = c('CASE_ID','OBJECTID', 'FireYear', 'YearFromFire','ENV_ZONE','CLC_Class','N_PIX',
                      'VIR_Bef_100','VIR_Bef_90','VIR_Bef_75','VIR_Bef_50','VIR_Bef_25','VIR_Bef_10','VIR_Bef_0',
                      'VIR_Aft_100','VIR_Aft_90','VIR_Aft_75','VIR_Aft_50','VIR_Aft_25','VIR_Aft_10','VIR_Aft_0',
                      'VIR_Dif_100','VIR_Dif_90','VIR_Dif_75','VIR_Dif_50','VIR_Dif_25','VIR_Dif_10','VIR_Dif_0')
  VI_Diff=join(out_diff, out_diff_Or, by = c('OBJECTID','CLC_Class','ENV_ZONE'))    # DF Containing Pre-Post differences for both original and rescaled indexes
  VI_Diff = VI_Diff[complete.cases(VI_Diff),]
  VI_Diff_tmp = join(VI_Diff, Data_join, by = "OBJECTID")

  load(Pre_Fire_File_Orig)
  out_diff_Or = out_diff
  names(out_diff_Or) = c('CASE_ID','OBJECTID', 'FireYear', 'YearFromFire','ENV_ZONE','CLC_Class','N_PIX',
                         'VI_Bef_100','VI_Bef_90','VI_Bef_75','VI_Bef_50','VI_Bef_25','VI_Bef_10','VI_Bef_0',
                         'VI_Aft_100','VI_Aft_90','VI_Aft_75','VI_Aft_50','VI_Aft_25','VI_Aft_10','VI_Aft_0',
                         'VI_Dif_100','VI_Dif_90','VI_Dif_75','VI_Dif_50','VI_Dif_25','VI_Dif_10','VI_Dif_0')
  load(Pre_Fire_File_Res)
  names(out_diff) = c('CASE_ID','OBJECTID', 'FireYear', 'YearFromFire','ENV_ZONE','CLC_Class','N_PIX',
                      'VIR_Bef_100','VIR_Bef_90','VIR_Bef_75','VIR_Bef_50','VIR_Bef_25','VIR_Bef_10','VIR_Bef_0',
                      'VIR_Aft_100','VIR_Aft_90','VIR_Aft_75','VIR_Aft_50','VIR_Aft_25','VIR_Aft_10','VIR_Aft_0',
                      'VIR_Dif_100','VIR_Dif_90','VIR_Dif_75','VIR_Dif_50','VIR_Dif_25','VIR_Dif_10','VIR_Dif_0')
  
  VI_Diff=join(out_diff, out_diff_Or, by = c('OBJECTID','CLC_Class','ENV_ZONE'))    # DF Containing Pre-Post differences for both original and rescaled indexes
  VI_Diff = VI_Diff[complete.cases(VI_Diff),]
  VI_Diff_tmp2 = join(VI_Diff, Data_join, by = "OBJECTID")

  VI_Diff_tmp[which(VI_Diff_tmp$doy >245),]=VI_Diff_tmp2[which(VI_Diff_tmp2$doy >245),]
  VI_Diff= VI_Diff_tmp
  

  data_in = data_in[complete.cases(data_in),]
  data_in = join(data_in, VI_Diff, by=c('OBJECTID', 'CLC_Class', 'ENV_ZONE'),type= 'left')
  data_in = droplevels(subset(data_in,VIR_Dif_50 >= -80))       # Remove one outlier
  data_in$numFireYear = as.numeric(as.character(data_in$FireYear))
  datatmp = data_in

 
# Load shapefile and jon the date field to data 
#   Shape_File = 'Z:/WORKING/Fire_Regeneration/Data/Burned_Areas/Burned_Areas_00_12.shp'
#   BAreas_Name = strsplit(basename(Shape_File),'.shp')[[1]]
#   BAreas_Dir = dirname(Shape_File)
#   BAreas_shp = readOGR(BAreas_Dir, BAreas_Name)
#   Data_Shape =BAreas_shp@data    								# Get attributes data from the shp
#   Data_Shape = arrange(Data_Shape,desc(Area_HA))
    
    # Reassign FireSeason according to FireDate: If FireDate exists, check if the fire happened after DOY 235: in that case reassign FireSeason 
    # as FireSeason + 1 (needed because images used to compute SDVI are of DOYS 209 and 225: if the fire happened after this date, the "effect" will be visible only on the next
    # year !
    
#   doys = as.POSIXlt(Data_Shape$FireDate)$yday
#   Data_Shape$doy  = doys
#   Data_Shape$YearSeason_rec = Data_Shape$YearSeason
#   Data_Shape$YearSeason_rec[which(doys >= 225 )] =Data_Shape$YearSeason[which(doys >= 225)] + 1  
#   Data_join = Data_Shape[,c("OBJECTID","FireDate","YearSeason", "YearSeason_rec","doy")]
#   datatmp = join(data, Data_join, by = "OBJECTID")
#   datatmp = datatmp[,c("OBJECTID","FireDate","YearSeason", "YearSeason_rec","FireYear","doy","N_Signif_Rec","N_Signif","Signif_FireYear")]
  
  datatmp$doy[which(is.finite(datatmp$doy)==FALSE)]=-999
 
#   datatmp$start_year = as.numeric(as.character(datatmp$YearSeason))
  datatmp$start_year = as.numeric(as.character(datatmp$FireYear))
  
  datatmp$end_year = datatmp$start_year    ; datatmp$end_year[] = NA
  
  datatmp$end_year[which(datatmp$Signif_FireYear== 0)] = datatmp$start_year[which(datatmp$Signif_FireYear== 0)] + 
                as.numeric(as.character(datatmp$N_Signif[which(datatmp$Signif_FireYear== 0 )]))+1

  datatmp$end_year[which(datatmp$Signif_FireYear== 1)] = datatmp$start_year[which(datatmp$Signif_FireYear== 1)] + 
                as.numeric(as.character(datatmp$N_Signif[which(datatmp$Signif_FireYear== 1 )]))

#   datatmp$end_year[which(datatmp$Signif_FireYear== 0 & datatmp$doy <=245) ] = 
#         datatmp$start_year[which(datatmp$Signif_FireYear== 0 & datatmp$doy <=245)]+
#         as.numeric(as.character(datatmp$N_Signif[which(datatmp$Signif_FireYear== 0 & datatmp$doy <=245)]))+1
#   
#   datatmp$end_year[which(datatmp$Signif_FireYear == 1 & datatmp$doy <=245)] = 
#               datatmp$start_year[which(datatmp$Signif_FireYear== 1 & datatmp$doy <=245)]+
#               as.numeric(as.character(datatmp$N_Signif[which(datatmp$Signif_FireYear== 1 & datatmp$doy <=245)]))
#     
#   
#   datatmp$end_year[which(datatmp$doy >245)] = 
#               datatmp$start_year[which(datatmp$doy >245)]+
#               as.numeric(as.character(datatmp$N_Signif[which(datatmp$doy >245)]))+1
#     
  datatmp$RT = datatmp$end_year-datatmp$start_year

  datatmp$high = datatmp$RT
  datatmp$event = array(dim = length(datatmp$high))   ; datatmp$event[which(datatmp$Recov=='UnRecovered')] = 0 ; datatmp$event[which(datatmp$Recov=='Recovered')] = 1
  datatmp$high[which(datatmp$Recov=='UnRecovered')] = datatmp$high[which(datatmp$Recov=='UnRecovered')]-1
  Y_r <- with(datatmp, Surv(high, event))

#   file = file.choose()
#   save(datatmp, file = file)

  


  plot_folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Plots/Survival_Analysis'   # out folder for plots
  dir.create(plot_folder, recursive = T)

# Repeat with new computed RT - Interval censoring -------------

# Perform KM analysis using right censored SURV object-----------------------------------------------------
  
  datatmp$high = datatmp$RT
  datatmp$event = array(dim = length(datatmp$high))   ; datatmp$event[which(datatmp$Recov=='UnRecovered')] = 0 ; datatmp$event[which(datatmp$Recov=='Recovered')] = 1
  datatmp$high[which(datatmp$Recov=='UnRecovered')] = datatmp$high[which(datatmp$Recov=='UnRecovered')]-1
  Y_r <- with(datatmp, Surv(high, event))
  VI_levs = c(60,45,30,15,0)
  KMfit_c = (survfit(Y_r ~1, data = datatmp))
  KMfit_CL_c = (survfit(Y_r ~ datatmp$CLC_Class, data = datatmp))
#   KMfit_VI_c = (survfit(Y_r ~ cut2(data$VIR_Dif_50,g = 5), data = datatmp)) 
#   KMfit_VI_Area = (survfit(Y_r ~ cut2(data$Area_rec,g = 5), data = datatmp)) 

  my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 12, vjust =2, hjust = 0.5),
                                  axis.text.x = element_text( size = 9), axis.text.y = element_text(size =9), 
                                  axis.title.x = element_text( vjust = 0, size = 10, angle = 0 ), 
                                  axis.title.y = element_text(hjust = 0.5, vjust = 0.3,size = 10, angle = 90))
# Plot KM estimates -------
  
  KMplot = createSurvivalFrame (KMfit_c)  # Create data frame from KM estimator

  KMplot$surv = 1-KMplot$surv             # cOMPUTE cumulated probabilities and CI from Survival function
  KMplot$upper = 1-KMplot$upper
  KMplot$lower = 1-KMplot$lower

  KMlabs = KMplot[2:11,]                  #Create the labs containing number of points
  KMlabs$time[1]=0
  KMlabs$ntot_labs = c('N[RT>=t]', KMlabs$n.risk[2:10])
  KMlabs$nevent_labs = c('N[RT==t]', KMlabs$n.event[2:10])
  KMlabs$cens_labs = c('N[cens]', KMlabs$n.censor[2:10])


  KMplot = KMplot[ which((KMplot$n.event == 0 & KMplot$time ==1)==FALSE ),]   # Remove useless data at time 0 
  p1 = ggplot(KMplot, aes(x = time, y = surv, ymin = lower, ymax = upper))+my_theme_bw
  p1 = p1 +geom_errorbar(width = 0.2, linetype = 1, alpha = 0.5)
  p1 = p1+ geom_point(size =2.5)
  p1 = p1+geom_step(lty = 3, alpha = 0.8, color = 'grey50')
  p1 = p1 + geom_text(aes(x = time, y = 1, label = ntot_labs),parse =T, data = KMlabs, size = 2.5)
  p1 = p1 + geom_text(aes(x = time, y = 0.95, label = nevent_labs),parse =T, data = KMlabs, size = 2.5)
#   p1 = p1 + geom_text(aes(x = time, y = 0.92, label = cens_labs),parse =T, data = KMlabs, size = 4)
  p1 = p1 + scale_x_continuous(breaks = seq(0,10,1))+ylim (0,1)
  p1 = p1 +xlab('Years After Fire')+ylab(expression(paste('Cumulated Recovery Probability   - ',P(RT<=t),sep = ' ')))
  out_file = file.path(plot_folder, 'KM_Curves_Overall_step.tiff')
  p1
  ggsave(out_file, dpi = 600, width = 3.33, height = 3.3)
  
  
  KMplot_cl = createSurvivalFrame (KMfit_CL_c)  # Create data frame from KM estimator separated by CLC class

  KMplot_cl = KMplot_cl[ which((KMplot_cl$n.event == 0 & KMplot_cl$time ==1)==FALSE ),]     # Remove useless data at time 0 
  KMplot_cl$surv = 1-KMplot_cl$surv       # cOMPUTE cumulated probabilities and CI from Survival function
  KMplot_cl$upper = 1-KMplot_cl$upper
  KMplot_cl$lower = 1-KMplot_cl$lower
  KMplot_cl$strata = factor(KMplot_cl$strata, levels = rev(levels(KMplot_cl$strata)), ordered = T)     # reOrder the CLC levels
  
  p2 = ggplot(KMplot_cl, aes(x = time, y = surv, ymin = lower, ymax = upper, color = strata, shape = strata))+my_theme_bw
  p2= p2+geom_step(lty = 3, alpha = 0.8, color = 'grey50') library(ggplot2)

  p2 = p2 +geom_errorbar(width = 0.2, linetype = 1, alpha = 0.5)
  p2 = p2 +theme(legend.position = 'none')
  p2 = p2+ geom_point(size =2.5)+ scale_x_continuous(breaks = seq(0,10,1))+ylim (0,1)
  p2 = p2 +xlab('Years After Fire')+ylab(expression(paste('Cumulated Recovery Probability   - ',P(RT<=t),sep = ' ')))
  p2 = p2 + theme(legend.position = c(0.68,0.20), legend.background = element_rect(fill = 'white',colour = 'black'))
  p2 = p2 +scale_colour_manual('CLC Class', labels = (levels(datatmp$CLC_Class)),values=c('black','red','darkorange','chartreuse4','blue'))+scale_shape_manual('CLC Class', labels = (levels(datatmp$CLC_Class)),values = seq(21,25,1))
  p2 = p2 +  theme(legend.text = element_text( size = 6), legend.title = element_text( size = 7), legend.key.size = unit(0.12,units = 'in'),
                   legend.position = c(0.735,0.17), legend.key = element_rect(fill = NULL, color = 'white'),
                   legend.background = element_rect(fill = 'white', color = 'grey50',size = 0.2))
  p2
  out_file = file.path(plot_folder, 'KM_Curves_Classe_step.pdf')
  pdf(out_file, width = 3.33, height = 3.3,useDingbats=F)
p2
dev.off()

  ggsave(out_file, dpi = 600, width = 3.33, height = 3.3,...useDingbats=F)
           
# Perform AFT analysis --------------

#   formulaint_c = as.formula('Y_r ~ rcs(VIR_Dif_50)+VIR_Bef_50+CLC_Class+wmat+ans+Area_rec+CLC_Class%ia%VIR_Dif_50+ FireYear')
  
#NON LINEAR analysis - using max VIDiff
  datatmp = data_old
  Y_r <- with(datatmp, Surv(high, event))
  datatmp$VIR_Dif_50 = -datatmp$VIR_Dif_50   # Change sign to VIRDiff
  datatmp$MaxVIDiff = -datatmp$MaxVIDiff   # Change sign to VIRDiff
  datatmp$ans = 0.01*datatmp$ans
  ddist <<- datadist(datatmp)    ;  options(datadist='ddist')  
  
  formulaint_c = as.formula('Y_r ~ rcs(VIR_Dif_50)+ans+CLC_Class+Area_rec+anat+ans%ia%VIR_Dif_50')
  ms_l_c <- psm(formulaint_c, data = datatmp, dist = "loglogistic", x = T, y = T, tol = 1e-20)
  anova_ms = anova(ms_l_c, tol = 1e-20, SS = T, Main = 'T')
  qplot(datatmp$VIR_Dif_50, exp(predict(ms_l_c)))
  qplot(as.factor(datatmp$RT),exp(predict(ms_l_c)), geom='boxplot')+coord_flip()+ylim(0,10)
  xtable(anova_ms)

  plot_curves(ms_l_c)

  null = formula('Y_r ~  rcs(VIR_Dif_50,3)+CLC_Class+Area_rec+CLC_Class%ia%VIR_Dif_50')
  fm_NULL_ord = survreg(null, data = datatmp)
  a = add1(fm_NULL_ord,~  rcs(VIR_Dif_50,3)+CLC_Class+Area_rec+ans+CLC_Class%ia%VIR_Dif_50+Aridit+sumt+aumt+anat+wqat+cqat+tmsp+dmsp+tqsp+dqsp+wisp+susp+spsp+wmat)

  fm0_ord = formula('Y_r ~  rcs(VIR_Dif_50,3)')
  fm_NULL_ord = survreg(fm0_ord, data = datatmp)
  fm_FULL = formula('Y_r ~ rcs(VIR_Dif_50,3)+CLC_Class+Area_rec+ans+CLC_Class%ia%VIR_Dif_50+Aridit+sumt+aumt+anat+wqat+cqat+tmsp+dmsp+tqsp+dqsp+wisp+susp+spsp+wmat')
  Step_Reg_Ord = stepAIC(fm_NULL_ord, scope = list(upper =fm_FULL, lower =fm_NULL_ord), direction = 'both')
 
  VI_Levs = c(10,20,30,40,50)
  par(mfrow = c(3,2))
  for (lev in VI_Levs) {
    p = survplot( ms_l_c,CLC_Reg, VIR_Dif_50 = lev, conf = "bands", conf.int = T,xlab = "Recovery Time", ylab = "Cumulated Probability of Recovery (1-Survival)",
           label.curves = list(keys = "lines", col = seq(1:5)),  # legend instead of direct label
           levels.only  = F, abbrev.label = F, fun = function(x) {1 - x},             # Cumulative probability plot         
           col = seq(1:5), xlim = c(0,9), col.fill=alpha(gray(seq(.95, .75, length=5)),0.5), ylim = c(0,1))                   # show number at risk
           title(paste('VIR Diff = ', lev, sep = ''))
           ## srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
           ## y.n.risk = 0, cex.n.risk = 0.6
         
  }
  plot(anova(ms_l_c))

# Create the data for the survival plots ----
plot_curves = function(ms_l_c) {
VI_Levs = c(10,20,30,40,50)
df = data.frame(VIR_Dif_50 = rep(VI_Levs,5),VIR_Bef_50 = rep(0,25),CLC_Class = rep(levels(datatmp$CLC_Class),each= 5), 
                anat =  rep(13.53,25), ans =  rep(8.15,25), Area_rec =  rep(2.55,25))

surv_fun_pts = function(df,ms_l_c) {
  est = survest(ms_l_c, df, times = seq(0,10,1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

surv_fun_lines = function(df,ms_l_c) {
  est = survest(ms_l_c, df, times = seq(0,10,0.1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

point_data =ddply(df, .(VIR_Dif_50,CLC_Class), function(df) surv_fun_pts(df, ms_l_c))
lines_data =ddply(df, .(VIR_Dif_50,CLC_Class), function(df) surv_fun_lines(df, ms_l_c))


p = ggplot(point_data)+my_theme_bw
p = p + geom_point(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Class, shape = CLC_Class),size = 2)
p = p +geom_errorbar(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Class, shape = CLC_Class),width = 0.2, lty = 1, alpha =0.5)
p = p +geom_line(data=lines_data,aes(x = time, y = Prob, color = CLC_Class),lty = 2, alpha = 0.5)
p = p + facet_wrap(~VIR_Dif_50, ncol = 2, scales = 'free')+scale_colour_manual(values=c('black','red','darkorange','chartreuse4','blue') )
p = p + scale_x_continuous(breaks = seq(0,9,1), limits =c(0,9.1))+ylim (0,1)+scale_shape_manual(values = seq(21,25,1))
p = p + theme(legend.position= c(0.65,.2))
p = p +xlab('Years After Fire')+ylab(expression(paste('Cumulated Recovery Probability   - ',P(RT<=t),sep = ' ')))
p
}

plot_curves_grouped = function(ms_l_c) {
VI_Levs = c(10,20,30,40,50)
df = data.frame(VIR_Dif_50 = rep(VI_Levs,2),VIR_Bef_50 = rep(0,10),CLC_Reg = rep(levels(datatmp$CLC_Reg),each= 5), 
                anat =  rep(13.53,10), ans =  rep(8.15,10), Area_rec =  rep(2.55,10))

surv_fun_pts = function(df,ms_l_c) {
  est = survest(ms_l_c, df, times = seq(0,10,1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

surv_fun_lines = function(df,ms_l_c) {
  est = survest(ms_l_c, df, times = seq(0,10,0.1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

point_data =ddply(df, .(VIR_Dif_50,CLC_Reg), function(df) surv_fun_pts(df, ms_l_c))
lines_data =ddply(df, .(VIR_Dif_50,CLC_Reg), function(df) surv_fun_lines(df, ms_l_c))


p = ggplot(point_data)+my_theme_bw
p = p + geom_point(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Reg, shape = CLC_Reg),size = 2)
p = p +geom_errorbar(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Reg, shape = CLC_Reg),width = 0.2, lty = 1, alpha =0.5)
p = p +geom_line(data=lines_data,aes(x = time, y = Prob, color = CLC_Reg),lty = 2, alpha = 0.5)
p = p + facet_wrap(~VIR_Dif_50, ncol = 2, scales = 'free')+scale_colour_manual(values=c('black','red','darkorange','chartreuse4','blue') )
p = p + scale_x_continuous(breaks = seq(0,9,1), limits =c(0,9.1))+ylim (0,1)+scale_shape_manual(values = seq(21,25,1))
p = p + theme(legend.position= c(0.65,.2))
p = p +xlab('Years After Fire')+ylab(expression(paste('Cumulated Recovery Probability   - ',P(RT<=t),sep = ' ')))
p
}

#NON LINEAR analysis - using VIDiff of next year
  datatmp = data_new
  Y_r <- with(datatmp, Surv(high, event))
  datatmp$VIR_Dif_50 = -datatmp$VIR_Dif_50   # Change sign to VIRDiff
  datatmp$MaxVIDiff = -datatmp$MaxVIDiff   # Change sign to VIRDiff
  datatmp$ans = 0.01*datatmp$ans
  ddist <<- datadist(datatmp)    ;  options(datadist='ddist')  

  # Regroup the CLC_Class variable----
  
  CLC_Reg = as.character(datatmp$CLC_Class)
  CLC_Reg[which(CLC_Reg == "Broadleaved Forests" )] = 'Broad +Schlero'
  CLC_Reg[which(CLC_Reg == "Schlerophyllus Vegetation" )] = 'Broad +Schlero'
  CLC_Reg[which(CLC_Reg == "Mixed Forests" )] = 'Conif + Mix + Trans'
  CLC_Reg[which(CLC_Reg == "Transitional Vegetation" )] = 'Conif + Mix + Trans'
  CLC_Reg[which(CLC_Reg == "Coniferous Forests" )] = 'Conif + Mix + Trans'
  a = as.factor(CLC_Reg)
  datatmp$CLC_Reg = a 
# -----

  ddist <<- datadist(datatmp)    ;  options(datadist='ddist')  
  formulaint_c = as.formula('Y_r ~ rcs(VIR_Dif_50, 3)  + Area_rec + CLC_Class+wimt+sumt+spmt+aumt+ausp+spsp+wisp+susp')

  formulaint_c = as.formula('Y_r ~ rcs(VIR_Dif_50, 3) + Area_rec + CLC_Class + cqat+ans+anat+dqsp+tqsp')

  ms_l_c <- psm(formulaint_c, data = datatmp, dist = "loglogistic", x = T, y = T, tol = 1e-40)
  anova_ms = anova(ms_l_c, tol = 1e-20, SS = T, Main = 'T')
  qplot(datatmp$VIR_Dif_50, exp(predict(ms_l_c)))
  qplot(as.factor(datatmp$RT),exp(predict(ms_l_c)), geom='boxplot')+coord_flip()+ylim(0,10)
  xtable(anova_ms)

  plot_curves(ms_l_c)

  formulaint_c_group = as.formula('Y_r ~ rcs(VIR_Dif_50,3)+ausp+CLC_Reg+Area_rec')

  formulaint_c_group = as.formula('Y_r ~ rcs(VIR_Dif_50,3)+CLC_Reg+Area_rec+ans+Aridit+sumt+aumt+anat+wqat+cqat+tmsp+dmsp+tqsp+dqsp+wisp+susp+spsp+wmat')

  formulaint_c_group = as.formula('Y_r ~ pol(VIR_Dif_50,3)+Area_rec+ausp')
  ms_l_c <- psm(formulaint_c_group, data = datatmp, dist = "loglogistic", x = T, y = T, tol = 1e-20)
  anova_ms = anova(ms_l_c, tol = 1e-20, SS = T, Main = 'T')
  qplot(datatmp$VIR_Dif_50, exp(predict(ms_l_c)))
  qplot(as.factor(datatmp$RT),exp(predict(ms_l_c)), geom='boxplot')+coord_flip()+ylim(0,10)
  xtable(anova_ms)

  plot_curves_grouped(ms_l_c)
  plot_curves_prec(ms_l_c)

  null = formula('Y_r ~  rcs(VIR_Dif_50,3)+CLC_Class+Area_rec')
  fm_NULL_ord = survreg(null, data = datatmp)
  a = add1(fm_NULL_ord,~  rcs(VIR_Dif_50,3)+CLC_Class+Area_rec+ans+Aridit+sumt+aumt+anat+wqat+cqat+tmsp+dmsp+tqsp+dqsp+wisp+susp+spsp+wmat+ausp)

  a = add1(fm_NULL_ord,~  rcs(VIR_Dif_50,3)+CLC_Class+Area_rec+ans+Aridit+sumt+aumt+anat+wqat+cqat+tmsp+dmsp+tqsp+dqsp+wisp+susp+spsp+wmat)

  fm0_ord = formula('Y_r ~  rcs(VIR_Dif_50,3)')
  fm_NULL_ord = survreg(fm0_ord, data = datatmp)
  fm_FULL = formula('Y_r ~ rcs(VIR_Dif_50, 3)  + Area_rec + CLC_Class+wimt+sumt+spmt+ ausp+ spsp +wisp+ susp+aumt')
  Step_Reg_Ord = stepAIC(fm_NULL_ord, scope = list(upper =fm_FULL, lower =fm_NULL_ord), direction = 'forward')

  Step_Reg_Ord2 = stepAIC(Step_Reg_Ord, scope = list(upper =Step_Reg_Ord, lower =fm_NULL_ord), direction = 'backward')
 
  null = formula('Y_r ~  rcs(VIR_Dif_50,3)+Area_rec')
  fm_NULL_ord = survreg(null, data = datatmp)
  a = add1(fm_NULL_ord,~  rcs(VIR_Dif_50,3)+ausp+CLC_Reg+Area_rec+dqsp*ans+ans+Aridit+sumt+ausp+aumt+anat+wqat+cqat+tmsp+dmsp+tqsp+dqsp+wisp+susp+spsp+wmat)


  fm0_ord = formula('Y_r ~  rcs(VIR_Dif_50,3)+Area_rec')
  fm_NULL_ord = survreg(fm0_ord, data = datatmp)
  fm_FULL = formula('Y_r ~ rcs(VIR_Dif_50,3)+CLC_Reg+Area_rec+ans+Aridit+ausp+sumt+aumt+anat+wqat+cqat+tmsp+dmsp+tqsp+dqsp+wisp+susp+spsp+wmat')
  Step_Reg_Ord = stepAIC(fm_NULL_ord, scope = list(upper =fm_FULL, lower =fm_NULL_ord), direction = 'forward')

  VI_Levs = c(10,20,30,40,50)
  prec_levs = quantile(datatmp$ans, probs = c(0.15,0.30,0.45,0.60,0.75))
  datatmp$preccut = cut(datatmp$ans, breaks = prec_levs)
  par(mfrow = c(3,2))
  for (lev in VI_Levs) {
    p = survplot( ms_l_c,preccut, VIR_Dif_50 = lev, conf = "bands", conf.int = T,xlab = "Recovery Time", ylab = "Cumulated Probability of Recovery (1-Survival)",
           label.curves = list(keys = "lines", col = seq(1:5)),  # legend instead of direct label
           levels.only  = F, abbrev.label = F, fun = function(x) {1 - x},             # Cumulative probability plot         
           col = seq(1:5), xlim = c(0,9), col.fill=alpha(gray(seq(.95, .75, length=5)),0.5), ylim = c(0,1))                   # show number at risk
           title(paste('VIR Diff = ', lev, sep = ''))
           ## srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
           ## y.n.risk = 0, cex.n.risk = 0.6
         
  }
  plot(anova(ms_l_c))





# Prova censurando tutto a 6 --------------------------

  datatmp$high = datatmp$RT
  event = array(dim = length(high))   ; event[which(datatmp$Recov=='UnRecovered')] = 0 ; event[which(datatmp$Recov=='Recovered')] = 1
  datatmp$high[which(datatmp$Recov=='UnRecovered')] =  datatmp$high[which(datatmp$Recov=='UnRecovered')]-1  
  event[which(high >6)] = 0  ;  datatmp$high[which(datatmp$high >6)] = 6
  

  Y_r_c <- with(datatmp, Surv(high, event))
  
  VI_levs = c(-100,-60,-45,-30,-15,10)
  KMfit_r_c = (survfit(Y_r_c ~1, data = datatmp))
  KMfit_CL_r_c = (survfit(Y_r_c ~ data$CLC_Class, data = datatmp))
  KMfit_VI_r_c = (survfit(Y_r_c ~ cut2(data$VIR_Dif_50,g = 5), data = datatmp)) 
  KMfit_VI_r_Area = (survfit(Y_r_c ~ cut2(data$Area_rec,g = 5), data = datatmp)) 

# Perform AFT analysis --------------

  formulaint_r_c = as.formula('Y_r_c ~ rcs(VIR_Dif_50)+VIR_Bef_50+CLC_Class+wmat+ans+Area_rec+CLC_Class%ia%VIR_Dif_50')

  formulaint_r_c = as.formula('Y_r ~ rcs(VIR_Dif_50)+VIR_Bef_50+CLC_Class+wmat+ans+Area_rec')
  ms_l_r_c <- psm(formulaint_r_c, data = datatmp, dist = "loglogistic", x = T, y = T)

  VI_Levs = c(0,-15,-30,-45,-60)
  par(mfrow = c(3,2))
  for (lev in VI_Levs) {
    survplot( ms_l_r_c,CLC_Class, VIR_Dif_50 = lev, conf = "bands", conf.int = T,xlab = "Recovery Time", ylab = "Cumulated Probability of Recovery (1-Survival)",
           label.curves = list(keys = "lines", col = seq(1:5)),  # legend instead of direct label
           levels.only  = F, abbrev.label = F, fun = function(x) {1 - x},             # Cumulative probability plot         
           col = seq(1:5), xlim = c(0,10), col.fill=alpha(gray(seq(.95, .75, length=5)),0.5), ylim = c(0,1))                   # show number at risk
           title(paste('VIR Diff = ', lev, sep = ''))
           ## srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
           ## y.n.risk = 0, cex.n.risk = 0.6
         
  }
  plot(anova(ms_l_r_c))

survplot( ms_l_r_c,VIR_Dif_50 = VI_Levs, conf = "bands", conf.int = T,xlab = "Recovery Time", ylab = "Cumulated Probability of Recovery (1-Survival)",
         label.curves = list(keys = "lines",col = seq(1:5)),  # legend instead of direct label
         levels.only  = F, abbrev.label = F, fun = function(x) {1 - x},             # Cumulative probability plot         
         col = seq(1:5), xlim = c(0,9), col.fill=alpha(gray(seq(.95, .75, length=5)),0.5)                    # show number at risk
         ## srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
         ## y.n.risk = 0, cex.n.risk = 0.6
         )
 title('Estimated effect of VIR_Diff on Recovery Probability')

var = "wmat"
#  Survplot of modeled recovery % as a funhction of CLC_Class
survplot( ms_l_n,ans,conf = "bars", conf.int = T,xlab = "Recovery Time", ylab = "Cumulated Probability of Recovery (1-Survival)",
         label.curves = list(keys = "lines", col = seq(1:5)),  # legend instead of direct label
         levels.only  = F, abbrev.label = F, fun = function(x) {1 - x},             # Cumulative probability plot         
         col = seq(1:5), xlim = c(0,9)                    # show number at risk
         ## srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
         ## y.n.risk = 0, cex.n.risk = 0.6
         )

#  Survplot of modeled recovery % as a funhction of Area
survplot( ms_l_n,Area_rec, conf = "bars", conf.int = T,xlab = "Recovery Time", ylab = "Cumulated Probability of Recovery (1-Survival)",
         label.curves = list(keys = "lines", col = seq(1:5)),  # legend instead of direct label
         levels.only  = F, abbrev.label = F, fun = function(x) {1 - x},             # Cumulative probability plot         
         col = seq(1:5), xlim = c(0,9)                    # show number at risk
         ## srt.n.risk = 0, sep.n.risk = 0.056, adj.n.risk = 1,
         ## y.n.risk = 0, cex.n.risk = 0.6
#          )

#-------
          
plot_curves = function(ms_l_c) {
VI_Levs = c(10,20,30,40,50)
df = data.frame(VIR_Dif_50 = rep(VI_Levs,5),VIR_Bef_50 = rep(0,25),CLC_Class = rep(levels(datatmp$CLC_Class),each= 5), 
                anat =  rep(13.53,25), ans =  rep(8.15,25), Area_rec =  rep(2.55,25))

surv_fun_pts = function(df,ms_l_c) {
  est = survest(ms_l_c, df, times = seq(0,10,1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

surv_fun_lines = function(df,ms_l_c) {
  est = survest(ms_l_c, df, times = seq(0,10,0.1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

point_data =ddply(df, .(VIR_Dif_50,CLC_Class), function(df) surv_fun_pts(df, ms_l_c))
lines_data =ddply(df, .(VIR_Dif_50,CLC_Class), function(df) surv_fun_lines(df, ms_l_c))


p = ggplot(point_data)+my_theme_bw
p = p + geom_point(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Class, shape = CLC_Class),size = 2)
p = p +geom_errorbar(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Class, shape = CLC_Class),width = 0.2, lty = 1, alpha =0.5)
p = p +geom_line(data=lines_data,aes(x = time, y = Prob, color = CLC_Class),lty = 2, alpha = 0.5)
p = p + facet_wrap(~VIR_Dif_50, ncol = 2, scales = 'free')+scale_colour_manual(values=c('black','red','darkorange','chartreuse4','blue') )
p = p + scale_x_continuous(breaks = seq(0,9,1), limits =c(0,9.1))+ylim (0,1)+scale_shape_manual(values = seq(21,25,1))
p = p + theme(legend.position= c(0.65,.2))
p = p +xlab('Years After Fire')+ylab(expression(paste('Cumulated Recovery Probability   - ',P(RT<=t),sep = ' ')))
p
}

plot_curves_grouped = function(ms_l_c) {
VI_Levs = c(10,20,30,40,50)
df = data.frame(VIR_Dif_50 = rep(VI_Levs,2),VIR_Bef_50 = rep(0,10),CLC_Reg = rep(levels(datatmp$CLC_Reg),each= 5), 
                anat =  rep(13.53,10), ans =  rep(8.15,10), Area_rec =  rep(2.55,10))

surv_fun_pts = function(df,ms_l_c) {
  est = survest(ms_l_c, df, times = seq(0,10,1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

surv_fun_lines = function(df,ms_l_c) {
  est = survest(ms_l_c, df, times = seq(0,10,0.1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

point_data =ddply(df, .(VIR_Dif_50,CLC_Reg), function(df) surv_fun_pts(df, ms_l_c))
lines_data =ddply(df, .(VIR_Dif_50,CLC_Reg), function(df) surv_fun_lines(df, ms_l_c))


p = ggplot(point_data)+my_theme_bw
p = p + geom_point(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Reg, shape = CLC_Reg),size = 2)
p = p +geom_errorbar(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Reg, shape = CLC_Reg),width = 0.2, lty = 1, alpha =0.5)
p = p +geom_line(data=lines_data,aes(x = time, y = Prob, color = CLC_Reg),lty = 2, alpha = 0.5)
p = p + facet_wrap(~VIR_Dif_50, ncol = 2, scales = 'free')+scale_colour_manual(values=c('black','red','darkorange','chartreuse4','blue') )
p = p + scale_x_continuous(breaks = seq(0,9,1), limits =c(0,9.1))+ylim (0,1)+scale_shape_manual(values = seq(21,25,1))
p = p + theme(legend.position= c(0.65,.2))
p = p +xlab('Years After Fire')+ylab(expression(paste('Cumulated Recovery Probability   - ',P(RT<=t),sep = ' ')))
p
}
          
plot_curves_prec = function(ms_l_c) {
VI_Levs = c(10,20,30,40,50)
prec_levs = prec_levs
df = data.frame(VIR_Dif_50 = rep(VI_Levs,5),VIR_Bef_50 = rep(0,25),CLC_Reg = 'Broad +Schlero', 
                anat =  rep(13.53,25), ans =  rep(prec_levs,each = 5), Area_rec =  rep(2.55,25))

surv_fun_pts = function(df,ms_l_c) {
  est = survest(ms_l_c, df, times = seq(0,10,1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper,prec_fact = df$ans)
  return(out)
}

surv_fun_lines = function(df,ms_l_c) {
  est = survest(ms_l_c, df, times = seq(0,10,0.1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper,prec_fact = df$ans)
  return(out)
}


point_data =ddply(df, .(VIR_Dif_50,ans), function(df) surv_fun_pts(df, ms_l_c))
lines_data =ddply(df, .(VIR_Dif_50,ans), function(df) surv_fun_lines(df, ms_l_c))
point_data$prec_fact = as.factor(point_data$prec_fact)
lines_data$prec_fact = as.factor(lines_data$prec_fact)

p = ggplot(point_data)+my_theme_bw
p = p + geom_point(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = prec_fact, shape = prec_fact),size = 2)
p = p +geom_errorbar(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = prec_fact, shape = prec_fact),width = 0.2, lty = 1, alpha =0.5)
p = p +geom_line(data=lines_data,aes(x = time, y = Prob, color = prec_fact),lty = 2, alpha = 0.5)
p = p + facet_wrap(~VIR_Dif_50, ncol = 2, scales = 'free')+scale_colour_manual(values=c('black','red','darkorange','chartreuse4','blue') )
p = p + scale_x_continuous(breaks = seq(0,9,1), limits =c(0,9.1))+ylim (0,1)+scale_shape_manual(values = seq(21,25,1))
p = p + theme(legend.position= c(0.65,.2))
p = p +xlab('Years After Fire')+ylab(expression(paste('Cumulated Recovery Probability   - ',P(RT<=t),sep = ' ')))
p
}