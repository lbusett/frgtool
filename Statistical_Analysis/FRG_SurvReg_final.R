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
  
  
   my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 12, vjust =2, hjust = 0.5),
                                  axis.text.x = element_text( size = 9), axis.text.y = element_text(size =9), 
                                  axis.title.x = element_text( vjust = 0, size = 10, angle = 0 ), 
                                  axis.title.y = element_text(hjust = 0.5, vjust = 0.3,size = 10, angle = 90))

  # Define the main FRG output file to be used for the analysis

  SNDVI_Folder = 'H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Med_SNDVI/Stat_Analysis/Burned_Once/'
  files_SNDVI = c('Stat_Analysis_Med_SNDVI_2000_2012_META_RData.RData')   # Final output file for areas burned once
  In_Files_SNDVI = file.path(SNDVI_Folder,files_SNDVI) 
    
  # Define and Load file containing the Ancillary Meteo information (MeteoClim)
  Meteo_data_File = "E:/Projects_Data/Fire_Regeneration_Data/Climate_Data/Meteo_Data.RData"   # Fiule contatining the WorldClim data extracted on burnt areas
  
  # Define prepost datasets (Derived from "comp_pre_fire.R and FRG_PrePost_Differences_new.R)

  Pre_Fire_SNDVI = c('H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Prefire_Analysis/NDVI/SNDVI_PrePost_Differences.RData')
  Pre_Fire_NDVI = c('H:/FIRE_REGENERATION_TOOL_ROBERTO/Data/Results_2000_2012/Prefire_Analysis/NDVI/NDVI_PrePost_Differences.RData')

  Results_File = In_Files_SNDVI ;Pre_Fire_File_Orig = Pre_Fire_NDVI ; Pre_Fire_File_Res = Pre_Fire_SNDVI
   
  #- -------------------------------------------------------- -#
  # Load the data and do necessary pre-elaborations
  #- -------------------------------------------------------- -#

# Load output FRG Data
  
  load(Results_File)
  recov_stat = droplevels(subset(recov_stat, Comp_N == 3 & Area_CLC !=0 &     #  Get only results obtained with MedWdt = 3, separated CLC_Classes
                                   CLC_Class != 'All' ))    # and Recover =1
  recov_stat$numFireYear = as.numeric(as.character(recov_stat$FireYear))
  data_in = recov_stat
  data_in$N_Signif_ord = ordered(data_in$N_Signif_Rec)                                # Convert the number of significant years with differences to ordinal (Ordered) response variable)
#   data_in$N_Signif_num = as.numeric(data_in$N_Signif_ord)-1                           # Convert the number of significant years with differences to numeric response variable)
  data_in$Area_rec = log10(data_in$Area_CLC)                                          # Compute logarithm of the Area       

# Load meteo data and join it with Burnt areas data
  load(Meteo_data_File)
  names(Meteo_Data)[4:28] = substr(names(Meteo_Data)[4:28], 1, nchar(names(Meteo_Data))[4:28]-1)
  data_in = join(data_in, Meteo_Data, type= 'left')
  data_in$Aridit = data_in$Aridit/10000


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

  data_in = data_in[complete.cases(data_in),]
  data_in = join(data_in, VI_Diff, by=c('OBJECTID', 'CLC_Class', 'ENV_ZONE'),type= 'left')
  data_in = droplevels(subset(data_in,VIR_Dif_50 >= -80))       # Remove one outlier
  data_in$numFireYear = as.numeric(as.character(data_in$FireYear))
  datatmp = data_in
  
  
  datatmp$start_year = as.numeric(as.character(datatmp$FireYear))
  datatmp$end_year = datatmp$start_year    ; datatmp$end_year[] = NA
  datatmp$end_year[which(datatmp$Signif_FireYear== 0)] = datatmp$start_year[which(datatmp$Signif_FireYear== 0)] + 
                as.numeric(as.character(datatmp$N_Signif[which(datatmp$Signif_FireYear== 0 )]))+1

  datatmp$end_year[which(datatmp$Signif_FireYear== 1)] = datatmp$start_year[which(datatmp$Signif_FireYear== 1)] + 
                as.numeric(as.character(datatmp$N_Signif[which(datatmp$Signif_FireYear== 1 )]))

  datatmp$RT = datatmp$end_year-datatmp$start_year

  datatmp$high = datatmp$RT
  datatmp$event = array(dim = length(datatmp$high))   ; datatmp$event[which(datatmp$Recov=='UnRecovered')] = 0 ; datatmp$event[which(datatmp$Recov=='Recovered')] = 1
  datatmp$high[which(datatmp$Recov=='UnRecovered')] = datatmp$high[which(datatmp$Recov=='UnRecovered')]-1
  Y_r <- with(datatmp, Surv(high, event))


  plot_folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Plots/Survival_Analysis'   # out folder for plots
  dir.create(plot_folder, recursive = T)
# Model without climate and severity----
  data = datatmp
  ddist <<- datadist(data)    ;  options(datadist='ddist')  
  form_null = 'Y_r ~1+CLC_Class+Area_rec'
  psm_mod = psm(formula(form_null), dist = 'loglogistic', data = data)
  psm_mod
  anova(psm_mod)
  predplot = plot(Predict(psm_mod),ylim = c(-0.5,2.5))
  curves = plot_curves_prec(psm_mod)
  recovered = which(data$Recov == 'Recovered')
  qplot(as.factor(data$RT[recovered]),exp(predict(psm_mod)[recovered]), geom = 'boxplot')+my_theme_bw+ylim(0,10)+scale_y_continuous(breaks = seq(1,10,1))+geom_jitter(alpha = 0.2, position = position_jitter(height = 0, width = 0.2))

# Model WITHOUT Severity----
  data = datatmp
  ddist <<- datadist(data)    ;  options(datadist='ddist')  
  form_null = 'Y_r ~1+CLC_Class+Area_rec'
  fm_NULL_ord = survreg(formula(form_null), data = datatmp, dist = 'loglogistic')
#   fm_FULL = formula('~ CLC_Class+Area_rec+ans+Aridit+spmt+wimt+sumt+aumt+anat+wqat+cqat+tqsp+dqsp+ausp+wisp+susp+spsp')
  fm_FULL = formula('~ CLC_Class+Area_rec+ans+Aridit+anat+ausp+wisp+susp+spsp+dqsp')
  oldAIC = extractAIC(fm_NULL_ord)
  #Step_Reg_Ord = stepAIC(fm_NULL_ord, scope = list(upper =fm_FULL, lower =fm_NULL_ord), direction = 'both')
  for (add in seq(1:5)) {
    add1 = add1(fm_NULL_ord,fm_FULL, test = 'Chi')
    addvar = rownames(add1)[which.min(add1$AIC)] ; print(addvar)
    add1_formula = paste(form_null,'+',addvar, sep = '')
    form_null = add1_formula
    fm_NULL_ord = survreg(formula(form_null), data = datatmp, dist = 'loglogistic')
    newAIC = extractAIC(fm_NULL_ord)
    print(paste('DeltaAIC = ', newAIC[2]-oldAIC[2]))
    oldAIC = newAIC
  }

  Step_Reg_form = stepAIC(fm_NULL_ord, scope = list(upper =fm_FULL, lower =fm_NULL_ord), direction = 'both')
  psm_mod = psm(Y_r ~ 1+ CLC_Class + Area_rec + ausp +susp+anat, dist = 'loglogistic', data = data)
  psm_mod = psm(Y_r ~ 1+ CLC_Class + Area_rec + Aridit*CLC_Class+anat, x = T, y = T, dist = 'loglogistic', data = data)

  psm_mod = psm(Y_r ~ 1+ CLC_Class + Area_rec + ans*CLC_Class+anat, x = T, y = T, dist = 'loglogistic', data = data)

  psm_mod = psm(Y_r ~ 1+ CLC_Class + Area_rec + Aridit+anat, x = T, y = T, dist = 'loglogistic', data = data)
  psm_mod
  anova(psm_mod)
  df = data.frame(ausp = rep(prec_levs,5),CLC_Class = rep(levels(data$CLC_Class),each= 5) ,
                anat =  rep(13.53,25), susp =  rep(92.8,25), Area_rec =  rep(2.55,25))
  est = survest(ms_l_c, df, times = seq(0,10,1), what = 'survival')
  predplot = plot(Predict(psm_mod),ylim = c(-0.5,2.5))
  curves = plot_curves_prec(psm_mod)
  qplot(as.factor(data$RT[recovered]),exp(predict(psm_mod)[recovered]), geom = 'boxplot')+my_theme_bw+ylim(0,10)+scale_y_continuous(breaks = seq(1,10,1))+geom_jitter(alpha = 0.2, position = position_jitter(height = 0, width = 0.2))
  
  
  
# Model WITH Severity - NOT ADJUSTED -----
  data = datatmp
 
  data$VIR_Dif_50 = -data$VIR_Dif_50
  data$yy = as.factor(data$FireYear)
  ddist <<- datadist(data)    ;  options(datadist='ddist')  
#   form_null = 'Y_r ~1+rcs(VIR_Dif_50,3)+CLC_Class+Area_rec+Aridity'
#   fm_NULL_ord = survreg(formula(form_null), data = datatmp, dist = 'loglogistic')
# #   fm_FULL = formula('~ CLC_Class+Area_rec+ans+Aridit+spmt+wimt+sumt+aumt+anat+wqat+cqat+tqsp+dqsp+ausp+wisp+susp+spsp')
#   fm_FULL = formula('~rcs(VIR_Dif_50,3)+CLC_Class+Area_rec+ans+Aridit+anat+ausp+wisp+susp+spsp')
#   Step_Reg_form = stepAIC(fm_NULL_ord, scope = list(upper =fm_FULL, lower =fm_NULL_ord), direction = 'both')
#   psm_mod = psm(Step_Reg_form, x = T, y = T, dist = 'loglogistic', data = data)
#   
# #   psm_mod = psm(Y_r ~ 1+rcs(VIR_Dif_50,3)+ CLC_Class + Area_rec + ausp +susp+anat, dist = 'loglogistic', data = data)
  
  psm_mod = psm(Y_r ~ rcs(VIR_Dif_50,3)+ CLC_Class + Area_rec + Aridit+yy, x = T, y = T, dist = 'loglogistic', data = data)
#   psm_mod = psm(Y_r ~ 1+rcs(VIR_Dif_50,3)+ CLC_Class + Area_rec + Aridit*CLC_Class, x = T, y = T, dist = 'loglogistic', data = data)
  psm_mod
  anova(psm_mod)
  predplot_sev_notadj = plot(Predict(psm_mod),ylim = c(-0.5,2.5))
  recovered = which(data$Recov == 'Recovered')
  qplot(as.factor(data$RT[recovered]),exp(predict(psm_mod)[recovered]), geom = 'boxplot')+theme_bw()+ylim(0,10)+scale_y_continuous(breaks = seq(1,10,1))+geom_jitter(alpha = 0.2, position = position_jitter(height = 0, width = 0.2))
  qplot(as.factor(data$RT[recovered]),round(exp(predict(psm_mod))-0.5)[recovered], geom = 'boxplot')+theme_bw()+ylim(0,10)+scale_y_continuous(breaks = seq(1,10,1))+geom_jitter(alpha = 0.2, position = position_jitter(height = 0, width = 0.2))


# Plot prediction for VIDif ----
  a = Predict(psm_mod, VIR_Dif_50)
  a$yhat = exp(a$yhat)    ;   a$lower = exp(a$lower)    ; a$upper = exp(a$upper)
#   plot(a,xlim = c(0,50), ylim = c(0,10), data = data)
  p = ggplot(a,aes(x = VIR_Dif_50, y = yhat))
  p = p +geom_ribbon(aes(ymin = a$lower, ymax = a$upper), fill = 'grey75', colour ='grey75')+geom_line(color = 'red')
  p = p+my_theme_bw+xlim (0,55)
  p = p +xlab(expression(paste(Delta,NDVI[50]^R)))+ylim (0,10)+ylab('RT')

# Plot prediction for Area ----
  a1 = Predict(psm_mod, Area_rec)
  a1$yhat = exp(a1$yhat)    ;   a1$lower = exp(a1$lower)    ; a1$upper = exp(a1$upper)
  p1 = ggplot(a1,aes(x = Area_rec, y = yhat))
  p1 = p1 +geom_ribbon(aes(ymin = a1$lower, ymax = a1$upper), fill = 'grey75', colour ='grey75')+geom_line(color = 'red')
  p1 = p1+my_theme_bw
  p1 = p1 +xlab(expression(paste(log[10],(Area))))+ylab('RT')+ylim(0,10)
  p1

# Plot prediction for Aridity ----
  a2 = Predict(psm_mod, Aridit)
  a2$yhat = exp(a2$yhat)    ;   a2$lower = exp(a2$lower)    ; a2$upper = exp(a2$upper)
  p2 = ggplot(a2,aes(x = Aridit, y = yhat))
  p2 = p2 +geom_ribbon(aes(ymin = a2$lower, ymax = a2$upper), fill = 'grey75', colour ='grey75')+geom_line(color = 'red')
  p2 = p2+my_theme_bw
  p2 = p2 +xlab(expression('Aridity Index'))+ylab('RT')+ylim(0,10)
  p2

# Plot prediction for CLC_Class ----
  a3 = Predict(psm_mod, CLC_Class)
  a3$yhat = exp(a3$yhat)    ;   a3$lower = exp(a3$lower)    ; a3$upper = exp(a3$upper)
  p3 = ggplot(a3,aes(x = CLC_Class, y = yhat))
  p3 = p3 +geom_point(size = 1)+geom_errorbar(aes(ymin = a3$lower, ymax = a3$upper, width = 0.2), fill = 'grey75', colour ='black')
  p3 = p3+my_theme_bw
  p3 = p3 +scale_x_discrete(expression('CLC Class'), labels = c('Br.For', 'Con.For','Mix.For','Schl','Trans.W/S'))+ylab('RT')+ylim(0,10)
  p3

# Plot prediction for FireYear ----
  a4 = Predict(psm_mod, yy)
  a4$yhat = exp(a4$yhat)    ;   a4$lower = exp(a4$lower)    ; a4$upper = exp(a4$upper)
  p4 = ggplot(a4,aes(x = yy, y = yhat))
  p4 = p4 +geom_errorbar(aes(ymin = a4$lower, ymax = a4$upper, width = 0.3), fill = 'grey75', colour ='black')+geom_point(size =1)
  p4 = p4+my_theme_bw
  p4 = p4 +scale_x_discrete(expression('Fire Year'), labels = c('03','04','05','06','07','08','09','10','11'))+ylab('RT')+ylim(0,10)
  p4

  curves = plot_curves_VI(psm_mod)

  CLC_Reg = as.character(datatmp$CLC_Class)
  CLC_Reg[which(CLC_Reg == "Broadleaved Forests" )] = 'Broad +Schlero'
  CLC_Reg[which(CLC_Reg == "Schlerophyllus Vegetation" )] = 'Broad +Schlero'
  CLC_Reg[which(CLC_Reg == "Mixed Forests" )] = 'Conif + Mix + Trans'
  CLC_Reg[which(CLC_Reg == "Transitional Vegetation" )] = 'Conif + Mix + Trans'
  CLC_Reg[which(CLC_Reg == "Coniferous Forests" )] = 'Conif + Mix + Trans'
  data$CLC_Reg = as.factor(CLC_Reg)

  psm_mod = psm(Y_r ~ 1+rcs(VIR_Dif_50,3)+ CLC_Class + Area_rec + Aridit+yy, x = T, y = T, dist = 'loglogistic', data = data)
  anova(psm_mod)
  curves = plot_curves_VI_grouped(psm_mod)
  a = Predict(psm_mod, VIR_Dif_50, yy)
  a$yhat = exp(a$yhat)    ;   a$lower = exp(a$lower)    ; a$upper = exp(a$upper)
  plot(Predict(psm_mod, yy), ylim = c(0.6,1.5))
  plot(Predict(psm_mod, VIR_Dif_50, yy), data = dd, nlevels = 10)
  plot(a, data = data, nlevels = 10, xlim = c(0,50), ylim = c(0,10))
 
  curves = plot_curves_VI(psm_mod)
  plot_folder = 'E:/busetlo/Documents/Articles/DRAFTS/Fire_Regeneration/Figures'
  out_file = file.path(plot_folder, 'SurvCurves_VI_CLC.pdf')
  
  Cairo_pdf( width = 8, height = 11,file = out_file)
  print(curves)
  dev.off()

  curves = plot_curves_prec(psm_mod)
  plot_folder = 'E:/busetlo/Documents/Articles/DRAFTS/Fire_Regeneration/Figures'
  out_file = file.path(plot_folder, 'SurvCurves_VI_Aridity.eps')
  CairoPDF(file = out_file, width = 8, height = 11, units = 'in', dpi = 100,useDingbats=F)
   curves
  dev.off()

  
dev.off()
# Model WITH Severity -  ADJUSTED -----
  
  # Compute the adjusted VIRDiff (Mean centering on CLC_Class)
  
  comp_adj = function(df) {
    mean= mean(df$VIR_Dif_50, na.rm = T)
    VI_adj = df$VIR_Dif_50-mean
    
    mean= mean(df$ausp, na.rm = T)
    ausp_adj = df$ausp-mean
    
    mean= mean(df$susp, na.rm = T)
    susp_adj = df$susp-mean
    
    mean= mean(df$anat, na.rm = T)
    anat_adj = df$anat-mean
    
    mean= mean(df$Aridit, na.rm = T)
    Aridit_adj = df$Aridit-mean
    
    mean= mean(df$Area_rec, na.rm = T)
    Area_adj = df$Area_rec-mean
    
    out = df
    out$VIR_Dif_50_adj = VI_adj
    out$aus_adj = ausp_adj
    out$susp_adj = susp_adj
    out$anat_adj = anat_adj
    out$Aridit_adj = Aridit_adj
    out$Area_adj = Area_adj
#       
#       data.frame(CLC_Class = df$CLC_Class, VI_or = df$VIR_Dif_50, VI_adj = VI_adj, Aridit = df$Aridit, Area_rec = df$Area_rec, RT = df$Y_r[,1], event = df$Y_r[,2])
    out
#     browser()
  }
  dataadj =ddply(data, .(CLC_Class), function(df) comp_adj(df))
  Y_r_adj = Surv(dataadj$RT,dataadj$event ) 
  ddist <<- datadist(dataadj)    ;  options(datadist='ddist')  
  
  psm_mod = psm(Y_r_adj ~ 1+rcs(VIR_Dif_50,3)+ CLC_Class + Area_rec + ausp +susp+anat, dist = 'loglogistic', data = dataadj)
  psm_mod = psm(Y_r_adj ~ 1+rcs(VIR_Dif_50,3)+ CLC_Class + Area_rec + ans+anat, x = T, y = T, dist = 'loglogistic', data = dataadj)
  
#   psm_mod = psm(Y_r_adj ~ 1+rcs(VIR_Dif_50_adj,3)+ CLC_Class + Area_adj + Aridit_adj, x = T, y = T, dist = 'loglogistic', data = dataadj)
  
  psm_mod
  anova(psm_mod)
  predplot_sev_adj = plot(Predict(psm_mod),ylim = c(-0.5,2.5))
  recovered = which(dataadj$Recov == 'Recovered')
  qplot(as.factor(dataadj$RT[recovered]),exp(predict(psm_mod)[recovered]), geom = 'boxplot')+my_theme_bw+ylim(0,10)+scale_y_continuous(breaks = seq(1,10,1))+geom_jitter(alpha = 0.2, position = position_jitter(height = 0, width = 0.2))
  curves = plot_curves_VI(psm_mod)
  predplot_sev_adj = plot(Predict(psm_mod),ylim = c(-0.5,2.5))
  
 
  
  datatmp$VI_adj = VI_adj_t$VI_adj
  datatmp$Aridit_adj = Aridit_adj_t$Aridit_adj
  datatmp$Area_adj = Area_adj_t$Area_adj
  datatmp$Y_r = Y_r
#----- 
# Tests on Years -----  

# Model including year and no severity

  dd = droplevels(subset(data, YearSeason != '2011'))
  ddist <<- datadist(dd)    ;  options(datadist='ddist')  
  psm_mod = psm(Y_r ~ 1+yy+Aridit+Area_rec +anat+CLC_Class+Aridit*CLC_Class, x = T, y = T, dist = 'loglogistic', data = dd)
  a = Predict(psm_mod,  yy)
  a$yhat = exp(a$yhat)    ;   a$lower = exp(a$lower)    ; a$upper = exp(a$upper)
  plot(Predict(psm_mod, yy), data = dd, nlevels = 11)
  qplot(data=dd, yy,VIR_Dif_50, geom = 'boxplot')+coord_flip()
  plot(Predict(psm_mod, yy), ylim = c(0.6,1.5))

# No interaction
  dd = droplevels(subset(data, YearSeason != '2011'))
  ddist <<- datadist(dd)    ;  options(datadist='ddist')  
  psm_mod = psm(Y_r ~ 1+rcs(VIR_Dif_50,3)+yy+Aridit+Area_rec +anat+CLC_Class, x = T, y = T, dist = 'loglogistic', data = dd)
  a = Predict(psm_mod, VIR_Dif_50, yy)
  a$yhat = exp(a$yhat)    ;   a$lower = exp(a$lower)    ; a$upper = exp(a$upper)
 
  plot(Predict(psm_mod, VIR_Dif_50, yy), data = dd, nlevels = 11)
  plot(a, data = dd, nlevels = 11, xlim = c(0,50), ylim = c(0,10))
  plot(Predict(psm_mod, yy), ylim = c(0.6,1.5))

# Model including year and interaction (Remove 2001)

  dd = droplevels(subset(data, YearSeason != '2011'))
  ddist <<- datadist(dd)    ;  options(datadist='ddist')  
  psm_mod = psm(Y_r ~ 1+rcs(VIR_Dif_50,3)+yy+yy%ia%(rcs(VIR_Dif_50,3))+Aridit+Area_rec +anat+CLC_Class, x = T, y = T, dist = 'loglogistic', data = dd)
  a = Predict(psm_mod, VIR_Dif_50, yy)
  a$yhat = exp(a$yhat)    ;   a$lower = exp(a$lower)    ; a$upper = exp(a$upper)
  plot(Predict(psm_mod, yy), ylim = c(0.6,1.5))
  plot(Predict(psm_mod, VIR_Dif_50, yy), data = dd, nlevels = 11)
  plot(a, data = dd, nlevels = 11, xlim = c(0,50), ylim = c(0,10))
 
  

  

#-----
plot_curves_VI = function(psm_mod) {
VI_Levs = c(10,20,30,40,50)
VI_Levs =quantile(data$VIR_Dif_50, probs = c(10,25,50,75,90)/100)
df = data.frame(VIR_Dif_50 = rep(VI_Levs,5),CLC_Class = rep(levels(datatmp$CLC_Class),each= 5), 
                anat =  rep(13.53,25), ans =  rep(813,25), ausp = rep(303.8,25),Area_rec =  rep(2.55,25), Aridit = 0.8382, yy = 2007)

surv_fun_pts = function(df,psm_mod) {
  est = survest(psm_mod, df, times = seq(0,10,1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

surv_fun_lines = function(df,psm_mod) {
  est = survest(psm_mod, df, times = seq(0,10,0.1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

point_data =ddply(df, .(VIR_Dif_50,CLC_Class), function(df) surv_fun_pts(df, psm_mod))
lines_data =ddply(df, .(VIR_Dif_50,CLC_Class), function(df) surv_fun_lines(df, psm_mod))


p = ggplot(point_data)+my_theme_bw
p = p + geom_point(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Class, shape = CLC_Class),size = 2)
p = p +geom_errorbar(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Class, shape = CLC_Class),width = 0.2, lty = 1,alpha =0.5)
p = p +geom_line(data=lines_data,aes(x = time, y = Prob, color = CLC_Class),,alpha =0.5,lty = 2)
p = p + facet_wrap(~VIR_Dif_50, ncol = 2, scales = 'free')+scale_colour_manual(values=c('black','red','darkorange','chartreuse4','blue') )
p = p + scale_x_continuous(breaks = seq(0,9,1), limits =c(0,9.1))+ylim (0,1)+scale_shape_manual(values = seq(21,25,1))
p = p + theme(legend.position= c(0.65,.2))
p = p +xlab('Years After Fire')+ylab(expression(paste('Cumulated Recovery Probability   - ',P(RT<=t),sep = ' ')))
p
}

plot_curves_VI_grouped = function(psm_mod) {
VI_Levs = c(10,20,30,40,50)
VI_Levs =quantile(data$VIR_Dif_50, probs = c(10,25,50,75,90)/100)
df = data.frame(VIR_Dif_50 = rep(VI_Levs,2),CLC_Reg = rep(levels(data$CLC_Reg),each= 5), 
                anat =  rep(13.53,10), ans =  rep(813,10), ausp = rep(303.8,10),Area_rec =  rep(2.55,10), Aridit = 0.8382, yy = '2007')

surv_fun_pts = function(df,psm_mod) {
  est = survest(psm_mod, df, times = seq(0,10,1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

surv_fun_lines = function(df,psm_mod) {
  est = survest(psm_mod, df, times = seq(0,10,0.1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  return(out)
}

point_data =ddply(df, .(VIR_Dif_50,CLC_Reg), function(df) surv_fun_pts(df, psm_mod))
lines_data =ddply(df, .(VIR_Dif_50,CLC_Reg), function(df) surv_fun_lines(df, psm_mod))


p = ggplot(point_data)+my_theme_bw
p = p + geom_point(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Reg, shape = CLC_Reg),size = 2)
p = p +geom_errorbar(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = CLC_Reg, shape = CLC_Reg),width = 0.2, lty = 1)
p = p +geom_line(data=lines_data,aes(x = time, y = Prob, color = CLC_Reg),lty = 3)
p = p + facet_wrap(~VIR_Dif_50, ncol = 2, scales = 'free')+scale_colour_manual(values=c('black','red','darkorange','chartreuse4','blue') )
p = p + scale_x_continuous(breaks = seq(0,9,1), limits =c(0,9.1))+ylim (0,1)+scale_shape_manual(values = seq(21,25,1))
p = p + theme(legend.position= c(0.65,.2))
p = p +xlab('Years After Fire')+ylab(expression(paste('Cumulated Recovery Probability   - ',P(RT<=t),sep = ' ')))
p
}
#-----
plot_curves_prec = function(psm_mod) {
VI_Levs =quantile(data$VIR_Dif_50, probs = c(10,25,50,75,90)/100)
prec_levs = quantile(data$Aridit, probs = c(0.10,0.25,0.50,0.75,0.90))
df = data.frame(VIR_Dif_50 = rep(VI_Levs,5), Aridit = rep(prec_levs,each =5),CLC_Class = rep('Schlerophyllus Vegetation',25) ,
                anat =  rep(13.53,25), susp =  rep(92.8,25), Area_rec =  rep(2.55,25), yy = rep('2007',25))

surv_fun_pts = function(df,psm_mod) {
   
  est = survest(psm_mod, df, times = seq(0,10,1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper,prec_fact = df$Aridit)
  return(out)
}

surv_fun_lines = function(df,psm_mod) {
  est = survest(psm_mod, df, times = seq(0,10,0.1))
  est$prob = 1-est$surv
  #out = data.frame(VIR_Dif50 = df$VIR_Dif_50, CLC_Class = df$CLC_Class, time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper)
  out = data.frame( time = est$time, Prob = 1-est$surv, upper = 1-est$lower, lower = 1-est$upper,prec_fact = df$Aridit)
  return(out)
}


point_data =ddply(df, .(VIR_Dif_50,Aridit), function(df) surv_fun_pts(df, psm_mod))
lines_data =ddply(df, .(VIR_Dif_50,Aridit), function(df) surv_fun_lines(df, psm_mod))
point_data$prec_fact = as.factor(point_data$prec_fact)
lines_data$prec_fact = as.factor(lines_data$prec_fact)

p = ggplot(point_data)+my_theme_bw
p = p + geom_point(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = prec_fact, shape = prec_fact),size = 2)
p = p +geom_errorbar(aes(x = time, y = Prob, ymin = lower, ymax = upper, color = prec_fact, shape = prec_fact),width = 0.2, lty = 1,alpha =0.5)
p = p +geom_line(data=lines_data,aes(x = time, y = Prob, color = prec_fact),lty = 3,alpha =0.5)
p = p + facet_wrap(~VIR_Dif_50, ncol = 2, scales = 'free')+scale_colour_manual('Aridity Index',values=c('black','red','darkorange','chartreuse4','blue') )
p = p + scale_x_continuous(breaks = seq(0,9,1), limits =c(0,9.1))+ylim (0,1)+scale_shape_manual('Aridity Index',values = seq(21,25,1))
p = p + theme(legend.position= c(0.65,.2))
p = p +xlab('Years After Fire')+ylab(expression(paste('Cumulated Recovery Probability   - ',P(RT<=t),sep = ' ')))
p
}


psm_mod1 = psm(Y_r ~ 1+rcs(VIR_Dif_50,3)+ CLC_Class + Area_rec + Aridit+yy, x = T, y = T, dist = 'loglogistic', data = data)
psm_mod2 = psm(Y_r ~ 1+rcs(VIR_Dif_50,3)+ CLC_Class + Area_rec + Aridit, x = T, y = T, dist = 'loglogistic', data = data)

LR = -2*logLik(psm_mod1)+2*logLik(psm_mod2)
LR



psm_mod1 = psm(Y_r ~ 1+rcs(VIR_Dif_50,3)+ CLC_Class + Area_rec + Aridit+yy+ans, x = T, y = T, dist = 'loglogistic', data = data)
psm_mod2 = psm(Y_r ~ 1+rcs(VIR_Dif_50,3)+ CLC_Class + Area_rec + Aridit+yy, x = T, y = T, dist = 'loglogistic', data = data)
lrtest(psm_mod1,psm_mod2)
