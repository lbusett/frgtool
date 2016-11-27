prova_survreg <- function() {

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


  my_theme_bw <- theme_bw()+theme(plot.title = element_text(face = "bold",size  = 12, vjust =2, hjust = 0.5),
                                  axis.text.x = element_text( size = 10), axis.text.y = element_text(size =10),
                                  axis.title.x = element_text( vjust = 0, size = 11, angle = 0 ),
                                  axis.title.y = element_text(hjust = 0.5, vjust = 0.3,size = 11, angle = 90))
  SNDVI_Folder = 'Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Erosion_50/Med_SNDVI/Stat_Analysis/'
  files_SNDVI = c('10/9.5%/Eroded/Percentages/Percentages_Area9.5%.RData')
  In_Files_SNDVI = file.path(SNDVI_Folder,files_SNDVI)

  # Define and Load file containing the Ancillary Meteo information (MeteoClim)
  Meteo_data_File = "E:/Projects_Data/Fire_Regeneration_Data/Climate_Data/Meteo_Data.RData"

  # Define prepost datasewts

  Pre_Fire_SNDVI = c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/NDVI/SNDVI_PrePost_Differences.RData')
  Pre_Fire_NDVI = c('Z:/WORKING/Fire_Regeneration/Data/Final_Analysis/Results_2000_2012/Prefire_Analysis/NDVI/NDVI_PrePost_Differences.RData')

  Results_File = In_Files_SNDVI ;Pre_Fire_File_Orig = Pre_Fire_NDVI ; Pre_Fire_File_Res = Pre_Fire_SNDVI

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

  data_in = join(data_in, Meteo_Data, type= 'left')
  data_in$Aridit = data_in$Aridit/10000

  data_in = join(data_in, VI_Diff, by=c('OBJECTID', 'CLC_Class', 'ENV_ZONE'),type= 'left')
  data_in = droplevels(subset(data_in,VIR_Dif_50 >= -80))       # Remove one outlier
  data = data_in[complete.cases(data_in),]
  data$numFireYear = as.numeric(as.character(data$FireYear))

  datasub = droplevels(subset(data, numFireYear<2008 & Comp_N ==3 ))                             # Remove fires after 2007
  datasub$N_Signif_ord[which(datasub$N_Signif_ord %in% c("6","7",'8','9','NR'))] = 'NR'     # Assign all recovery times above 5 to "NR"
  datasub = droplevels(datasub)                                                         # drop unused fires
  levels(datasub$N_Signif_ord)[length(levels(datasub$N_Signif_ord))] = "6"
  datasub$Recov[which(datasub$N_Signif_ord== 6)] = 'UnRecovered'
#   datasub = droplevels(subset(datasub, N_Signif_ord != 6))

# Do something  ---------------
  formulaint = as.formula('Y_int ~ rcs(VIR_Dif_50)+CLC_Class+anat+ans+Area_rec')
# Create "Surv" objects
  ddist <<- datadist(datasub)    ;  options(datadist='ddist')    #
  # Create left intervals
  low = as.numeric(as.character(datasub$N_Signif))

  # Create right intervals
  high = low +1 ;  high[which(datasub$Recov=='UnRecovered')]= NA
  low[which(low==0)]= NA
  intervals = paste(low, '-',high, sep = '')
  intervals = factor(intervals, levels = c('NA-1','1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','9-10','5-NA','6-NA','7-NA','8-NA','9-NA','10-NA'))
  datasub$low = low    ; datasub$high = high
  Y_int <- with(datasub, Surv(low, high, event = rep(3, nrow(data)), type = "interval2"))
  ms <- survreg(formulaint, data = datasub, dist = "weibull")

  qplot(intervals, predict(ms))+geom_boxplot()

  survreg.curves <- function(model, col = "black", values = c('Coniferous Forests', 'Broadleaved Forests'),seq.quantiles = seq(from = 0.00, to = 1.00, by = 0.01)) {

    l_ply(values, function(X) {
      browser()
        lines(x = predict(model,                    # survreg object to use
                  CLC_Class = X, # Dataset to perform prediction for
                  type = "quantile",                # Predict survival time (X-axis values) given event quantile
                  p = seq.quantiles),               # Vector of quantiles (Y-axis values)

              y = (1 - seq.quantiles),              # Change to survival quantile (proportion remaining)

              col = col, lty = X + 1)               # COLor and Line TYpe
    })
}

  survreg.curves(ms, "red")


  event = high   ; event[which(is.finite(high) == T)] = 1 ; event[is.finite(high) == F] = 0
  Y_int <- with(datasub, Surv( high,event))

  ms <- survreg(Y_int ~ rcs(VIR_Dif_50)+CLC_Class+anat+ans+Area_rec, data = datasub, dist = "weibull")

  percs <- (1:99)/100
  predms <- predict(ms, type="quantile", p=percs, se=TRUE)

  mp <- psm(Y_int ~ rcs(VIR_Dif_50)+CLC_Class+anat+ans+Area_rec, data = datasub, dist = "weibull", x = T, y =T)


  Y_cens <- with(data, Surv(N_Signif_num, rep(1, nrow(data))))

 low = as.numeric(as.character(datasub$N_Signif))





  ddist <<- datadist(data)    ;  options(datadist='ddist')    #
  # Create left intervals
  low = as.numeric(as.character(data$N_Signif))

  # Create right intervals
  high = low +1 ;  high[which(data$Recov=='UnRecovered')]= NA
  low[which(low==0)]= 0.001
  intervals = paste(low, '-',high, sep = '')
  intervals = factor(intervals, levels = c('0.001-1','1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','9-10','1-NA','2-NA','3-NA','4-NA','5-NA','6-NA','7-NA','8-NA','9-NA','10-NA'))
  data$low = low    ; data$high = high
  Y_int <- with(data, Surv(low, high, type = "interval2"))
  ms_exp <- survreg(formulaint, data = data, dist = "exponential")
  ms_w <- survreg(formulaint, data = data, dist = "weibull")
  ms_g <- survreg(formulaint, data = data, dist = "gaussian")
  ms_l <- survreg(formulaint, data = data, dist = "loglogistic")

  ms_null <- survreg(Y_int ~CLC_Class, data = data, dist = "loglogistic")

  qplot(intervals, predict(ms))+geom_boxplot()
  summary(survfit(Y_int ~1, data = data))
  summary(survfit(Y_int ~CLC_Class, data = data))


  formulacens = as.formula('Y_cens ~ rcs(VIR_Dif_50)+CLC_Class+anat+ans+Area_rec')
  low = as.numeric(as.character(data$N_Signif))
  high = low +1
  event = array(dim = length(low))   ; event[which(data$Recov=='UnRecovered')] = 0 ; event[which(data$Recov=='Recovered')] = 1
  Y_cens <- with(data, Surv(time = low+1,event = event))
  summary(survfit(Y_cens~event ))
  summary(survfit(Y_cens ~data$CLC_Class))
  ms_exp_c <- survreg(formulacens, data = data, dist = "exponential")
  ms_w_c <- survreg(formulacens, data = data, dist = "weibull")
  ms_g_c <- survreg(formulacens, data = data, dist = "gaussian")
  ms_l_c <- survreg(formulacens, data = data, dist = "loglogistic")

  kap = survfit(Y_int~1 )
  wei=survreg(Y_int~1,dist="w")
  kappa=wei$scale
  lambda=exp(-wei$coeff[1])^kappa

  zeit=seq(from=0,to=20,length.out=  1000)
  lambda.weibull= exp(-wei$coef[1]/wei$scale)
  rho.weibull= 1/wei$scale
  survival.weibull = exp(-lambda.weibull*zeit^( rho.weibull))
  weibullSurvival <- pweibull(zeit, shape= rho.weibull, scale=exp(wei$coef[1]),
                            lower.tail=FALSE)

  plot(kap,xlab= 't',ylab= expression(hat(S)(t)),lty=2,conf.int=F)
  lines(zeit,survival.weibull, col = 'red')

  kap = survfit(Y_int~1 )
  ll= survreg(Y_int~1,dist="loglogistic")
  lambda=exp(ll$coef[1])
  alpha=1/ll$scale
  zeit=seq(from=0,to=20,length.out=  1000)
  s_log=1/(1+(zeit/lambda)^alpha)
  plot(kap,xlab= 't',ylab= expression(hat(S)(t)),lty=2,conf.int=F)
  lines(zeit,s_log, col = 'green')

  ex= survreg(Y_int~1,dist="exponential")
  lambda= exp(-ex$coef[1]/ex$scale)
  rho= 1/ex$scale
  alpha=1/ll$scale
  zeit=seq(from=0,to=20,length.out=  1000)
  survival.ex = exp(-lambda*zeit^( rho))
  plot(kap,xlab= 't',ylab= expression(hat(S)(t)),lty=2,conf.int=F, xlim = c(0,20))
  lines(zeit,survival.ex, col = 'blue')


  plot(predict(ll), predict(wei))

  cox <- coxph(as.formula('Y_cens ~strata(FireYear)'), data = data)
   cc = survfit(cox )
  ph_fit_ps<-cox.zph(cox, transform ='identity')
  plot(ph_fit_ps)
  abline(h=0,lty=3)

  qplot(intervals, predict(ms_l_c))+geom_boxplot()

  ll= survreg(formulaint,data,dist="loglogistic")
  hist(predict(ll), xlim= c(0,20))

  w= survreg(formulaint,data,dist="weibull")
  hist(predict(w), xlim= c(0,20))

  exp= survreg(formulaint,data,dist="exponential")
  hist(predict(exp), xlim= c(0,20))
  plot(intervals,predict(exp))



  formulaint = as.formula('Y_int ~ rcs(VIR_Dif_50)+CLC_Class+anat+ans+Area_rec')
# Create "Surv" objects
  ddist <<- datadist(data)    ;  options(datadist='ddist')    #
  # Create left intervals


  # Create right intervals
  low = as.numeric(as.character(data$N_Signif))
  high = low +1 ;  high[which(data$Recov=='UnRecovered')]= NA
  low[which(low==0)]=0.001
  intervals = paste(low, '-',high, sep = '')
  intervals = factor(intervals, levels = c('NA-1','1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','9-10','1-NA','2-NA','3-NA','4-NA','5-NA','6-NA','7-NA','8-NA','9-NA','10-NA'))
  data$low = low    ; data$high = high
  Y_int <- with(data, Surv(low, high, type = "interval2"))
  # Plot K/M survival curve, with CI

  KMfit = (survfit(Y_int ~1, data = data))
  KMfit_CL = (survfit(Y_int ~ data$CLC_Class, data = data, type = 'fle'))

  datacon = droplevels(subset(data, CLC_Class != 'Mixed Forests'))


  # Create right intervals
  low = as.numeric(as.character(datacon$N_Signif))
  high = low +1 ;  high[which(datacon$Recov=='UnRecovered')]= NA
  low[which(low==0)]=0
  intervals = paste(low, '-',high, sep = '')
  intervals = factor(intervals, levels = c('0-1','1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','9-10','1-NA','2-NA','3-NA','4-NA','5-NA','6-NA','7-NA','8-NA','9-NA','10-NA'))
  datacon$low = low    ; datacon$high = high
  Y_int <- with(datacon, Surv(low, high, type = "interval2"))
  KMfit = (survfit(Y_int ~1, data = datacon))
  KMfit_CL = (survfit(Y_int ~ datacon$CLC_Class, data = data, type = 'fle'))



  datasub = droplevels(subset(data, numFireYear<2008 & Comp_N ==3 ))                             # Remove fires after 2007
#   datasub$N_Signif_ord[which(datasub$N_Signif_ord %in% c("6","7",'8','9','NR'))] = 'NR'     # Assign all recovery times above 5 to "NR"
  datasub = droplevels(datasub)                                                         # drop unused fires
  levels(datasub$N_Signif_ord)[length(levels(datasub$N_Signif_ord))] = "6"
  datasub$Recov[which(datasub$N_Signif_ord== 6)] = 'UnRecovered'
  # Create "Surv" objects
  ddist <<- datadist(datasub)    ;  options(datadist='ddist')    #
  # Create left intervals
  low = as.numeric(as.character(datasub$N_Signif))

  # Create right intervals
  high = low +1 ;  high[which(datasub$Recov=='UnRecovered')]= NA
  low[which(low==0)]=0
  intervals = paste(low, '-',high, sep = '')
  intervals = factor(intervals, levels = c('NA-1','1-2','2-3','3-4','4-5','5-6','6-7','7-8','8-9','9-10','1-NA','2-NA','3-NA','4-NA','5-NA','6-NA','7-NA','8-NA','9-NA','10-NA'))
  datasub$low = low    ; datasub$high = high
  Y_int <- with(datasub, Surv(low, high, type = "interval2"))
  # Plot K/M survival curve, with CI

  KMfit = (survfit(Y_int ~1, data = datasub))
  KMfit_CL = (survfit(Y_int ~datasub$CLC_Class, data = datasub))

}


