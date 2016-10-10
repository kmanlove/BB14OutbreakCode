#--------------------------------------------------#
#-- Code for Black Butte 2014 Epidemic paper ------#
#--------------------------------------------------#

#bb <- read.csv("~/work/Kezia/Research/FieldStudies/HCBighornContactStudies/Data/RelocationData/BlackButteLocationsFinal_20July2014.csv", header = T, sep = "\t")
bb <- read.csv("~/work/Kezia/Research/FieldStudies/HCBighornContactStudies/Data/RelocationData/BB_Locations_Clean_15Jan2015_WithEFCRelocs.csv", header = T)
bb$UTMN <- bb$Longitude
bb$UTME <- bb$Latitude
bb$DATE <- bb$Date

#-- pull out Uncollared nonunicorn --#
bb <- subset(bb, !(EWEID == ""))
bb$EWEID <- factor(bb$EWEID)
levels(bb$EWEID)
table(bb$EWEID)

bb2014 <- subset(bb, Year == 2014)
bb2014$EWEID <- factor(bb2014$EWEID)
health.color <- c("orange", "white", "red", "white", "white", "yellow")
lamb.health.color <- c(rep("orange", 7), "black", rep("darkorange", 4), "white", "red", rep("white", 3), rep("yellow", 3))
health.color2 <- c("red", "white", "red", "white", "white", "red")
health.size <- c(1, 0, 1, 0, 0, 1) * 2
lamb.health.color2 <- c("white", "orange", "black")

bb2013 <- subset(bb, Year == 2013)
bb2013$EWEID <- factor(bb2013$EWEID)
health.color <- c("orange", "white", "red", "white", "white", "yellow")
lamb.health.color <- c(rep("orange", 7), "black", rep("darkorange", 4), "white", "red", rep("white", 3), rep("yellow", 3))
health.color2 <- c("red", "white", "red", "white", "white", "red")
health.size <- c(1, 0, 1, 0, 0, 1) * 2
lamb.health.color2 <- c("white", "orange", "black")

#-------------------------------------------------------#
#-- Mortality hazards for ewes and lambs over summer ---#
#-------------------------------------------------------#
require(BayHaz)
require(survival)

# need tabulation of number at risk and number events at each death date
ad.time1 <- rep(120, 13)
ad.time2 <- c(137, 178, 197, 215, 208, 215, rep(273, 7))
ad.event <- c(rep(1, 6), rep(0, 7))
ad.data <- as.data.frame(cbind(ad.time1, ad.time2, ad.event))

lamb.ewe <- c("")
lamb.time1 <- c(127, 132, 132, 125, 125, 133, 120, 131, 125, 123, 125)
lamb.time2 <- c(130, 184, 178, 176, 164, 169, 179, 184, 174, 146, 164)
lamb.event <- rep(1, length(lamb.time1))
lamb.data <- as.data.frame(cbind(lamb.time1, lamb.time2, lamb.event))

# 2004-2013 hazards
# adults to add (Heller Bar 2004-2013)
# 06BB06 # first summer 2006; dls 8/22/2007; 1 full summer
# 06BB10 # first summer 2006; 8 full summers
# 06BB11 # first summer 2006; dls 07/01/08; 2 full summers
# 06BB12 # first summer 2006; 8 full summers
# 06BB13 # first summer 2006; 8 full summers

ad.time1.2013 <- rep(120, 29)
ad.time2.2013 <- c(182, 230, rep(270, 27))
ad.event.2013 <- c(rep(1, 2), rep(0, 27))
ad.data.2013 <- as.data.frame(cbind(ad.time1.2013, ad.time2.2013, ad.event.2013))
ad.surv <- Surv(ad.time1.2013, ad.time2.2013, event = ad.event.2013)
ind.2014 <- c(rep(1, length(ad.time1)), rep(0, length(ad.time1.2013)))
ad.2013.km <- survfit(Surv(c(ad.time1.2013), c(ad.time2.2013), event = c(ad.event.2013)) ~ 1)
ad.2014.km <- survfit(Surv(c(ad.time1), c(ad.time2), event = c(ad.event)) ~ 1)
ad.2013.km.expand <- rep(ad.2013.km$n.event / ad.2013.km$n.risk)
plot((ad.2013.km$n.event / ad.2013.km$n.risk) ~ ad.2013.km$time, pch = 16, ylim = c(0, .3), type = "l")
lines((ad.2014.km$n.event / ad.2014.km$n.risk) ~ ad.2014.km$time, pch = 16, lty = 2)
points((ad.2013.km$n.event / ad.2013.km$n.risk) ~ ad.2013.km$time, pch = 16, col = "red")
points((ad.2014.km$n.event / ad.2014.km$n.risk) ~ ad.2014.km$time, pch = 16, col = "red")
# adults set up for 2004-2013 on Jan 22, 2015.

lamb.time1.2013 <- c(140, 140, 137, 125)
lamb.time2.2013 <- c(181, 179, 163, 180)
lamb.event.2013 <- rep(1, length(lamb.time1.2013))
lamb.data.2013 <- as.data.frame(cbind(lamb.time1.2013, lamb.time2.2013, lamb.event.2013))
# read in merged lamb data
merged.lambs <- read.csv("~/work/Kezia/Research/EcologyPapers/BB14Outbreak/Data/MergedLambData_100311_netupdate_revisedForBB11And12.csv", header = T)
merged.heller <- subset(merged.lambs, EWEID %in% c("06BB06", "06BB10", "06BB12", "06BB11", "06BB12", "06BB13"))
merged.heller$Julian.birth <- c(125, 129, 129, 136, 136, 129, 129, 129, 117, 128, 136, 129, 131, 138, 121, 127, 133, 124, 125, 141, NA, 125, 134)
pre.2013.lamb.morts <- merged.heller$Julian.birth + merged.heller$SURV_DAYS
lamb.time1.2013 <- c(lamb.time1.2013, merged.heller$Julian.birth[c(-21)])
lamb.time2.2013 <- c(lamb.time2.2013, pre.2013.lamb.morts[c(-21)])
lamb.event.2013 <- c(lamb.event.2013, merged.heller$CENSOR2[c(-21)])
lamb.data.2013 <- as.data.frame(cbind(lamb.time1.2013, lamb.time2.2013, lamb.event.2013))



# histograms of mort date for 2014 and 2004-2013
par(mfrow = c(2, 1))
breaks.in <- seq(120, 280, length.out = 17)
hist(lamb.time2.2013, xlim = c(120, 240), breaks = breaks.in, xlab = "Julian date of lamb mortalities", main = "2004-2013", col = "grey60")
hist(lamb.time2, xlim = c(120, 240), breaks = breaks.in, xlab = "Julian date of lamb mortalities", main = "2014", col = "grey60")

source("~/work/Kezia/Research/EcologyPapers/BB14Outbreak/Code/KRMPlotsBayHaz_Source_05Dec2014.R")

hypars.ad <- CPPpriorElicit(r0 = 0.001, H = 1, T00 = 200, M00 = 2, extra = 0)
CPPplotHR(CPPpriorSample(ss = 10, hyp = hypars.ad), tu = "Year")
# generate a posterior sample
ad.post <- CPPpostSample(hypars.ad, times = (ad.data$ad.time2 - 120), obs = ad.data$ad.event)
ad.post.2013 <- CPPpostSample(hypars.ad, times = (ad.data.2013$ad.time2.2013 - 120), obs = ad.data.2013$ad.event.2013)
# check that no additional CPP jumps are needed:
# if this probability is not negligible,
# go back to prior selection stage and increase 'extra'
ecdf(ad.post$sgm[ , ad.post$hyp$F])(ad.post$hyp$T00 + 3 * ad.post$hyp$sd)
ecdf(ad.post.2013$sgm[ , ad.post.2013$hyp$F])(ad.post.2013$hyp$T00 + 3 * ad.post.2013$hyp$sd)
# plot some posterior hazard rate summaries

hypars.lamb <- CPPpriorElicit(r0 = 0.01, H = 1, T00 = 200, M00 = 3, extra = 0)
CPPplotHR(CPPpriorSample(ss = 10, hyp = hypars.lamb), tu = "Year")
lamb.post <- CPPpostSample(hypars.lamb, times = (lamb.data$lamb.time2 - 120), obs = lamb.data$lamb.event)
lamb.post.2013 <- CPPpostSample(hypars.lamb, times = (lamb.data.2013$lamb.time2.2013 - 120), obs = lamb.data.2013$lamb.event.2013)
ecdf(lamb.post$sgm[ , lamb.post$hyp$F])(lamb.post$hyp$T00 + 3 * lamb.post$hyp$sd)
ecdf(lamb.post.2013$sgm[ , lamb.post.2013$hyp$F])(lamb.post.2013$hyp$T00 + 3 * lamb.post.2013$hyp$sd)

# par(mfrow = c(1, 2))
# krmCPPplotHR2samps_adults(ad.post.2013, ad.post, tu = "Day", title = "")
# krmCPPplotHR2samps_lambs(sample1 = lamb.post.2013, sample2 = lamb.post, tu = "Day", title = "")

#------------------------------------------#
#-- Plot: Ewe and lamb mortality hazards --#
#------------------------------------------#
# layout(matrix(c(1, 1, 2, 3, 3, 4), nrow = 1, byrow = T))
# par(oma = c(2, 3, 3, 1), mar = c(4, 5, 2, 1))
# krmCPPplotHR2samps_adults(ad.post.2013, ad.post, tu = "Day", title = "")
# barplot(ad.prop.surv.tab[2, ], las = 1, names.arg = c("2004-13", "2014"), beside = T, xlab = "Years", ylim = c(0, 1), ylab = "Summer ewe mortalities")
# krmCPPplotHR2samps_lambs(lamb.post.2013, lamb.post, tu = "Day", title = "")
# barplot(lamb.prop.surv.tab[2, ], las = 1, names.arg = c("2004-13", "2014"), beside = T, xlab = "Years", ylim = c(0, 1), ylab = "Summer lamb mortalities")

par(mfrow = c(2, 2), oma = c(1, 1, 1, 1))
krmCPPplotHRintervals(sample1 = ad.post.2013, npts = 101, tu = "Time Unit", title = "Ewe Hazard, 2004-2013", max.haz.ad, max.haz.lamb)  
krmCPPplotHRintervalsLambs(sample = lamb.post.2013, npts = 101, tu = "Time Unit", title = "Lamb Hazard, 2004-2013")
krmCPPplotHRintervals(sample1 = ad.post, npts = 101, tu = "Time Unit", title = "Ewe Hazard, 2014", max.haz.ad, max.haz.lamb)  
krmCPPplotHRintervalsLambs(sample = lamb.post, npts = 101, tu = "Time Unit", title = "Lamb Hazard, 2014")

# Cox PH model for adults/lambs
ad.full.time1 <- c(ad.time1.2013, ad.time1)
ad.full.time2 <- c(ad.time2.2013, ad.time2)
ad.full.event <- c(ad.event.2013, ad.event)
ad.full.year <- c(rep(0, length(ad.event.2013)), rep(1, length(ad.event)))
coxph.ad <- coxph(Surv(ad.full.time1, ad.full.time2, ad.full.event) ~ factor(ad.full.year))
plot(Surv(ad.full.time1, ad.full.time2, ad.full.event) ~ factor(ad.full.year))
summary(coxph.ad)
ad.surv.tab <- table(ad.full.event, ad.full.year)
ad.prop.surv.tab <- cbind(ad.surv.tab[, 1] / sum(ad.surv.tab[ ,1]), ad.surv.tab[, 2] / sum(ad.surv.tab[ ,2]))

lamb.full.time1 <- c(lamb.time1.2013, lamb.time1)
lamb.full.time2 <- c(lamb.time2.2013, lamb.time2)
lamb.full.event <- c(lamb.event.2013, lamb.event)
lamb.full.year <- c(rep(0, length(lamb.event.2013)), rep(1, length(lamb.event)))
coxph.lambs <- coxph(Surv(lamb.full.time1, lamb.full.time2, lamb.full.event) ~ factor(lamb.full.year))
summary(coxph.lambs)
lamb.surv.tab <- table(lamb.full.event, lamb.full.year)
lamb.prop.surv.tab <- cbind(lamb.surv.tab[, 1] / sum(lamb.surv.tab[ ,1]), lamb.surv.tab[, 2] / sum(lamb.surv.tab[ ,2]))


ad.survfit <- survfit(Surv(ad.full.time1, ad.full.time2, ad.full.event) ~ factor(ad.full.year))
ad.survfit.2013 <- survfit(Surv(ad.time1.2013, ad.time2.2013, ad.event.2013) ~ 1)
ad.survfit.2014 <- survfit(Surv(ad.time1, ad.time2, ad.event) ~ 1)

lamb.survfit <- survfit(Surv(lamb.full.time1, lamb.full.time2, lamb.full.event) ~ factor(lamb.full.year))
lamb.survfit.2013 <- survfit(Surv(lamb.time1.2013, lamb.time2.2013, lamb.event.2013) ~ 1)
lamb.survfit.2014 <- survfit(Surv(lamb.time1, lamb.time2, lamb.event) ~ 1)

par(mfrow = c(1, 2))
plot(ad.survfit, conf.int = T, xlab = "Julian date", ylab = "Proportion surviving", 
     main = "Ewes", xlim = c(120, 250))
polygon(x = c(summary(ad.survfit.2013)$time, 250, 250, rev(summary(ad.survfit.2013)$time)), 
        y = c(summary(ad.survfit.2013)$lower, summary(ad.survfit.2013)$lower[2], 
              summary(ad.survfit.2013)$upper[2], rev(summary(ad.survfit.2013)$upper)), 
        col = rgb(.2, .2, .2, alpha = .8))
polygon(x = c(summary(ad.survfit.2014)$time, 250, 250, rev(summary(ad.survfit.2014)$time)), 
        y = c(summary(ad.survfit.2014)$lower, summary(ad.survfit.2014)$lower[5], 
              summary(ad.survfit.2014)$upper[5], rev(summary(ad.survfit.2014)$upper)), 
        col = rgb(1, 0, 0, alpha = .5))
leg.text <- c("2014", "2004-2013")
legend("bottomleft", leg.text, fill = c(rgb(1, 0, 0, alpha = .5), rgb(.2, .2, .2, alpha = .8)), bty = "n")

plot(lamb.survfit, conf.int = T, xlab = "Julian date", ylab = "Proportion surviving", 
     main = "Lambs", xlim = c(120, 250))
polygon(x = c(summary(lamb.survfit.2013)$time, 250, 250, 
              rev(summary(lamb.survfit.2013)$time)), 
        y = c(summary(lamb.survfit.2013)$lower, 
              summary(lamb.survfit.2013)$lower[17], 
              summary(lamb.survfit.2013)$upper[17], 
              rev(summary(lamb.survfit.2013)$upper)), 
        col = rgb(.2, .2, .2, alpha = .8))
polygon(x = c(summary(lamb.survfit.2014)$time[1:9], 250, 250,
              rev(summary(lamb.survfit.2014)$time[1:9])), 
        y = c(summary(lamb.survfit.2014)$lower[1:8], 0,
              0, 
              summary(lamb.survfit.2014)$upper[8], 
              summary(lamb.survfit.2014)$upper[8],
              rev(summary(lamb.survfit.2014)$upper[1:8])), 
        col = rgb(1, 0, 0, alpha = .5))


#------------------------------------------#
#-- moving average clinical signs : ewes --#
#------------------------------------------#
# 2013
bb2013$Ewe.Health
levels(bb2013$Ewe.Health)
bb2013$EweClinicalSignsDailyScore <- ifelse(bb2013$Ewe.Health == "Coughing", 2, ifelse(bb2013$Ewe.Health == "Runny Nose" | bb2013$Ewe.Health == "Lethargic", 1, 0))

# extract ewe-specific data
ewe.list.2013 <- vector("list", length(levels(bb2013$EWEID)))
for(i in 1:length(ewe.list.2013)){
  ewe.list.2013[[i]] <- subset(bb2013, EWEID == levels(bb2013$EWEID)[i])
}

# build matrix that is days x ewes; calculate seven-day moving average for each ewe over range of study
mov.avg.mat.2013 <- matrix(NA, ncol = range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1], nrow = length(levels(bb2013$EWEID)))
for(i in 1:dim(mov.avg.mat.2013)[1]){
  ewe.dat.2013 <- ewe.list.2013[[i]]
  for(j in 1:(range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1] - 6)){
    mov.avg.mat.2013[i, j] <- mean(na.omit(subset(ewe.dat.2013, JulianDate %in% seq(122 + j - 3, 122 + j + 3))$EweClinicalSignsDailyScore))
    mov.avg.mat.2013[i, j] <- ifelse(is.na(mov.avg.mat.2013[i, j]) == T, 0, mov.avg.mat.2013[i, j])    
  }
}

aggregate.clin.signs.2013 <- apply(mov.avg.mat.2013, 2, sum) / 4

# 2014
bb2014$Ewe.Health
levels(bb2014$Ewe.Health)
bb2014$EweClinicalSignsDailyScore <- ifelse(bb2014$Ewe.Health == "Coughing", 2, ifelse(bb2014$Ewe.Health == "Runny Nose" | bb2014$Ewe.Health == "Lethargic", 1, 0))

# extract ewe-specific data
ewe.list <- vector("list", length(levels(bb2014$EWEID)))
for(i in 1:length(ewe.list)){
  ewe.list[[i]] <- subset(bb2014, EWEID == levels(bb2014$EWEID)[i])
}

# build matrix that is days x ewes; calculate seven-day moving average for each ewe over range of study
mov.avg.mat <- matrix(NA, ncol = range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1], nrow = length(levels(bb2014$EWEID)))
for(i in 1:dim(mov.avg.mat)[1]){
  ewe.dat <- ewe.list[[i]]
  for(j in 1:(range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1] - 6)){
    mov.avg.mat[i, j] <- mean(na.omit(subset(ewe.dat, JulianDate %in% seq(122 + j - 3, 122 + j + 3))$EweClinicalSignsDailyScore))
    mov.avg.mat[i, j] <- ifelse(is.na(mov.avg.mat[i, j]) == T, 0, mov.avg.mat[i, j])    
  }
}

aggregate.clin.signs <- apply(mov.avg.mat, 2, sum) / 12

#------------------------------------------#
#-- moving average clinical signs: lambs --#
#------------------------------------------#
# 2013
levels(bb2013$Lamb.Health)
bb2013$LambClinicalSignsDailyScore <- ifelse(bb2013$Lamb.Health == "Coughing" | bb2013$Lamb.Health == "Droopy Ears and Lethargic" | bb2013$Lamb.Health == "Droopy Ears and Shaking Head" , 2, 
                                             ifelse(bb2013$Lamb.Health == "Coughing and Lethargic" | bb2013$Lamb.Health == "Droopy Ears and Lethargic and Shaking Head"  | bb2013$Lamb.Health == "Runny Nose and Droopy Ears and Lethargic" | bb2013$Lamb.Health == "Coughing and Shaking Head", 3, 
                                                    ifelse(bb2013$Lamb.Health == "Coughing and DroopyEars and Lethargic" | bb2013$Lamb.Health == "Coughing and Droopy Ears and Shaking Head"  | bb2013$Lamb.Health == "Coughing and Runny Nose and Droopy Ears", 4, 
                                                           ifelse(bb2013$Lamb.Health == "Runny Nose" | bb2013$Lamb.Health == "Lethargic" | bb2013$Lamb.Health == "Droopy Ears" | bb2013$Lamb.Health == "Shaking Head", 1, 
                                                                  ifelse(bb2013$Lamb.Health == "Coughing and Droopy Ears and Lethargic and Shaking Head", 5, 
                                                                         ifelse(bb2013$Lamb.Health == "Healthy", 0, NA))))))

bb2013$LambClinicalSignsDailyScore2 <- ifelse(bb2013$Lamb.Health == "Coughing" | bb2013$Lamb.Health == "Coughing and Lethargic" | bb2013$Lamb.Health == "Coughing and DroopyEars and Lethargic" | bb2013$Lamb.Health == "Coughing and Droopy Ears and Shaking Head" | bb2013$Lamb.Health == "Coughing and Runny Nose and Droopy Ears" | bb2013$Lamb.Health == "Coughing and Shaking Head" | bb2013$Lamb.Health == "Coughing and Droopy Ears and Lethargic and Shaking Head", 2, 
                                              ifelse(bb2013$Lamb.Health == "Droopy Ears and Lethargic" | bb2013$Lamb.Health == "Droopy Ears and Shaking Head" | bb2013$Lamb.Health == "Droopy Ears and Lethargic and Shaking Head"  | bb2013$Lamb.Health == "Runny Nose and Droopy Ears and Lethargic" | bb2013$Lamb.Health == "Runny Nose" | bb2013$Lamb.Health == "Lethargic" | bb2013$Lamb.Health == "Droopy Ears" | bb2013$Lamb.Health == "Shaking Head", 1, 
                                                     ifelse(bb2013$Lamb.Health == "Healthy", 0, NA)))

# extract ewe-specific data
lamb.list.2013 <- vector("list", length(levels(bb2013$EWEID)))
for(i in 1:length(lamb.list.2013)){
  lamb.list.2013[[i]] <- subset(bb2013, EWEID == levels(bb2013$EWEID)[i])
}

# build matrix that is days x ewes; calculate seven-day moving average for each ewe over range of study
# mov.avg.mat.l.2013 <- matrix(NA, ncol = range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1], nrow = length(levels(bb2013$EWEID)))
# for(i in 1:dim(mov.avg.mat.l.2013)[1]){
#   lamb.dat.2013 <- lamb.list.2013[[i]]
#   for(j in 1:(range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1] - 6)){
#     mov.avg.mat.l.2013[i, j] <- mean(na.omit(subset(lamb.dat.2013, JulianDate %in% seq(122 + j - 3, 122 + j + 3))$LambClinicalSignsDailyScore))
#     mov.avg.mat.l.2013[i, j] <- ifelse(is.na(mov.avg.mat.l.2013[i, j]) == T, 0, mov.avg.mat.l.2013[i, j])    
#   }
# }
# 
# aggregate.clin.signs.l.2013 <- apply(mov.avg.mat.l.2013, 2, sum) / 4

mov.avg.mat.l2.2013 <- matrix(NA, ncol = range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1], nrow = length(levels(bb2013$EWEID)))
for(i in 1:dim(mov.avg.mat.l2.2013)[1]){
  lamb.dat.2013 <- lamb.list.2013[[i]]
  for(j in 1:(range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1] - 6)){
    mov.avg.mat.l2.2013[i, j] <- mean(na.omit(subset(lamb.dat.2013, JulianDate %in% seq(122 + j - 3, 122 + j + 3))$LambClinicalSignsDailyScore2))
    mov.avg.mat.l2.2013[i, j] <- ifelse(is.na(mov.avg.mat.l2.2013[i, j]) == T, 0, mov.avg.mat.l2.2013[i, j])    
  }
}

aggregate.clin.signs.l2.2013 <- apply(mov.avg.mat.l2.2013, 2, sum) / 4

# 2014
levels(bb2014$Lamb.Health)
bb2014$LambClinicalSignsDailyScore <- ifelse(bb2014$Lamb.Health == "Coughing" | bb2014$Lamb.Health == "Droopy Ears and Lethargic" | bb2014$Lamb.Health == "Droopy Ears and Shaking Head" , 2, 
                                      ifelse(bb2014$Lamb.Health == "Coughing and Lethargic" | bb2014$Lamb.Health == "Droopy Ears and Lethargic and Shaking Head"  | bb2014$Lamb.Health == "Runny Nose and Droopy Ears and Lethargic" | bb2014$Lamb.Health == "Coughing and Shaking Head", 3, 
                                      ifelse(bb2014$Lamb.Health == "Coughing and DroopyEars and Lethargic" | bb2014$Lamb.Health == "Coughing and Droopy Ears and Shaking Head"  | bb2014$Lamb.Health == "Coughing and Runny Nose and Droopy Ears", 4, 
                                      ifelse(bb2014$Lamb.Health == "Runny Nose" | bb2014$Lamb.Health == "Lethargic" | bb2014$Lamb.Health == "Droopy Ears" | bb2014$Lamb.Health == "Shaking Head", 1, 
                                      ifelse(bb2014$Lamb.Health == "Coughing and Droopy Ears and Lethargic and Shaking Head", 5, 
                                      ifelse(bb2014$Lamb.Health == "Healthy", 0, NA))))))

bb2014$LambClinicalSignsDailyScore2 <- ifelse(bb2014$Lamb.Health == "Coughing" | bb2014$Lamb.Health == "Coughing and Lethargic" | bb2014$Lamb.Health == "Coughing and DroopyEars and Lethargic" | bb2014$Lamb.Health == "Coughing and Droopy Ears and Shaking Head" | bb2014$Lamb.Health == "Coughing and Runny Nose and Droopy Ears" | bb2014$Lamb.Health == "Coughing and Shaking Head" | bb2014$Lamb.Health == "Coughing and Droopy Ears and Lethargic and Shaking Head", 2, 
                                       ifelse(bb2014$Lamb.Health == "Droopy Ears and Lethargic" | bb2014$Lamb.Health == "Droopy Ears and Shaking Head" | bb2014$Lamb.Health == "Droopy Ears and Lethargic and Shaking Head"  | bb2014$Lamb.Health == "Runny Nose and Droopy Ears and Lethargic" | bb2014$Lamb.Health == "Runny Nose" | bb2014$Lamb.Health == "Lethargic" | bb2014$Lamb.Health == "Droopy Ears" | bb2014$Lamb.Health == "Shaking Head", 1, 
                                       ifelse(bb2014$Lamb.Health == "Healthy", 0, NA)))

# extract ewe-specific data
lamb.list <- vector("list", length(levels(bb2014$EWEID)))
for(i in 1:length(lamb.list)){
  lamb.list[[i]] <- subset(bb2014, EWEID == levels(bb2014$EWEID)[i])
}

# # build matrix that is days x ewes; calculate seven-day moving average for each ewe over range of study
# mov.avg.mat.l <- matrix(NA, ncol = range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1], nrow = length(levels(bb2014$EWEID)))
# for(i in 1:dim(mov.avg.mat.l)[1]){
#   lamb.dat <- lamb.list[[i]]
#   for(j in 1:(range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1] - 6)){
#     mov.avg.mat.l[i, j] <- mean(na.omit(subset(lamb.dat, JulianDate %in% seq(122 + j - 3, 122 + j + 3))$LambClinicalSignsDailyScore))
#     mov.avg.mat.l[i, j] <- ifelse(is.na(mov.avg.mat.l[i, j]) == T, 0, mov.avg.mat.l[i, j])    
#   }
# }
# 
# aggregate.clin.signs.l <- apply(mov.avg.mat.l, 2, sum)

mov.avg.mat.l2 <- matrix(NA, ncol = range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1], nrow = length(levels(bb2014$EWEID)))
for(i in 1:dim(mov.avg.mat.l2)[1]){
  lamb.dat <- lamb.list[[i]]
  for(j in 1:(range(bb2014$JulianDate)[2] - range(bb2014$JulianDate)[1] - 6)){
    mov.avg.mat.l2[i, j] <- mean(na.omit(subset(lamb.dat, JulianDate %in% seq(122 + j - 3, 122 + j + 3))$LambClinicalSignsDailyScore2))
    mov.avg.mat.l2[i, j] <- ifelse(is.na(mov.avg.mat.l2[i, j]) == T, 0, mov.avg.mat.l2[i, j])    
  }
}

aggregate.clin.signs.l2 <- apply(mov.avg.mat.l2, 2, sum) / 12

#-----------------------------------------------#
#-- Plot: ewe and lamb clin signs --------------#
#-----------------------------------------------#
# layout(matrix(c(1, 1, 1, 1, 2, 2, 3, 3, 2, 2, 3, 3, 2, 2, 3, 3, 4, 4, 5, 5, 4, 4, 5, 5, 4, 4, 5, 5), 7, 4, byrow = 2))
# plot(x = 0, y = 0, xlim = c(0, 1), ylim = c(0, 1), xaxt = "n", yaxt = "n")
# legend("topleft", leg.text, col = c("red", "black"), lwd = c(1, 1), lty = c(1, 1), pch = c(16, 16), pt.cex = c(.6, .6), bty = "n", cex = 1)

par(mfrow = c(1, 2), oma = c(2, 2, 1, 2), mar = c(2, 2, 1, 3), xpd = F)
plot(0, 0, col = "black", type = "p", yaxt = "n", ylab = "", xlab = "Date", xaxt = "n", xlim = c(120, 260),  lty = 2, lwd = .75, ylim = c(0, 1.7), pch = 16, cex = .6)
lines(lowess(aggregate.clin.signs.2013 ~ seq(124, 124 + 150), f = 1/4), col = "black",  lty = 1, lwd = 3)
points(jitter(aggregate.clin.signs.2013, 1.5) ~ seq(124, 124 + 150), pch = 16, cex = .6)
axis(side = 2, at = seq(0, 1.5, length.out = 4), label = round(seq(0, 1.5, length.out = 4), 2))
mtext(side = 2, outer = F, line = 2, "Clinical signs", las = 0)
axis(las = 1, side = 1, at = 120 + c(0, 31, 61, 92, 123), labels = c("May 1", "June 1", "July 1", "Aug 1", "Sept 1"))

par(new = T)
plot(0, 0, ylim = c(0, 1.7), col = "red", type = "p", yaxt = "n", ylab = "", xlab = "", xaxt = "n", xlim = c(120, 260), lty = 2, lwd = .75, pch = 16, cex = .6)
lines(lowess(aggregate.clin.signs.l2.2013[1:61] ~ seq(124, 124 + 60), f = 1/4), col = "red",  lty = 1, lwd = 3)
#points(jitter(aggregate.clin.signs.l2.2013, 5)[1:61] ~ seq(124, 124 + 60), pch = 16, col = "red", cex = .6)
points(aggregate.clin.signs.l2.2013[1:61] ~ seq(124, 124 + 60), pch = 16, col = "red", lty = 2, cex = .6)
#axis(side = 4, at = seq(0, 1.5, length.out = 4), label = round(seq(0, 1.5, length.out = 4), 2))
#mtext(side = 4, outer = F, line = 2, "", las = 0)
leg.text <- c("Lambs", "Ewes")
legend(x = 130, y = 1.77, leg.text, col = c("red", "black"), lwd = c(1, 1), lty = c(1, 1), pch = c(16, 16), pt.cex = c(.6, .6), bty = "n", cex = 1)
text(x = 240, y = 1.6, "2013")

plot(0 ~ 0, col = "black", type = "p", yaxt = "n", ylab = "", xlab = "Date", xaxt = "n", xlim = c(120, 260),  lty = 2, lwd = .75, ylim = c(0, 1.7), pch = 16, cex = .6)
points(jitter(aggregate.clin.signs, 1.5) ~ seq(124, 124 + 150), pch = 16, cex = .6)
lines(lowess(aggregate.clin.signs ~ seq(124, 124 + 150), f = 1/4), col = "black",  lty = 1, lwd = 3)
axis(side = 2, at = seq(0, 1.5, length.out = 4), label = round(seq(0, 1.5, length.out = 4), 2))
axis(las = 1, side = 1, at = 120 + c(0, 31, 61, 92, 123), labels = c("May 1", "June 1", "July 1", "Aug 1", "Sept 1"))
mtext(side = 2, outer = F, line = 2, "", las = 0)

par(new = T)
plot(0 ~ 0, ylim = c(0, 1.7), col = "red", type = "p", yaxt = "n", ylab = "", xlab = "", xaxt = "n", xlim = c(120, 260), lty = 2, lwd = .75, pch = 16, cex = .6)
lines(lowess(aggregate.clin.signs.l2[1:64] ~ seq(124, 124 + 63), f = 1/4), col = "red",  lty = 1, lwd = 3)
points(jitter(aggregate.clin.signs.l2, 5)[1:64] ~ seq(124, 124 + 63), pch = 16, cex = .6, col = "red")
points(aggregate.clin.signs.l2[1:64] ~ seq(124, 124 + 63), pch = 16, cex = .6, col = "red")
#axis(side = 4, at = seq(0, 1.5, length.out = 4), label = round(seq(0, 1.5, length.out = 4), 2))
#mtext(side = 4, outer = F, line = 2, "Lamb clinical signs", las = 0)
#leg.text <- c("Lambs", "Ewes")
#legend("topright", leg.text, col = c("red", "black"), lwd = c(1, 1), lty = c(1, 1), pch = c(16, 16), pt.cex = c(.6, .6), bty = "n", cex = 1)
text(x = 240, y = 1.6, "2014")

#-------------------------------------------#
#-- Plot: Time course of disease -----------#
#-------------------------------------------#
first.disease.date <- last.disease.date <- disease.duration <- rep(NA, length(levels(factor(bb2014$EWEID))))
first.disease.date.l <- last.disease.date.l <- disease.duration.l <- rep(NA, length(levels(factor(bb2014$EWEID))))
for(i in 1:length(levels(factor(bb2014$EWEID)))){
  k <- subset(bb2014, as.character(EWEID) == levels(factor(bb2014$EWEID))[i])
  first.disease.date[i] <- k$JulianDate[min(which(k$EweClinicalSignsDailyScore >= 1))]
  last.disease.date[i] <- k$JulianDate[max(which(k$EweClinicalSignsDailyScore >= 1))]
  disease.duration[i] <- last.disease.date[i] - first.disease.date[i]
  first.disease.date.l[i] <- k$JulianDate[min(which(k$LambClinicalSignsDailyScore >= 1))]
  last.disease.date.l[i] <- k$JulianDate[max(which(k$LambClinicalSignsDailyScore >= 1))]
  disease.duration.l[i] <- last.disease.date.l[i] - first.disease.date.l[i]
}
ewe.outcome <- factor(c(0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 1), levels = c(1, 0))
disease.duration.bin <- factor(c(0, 4, 4, 1, 6, 3, 3, NA, 0, 3, 4, 0, 4), levels = seq(0, 6))
disease.duration.bin.l <- factor(c(NA, 2, 1, 1, 2, 2, 0, NA, 0, 0, 2, NA, 0), levels = seq(0, 6))
lamb.outcome <- factor(c(rep(0, 7), NA, rep(0, 5)), levels = c(1, 0))
dat.tab <- table(ewe.outcome, disease.duration.bin)
dat.tab.l <- table(lamb.outcome, disease.duration.bin.l)

ewe.line.col <- c("black", "red")
ewe.labels <- c("Ewe 53", "Ewe 22", "Ewe 17", "Ewe 09", "Ewe 07", "Ewe 05", "Ewe 04", "Ewe 03", "Ewe 02", "Ewe 36", "Ewe 13", "Ewe 12", "Ewe 10")
lamb.labels <- c("Lamb 53", "Lamb 22", "Lamb 17", "Lamb 09", "Lamb 07", "Lamb 05", "Lamb 04", "Lamb 03", "Lamb 02", "Lamb 36", "Lamb 13", "Lamb 12", "Lamb 10")
  
par(mfrow = c(2, 2), mar = c(4, 4, 4, 4), oma = c(1, 1, 3, 1))
barplot(dat.tab, legend.text = c("Survived", "Died"), args.legend = list(x = 7, y = 4.2, bty = "n"), col = ewe.line.col, width = 2, xaxt = "n", xlab = "", ylab = "Ewes")
axis(side = 1, at = (seq(0, 6) * 2.4) + .2, labels = c("0", "10", "20", "30", "40", "50", "60"), las = 1)
plot(x = 0, y = 0, ylim = c(0, 13), xlim = c(120, 220), xlab = "", xaxt = "n", yaxt = "n", ylab = "")
for(i in 1:13){
  segments(y0 = i, y1 = i, lwd = 1.5, x0 = first.disease.date[i], x1 = last.disease.date[i], col = ewe.line.col[as.numeric(ewe.outcome)[i]])
}
axis(side = 1, at = c(120, 151, 182, 213), labels = c("May 1", "June 1", "July 1", "Aug 1"), las = 0)
axis(side = 2, at = seq(1:13), labels = ewe.labels, las = 1)
barplot(dat.tab.l, col = ewe.line.col, width = 2, xaxt = "n", xlab = "Duration (days)", ylab = "Lambs")
axis(side = 1, at = (seq(0, 6) * 2.4) + .2, labels = c("0", "10", "20", "30", "40", "50", "60"), las = 1)
plot(x = 0, y = 0, ylim = c(0, 13), xlim = c(120, 220), xlab = "Date", xaxt = "n", ylab = "", yaxt = "n")
for(i in 1:13){
  segments(y0 = i, y1 = i, lwd = 1.5, x0 = first.disease.date.l[i], x1 = last.disease.date.l[i], col = "red")
}
axis(side = 1, at = c(120, 151, 182, 213), labels = c("May 1", "June 1", "July 1", "Aug 1"), las = 0)
axis(side = 2, at = seq(1:13), labels = lamb.labels, las = 1)
mtext(side = 3, line = 0, outer = T, "Clinical Signs", las = 0)

# rank test for difference in medians
disease.duration.full <- c(disease.duration, disease.duration.l)
ewe.lamb.ind <- c(rep("E", length(disease.duration)), rep("L", length(disease.duration.l)))
duration.data <- as.data.frame(cbind(disease.duration.full, factor(ewe.lamb.ind)))
names(duration.data)[2] <- "ewe.lamb.ind"
duration.data <- duration.data[complete.cases(duration.data), ]

require(coin)
mann.whitney.duration <- wilcox.test(disease.duration.full ~ ewe.lamb.ind, data = duration.data)
mann.whitney.duration.coin <- wilcox_test(disease.duration.full ~ factor(ewe.lamb.ind), distribution = "exact", data = duration.data)
r = rank(duration.data$disease.duration.full) 
duration.data$r <- r
duration.data.ad <- subset(duration.data, ewe.lamb.ind == 1)
duration.data.lamb <- subset(duration.data, ewe.lamb.ind == 2)
mean(duration.data.ad$r) # mean rank = 14.75
mean(duration.data.lamb$r) # mean rank = 7.9

# bootstrapped CIs for each group
nboot <- 1000
boot.ad.mean <- boot.lamb.mean <- rep(NA, nboot)
for(i in 1:nboot){
  sample.i.ad <- sample(1:length(na.omit(disease.duration)), size = length(na.omit(disease.duration)), replace = T)
  boot.ad.mean[i] <- mean(na.omit(disease.duration)[sample.i.ad])
  sample.i.lamb <- sample(1:length(na.omit(disease.duration.l)), size = length(na.omit(disease.duration.l)), replace = T)
  boot.lamb.mean[i] <- mean(na.omit(disease.duration.l)[sample.i.lamb])
}

quantile(boot.ad.mean, c(0.025, 0.5, 0.975))
quantile(boot.lamb.mean, c(0.025, 0.5, 0.975))

#---------------------------------------#
#-- IGS strain type data ---------------#
#---------------------------------------#
igs <- read.csv("~/work/Kezia/Research/EcologyPapers/BB14Outbreak/Data/HellsCanyon_MoviIGS_23Jan2015.csv", header = T)
igs.0414 <- subset(igs, YEAR >= 2004) 
dim(igs.0414) # 115 IGS samples from Hells Canyon from 2004 to present
table(is.na(igs.0414$LENGTH_IGS)) # 12 NAs in IGS length
length(levels(factor(igs.0414$ANIMAL.ID))) # 82 unique animals

# build clean igs dataset (only one row per animal; remove rows with missing IGS lengths)
igs.mismatch <- igs.individ <- animal.id <- year <- pop <- rep(NA, length(levels(factor(igs.0414$ANIMAL.ID))))
for(i in 1:length(levels(factor(igs.0414$ANIMAL.ID)))){
  k <- subset(igs.0414, as.character(ANIMAL.ID) == levels(factor(igs.0414$ANIMAL.ID))[i])
  animal.id[i] <- as.character(levels(factor(igs.0414$ANIMAL.ID))[i])
  year[i] <- k$YEAR[1]
  pop[i] <- as.character(k$POPULATION[1])
  igs.individ[i] <- k$LENGTH_IGS[1]
  igs.mismatch[i] <- ifelse(length(levels(factor(k$LENGTH_IGS))) == 1 | length(levels(factor(k$LENGTH_IGS))) == 0, 0, 1)
}

# only animal with IGS mismatch is 14BB07 (= 1291 in Black Butte this summer)
clean.igs <- as.data.frame(cbind(animal.id, year, pop, igs.individ, igs.mismatch))
clean.igs <- clean.igs[complete.cases(clean.igs), ]

# pull out animal with strain labeled "354" -- likely partial strain
clean.igs <- subset(clean.igs, igs.individ != 354)

# clean up population field
clean.igs$pop2 <- ifelse(clean.igs$pop == "HC_AsotinCreek" | clean.igs$pop == "HC_AsotinCreek " | clean.igs$pop == "HC_Asotin_Creek " | clean.igs$pop == "HC_Asotin_Creek" | clean.igs$pop == "HC_AsotinCr captured in Asotin", "HC_ASOTIN", as.character(clean.igs$pop))
clean.igs$pop3 <- ifelse(clean.igs$pop2 == "HC_ASOTIN", "Aso",
                  ifelse(clean.igs$pop2 == "HC_BIGCANYON", "BCan",
                  ifelse(clean.igs$pop2 == "HC_BLACKBUTTE", "BB",
                  ifelse(clean.igs$pop2 == "HC_IMNAHA", "Imn",
                  ifelse(clean.igs$pop2 == "HC_LOSTINE", "Los",
                  ifelse(clean.igs$pop2 == "HC_LOWERHELLSCANYON", "LHC",
                  ifelse(clean.igs$pop2 == "HC_MOUNTAINVIEW", "MV",
                  ifelse(clean.igs$pop2 == "HC_MUIR", "Muir",
                  ifelse(clean.igs$pop2 == "HC_MYERS", "Myers",
                  ifelse(clean.igs$pop2 == "HC_REDBIRD", "RB",
                  ifelse(clean.igs$pop2 == "HC_SADDLECREEK", "SC",
                  ifelse(clean.igs$pop2 == "HC_SHEEP_MOUNTAIN", "SM",
                  ifelse(clean.igs$pop2 == "HC_UPPERHELLSCANYON", "UHC",
                  ifelse(clean.igs$pop2 == "HC_WENAHA", "Wen", NA
                    ))))))))))))))

# descriptives on clean.igs
dim(clean.igs) # 73 observations
table(factor(clean.igs$pop2))
table(clean.igs$year)
table(factor(clean.igs$igs.individ))
clean.igs.to2013 <- subset(clean.igs, as.numeric(as.character(year)) <= 2013)
igs.props.to2013 <- c(56 / 61, 5 / 61)
igs.props.with2014 <- c(63 / 72, 5 / 72, 4 / 72)
igs.props.393vsNot <- c((63 + 5) / 72, 4 / 72)
# what is the probability that we would not have detected 393 previously? 
# ... in 61 trials prior to 2014, what is chance of no 393? 
prob.no393.PriorTo2013 <- pbinom(0, 61, igs.props.393vsNot[2]) # prob of missing is 0.03

# Now get prob for just Aso/Wen/MV/Imn/LHC/RB/BB
neighbor.pops <- c("Aso", "BB", "Imn", "LHC", "Wen", "MV", "RB")
clean.igs.neighborhood <- subset(clean.igs, as.character(pop3) %in% neighbor.pops)
dim(clean.igs.neighborhood) # 48 observations in the neighborhood
table(clean.igs.neighborhood$igs.individ)
neighborhood.props <- c(40 / 48, 4 / 48, 4 / 48)
clean.igs.neighborhood.to2013 <- subset(clean.igs.neighborhood, as.numeric(as.character(year)) <= 2013)
dim(clean.igs.neighborhood.to2013) # 38 sequences in neighborhood prior to 2014
neighborhood.prob.no393.PriorTo2013 <- pbinom(0, 38, neighborhood.props[3]) # prob of missing is 0.03
  # P = 0.036 of missing prior to 2014 if we only use neighborhood, 
  # and strain is actually always circulating at spatiotemporally 
  # aggregated rate of 4 / 48.

# adjust for maybe oversampling BB -- what if we'd only seen the one
# 393 ram in Aso...
neighborhood.props.2 <- c(40 / 45, 4 / 45, 1 / 45)
neighborhood.prob.no393.PriorTo2013.2 <- pbinom(0, 38, neighborhood.props.2[3]) # prob of missing is 0.03
  # P = .42 of missing 393 prior to 2014 if the 393 strain is actually only present at rate
  # represented by the one randomly-hit Asotin ram (in which case 393 strain 
  # is actually very rare -- only present at 1 / 45 samples (denominator adjusted))
  # to eliminate BB 393 samples for 2014. 
neighborhood.prob.no393.PriorTo2013.2.test20 <- pbinom(0, 58, neighborhood.props.2[3]) # prob of missing is 0.03
neighborhood.prob.no393.PriorTo2013.2.test40 <- pbinom(0, 78, neighborhood.props.2[3]) # prob of missing is 0.03
neighborhood.prob.no393.PriorTo2013.2.test60 <- pbinom(0, 98, neighborhood.props.2[3]) # prob of missing is 0.03

# how is BB represented in the existing data?
clean.igs.bb <- subset(clean.igs, as.character(pop3) == "BB")
table(clean.igs.bb$year)

#--------------------------------------------------#
#-- Plot: IGS length data through space and time --#
#--------------------------------------------------#
par(mfrow = c(1, 2), mar = c(2, 4, 1, 1), oma = c(3, 1, 1, 1))
barplot(table(factor(clean.igs$igs.individ), clean.igs$year), legend.text = levels(factor(clean.igs$igs.individ)), args.legend = list(x = 2.5, y = 12, bty = "n"), ylab = "Frequency")
barplot(table(factor(clean.igs$igs.individ), clean.igs$pop3), legend.text = levels(factor(clean.igs$igs.individ)), args.legend = list(x = 16, y = 20, bty = "n"), ylab = "")

