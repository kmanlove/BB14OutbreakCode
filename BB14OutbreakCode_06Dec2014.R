#--------------------------------------------------#
#-- Code for Black Butte 2014 Epidemic paper ------#
#--------------------------------------------------#

bb <- read.csv("~/work/Kezia/Research/FieldStudies/HCBighornContactStudies/Data/RelocationData/BlackButteLocationsFinal_20July2014.csv", header = T, sep = "\t")
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

# #-----------------------------------#
# #-- KRM timeline stripplot ---------#
# #-----------------------------------#
# # stripplot
# max.date <- 243
# par(mfrow = c(1, 1), oma = c(1, 1, 0, 0), mar = c(4, 6, 1, 1))
# plot(as.numeric(factor(bb2014$EWEID)) ~ as.numeric(as.character(bb2014$JulianDate)), pch = 16, col = health.color2[bb2014$Ewe.Health], cex = health.size[bb2014$Ewe.Health], xlab = "Date", yaxt = "n", ylab = "", xlim = c(121, max.date), xaxt = "n")
# axis(side = 2, line = 1, outer = F, las = 1, at = seq(1:12), labels = levels(factor(bb2014$EWEID)))
# axis(side = 1, line = 0, outer = F, las = 1, at = c(120, 151, 181, 212, 243), labels = c("May 01", "June 01", "July 01", "Aug 01", "Sept 01"))
# 
# # pre-lambing polygons
# polygon(x = c(120, 120, 127, 127), y = c(.75, 1.25, 1.25, .75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 132, 132), y = c(1.75, 2.25, 2.25, 1.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 132, 132), y = c(2.75, 3.25, 3.25, 2.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 125, 125), y = c(3.75, 4.25, 4.25, 3.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 125, 125), y = c(4.75, 5.25, 5.25, 4.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 133, 133), y = c(5.75, 6.25, 6.25, 5.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 131, 131), y = c(7.75, 8.25, 8.25, 7.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 125, 125), y = c(9.75, 10.25, 10.25, 9.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 123, 123), y = c(10.75, 11.25, 11.25, 10.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 125, 125), y = c(11.75, 12.25, 12.25, 11.75), col = "grey80", border = "grey80")
# 
# # post-lamb-mort polygons
# polygon(x = c(130, 130, max.date, max.date), y = c(.75, 1.25, 1.25, .75), col = "grey80", border = "grey80")
# polygon(x = c(184, 184, max.date, max.date), y = c(1.75, 2.25, 2.25, 1.75), col = "grey80", border = "grey80")
# polygon(x = c(178, 178, max.date, max.date), y = c(2.75, 3.25, 3.25, 2.75), col = "grey80", border = "grey80")
# polygon(x = c(176, 176, max.date, max.date), y = c(3.75, 4.25, 4.25, 3.75), col = "grey80", border = "grey80")
# polygon(x = c(164, 164, max.date, max.date), y = c(4.75, 5.25, 5.25, 4.75), col = "grey80", border = "grey80")
# polygon(x = c(169, 169, max.date, max.date), y = c(5.75, 6.25, 6.25, 5.75), col = "grey80", border = "grey80")
# polygon(x = c(179, 179, max.date, max.date), y = c(6.75, 7.25, 7.25, 6.75), col = "grey80", border = "grey80")
# polygon(x = c(184, 184, max.date, max.date), y = c(7.75, 8.25, 8.25, 7.75), col = "grey80", border = "grey80")
# polygon(x = c(174, 174, max.date, max.date), y = c(9.75, 10.25, 10.25, 9.75), col = "grey80", border = "grey80")
# polygon(x = c(146, 146, max.date, max.date), y = c(10.75, 11.25, 11.25, 10.75), col = "grey80", border = "grey80")
# polygon(x = c(164, 164, max.date, max.date), y = c(11.75, 12.25, 12.25, 11.75), col = "grey80", border = "grey80")
#   
# # ewe-mort polygons
# polygon(x = c(137, 137, max.date, max.date), y = c(8.75, 9.25, 9.25, 8.75), col = "black", border = "grey80")
# text(x = 145, y = 9, "Ewe Died", col = "white")
# polygon(x = c(178, 178, max.date, max.date), y = c(10.75, 11.25, 11.25, 10.75), col = "black", border = "grey80")
# text(x = 186, y = 11, "Ewe Died", col = "white")
# polygon(x = c(197, 197, max.date, max.date), y = c(.75, 1.25, 1.25, 0.75), col = "black", border = "grey80")
# text(x = 205, y = 1, "Ewe Died", col = "white")
# polygon(x = c(197, 197, max.date, max.date), y = c(.75, 1.25, 1.25, 0.75), col = "black", border = "grey80")
# text(x = 205, y = 1, "Ewe Died", col = "white")
# polygon(x = c(215, 215, max.date, max.date), y = c(5.75, 6.25, 6.25, 5.75), col = "black", border = "grey80")
# text(x = 222, y = 6, "Ewe Died", col = "white")
# polygon(x = c(208, 208, max.date, max.date), y = c(7.75, 8.25, 8.25, 7.75), col = "black", border = "grey80")
# text(x = 215, y = 8, "Ewe Died", col = "white")
# mtext(side = 2, line = 0, outer = T, "Black Butte Ewe ID")
# 
# # ewe symptom points
# points(as.numeric(factor(bb2014$EWEID)) ~ as.numeric(as.character(bb2014$JulianDate)), pch = 16, col = health.color2[bb2014$Ewe.Health], cex = health.size[bb2014$Ewe.Health])

#------------------------------------------------------#
#-- Bayesian hazards for ewes and lambs over summer ---#
#------------------------------------------------------#
#require(survival)
require(BayHaz)
#levels(bb2014$EWEID)
#lamb.surv.dat <- adult.surv.dat <- matrix(NA, nrow = length(levels(bb2014$EWEID)), ncol = 70)

# need tabulation of number at risk and number events at each death date
ad.time1 <- rep(120, 13)
ad.time2 <- c(137, 178, 197, 215, 208, 215, rep(273, 7))
ad.event <- c(rep(1, 6), rep(0, 7))
#ad.surv <- Surv(ad.time1, ad.time2, ad.event)
ad.data <- as.data.frame(cbind(ad.time1, ad.time2, ad.event))

lamb.time1 <- c(127, 132, 132, 125, 125, 133, 120, 131, 125, 123, 125)
lamb.time2 <- c(130, 184, 178, 176, 164, 169, 179, 184, 174, 146, 164)
lamb.event <- rep(1, length(lamb.time1))
lamb.data <- as.data.frame(cbind(lamb.time1, lamb.time2, lamb.event))

# tot.time1 <- c(ad.time1, lamb.time1)
# tot.time2 <- c(ad.time2, lamb.time2)
# tot.event <- c(ad.event, lamb.event)
# ad.lamb.ind <- c(rep("adult", length(ad.time1)), rep("lamb", length(lamb.time1)))
# bb14.data <- as.data.frame(cbind(tot.time1, tot.time2, tot.event, ad.lamb.ind))
# bb14.data$tot.time1 <- as.numeric(as.character(bb14.data$tot.time1))
# bb14.data$tot.time2 <- as.numeric(as.character(bb14.data$tot.time2))
# bb14.data$tot.event <- as.numeric(as.character(bb14.data$tot.event))
# 
# par(mfrow = c(1, 1), oma = c(1, 0, 0, 0))
# tot.survfit <- survfit(Surv(tot.time1, tot.time2, tot.event) ~ ad.lamb.ind)
# plot(tot.survfit, conf.int = T, xlim = c(120, 220), lwd = 2, yaxt = "n", col = c("red", "black"), xaxt = "n", ylab = "Proportion surviving", xlab = "")
# axis(side = 2, at = seq(0, 1, length.out = 6), labels = seq(0, 1, length.out = 6), cex.axis = .8, las = 2)
# axis(side = 1, at = c(120, 151, 181, 212), labels = c("May 01", "June 01", "July 01", "August 01"), cex.axis = .8, las = 2)
# leg.text <- c("adults", "lambs")
# legend("bottomleft", leg.text, col = c("red", "black"), lwd = c(2, 2), lty = c(1, 1), bty = "n", cex = .8)

hypars.ad <- CPPpriorElicit(r0 = 0.001, H = 1, T00 = 153, M00 = 2, extra = 0)
CPPplotHR(CPPpriorSample(ss = 10, hyp = hypars.ad), tu = "Year")
# generate a posterior sample
ad.post <- CPPpostSample(hypars.ad, times = (ad.data$ad.time2 - 120), obs = ad.data$ad.event)
# check that no additional CPP jumps are needed:
# if this probability is not negligible,
# go back to prior selection stage and increase 'extra'
ecdf(ad.post$sgm[ , ad.post$hyp$F])(ad.post$hyp$T00 + 3 * ad.post$hyp$sd)
# plot some posterior hazard rate summaries

hypars.lamb <- CPPpriorElicit(r0 = 0.01, H = 1, T00 = 130, M00 = 3, extra = 0)
CPPplotHR(CPPpriorSample(ss = 10, hyp = hypars.lamb), tu = "Year")
# generate a posterior sample
lamb.post <- CPPpostSample(hypars.lamb, times = (lamb.data$lamb.time2 - 120), obs = lamb.data$lamb.event)
# check that no additional CPP jumps are needed:
# if this probability is not negligible,
# go back to prior selection stage and increase 'extra'
ecdf(lamb.post$sgm[ , lamb.post$hyp$F])(lamb.post$hyp$T00 + 3 * lamb.post$hyp$sd)
# plot some posterior hazard rate summaries

source("~/work/Kezia/Research/EcologyPapers/BB14Outbreak/Code/KRMPlotsBayHaz_Source_05Dec2014.R")
par(mfrow = c(1, 1), mar = c(3, 3, 3, 3))
krmCPPplotHR(ad.post , tu = "Day", title = "")
CPPplotHR(lamb.post , tu = "Day", title = "")
krmCPPplotHR2samps(ad.post, lamb.post, tu = "Day", title = "")

#--------------------------------------------------#
#-- Figure 5 code for ELISAs and qPCRs ------------#
#--------------------------------------------------#
elisa.data <- read.csv("~/work/Kezia/Research/EcologyPapers/BB14Outbreak/Data/BHSLongitudinal_ELISA_qPCR_23Sept2014.csv", header = T)
elisa <- subset(elisa.data, Herd == "Black Butte")

par(mar = c(4, 6, 1, 1), oma = c(0, 0, 0, 0))
plot(as.numeric(elisa$Movipneumonia.ELISA) ~ as.numeric(factor(elisa$CaptureDate)), type = "p", ylim = c(0, 100), pch = 16, xaxt = "n", ylab = "Titer", xlab = "", xlim = c(0.5, 3.5))
axis(side = 1, outer = F, at = c(1, 2, 3), labels = c("February 27th", "July 3rd", "October 8th"))


#-- BLACK BUTTE --#
bb <- read.csv("~/work/Kezia/Research/FieldStudies/HCBighornContactStudies/Data/RelocationData/BlackButteLocationsFinal_20July2014.csv", header = T, sep = "\t")

bb$UTMN <- bb$Longitude
bb$UTME <- bb$Latitude
bb$DATE <- bb$Date

#-- pull out Uncollared nonunicorn --#
bb <- subset(bb, !(EWEID == ""))
bb$EWEID <- factor(bb$EWEID)
levels(bb$EWEID)
table(bb$EWEID)

#-- 2014 stripplot --#
bb2014 <- subset(bb, Year == 2014)
bb2014$EWEID <- factor(bb2014$EWEID)


# health.color <- c("orange", "white", "red", "white", "white", "yellow")
# lamb.health.color <- c(rep("orange", 7), "black", rep("darkorange", 4), "white", "red", rep("white", 3), rep("yellow", 3))
# 
# health.color2 <- c("red", "white", "red", "white", "white", "red")
# lamb.health.color2 <- c("white", "orange", "black")
# 
# par(mfrow = c(1, 1), oma = c(1, 1, 0, 0), mar = c(4, 6, 1, 1))
# plot(as.numeric(factor(bb2014$EWEID)) ~ as.numeric(as.character(bb2014$JulianDate)), cex = 1.75, pch = 16, col = health.color2[bb2014$Ewe.Health], xlab = "Julian Date", yaxt = "n", ylab = "", xlim = c(121, 191), xaxt = "n")
# #points(as.numeric(factor(bb2014$EWEID)) ~ as.numeric(as.character(bb2014$JulianDate)), cex = ifelse(bb2014$Symptomatic.Lamb == 2, 1, bb2014$Symptomatic.Lamb), pch = 16, col = lamb.health.color2[as.numeric(bb2014$Symptomatic.Lamb) + 1])
# axis(side = 2, line = 1, outer = F, las = 1, at = seq(1:12), labels = levels(factor(bb2014$EWEID)))
# axis(side = 1, line = 0, outer = F, las = 1, at = c(120, 151, 181), labels = c("May 01", "June 01", "July 01"))
# polygon(x = c(137, 137, 191, 191), y = c(8.75, 9.25, 9.25, 8.75), col = "grey80", border = "grey80")
# text(x = 141, y = 9, "Ewe Died")
# polygon(x = c(178, 178, 191, 191), y = c(10.75, 11.25, 11.25, 10.75), col = "grey80", border = "grey80")
# text(x = 182, y = 11, "Ewe Died")
# mtext(side = 2, line = 0, outer = T, "Black Butte Ewe ID")
# polygon(x = c(120, 120, 137, 137), y = c(.75, 1.25, 1.25, .75), col = "grey80", border = "grey80")
# text(x = 125, y = 1, "Pre-lambing")
# polygon(x = c(198, 198, 200, 200), y = c(.75, 1.25, 1.25, .75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 132, 132), y = c(2.75, 3.25, 3.25, 2.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 132, 132), y = c(3.75, 4.25, 4.25, 3.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 125, 125), y = c(4.75, 5.25, 5.25, 4.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 125, 125), y = c(4.75, 5.25, 5.25, 4.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 133, 133), y = c(5.75, 6.25, 6.25, 5.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 131, 131), y = c(7.75, 8.25, 8.25, 7.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 125, 125), y = c(9.75, 10.25, 10.25, 9.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 123, 123), y = c(10.75, 11.25, 11.25, 10.75), col = "grey80", border = "grey80")
# polygon(x = c(120, 120, 125, 125), y = c(11.75, 12.25, 12.25, 11.75), col = "grey80", border = "grey80")

#----------------------------------------#
#-- moving average clinical signs plot --#
#----------------------------------------#
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


aggregate.clin.signs <- apply(mov.avg.mat, 2, sum)

par(mfrow = c(1, 1), oma = c(2, 0, 1, 4))
plot(mov.avg.mat[1, ] ~ seq(124, 192), type = "l", ylim = c(0, 1.75), yaxt = "n", xlab = "", ylab = "7-day individual clinical signs", xaxt = "n", xlim = c(120, 185))
for(i in 2: dim(mov.avg.mat)[1]){
  lines(mov.avg.mat[i, ] ~ seq(124, 192), type = "l", col = "grey40", lty = i)
}
axis(side = 2, at= c(0, .5, 1.0, 1.5), labels = c("0.0", ".5", "1.0", "1.5"), cex.axis = .8, las = 2)
axis(side = 1, at = c(120, 151, 181), labels = c("May 01", "June 01", "July 01"), cex.axis = .8, las = 2)
mtext(side = 1, line = 3.5, "Date")

par(new = T)
plot(aggregate.clin.signs ~ seq(124, 192), lwd = 2, col = "black", type = "l", yaxt = "n", ylab = "", xlab = "", xaxt = "n", xlim = c(120, 185))
axis(side = 4, at = seq(0, 6), label = seq(0, 6))
mtext(side = 4, outer = T, line = 1, "7-day aggregated clinical signs", adj = .7)
leg.text <- c("Specific ewes", "Population aggregate")
legend("topleft", leg.text, col = c("grey40", "black"), lwd = c(1, 2), lty = c(2, 1), bty = "n", cex = .8)
