# DHIMS data analysis
# Christopher Boyer
# 23 Nov 2014
library(lattice)
# load district data sets: Bawku
Bawku <- read.csv("~/Desktop/Bawku.csv")
Bawku$District <- "Bawku"
Bawku$"ANC.attendance" <- Bawku$ANC.registrants
Bawku$ANC.registrants <- NULL

# load district data sets: Bongo
Bongo <- read.csv("~/Desktop/Bongo.csv")
Bongo$District <- "Bongo"

# load district data sets: Builsa North
BuilsaN <- read.csv("~/Desktop/BuilsaN.csv")
BuilsaN$District <- "BuilsaN"

# load district data sets: Builsa South
BuilsaS <- read.csv("~/Desktop/BuilsaS.csv")
BuilsaS$District <- "BuilsaS"

# load district data sets: Nabdam
Nabdam <- read.csv("~/Desktop/Nabdam.csv")
Nabdam$District <- "Nabdam"

# load district data sets: Talensi
Talensi <- read.csv("~/Desktop/Talensi.csv")
Talensi$District <- "Talensi"

# combine district data sets
DHIMStotal <- rbind(Bawku,Bongo,BuilsaN,BuilsaS,Nabdam,Talensi)

# create R date-time variable, sort by date
DHIMStotal$date <- as.Date(paste("1", as.character(DHIMStotal$Period), sep=" "), format = "%d %B %Y")
DHIMStotal <- DHIMStotal[order(DHIMStotal$date),]

DHIMStotal[ DHIMStotal$Organisation.unit=="Zebilla Hospital" & DHIMStotal$date < as.Date("1 May 2009",format = "%d %B %Y"),3:5] <- DHIMStotal[DHIMStotal$Organisation.unit=="Zebilla Rch" & DHIMStotal$date < as.Date("1 May 2009",format = "%d %B %Y"),3:5]
DHIMStotal <- DHIMStotal[ DHIMStotal$Organisation.unit!="Zebilla Rch",]
DHIMStotal$csrate <- DHIMStotal$Caesarean.section.deliveries/DHIMStotal$Mothers..all.deliveries
           #------------------------------#
#----------| subset monthly birth tallies |----------#
           #------------------------------#

AllBirths <- data.frame(
  date = DHIMStotal$date,
  facility = DHIMStotal$Organisation.unit,
  district = DHIMStotal$District,
  births = DHIMStotal$Mothers..all.deliveries
)

# remove facilities with less than 5 total births
TotalsList <- aggregate(AllBirths$births,by=list(cat=AllBirths$facility), FUN=sum, na.rm=T)
RemoveList <- TotalsList[TotalsList$x <= 15, ]
AllBirths <- AllBirths[!AllBirths$facility %in% RemoveList$cat,]

# add time-varying SERC covariate
AllBirths$SERC <- 0
AllBirths$SERC[which(AllBirths$district%in%c("Bongo", "BuilsaN", "BuilsaS") & AllBirths$date >= as.Date("1 August 2013", format = "%d %B %Y"))] <- 1
#AllBirths[is.na(AllBirths)] <- 0

# add treatment/control indicator
AllBirths$treatment <- "treatment"
AllBirths$treatment[!AllBirths$district%in%c("Bongo", "BuilsaN", "BuilsaS")] <- "control"

# build facility level indicator
AllBirths$level <- "Health Centre/Clinic"
AllBirths$level[grep("Hospital", AllBirths$facility)] <- "Hospital"
AllBirths$level[grep("CHPS", AllBirths$facility)] <- "CHPS"

# add birth lag and integer time variables
AllBirths$time <- rep(seq(1:81),each=59)
AllBirths$births_lag <- c(rep(NA,59), AllBirths$births[1:4720])
AllBirths$births_lag12 <- c(rep(NA,708), AllBirths$births[1:4071])


           #---------------------------------#
#----------| subset monthly csection tallies |----------#
           #---------------------------------#

AllCS <- data.frame(
  date = DHIMStotal$date,
  facility = DHIMStotal$Organisation.unit,
  district = DHIMStotal$District,
  csections = DHIMStotal$Caesarean.section.deliveries
)

# remove facilities that have performed less than 5 total csections
TotalsList <- aggregate(AllCS$csections,by=list(cat=AllCS$facility), FUN=sum, na.rm=T)
RemoveList <- TotalsList[TotalsList$x <= 5, ]
AllCS <- AllCS[!AllCS$facility %in% RemoveList$cat,]

# add time-varying SERC covariate
AllCS$SERC <- 0
AllCS$SERC[which(AllCS$district%in%c("Bongo", "BuilsaN", "BuilsaS") & AllCS$date >= as.Date("1 August 2013", format = "%d %B %Y"))] <- 1
AllCS[is.na(AllCS)] <- 0

# add treatment/control indicator
AllCS$treatment <- "treatment"
AllCS$treatment[!AllCS$district%in%c("Bongo", "BuilsaN", "BuilsaS")] <- "control"

# add birth lag and integer time variables
AllCS$time <- rep(seq(1:81),each=3)
AllCS$csection_lag <- c(rep(NA,3), AllDeaths$deaths[1:240])

           #-------------------------------#
#----------| subset monthly csection rates |----------#
           #-------------------------------#

AllCSRate <- data.frame(
  date = DHIMStotal$date,
  facility = DHIMStotal$Organisation.unit,
  district = DHIMStotal$District,
  csrate = DHIMStotal$csrate,
  births = DHIMStotal$Mothers..all.deliveries,
  csections = DHIMStotal$Caesarean.section.deliveries
)

# remove facilities that have performed less than 5 total csections
TotalsList <- aggregate(AllCSRate$csrate,by=list(cat=AllCSRate$facility), FUN=sum, na.rm=T)
RemoveList <- TotalsList[TotalsList$x <= 5, ]
AllCSRate <- AllCSRate[!AllCSRate$facility %in% RemoveList$cat,]

# add time-varying SERC covariate
AllCSRate$SERC <- 0
AllCSRate$SERC[which(AllCSRate$district%in%c("Bongo", "BuilsaN", "BuilsaS") & AllCSRate$date >= as.Date("1 August 2013", format = "%d %B %Y"))] <- 1
AllCSRate[is.na(AllCSRate)] <- 0

# add treatment/control indicator
AllCSRate$treatment <- "treatment"
AllCSRate$treatment[!AllCSRate$district%in%c("Bongo", "BuilsaN", "BuilsaS")] <- "control"

# add birth lag and integer time variables
AllCSRate$time <- rep(seq(1:81),each=3)
AllCSRate$csection_lag <- c(rep(NA,3), AllDeaths$deaths[1:240])
           #---------------------------------------#
#----------| subset monthly maternal death tallies |----------#
           #---------------------------------------#

AllDeaths <- data.frame(
  date = DHIMStotal$date,
  facility = DHIMStotal$Organisation.unit,
  district = DHIMStotal$District,
  deaths = DHIMStotal$Total.maternal.deaths,
  births = DHIMStotal$Mothers..all.deliveries
)

# remove facilities that have performed less than 5 total deaths
TotalsList <- aggregate(AllDeaths$deaths,by=list(cat=AllDeaths$facility), FUN=sum, na.rm=T)
RemoveList <- TotalsList[TotalsList$x <= 5, ]
AllDeaths <- AllDeaths[!AllDeaths$facility %in% RemoveList$cat,]

# add time-varying SERC covariate
AllDeaths$SERC <- 0
AllDeaths$SERC[which(AllDeaths$district%in%c("Bongo", "BuilsaN", "BuilsaS") & AllDeaths$date >= as.Date("1 August 2013", format = "%d %B %Y"))] <- 1
AllDeaths[is.na(AllDeaths)] <- 0

# add treatment/control indicator
AllDeaths$treatment <- "treatment"
AllDeaths$treatment[!AllDeaths$district%in%c("Bongo", "BuilsaN", "BuilsaS")] <- "control"

# add birth lag and integer time variables
AllDeaths$time <- rep(seq(1:81),each=5)
AllDeaths$deaths_lag <- c(rep(NA,5), AllDeaths$deaths[1:400])
AllDeaths$deaths[AllDeaths$deaths >5] <- 0
AllDeaths$policy <- 0
AllDeaths$policy[AllDeaths$date >= as.Date("1 August 2013", format = "%d %B %Y")] <- 1
           #------------------------------------#
#----------| make descriptive plots of the data |----------#
           #------------------------------------#
# black and white theme No. 1
bw.theme <- trellis.par.get()
bw.theme$strip.background$col <- "grey80"
bw.theme$superpose.line$lty <- c(1,2,1,2,1,2)
bw.theme$superpose.line$col <- c(1,2,1,2,1,2)
bw.theme$superpose.symbol$pch <- c(15,1,10,9,8,7)
bw.theme$superpose.symbol$col <- c(1,2,1,2,1,2)

# black and white theme No. 2
bw2.theme <- trellis.par.get()
bw2.theme$box.dot$pch <- "|"
bw2.theme$box.rectangle$col <- "black"
bw2.theme$box.rectangle$lwd <- 2
bw2.theme$box.rectangle$fill <- clrs.hcl(2)
bw2.theme$box.umbrella$lty <- 1
bw2.theme$box.umbrella$col <- "black"
bw2.theme$plot.symbol$col <- "grey40"
bw2.theme$plot.symbol$pch <- 1
bw2.theme$plot.symbol$cex <- 1
bw2.theme$strip.background$col <- c("grey80","darkgray")

# Births #
Levels <- aggregate(AllBirths$births,by=list(cat=AllBirths$level, date=AllBirths$date, SERC=AllBirths$SERC, txgrp=AllBirths$treatment), FUN=sum, na.rm=T)
Levels <- Levels[order(Levels$date),]
Levels$time <- "pre"
Levels$time[Levels$date >= as.Date("1 August 2013",format="%d %B %Y")] <- "post"
Levels$time <- relevel(factor(Levels$time),ref="pre")

Totals <- aggregate(Levels$x, by=list(date=Levels$date, SERC=Levels$SERC, time=Levels$time, txgrp=Levels$txgrp ), FUN=sum, na.rm=T)

# time series plot for births: intervention vs. control
png(filename="~/Dropbox/ARCHeS/CU Practicum Ghana/2014/Christopher Boyer/SERC Evaluation/births1.png",width=700,height=600)
xyplot(x~date|cat,
               groups=txgrp,
               data=Levels,
               xlab.top=list(label="DHIMS monthly reported births",cex=1.5,font=2),
               ylab="Births",
               xlab="Date",
               layout=c(1,3),
               type=c("b"),
               par.settings=bw.theme,
                key=list(text=list(c("Control", "Intervention")),
                         lines=list(pch=c(15,1),lty=1:2, col=1:2, type=c("b")),
                         corner=c(0.05,0.95)),
               panel = function(x,y,...) {
                 panel.xyplot(x,y,...)
                 panel.xblocks(x,x >= as.Date("1 August 2013",format="%d %B %Y"), col = "#6CCFF6", alpha = 0.2)
                 panel.abline(v=as.Date("1 August 2013",format="%d %B %Y"), lty = "dashed", col = "grey60",lwd=2)
                 if(packet.number()==3){ 
                   panel.text(as.Date("1 February 2014",format="%d %B %Y"),350,c("SERC"),font=3)}
               })
dev.off()
# box plot pre and post intervention
png(filename="~/Dropbox/ARCHeS/CU Practicum Ghana/2014/Christopher Boyer/SERC Evaluation/births2.png",width=700,height=600)
bwplot(x~time|txgrp,data=Totals,
       horizontal=F,
       par.settings=bw2.theme,
       ylab="Births",
       xlab="Intervention period",
       xlab.top=list(label="DHIMS mean monthly reported births",cex=1.5,font=2)
)
dev.off()
       
# box plot pre and post intervetion by level
png(filename="~/Dropbox/ARCHeS/CU Practicum Ghana/2014/Christopher Boyer/SERC Evaluation/births3.png",width=700,height=600)
bwplot(x~time|txgrp+cat,data=Levels,
       horizontal=F,
       par.settings=bw2.theme,
       ylab="Births",
       xlab="Intervention period",
       xlab.top=list(label="DHIMS mean monthly reported births by facility level",cex=1.5,font=2)       
)
dev.off()


# C-sections #
CSLevels <- aggregate(AllCS$csections,by=list(date=AllCS$date, SERC=AllCS$SERC, txgrp=AllCS$treatment), FUN=sum, na.rm=T)
CSLevels <- CSLevels[order(CSLevels$date),]
CSLevels$time <- "pre"
CSLevels$time[CSLevels$date >= as.Date("1 August 2013",format="%d %B %Y")] <- "post"
CSLevels$time <- relevel(factor(CSLevels$time),ref="pre")

Totals <- aggregate(Levels$x, by=list(date=Levels$date, SERC=Levels$SERC, time=Levels$time, txgrp=Levels$txgrp ), FUN=sum, na.rm=T)

# time series plot for c-sections: intervention vs. control
png(filename="~/Dropbox/ARCHeS/CU Practicum Ghana/2014/Christopher Boyer/SERC Evaluation/csections1.png",width=700,height=600)
xyplot(x~date,
       groups=txgrp,
       data=CSLevels,
       xlab.top=list(label="DHIMS monthly reported C/S totals",cex=1.5,font=2),
       ylab="C-sections",
       xlab="Date",
       type=c("b"),
       par.settings=bw.theme,
       key=list(text=list(c("Control", "Intervention")),
                lines=list(pch=c(15,1),lty=1:2, col=1:2, type=c("b")),
                corner=c(0.05,0.95)),
       panel = function(x,y,...) {
         panel.xyplot(x,y,...)
         panel.xblocks(x,x >= as.Date("1 August 2013",format="%d %B %Y"), col = "#6CCFF6", alpha = 0.2)
         panel.abline(v=as.Date("1 August 2013",format="%d %B %Y"), lty = "dashed", col = "grey60",lwd=2)
         panel.text(as.Date("1 February 2014",format="%d %B %Y"),39,c("SERC"),font=3)
       })
dev.off()



png(filename="~/Dropbox/ARCHeS/CU Practicum Ghana/2014/Christopher Boyer/SERC Evaluation/csections2.png",width=700,height=600)
bwplot(x~time|txgrp,data=CSLevels,
       horizontal=F,
       par.settings=bw2.theme,
       ylab="C-sections",
       xlab="Intervention period",
       xlab.top=list(label="DHIMS mean monthly reported C/S",cex=1.5,font=2)       
)
dev.off()


# C-section rates #
CSRLevels <- aggregate(list(csections=AllCSRate$csections, births=AllCSRate$births),by=list(date=AllCSRate$date, SERC=AllCSRate$SERC, txgrp=AllCSRate$treatment), FUN=sum, na.rm=T)
CSRLevels <- CSRLevels[order(CSRLevels$date),]
CSRLevels$time <- "pre"
CSRLevels$time[CSRLevels$date >= as.Date("1 August 2013",format="%d %B %Y")] <- "post"
CSRLevels$time <- relevel(factor(CSRLevels$time),ref="pre")
CSRLevels$csrate <- CSRLevels$csections/CSRLevels$births

Totals <- aggregate(Levels$x, by=list(date=Levels$date, SERC=Levels$SERC, time=Levels$time, txgrp=Levels$txgrp ), FUN=sum, na.rm=T)

# time series plot for c-sections: intervention vs. control
png(filename="~/Dropbox/ARCHeS/CU Practicum Ghana/2014/Christopher Boyer/SERC Evaluation/csrate1.png",width=700,height=600)
xyplot(csrate~date,
       groups=txgrp,
       data=CSRLevels,
       xlab.top=list(label="DHIMS calculated monthly C/S rate",cex=1.5,font=2),
       ylab="C-section rate",
       xlab="Date",
       type=c("b"),
       par.settings=bw.theme,
       key=list(text=list(c("Control", "Intervention")),
                lines=list(pch=c(15,1),lty=1:2, col=1:2, type=c("b")),
                corner=c(0.05,0.95)),
       panel = function(x,y,...) {
         panel.xyplot(x,y,...)
         panel.xblocks(x,x >= as.Date("1 August 2013",format="%d %B %Y"), col = "#6CCFF6", alpha = 0.2)
         panel.abline(v=as.Date("1 August 2013",format="%d %B %Y"), lty = "dashed", col = "grey60",lwd=2)
         panel.text(as.Date("1 February 2014",format="%d %B %Y"),0.28,c("SERC"),font=3)
       })
dev.off()

png(filename="~/Dropbox/ARCHeS/CU Practicum Ghana/2014/Christopher Boyer/SERC Evaluation/csrate2.png",width=700,height=600)
bwplot(csrate~time|txgrp,data=CSRLevels,
       horizontal=F,
       par.settings=bw2.theme,
       ylab="C-section rate",
       xlab="Intervention period",
       xlab.top=list(label="DHIMS mean monthly reported C/S rate",cex=1.5,font=2)       
)
dev.off()

# Deaths #
DeathLevels <- aggregate(AllDeaths$deaths,by=list(date=AllDeaths$date, SERC=AllDeaths$SERC, txgrp=AllDeaths$treatment), FUN=sum, na.rm=T)
DeathLevels <- DeathLevels[order(DeathLevels$date),]
DeathLevels$time <- "pre"
DeathLevels$time[DeathLevels$date >= as.Date("1 August 2013",format="%d %B %Y")] <- "post"
DeathLevels$time <- relevel(factor(DeathLevels$time),ref="pre")

# time series plot for deaths: intervention vs. control
png(filename="~/Dropbox/ARCHeS/CU Practicum Ghana/2014/Christopher Boyer/SERC Evaluation/deaths1.png",width=700,height=600)
xyplot(x~date,
       groups=txgrp,
       data=DeathLevels,
       xlab.top=list(label="DHIMS monthly reported maternal deaths",cex=1.5,font=2),
       ylab="C-sections",
       xlab="Date",
       type=c("h","p"),
       par.settings=bw.theme,
       key=list(text=list(c("Control", "Intervention")),
                lines=list(pch=c(15,1),lty=1:2, col=1:2, type=c("p")),
                corner=c(0.05,0.95)),
       panel = function(x,y,...) {
         panel.xyplot(x,y,...)
         panel.xblocks(x,x >= as.Date("1 August 2013",format="%d %B %Y"), col = "#6CCFF6", alpha = 0.2)
         panel.abline(v=as.Date("1 August 2013",format="%d %B %Y"), lty = "dashed", col = "grey60",lwd=2)
         panel.text(as.Date("1 February 2014",format="%d %B %Y"),350,c("SERC"),font=3)
       })
dev.off()
DeathLevels.NoOutliers <- DeathLevels
DeathLevels.NoOutliers$x[DeathLevels.NoOutliers$x >= 8] <- NA

# time series plot for deaths: intervention vs. control (no outliers)
png(filename="~/Dropbox/ARCHeS/CU Practicum Ghana/2014/Christopher Boyer/SERC Evaluation/deaths2.png",width=700,height=600)
xyplot(x~date|txgrp,
       groups=txgrp,
       data=DeathLevels.NoOutliers,
       xlab.top=list(label="DHIMS monthly reported maternal deaths",cex=1.5,font=2),
       ylab="Deaths",
       xlab="Date",
       type=c("h","p"),
       par.settings=bw.theme,
       key=list(text=list(c("Control", "Intervention")),
                lines=list(pch=c(15,1),lty=1:2, col=1:2, type=c("p")),
                corner=c(0.05,0.95)),
       panel = function(x,y,...) {
         panel.xyplot(x,y,...)
         panel.xblocks(x,x >= as.Date("1 August 2013",format="%d %B %Y"), col = "#6CCFF6", alpha = 0.2)
         panel.abline(v=as.Date("1 August 2013",format="%d %B %Y"), lty = "dashed", col = "grey60",lwd=2)
         panel.text(as.Date("1 February 2014",format="%d %B %Y"),3,c("SERC"),font=3)
       })
dev.off()

           #----------------------------------#
#----------| Regression Analysis of the data  |----------#
           #----------------------------------#


births.lmer <- lmer(births ~ time + births_lag + SERC*level + (1 + level | treatment), data=AllBirths,REML=F)
births.lme <- lme(births ~ births_lag + SERC*level, random=list((~ 1| facility)), data=AllBirths, method="ML", correlation=corAR1(form= ~ 1 | facility), na.action="na.omit")
summary(births.lmer)

df <- unstack(AllBirths,births~factor(facility))
df$SERC <- c(rep(0,67), rep(1,14))
births.lm <- lm(cbind(Sandema.Hospital,Bongo.Hospital))

# based on time series plots, need to check for autocorrelation
births.lm <- lm(formula=x~date, data=Totals[Totals$txgrp=="treatment",])
dwtest(births.lm,alternative="two.sided")
xyplot(x~date,data=Totals[Totals$txgrp=="treatment",],type=c("b"),panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.lines(x, fitted(births.lm), col.line = "black")
},)

auto.arima(Totals$x[Totals$txgrp=="treatment"], xreg=Totals$SERC[Totals$txgrp=="treatment"])




# hockey stick #
AllBirths$SERCd <- AllBirths$SERC*(AllBirths$time-67)
births.lm <- lm(births~ time + SERCd * level, data=AllBirths)


xyplot(sunspot.year, panel = panel.xyarea, origin = 0,
       aspect = "xy", cut = list(n = 3, overlap = 0))
## two series superposed: one filled, one as a line.
xyplot(ts.union(data = sunspot.year, lag10 = lag(sunspot.year, 10)),
       aspect = "xy", cut = list(n = 3, overlap = 0),
       superpose = TRUE,
       panel = panel.superpose,
       panel.groups = function(..., group.number) {
         if (group.number == 1)
           panel.xyarea(...) else panel.xyplot(...)
       }, border = NA,
       par.settings = simpleTheme(col = c("grey", "black"), lwd = c(5,2)))

xyplot(births~time, data=AllBirths, 
       panel = function(x, y, ...) {
          panel.xyplot(x, y, ...)
          panel.lines(x, fitted(births.lm), col.line = "black")
})

library('foreign')
AllBirths3 <- AllBirths[AllBirths$level=="Health Centre/Clinic",]
write.dta(AllDeaths,"~/Desktop/AllDeaths.dta")


test <- aggregate(births ~ district + date, AllBirths, FUN=sum, na.rm=T)
test <- test[test$district%in%c("Bawku","Bongo","BuilsaN", "BuilsaS"),]
test <- aggregate(births ~ district + date, test, FUN=sum, na.rm=T)