# DHIMS data analysis
# Christopher Boyer
# 23 Nov 2014

# set working directory
setwd("~/Dropbox/ARCHeS/CU Practicum Ghana/2014/Christopher Boyer/SERC Evaluation/")
library(lattice)
library(ggplot2)
library(dplyr)
library(tidyr)
library(geepack)
library(splines)
library(lme4)

# load district data sets: Bawku
BawkuM <- read.csv("./data/BawkuMunicipal.csv")
BawkuM$District <- "Bawku Municipal"
BawkuM$X <- NULL

# load district data sets: Bawku West
BawkuW <- read.csv("./data/BawkuWest.csv")
BawkuW$District <- "Bawku West"

# load district data sets: Binduri
Binduri <- read.csv("./data/Binduri.csv")
Binduri$District <- "Binduri"
Binduri$X <- NULL

# load district data sets: Bolgatanga
Bolgatanga <- read.csv("./data/Bolgatanga.csv")
Bolgatanga$District <- "Bolgatanga"

# load district data sets: Bongo
Bongo <- read.csv("./data/Bongo.csv")
Bongo$District <- "Bongo"

# load district data sets: Builsa North
BuilsaN <- read.csv("./data/BuilsaNorth.csv")
BuilsaN$District <- "Builsa North"

# load district data sets: Builsa South
BuilsaS <- read.csv("./data/BuilsaSouth.csv")
BuilsaS$District <- "Builsa South"

# load district data sets: Garu Tempane
GaruTempane <- read.csv("./data/GaruTempane.csv")
GaruTempane$District <- "Garu Tempane"
GaruTempane <- GaruTempane %>% filter(Period != "")

# load district data sets: Kassena Nankana West
KNW <- read.csv("./data/KNW.csv")
KNW$District <- "Kassena Nankana West"

# load district data sets: Kassena Nankana Municipal
KNM <- read.csv("./data/KNM.csv")
KNM$District <- "Kassena Nankana Municipal"

# load district data sets: Nabdam
Nabdam <- read.csv("./data/Nabdam.csv")
Nabdam$District <- "Nabdam"

# load district data sets: Pusiga
Pusiga <- read.csv("./data/Pusiga.csv")
Pusiga$District <- "Pusiga"
Pusiga$X <- NULL

# load district data sets: Talensi
Talensi <- read.csv("./data/Talensi.csv")
Talensi$District <- "Talensi"

# combine district data sets
DHIMStotal <- rbind(BawkuM, BawkuW, Binduri, Bolgatanga, Bongo, BuilsaN, BuilsaS, GaruTempane, KNW, KNM, Nabdam, Pusiga, Talensi)

# create R date-time variable, sort by date
DHIMStotal$Region <- "Upper East"

# load district data sets: Daffiama
Daffiama <- read.csv("./data/Daffiama.csv")
Daffiama$District <- "Daffiama"
Daffiama$X <- NULL

# load district data sets: Jirapa
Jirapa <- read.csv("./data/Jirapa.csv")
Jirapa$District <- "Jirapa"
Jirapa$X <- NULL

# load district data sets: Lambussie
Lambussie <- read.csv("./data/Lambussie.csv")
Lambussie$District <- "Lambussie"

# load district data sets: Lawra
Lawra <- read.csv("./data/Lawra.csv")
Lawra$District <- "Lawra"

# load district data sets: Nadowlie
Nadowlie <- read.csv("./data/Nadowlie.csv")
Nadowlie$District <- "Nadowlie"

# load district data sets: Nandom
Nandom <- read.csv("./data/Nandom.csv")
Nandom$District <- "Nandom"

# load district data sets: Sissala East
SissalaE <- read.csv("./data/Sissala East.csv")
SissalaE$District <- "Sissala East"

# load district data sets: Sissala West
SissalaW <- read.csv("./data/Sissala West.csv")
SissalaW$District <- "Sissala West"

# load district data sets: Wa East
WaE <- read.csv("./data/Wa East.csv")
WaE$District <- "Wa East"
WaE$X <- NULL
WaE$X.1 <- NULL
WaE$X.2 <- NULL
WaE$X.3 <- NULL
WaE$X.4 <- NULL

# load district data sets: Wa Municipal
WaM <- read.csv("./data/Wa Mun.csv")
WaM$District <- "Wa Municipal"
WaM$X <- NULL

# load district data sets: Wa West
WaW <- read.csv("./data/Wa West.csv")
WaW$District <- "Wa West"

# combine district data sets
DHIMStotal2 <- rbind(Daffiama, Jirapa, Lambussie, Lawra, Nadowlie, Nandom, SissalaE, SissalaW, WaE, WaM, WaW)

# create R date-time variable, sort by date
DHIMStotal2$Region <- "Upper West"

DHIMStotal <- rbind(DHIMStotal, DHIMStotal2)

# create R date-time variable, sort by date
DHIMStotal$date <- as.Date(paste("1", as.character(DHIMStotal$Period), sep=" "), format = "%d %B %Y")
DHIMStotal <- DHIMStotal[order(DHIMStotal$date),]

# create facility variable
DHIMStotal$facility <- DHIMStotal$Organisation.unit 
DHIMStotal$Organisation.unit <- NULL

# create level variable
DHIMStotal$level <- "Health Centre/Clinic"
DHIMStotal$level[grep("Hospital", DHIMStotal$facility)] <- "District Hospital"
DHIMStotal$level[grep("CHPS", DHIMStotal$facility)] <- "CHPS"
DHIMStotal$level[grep("Regional Hospital", DHIMStotal$facility)] <- "Regional Hospital"

# create variables of interest
DHIMStotal$births <- DHIMStotal$Mothers..all.deliveries
DHIMStotal$csections <- DHIMStotal$Caesarean.section.deliveries
DHIMStotal$csrate <- DHIMStotal$csections/DHIMStotal$births
DHIMStotal$deaths <- DHIMStotal$Total.maternal.deaths
DHIMStotal$month <- months(DHIMStotal$date)

# create intervention/control variable
DHIMStotal$SERC <- ifelse(DHIMStotal$District %in% c("Builsa North", "Bongo", "Builsa South") | 
                          DHIMStotal$facility == "Regional Hospital, Bolgatanga", "SERC", "control")
DHIMStotal$time <- floor(as.numeric(DHIMStotal$date - min(DHIMStotal$date))/30)
DHIMStotal$intervention <- ifelse(DHIMStotal$date < as.Date("2013-08-01"), 0, DHIMStotal$time - floor(as.numeric(as.Date("2013-08-01") - min(DHIMStotal$date))/30))
DHIMStotal$intervention2 <- ifelse(DHIMStotal$date < as.Date("2013-08-01"), 0, 1)

DHIMStotal <- DHIMStotal %>% arrange(facility, date) %>% filter(level != "Regional Hospital")
DHIMStotal$level <- factor(DHIMStotal$level, c("CHPS", "Health Centre/Clinic", "District Hospital"))

DHIMS <- DHIMStotal %>% select(facility, date, District, Region, level, births, csections,
                      csrate, deaths, month, SERC, time, intervention)

save(DHIMS, file = "DHIMS.rda")


# create heatmap of principal component loading vectors  
png(filename = "./births.png",res = 300, width = 5500, height = 3000)
DHIMStotal %>% group_by(SERC, level, date) %>%
               filter(date >= as.Date("2009-01-01") & date <= as.Date("2014-10-01")) %>%
               summarise(births = mean(births, na.rm = T)) %>%
               ggplot(., aes(x = date, y = births, colour = SERC)) + 
               geom_point() + geom_vline(aes(xintercept = as.numeric(date[date == as.Date("2013-08-01")])), linetype = 2) +
               stat_smooth(method = "lm", formula = y ~ bs(x, degree = 1, knots = as.Date("2013-08-01"))) + 
               facet_wrap(~level)

total <- DHIMStotal %>% group_by(SERC, date) %>%
  filter(date >= as.Date("2009-01-01") & date <= as.Date("2014-10-01")) %>%
  summarise(total_births = sum(births, na.rm = T))

p1 <- DHIMStotal %>% group_by(SERC, level, date) %>%
  filter(date >= as.Date("2009-01-01") & date <= as.Date("2014-10-01")) %>%
  summarise(births = sum(births, na.rm = T)) %>%
  left_join(.,total) %>%
  mutate(prop = births/total_births)
  ggplot(p1, aes(x = date, y = prop, colour = SERC)) + 
  geom_point() + geom_vline(aes(xintercept = as.numeric(date[date == as.Date("2013-08-01")])), linetype = 2) +
  stat_smooth(method = "lm", formula = y ~ bs(x,degree = 1, knots = as.Date("2013-08-01"))) +
  facet_wrap(~level)
dev.off()
DHIMStotal %>% filter(level == "Hospital" & date >= as.Date("2009-01-01")) %>% group_by(SERC, level, date) %>%
  summarise(death_rate = sum(deaths, na.rm = T)/sum(births, na.rm = T)*100000) %>%
  ggplot(., aes(x = date, y = death_rate, colour = SERC)) + 
  geom_point() + stat_smooth(method = "lm", formula = y ~ bs(x,degree = 1, df = 2)) + facet_wrap(~level)

DHIMStotal %>% filter(level !="CHPS"  & date >= as.Date("2009-01-01") & Region != "Upper West") %>% group_by(SERC, level, date) %>%
  summarise(csrate = sum(csections, na.rm = T)/sum(births, na.rm = T)*100) %>%
  ggplot(., aes(x = date, y = csrate, colour = SERC)) + 
  geom_point()  + stat_smooth(method = "glm", formula = y ~ bs(x,degree = 1, knots = as.Date("2013-08-01"))) + facet_wrap(~level)

DHIMStotal %>% filter(date >= as.Date("2009-01-01")) %>% group_by(SERC, level, date) %>%
  summarise(uri  = mean(Labour.Referrals.In, na.rm = T)) %>%
  ggplot(., aes(x = date, y = uri, colour = SERC)) + 
  geom_point()  + stat_smooth(method = "lm", formula = y ~ bs(x,degree = 1, knots = as.Date("2013-08-01"))) + facet_wrap(~level)

DHIMStotal %>% filter(date >= as.Date("2009-01-01")) %>% group_by(SERC, level, date) %>%
  summarise(pneu  = sum(Pneumonia  , na.rm = T)) %>%
  ggplot(., aes(x = date, y = pneu, colour = SERC)) + 
  geom_point()  + stat_smooth(method = "lm", formula = y ~ bs(x,degree = 1, knots = as.Date("2013-08-01"))) + facet_wrap(~level)


# # time series plot for births: intervention vs. control
# png(filename="~/Dropbox/ARCHeS/CU Practicum Ghana/2014/Christopher Boyer/SERC Evaluation/births1.png",width=700,height=600)
# 
# dev.off()

t <- DHIMStotal %>% filter(date >= as.Date("2009-01-01") & date <= as.Date("2014-10-01")) %>%
                    select(births, deaths, csections, level, facility, time, SERC, intervention)

fit_births <-  lme(births ~ level * SERC * time + level * SERC * intervention, 
                   data = na.omit(t), 
                   random = ~ 1 | facility,
                   correlation = corIdent(form = ~ 1|facility))
fit_births <-  lme(births ~ level * SERC * time + level * SERC * intervention, 
                   data = na.omit(t), 
                   random = ~ 1 | facility, 
                   correlation = corARMA(p = 1,q = 1))
  
fit_deaths <-  nlme(deaths ~ SERC * time + SERC * intervention, 
                   data = na.omit(t), 
                   random = ~ 1 | facility, 
                   correlation = corARMA(p = 1,q = 1))

fit <- geeglm(births ~ level*SERC*time + level*SERC*intervention ,
         data = t,
         id = facility,
         corstr = "exchangeable")


fit_births <- DHIMS %>% filter(level =="District Hospital" & date >= as.Date("2009-01-01")) %>%
                             geeglm(deaths ~ SERC * time,
                                    data = .,
                                    family = "poisson",
                                    id = facility,corstr = "ar1",
                                    offset = log(births))

fit_csrate <- geeglm(csections ~ SERC*time + SERC*intervention,
         data = t,
         family = "poisson",
         id = facility)

t$p <- as.numeric(predict(fit_births,newdata = t))
t %>% group_by(SERC, level, date) %>%
  filter(date >= as.Date("2009-01-01") & date <= as.Date("2014-10-01")) %>%
  summarise(births = mean(births, na.rm = T)) %>%
  ggplot(., aes(x = date, y = births, colour = SERC)) + 
  geom_point() + 
  stat_smooth(method = "lm", formula = y ~ bs(x,degree = 1, knots = as.Date("2013-08-01"))) + 
  facet_wrap(~level)

s <- DHIMStotal %>% filter(level !="CHPS"  & date >= as.Date("2009-01-01") & Region != "Upper West") %>% 
                    group_by(SERC, level, facility, date) %>%
                    summarise(csrate = sum(csections, na.rm = T)/sum(births, na.rm = T)*100) %>%
                    filter(level == "Health Centre/Clinic" & csrate != 0)
  
total <- DHIMStotal %>% group_by(SERC, date) %>%
  filter(date >= as.Date("2009-01-01") & date <= as.Date("2014-10-01")) %>%
  summarise(total_births = sum(births, na.rm = T))

full <- DHIMStotal %>%
  left_join(.,total) %>%
  mutate(prop = births/total_births)




library(doBy)
coef <- esticon(fit_births, diag(4))
zou.mod.expci <- exp(cbind(zou.mod.coefci$Estimate, zou.mod.coefci$Lower, zou.mod.coefci$Upper))
rownames(zou.mod.expci) <- names(coef(zou.mod))
colnames(zou.mod.expci) <- c("RR", "Lower RR", "Upper RR")
zou.mod.expci
