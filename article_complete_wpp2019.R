# Checking if if all packages are installed

list.of.packages = c("devtools", "survey", "readxl", "reshape2", "ggplot2", "zoo",
                     "ggthemes", "plyr")
new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages)
rm(list.of.packages, new.packages)

# Loading and installing packages from Github
library(devtools)
install_github("ajdamico/lodown", dependencies = TRUE)  # When installing lodown --MAC user terminal: xcode-select --install
install_github("timriffe/DemoTools")

library(lodown)

#---------------------------------------------------------------------------------------------------------------------

# Population in the workforce Estimates
# We provided the estimates for years previous to 2001 as it can be challeging to obtain the data.

# Downloading PNAD data from the brazilian Statistical Office Website --only do
# it the first time you run the code!  The lodown function will download the .rds
# files
lodown("pnad", output_dir = file.path("~/Google Drive/Paper - Matheus & Bernardo",
                                      "PNAD"))

# Now we can work on the databases Generating the PNAD catalog
pnad_cat = get_catalog("pnad", output_dir = file.path(path.expand("~/Google Drive/Paper - Matheus & Bernardo"),
                                                      "PNAD"))

# The following code is required to generate the estimates from the survey
# source: http://bit.ly/2yqXUzd
options(survey.lonely.psu = "adjust")

years = as.numeric(pnad_cat$year)

for (k in 1:length(years)) {
  
  i = years[k]
  
  pnad_df = readRDS(pnad_cat[k, "output_filename"])
  
  pop_types = data.frame(v4609 = unique(pnad_df$v4609), Freq = unique(pnad_df$v4609))
  
  prestratified_design = svydesign(id = ~v4618, strata = ~v4617, data = pnad_df,
                                   weights = ~pre_wgt, nest = TRUE)
  
  rm(pnad_df)
  gc()
  
  pnad_design = postStratify(design = prestratified_design, strata = ~v4609, population = pop_types)
  
  rm(prestratified_design)
  gc()
  
  pnad_design = update(pnad_design, agecat = factor(9 + findInterval(v8005, seq(10,
                                                                                85, 1))))
  
  # v0302 = sex; v9001, v9002, v9115 = variables related to the labor market
  data = svyby(~one, ~agecat + addNA(v0302) + addNA(v9001) + addNA(v9002) + addNA(v9115),
               pnad_design, svytotal)
  data$year = rep(i, nrow(data))  # creating a year identification variable
  
  write.csv(data, paste0("PNAD-", i, ".csv"))
}

#---------------------------------------------------------------------------------------------------------------------

# Parameters -- very important to run code correctly
StartYear = 1979
projStartYear = 1992
check = "Y"
proj_length = 10
last_before_proj = 2015 - proj_length

#---------------------------------------------------------------------------------------------------------------------

# Loading required function for data analysis
setwd("~/Google Drive/Paper - Matheus & Bernardo/Functions")
source("auto_loess.R")
source("lee_carter.R")
source("life_table.R")
source("length_retirement.R")

library(survey)
library(readxl)
library(reshape2)
library(zoo)
library(ggplot2)
library(ggthemes)
library(plyr)
library(DemoTools)

#---------------------------------------------------------------------------------------------------------------------

# Once the .csv files for every year are ready we should read them all and merge
# 'em set your working directory --must be where the .csv files are
setwd("~/Google Drive/Paper - Matheus & Bernardo/PNAD/2001-'15/estimates")

#---------------------------------------------------------------------------------------------------------------------

PNAD = do.call(rbind, lapply(list.files(pattern = "*.csv"), read.csv))  #reading all the PNAD files
PNAD = PNAD[, -1]  #the first column is not useful, thta's why we delete it
PNAD$ID = paste(PNAD$addNA.v9001., PNAD$addNA.v9002., PNAD$addNA.v9115.)  # creating an ID --helpful when subsetting

names(PNAD) = c("agecat", "v0302", "v9001", "v9002", "v9115", "one", "sd", "year",
                "ID")

# Now we aggregate the data by sex and age
PNAD_total = aggregate(one ~ v0302 + agecat + year, PNAD, FUN = sum)  #v0302 is the sex variable
PNAD_total = split(PNAD_total, PNAD_total$v0302)

PNAD_total = lapply(seq_along(PNAD_total), function(x) as.data.frame(PNAD_total[[x]])[,
                                                                                      -1])

invisible(lapply(seq_along(PNAD_total), function(x) {
  assign(c("PNADMale", "PNADFemale")[x], PNAD_total[[x]], envir = .GlobalEnv)
}))
names(PNADMale) = c("Age", "Year", "Value")
names(PNADFemale) = c("Age", "Year", "Value")

PNADMale = subset(PNADMale, Age > 9)
PNADFemale = subset(PNADFemale, Age > 9)

# The sub vector contains the IDs we will actually be using --working population
wrk = c("1 NA 1", "1 NA 3", "1 NA 9", "3 2 1", "3 2 3", "3 4 1")

# Now we subset the data and aggregate it by sex, age and year
PNAD_wrk = subset(PNAD, ID %in% wrk)
PNAD_wrk = aggregate(one ~ v0302 + agecat + year, PNAD_wrk, FUN = sum)  #v0302 is the sex variable
PNAD_wrk = dcast(PNAD_wrk, v0302 + agecat ~ year, value.var = "one", drop = FALSE)
PNAD_wrk = melt(PNAD_wrk, id.vars = c("agecat", "v0302"))
PNAD_wrk = split(PNAD_wrk, PNAD_wrk$v0302)

PNAD_wrk = lapply(seq_along(PNAD_wrk), function(x) as.data.frame(PNAD_wrk[[x]])[,
                                                                                -2])

invisible(lapply(seq_along(PNAD_wrk), function(x) {
  assign(c("PNADMale_wrk", "PNADFemale_wrk")[x], PNAD_wrk[[x]], envir = .GlobalEnv)
}))
names(PNADMale_wrk) = c("Age", "Year", "Value")
names(PNADFemale_wrk) = c("Age", "Year", "Value")

PNADMale_wrk = subset(PNADMale_wrk, Age > 9)
PNADFemale_wrk = subset(PNADFemale_wrk, Age > 9)

# write.csv(PNADMale_wrk, 'PNAD.csv')

rm(PNAD, PNAD_total, wrk, PNAD_wrk)

#---------------------------------------------------------------------------------------------------------------------

# Merging data from 1981 to 1999 with data from 2011 to 2015
setwd("~/Google Drive/Paper - Matheus & Bernardo/PNAD/1981-'99/estimates")

PNADMaleBefore01 = do.call(rbind, lapply(list.files(pattern = "PNADMale.csv"), read.csv))  #reading all the PNAD files
PNADMaleBefore01 = PNADMaleBefore01[, -1]  #the first column is not useful, thta's why we delete it

PNADMale_wrkBefore01 = do.call(rbind, lapply(list.files(pattern = "PNADMale_wrk.csv"),
                                             read.csv))  #reading all the PNAD files
PNADMale_wrkBefore01 = PNADMale_wrkBefore01[, -1]  #the first column is not useful, thta's why we delete it

PNADFemaleBefore01 = do.call(rbind, lapply(list.files(pattern = "PNADFemale.csv"),
                                           read.csv))  #reading all the PNAD files
PNADFemaleBefore01 = PNADFemaleBefore01[, -1]  #the first column is not useful, thta's why we delete it

PNADFemale_wrkBefore01 = do.call(rbind, lapply(list.files(pattern = "PNADFemale_wrk.csv"),
                                               read.csv))  #reading all the PNAD files
PNADFemale_wrkBefore01 = PNADFemale_wrkBefore01[, -1]  #the first column is not useful, that's why we delete it

PNADMale = rbind(PNADMaleBefore01, PNADMale)
PNADFemale = rbind(PNADFemaleBefore01, PNADFemale)

PNADMale_wrk = rbind(PNADMale_wrkBefore01, PNADMale_wrk)
PNADFemale_wrk = rbind(PNADFemale_wrkBefore01, PNADFemale_wrk)

for (i in 1:nrow(PNADFemale)) {
  PNADMale$Age[i] = min(PNADMale$Age[i], 85)
  PNADMale_wrk$Age[i] = min(PNADMale_wrk$Age[i], 85)
  PNADFemale$Age[i] = min(PNADFemale$Age[i], 85)
  PNADFemale_wrk$Age[i] = min(PNADFemale_wrk$Age[i], 85)
}

PNADMale = aggregate(Value ~ Age + Year, PNADMale, FUN = sum)
PNADMale_wrk = aggregate(Value ~ Age + Year, PNADMale_wrk, FUN = sum)
PNADFemale = aggregate(Value ~ Age + Year, PNADFemale, FUN = sum)
PNADFemale_wrk = aggregate(Value ~ Age + Year, PNADFemale_wrk, FUN = sum)

PNADMale = subset(PNADMale, Year %in% seq(StartYear, 2015))
PNADMale_wrkBefore01 = subset(PNADMale_wrk, Year %in% seq(StartYear, 2015))
PNADFemale = subset(PNADFemale, Year %in% seq(StartYear, 2015))
PNADFemale_wrk = subset(PNADFemale_wrk, Year %in% seq(StartYear, 2015))

rm(PNADMaleBefore01, PNADFemaleBefore01, PNADFemale_wrkBefore01, PNADMale_wrkBefore01,
   i)

#---------------------------------------------------------------------------------------------------------------------

# Creating 'ages' vector
ages <- c(seq(10, 85, 1))
years <- unique(PNADFemale$Year)

LFPRMale <- merge(PNADMale_wrk, PNADMale, by.x = c("Age", "Year"), by.y = c("Age",
                                                                            "Year"))
LFPRFemale <- merge(PNADFemale_wrk, PNADFemale, by.x = c("Age", "Year"), by.y = c("Age",
                                                                                  "Year"))

LFPRMale$Ratio = LFPRMale$Value.x/LFPRMale$Value.y
rm(PNADMale, PNADMale_wrk)

LFPRMale = dcast(LFPRMale, Age ~ Year, value.var = "Ratio")
LFPRMale = na.locf(LFPRMale, x = ages, xout = ages)
LFPRMale = as.data.frame(LFPRMale)
LFPRMale = melt(LFPRMale, id.vars = "Age", variable.name = "Year", value.name = "Ratio")

LFPRFemale$Ratio = LFPRFemale$Value.x/LFPRFemale$Value.y
rm(PNADFemale, PNADFemale_wrk)
LFPRFemale = dcast(LFPRFemale, Age ~ Year, value.var = "Ratio")
LFPRFemale = na.locf(LFPRFemale, x = ages, xout = ages)
LFPRFemale = as.data.frame(LFPRFemale)
LFPRFemale = melt(LFPRFemale, id.vars = "Age", variable.name = "Year", value.name = "Ratio")

#---------------------------------------------------------------------------------------------------------------------

LFPRMale = dcast(LFPRMale, Age ~ Year, value.var = "Ratio")
LFPRFemale = dcast(LFPRFemale, Age ~ Year, value.var = "Ratio")

# For loop that puts all the data in a matrix with the loess smoothing method
ratesLOESSMale <- matrix(nrow = length(ages), ncol = length(years))
ratesLOESSFemale <- matrix(nrow = length(ages), ncol = length(years))

for (i in 1:length(years)) {
  ratesLOESSMale[, i] <- autoloess(loess(LFPRMale[, i + 1] ~ ages))$fitted
  ratesLOESSFemale[, i] <- autoloess(loess(LFPRFemale[, i + 1] ~ ages))$fitted
}

for (i in 1:length(ages)) {
  for (k in 1:length(years)) {
    LFPRMale[i, k + 1] = ratesLOESSMale[i, k]
    LFPRFemale[i, k + 1] = ratesLOESSFemale[i, k]
  }
}

rm(ratesLOESSMale, ratesLOESSFemale, i, k)

LFPRMale = melt(LFPRMale, id.vars = "Age", variable.name = "Year", value.name = "Ratio")
LFPRFemale = melt(LFPRFemale, id.vars = "Age", variable.name = "Year", value.name = "Ratio")

#---------------------------------------------------------------------------------------------------------------------

ggplot(subset(LFPRMale, Year %in% seq(head(years, 1), tail(years, 1), by = 4)), aes(x = Age, y = Ratio, colour = Year)) +
  geom_line() + theme_few() + scale_colour_tableau()

ggplot() +
  geom_line(data = subset(LFPRFemale, Year %in% seq(head(years, 1), tail(years, 1), by = 9)), 
            aes(x = Age, y = Ratio, fill = Year), show.legend = FALSE) +
  geom_point(data = subset(LFPRFemale, Year %in% seq(head(years, 1), tail(years, 1), by = 18)), 
             aes(x = Age, y = Ratio, shape = Year, colour = Year)) +
  theme_few()

ggplot(subset(LFPRFemale, Year %in% seq(head(years, 1), tail(years, 1), by = 4)), aes(x = Age, y = Ratio, colour = Year)) +
  geom_line() + theme_few() + scale_colour_tableau()

#---------------------------------------------------------------------------------------------------------------------

years_interpolation <- (seq(years[1], tail(years, n = 1)))

LFPRMale = dcast(LFPRMale, Year ~ Age, value.var = "Ratio")[, -1]
LFPRFemale = dcast(LFPRFemale, Year ~ Age, value.var = "Ratio")[, -1]

MatrixMale = data.matrix(LFPRMale)
MatrixMale <- ifelse(MatrixMale < 0, 0, MatrixMale)
MatrixFemale = data.matrix(LFPRFemale)
MatrixFemale <- ifelse(MatrixFemale < 0, 0, MatrixFemale)

MatrixMale_interpolation = matrix(nrow = length(years_interpolation), ncol = dim(MatrixMale)[2])
MatrixFemale_interpolation = matrix(nrow = length(years_interpolation), ncol = dim(MatrixFemale)[2])

for (i in 1:length(ages)) {
  MatrixMale_interpolation[, i] <- approx(x = years, y = MatrixMale[, i], xout = years_interpolation)$y
  MatrixFemale_interpolation[, i] <- approx(x = years, y = MatrixFemale[, i], xout = years_interpolation)$y
}
rm(i, MatrixMale, MatrixFemale)

#---------------------------------------------------------------------------------------------------------------------
# Checking if Model is weel adjusted for projections -- optional (assign 'Y' or
# 'N')

if (check == "N") {
  years = subset(years_interpolation, years_interpolation >= projStartYear)
  years_forecast = seq(tail(years, n = 1) + 1, tail(years, n = 1) + proj_length)
  years_all = seq(years[1], tail(years_forecast, n = 1))
} else {
  years = seq(projStartYear, last_before_proj)
  years_forecast = seq(tail(years, n = 1) + 1, tail(years, n = 1) + proj_length)
  years_all = seq(years[1], tail(years_forecast, n = 1))
}

LFPRMale_interpolation = as.data.frame(MatrixMale_interpolation)
rownames(LFPRMale_interpolation) = years_interpolation
colnames(LFPRMale_interpolation) = ages
LFPRFemale_interpolation = as.data.frame(MatrixFemale_interpolation)
rownames(LFPRFemale_interpolation) = years_interpolation
colnames(LFPRFemale_interpolation) = ages

if (check == "Y") {
  
  LFPRMale_interpolation_check = subset(LFPRMale_interpolation, rownames(LFPRMale_interpolation) %in%
                                          years)
  MaleModel_check = leecarter(LFPRMale_interpolation_check, proj_length)
  FLFPRMale_check <- matrix(nrow = length(MaleModel_check$kt_forecast), ncol = length(ages))
  for (i in 1:length(MaleModel_check$kt_forecast)) {
    FLFPRMale_check[i, ] <- exp((MaleModel_check$bx * MaleModel_check$kt_forecast[i]) +
                                  MaleModel_check$ax)
  }
  
  LFPRFemale_interpolation_check = subset(LFPRFemale_interpolation, rownames(LFPRFemale_interpolation) %in%
                                            years)
  FemaleModel_check = leecarter(LFPRFemale_interpolation_check, proj_length)
  FLFPRFemale_check <- matrix(nrow = length(FemaleModel_check$kt_forecast), ncol = length(ages))
  for (i in 1:length(FemaleModel_check$kt_forecast)) {
    FLFPRFemale_check[i, ] <- exp((FemaleModel_check$bx * FemaleModel_check$kt_forecast[i]) +
                                    FemaleModel_check$ax)
  }
  
  FLFPRMale_check <- t(FLFPRMale_check)
  FLFPRMale_check <- as.data.frame(FLFPRMale_check)
  rownames(FLFPRMale_check) = ages
  colnames(FLFPRMale_check) = years_forecast
  FLFPRMale_check = melt(t(FLFPRMale_check))
  colnames(FLFPRMale_check) = c("Year", "Age", "Ratio")
  LFPRMale_interpolation_check = melt(t(LFPRMale_interpolation))
  colnames(LFPRMale_interpolation_check) = c("Age", "Year", "Ratio")
  
  FLFPRFemale_check <- t(FLFPRFemale_check)
  FLFPRFemale_check <- as.data.frame(FLFPRFemale_check)
  rownames(FLFPRFemale_check) = ages
  colnames(FLFPRFemale_check) = years_forecast
  FLFPRFemale_check = melt(t(FLFPRFemale_check))
  colnames(FLFPRFemale_check) = c("Year", "Age", "Ratio")
  LFPRFemale_interpolation_check = melt(t(LFPRFemale_interpolation))
  colnames(LFPRFemale_interpolation_check) = c("Age", "Year", "Ratio")
  
  cols <- c(Observed = "#15B7B9", Projected = "#F57170")
  
  MalePlot_check = ggplot() + 
    geom_line(data = subset(FLFPRMale_check, Year %in% 2015), aes(x = Age, y = Ratio, colour = "Projected")) +
    geom_line(data = subset(LFPRMale_interpolation_check, Year %in% 2015), aes(x = Age, y = Ratio, colour = "Observed")) + 
    theme_few() + scale_colour_tableau() +
    #scale_colour_manual(name = "Curves", values = cols) + scale_fill_manual(name = "Curves", values = cols) + 
    theme(legend.position = "right", legend.title = element_blank())
  
  FemalePlot_check = ggplot() + geom_line(data = subset(FLFPRFemale_check, Year %in% 2015), aes(x = Age, y = Ratio, colour = "Projected")) + 
    geom_line(data = subset(LFPRFemale_interpolation_check, Year %in% 2015), aes(x = Age, y = Ratio, colour = "Observed")) +
    theme_few() + scale_colour_tableau() +
    #scale_colour_manual(name = "Curves", values = cols) + scale_fill_manual(name = "Curves", values = cols) +
    theme(legend.position = "right", legend.title = element_blank())
  
  print(MalePlot_check)
  FemalePlot_check
}

if (check == "N") {
  rm(LFPRMale, LFPRFemale, MatrixMale_interpolation, MatrixFemale_interpolation,
     check, StartYear, last_before_proj)
} else {
  rm(LFPRMale, LFPRFemale, MatrixMale_interpolation, MatrixFemale_interpolation,
     MaleModel_check, FemaleModel_check, FLFPRMale_check, FLFPRFemale_check, LFPRMale_interpolation_check,
     LFPRFemale_interpolation_check, MalePlot_check, FemalePlot_check, StartYear,
     last_before_proj)
}

#---------------------------------------------------------------------------------------------------------------------

if (check == "Y") {
  years = years_all
  years_forecast = seq(tail(years, n = 1) + 1, tail(years, n = 1) + proj_length)
  years_all = seq(years[1], tail(years_forecast, n = 1))
}

MaleModel = leecarter(subset(LFPRMale_interpolation, rownames(LFPRMale_interpolation) %in% years_all), proj_length)
MaleParameters = data.frame(MaleModel$ax, MaleModel$bx)
Male_kt = MaleModel$kt

ggplot(MaleParameters, aes(x = ages, y = MaleModel.ax)) + labs(x = "Ages", y = "ax - Male LC Model") +
  geom_line() + theme_few() + scale_colour_tableau()
ggplot(MaleParameters, aes(x = ages, y = MaleModel.bx)) + labs(x = "Ages", y = "bx - Male LC Model") +
  geom_line() + theme_few() + scale_colour_tableau()

FLFPRMale = matrix(nrow = length(MaleModel$kt_forecast), ncol = length(ages))
for (i in 1:length(MaleModel$kt_forecast)) {
  FLFPRMale[i, ] <- exp((MaleModel$bx * MaleModel$kt_forecast[i]) + MaleModel$ax)
}

FLFPRMale = t(FLFPRMale)

rep.col = function(x,n){
  matrix(rep(x, each = n), ncol = n, byrow = TRUE)
}

constantFLFPRMale = rep.col(FLFPRMale[, ncol(FLFPRMale)], (2100 - 2025))

FLFPRMale = cbind(FLFPRMale, constantFLFPRMale)
FLFPRMale = as.data.frame(FLFPRMale)
rownames(FLFPRMale) = ages
colnames(FLFPRMale) = c(years_forecast, seq(2026, 2100))

Male_kt = as.data.frame(MaleModel$kt)
Male_kt$Year = years
colnames(Male_kt) = c("kt", "Year")
Male_kt_forecast = as.data.frame(MaleModel$kt_forecast)
Male_kt_forecast$Year = years_forecast
Male_kt_forecast$low = MaleModel$kt_lo_forecast
Male_kt_forecast$high = MaleModel$kt_hi_forecast
colnames(Male_kt_forecast) = c("kt", "Year", "kt_low", "kt_high")

ggplot() + geom_line(data = rbind(Male_kt, Male_kt_forecast[, 1:2]), aes(x = Year, y = kt)) + 
  geom_ribbon(data = Male_kt_forecast, aes(x = Year, ymin = kt_low, ymax = kt_high), fill = "grey70", alpha = 0.25) + 
  geom_line(data = Male_kt_forecast, aes(x = years_forecast, y = kt), colour = "#0074E4") + theme_few() + scale_colour_tableau() +
  labs(x = "Ages", y = "kt - Male LC Model") +
  geom_vline(aes(xintercept = years_forecast[1]), colour = "#1B3C68", linetype = "dashed")

rm(Male_kt, Male_kt_forecast, MaleModel, MaleParameters, constantFLFPRMale)

#---------------------------------------------------------------------------------------------------------------------

FemaleModel = leecarter(subset(LFPRFemale_interpolation, rownames(LFPRFemale_interpolation) %in% years_all), proj_length)
FemaleParameters = data.frame(FemaleModel$ax, FemaleModel$bx)
Female_kt = FemaleModel$kt

ggplot(FemaleParameters, aes(x = ages, y = FemaleModel.ax)) + labs(x = "Ages", y = "ax - Female LC Model") +
  geom_line() + theme_few() + scale_colour_tableau()
ggplot(FemaleParameters, aes(x = ages, y = FemaleModel.bx)) + labs(x = "Ages", y = "bx - Female LC Model") +
  geom_line() + theme_few() + scale_colour_tableau()

FLFPRFemale = matrix(nrow = length(FemaleModel$kt_forecast), ncol = length(ages))
for (i in 1:length(FemaleModel$kt_forecast)) {
  FLFPRFemale[i, ] <- exp((FemaleModel$bx * FemaleModel$kt_forecast[i]) + FemaleModel$ax)
}

FLFPRFemale = t(FLFPRFemale)

constantFLFPRFemale = rep.col(FLFPRFemale[, ncol(FLFPRFemale)], (2100 - 2025))

FLFPRFemale = cbind(FLFPRFemale, constantFLFPRFemale)
FLFPRFemale = as.data.frame(FLFPRFemale)
rownames(FLFPRFemale) = ages
colnames(FLFPRFemale) = c(years_forecast, seq(2026, 2100))

Female_kt = as.data.frame(FemaleModel$kt)
Female_kt$Year = years
colnames(Female_kt) = c("kt", "Year")
Female_kt_forecast = as.data.frame(FemaleModel$kt_forecast)
Female_kt_forecast$Year = years_forecast
Female_kt_forecast$low = FemaleModel$kt_lo_forecast
Female_kt_forecast$high = FemaleModel$kt_hi_forecast
colnames(Female_kt_forecast) = c("kt", "Year", "kt_low", "kt_high")

ggplot() + geom_line(data = rbind(Female_kt, Female_kt_forecast[, 1:2]), aes(x = Year, y = kt)) + 
  geom_ribbon(data = Female_kt_forecast, aes(x = Year, ymin = kt_low, ymax = kt_high), fill = "grey70", alpha = 0.25) + 
  geom_line(data = Female_kt_forecast, aes(x = years_forecast, y = kt), colour = "#0074E4") + theme_few() + scale_colour_tableau() +
  labs(x = "Ages", y = "kt - Female LC Model") +
  geom_vline(aes(xintercept = years_forecast[1]), colour = "#1B3C68", linetype = "dashed")

rm(Female_kt, Female_kt_forecast, FemaleModel, FemaleParameters, ages, i, constantFLFPRFemale)

#---------------------------------------------------------------------------------------------------------------------

LFPRMale_interpolation = melt(t(LFPRMale_interpolation))
colnames(LFPRMale_interpolation) = c("Age", "Year", "Ratio")
FLFPRMale = melt(t(FLFPRMale))
colnames(FLFPRMale) = c("Year", "Age", "Ratio")

LFPRFemale_interpolation = melt(t(LFPRFemale_interpolation))
colnames(LFPRFemale_interpolation) = c("Age", "Year", "Ratio")
FLFPRFemale = melt(t(FLFPRFemale))
colnames(FLFPRFemale) = c("Year", "Age", "Ratio")

ForecastLFPRMale = rbind(LFPRMale_interpolation, FLFPRMale)
ForecastLFPRFemale = rbind(LFPRFemale_interpolation, FLFPRFemale)

rm(FLFPRMale, FLFPRFemale, LFPRMale_interpolation, LFPRFemale_interpolation)

ggplot(subset(ForecastLFPRMale, Year %in% seq(years_all[1], tail(years_forecast, n = 1), by = 5)), aes(x = Age, y = Ratio, colour = factor(Year))) + 
  geom_line() + theme_few() + scale_colour_tableau() +
  theme(legend.position = "right", legend.title = element_blank()) + 
  geom_line(aes(y = 1), colour = "black", linetype = "dashed") + 
  labs(x = "Ages", y = "Male LFPR - Select Years")

ggplot(subset(ForecastLFPRFemale, Year %in% seq(years_all[1], tail(years_forecast, n = 1), by = 5)), aes(x = Age, y = Ratio, colour = factor(Year))) + 
  geom_line() + theme_few() + scale_colour_tableau() +
  theme(legend.position = "right", legend.title = element_blank()) + geom_line(aes(y = 1), colour = "black", linetype = "dashed") + 
  labs(x = "Ages", y = "Female LFPR - Select Years")

rm(proj_length, years, years_forecast, years_interpolation, years_all, projStartYear, cols, check)

#---------------------------------------------------------------------------------------------------------------------

EstimatesMale <- read_excel("~/Google Drive/Paper - Matheus & Bernardo/UN/Life Tables/WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.XLSX",
                            sheet = "ESTIMATES", col_types = c("skip", "skip", "text", "skip", "numeric", "skip", "skip",
                                                               "text", "numeric", "numeric", "numeric", "text", "skip", "skip", "skip",
                                                               "skip", "skip", "skip", "numeric", "numeric"), skip = 16)

Medium_2020_2050_Male <- read_excel("~/Google Drive/Paper - Matheus & Bernardo/UN/Life Tables/WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.XLSX",
                                    sheet = "MEDIUM 2020-2050", col_types = c("skip", "skip", "text", "skip", "numeric", "skip", "skip",
                                                                              "text", "numeric", "numeric", "numeric", "text", "skip", "skip", "skip",
                                                                              "skip", "skip", "skip", "numeric", "numeric"), skip = 16)


Medium_2050_2100_Male <- read_excel("~/Google Drive/Paper - Matheus & Bernardo/UN/Life Tables/WPP2019_MORT_F17_2_ABRIDGED_LIFE_TABLE_MALE.XLSX",
                                    sheet = "MEDIUM 2050-2100", col_types = c("skip", "skip", "text", "skip", "numeric", "skip", "skip",
                                                                              "text", "numeric", "numeric", "numeric", "text", "skip", "skip", "skip",
                                                                              "skip", "skip", "skip", "numeric", "numeric"), skip = 16)

dataMale = rbind(EstimatesMale, Medium_2020_2050_Male, Medium_2050_2100_Male)
colnames(dataMale) = c("Country", "LocID", "Period", "Age", "Length", "mx", "qx", "ex",
                       "ax")

rm(EstimatesMale, Medium_2020_2050_Male, Medium_2050_2100_Male)

EstimatesFemale <- read_excel("~/Google Drive/Paper - Matheus & Bernardo/UN/Life Tables/WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.XLSX",
                              sheet = "ESTIMATES", col_types = c("skip", "skip", "text", "skip", "numeric", "skip", "skip",
                                                                 "text", "numeric", "numeric", "numeric", "text", "skip", "skip", "skip",
                                                                 "skip", "skip", "skip", "numeric", "numeric"), skip = 16)

Medium_2020_2050_Female <- read_excel("~/Google Drive/Paper - Matheus & Bernardo/UN/Life Tables/WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.XLSX",
                                      sheet = "MEDIUM 2020-2050", col_types = c("skip", "skip", "text", "skip", "numeric", "skip", "skip",
                                                                                "text", "numeric", "numeric", "numeric", "text", "skip", "skip", "skip",
                                                                                "skip", "skip", "skip", "numeric", "numeric"), skip = 16)


Medium_2050_2100_Female <- read_excel("~/Google Drive/Paper - Matheus & Bernardo/UN/Life Tables/WPP2019_MORT_F17_3_ABRIDGED_LIFE_TABLE_FEMALE.XLSX",
                                      sheet = "MEDIUM 2050-2100", col_types = c("skip", "skip", "text", "skip", "numeric", "skip", "skip",
                                                                                "text", "numeric", "numeric", "numeric", "text", "skip", "skip", "skip",
                                                                                "skip", "skip", "skip", "numeric", "numeric"), skip = 16)

dataFemale = rbind(EstimatesFemale, Medium_2020_2050_Female, Medium_2050_2100_Female)
colnames(dataFemale) = c("Country", "LocID", "Period", "Age", "Length", "mx", "qx", "ex",
                         "ax")

rm(EstimatesFemale, Medium_2020_2050_Female, Medium_2050_2100_Female)

LTMale = life.table(dataMale)
resultMale = length.retirement(ForecastLFPRMale, LTMale)
resultMale = melt(resultMale, value.name = "ELRP")

LTFemale = life.table(dataFemale)
resultFemale = length.retirement(ForecastLFPRFemale, LTFemale)
resultFemale = melt(resultFemale, value.name = "ELRP")

LifeExpectancy = rbind(LTMale$ex[18, ], LTFemale$ex[18, ])
rownames(LifeExpectancy) = c("Males", "Females")
LifeExpectancy = t(LifeExpectancy)
LifeExpectancy = melt(LifeExpectancy)
colnames(LifeExpectancy) = c("Year", "Sex", "e20")

ggplot(LifeExpectancy, aes(x = Year, y = e20, colour = factor(Sex))) + geom_line() +
  theme_few() + scale_colour_tableau() +
  theme(legend.position = "right", legend.title = element_blank()) + 
  labs(x = "Years", y = "Life Expectancy at Age 20 - Select Years")

#---------------------------------------------------------------------------------------------------------------------

# Alternative Scenarios - Constant LFPR at 1979 levels
ALT1_ForecastLFPRMale = dcast(ForecastLFPRMale, Age ~ Year)

for (i in 1:nrow(ALT1_ForecastLFPRMale)){
  for (j in 3:ncol(ALT1_ForecastLFPRMale)){
    ALT1_ForecastLFPRMale[i, j] = ALT1_ForecastLFPRMale[i, 2]
  }
}
ALT1_ForecastLFPRMale = melt(ALT1_ForecastLFPRMale, id.vars = "Age")
colnames(ALT1_ForecastLFPRMale) = c("Age", "Year", "Ratio")
ALT1_ForecastLFPRMale$Year = as.integer(as.character(ALT1_ForecastLFPRMale$Year))

ALT1_resultMale = length.retirement(ALT1_ForecastLFPRMale, LTMale)
ALT1_resultMale = melt(ALT1_resultMale, value.name = "ELRP")
rm(ALT1_ForecastLFPRMale, LTMale)

ALT1_ForecastLFPRFemale = dcast(ForecastLFPRFemale, Age ~ Year)

for (i in 1:nrow(ALT1_ForecastLFPRFemale)){
  for (j in 3:ncol(ALT1_ForecastLFPRFemale)){
    ALT1_ForecastLFPRFemale[i, j] = ALT1_ForecastLFPRFemale[i, 2]
  }
}
ALT1_ForecastLFPRFemale = melt(ALT1_ForecastLFPRFemale, id.vars = "Age")
colnames(ALT1_ForecastLFPRFemale) = c("Age", "Year", "Ratio")
ALT1_ForecastLFPRFemale$Year = as.integer(as.character(ALT1_ForecastLFPRFemale$Year))

ALT1_resultFemale = length.retirement(ALT1_ForecastLFPRFemale, LTFemale)
ALT1_resultFemale = melt(ALT1_resultFemale, value.name = "ELRP")
rm(ALT1_ForecastLFPRFemale, LTFemale)

#---------------------------------------------------------------------------------------------------------------------

# Alternative Scenarios - Constant Mortality at 1979 levels

# Subsetting data in order to use only Brazilian information
dataMale = subset(dataMale, LocID == 76)
# Subsetting data again in order to use only age groups lower or equal to 85 to match PNAD availiability
dataMale = subset(dataMale, Age <= 85)

ALT2_mxMale = dcast(dataMale, Country + Age ~ Period, value.var = "mx")
ALT2_qxMale = dcast(dataMale, Country + Age ~ Period, value.var = "qx")
ALT2_axMale = dcast(dataMale, Country + Age ~ Period, value.var = "ax")
ALT2_exMale = dcast(dataMale, Country + Age ~ Period, value.var = "ex")


for (i in 1:nrow(ALT2_mxMale)){
  for (j in 4:ncol(ALT2_mxMale)){
    ALT2_mxMale[i, j] = ALT2_mxMale[i, 3]
    ALT2_qxMale[i, j] = ALT2_qxMale[i, 3]
    ALT2_axMale[i, j] = ALT2_axMale[i, 3]
    ALT2_exMale[i, j] = ALT2_exMale[i, 3]
  }
}

ALT2_mxMale = melt(ALT2_mxMale, id.vars = c("Country", "Age"), variable.name = "Period", value.name = "mx")
ALT2_qxMale = melt(ALT2_qxMale, id.vars = c("Country", "Age"), variable.name = "Period", value.name = "qx")
ALT2_axMale = melt(ALT2_axMale, id.vars = c("Country", "Age"), variable.name = "Period", value.name = "ax")
ALT2_exMale = melt(ALT2_exMale, id.vars = c("Country", "Age"), variable.name = "Period", value.name = "ex")
ALT2_dataMale = Reduce(function(x, y) merge(x, y, all=TRUE), list(ALT2_mxMale, ALT2_qxMale, ALT2_axMale, ALT2_exMale))

temp_dataMale = dataMale[, 1:5]

ALT2_dataMale = merge(temp_dataMale, ALT2_dataMale, by.x = c("Country", "Period", "Age"))
rm(dataMale, ALT2_axMale, ALT2_mxMale, ALT2_qxMale)

ALT2_LTMale = life.table(ALT2_dataMale)
ALT2_resultMale = length.retirement(ForecastLFPRMale, ALT2_LTMale)
ALT2_resultMale = melt(ALT2_resultMale, value.name = "ELRP")

rm(ALT2_LTMale, ForecastLFPRMale, ALT2_dataMale, temp_dataMale, ALT2_exMale)

# Subsetting data in order to use only Brazilian information
dataFemale = subset(dataFemale, LocID == 76)

ALT2_mxFemale = dcast(dataFemale, Country + Age ~ Period, value.var = "mx")
ALT2_qxFemale = dcast(dataFemale, Country + Age ~ Period, value.var = "qx")
ALT2_axFemale = dcast(dataFemale, Country + Age ~ Period, value.var = "ax")
ALT2_exFemale = dcast(dataFemale, Country + Age ~ Period, value.var = "ex")

for (i in 1:nrow(ALT2_mxFemale)){
  for (j in 4:ncol(ALT2_mxFemale)){
    ALT2_mxFemale[i, j] = ALT2_mxFemale[i, 3]
    ALT2_qxFemale[i, j] = ALT2_qxFemale[i, 3]
    ALT2_axFemale[i, j] = ALT2_axFemale[i, 3]
    ALT2_exFemale[i, j] = ALT2_exFemale[i, 3]
  }
}

ALT2_mxFemale = melt(ALT2_mxFemale, id.vars = c("Country", "Age"), variable.name = "Period", value.name = "mx")
ALT2_qxFemale = melt(ALT2_qxFemale, id.vars = c("Country", "Age"), variable.name = "Period", value.name = "qx")
ALT2_axFemale = melt(ALT2_axFemale, id.vars = c("Country", "Age"), variable.name = "Period", value.name = "ax")
ALT2_exFemale = melt(ALT2_exFemale, id.vars = c("Country", "Age"), variable.name = "Period", value.name = "ex")
ALT2_dataFemale = Reduce(function(x, y) merge(x, y, all=TRUE), list(ALT2_mxFemale, ALT2_qxFemale, ALT2_axFemale, ALT2_exFemale))

temp_dataFemale = dataFemale[, 1:5]

ALT2_dataFemale = merge(temp_dataFemale, ALT2_dataFemale, by.x = c("Country", "Period", "Age"))
rm(dataFemale, ALT2_axFemale, ALT2_mxFemale, ALT2_qxFemale, temp_dataFemale, ALT2_exFemale)

ALT2_LTFemale = life.table(ALT2_dataFemale)
ALT2_resultFemale = length.retirement(ForecastLFPRFemale, ALT2_LTFemale)
ALT2_resultFemale = melt(ALT2_resultFemale, value.name = "ELRP")

rm(ALT2_LTFemale, ForecastLFPRFemale, ALT2_dataFemale)

cols <- c("Base Scenario"="#66c2a5","Constant LFPR"="#fc8d62","Constant Mortality"="#8da0cb")
ggplot() + geom_line(data = resultMale, aes(x = variable, y = ELRP, group = 1, colour = "Base Scenario"), size = 0.65) +
  geom_line(data = ALT1_resultMale, aes(x = variable, y = ELRP, group = 1, colour = "Constant LFPR"), size = 0.65) +
  geom_line(data = ALT2_resultMale, aes(x = variable, y = ELRP, group = 1, colour = "Constant Mortality"), size = 0.65) +
  theme_few() + scale_colour_tableau() +
  xlab("Year") + ylab("Male ELRP") +
  scale_x_discrete(breaks = seq(1983, 2098, by = 10)) +
  theme(legend.position = "right", legend.title = element_blank())

ggplot() + geom_line(data = resultFemale, aes(x = variable, y = ELRP, group = 1, colour = "Base Scenario"), size = 0.65) +
  geom_line(data = ALT1_resultFemale, aes(x = variable, y = ELRP, group = 1, colour = "Constant LFPR"), size = 0.65) +
  geom_line(data = ALT2_resultFemale, aes(x = variable, y = ELRP, group = 1, colour = "Constant Mortality"), size = 0.65) +
  theme_few() + scale_colour_tableau() +
  xlab("Year") + ylab("Female ELRP") +
  scale_x_discrete(breaks = seq(1983, 2098, by = 10)) +
  theme(legend.position = "right", legend.title = element_blank())
