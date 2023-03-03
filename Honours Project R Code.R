# Install required Packages
install.packages(c("cowplot", "ggplot2", "readxl", "lattice", "mgcv", "nlme",
                   "MuMIn", "forecast", "writexl", "Rmisc"))

# Load required Libraries
library(cowplot)
library(ggplot2)
theme_set(theme_bw())
library(readxl)
library(lattice)
library(nlme)
library(mgcv)
library(MuMIn)
library(forecast)
library(writexl)
library(plyr)
library(dplyr)
library(Rmisc)

# Setting the work directory:

setwd("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF")

# ------ PART 1: PERIODS ------
# Importing data:

Incubation <- read_xlsx("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/EXTRACTED DATA.xlsx", sheet = 1)
Nestling <- read_xlsx("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/EXTRACTED DATA.xlsx", sheet = 2)

IncubData<- summarySE(Incubation,measurevar="INCUB_PERIOD", groupvars=("INCUB_YEAR"))
IncubData

Incub.plot <- ggplot(data=Incubation, aes(x=INCUB_YEAR, y=INCUB_PERIOD))+
  xlab("Year")+ylab("Incubation Period (days)")+
  geom_point(shape=21, fill="grey80", color="grey80",size=0.6)+
  stat_smooth(color="grey50", fill="grey70", size=0.8, method="loess")+
  geom_point(data=IncubData, shape=21, fill="grey32", color="grey32", size=2)+
  geom_line(data=IncubData, color="grey32", size=0.8)+
  xlim(1994.5,2021.5)+
  ylim(0,60)+
  geom_errorbar(data=IncubData, aes(ymin=INCUB_PERIOD-ci, ymax=INCUB_PERIOD+ci),
                color="grey32", width=.1)+
  theme_bw()+
  theme(legend.position="bottom",
        axis.text.x = element_text(color="black", size=10),
        axis.title.x = element_text(color="black", size=10,
                                    face="bold"),
        axis.text.y = element_text(color="black", size=10),
        axis.title.y = element_text(color="black", size=10,
                                    face="bold"))
Incub.plot

NestlData<- summarySE(Nestling,measurevar="NESTL_PERIOD", groupvars=("NESTL_YEAR"))
NestlData

Nestl.plot <- ggplot(data=Nestling, aes(x=NESTL_YEAR, y=NESTL_PERIOD))+
  xlab("Year")+ylab("Nestling Period (days)")+
  geom_point(shape=21, fill="grey80", color="grey80",size=0.6)+
  stat_smooth(color="grey50", fill="grey70", size=0.8, method="loess")+
  geom_point(data=NestlData, shape=21, fill="grey32", color="grey32", size=2)+
  geom_line(data=NestlData, color="grey32", size=0.8)+
  xlim(1994.5,2021.5)+
  ylim(0,60)+
  geom_errorbar(data=NestlData, aes(ymin=NESTL_PERIOD-ci, ymax=NESTL_PERIOD+ci),
                color="grey32", width=.1)+
  theme_bw()+
  theme(legend.position="bottom",
        axis.text.x = element_text(color="black", size=10),
        axis.title.x = element_text(color="black", size=10,
                                    face="bold"),
        axis.text.y = element_text(color="black", size=10),
        axis.title.y = element_text(color="black", size=10,
                                    face="bold"))
Nestl.plot

# ------ GLS Models ------
GLS.Incubation <- gls(data = Incubation, INCUB_PERIOD ~ INCUB_YEAR)
summary(GLS.Incubation)
# AIC value is 5108.257

GLS.Nestling <- gls(data = Nestling, NESTL_PERIOD ~ NESTL_YEAR)
summary(GLS.Nestling)
# AIC value is 3251.751

# ------ ACF plots for GLS Models ------
Residuals.GLS.Incubation <- residuals(GLS.Incubation, type = "normalized")
acf(Residuals.GLS.Incubation, main = "Autocorrelation plot for Residuals", 
    sub = "Incubation Period vs Incubation Year")

Residuals.GLS.Nestling <- residuals(GLS.Nestling, type = "normalized")
acf(Residuals.GLS.Nestling)

# ------ Incubation ARMA Models ------
inc.Mod1 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=0, q=1))
inc.Mod2 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=1, q=0))
inc.Mod3 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=0, q=2))
inc.Mod4 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=2, q=0))
inc.Mod5 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=1, q=2))
# inc.Mod6 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=2, q=1))
# inc.Mod7 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=2, q=2))
inc.Mod8 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=0, q=3))
inc.Mod9 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=3, q=0))
inc.Mod10 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=1, q=3))
inc.Mod11 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=3, q=1))
inc.Mod12 <- update(GLS.Incubation, corr=corARMA(c(0.3, 0.3, 0.3, 0.3, 0.3),form=~1|INCUB_YEAR, p=2, q=3))
inc.Mod13 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=3, q=2))
inc.Mod14 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=3, q=3))
inc.Mod15 <- update(GLS.Incubation, corr=corARMA(form=~1|INCUB_YEAR, p=1, q=1))

# inc.Mod6, inc.Mod7, and inc.Mod15 were taken out due to models exhibiting the same errors:
# Error in `coef<-.corARMA`(`*tmp*`, value = value[parMap[, i]]) : 
#   Coefficient matrix not invertible

Output.Incubation <- model.sel(GLS.Incubation,inc.Mod2,inc.Mod3,inc.Mod4,inc.Mod5,
                               inc.Mod8,inc.Mod9,inc.Mod10,inc.Mod11,inc.Mod12,
                               inc.Mod13,inc.Mod14)
Output.Incubation
# inc.Mod14 with AIC 4966.5 (AICc)

# ------ Nestling ARMA Models ------
nes.Mod1 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=0, q=1))
nes.Mod2 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=1, q=0))
nes.Mod3 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=0, q=2))
nes.Mod4 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=2, q=0))
nes.Mod5 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=1, q=2))
nes.Mod6 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=2, q=1))
nes.Mod7 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=2, q=2))
nes.Mod8 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=0, q=3))
nes.Mod9 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=3, q=0))
nes.Mod10 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=1, q=3))
nes.Mod11 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=3, q=1))
nes.Mod12 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=2, q=3))
nes.Mod13 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=3, q=2))
nes.Mod14 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=3, q=3))
nes.Mod15 <- update(GLS.Nestling, corr=corARMA(form=~1|NESTL_YEAR, p=1, q=1))

Output.Nestling <- model.sel(GLS.Nestling,nes.Mod1, nes.Mod2,nes.Mod3,nes.Mod4,nes.Mod5,
                             nes.Mod6,nes.Mod7,nes.Mod8,nes.Mod9,nes.Mod10,nes.Mod11,
                             nes.Mod12,nes.Mod13,nes.Mod14, nes.Mod15)
Output.Nestling
# nes.Mod15 with AIC 3084.0 (AICc)

# ------ Auto-Arima ------
auto.arima(residuals(GLS.Incubation))
# Incubation Model AIC = 4993.63 (4966.5 above)
auto.arima(residuals(GLS.Nestling))
# Nestling Model AIC = 3102.62 (3084.8 above)

# HM: This is really just a check to see if the ARMA structure found above is the
# HM: same as the one suggested here. For Incubation, it is not. I'm not sure why. 
# HM: But this isn't a step that should stop you, I use it out of curiosity only.
# HM: For nestling period they are the same.

# ------ ACF Plots for Autocorrelated Models ------
Residuals.inc.Mod14 <- residuals(inc.Mod14, type = "normalized")
acf(Residuals.inc.Mod14, main = "Autocorrelation plot for Residuals", 
    sub = "Autocorrelated Structure (Incubation Period)")

Residuals.nes.Mod7 <- residuals(nes.Mod7, type = "normalized")
acf(Residuals.nes.Mod7, main = "Autocorrelation plot for Residuals", 
    sub = "Autocorrelated Structure (Nestling Period)")

# ------ Standardizing the Variables ------
z.Incubation.Year <- (Incubation$INCUB_YEAR-mean(Incubation$INCUB_YEAR))/0.5*sd(Incubation$INCUB_YEAR)
z.Incubation.Period <-(Incubation$INCUB_PERIOD-mean(Incubation$INCUB_PERIOD))/0.5*sd(Incubation$INCUB_PERIOD)
z.Nestling.Year <- (Nestling$NESTL_YEAR-mean(Nestling$NESTL_YEAR))/0.5*sd(Nestling$NESTL_YEAR)
z.Nestling.Period <-(Nestling$NESTL_PERIOD-mean(Nestling$NESTL_PERIOD))/0.5*sd(Nestling$NESTL_PERIOD)

# ------ Candidate Model Set ------
# First with Incubation:
inc.Candidate.Mod1 <- gls(data = Incubation, INCUB_PERIOD ~ z.Incubation.Year, 
                          corr = corARMA(form =~ 1 | INCUB_YEAR, p=3, q=3), method="ML")
inc.Candidate.Mod2 <- gls(data = Incubation, INCUB_PERIOD ~ 1, 
                           corr = corARMA(form =~ 1 | INCUB_YEAR, p=3, q=3), method="ML")

# Then with Nestling:
nes.Candidate.Mod1 <- gls(data = Nestling, NESTL_PERIOD ~ z.Nestling.Year, 
                          corr = corARMA(form =~ 1 | NESTL_YEAR, p=2, q=2), method="ML")
nes.Candidate.Mod2 <- gls(data = Nestling, NESTL_PERIOD ~ 1, 
                          corr = corARMA(form =~ 1 | NESTL_YEAR, p=2, q=2), method="ML")

# ------ Generating Tables ------
# For Incubation:
Output.Incubation2 <- model.sel(inc.Candidate.Mod1, inc.Candidate.Mod2)
Output.Incubation2


# For Nestling:
Output.Nestling2 <- model.sel(nes.Candidate.Mod1, nes.Candidate.Mod2)
Output.Nestling2

install.packages("openxlsx") # need "openxlsx" to make multiple sheets
library(openxlsx)

ExcelTable1Names <- list("Incubation" = Output.Incubation2, "Nestling" = Output.Nestling2)
openxlsx::write.xlsx(ExcelTable1Names, file = "Table 1.xlsx")

# ------ Model Averaging ------
# For Incubation: 
inc.ModelAverage <- model.avg(Output.Incubation2)
inc.Avg.Out <- summary(inc.ModelAverage)$coefmat.subset
inc.Avg.Out[,1:5] <- round(inc.Avg.Out[,1:5],3)

# For Nestling:
nes.ModelAverage <- model.avg(Output.Nestling2)
nes.Avg.Out <- summary(nes.ModelAverage)$coefmat.subset



ExcelTable2Names <- list("Incubation" = inc.Avg.Out, "Nestling" = nes.Avg.Out)
openxlsx::write.xlsx(ExcelTable2Names, file = "Table 2.xlsx")

# ------ PART 2: ENVIRONMENTAL VARIABLES ------
# Import Environmental Data:
Env.PreyQuality <- read_xlsx("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/Environmental Variables.xlsx", sheet = 1)
Env.NAO <- read_xlsx("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/Environmental Variables.xlsx", sheet = 2)
Env.WinterSST <- read_xlsx("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/Environmental Variables.xlsx", sheet = 3)
Env.SpringSST <- read_xlsx("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/Environmental Variables.xlsx", sheet = 4)
Env.SummerSST <- read_xlsx("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/Environmental Variables.xlsx", sheet = 5)
Env.SpringAT <- read_xlsx("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/Environmental Variables.xlsx", sheet = 6)
Env.SpringRain <- read_xlsx("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/Environmental Variables.xlsx", sheet = 7)
Env.SummerAT <- read_xlsx("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/Environmental Variables.xlsx", sheet = 8)
Env.SummerRain <- read_xlsx("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/Environmental Variables.xlsx", sheet = 9)
Env.Variables <- read.csv("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/EnvCond.csv")
# 1 = Prey Quality | 2 = NAO | 3 = Winter SST | 4 = Spring SST | 5 = Summer SST | 6 = Spring AT |
# 7 = Spring Rain | 8 = Summer AT | 9 = Summer Rain
Env.Variables <- read.csv("/Users/francissoriano/Desktop/BIOL4090/PROGRAMMING STUFF/EnvCond.csv")

# Need to put all data in one data frame:
inc.Env.SpringSST <- merge(Incubation, Env.SpringSST)


nes.Env.SummerAT <- merge(Nestling, Env.SummerAT)


# ------ GLS Models -----
# Incubation Period
inc.Env.Mod1 <- gls(data = inc.Env.SpringSST, INCUB_PERIOD ~ SprSST, 
                    corr = corARMA(c(0.1,0.1,0.1,0.1,0.1,0.1),form =~ 1 | INCUB_YEAR, p=3, q=3), method="ML")
inc.Env.Mod2 <- gls(data = Env.Var1)
inc.Env.Mod3
inc.Env.Mod4
inc.Env.Mod5
inc.Env.Mod6 

# Nestling Period
inc.Env.Mod1 <- gls(data = nes.Env.SummerAT, NESTL_PERIOD ~ SumAT, 
                    corr = corARMA(c(0.2, 0.4, 0.1, 0.5), form =~ 1 | NESTL_YEAR, 
                                   p=2, q=2), method="ML",
                    na.action = "na.omit")



