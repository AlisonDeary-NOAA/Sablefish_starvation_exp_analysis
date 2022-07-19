##############################NPRB Sablefish Rearing Experiments: USGS Starvation Resiliency
install.packages("readxl")
install.packages("tidyverse")
install.packages("usethis")

library(here)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(usethis)

#connecting project to R studio
use_git_config(use.name="AlisonDeary-NOAA",user.email="alison.deary@noaa.gov")
#create a personal access token
#May need to re-run code every so often
#usethis::create_github_token()
#gitcreds::gitcreds_set()
#token is: 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



getwd()
 here("data", "2022_rearing_metadata_sampling_spreadsheet.csv")
 list.files("data")
sab <- read.csv(here("data", "2022_rearing_metadata_sampling_spreadsheet.csv"))

sab=read.csv("C:/Users/aldea/Documents/NOAA_FOCI/Sablefish/NPRB Project/2022_rearing/Sablefish_starvation_experiments/data/2022_rearing_metadata_sampling_spreadsheet.csv")

#setwd("C:/Users/aldea/Documents/NOAA_FOCI/Sablefish/NPRB Project/2022_rearing/Sablefish_starvation_experiments/data")
#sab <- readxl::read_excel("D:/2022_Sablefish_rearing/Sablefish_starvation_experiments/data/2022_rearing_metadata_sampling_spreadsheet.xlsx")

#Can not get excel spreadsheet to open
#sab.test <- readxl::read_excel(here("data", "2022_rearing_metadata_sampling_spreadsheet.xlsx"))

names(sab) <- gsub(" ", ".", names(sab))
# sab$Date=mdy(sab$Date)  


head(sab)
names(sab)

#####look just at feeding incidence
colnames(sab)
sab.fi=sab%>%
  filter(!is.na(Feeding.Incidence))%>%
  mutate(FI.Threshold=0.5*(Feeding.Incidence))%>%
  as.data.frame()

class(sab.fi$Treatment.Tank)
class(sab$Date)

sab.fi$Treatment.Tank=as.factor(sab.fi$Treatment.Tank)
###Plotted by treatment
plot(sab.fi$Treatment.Tank,sab.fi$Feeding.Incidence,xlab="Feeding Trial", 
     ylab = "Feeding Incidence (# larvae feeding)")

#Plotted by date-
  ###Calculate the Feeding incidence threshold (50% of highest observed feeding incidence) and plot that
      #to visualize when the threshold is reached
sab.fi$Date=mdy(sab.fi$Date)  
names(sab.fi)
unique(sab.fi$Date)

#########Feeding incidence
windows()
par(mfrow=c(1,2))
plot(sab.fi$Date[sab.fi$Treatment.Tank=="B1"],sab.fi$Feeding.Incidence[sab.fi$Treatment.Tank=="B1"],
     xlab="Date", ylab = "Feeding Incidence (# larvae feeding)",pch=16,ylim=c(0,1.1),cex=1.5, main = "B Table")
points(sab.fi$Date[sab.fi$Treatment.Tank=="B2"],sab.fi$Feeding.Incidence[sab.fi$Treatment.Tank=="B2"],pch=16,
       col="blue",cex=1.5)
points(sab.fi$Date[sab.fi$Treatment.Tank=="B1"],sab.fi$FI.Threshold[sab.fi$Treatment.Tank=="B1"],pch=3,
       col="red",cex=1.5)

plot(sab.fi$Date[sab.fi$Treatment.Tank=="A1"],sab.fi$Feeding.Incidence[sab.fi$Treatment.Tank=="A1"],
     xlab="Date", ylab = "Feeding Incidence (# larvae feeding)",pch=19,ylim=c(0,1.1),cex=1.5, main = "A Table")
points(sab.fi$Date[sab.fi$Treatment.Tank=="A2"],sab.fi$Feeding.Incidence[sab.fi$Treatment.Tank=="A2"],pch=19,
       col="blue",cex=1.5)
points(sab.fi$Date[sab.fi$Treatment.Tank=="A1"],sab.fi$FI.Threshold[sab.fi$Treatment.Tank=="A1"],pch=3,
       col="red",cex=1.5)

### Wet weight
colnames(sab)
names(sab) <- gsub("(", ".", names(sab),fixed = TRUE)
names(sab) <- gsub(")", ".", names(sab))

sab.ww=sab%>%
    filter(!is.na(Wet.Weight..g.))%>%
    as.data.frame()

 sab.ww$Date=mdy(sab.ww$Date)  

windows()
par(mfrow=c(1,2))
plot(sab.ww$Date[sab.ww$Treatment.Tank=="B1"],sab.ww$Wet.Weight..g.[sab.ww$Treatment.Tank=="B1"],
     xlab="Date", ylab = "Wet Weight (g)",pch=16,ylim=c(0,0.019),cex=1.5, main = "B Table")
points(sab.ww$Date[sab.ww$Treatment.Tank=="B2"],sab.ww$Wet.Weight..g.[sab.ww$Treatment.Tank=="B2"],pch=16,
       col="blue",cex=1.5)
range(na.omit(sab.ww$Wet.Weight..g.))

plot(sab.ww$Date[sab.ww$Treatment.Tank=="A1"],sab.ww$Wet.Weight..g.[sab.ww$Treatment.Tank=="A1"],
     xlab="Date", ylab = "Wet Weight (g)",pch=19,ylim=c(0,0.019),cex=1.5, main = "A Table")
points(sab.ww$Date[sab.ww$Treatment.Tank=="A2"],sab.ww$Wet.Weight..g.[sab.ww$Treatment.Tank=="A2"],pch=19,
       col="blue",cex=1.5)


