##############################NPRB Sablefish Rearing Experiments: USGS Starvation Resiliency
install.packages("readxl")
install.packages("tidyverse")

library(here)
library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)

getwd()
# here("data", "2022_rearing_metadata_sampling_spreadsheet.csv")
# list.files("data")
# sab <- read.csv(here("data", "2022_rearing_metadata_sampling_spreadsheet.csv"))

setwd("D:/2022_Sablefish_rearing/Sablefish_starvation_experiments")
sab <- readxl::read_excel("D:/2022_Sablefish_rearing/Sablefish_starvation_experiments/data/2022_rearing_metadata_sampling_spreadsheet.xlsx")

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
# sab.fi$Date=mdy(sab.fi$Date)  
names(sab.fi)

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

###Feeding incidence ggplot #####
sab.fi_df<-sab.fi %>% dplyr::mutate(Treatment.Group = case_when(startsWith(as.character(Treatment.Tank), "A") ~ "A",
                                                     TRUE ~ "B"),
                                    Treatment.Type = case_when(endsWith(as.character(Treatment.Tank), "1") ~ "Fed",
                                                TRUE ~ "Starved"))
class(sab.fi_df$Date)
summary(sab.fi_df)
p <- ggplot()+
  geom_point(data=sab.fi_df,
             aes(x=Date, y=Feeding.Incidence, fill=Treatment.Type), alpha=1, shape=21, size=4) + 
  geom_point(data=sab.fi_df %>% dplyr::filter(Treatment.Type == "Fed"),
             aes(x=Date, y=FI.Threshold, group=Treatment.Type), alpha=1, shape=4, size=3,color="red") +
  geom_smooth(data=sab.fi_df,
              aes(x=Date, y=Feeding.Incidence, fill=Treatment.Type))+
  scale_x_datetime(name="",date_breaks = "2 day",date_labels="%b-%d")+
  scale_y_continuous(name="Feeding Incidence")+
  theme_bw()+
  theme(axis.text.x = element_text(size=12),  # increase x-axis text
        axis.text.y = element_text(size=12), # increase y-axis text
        axis.title.x = element_text(size=12), # increase x-axis labels size
        axis.title.y = element_text(size=12), # increase y-axis labels size
        panel.background = element_blank(), 
        # panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.text = element_text(colour="black", size=12))+
  guides(fill = guide_legend(title="Treatment Group", override.aes = list(size = 3)))
p <- p + facet_wrap( ~ Treatment.Group, ncol=2)
windows();p


### Wet weight
colnames(sab)
names(sab) <- gsub("(", ".", names(sab),fixed = TRUE)
names(sab) <- gsub(")", ".", names(sab))

sab.ww=sab%>%
    filter(!is.na(Wet.Weight..g.))%>%
    as.data.frame()

# sab.ww$Date=mdy(sab.ww$Date)  

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



###wet weight ggplot####
sab.ww<-sab %>% dplyr::mutate(Treatment.Group = case_when(startsWith(as.character(Treatment.Tank), "A") ~ "A",
                                                                TRUE ~ "B"),
                                    Treatment.Type = case_when(endsWith(as.character(Treatment.Tank), "1") ~ "Fed",
                                                               TRUE ~ "Starved"))

ww <- ggplot()+
  geom_point(data=sab.ww %>% dplyr::filter(!is.na(Wet.Weight..g.)),
             aes(x=Date, y=as.numeric(Wet.Weight..g.), group=Treatment.Type,fill=Treatment.Type), alpha=1, shape=21, size=2) + 
  geom_smooth(data=sab.ww %>% dplyr::filter(!is.na(Wet.Weight..g.)),
              aes(x=Date, y=as.numeric(Wet.Weight..g.), group=Treatment.Type,fill=Treatment.Type))+
  scale_x_datetime(name="",date_breaks = "2 day",date_labels="%b-%d")+
  scale_y_continuous(name="Wet Weight (g)", limits=c(0,0.02))+
  theme_bw()+
  theme(axis.text.x = element_text(size=12),  # increase x-axis text
        axis.text.y = element_text(size=12), # increase y-axis text
        axis.title.x = element_text(size=12), # increase x-axis labels size
        axis.title.y = element_text(size=12), # increase y-axis labels size
        panel.background = element_blank(), 
        # panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank(),
        legend.text = element_text(colour="black", size=12))+
  guides(fill = guide_legend(title="Treatment Group", override.aes = list(size = 2)))
ww <- ww + facet_wrap( ~ Treatment.Group, ncol=2)
windows();ww
