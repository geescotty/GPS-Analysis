### Day Predictions ###
library(ggplot2)
library(dplyr)
library(plotly)
library(htmlwidgets)
library(hms)
library(readxl)
library(condformat)
drills<-c("DrillName1","DrillName2") 
daytype<-"Daytype"

round_mean <- function(x) {
  round(mean(x), digits = 0) 
}

round_mean2 <- function(x) {
  round(mean(x), digits = 1)  
}


setwd("DataLocation")
#datadump<-subset(datadump,datadump$Season=="2025/26")
sched<-read.csv("Schedule.csv") # day type
sched$active_days<- as.Date(sched$active_days, format = "%d/%m/%Y")
datadump$`Total Time`<-as.numeric(as_hms(datadump$`Total Time`))/60
datadump$`Session Date`<-as.Date(datadump$`Session Date`)
datadump$`Total Distance`<-round(datadump$`Total Distance`,digits=0)
datadump$`Player Position` <- factor(datadump$`Player Position`, levels = c("Prop","Hooker","Second Row","Back Row","Scrum Half","Out Half","Centre","Back Three"))

datadump$PositionGroup<-ifelse(datadump$`Player Position` %in% c("Prop","Hooker","Back Row","Second Row"),"Forward","Back")
colnames(datadump)[colnames(datadump) == "Total Distance"] <- "Total Distance"
colnames(datadump)[colnames(datadump) == "Player Name"] <- "Name"
colnames(datadump)[colnames(datadump) == "HML Distance"] <- "HMLD"
colnames(datadump)[colnames(datadump) == "Position"] <- "Position2"
colnames(datadump)[colnames(datadump) == "Player Position"] <- "Position"
colnames(datadump)[colnames(datadump) == "High Speed Running (Relative)"] <- "VHSR >70%"
colnames(datadump)[colnames(datadump) == "Accelerations"] <- "Accels"
colnames(datadump)[colnames(datadump) == "% Accel GD"] <- "Accels old"
colnames(datadump)[colnames(datadump) == "Accels GD"] <- "Accels old2"
colnames(datadump)[colnames(datadump) == "VHSR GD"] <- "VHSR old2"
colnames(datadump)[colnames(datadump) == "% VHSR GD"] <- "VHSR old"


datadump$MMin<-round(datadump$`Total Distance`/datadump$`Total Time`,digits=1)
datadump$HMLMin<-round(datadump$`HMLD`/datadump$`Total Time`,digits=1)
datadump$AccelMin<-round(datadump$Accels/datadump$`Total Time`,digits=1)
datadump$VHSRMin<-round(datadump$`VHSR >70%`/datadump$`Total Time`,digits=1)
datadump$Initials <- sapply(strsplit(datadump$Name, " "), function(x) paste0(substr(x, 1, 1), collapse = ""))
datadump$`VHSR >70%`<-round(datadump$`VHSR >70%`,digits=0)
datadump$HMLD<-round(datadump$HMLD,digits=0)
datadump<-subset(datadump,datadump$`Session Date`>"2024-07-28") ## season start date or further back if required
datadump <- merge(datadump, sched, by.x = "Session Date", by.y = "active_days", all.x = TRUE)
datadump<-subset(datadump,datadump$`Drill Title`=="drill name you need daily" & datadump$day_type==daytype |datadump$`Drill Title` %in% drills)

### team average ###
setwd("outputlocation")
datasum<-datadump %>%
  group_by(`Drill Title`) %>%
  dplyr::summarise(TotalDistance=mean(`Total Distance`),
                   NWD=mean(`Distance Zone 2 - Zone 6 (Relative)`),
                   HSR= mean(`Distance Zone 4 - Zone 6 (Relative)`),
                   VHSR= mean(`VHSR >70%`),
                   SprintDistance= mean(`Distance Zone 6 (Relative)`),
                   SprintEntries= mean(`Entries Zone 6 (Relative)`),
                   MaxSpeed= max(`Max Speed`),
                   MaxSpeedPercent=max(`Max Speed Percentage`),
                   Accels46=mean(Accels),
                   Accels56= mean(`Accelerations Zone 5 - Zone 6`),
                   Decels=mean(Decelerations),
                   Decels56= mean(`Decelerations Zone 5 - Zone 6`),
                   HMLD=mean(HMLD),
                   HMLEffort=mean(`HML Efforts`),
                   Dmin=mean(`Distance Per Min`),
                   #VHSR_min =max(VHSRMin),
                   #AccelMin=max(AccelMin),
                   #DecelMin=max(DecelMin),
                   time=mean(`Total Time`)
  )
datasum1<-datasum %>%
  dplyr::summarise(TotalDistance=sum(TotalDistance),
                   NWD=sum(NWD),
                   HSR= sum(HSR),
                   VHSR= sum(VHSR),
                   SprintDistance= sum(SprintDistance),
                   SprintEntries= sum(SprintEntries),
                   MaxSpeed= max(MaxSpeed),
                   MaxSpeedPercent=max(MaxSpeedPercent),
                   Accels46=sum(Accels46),
                   Accels56= sum(Accels56),
                   Decels=sum(Decels),
                   Decels56= sum(Decels56),
                   HMLD=sum(HMLD),
                   HMLEffort=mean(HMLEffort),
                   Dmin=mean(Dmin),
                   #VHSR_min =max(VHSRMin),
                   #AccelMin=max(AccelMin),
                   #DecelMin=max(DecelMin),
                   time=sum(time)
  )
datasum1 <- lapply(datasum1, round, digits = 0)

write.csv(datasum1, paste0("Team prediction ",format(Sys.Date(), "%Y-%m-%d"), ".csv"))

## calculate the 70% quantile for the individual
datasum<-datadump %>%
  group_by(`Drill Title`,Name) %>%
  dplyr::summarise(TotalDistance=quantile(`Total Distance`,prob=0.7),
                   NWD=quantile(`Distance Zone 2 - Zone 6 (Relative)`,prob=0.7),
                   HSR= quantile(`Distance Zone 4 - Zone 6 (Relative)`,prob=0.7),
                   VHSR= quantile(`VHSR >70%`,prob=0.7),
                   SprintDistance= quantile(`Distance Zone 6 (Relative)`,prob=0.7),
                   SprintEntries= quantile(`Entries Zone 6 (Relative)`,prob=0.7),
                   MaxSpeed= max(`Max Speed`),
                   MaxSpeedPercent=max(`Max Speed Percentage`),
                   Accels46=quantile(Accels,prob=0.7),
                   Accels56= quantile(`Accelerations Zone 5 - Zone 6`,prob=0.7),
                   Decels=quantile(Decelerations,prob=0.7),
                   Decels56= quantile(`Decelerations Zone 5 - Zone 6`,prob=0.7),
                   HMLD=quantile(HMLD,prob=0.7),
                   HMLEffort=quantile(`HML Efforts`,prob=0.7),
                   Dmin=quantile(`Distance Per Min`,prob=0.7),
                   #VHSR_min =max(VHSRMin),
                   #AccelMin=max(AccelMin),
                   #DecelMin=max(DecelMin)
  )


### calculate the positional 70% quartile for the drills

datasumpos<-datadump %>%
  group_by(`Drill Title`,Position) %>%
  dplyr::summarise(TotalDistance=quantile(`Total Distance`,prob=0.7),
                   NWD=quantile(`Distance Zone 2 - Zone 6 (Relative)`,prob=0.7),
                   HSR= quantile(`Distance Zone 4 - Zone 6 (Relative)`,prob=0.7),
                   VHSR= quantile(`VHSR >70%`,prob=0.7),
                   SprintDistance= quantile(`Distance Zone 6 (Relative)`,prob=0.7),
                   SprintEntries= quantile(`Entries Zone 6 (Relative)`,prob=0.7),
                   MaxSpeed= max(`Max Speed`),
                   MaxSpeedPercent=max(`Max Speed Percentage`),
                   Accels46=quantile(Accels,prob=0.7),
                   Accels56= quantile(`Accelerations Zone 5 - Zone 6`,prob=0.7),
                   Decels=quantile(Decelerations,prob=0.7),
                   Decels56= quantile(`Decelerations Zone 5 - Zone 6`,prob=0.7),
                   HMLD=quantile(HMLD,prob=0.7),
                   HMLEffort=quantile(`HML Efforts`,prob=0.7),
                   Dmin=quantile(`Distance Per Min`,prob=0.7),
                   #VHSR_min =max(VHSRMin),
                   #AccelMin=max(AccelMin),
                   #DecelMin=max(DecelMin)
  )

write.csv(datasumpos, paste0("Position prediction ",format(Sys.Date(), "%Y-%m-%d"), ".csv"))

datasumplayer<-datasum %>%
  group_by(Name) %>%
  dplyr::summarise(TotalDistance=sum(TotalDistance),
                   NWD=round(sum(NWD),digits=0),
                   HSR= round(sum(HSR),digits=0),
                   VHSR= round(sum(VHSR),digits=0),
                   SprintDistance= round(sum(SprintDistance),digits=0),
                   SprintEntries= round(sum(SprintEntries),digits=0),
                   MaxSpeed= round(max(MaxSpeed),digits=0),
                   MaxSpeedPercent=round(max(MaxSpeedPercent),digits=0),
                   Accels46=round(sum(Accels46),digits=0),
                   Accels56= round(sum(Accels56),digits=0),
                   Decels=round(sum(Decels),digits=0),
                   Decels56= round(sum(Decels56),digits=0),
                   HMLD=round(sum(HMLD),digits=0),
                   HMLEffort=round(sum(HMLEffort),digits=0),
                   Dmin=round(max(Dmin),digits=1),
                   #VHSR_min =max(VHSRMin),
                   #AccelMin=max(AccelMin),
                   #DecelMin=max(DecelMin)
  )

write.csv(datasumplayer, paste0("Player prediction ",format(Sys.Date(), "%Y-%m-%d"), ".csv"))
