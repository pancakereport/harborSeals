library("MARSS")
library(ggplot2)
library("readxl")
library(dplyr)
library(tidyverse)
library(sjPlot) #used at end for 1 plot theme

##%######################################################%##
#                                                          #
####                   data wrangling                   ####
#                                                          #
##%######################################################%##

#2023-12-21
#ALL SITES ALL YEARS !!



## get PRNS data
Phoca <- read_excel("Data/1997_2022_Phocadata.xls")

Phoca <- Phoca[-c(3:4, 8:10)]

#older data
hseal_max_1975_1999 <- read_excel("Data/1975-1999_max_counts.xlsx")

hseal_max_1975_1999$Age <- ifelse(hseal_max_1975_1999$Season =="MOLTING", "MOLTING", hseal_max_1975_1999$Age)

hseal_max_1975_1999_long <- hseal_max_1975_1999 %>% 
  pivot_longer(!c(Year, Age, Season), names_to = "Subsite", values_to = "Count")
hseal_max_1975_1999_long

# remove pre-1996 data since in the modern data file
hseal_max_1975_1995_long <- hseal_max_1975_1999_long %>%
  filter(Year < 1996)



## need some year and julian date fields to plot by year and day of year
library(lubridate)
## tell lubridate the format
Phoca$Date2 <- ymd(Phoca$Date) #separates out month,day and year
## add year column for faceting plots
Phoca$Year <- year(Phoca$Date2)
## get Julian Date (yday) for within year dates
Phoca$Julian <- yday(Phoca$Date2)

#rename adults during molting season "MOLTING"
Phoca$Age <- ifelse(Phoca$Julian > 135, "MOLTING", Phoca$Age)
Phoca$Season <- ifelse(Phoca$Julian <= 135, "PUPPING", "MOLTING")



## Looks like need to remove Dead adult and deadpup
library(plyr)
library(dplyr)

Phoca <- filter(Phoca, Age != "DEADPUP" & Age != "DEADADULT")
unique(Phoca$Age)


## and need to convert HPUP (typo in database!) to PUP
Phoca$Age[Phoca$Age == "HPUP"] <- "PUP"
Phoca$Yearf <- as.factor(Phoca$Year)


#now put files together
Phoca
hseal_max_1975_1995_long

all_data <- bind_rows(hseal_max_1975_1995_long, Phoca)



all_data$Yearf <- as.factor(all_data$Year)
all_data

# Change PR to PRH
unique(all_data$Subsite)

all_data$Subsite <- ifelse(all_data$Subsite == "PR", "PRH", all_data$Subsite)
unique(all_data$Subsite)

#top1 master file (all data)
top1_all_data <- all_data %>% 
  group_by(Year, Subsite, Age) %>%
  slice_max(Count, n = 1) %>%
  filter(Age != "PUP" | Season != "MOLTING") #remove pups counted in early molting season
top1_all_data

top.plot <- ggplot(top1_all_data,
                   aes(Year, Count)) + 
  geom_point() + 
  geom_smooth() + 
  geom_vline(xintercept = c(1988, 1994), lty = 3) +
  ylab("Max Count") +
  #scale_y_continuous(trans='log10') +
  geom_vline(xintercept = 1994, lty = 3) +
  theme_gray(base_size = 14)
  
top.plot + facet_grid(Age ~ Subsite)



## now within year plots to look at breeding and molting season peaks

#Phoca.Adult <- dplyr::filter(Phoca, Age == "ADULT")

seasonal.plot <- ggplot(all_data, aes(Julian, Count, color = Yearf)) + 
  geom_point() + 
  geom_smooth(aes(colour = Yearf))
seasonal.plot + facet_grid(Subsite ~ Age)

#remove Point Bonita and Duxbury PUP data since no or few pups
all_data.MARSS <- subset(top1_all_data, Age != "PUP") # | Subsite != "DR" & Subsite != "PB" & Subsite != "PRH")

#remove Point Bonita and Duxbury PUP data since no or few pups and shorter time series
all_data.MARSS <- subset(all_data.MARSS, Subsite != "DR" & Subsite != "PB")


#all sites start in 1982...so use that
all_data.MARSS <- subset(all_data.MARSS, Year > 1981)



# unique(all_data.MARSS$Subsite)

# remove extra columns
all_data.MARSS <- all_data.MARSS[,c(1, 2, 4,5)]

## log all the counts, # log = ln in R
all_data.MARSS$Count <- log(all_data.MARSS$Count) 

# check no duplicate rows
all_data.MARSS <- distinct(all_data.MARSS)

#make new age-subsite column
all_data.MARSS$Subsite_Age <- paste0(all_data.MARSS$Subsite, "_", all_data.MARSS$Age)

#remove extra columns
all_data.MARSS <- all_data.MARSS[,-c(2:3)]

# #use only data post 1997 ? ------------------------------------
all_data.MARSS_1997_2022 <- all_data.MARSS %>%
                      filter(Year>1997) # if filter 1996, then no BL molt so order off.
# #--------------------------------------------------------------

## now need to make the data wide for MARSS
all_data.MARSS.wide <- all_data.MARSS %>% 
  pivot_wider(names_from = Subsite_Age, values_from = Count)


dat2 <- all_data.MARSS.wide ## for use in SealPopStructure Script

## now use names in the basic MARSS code 
dat <- t(all_data.MARSS.wide)
## OR FOR PUPS
## dat <- t(top1.pup.breed.spread)

#Transpose since MARSS needs time ACROSS columns
years = dat[1,]     # remove years
n = nrow(dat)-1
dat = dat[2:nrow(dat),]
legendnames = (unlist(dimnames(dat)[1]))

