library(readxl)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

rm(list = ls())

setwd("~/DATA-331/FinalDataProject-Ladybug/data")

#made data frames that contain all the ORIGINAL data
summarized_lb_data <- read_excel('Ladybug Data.xlsx', .name_repair = "universal")
all_lb_data <- read.csv("Scan LadyBug Data.csv")

#made a temporary data frame with important variables from all_lb_data that I will use for analysis
temp_lb_data <- all_lb_data %>%
  dplyr::select(id, catalogNumber, kingdom, phylum, class, order, family, scientificName, genus, 
         specificEpithet, year, stateProvince, county, dateIdentified)


#Cleaning all the data in temp_lb_data
#removing null values, empty values, renaming, and overall minor changes.
#also preparing for a left join
temp_lb_data[temp_lb_data == 'Cycloneda munda	AUGIEENTB0000141'] <- 'AUGIEENTB0000141'
temp_lb_data[temp_lb_data == 'coccinellidae'] <- 'Coccinellidae'
temp_lb_data[temp_lb_data == 'IL'] <- 'Illinois'
temp_lb_data[temp_lb_data == 'iL'] <- 'Illinois'
temp_lb_data[temp_lb_data == 'Il'] <- 'Illinois'
temp_lb_data[temp_lb_data == 'IA'] <- 'Iowa'
temp_lb_data[temp_lb_data == 'ia'] <- 'Iowa'
temp_lb_data[temp_lb_data == 'Ia'] <- 'Iowa'
temp_lb_data[temp_lb_data == 'harmonia axyridis'] <- 'Harmonia axyridis'

temp_lb_data <- subset(temp_lb_data, kingdom != '')
temp_lb_data <- subset(temp_lb_data, genus != '')
temp_lb_data <- subset(temp_lb_data, county != '')
temp_lb_data <- subset(temp_lb_data, year == '2021')

summarized_lb_data <- summarized_lb_data %>% 
  rename('catalogNumber' = 'SCAN.CODE')

#left joining the scan data to the temp data and getting rid of null values
clean_lb_data <- temp_lb_data %>%
  left_join(summarized_lb_data, by=c('catalogNumber')) %>%
  na.omit(temp_lb_data)


#Intro to basic data
dfSpeciesCount <- clean_lb_data %>%
  dplyr::select(stateProvince, scientificName) %>%
  count(scientificName)%>%
  rename('count' = 'n')

#intro plot
plSpeciesCount<-ggplot(dfSpeciesCount,
                         aes(scientificName,count)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +  
  geom_text(aes(label = signif(count)), nudge_y = 4) 
theme <- theme(axis.text.x = element_text(face = "bold", color = "black", size = 5, angle = 25))
plSpeciesCount + theme + labs(y = "Count of Ladybugs", x = "Ladybug Species")


#Question 1: 
#Is there a proportional difference in the number of species 
  #found in certain areas (mowed grass, agriculture, industrial, etc) between Illinois and Iowa?

#Quick rename cleaning
#clean_lb_data[clean_lb_data == 'Lp-PR-5'] <- 'LP-PR'

#data frame for q1
dfSpeciesCountStateArea <- clean_lb_data %>%
  dplyr::group_by(stateProvince, plot)%>%
  rename('area' = 'plot')
dfSpeciesCountStateArea$area <- strtrim(dfSpeciesCountStateArea$area, 5)
dfSpeciesCountStateArea <- subset(dfSpeciesCountStateArea, area != 'Lp-PR')
dfSpeciesCountStateArea <- dfSpeciesCountStateArea %>%
  count(area)%>%
  rename('ladyBugCount' = 'n')

#graph for q1
plSpeciesCountStateArea<-ggplot(dfSpeciesCountStateArea,
                         aes(area,ladyBugCount, fill = stateProvince)) +
  geom_bar(stat = "identity", position = 'dodge')
plSpeciesCountStateArea + labs(y = "Count of Ladybugs", x = "Area Plot", fill = "State")
###q1 done

#Question 2:
#Was there a time span where certain ladybug species may have 
  #been more active meaning more of them were found?

#data frame for q2
dfLadyBugDates <- clean_lb_data %>%
  dplyr::select(date, scientificName)

#graph for q1
plLadyBugDates <- ggplot(dfLadyBugDates, aes(x=date, y=scientificName)) +
 geom_point(aes(color=scientificName), show.legend = FALSE)
theme <- theme(axis.text.y = element_text(face = "bold", color = "black"), legend.position = "none")
plLadyBugDates + theme + labs(y = "Ladybug Species", x = "Date Collected")


###q2 done


###df for q3 & q4
dfCollector<- clean_lb_data %>%
  dplyr::select(collector, scientificName, plot)

#cleaning for q3 & q4
#J.Hughes name change
dfCollector[dfCollector == 'J Hughes'] <- 'J. Hughes'
dfCollector[dfCollector == 'J. Hughees'] <- 'J. Hughes'
dfCollector[dfCollector == 'j. hughes'] <- 'J. Hughes'
dfCollector[dfCollector == 'j. Hughes'] <- 'J. Hughes'
dfCollector[dfCollector == 'J. hughes'] <- 'J. Hughes'
dfCollector[dfCollector == 'jack hughes'] <- 'J. Hughes'
dfCollector[dfCollector == 'Jack Hughes'] <- 'J. Hughes'

#M. Gorsegner name change
dfCollector[dfCollector == 'm gorsegner'] <- 'M. Gorsegner'
dfCollector[dfCollector == 'm. gorsegner'] <- 'M. Gorsegner'
dfCollector[dfCollector == 'M. gorsegner'] <- 'M. Gorsegner'
dfCollector[dfCollector == 'M.Gorsegner'] <- 'M. Gorsegner'
dfCollector[dfCollector == 'Marissa Gorsegner'] <- 'M. Gorsegner'

#O. Ruffato name change
dfCollector[dfCollector == 'o. ruffatto'] <- 'O. Ruffatto'
dfCollector[dfCollector == 'O. ruffatto'] <- 'O. Ruffatto'
dfCollector[dfCollector == 'o. ruffattto'] <- 'O. Ruffatto'
dfCollector[dfCollector == 'Olivia Ruffatto'] <- 'O. Ruffatto'
dfCollector[dfCollector == 'OliviaRuffatto'] <- 'O. Ruffatto'

#V. Cervantes name change
dfCollector[dfCollector == 'v cervantes'] <- 'V. Cervantes'
dfCollector[dfCollector == 'v. cervantes'] <- 'V. Cervantes'
dfCollector[dfCollector == 'V. cervantes'] <- 'V. Cervantes'
dfCollector[dfCollector == 'V.Cervantes'] <- 'V. Cervantes'
dfCollector[dfCollector == 'Veronica Cervantes'] <- 'V. Cervantes'
dfCollector[dfCollector == 'Veronica Cervatnes'] <- 'V. Cervantes'

#Single data name change
dfCollector[dfCollector == 'Lp-PR-5'] <- 'LP-PR'

#strip string for just simple area
dfCollector$plot <- strtrim(dfCollector$plot, 5)


#Question 3:
#Is the distribution of the type of Ladybugs collected different for each Collector?

#df mutate
dfCollectorSpecies<- dfCollector %>%
  count(collector, scientificName)%>%
  rename('count' = 'n')

#plot for q3
plCollector <- ggplot(dfCollectorSpecies, aes(x = collector, y = count, fill = scientificName)) +
  geom_bar(stat = "identity", position = "dodge")
plCollector + labs(y = "Ladybug Count", x = "Name of Collector", fill = "Ladybug Species")

###q3 done

#Questions 4:
#Is the distribution of the area the Ladybugs were found in different for each Collector?

#df for q4
dfCollectorArea <- dfCollector %>%
  count(collector, plot) %>%
  rename('count' = 'n')

#plot for q4
plCollector <- ggplot(dfCollectorArea, aes(x = collector, y = count, fill = plot)) +
  geom_bar(stat = "identity", position = "dodge")
plCollector + labs(y = "Area Frequency", x = "Name of Collector", fill = "Area")

###q4 done














