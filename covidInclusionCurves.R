
#---------------------------------------------
#------------------------------------------
#Programme cr?? par Jacquinot le 14/09/2020 
#COVIDICUS : Cr?ation de courbes d'inclusion
#------------------------------------------
# Clear environment
rm(list = ls())

# Load packages
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(EpiCurve)
#library(openxlsx)
library(stringr)

###Importation des donn?es 

covidicus <- read_delim("ANSI.csv", ";", escape_double = FALSE, col_types = cols(EXTRACTION_DATE = col_character()),
                      
                        trim_ws = TRUE)

##Cr?ation une table contenant uniquement des suivis

covidsuiv=covidicus%>%mutate(EXTRACTION_DATE=substr(as.character(EXTRACTION_DATE),5, 15)) %>%select(SUBJECT_REF,SITE_ID,INC_CENTER,EXTRACTION_DATE,INC_DATE)

covidsuiv$EXTRACTION_DATE=as.Date(covidsuiv$EXTRACTION_DATE,"%m/%d/%Y")
covidsuiv$INC_DATE=dmy(as.character(covidicus$INC_DATE))
#covidsuiv$INC_DATE=as.Date(covidsuiv$INC_DATE)
#covidsuiv$diff=(covidsuiv$EXTRACTION_DATE-covidsuiv$INC_DATE)/365.5

#structure of data base
str(covidsuiv)
#Test
ggplot(covidsuiv) +
  geom_histogram(aes(x = INC_DATE, fill = "dodgerblue3")) +
  scale_x_date(labels = date_format("%m %d %Y"), date_breaks = "week") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

#Cumulative case by  week
ggplot(covidsuiv) +
  geom_freqpoly(aes(x = INC_DATE)) +
  scale_x_date(labels = date_format("%d %B %Y"), date_breaks = "week") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))


#------------------------------------------------------#
###   cumulative curves : nb of inclusion by week#####
#______________________________________________________#


covidsuiv <- data.frame(covidsuiv) %>% 
  group_by(table(covidsuiv$INC_DATE)) %>% 
  summarise(covidsuiv$Frequency = table(INC_DATE))


inclusion_curve = ggplot(data = covidsuiv, aes(x = INC_DATE, y = Frequency, fill = "dodgerblue3")) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_y_continuous(breaks = seq(0,160,20), expand = c(0,0)) +
  scale_x_date(breaks = seq(min(covidsuiv$INC_DATE),max(covidsuiv$INC_DATE),by="week"),
               date_labels = "%d/%m/%Y")+
  #scale_x_date(date_labels = "%d/%m/%Y", breaks = date_breaks("1 week")) +#
  scale_fill_manual(values = c("dodgerblue3")) +
  theme_bw(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, size = 10),
        axis.title = element_blank()) +
ggtitle(label = paste0("Number of inclusions per day (N = ",nrow(covidsuiv),"*)"),
          subtitle = paste0("*Including ")) 

#ggtitle(label = paste0("Number of inclusions per day (N = ",N,"*)"),
        #subtitle = paste0("*Including ")) 


inclusion_curve






