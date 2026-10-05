#Import and tidy water quality data


#Import and tidy fire water quality data


library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(colorspace)
library(readxl)

# Import Data -------------------------------------------------------------

#work links
LIMSP_Provisional_Data <- read_excel("./LIMSP Provisional Data.xlsx")



# Tidy Data ---------------------------------------------------------------

LIMSP_Provisional_Data_Tidy <- LIMSP_Provisional_Data %>%
select(SAMPLE_ID,STATION,MATRIX,COLLECT_METHOD,SAMPLE_TYPE,COLLECT_DATE,DEPTH,TEST_NAME,VALUE,UNITS, SAMP_COMMENT_NR) %>%  
mutate(TEST_NAME=if_else(TEST_NAME=="OPO4","SRP",TEST_NAME)) %>%                                                         #rename OPO4 to SRP
mutate(Treatment=case_when(str_detect(STATION,"Untreated")~"Untreated",                                                  #add treatment variable based on plot name
                           str_detect(STATION,"Burn_Herb")~"Burn/Herbicide",                  
                           STATION %in% c("Burn_A","Burn_B","Burn_C")~"Burn",
                           str_detect(STATION,"Herbicide")~"Herbicide",
                           TRUE ~ NA)) %>%
mutate(STATION=ifelse(str_detect(STATION,"Burn_Herb"),str_replace(STATION,"Burn_Herb","Burn/Herbicide"),STATION)) %>%    #create label friendly names for plots
mutate(`Burn Day`= as.numeric(difftime(date(COLLECT_DATE),date("2025-04-10 14:00:00 UTC"),units="day")))                 #calculate days to burn

#Calculate DOP and PP 
DOP_PP <- LIMSP_Provisional_Data_Tidy %>%
filter(TEST_NAME %in% c("TDPO4","SRP","TPO4"),COLLECT_METHOD=="GP")  %>%   #Select analytes needed for calculation
pivot_wider(names_from = "TEST_NAME",values_from = "VALUE")  %>%           #Pivot to wide format
filter(!is.na(TDPO4)) %>%                                                  #Remove NA data  
rowwise() %>%                                                              
mutate(DOP=TDPO4-SRP,PP=TPO4-TDPO4) %>%                                    #Make DOP calulation
pivot_longer(names_to = "TEST_NAME",values_to = "VALUE",12:16) %>%         #pivot back to long format
filter(TEST_NAME %in% c("PP","DOP"))                                       #Filter for only DOP and PP

#Add DOP to dataset 
LIMSP_Provisional_Data_Tidy <- bind_rows(LIMSP_Provisional_Data_Tidy,DOP_PP)   #join DOP and PP back with other analytes

write_csv(LIMSP_Provisional_Data_Tidy ,"./LIMSP_Provisional_Data_Tidy.csv")    #write CSV 


