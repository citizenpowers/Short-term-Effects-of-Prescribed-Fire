#this script contains code to create the water qualtiy figures for the manuscript


library(tidyverse)
library(interactions)
library(lme4)
library(lmerTest)
library(broom)
library(cowplot)
library(ggh4x)
library(magick)






# Import Data -------------------------------------------------------------


LIMSP_Provisional_Data_Tidy <-read_csv("LIMSP_Provisional_Data_Tidy.csv")
