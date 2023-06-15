#Script to process comparison of orthographic forms
#Finds common lemma in two datasets (extracted from TXM or other sources), one with one orthographic variant and the other without it. Can be adapted for more than two datasets.
#Timothée Premat | 2022-11-18

library('readxl')
library('readr')
library('dplyr')
library('tidyverse')
library('ggplot2')
library('writexl')
library('stringr')
library('ggpubr')

##Define the variable you want to investigate, in order to maintain traceability
my_var <- readline(prompt="Enter the name of the POS you want to investigate: ")

#Chose one of the following depending on wether you need to filter by gender
#and number, and by case
source("pre-treatment_R_form_SRC.R")
source("pre-treatment_R_form_SRC_gender_filtered.R")
source("pre-treatment_R_form_SRC_case_filtered.R")
