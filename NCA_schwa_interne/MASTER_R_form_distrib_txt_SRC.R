library('readxl')
library('readr')
library('dplyr')
library('tidyverse')
library('ggplot2')
library('writexl')
library('stringr')
library('ggpubr')
library('Hmisc')
library('spatstat')
library('weights')
library('Unicode')
library('ggtext')
library('RColorBrewer')
library('sp')
library('ggspatial')
library('rgdal')

italic_p <- u_char_inspect(u_char_from_name("MATHEMATICAL SANS-SERIF ITALIC SMALL P"))["Char"]
rho <- u_char_inspect(u_char_from_name("GREEK SMALL LETTER RHO"))["Char"]
schwa <- u_char_inspect(u_char_from_name("LATIN SMALL LETTER SCHWA"))["Char"]
chi_char <- u_char_inspect(u_char_from_name("GREEK SMALL LETTER CHI"))["Char"]
logiAND <- u_char_inspect(u_char_from_name("AMPERSAND"))["Char"]
schwa_phono <- paste("/",schwa,"/", sep="")

##Define the variable you want to investigate, in order to maintain traceability
my_var <- readline(prompt="Enter the name of the POS you want to investigate, to name the files: ")
my_var2 <- readline(prompt="Enter the context you want to print in the title: ")
my_var2_short <- readline(prompt="Enter the abreviated context you want to print in the title: ")

#For usual analysis
source("A1_import_data.R")
source("ZZ_test_filtering.R")
source("A4_filter_dates.R")
source("A5_diachro_plot.R")
# source("A5_chi-2.R") #Beware: must be parametered by hand!!!
source("A6_save_diachro_plot.R")
# source("A6_chi-2_save.R")
source("A8_carto.R") #Uncomment l.33 if you have enough texts
source("A9_carto_save.R")

#For maps by periods
bound_1b <- readline(prompt="Whats the top boundary of the first period?")
bound_2a <- readline(prompt="Whats the bottom boundary of the second period?")
bound_2b <- readline(prompt="Whats the top boundary of the second period?")
bound_3a <- readline(prompt="Whats the bottom boundary of the third period?")
#bound_3b <- readline(prompt="Whats the top boundary of the third period?")
#bound_4a <- readline(prompt="Whats the bottom boundary of the last period?")
source("A8bis_carto_time.R")
source("A8bis_carto_time_save.R")
