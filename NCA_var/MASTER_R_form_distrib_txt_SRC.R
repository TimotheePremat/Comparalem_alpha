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
library('ggpmisc')
library('viridis')

italic_p <- u_char_inspect(u_char_from_name("MATHEMATICAL SANS-SERIF ITALIC SMALL P"))["Char"]
rho <- u_char_inspect(u_char_from_name("GREEK SMALL LETTER RHO"))["Char"]
schwa <- u_char_inspect(u_char_from_name("LATIN SMALL LETTER SCHWA"))["Char"]
chi_char <- u_char_inspect(u_char_from_name("GREEK SMALL LETTER CHI"))["Char"]
logiAND <- u_char_inspect(u_char_from_name("AMPERSAND"))["Char"]


##Define the variable you want to investigate, in order to maintain traceability
my_var <- readline(prompt="Enter the name of the POS you want to investigate, to name the files: ")
my_var2 <- readline(prompt="Enter the name of the POS you want to print as title: ")
my_var_filter <- readline(prompt="Enter the variable you want to use to filter
                          forms. Can be gender_number, or
                          gender_number_case:")
bypass <- readline(prompt="Will you by-pass filtering by lemmas? Answer Y or N:")

#For non contextual analysis
source("A1_import_data.R")
source("A3_post_cleaning.R")
	#Can be by-passed (if all forms are considered the same lemma) by calling instead
	#the following dummy script:
	source("A3_fake_cleaning.R")
source("ZZ_test_filtering_non_contextual.R") #Can be by-passed if not needed: doesn't change variables' name.
source("A4_filter_dates.R")
source("A5_diachro_plot.R")
source("A6_save_diachro_plot.R")

#For contextual analysis
source("A1_import_data_context.R")
source("A3_post_cleaning_context.R")
  #Can be by-passed (if all forms are considered the same lemma) by calling instead
  #the following dummy script:
  source("A3_fake_cleaning_context.R")
source("A4_filter_dates_context.R")
source("ZZ_test_filtering.R") #Can be by-passed if not needed: doesn't change variables' name.
source("A5_diachro_plot_context.R")         #g_POS_V_vs_C_ALL
source("A5_diachro_plot_context_lemma.R")   #p_POS__V_lemma_arrange
source("A5_count_choose_SRC.R") #This lead to R_count or R_count_bypass_lemma
                                #depending on the var. 'bypass' == Y/N
source("A5_chi-2.R") #Beware: must be parametered by hand!!! Can be by-passed.
source("A6_save_diachro_plot_context.R")
source("A6_save_diachro_plot_context_lemma.R")
source("A6_save_simple_table.R")
source("A6_save_table.R")
source("A6_chi-2_save.R")
source("A8_carto.R")
source("A9_carto_save.R")

#For contextual analysis with gender and number inflexion
source("A1_import_data_context.R")
source("A3_post_cleaning_context.R")
  #Can be by-passed (if all forms are considered the same lemma) by calling instead
  #the following dummy script:
  source("A3_fake_cleaning_context.R")
source("A4_filter_dates_context.R")
source("ZZ_test_filtering.R")
source("AA_filter_gender_context.R")
source("A5_diachro_plot_context.R")         #g_POS_V_vs_C_ALL
source("A5_diachro_plot_context_lemma.R")   #p_POS__V_lemma_arrange
source("A5_count_choose_SRC.R") #This lead to R_count or R_count_bypass_lemma
                                #depending on the var. 'bypass' == Y/N
source("A5_chi-2.R") #Beware: must be parametered by hand!!! Can be by-passed.
source("A6_save_diachro_plot_context.R")
source("A6_save_diachro_plot_context_lemma.R")
source("A6_save_simple_table.R")
source("A6_save_table.R")
source("A6_chi-2_save.R")
source("A8_carto.R")
source("A9_carto_save.R")

#For contextual analysis with gender, number and case inflexion
source("A1_import_data_context.R")
source("A3_post_cleaning_context.R")
  #Can be by-passed (if all forms are considered the same lemma) by calling instead
  #the following dummy script:
  source("A3_fake_cleaning_context.R")
source("A4_filter_dates_context.R")
source("ZZ_test_filtering.R")
source("AA_filter_case_context.R")
source("A5_diachro_plot_context.R")         #g_POS_V_vs_C_ALL
source("A5_diachro_plot_context_lemma.R")   #p_POS__V_lemma_arrange
source("A5_count_choose_SRC.R") #This lead to R_count or R_count_bypass_lemma
                                #depending on the var. 'bypass' == Y/N
source("A5_chi-2.R") #Beware: must be parametered by hand!!!
source("A6_save_diachro_plot_context.R")
source("A6_save_diachro_plot_context_lemma.R")
source("A6_save_simple_table.R")
source("A6_save_table.R")
source("A6_chi-2_save.R")
source("A8_carto.R")
source("A9_carto_save.R")

#For custom forms treatment
#Run the following line if you treat a non-schwa ending word. Do not run it if is a schwa-ending word.
##Only concerns printing schwa or V in y-axis.
schwa <- readline(prompt="What do you want to print ('V') on some Y axis?")
source("A1_import_custom_data_context.R")
source("A4_filter_dates_context.R")
source("ZZ_test_filtering.R")
source("A5_diachro_plot_context.R")
source("A5_chi-2.R") #Beware: must be parametered by hand!!!
source("A5_diachro_plot_context_sans_NA.R")
source("A6_save_diachro_plot_context.R")
source("A6_save_diachro_plot_context_sans_NA.R")
source("A6_chi-2_save.R")
source("A8_carto.R")
source("A9_carto_save.R")

#For maps by periods
#If you want custom periods, run the following lines:
 bound_1b <- readline(prompt="Whats the top boundary of the first period?")
 bound_2a <- readline(prompt="Whats the bottom boundary of the second period?")
 bound_2b <- readline(prompt="Whats the top boundary of the second period?")
 bound_3a <- readline(prompt="Whats the bottom boundary of the third period?")
 bound_3b <- readline(prompt="Whats the top boundary of the third period?")
 bound_4a <- readline(prompt="Whats the bottom boundary of the last period?")
#If you want half century periods, run the following lines:
 bound_1b <- 1151
 bound_2a <- 1150
 bound_2b <- 1201
 bound_3a <- 1200
 bound_3b <- 1251
 bound_4a <- 1250
source("A8bis_carto_time.R")
source("A8bis_carto_time_save.R")
