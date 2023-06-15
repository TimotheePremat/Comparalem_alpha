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

#Choose the files to investigate
##Either two files for general comparison, or more than two if working with contexts
##First step is to ask the user to assign a file to the var POS_E and POS_nonE
POS_E_file <- file.choose(new = FALSE)
POS_nonE_file <- file.choose(new = FALSE)

##Second step is to read that file as a CSV file
POS_E <- read_delim(POS_E_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
POS_nonE <- read_delim(POS_nonE_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)

##Load texts' meta-data
Dates_NCA <- read_excel("Data/Dates_NCA.xlsx", sheet = "Small_caps")

###Merge to incorporate text metadata in the list of forms
POS_E <- merge (POS_E, Dates_NCA, by="id")
POS_nonE <- merge (POS_nonE, Dates_NCA, by="id")

#Transform frequency table to observation table
## Instead of having some observations in the same row (when same page or line
   #ref) with frequency indication, duplicate these rows (by factor F) and
   #remove column F. Needed to get the right frequencies with the code below.
POS_E <- uncount(POS_E, weights=F)
POS_nonE <- uncount(POS_nonE, weights=F)

POS_E$Duplicated <- POS_E$lemma %in% POS_nonE$lemma
POS_nonE$Duplicated <- POS_nonE$lemma %in% POS_E$lemma

POS_E_duplicated <- POS_E %>% filter(Duplicated=='TRUE')
POS_E_freq <- POS_E_duplicated %>% count(lemma, sort=TRUE)

POS_nonE_duplicated <- POS_nonE %>% filter(Duplicated=='TRUE')
POS_nonE_freq <- POS_nonE_duplicated %>% count(lemma, sort=TRUE)

View(POS_E_duplicated)
View(POS_nonE_duplicated)

POS_merged_freq <- merge (POS_E_freq, POS_nonE_freq, by="lemma")
View(POS_merged_freq)

#Export
##Non-cleaned data
write_xlsx(POS_E,path=paste("Data/pre-treatment/junk/R",my_var,"E treated.xlsx"))
write_xlsx(POS_nonE,path=paste("Data/pre-treatment/junk/R",my_var,"nonE treated.xlsx"))
write_xlsx(POS_E_duplicated,path=paste("Data/pre-treatment/junk/R",my_var,"E duplicated.xlsx"))
write_xlsx(POS_nonE_duplicated,path=paste("Data/pre-treatment/junk/R",my_var,"nonE duplicated.xlsx"))
write_xlsx(POS_E_freq,path=paste("Data/pre-treatment/junk/R",my_var,"E freq.xlsx"))
write_xlsx(POS_nonE_freq,path=paste("Data/pre-treatment/junk/R",my_var,"nonE freq.xlsx"))
write_xlsx(POS_merged_freq,path=paste("Data/pre-treatment/R",my_var,"merged freq.xlsx"))
