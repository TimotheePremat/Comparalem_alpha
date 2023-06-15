#Choose the files to investigate
##Either two files for general comparison, or more than two if working with contexts
##First step is to ask the user to assign a file to the var POS_E and POS_nonE
POS_E_a <- file.choose(new = FALSE)
POS_nonE_a <- file.choose(new = FALSE)

##Second step is to read that file as a CSV file
POS_E <- read_delim(POS_E_a, delim = ";", escape_double = FALSE, trim_ws = TRUE)
POS_nonE <- read_delim(POS_nonE_a, delim = ";", escape_double = FALSE, trim_ws = TRUE)

##Load texts' meta-data
Dates_NCA <- read_excel("Dates_NCA.xlsx", sheet = "data_basic")

Dates_NCA <- Dates_NCA %>% rename(datecomposition = dateComposition) %>%
    rename(datemanuscrit = dateManuscrit) %>%
    rename(lieucomposition = lieuComposition) %>%
    rename(lieumanuscrit = lieuManuscrit) %>%
    rename(datemoyennedees = dateMoyenneDees)

###Merge to incorporate text metadata in the list of forms
POS_E <- merge (POS_E, Dates_NCA, by="id")
POS_nonE <- merge (POS_nonE, Dates_NCA, by="id")

#Transform frequency table to observation table
## Instead of having some observations in the same row (when same page or line
   #ref) with frequency indication, duplicate these rows (by factor F) and
   #remove column F. Needed to get the right frequencies with the code below.
POS_E <- uncount(POS_E, weights=F)
POS_nonE <- uncount(POS_nonE, weights=F)

#Fake it 'til you make it: pseudo clean the files (they are already clean) for input in next scripts. Useless if you use ZZ_test_filtering.R
# POS_E_cleaned <- POS_E
# POS_nonE_cleaned <- POS_nonE
