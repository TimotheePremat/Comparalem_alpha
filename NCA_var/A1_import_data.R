#Choose the files to investigate
##Either two files for general comparison, or more than two if working with contexts
##First step is to ask the user to assign a file to the var POS_E and POS_nonE
POS_E <- file.choose(new = FALSE)
POS_nonE <- file.choose(new = FALSE)

##Second step is to read that file as a CSV file
POS_E <- read_delim(POS_E, delim = ";", escape_double = FALSE, trim_ws = TRUE)
POS_nonE <- read_delim(POS_nonE, delim = ";", escape_double = FALSE, trim_ws = TRUE)

##Load texts' meta-data
Dates_NCA <- read_excel("Data/Dates_NCA.xlsx", sheet = "data_basic")

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


#TRASH/OLDIES
# #-------
# #ADV
# #-------
# #Without context
# #-------
# ADV_E <- read_delim("Data/ADV_E_output.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
# ADV_nonE <- read_delim("Data/ADV_nonE_output.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
#
# ADV_E <- merge (ADV_E, Dates_NCA, by="id")
# ADV_nonE <- merge (ADV_nonE, Dates_NCA, by="id")
#
# ADV_E <- uncount(ADV_E, weights=F)
# ADV_nonE <- uncount(ADV_nonE, weights=F)
#
# #With context
# #-------
# ADV_EC_V <- read_delim("Data/ADV_EC_V_output.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
# ADV_EC_V <- merge (ADV_EC_V, Dates_NCA, by="id")
#
# ADV_EC_C <- read_delim("Data/ADV_EC_C_output.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
# ADV_EC_C <- merge (ADV_EC_C, Dates_NCA, by="id")
#
# ADV_E_V <- read_delim("Data/ADV_E_V_output.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
# ADV_E_V <- merge (ADV_E_V, Dates_NCA, by="id")
#
# ADV_E_C <- read_delim("Data/ADV_E_C_output.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
# ADV_E_C <- merge (ADV_E_C, Dates_NCA, by="id")
#
# ADV_nonE_C <- read_delim("Data/ADV_nonE_C_output.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
# ADV_nonE_C <- merge (ADV_nonE_C, Dates_NCA, by="id")
#
# ADV_nonE_V <- read_delim("Data/ADV_nonE_V_output.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
# ADV_nonE_V <- merge (ADV_nonE_V, Dates_NCA, by="id")
