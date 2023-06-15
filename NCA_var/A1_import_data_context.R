#Choose the files to investigate
##Either two files for general comparison, or more than two if working with contexts
##First step is to ask the user to assign a file to the var POS_E and POS_nonE
POS_E_C_file <- file.choose(new = FALSE)
POS_E_V_file <- file.choose(new = FALSE)
POS_nonE_C_file <- file.choose(new = FALSE)
POS_nonE_V_file <- file.choose(new = FALSE)
# POS_EC_C_file <- file.choose(new = FALSE)
# POS_EC_V_file <- file.choose(new = FALSE)

##Second step is to read that file as a CSV file
POS_E_C_df <- read_delim(POS_E_C_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
POS_E_V_df <- read_delim(POS_E_V_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
POS_nonE_C_df <- read_delim(POS_nonE_C_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
POS_nonE_V_df <- read_delim(POS_nonE_V_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
# POS_EC_C_df <- read_delim(POS_EC_C_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
# POS_EC_V_df <- read_delim(POS_EC_V_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)

##Load texts' meta-data
Dates_NCA <- read_excel("Data/Dates_NCA.xlsx", sheet = "data_basic")

Dates_NCA <- Dates_NCA %>% rename(datecomposition = dateComposition) %>%
    rename(datemanuscrit = dateManuscrit) %>%
    rename(lieucomposition = lieuComposition) %>%
    rename(lieumanuscrit = lieuManuscrit) %>%
    rename(datemoyennedees = dateMoyenneDees)

###Merge to incorporate text metadata in the list of forms
POS_E_V <- merge (POS_E_V_df, Dates_NCA, by="id")
POS_E_C <- merge (POS_E_C_df, Dates_NCA, by="id")
POS_nonE_C <- merge (POS_nonE_C_df, Dates_NCA, by="id")
POS_nonE_V <- merge (POS_nonE_V_df, Dates_NCA, by="id")
# POS_EC_V <- merge (POS_EC_V_df, Dates_NCA, by="id")
# POS_EC_C <- merge (POS_EC_C_df, Dates_NCA, by="id")

#Transform frequency table to observation table
## Instead of having some observations in the same row (when same page or line
   #ref) with frequency indication, duplicate these rows (by factor F) and
   #remove column F. Needed to get the right frequencies with the code below.
POS_E_V <- uncount(POS_E_V, weights=F)
POS_E_C <- uncount(POS_E_C, weights=F)
POS_nonE_C <- uncount(POS_nonE_C, weights=F)
POS_nonE_V <- uncount(POS_nonE_V, weights=F)
# POS_EC_V <- uncount(POS_EC_V, weights=F)
# POS_EC_C <- uncount(POS_EC_C, weights=F)


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
