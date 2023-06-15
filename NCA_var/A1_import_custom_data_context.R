#Choose the files to investigate
##Either two files for general comparison, or more than two if working with contexts
##First step is to ask the user to assign a file to the var POS_E and POS_nonE
POS_V_C_file <- file.choose(new = FALSE)
POS_V_V_file <- file.choose(new = FALSE)
POS_nonV_C_file <- file.choose(new = FALSE)
POS_nonV_V_file <- file.choose(new = FALSE)

##Second step is to read that file as a CSV file
POS_V_C_df <- read_delim(POS_V_C_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
POS_V_V_df <- read_delim(POS_V_V_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
POS_nonV_C_df <- read_delim(POS_nonV_C_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)
POS_nonV_V_df <- read_delim(POS_nonV_V_file, delim = ";", escape_double = FALSE, trim_ws = TRUE)

##Load texts' meta-data
Dates_NCA <- read_excel("Data/Dates_NCA.xlsx", sheet = "Small_caps")

###Merge to incorporate text metadata in the list of forms
POS_V_V <- merge (POS_V_V_df, Dates_NCA, by="id")
POS_V_C <- merge (POS_V_C_df, Dates_NCA, by="id")
POS_nonV_C <- merge (POS_nonV_C_df, Dates_NCA, by="id")
POS_nonV_V <- merge (POS_nonV_V_df, Dates_NCA, by="id")

#Transform frequency table to observation table
## Instead of having some observations in the same row (when same page or line
   #ref) with frequency indication, duplicate these rows (by factor F) and
   #remove column F. Needed to get the right frequencies with the code below.
POS_V_V <- uncount(POS_V_V, weights=F)
POS_V_C <- uncount(POS_V_C, weights=F)
POS_nonV_C <- uncount(POS_nonV_C, weights=F)
POS_nonV_V <- uncount(POS_nonV_V, weights=F)

#Change name to output to A4_filter_dates_context.R
POS_E_V_cleaned <- POS_V_V
POS_E_C_cleaned <- POS_V_C
POS_nonE_C_cleaned <- POS_nonV_C
POS_nonE_V_cleaned <- POS_nonV_V
