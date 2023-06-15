#Filtering for texts of discontinuous date
POS_E_cleaned <- 	POS_E_cleaned %>%
 filter(datecomposition < 1325)
POS_nonE_cleaned <- POS_nonE_cleaned %>%
 filter(datecomposition < 1325)
POS_E <- 	POS_E %>%
	 filter(datecomposition < 1325)
POS_nonE <- POS_nonE %>%
	 filter(datecomposition < 1325)
