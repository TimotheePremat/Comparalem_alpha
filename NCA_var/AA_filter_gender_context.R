#Filtering for gender and number (specially for ADJs)
POS_E_V_masc_sg <- POS_E_V_cleaned %>%
 filter(gender == "masc") %>%
	filter(number == "sg")
POS_E_C_masc_sg <- POS_E_C_cleaned %>%
 filter(gender == "masc") %>%
	filter(number == "sg")
POS_nonE_C_masc_sg <- POS_nonE_C_cleaned %>%
 filter(gender == "masc") %>%
	filter(number == "sg")
POS_nonE_V_masc_sg <- POS_nonE_V_cleaned %>%
 filter(gender == "masc") %>%
	filter(number == "sg")
# POS_EC_V_masc_sg <- POS_EC_V_cleaned %>%
#  filter(gender == "masc") %>%
# 	filter(number == "sg")
# POS_EC_C_masc_sg <- POS_EC_C_cleaned %>%
#  filter(gender == "masc") %>%
# 	filter(number == "sg")

POS_E_V_masc_pl <- POS_E_V_cleaned %>%
	filter(gender == "masc") %>%
	filter(number == "pl")
POS_E_C_masc_pl <- POS_E_C_cleaned %>%
	filter(gender == "masc") %>%
	filter(number == "pl")
POS_nonE_C_masc_pl <- POS_nonE_C_cleaned %>%
	filter(gender == "masc") %>%
	filter(number == "pl")
POS_nonE_V_masc_pl <- POS_nonE_V_cleaned %>%
	filter(gender == "masc") %>%
	filter(number == "pl")
# POS_EC_V_masc_pl <- POS_EC_V_cleaned %>%
# 	filter(gender == "masc") %>%
# 	filter(number == "pl")
# POS_EC_C_masc_pl <- POS_EC_C_cleaned %>%
# 	filter(gender == "masc") %>%
# 	filter(number == "pl")

POS_E_V_femi_sg <- POS_E_V_cleaned %>%
	filter(gender == "femi") %>%
	filter(number == "sg")
POS_E_C_femi_sg <- POS_E_C_cleaned %>%
	filter(gender == "femi") %>%
	filter(number == "sg")
POS_nonE_C_femi_sg <- POS_nonE_C_cleaned %>%
	filter(gender == "femi") %>%
	filter(number == "sg")
POS_nonE_V_femi_sg <- POS_nonE_V_cleaned %>%
	filter(gender == "femi") %>%
	filter(number == "sg")
# POS_EC_V_femi_sg <- POS_EC_V_cleaned %>%
# 	filter(gender == "femi") %>%
# 	filter(number == "sg")
# POS_EC_C_femi_sg <- POS_EC_C_cleaned %>%
# 	filter(gender == "femi") %>%
# 	filter(number == "sg")

POS_E_V_femi_pl <- POS_E_V_cleaned %>%
	filter(gender == "femi") %>%
	filter(number == "pl")
POS_E_C_femi_pl <- POS_E_C_cleaned %>%
	filter(gender == "femi") %>%
	filter(number == "pl")
POS_nonE_C_femi_pl <- POS_nonE_C_cleaned %>%
	filter(gender == "femi") %>%
	filter(number == "pl")
POS_nonE_V_femi_pl <- POS_nonE_V_cleaned %>%
	filter(gender == "femi") %>%
	filter(number == "pl")
# POS_EC_V_femi_pl <- POS_EC_V_cleaned %>%
# 	filter(gender == "femi") %>%
# 	filter(number == "pl")
# POS_EC_C_femi_pl <- POS_EC_C_cleaned %>%
# 	filter(gender == "femi") %>%
# 	filter(number == "pl")

POS_E_V_cleaned <- get(paste("POS_E_V",my_var_filter, sep = "_"))
POS_E_C_cleaned <- get(paste("POS_E_C",my_var_filter, sep = "_"))
POS_nonE_C_cleaned <- get(paste("POS_nonE_C",my_var_filter, sep = "_"))
POS_nonE_V_cleaned <- get(paste("POS_nonE_V",my_var_filter, sep = "_"))
# POS_EC_V_cleaned <- get(paste("POS_EC_V",my_var_filter, sep = "_"))
# POS_EC_C_cleaned <- get(paste("POS_EC_C",my_var_filter, sep = "_"))
