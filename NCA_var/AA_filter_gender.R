#Filtering for gender and number (specially for ADJs)
POS_E_masc_sg <- POS_E %>%
 filter(gender == "masc") %>%
	filter(number == "sg")
POS_E_masc_pl <- POS_E %>%
	filter(gender == "masc") %>%
	filter(number == "pl")
POS_E_femi_sg <- POS_E %>%
	filter(gender == "femi") %>%
	filter(number == "sg")
POS_E_femi_pl <- POS_E %>%
	filter(gender == "femi") %>%
	filter(number == "pl")

POS_nonE_masc_sg <- POS_nonE %>%
 filter(gender == "masc") %>%
	filter(number == "sg")
POS_nonE_masc_pl <- POS_nonE %>%
	filter(gender == "masc") %>%
	filter(number == "pl")
POS_nonE_femi_sg <- POS_nonE %>%
	filter(gender == "femi") %>%
	filter(number == "sg")
POS_nonE_femi_pl <- POS_nonE %>%
	filter(gender == "femi") %>%
	filter(number == "pl")
