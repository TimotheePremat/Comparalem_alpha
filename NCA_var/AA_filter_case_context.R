#Filtering for gender, number and case (specially for PROs)

#-------------------------------
# E
#-------------------------------
## E_V
	POS_E_V_masc_sg_suj <- POS_E_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_E_V_masc_sg_acc <- POS_E_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_E_V_masc_sg_dat <- POS_E_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_E_V_masc_sg_obj <- POS_E_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

	POS_E_V_masc_pl_suj <- POS_E_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "suj")
	POS_E_V_masc_pl_acc <- POS_E_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "acc")
	POS_E_V_masc_pl_dat <- POS_E_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "dat")
	POS_E_V_masc_pl_obj <- POS_E_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "obj")

	POS_E_V_femi_sg_suj <- POS_E_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_E_V_femi_sg_acc <- POS_E_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_E_V_femi_sg_dat <- POS_E_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_E_V_femi_sg_obj <- POS_E_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

	POS_E_V_femi_pl_suj <- POS_E_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "suj")
	POS_E_V_femi_pl_acc <- POS_E_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "acc")
	POS_E_V_femi_pl_dat <- POS_E_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "dat")
	POS_E_V_femi_pl_obj <- POS_E_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "obj")

	POS_E_V_neutre_sg_suj <- POS_E_V_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_E_V_neutre_sg_acc <- POS_E_V_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_E_V_neutre_sg_dat <- POS_E_V_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_E_V_neutre_sg_obj <- POS_E_V_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

# E_C
	POS_E_C_masc_sg_suj <- POS_E_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_E_C_masc_sg_acc <- POS_E_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_E_C_masc_sg_dat <- POS_E_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_E_C_masc_sg_obj <- POS_E_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

	POS_E_C_masc_pl_suj <- POS_E_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "suj")
	POS_E_C_masc_pl_acc <- POS_E_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "acc")
	POS_E_C_masc_pl_dat <- POS_E_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "dat")
	POS_E_C_masc_pl_obj <- POS_E_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "obj")

	POS_E_C_femi_sg_suj <- POS_E_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_E_C_femi_sg_acc <- POS_E_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_E_C_femi_sg_dat <- POS_E_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_E_C_femi_sg_obj <- POS_E_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

	POS_E_C_femi_pl_suj <- POS_E_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "suj")
	POS_E_C_femi_pl_acc <- POS_E_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "acc")
	POS_E_C_femi_pl_dat <- POS_E_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "dat")
	POS_E_C_femi_pl_obj <- POS_E_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "obj")

	POS_E_C_neutre_sg_suj <- POS_E_C_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_E_C_neutre_sg_acc <- POS_E_C_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_E_C_neutre_sg_dat <- POS_E_C_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_E_C_neutre_sg_obj <- POS_E_C_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

#-------------------------------
# nonE
#-------------------------------
## nonE_V
	POS_nonE_V_masc_sg_suj <- POS_nonE_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_nonE_V_masc_sg_acc <- POS_nonE_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_nonE_V_masc_sg_dat <- POS_nonE_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_nonE_V_masc_sg_obj <- POS_nonE_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

	POS_nonE_V_masc_pl_suj <- POS_nonE_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "suj")
	POS_nonE_V_masc_pl_acc <- POS_nonE_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "acc")
	POS_nonE_V_masc_pl_dat <- POS_nonE_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "dat")
	POS_nonE_V_masc_pl_obj <- POS_nonE_V_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "obj")

	POS_nonE_V_femi_sg_suj <- POS_nonE_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_nonE_V_femi_sg_acc <- POS_nonE_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_nonE_V_femi_sg_dat <- POS_nonE_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_nonE_V_femi_sg_obj <- POS_nonE_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

	POS_nonE_V_femi_pl_suj <- POS_nonE_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "suj")
	POS_nonE_V_femi_pl_acc <- POS_nonE_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "acc")
	POS_nonE_V_femi_pl_dat <- POS_nonE_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "dat")
	POS_nonE_V_femi_pl_obj <- POS_nonE_V_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "obj")

	POS_nonE_V_neutre_sg_suj <- POS_nonE_V_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_nonE_V_neutre_sg_acc <- POS_nonE_V_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_nonE_V_neutre_sg_dat <- POS_nonE_V_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_nonE_V_neutre_sg_obj <- POS_nonE_V_cleaned %>%
		filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

## nonE_C
	POS_nonE_C_masc_sg_suj <- POS_nonE_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_nonE_C_masc_sg_acc <- POS_nonE_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_nonE_C_masc_sg_dat <- POS_nonE_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_nonE_C_masc_sg_obj <- POS_nonE_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

	POS_nonE_C_masc_pl_suj <- POS_nonE_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "suj")
	POS_nonE_C_masc_pl_acc <- POS_nonE_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "acc")
	POS_nonE_C_masc_pl_dat <- POS_nonE_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "dat")
	POS_nonE_C_masc_pl_obj <- POS_nonE_C_cleaned %>%
	 filter(gender == "masc") %>%
		filter(number == "pl") %>%
		filter(decl == "obj")

	POS_nonE_C_femi_sg_suj <- POS_nonE_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_nonE_C_femi_sg_acc <- POS_nonE_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_nonE_C_femi_sg_dat <- POS_nonE_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_nonE_C_femi_sg_obj <- POS_nonE_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

	POS_nonE_C_femi_pl_suj <- POS_nonE_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "suj")
	POS_nonE_C_femi_pl_acc <- POS_nonE_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "acc")
	POS_nonE_C_femi_pl_dat <- POS_nonE_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "dat")
	POS_nonE_C_femi_pl_obj <- POS_nonE_C_cleaned %>%
	 filter(gender == "femi") %>%
		filter(number == "pl") %>%
		filter(decl == "obj")

	POS_nonE_C_neutre_sg_suj <- POS_nonE_C_cleaned %>%
	 filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "suj")
	POS_nonE_C_neutre_sg_acc <- POS_nonE_C_cleaned %>%
	 filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "acc")
	POS_nonE_C_neutre_sg_dat <- POS_nonE_C_cleaned %>%
	 filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "dat")
	POS_nonE_C_neutre_sg_obj <- POS_nonE_C_cleaned %>%
	 filter(gender == "neutre") %>%
		filter(number == "sg") %>%
		filter(decl == "obj")

# #-------------------------------
# # EC
# #-------------------------------
# ## EC_V
# 	POS_EC_V_masc_sg_suj <- POS_EC_V_cleaned %>%
# 		filter(gender == "masc") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "suj")
# 	POS_EC_V_masc_sg_acc <- POS_EC_V_cleaned %>%
# 		filter(gender == "masc") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "acc")
# 	POS_EC_V_masc_sg_dat <- POS_EC_V_cleaned %>%
# 		filter(gender == "masc") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "dat")
#
# 	POS_EC_V_masc_pl_suj <- POS_EC_V_cleaned %>%
# 		filter(gender == "masc") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "suj")
# 	POS_EC_V_masc_pl_acc <- POS_EC_V_cleaned %>%
# 		filter(gender == "masc") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "acc")
# 	POS_EC_V_masc_pl_dat <- POS_EC_V_cleaned %>%
# 		filter(gender == "masc") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "dat")
#
# 	POS_EC_V_femi_sg_suj <- POS_EC_V_cleaned %>%
# 		filter(gender == "femi") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "suj")
# 	POS_EC_V_femi_sg_acc <- POS_EC_V_cleaned %>%
# 		filter(gender == "femi") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "acc")
# 	POS_EC_V_femi_sg_dat <- POS_EC_V_cleaned %>%
# 		filter(gender == "femi") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "dat")
#
# 	POS_EC_V_femi_pl_suj <- POS_EC_V_cleaned %>%
# 		filter(gender == "femi") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "suj")
# 	POS_EC_V_femi_pl_acc <- POS_EC_V_cleaned %>%
# 		filter(gender == "femi") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "acc")
# 	POS_EC_V_femi_pl_dat <- POS_EC_V_cleaned %>%
# 		filter(gender == "femi") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "dat")
#
# 	POS_EC_V_neutre_pl_suj <- POS_EC_V_cleaned %>%
# 		filter(gender == "neutre") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "suj")
# 	POS_EC_V_neutre_pl_acc <- POS_EC_V_cleaned %>%
# 		filter(gender == "neutre") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "acc")
# 	POS_EC_V_neutre_pl_dat <- POS_EC_V_cleaned %>%
# 		filter(gender == "neutre") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "dat")
#
# ## EC_C
# 	POS_EC_C_masc_sg_suj <- POS_EC_C_cleaned %>%
# 	 filter(gender == "masc") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "suj")
# 	POS_EC_C_masc_sg_acc <- POS_EC_C_cleaned %>%
# 	 filter(gender == "masc") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "acc")
# 	POS_EC_C_masc_sg_dat <- POS_EC_C_cleaned %>%
# 	 filter(gender == "masc") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "dat")
#
# 	POS_EC_C_masc_pl_suj <- POS_EC_C_cleaned %>%
# 	 filter(gender == "masc") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "suj")
# 	POS_EC_C_masc_pl_acc <- POS_EC_C_cleaned %>%
# 	 filter(gender == "masc") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "acc")
# 	POS_EC_C_masc_pl_dat <- POS_EC_C_cleaned %>%
# 	 filter(gender == "masc") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "dat")
#
# 	POS_EC_C_femi_sg_suj <- POS_EC_C_cleaned %>%
# 	 filter(gender == "femi") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "suj")
# 	POS_EC_C_femi_sg_acc <- POS_EC_C_cleaned %>%
# 	 filter(gender == "femi") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "acc")
# 	POS_EC_C_femi_sg_dat <- POS_EC_C_cleaned %>%
# 	 filter(gender == "femi") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "dat")
#
# 	POS_EC_C_femi_pl_suj <- POS_EC_C_cleaned %>%
# 	 filter(gender == "femi") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "suj")
# 	POS_EC_C_femi_pl_acc <- POS_EC_C_cleaned %>%
# 	 filter(gender == "femi") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "acc")
# 	POS_EC_C_femi_pl_dat <- POS_EC_C_cleaned %>%
# 	 filter(gender == "femi") %>%
# 		filter(number == "pl") %>%
# 		filter(decl == "dat")
#
# 	POS_EC_C_neutre_sg_suj <- POS_EC_C_cleaned %>%
# 		filter(gender == "neutre") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "suj")
# 	POS_EC_C_neutre_sg_acc <- POS_EC_C_cleaned %>%
# 		filter(gender == "neutre") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "acc")
# 	POS_EC_C_neutre_sg_dat <- POS_EC_C_cleaned %>%
# 		filter(gender == "neutre") %>%
# 		filter(number == "sg") %>%
# 		filter(decl == "dat")
#-------------------------------
#Filter data to be considered by R script
##Filter your data based on my_var_filter variable inputed by user in
##MASTER R file.
#-------------------------------
POS_E_V_cleaned <- get(paste("POS_E_V",my_var_filter, sep = "_"))
POS_E_C_cleaned <- get(paste("POS_E_C",my_var_filter, sep = "_"))
POS_nonE_C_cleaned <- get(paste("POS_nonE_C",my_var_filter, sep = "_"))
POS_nonE_V_cleaned <- get(paste("POS_nonE_V",my_var_filter, sep = "_"))
# POS_EC_V_cleaned <- get(paste("POS_EC_V",my_var_filter, sep = "_"))
# POS_EC_C_cleaned <- get(paste("POS_EC_C",my_var_filter, sep = "_"))
