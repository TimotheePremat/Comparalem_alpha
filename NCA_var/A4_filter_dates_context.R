#Filtering for texts of discontinuous date
POS_E_V_cleaned <- POS_E_V_cleaned %>%
 filter(datecomposition < 1325)
POS_E_C_cleaned <- POS_E_C_cleaned %>%
 filter(datecomposition < 1325)
POS_nonE_C_cleaned <- POS_nonE_C_cleaned %>%
 filter(datecomposition < 1325)
POS_nonE_V_cleaned <- POS_nonE_V_cleaned %>%
 filter(datecomposition < 1325)
# POS_EC_V_cleaned <- POS_EC_V_cleaned %>%
#  filter(datecomposition < 1325)
# POS_EC_C_cleaned <- POS_EC_C_cleaned %>%
#  filter(datecomposition < 1325)
