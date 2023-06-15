#Chose your exploratory file
# relevant_forms <- file.choose(new = FALSE)
# list <- read_excel(relevant_forms, sheet = "Feuil2")
#
# POS_E_V_femi_pl_cleaned <- POS_E_femi_pl %>%
#   filter(lemma %in% list$lemma)
# POS_nonE_V_femi_pl_cleaned <- POS_nonE_femi_pl %>%
#   filter(lemma %in% list$lemma)

irrelevant_forms_file <- file.choose(new = FALSE)
irrelevant_forms <- read_excel(irrelevant_forms_file)
POS_nonE_cleaned <- POS_nonE_cleaned %>%
  anti_join(irrelevant_forms, by=c("word", "lemma", "ref1", "id"))
POS_E_cleaned <- POS_E_cleaned %>%
  anti_join(irrelevant_forms, by=c("word", "lemma", "ref1", "id"))
