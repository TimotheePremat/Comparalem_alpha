#Script to print numbers of lemmas and occ.

#For total
POS <- bind_rows(POS_E_V, POS_E_C, POS_nonE_C, POS_nonE_V)
# POS <- POS %>%
#   filter(lemma %in% list$lemma)

POS_nrow <- nrow(POS)

POS_lemma <- POS %>% group_by(lemma)
POS_lemma <- POS_lemma %>% count(lemma, sort=TRUE) #First time gets the number of occ. per lemma
POS_lemma <- POS_lemma %>% count(lemma, sort=TRUE) #Second time get 1 by lemma
POS_lemma_sum <- sum(POS_lemma$n)

#For schwaC
# POS_schwaC <- bind_rows(POS_EC_V, POS_EC_C)
#
# POS_schwaC_nrow <- nrow(POS_schwaC)
#
# POS_schwaC_lemma <- POS_schwaC %>% group_by(lemma)
# POS_schwaC_lemma <- POS_schwaC_lemma %>% count(lemma, sort=TRUE) #First time gets the number of occ. per lemma
# POS_schwaC_lemma <- POS_schwaC_lemma %>% count(lemma, sort=TRUE) #Second time get 1 by lemma
# POS_schwaC_lemma_sum <- sum(POS_schwaC_lemma$n)

#For variable lemmas
POS_var <- bind_rows(POS_E_V_cleaned, POS_E_C_cleaned, POS_nonE_C_cleaned, POS_nonE_V_cleaned)

POS_var_nrow <- nrow(POS_var)

POS_var_lemma <- POS_var %>% group_by(lemma)
POS_var_lemma <- POS_var_lemma %>% count(lemma, sort=TRUE) #First time gets the number of occ. per lemma
POS_var_lemma <- POS_var_lemma %>% count(lemma, sort=TRUE) #Second time get 1 by lemma
POS_var_lemma_sum <- sum(POS_var_lemma$n)

#For variable lemmas ending in -Ø
POS_var_zero <- bind_rows(POS_nonE_C_cleaned, POS_nonE_V_cleaned)
POS_var_zero_nrow <- nrow(POS_var_zero)

#Print
##Tot.
###Number of occ.:
POS_nrow
###Number of lemmas:
POS_lemma_sum

# ##Schwa_C
# ###Number of occ.:
# POS_schwaC_nrow
# ###Number of lemmas:
# POS_schwaC_lemma_sum

##Variable lemmas
###Number of occ.:
POS_var_nrow
###Number of lemmas:
POS_var_lemma_sum
###Number of -Ø forms
POS_var_zero_nrow

###Calculations
Tx_var_lemma <- (POS_var_lemma_sum / POS_lemma_sum)*100
if(Tx_var_lemma < 0.01){Tx_var_lemma <- "<0,01 %"}
if(Tx_var_lemma > 0.01){Tx_var_lemma <- round(Tx_var_lemma, digits=2)}
if(Tx_var_lemma > 0.01){Tx_var_lemma <- paste(Tx_var_lemma, "%")}

Tx_var_nrow <- (POS_var_nrow / POS_nrow)*100
if(Tx_var_nrow < 0.01){Tx_var_nrow <- "<0,01 %"}
if(Tx_var_nrow > 0.01){Tx_var_nrow <- round(Tx_var_nrow, digits=2)}
if(Tx_var_nrow > 0.01){Tx_var_nrow <- paste(Tx_var_nrow, "%")}

Tx_var_zero_nrow <- (POS_var_zero_nrow / POS_nrow)*100
if(Tx_var_zero_nrow < 0.01){Tx_var_zero_nrow <- "<0,01 %"}
if(Tx_var_zero_nrow > 0.01){Tx_var_zero_nrow <- round(Tx_var_zero_nrow, digits=2)}
if(Tx_var_zero_nrow > 0.01){Tx_var_zero_nrow <- paste(Tx_var_zero_nrow, "%")}

##Synoptic table
table <- data.frame("AA_temp" = c("Lemmes", "Occ."),
                    "Nb." = c(POS_lemma_sum, POS_nrow),
																				"Nb.var." = c(POS_var_lemma_sum, POS_var_nrow),
																				"Taux" = c(Tx_var_lemma, Tx_var_nrow),
																				"Nb.Var.Ø" = c("", POS_var_zero_nrow),
																				"Taux.Ø" = c("",Tx_var_zero_nrow))
table <- table %>% rename(!!my_var := AA_temp)
