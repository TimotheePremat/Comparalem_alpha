#Print all forms
forms_E_C <- POS_E_C_cleaned %>% count(word)
forms_nonE_C <- POS_nonE_C_cleaned %>% count(word)
forms_E_V <- POS_E_V_cleaned %>% count(word)
forms_nonE_V <- POS_nonE_V_cleaned %>% count(word)

#sum occurrences
sum_E_C <- sum(forms_E_C$n) %>%
	round(digits=2)
sum_nonE_C <- sum(forms_nonE_C$n)
sum_E_V <- sum(forms_E_V$n)
sum_nonE_V <- sum(forms_nonE_V$n)

sum__C <- sum_E_C + sum_nonE_C
sum__V <- sum_E_V + sum_nonE_V

sum_E <- sum_E_C + sum_E_V
sum_nonE <- sum_nonE_C + sum_nonE_V

#compute rates
tx__C <- (sum_nonE_C / sum__C) *100
 if(tx__C < 0.01){tx__C <- "<0.01 %"}
 if(tx__C > 0.01){tx__C <- round(tx__C, digits=2)}
 if(tx__C > 0.01){tx__C <- paste(tx__C, "%")}
	if(tx__C == 0.01){tx__C <- paste(tx__C, "%")}
tx__V <- (sum_nonE_V / sum__V) *100
 if(tx__V < 0.01){tx__V <- "<0.01 %"}
 if(tx__V > 0.01){tx__V <- round(tx__V, digits=2)}
 if(tx__V > 0.01){tx__V <- paste(tx__V, "%")}
	if(tx__V == 0.01){tx__V <- paste(tx__V, "%")}

tx_nonE_V <- sum_nonE_V / sum_nonE
tx_nonE_C <- sum_nonE_C / sum_nonE

#Print table
table_simple <- data.frame("CAT" = c("Voyelle", "Apocope", "Taux apo.", "Somme"),
																				"_C" = c(sum_E_C, sum_nonE_C, tx__C, sum__C),
																			 "_V" = c(sum_E_V, sum_nonE_V, tx__V, sum__V))
