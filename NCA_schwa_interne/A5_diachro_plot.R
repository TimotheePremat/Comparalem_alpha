#Remove empty cols for readability
index_POS_E <- map_lgl(POS_E_cleaned, ~ all(is.na(.)))
POS_E_cleaned <- POS_E_cleaned[, !index_POS_E]
index_POS_nonE <- map_lgl(POS_nonE_cleaned, ~ all(is.na(.)))
POS_nonE_cleaned <- POS_nonE_cleaned[, !index_POS_E]

#Transform date values into numeric values (there are discreet for now)
POS_E_cleaned$datecomposition <- as.numeric(as.character(POS_E_cleaned$datecomposition))
POS_nonE_cleaned$datecomposition <- as.numeric(as.character(POS_nonE_cleaned$datecomposition))
POS_nonE$datecomposition <- as.numeric(as.character(POS_nonE$datecomposition))
POS_E$datecomposition <- as.numeric(as.character(POS_E$datecomposition))

#---------------------------------------------------------------------
#POS_EnonE_count: global observations
##This is to plot the number of observations of lemmas with <e>/Ø variation
#---------------------------------------------------------------------
#Prepare POS_EnonE_count for plotting qty of words with irregular E by text
POS_nonE_count <- POS_nonE_cleaned %>% count(id) %>%
																	rename(n.nonE = n)
POS_E_count <- POS_E_cleaned %>% count(id)  %>%
																	rename(n.E = n)

POS_EnonE_count <- full_join(POS_E_count,
                             POS_nonE_count,
																	            by = "id")
POS_EnonE_count <-	replace(POS_EnonE_count, is.na(POS_EnonE_count), 0)
POS_EnonE_count <- POS_EnonE_count %>%
 mutate(Nb_POS_EnonE = n.E + n.nonE) %>%
 mutate(Tx_POS_nonE = n.nonE / Nb_POS_EnonE)

## Reinject metadata from Dates_NCA
POS_EnonE_count <- merge (POS_EnonE_count,
                          Dates_NCA,
                          by="id")

 ###Remove empty cols for readability
 index_POS_EnonE_count <- map_lgl(POS_EnonE_count, ~ all(is.na(.)))
 POS_EnonE_count <- POS_EnonE_count[, !index_POS_EnonE_count]
 #Transform date values into numeric values (there are discreet for now)
 POS_EnonE_count$datecomposition <- as.numeric(as.character(POS_EnonE_count$datecomposition))
 POS_EnonE_count$datemanuscrit <- as.numeric(as.character(POS_EnonE_count$datemanuscrit))
	POS_EnonE_count$datemoyennedees <- as.numeric(as.character(POS_EnonE_count$datemanuscrit))

	POS_EnonE_mean <- mean(POS_EnonE_count$Tx_POS_nonE, na.rm=TRUE)
	POS_EnonE_mean <- round(POS_EnonE_mean, digits=2)
	POS_EnonE_median <- median(POS_EnonE_count$Tx_POS_nonE, na.rm=TRUE)
	POS_EnonE_median <- round(POS_EnonE_median, digits=2)
	POS_EnonE_sd <- sd(POS_EnonE_count$Tx_POS_nonE, na.rm=TRUE)
	POS_EnonE_sd <- round(POS_EnonE_sd, digits=2)
	POS_EnonE_sum <- sum(POS_EnonE_count$Nb_POS_EnonE, na.rm=TRUE)

g_POS_EnonE_tx_compo <- ggplot(POS_EnonE_count,aes(x = datecomposition,
                                                   y = Tx_POS_nonE)) +
 geom_point() +
 #stat_summary_bin(fun.y = mean, binwidth = 25, na.rm = TRUE, color = 'black', geom ='line') +
 geom_smooth(method = "loess",
             se = FALSE,
             na.rm = TRUE,
             colour = "black",
             span = 0.75) +
 theme_classic() +
 scale_x_continuous(name="Date de composition", breaks=c(1000,
                                                         1100,
                                                         1200,
                                                         1300,
                                                         1400,
                                                         1500,
                                                         1600,
                                                         1700,
                                                         1800,
                                                         1900,
                                                         2000)) +
 scale_y_continuous(limits = c(0,1),
                    name=paste("Taux de chute de schwa",my_var2)) +
	annotate("text",
										Inf,
										Inf,
										label = paste(my_var),
										hjust = 1,
										vjust = 1,
										colour="grey")

g_POS_EnonE_tx_compo_arrange <- g_POS_EnonE_tx_compo +
	annotate("rect",
										xmin = 1350,
										xmax = 1500,
										ymin = 0,
										ymax = 0.25,
										alpha = .75,
										fill="white") +
	annotate("text",
										x=1450,
										y=0,
										label=paste("",POS_EnonE_mean,"\n",
																						POS_EnonE_median,"\n",
																						POS_EnonE_sd,"\n",
																						POS_EnonE_sum),
										hjust=0,
										vjust=0) +
	annotate("text",
										x=1425,
										y=0,
										label="moy.\n méd.\n sd \n Nb. obs.",
										hjust=1,
										vjust=0)

g_POS_EnonE_tx_compo_leg <- g_POS_EnonE_tx_compo_arrange +
 labs(title = paste("Schwa",my_var2),
      caption = "Chaque point représente un texte.")
