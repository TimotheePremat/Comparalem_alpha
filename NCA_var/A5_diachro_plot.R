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

POS_EnonE_count <- left_join(POS_E_count,
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
	POS_EnonE_sum <- sum(POS_EnonE_count$Nb_POS_nonE, na.rm=TRUE)
	POS_EnonE_sum <- round(POS_EnonE_sum, digits=2)

g_POS_EnonE_tx_compo <- ggplot(POS_EnonE_count,aes(x = datecomposition,
                                                   y = Tx_POS_nonE)) +
 geom_point() +
 #stat_summary_bin(fun.y = mean, binwidth = 25, na.rm = TRUE, color = 'black', geom ='line') +
 geom_smooth(method = "loess",
             se = FALSE,
             na.rm = TRUE,
             colour = "black",
             span = 0.75) +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "black",
             span = 0.75,
             linetype = "dashed") +
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
                    name=paste("Taux de Ø sur les",my_var2,"à var. <e>/Ø")) +
	annotate("text",
										Inf,
										Inf,
										label = paste(my_var),
										hjust = 1,
										vjust = 1,
										colour="grey")

 # cor.test(POS_EnonE_count$datecomposition, POS_EnonE_count$Tx_POS_EnonE, method = "pearson")

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

g_POS_EnonE_tx_compo_leg <- g_POS_EnonE_tx_compo +
 labs(title = "Taux de variantes Ø des lemmes à variation <e>/Ø",
      subtitle = "<e> supprimés et <e> ajoutés",
      caption = "Chaque point représente le taux de variantes Ø d'un texte.\nLe tracé représente une régression locale.")

POS_EnonE_mean <- mean(POS_EnonE_count$Tx_POS_nonE, na.rm=TRUE)
POS_EnonE_mean <- round(POS_EnonE_mean, digits=2)
POS_EnonE_median <- median(POS_EnonE_count$Tx_POS_nonE, na.rm=TRUE)
POS_EnonE_median <- round(POS_EnonE_median, digits=2)
POS_EnonE_sd <- sd(POS_EnonE_count$Tx_POS_nonE, na.rm=TRUE)
POS_EnonE_sd <- round(POS_EnonE_sd, digits=2)
POS_EnonE_sum <- sum(POS_EnonE_count$Nb_POS_nonE, na.rm=TRUE)
POS_EnonE_sum <- round(POS_EnonE_sum, digits=2)

g_POS_EnonE_tx_ms <- ggplot(POS_EnonE_count,aes(x = datemanuscrit,
                                                y = Tx_POS_nonE)) +
 geom_point() +
 #stat_summary_bin(fun.y = mean, binwidth = 25, na.rm = TRUE, color = 'black', geom ='line') +
 geom_smooth(method = "loess",
             se = FALSE,
             na.rm = TRUE,
             colour = "black",
             span = 0.75) +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "black",
             span = 0.75,
             linetype = "dashed") +
 theme_classic() +
	scale_x_continuous(name="Date de copie", breaks=c(1000,
                                                         1100,
                                                         1200,
                                                         1300,
                                                         1400,
                                                         1500,
                                                         1600,
                                                         1700,
                                                         1800,
                                                         1900,
                                                         2000),
																																																limits = c(1100,1800)) +
 scale_y_continuous(limits = c(0,1),
                    name=paste("Taux de Ø sur les",my_var2,"à var. <e>/Ø")) +
	annotate("text",
										Inf,
										Inf,
										label = paste(my_var),
										hjust = 1,
										vjust = 1,
										colour="grey")

	g_POS_EnonE_tx_ms_arrange <- g_POS_EnonE_tx_ms +
		annotate("rect",
											xmin = 1525,
											xmax = 1800,
											ymin = 0,
											ymax = 0.25,
											alpha = .75,
											fill="white") +
		annotate("text",
											x=1700,
											y=0,
											label=paste("",POS_EnonE_mean,"\n",
																							POS_EnonE_median,"\n",
																							POS_EnonE_sd,"\n",
																							POS_EnonE_sum),
											hjust=0,
											vjust=0) +
		annotate("text",
											x=1650,
											y=0,
											label="moy.\n méd.\n sd \n Nb. obs.",
											hjust=1,
											vjust=0)

g_POS_EnonE_tx_ms_leg <- g_POS_EnonE_tx_ms +
 labs(title = "Taux de variantes Ø des lemmes à variation <e>/Ø",
      subtitle = "<e> supprimés et <e> ajoutés",
      caption = "Chaque point représente le taux de variantes Ø d'un texte.\nLe tracé représente une régression locale.")


#---------------------------------------------------------------------
#POS_count: global reference observations
##This is to plot the number of observations of lemmas tagged as POS,
#for reference.
#---------------------------------------------------------------------
#Remove empty cols for readability
index_POS_E_for_count <- map_lgl(POS_E, ~ all(is.na(.)))
POS_E_for_count <- POS_E[, !index_POS_E_for_count]
index_POS_nonE_for_count <- map_lgl(POS_nonE, ~ all(is.na(.)))
POS_nonE_for_count <- POS_nonE[, !index_POS_nonE_for_count]

#Prepare POS_count for plotting qty of words with irregular E by text
POS_nonE_for_count <- POS_nonE_for_count %>% count(id) %>%
																	rename(n.nonE = n)
POS_E_for_count <- POS_E_for_count %>% count(id)  %>%
																	rename(n.E = n)

POS_count <- left_join(POS_E_for_count,
																	            POS_nonE_for_count,
																													by = "id")
POS_count <- POS_count %>%
 mutate(Nb_POS = n.E + n.nonE) %>%
 mutate(Tx_POS_nonE = n.nonE / Nb_POS)

## Reinject metadata from Dates_NCA
POS_count <- merge (POS_count,
                          Dates_NCA,
                          by="id")

###Remove empty cols for readability
index_POS_count <- map_lgl(POS_count, ~ all(is.na(.)))
POS_count <- POS_count[, !index_POS_count]
#Transform date values into numeric values (there are discreet for now)
POS_count$datecomposition <- as.numeric(as.character(POS_count$datecomposition))
POS_count$datemanuscrit <- as.numeric(as.character(POS_count$datemanuscrit))

POS_count_mean <- mean(POS_count$Nb_POS, na.rm=TRUE)
POS_count_mean <- round(POS_count_mean, digits=2)
POS_count_median <- median(POS_count$Nb_POS, na.rm=TRUE)
POS_count_median <- round(POS_count_median, digits=2)
POS_count_sd <- sd(POS_count$Nb_POS, na.rm=TRUE)
POS_count_sd <- round(POS_count_sd, digits=2)
POS_count_sum <- sum(POS_count$Nb_POS, na.rm=TRUE)
POS_count_sum <- round(POS_count_sum, digits=2)

#PRINT and SAVE
g_POS_count_compo <- ggplot(POS_count, aes(x = datecomposition, y=Nb_POS)) +
 geom_point() +
 stat_summary_bin(fun.y = sum,
                  binwidth = 25,
                  na.rm = TRUE,
                  color = 'black',
                  geom ='line') +
 theme_classic() +
 scale_x_continuous(name="Date de composition",
                    breaks=c(1000,
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
 scale_y_continuous(name=paste("Nombre de mots annotés",my_var2)) +
	annotate("text",
										Inf,
										Inf,
										label = paste(my_var),
										hjust = 1,
										vjust = 1,
										colour="grey")

g_POS_count_compo_leg <- g_POS_count_compo +
 labs(title = "Dimensions (nombres de POS) du corpus NCA",
      caption = "Chaque point représente le nombre de POSs d'un texte.\nLe tracé représente la somme par période de 25 ans.")

g_POS_count_compo_arrange <- g_POS_count_compo +
	annotate("rect",
										xmin = 1350,
										xmax = 1500,
										ymin = 0,
										ymax = 50000,
										alpha = .75,
										fill="white") +
	annotate("text",
										x=1450,
										y=0,
										label=paste("",POS_count_mean,"\n",
																						POS_count_median,"\n",
																						POS_count_sd,"\n",
																						POS_count_sum),
										hjust=0,
										vjust=0) +
	annotate("text",
										x=1425,
										y=0,
										label="moy.\n méd.\n sd \n Nb. obs.",
										hjust=1,
										vjust=0)

g_POS_count_ms <- ggplot(POS_count, aes(x = datemanuscrit, y=Nb_POS)) +
 geom_point() +
 stat_summary_bin(fun.y = sum,
                  binwidth = 25,
                  na.rm = TRUE,
                  color = 'black',
                  geom ='line') +
 theme_classic() +
	scale_x_continuous(name="Date de copie", breaks=c(1000,
                                                         1100,
                                                         1200,
                                                         1300,
                                                         1400,
                                                         1500,
                                                         1600,
                                                         1700,
                                                         1800,
                                                         1900,
                                                         2000),
																																																limits = c(1100,1800)) +
 scale_y_continuous(name=paste("Nombre de mots annotés",my_var2)) +
	annotate("text",
										Inf,
										Inf,
										label = paste(my_var),
										hjust = 1,
										vjust = 1,
										colour="grey")

g_POS_count_ms_arrange <- g_POS_count_ms +
	annotate("rect",
										xmin = 1525,
										xmax = 1800,
										ymin = 0,
										ymax = 50000,
										alpha = .75,
										fill="white") +
	annotate("text",
										x=1700,
										y=0,
										label=paste("",POS_count_mean,"\n",
																						POS_count_median,"\n",
																						POS_count_sd,"\n",
																						POS_count_sum),
										hjust=0,
										vjust=0) +
	annotate("text",
										x=1650,
										y=0,
										label="moy.\n méd.\n sd \n Nb. obs.",
										hjust=1,
										vjust=0)

g_POS_count_ms_leg <- g_POS_count_ms +
 labs(title = "Dimensions (Nombres d'POS) du corpus NCA",
      caption = "Chaque point représente le nombre de POSs d'un texte.\nLe tracé représente la somme par période de 25 ans.")

#---------------------------------------------------------------------
#POS_count_var: number of irregular lemmas observations
##This is to plot the number of observations of lemmas tagged as common names
##that have a <e>/Ø variation, for reference.
#---------------------------------------------------------------------
#Prepare POS_EnonE_count for plotting qty of words with irregular E by text

POS_EnonE_count_mean <- mean(POS_EnonE_count$Nb_POS_EnonE, na.rm=TRUE)
POS_EnonE_count_mean <- round(POS_EnonE_count_mean, digits=2)
POS_EnonE_count_median <- median(POS_EnonE_count$Nb_POS_EnonE, na.rm=TRUE)
POS_EnonE_count_median <- round(POS_EnonE_count_median, digits=2)
POS_EnonE_count_sd <- sd(POS_EnonE_count$Nb_POS_EnonE, na.rm=TRUE)
POS_EnonE_count_sd <- round(POS_EnonE_count_sd, digits=2)
POS_EnonE_count_sum <- sum(POS_EnonE_count$Nb_POS_EnonE, na.rm=TRUE)
POS_EnonE_count_sum <- round(POS_EnonE_count_sum, digits=2)

g_POS_count_var_compo <- ggplot(POS_EnonE_count,aes(x = datecomposition,
                                                y = Nb_POS_EnonE)) +
 geom_point() +
 stat_summary_bin(fun.y = sum,
                  binwidth = 25,
                  na.rm = TRUE,
                  color = 'black',
                  geom ='line') +
 theme_classic() +
 scale_x_continuous(name="Date de composition",
                    breaks=c(1000,
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
 scale_y_continuous(name=paste("Nombre d'occurrences des",my_var2,"à var. <e>/Ø")) +
	annotate("text",
										Inf,
										Inf,
										label = paste(my_var),
										hjust = 1,
										vjust = 1,
										colour="grey")

g_POS_count_var_compo_arrange <- g_POS_count_var_compo +
	annotate("rect",
										xmin = 1350,
										xmax = 1500,
										ymin = 0,
										ymax = 300,
										alpha = .75,
										fill="white") +
	annotate("text",
										x=1450,
										y=0,
										label=paste("",POS_EnonE_count_mean,"\n",
																						POS_EnonE_count_median,"\n",
																						POS_EnonE_count_sd,"\n",
																						POS_EnonE_count_sum),
										hjust=0,
										vjust=0) +
	annotate("text",
										x=1425,
										y=0,
										label="moy.\n méd.\n sd \n Nb. obs.",
										hjust=1,
										vjust=0)

g_POS_count_var_compo_leg <- g_POS_count_var_compo +
 labs(title = "Dimensions (nombres de POS) de la variation <e>/Ø",
      caption = "Chaque point représente le nom de POSs à variation <e>/Ø d'un texte.\nLe tracé représente la somme par période de 25 ans.")


g_POS_count_var_ms <- ggplot(POS_EnonE_count,aes(x = datemanuscrit,
                                                y = Nb_POS_EnonE)) +
 geom_point() +
 stat_summary_bin(fun.y = sum,
                  binwidth = 25,
                  na.rm = TRUE,
                  color = 'black',
                  geom ='line') +
 theme_classic() +
	scale_x_continuous(name="Date de copie", breaks=c(1000,
                                                         1100,
                                                         1200,
                                                         1300,
                                                         1400,
                                                         1500,
                                                         1600,
                                                         1700,
                                                         1800,
                                                         1900,
                                                         2000),
																																																limits = c(1100,1800)) +
 scale_y_continuous(name=paste("Nombre d'occurrences des",my_var2,"à var. <e>/Ø")) +
	annotate("text",
										Inf,
										Inf,
										label = paste(my_var),
										hjust = 1,
										vjust = 1,
										colour="grey")

g_POS_count_var_ms_arrange <- g_POS_count_var_ms +
	annotate("rect",
										xmin = 1525,
										xmax = 1800,
										ymin = 0,
										ymax = 400,
										alpha = .75,
										fill="white") +
	annotate("text",
										x=1700,
										y=0,
										label=paste("",POS_EnonE_count_mean,"\n",
																						POS_EnonE_count_median,"\n",
																						POS_EnonE_count_sd,"\n",
																						POS_EnonE_count_sum),
										hjust=0,
										vjust=0) +
	annotate("text",
										x=1650,
										y=0,
										label="moy.\n méd.\n sd \n Nb. obs.",
										hjust=1,
										vjust=0)

g_POS_count_var_ms_leg <- g_POS_count_var_ms +
 labs(title = "Dimensions (nombres de POS) de la variation <e>/Ø",
      caption = "Chaque point représente le nombre de POSs à variation <e>/Ø d'un texte.\nLe tracé représente la somme par période de 25 ans.")


#---------------------------------------------------------------------
#ARRANGE plots together
#---------------------------------------------------------------------
g_POS_ALL <- ggarrange(g_POS_EnonE_tx_compo_arrange,
          g_POS_EnonE_tx_ms_arrange,
          g_POS_count_var_compo_arrange,
          g_POS_count_var_ms_arrange,
          g_POS_count_compo_arrange,
          g_POS_count_ms_arrange,
          labels="auto",
          ncol = 2,
          nrow = 3,
									 align = "v")
g_POS_ALL_leg <- annotate_figure(g_POS_ALL, bottom = text_grob("Chaque point représente un texte.\nEn (a-b), la ligne continue est une régression locale, et la ligne discontinue une régression linéaire.\nEn (c-f), la ligne représente la somme des occurrences sur une fenêtre de 25 ans.",
                                  hjust = 1,
																																		x = 1,
																																		face = "italic",
																																		size = 10))
