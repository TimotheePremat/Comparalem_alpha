#Add cat. column for merging
POS_EC_V_cleaned <- POS_EC_V_cleaned %>%
                     mutate(cat = "EC_V") %>%
                     relocate(cat)
POS_EC_C_cleaned <- POS_EC_C_cleaned %>%
                     mutate(cat = "EC_C") %>%
                     relocate(cat)
POS_E_V_cleaned <- POS_E_V_cleaned %>%
                     mutate(cat = "E_V") %>%
                     relocate(cat)
POS_E_C_cleaned <- POS_E_C_cleaned %>%
                     mutate(cat = "E_C") %>%
                     relocate(cat)
POS_nonE_C_cleaned <- POS_nonE_C_cleaned %>%
                     mutate(cat = "nonE_C") %>%
                     relocate(cat)
POS_nonE_V_cleaned <- POS_nonE_V_cleaned %>%
                     mutate(cat = "nonE_V") %>%
                     relocate(cat)

POS_context <- bind_rows(POS_EC_V_cleaned,
	                        POS_EC_C_cleaned,
	                        POS_E_V_cleaned,
	                        POS_E_C_cleaned,
	                        POS_nonE_C_cleaned,
	                        POS_nonE_V_cleaned)

POS_context_count <- POS_context %>% count(cat,id)


POS_context_count <- merge (POS_context_count,
                            Dates_NCA,
                            by="id")

###Remove empty cols for readability
index_POS_context_count <- map_lgl(POS_context_count, ~ all(is.na(.)))
POS_context_count <- POS_context_count[, !index_POS_context_count]

#Transform date values into numeric values (there are discreet for now)
POS_context_count$datecomposition <- as.numeric(as.character(POS_context_count$datecomposition))
POS_context_count$datemanuscrit <- as.numeric(as.character(POS_context_count$datemanuscrit))

#-------
# POS__C: behaviour of forms before C-initial word
#-------
POS_E_C_count <- POS_E_C_cleaned %>% count(id) %>%
                 rename(n.E = n)
POS_nonE_C_count <- POS_nonE_C_cleaned %>% count(id) %>%
                 rename(n.nonE = n)
POS__C <- full_join(POS_E_C_count,
                    POS_nonE_C_count,
																			 by = "id")
POS__C <- replace(POS__C, is.na(POS__C), 0)
POS__C <- POS__C %>% mutate(Nb_POS_EnonE = n.E + n.nonE) %>%
			                  mutate(Tx_POS_EnonE = n.nonE / Nb_POS_EnonE) %>%
																					mutate(Minus = n.nonE - n.E)

## Reinject metadata from Dates_NCA
POS__C <- merge (POS__C,
										       Dates_NCA,
										       by="id")

POS__C$datecomposition <- as.numeric(as.character(POS__C$datecomposition))
POS__C$datemanuscrit <- as.numeric(as.character(POS__C$datemanuscrit))
POS__C$datemoyennedees <- as.numeric(as.character(POS__C$datemoyennedees))

POS__C_mean <- mean(POS__C$Tx_POS_EnonE, na.rm=TRUE)
POS__C_mean <- round(POS__C_mean, digits=2)
POS__C_median <- median(POS__C$Tx_POS_EnonE, na.rm=TRUE)
POS__C_median <- round(POS__C_median, digits=2)
POS__C_sd <- sd(POS__C$Tx_POS_EnonE, na.rm=TRUE)
POS__C_sd <- round(POS__C_sd, digits=2)
POS__C_sum <- sum(POS__C$Nb_POS_EnonE, na.rm=TRUE)
POS__C_sum <- round(POS__C_sum, digits=2)

p_POS__C <- ggplot(POS__C, aes(datecomposition, Tx_POS_EnonE)) +
	geom_point() +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "black",
													span = 0.75)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "black",
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
                                                         2000),
																																																limits = c(1100,1500)) +
 scale_y_continuous(limits = c(0,1),
                    name="Taux de Ø# devant #C") +
	annotate("text",
          Inf,
          Inf,
          label = paste(my_var),
          hjust = 1,
          vjust = 1,
          colour="grey")

	p_POS__Cstandalone <- p_POS__C +
	annotate("text",
										x=1450,
										y=0.25,
										label=paste(POS__C_mean)) +
	annotate("text",
										x=1450,
										y=0.20,
										label=paste(POS__C_median)) +
	annotate("text",
										x=1450,
										y=0.15,
										label=paste(POS__C_sd)) +
	annotate("text",
										x=1425,
										y=0.25,
										label="moy.") +
	annotate("text",
										x=1425,
										y=0.20,
										label="méd.") +
	annotate("text",
										x=1425,
										y=0.15,
										label="sd")

	p_POS__Carrange <- p_POS__C +
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
										label=paste("",POS__C_mean,"\n",
																						POS__C_median,"\n",
																						POS__C_sd,"\n",
																						POS__C_sum),
										hjust=0,
										vjust=0) +
	annotate("text",
										x=1425,
										y=0,
										label="moy.\n méd.\n sd \n Nb. obs.",
										hjust=1,
										vjust=0)

#-------
# POS__V: behaviour of forms before V-initial word
#-------
POS_E_V_count <- POS_E_V_cleaned %>% count(id) %>%
                 rename(n.E = n)
POS_nonE_V_count <- POS_nonE_V_cleaned %>% count(id)  %>%
                 rename(n.nonE = n)
POS__V <- full_join(POS_E_V_count,
                    POS_nonE_V_count,
																			 by = "id")
POS__V <- replace(POS__V, is.na(POS__V), 0)
POS__V <- POS__V %>% mutate(Nb_POS_EnonE = n.E + n.nonE) %>%
			                  mutate(Tx_POS_EnonE = n.nonE / Nb_POS_EnonE)


## Reinject metadata from Dates_NCA
POS__V <- merge (POS__V,
										       Dates_NCA,
										       by="id")

POS__V$datecomposition <- as.numeric(as.character(POS__V$datecomposition))
POS__V$datemanuscrit <- as.numeric(as.character(POS__V$datemanuscrit))
POS__V$datemoyennedees <- as.numeric(as.character(POS__V$datemoyennedees))

POS__V_mean <- mean(POS__V$Tx_POS_EnonE, na.rm=TRUE)
POS__V_mean <- round(POS__V_mean, digits=2)
POS__V_median <- median(POS__V$Tx_POS_EnonE, na.rm=TRUE)
POS__V_median <- round(POS__V_median, digits=2)
POS__V_sd <- sd(POS__V$Tx_POS_EnonE, na.rm=TRUE)
POS__V_sd <- round(POS__V_sd, digits=2)
POS__V_sum <- sum(POS__V$Nb_POS_EnonE, na.rm=TRUE)
POS__V_sum <- round(POS__V_sum, digits=2)

p_POS__V <- ggplot(POS__V, aes(x=datecomposition, y=Tx_POS_EnonE)) +
	geom_point() +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "black",
													span = 0.75)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "black",
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
                                                         2000),
																																																limits = c(1100,1500)) +
 scale_y_continuous(limits = c(0,1),
                    name="Taux de Ø# devant #V") +
 annotate("text",
									Inf,
									Inf,
									label = paste(my_var),
									hjust = 1,
									vjust = 1,
									colour="grey")

	p_POS__Vstandalone <- p_POS__V +
	annotate("text",
          x=1450,
									 y=0.25,
									 label=paste(POS__V_mean)) +
	annotate("text",
										x=1450,
										y=0.20,
										label=paste(POS__V_median)) +
	annotate("text",
										x=1450,
										y=0.15,
										label=paste(POS__V_sd)) +
	annotate("text",
										x=1425,
										y=0.25,
										label="moy.") +
	annotate("text",
										x=1425,
										y=0.20,
										label="méd.") +
	annotate("text",
										x=1425,
										y=0.15,
										label="sd")

p_POS__Varrange <- p_POS__V +
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
									label=paste("",POS__V_mean,"\n",
																					POS__V_median,"\n",
																					POS__V_sd,"\n",
																					POS__V_sum),
									hjust=0,
									vjust=0) +
annotate("text",
									x=1425,
									y=0,
									label="moy.\n méd.\n sd \n Nb. obs.",
									hjust=1,
									vjust=0)

#-------
# POS_E_: context for forms with final <e>
#-------
POS_E_V_count <- POS_E_V_cleaned %>% count(id)  %>%
                 rename(n.E = n)
POS_E_C_count <- POS_E_C_cleaned %>% count(id)  %>%
                 rename(n.nonE = n)
POS_E_ <- full_join(POS_E_V_count,
                    POS_E_C_count,
																			 by = "id")
POS_E_ <- replace(POS_E_, is.na(POS_E_), 0)
POS_E_ <- POS_E_ %>% mutate(Nb_POS_E_CV = n.E + n.nonE) %>%
			                  mutate(Tx_POS_E_C = n.nonE / Nb_POS_E_CV)

## Reinject metadata from Dates_NCA
POS_E_ <- merge (POS_E_,
										       Dates_NCA,
										       by="id")

POS_E_$datecomposition <- as.numeric(as.character(POS_E_$datecomposition))
POS_E_$datemanuscrit <- as.numeric(as.character(POS_E_$datemanuscrit))
POS_E_$datemoyennedees <- as.numeric(as.character(POS_E_$datemoyennedees))

POS_E__mean <- mean(POS_E_$Tx_POS_E_C, na.rm=TRUE)
POS_E__mean <- round(POS_E__mean, digits=2)
POS_E__median <- median(POS_E_$Tx_POS_E_C, na.rm=TRUE)
POS_E__median <- round(POS_E__median, digits=2)
POS_E__sd <- sd(POS_E_$Tx_POS_E_C, na.rm=TRUE)
POS_E__sd <- round(POS_E__sd, digits=2)
POS_E__sum <- sum(POS_E_$Nb_POS_E_CV, na.rm=TRUE)
POS_E__sum <- round(POS_E__sum, digits=2)

p_POS_E_ <- ggplot(POS_E_, aes(datecomposition, Tx_POS_E_C)) +
	geom_point() +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "black",
													span = 0.75)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "black",
             linetype = "dashed") +
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
                             2000),
																				limits = c(1100,1500)) +
 scale_y_continuous(limits = c(0,1),
                    name="Taux de #C après schwa#") +
 annotate("text",
          Inf,
          Inf,
          label = paste(my_var),
          hjust = 1,
          vjust = 1,
          colour="grey")

p_POS_E_standalone <- p_POS_E_ +
annotate("text",
									x=1450,
									y=0.25,
									label=paste(POS_E__mean)) +
annotate("text",
									x=1450,
									y=0.20,
									label=paste(POS_E__median)) +
annotate("text",
									x=1450,
									y=0.15,
									label=paste(POS_E__sd)) +
annotate("text",
									x=1425,
									y=0.25,
									label="moy.") +
annotate("text",
									x=1425,
									y=0.20,
									label="méd.") +
annotate("text",
									x=1425,
									y=0.15,
									label="sd")

p_POS_E_arrange <- p_POS_E_ +
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
									label=paste("",POS_E__mean,"\n",
																					POS_E__median,"\n",
																					POS_E__sd,"\n",
																					POS_E__sum),
									hjust=0,
									vjust=0) +
annotate("text",
									x=1425,
									y=0,
									label="moy.\n méd.\n sd \n Nb. obs.",
									hjust=1,
									vjust=0)

#-------
# POS_E_: context for forms with final Ø
#-------
POS_nonE_V_count <- POS_nonE_V_cleaned %>% count(id) %>%
                 rename(n.E = n)
POS_nonE_C_count <- POS_nonE_C_cleaned %>% count(id) %>%
                 rename(n.nonE = n)
POS_nonE_ <- full_join(POS_nonE_V_count,
                    POS_nonE_C_count,
																			 by = "id")
POS_nonE_ <- replace(POS_nonE_, is.na(POS_nonE_), 0)
POS_nonE_ <- POS_nonE_ %>% mutate(Nb_POS_nonE_CV = n.E + n.nonE) %>%
			       mutate(Tx_POS_nonE_C = n.nonE / Nb_POS_nonE_CV)

## Reinject metadata from Dates_NCA
POS_nonE_ <- merge (POS_nonE_,
										       Dates_NCA,
										       by="id")

POS_nonE_$datecomposition <- as.numeric(as.character(POS_nonE_$datecomposition))
POS_nonE_$datemanuscrit <- as.numeric(as.character(POS_nonE_$datemanuscrit))
POS_nonE_$datemoyennedees <- as.numeric(as.character(POS_nonE_$datemoyennedees))

POS_nonE__mean <- mean(POS_nonE_$Tx_POS_nonE_C, na.rm=TRUE)
POS_nonE__mean <- round(POS_nonE__mean, digits=2)
POS_nonE__median <- median(POS_nonE_$Tx_POS_nonE_C, na.rm=TRUE)
POS_nonE__median <- round(POS_nonE__median, digits=2)
POS_nonE__sd <- sd(POS_nonE_$Tx_POS_nonE_C, na.rm=TRUE)
POS_nonE__sd <- round(POS_nonE__sd, digits=2)
POS_nonE__sum <- sum(POS_nonE_$Nb_POS_nonE_CV, na.rm=TRUE)
POS_nonE__sum <- round(POS_nonE__sum, digits=2)

p_POS_nonE_ <- ggplot(POS_nonE_, aes(datecomposition, Tx_POS_nonE_C)) +
	geom_point() +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "black",
													span = 0.75)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "black",
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
                                                         2000),
																																																limits = c(1100,1500)) +
 scale_y_continuous(limits = c(0,1),
                    name="Taux de #C après Ø#") +
	annotate("text",
										Inf,
										Inf,
										label = paste(my_var),
										hjust = 1,
										vjust = 1,
										colour="grey")

p_POS_nonE_standalone <- p_POS_nonE_ +
	annotate("text",
          x=1450,
									 y=0.25,
									 label=paste(POS_nonE__mean)) +
	annotate("text",
										x=1450,
										y=0.20,
										label=paste(POS_nonE__median)) +
	annotate("text",
										x=1450,
										y=0.15,
										label=paste(POS_nonE__sd)) +
	annotate("text",
										x=1425,
										y=0.25,
										label="moy.") +
	annotate("text",
										x=1425,
										y=0.20,
										label="méd.") +
	annotate("text",
										x=1425,
										y=0.15,
										label="sd")

p_POS_nonE_arrange <- p_POS_nonE_  +
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
									label=paste("",POS_nonE__mean,"\n",
																					POS_nonE__median,"\n",
																					POS_nonE__sd,"\n",
																					POS_nonE__sum),
									hjust=0,
									vjust=0) +
annotate("text",
									x=1425,
									y=0,
									label="moy.\n méd.\n sd \n Nb. obs.",
									hjust=1,
									vjust=0)

#-------
# POS_coda: context for forms with final consonant
#-------
POS_EC_V_count <- POS_EC_V_cleaned %>% count(id) %>%
                 rename(n.E = n)
POS_EC_C_count <- POS_EC_C_cleaned %>% count(id) %>%
                 rename(n.nonE = n)
POS_EC_ <- full_join(POS_EC_V_count,
                    POS_EC_C_count,
																			 by = "id")
POS_EC_ <- replace(POS_EC_, is.na(POS_EC_), 0)
POS_EC_ <- POS_EC_ %>% mutate(Nb_POS_EnonE = n.E + n.nonE) %>%
			                  mutate(Tx_POS_EnonE = n.nonE / Nb_POS_EnonE)

## Reinject metadata from Dates_NCA
POS_EC_ <- merge (POS_EC_,
										       Dates_NCA,
										       by="id")

POS_EC_$datecomposition <- as.numeric(as.character(POS_EC_$datecomposition))
POS_EC_$datemanuscrit <- as.numeric(as.character(POS_EC_$datemanuscrit))
POS_EC_$datemoyennedees <- as.numeric(as.character(POS_EC_$datemoyennedees))

POS_EC_mean <- mean(POS_EC_$Tx_POS_EnonE, na.rm=TRUE)
POS_EC_mean <- round(POS_EC_mean, digits=2)
POS_EC_median <- median(POS_EC_$Tx_POS_EnonE, na.rm=TRUE)
POS_EC_median <- round(POS_EC_median, digits=2)
POS_EC_sd <- sd(POS_EC_$Tx_POS_EnonE, na.rm=TRUE)
POS_EC_sd <- round(POS_EC_sd, digits=2)
POS_EC_sum <- sum(POS_EC_$Nb_POS_EnonE, na.rm=TRUE)
POS_EC_sum <- round(POS_EC_sum, digits=2)

p_POS_EC_ <- ggplot(POS_EC_, aes(datecomposition, Tx_POS_EnonE)) +
	geom_point() +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "black",
													span = 0.75)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "black",
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
                                                         2000),
																																																limits = c(1100,1500)) +
 scale_y_continuous(limits = c(0,1),
                    name="Taux de #C après schwaC#") +
	annotate("text",
										Inf,
										Inf,
										label = paste(my_var),
										hjust = 1,
										vjust = 1,
										colour="grey")

	p_POS_EC_standalone <- p_POS_EC_ +
	annotate("text",
          x=1450,
									 y=0.25,
									 label=paste(POS_EC_mean)) +
	annotate("text",
										x=1450,
										y=0.20,
										label=paste(POS_EC_median)) +
	annotate("text",
										x=1450,
										y=0.15,
										label=paste(POS_EC_sd)) +
	annotate("text",
										x=1425,
										y=0.25,
										label="moy.") +
	annotate("text",
										x=1425,
										y=0.20,
										label="méd.") +
	annotate("text",
										x=1425,
										y=0.15,
										label="sd")

p_POS_EC_arrange <- p_POS_EC_ +
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
									label=paste("",POS_EC_mean,"\n",
																					POS_EC_median,"\n",
																					POS_EC_sd,"\n",
																					POS_EC_sum),
									hjust=0,
									vjust=0) +
annotate("text",
									x=1425,
									y=0,
									label="moy.\n méd.\n sd \n Nb. obs.",
									hjust=1,
									vjust=0)

#---------------------------------------------------------------------
#ARRANGE plots together
#---------------------------------------------------------------------
g_POS_ALL <- ggarrange(p_POS__Carrange,
          p_POS__Varrange,
          p_POS_E_arrange,
          p_POS_nonE_arrange,
          p_POS_EC_arrange,
          labels="auto",
          ncol = 2,
          nrow = 3,
									 align = "v")
g_POS_ALL_leg <- annotate_figure(g_POS_ALL, bottom = text_grob("Chaque point représente un texte.\nLes lignes continues représentent une régression locale, les lignes discontinues une régression linéaire.",
                                  hjust = 1,
																																		x = 1,
																																		face = "italic",
																																		size = 10))
