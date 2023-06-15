#Run afer A5_diachro_plot_context to get evolution for each lemma

POS_context_count_lemma <- POS_context %>% count(cat,id, lemma)


POS_context_count_lemma <- merge (POS_context_count_lemma,
                            Dates_NCA,
                            by="id")

#Transform date values into numeric values (there are discreet for now)
POS_context_count_lemma$datecomposition <- as.numeric(as.character(POS_context_count_lemma$datecomposition))
POS_context_count_lemma$datemanuscrit <- as.numeric(as.character(POS_context_count_lemma$datemanuscrit))

#-------
# POS__C: behaviour of forms before C-initial word
#-------
POS_E_C_lemma_count <- POS_E_C_cleaned %>% count(id, lemma) %>%
                      rename(n.E = n)
POS_nonE_C_lemma_count <- POS_nonE_C_cleaned %>% count(id, lemma) %>%
                      rename(n.nonE = n)
POS__C_lemma <- full_join(POS_E_C_lemma_count,
                    POS_nonE_C_lemma_count,
																			 by = c("id", "lemma"))
POS__C_lemma <- replace(POS__C_lemma, is.na(POS__C_lemma), 0)
POS__C_lemma <- POS__C_lemma %>% mutate(Nb_POS_EnonE = n.E + n.nonE) %>%
			                  mutate(Tx_POS_EnonE = n.nonE / Nb_POS_EnonE) %>%
																					mutate(Minus = n.nonE - n.E)

#Prepare table data
POS__C_lemma_summary <- POS__C_lemma %>%
    group_by(lemma) %>%
	   summarise(nb.e = sum(n.E),
				          nb.Ø = sum(n.nonE),
														tot. = sum(Nb_POS_EnonE)) %>%
				mutate(tx.Ø = nb.Ø / tot.) %>%
				arrange(desc(tx.Ø), desc(tot.))
POS__C_lemma_summary$tx.Ø <- round(POS__C_lemma_summary$tx.Ø, digits=2)
	#Truncate table
	if (nrow(POS__C_lemma_summary) > 10)
	 {
			#Prepare resume of truncated data
	  others_C <- POS__C_lemma_summary %>%
	    filter(row_number() > 10)
					 others_C.nb.lemma <- nrow(others_C)
					 others_C.nb.lemma <- paste("[autres : ",others_C.nb.lemma,"]", sep="")
		   others_C.e <- sum(others_C$nb.e)
		   others_C.nonE <- sum(others_C$nb.Ø)
		   others_C.tot <- others_C.e + others_C.nonE
					others_C.rate <- others_C.nonE / others_C.tot
					 others_C.rate <- round(others_C.rate, digits=2)
					others_C.row <- data.frame(lemma = others_C.nb.lemma, nb.e = others_C.e, nb.Ø = others_C.nonE, tot. = others_C.tot, tx.Ø = others_C.rate)
	  #Truncate data
			POS__C_lemma_summary <- POS__C_lemma_summary %>%
		   filter(!row_number() > 10)
			#Append resume of truncated data
			POS__C_lemma_summary <- bind_rows(POS__C_lemma_summary, others_C.row)
		}

## Reinject metadata from Dates_NCA
POS__C_lemma <- merge (POS__C_lemma,
										       Dates_NCA,
										       by="id")

POS__C_lemma$datecomposition <- as.numeric(as.character(POS__C_lemma$datecomposition))
POS__C_lemma$datemanuscrit <- as.numeric(as.character(POS__C_lemma$datemanuscrit))
POS__C_lemma$datemoyennedees <- as.numeric(as.character(POS__C_lemma$datemoyennedees))

POS__C_lemma_mean <- mean(POS__C_lemma$Tx_POS_EnonE, na.rm=TRUE)
  POS__C_lemma_mean <- round(POS__C_lemma_mean, digits=2)
POS__C_lemma_median <- median(POS__C_lemma$Tx_POS_EnonE, na.rm=TRUE)
  POS__C_lemma_median <- round(POS__C_lemma_median, digits=2)
POS__C_lemma_sd <- sd(POS__C_lemma$Tx_POS_EnonE, na.rm=TRUE)
  POS__C_lemma_sd <- round(POS__C_lemma_sd, digits=2)
POS__C_lemma_sum <- sum(POS__C_lemma$Nb_POS_EnonE, na.rm=TRUE)
  POS__C_lemma_sum <- round(POS__C_sum, digits=2)
POS__C_lemma_date_mean <- mean(POS__C_lemma$datecomposition, na.rm=TRUE)
  POS__C_lemma_date_mean <- round(POS__C_lemma_date_mean, digits=0)
POS__C_lemma_corr <- wtd.cor(POS__C_lemma$datecomposition, POS__C_lemma$Tx_POS_EnonE)
 POS__C_lemma_corr <- as.data.frame(POS__C_corr)
  POS__C_lemma_rho <- POS__C_lemma_corr$correlation
		 POS__C_lemma_rho <- round(POS__C_lemma_rho, digits=3)
		POS__C_lemma_SE <- POS__C_lemma_corr$std.err
		 POS__C_lemma_SE <- round(POS__C_lemma_SE, digits=3)
		POS__C_lemma_tvalue <- POS__C_lemma_corr$t.value
			POS__C_lemma_tvalue <- round(POS__C_lemma_tvalue, digits=3)
		POS__C_lemma_pvalue <- POS__C_lemma_corr$p.value
			POS__C_lemma_pvalue <- round(POS__C_lemma_pvalue, digits=5)

p_POS__C_lemma <- ggplot(POS__C_lemma, aes(datecomposition, Tx_POS_EnonE, size=0.5)) +
geom_point(aes(shape=lemma, color=lemma)) +
scale_shape_manual(values = c(letters, LETTERS)) +
scale_color_viridis(discrete = TRUE, end=0.8) +
	# geom_smooth(method = "loess",
	# 												se = FALSE,
	# 												na.rm = TRUE,
	# 												colour = "grey",
	# 												span = 0.75)  +
 # geom_smooth(method = "lm",
 #             se = FALSE,
 #             na.rm = TRUE,
 #             colour = "grey",
 #             linetype = "dashed") +
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
                    name="Devant #C, taux de Ø") +
	theme(legend.position = c(0.8, 0.5)) +
	guides(size = FALSE,
		      shape = guide_legend(override.aes = list(size = 3.5)))

	POS__C_theme <- ttheme(tbody.style = tbody_style(
	      hjust = as.vector(matrix(c(0, 1, 1, 1, 1), ncol = 5, nrow = nrow(POS__C_lemma_summary), byrow = TRUE)),
	      x = as.vector(matrix(c(.1, .9, .9, .9, .9), ncol = 5, nrow = nrow(POS__C_lemma_summary), byrow = TRUE)),
						 fill=FALSE),
					colnames.style = colnames_style(
						hjust = as.vector(matrix(c(0, 0.5, 0.5, 0.5, 0.5), ncol = 5)),
						x = as.vector(matrix(c(.1, .5, .5, .5, .5), ncol = 5)),
					 face = "plain",
					 fill=FALSE),
					)

	POS__C_lemma_summary_table <- ggtexttable(POS__C_lemma_summary, rows = NULL, theme = POS__C_theme)
	POS__C_lemma_summary_table <- POS__C_lemma_summary_table %>%
	 tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 1) %>%
		tab_add_hline(at.row = tab_nrow(POS__C_lemma_summary_table), row.side = "bottom", linewidth = 1)

	p_POS__C_lemma_arrange <- ggarrange(p_POS__C_lemma,
											POS__C_lemma_summary_table,
											labels=NULL,
											ncol = 1,
											nrow = 2,
										 heights = c(1, 0.5))

#-------
# POS__V: behaviour of forms before V-initial word
#-------
POS_E_V_lemma_count <- POS_E_V_cleaned %>% count(id, lemma) %>%
                 rename(n.E = n)
POS_nonE_V_lemma_count <- POS_nonE_V_cleaned %>% count(id, lemma)  %>%
                 rename(n.nonE = n)
POS__V_lemma <- full_join(POS_E_V_lemma_count,
                    POS_nonE_V_lemma_count,
																			 by = c("id", "lemma"))
POS__V_lemma <- replace(POS__V_lemma, is.na(POS__V_lemma), 0)
POS__V_lemma <- POS__V_lemma %>% mutate(Nb_POS_EnonE = n.E + n.nonE) %>%
			                  mutate(Tx_POS_EnonE = n.nonE / Nb_POS_EnonE)

#Prepare table data
POS__V_lemma_summary <- POS__V_lemma %>%
    group_by(lemma) %>%
	   summarise(nb.e = sum(n.E),
				          nb.Ø = sum(n.nonE),
														tot. = sum(Nb_POS_EnonE)) %>%
				mutate(tx.Ø = nb.Ø / tot.) %>%
				arrange(desc(tx.Ø), desc(tot.))
POS__V_lemma_summary$tx.Ø <- round(POS__V_lemma_summary$tx.Ø, digits=2)
	#Truncate table
	if (nrow(POS__V_lemma_summary) > 10)
	 {
			#Prepare resume of truncated data
	  others <- POS__V_lemma_summary %>%
	    filter(row_number() > 10)
					 others.nb.lemma <- nrow(others)
					 others.nb.lemma <- paste("[autres : ",others.nb.lemma,"]", sep="")
		   others.e <- sum(others$nb.e)
		   others.nonE <- sum(others$nb.Ø)
		   others.tot <- others.e + others.nonE
					others.rate <- others.nonE / others.tot
					 others.rate <- round(others.rate, digits=2)
					others.row <- data.frame(lemma = others.nb.lemma, nb.e = others.e, nb.Ø = others.nonE, tot. = others.tot, tx.Ø = others.rate)
	  #Truncate data
			POS__V_lemma_summary <- POS__V_lemma_summary %>%
		   filter(!row_number() > 10)
			#Append resume of truncated data
			POS__V_lemma_summary <- bind_rows(POS__V_lemma_summary, others.row)
		}


## Reinject metadata from Dates_NCA
POS__V_lemma <- merge (POS__V_lemma,
										       Dates_NCA,
										       by="id")

POS__V_lemma$datecomposition <- as.numeric(as.character(POS__V_lemma$datecomposition))
POS__V_lemma$datemanuscrit <- as.numeric(as.character(POS__V_lemma$datemanuscrit))
POS__V_lemma$datemoyennedees <- as.numeric(as.character(POS__V_lemma$datemoyennedees))

POS__V_lemma_mean <- mean(POS__V_lemma$Tx_POS_EnonE, na.rm=TRUE)
  POS__V_lemma_mean <- round(POS__V_lemma_mean, digits=2)
POS__V_lemma_median <- median(POS__V_lemma$Tx_POS_EnonE, na.rm=TRUE)
  POS__V_lemma_median <- round(POS__V_lemma_median, digits=2)
POS__V_lemma_sd <- sd(POS__V_lemma$Tx_POS_EnonE, na.rm=TRUE)
  POS__V_lemma_sd <- round(POS__V_lemma_sd, digits=2)
POS__V_lemma_sum <- sum(POS__V_lemma$Nb_POS_EnonE, na.rm=TRUE)
  POS__V_lemma_sum <- round(POS__V_lemma_sum, digits=2)
POS__V_lemma_date_mean <- mean(POS__V_lemma$datecomposition, na.rm=TRUE)
  POS__V_lemma_date_mean <- round(POS__V_lemma_date_mean, digits=0)
POS__V_lemma_corr <- wtd.cor(POS__V_lemma$datecomposition, POS__V_lemma$Tx_POS_EnonE)
 POS__V_lemma_corr <- as.data.frame(POS__V_lemma_corr)
  POS__V_lemma_rho <- POS__V_lemma_corr$correlation
		 POS__V_lemma_rho <- round(POS__V_lemma_rho, digits=3)
		POS__V_lemma_SE <- POS__V_lemma_corr$std.err
		 POS__V_lemma_SE <- round(POS__V_lemma_SE, digits=3)
		POS__V_lemma_tvalue <- POS__V_lemma_corr$t.value
			POS__V_lemma_tvalue <- round(POS__V_lemma_tvalue, digits=3)
		POS__V_lemma_pvalue <- POS__V_lemma_corr$p.value
			POS__V_lemma_pvalue <- round(POS__V_lemma_pvalue, digits=5)

p_POS__V_lemma <- ggplot(POS__V_lemma, aes(x=datecomposition, y=Tx_POS_EnonE, size=0.5)) +
	geom_point(aes(shape=lemma, color=lemma)) +
	scale_shape_manual(values = c(letters, LETTERS)) +
	scale_color_viridis(discrete = TRUE, end=0.8) +
	# geom_smooth(mapping= aes(linetype=lemma),
	# 	method = "loess",
	# 												se = FALSE,
	# 												na.rm = TRUE,
	# 												span = 0.75,
	# 											 colour = "grey")  +
 # geom_smooth(method = "lm",
 #             se = FALSE,
 #             na.rm = TRUE,
 #             colour = "grey",
 #             linetype = "dashed") +
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
                    name="Devant #V, taux de Ø") +
	theme(legend.position = c(0.8, 0.5)) +
	guides(size = FALSE,
		      shape = guide_legend(override.aes = list(size = 3.5)))

POS__V_theme <- ttheme(tbody.style = tbody_style(
      hjust = as.vector(matrix(c(0, 1, 1, 1, 1), ncol = 5, nrow = nrow(POS__V_lemma_summary), byrow = TRUE)),
      x = as.vector(matrix(c(.1, .9, .9, .9, .9), ncol = 5, nrow = nrow(POS__V_lemma_summary), byrow = TRUE)),
					 fill=FALSE),
				colnames.style = colnames_style(
					hjust = as.vector(matrix(c(0, 0.5, 0.5, 0.5, 0.5), ncol = 5)),
					x = as.vector(matrix(c(.1, .5, .5, .5, .5), ncol = 5)),
				 face = "plain",
				 fill=FALSE),
				)

POS__V_lemma_summary_table <- ggtexttable(POS__V_lemma_summary, rows = NULL, theme = POS__V_theme)
POS__V_lemma_summary_table <- POS__V_lemma_summary_table %>%
 tab_add_hline(at.row = 1:2, row.side = "top", linewidth = 1) %>%
	tab_add_hline(at.row = tab_nrow(POS__V_lemma_summary_table), row.side = "bottom", linewidth = 1)

p_POS__V_lemma_arrange <- ggarrange(p_POS__V_lemma,
										POS__V_lemma_summary_table,
										labels=NULL,
										ncol = 1,
										nrow = 2,
									 heights = c(1, 0.5))

p_POS__V_lemma_arrange
