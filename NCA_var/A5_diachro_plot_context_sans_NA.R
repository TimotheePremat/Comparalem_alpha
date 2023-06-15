#Add cat. column for merging
# POS_EC_V_cleaned <- POS_EC_V_cleaned %>%
#                      mutate(cat = "EC_V") %>%
#                      relocate(cat)
# POS_EC_C_cleaned <- POS_EC_C_cleaned %>%
#                      mutate(cat = "EC_C") %>%
#                      relocate(cat)
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

POS_context <- bind_rows(#POS_EC_V_cleaned,
	                        #POS_EC_C_cleaned,
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
# POS_E_: following context for schwa-ending forms
#-------
POS_E_V_count <- POS_E_V_cleaned %>% count(id) %>%
                 rename(n.V = n)
POS_E_C_count <- POS_E_C_cleaned %>% count(id) %>%
                 rename(n.C = n)
POS_E_ <- full_join(POS_E_V_count,
                    POS_E_C_count,
																			 by = "id")
#POS_E_ <- replace(POS_E_, is.na(POS_E_), 0)
POS_E_ <- POS_E_ %>% mutate(Nb_POS_E_ = n.V + n.C) %>%
			                  mutate(Tx_POS_E_V = n.V / Nb_POS_E_) %>%
																					mutate(Minus = n.C - n.V)

## Reinject metadata from Dates_NCA
POS_E_ <- merge (POS_E_,
										       Dates_NCA,
										       by="id")

POS_E_$datecomposition <- as.numeric(as.character(POS_E_$datecomposition))
POS_E_$datemanuscrit <- as.numeric(as.character(POS_E_$datemanuscrit))
POS_E_$datemoyennedees <- as.numeric(as.character(POS_E_$datemoyennedees))

POS_E__mean <- mean(POS_E_$Tx_POS_E_V, na.rm=TRUE)
  POS_E__mean <- round(POS_E__mean, digits=2)
POS_E__median <- median(POS_E_$Tx_POS_E_V, na.rm=TRUE)
  POS_E__median <- round(POS_E__median, digits=2)
POS_E__sd <- sd(POS_E_$Tx_POS_E_V, na.rm=TRUE)
  POS_E__sd <- round(POS_E__sd, digits=2)
POS_E__sum <- sum(POS_E_$Nb_POS_E_, na.rm=TRUE)
  POS_E__sum <- round(POS_E__sum, digits=2)
POS_E__date_mean <- mean(POS_E_$datecomposition, na.rm=TRUE)
  POS_E__date_mean <- round(POS_E__date_mean, digits=0)
POS_E__corr <- wtd.cor(POS_E_$datecomposition, POS_E_$Tx_POS_E_V)
 POS_E__corr <- as.data.frame(POS_E__corr)
  POS_E__rho <- POS_E__corr$correlation
		 POS_E__rho <- round(POS_E__rho, digits=3)
		POS_E__SE <- POS_E__corr$std.err
		 POS_E__SE <- round(POS_E__SE, digits=3)
		POS_E__tvalue <- POS_E__corr$t.value
			POS_E__tvalue <- round(POS_E__tvalue, digits=3)
		POS_E__pvalue <- POS_E__corr$p.value
			POS_E__pvalue <- round(POS_E__pvalue, digits=5)

p_POS_E_ <- ggplot(POS_E_, aes(datecomposition, Tx_POS_E_V)) +
	geom_point() +
	scale_size(range = c(0, 5)) +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "grey",
													span = 0.75)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "grey",
             linetype = "dashed")  +
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
                    name=paste("Taux de #V après",schwa))

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
										label=paste("",POS_E__rho,"\n",
										            #POS_E__pvalue,"\n",
										            POS_E__mean,"\n",
																						POS_E__median,"\n",
																						POS_E__sd,"\n",
																						POS_E__sum,"\n",
																					 POS_E__date_mean),
										hjust=0,
										vjust=0) +
	annotate("text",
										x=1425,
										y=0,
										label=paste(rho,"\n",
										            #"p","\n",
																						"moy. \n",
																						"méd. \n",
																						"sd \n",
																						"nb. obs. \n",
																						"date moy."),
										hjust=1,
										vjust=0) #+
										# geom_richtext(x = 1425,
										# 				          y = 0,
										# 				          label = "&rho;\n
										# 				                  <i>p</i><br>
										# 				          								moy.<br>
										# 				          								méd.<br>
										# 				          								sd<br>
										# 				          								nb. obs.<br>
										# 				          								date. moy.",
		        #               hjust=1,
		        #               vjust=0)
	# stat_cor(p.accuracy = 0.001,
	# 									r.accuracy = 0.01,
	# 									cor.coef.name = "rho",
	# 									method="pearson",
	# 									label.sep = "\n",
	# 									label.x = 1475,
	# 									label.y = 0.5,
	# 								 hjust=1)

##--
##Weighted
##--
POS_E__w_mean <- weighted.mean(POS_E_$Tx_POS_E_V, w = POS_E_$Nb_POS_E, na.rm=TRUE)
  POS_E__w_mean <- round(POS_E__w_mean, digits=2)
POS_E__w_median <- weighted.median(POS_E_$Tx_POS_E_V, w = POS_E_$Nb_POS_E, na.rm=TRUE)
		POS_E__w_median <- round(POS_E__w_median, digits=2)
POS_E__w_sd <- wtd.var(POS_E_$Tx_POS_E_V, w = POS_E_$Nb_POS_E, na.rm=TRUE)
  POS_E__w_sd <- sqrt(POS_E__w_sd)
  POS_E__w_sd <- round(POS_E__w_sd, digits=2)
POS_E__w_date_mean <- weighted.mean(POS_E_$datecomposition, w = POS_E_$Nb_POS_E, na.rm=TRUE)
  POS_E__w_date_mean <- round(POS_E__w_date_mean, digits=0)
# POS_E__rho_w <- wtd.cors(POS_E_$datecomposition, POS_E_$Tx_POS_E_V, POS_E_$Nb_POS_E_)
#   POS_E__rho_w <- round(POS_E__rho_w, digits=3)
POS_E__w_corr <- wtd.cor(POS_E_$datecomposition, POS_E_$Tx_POS_E_V, POS_E_$Nb_POS_E_)
 POS_E__w_corr <- as.data.frame(POS_E__w_corr)
  POS_E__w_rho <- POS_E__w_corr$correlation
		 POS_E__w_rho <- round(POS_E__w_rho, digits=3)
		POS_E__w_SE <- POS_E__w_corr$std.err
		 POS_E__w_SE <- round(POS_E__w_SE, digits=3)
		POS_E__w_tvalue <- POS_E__w_corr$t.value
			POS_E__w_tvalue <- round(POS_E__w_tvalue, digits=3)
		POS_E__w_pvalue <- POS_E__w_corr$p.value
			POS_E__w_pvalue <- round(POS_E__w_pvalue, digits=5)

p_POS_E_w <- ggplot(POS_E_, aes(datecomposition, Tx_POS_E_V, size=Nb_POS_E_)) +
	geom_point() +
	scale_size(range = c(1, 5)) +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "grey",
													span = 0.75,
												 aes(weight=Nb_POS_E_),
												 show.legend = FALSE)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "grey",
             linetype = "dashed",
												 aes(weight=Nb_POS_E_),
												 show.legend = FALSE)  +
 theme_classic() +
	theme(legend.position = c(.95, .95),
       legend.justification = c("right", "top"),
       legend.box.just = "right",
       legend.margin = margin(6, 6, 6, 6)) +
	labs(size="Nb obs.") +
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
                    name=paste("Taux de #V après",schwa))

	p_POS_E_w_arrange <- p_POS_E_w +
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
										label=paste("",POS_E__w_rho,"\n",
																						#POS_E__pvalue,"\n",
																						POS_E__w_mean,"\n",
																						POS_E__w_median,"\n",
																						POS_E__w_sd,"\n",
																						POS_E__sum,"\n",
																						POS_E__w_date_mean),
										hjust=0,
										vjust=0) +
	annotate("text",
										x=1425,
										y=0,
										label=paste(rho,"\n",
																						#"p","\n",
																						"moy. \n",
																						"méd. \n",
																						"sd \n",
																						"nb. obs. \n",
																						"date moy."),
										hjust=1,
										vjust=0)
	# annotate("text",
	# 									x=1450,
	# 									y=0,
	# 									label=paste("",POS_E__rho_w,"\n",
	# 									            POS_E__w_mean,"\n",
	# 																					POS_E__w_median,"\n",
	# 																					POS_E__w_sd,"\n",
	# 																					POS_E__sum,"\n",
	# 																				 POS_E__w_date_mean),
	# 									hjust=0,
	# 									vjust=0) +
	# annotate("text",
	# 									x=1425,
	# 									y=0,
	# 									label="ρ\n moy.\n méd.\n sd\n nb. obs.\n date moy.",
	# 									hjust=1,
	# 									vjust=0)


	# annotate("text",
	# 									x=1450,
	# 									y=0.5,
	# 									label=paste(POS_E__rho_w),
	# 									hjust=0,
	# 									vjust=0)
	#
	#
	# stat_cor(p.accuracy = 0.001,
	# 									r.accuracy = 0.01,
	# 									cor.coef.name = "rho",
	# 									method="pearson",
	# 									label.sep = "\n",
	# 									label.x = 1475,
	# 									label.y = 0.5,
	# 								 hjust=1)

#-------
# POS_nonE_: following context for zero-ending forms
#-------
POS_nonE_V_count <- POS_nonE_V_cleaned %>% count(id) %>%
                 rename(n.V = n)
POS_nonE_C_count <- POS_nonE_C_cleaned %>% count(id) %>%
                 rename(n.C = n)
POS_nonE_ <- full_join(POS_nonE_V_count,
                    POS_nonE_C_count,
																			 by = "id")
#POS_nonE_ <- replace(POS_nonE_, is.na(POS_nonE_), 0)
POS_nonE_ <- POS_nonE_ %>% mutate(Nb_POS_nonE_ = n.V + n.C) %>%
			                  mutate(Tx_POS_nonE_V = n.V / Nb_POS_nonE_) %>%
																					mutate(Minus = n.C - n.V)

## Reinject metadata from Dates_NCA
POS_nonE_ <- merge (POS_nonE_,
										       Dates_NCA,
										       by="id")

POS_nonE_$datecomposition <- as.numeric(as.character(POS_nonE_$datecomposition))
POS_nonE_$datemanuscrit <- as.numeric(as.character(POS_nonE_$datemanuscrit))
POS_nonE_$datemoyennedees <- as.numeric(as.character(POS_nonE_$datemoyennedees))

POS_nonE__mean <- mean(POS_nonE_$Tx_POS_nonE_V, na.rm=TRUE)
  POS_nonE__mean <- round(POS_nonE__mean, digits=2)
POS_nonE__median <- median(POS_nonE_$Tx_POS_nonE_V, na.rm=TRUE)
  POS_nonE__median <- round(POS_nonE__median, digits=2)
POS_nonE__sd <- sd(POS_nonE_$Tx_POS_nonE_V, na.rm=TRUE)
  POS_nonE__sd <- round(POS_nonE__sd, digits=2)
POS_nonE__sum <- sum(POS_nonE_$Nb_POS_nonE_, na.rm=TRUE)
  POS_nonE__sum <- round(POS_nonE__sum, digits=2)
POS_nonE__date_mean <- mean(POS_nonE_$datecomposition, na.rm=TRUE)
  POS_nonE__date_mean <- round(POS_nonE__date_mean, digits=0)
POS_nonE__corr <- wtd.cor(POS_nonE_$datecomposition, POS_nonE_$Tx_POS_nonE_V)
 POS_nonE__corr <- as.data.frame(POS_nonE__corr)
  POS_nonE__rho <- POS_nonE__corr$correlation
		 POS_nonE__rho <- round(POS_nonE__rho, digits=3)
		POS_nonE__SE <- POS_nonE__corr$std.err
		 POS_nonE__SE <- round(POS_nonE__SE, digits=3)
		POS_nonE__tvalue <- POS_nonE__corr$t.value
			POS_nonE__tvalue <- round(POS_nonE__tvalue, digits=3)
		POS_nonE__pvalue <- POS_nonE__corr$p.value
			POS_nonE__pvalue <- round(POS_nonE__pvalue, digits=5)

p_POS_nonE_ <- ggplot(POS_nonE_, aes(datecomposition, Tx_POS_nonE_V)) +
	geom_point() +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "grey",
													span = 0.75,
												 show.legend = FALSE)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "grey",
             linetype = "dashed",
												 show.legend = FALSE)  +
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
                    name=paste("Taux de #V après Ø"))

	p_POS_nonE_arrange <- p_POS_nonE_ +
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
									label=paste("",POS_nonE__rho,"\n",
																					#POS_E__pvalue,"\n",
																					POS_nonE__mean,"\n",
																					POS_nonE__median,"\n",
																					POS_nonE__sd,"\n",
																					POS_nonE__sum,"\n",
																					POS_nonE__date_mean),
									hjust=0,
									vjust=0) +
annotate("text",
									x=1425,
									y=0,
									label=paste(rho,"\n",
																					#"p","\n",
																					"moy. \n",
																					"méd. \n",
																					"sd \n",
																					"nb. obs. \n",
																					"date moy."),
									hjust=1,
									vjust=0)

##--
##Weighted
##--

POS_nonE__w_mean <- weighted.mean(POS_nonE_$Tx_POS_nonE_V, w = POS_nonE_$Nb_POS_nonE, na.rm=TRUE)
  POS_nonE__w_mean <- round(POS_nonE__w_mean, digits=2)
POS_nonE__w_median <- weighted.median(POS_nonE_$Tx_POS_nonE_V, w = POS_nonE_$Nb_POS_nonE, na.rm=TRUE)
		POS_nonE__w_median <- round(POS_nonE__w_median, digits=2)
POS_nonE__w_sd <- wtd.var(POS_nonE_$Tx_POS_nonE_V, w = POS_nonE_$Nb_POS_nonE, na.rm=TRUE)
  POS_nonE__w_sd <- sqrt(POS_nonE__w_sd)
  POS_nonE__w_sd <- round(POS_nonE__w_sd, digits=2)
POS_nonE__w_date_mean <- weighted.mean(POS_nonE_$datecomposition, w = POS_nonE_$Nb_POS_nonE, na.rm=TRUE)
  POS_nonE__w_date_mean <- round(POS_nonE__w_date_mean, digits=0)
POS_nonE__w_corr <- wtd.cor(POS_nonE_$datecomposition, POS_nonE_$Tx_POS_nonE_V, POS_nonE_$Nb_POS_nonE_)
 POS_nonE__w_corr <- as.data.frame(POS_nonE__w_corr)
  POS_nonE__w_rho <- POS_nonE__w_corr$correlation
		 POS_nonE__w_rho <- round(POS_nonE__w_rho, digits=3)
		POS_nonE__w_SE <- POS_nonE__w_corr$std.err
		 POS_nonE__w_SE <- round(POS_nonE__w_SE, digits=3)
		POS_nonE__w_tvalue <- POS_nonE__w_corr$t.value
			POS_nonE__w_tvalue <- round(POS_nonE__w_tvalue, digits=3)
		POS_nonE__w_pvalue <- POS_nonE__w_corr$p.value
			POS_nonE__w_pvalue <- round(POS_nonE__w_pvalue, digits=5)

p_POS_nonE_w <- ggplot(POS_nonE_, aes(datecomposition, Tx_POS_nonE_V, size=Nb_POS_nonE_)) +
	geom_point() +
	scale_size(range = c(1, 5)) +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "grey",
													span = 0.75,
												 aes(weight=Nb_POS_nonE_),
												 show.legend = FALSE)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "grey",
             linetype = "dashed",
												 aes(weight=Nb_POS_nonE_),
												 show.legend = FALSE)  +
 theme_classic() +
	theme(legend.position = c(.95, .95),
       legend.justification = c("right", "top"),
       legend.box.just = "right",
       legend.margin = margin(6, 6, 6, 6)) +
	labs(size="Nb obs.") +
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
                    name=paste("Taux de #V après Ø"))

	p_POS_nonE_w_arrange <- p_POS_nonE_w +
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
									label=paste("",POS_nonE__w_rho,"\n",
																					#POS_E__pvalue,"\n",
																					POS_nonE__w_mean,"\n",
																					POS_nonE__w_median,"\n",
																					POS_nonE__w_sd,"\n",
																					POS_nonE__sum,"\n",
																					POS_nonE__w_date_mean),
									hjust=0,
									vjust=0) +
annotate("text",
									x=1425,
									y=0,
									label=paste(rho,"\n",
																					#"p","\n",
																					"moy. \n",
																					"méd. \n",
																					"sd \n",
																					"nb. obs. \n",
																					"date moy."),
									hjust=1,
									vjust=0)

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
#POS__C <- replace(POS__C, is.na(POS__C), 0)
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
POS__C_date_mean <- mean(POS__C$datecomposition, na.rm=TRUE)
  POS__C_date_mean <- round(POS__C_date_mean, digits=0)
POS__C_corr <- wtd.cor(POS__C$datecomposition, POS__C$Tx_POS_EnonE)
 POS__C_corr <- as.data.frame(POS__C_corr)
  POS__C_rho <- POS__C_corr$correlation
		 POS__C_rho <- round(POS__C_rho, digits=3)
		POS__C_SE <- POS__C_corr$std.err
		 POS__C_SE <- round(POS__C_SE, digits=3)
		POS__C_tvalue <- POS__C_corr$t.value
			POS__C_tvalue <- round(POS__C_tvalue, digits=3)
		POS__C_pvalue <- POS__C_corr$p.value
			POS__C_pvalue <- round(POS__C_pvalue, digits=5)

p_POS__C <- ggplot(POS__C, aes(datecomposition, Tx_POS_EnonE)) +
	geom_point() +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "grey",
													span = 0.75)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "grey",
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
                    name="Devant #C, taux de Ø")

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
									label=paste("",POS__C_rho,"\n",
																					#POS_E__pvalue,"\n",
																					POS__C_mean,"\n",
																					POS__C_median,"\n",
																					POS__C_sd,"\n",
																					POS__C_sum,"\n",
																					POS__C_date_mean),
									hjust=0,
									vjust=0) +
annotate("text",
									x=1425,
									y=0,
									label=paste(rho,"\n",
																					#"p","\n",
																					"moy. \n",
																					"méd. \n",
																					"sd \n",
																					"nb. obs. \n",
																					"date moy."),
									hjust=1,
									vjust=0)

##--
##Weighted
##--

POS__C_w_mean <- weighted.mean(POS__C$Tx_POS_EnonE, w = POS__C$Nb_POS_EnonE, na.rm=TRUE)
  POS__C_w_mean <- round(POS__C_w_mean, digits=2)
POS__C_w_median <- weighted.median(POS__C$Tx_POS_EnonE, w = POS__C$Nb_POS_EnonE, na.rm=TRUE)
		POS__C_w_median <- round(POS__C_w_median, digits=2)
POS__C_w_sd <- wtd.var(POS__C$Tx_POS_EnonE, w = POS__C$Nb_POS_EnonE, na.rm=TRUE)
  POS__C_w_sd <- sqrt(POS__C_w_sd)
  POS__C_w_sd <- round(POS__C_w_sd, digits=2)
POS__C_w_date_mean <- weighted.mean(POS__C$datecomposition, w = POS__C$Nb_POS_EnonE, na.rm=TRUE)
  POS__C_w_date_mean <- round(POS__C_w_date_mean, digits=0)
POS__C_w_corr <- wtd.cor(POS__C$datecomposition, POS__C$Tx_POS_EnonE, POS__C$Nb_POS_EnonE)
 POS__C_w_corr <- as.data.frame(POS__C_w_corr)
  POS__C_w_rho <- POS__C_w_corr$correlation
		 POS__C_w_rho <- round(POS__C_w_rho, digits=3)
		POS__C_w_SE <- POS__C_w_corr$std.err
		 POS__C_w_SE <- round(POS__C_w_SE, digits=3)
		POS__C_w_tvalue <- POS__C_w_corr$t.value
			POS__C_w_tvalue <- round(POS__C_w_tvalue, digits=3)
		POS__C_w_pvalue <- POS__C_w_corr$p.value
			POS__C_w_pvalue <- round(POS__C_w_pvalue, digits=5)

p_POS__C_w <- ggplot(POS__C, aes(x=datecomposition, y=Tx_POS_EnonE, size=Nb_POS_EnonE)) +
 geom_point() +
 scale_size(range = c(1, 5)) +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "grey",
													span = 0.75,
													aes(weight=Nb_POS_EnonE),
												 show.legend = FALSE)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "grey",
             linetype = "dashed",
												 aes(weight=Nb_POS_EnonE),
												 show.legend = FALSE)  +
 theme_classic() +
	theme(legend.position = c(.95, .95),
       legend.justification = c("right", "top"),
       legend.box.just = "right",
       legend.margin = margin(6, 6, 6, 6)) +
	labs(size="Nb obs.") +
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
                    name="Devant #C, taux de Ø")

p_POS__C_w_arrange <- p_POS__C_w +
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
									label=paste("",POS__C_w_rho,"\n",
																					#POS_E__pvalue,"\n",
																					POS__C_w_mean,"\n",
																					POS__C_w_median,"\n",
																					POS__C_w_sd,"\n",
																					POS__C_sum,"\n",
																					POS__C_w_date_mean),
									hjust=0,
									vjust=0) +
annotate("text",
									x=1425,
									y=0,
									label=paste(rho,"\n",
																					#"p","\n",
																					"moy. \n",
																					"méd. \n",
																					"sd \n",
																					"nb. obs. \n",
																					"date moy."),
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
#POS__V <- replace(POS__V, is.na(POS__V), 0)
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
POS__V_date_mean <- mean(POS__V$datecomposition, na.rm=TRUE)
  POS__V_date_mean <- round(POS__V_date_mean, digits=0)
POS__V_corr <- wtd.cor(POS__V$datecomposition, POS__V$Tx_POS_EnonE)
 POS__V_corr <- as.data.frame(POS__V_corr)
  POS__V_rho <- POS__V_corr$correlation
		 POS__V_rho <- round(POS__V_rho, digits=3)
		POS__V_SE <- POS__V_corr$std.err
		 POS__V_SE <- round(POS__V_SE, digits=3)
		POS__V_tvalue <- POS__V_corr$t.value
			POS__V_tvalue <- round(POS__V_tvalue, digits=3)
		POS__V_pvalue <- POS__V_corr$p.value
			POS__V_pvalue <- round(POS__V_pvalue, digits=5)

p_POS__V <- ggplot(POS__V, aes(x=datecomposition, y=Tx_POS_EnonE)) +
	geom_point() +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "grey",
													span = 0.75)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "grey",
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
                    name="Devant #V, taux de Ø")

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
									label=paste("",POS__V_rho,"\n",
																					#POS_E__pvalue,"\n",
																					POS__V_mean,"\n",
																					POS__V_median,"\n",
																					POS__V_sd,"\n",
																					POS__V_sum,"\n",
																					POS__V_date_mean),
									hjust=0,
									vjust=0) +
annotate("text",
									x=1425,
									y=0,
									label=paste(rho,"\n",
																					#"p","\n",
																					"moy. \n",
																					"méd. \n",
																					"sd \n",
																					"nb. obs. \n",
																					"date moy."),
									hjust=1,
									vjust=0)

##--
##Weighted
##--

POS__V_w_mean <- weighted.mean(POS__V$Tx_POS_EnonE, w = POS__V$Nb_POS_EnonE, na.rm=TRUE)
  POS__V_w_mean <- round(POS__V_w_mean, digits=2)
POS__V_w_median <- weighted.median(POS__V$Tx_POS_EnonE, w = POS__V$Nb_POS_EnonE, na.rm=TRUE)
		POS__V_w_median <- round(POS__V_w_median, digits=2)
POS__V_w_sd <- wtd.var(POS__V$Tx_POS_EnonE, w = POS__V$Nb_POS_EnonE, na.rm=TRUE)
  POS__V_w_sd <- sqrt(POS__V_w_sd)
  POS__V_w_sd <- round(POS__V_w_sd, digits=2)
POS__V_w_date_mean <- weighted.mean(POS__V$datecomposition, w = POS__V$Nb_POS_EnonE, na.rm=TRUE)
  POS__V_w_date_mean <- round(POS__V_w_date_mean, digits=0)
POS__V_w_corr <- wtd.cor(POS__V$datecomposition, POS__V$Tx_POS_EnonE, POS__V$Nb_POS_EnonE)
 POS__V_w_corr <- as.data.frame(POS__V_w_corr)
  POS__V_w_rho <- POS__V_w_corr$correlation
		 POS__V_w_rho <- round(POS__V_w_rho, digits=3)
		POS__V_w_SE <- POS__V_w_corr$std.err
		 POS__V_w_SE <- round(POS__V_w_SE, digits=3)
		POS__V_w_tvalue <- POS__V_w_corr$t.value
			POS__V_w_tvalue <- round(POS__V_w_tvalue, digits=3)
		POS__V_w_pvalue <- POS__V_w_corr$p.value
			POS__V_w_pvalue <- round(POS__V_w_pvalue, digits=5)

p_POS__V_w <- ggplot(POS__V, aes(x=datecomposition, y=Tx_POS_EnonE, size=Nb_POS_EnonE)) +
 geom_point() +
 scale_size(range = c(1, 5)) +
	geom_smooth(method = "loess",
													se = FALSE,
													na.rm = TRUE,
													colour = "grey",
													span = 0.75,
													aes(weight=Nb_POS_EnonE),
												 show.legend = FALSE)  +
 geom_smooth(method = "lm",
             se = FALSE,
             na.rm = TRUE,
             colour = "grey",
             linetype = "dashed",
												 aes(weight=Nb_POS_EnonE),
												 show.legend = FALSE)  +
 theme_classic() +
	theme(legend.position = c(.95, .95),
       legend.justification = c("right", "top"),
       legend.box.just = "right",
       legend.margin = margin(6, 6, 6, 6)) +
	      labs(size="Nb obs.") +
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
                    name="Devant #V, taux de Ø")

p_POS__V_w_arrange <- p_POS__V_w +
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
									label=paste("",POS__V_w_rho,"\n",
																					#POS_E__pvalue,"\n",
																					POS__V_w_mean,"\n",
																					POS__V_w_median,"\n",
																					POS__V_w_sd,"\n",
																					POS__V_sum,"\n",
																					POS__V_w_date_mean),
									hjust=0,
									vjust=0) +
annotate("text",
									x=1425,
									y=0,
									label=paste(rho,"\n",
																					#"p","\n",
																					"moy. \n",
																					"méd. \n",
																					"sd \n",
																					"nb. obs. \n",
																					"date moy."),
									hjust=1,
									vjust=0)

#---------------------------------------------------------------------
#OVERVIEW (OVW) and stats
#---------------------------------------------------------------------
#Prep Data
 ##Depending on word ending
	POS_E_ <- POS_E_ %>% rename(Tx_POS__V = Tx_POS_E_V)
	POS_nonE_ <- POS_nonE_ %>% rename(Tx_POS__V = Tx_POS_nonE_V)

	POS_E_OVW <-  POS_E_ %>%
	               mutate(cat = "E") %>%
	               relocate(cat)
	POS_nonE_OVW <-  POS_nonE_ %>%
	               mutate(cat = "nonE") %>%
	               relocate(cat)
	POS_fin_OVW <- bind_rows(POS_E_OVW,POS_nonE_OVW)

	POS_E_OVW_in_nonE <- semi_join(POS_E_OVW,POS_nonE_OVW,by="id")
	POS_nonE_OVW_in_E <- semi_join(POS_nonE_OVW,POS_E_OVW,by="id")

	##Depending on following context
	POS__C_OVW <-  POS__C %>%
	               mutate(cat = "C") %>%
	               relocate(cat)
	POS__V_OVW <-  POS__V %>%
	               mutate(cat = "V") %>%
	               relocate(cat)
	POS_init_OVW <- bind_rows(POS__C_OVW,POS__V_OVW)

	POS__C_OVW_in_V <- semi_join(POS__C_OVW,POS__V_OVW,by="id")
	POS__V_OVW_in_C <- semi_join(POS__V_OVW,POS__C_OVW,by="id")

#compute stats
	##Depending on word ending
	E_vs_nonE_t <- t.test(POS_E_$Tx_POS__V,POS_nonE_$Tx_POS__V)
	 E_vs_nonE_t <- t.test(POS_E_$Tx_POS__V,POS_nonE_$Tx_POS__V)$statistic
			E_vs_nonE_t <- round(E_vs_nonE_t, digits=2)
		E_vs_nonE_p <- t.test(POS_E_$Tx_POS__V,POS_nonE_$Tx_POS__V)$p.value
		 E_vs_nonE_p <- round(E_vs_nonE_p, digits=10)
	E_vs_nonE_t_paired <- t.test(POS_nonE_OVW_in_E$Tx_POS__V,POS_E_OVW_in_nonE$Tx_POS__V,paired=TRUE)
	 E_vs_nonE_paired_t <- t.test(POS_nonE_OVW_in_E$Tx_POS__V,POS_E_OVW_in_nonE$Tx_POS__V,paired=TRUE)$statistic
			E_vs_nonE_paired_t <- round(E_vs_nonE_paired_t, digits=2)
	 E_vs_nonE_paired_p <- t.test(POS_nonE_OVW_in_E$Tx_POS__V,POS_E_OVW_in_nonE$Tx_POS__V,paired=TRUE)$p.value
		 E_vs_nonE_paired_p <- round(E_vs_nonE_paired_p, digits=10)
	##Depending on following context
	V_vs_C_t <- t.test(POS__V$Tx_POS_EnonE,POS__C$Tx_POS_EnonE)
	 V_vs_C_t <- t.test(POS__V$Tx_POS_EnonE,POS__C$Tx_POS_EnonE)$statistic
			V_vs_C_t <- round(V_vs_C_t, digits=2)
		V_vs_C_p <- t.test(POS__V$Tx_POS_EnonE,POS__C$Tx_POS_EnonE)$p.value
		 V_vs_C_p <- round(V_vs_C_p, digits=10)
	V_vs_C_t_paired <- t.test(POS__V_OVW_in_C$Tx_POS_EnonE,POS__C_OVW_in_V$Tx_POS_EnonE,paired=TRUE)
	 V_vs_C_paired_t <- t.test(POS__V_OVW_in_C$Tx_POS_EnonE,POS__C_OVW_in_V$Tx_POS_EnonE,paired=TRUE)$statistic
			V_vs_C_paired_t <- round(V_vs_C_paired_t, digits=2)
	 V_vs_C_paired_p <- t.test(POS__V_OVW_in_C$Tx_POS_EnonE,POS__C_OVW_in_V$Tx_POS_EnonE,paired=TRUE)$p.value
		 V_vs_C_paired_p <- round(V_vs_C_paired_p, digits=10)

#Prepare box plot
	##Depending on word ending
	Overview_E_vs_nonE <- ggplot(POS_fin_OVW, aes(x=cat, y=Tx_POS__V)) +
	  geom_boxplot() +
			theme_classic() +
			scale_y_continuous(limits = c(0,1),
																						name="Taux de #V") +
	  scale_x_discrete(name="Contexte subséquent",
	                   labels = c(
	                     "E" = paste(schwa,"#"),
	                     "nonE" = "Ø #"))

	# Overview_fin <- Overview_fin +
	# 	annotate("text",
	#           x=0.5,
	# 									 y=1,
	# 									 label=paste("Val. indépendantes\n",
	# 									             "","","t :",V_vs_C_t,"\n",
	# 																					 "","","p :",V_vs_C_p,"\n",
	# 																					 "Val. appairées\n",
	# 																					 "","","t :",V_vs_C_paired_t,"\n",
	# 																					 "","","p :",V_vs_C_paired_p),
	# 										hjust=0,
	# 									 vjust=1)

	##Depending on following context
	Overview_V_vs_C <- ggplot(POS_init_OVW, aes(x=cat, y=Tx_POS_EnonE)) +
	  geom_boxplot() +
			theme_classic() +
			scale_y_continuous(limits = c(0,1),
																						name="Taux de Ø#") +
	  scale_x_discrete(name="Contexte subséquent",
	                   labels = c(
	                     "C" = "#C",
	                     "V" = "#V"))

	# Overview_init <- Overview_init +
	# 	annotate("text",
	#           x=0.5,
	# 									 y=1,
	# 									 label=paste("Val. indépendantes\n",
	# 									             "","","t :",V_vs_C_t,"\n",
	# 																					 "","","p :",V_vs_C_p,"\n",
	# 																					 "Val. appairées\n",
	# 																					 "","","t :",V_vs_C_paired_t,"\n",
	# 																					 "","","p :",V_vs_C_paired_p),
	# 										hjust=0,
	# 									 vjust=1)

#---------------------------------------------------------------------
#ARRANGE plots together
#---------------------------------------------------------------------
#Prepare legend text
##Depending on word ending
	text_Student_E_vs_nonE_1 <- paste("Indép. : t =",E_vs_nonE_t,"; p = ",E_vs_nonE_p, sep=" ")
	text_Student_E_vs_nonE_2 <- paste("Dép. : t =",E_vs_nonE_paired_t,";","p =",E_vs_nonE_paired_p, sep=" ")
	 text_Student_E_vs_nonE <- paste(text_Student_E_vs_nonE_1, text_Student_E_vs_nonE_2, sep="\n")
			text_p_Student_E_vs_nonE <- ggparagraph(text = text_Student_E_vs_nonE, size = 11, color = "black")
	text_leg_1 <- paste("Chaque point représente un texte. Les lignes continues représentent une régression locale, les lignes discontinues une régression linéaire. En (c-d), les mesures sont pondérées par le nombre d'observations, figuré par la taille des points.")
	 text_p_leg_1 <- ggparagraph(text = text_leg_1, size = 11, face = "italic", color = "black")
	text_leg_2 <- paste("En (f), le résultat du test de Student est donné pour des variables indépendantes (Indép.) et pour des variables dépendantes (Dép.). Dans la version Dép., le test s'appuie sur la moyenne des différences de chaque texte entre les deux catégories (les textes n'ayant pas d'occurrence dans une catégorie sont retirés).")
	 text_p_leg_2 <- ggparagraph(text = text_leg_2, size = 11, face = "italic", color = "black")
##Depending on following context
	text_Student_V_vs_C_1 <- paste("Indép. : t =",V_vs_C_t,"; p = ",V_vs_C_p, sep=" ")
	text_Student_V_vs_C_2 <- paste("Dép. : t =",V_vs_C_paired_t,";","p =",V_vs_C_paired_p, sep=" ")
	 text_Student_V_vs_C <- paste(text_Student_V_vs_C_1, text_Student_V_vs_C_2, sep="\n")
			text_p_Student_V_vs_C <- ggparagraph(text = text_Student_V_vs_C, size = 11, color = "black")
	text_leg_1 <- paste("Chaque point représente un texte. Les lignes continues représentent une régression locale, les lignes discontinues une régression linéaire. En (c-d), les mesures sont pondérées par le nombre d'observations, figuré par la taille des points.")
	 text_p_leg_1 <- ggparagraph(text = text_leg_1, size = 11, face = "italic", color = "black")
	text_leg_2 <- paste("En (f), le résultat du test de Student est donné pour des variables indépendantes (Indép.) et pour des variables dépendantes (Dép.). Dans la version Dép., le test s'appuie sur la moyenne des différences de chaque texte entre les deux catégories (les textes n'ayant pas d'occurrence dans une catégorie sont retirés). Si p = 0, il y a plus de 10 décimales nulles avant une décimale > 0.")
	 text_p_leg_2 <- ggparagraph(text = text_leg_2, size = 11, face = "italic", color = "black")

#PLOT IT!
##Depending on word ending
	p_text_E_vs_nonE <- ggarrange(text_p_Student_E_vs_nonE,
	                 text_p_leg_1,
	                 text_p_leg_2,
								          ncol = 1,
								          nrow = 3,
																	 heights = c(0.5, 0.7, 1))
	p_text_E_vs_nonE <- annotate_figure(p_text_E_vs_nonE, top = text_grob("Test de Student", color = "black", face = "bold", size = 11))

	g_POS_E_vs_nonE_ALL_sans_NA <- ggarrange(p_POS_E_arrange,
		         p_POS_nonE_arrange,
		         p_POS_E_w_arrange,
		         p_POS_nonE_w_arrange,
											Overview_E_vs_nonE,
											p_text_E_vs_nonE,
	          labels="auto",
	          ncol = 2,
	          nrow = 3,
										 align = "v")
	g_POS_E_vs_nonE_ALL_sans_NA <- annotate_figure(g_POS_E_vs_nonE_ALL_sans_NA,top =
		text_grob(paste(my_var2, "(textes à variation uniquement)"), face = "bold", size = 14))

##Depending on following context
	p_text_V_vs_C <- ggarrange(text_p_Student_V_vs_C,
	                 text_p_leg_1,
	                 text_p_leg_2,
								          ncol = 1,
								          nrow = 3,
																	 heights = c(0.5, 0.7, 1))
	p_text_V_vs_C <- annotate_figure(p_text_V_vs_C, top = text_grob("Test de Student", color = "black", face = "bold", size = 11))

	g_POS_V_vs_C_ALL_sans_NA <- ggarrange(p_POS__Carrange,
	          p_POS__Varrange,
											p_POS__C_w_arrange,
											p_POS__V_w_arrange,
											Overview_V_vs_C,
											p_text_V_vs_C,
	          labels="auto",
	          ncol = 2,
	          nrow = 3,
										 align = "v")
 g_POS_V_vs_C_ALL_sans_NA <- annotate_figure(g_POS_V_vs_C_ALL_sans_NA,top =
		text_grob(paste(my_var2, "(textes à variation uniquement)"), face = "bold", size = 14))




#---------------------------------------------------------------------
#Archive/draft: prepare singular plots.
#Be careful, this hasn't been updated and might not work properly.
#---------------------------------------------------------------------
# p_POS_E_w_standalone <- p_POS_E_w +
# annotate("text",
# 									x=1450,
# 									y=0.25,
# 									label=paste(POS_E__mean)) +
# annotate("text",
# 									x=1450,
# 									y=0.20,
# 									label=paste(POS_E__median)) +
# annotate("text",
# 									x=1450,
# 									y=0.15,
# 									label=paste(POS_E__sd)) +
# annotate("text",
# 									x=1425,
# 									y=0.25,
# 									label="moy.") +
# annotate("text",
# 									x=1425,
# 									y=0.20,
# 									label="méd.") +
# annotate("text",
# 									x=1425,
# 									y=0.15,
# 									label="sd")
#
# p_POS_E_standalone <- p_POS_E_ +
# annotate("text",
# 									x=1450,
# 									y=0.25,
# 									label=paste(POS_E__mean)) +
# annotate("text",
# 									x=1450,
# 									y=0.20,
# 									label=paste(POS_E__median)) +
# annotate("text",
# 									x=1450,
# 									y=0.15,
# 									label=paste(POS_E__sd)) +
# annotate("text",
# 									x=1425,
# 									y=0.25,
# 									label="moy.") +
# annotate("text",
# 									x=1425,
# 									y=0.20,
# 									label="méd.") +
# annotate("text",
# 									x=1425,
# 									y=0.15,
# 									label="sd")
#
# p_POS_nonE_standalone <- p_POS_nonE_ +
# annotate("text",
# 									x=1450,
# 									y=0.25,
# 									label=paste(POS_nonE__mean)) +
# annotate("text",
# 									x=1450,
# 									y=0.20,
# 									label=paste(POS_nonE__median)) +
# annotate("text",
# 									x=1450,
# 									y=0.15,
# 									label=paste(POS_nonE__sd)) +
# annotate("text",
# 									x=1425,
# 									y=0.25,
# 									label="moy.") +
# annotate("text",
# 									x=1425,
# 									y=0.20,
# 									label="méd.") +
# annotate("text",
# 									x=1425,
# 									y=0.15,
# 									label="sd")
#
# p_POS_nonE_w_standalone <- p_POS_nonE_w +
# annotate("text",
# 									x=1450,
# 									y=0.25,
# 									label=paste(POS_nonE__mean)) +
# annotate("text",
# 									x=1450,
# 									y=0.20,
# 									label=paste(POS_nonE__median)) +
# annotate("text",
# 									x=1450,
# 									y=0.15,
# 									label=paste(POS_nonE__sd)) +
# annotate("text",
# 									x=1425,
# 									y=0.25,
# 									label="moy.") +
# annotate("text",
# 									x=1425,
# 									y=0.20,
# 									label="méd.") +
# annotate("text",
# 									x=1425,
# 									y=0.15,
# 									label="sd")
#
# p_POS__Cstandalone <- p_POS__C +
# annotate("text",
# 									x=1450,
# 									y=0.25,
# 									label=paste(POS__C_mean)) +
# annotate("text",
# 									x=1450,
# 									y=0.20,
# 									label=paste(POS__C_median)) +
# annotate("text",
# 									x=1450,
# 									y=0.15,
# 									label=paste(POS__C_sd)) +
# annotate("text",
# 									x=1425,
# 									y=0.25,
# 									label="moy.") +
# annotate("text",
# 									x=1425,
# 									y=0.20,
# 									label="méd.") +
# annotate("text",
# 									x=1425,
# 									y=0.15,
# 									label="sd")
#
# p_POS__Vstandalone <- p_POS__V +
# annotate("text",
# 									x=1450,
# 									y=0.25,
# 									label=paste(POS__V_mean)) +
# annotate("text",
# 									x=1450,
# 									y=0.20,
# 									label=paste(POS__V_median)) +
# annotate("text",
# 									x=1450,
# 									y=0.15,
# 									label=paste(POS__V_sd)) +
# annotate("text",
# 									x=1425,
# 									y=0.25,
# 									label="moy.") +
# annotate("text",
# 									x=1425,
# 									y=0.20,
# 									label="méd.") +
# annotate("text",
# 									x=1425,
# 									y=0.15,
# 									label="sd")




# g_POS_ALL <- ggarrange(p_POS__Carrange,
#           p_POS__Varrange,
#           p_POS_E_arrange,
#           p_POS_nonE_arrange,
#           p_POS_E_w_arrange,
#           p_POS_nonE_w_arrange,
#           labels="auto",
#           ncol = 2,
#           nrow = 3,
# 									 align = "v")
# g_POS_ALL_leg <- annotate_figure(g_POS_ALL, bottom = text_grob("Chaque point représente un texte.\nLes lignes continues représentent une régression locale, les lignes discontinues une régression linéaire.\nEn (e-f), les taux, moyennes et régressions sont pondérés par le nombre d'observations, figuré par la taille des points.",
#                                   hjust = 1,
# 																																		x = 1,
# 																																		face = "italic",
# 																																		size = 10))


# g_POS_Initial_ALL_leg <- annotate_figure(g_POS_Initial_ALL,
# 	                       top = text_grob(paste("<e>/Ø en fonction du contexte subséquent :", my_var), color = "Black", face = "bold", size = 14),
# 	                       bottom = text_grob("Chaque point représente un texte.\nLes lignes continues représentent une régression locale, les lignes discontinues une régression linéaire.\nEn (c-d), les mesures sont pondérées par le nombre d'observations, figuré par la taille des points.",
# 										              hjust = 1,
# 																								x = 1,
# 																								face = "italic",
# 																								size = 10))

# g_POS_Final_ALL <- ggarrange(p_POS_E_arrange,
# 	         p_POS_nonE_arrange,
# 	         p_POS_E_w_arrange,
# 	         p_POS_nonE_w_arrange,
#           labels="auto",
#           ncol = 2,
#           nrow = 2,
# 									 align = "v")
# g_POS_Final_ALL_leg <- annotate_figure(g_POS_Final_ALL,
# 	                       top = text_grob(paste("C/V en fonction de <e>/Ø :", my_var), color = "Black", face = "bold", size = 14),
# 	                       bottom = text_grob("Chaque point représente un texte.\nLes lignes continues représentent une régression locale, les lignes discontinues une régression linéaire.\nEn (c-d), les mesures sont pondérées par le nombre d'observations, figuré par la taille des points.",
# 										              hjust = 1,
# 																								x = 1,
# 																								face = "italic",
# 																								size = 10))
