bp_vers <- ggplot(POS__V, aes(x = vers, y = Tx_POS_EnonE)) +
  geom_boxplot() +
		annotate("text",
											x=1:length(table(POS__V$vers)),
											y=1.05,
											label=table(POS__V$vers)) +
		annotate("text",
											x=1,
											y=1.1,
											label="Nombre de textes impliqués, par groupe :",
											hjust=0.1) +
											scale_y_continuous(name="Taux d'élision") +
											scale_x_discrete(name="Versification") +
		theme_classic()

ggplot(POS__V, aes(x = lieu_compo_normalise, y = Tx_POS_EnonE)) +
		geom_boxplot() +
		theme_classic()
ggplot(POS__V, aes(x = lieu_manuscrit_normalise, y = Tx_POS_EnonE)) +
		geom_boxplot() +
		theme_classic()

POS__V$lieu_compo_simplifie[is.na(POS__V$lieu_compo_simplifie)] <- "NA"
POS__V$lieu_manuscrit_simplifie[is.na(POS__V$lieu_manuscrit_simplifie)] <- "NA"

POS__V$lieu_compo_simplifie <- factor(POS__V$lieu_compo_simplifie,
	                             levels=c("g. sud-ouest",
                                       "g. ouest",
                                       "agn.",
                                       "g. nord-ouest",
                                       "g. nord",
                                       "g. nord-est",
                                       "g. est",
                                       "g. sud-est",
                                       "g. sud",
                                       "g. francien",
                                       "NA"))

bp_dial <- ggplot(POS__V, aes(x = lieu_compo_simplifie, y = Tx_POS_EnonE)) +
				geom_boxplot() +
				# stat_summary(fun.data = nb.text, geom = "text") +
    annotate("text",
				         x=1:length(table(POS__V$lieu_compo_simplifie)),
													y=1.05,
												 label=table(POS__V$lieu_compo_simplifie)) +
				annotate("text",
			          x=1,
												 y=1.1,
												 label="Nombre de textes impliqués, par groupe :",
												 hjust=0.1) +
				theme_classic() +
				scale_y_continuous(name="Taux d'élision") +
				scale_x_discrete(name="Lieu de composition")


POS__V$lieu_manuscrit_simplifie <- factor(POS__V$lieu_manuscrit_simplifie,
	                             levels=c("g. sud-ouest",
																														         "g. ouest",
																																							"agn.",
	                                      "g. nord-ouest",
		                                     "g. nord",
				                                   "g. nord-est",
				                                   "g. est",
				                                   "g. sud-est",
				                                   "g. sud",
				                                   "g. francien",
				                                   "NA"))
bp_ms <- ggplot(POS__V, aes(x = lieu_manuscrit_simplifie, y = Tx_POS_EnonE)) +
				geom_boxplot() +
				# stat_summary(fun.data = nb.text, geom = "text") +
    annotate("text",
				         x=1:length(table(POS__V$lieu_manuscrit_simplifie)),
													y=1.05,
												 label=table(POS__V$lieu_manuscrit_simplifie)) +
				annotate("text",
			          x=1,
												 y=1.1,
												 label="Nombre de textes impliqués, par groupe :",
												 hjust=0.1) +
				theme_classic() +
				scale_y_continuous(name="Taux d'élision") +
				scale_x_discrete(name="Lieu de copie")

g_dial_arrange <- ggarrange(bp_dial,
										bp_ms,
										labels="auto",
										ncol = 2,
										align = "v")

POS__V$qualite <- factor(POS__V$qualite,
	                        levels=c("ms",
																								          "ms1",
																								 									"ms1-2",
                                  "ms2",
                                  "ms3",
                                  "cr",
                                  "cr1",
                                  "cr2",
                                  "cr3",
                                  "nil"))

bp_quali <- ggplot(POS__V, aes(x = qualite, y = Tx_POS_EnonE)) +
  geom_boxplot() +
		annotate("text",
											x=1:length(table(POS__V$qualite)),
											y=1.05,
											label=table(POS__V$qualite)) +
		annotate("text",
											x=1,
											y=1.1,
											label="Nombre de textes impliqués, par groupe :",
											hjust=0.1) +
											scale_y_continuous(name="Taux d'élision") +
											scale_x_discrete(name="Qualité de l'édition") +
		theme_classic()
