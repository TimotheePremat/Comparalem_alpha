# Script to plot regressions lines, calculate Chi-2 and highlight the windows on which Chi-2 is based.
## To run after A5_diachro_plot_context.R
## Saving is done by A6_chi-2_save.R
## Must be parametered by hand to find the windows, the right level of rounding of p-values, etc.

p_POS_reg <- ggplot() +
	geom_smooth(data = POS__C, aes(x = datecomposition,
		                              y = Tx_POS_EnonE,
																															 colour = "Devant #C"),
													method = "loess",
													se = FALSE,
													na.rm = TRUE,
													# aes(colour = "Devant #C"),
													span = 0.5)  +
	geom_smooth(data = POS__V, aes(x = datecomposition,
		                              y = Tx_POS_EnonE,
																															 colour = "Devant #V"),
		           method = "loess",
													se = FALSE,
													na.rm = TRUE,
													# aes(colour = "Devant #V"),
													span = 0.5)  +
 theme_classic() +
	theme(legend.position = c(.70, .80)) +
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
 scale_y_continuous(name="Taux de Ø") +
	scale_colour_manual(name="Regressions locales", values=c("gray40", "black")) +
	annotate("text", label=paste("Empan des régressions : 0.5",
	                            "\nEmpan pour ",chi_char, "² : ~20 ans", sep = ""),
	         x = 1350, y = 0.08,
										hjust = 0, vjust = 1) +
	annotate("rect", xmin = 1100, xmax = 1125, ymin = 0.007, ymax = 0.06, colour="black", fill=NA) +
	 annotate("text", label = "1", x = 1112.5, y = 0.061, hjust = 0.5, vjust = 0) +
	annotate("rect", xmin = 1155, xmax = 1175, ymin = 0.085, ymax = 0.11, colour="black", fill=NA) +
	 annotate("text", label = "2", x = 1165, y = 0.111, hjust = 0.5, vjust = 0) +
	annotate("rect", xmin = 1235, xmax = 1255, ymin = 0.017, ymax = 0.027, colour="black", fill=NA) +
	 annotate("text", label = "3", x = 1245, y = 0.028, hjust = 0.5, vjust = 0) +
	annotate("rect", xmin = 1302, xmax = 1322, ymin = 0.065, ymax = 0.14, colour="black", fill=NA) +
	 annotate("text", label = "4", x = 1312, y = 0.141, hjust = 0.5, vjust = 0)

POS__V_valley1 <- POS__V %>% filter(datecomposition < 1126)
POS__V_pike1   <- POS__V %>% filter(datecomposition > 1154) %>%
                             filter(datecomposition < 1176)
POS__V_valley2 <- POS__V %>% filter(datecomposition > 1234) %>%
                             filter(datecomposition < 1256)
POS__V_pike2   <- POS__V %>% filter(datecomposition > 1301)

POS__V_valley1_E    <- sum(POS__V_valley1$n.E)
POS__V_valley1_nonE <- sum(POS__V_valley1$n.nonE)
POS__V_pike1_E    <- sum(POS__V_pike1$n.E)
POS__V_pike1_nonE <- sum(POS__V_pike1$n.nonE)
POS__V_valley2_E    <- sum(POS__V_valley2$n.E)
POS__V_valley2_nonE <- sum(POS__V_valley2$n.nonE)
POS__V_pike2_E      <- sum(POS__V_pike2$n.E)
POS__V_pike2_nonE   <- sum(POS__V_pike2$n.nonE)

pike1_table <- matrix(c(POS__V_valley1_E,POS__V_valley1_nonE,
	                       POS__V_pike1_E, POS__V_pike1_nonE),
																							 ncol = 2, byrow = TRUE)
valley2_table <- matrix(c(POS__V_pike1_E,POS__V_pike1_nonE,
                          POS__V_valley2_E,POS__V_valley2_nonE),
																									 ncol = 2, byrow = TRUE)
pike2_table <- matrix(c(POS__V_valley2_E,POS__V_valley2_nonE,
	                       POS__V_pike2_E, POS__V_pike2_nonE),
																							 ncol = 2, byrow = TRUE)

chisq.test(pike1_table)
 pike1_chi_df <- chisq.test(pike1_table)$parameter
	pike1_chi_sum <- sum(pike1_table)
	pike1_chi_stat <- chisq.test(pike1_table)$statistic
	pike1_chi_p <- chisq.test(pike1_table)$p.value
	pike1_chi_p <- format.pval(pv = pike1_chi_p,
		                          digits = 3,   # digits: number of digits, but after the 0.0
		                          eps = 0.001, # eps = the threshold value above wich the function will replace the pvalue by "<0.0xxx"
		                          nsmall = 0,   # nsmall = how much tails 0 to keep if digits of original value < to digits defined
												                scientific = FALSE) #prohibits e-n scientific notation

chisq.test(valley2_table)
 valley2_chi_df <- chisq.test(valley2_table)$parameter
	valley2_chi_sum <- sum(valley2_table)
	valley2_chi_stat <- chisq.test(valley2_table)$statistic
	valley2_chi_p <- chisq.test(valley2_table)$p.value
	valley2_chi_p <- format.pval(pv = valley2_chi_p,
		                            digits = 3,
		                            eps = 0.0001,
		                            nsmall = 0,
												                  scientific = FALSE)

chisq.test(pike2_table)
 pike2_chi_df <- chisq.test(pike2_table)$parameter
	pike2_chi_sum <- sum(pike2_table)
	pike2_chi_stat <- chisq.test(pike2_table)$statistic
	pike2_chi_p <- chisq.test(pike2_table)$p.value
	pike2_chi_p <- format.pval(pv = pike2_chi_p,
		                          digits = 3,
		                          eps = 0.0001,
		                          nsmall = 0,
												                scientific = FALSE)

f = function(X1)gsub("0\\.","\\.", X1) #fonction to remove 0 before decimal mark.

pike1_chi <- paste(chi_char, "²"," (",pike1_chi_df,", N=",pike1_chi_sum,")=",
             round(pike1_chi_stat, digits = 3), ", p", f(pike1_chi_p), sep = "")
valley2_chi <- paste(chi_char, "²"," (",valley2_chi_df,", N=",valley2_chi_sum,")=",
             round(valley2_chi_stat, digits = 3), ", p", f(valley2_chi_p), sep = "")
pike2_chi <- paste(chi_char, "²"," (",pike2_chi_df,", N=",pike2_chi_sum,")=",
             round(pike2_chi_stat, digits = 3), ", p", f(pike2_chi_p), sep = "")

p_POS_reg_chi <- p_POS_reg +
 annotate("text", label=paste("1~2 : ",pike1_chi,
	         "\n2~3 : ",valley2_chi,
										"\n3~4 : ",pike2_chi),
										x = 1350, y = 0,
										hjust = 0, vjust = 0)
