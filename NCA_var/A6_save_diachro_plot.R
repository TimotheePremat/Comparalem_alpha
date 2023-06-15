#Script to save plots as PDFs

#Save plots for rate (tx) or nonE forms for EnonE lemmas
ggsave(g_POS_EnonE_tx_compo_leg,
	     path = "Graphs",
      filename = paste(my_var,"EnonE_tx_compo.pdf"),
						device=cairo_pdf,
						width=9,
						height = 9,
						units = "in")
ggsave(g_POS_EnonE_tx_ms_leg,
	     path = "Graphs",
	     filename = paste(my_var,"EnonE_tx_ms.pdf"),
						device=cairo_pdf,
						width=9,
						height = 9,
						units = "in")

#Save plots for absolute number of lemmas
ggsave(g_POS_count_compo_leg,
	     path = "Graphs",
	     filename = paste(my_var,"count_compo.pdf"),
						device=cairo_pdf,
						width=9,
						height = 9,
						units = "in")
ggsave(g_POS_count_ms_leg,
	     path = "Graphs",
	     filename = paste(my_var,"count_ms.pdf"),
						device=cairo_pdf,
						width=9,
						height = 9,
						units = "in")

#Save plots for number of lemmas that have <e>/Ø variation
ggsave(g_POS_count_var_compo_leg,
	     path = "Graphs",
	     filename = paste(my_var,"count_var_compo.pdf"),
						device=cairo_pdf,
						width=9,
						height = 9,
						units = "in")
ggsave(g_POS_count_var_ms_leg,
	     path = "Graphs",
	     filename = paste(my_var,"count_var_ms.pdf"),
						device=cairo_pdf,
						width=9,
						height = 9,
						units = "in")

#Save arranged plots
ggsave(g_POS_ALL,
	      path = "Graphs",
							filename = paste(my_var,"ALL.pdf"),
							device=cairo_pdf,
							width = 9,
						 height = 13.5,
						 units = "in")

ggsave(g_POS_ALL_leg,
		      path = "Graphs",
								filename = paste(my_var,"ALL_leg.pdf"),
								device=cairo_pdf,
								width = 9,
							 height = 14,
							 units = "in")
