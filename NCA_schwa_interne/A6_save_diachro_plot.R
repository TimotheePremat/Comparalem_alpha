#Script to save plots as PDFs

ggsave(g_POS_EnonE_tx_compo_leg,
	      path = "Graphs",
							filename = paste(my_var,".pdf"),
							device=cairo_pdf,
							width = 6,
						 height = 6,
						 units = "in")
