#Script to save plots as PDFs

#Save arranged plots
ggsave(p_POS__C_lemma_arrange,
	      path = "Graphs",
							filename = paste(my_var,"__C_lemma.pdf"),
							device=cairo_pdf,
							width = 8,
						 height = 12,
						 units = "in")

ggsave(p_POS__V_lemma_arrange,
	      path = "Graphs",
							filename = paste(my_var,"__V_lemma.pdf"),
							device=cairo_pdf,
							width = 8,
						 height = 12,
						 units = "in")
