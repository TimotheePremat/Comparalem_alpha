#Script to save regressions + Chi-2 plot.

ggsave(p_POS_reg_chi,
	      path = "Graphs",
							filename = paste(my_var,"Chi-2.pdf"),
							device=cairo_pdf,
							width = 9,
						 height = 4,
						 units = "in")
