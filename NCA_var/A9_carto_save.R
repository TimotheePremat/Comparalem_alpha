#Script to save as PDF files the maps produced by A8_carto.R

ggsave(plot2_V,
	      path = "Graphs",
							filename = paste(my_var,"map_V.pdf"),
							width = 25,
						 height = 19.5,
						 units = "cm",
						 scale = 1,
						 dpi = "retina",
							device = cairo_pdf)

ggsave(plot2_C,
	      path = "Graphs",
							filename = paste(my_var,"map_C.pdf"),
							width = 25,
						 height = 19.5,
						 units = "cm",
						 scale= 1,
						 dpi = "retina",
							device = cairo_pdf)

ggsave(plot2_CV,
	      path = "Graphs",
							filename = paste(my_var,"map_ALL.pdf"),
							width = 25,
						 height = 39,
						 units = "cm",
						 scale= 1,
						 dpi = "retina",
							device = cairo_pdf)

ggsave(plot2_W,
	      path = "Graphs",
							filename = paste(my_var,"map_elision_weight.pdf"),
							width = 25,
						 height = 19.5,
						 units = "cm",
						 scale= 1,
						 dpi = "retina",
							device = cairo_pdf)
