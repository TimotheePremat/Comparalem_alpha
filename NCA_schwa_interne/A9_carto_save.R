#Script to save as PDF files the maps produced by A8_carto.R

ggsave(plot2,
	      path = "Graphs",
							filename = paste(my_var,"map.pdf"),
							width = 25,
						 height = 19.5,
						 units = "cm",
						 scale = 1,
						 dpi = "retina",
							device = cairo_pdf)

ggsave(plot2_supplee,
	      path = "Graphs",
							filename = paste(my_var,"map_loc_supplee.pdf"),
							width = 25,
						 height = 19.5,
						 units = "cm",
						 scale = 1,
						 dpi = "retina",
							device = cairo_pdf)
