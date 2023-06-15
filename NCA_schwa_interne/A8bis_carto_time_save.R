ggsave(plot2_3,
	      path = "Graphs",
							filename = paste(my_var,"map_t3.pdf"),
							width = 25,
						 height = 19.5,
						 units = "cm",
						 scale= 1,
						 dpi = "retina",
							device = cairo_pdf)


ggsave(plot2_t1_t2,
	      path = "Graphs",
							filename = paste(my_var,"map_t1-t2.pdf"),
							width = 25,
						 height = 39,
						 units = "cm",
						 scale= 1,
						 dpi = "retina",
							device = cairo_pdf)

ggsave(plot2_3_supplee,
	      path = "Graphs",
							filename = paste(my_var,"map_supplee_t3.pdf"),
							width = 25,
						 height = 19.5,
						 units = "cm",
						 scale= 1,
						 dpi = "retina",
							device = cairo_pdf)


ggsave(plot2_supplee_t1_t2,
	      path = "Graphs",
							filename = paste(my_var,"map_supplee_t1-t2.pdf"),
							width = 25,
						 height = 39,
						 units = "cm",
						 scale= 1,
						 dpi = "retina",
							device = cairo_pdf)
