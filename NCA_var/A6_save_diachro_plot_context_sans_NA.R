#Script to save plots as PDFs

#Save plots for rate of Ø forms before #C
# ggsave(p_POS__Cstandalone,
# 	     path = "Graphs",
#       filename = paste(my_var,"context _C.pdf"),
# 						width=9,
# 						height = 9,
# 						units = "in")
#
# #Save plots for rate of Ø forms before #V
# ggsave(p_POS__Vstandalone,
# 	     path = "Graphs",
# 	     filename = paste(my_var,"context _V.pdf"),
# 						width=9,
# 						height = 9,
# 						units = "in")
#
# #Save plots for rate of #C context for -e forms
# ggsave(p_POS_E_standalone,
# 	     path = "Graphs",
# 	     filename = paste(my_var,"context E_.pdf"),
# 						width=9,
# 						height = 9,
# 						units = "in")
#
# #Save plots for rate of #C context for -Ø forms
# ggsave(p_POS_nonE_standalone,
# 	     path = "Graphs",
# 	     filename = paste(my_var,"context nonE_.pdf"),
# 						width=9,
# 						height = 9,
# 						units = "in")
#
# #Save plots for rate of #C context for <eC> forms
# ggsave(p_POS_EC_standalone,
# 	     path = "Graphs",
# 	     filename = paste(my_var,"context EC_.pdf"),
# 						width=9,
# 						height = 9,
# 						units = "in")

#Save arranged plots
ggsave(g_POS_E_vs_nonE_ALL_sans_NA,
	      path = "Graphs",
							filename = paste(my_var,"E_nonE_noNA.pdf"),
							device=cairo_pdf,
							width = 9,
						 height = 13.5,
						 units = "in")

ggsave(g_POS_V_vs_C_ALL_sans_NA,
	      path = "Graphs",
							filename = paste(my_var,"C_V_noNA.pdf"),
							device=cairo_pdf,
							width = 9,
						 height = 13.5,
						 units = "in")

# ggsave(g_POS_Initial_ALL_leg,
# 	      path = "Graphs",
# 							filename = paste(my_var,"E_nonE_leg.pdf"),
# 							device=cairo_pdf,
# 							width = 9,
# 						 height = 9.5,
# 						 units = "in")
#
# ggsave(g_POS_Final_ALL_leg,
# 	      path = "Graphs",
# 							filename = paste(my_var,"C_V_leg.pdf"),
# 							device=cairo_pdf,
# 							width = 9,
# 						 height = 9.5,
# 						 units = "in")

# ggsave(g_POS_ALL,
# 	      path = "Graphs",
# 							filename = paste(my_var,"context ALL.pdf"),
# 							width = 9,
# 						 height = 13.5,
# 						 units = "in")
#
# ggsave(g_POS_ALL_leg,
# 		      path = "Graphs",
# 								filename = paste(my_var,"context ALL_leg.pdf"),
# 								width = 9,
# 							 height = 14,
# 							 units = "in")
