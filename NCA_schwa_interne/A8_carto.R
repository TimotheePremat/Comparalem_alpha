#Script to print maps from A5_diachro_plot_context data.
#Nota: Saving is done in A9_carto_save.R script.
#data2 and data4 words with suppletive location for texts.

#--------------------------------------------------
#Prepare data
#--------------------------------------------------
# Prepare data
data1 <- POS_EnonE_count %>%
 rename(R_Code = R_code_from_regionDees) %>% #rename region code to match those of
																																				#Dates NCA.xlsx file.
	rename(regiondees = regionDees) %>%
	group_by(R_Code) %>% #[1]group_by(regiondees, R_Code) if you want to read the
	                     #results in R, but it creates 'non-unique matches detected'
																						#problems when regionDees are not aligned with R_Code.
	summarise(taux_moyen = mean(Tx_POS_nonE), nb_txt = n())
data1$taux_moyen <- data1$taux_moyen*100

data2 <- POS_EnonE_count %>%
rename(R_Code = R_code_suppl_total) %>% #rename region code to match those of
																																			#Dates NCA.xlsx file.
rename(regiondees = regionDees_supp) %>%
group_by(R_Code) %>% #cf. [1] supra.
	summarise(taux_moyen = mean(Tx_POS_nonE), nb_txt = n())
data2$taux_moyen <- data2$taux_moyen*100

#Import spatial data
data_spatial <- readOGR("Regions-Shapefile")

#Calcuate pseudo-centroids, for printing values on polygons later
centroids.df <- as.data.frame(coordinates(data_spatial))
names(centroids.df) <- c("C_long", "C_lat") #rename centroids columns

centroids.df_lower <- centroids.df %>%
	mutate(C_lat = C_lat-0.12)
centroids.df_higher <- centroids.df %>%
	mutate(C_lat = C_lat+0.1)

## Prepare table for NA
my_var_nil_data1 <- data1 %>% filter(is.na(R_Code))
	ifelse(is.empty(my_var_nil_data1$nb_txt),
		my_var_nil_data1 <- my_var_nil_data1 %>%
																						ungroup() %>%
																						add_row(regiondees_supplee = "nil",
																														R_Code = "nil",
																														taux_moyen = NA,
																														nb_txt = 0),
		my_var_nil_data1 <- my_var_nil_data1)
	# my_var_nil_data1 <- if(nrow(my_var_nil_data1) == 0){
	# 	data.frame(R_Code = NA, taux_moyen = NA, nb_txt = 0)}
my_var_nil_data2 <- data2 %>% filter(is.na(R_Code))
	ifelse(is.empty(my_var_nil_data2$nb_txt),
	 my_var_nil_data2 <- my_var_nil_data2 %>%
		                    ungroup() %>%
																						add_row(regiondees_supplee = "nil",
																						        R_Code = "nil",
																														taux_moyen = NA,
																														nb_txt = 0),
		my_var_nil_data2 <- my_var_nil_data2)
	# my_var_nil_data2 <- if(nrow(my_var_nil_data2) == 0){
	# 	data.frame(R_Code = NA, taux_moyen = NA, nb_txt = 0)}

# my_var_nil_data2 <- data2 %>% filter(is.na(R_Code))
# 	my_var_nil_data2 <- ifelse(
# 		is.empty(my_var_nil_data2$regiondees_supplee),
# 		my_var_nil_data2 <- my_var_nil_data2 %>% ungroup() %>% add_row(regiondees_supplee = "nil", R_Code = "nil", taux_moyen = NA, nb_txt = 0),
# 		my_var_nil_data2$regiondees_supplee)
#
# #Seems to work:
# ifelse(is.empty(my_var_nil_data1$nb_txt),
#  my_var_nil_data1 <- my_var_nil_data1 %>%
# 	                    ungroup() %>%
# 																					add_row(regiondees_supplee = "nil",
# 																					        R_Code = "nil",
# 																													taux_moyen = NA,
# 																													nb_txt = 0),
# 	my_var_nil_data1 <- my_var_nil_data1)
#
# ifelse(is.empty(my_var_nil_data2$nb_txt),
#  my_var_nil_data2 <- my_var_nil_data2 %>%
# 	                    ungroup() %>%
# 																					add_row(regiondees_supplee = "nil",
# 																					        R_Code = "nil",
# 																													taux_moyen = NA,
# 																													nb_txt = 0),
# 	my_var_nil_data2 <- my_var_nil_data2)
#
# 																											data.frame(R_Code = NA,
#                                       taux_moyen = NA,
# 																																						nb_txt = 0),
# 																										 data.frame(R_Code = my_var_nil_data1$R_Code,
# 																										            taux_moyen = my_var_nil_data1$taux_moyen,
# 																																					 nb_txt = my_var_nil_data1$nb_txt))

## Merge datasets
data3 <- merge (data_spatial, data1, by="R_Code")
data4 <- merge (data_spatial, data2, by="R_Code")

#Replace NA values by 0 values for regions with no texts
data3$nb_txt <- replace(data3$nb_txt, is.na(data3$nb_txt), 0)
data4$nb_txt <- replace(data4$nb_txt, is.na(data4$nb_txt), 0)

#Neutralize regions with less than 5 texts by replacing taux_moyen value by NA
#data3$taux_moyen <- replace(data3$taux_moyen, data3$nb_txt <5, NA)

#--------------------------------------------------
#Plot for Dees locations
#--------------------------------------------------
#NAME THE VARIABLE YOU WANT TO USE
my_var_carto <- data3$taux_moyen
my_var_carto_name <- paste("Taux de chute de ",schwa_phono,"\n",my_var2_short, sep="")
my_var_display <- as.numeric(my_var_carto)
my_var_display_nb_txt <- as.numeric(data3$nb_txt)

#Define the plot
plot1 <- ggplot() +
	#Pass fill=item to annotation_spatial to make it available for layer_spatial below
	annotation_spatial(data3, aes(fill=taux_moyen)) +
	layer_spatial(data3, aes(fill=taux_moyen)) +
	scale_fill_gradient(low="gray90",
			high="black",
			na.value="white",
			name=paste(my_var_carto_name),
			limits=c(0,100)) +
			#Verbose theme definition
			theme(panel.background=element_rect(fill = "transparent", colour = "transparent"),
		        panel.grid.major = element_blank(),
		        panel.grid.minor = element_blank(),
		        legend.justification=c(0,0),
		        axis.title.x=element_blank(),
		        axis.text.x=element_blank(),
		        axis.ticks.x=element_blank(),
		        axis.title.y=element_blank(),
		        axis.text.y=element_blank(),
		        axis.ticks.y=element_blank(),
		        legend.position=c(0.02,0.03),
		        legend.spacing=unit(1,"lines"),
		        legend.box="vertical",
		        legend.key.size=unit(1.2,"lines"),
		        legend.text.align=0,
		        legend.title.align=0,
		        legend.text=element_text(size=12, color="black"),
		        legend.key = element_rect(fill = "transparent", color="transparent"),
		        legend.title=element_text(size=12, color="black", face = "bold"),
		        legend.background = element_rect(fill = "transparent", colour = "transparent")) +

#Add textual annotations
	annotate("text", x = -4.5, y = 51, label = paste(my_var_carto_name),
		size=6, color = "black", hjust = 0) +
	annotate("text", x = -4.5, y = 50.5, label = "Loc. Dees uniquement",
		size=4.5, color = "black", hjust = 0) +
	annotate("text", x = -4.5, y = 50.1, label = "Données : NCA, Dees (1987)\n© Timothée Premat",
		size=3.5, color = "black", hjust = 0) +
	#Annotations for statistical values
		##Labels
		annotate("text", x = 2, y = 45.8, label = paste0("min. "),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2, y = 45.6, label = paste0("moy. "),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2, y = 45.4, label = paste0("max. "),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2, y = 45.2, label = paste0("σ"),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 4.5, y = 45.8, label = "Textes non localisés",
											size=4, color = "black", hjust = 0) +
		annotate("text", x = 4.5, y = 45.6, label = "moy.",
											size=4, color = "black", hjust = 0) +
		annotate("text", x = 4.5, y = 45.4, label = "nb. txt",
											size=4, color = "black", hjust = 0) +
		##Values
		annotate("text", x = 2.75, y = 45.8, label = paste0(round(min(my_var_display, na.rm=TRUE),2), "%"),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2.75, y = 45.6, label = paste0(round(mean(my_var_display, na.rm=TRUE),2), "%"),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2.75, y = 45.4, label = paste0(round(max(my_var_display, na.rm=TRUE),2), "%"),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2.75, y = 45.2, label = paste0(round(sd(my_var_display, na.rm = TRUE),2)),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 5.75, y = 45.6, label = paste0(round(my_var_nil_data1$taux_moyen)),
											size=4, color = "black", hjust = 0) +
		annotate("text", x = 5.75, y = 45.4, label = paste0(my_var_nil_data1$nb_txt),
											size=4, color = "black", hjust = 0)

#Add values and nb of texts on each polygon.
plot2 <- plot1 +
geom_label(aes(label = ifelse(is.na(my_var_display_nb_txt), "", paste0(my_var_display_nb_txt," txt")),
	x = centroids.df_lower$C_long,
	y = centroids.df_lower$C_lat),
 label.size = 0,
 size=3.5) +
geom_label(aes(label = ifelse(is.na(my_var_carto), "", paste0(round(my_var_carto, digits =0)," %")),
	x = centroids.df_higher$C_long,
	y = centroids.df_higher$C_lat),
 label.size=0,
 fontface = "bold")
plot2

#--------------------------------------------------
#Plot for Dees + suppletive locations
#--------------------------------------------------
#NAME THE VARIABLE YOU WANT TO USE
my_var_carto_supplee <- data4$taux_moyen
my_var_carto_name_supplee <- paste("Taux de chute de ",schwa_phono,"\n",my_var2_short, sep="")
my_var_display_supplee <- as.numeric(my_var_carto_supplee)
my_var_display_nb_txt_supplee <- as.numeric(data4$nb_txt)

#Define the plot
plot1_supplee <- ggplot() +
	#Pass fill=item to annotation_spatial to make it available for layer_spatial below
	annotation_spatial(data4, aes(fill=taux_moyen)) +
	layer_spatial(data4, aes(fill=taux_moyen)) +
	scale_fill_gradient(low="gray90",
			high="black",
			na.value="white",
			name=paste(my_var_carto_name_supplee),
			limits=c(0,100)) +
			#Verbose theme definition
			theme(panel.background=element_rect(fill = "transparent", colour = "transparent"),
		        panel.grid.major = element_blank(),
		        panel.grid.minor = element_blank(),
		        legend.justification=c(0,0),
		        axis.title.x=element_blank(),
		        axis.text.x=element_blank(),
		        axis.ticks.x=element_blank(),
		        axis.title.y=element_blank(),
		        axis.text.y=element_blank(),
		        axis.ticks.y=element_blank(),
		        legend.position=c(0.02,0.03),
		        legend.spacing=unit(1,"lines"),
		        legend.box="vertical",
		        legend.key.size=unit(1.2,"lines"),
		        legend.text.align=0,
		        legend.title.align=0,
		        legend.text=element_text(size=12, color="black"),
		        legend.key = element_rect(fill = "transparent", color="transparent"),
		        legend.title=element_text(size=12, color="black", face = "bold"),
		        legend.background = element_rect(fill = "transparent", colour = "transparent")) +

#Add textual annotations
	annotate("text", x = -4.5, y = 51, label = paste(my_var_carto_name_supplee),
		size=6, color = "black", hjust = 0) +
	annotate("text", x = -4.5, y = 50.5, label = "Loc. Dees complétée",
		size=4.5, color = "black", hjust = 0) +
	annotate("text", x = -4.5, y = 50.1, label = "Données : NCA, Dees (1987)\n© Timothée Premat",
		size=3.5, color = "black", hjust = 0) +
	#Annotations for statistical values
		##Labels
		annotate("text", x = 2, y = 45.8, label = paste0("min. "),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2, y = 45.6, label = paste0("moy. "),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2, y = 45.4, label = paste0("max. "),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2, y = 45.2, label = paste0("σ"),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 4.5, y = 45.8, label = "Textes non localisés",
											size=4, color = "black", hjust = 0) +
		annotate("text", x = 4.5, y = 45.6, label = "moy.",
											size=4, color = "black", hjust = 0) +
		annotate("text", x = 4.5, y = 45.4, label = "nb. txt",
											size=4, color = "black", hjust = 0) +
		##Values
		annotate("text", x = 2.75, y = 45.8, label = paste0(round(min(my_var_display_supplee, na.rm=TRUE),2), "%"),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2.75, y = 45.6, label = paste0(round(mean(my_var_display_supplee, na.rm=TRUE),2), "%"),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2.75, y = 45.4, label = paste0(round(max(my_var_display_supplee, na.rm=TRUE),2), "%"),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 2.75, y = 45.2, label = paste0(round(sd(my_var_display_supplee, na.rm = TRUE),2)),
			        size=4, color = "black", hjust = 0) +
		annotate("text", x = 5.75, y = 45.6, label = paste0(round(my_var_nil_data2$taux_moyen)),
											size=4, color = "black", hjust = 0) +
		annotate("text", x = 5.75, y = 45.4, label = paste0(my_var_nil_data2$nb_txt),
											size=4, color = "black", hjust = 0)

#Add values and nb of texts on each polygon.
plot2_supplee <- plot1_supplee +
geom_label(aes(label = ifelse(is.na(my_var_display_nb_txt_supplee), "", paste0(my_var_display_nb_txt_supplee," txt")),
	x = centroids.df_lower$C_long,
	y = centroids.df_lower$C_lat),
 label.size = 0,
 size=3.5) +
geom_label(aes(label = ifelse(is.na(my_var_carto_supplee), "", paste0(round(my_var_carto_supplee, digits =0)," %")),
	x = centroids.df_higher$C_long,
	y = centroids.df_higher$C_lat),
 label.size=0,
 fontface = "bold")
plot2_supplee
