#Script to print maps from A5_diachro_plot_context data.
#Nota: Saving is done in A9_carto_save.R script.
#Nota: this script is independant from A8_carto.R script, because the latter is
 #lighter than this one, running on simplified data (try 'View(data1_V)' in
	#A8_carto.R to see how simplified the data is.

#Import spatial data
data_spatial <- readOGR("Regions-Shapefile")

#Calcuate pseudo-centroids, for printing values on polygons later
centroids.df <- as.data.frame(coordinates(data_spatial))
names(centroids.df) <- c("C_long", "C_lat") #rename centroids columns

centroids.df_lower <- centroids.df %>%
	mutate(C_lat = C_lat-0.12)
centroids.df_higher <- centroids.df %>%
	mutate(C_lat = C_lat+0.1)

#--------------------------------------------------
#Prepare data
#--------------------------------------------------
 ##For all periods: merge basic data
 #--------------------------------------------------
  POS_EnonE <- bind_rows(POS_E_cleaned, POS_nonE_cleaned)
 #--------------------------------------------------
	##For first period
	#--------------------------------------------------
	# Prepare data for Dees' locations
		data1_1 <- POS_EnonE_count %>% filter(datecomposition < bound_1b)
		data1_1 <- data1_1 %>%
		 rename(R_Code = R_code_from_regionDees) %>% #rename region code to match those of
																																					#Dates NCA.xlsx file.
		 group_by(R_Code) %>% #[1]group_by(regiondees, R_Code) if you want to read the
			                     #results in R, but it creates 'non-unique matches detected'
																								#problems when regionDees are not aligned with R_Code.
		 summarise(taux_moyen = mean(Tx_POS_nonE), nb_txt = n())
	 data1_1$taux_moyen <- data1_1$taux_moyen*100

		## Prepare table for NA
		 my_var_nil_data1_1 <- data1_1 %>% filter(is.na(R_Code))

		## Merge datasets
			data3_1 <- merge (data_spatial, data1_1, by="R_Code")

		#Replace NA values by 0 values for regions with no texts
			data3_1$nb_txt <- replace(data3_1$nb_txt, is.na(data3_1$nb_txt), 0)

	# Prepare data for Dees' locations + custom location when Dees is NA
		data2_1 <- POS_EnonE_count %>% filter(datecomposition < bound_1b)
		data2_1 <- data2_1 %>%
			rename(R_Code = R_code_suppl_total) %>% #rename region code to match those of
																																					      #Dates NCA.xlsx file.
			group_by(R_Code) %>% #cf. [1] supra.
			summarise(taux_moyen = mean(Tx_POS_nonE), nb_txt = n())
		data2_1$taux_moyen <- data2_1$taux_moyen*100

		## Prepare table for NA (non-located texts)
		 my_var_nil_data2_1 <- data2_1 %>% filter(is.na(R_Code))
			my_var_nil_data2_1 <- if(nrow(my_var_nil_data2_1) == 0){
				data.frame(R_Code = NA, taux_moyen = NA, nb_txt = 0)}

		## Merge datasets
			data4_1 <- merge (data_spatial, data2_1, by="R_Code")

		#Replace NA values by 0 values for regions with no texts
			data4_1$nb_txt <- replace(data4_1$nb_txt, is.na(data4_1$nb_txt), 0)

	#--------------------------------------------------
	##For second period
	#--------------------------------------------------
	# Prepare data for #V environment
		data1_2 <- POS_EnonE_count %>% filter(datecomposition > bound_2a) %>%
		                               filter(datecomposition < bound_2b)
		data1_2 <- data1_2 %>%
			rename(R_Code = R_code_from_regionDees) %>% #rename region code to match those of
																																					#Dates NCA.xlsx file.
			group_by(R_Code) %>% #cf. [1] supra.
			summarise(taux_moyen = mean(Tx_POS_nonE), nb_txt = n())
		data1_2$taux_moyen <- data1_2$taux_moyen*100
		my_var_nil_data1_2 <- data1_2 %>% filter(is.na(R_Code))

		## Merge datasets
			data3_2 <- merge (data_spatial, data1_2, by="R_Code")

		#Replace NA values by 0 values for regions with no texts
			data3_2$nb_txt <- replace(data3_2$nb_txt, is.na(data3_2$nb_txt), 0)

	# Prepare data for Dees' locations + custom location when Dees is NA
		data2_2 <- POS_EnonE_count %>% filter(datecomposition > bound_2a) %>%
		                               filter(datecomposition < bound_2b)
		data2_2 <- data2_2 %>%
			rename(R_Code = R_code_suppl_total) %>% #rename region code to match those of
																																					#Dates NCA.xlsx file.
			group_by(R_Code) %>% #cf. [1] supra.
			summarise(taux_moyen = mean(Tx_POS_nonE), nb_txt = n())
		data2_2$taux_moyen <- data2_2$taux_moyen*100

		## Prepare table for NA (non-located texts)
		 my_var_nil_data2_2 <- data2_2 %>% filter(is.na(R_Code))
			# my_var_nil_data2_2 <- if(nrow(my_var_nil_data2_2) == 0){
			# 	data.frame(R_Code = NA, taux_moyen = NA, nb_txt = 0)}

		## Merge datasets
			data4_2 <- merge (data_spatial, data2_2, by="R_Code")

		#Replace NA values by 0 values for regions with no texts
			data4_2$nb_txt <- replace(data4_2$nb_txt, is.na(data4_2$nb_txt), 0)

	#--------------------------------------------------
	##For third period
	#--------------------------------------------------
	# Prepare data for #V environment
		data1_3 <- POS_EnonE_count %>% filter(datecomposition > bound_3a)
		data1_3 <- data1_3 %>%
			rename(R_Code = R_code_from_regionDees) %>% #rename region code to match those of
																																					#Dates NCA.xlsx file.
			group_by(R_Code) %>% #cf. [1] supra.
			summarise(taux_moyen = mean(Tx_POS_nonE), nb_txt = n())
		data1_3$taux_moyen <- data1_3$taux_moyen*100
		my_var_nil_data1_3 <- data1_3 %>% filter(is.na(R_Code))

		## Merge datasets
			data3_3 <- merge (data_spatial, data1_3, by="R_Code")

		#Replace NA values by 0 values for regions with no texts
			data3_3$nb_txt <- replace(data3_3$nb_txt, is.na(data3_3$nb_txt), 0)

	# Prepare data for Dees' locations + custom location when Dees is NA
		data2_3 <- POS_EnonE_count %>% filter(datecomposition > bound_3a)
		data2_3 <- data2_3 %>%
			rename(R_Code = R_code_suppl_total) %>% #rename region code to match those of
																																					#Dates NCA.xlsx file.
			group_by(R_Code) %>% #cf. [1] supra.
			summarise(taux_moyen = mean(Tx_POS_nonE), nb_txt = n())
		data2_3$taux_moyen <- data2_3$taux_moyen*100

		## Prepare table for NA (non-located texts)
		 my_var_nil_data2_3 <- data2_3 %>% filter(is.na(R_Code))
			my_var_nil_data2_3 <- if(nrow(my_var_nil_data2_3) == 0){
				data.frame(R_Code = NA, taux_moyen = NA, nb_txt = 0)}

		## Merge datasets
			data4_3 <- merge (data_spatial, data2_3, by="R_Code")

		#Replace NA values by 0 values for regions with no texts
			data4_3$nb_txt <- replace(data4_3$nb_txt, is.na(data4_3$nb_txt), 0)

	# #--------------------------------------------------
	# ##For last period
	# #--------------------------------------------------
	# # Prepare data for #V environment
	# 	POS__V_4 <- POS__V %>% filter(datecomposition > bound_4a)
	# 	data1_V_4 <- POS__V_4 %>%
	# 		group_by(regiondees_supplee,r_code_custom) %>%
	# 		summarise(taux_moyen = mean(Tx_POS_EnonE), nb_txt = n()) %>%
	# 		rename(R_Code = r_code_custom)
	# 	data1_V_4$taux_moyen <- data1_V_4$taux_moyen*100
	#
	# # Prepare data for #C environment
	# 	POS__C_4 <- POS__C %>% filter(datecomposition > bound_4a)
	# 	data1_C_4 <- POS__C_4 %>%
	# 		group_by(regiondees_supplee,r_code_custom) %>%
	# 		summarise(taux_moyen = mean(Tx_POS_EnonE), nb_txt = n()) %>%
	# 		rename(R_Code = r_code_custom)
	# 	data1_C_4$taux_moyen <- data1_C_4$taux_moyen*100
	#
	# ## Merge datasets
	# 	data3_V_4 <- merge (data_spatial, data1_V_4, by="R_Code")
	# 	data3_C_4 <- merge (data_spatial, data1_C_4, by="R_Code")
	#
	# #Replace NA values by 0 values for regions with no texts
	# 	data3_V_4$nb_txt <- replace(data3_V_4$nb_txt, is.na(data3_V_4$nb_txt), 0)
	# 	data3_C_4$nb_txt <- replace(data3_C_4$nb_txt, is.na(data3_C_4$nb_txt), 0)
 #
	# #Neutralize regions with less than 5 texts by replacing taux_moyen value by NA
		# data3_V_2$taux_moyen <- replace(data3_V_2$taux_moyen, data3_V_2$nb_txt <5, NA)
		# data3_C_2$taux_moyen <- replace(data3_C_2$taux_moyen, data3_C_2$nb_txt <5, NA)

#--------------------------------------------------
#Plots for Dees locations
#--------------------------------------------------
#Define the general title of the maps
	my_var_carto_name <- paste("Taux de chute de ",schwa_phono,"\n",my_var2, sep="")
#Define the subtitle of plots based on Dees' location only
 my_var_Dees_loc <- paste("Loc. Dees uniquement")

 #--------------------------------------------------
	##Plot for time 1
	#--------------------------------------------------
	#NAME THE VARIABLE YOU WANT TO USE
		my_var_carto_1 <- data3_1$taux_moyen
		my_var_display_1 <- as.numeric(my_var_carto_1)
		my_var_display_1_nb_txt <- as.numeric(data3_1$nb_txt)

	#Define the plot
		plot_1 <- ggplot() +
			#Pass fill=item to annotation_spatial to make it available for layer_spatial below
			annotation_spatial(data3_1, aes(fill=taux_moyen)) +
			layer_spatial(data3_1, aes(fill=taux_moyen)) +
			scale_fill_gradient(low="white",
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
			annotate("text", x = -4.5, y = 51.25, label = paste(my_var_carto_name),
				size=6, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.75, label = paste(my_var_Dees_loc),
				size=4.5, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.35, label = "Données : NCA, Dees (1987)\n© Timothée Premat",
				size=3.5, color = "black", hjust = 0) +
			annotate("text", x = 3 , y = 51.25, label=paste("Période 1 :\nDate compo. <",bound_1b),
		  size=4.5, color = "black", hjust = 0, vjust=0.3) +
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
				annotate("text", x = 2.75, y = 45.8, label = paste0(round(min(my_var_display_1, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.6, label = paste0(round(mean(my_var_display_1, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.4, label = paste0(round(max(my_var_display_1, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.2, label = paste0(round(sd(my_var_display_1, na.rm = TRUE),2)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.6, label = paste0(round(my_var_nil_data2_1$taux_moyen)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.4, label = paste0(my_var_nil_data2_1$nb_txt),
					        size=4, color = "black", hjust = 0)

		#Add values and nb of texts on each polygon.
		plot2_1 <- plot_1 +
		geom_label(aes(label = ifelse(is.na(my_var_display_1_nb_txt), "", paste0(my_var_display_1_nb_txt," txt")),
			x = centroids.df_lower$C_long,
			y = centroids.df_lower$C_lat),
		 label.size = 0,
		 size=3.5) +
		geom_label(aes(label = ifelse(is.na(my_var_carto_1), "", paste0(round(my_var_carto_1, digits =0)," %")),
			x = centroids.df_higher$C_long,
			y = centroids.df_higher$C_lat),
		 label.size=0,
		 fontface = "bold")

		#--------------------------------------------------
		##Plot for time 2
		#--------------------------------------------------
		#NAME THE VARIABLE YOU WANT TO USE
		 my_var_carto_2 <- data3_2$taux_moyen
		 my_var_display_2 <- as.numeric(my_var_carto_1)
		 my_var_display_2_nb_txt <- as.numeric(data3_2$nb_txt)

		#Define the plot
			plot_2 <- ggplot() +
				#Pass fill=item to annotation_spatial to make it available for layer_spatial below
				annotation_spatial(data3_2, aes(fill=taux_moyen)) +
				layer_spatial(data3_2, aes(fill=taux_moyen)) +
				scale_fill_gradient(low="white",
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
			annotate("text", x = -4.5, y = 51.25, label = paste(my_var_carto_name),
				size=6, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.75, label = paste(my_var_Dees_loc),
				size=4.5, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.35, label = "Données : NCA, Dees (1987)\n© Timothée Premat",
				size=3.5, color = "black", hjust = 0) +
			annotate("text", x = 3 , y = 51.25, label=paste("Période 2 :\nDate compo. >", bound_2a, logiAND,"<",bound_2b),
		  size=4.5, color = "black", hjust = 0, vjust=0.3) +
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
				annotate("text", x = 2.75, y = 45.8, label = paste0(round(min(my_var_display_2, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.6, label = paste0(round(mean(my_var_display_2, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.4, label = paste0(round(max(my_var_display_2, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.2, label = paste0(round(sd(my_var_display_2, na.rm = TRUE),2)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.6, label = paste0(round(my_var_nil_data1_2$taux_moyen)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.4, label = paste0(my_var_nil_data1_2$nb_txt),
					        size=4, color = "black", hjust = 0)

		#Add values and nb of texts on each polygon.
			plot2_2 <- plot_2 +
			geom_label(aes(label = ifelse(is.na(my_var_display_2_nb_txt), "", paste0(my_var_display_2_nb_txt," txt")),
				x = centroids.df_lower$C_long,
				y = centroids.df_lower$C_lat),
			 label.size = 0,
			 size=3.5) +
			geom_label(aes(label = ifelse(is.na(my_var_carto_2), "", paste0(round(my_var_carto_2, digits =0)," %")),
				x = centroids.df_higher$C_long,
				y = centroids.df_higher$C_lat),
			 label.size=0,
			 fontface = "bold")

		# plot2_2

		#--------------------------------------------------
		##Plot for time 3
		#--------------------------------------------------
		#NAME THE VARIABLE YOU WANT TO USE
			my_var_carto_3 <- data3_3$taux_moyen
			my_var_display_3 <- as.numeric(my_var_carto_3)
			my_var_display_3_nb_txt <- as.numeric(data3_3$nb_txt)

		#Define the plot
			plot_3 <- ggplot() +
				#Pass fill=item to annotation_spatial to make it available for layer_spatial below
				annotation_spatial(data3_3, aes(fill=taux_moyen)) +
				layer_spatial(data3_3, aes(fill=taux_moyen)) +
				scale_fill_gradient(low="white",
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
			annotate("text", x = -4.5, y = 51.25, label = paste(my_var_carto_name),
				size=6, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.75, label = paste(my_var_Dees_loc),
				size=4.5, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.35, label = "Données : NCA, Dees (1987)\n© Timothée Premat",
				size=3.5, color = "black", hjust = 0) +
			annotate("text", x = 3 , y = 51.25, label=paste("Période 3 :\nDate compo. >",bound_3a),
		  size=4.5, color = "black", hjust = 0, vjust=0.3) +
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
				annotate("text", x = 2.75, y = 45.8, label = paste0(round(min(my_var_display_3, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.6, label = paste0(round(mean(my_var_display_3, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.4, label = paste0(round(max(my_var_display_3, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.2, label = paste0(round(sd(my_var_display_3, na.rm = TRUE),2)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.6, label = paste0(round(my_var_nil_data2_3$taux_moyen)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.4, label = paste0(my_var_nil_data2_3$nb_txt),
					        size=4, color = "black", hjust = 0)

		#Add values and nb of texts on each polygon.
			plot2_3 <- plot_3 +
			geom_label(aes(label = ifelse(is.na(my_var_display_3_nb_txt), "", paste0(my_var_display_3_nb_txt," txt")),
				x = centroids.df_lower$C_long,
				y = centroids.df_lower$C_lat),
			 label.size = 0,
			 size=3.5) +
			geom_label(aes(label = ifelse(is.na(my_var_carto_3), "", paste0(round(my_var_carto_3, digits =0)," %")),
				x = centroids.df_higher$C_long,
				y = centroids.df_higher$C_lat),
			 label.size=0,
			 fontface = "bold")

		# plot2_3

	##Arrange plots two by two
		plot2_t1_t2 <- ggarrange(plot2_1,
												plot2_2,
												labels="auto",
												ncol = 1,
												nrow = 2,
												align = "v")
		# plot2_V_t3_t4 <- ggarrange(plot2_3,
		# 										plot2_3,
		# 										labels="auto",
		# 										ncol = 1,
		# 										nrow = 2,
		# 										align = "v")

#--------------------------------------------------
#Plots for Dees locations + custom locations when Dees is NA
#--------------------------------------------------
#Define the subtitle of plots based on Dees' location only
 my_var_supplee_loc <- paste("Loc. Dees complétée")

 #--------------------------------------------------
	##Plot for time 1
	#--------------------------------------------------
	#NAME THE VARIABLE YOU WANT TO USE
		my_var_carto2_1 <- data4_1$taux_moyen
		my_var_display2_1 <- as.numeric(my_var_carto2_1)
		my_var_display2_1_nb_txt <- as.numeric(data4_1$nb_txt)

	#Define the plot
		plot_1_supplee <- ggplot() +
			#Pass fill=item to annotation_spatial to make it available for layer_spatial below
			annotation_spatial(data4_1, aes(fill=taux_moyen)) +
			layer_spatial(data4_1, aes(fill=taux_moyen)) +
			scale_fill_gradient(low="white",
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
			annotate("text", x = -4.5, y = 51.25, label = paste(my_var_carto_name),
				size=6, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.75, label = paste(my_var_supplee_loc),
				size=4.5, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.35, label = "Données : NCA, Dees (1987)\n© Timothée Premat",
				size=3.5, color = "black", hjust = 0) +
			annotate("text", x = 3 , y = 51.25, label=paste("Période 1 :\nDate compo. <",bound_1b),
		  size=4.5, color = "black", hjust = 0, vjust=0.3) +
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
				annotate("text", x = 2.75, y = 45.8, label = paste0(round(min(my_var_display2_1, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.6, label = paste0(round(mean(my_var_display2_1, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.4, label = paste0(round(max(my_var_display2_1, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.2, label = paste0(round(sd(my_var_display2_1, na.rm = TRUE),2)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.6, label = paste0(round(my_var_nil_data2_1$taux_moyen)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.4, label = paste0(my_var_nil_data2_1$nb_txt),
					        size=4, color = "black", hjust = 0)

		#Add values and nb of texts on each polygon.
		plot2_1_supplee <- plot_1_supplee +
		geom_label(aes(label = ifelse(is.na(my_var_display2_1_nb_txt), "", paste0(my_var_display2_1_nb_txt," txt")),
			x = centroids.df_lower$C_long,
			y = centroids.df_lower$C_lat),
		 label.size = 0,
		 size=3.5) +
		geom_label(aes(label = ifelse(is.na(my_var_carto2_1), "", paste0(round(my_var_carto2_1, digits =0)," %")),
			x = centroids.df_higher$C_long,
			y = centroids.df_higher$C_lat),
		 label.size=0,
		 fontface = "bold")

		#--------------------------------------------------
		##Plot for time 2
		#--------------------------------------------------
		#NAME THE VARIABLE YOU WANT TO USE
		 my_var_carto2_2 <- data4_2$taux_moyen
		 my_var_display2_2 <- as.numeric(my_var_carto2_2)
		 my_var_display2_2_nb_txt <- as.numeric(data4_2$nb_txt)

		#Define the plot
			plot_2_supplee <- ggplot() +
				#Pass fill=item to annotation_spatial to make it available for layer_spatial below
				annotation_spatial(data4_2, aes(fill=taux_moyen)) +
				layer_spatial(data4_2, aes(fill=taux_moyen)) +
				scale_fill_gradient(low="white",
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
			annotate("text", x = -4.5, y = 51.25, label = paste(my_var_carto_name),
				size=6, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.75, label = paste(my_var_supplee_loc),
				size=4.5, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.35, label = "Données : NCA, Dees (1987)\n© Timothée Premat",
				size=3.5, color = "black", hjust = 0) +
			annotate("text", x = 3 , y = 51.25, label=paste("Période 2 :\nDate compo. >", bound_2a, logiAND,"<",bound_2b),
		  size=4.5, color = "black", hjust = 0, vjust=0.3) +
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
				annotate("text", x = 2.75, y = 45.8, label = paste0(round(min(my_var_display2_2, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.6, label = paste0(round(mean(my_var_display2_2, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.4, label = paste0(round(max(my_var_display2_2, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.2, label = paste0(round(sd(my_var_display2_2, na.rm = TRUE),2)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.6, label = paste0(round(my_var_nil_data2_2$taux_moyen)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.4, label = paste0(my_var_nil_data2_2$nb_txt),
					        size=4, color = "black", hjust = 0)

		#Add values and nb of texts on each polygon.
			plot2_2_supplee <- plot_2_supplee +
			geom_label(aes(label = ifelse(is.na(my_var_display2_2_nb_txt), "", paste0(my_var_display2_2_nb_txt," txt")),
				x = centroids.df_lower$C_long,
				y = centroids.df_lower$C_lat),
				label.size = 0,
				size=3.5) +
			geom_label(aes(label = ifelse(is.na(my_var_carto2_2), "", paste0(round(my_var_carto2_2, digits =0)," %")),
				x = centroids.df_higher$C_long,
				y = centroids.df_higher$C_lat),
				label.size=0,
				fontface = "bold")

		#--------------------------------------------------
		##Plot for time 3
		#--------------------------------------------------
		#NAME THE VARIABLE YOU WANT TO USE
		 my_var_carto2_3 <- data4_3$taux_moyen
		 my_var_display2_3 <- as.numeric(my_var_carto2_3)
		 my_var_display2_3_nb_txt <- as.numeric(data4_3$nb_txt)

		#Define the plot
			plot_3_supplee <- ggplot() +
				#Pass fill=item to annotation_spatial to make it available for layer_spatial below
				annotation_spatial(data4_3, aes(fill=taux_moyen)) +
				layer_spatial(data4_3, aes(fill=taux_moyen)) +
				scale_fill_gradient(low="white",
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
			annotate("text", x = -4.5, y = 51.25, label = paste(my_var_carto_name),
				size=6, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.75, label = paste(my_var_supplee_loc),
				size=4.5, color = "black", hjust = 0) +
			annotate("text", x = -4.5, y = 50.35, label = "Données : NCA, Dees (1987)\n© Timothée Premat",
				size=3.5, color = "black", hjust = 0) +
			annotate("text", x = 3 , y = 51.25, label=paste("Période 3 :\nDate compo. >",bound_3a),
		  size=4.5, color = "black", hjust = 0, vjust=0.3) +
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
				annotate("text", x = 2.75, y = 45.8, label = paste0(round(min(my_var_display2_3, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.6, label = paste0(round(mean(my_var_display2_3, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.4, label = paste0(round(max(my_var_display2_3, na.rm=TRUE),2), "%"),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 2.75, y = 45.2, label = paste0(round(sd(my_var_display2_3, na.rm = TRUE),2)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.6, label = paste0(round(my_var_nil_data2_3$taux_moyen)),
					        size=4, color = "black", hjust = 0) +
				annotate("text", x = 5.75, y = 45.4, label = paste0(my_var_nil_data2_3$nb_txt),
					        size=4, color = "black", hjust = 0)

		#Add values and nb of texts on each polygon.
			plot2_3_supplee <- plot_3_supplee +
			geom_label(aes(label = ifelse(is.na(my_var_display2_3_nb_txt), "", paste0(my_var_display2_3_nb_txt," txt")),
				x = centroids.df_lower$C_long,
				y = centroids.df_lower$C_lat),
				label.size = 0,
				size=3.5) +
			geom_label(aes(label = ifelse(is.na(my_var_carto2_3), "", paste0(round(my_var_carto2_3, digits =0)," %")),
				x = centroids.df_higher$C_long,
				y = centroids.df_higher$C_lat),
				label.size=0,
				fontface = "bold")

	##Arrange plots two by two
		plot2_supplee_t1_t2 <- ggarrange(plot2_1_supplee,
												plot2_2_supplee,
												labels="auto",
												ncol = 1,
												nrow = 2,
												align = "v")
