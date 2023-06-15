library('readxl')
library('readr')
library('dplyr')
library('tidyverse')
library('ggplot2')
library('writexl')
library('stringr')
library('ggpubr')
library('Hmisc')
library('spatstat')
library('weights')
library('Unicode')
library('ggtext')
library('RColorBrewer')
library('sp')
library('ggspatial')
library('rgdal')

italic_p <- u_char_inspect(u_char_from_name("MATHEMATICAL SANS-SERIF ITALIC SMALL P"))["Char"]
rho <- u_char_inspect(u_char_from_name("GREEK SMALL LETTER RHO"))["Char"]
schwa <- u_char_inspect(u_char_from_name("LATIN SMALL LETTER SCHWA"))["Char"]
chi_char <- u_char_inspect(u_char_from_name("GREEK SMALL LETTER CHI"))["Char"]
logiAND <- u_char_inspect(u_char_from_name("AMPERSAND"))["Char"]
schwa_phono <- paste("/",schwa,"/", sep="")

##Define the variable you want to investigate, in order to maintain traceability
my_var <- readline(prompt="Enter the name of the POS you want to investigate, to name the files: ")
my_var2 <- readline(prompt="Enter the context you want to print in the title: ")

#--------------------------------------------------
#Prepare data
#--------------------------------------------------
# Prepare data
Dates_NCA <- read_excel("Dates_NCA.xlsx", sheet = "Caps_normalised")
missing_regions <- read_excel("Dates_NCA.xlsx", sheet = "Missing_regions") %>%
                   rename(R_Code = R_Code)

data1 <- Dates_NCA %>% group_by(r_code,regiondees) %>%
                       summarise(nb_txt = n()) %>%
																							rename(R_Code = r_code)
data1$R_Code <- replace(data1$R_Code, is.na(data1$R_Code), 0)

data2 <- Dates_NCA %>% group_by(r_code_custom) %>%
                       summarise(nb_txt = n()) %>%
																							rename(R_Code = r_code_custom) %>%
																							mutate(R_Code = na_if(R_Code, "na"))
data2$R_Code <- replace(data2$R_Code, is.na(data2$R_Code), 0)

missing_regions$R_Code <- as.character(missing_regions$R_Code)

data1 <- bind_rows(data1, missing_regions)
data2 <- bind_rows(data2, missing_regions)

#Import spatial data
data_spatial <- readOGR("Regions-Shapefile")

#Calcuate pseudo-centroids, for printing values on polygons later
centroids.df <- as.data.frame(coordinates(data_spatial))
names(centroids.df) <- c("C_long", "C_lat") #rename centroids columns

centroids.df_lower <- centroids.df %>%
	mutate(C_lat = C_lat-0.12)
centroids.df_higher <- centroids.df %>%
	mutate(C_lat = C_lat+0.1)

## Merge datasets
data3 <- merge (data_spatial, data1, by="R_Code")
data4 <- merge (data_spatial, data2, by="R_Code")

# #Replace NA values by 0 values for regions with no texts
# data3$nb_txt <- replace(data3$nb_txt, is.na(data3$nb_txt), 0)
# data4$nb_txt <- replace(data4$nb_txt, is.na(data4$nb_txt), 0)

## Prepare table for NA (non-located texts)
	my_var_nil_data1 <- data1 %>% filter(R_Code == 0)
	my_var_nil_data2 <- data2 %>% filter(R_Code == 0)

#--------------------------------------------------
#Plot for Dees locations
#--------------------------------------------------
#NAME THE VARIABLE YOU WANT TO USE
my_var_carto <- data3$nb_txt
my_var_carto_name <- paste("Nombre de textes\npar région")
data3$nb_txt <- as.numeric(data3$nb_txt)

plot1 <- ggplot() +
	#Pass fill=item to annotation_spatial to make it available for layer_spatial below
	# annotation_spatial(data3, aes(fill=regiondees)) +
	layer_spatial(data3, aes(fill=nb_txt)) +
	scale_fill_gradient(low="gray90",
			high="black",
			na.value="white",
			name=paste(my_var_carto_name)) +
			# Verbose theme definition
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
		        legend.position="none",
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
	annotate("text", x = -4.5, y = 50.5, label = "Localisation Dees uniquement",
		size=4.5, color = "black", hjust = 0) +
	annotate("text", x = -4.5, y = 50.1, label = "Données : NCA, Dees (1987)\n© Timothée Premat",
		size=3.5, color = "black", hjust = 0) +
	#Annotations for statistical values
		##Labels
		annotate("text", x = 4.5, y = 45.8, label = "Textes non localisés",
											size=4, color = "black", hjust = 0) +
		annotate("text", x = 4.5, y = 45.6, label = "nb. txt",
											size=4, color = "black", hjust = 0) +
		##Values
		annotate("text", x = 5.75, y = 45.6, label = paste0(my_var_nil_data1$nb_txt),
											size=4, color = "black", hjust = 0)

#Add values and nb of texts on each polygon.
plot2 <- plot1 +
geom_label(aes(label = ifelse(is.na(data3$nb_txt), "", paste0(data3$nb_txt," txt")),
	x = centroids.df_lower$C_long,
	y = centroids.df_lower$C_lat),
 label.size = 0,
 size=3.5) +
geom_label(aes(label = paste0(data3$regiondees),
	x = centroids.df_higher$C_long,
	y = centroids.df_higher$C_lat),
 label.size=0,
 fontface = "bold")
plot2

#--------------------------------------------------
#Plots for Dees locations + custom locations when Dees is NA
#--------------------------------------------------
#NAME THE VARIABLE YOU WANT TO USE
my_var_carto <- data4$nb_txt
# data4$nb_txt <- as.numeric(data4$nb_txt)

plot1_supplee <- ggplot() +
	#Pass fill=item to annotation_spatial to make it available for layer_spatial below
	# annotation_spatial(data3, aes(fill=regiondees)) +
	layer_spatial(data4, aes(fill=nb_txt)) +
	scale_fill_gradient(low="gray90",
			high="black",
			na.value="white",
			name=paste(my_var_carto_name)) +
			# Verbose theme definition
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
		        legend.position="none",
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
	annotate("text", x = -4.5, y = 50.5, label = "Loc. Dees complétée",
		size=4.5, color = "black", hjust = 0) +
	annotate("text", x = -4.5, y = 50.1, label = "Données : NCA, Dees (1987)\n© Timothée Premat",
		size=3.5, color = "black", hjust = 0) +
	#Annotations for statistical values
		##Labels
		annotate("text", x = 4.5, y = 45.8, label = "Textes non localisés",
											size=4, color = "black", hjust = 0) +
		annotate("text", x = 4.5, y = 45.6, label = "nb. txt",
											size=4, color = "black", hjust = 0) +
		##Values
		annotate("text", x = 5.75, y = 45.6, label = paste0(my_var_nil_data2$nb_txt),
											size=4, color = "black", hjust = 0)

#Add values and nb of texts on each polygon.
plot2_supplee <- plot1_supplee +
geom_label(aes(label = ifelse(is.na(data3$nb_txt), "", paste0(data4$nb_txt," txt")),
	x = centroids.df_lower$C_long,
	y = centroids.df_lower$C_lat),
 label.size = 0,
 size=3.5) +
geom_label(aes(label = paste0(data3$regiondees),
	x = centroids.df_higher$C_long,
	y = centroids.df_higher$C_lat),
 label.size=0,
 fontface = "bold")
plot2_supplee
