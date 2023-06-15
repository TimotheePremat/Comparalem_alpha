library('ggplot2')
library('readxl')

data <- read_excel("AA_Taux de var par categorie.xlsx",
                                sheet = "For_R_detailed")

a
ggplot(data, aes(fill=sub_cat, y=elision_fac, x=cat)) + 
    geom_bar(position="stack", stat="identity")
