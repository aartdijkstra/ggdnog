library(tidyverse)

# kleurenpatroon met NOG huisstijl
nog_colors = c("#1D1756", "#D1005D", "#628dcf", "#cdd344", "#685c8c", "#e9748f", "#b4c4e7", "#eae9a5", "#b2aac4", "#f9bbc6", "#eff4fa")
use_data(nog_colors, overwrite=T)
nog_palette = c("#FAE6EF","#F6CCDF","#ED99BE","#E3669E","#D1005D")
use_data(nog_palette, overwrite=T)

# gemeenten in de regio NOG
gemeenten.nog = "Aalten, Apeldoorn, Berkelland, Bronckhorst, Brummen, Doetinchem, Elburg, Epe, Ermelo, Harderwijk, Hattem, Heerde, Lochem, Montferland, Nunspeet, Oldebroek, Oost Gelre, Oude IJsselstreek, Putten, Voorst, Winterswijk, Zutphen"
gemeenten.nog = str_trim(unlist(str_split(gemeenten.nog, ",")))
use_data(gemeenten.nog, overwrite=T)
gemeenten.ah = gemeenten.nog[c(1, 3, 4, 6, 14, 17, 18, 21)]
gemeenten.mijov = gemeenten.nog[c(2, 5, 8, 11, 12, 13, 20, 22)]
gemeenten.nv = gemeenten.nog[c(7, 9, 10, 15, 16, 19)]
use_data(gemeenten.ah, overwrite=T)
use_data(gemeenten.mijov, overwrite=T)
use_data(gemeenten.nv, overwrite=T)
