library(tmap)
library(sf)
library(dplyr)

# Mode carte interactive
tmap_mode("view")



##carte d'index
# Charger les données
idf <- st_read("communes-france.geojson", quiet = TRUE)  # adapte le chemin si besoin
# Extraire Paris
paris <- idf %>%
  filter(com_name_upper %in% c("PARIS"))
# Extraire les 3 communes étudiées
selection <- idf %>%
  filter(com_name_upper %in% c("CLAMART", "GENNEVILLIERS", "LE CHÂTELET-EN-BRIE"))
# Construire la carte
ma_carte <- tm_shape(idf) +
  tm_fill(col = "lightgrey", alpha = 0.8, popup.vars = "com_name_upper") +
  
  tm_shape(paris) +
  tm_fill(col = "red", alpha = 0.8, popup.vars = "com_name_upper") +
  tm_borders(col = "darkred", lwd = 2) +
  
  tm_shape(selection) +
  tm_fill(col = "skyblue", alpha = 0.8,popup.vars = "com_name_upper") +
  tm_borders(col = "darkblue", lwd = 2) +
  
  tm_layout(main.title = "Communes étudiées et Paris",
            main.title.size = 1.1) +
  
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("left", "top"))
# Exporter la carte en HTML autonome dans le dossier /docs
tmap_save(ma_carte, filename = "docs/carte_interactive.html", selfcontained = TRUE)



##Carte méthodo
#extraction des données
sol <- st_read("occupation-du-sol-en-5-postes-1994-mos-simplifie-saisi-au-150000-dile-de-france.geojson", quiet = TRUE)
#filtre des données
vege <- sol %>%
  filter(codzon %in% c("2"))
#création de la carte
carte_vege <- tm_shape(vege) +
  tm_fill( col = "darkgreen", alpha = 0.5, popup.vars = "codzon") +
  tm_borders(col = "white", lwd = 0.2) +
  tm_layout(
    main.title = "Espaces végétalisés en Île-de-France",
    main.title.size = 1.1,
    legend.outside = TRUE
  )
#enregistrement de la carte
tmap_save(carte_vege, filename = "docs/carte_vege.html", selfcontained = TRUE)



##cartes pour analyses
#charger les données
communes <- st_read("communes-france.geojson", quiet = TRUE)
vegetation <- st_read("mode-doccupation-du-sol-mos-en-11-postes-en-2017.geojson", quiet = TRUE)
icu <- st_read("ilots-de-chaleur-urbains-icu-classification-des-imu-en-zone-climatique-locale-lc.geojson", quiet = TRUE)
#filtrer les données
vegetation <- vegetation %>%
  filter(mos2017_11 %in% c("1", "2", "5"))
#Reprojeter les couches dans le même système que les communes
vegetation <- st_transform(vegetation, st_crs(communes))
icu <- st_transform(icu, st_crs(communes))
#filtrage des communes étudiées
clamart <- communes %>% filter(com_name_upper == "CLAMART")
gennevilliers <- communes %>% filter(com_name_upper == "GENNEVILLIERS")
chatelet <- communes %>% filter(com_name_upper == "LE CHÂTELET-EN-BRIE")
#Découpage des couches végétation et ICU par communes
veg_clamart <- st_intersection(vegetation, clamart)
veg_gennevilliers <- st_intersection(vegetation, gennevilliers)
veg_chatelet <- st_intersection(vegetation, chatelet)

icu_clamart <- st_intersection(icu, clamart)
icu_gennevilliers <- st_intersection(icu, gennevilliers)
icu_chatelet <- st_intersection(icu, chatelet)

#Création de la carte pour clamart
carte_clamart <- tm_shape(clamart) +
  tm_borders(col = "gray") +
  tm_shape(veg_clamart) +
  tm_fill(col = "forestgreen", alpha = 0.4, group = "Espaces verts") +
  tm_shape(icu_clamart) +
  tm_fill(col = "aleaj_note", palette = "YlOrRd", style = "quantile", group = "îlots de chaleur") +
  tm_shape(icu_clamart) +
  tm_fill(col = "vulnj_note", palette = "Purples", style = "quantile", group = "Vulnérabilités") +
  tm_layout(main.title = "Clamart: analyse croisée", lengend.outside = TRUE)
#sauvegarde de la carte clamart
tmap_save(carte_clamart, "docs/carte-clamart.html", selfcontained = TRUE)

#création de la carte pour gennevilliers
carte_gennevilliers <- tm_shape(gennevilliers) +
  tm_borders(col = "gray") +
  tm_shape(veg_gennevilliers) +
  tm_fill(col = "forestgreen", alpha = 0.4, group = "Espaces verts") +
  tm_shape(icu_gennevilliers) +
  tm_fill(col = "aleaj_note", palette = "YlOrRd", style = "quantile", group = "îlots de chaleur") +
  tm_shape(icu_gennevilliers) +
  tm_fill(col = "vulnj_note", palette = "Purples", style = "quantile", group = "Vulnérabilités") +
  tm_layout(main.title = "Gennevilliers: analyse croisée", lengend.outside = TRUE)
#sauvegarde de la carte pour gennevilliers
tmap_save(carte_gennevilliers, "docs/carte-gennevilliers.html", selfcontained = TRUE)

#création de la carte pour chatelet
carte_chatelet <- tm_shape(chatelet) +
  tm_borders(col = "gray") +
  tm_shape(veg_chatelet) +
  tm_fill(col = "forestgreen", alpha = 0.4, group = "Espaces verts") +
  tm_shape(icu_chatelet) +
  tm_fill(col = "aleaj_note", palette = "YlOrRd", style = "quantile", group = "îlots de chaleur") +
  tm_shape(icu_chatelet) +
  tm_fill(col = "vulnj_note", palette = "Purples", style = "quantile", group = "Vulnérabilités") +
  tm_layout(main.title = "Le Châtelet-en-Brie: analyse croisée", lengend.outside = TRUE)
#sauvegarde de la carte pour chatelet
tmap_save(carte_chatelet, "docs/carte-chatelet.html", selfcontained = TRUE)

