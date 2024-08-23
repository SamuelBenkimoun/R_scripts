library(readxl)
library(dplyr)
library(sf)
library(ggplot2)
library(leaflet)
library(dotwhisker)
library(stargazer)
library(pROC)

# Reading the survey data and the coordinates
cd <- read_excel("~/Challineq_with_pm_28Nov2023 (1).xlsx")
cd_sf <- st_as_sf(cd, coords = c("longitude", "latitude"), crs = 4326)

# To test if the google data on parks is more accurate
sample2 <- read_excel("~/Challineq_Closest_Park_Google_23_aout_2024.xlsx")

# Importing the Delhi NCT outline for mapping
nct <- st_read("Documents/Charbon/Geo Data/NCT_boundaries.gpkg")

# Create convex hulls for each group of 'colony'
convex <- cd_sf %>%
  group_by(colony) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull()
plot(convex)

# Adding a number id to each colony for the map legend
convex <- convex %>%
  mutate(colony_id = as.numeric(factor(colony)))

# Keeping the centroids of the convex enveloppes for labelling purpose when mapping
convex_centroids <- st_centroid(convex) %>% st_coordinates() %>%
  as.data.frame()

# Map = green space + garden per neighborhood
### GREEN-SPACE
table(cd_sf$shared_green)
table(cd_sf$garden)

cd_sf <- cd_sf %>%
  mutate(shared_green_french = forcats::fct_recode(shared_green,
                                                   "Oui" = "Yes",
                                                   "Non" = "No"))
# If relevant include the private gardens as green spaces accessed, i.e. combining it with the shared green spaces
cd_sf <- cd_sf %>% mutate(shared_green_french = case_when(garden == "Yes" ~ "Oui",
                                                          garden == "No" ~ shared_green_french))

# Create a colorFactor palette including NA
palette_green_space_french <- leaflet::colorFactor(
  palette = c("indianred", "darkgreen"),
  domain = c("Non", "Oui"),
  na.color = "gainsboro",
  ordered = TRUE
)

# Computing the percentage of declared access to a shared green space by colony
green_space_percentage_french <- cd_sf %>%
  group_by(colony, shared_green_french) %>%
  summarize(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage_green = (count / total) * 100) %>%
  st_drop_geometry()

# Adding the access to green space percentage to the convex enveloppes of the colonies for mapping

convex <- merge(convex, 
                subset(green_space_percentage_french, shared_green_french == "Oui"), 
                by = "colony", all.x = TRUE)
palette_convex <- colorNumeric(palette = "Greens", domain = convex$percentage_green)

# Mapping the enveloppes and households with regards to the declared access to a shared green space
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = nct, color = "black", weight = 4, fill = FALSE) %>%
  setView(lng = mean(st_coordinates(cd_sf)[, "X"]), 
          lat = mean(st_coordinates(cd_sf)[, "Y"]), 
          zoom = 11) %>%
  fitBounds(lng1 = min(st_coordinates(convex)[, "X"]), 
            lat1 = min(st_coordinates(convex)[, "Y"]), 
            lng2 = max(st_coordinates(convex)[, "X"]), 
            lat2 = max(st_coordinates(convex)[, "Y"])) %>%
  addPolygons(data = convex, 
              fillColor = ~palette_convex(percentage_green),  # Use the calculated percentage to color
              color = "black",
              weight = 2, 
              fillOpacity = 0.6) %>%
  addCircleMarkers(data = cd_sf, 
                   radius = 2.5, 
                   color = "black",
                   weight = 0.1,
                   fillColor = ~palette_green_space_french(shared_green_french),  # Use palette_green_space directly
                   fillOpacity = 0.7,
                   popup = ~paste("Colony:", colony, "<br>", "Accès à un espace vert:", shared_green_french)) %>%
  addLegendFactor(pal = palette_green_space_french, 
                  values = factor(cd_sf$shared_green_french, exclude=NULL, levels = c("Oui", "Non")),
                  title = "Ménages déclarant un accès à un espace vert (partagé ou privatif):",
                  position = 'topright',
                  shape = 'circle') %>%
  addLegendNumeric(pal = palette_convex,
                   values = convex$percentage_green,
                   title = "%age des ménages déclarant un accès à un espace vert dans les zones enquêtées:",
                   position = 'bottomleft') %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))

# Facetted plot of the green-space access by areas

ggplot(green_space_percentage_french, aes(x = shared_green_french, y = percentage_green, fill = shared_green_french)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = palette_green_space_french(levels(as.factor(cd_sf$shared_green_french))),
                    na.translate = FALSE) + # because "NA" is appearing two times in the legend otherwise...
  labs(title = "Accès déclaré à un espace vert partagé ou privatif, par zones enquêtées:",
       x = "Accès",
       y = "Pourcentage du Total",
       fill = "Accès déclaré:") +
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text.x = element_blank(),
    strip.text = element_text(size = 9.5)) +
  facet_wrap(~ colony, 
             scales = "free_y",
             #scales = "fixed"
  )

## Statistical tests

# Combining people declaring access to a private garden or to a mutually shared green space
cd_sf <- cd_sf %>% mutate(access_green = case_when(garden == "Yes" ~ "Yes",
                                                   garden == "No" ~ shared_green))
table(cd_sf$access_green)
# Isolating the data about income and area where the households are located
data_green <- cd_sf %>% st_drop_geometry() %>% 
  select(colony, access_green) %>% 
  filter(!is.na(access_green)) %>% 
  group_by(colony) %>%  # Group by colony
  summarise(
    total = n(),  # Total of households by colony
    positives = sum(access_green == "Yes"),  # Number of positive answers within the colony
    percentage_access = (positives / total) * 100  
  )
# Mean and relation standard deviation
mean(data_green$percentage_access)
sd(data_green$percentage_access) / mean(data_green$percentage_access) * 100

# Boxplot to visualize the outliers 
ggplot(data_green, aes(x = 1, y = percentage_access)) +
  geom_boxplot() + 
  geom_jitter(width = 0.15, color = "blue", size = 2) +  # Ajout des points avec jitter
  geom_text(aes(label = colony), 
            position = position_jitter(), 
            size = 3, 
            vjust = -1, 
            check_overlap = FALSE) +  # Ajout des étiquettes avec légers décalages
  theme_minimal(base_family = "Avenir Next") + 
  labs(title = "Distribution de l'accès à un espace vert (collectif ou privatif) par zone enquêtée",
       y = "Taux d'accès à un espace vert",
       x = "",
       caption = "Source: CHALLINEQ dataset 2020-2022; Auteur: Samuel Benkimoun, 2024.")

#Plotting the share of declared access to green space or not by type of housing
green_hab <- cd_sf %>%
  st_drop_geometry() %>%
  mutate(house_status = case_when(
    house_status == "Delhi Government Quarters" ~ "Government Quarters",
    house_status == "Central Government Quarters" ~ "Government Quarters",
    TRUE ~ house_status)) %>%
  count(house_status, access_green) %>%
  group_by(house_status) %>%
  mutate(lab = paste0(round(prop.table(n) * 100, 2), '%')) %>%
  subset(house_status != "NA" & house_status != "Labour Colony" & access_green != "NA") # Removing NAs and housing status "labour colony" because of only one occurence as per now
  
ggplot(data = green_hab, aes(house_status,n, fill=access_green)) + 
  geom_col() +
  scale_fill_manual(name="Accès déclaré à \n un espace-vert:",
                    labels = c("Pas d'accès", "Accès"),
                    values = c("LightGray", "MediumSeaGreen"))+
  #scale_x_discrete(labels= c("DDA: group housing", "DDA: simple housing","Government quarters","Private housing", "Private Resettlement", "Slum: unauthorized", "Slum: resettlement", "Urban village"))+
  geom_text(aes(label=lab),position=position_stack(vjust = 0.5), size = 3) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, face = "bold", family = "Avenir Next", angle = 45, hjust = 1),
        axis.text.y = element_text(size = 10, face = "bold", family = "Avenir Next"))+
  labs(title = "Distribution de l'accès à un espace vert (collectif ou privatif) par type de logement enquêté",
    caption = "Source: CHALLINEQ dataset 2020-2022; Auteur: Samuel Benkimoun, 2024.")


# Regression
cd_reg <- cd_sf[c("ID", "access_green","income","house_independant",
                  "house_status", "common_entry", "odour","incident","children_number","SC","ST","OBC","General",
                  "Other","religion","colony", "euc_distance_park")] %>%
  na.omit() #shortlisting a certain set of varables to include in the regression, based on potential explanatory power
#Reformat certain variables into binary values
cd_reg$access_green <- ifelse(cd_reg$access_green=="Yes",1,0) 
cd_reg$house_independant <- ifelse(cd_reg$house_independant=="Yes",1,0) 
cd_reg$odour <- ifelse(cd_reg$odour=="Yes",1,0) 
cd_reg$incident <- ifelse(cd_reg$incident=="Yes",1,0)  
cd_reg$common_entry <- ifelse(cd_reg$common_entry=="Common entry",1,0)
#Reformat certain variables into numeric values
cd_reg$children_number <- as.numeric(cd_reg$children_number)
#Reformat and order the income brackets
cd_reg$income

# Create a new variable 'income_merged' to reduce the number of levels
cd_reg$income_merged <- as.character(cd_reg$income) # Converting it into character to be able to modify it with values which are not part of the actual factor levels
cd_reg$income_merged[cd_reg$income_merged %in% c("30,000 to 40,000", "40,000 to 50,000")] <- "30,000 to 50,000"
cd_reg$income_merged[cd_reg$income_merged %in% c("50,000 to 60,000", "60,000 to 70,000")] <- "50,000 to 70,000"
cd_reg <- mutate(cd_reg, income_merged = as.factor(income_merged))
cd_reg$income_merged <- factor(cd_reg$income_merged, exclude=NULL, levels = c(
  "Less than 2,000 Rs/month",
  "2,000 to 6,000",
  "6,000 to 10,000",
  "10,000 to 20,000",
  "20,000 to 30,000",
  "30,000 to 50,000",
  "50,000 to 70,000",
  "70,000 to 110,000",
  "110,000 and above"))

# Translating the income levels in french (if needed)
cd_reg <- cd_reg %>%
  mutate(income_merged = forcats::fct_recode(income_merged,
                                             "Moins de 2 000 Rs/mois" = "Less than 2,000 Rs/month",
                                             "2 000 à 6 000" = "2,000 to 6,000",
                                             "6 000 à 10 000" = "6,000 to 10,000",
                                             "10 000 à 20 000" = "10,000 to 20,000",
                                             "20 000 à 30 000" = "20,000 to 30,000",
                                             "30 000 à 50 000" = "30,000 to 50,000",
                                             "50 000 à 70 000" = "50,000 to 70,000",
                                             "70 000 à 110 000" = "70,000 to 110,000",
                                             "110 000 et plus" = "110,000 and above"
  ))

cd_reg <- cd_reg %>% mutate(house_status = case_when(
  house_status == "Delhi Government Quarters" ~ "Government Quarters",
  house_status == "Central Government Quarters" ~ "Government Quarters",
  TRUE ~ house_status)) %>%
  subset(house_status != "Labour Colony") # Because only one occurrence for now, removed to spare a degree of freedom

#Building the caste attribute from several binary fields
cd_reg <- cd_reg %>% mutate(caste = 
                              ifelse(cd_reg$SC==1,"SC",ifelse(cd_reg$OBC==1,"OBC", "General")) 
                            %>% as.factor)

#Building distance to greenspace threshold variables
cd_reg$park250 <- ifelse(cd_reg$euc_distance_park < 250,"1", "0")
cd_reg$park500 <- ifelse(cd_reg$euc_distance_park < 500,"1", "0")
cd_reg$nopark250 <- ifelse(cd_reg$euc_distance_park > 250,"1", "0")
cd_reg$nopark500 <- ifelse(cd_reg$euc_distance_park > 500,"1", "0")


#Keeping only religious belonging with sufficient individuals in the sample
cd_reg <- cd_reg %>% subset(religion %in% c("Hindu", "Sikh", "Christian", "Jain", "Muslim"))

#Fixing the reference level for the qualitative variables so as to express the regression coefficients in comparison to those
cd_reg <- cd_reg %>%
  mutate(religion=relevel(as.factor(religion),ref="Hindu")) %>%
  mutate(house_status=relevel(as.factor(house_status),ref="Government Quarters")) %>%
  mutate(caste=relevel(as.factor(caste), ref="General")) %>%
  mutate(income_merged= relevel(income_merged, ref = "30 000 à 50 000"))

#Removing NA values from the regression dataset
cd_reg <- na.omit(cd_reg)

#Fiting the regression model
fit_green <- glm(access_green ~ house_status + 
                   #house_independant + common_entry + 
                   children_number + odour + incident + caste + religion + 
                   nopark500 +
                   #nopark250 +
                   #nopark500Google +
                   income_merged, 
                 #+ nopark500:house_status, 
                 data = cd_reg,
                 #data = merge(cd_reg, sample2[c("ID", "w_dist_park")]) %>% mutate(nopark500Google = ifelse(w_dist_park > 500, "1", "0")),
                 family = binomial(link = "logit"))
summary(fit_green)
stargazer::stargazer(fit_green)

#Checking the access to a green space for the reference category
cd_reg %>% subset(religion == "Hindu" & caste == "General" & house_status == "Government Quarters") %>% select(access_green) %>% st_drop_geometry() %>% table()

#Computing the ROC Curve and AUC
prob = predict(fit_green, newdata = cd_reg, type = "response")
auc = roc(cd_reg$access_green ~ prob, plot = TRUE, print.auc = TRUE)

#Plotting the results of the regression
dwplot(fit_green, show_intercept = TRUE,
       model_order = c("Logistic regression"),
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2,
         size=1.0
       ))+
  ggtitle("Determinants de l'accès à un espace vert (régression logistique)")+
  #ggtitle("Determinants of the declared access to a shared green space")+
  theme_minimal(base_family = "Avenir Next") +
  xlab("Axe des x: Coefficient de la variable estimé par le modèle")+
  #xlab("Coefficient Estimate (dot) with 95% Confidence Interval (segment)") + ylab("") +
  #xlab("Coefficient de la variable (point) avec odds-ratios en %age (étiquette) et intervalle de confiance de 95% (segment) \n Couleur verte lorsque variable significative (p value <0.1)") + ylab("") +
  #xlim(-3, 3) +
  geom_segment(aes(x=conf.low,y=term,xend=conf.high,
                   yend=term,colour=p.value<0.10, size = .2))+
  geom_point(aes(x=estimate,y=term,colour=p.value<0.10,size=.5)) +
  geom_label(aes(x=estimate,y=term,
                 label = ifelse((exp(estimate)>1),
                                paste("+",round((exp(estimate)-1)*100,1),"%", sep = ""),
                                paste(round((exp(estimate)-1)*100,1),"%", sep = ""))), 
             vjust=(-1), 
             size=3.5,
             label.padding = unit(0.09, "lines"), colour="DarkSlateGray")+
  scale_color_manual(values = c("gray","gray","3d9f88"))+
  scale_y_discrete(labels=
                     c("(Intercept)" = "\nRéférence (Religion: Hindu, Caste: General, \nLogement: Government Quarters, Revenu: 30 000 à 50 000)\n",
                       "house_statusDDA 1 : group housing" = "Logement: DDA group housing",
                       "house_statusDDA 2 : simple housing" = "Logement: DDA simple housing",
                       "house_statusPrivate Housing" = "Logement: Private housing",
                       "house_statusPrivate Resettlement" = "Logement: Private resettlement",
                       "house_statusSlum 1 : unauthorized colony" = "Logement: Slum unauthorized",
                       "house_statusSlum 2 : resettlement colony" = "Logement: Slum resettlement",
                       "house_statusUrban Village" = "Logement: Urban village",
                       "house_statusGovernment Quarters" = "Logement: Government quarters",
                       "children_number" = "Nombre d'enfants",
                       "odour" = "Présence d'une odeur déplaisante",
                       "incident" = "Occurence d'un incident récent",    
                       "casteOBC" = "Caste: Other Backward Class",
                       "casteSC" = "Caste: Schedule Caste",
                       "religionChristian" = "Religion: Christian",
                       "religionJain" = "Religion: Jain",   
                       "religionMuslim" = "Religion: Muslim",
                       "religionSikh" = "Religion: Sikh",    
                       "nopark5001" = "Pas d'espace-vert à moins de 500m (OSM)",
                       "income_mergedMoins de 2 000 Rs/mois" = "Revenu inférieur à 2 000 Rs/mois",
                       "income_merged2 000 à 6 000" = "De 2 000 à 6 000", 
                       "income_merged6 000 à 10 000" = "De 6 000 à 10 000",    
                       "income_merged10 000 à 20 000" = "De 10 000 à 20 000", 
                       "income_merged20 000 à 30 000" = "De 20 000 à 30 000",  
                       "income_merged30 000 à 50 000" = "De 30 000 à 50 000",  
                       "income_merged50 000 à 70 000" = "De 50 000 à 70 000",   
                       "income_merged70 000 à 110 000" = "De 70 000 à 110 000", 
                       "income_merged110 000 et plus" = "Plus de 110 000"
                     )
  )+
  scale_x_continuous(breaks = seq(-3, 4, by = 0.5))+
  theme(legend.position="none",
        #legend.background = element_rect(colour = "grey80"),
        plot.title = element_text(hjust = 0.5, face = "bold", margin=margin(0,0,20,0)),
        axis.text.x = element_text(face="bold", size = 10, margin=margin(0,0,10,0)),
        axis.text.y = element_text(face="bold", size = 11))+
  labs(caption = "Source: CHALLINEQ dataset 2020-2022; Auteur: Samuel Benkimoun, 2024.")
