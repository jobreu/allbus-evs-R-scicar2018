# Install & load required packages ####

#install.packages('rstudioapi')
#install.packages('devtools)
#install.packages('rvest)
#install.packages('httr)
#install.packages('haven')
#install.packages('tidyverse')
#install.packages('cartography')
#install.packages('sp')
#install.packages('geojsonio')

library(rstudioapi)
library(devtools)
#install_github("cttobin/ggthemr") # ggthemr is only available via GitHub
#install_github("expersso/gesis") # we need the GitHub version as the download_codebook command from the CRAN version does not work for ALLBUS & EVS because they have multiple documents whose names include '_cdb'
library(ggthemr)
library(gesis)
library(haven)
library(rvest)
library(httr)
library(tidyverse)
library(cartography)
library(sp)
library(geojsonio)

# Set working directory ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define custom function for downloading questionnaires from GESIS DBK ####
get_questionnaire <- function (studynr, path = ".", quiet = FALSE) 
{
  for (d in studynr) {
    url <- paste0("https://dbk.gesis.org/dbksearch/SDesc2.asp?db=E&no=", 
                  d)
    nodename <- paste0("ZA", d, "_q.pdf")
    page <- read_html(url)
    node <- html_nodes(page, xpath = "//a[contains(text(), '_q')]")
    node <- subset(node, html_text(node) == nodename)
    node <- paste0("https://dbk.gesis.org/dbksearch/", html_attr(node, 
                                                                 "href"))
    resp <- GET(node)
    if (!quiet) 
      message("Downloaded questionnaire for ZA",d)
    filename <- gsub("^.*?\"|\"", "", resp$headers$`content-disposition`)
    filename <- file.path(path, filename)
    writeBin(content(resp, "raw"), filename)
  }
}


# Set up GEIS DBK access & explore available studies ####
s <- login(username = "johannes.breuer@gesis.org", password = "IAmYourData!")
# create account at https://dbk.gesis.org/dbksearch/register.asp?db=e
# then paste your username (= the e-mail address you use for your DBK account) & password into the login function above

get_study_groups()

# ALLBUS ####

# Download the data
get_datasets("0007") %>% 
  View("Available ALLBUS datasets")

download_dataset(s, doi = "4587", path = "./Data", filetype = ".sav", purpose = 1)
# purpose 1 =  for scientific research (incl. PhD); for other possible purposes, check ?download_dataset


download_codebook(studynr = "4587", path = "./Data")

get_questionnaire(studynr = "4587", path = "./Data")

# Read in data
allbus <- read_sav("Data/ZA4587_v1-0-0.sav")
dim(allbus) # number of respondents & variables
names(allbus) # variable names

# Select variables of interest
contact <- allbus %>% 
  select(year, eastwest, starts_with("mc"), wghtpt)

# Recode variable values
contact <- contact %>%
  mutate(mc01 = recode(as.numeric(mc01), "2"= 0)) %>% 
  mutate(mc02 = recode(as.numeric(mc02), "2"= 0)) %>% 
  mutate(mc03 = recode(as.numeric(mc03), "2"= 0)) %>% 
  mutate(mc04 = recode(as.numeric(mc04), "2"= 0))

# Weight data & check effect of weights
contact <- contact %>% 
  mutate(mc01_w = mc01 * wghtpt) %>% 
  mutate(mc02_w = mc02 * wghtpt) %>% 
  mutate(mc03_w = mc03 * wghtpt) %>% 
  mutate(mc04_w = mc04 * wghtpt)

mean(contact$mc01, na.rm = T)
mean(contact$mc01_w, na.rm = T)
mean(contact$mc02, na.rm = T)
mean(contact$mc02_w, na.rm = T)
mean(contact$mc03, na.rm = T)
mean(contact$mc03_w, na.rm = T)
mean(contact$mc04, na.rm = T)
mean(contact$mc04_w, na.rm = T)


# Reshape dataframe for plotting
contact_long <- contact %>% 
  select(year, eastwest, mc01_w:mc04_w) %>% 
  gather(key = type_of_contact, value = yes_no, -year, -eastwest)


contact_long_plot <- contact_long %>%
  mutate(type_of_contact = recode(type_of_contact,
                                  "mc01_w" = "In der Familie",
                                  "mc02_w" = "Am Arbeitsplatz",
                                  "mc03_w" = "In der Nachbarschaft",
                                  "mc04_w" = "Im Freundeskreis")) %>% 
  mutate(eastwest = as_factor(eastwest)) %>% 
  mutate(eastwest = recode(eastwest,
                                  "ALTE BUNDESLAENDER" = "Westdeutschland",
                                  "NEUE BUNDESLAENDER" = "Ostdeutschland")) %>%
  rename(Region = eastwest,
         Jahr = year) %>% 
  filter(!is.na(yes_no)) %>% 
  group_by(Jahr, Region, type_of_contact) %>% 
  summarise(sum = sum(yes_no), n = n()) %>% 
  mutate (Anteil = (sum/n)*100)

# Choose a custom theme from the ggthemr package
ggthemr("flat")

# Plot time series
contact_long_plot %>% 
  ggplot(aes(x = Jahr, y = Anteil, color = Region, shape = Region)) +
  geom_point(size = 2) +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, 100, 10),
                     limits = c(0,100),
                     labels = c("0%", "10%", "20%", "30%", "40%" , "50%", "60%", "70%", "80%", "90%", "100%")) +
  ylab("Anteil der Befragten mit Kontakten") +
  scale_x_continuous(breaks = seq(1980, 2016, 2)) +
  theme(axis.text.x = element_text (angle = 90)) +
  xlab(NULL) +
  facet_wrap(~type_of_contact, scales = "free") +
  ggtitle("Kontakte zwischen Deutschen und Ausländern",
          subtitle = "Quelle: ALLBUScompact - Kumulation 1980-2016.GESIS Datenarchiv, Köln.\nZA4587 Datenfile Version 1.0.0, doi:10.4232/1.13048")

# Save plot
ggsave("./Plots/Kontakte_mit_Auslaendern_ALLBUS_1980-2016.png")

# EVS ####

# Download the data
get_datasets("0009") %>% 
  View("Available EVS datasets")
evs_datasets <- get_datasets("0009")
download_dataset(s, doi = "4800", path = "./Data", filetype = ".sav", purpose = 1)
# purpose 1 =  for scientific research (incl. PhD); for other possible purposes, check ?download_dataset

get_codebook(studynr = "4800", path = "./Data")
# download_codebook command from the gesis package does not work for this study because there are multiple documents whose names include '_cdb'for this study

get_questionnaire(studynr = "4800", path = "./Data")

# Read in data
evs <- read_sav("Data/ZA4800_v4-0-0.sav")
dim(evs) # number of respondents & variables
names(evs) # variable names

# Select variables of interest
attitudes <- evs %>%
  select(c_abrv, v240, v241, v243, v251)

# Get map of Europe (shapefile)
url <- "https://raw.githubusercontent.com/leakyMirror/map-of-europe/master/TopoJSON/europe.topojson"
europe_map <- topojson_read(url, verbose = FALSE)
# alternatively, load map from local folder
europe_map <- topojson_read("./Data/europe.topojson")

# check if map has been loaded correctly
plot(europe_map)

# Check for mismatches between country codes (EVS dataset vs. map)
europe_map_countries <- as.data.frame(europe_map$id)

attitudes %>% 
  group_by(c_abrv) %>%
  summarise() %>%
  anti_join(europe_map_countries, by = c('c_abrv' = 'europe_map$id'))

# List country codes
attitudes_map %>%
  group_by(c_abrv) %>%
  summarise() %>%
  View("EVS countries")
# EVS uses ISO 3166-2 country codes
# A List of these codes can, e.g., be found on Wikipedia: https://de.wikipedia.org/wiki/ISO-3166-1-Kodierliste

europe_map_countries %>% 
  View("Map countries")
# The map file uses ISO 3166 Alpha-2 country codes

# Recode country codes
attitudes <- attitudes %>%  mutate(c_abrv = recode(c_abrv, `GB-GBN` = 'GB',
                                                           `GB-NIR` = 'GB',
                                                           `CY-TCC` = 'CY',
                                                           `RS-KM` = 'RS'))

# Reshape dataframe for plotting
attitudes_map <- attitudes %>% 
  group_by(c_abrv) %>% 
  summarise(homo = mean (v240, na.rm = T),
            abort = mean(v241, na.rm = T),
            euth = mean(v243, na.rm = T),
            invitro = mean(v251, na.rm = T))

# Look at summary statictics for variables of interest (to choose categories for map plots)
attitudes_map %>% 
  select(homo:invitro) %>% 
  summary()

# Rank countries by attitudes (from highest justification to lowest)
attitudes_map %>% 
  select(c_abrv, invitro) %>% 
  arrange(-invitro) %>% 
  View("Invitro fertilization")

attitudes_map %>% 
  select(c_abrv, abort) %>% 
  arrange(-abort) %>% 
  View("Abortion")

attitudes_map %>% 
  select(c_abrv, euth) %>% 
  arrange(-euth) %>% 
  View("Euthanasia")

attitudes_map %>% 
  select(c_abrv, homo) %>% 
  arrange(-homo) %>% 
  View("Homosexuality")
  
# Plot maps

# Set a custom color palette
cols <- carto.pal(pal1 = "red.pal", # first color gradient
                  n1 = 7, # number of colors in the first gradiant
                  pal2 = "green.pal", # second color gradient
                  n2 = 9) # number of colors in the second gradiant

# Map: attitudes towards invitro fertilization
choroLayer(spdf = europe_map,
           df = attitudes_map, 
           var = "euth",
           breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9),
           col = cols,
           legend.pos = "right", # position of the legend
           legend.title.txt = "Justification of\ninvitro fertilization\n1 = never\n10 = always", # title of the legend
           legend.values.rnd = 1, # number of decimal in the legend values
           legend.frame = T)


layoutLayer(title = "Attitudes towards invitro fertilization in Europe",
            sources = "Source:\nEuropean Values Study 2008: Integrated Dataset (EVS 2008).\nGESIS Datenarchiv, Köln. ZA4800 Datenfile Version 4.0.0,\ndoi:10.4232/1.12458",
            author = NULL,
            frame = TRUE,
            col = NA, 
            scale = NULL,
            coltitle = "black")

# Export plot
sizes <- getFigDim(x = europe_map, width = 2715, mar = c(10,0,4,0))

png("./Plots/evs_map_invitro.png", width = sizes[1], height = sizes[2], res = 240)

choroLayer(spdf = europe_map,
           df = attitudes_map, 
           var = "euth",
           breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9),
           col = cols,
           legend.pos = "right", # position of the legend
           legend.title.txt = "Justification of\ninvitro fertilization\n1 = never\n10 = always", # title of the legend
           legend.values.rnd = 1, # number of decimal in the legend values
           legend.frame = T)


layoutLayer(title = "Attitudes towards invitro fertilization in Europe",
            sources = "Source:\nEuropean Values Study 2008: Integrated Dataset (EVS 2008).\nGESIS Datenarchiv, Köln. ZA4800 Datenfile Version 4.0.0,\ndoi:10.4232/1.12458",
            author = NULL,
            frame = TRUE,
            col = NA, 
            scale = NULL,
            coltitle = "black")

dev.off()

# Map: attitudes towards abortion
choroLayer(spdf = europe_map,
           df = attitudes_map, 
           var = "abort",
           breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5),
           col = cols,
           legend.pos = "right", # position of the legend
           legend.title.txt = "Justification of\nabortion\n1 = never\n10 = always", # title of the legend
           legend.values.rnd = 1, # number of decimal in the legend values
           legend.frame = T)


layoutLayer(title = "Attitudes towards abortion in Europe",
            sources = "Source:\nEuropean Values Study 2008: Integrated Dataset (EVS 2008).\nGESIS Datenarchiv, Köln. ZA4800 Datenfile Version 4.0.0,\ndoi:10.4232/1.12458",
            author = NULL,
            frame = TRUE,
            col = NA, 
            scale = NULL,
            coltitle = "black")

# Export plot
sizes <- getFigDim(x = europe_map, width = 2715, mar = c(10,0,4,0))

png("./Plots/evs_map_abortion.png", width = sizes[1], height = sizes[2], res = 240)

choroLayer(spdf = europe_map,
           df = attitudes_map, 
           var = "abort",
           breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5),
           col = cols,
           legend.pos = "right", # position of the legend
           legend.title.txt = "Justification of\nabortion\n1 = never\n10 = always", # title of the legend
           legend.values.rnd = 1, # number of decimal in the legend values
           legend.frame = T)


layoutLayer(title = "Attitudes towards abortion in Europe",
            sources = "Source:\nEuropean Values Study 2008: Integrated Dataset (EVS 2008).\nGESIS Datenarchiv, Köln. ZA4800 Datenfile Version 4.0.0,\ndoi:10.4232/1.12458",
            author = NULL,
            frame = TRUE,
            col = NA, 
            scale = NULL,
            coltitle = "black")

dev.off()

# Map: attitudes towards euthanasia
choroLayer(spdf = europe_map,
           df = attitudes_map, 
           var = "euth",
           breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5),
           col = cols,
           legend.pos = "right", # position of the legend
           legend.title.txt = "Justification of\neuthanasia\n1 = never\n10 = always", # title of the legend
           legend.values.rnd = 1, # number of decimal in the legend values
           legend.frame = T)
           

layoutLayer(title = "Attitudes towards euthanasia in Europe",
            sources = "Source:\nEuropean Values Study 2008: Integrated Dataset (EVS 2008).\nGESIS Datenarchiv, Köln. ZA4800 Datenfile Version 4.0.0,\ndoi:10.4232/1.12458",
            author = NULL,
            frame = TRUE,
            col = NA, 
            scale = NULL,
            coltitle = "black")

# Export plot
sizes <- getFigDim(x = europe_map, width = 2715, mar = c(10,0,4,0))

png("./Plots/evs_map_euthanasia.png", width = sizes[1], height = sizes[2], res = 240)

choroLayer(spdf = europe_map,
           df = attitudes_map, 
           var = "euth",
           breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5),
           col = cols,
           legend.pos = "right", # position of the legend
           legend.title.txt = "Justification of\neuthanasia\n1 = never\n10 = always", # title of the legend
           legend.values.rnd = 1, # number of decimal in the legend values
           legend.frame = T)


layoutLayer(title = "Attitudes towards euthanasia in Europe",
            sources = "Source:\nEuropean Values Study 2008: Integrated Dataset (EVS 2008).\nGESIS Datenarchiv, Köln. ZA4800 Datenfile Version 4.0.0,\ndoi:10.4232/1.12458",
            author = NULL,
            frame = TRUE,
            col = NA, 
            scale = NULL,
            coltitle = "black")

dev.off()

# Map: attitudes towards homosexuality
choroLayer(spdf = europe_map,
           df = attitudes_map, 
           var = "homo",
           breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9),
           col = cols,
           legend.pos = "right", # position of the legend
           legend.title.txt = "Justification of\nhomosexuality\n1 = never\n10 = always", # title of the legend
           legend.values.rnd = 1, # number of decimal in the legend values
           legend.frame = T)


layoutLayer(title = "Attitudes towards homosexuality in Europe",
            sources = "Source:\nEuropean Values Study 2008: Integrated Dataset (EVS 2008).\nGESIS Datenarchiv, Köln. ZA4800 Datenfile Version 4.0.0,\ndoi:10.4232/1.12458",
            author = NULL,
            frame = TRUE,
            col = NA, 
            scale = NULL,
            coltitle = "black")

# Export plot
sizes <- getFigDim(x = europe_map, width = 2715, mar = c(10,0,4,0))

png("./Plots/evs_map_homosexuality.png", width = sizes[1], height = sizes[2], res = 240)

choroLayer(spdf = europe_map,
           df = attitudes_map, 
           var = "homo",
           breaks = c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9),
           col = cols,
           legend.pos = "right", # position of the legend
           legend.title.txt = "Justification of\nhomosexuality\n1 = never\n10 = always", # title of the legend
           legend.values.rnd = 1, # number of decimal in the legend values
           legend.frame = T)


layoutLayer(title = "Attitudes towards homosexuality in Europe",
            sources = "Source:\nEuropean Values Study 2008: Integrated Dataset (EVS 2008).\nGESIS Datenarchiv, Köln. ZA4800 Datenfile Version 4.0.0,\ndoi:10.4232/1.12458",
            author = NULL,
            frame = TRUE,
            col = NA, 
            scale = NULL,
            coltitle = "black")

dev.off()