setwd("D:/UNI/000 Iza/PhD on Data/Review - graphics for Tani")

#figures for the review

#Sankey diagram ----
library(highcharter)
library(htmlwidgets)

Sankey <- read.delim("D:/UNI/000 Iza/PhD on Data/Review - graphics for Tani/Sankey.txt")
View(Sankey)

Sankey$Species <- as.character(Sankey$Species)
Sankey$Continents <- as.character(Sankey$Continents)
Sankey$Trophic_level <- as.character(Sankey$Trophic_level)
Sankey$Vertebrate_taxa <- as.character(Sankey$Vertebrate_taxa)

spp <- Sankey$Species
Cont <- Sankey$Continents
troph <- Sankey$Trophic_level
taxa <- Sankey$Vertebrate_taxa

d <- data.frame(cbind(Cont, taxa, troph))
names(d) <- c('Continents', 'Vertebrate_taxa', 'Trophic_level')


highchart() %>%
  hc_add_series(data = data_to_sankey(d), type = "sankey"
                ,   hcaes(from = from, to = to, weight = weight)
                ,   nodes = list(list(id = 'North America'  , color = "sienna")
                                 ,list(id = 'South America'  , color = "blue")
                                 ,list(id = 'Amphibians'  , color = "forestgreen")
                                 ,list(id = 'Birds' , color = "steelblue")
                                 ,list(id = 'Insects'   , color = "grey")
                                 ,list(id = "Mammal"  , color = "rosybrown")
                                 ,list(id = "Reptiles"  , color = "salmon")
                                 ,list(id = "C"  , color = "wheat")
                                 ,list(id = "H"  , color = "khaki")
                                 ,list(id = "O"  , color = "peachpuff")
                                 ,list(id = "I"  , color = "lightsteelblue")
                                 ,list(id = "I, H"  , color = "lightskyblue")
                                 ,list(id = "O, C"    , color = "tan")
                )) %>%
  hc_plotOptions(series = list(dataLabels = list( style = list(fontSize = "16px" , color = "black")
                                                  , backgroundColor = "white"
                                                  , borderRadius = 3
                                                  , borderWidth = 1
                                                  , borderColor = 'slategray'
                                                  , padding = 1
                                                  , shadow = FALSE
                                                  
  )))
#Wordcloud ----
library(wordcloud)
library(RColorBrewer)

#species wordcloud
Species <- read.delim("D:/UNI/000 Iza/PhD on Data/Review - graphics for Tani/Keywords_species.txt")
View(Species)
Species$species <- as.character(Species$species)
Species$trophic <- as.character(Species$trophic)
Species$col <- as.character(Species$col)

wordcloud(Species$species, Species$species_n, min.freq = 1, max.words = 220,
          colors = Species$col, ordered.colors = T, rot.per = 0.30)

#relations (attitude) wordcloud
Relations <- read.delim("D:/UNI/000 Iza/PhD on Data/Review - graphics for Tani/Keywords_relations.txt")
View(Relations)
Relations$relation <- as.character(Relations$relation)
Relations$col <- as.character(Relations$col)

wordcloud(Relations$relation, Relations$n, min.freq = 1, max.words = 800,
          colors = Relations$col, ordered.colors = T, rot.per = 0.60)

#topic wordcloud
What <- read.delim("D:/UNI/000 Iza/PhD on Data/Review - graphics for Tani/Keywords_what.txt")
View(What)
What$what.was.tested <- as.character(What$what.was.tested)

wordcloud(What$what.was.tested, What$n, min.freq = 1, max.words = 800,
          rot.per = 0.60, colors=brewer.pal(10, "Paired"))

#Barchart ----
library(ggplot2)
library(dplyr)

years <- read.delim("D:/UNI/000 Iza/PhD on Data/Review - graphics for Tani/years.txt")
View(years)

years$col <- as.character(years$col)
hchart(years, "bar", width = 8, hcaes(x = year, y = number, color = col))


legend_title<-"Decade"
ggp <- ggplot(years, aes(x = year, y = number, fill = col))+   
  geom_bar(stat = "identity", colour = "black",  aes(fill=col), width = 1) +
  scale_fill_brewer(legend_title, palette = "Pastel1")+
  ylab("Number of studies published") +
  geom_text(aes(label = number), nudge_y = 0.7, colour = "slategray4", size = 5)+
  scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010,2012,2014,2016,2018,2020)) +
  theme_classic()

#for the line
regf <- lm(number~year, data = years)
regf
#Coefficients:
#(Intercept)         year  
#-1537.9032       0.7678

ggp + geom_abline(intercept = -1537.90, slope = 0.77, 
                  colour = "slategray4", linetype = "dashed", size = 1.1)+
  theme(text = element_text(size = 17)) 

#fancy one
ggp +  coord_flip() + scale_x_reverse()




#Donutpies ####
taxa <- read.delim("D:/UNI/000 Iza/PhD on Data/Review - graphics for Tani/Continent_taxa.txt")
View(taxa)
library(ggsci)
ggplot(data = taxa, aes (x=2, y = Africa, fill = topic))+
  geom_bar(stat = "identity")+  coord_polar("y", start = 200) + theme_void() +
  xlim(.2,2.5) + scale_fill_npg()
ggplot(data = taxa, aes (x=2, y = Asia, fill = topic))+
  geom_bar(stat = "identity")+  coord_polar("y", start = 200) + theme_void() +
  xlim(.2,2.5) + scale_fill_npg()
ggplot(data = taxa, aes (x=2, y = Australia.and.Oceania, fill = topic))+
  geom_bar(stat = "identity")+  coord_polar("y", start = 200) + theme_void() +
  xlim(.2,2.5) + scale_fill_npg()
ggplot(data = taxa, aes (x=2, y = Europe, fill = topic))+
  geom_bar(stat = "identity")+  coord_polar("y", start = 200) + theme_void() +
  xlim(.2,2.5) + scale_fill_npg()
ggplot(data = taxa, aes (x=2, y = North.America, fill = topic))+
  geom_bar(stat = "identity")+  coord_polar("y", start = 200) + theme_void() +
  xlim(.2,2.5) + scale_fill_npg()
ggplot(data = taxa, aes (x=2, y = South.America, fill = topic))+
  geom_bar(stat = "identity")+  coord_polar("y", start = 200) + theme_void() +
  xlim(.2,2.5) + scale_fill_npg()

#end of script