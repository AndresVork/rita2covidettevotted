#global.R

#paketid
library(shiny)
library(DT)
library(shinyWidgets)
library(zoo)
library(tidyverse)
library(shinydashboard)
library(shinycssloaders)
library(showtext)

#andmed
load(file = "../../Andmed/R_andmed/df_koik.RData")
load(file = "../../Andmed/R_andmed/df_meetmed.RData")

#plusmiinus sümbol
plusmiinus <- "\u00b1"
Encoding(plusmiinus) <- "UTF-8"

#Roboto Condensed font jooniste jaoks
font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

#tunnused
df_koik <- df_koik %>% 
  mutate(emtaktahttekst = droplevels(emtaktahttekst), #jätame alles ainult esinevad tasemed
         emtak2tekst = droplevels(emtak2tekst))
sektorid <- sort(unique(df_koik$emtaktahttekst))
emtak2 <- sort(unique(df_koik$emtak2tekst))
maakonnad <- sort(unique(df_koik$maakond))

max_kaive <- df_koik %>% filter(aasta == 2019, liider_kood != 1) %>% #meetme lehel ei kuvata grupeeritud ridu
  group_by(registrikood) %>% summarise(aasta_kaive = sum(kaive))%>%
  pull(aasta_kaive) %>% max()
max_tootajad <- df_koik %>% filter(aasta == 2019, liider_kood != 1) %>% #meetme lehel ei kuvata grupeeritud ridu
  group_by(registrikood) %>% summarise(aasta_tootajad = mean(tootajad))%>%
  pull(aasta_tootajad) %>% max()

ettevotte_nimekiri <- df_koik %>% 
  filter(liider_kood != 1) %>% #viskame grupeeritud read välja
  group_by(registrikood) %>% 
  summarise(nimi = last(nimi)) %>% #võtame igale registrikoodile viimase kvartali nime
  ungroup() %>% 
  rbind(df_koik %>% 
          filter(liider_kood == 1) %>% 
          group_by(registrikood) %>% 
          summarise(nimi = last(nimi))) %>% #lisame grupi valikud
  arrange(nimi) %>% 
  select(registrikood, nimi)

tunnusedjarjestamiseks <- c("rmaksud", "toomaksud", "kaive", "tootajad", 
                            "toomaksud_tootajakohta", "kaive_tootajakohta", 
                            "kaive_kasv4", "tootajate_kasv4", "toomaksud_kasv4", "toomaksud_tootajakohta_kasv4",
                            "dkaive", "dtootajad", "dtoomaksud", "dtoomaksud_tootajakohta", 
                            "kaive_olulisus_valdlinn", "tootajad_olulisus_valdlinn", "rmaksud_olulisus_valdlinn", "toomaksud_olulisus_valdlinn", 
                            "kaive_olulisus_maakond", "tootajad_olulisus_maakond", "rmaksud_olulisus_maakond", "toomaksud_olulisus_maakond", 
                            "kaive_olulisus_riik", "tootajad_olulisus_riik", "rmaksud_olulisus_riik", "toomaksud_olulisus_riik", 
                            "haru_myyjaid", "haru_ostjaid")

#jooniste theme
theme_ev <- function(){ 
  font <- "Roboto Condensed"   #font family
  
  theme_light() %+replace%    #muudame vajalikud parameetrid
    
    theme(
      #text elements
      plot.title = element_text(             #title
        family = font,
        size = 18, 
        face = 'bold',
        colour = '#53565A',
        vjust = 2,              #kergelt tõstetud
        hjust = 0),             #left align
      
      axis.title = element_text(             #axis titles
        family = font,            
        size = 13,
        colour = "#53565A"),              
      
      axis.text = element_text(              #axis text
        family = font,            
        size = 12,
        colour = "#75787B"),
      
      legend.text = element_text(              #legend text
        family = font,            
        size = 12,
        colour = "#53565A")
    )
}

#y telje muutmine 
kymne_aste <- function(tunnus){
max_vaartus <- max(abs(tunnus[abs(tunnus) < Inf]), na.rm = TRUE)
if (max_vaartus > 1e+09) {
  div <- 1e+09
  label <- "(miljardites)"
  label2 <- "mld"
} else { 
  if (max_vaartus > 1e+06) {
    div <- 1e+06
    label <- "(miljonites)"
    label2 <- "mln"
  } else {
    if (max_vaartus > 1000) {
      div <- 1000
      label <- "(tuhandetes)"
      label2 <- "tuhat"
    } else {
      div <- 1
      label <- ""
      label2 <- ""
    }
  }}
return(list(div = div, label = label, label2 = label2)) 
}

#jooniste ülesehitus
line_graph <- function(data, ..., group_labels = NULL, title = NULL, 
                       xlab = "", ylab = NULL) {
  validate(need(nrow(data) > 0, "Pole andmeid"))
  if (length(group_labels) == 1) {
    varv <- "#0072CE"
  } else {
    varv <- c("#97999B", "#0072CE")
  }
  ggplot(data, aes(...)) + 
    geom_line(size = 1.2) +
    geom_point() + 
    scale_colour_manual(name = "", values = varv, labels = group_labels) +
    labs(title = title, x = xlab, y = ylab) +
    theme_ev()
}

line_graph_yksik <- function(data, ..., valitud, sarnased = FALSE, title = NULL, 
                       xlab = "", ylab = NULL) {
  validate(need(nrow(data) > 0, "Pole andmeid"))
  if(sarnased){
    ggplot(data %>% filter(grupp != c(valitud, "Sarnaste ettevõtete keskmine")), 
           aes(...)) + 
      geom_line(aes(color = "#D9D9D6"), size = 1.2) +
      geom_point(aes(color = "#D9D9D6")) +
      geom_line(data = data %>% filter(grupp == "Sarnaste ettevõtete keskmine"),
                aes(colour = "#97999B"), size = 1.2) +
      geom_point(data = data %>% filter(grupp == "Sarnaste ettevõtete keskmine"), 
                 aes(colour = "#97999B")) +
      geom_line(data = data %>% filter(grupp == valitud),
                aes(colour = "#0072CE"), size = 1.2) +
      geom_point(data = data %>% filter(grupp == valitud), 
                 aes(colour = "#0072CE")) +
      scale_color_identity(name = "",
                           breaks = c("#0072CE", "#97999B", "#D9D9D6"),
                           labels = c(valitud, "Sarnaste ettevõtete keskmine", "Sarnane ettevõte"),
                           guide = "legend") +
      labs(title = title, x = xlab, y = ylab) +
      theme_ev()
  } else {
    ggplot(data, aes(..., color = valitud)) + 
      geom_line(size = 1.2) +
      geom_point() + 
      scale_colour_manual(name = "", values = "#0072CE", labels = valitud) +
      labs(title = title, x = xlab, y = ylab) +
      theme_ev()
  }
  
}
