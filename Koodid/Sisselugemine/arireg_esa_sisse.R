#Äriregistri ja ESA andmete sisselugemine

#Logi
#10.12.2021 AV uued andmed, parandasin ESA andmete sisselugemisel tekkinud vea, 
# sest osasid tähekujulisi registrikoode ei lugenud sisse, sest arvas, et peab olema number
# ei mõjutanud lõpptulemust, sest olid pangad

#06.12.2020 AV ESA lisaandmed
#23.11.2020 Annegrete Molloka

#paketid
require(tidyverse)
require(data.table)
require(readxl)


#Äriregister-------------
#Esimesed andmed
andmed_ari <- fread("Andmed/Ariregister/ettevotja_rekvisiidid_2020-10-07.csv", encoding = "UTF-8" ) 
andmed_ari <- andmed_ari %>% 
  select(nimi, registrikood = ariregistri_kood, vorm = ettevotja_oiguslik_vorm, kmkr_nr, 
         staatus = ettevotja_staatus_tekstina, asutamise_kp = ettevotja_esmakande_kpv, asukoht = asukoha_ehak_tekstina)
andmed_ari$andmeteseis <- as.Date("2020-10-07")
andmed_ari$registrikood <- as.character(andmed_ari$registrikood)

#Uued andmed
#Väljavõte 2021-07-07
andmed_ari_uus <- fread("Andmed/Ariregister/ettevotja_rekvisiidid_2021-07-07.csv", encoding = "UTF-8" )
andmed_ari_uus <- andmed_ari_uus %>% 
  select(nimi, registrikood = ariregistri_kood, vorm = ettevotja_oiguslik_vorm, kmkr_nr, 
         staatus = ettevotja_staatus_tekstina, asutamise_kp = ettevotja_esmakande_kpv, asukoht = asukoha_ehak_tekstina)
andmed_ari_uus$andmeteseis <- as.Date("2021-07-07")
andmed_ari_uus$registrikood <- as.character(andmed_ari_uus$registrikood)

#Lisame vanadest andmed need registrikoodid juurde, mida uutes ei ole. Seega sisuliselt lõpetanud ettevõtted.
andmed_ari <- bind_rows(andmed_ari_uus, 
                        anti_join(andmed_ari, andmed_ari_uus, 
                                  by = c("registrikood")))

#Väljavõte 2021-12-10
andmed_ari_uus <- fread("Andmed/Ariregister/ettevotja_rekvisiidid_2021-12-10.csv", encoding = "UTF-8" ) 
andmed_ari_uus <- andmed_ari_uus %>% 
  select(nimi, registrikood = ariregistri_kood, vorm = ettevotja_oiguslik_vorm, kmkr_nr, 
         staatus = ettevotja_staatus_tekstina, asutamise_kp = ettevotja_esmakande_kpv, asukoht = asukoha_ehak_tekstina)
andmed_ari_uus$andmeteseis <- as.Date("2021-12-10")
andmed_ari_uus$registrikood <- as.character(andmed_ari_uus$registrikood)

andmed_ari <- bind_rows(andmed_ari_uus, 
                  anti_join(andmed_ari, andmed_ari_uus, 
                            by = c("registrikood")))

save(andmed_ari, file = "Andmed/R_andmed/andmed_ari.RData")


#ESA-------------
andmed_esa <- read_excel("Andmed/ESAuksused/yksuste_nimekiri_01.12.2019_revid.23.01.20.xlsx",
                         col_types = c("text", "text", "text", "text", "text", "text", "date", "date")) %>% 
  as.data.frame()

andmed_esa <- andmed_esa %>% 
  select(registrikood = Registrikood, 
         emtak_taht = `EMTAK2008 Tähtkoodiga valdkond`, 
         emtak4 = `EMTAK2008 4-kohaline kood`,
         registrkpv = `Üksuse registreerimise kuupäev`,
         kustutkpv = `Üksuse registrist kustutamise kuupäev`) %>% 
  mutate(registrkpv = as.Date(registrkpv),
         kustutkpv = as.Date(kustutkpv))
andmed_esa$andmeteseis <- as.Date("2020-01-23")

#Uute andmete lisamine
andmed_esa_uus <- read_excel("Andmed/ESAuksused/Üksuste nimekiri 01.12.2021_av.xlsx", 
                             col_types = c("text", "text", "text", "text", "text", "text", "text", "date", "date", "date")) %>% 
  as.data.frame()

andmed_esa_uus <- andmed_esa_uus %>% 
  select(registrikood = Registrikood, 
         emtak_taht = `EMTAK2008 Tähtkoodiga valdkond`, 
         emtak4 = `EMTAK2008 4-kohaline kood`,
         registrkpv = `Üksuse registreerimise kuupäev`,
         kustutkpv = `Üksuse registrist kustutamise kuupäev`) %>% 
  mutate(registrkpv = as.Date(registrkpv),
         kustutkpv = as.Date(kustutkpv))
andmed_esa_uus$andmeteseis <- as.Date("2021-12-01")

#Lisame vanadest andmetest need registrikoodid juurde, mida uutes ei ole. Seega need on ESA baasist välja langenud ettevõtted!
andmed_esa <- bind_rows(andmed_esa_uus, 
                        anti_join(andmed_esa, andmed_esa_uus, 
                                  by = c("registrikood")))

save(andmed_esa, file = "Andmed/R_andmed/andmed_esa.RData")
