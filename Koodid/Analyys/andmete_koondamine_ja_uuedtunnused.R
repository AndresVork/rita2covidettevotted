#Andmete kokku panek ja uute tunnuste tegemine analüüsiks

#AV TODO:! Kontrollida, et kasvutempode arvutamisel oleks olemas aastatagune kvartal
#AV TODO:! kontrollida, mis tuleb kasvutempoks, kui nii lugeja kui nimetaja on nullid.

library(tidyverse)
library(zoo)
#EMTA
load(file = "./Andmed/R_andmed/andmed_emta.RData")
#ESA profiil
load(file = "./Andmed/R_andmed/andmed_esa.RData")
#Äriregister
load(file = "./Andmed/R_andmed/andmed_ari.RData")
#KMK grupi andmed
load(file = "./Andmed/R_andmed/kmk_grupp.RData")

#Puuduvad nulliks
missingtozero <- function(x) ifelse(is.na(x), 0, x)
#Võtame sidumise aluseks EMTA andmed
#Jätame alles vaid ettevõtted
andmed_emta_sidumiseks <- andmed_emta %>% 
  filter(liik == "Äriühing") %>% 
  #Puuduvad väärtused nulliks maksude ja tootajate arvu puhul, sest makse peab maksma ja TSD esitama. 
  #TODO:! Kaaluda: käibe puhul võiks võtta nulli siis, kui ettevõte on käibemaksukohuslane, ja deklareerib nulli
  #Väikestel ettevõtetel alahindab käivet (piiriks kuni 40 tuhat eurot aastas ehk 10 tuhat eurot kvartalis)
  mutate_at(vars(rmaksud, toomaksud, kaive, tootajad), missingtozero) %>% 
  #eraldame viimaste kvartalite andmetest välja valla ja linna
  mutate(aadress = ifelse(maakond==""| is.na(maakond), NA, maakond)) %>% 
  mutate(maakond = stringr::word(aadress, sep = fixed(" ")), #võtame esimese sõna, annab maakonna nime, nt Harju, Ida-Viru jne
         valdlinn = stringr::str_extract(aadress, "(?<=\\(\\s)[\\w\\s-]+(?=\\s\\))"))

#Kõige vanema ja uusima kvartali leidmine ettevõtte kohta
abi_emta <- andmed_emta_sidumiseks %>% 
  mutate(aastakv = as.numeric(paste0(aasta, kvartal))) %>% 
  group_by(registrikood) %>% 
  summarise(min_aastakv = min(aastakv),
            max_aastakv = max(aastakv))

#Kõikidele ettevõtetele kõik kvartalid ja min-max järgi filtreerimine ja andmete täitmine
#Äriühingute korral lisab andmestikku (seis kuni 2020 III kvartal) 52469 rida (1643907st saab 1696376)
andmed_emta_sidumiseks <- andmed_emta_sidumiseks %>% 
  complete(registrikood, nesting(aasta, kvartal), fill = list(rmaksud = 0, toomaksud = 0, kaive = 0, tootajad = 0)) %>% 
  mutate(aastakv = as.numeric(paste0(aasta, kvartal))) %>% 
  left_join(abi_emta) %>% 
  filter(aastakv >= min_aastakv, aastakv <= max_aastakv) %>% 
  select(-aastakv, -min_aastakv, -max_aastakv) %>% 
  group_by(registrikood) %>% 
  fill(nimi, liik, KMK, maakond, emtak_emta, aadress, valdlinn) %>%  #Vaikimisi võtab fill valiku "down", ehk asendatakse eelmise väärtusega kui puudub
  ungroup()

#Moodi leidmine
getmode <- function(v) {
  uniqv <- unique(v[!is.na(v)])
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Enim esinenud maakond ja valdlinn, kui see on olemas, iga ettevõtte jaoks 
abi_valdlinn <- andmed_emta_sidumiseks %>% 
  group_by(registrikood) %>% 
  summarise(valdlinn = getmode(valdlinn),
            maakond = getmode(maakond))

#Seome juurde kui puuduv
andmed_emta_sidumiseks <- left_join(andmed_emta_sidumiseks, abi_valdlinn, by = c("registrikood"), 
                                    suffix = c("", ".y")) %>% 
  #kui maakond või valdlinn puuduv, aga mõni kord on olnud ja seega olemas tabelis abi_valdlinn, siis võta too
  mutate(maakond = ifelse(is.na(maakond), maakond.y, maakond),
         valdlinn = ifelse(is.na(valdlinn), valdlinn.y, valdlinn)) %>% 
  select(-valdlinn.y, -maakond.y)

#Seome juurde ESA andmetest EMTAKi, see on viimane seis
andmed_seotud <- andmed_emta_sidumiseks %>% left_join(andmed_esa %>% 
                                                        rename(emtak_taht_esa = emtak_taht, emtak4_esa = emtak4), by = "registrikood")  

#Sektori nimede juurde sidumine ja puuduvate EMTAKi andmete täitmine
load(file = "./Andmed/R_andmed/emtaknames.RData")

#Seome juurde tähe EMTAKi nimele, et saaks kombineerida ESA andmetega
andmed_seotud <- left_join(andmed_seotud, 
                           emtaknames %>% filter(nchar(emtak)==1) %>% select(emtak, tekst),  #võtame vaid tähe tasandi
                           by = c("emtak_emta"= "tekst")) %>% 
  rename(emtak_taht_emta = emtak) %>% 
  
  #tähe tasandi sektor on EMTA oma ja kui puudu, siis võtame ESA oma 
  mutate(emtak_taht = ifelse(is.na(emtak_taht_emta), emtak_taht_esa, emtak_taht_emta)) %>% 
  #Teeme emtak2 koodi ESA andmetest. EMTA andmetes seda ei ole
  mutate(emtak2 = substr(emtak4_esa, 1,2))

#ja seome uuesti juurde EMTAKi pikad nimed
andmed_seotud <- andmed_seotud %>% 
  left_join(emtaknames %>% mutate(emtaktahttekst = as.factor(nrtekst)) %>% select(emtak, emtaktahttekst), 
            by = c("emtak_taht" = "emtak")) %>% 
  left_join(emtaknames %>% mutate(emtak2tekst = as.factor(nrtekst)) %>% select(emtak, emtak2tekst), 
            by = c("emtak2" = "emtak")) 


#Osade jaoks ikka address puudub, sest mõnel ei ole üldse aadressi.
#Sel juhul seome juurde aadressi äriregistrist
#Äriregistri andmetest saame siduda juurde ka staatuse viimase hetke seisuga
abi_aadressid_ari <- strsplit(andmed_ari$asukoht, ", ", fixed = TRUE)
andmed_ari_sidumiseks <- andmed_ari %>% 
  #võtan aadressist viimase elemendi ja sellest esimese sõna - saan maakonna. Teine rida annab valla või linna
  mutate(maakond = word(sapply(abi_aadressid_ari, "[", 3), sep = fixed(" ")),
         valdlinn = sapply(abi_aadressid_ari, "[", 2),
         registrkpv = as.Date(andmed_ari$asutamise_kp, format = "%d.%m.%Y"),
         registrikood=as.character(registrikood))  %>% 
  as.data.frame()

andmed_seotud <- andmed_seotud %>% 
  left_join(andmed_ari_sidumiseks %>% select(registrikood, staatus, maakond, valdlinn, registrkpv, kmkr_nr),
            by = "registrikood", suffix = c("", ".y")) %>% 
  #kui maakond või valdlinn puuduv, aga on olemas äriregistri andmetes, võta sealt
  mutate(maakond = ifelse(is.na(maakond), maakond.y, maakond),
         valdlinn = ifelse(is.na(valdlinn), valdlinn.y, valdlinn),
         registrkpv = as.Date(ifelse(is.na(registrkpv), registrkpv.y, registrkpv), origin = '1970-01-01')
         ) %>% 
  select(-valdlinn.y, -maakond.y, -registrkpv.y)

rm(abi_aadressid_ari, abi_valdlinn, andmed_ari_sidumiseks, 
   andmed_emta_sidumiseks, andmed_ari, andmed_emta, andmed_esa, abi_emta, emtaknames)

# KMK grupi ridade lisamine -----------------------------------------------
andmed_seotud <- andmed_seotud %>% 
  mutate(aeg = as.yearqtr(paste(aasta, kvartal, sep = "-"))) %>% 
  left_join(kmk_grupp4, by = c("registrikood", "aeg"))

kmk_grupi_read <- andmed_seotud %>% 
  filter(!is.na(liider_kood),
         liider_kood != 1) %>% 
  group_by(liider_kood, aeg) %>% 
  mutate(rmaksud = sum(rmaksud*kaal),
            toomaksud = sum(toomaksud*kaal),
            kaive = sum(kaive*kaal),
            tootajad = sum(tootajad*kaal)) %>% 
  ungroup() %>% 
  filter(registrikood == liider_kood) %>% 
  mutate(liider_kood = 1,
         nimi = paste(nimi, "(KMK grupp)"))

andmed_seotud <- andmed_seotud %>% 
  rbind(kmk_grupi_read) %>% 
  select(-kaal) %>% 
  replace_na(list(liider_kood = 0)) %>% 
  arrange(registrikood, aasta, kvartal)
  
save(andmed_seotud, file = "./Andmed/R_andmed/andmed_seotud.RData")

#Tunnuste tegemine mikroandmete põhjal -------------------
load(file = "./Andmed/R_andmed/andmed_seotud.RData")

df_anal <- andmed_seotud %>% 
  #Ad hoc muutused
  mutate(
    #1. Kui kvartali käive on nullist erinev, kuid ei ole ühtegi töötajad kvartali lõpus, paneme töötaja võrdseks 1-ga
    tootajad = ifelse(kaive!=0 & tootajad==0, 1, tootajad),
    #2. Kui tööjõumaksud on nullist erinev, kuid ei ole ühtegi töötajad kvartali lõpus, paneme töötaja võrdseks 1-ga
    tootajad = ifelse(toomaksud!=0 & tootajad==0, 1, tootajad)) %>% 
  
  #Ettevõtte vanus aastates, tööjõumaksud töötaja kohta, käive töötaja kohta
  mutate(vanus = round((Sys.Date()-registrkpv)/365,2),
         toomaksud_tootajakohta = toomaksud/tootajad,
         kaive_tootajakohta = kaive/tootajad) %>% 
  group_by(registrikood) %>% 
  arrange(aasta, kvartal) %>% 
  #1. Dünaamika
  #Aastane kasvutempo keskpunkti meetodil - TODO:! Kas parim?
  #kui mõlemad tulevad nullid, siis ei ole defineeritud ja peaks tulema NaN
  mutate(kaive_kasv4 = (kaive - dplyr::lag(kaive,4))/((kaive + dplyr::lag(kaive,4))/2),
         tootajate_kasv4 = (tootajad - dplyr::lag(tootajad,4)) / ((tootajad + dplyr::lag(tootajad,4))/2),
         toomaksud_kasv4 = (toomaksud - dplyr::lag(toomaksud,4)) / ((toomaksud + dplyr::lag(toomaksud,4))/2),
         #Iseloomustab palga kasvu
         toomaksud_tootajakohta_kasv4 = (toomaksud_tootajakohta - dplyr::lag(toomaksud_tootajakohta,4)) / ((toomaksud_tootajakohta + dplyr::lag(toomaksud_tootajakohta,4))/2)) %>% 
  #2. Muutus võrreldes enda keskmisega, alates EMTA andmete algusest, iseloomustab ettevõtte seisu sarnaselt kasvutempoga
  mutate(dkaive = kaive - mean(kaive, na.rm = TRUE),
         dtootajad = tootajad - mean(tootajad, na.rm = TRUE),
         dtoomaksud = toomaksud - mean(toomaksud, na.rm = TRUE),
         dtoomaksud_tootajakohta = toomaksud_tootajakohta - mean(toomaksud_tootajakohta, na.rm = TRUE)) %>% 
  ungroup() %>% 
  #Ettevõtte olulisus riigis, maakonnas, vallas käibe, riiklike maksude, töötajate arvu ja tööjõumaksude järgi
  #TODO:! See võib olla tehtud ka mingi perioodi peale kokku, mitte igas kvartalis.Võib jätta selle ka rakenduse teha.
  #Vallalinna järgi
  group_by(valdlinn, aasta, kvartal) %>%  
  mutate(kaive_olulisus_valdlinn = ifelse(liider_kood %in% c(0,1), kaive/sum(kaive*(liider_kood %in% c(0,1))), NA), #KMK grupi originaalread jäävad sum arvutamisest välja
         tootajad_olulisus_valdlinn = ifelse(liider_kood %in% c(0,1), tootajad/sum(tootajad*(liider_kood %in% c(0,1))), NA),
         rmaksud_olulisus_valdlinn = ifelse(liider_kood %in% c(0,1), rmaksud/sum(rmaksud*(liider_kood %in% c(0,1))), NA),
         toomaksud_olulisus_valdlinn = ifelse(liider_kood %in% c(0,1), toomaksud/sum(toomaksud*(liider_kood %in% c(0,1))), NA)) %>% 
  #Maakonna järgi
  group_by(maakond, aasta, kvartal) %>%  
  mutate(kaive_olulisus_maakond = ifelse(liider_kood %in% c(0,1), kaive/sum(kaive*(liider_kood %in% c(0,1))), NA),
         tootajad_olulisus_maakond = ifelse(liider_kood %in% c(0,1), tootajad/sum(tootajad*(liider_kood %in% c(0,1))), NA),
         rmaksud_olulisus_maakond = ifelse(liider_kood %in% c(0,1), rmaksud/sum(rmaksud*(liider_kood %in% c(0,1))), NA),
         toomaksud_olulisus_maakond = ifelse(liider_kood %in% c(0,1), toomaksud/sum(toomaksud*(liider_kood %in% c(0,1))), NA)) %>% 
  #Riigi järgi
  group_by(aasta, kvartal) %>%  
  mutate(kaive_olulisus_riik = ifelse(liider_kood %in% c(0,1), kaive/sum(kaive*(liider_kood %in% c(0,1))), NA),
         tootajad_olulisus_riik = ifelse(liider_kood %in% c(0,1), tootajad/sum(tootajad*(liider_kood %in% c(0,1))), NA),
         rmaksud_olulisus_riik = ifelse(liider_kood %in% c(0,1), rmaksud/sum(rmaksud*(liider_kood %in% c(0,1))), NA),
         toomaksud_olulisus_riik = ifelse(liider_kood %in% c(0,1), toomaksud/sum(toomaksud*(liider_kood %in% c(0,1))), NA)) %>% 
  ungroup()

#Sektori tunnused EMTAKi järgi

#Võrgustiku näitajad
load(file = "./Andmed/R_andmed/ostjadmyyjad.RData")
df_anal <- df_anal %>% 
  #seome haru tasemel müüjate ja ostjate keskmised arvud. AV TODO:! hetkel käibega kaalumata keskmised
  left_join(ostjadmyyjad, by = c("emtak2" = "emtak2")) 

#Jätame alles vaid minimaalselt vajalikud tunnused, et andmestik oleks väiksem serveris
df_anal <- df_anal %>% select(registrikood, aasta, kvartal, aeg, nimi, KMK, maakond, valdlinn, emtak_taht, emtaktahttekst, emtak2, emtak2tekst, emtak4 = emtak4_esa, staatus,
                              liider_kood, rmaksud, toomaksud, kaive, tootajad, toomaksud_tootajakohta, kaive_tootajakohta, 
                              kaive_kasv4, tootajate_kasv4, toomaksud_kasv4, toomaksud_tootajakohta_kasv4,
                              dkaive, dtootajad, dtoomaksud, dtoomaksud_tootajakohta, 
                              kaive_olulisus_valdlinn, tootajad_olulisus_valdlinn, rmaksud_olulisus_valdlinn, toomaksud_olulisus_valdlinn, 
                              kaive_olulisus_maakond, tootajad_olulisus_maakond, rmaksud_olulisus_maakond, toomaksud_olulisus_maakond, 
                              kaive_olulisus_riik, tootajad_olulisus_riik, rmaksud_olulisus_riik, toomaksud_olulisus_riik, 
                              haru_myyjaid, haru_ostjaid)

save(df_anal, file = "./Andmed/R_andmed/df_anal.RData")
