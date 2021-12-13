#KMK grupi andmed
#NB! Ridadel 33-35 tuleb käsitsi muuta lõppkvartalit, mis EMTA andmed on olemas

#paketid
require(tidyverse)
require(readxl)
require(lubridate)
require(zoo)

#andmed 
#vanad andmed: KMKgrupp_3012202.xlsx, KMKgrupp_31032021.xlsx, KMKgrupp_29062021.xlsx
#Uued andmed sisaldavad alati ka ajalugu (ALGUS ja LOPP tunnused, mistõttu võib võtta kõige viimase faili ja üle kirjutada)
kmk_grupp <- read_excel("Andmed/EMTAKMKR/KMKgrupp_30112021.xlsx")

kmk_grupp <- kmk_grupp %>% 
  mutate(ALGUS = as.Date(ALGUS, format = "%d.%m.%Y"),
         LOPP = as.Date(LOPP, format = "%d.%m.%Y")) %>% 
  #viskame välja read, mis lõppevad enne EMTA kvartaalseid andmeid (01.12.2016) %>% 
  filter(is.na(LOPP) | LOPP >= as.Date("2016-12-01")) %>%  #1734st sai 959 rida
  arrange(KOOD, LIIGE_KOOD, ALGUS)

#leiame gruppi kuulumise alguse ja lõpu (EMTA kvartalitele vastava) kvartali
kmk_grupp2 <- kmk_grupp %>%
  mutate(algus_kv = as.yearqtr(paste0(
    ifelse(month(ALGUS) == 12, year(ALGUS) +1, year(ALGUS)), #aasta
    "-",
    case_when(month(ALGUS) <= 2 ~ 1, #kvartal
              month(ALGUS) <= 5 ~ 2, 
              month(ALGUS) <= 8 ~ 3,
              month(ALGUS) <= 11 ~ 4,
              month(ALGUS) == 12 ~ 1
    ))),
    lopp_kv = as.yearqtr(ifelse(is.na(LOPP),
    #SIIN TULEB MUUTA LÕPPKVARTALIT!!!!!!!
                                "2021-4", #EMTA viimane kvartal + 1, sest EMTA kvartalid on nihkes. 
    #Võib juba esineda gruppe, mis EMTA andmete puhul on järgmises kvartalis. 
    #Nt grupp algas 01.03, aga EMTA andmed lõppevad 28.02.
                                paste0(
      ifelse(month(LOPP) == 12, year(LOPP) +1, year(LOPP)), #aasta
      "-",
      case_when(month(LOPP) <= 2 ~ 1, #kvartal
                month(LOPP) <= 5 ~ 2, 
                month(LOPP) <= 8 ~ 3,
                month(LOPP) <= 11 ~ 4,
                month(LOPP) == 12 ~ 1
      )))
  )) %>% 
  #kooskõlastame veerunimed teiste andmetega
  rename(registrikood = LIIGE_KOOD,
         liider_kood = KOOD
  ) %>% 
  select(registrikood, liider_kood, algus_kv, lopp_kv) 

#seq kasutamiseks peab olema kvartal numbriline väärtus
kmk_grupp3 <- kmk_grupp2 %>% 
  mutate(algus_kv = as.numeric(algus_kv), 
         lopp_kv = as.numeric(lopp_kv))

#Igat rida paljundame nii palju kui kvartaleid oli.
#Nii saame saame df_koik andmestikuga left_joinida by registrikood ja aeg
kmk_grupp3 <- do.call(rbind, 
        lapply(seq(nrow(kmk_grupp3)), function(x){
          data.frame(registrikood=kmk_grupp3[x,"registrikood"], 
                     liider_kood = kmk_grupp3[x, "liider_kood"],
                     aeg=seq(as.yearqtr(kmk_grupp3[x,"algus_kv"]), 
                             as.yearqtr(kmk_grupp3[x,"lopp_kv"]), by=0.25) ) } ))

#viskame välja kvartalid, mis olid enne EMTA andmeid
#kuna sama ema-tütar kooslust oli mitmel real, 
#ühel rea ALGUS ja teise rea LOPP langevad samasse kvartalisse, 
#siis mõned kvartalid andmetes duublis eemaldame need
kmk_grupp3 <- kmk_grupp3 %>% 
  filter(aeg >= as.yearqtr("2017-1")) %>% 
  unique()

#mõni ettevõte on ühes kvartalis mitmesse gruppi kuulunud
abi <- kmk_grupp3 %>% 
  group_by(registrikood, aeg) %>% 
  filter(n() == 2) %>% #suurem kui 1 ? Kolme korral tuleb uus loogika teha.
  #leiame selle kvartali algus- ja lõpukuupäevad
  mutate(kvartal_algus = as.Date(aeg) %m-% months(1),
         kvartal_lopp = as.Date(aeg + 0.25) %m-% months(1) - 1) %>% 
  #lisame neile ridadele algandmetest ALGUS ja LOPP kuupäevad, et arvutada kaalusid
  left_join(kmk_grupp, by = c("registrikood" = "LIIGE_KOOD", "liider_kood" = "KOOD")) %>% 
  #mõnel ettevõttel on sama ema ettevõttega mitu rida, siis jätame ainult selle rea, mis sisaldab antud kvartalit
  filter((ALGUS > kvartal_algus & ALGUS < kvartal_lopp) | (LOPP > kvartal_algus & LOPP < kvartal_lopp)) %>% 
  #antud gruppi kuulumise pikkus päevades. Kui gruppi kuulumise ALGUS on enne kvartali algust, 
  #siis gruppikuulumise LOPP - kvartali algus, muidu kvartali lõpp - kuulumise ALGUS
  mutate(pikkus = ifelse(ALGUS <= kvartal_algus, LOPP - kvartal_algus, kvartal_lopp - ALGUS)) %>% # kui on kolm gruppi, siis ei tööta!!
  #leiame igale ettevõttele antud gruppi kuulumise kaalu
  group_by(registrikood, aeg) %>% 
  mutate(kaal = round(pikkus/sum(pikkus), 3)) %>% 
  select(registrikood, liider_kood, aeg, kaal)

#lisame andmetele leitud kaalud, teised saavad kaalu 1
kmk_grupp4 <- kmk_grupp3 %>% 
  left_join(abi) %>% 
  mutate(kaal = ifelse(is.na(kaal), 1, kaal))

save(kmk_grupp4, file = "Andmed/R_andmed/kmk_grupp.RData")

# #Meetmete analüüs
# load("Andmed/R_andmed/df_koik.RData")
# kmk_grupp_uni <- kmk_grupp4 %>% pull(registrikood) %>% unique()
# kmk_abi <- kmk_grupp4 %>% select(-aeg, -kaal) %>% unique()
# 
# kmk_grupp_meetmed <- df_koik %>% 
#   select(registrikood, meede_EAS, meede_TK, meede_kredex, meede_MES) %>% 
#   unique() %>% 
#   right_join(kmk_abi) %>% 
#   mutate(liider = ifelse(registrikood == liider_kood, 1, 0)) %>% 
#   filter(!(is.na(meede_EAS) & is.na(meede_TK) & is.na(meede_kredex) & is.na(meede_MES)))
#          