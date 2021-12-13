#Meetmete lisamine

#Paketid
require(tidyverse)
require(zoo)

#Võtame aluseks andmete_koondamine_ja_uuedtunnused.R failist saadud andmed df_anal
load(file = "./Andmed/R_andmed/df_anal.RData")
#Meetmed
load("./Andmed/R_andmed/andmed_eas.RData") 
load("./Andmed/R_andmed/andmed_mes.RData")
load("./Andmed/R_andmed/andmed_emta.RData") #MES andmete registrikoodi jaoks
load("./Andmed/R_andmed/andmed_tkkum.RData") #Töötukassa 2020
load("./Andmed/R_andmed/andmed_tk21.RData") #Töötukassa 2021
load("./Andmed/R_andmed/andmed_tk21_hi.RData") #Harju ja Ida-Viru eri
load("./Andmed/R_andmed/andmed_kredex.RData")

#Lisame MES andmetele registrikoodi nime kaudu kui registrikood on puudu
abi <- andmed_emta %>% select(nimi, registrikood) %>% unique()

abi <- abi %>% mutate(nimi = trimws(gsub(",", "", nimi)))
andmed_mes2 <- andmed_mes %>% 
  filter(is.na(registrikood)) %>% 
  select(-registrikood) %>% 
  mutate(nimi = trimws(gsub("\\((.*?)\\)", "",toupper(nimi)))) %>% 
  left_join(abi, by ="nimi") %>% 
  select(names(andmed_mes))

abi <- abi %>% 
  mutate(nimi = trimws(gsub("&", "JA",gsub("^AKTSIASELTS|^SIHTASUTUS|^INSENERI- JA TEHNIKAÜHISTU|^AS|^OÜ|OÜ$|AS$|FIE$|^OSAÜHING|AKTSIASELTS$|TÜ$|SA$|TÜH$|OSAÜHING$", "", nimi))))
andmed_mes1 <- andmed_mes2 %>% 
  filter(is.na(registrikood)) %>% 
  select(nimi, mes_laenusumma, mes_kaendussumma) %>% 
  mutate(nimi = trimws(gsub("^AKTSIASELTS|^TÄISÜHING|INSENERI- JA TEHNIKAÜHISTU$|^AS|^OÜ|OÜ$|AS$|FIE$|^OSAÜHING|AKTSIASELTS$|TÜ$|SA$|OSAÜHING$", "", nimi))) %>% 
  left_join(abi, by = "nimi") %>% 
  select(names(andmed_mes))
#Kui mõni ettevõtte jääb ikka registrikoodiga, siis võib uurida, kas mõne sõna lisamine gsubi aitaks
#Kui sõna on alguses, siis ^sõna; kui sõna on lõpus siis sõna$

andmed_mes <- andmed_mes %>% 
  filter(!is.na(registrikood)) %>% 
  rbind(andmed_mes1) %>% 
  rbind(andmed_mes2) %>% 
  #select(registrikood) %>% 
  filter(!is.na(registrikood)) %>% 
  unique()
  

#Teeme igale meetmele binaarse tunnuse ja lisame EMTA andmetele
df_koik <- df_anal %>% 
  left_join(andmed_eas %>% mutate(meede_EAS = 1) %>% select(registrikood, meede_EAS) %>% unique(),
            by = "registrikood") %>%
  left_join(andmed_tkkum %>% mutate(meede_TK20 = 1, registrikood = as.character(registrikood)) %>% select(registrikood, meede_TK20),
            by = "registrikood") %>%
  left_join(andmed_kredex %>% mutate(meede_kredex_kaendus = ifelse(!is.na(kredex_kaendussumma),1,0), 
                                     meede_kredex_laen = ifelse(is.na(kredex_kaendussumma),1,0),
                                     registrikood = as.character(registrikood)) %>% 
              select(registrikood, meede_kredex_kaendus, meede_kredex_laen) %>% unique(),
            by = "registrikood") %>% 
  left_join(andmed_mes %>% mutate(meede_MES_kaendus = ifelse(!is.na(mes_kaendussumma),1, 0),
                                  meede_MES_laen = ifelse(is.na(mes_kaendussumma),1, 0),) %>% 
              select(registrikood, meede_MES_kaendus,meede_MES_laen),
          by = "registrikood") %>% 
  left_join(andmed_tk21 %>% mutate(meede_TK21 = 1, registrikood = as.character(registrikood)) %>% select(registrikood, meede_TK21),
            by = "registrikood") %>%
  left_join(andmed_tk21_hi %>% mutate(meede_TK21_hi = 1, registrikood = as.character(registrikood)) %>% select(registrikood, meede_TK21_hi),
            by = "registrikood") %>%
  #kui ei saanud meedet, siis on väärtus 0
  replace_na(list(meede_EAS = 0, meede_TK20 = 0, meede_kredex_kaendus = 0, meede_kredex_laen = 0, meede_MES_kaendus = 0, meede_MES_laen = 0,
                  meede_TK21 = 0, meede_TK21_hi = 0)) %>% 
  unique()

save(df_koik, file = "./Andmed/R_andmed/df_koik.RData")

