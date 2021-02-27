#Meetmete lisamine

#Paketid
library(tidyverse)
library(zoo)

#Võtame aluseks andmete_koondamine_ja_uuedtunnused.R failist saadud andmed df_anal
load(file = "./Andmed/R_andmed/df_anal.RData")
#Meetmed
load("./Andmed/R_andmed/andmed_eas.RData") 
load("./Andmed/R_andmed/andmed_mes.RData")
load("./Andmed/R_andmed/andmed_emta.RData") #MES andmete registrikoodi jaoks
load("./Andmed/R_andmed/andmed_tkkum.RData")
load("./Andmed/R_andmed/andmed_kredex.RData")

#Lisame MES andmetele registrikoodi nime kaudu
abi <- andmed_emta %>% select(nimi, registrikood) %>% unique()

abi <- abi %>% mutate(nimi = trimws(gsub(",", "", nimi)))
andmed_mes <- andmed_mes %>% mutate(nimi = trimws(gsub("\\((.*?)\\)", "",toupper(nimi)))) %>% left_join(abi, by ="nimi")

abi <- abi %>% mutate(nimi = trimws(gsub("&", "JA",gsub("^AKTSIASELTS|^SIHTASUTUS|^INSENERI- JA TEHNIKAÜHISTU|^AS|^OÜ|OÜ$|AS$|FIE$|^OSAÜHING|AKTSIASELTS$|TÜ$|SA$|TÜH$", "", nimi))))
andmed_mes1 <- andmed_mes %>% filter(is.na(registrikood)) %>% select(nimi, mes_laenusumma, mes_kaendussumma) %>% 
  mutate(nimi = trimws(gsub("^TÄISÜHING|INSENERI- JA TEHNIKAÜHISTU$|^AS|^OÜ|OÜ$|AS$|FIE$|^OSAÜHING|AKTSIASELTS$|TÜ$|SA$", "", nimi))) %>% left_join(abi, by = "nimi")

andmed_mes <- andmed_mes %>% filter(!is.na(registrikood)) %>% rbind(andmed_mes1) %>% 
  select(registrikood) %>% filter(!is.na(registrikood)) %>% unique()
  

#Teeme igale meetmele binaarse tunnuse ja lisame EMTA andmetele
df_koik <- df_anal %>% 
  left_join(andmed_eas %>% mutate(meede_EAS = 1) %>% select(registrikood, meede_EAS) %>% unique(),
            by = "registrikood") %>%
  left_join(andmed_tkkum %>% mutate(meede_TK = 1, registrikood = as.character(registrikood)) %>% select(registrikood, meede_TK),
            by = "registrikood") %>%
  left_join(andmed_kredex %>% mutate(meede_kredex = 1, registrikood = as.character(registrikood)) %>% select(registrikood, meede_kredex) %>% unique(),
            by = "registrikood") %>% 
  left_join(andmed_mes %>% mutate(meede_MES = 1) %>% select(registrikood, meede_MES),
          by = "registrikood") %>% 
  #kui ei saanud meedet, siis on väärtus 0
  replace_na(list(meede_EAS = 0, meede_TK = 0, meede_kredex = 0, meede_MES = 0)) %>% 
  unique()

save(df_koik, file = "./Andmed/R_andmed/df_koik.RData")

