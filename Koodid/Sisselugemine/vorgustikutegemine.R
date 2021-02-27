
#Need andmed on Anna rakenduses valmis tehtud andmed, kus olid olemas ka osad seosed ettevõtete vahel. Kasutan seda näidisena
#load("C:/Users/avork/OneDrive - Tartu Ülikool/CITIS/EMTA/Vorgustiku_analyys/positiivne_m6ju_input.RData")
#save(graph_riik_aasta_2016, file = "C:/Users/avork/OneDrive - Tartu Ülikool/CITIS/EMTA/Vorgustiku_analyys/graph_riik_aasta_2016.RData")
#save(emtak_dt, file = "C:/Users/avork/OneDrive - Tartu Ülikool/CITIS/EMTA/Vorgustiku_analyys/emtak_dt.RData")

#Edaspidi peab tegema valmis ESA serveris ja sealt paluma välja kirjutada

load("C:/Users/avork/OneDrive - Tartu Ülikool/CITIS/EMTA/Vorgustiku_analyys/graph_riik_aasta_2016.RData")
load("C:/Users/avork/OneDrive - Tartu Ülikool/CITIS/EMTA/Vorgustiku_analyys/emtak_dt.RData")

temp<- graph_riik_aasta_2016 %>% 
  select(Myyja_AID2, Ostja_AID2, TEHINGUSUM_KMta)
#seome juurde emtak2
temp <- temp %>% left_join(emtak_dt, by=c("Myyja_AID2"="ART_ID")) %>% 
  rename(Maakond_n_myyja = Maakond_n,
         Sektor_myyja = SEKTOR,
         Emtak2_myyja = EMTAK2_n) %>% 
  left_join(emtak_dt, by=c("Ostja_AID2"="ART_ID")) %>% 
  rename(Maakond_n_ostja = Maakond_n,
         Sektor_ostja = SEKTOR,
         Emtak2_ostja = EMTAK2_n)

#EMTAK2 järgi igas sektoris keskmine partnerite arv
temp_ostjaid <- temp %>%  
  group_by(Myyja_AID2) %>% 
  summarise(ostjaid = n_distinct(Ostja_AID2),
            emtak2= first(Emtak2_myyja)) %>% 
  group_by(emtak2) %>% 
  summarise(haru_ostjaid = mean(ostjaid, na.rm = TRUE))

temp_myyjaid <- temp %>%  
  group_by(Ostja_AID2) %>% 
  summarise(myyjaid = n_distinct(Myyja_AID2),
            emtak2= first(Emtak2_ostja)) %>% 
  group_by(emtak2) %>% 
  summarise(haru_myyjaid = mean(myyjaid, na.rm = TRUE))

ostjadmyyjad <- full_join(temp_myyjaid, temp_ostjaid)
ostjadmyyjad <- ostjadmyyjad %>% 
  mutate(emtak2 = ifelse(nchar(as.character(emtak2))==1, paste0("0", as.character(emtak2)), as.character(emtak2)))

save(ostjadmyyjad, file = "./Andmed/R_andmed/ostjadmyyjad.RData")

rm(emtak_dt, graph_riik_aasta_2016, temp, temp_myyjaid, temp_ostjaid)

