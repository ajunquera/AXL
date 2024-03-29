#...............................................................................
# ASSEGNO PER IL LAVORO
# Author: Álvaro F. Junquera (UAB)
# Date: 31/01/2024
#...............................................................................

library(tidyverse)
library(data.table)
library(tidytable)
library(lubridate)
library(descr)
library(janitor)

library(readxl)
library(openxlsx)

library(rdrobust)
library(rd.categorical)
library(rddensity)
library(binsreg)

library(modelsummary)

if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Intermediate_outputs")) {
  setwd("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Intermediate_outputs")
} else{
  setwd("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/3_mix_Veneto/Intermediate_outputs")
}

# 0. Some functions -------
"%out%" <- Negate("%in%")

checknas <- function(data_frame) {
  na_counts <- sapply(data_frame, function(x) sum(is.na(x)))
  return(na_counts)
}

# 1. Reading data ---------
## First delivery (nov. 2022)
if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl.xlsx")) {
  data <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl.xlsx",
             sheet = "Dati")
} else{
  data <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl.xlsx",
             sheet = "Dati")
}

## Second delivery (nov. 2022)
if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/indice profiling continuo.xlsx")) {
  profiling <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/indice profiling continuo.xlsx")
} else{
  profiling <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/indice profiling continuo.xlsx")
}

## Third delivery (dec. 2022)
if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/axl_unipd_post_max.xlsx")) {
  longest <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/axl_unipd_post_max.xlsx")
} else{
  longest <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/axl_unipd_post_max.xlsx")
}

## Fourth delivery (jan. 2023)
if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/provcomuniVEN.xlsx")) {
  provcomuni <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/provcomuniVEN.xlsx")
} else{
  provcomuni <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/3_mix_Veneto/provcomuniVEN.xlsx")
}

if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/Datos/Classifications/Ocupaciones/Italy/raccordo_Isco08_CP2011.xls")) {
  ISCOCPitalia <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/Datos/Classifications/Ocupaciones/Italy/raccordo_Isco08_CP2011.xls",
                             sheet = "Foglio1", col_names = T)
} else{
  ISCOCPitalia <- read_excel("C:/Users/1604834/OneDrive - UAB/Datos/Classifications/Ocupaciones/Italy/raccordo_Isco08_CP2011.xls",
                           sheet = "Foglio1", col_names = T)
}

if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl estrazione 012023.xlsx")) {
  formadomi <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl estrazione 012023.xlsx",
                             sheet = "Foglio1", col_names = T)
} else{
  formadomi <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl estrazione 012023.xlsx",
                             sheet = "Foglio1", col_names = T)
}

if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl estrazione 022023.xlsx")) {
  disab <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl estrazione 022023.xlsx")
} else{
  disab <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl estrazione 022023.xlsx")
}

if(file.exists("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl.xlsx")) {
  data_notp <- read_excel("C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl.xlsx",
                          sheet = "Dati")
} else{
  data_notp <- read_excel("C:/Users/1604834/OneDrive - UAB/PhD thesis/00A_thesis/3_mix_Veneto/Data_xxx/unipd_axl.xlsx",
                          sheet = "Dati")
}



# 2. Data cleaning ---------

## Removing wrong cases (individuals with measurement error) ---------------------------
## Wrong case = (a) discrepancy btw reported & estimated, or (b) discrepancy btw treatment level in data and treatment level in profiling

### Wrong according to (a)

# Variable "indice" was registered at the moment the individual claimed the AXL.
# Variable "indice_previsto" was estimated at November 2022, possibly with changes at the values of the ingredients of the score function

profiling$indice_num <- as.numeric(profiling$indice)
profiling$indice_num3 <- round_half_up(profiling$indice_num, digits = 2) # implemented score

profiling$roundhu_3 <- round_half_up(profiling$indice_previsto, digits = 2)

profiling$dif_ass_prev_hu3 <- ifelse(profiling$indice_num3 != profiling$roundhu_3, TRUE, FALSE) # This marks the wrong cases!

## Joining *data* and *profiling*
data <- inner_join(data, profiling, by = c("ide_soggetto", "axl_data_attribuzione"))

data <- data %>% filter(dif_ass_prev_hu3 == FALSE)

### Wrong according to (b)
data$checking_join <- ifelse(data$axl_profil_fascia == data$intensita, TRUE, FALSE)
freq(data$checking_join) # There are 5 problematic cases!
data <- data %>% filter(checking_join == TRUE) # There are only 5 problematic cases (they are REMOVED)

## Declaring proper classes --------------
## Declaring factor variables as factor
data$axl_fascia <- factor(data$axl_profil_fascia, levels = c("BASSA", "MEDIA", "ALTA"))

## Declaring numeric variables as numeric
class(data$indice_previsto) # estimated score
class(data$axl_profil_punteggio) # reported score


#data %>% filter(axl_profil_fascia == "BASSA" & ppa_attivata == "attivata") %>% tabyl(indice_previsto) # group A Rounded with 2 digits:  (0.60 <= s) // Rounded with 3 digits: (0.595 <= s) 
#data %>% filter(axl_profil_fascia == "BASSA" & ppa_attivata == "attivata") %>% tabyl(flg_da_eliminare) 
#imedia <- data %>% filter(axl_profil_fascia == "MEDIA" & ppa_attivata == "attivata") %>% tabyl(indice_previsto) # group B  Rounded with 2 digits: (0.40 <= s < 0.60) // Rounded with 3 digits: (0.395 <= s < 0.595)
#data %>% filter(axl_profil_fascia == "MEDIA" & ppa_attivata == "attivata") %>% tabyl(flg_da_eliminare) 
#ialta <- data %>% filter(axl_profil_fascia == "ALTA" & ppa_attivata == "attivata") %>% tabyl(indice_previsto) # group C Rounded with 2 digits: (s < 0.40) // Rounded with 3 digits: (s < 0.395)
#data %>% filter(axl_profil_fascia == "ALTA" & ppa_attivata == "attivata") %>% tabyl(flg_da_eliminare) 

## Declaring date variables as dates
data$axl_data_attribuzione_d <- ymd(data$axl_data_attribuzione)
data$axl_year_attribuzione <- year(data$axl_data_attribuzione_d)

data$ppa_data_avvio_d <- ymd(data$ppa_data_avvio) # Fecha inicio acciones # 5919 failed. 5457 due to non activation of the PPA.
freq(data$ppa_attivata)

data$data_fine_ppa_d <- ymd(data$data_fine_ppa)
data$data_fine_assegno_d <- ymd(data$data_fine_assegno)

data$axl_duration <- data$data_fine_assegno_d - data$ppa_data_avvio_d
data$axl_duration_actions <- ifelse(is.na(data$data_fine_ppa_d), NA,
                                    data$data_fine_ppa_d - data$ppa_data_avvio_d)


## Scoring variable in the usual direction ----------
# In the program design, the scoring variable is defined with 3 fractional digits. (A if 0.590 <= s <= 1.000) (B if 0.396 <= s <= 0.589) (C if 0.000 <= s <= 0.395)
# Program implementation was executed with a scoring variable with 2 fractional digits.

### Discrete scoring variable ----------
data_notp$indice_num <- as.numeric(data_notp$axl_profil_punteggio)
data_notp$reversed_indice <- 1 - data_notp$indice_num

data_notp$group <- car::recode(data_notp$axl_profil_fascia, "'BASSA' = 'A'; 'MEDIA' = 'B'; 'ALTA' = 'C'",
                               levels = c("A", "B", "C"))

data_notp %>%
  ggplot(aes(x = reversed_indice, y = group)) +
  geom_point() +
  theme_light() +
  geom_vline(xintercept = 0.41, linewidth = 0.5) +
  geom_vline(xintercept = 0.61, linewidth = 0.5) +
  labs(x = "Scoring variable (S)", y = "Treatment group") # Figure A1 (saved 395 x 300)

rm(data_notp)

### Continuous scoring variable ----------
#freq(data$indice_previsto) # Prob[SÍ estar empleado en un plazo de 24 meses desde el registro]. 
data$axl_scoring <- 1 - data$indice_previsto
#freq(data$axl_scoring) # Prob[NO estar empleado en un plazo de 24 meses desde el registro]

# group A: Rounded with 3 digits (s < 0.405)
iA <- data %>% filter(axl_profil_fascia == "BASSA" & ppa_attivata == "attivata") %>% tabyl(axl_scoring)

# group B: Rounded with 2 digits (s >= 0.41  &  s < 0.61) // Rounded with 3 digits: (s >= 0.405 & s < 0.605)
iB <- data %>% filter(axl_profil_fascia == "MEDIA" & ppa_attivata == "attivata") %>% tabyl(axl_scoring)

# group C: Rounded with 2 digits (s >= 0.61) // Rounded with 3 digits (s >= 0.605)
iC <- data %>% filter(axl_profil_fascia == "ALTA" & ppa_attivata == "attivata") %>% tabyl(axl_scoring)

# And centered at cero...
data$scoringD1_0 <- data$axl_scoring - 0.405 # para el tratamiento 1 (de A a B)
data$scoringD2_0 <- data$axl_scoring - 0.605 # para el tratamiento 2 (de B a C)
  
## Outcome variables ------------
### Outcomes with interval scale
data$post_interval6 <- data$m1_gg_lav + data$m2_gg_lav + data$m3_gg_lav + data$m4_gg_lav + data$m5_gg_lav + data$m6_gg_lav

data$post_interval712 <- data$m7_gg_lav + data$m8_gg_lav + data$m9_gg_lav + data$m10_gg_lav + data$m11_gg_lav + data$m12_gg_lav

data$post_interval12 <- data$m1_gg_lav + data$m2_gg_lav + data$m3_gg_lav + data$m4_gg_lav + data$m5_gg_lav + data$m6_gg_lav +
  data$m7_gg_lav + data$m8_gg_lav + data$m9_gg_lav + data$m10_gg_lav + data$m11_gg_lav + data$m12_gg_lav

data$post_interval1318 <- data$m13_gg_lav + data$m14_gg_lav + data$m15_gg_lav + data$m16_gg_lav + data$m17_gg_lav + data$m18_gg_lav

data$post_interval18 <- data$m1_gg_lav + data$m2_gg_lav + data$m3_gg_lav + data$m4_gg_lav + data$m5_gg_lav + data$m6_gg_lav +
  data$m7_gg_lav + data$m8_gg_lav + data$m9_gg_lav + data$m10_gg_lav + data$m11_gg_lav + data$m12_gg_lav +
  data$m13_gg_lav + data$m14_gg_lav + data$m15_gg_lav + data$m16_gg_lav + data$m17_gg_lav + data$m18_gg_lav

data$post_interval24 <- data$m1_gg_lav + data$m2_gg_lav + data$m3_gg_lav + data$m4_gg_lav + data$m5_gg_lav + data$m6_gg_lav +
  data$m7_gg_lav + data$m8_gg_lav + data$m9_gg_lav + data$m10_gg_lav + data$m11_gg_lav + data$m12_gg_lav +
  data$m13_gg_lav + data$m14_gg_lav + data$m15_gg_lav + data$m16_gg_lav + data$m17_gg_lav + data$m18_gg_lav +
  data$m19_gg_lav + data$m20_gg_lav + data$m21_gg_lav + data$m22_gg_lav + data$m23_gg_lav + data$m24_gg_lav

data$post_interval1924 <- data$m19_gg_lav + data$m20_gg_lav + data$m21_gg_lav + data$m22_gg_lav + data$m23_gg_lav + data$m24_gg_lav

data$jshours <- data$attiv_indiv_ore_eff


### Outcomes with nominal scale
data$sil_rl_data_inizio_d <- ymd(data$sil_rl_data_inizio)

data$axl_data_attribuzione_d_plus6 <- data$axl_data_attribuzione_d %m+% months(6)
data$axl_data_attribuzione_d_plus3 <- data$axl_data_attribuzione_d %m+% months(3)
data$axl_data_attribuzione_d_plus5 <- data$axl_data_attribuzione_d %m+% months(5)

data$interval1_6 <- interval(start = data$axl_data_attribuzione_d,
                             end = data$axl_data_attribuzione_d_plus6)
data$interval1_3 <- interval(start = data$axl_data_attribuzione_d,
                             end = data$axl_data_attribuzione_d_plus3)
data$interval1_5 <- interval(start = data$axl_data_attribuzione_d,
                             end = data$axl_data_attribuzione_d_plus5)

data$OEC_interval1_3 <- ifelse(data$sil_rl_contratto == "a- Cti" & data$sil_rl_data_inizio_d %within% data$interval1_3,
                               "Si", "No")
data$FTC612_interval1_3 <- ifelse(data$sil_rl_contratto %in% c("b- Cap", "c- Ctd", "d- Som") &
                                    data$sill_rl_durata_prev == "a. 6-12 mesi" &
                                    data$sil_rl_data_inizio_d %within% data$interval1_3,
                                  "Si", "No")
data$FTCm12_interval1_3 <- ifelse(data$sil_rl_contratto %in% c("b- Cap", "c- Ctd", "d- Som") &
                                    data$sill_rl_durata_prev == "b. >12 mesi" &
                                    data$sil_rl_data_inizio_d %within% data$interval1_3,
                                  "Si", "No")

data$OEC_interval1_5 <- ifelse(data$sil_rl_contratto == "a- Cti" & data$sil_rl_data_inizio_d %within% data$interval1_5,
                               "Si", "No")
data$FTC612_interval1_5 <- ifelse(data$sil_rl_contratto %in% c("b- Cap", "c- Ctd", "d- Som") &
                                    data$sill_rl_durata_prev == "a. 6-12 mesi" &
                                    data$sil_rl_data_inizio_d %within% data$interval1_5,
                                  "Si", "No")
data$FTCm12_interval1_5 <- ifelse(data$sil_rl_contratto %in% c("b- Cap", "c- Ctd", "d- Som") &
                                    data$sill_rl_durata_prev == "b. >12 mesi" &
                                    data$sil_rl_data_inizio_d %within% data$interval1_5,
                                  "Si", "No")

data$OEC_interval1_6 <- ifelse(data$sil_rl_contratto == "a- Cti" & data$sil_rl_data_inizio_d %within% data$interval1_6,
                               "Si", "No")
data$FTC612_interval1_6 <- ifelse(data$sil_rl_contratto %in% c("b- Cap", "c- Ctd", "d- Som") &
                                    data$sill_rl_durata_prev == "a. 6-12 mesi" &
                                    data$sil_rl_data_inizio_d %within% data$interval1_6,
                               "Si", "No")
data$FTCm12_interval1_6 <- ifelse(data$sil_rl_contratto %in% c("b- Cap", "c- Ctd", "d- Som") &
                                    data$sill_rl_durata_prev == "b. >12 mesi" &
                                    data$sil_rl_data_inizio_d %within% data$interval1_6,
                                  "Si", "No")

data$axl_data_attribuzione_d_plus12 <- data$axl_data_attribuzione_d_plus6 %m+% months(6)
data$interval7_12 <- interval(start = data$axl_data_attribuzione_d_plus6,
                             end = data$axl_data_attribuzione_d_plus12)
data$OEC_interval7_12 <- ifelse(data$sil_rl_contratto == "a- Cti" & data$sil_rl_data_inizio_d %within% data$interval7_12,
                               "Si", "No")
data$FTC612_interval7_12 <- ifelse(data$sil_rl_contratto %in% c("b- Cap", "c- Ctd", "d- Som") &
                                    data$sill_rl_durata_prev == "a. 6-12 mesi" &
                                    data$sil_rl_data_inizio_d %within% data$interval7_12,
                                  "Si", "No")
data$FTCm12_interval7_12 <- ifelse(data$sil_rl_contratto %in% c("b- Cap", "c- Ctd", "d- Som") &
                                    data$sill_rl_durata_prev == "b. >12 mesi" &
                                    data$sil_rl_data_inizio_d %within% data$interval7_12,
                                  "Si", "No")



## Costs (for the training cost, we only need to consider the cost of the first AXL, so we need to use the dataset at individual level)
data$ente_ris_occ_importo_r <- car::recode(data$ente_ris_occ_importo, '"no risultato occupaz" = "0"',
                                              as.factor = F)

data$spent_incentive <- as.numeric(data$ente_ris_occ_importo_r)
data$spent_js <- data$jshours * 27
data$spent_incentivejs <- data$spent_incentive + data$spent_js

#data$sil_rl_data_inizio_d <- ymd(data$sil_rl_data_inizio)
#freq(data$sil_rl_data_inizio_d)
#summary(data$sil_rl_data_inizio_d)

#data$duringwd <- data$data_fine_assegno_d - data$sil_rl_data_inizio_d 
#freq(data$duringwd)
#Hmisc::describe(data$duringwd)

#data$duringwd0 <- ifelse(is.na(data$duringwd), 0, data$duringwd)

data$axlduration <- as.numeric(data$axl_duration)
data$axldurationactions <- as.numeric(data$axl_duration_actions)
#freq(data$axlduration)

#data$duringwd0stan <- data$duringwd0 / data$axlduration
#freq(data$duringwd0stan)

#### Categorical outcomes ----------
class(longest$post_oss_max_durata)
longest$post_oss_max_durata_n <- as.numeric(longest$post_oss_max_durata)

class(longest$post_no_oss_max_durata)
longest$post_no_oss_max_durata_n <- as.numeric(longest$post_no_oss_max_durata)

longest <- longest %>% mutate(longestcpost = case_when(post_oss_max_durata_n > post_no_oss_max_durata_n ~ post_oss_max_contratto2_ass,
                                                       post_oss_max_durata_n < post_no_oss_max_durata_n ~ post_no_oss_max_contratto2_ass,
                                                       post_oss_max_durata_n == post_no_oss_max_durata_n ~ post_oss_max_contratto2_ass,
                                                       TRUE ~ post_no_oss_max_contratto2_ass))
freq(longest$longestcpost)
sum(is.na(longest$post_oss_max_contratto2_ass) & is.na(longest$post_no_oss_max_contratto2_ass)) # The number of NAs is ok

longest <- longest %>% mutate(longestcontract = case_when(longestcpost %in% c("b- Cap", "f- Dom", "g- Par") ~ "AC",
                                                          longestcpost == "a- Cti"  ~ "OEC",
                                                          longestcpost == "c- Ctd" ~ "FTC",
                                                          longestcpost == "d- Som" ~ "SC",
                                                          longestcpost == "e- Int" ~ "DC",
                                                          is.na(longestcpost) ~ "NE"))
freq(longest$longestcontract)

longest <- longest %>% mutate(durata_longestcpost = case_when(post_oss_max_durata_n > post_no_oss_max_durata_n ~ post_oss_max_durata_n,
                                                       post_oss_max_durata_n < post_no_oss_max_durata_n ~ post_no_oss_max_durata_n,
                                                       post_oss_max_durata_n == post_no_oss_max_durata_n ~ post_oss_max_durata_n,
                                                       TRUE ~ post_no_oss_max_durata_n))

longest$durata_longestcpost_ld <- days(longest$durata_longestcpost)
longest$durata_longestcpost_ld_months <- longest$durata_longestcpost_ld %/% months(1)

# longestcontract_ris registers the longest contract signed in the [+1, +24] time interval after *starting* the treatment
longest <- longest %>% mutate(longestcontract_ris_wrong = case_when(durata_longestcpost_ld == "10000d 0H 0M 0S" ~ "OEC", # this includes both a-Cti and d-Som with open-ended duration
                                                              durata_longestcpost_ld_months > 12 ~ "FTCm12",
                                                              durata_longestcpost_ld_months > 5 & durata_longestcpost_ld_months < 13 ~ "FTC612",
                                                              durata_longestcpost_ld_months < 6 & durata_longestcpost_ld > 0 ~ "FTCme6",
                                                              durata_longestcpost_ld_months == 0 ~ "NE"),
                              longestcontract_ris = case_when(durata_longestcpost_ld == "10000d 0H 0M 0S" ~ "OEC", # this includes both a-Cti and d-Som with open-ended duration
                                                              durata_longestcpost_ld_months > 12 & longestcpost %in% c("b- Cap", "c- Ctd", "d- Som") ~ "FTCm12", # this limits FTC contracts for these 3 types of contract
                                                              durata_longestcpost_ld_months > 5 & durata_longestcpost_ld_months < 13 & longestcpost %in% c("b- Cap", "c- Ctd", "d- Som") ~ "FTC612",
                                                              durata_longestcpost_ld_months < 6 & durata_longestcpost_ld > 0 & longestcpost %in% c("b- Cap", "c- Ctd", "d- Som") ~ "FTCme6",
                                                              TRUE ~ "Not_employed_or_other_contract"))

## Cleaning variables on disability and occupations ---------
disab$ide_data <- paste0(disab$ide_soggetto, "_", disab$axl_dat_attribuzione)

## Occupations
colnames(ISCOCPitalia) <- c("ISCO08_code", "ISCO08_name", "CP2011_code", "CP2011_name")
ISCOCPitalia <- ISCOCPitalia[-c(1), ]
ISCOCPitalia$ISCO08_1digit <- str_sub(ISCOCPitalia$ISCO08_code, 1, 1)
ISCOCPitalia$CP2011_1digit <- str_sub(ISCOCPitalia$CP2011_code, 1, 1)



## Dataset at individual level ------------

### Removing non activated PPA
freq(data$ppa_attivata)
axlstarted <- data %>% filter(ppa_attivata == "attivata")

### Without removing those individuals treated more than once (but considering only its first treatment)
indi <- axlstarted %>% group_by(ide_soggetto) %>%
  filter(axl_data_attribuzione_d == min(axl_data_attribuzione_d))


### Removing suspicious cases --------------
## 1. Suspicious A = actual hours of treatment > maximum hours treatment by design
indi <- indi %>% mutate(suspicious = case_when(axl_fascia == "BASSA" & attiv_indiv_ore_eff > 7 ~ "Yes",
                                               axl_fascia == "MEDIA" & attiv_indiv_ore_eff > 13 ~ "Yes",
                                               axl_fascia == "ALTA" & attiv_indiv_ore_eff > 27 ~ "Yes",
                                               TRUE ~ "No")) # The number is the maximum hours of job search measures + 1 (according to the design)
freq(indi$suspicious)

## 2. Suspicious B = AXL duration < 0
indi <- indi %>% mutate(negativeaxlduration = case_when(axlduration < 0 ~ "Yes",
                                                        axlduration == 0 ~ "No",
                                                        TRUE ~ "No"))
freq(indi$negativeaxlduration)

#problematicos_axl_levl <- data %>% filter(ide_soggetto %in% problematicos$ide_soggetto) # 21 observations vs. 19 individuals. Only 2 individuals have been treated more than once: 3124300 and 3451008 (this with AXL "retired" the second time)

summary <- indi %>% filter(suspicious == "No" | negativeaxlduration == "No") %>% group_by(axl_fascia) %>%
  summarise(jobsearch_h_m = mean(attiv_indiv_ore_eff),
            jobsearch_h_sd = sd(attiv_indiv_ore_eff),
            training_h_m = mean(attiv_form_ore_prev),
            training_h_sd = sd(attiv_form_ore_prev),
            jobsearch_med = median(attiv_indiv_ore_eff),
            training_med = median(attiv_form_ore_prev),
            employed_during = 1 - mean(ente_ris_occ_contratto == "no risultato occupaz"))

summary <- adorn_rounding(summary, digits = 2) 

#openxlsx::write.xlsx(summary, 'C:/Users/alvar/UAB/OneDrive - Universitat Autònoma de Barcelona/PhD thesis/00A_thesis/3_mix_Veneto/Intermediate_outputs/summary_actualtr2201.xlsx')

# 3. Filtering
indi_ns <- indi %>% filter(suspicious == "No") %>% filter(negativeaxlduration == "No")
nrow(indi) - nrow(indi_ns) # 39 cases were removed

indi_ns$ide_data <- paste0(indi_ns$ide_soggetto, "_", indi_ns$axl_data_attribuzione)

### Pretreatment covariates ---------------
### Maximum educational attainment
indi_ns <- indi_ns %>% mutate(studio2_grouped = case_when(studio2 == "Licenza elementare" ~ "1_primary",
                                                                  studio2 == "Licenza media" ~ "2_lowersec",
                                                                  studio2 == "Diploma di qualifica professionale" ~ "353_uppersecP_3y",
                                                                  studio2 == "Diploma Tecnico" ~ "353_uppersecP_4y_tec",
                                                                  studio2 == "Diploma Professionale" ~ "353_uppersecP_4y_prof",
                                                                  studio2 == "Diploma Liceale" ~ "344_uppersecG",
                                                                  studio2 == "Laurea I livello (triennale)" | studio3 == "Diploma universitario" ~ "660_terBach_lev1", # Art. 17 legge 30 dicembre 2010, n. 240.
                                                                  studio2 %in% c("Laurea - vecchio o nuovo ordinamento", "Post laurea") ~ "760+_terMastery+_lev2",
                                                                  studio3 %in% c("Altri titoli di istruzione superiore", "Diploma Conservatorio musicale",
                                                                                 "Maestro d'arte", "Scuola magistrale (triennale)", "Diploma di istruzione artistica", "Diploma interprete, traduttore, archivista") ~ "6or7_artshhss",
                                                                  TRUE ~ "Nondetermined")) # the first number is the ISCED-A

indi_ns$studio2_grouped2 <- car::recode(indi_ns$studio2_grouped,
                                            'c("660_terBach_lev1", "6or7_artshhss") = "660_terBachy+_lev1"; "Nondetermined" = NA',
                                            as.factor = T)

### Lagged outcome variables (number of working days)
indi_ns$prewd1_6 <- indi_ns$m_meno1_gg_lav + indi_ns$m_meno2_gg_lav + indi_ns$m_meno3_gg_lav + indi_ns$m_meno4_gg_lav +
  indi_ns$m_meno5_gg_lav + indi_ns$m_meno6_gg_lav

indi_ns$prewd7_12 <- indi_ns$m_meno7_gg_lav + indi_ns$m_meno8_gg_lav + indi_ns$m_meno9_gg_lav +
  indi_ns$m_meno10_gg_lav + indi_ns$m_meno11_gg_lav + indi_ns$m_meno12_gg_lav

indi_ns$prewd13_18 <- indi_ns$m_meno13_gg_lav + indi_ns$m_meno14_gg_lav + indi_ns$m_meno15_gg_lav + indi_ns$m_meno16_gg_lav +
  indi_ns$m_meno17_gg_lav + indi_ns$m_meno18_gg_lav

indi_ns$prewd19_24 <- indi_ns$m_meno19_gg_lav + indi_ns$m_meno20_gg_lav + indi_ns$m_meno21_gg_lav +
  indi_ns$m_meno22_gg_lav + indi_ns$m_meno23_gg_lav + indi_ns$m_meno24_gg_lav

indi_ns$prewd1_12 <- indi_ns$m_meno1_gg_lav + indi_ns$m_meno2_gg_lav + indi_ns$m_meno3_gg_lav + indi_ns$m_meno4_gg_lav +
  indi_ns$m_meno5_gg_lav + indi_ns$m_meno6_gg_lav + indi_ns$m_meno7_gg_lav + indi_ns$m_meno8_gg_lav + indi_ns$m_meno9_gg_lav +
  indi_ns$m_meno10_gg_lav + indi_ns$m_meno11_gg_lav + indi_ns$m_meno12_gg_lav

indi_ns$prewd13_24 <- indi_ns$m_meno13_gg_lav + indi_ns$m_meno14_gg_lav + indi_ns$m_meno15_gg_lav + indi_ns$m_meno16_gg_lav +
  indi_ns$m_meno17_gg_lav + indi_ns$m_meno18_gg_lav + indi_ns$m_meno19_gg_lav + indi_ns$m_meno20_gg_lav + indi_ns$m_meno21_gg_lav +
  indi_ns$m_meno22_gg_lav + indi_ns$m_meno23_gg_lav + indi_ns$m_meno24_gg_lav

indi_ns$prewd25_36 <- indi_ns$m_meno25_gg_lav + indi_ns$m_meno26_gg_lav + indi_ns$m_meno27_gg_lav + indi_ns$m_meno28_gg_lav +
  indi_ns$m_meno29_gg_lav + indi_ns$m_meno30_gg_lav + indi_ns$m_meno31_gg_lav + indi_ns$m_meno32_gg_lav + indi_ns$m_meno33_gg_lav +
  indi_ns$m_meno34_gg_lav + indi_ns$m_meno35_gg_lav + indi_ns$m_meno36_gg_lav

### Duration of the last unemployment spell (in number of days)
indi_ns$pre_oss_ultimo_data_cessazione_d <- ymd(indi_ns$pre_oss_ultimo_data_cessazione)
indi_ns$pre_no_oss_ultimo_data_cessazione_d <- ymd(indi_ns$pre_no_oss_ultimo_data_cessazione)

indi_ns$last_nempl_spell_oss <- indi_ns$axl_data_attribuzione_d - indi_ns$pre_oss_ultimo_data_cessazione_d
indi_ns$last_nempl_spell_no_oss <- indi_ns$axl_data_attribuzione_d - indi_ns$pre_no_oss_ultimo_data_cessazione_d # 143 cases with incoherent values (negatives)

proble_ultimoepi <- subset(indi_ns, indi_ns$last_nempl_spell_no_oss < 0)
proble_ultimoepi <- proble_ultimoepi[, c("axl_data_attribuzione_d", "pre_no_oss_ultimo_data_cessazione_d")] # observing these incoherent values, better to consider them as NAs
rm(proble_ultimoepi)

indi_ns$last_nempl_spell_no_oss_ok <- ifelse(indi_ns$last_nempl_spell_no_oss < 0, NA, indi_ns$last_nempl_spell_no_oss) # class numeric
summary(indi_ns$last_nempl_spell_no_oss_ok)

indi_ns$last_nempl_spell_oss_ok <- as.numeric(indi_ns$last_nempl_spell_oss)

indi_ns$last_nempl_spell <- pmin(indi_ns$last_nempl_spell_oss_ok, indi_ns$last_nempl_spell_no_oss_ok, na.rm = T)

### Type of the last contract
indi_ns$group_last_contract <- ifelse(indi_ns$last_nempl_spell == indi_ns$last_nempl_spell_oss_ok, "standard", "non-standard")

indi_ns$type_last_contract <- ifelse(indi_ns$group_last_contract == "standard", indi_ns$pre_oss_ultimo_contratto2_ass, indi_ns$pre_no_oss_ultimo_contratto2_ass)

### Sector of the last contract
indi_ns$sector_last_contract <- ifelse(indi_ns$group_last_contract == "standard", indi_ns$pre_oss_ultimo_cod_ateco, indi_ns$pre_no_oss_ultimo_cod_ateco)

indi_ns$sector2_last_contract <- str_sub(indi_ns$sector_last_contract, start = 1, end = 2)
indi_ns$sector3_last_contract <- paste0(indi_ns$sector2_last_contract, str_sub(indi_ns$sector_last_contract, start = 4, end = 4))
indi_ns$sector4_last_contract <- paste0(indi_ns$sector2_last_contract, str_sub(indi_ns$sector_last_contract, start = 4, end = 5))
head(indi_ns$sector4_last_contract)

indi_ns$sector5_last_contract <- paste0(indi_ns$sector4_last_contract, str_sub(indi_ns$sector_last_contract, start = 7, end = 7))
head(indi_ns$sector5_last_contract)

indi_ns <- indi_ns %>% mutate(sectorVL = case_when(sector2_last_contract %in% c("10", "11", "12", "13", "14", "16", "31") |
                                                     sector3_last_contract %in% c("151", "152", "231", "234", "237", "321", "322", "323", "324") |
                                                     sector5_last_contract %in% c("32504", "32505") ~ "MadeInItaly",
                                                   sector2_last_contract %in% c("17", "18", "19", "20", "22", "21",  "35", "36", "37", "38", "39", "05", "06", "07", "08", "09") |
                                                     sector5_last_contract %in% c("32501", "32502", "32503") |
                                                     sector3_last_contract %in% c("232", "233", "235", "236", "239", "329") ~ "AltreIndustrie", # desde el 35 pertenece a utilities; también añadidas las industria extractivas ("05", "06", "07", "08", "09")
                                                   sector2_last_contract %in% c("41", "42", "43") ~ "Costruzione",
                                                   sector2_last_contract %in% c("47", "55","56","79","90","91","92","93") |
                                                     sector3_last_contract %in% c("823") | sector4_last_contract %in% c("4532", "9604") ~ "CommercioTempoLib",
                                                   sector2_last_contract %in% c("84", "85", "75", "86", "87", "88", "97", "95", "94", "96", "98", "99") |
                                                     sector3_last_contract %in% c("452", "772") | sector5_last_contract %in% c("45403") ~ "ServiziPersona",
                                                   sector2_last_contract %in% c("46", "49", "50", "51", "52", "53") | sector3_last_contract %in% c("451", "453", "454") ~ "IngrossoLogistica",
                                                   sector2_last_contract %in% c("58", "59", "60", "61", "62", "63", "69", "70", "71", "73", "74", "72") |
                                                     sector3_last_contract %in% c("774", "781") | sector4_last_contract %in% c("6391") ~ "TerziarioAvanzato",
                                                   sector2_last_contract %in% c("80", "82", "77", "68", "64", "65", "66") |
                                                     sector3_last_contract %in% c("812", "811", "813", "782", "783") ~ "AltriServizi", # ultimas dos de agenzie somministrazione, también añadidos los de servicios financieros ("64", "65", "66")
                                                   sector2_last_contract %in% c("24", "25", "28", "33", "26", "27", "29", "30") ~ "MetalMeccanico",
                                                   is.na(sector2_last_contract) ~ "Unobserved",
                                                   sector2_last_contract %in% c("01", "02", "03") ~ "Agricoltura")) # not included in the profiling model


### Occupation of the last contract
indi_ns$edu_last_contract <- ifelse(indi_ns$group_last_contract == "standard", indi_ns$pre_oss_ultimo_cod_qualifica, indi_ns$pre_no_oss_ultimo_cod_qualifica)

indi_ns$edu1_last_contract <- str_sub(indi_ns$edu_last_contract, 1, 1)
freq(indi_ns$edu1_last_contract)

indi_ns <- indi_ns %>% mutate(edulastcontract = case_when(edu1_last_contract %in% c("1", "2") ~ "Intellettuali",
                                                          edu1_last_contract == "3" ~ "Tecniche",
                                                          edu1_last_contract == "5" ~ "Qualiservizi",
                                                          edu1_last_contract == "6" ~ "OpeSpecializzati",
                                                          edu1_last_contract == "7" ~ "Semispecializzati",
                                                          edu1_last_contract == "8" ~ "Nonqualificate",
                                                          is.na(edu1_last_contract) ~ "Nonobserved",
                                                          edu1_last_contract == "4" ~ "Impiegati",
                                                          TRUE ~ "NA"))
indi_ns$edu_lastcont <- car::recode(indi_ns$edulastcontract, '"NA" = NA', as.factor = T)

# From CP2011 (Italian classification) to ISCO-08
indi_ns <- left_join(indi_ns, ISCOCPitalia[, c("CP2011_code", "ISCO08_code", "ISCO08_1digit")],
                      by = c("edu_last_contract" = "CP2011_code"))


### Duration of the last employment spell (in number of days)
indi_ns$duration_last_es <- ifelse(indi_ns$group_last_contract == "standard", indi_ns$pre_oss_ultimo_durata, indi_ns$pre_no_oss_ultimo_durata)


### Reviewing all candidate control variables
indi_ns$last_nempl_spell_ok <- ifelse(is.na(indi_ns$last_nempl_spell), 1095, indi_ns$last_nempl_spell)

indi_ns$type_last_contract_ok <- ifelse(is.na(indi_ns$type_last_contract), "Unobserved", indi_ns$type_last_contract)

indi_ns$sector2_last_contract_ok <- ifelse(is.na(indi_ns$sector2_last_contract), "Unobserved", indi_ns$sector2_last_contract)

indi_ns$sector2_last_contract_num <- as.numeric(indi_ns$sector2_last_contract_ok)
indi_ns <- indi_ns %>% mutate(sector1_last_contract_ok = case_when(sector2_last_contract_num %in% c(1:3) ~ "A",
                                                                   sector2_last_contract_num %in% c(5:9) ~ "B",
                                                                   sector2_last_contract_num %in% c(10:33) ~ "C",
                                                                   sector2_last_contract_num %in% c(35) ~ "D",
                                                                   sector2_last_contract_num %in% c(36:39) ~ "E",
                                                                   sector2_last_contract_num %in% c(41:43) ~ "F",
                                                                   sector2_last_contract_num %in% c(45:47) ~ "G",
                                                                   sector2_last_contract_num %in% c(49:53) ~ "H",
                                                                   sector2_last_contract_num %in% c(55:56) ~ "I",
                                                                   sector2_last_contract_num %in% c(58:63) ~ "J",
                                                                   sector2_last_contract_num %in% c(64:66) ~ "K",
                                                                   sector2_last_contract_num %in% c(68) ~ "L",
                                                                   sector2_last_contract_num %in% c(69:75) ~ "M",
                                                                   sector2_last_contract_num %in% c(77:82) ~ "N",
                                                                   sector2_last_contract_num %in% c(84) ~ "O",
                                                                   sector2_last_contract_num %in% c(85) ~ "P",
                                                                   sector2_last_contract_num %in% c(86:88) ~ "Q",
                                                                   sector2_last_contract_num %in% c(90:93) ~ "R",
                                                                   sector2_last_contract_num %in% c(94:96) ~ "S",
                                                                   sector2_last_contract_num %in% c(97:98) ~ "T",
                                                                   sector2_last_contract_num %in% c(99) ~ "U",
                                                                   TRUE ~ "Unobserved"))


indi_ns$duration_last_es36_ok <- ifelse(is.na(indi_ns$duration_last_es), 0, indi_ns$duration_last_es)

## Exporting datasets to use them in Stata --------------
indi_ns_stata <- indi_ns[, c("genere", "eta", "studio3", "studio2", "flg_italiano", "prewd1_12", "prewd13_24", "prewd25_36", "last_nempl_spell_ok", "sector1_last_contract_ok", 
                             "type_last_contract_ok", "sector2_last_contract_ok", "edu_lastcont", "duration_last_es36_ok", "pre_oss_max_durata", "pre_no_oss_max_durata",  "ISCO08_code",
                             "ide_soggetto", "axl_data_attribuzione", "scoringD1_0", "scoringD2_0", "post_interval6", "post_interval12", "post_interval18", "post_interval24")]

indi_ns_stataD1 <- subset(indi_ns_stata, indi_ns$scoringD2_0 < 0) # This includes group A and group B
indi_ns_stataD1$treatedD1 <- ifelse(indi_ns_stataD1$scoringD1_0 > 0, "Treated", "Nontreated")


indi_ns_stataD2 <- subset(indi_ns_stata, indi_ns$scoringD1_0 > 0) # This includes group B and group C

#haven::write_dta(indi_ns_stataD1, "indi_ns_stataD1ok.dta")
#haven::write_dta(indi_ns_stataD2, "indi_ns_stataD2.dta")

indi_ns_stata6 <- indi_ns[, c("genere", "eta", "studio2", "studio3", "flg_italiano", "prewd1_12", "prewd13_24", "prewd25_36", "last_nempl_spell_ok",
                              "prewd1_6", "prewd7_12", "prewd13_18", "prewd19_24", "edu_lastcont",  "sector1_last_contract_ok", "ISCO08_code",
                              "type_last_contract_ok", "sector2_last_contract_ok", "duration_last_es36_ok", "pre_oss_max_durata", "pre_no_oss_max_durata",
                              "ide_soggetto", "axl_data_attribuzione", "scoringD1_0", "scoringD2_0", "post_interval6", "post_interval12", "post_interval18", "post_interval24",
                              "post_interval712", "post_interval1318", "post_interval1924", "jshours", "attiv_form_ore_prev",
                              "indice_previsto", "axl_scoring", "ppa_data_avvio_d", "OEC_interval1_3", "FTC612_interval1_3", "FTCm12_interval1_3",
                              "OEC_interval1_6", "FTC612_interval1_6", "FTCm12_interval1_6", "OEC_interval7_12", "FTC612_interval7_12", "FTCm12_interval7_12",
                              "axl_profil_punteggio", "axl_profil_fascia", "spent_incentivejs", "spent_incentive", "spent_js",
                              "studio2_grouped", "studio2_grouped2", "sectorVL", "data_fine_ppa_d",
                              "m_meno1_gg_lav", "m_meno2_gg_lav", "m_meno3_gg_lav", "m_meno4_gg_lav", "m_meno5_gg_lav", "m_meno6_gg_lav",
                              "axl_ente", "axl_sportello", "cpi_titolare")] # needed for heterogeneity effects

indi_ns6_stataD1 <- subset(indi_ns_stata6, indi_ns_stata6$scoringD2_0 < 0) # This includes group A and group B

#haven::write_dta(indi_ns6_stataD1, "indi_ns6_stataD1_141222.dta")
#saveRDS(indi_ns6_stataD1, "indi_ns6_stataD1.RDS")

# 3. Summary statistics -----------------------
### Table of summary statistics by group and total (Table A1)
indi_ns$trgroup <- car::recode(indi_ns$axl_profil_fascia,
                               '"ALTA" = "C"; "MEDIA" = "B"; "BASSA" = "A"', as.factor = T,
                               levels = c("A", "B", "C"))

indi_ns$studio2_grouped3 <- car::recode(indi_ns$studio2_grouped2,
                                        'c("353_uppersecP_4y_prof", "353_uppersecP_4y_tec") = "353_uppersecP_4y"',
                                        as.factor = T)

f_study <- indi_ns %>% tabyl(studio2_grouped3, trgroup, show_na = F) %>%
  adorn_percentages(denominator = "col", na.rm = T) %>%
  adorn_pct_formatting(affix_sign = F)

f_study_all <- indi_ns %>% tabyl(studio2_grouped3) %>% adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting(affix_sign = F)

freqstudy <- cbind(f_study, all = f_study_all[1:7, 4])

indi_ns$ISCO08_code1 <- str_sub(indi_ns$ISCO08_code, 1, 1)

indi_ns$ISCO08_code1r <- car::recode(indi_ns$ISCO08_code1,
                                     '"0" = "0 Armed forces"; "1" = "1 Managers";
                                     "2" = "2 Professionals"; "3" = "3 Technicians and associate professionals";
                                     "4" = "4 Clerical support workers"; "5" = "5 Services and sales workers";
                                     "6" = "6 Skilled agricultural, forestry and fishery workers";
                                     "7" = "7 Craft and related trades workers";
                                     "8" = "8 Plant and machine operators, and assemblers";
                                     "9" = "9 Elementary occupations";
                                     NA = "X Unobserved"', as.factor = T)

flastoccu <- indi_ns %>% tabyl(ISCO08_code1r, trgroup) %>% adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting(affix_sign = F)

flastoccu_all <- indi_ns %>% tabyl(ISCO08_code1r) %>% adorn_percentages(denominator = "col") %>%
  adorn_pct_formatting(affix_sign = F)

freqlo <- cbind(flastoccu, all = flastoccu_all[, 3])


indi_ns$disabil <- ifelse(indi_ns$ide_data %in% disab$ide_data, "Yes", "No")

f_asfd <- indi_ns %>% group_by(trgroup) %>% summarise(age = mean(eta),
                                            sex_w = mean(genere == "F") * 100,
                                            foreign = mean(flg_italiano == "no") * 100,
                                            disability = mean(disabil == "Yes") * 100) %>%
  pivot_longer(-1) %>% pivot_wider(names_from = "trgroup", values_from = "value") %>%
  adorn_rounding(1)

f_asfd$all <- c(mean(indi_ns$eta), mean(indi_ns$genere == "F") * 100,
                mean(indi_ns$flg_italiano == "no") * 100,
                mean(indi_ns$disabil == "Yes") * 100)

f_asfdx <- adorn_rounding(f_asfd, 1)

colnames(f_asfdx)[1] <- "variable"
colnames(freqstudy)[1] <- "variable"
colnames(freqlo)[1] <- "variable"

samplesize <- indi_ns %>% tabyl(trgroup) %>% adorn_totals()
ssize <- as.data.frame(cbind("n", (t(samplesize$n))))
colnames(ssize) <- c("variable", "A", "B", "C", "all")
summaryf <- rbind(f_asfdx, freqstudy, freqlo, ssize)

#openxlsx::write.xlsx(summaryf, 'summaryftable.xlsx')


## Subsamples according to treatment received --------------------
indi_ns_ss1 <- subset(indi_ns6_stataD1, indi_ns6_stataD1$scoringD1_0 < 0.2) # subsample for treatment 1
indi_ns_ss2 <- subset(indi_ns, indi_ns$scoringD1_0 == 0 | indi_ns$scoringD1_0 > 0) # subsample for treatment 2

indi_ns_ss1$ide_data <- paste0(indi_ns_ss1$ide_soggetto, "_", indi_ns_ss1$axl_data_attribuzione)
indi_ns_ss1$disabil <- ifelse(indi_ns_ss1$ide_data %in% disab$ide_data, "Yes", "No")
indi_ns_ss1$disability <- as.factor(indi_ns_ss1$disabil)

indi_ns_ss2$ide_data <- paste0(indi_ns_ss2$ide_soggetto, "_", indi_ns_ss2$axl_data_attribuzione)
indi_ns_ss2$disabil <- ifelse(indi_ns_ss2$ide_data %in% disab$ide_data, "Yes", "No")
indi_ns_ss2$disability <- as.factor(indi_ns_ss2$disabil)


# 4. ANALYSIS: LATE with 3 treatments (A, B, C) -------------

## Preparing table of estimates
tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[, 1], # Conventional (nor bias-corrected, nor robust-bias-corrected)
    std.error = model$se[, 1],
    p.value = model$pv[3] # Robust-bias-corrected inference
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    Bandwidth = round(model$bws[1], 3),
    N_h = sum(model$N_h),
    Kernel = model$kernel,
    Polynomial_degree = round(model$p)
  )
  ret
}

cm <- c('Conventional' = 'Treatment')


## 4.1. Treatment 1 (D1: B vs. A) ------------
### Quantitative outcomes about employment ----------------

### Outcome 1: days worked post months [1, 6]
rddD1ei6_p1kT <- rdrobust(y = indi_ns_ss1$post_interval6, x = indi_ns_ss1$scoringD1_0,
                          kernel = "triangular",
                          c = 0, p = 1, bwselect = "mserd", cluster = NULL, all = T)
summary(rddD1ei6_p1kT)

rddD1ei6_p2kT <- rdrobust(y = indi_ns_ss1$post_interval6, x = indi_ns_ss1$scoringD1_0,
                          kernel = "triangular",
                          c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(rddD1ei6_p2kT)

# Slight change at the optimal bw
srdd_2_D1ei6_p1kT <- rdrobust(y = indi_ns_ss1$post_interval6, x = indi_ns_ss1$scoringD1_0,
                              kernel = "triangular", h = rddD1ei6_p1kT$bws[1,1] - 0.01,
                              c = 0, p = 1, cluster = NULL)
summary(srdd_2_D1ei6_p1kT)

srdd_2_D1ei6_p2kT <- rdrobust(y = indi_ns_ss1$post_interval6, x = indi_ns_ss1$scoringD1_0,
                              kernel = "triangular", h = rddD1ei6_p2kT$bws[1,1] - 0.01,
                              c = 0, p = 2, cluster = NULL)
summary(srdd_2_D1ei6_p2kT)

# Table B1
#modelsummary(list(rddD1ei6_p1kT, rddD1ei6_p2kT, srdd_2_D1ei6_p1kT, srdd_2_D1ei6_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D1_post6_nocl.docx")


### Outcome 2: days worked post months [7, 12]
rddD1ei12_p1kT <- rdrobust(y = indi_ns_ss1$post_interval712, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular", c = 0, p = 1, bwselect = "mserd",
                           cluster = NULL)
summary(rddD1ei12_p1kT)

rddD1ei12_p2kT <- rdrobust(y = indi_ns_ss1$post_interval712, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular",
                           c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(rddD1ei12_p2kT)

# Slight change at the bw
srddD1ei12_p1kT <- rdrobust(y = indi_ns_ss1$post_interval712, x = indi_ns_ss1$scoringD1_0,
                            kernel = "triangular", c = 0, p = 1,
                            h = rddD1ei12_p1kT$bws[1,1] - 0.01,
                            cluster = NULL)
summary(srddD1ei12_p1kT)

srddD1ei12_p2kT <- rdrobust(y = indi_ns_ss1$post_interval712, x = indi_ns_ss1$scoringD1_0,
                            kernel = "triangular",
                            c = 0, p = 2, cluster = NULL,
                            h = rddD1ei12_p2kT$bws[1,1] - 0.01)
summary(srddD1ei12_p2kT)

# Table B2
#modelsummary(list(rddD1ei12_p1kT, rddD1ei12_p2kT, srddD1ei12_p1kT, srddD1ei12_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D1_post12_nocl.docx")


### Outcome 3: days worked post [13, 18] months
rddD1ei18_p1kT <- rdrobust(y = indi_ns_ss1$post_interval1318, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular", c = 0, p = 1, bwselect = "mserd",
                           cluster = NULL)
summary(rddD1ei18_p1kT)

rddD1ei18_p2kT <- rdrobust(y = indi_ns_ss1$post_interval1318, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular", c = 0, p = 2, bwselect = "mserd",
                           cluster = NULL)
summary(rddD1ei18_p2kT)

# Slight change at the bw
srddD1ei18_p1kT <- rdrobust(y = indi_ns_ss1$post_interval1318, x = indi_ns_ss1$scoringD1_0,
                            kernel = "triangular", c = 0, p = 1,
                            h = rddD1ei18_p1kT$bws[1,1] - 0.01,
                            cluster = NULL)
summary(srddD1ei18_p1kT)

srddD1ei18_p2kT <- rdrobust(y = indi_ns_ss1$post_interval1318, x = indi_ns_ss1$scoringD1_0,
                            kernel = "triangular", c = 0, p = 2,
                            h = rddD1ei18_p2kT$bws[1,1] - 0.01,
                            cluster = NULL)
summary(srddD1ei18_p2kT)

## Table B3
#modelsummary(list(rddD1ei18_p1kT, rddD1ei18_p2kT, srddD1ei18_p1kT, srddD1ei18_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D1_post18_nocl.docx")



### Outcome 4: days worked post [19, 24] months
rdd24_p1kT <- rdrobust(y = indi_ns_ss1$post_interval1924, x = indi_ns_ss1$scoringD1_0,
                      kernel = "triangular",
                      c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(rdd24_p1kT)

rdd24_p2kT <- rdrobust(y = indi_ns_ss1$post_interval1924, x = indi_ns_ss1$scoringD1_0,
                      kernel = "triangular",
                      c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(rdd24_p2kT)

# Slight change at the optimal bw
srdd24_p1kT <- rdrobust(y = indi_ns_ss1$post_interval1924, x = indi_ns_ss1$scoringD1_0,
                        kernel = "triangular",
                        c = 0, p = 1, cluster = NULL,
                        h = rdd24_p1kT$bws[1,1] - 0.01)
summary(srdd24_p1kT)

srdd24_p2kT <- rdrobust(y = indi_ns_ss1$post_interval1924, x = indi_ns_ss1$scoringD1_0,
                        kernel = "triangular",
                        c = 0, p = 2, cluster = NULL,
                        h = rdd24_p2kT$bws[1,1] - 0.01)
summary(srdd24_p2kT)

# Table B4
#modelsummary(list(rdd24_p1kT, rdd24_p2kT, srdd24_p1kT, srdd24_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D1_post24_nocl.docx")



### Qualitative outcomes about employment -------------------
### Outcome 5: type of contract (longest one) with Xu's method
#### Joining datasets
longest$id <- paste0(longest$ide_soggetto, "_", longest$axl_data_attribuzione)
longestforjoin <- longest[, c("id", "longestcontract", "longestcontract_ris")]

indi_ns_ss1$id <- paste0(indi_ns_ss1$ide_soggetto, "_", indi_ns_ss1$axl_data_attribuzione)
indi_ns_ss1_c <- left_join(indi_ns_ss1, longestforjoin, by = "id")

# Classic classification of contracts

indi_ns_ss1_c$lc_AC <- ifelse(indi_ns_ss1_c$longestcontract == "AC", 1, 0)
indi_ns_ss1_c$lc_DC <- ifelse(indi_ns_ss1_c$longestcontract == "DC", 1, 0)
indi_ns_ss1_c$lc_FTC <- ifelse(indi_ns_ss1_c$longestcontract == "FTC", 1, 0)
indi_ns_ss1_c$lc_NE <- ifelse(indi_ns_ss1_c$longestcontract == "NE", 1, 0)
indi_ns_ss1_c$lc_OEC <- ifelse(indi_ns_ss1_c$longestcontract == "OEC", 1, 0)
indi_ns_ss1_c$lc_SC <- ifelse(indi_ns_ss1_c$longestcontract == "SC", 1, 0)

# Ris classification of contracts
indi_ns_ss1_c$lcris_OEC <- ifelse(indi_ns_ss1_c$longestcontract_ris == "OEC", 1, 0)
indi_ns_ss1_c$lcris_FTCm12 <- ifelse(indi_ns_ss1_c$longestcontract_ris == "FTCm12", 1, 0)
indi_ns_ss1_c$lcris_FTC612 <- ifelse(indi_ns_ss1_c$longestcontract_ris == "FTC612", 1, 0)


#### Type of contract (binary specifications) in 1-24 time interval
create_ci <- function(x) {
  paste0("(", round(x[1], 3), ", ", round(x[2], 3), ")")
}


rdcate <- function(score, outcome) {
  #iD1lc <- indi_ns_ss1_c[, c("scoringD1_0", "lcris_OEC")]
  #setnames(iD1lc, "lcris_OEC", "lcris_OEC_yes")
  #iD1lc$lcris_OEC_no <- ifelse(iD1lc$lcris_OEC_yes == 1, 0, 1)
  outcome_no <- ifelse(outcome == 1, 0, 1)
  ilc <- cbind(score, outcome, outcome_no)
  colnames(ilc) <- c("scoring", "outcome_yes", "outcome_no")
  
  ilc_ord <- as.data.frame(ilc) %>% arrange(scoring) # de forma ascendente
  ilc_ordm <- as.matrix(ilc_ord)
  
  miJ <- length(ilc_ordm[1, ]) - 2
  
  rd_cat_est99 <- rd.mnl(DAT = ilc_ordm, c = 0,
                         H0_t = matrix(data = 0, nrow = miJ, ncol = 1),
                         H0_R = diag(miJ),
                         H0_r = matrix(0, miJ, 1),
                         level = 0.99)
  
  rd_cat_est95 <- rd.mnl(DAT = ilc_ordm, c = 0,
                         H0_t = matrix(data = 0, nrow = miJ, ncol = 1),
                         H0_R = diag(miJ),
                         H0_r = matrix(0, miJ, 1),
                         level = 0.95)
  
  rd_cat_est90 <- rd.mnl(DAT = ilc_ordm, c = 0,
                         H0_t = matrix(data = 0, nrow = miJ, ncol = 1),
                         H0_R = diag(miJ),
                         H0_r = matrix(0, miJ, 1),
                         level = 0.90)
  
  tabla_est <- data.frame(pointest = round(rd_cat_est95$ATE, 3),
                          ci95 = create_ci(rd_cat_est95$ci_rob),
                          signif90 = if(data.table::between(0, rd_cat_est90$ci_rob[1], rd_cat_est90$ci_rob[2]) == T) {F} else{T},
                          signif95 = if(data.table::between(0, rd_cat_est95$ci_rob[1], rd_cat_est95$ci_rob[2]) == T) {F} else{T},
                          signif99 = if(data.table::between(0, rd_cat_est99$ci_rob[1], rd_cat_est99$ci_rob[2]) == T) {F} else{T})
  
  tabla_est <- tabla_est %>%
    mutate(stars = case_when(signif99 == T ~ "***",
                             signif95 == T ~ "**",
                             signif90 == T ~ "*",
                             T ~ ""))
  
  tabla_est
  
}

## For D1
oec_d1 <- rdcate(score = indi_ns_ss1_c$scoringD1_0,
                 outcome = indi_ns_ss1_c$lcris_OEC)
ftcm12_d1 <- rdcate(score = indi_ns_ss1_c$scoringD1_0,
                    outcome = indi_ns_ss1_c$lcris_FTCm12)
ftc612_d1 <- rdcate(score = indi_ns_ss1_c$scoringD1_0,
                    outcome = indi_ns_ss1_c$lcris_FTC612)


table_qualityD1 <- data.frame(treatment = c(rep("D1", 2)),
                              prOEC = c(paste0(oec_d1$pointest, oec_d1$stars),
                                        oec_d1$ci95),
                              prFTCm12 = c(paste0(ftcm12_d1$pointest, ftcm12_d1$stars),
                                           ftcm12_d1$ci95),
                              prFTC612 = c(paste0(ftc612_d1$pointest, ftc612_d1$stars),
                                           ftc612_d1$ci95))


### Mechanisms: hours of treatment ----------
### Outcome HS: actual treatment received of job search measures
actualtr_p1kT <-  rdrobust(y = indi_ns_ss1$jshours, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular",
                           c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(actualtr_p1kT)

actualtr_p2kT <- rdrobust(y = indi_ns_ss1$jshours, x = indi_ns_ss1$scoringD1_0,
                          kernel = "triangular",
                          c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(actualtr_p2kT)

## With slight change at the bandwidth
sactualtr_p1kT <-  rdrobust(y = indi_ns_ss1$jshours, x = indi_ns_ss1$scoringD1_0,
                            kernel = "triangular",
                            c = 0, p = 1, cluster = NULL,
                            h = actualtr_p1kT$bws[1,1] - 0.01)
summary(sactualtr_p1kT)

sactualtr_p2kT <- rdrobust(y = indi_ns_ss1$jshours, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular",
                           c = 0, p = 2, cluster = NULL,
                           h = actualtr_p2kT$bws[1,1] - 0.01)
summary(sactualtr_p2kT)

# Table B17
#modelsummary(list(actualtr_p1kT, actualtr_p2kT, sactualtr_p1kT, sactualtr_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D1_received_nocl.docx")


### Outcome HT: actual treatment received of training measures
prevtr_p1kT <-  rdrobust(y = indi_ns_ss1$attiv_form_ore_prev, x = indi_ns_ss1$scoringD1_0,
                         kernel = "triangular",
                         c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(prevtr_p1kT)

prevtr_p2kT <- rdrobust(y = indi_ns_ss1$attiv_form_ore_prev, x = indi_ns_ss1$scoringD1_0,
                        kernel = "triangular",
                        c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(prevtr_p2kT)

# Slight changes at the optimal bw
sprevtr_p1kT <-  rdrobust(y = indi_ns_ss1$attiv_form_ore_prev, x = indi_ns_ss1$scoringD1_0,
                          kernel = "triangular",
                          c = 0, p = 1, cluster = NULL,
                          h = prevtr_p1kT$bws[1,1] - 0.01)
summary(sprevtr_p1kT)

sprevtr_p2kT <- rdrobust(y = indi_ns_ss1$attiv_form_ore_prev, x = indi_ns_ss1$scoringD1_0,
                         kernel = "triangular",
                         c = 0, p = 2, cluster = NULL,
                         h = prevtr_p2kT$bws[1,1] - 0.01)
summary(sprevtr_p2kT)

# Table B18 
#modelsummary(list(actualtr_p1kT, actualtr_p2kT, sprevtr_p1kT, sprevtr_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D1_received_nocl.docx")


### ROBUSTNESS (LATE): Changing the bandwidth selector (one selector for each side) --------------
glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    Bandwidths = paste(round(model$bws[1,1], 3), round(model$bws[1,2], 3)),
    N_h = sum(model$N_h),
    Kernel = model$kernel,
    Polynomial_degree = round(model$p)
  )
  ret
} 

cm <- c('Conventional' = 'Treatment')


### Outcome 1: days worked post months [1, 6]
rdd_2_D1ei6_p1kT <- rdrobust(y = indi_ns_ss1$post_interval6, x = indi_ns_ss1$scoringD1_0,
                          kernel = "triangular",
                          c = 0, p = 1, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_D1ei6_p1kT)

rdd_2_D1ei6_p2kT <- rdrobust(y = indi_ns_ss1$post_interval6, x = indi_ns_ss1$scoringD1_0,
                          kernel = "triangular",
                          c = 0, p = 2, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_D1ei6_p2kT)

# Table B9
#modelsummary(list(rdd_2_D1ei6_p1kT, rdd_2_D1ei6_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_2__D1_post6_nocl.docx")


# Function to write Stata code for AMSE estimation
paste0("rmdse ", rdd_2_D1ei6_p1kT$call$y[[3]], " ", rdd_2_D1ei6_p1kT$call$x[[3]], ", ",
       "deriv(0) c(0) pl(", rdd_2_D1ei6_p1kT$call$p, ") pr(", rdd_2_D1ei6_p1kT$call$p,
       ") hl(", rdd_2_D1ei6_p1kT$bws[1,1], ") hr(", rdd_2_D1ei6_p1kT$bws[1,2], ") bl(",
       rdd_2_D1ei6_p1kT$bws[2,1], ") br(", rdd_2_D1ei6_p1kT$bws[2,2], ") kernel(",
       str_to_lower(rdd_2_D1ei6_p1kT$kernel), ")")

tostata <- function(model) {
  code <- paste0("rdmse ", model$call$y[[3]], " ", model$call$x[[3]], ", ",
                 "deriv(0) c(0) twosided pl(", model$call$p, ") pr(", model$call$p,
                 ") hl(", model$bws[1,1], ") hr(", model$bws[1,2], ") bl(",
                 model$bws[2,1], ") br(", model$bws[2,2], ") kernel(",
                 str_to_lower(model$kernel), ")",
                 "\n", "scalar ", model$call$y[[3]], model$call$p, model$kernel, "_l = ", "r(amse_l_cl)",
                 "\n", "scalar ", model$call$y[[3]], model$call$p, model$kernel,"_r = ", "r(amse_r_cl)")
  #code2 <- paste0( "scalar ", model$call$y[[3]], "_l = ", "r(amse_l_cl)")
  
  #cat(code)
  #cat(code2)
}

robust_stata_d1_1 <- data.frame(stata = c(tostata(rdd_2_D1ei6_p1kT), tostata(rdd_2_D1ei6_p2kT)))

#write.xlsx(robust_stata_d1_1, 'robust_stata_d1_1.xlsx')



### Outcome 2: days worked post months [7, 12]
rdd_2_D1ei12_p1kT <- rdrobust(y = indi_ns_ss1$post_interval712, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular", c = 0, p = 1, bwselect = "msetwo",
                           cluster = NULL)
summary(rdd_2_D1ei12_p1kT)

rdd_2_D1ei12_p2kT <- rdrobust(y = indi_ns_ss1$post_interval712, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular",
                           c = 0, p = 2, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_D1ei12_p2kT)

# Table B10
#modelsummary(list(rdd_2_D1ei12_p1kT, rdd_2_D1ei12_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_2__D1_post12_nocl.docx")

robust_stata_d1_2 <- data.frame(stata = c(tostata(rdd_2_D1ei12_p1kT), tostata(rdd_2_D1ei12_p2kT)))

#write.xlsx(robust_stata_d1_2, 'robust_stata_d1_2.xlsx')

### Outcome 3: days worked post [13, 18] months
rdd_2_D1ei18_p1kT <- rdrobust(y = indi_ns_ss1$post_interval1318, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular", c = 0, p = 1, bwselect = "msetwo",
                           cluster = NULL)
summary(rdd_2_D1ei18_p1kT)

rdd_2_D1ei18_p2kT <- rdrobust(y = indi_ns_ss1$post_interval1318, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular", c = 0, p = 2, bwselect = "msetwo",
                           cluster = NULL)
summary(rdd_2_D1ei18_p2kT)


# Table B11
#modelsummary(list(rdd_2_D1ei18_p1kT, rdd_2_D1ei18_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_2__D1_post18_nocl.docx")


robust_stata_d1_3 <- data.frame(stata = c(tostata(rdd_2_D1ei18_p1kT), tostata(rdd_2_D1ei18_p2kT)))

#write.xlsx(robust_stata_d1_3, 'robust_stata_d1_3.xlsx')

### Outcome 4: days worked post [19, 24] months
rdd_2_24_p1kT <- rdrobust(y = indi_ns_ss1$post_interval1924, x = indi_ns_ss1$scoringD1_0,
                       kernel = "triangular",
                       c = 0, p = 1, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_24_p1kT)

rdd_2_24_p2kT <- rdrobust(y = indi_ns_ss1$post_interval1924, x = indi_ns_ss1$scoringD1_0,
                       kernel = "triangular",
                       c = 0, p = 2, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_24_p2kT)

cm <- c('Conventional' = 'Treatment')

# Table B12
#modelsummary(list(rdd_2_24_p1kT, rdd_2_24_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_2__D1_post24_nocl.docx")

robust_stata_d1_4 <- data.frame(stata = c(tostata(rdd_2_24_p1kT), tostata(rdd_2_24_p2kT)))

#write.xlsx(robust_stata_d1_4, 'robust_stata_d1_4.xlsx')

### PLAUSIBILITY (LATE): Pseudo-treatments ----------------
indi_ns_ss1_rmT1 <- indi_ns_ss1 %>%
  filter(ppa_data_avvio_d > ymd("2017-06-30"))

#haven::write_dta(indi_ns_ss1[, c("scoringD1_0", "prewd1_6", "prewd7_12", "prewd13_18", "prewd19_24")], "indi_ns_ss1_pse_n2023_rmT1.dta")


le_val1_prewd16 <- rdrobust(y = indi_ns_ss1_rmT1$prewd1_6, x = indi_ns_ss1_rmT1$scoringD1_0,
                            kernel = "triangular",
                            c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(le_val1_prewd16)

le_val1_prewd16_2 <- rdrobust(y = indi_ns_ss1_rmT1$prewd1_6, x = indi_ns_ss1_rmT1$scoringD1_0,
                              kernel = "triangular",
                              c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(le_val1_prewd16_2)



### PLAUSIBILITY: Checking balance in pretreatment variables ----------------
## Continuous outcome variables (age) 
val1_age <- rdrobust(y = indi_ns_ss1$eta, x = indi_ns_ss1$scoringD1_0,
                     kernel = "triangular",
                     c = 0, p = 1, bwselect = "mserd")
summary(val1_age)

val1_age2 <- rdrobust(y = indi_ns_ss1$eta, x = indi_ns_ss1$scoringD1_0,
                      kernel = "triangular",
                      c = 0, p = 2, bwselect = "mserd")
summary(val1_age2)

val1u_age <- rdrobust(y = indi_ns_ss1$eta, x = indi_ns_ss1$scoringD1_0,
                      kernel = "triangular",
                      c = 0, p = 1, bwselect = "mserd")
summary(val1_age)

val1u_age2 <- rdrobust(y = indi_ns_ss1$eta, x = indi_ns_ss1$scoringD1_0,
                       kernel = "triangular",
                       c = 0, p = 2, bwselect = "mserd")
summary(val1_age2)


cm <- c('Conventional' = 'Treatment')
val1tabc <- modelsummary(list(val1_age, val1_age2),
                         statistic = "{p.value} [{conf.low}, {conf.high}]", coef_map = cm,
                         stars = c('*' = .1, '**' = .05, '***' = 0.01),
                         output = "data.frame")

val1tabc <- val1tabc[, -c(1:3)]
val1tabct <- as.data.frame(t(val1tabc))
val1tabct$pvalue <- str_sub(val1tabct$V2, 1, 5)
val1tabct$ci <- str_sub(val1tabct$V2, 7)
val1tabct$V2 <- NULL
colnames(val1tabct) <- c("point", "bw", "n_bw", "pvalue", "ci")
val1tabct$outcome <- c("age", "age")
val1tabct$polydegreeX <- rep(c(1,2))


## Categorical outcome variables (sex, foreign, studies, disability, lastoccupation, last sector) 
# Data preparation 
### Sex 
indi_ns_ss1$sex_f <- ifelse(indi_ns_ss1$genere == "F", 1, 0)
#indi_ns_ss1$sex_f_f <- as.factor(indi_ns_ss1$sex_f)

v1_sexF <- rdrobust(y = indi_ns_ss1$sex_f, x = indi_ns_ss1$scoringD1_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")

summary(v1_sexF)

v1_sexF_2 <- rdrobust(y = indi_ns_ss1$sex_f, x = indi_ns_ss1$scoringD1_0,
                      kernel = "triangular",
                      c = 0, p = 2, bwselect = "mserd")
summary(v1_sexF_2)


v1u_sexF <- rdrobust(y = indi_ns_ss1$sex_f, x = indi_ns_ss1$scoringD1_0,
                     kernel = "triangular",
                     c = 0, p = 1, bwselect = "mserd")

summary(v1_sexF)

v1u_sexF_2 <- rdrobust(y = indi_ns_ss1$sex_f, x = indi_ns_ss1$scoringD1_0,
                       kernel = "triangular",
                       c = 0, p = 2, bwselect = "mserd")
summary(v1_sexF_2)

### Foreign
indi_ns_ss1$foreign <- ifelse(indi_ns_ss1$flg_italiano == "no", 1, 0)

v1_foreignF <- rdrobust(y = indi_ns_ss1$foreign, x = indi_ns_ss1$scoringD1_0,
                        kernel = "triangular",
                        c = 0, p = 1, bwselect = "mserd")

summary(v1_foreignF)

v1_foreignF_2 <- rdrobust(y = indi_ns_ss1$foreign, x = indi_ns_ss1$scoringD1_0,
                          kernel = "triangular",
                          c = 0, p = 2, bwselect = "mserd")
summary(v1_foreignF_2)


v1u_foreignF <- rdrobust(y = indi_ns_ss1$foreign, x = indi_ns_ss1$scoringD1_0,
                         kernel = "triangular",
                         c = 0, p = 1, bwselect = "mserd")

summary(v1_foreignF)

v1u_foreignF_2 <- rdrobust(y = indi_ns_ss1$foreign, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular",
                           c = 0, p = 2, bwselect = "mserd")
summary(v1_foreignF_2)


### Education
indi_ns_ss1 <- indi_ns_ss1 %>% mutate(studio2_groupedX = case_when(studio2 == "Licenza elementare" ~ "1_LicElementare",
                                                                   studio2 == "Licenza media" ~ "2_LicMedia",
                                                                   studio2 %in% c("Diploma di qualifica professionale") ~ "3_QualificaProf",
                                                                   studio2 %in% c("Diploma Tecnico", "Diploma Professionale", "Diploma Liceale", "Diploma universitario",
                                                                                  "Diploma Conservatorio musicale",
                                                                                  "Maestro d'arte", "Scuola magistrale (triennale)", "Diploma di istruzione artistica", "Diploma interprete, traduttore, archivista") ~ "4_Diploma",
                                                                   studio2 %in% c("Laurea I livello (triennale)", "Laurea - vecchio o nuovo ordinamento", "Post laurea") ~ "5_Laurea",
                                                                   TRUE ~ "Nondetermined"))
freq(indi_ns_ss1$studio2_groupedX)

indi_ns_ss1$elementare <- ifelse(indi_ns_ss1$studio2_groupedX == "1_LicElementare", 1, 0)
indi_ns_ss1$media <- ifelse(indi_ns_ss1$studio2_groupedX == "2_LicMedia", 1, 0)
indi_ns_ss1$qualifica <- ifelse(indi_ns_ss1$studio2_groupedX == "3_QualificaProf", 1, 0)
indi_ns_ss1$diploma <- ifelse(indi_ns_ss1$studio2_groupedX == "4_Diploma", 1, 0)
indi_ns_ss1$laurea <- ifelse(indi_ns_ss1$studio2_groupedX == "5_Laurea", 1, 0)

#### elementare

v1_elementareF <- rdrobust(y = indi_ns_ss1$elementare, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular",
                           c = 0, p = 1, bwselect = "mserd")

summary(v1_elementareF)

v1_elementareF_2 <- rdrobust(y = indi_ns_ss1$elementare, x = indi_ns_ss1$scoringD1_0,
                             kernel = "triangular",
                             c = 0, p = 2, bwselect = "mserd")
summary(v1_elementareF_2)


v1u_elementareF <- rdrobust(y = indi_ns_ss1$elementare, x = indi_ns_ss1$scoringD1_0,
                            kernel = "triangular",
                            c = 0, p = 1, bwselect = "mserd")

summary(v1_elementareF)

v1u_elementareF_2 <- rdrobust(y = indi_ns_ss1$elementare, x = indi_ns_ss1$scoringD1_0,
                              kernel = "triangular",
                              c = 0, p = 2, bwselect = "mserd")
summary(v1_elementareF_2)

#### media

v1_mediaF <- rdrobust(y = indi_ns_ss1$media, x = indi_ns_ss1$scoringD1_0,
                      kernel = "triangular",
                      c = 0, p = 1, bwselect = "mserd")

summary(v1_mediaF)

v1_mediaF_2 <- rdrobust(y = indi_ns_ss1$media, x = indi_ns_ss1$scoringD1_0,
                        kernel = "triangular",
                        c = 0, p = 2, bwselect = "mserd")
summary(v1_mediaF_2)


v1u_mediaF <- rdrobust(y = indi_ns_ss1$media, x = indi_ns_ss1$scoringD1_0,
                       kernel = "triangular",
                       c = 0, p = 1, bwselect = "mserd")

summary(v1_mediaF)

v1u_mediaF_2 <- rdrobust(y = indi_ns_ss1$media, x = indi_ns_ss1$scoringD1_0,
                         kernel = "triangular",
                         c = 0, p = 2, bwselect = "mserd")
summary(v1_mediaF_2)


#### qualifica 

v1_qualificaF <- rdrobust(y = indi_ns_ss1$qualifica, x = indi_ns_ss1$scoringD1_0,
                          kernel = "triangular",
                          c = 0, p = 1, bwselect = "mserd")

summary(v1_qualificaF)

v1_qualificaF_2 <- rdrobust(y = indi_ns_ss1$qualifica, x = indi_ns_ss1$scoringD1_0,
                            kernel = "triangular",
                            c = 0, p = 2, bwselect = "mserd")
summary(v1_qualificaF_2)


v1u_qualificaF <- rdrobust(y = indi_ns_ss1$qualifica, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular",
                           c = 0, p = 1, bwselect = "mserd")

summary(v1_qualificaF)

v1u_qualificaF_2 <- rdrobust(y = indi_ns_ss1$qualifica, x = indi_ns_ss1$scoringD1_0,
                             kernel = "triangular",
                             c = 0, p = 2, bwselect = "mserd")
summary(v1_qualificaF_2)


#### diploma

v1_diplomaF <- rdrobust(y = indi_ns_ss1$diploma, x = indi_ns_ss1$scoringD1_0,
                        kernel = "triangular",
                        c = 0, p = 1, bwselect = "mserd")

summary(v1_diplomaF)

v1_diplomaF_2 <- rdrobust(y = indi_ns_ss1$diploma, x = indi_ns_ss1$scoringD1_0,
                          kernel = "triangular",
                          c = 0, p = 2, bwselect = "mserd")
summary(v1_diplomaF_2)


v1u_diplomaF <- rdrobust(y = indi_ns_ss1$diploma, x = indi_ns_ss1$scoringD1_0,
                         kernel = "triangular",
                         c = 0, p = 1, bwselect = "mserd")

summary(v1_diplomaF)

v1u_diplomaF_2 <- rdrobust(y = indi_ns_ss1$diploma, x = indi_ns_ss1$scoringD1_0,
                           kernel = "triangular",
                           c = 0, p = 2, bwselect = "mserd")
summary(v1_diplomaF_2)



#### laurea

v1_laureaF <- rdrobust(y = indi_ns_ss1$laurea, x = indi_ns_ss1$scoringD1_0,
                       kernel = "triangular",
                       c = 0, p = 1, bwselect = "mserd")

summary(v1_laureaF)

v1_laureaF_2 <- rdrobust(y = indi_ns_ss1$laurea, x = indi_ns_ss1$scoringD1_0,
                         kernel = "triangular",
                         c = 0, p = 2, bwselect = "mserd")
summary(v1_laureaF_2)


v1u_laureaF <- rdrobust(y = indi_ns_ss1$laurea, x = indi_ns_ss1$scoringD1_0,
                        kernel = "triangular",
                        c = 0, p = 1, bwselect = "mserd")

summary(v1_laureaF)

v1u_laureaF_2 <- rdrobust(y = indi_ns_ss1$laurea, x = indi_ns_ss1$scoringD1_0,
                          kernel = "triangular",
                          c = 0, p = 2, bwselect = "mserd")
summary(v1_laureaF_2)


### Disability
indi_ns_ss1$disa <- ifelse(indi_ns_ss1$disability == "Yes", 1, 0)

v1_disaF <- rdrobust(y = indi_ns_ss1$disa, x = indi_ns_ss1$scoringD1_0,
                     kernel = "triangular",
                     c = 0, p = 1, bwselect = "mserd")

summary(v1_disaF)

v1_disaF_2 <- rdrobust(y = indi_ns_ss1$disa, x = indi_ns_ss1$scoringD1_0,
                       kernel = "triangular",
                       c = 0, p = 2, bwselect = "mserd")
summary(v1_disaF_2)


v1u_disaF <- rdrobust(y = indi_ns_ss1$disa, x = indi_ns_ss1$scoringD1_0,
                      kernel = "triangular",
                      c = 0, p = 1, bwselect = "mserd")

summary(v1_disaF)

v1u_disaF_2 <- rdrobust(y = indi_ns_ss1$disa, x = indi_ns_ss1$scoringD1_0,
                        kernel = "triangular",
                        c = 0, p = 2, bwselect = "mserd")
summary(v1_disaF_2)

### Occupation of last employment
indi_ns$qualifica1d <- str_sub(indi_ns$edu_last_contract , 1, 1)
indi_ns <- indi_ns %>% mutate(qualifica1dm = case_when(is.na(qualifica1d) | qualifica1d %in% c("1", "9") ~ "Pre36_Other",
                                                       qualifica1d == "2" ~ "2_Intellectual",
                                                       qualifica1d == "3" ~ "3_Technical",
                                                       qualifica1d == "4" ~ "4_WhiteLowSkilled",
                                                       qualifica1d == "5" ~ "5_WhiteHighSkilled",
                                                       qualifica1d == "6" ~ "6_BlueHighSkilled",
                                                       qualifica1d == "7" ~ "7_BlueMediumSkilled",
                                                       qualifica1d == "8" ~ "8_NonSkilled"))

indi_ns_ss1 <- left_join(indi_ns_ss1, indi_ns[, c("ide_data", "qualifica1dm")], by = "ide_data")

indi_ns_ss1$o2I <- ifelse(indi_ns_ss1$qualifica1dm == "2_Intellectual", 1, 0)
indi_ns_ss1$o3T <- ifelse(indi_ns_ss1$qualifica1dm == "3_Technical", 1, 0)
indi_ns_ss1$o4WLS <- ifelse(indi_ns_ss1$qualifica1dm == "4_WhiteLowSkilled", 1, 0)
indi_ns_ss1$o5WHS <- ifelse(indi_ns_ss1$qualifica1dm == "5_WhiteHighSkilled", 1, 0)
indi_ns_ss1$o6BHS <- ifelse(indi_ns_ss1$qualifica1dm == "6_BlueHighSkilled", 1, 0)
indi_ns_ss1$o7BMS <- ifelse(indi_ns_ss1$qualifica1dm == "7_BlueMediumSkilled", 1, 0)
indi_ns_ss1$o8NS <- ifelse(indi_ns_ss1$qualifica1dm == "8_NonSkilled", 1, 0)
indi_ns_ss1$oX <- ifelse(indi_ns_ss1$qualifica1dm == "Pre36_Other", 1, 0)

v1u_o2 <- rdrobust(y = indi_ns_ss1$o2I, x = indi_ns_ss1$scoringD1_0,
                   kernel = "triangular",
                   c = 0, p = 1, bwselect = "mserd")

summary(v1u_o2)


v1u_o3 <- rdrobust(y = indi_ns_ss1$o3T, x = indi_ns_ss1$scoringD1_0,
                   kernel = "triangular",
                   c = 0, p = 1, bwselect = "mserd")
summary(v1u_o3)


v1u_o4 <- rdrobust(y = indi_ns_ss1$o4WLS, x = indi_ns_ss1$scoringD1_0,
                   kernel = "triangular",
                   c = 0, p = 1, bwselect = "mserd")
summary(v1u_o4)


v1u_o5 <- rdrobust(y = indi_ns_ss1$o5WHS, x = indi_ns_ss1$scoringD1_0,
                   kernel = "triangular",
                   c = 0, p = 1, bwselect = "mserd")
summary(v1u_o5)


v1u_o6 <- rdrobust(y = indi_ns_ss1$o6BHS, x = indi_ns_ss1$scoringD1_0,
                   kernel = "triangular",
                   c = 0, p = 1, bwselect = "mserd")
summary(v1u_o6)


v1u_o7 <- rdrobust(y = indi_ns_ss1$o7BMS, x = indi_ns_ss1$scoringD1_0,
                   kernel = "triangular",
                   c = 0, p = 1, bwselect = "mserd")
summary(v1u_o7)


v1u_o8 <- rdrobust(y = indi_ns_ss1$o8NS, x = indi_ns_ss1$scoringD1_0,
                   kernel = "triangular",
                   c = 0, p = 1, bwselect = "mserd")
summary(v1u_o8)


v1u_oX <- rdrobust(y = indi_ns_ss1$oX, x = indi_ns_ss1$scoringD1_0,
                   kernel = "triangular",
                   c = 0, p = 1, bwselect = "mserd")
summary(v1u_oX)

### Sector of last employment
indi_ns_ss1 <- indi_ns_ss1 %>%
  mutate(sectorVLm = case_when(sectorVL %in% c("Agricoltura", "Unobserved") ~ "Pre36_Other",
                               TRUE ~ sectorVL))

indi_ns_ss1$sAE <- ifelse(indi_ns_ss1$sectorVLm == "AltreIndustrie", 1, 0)
indi_ns_ss1$sAS <- ifelse(indi_ns_ss1$sectorVLm == "AltriServizi", 1, 0)
indi_ns_ss1$sCTL <- ifelse(indi_ns_ss1$sectorVLm == "CommercioTempoLib", 1, 0)
indi_ns_ss1$sC <- ifelse(indi_ns_ss1$sectorVLm == "Costruzione", 1, 0)
indi_ns_ss1$sIL <- ifelse(indi_ns_ss1$sectorVLm == "IngrossoLogistica", 1, 0)
indi_ns_ss1$sMII <- ifelse(indi_ns_ss1$sectorVLm == "MadeInItaly", 1, 0)
indi_ns_ss1$sMM <- ifelse(indi_ns_ss1$sectorVLm == "MetalMeccanico", 1, 0)
indi_ns_ss1$sX <- ifelse(indi_ns_ss1$sectorVLm == "Pre36_Other", 1, 0)
indi_ns_ss1$sSP <- ifelse(indi_ns_ss1$sectorVLm == "ServiziPersona", 1, 0)
indi_ns_ss1$sTA <- ifelse(indi_ns_ss1$sectorVLm == "TerziarioAvanzato", 1, 0)


v1u_sAE <- rdrobust(y = indi_ns_ss1$sAE, x = indi_ns_ss1$scoringD1_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")

summary(v1u_sAE)


v1u_sAS <- rdrobust(y = indi_ns_ss1$sAS, x = indi_ns_ss1$scoringD1_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(v1u_sAS)


v1u_sCTL <- rdrobust(y = indi_ns_ss1$sCTL, x = indi_ns_ss1$scoringD1_0,
                     kernel = "triangular",
                     c = 0, p = 1, bwselect = "mserd")
summary(v1u_sCTL)


v1u_sC <- rdrobust(y = indi_ns_ss1$sC, x = indi_ns_ss1$scoringD1_0,
                   kernel = "triangular",
                   c = 0, p = 1, bwselect = "mserd")
summary(v1u_sC)


v1u_sIL <- rdrobust(y = indi_ns_ss1$sIL, x = indi_ns_ss1$scoringD1_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(v1u_sIL)


v1u_sMII <- rdrobust(y = indi_ns_ss1$sMII, x = indi_ns_ss1$scoringD1_0,
                     kernel = "triangular",
                     c = 0, p = 1, bwselect = "mserd")
summary(v1u_sMII)


v1u_sX <- rdrobust(y = indi_ns_ss1$sX, x = indi_ns_ss1$scoringD1_0,
                   kernel = "triangular",
                   c = 0, p = 1, bwselect = "mserd")
summary(v1u_sX)


v1u_sSP <- rdrobust(y = indi_ns_ss1$sSP, x = indi_ns_ss1$scoringD1_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(v1u_sSP)

v1u_sTA <- rdrobust(y = indi_ns_ss1$sTA, x = indi_ns_ss1$scoringD1_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(v1u_sTA)

## Export
tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[, 1],
    std.error = model$se[, 1],
    p.value = model$pv[3]
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    Bandwidth = round(model$bws[1], 3),
    N_h = sum(model$N_h),
    Kernel = model$kernel,
    Polynomial_degree = round(model$p)
  )
  ret
}

cm <- c('Conventional' = 'Treatment')

# Table A6
#modelsummary(list(v1u_sexF, val1u_age, v1u_foreignF,
#                  v1u_elementareF, v1u_mediaF, v1u_qualificaF, v1u_diplomaF, v1u_laureaF,
#                  v1u_disaF,
#                  v1u_o2, v1u_o3, v1u_o4, v1u_o5, v1u_o6, v1u_o7, v1u_o8, v1u_oX,
#                  v1u_sAE, v1u_sAS, v1u_sCTL, v1u_sC, v1u_sIL, v1u_sMII, v1u_sX, v1u_sSP, v1u_sTA),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "balance_D1_d1kT.xlsx")

modelsummary(list(v1u_sexF, val1u_age, v1u_foreignF,
                  v1u_elementareF, v1u_mediaF, v1u_qualificaF, v1u_diplomaF, v1u_laureaF,
                  v1u_disaF,
                  v1u_o2, v1u_o3, v1u_o4, v1u_o5, v1u_o6, v1u_o7, v1u_o8, v1u_oX,
                  v1u_sAE, v1u_sAS, v1u_sCTL, v1u_sC, v1u_sIL, v1u_sMII, v1u_sX, v1u_sSP, v1u_sTA),
             statistic = "std.error", coef_map = cm,
             stars = c('*' = .1, '**' = .05, '***' = 0.01))

## Size of the problem: standardized mean difference for variable AGE
# see specification of the Absolute Standardized Difference (ASD) in
# Zhou, T., Tong, G., Li, F., & Thomas, L. E. (2022). PSweight: An R Package for Propensity Score Weighting Analysis. R Journal, 14(1).

denom_inside <- ( var(indi_ns_ss1$eta[indi_ns_ss1$scoringD1_0 > 0 & indi_ns_ss1$scoringD1_0 < 0.052]) +
                    var(indi_ns_ss1$eta[indi_ns_ss1$scoringD1_0 < 0 & indi_ns_ss1$scoringD1_0 > -0.052]) ) / 2
round(1.071/sqrt(denom_inside), 4)

### PLAUSIBILITY: Density around the cutoff -----------
depre1 <- rddensity(indi_ns$indice_previsto, massPoints = F, c = 0.4)
c1plot <- rdplotdensity(depre1, indi_ns$indice_previsto)


# Figure A5(a)
c1plot$Estplot +
  geom_vline(xintercept = 0.4, linewidth = 0.5) +
  coord_cartesian(ylim = c(0, 3)) +
  labs(x = "Original score", y = "Number of observations") # Saved at 395x300



## 4.2. Treatment 2 (D2: C vs. B) ----------

### Quantitative outcomes on employment -----------------
### Outcome 1: days worked post months [1, 6]
rddei6_p1kT <- rdrobust(y = indi_ns_ss2$post_interval6, x = indi_ns_ss2$scoringD2_0,
                        kernel = "triangular",
                        c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(rddei6_p1kT)

rddei6_p2kT <- rdrobust(y = indi_ns_ss2$post_interval6, x = indi_ns_ss2$scoringD2_0,
                        kernel = "triangular",
                        c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(rddei6_p2kT)

# Slight change at the optimal bw
srddei6_p1kT <- rdrobust(y = indi_ns_ss2$post_interval6, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 1, cluster = NULL,
                         h = rddei6_p1kT$bws[1,1] - 0.01)
summary(srddei6_p1kT)

srddei6_p2kT <- rdrobust(y = indi_ns_ss2$post_interval6, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 2, cluster = NULL,
                         h = rddei6_p2kT$bws[1,1] - 0.01)
summary(srddei6_p2kT)

# Table B5
#modelsummary(list(rddei6_p1kT, rddei6_p2kT, srddei6_p1kT, srddei6_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D2_post6_nocl.docx")


### Outcome 2: days worked post [7, 12] months
rddei12_p1kT <- rdrobust(y = indi_ns_ss2$post_interval712, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(rddei12_p1kT)

rddei12_p2kT <- rdrobust(y = indi_ns_ss2$post_interval712, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(rddei12_p2kT)

# Slight changes at the optimal bw
srddei12_p1kT <- rdrobust(y = indi_ns_ss2$post_interval712, x = indi_ns_ss2$scoringD2_0,
                          kernel = "triangular",
                          c = 0, p = 1, cluster = NULL,
                          h = rddei12_p1kT$bws[1,1] - 0.01)
summary(srddei12_p1kT)

srddei12_p2kT <- rdrobust(y = indi_ns_ss2$post_interval712, x = indi_ns_ss2$scoringD2_0,
                          kernel = "triangular",
                          c = 0, p = 2, cluster = NULL,
                          h = rddei12_p2kT$bws[1,1] - 0.01)
summary(srddei12_p2kT)

# Table B6
#modelsummary(list(rddei12_p1kT, rddei12_p2kT, srddei12_p1kT, srddei12_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D2_post712_nocl.docx")


### Outcome: days worked post [13, 18] months
rddei18_p1kT <- rdrobust(y = indi_ns_ss2$post_interval1318, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(rddei18_p1kT)

rddei18_p2kT <- rdrobust(y = indi_ns_ss2$post_interval1318, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(rddei18_p2kT)

# Slight change at the bw
srddei18_p1kT <- rdrobust(y = indi_ns_ss2$post_interval1318, x = indi_ns_ss2$scoringD2_0,
                          kernel = "triangular",
                          c = 0, p = 1, cluster = NULL,
                          h = rddei18_p1kT$bws[1,1] - 0.01)
summary(srddei18_p1kT)

srddei18_p2kT <- rdrobust(y = indi_ns_ss2$post_interval1318, x = indi_ns_ss2$scoringD2_0,
                          kernel = "triangular",
                          c = 0, p = 2, cluster = NULL,
                          h = rddei18_p2kT$bws[1,1] - 0.01)
summary(srddei18_p2kT)

# Table B7
#modelsummary(list(rddei18_p1kT, rddei18_p2kT, srddei18_p1kT, srddei18_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D2_post1318_nocl.docx")



### Outcome 4: days worked post [19, 24] months (RDD)
rdd24_p1kT <- rdrobust(y = indi_ns_ss2$post_interval1924, x = indi_ns_ss2$scoringD2_0,
                       kernel = "triangular",
                       c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(rdd24_p1kT)

rdd24_p2kT <- rdrobust(y = indi_ns_ss2$post_interval1924, x = indi_ns_ss2$scoringD2_0,
                       kernel = "triangular",
                       c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(rdd24_p2kT)

# Slight change at the optimal bw
srdd24_p1kT <- rdrobust(y = indi_ns_ss2$post_interval1924, x = indi_ns_ss2$scoringD2_0,
                        kernel = "triangular",
                        c = 0, p = 1, cluster = NULL,
                        h = rdd24_p1kT$bws[1,1] - 0.01)
summary(srdd24_p1kT)

srdd24_p2kT <- rdrobust(y = indi_ns_ss2$post_interval1924, x = indi_ns_ss2$scoringD2_0,
                        kernel = "triangular",
                        c = 0, p = 2, cluster = NULL,
                        h = rdd24_p2kT$bws[1,1] - 0.01)
summary(srdd24_p2kT)

cm <- c('Conventional' = 'Treatment')

# Table B8
#modelsummary(list(rdd24_p1kT, rdd24_p2kT, srdd24_p1kT, srdd24_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D2_post1924_nocl.docx")


### Qualitative outcomes on employment ----------------
### Outcome 5: type of contract (longest one) with Xu's method
#### Joining datasets
indi_ns_ss2$id <- paste0(indi_ns_ss2$ide_soggetto, "_", indi_ns_ss2$axl_data_attribuzione)
indi_ns_ss2_c <- left_join(indi_ns_ss2, longestforjoin, by = "id")

# Classic classification of contracts
indi_ns_ss2_c$lc_AC <- ifelse(indi_ns_ss2_c$longestcontract == "AC", 1, 0)
indi_ns_ss2_c$lc_DC <- ifelse(indi_ns_ss2_c$longestcontract == "DC", 1, 0)
indi_ns_ss2_c$lc_FTC <- ifelse(indi_ns_ss2_c$longestcontract == "FTC", 1, 0)
indi_ns_ss2_c$lc_NE <- ifelse(indi_ns_ss2_c$longestcontract == "NE", 1, 0)
indi_ns_ss2_c$lc_OEC <- ifelse(indi_ns_ss2_c$longestcontract == "OEC", 1, 0)
indi_ns_ss2_c$lc_SC <- ifelse(indi_ns_ss2_c$longestcontract == "SC", 1, 0)


# Ris classification of contracts
indi_ns_ss2_c$lcris_OEC <- ifelse(indi_ns_ss2_c$longestcontract_ris == "OEC", 1, 0)
indi_ns_ss2_c$lcris_FTCm12 <- ifelse(indi_ns_ss2_c$longestcontract_ris == "FTCm12", 1, 0)
indi_ns_ss2_c$lcris_FTC612 <- ifelse(indi_ns_ss2_c$longestcontract_ris == "FTC612", 1, 0)


## Type of contract (binary specifications) in 1-24 time interval
oec_d2 <- rdcate(score = indi_ns_ss2_c$scoringD2_0,
                 outcome = indi_ns_ss2_c$lcris_OEC)
ftcm12_d2 <- rdcate(score = indi_ns_ss2_c$scoringD2_0,
                    outcome = indi_ns_ss2_c$lcris_FTCm12)
ftc612_d2 <- rdcate(score = indi_ns_ss2_c$scoringD2_0,
                    outcome = indi_ns_ss2_c$lcris_FTC612)


table_qualityD2 <- data.frame(treatment = c(rep("D2", 2)),
                              prOEC = c(paste0(oec_d2$pointest, oec_d2$stars),
                                        oec_d2$ci95),
                              prFTCm12 = c(paste0(ftcm12_d2$pointest, ftcm12_d2$stars),
                                           ftcm12_d2$ci95),
                              prFTC612 = c(paste0(ftc612_d2$pointest, ftc612_d2$stars),
                                           ftc612_d2$ci95))

tablequality <- rbind(table_qualityD1, table_qualityD2)

# Table 5
# openxlsx::write.xlsx(tablequality, 'tablequality.xlsx')

### Mechanisms: hours of treatment -----------------
### Outcome HS: actual treatment received of job search measures
actualtr_p1kT <-  rdrobust(y = indi_ns_ss2$jshours, x = indi_ns_ss2$scoringD2_0,
                           kernel = "triangular",
                           c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(actualtr_p1kT)

actualtr_p2kT <- rdrobust(y = indi_ns_ss2$jshours, x = indi_ns_ss2$scoringD2_0,
                          kernel = "triangular",
                          c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(actualtr_p2kT)

# Slight changes at the optimal bw
sactualtr_p1kT <-  rdrobust(y = indi_ns_ss2$jshours, x = indi_ns_ss2$scoringD2_0,
                            kernel = "triangular",
                            c = 0, p = 1, cluster = NULL,
                            h = actualtr_p1kT$bws[1,1] - 0.01)
summary(sactualtr_p1kT)

sactualtr_p2kT <- rdrobust(y = indi_ns_ss2$jshours, x = indi_ns_ss2$scoringD2_0,
                           kernel = "triangular",
                           c = 0, p = 2, cluster = NULL,
                           h = actualtr_p2kT$bws[1,1] - 0.01)
summary(sactualtr_p2kT)


# Table B19
#modelsummary(list(actualtr_p1kT, actualtr_p2kT, sactualtr_p1kT, sactualtr_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D2_receivedjsa_nocl.docx")

#### Outcome HT: actual treatment received of training measures
prevtr_p1kT <-  rdrobust(y = indi_ns_ss2$attiv_form_ore_prev, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(prevtr_p1kT)

prevtr_p2kT <- rdrobust(y = indi_ns_ss2$attiv_form_ore_prev, x = indi_ns_ss2$scoringD2_0,
                        kernel = "triangular",
                        c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(prevtr_p2kT)

# Slight changes at the optimal bw
sprevtr_p1kT <-  rdrobust(y = indi_ns_ss2$attiv_form_ore_prev, x = indi_ns_ss2$scoringD2_0,
                          kernel = "triangular",
                          c = 0, p = 1, cluster = NULL,
                          h = prevtr_p1kT$bws[1,1] - 0.01)
summary(sprevtr_p1kT)

sprevtr_p2kT <- rdrobust(y = indi_ns_ss2$attiv_form_ore_prev, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 2, cluster = NULL,
                         h = prevtr_p2kT$bws[1,1] - 0.01)
summary(sprevtr_p2kT)

# Table B20
#modelsummary(list(prevtr_p1kT, prevtr_p2kT, sprevtr_p1kT, sprevtr_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_D2_received_nocl.docx")



### ROBUSTNESS (LATE): Changing the bandwidth selector (one selector for each side) ---------- 

### Outcome: days worked post 6 months (rdd_2_d2_)
rdd_2_d2_ei6_p1kT <- rdrobust(y = indi_ns_ss2$post_interval6, x = indi_ns_ss2$scoringD2_0,
                        kernel = "triangular",
                        c = 0, p = 1, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_d2_ei6_p1kT)

rdd_2_d2_ei6_p2kT <- rdrobust(y = indi_ns_ss2$post_interval6, x = indi_ns_ss2$scoringD2_0,
                        kernel = "triangular",
                        c = 0, p = 2, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_d2_ei6_p2kT)

# Table B13
#modelsummary(list(rdd_2_d2_ei6_p1kT, rdd_2_d2_ei6_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_2_D2_post6_nocl.docx")

robust_stata_d2_1 <- data.frame(stata = c(tostata(rdd_2_d2_ei6_p1kT), tostata(rdd_2_d2_ei6_p2kT)))

#write.xlsx(robust_stata_d2_1, 'robust_stata_d2_1.xlsx')

### Outcome: days worked post [7, 12] months
rdd_2_d2_ei12_p1kT <- rdrobust(y = indi_ns_ss2$post_interval712, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 1, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_d2_ei12_p1kT)

rdd_2_d2_ei12_p2kT <- rdrobust(y = indi_ns_ss2$post_interval712, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 2, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_d2_ei12_p2kT)

# Table B14
#modelsummary(list(rdd_2_d2_ei12_p1kT, rdd_2_d2_ei12_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_2_D2_post712_nocl.docx")

robust_stata_d2_2 <- data.frame(stata = c(tostata(rdd_2_d2_ei12_p1kT), tostata(rdd_2_d2_ei12_p2kT)))

#write.xlsx(robust_stata_d2_2, 'robust_stata_d2_2.xlsx')


### Outcome: days worked post [13, 18] months
rdd_2_d2_ei18_p1kT <- rdrobust(y = indi_ns_ss2$post_interval1318, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 1, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_d2_ei18_p1kT)

rdd_2_d2_ei18_p2kT <- rdrobust(y = indi_ns_ss2$post_interval1318, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 2, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_d2_ei18_p2kT)

# Table B15
#modelsummary(list(rdd_2_d2_ei18_p1kT, rdd_2_d2_ei18_p2kT),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_2_D2_post1318_nocl.docx")

robust_stata_d2_3 <- data.frame(stata = c(tostata(rdd_2_d2_ei18_p1kT), tostata(rdd_2_d2_ei18_p2kT)))

#write.xlsx(robust_stata_d2_3, 'robust_stata_d2_3.xlsx')


### Outcome 4: days worked post [19, 24] months (rdd_2_d2_)
rdd_2_d2_24_p1kT <- rdrobust(y = indi_ns_ss2$post_interval1924, x = indi_ns_ss2$scoringD2_0,
                       kernel = "triangular",
                       c = 0, p = 1, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_d2_24_p1kT)

rdd_2_d2_24_p2kT <- rdrobust(y = indi_ns_ss2$post_interval1924, x = indi_ns_ss2$scoringD2_0,
                       kernel = "triangular",
                       c = 0, p = 2, bwselect = "msetwo", cluster = NULL)
summary(rdd_2_d2_24_p2kT)

# Table B16
#modelsummary(list(rdd_2_d2_24_p1kT, rdd_2_d2_24_p2kT, rdd_2_d2_24_p1kU, rdd_2_d2_24_p2kU),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "cont_2_D2_post1924_nocl.docx")

robust_stata_d2_4 <- data.frame(stata = c(tostata(rdd_2_d2_24_p1kT), tostata(rdd_2_d2_24_p2kT)))

#write.xlsx(robust_stata_d2_4, 'robust_stata_d2_4.xlsx')

### PLAUSIBILITY (LATE): Pseudo-treatments ----------------
indi_ns_ss2_rmT1 <- indi_ns_ss2 %>%
  filter(ppa_data_avvio_d > ymd("2017-06-30"))

#haven::write_dta(indi_ns_ss2[, c("scoringD2_0", "prewd1_6", "prewd7_12", "prewd13_18", "prewd19_24")], "indi_ns_ss2_pse_n2023_rmT1.dta")


le_val2_prewd16 <- rdrobust(y = indi_ns_ss2_rmT1$prewd1_6, x = indi_ns_ss2_rmT1$scoringD2_0,
                            kernel = "triangular",
                            c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(le_val2_prewd16)

le_val2_prewd16_2 <- rdrobust(y = indi_ns_ss2_rmT1$prewd1_6, x = indi_ns_ss2_rmT1$scoringD2_0,
                              kernel = "triangular",
                              c = 0, p = 2, bwselect = "mserd", cluster = NULL)
summary(le_val2_prewd16_2)

### PLAUSIBILITY: Checking balance in pretreatment variables ----------------
tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[, 1],
    p.value = model$pv[3],
    conf.low = model$ci[3, 1],
    conf.high = model$ci[3, 2]
  )
  row.names(ret) <- NULL
  ret
}

#glance.rdrobust <- function(model, ...) {
#  ret <- data.frame(
#    Bandwidth = model$bws[1],
#    N_h = sum(model$N_h)
#  )
#  ret
#}

## Continuous outcome variables (age)
vd2u_age <- rdrobust(y = indi_ns_ss2$eta, x = indi_ns_ss2$scoringD2_0,
                     kernel = "triangular",
                     c = 0, p = 1, bwselect = "mserd")
summary(vd2u_age)



## Categorical outcome variables (sex, foreign, studies, disability, lastoccupation, last sector)
# Data preparation 
### Sex 
indi_ns_ss2$sex_f <- ifelse(indi_ns_ss2$genere == "F", 1, 0)
#indi_ns_ss2$sex_f_f <- as.factor(indi_ns_ss2$sex_f)


vd2u_sexF <- rdrobust(y = indi_ns_ss2$sex_f, x = indi_ns_ss2$scoringD2_0,
                      kernel = "triangular",
                      c = 0, p = 1, bwselect = "mserd")

summary(vd2u_sexF)

### Foreign
indi_ns_ss2$foreign <- ifelse(indi_ns_ss2$flg_italiano == "no", 1, 0)

vd2u_foreignF <- rdrobust(y = indi_ns_ss2$foreign, x = indi_ns_ss2$scoringD2_0,
                          kernel = "triangular",
                          c = 0, p = 1, bwselect = "mserd")

summary(vd2u_foreignF)

### Education 
indi_ns_ss2 <- indi_ns_ss2 %>% mutate(studio2_groupedX = case_when(studio2 == "Licenza elementare" ~ "1_LicElementare",
                                                                   studio2 == "Licenza media" ~ "2_LicMedia",
                                                                   studio2 %in% c("Diploma di qualifica professionale") ~ "3_QualificaProf",
                                                                   studio2 %in% c("Diploma Tecnico", "Diploma Professionale", "Diploma Liceale", "Diploma universitario",
                                                                                  "Diploma Conservatorio musicale",
                                                                                  "Maestro d'arte", "Scuola magistrale (triennale)", "Diploma di istruzione artistica", "Diploma interprete, traduttore, archivista") ~ "4_Diploma",
                                                                   studio2 %in% c("Laurea I livello (triennale)", "Laurea - vecchio o nuovo ordinamento", "Post laurea") ~ "5_Laurea",
                                                                   TRUE ~ "Nondetermined"))
freq(indi_ns_ss2$studio2_groupedX)

indi_ns_ss2$elementare <- ifelse(indi_ns_ss2$studio2_groupedX == "1_LicElementare", 1, 0)
indi_ns_ss2$media <- ifelse(indi_ns_ss2$studio2_groupedX == "2_LicMedia", 1, 0)
indi_ns_ss2$qualifica <- ifelse(indi_ns_ss2$studio2_groupedX == "3_QualificaProf", 1, 0)
indi_ns_ss2$diploma <- ifelse(indi_ns_ss2$studio2_groupedX == "4_Diploma", 1, 0)
indi_ns_ss2$laurea <- ifelse(indi_ns_ss2$studio2_groupedX == "5_Laurea", 1, 0)

#### elementare 


vd2u_elementareF <- rdrobust(y = indi_ns_ss2$elementare, x = indi_ns_ss2$scoringD2_0,
                             kernel = "triangular",
                             c = 0, p = 1, bwselect = "mserd")

summary(vd2u_elementareF)

#### media 

vd2u_mediaF <- rdrobust(y = indi_ns_ss2$media, x = indi_ns_ss2$scoringD2_0,
                        kernel = "triangular",
                        c = 0, p = 1, bwselect = "mserd")

summary(vd2u_mediaF)


#### qualifica 


vd2u_qualificaF <- rdrobust(y = indi_ns_ss2$qualifica, x = indi_ns_ss2$scoringD2_0,
                            kernel = "triangular",
                            c = 0, p = 1, bwselect = "mserd")

summary(vd2u_qualificaF)



#### diploma 


vd2u_diplomaF <- rdrobust(y = indi_ns_ss2$diploma, x = indi_ns_ss2$scoringD2_0,
                          kernel = "triangular",
                          c = 0, p = 1, bwselect = "mserd")

summary(vd2u_diplomaF)



#### laurea

vd2u_laureaF <- rdrobust(y = indi_ns_ss2$laurea, x = indi_ns_ss2$scoringD2_0,
                         kernel = "triangular",
                         c = 0, p = 1, bwselect = "mserd")

summary(vd2u_laureaF)



### Disability
indi_ns_ss2$disa <- ifelse(indi_ns_ss2$disability == "Yes", 1, 0)


vd2u_disaF <- rdrobust(y = indi_ns_ss2$disa, x = indi_ns_ss2$scoringD2_0,
                       kernel = "triangular",
                       c = 0, p = 1, bwselect = "mserd")

summary(vd2u_disaF)

### Occupation of last employment 
indi_ns_ss2 <- left_join(indi_ns_ss2, indi_ns[, c("ide_data", "qualifica1dm")], by = "ide_data")

indi_ns_ss2$o2I <- ifelse(indi_ns_ss2$qualifica1dm == "2_Intellectual", 1, 0)
indi_ns_ss2$o3T <- ifelse(indi_ns_ss2$qualifica1dm == "3_Technical", 1, 0)
indi_ns_ss2$o4WLS <- ifelse(indi_ns_ss2$qualifica1dm == "4_WhiteLowSkilled", 1, 0)
indi_ns_ss2$o5WHS <- ifelse(indi_ns_ss2$qualifica1dm == "5_WhiteHighSkilled", 1, 0)
indi_ns_ss2$o6BHS <- ifelse(indi_ns_ss2$qualifica1dm == "6_BlueHighSkilled", 1, 0)
indi_ns_ss2$o7BMS <- ifelse(indi_ns_ss2$qualifica1dm == "7_BlueMediumSkilled", 1, 0)
indi_ns_ss2$o8NS <- ifelse(indi_ns_ss2$qualifica1dm == "8_NonSkilled", 1, 0)
indi_ns_ss2$oX <- ifelse(indi_ns_ss2$qualifica1dm == "Pre36_Other", 1, 0)

vd2u_o2 <- rdrobust(y = indi_ns_ss2$o2I, x = indi_ns_ss2$scoringD2_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")

summary(vd2u_o2)


vd2u_o3 <- rdrobust(y = indi_ns_ss2$o3T, x = indi_ns_ss2$scoringD2_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(vd2u_o3)


vd2u_o4 <- rdrobust(y = indi_ns_ss2$o4WLS, x = indi_ns_ss2$scoringD2_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(vd2u_o4)


vd2u_o5 <- rdrobust(y = indi_ns_ss2$o5WHS, x = indi_ns_ss2$scoringD2_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(vd2u_o5)


vd2u_o6 <- rdrobust(y = indi_ns_ss2$o6BHS, x = indi_ns_ss2$scoringD2_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(vd2u_o6)


vd2u_o7 <- rdrobust(y = indi_ns_ss2$o7BMS, x = indi_ns_ss2$scoringD2_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(vd2u_o7)


vd2u_o8 <- rdrobust(y = indi_ns_ss2$o8NS, x = indi_ns_ss2$scoringD2_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(vd2u_o8)


vd2u_oX <- rdrobust(y = indi_ns_ss2$oX, x = indi_ns_ss2$scoringD2_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(vd2u_oX)

### Sector of last employment 
indi_ns_ss2 <- indi_ns_ss2 %>%
  mutate(sectorVLm = case_when(sectorVL %in% c("Agricoltura", "Unobserved") ~ "Pre36_Other",
                               TRUE ~ sectorVL))

indi_ns_ss2$sAE <- ifelse(indi_ns_ss2$sectorVLm == "AltreIndustrie", 1, 0)
indi_ns_ss2$sAS <- ifelse(indi_ns_ss2$sectorVLm == "AltriServizi", 1, 0)
indi_ns_ss2$sCTL <- ifelse(indi_ns_ss2$sectorVLm == "CommercioTempoLib", 1, 0)
indi_ns_ss2$sC <- ifelse(indi_ns_ss2$sectorVLm == "Costruzione", 1, 0)
indi_ns_ss2$sIL <- ifelse(indi_ns_ss2$sectorVLm == "IngrossoLogistica", 1, 0)
indi_ns_ss2$sMII <- ifelse(indi_ns_ss2$sectorVLm == "MadeInItaly", 1, 0)
indi_ns_ss2$sMM <- ifelse(indi_ns_ss2$sectorVLm == "MetalMeccanico", 1, 0)
indi_ns_ss2$sX <- ifelse(indi_ns_ss2$sectorVLm == "Pre36_Other", 1, 0)
indi_ns_ss2$sSP <- ifelse(indi_ns_ss2$sectorVLm == "ServiziPersona", 1, 0)
indi_ns_ss2$sTA <- ifelse(indi_ns_ss2$sectorVLm == "TerziarioAvanzato", 1, 0)


vd2u_sAE <- rdrobust(y = indi_ns_ss2$sAE, x = indi_ns_ss2$scoringD2_0,
                     kernel = "triangular",
                     c = 0, p = 1, bwselect = "mserd")

summary(vd2u_sAE)


vd2u_sAS <- rdrobust(y = indi_ns_ss2$sAS, x = indi_ns_ss2$scoringD2_0,
                     kernel = "triangular",
                     c = 0, p = 1, bwselect = "mserd")
summary(vd2u_sAS)


vd2u_sCTL <- rdrobust(y = indi_ns_ss2$sCTL, x = indi_ns_ss2$scoringD2_0,
                      kernel = "triangular",
                      c = 0, p = 1, bwselect = "mserd")
summary(vd2u_sCTL)


vd2u_sC <- rdrobust(y = indi_ns_ss2$sC, x = indi_ns_ss2$scoringD2_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(vd2u_sC)


vd2u_sIL <- rdrobust(y = indi_ns_ss2$sIL, x = indi_ns_ss2$scoringD2_0,
                     kernel = "triangular",
                     c = 0, p = 1, bwselect = "mserd")
summary(vd2u_sIL)


vd2u_sMII <- rdrobust(y = indi_ns_ss2$sMII, x = indi_ns_ss2$scoringD2_0,
                      kernel = "triangular",
                      c = 0, p = 1, bwselect = "mserd")
summary(vd2u_sMII)


vd2u_sX <- rdrobust(y = indi_ns_ss2$sX, x = indi_ns_ss2$scoringD2_0,
                    kernel = "triangular",
                    c = 0, p = 1, bwselect = "mserd")
summary(vd2u_sX)


vd2u_sSP <- rdrobust(y = indi_ns_ss2$sSP, x = indi_ns_ss2$scoringD2_0,
                     kernel = "triangular",
                     c = 0, p = 1, bwselect = "mserd")
summary(vd2u_sSP)

vd2u_sTA <- rdrobust(y = indi_ns_ss2$sTA, x = indi_ns_ss2$scoringD2_0,
                     kernel = "triangular",
                     c = 0, p = 1, bwselect = "mserd")
summary(vd2u_sTA)

## Export 
# Table A6
#modelsummary(list(vd2u_sexF, vd2u_age, vd2u_foreignF,
#                  vd2u_elementareF, vd2u_mediaF, vd2u_qualificaF, vd2u_diplomaF, vd2u_laureaF,
#                  vd2u_disaF,
#                  vd2u_o2, vd2u_o3, vd2u_o4, vd2u_o5, vd2u_o6, vd2u_o7, vd2u_o8, vd2u_oX,
#                  vd2u_sAE, vd2u_sAS, vd2u_sCTL, vd2u_sC, vd2u_sIL, vd2u_sMII, vd2u_sX, vd2u_sSP, vd2u_sTA),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "balance_D2_d1kT.xlsx")

modelsummary(list(vd2u_sexF, vd2u_age, vd2u_foreignF,
                  vd2u_elementareF, vd2u_mediaF, vd2u_qualificaF, vd2u_diplomaF, vd2u_laureaF,
                  vd2u_disaF,
                  vd2u_o2, vd2u_o3, vd2u_o4, vd2u_o5, vd2u_o6, vd2u_o7, vd2u_o8, vd2u_oX,
                  vd2u_sAE, vd2u_sAS, vd2u_sCTL, vd2u_sC, vd2u_sIL, vd2u_sMII, vd2u_sX, vd2u_sSP, vd2u_sTA),
             statistic = "std.error", coef_map = cm,
             stars = c('*' = .1, '**' = .05, '***' = 0.01))

## Size of the problem 
denom_inside2 <- ( var(indi_ns_ss2$eta[indi_ns_ss2$scoringD2_0 > 0 & indi_ns_ss2$scoringD2_0 < 0.062]) +
                     var(indi_ns_ss2$eta[indi_ns_ss2$scoringD2_0 < 0 & indi_ns_ss2$scoringD2_0 > -0.062]) ) / 2
round(0.499/sqrt(denom_inside2), 4)

### PLAUSIBILITY: Density around the cutoff -----------
depre2 <- rddensity(indi_ns$indice_previsto, massPoints = T, c = 0.6)
c2plot <- rdplotdensity(depre2, indi_ns$indice_previsto)


# Figure A5(b)
c2plot$Estplot +
  geom_vline(xintercept = 0.6, linewidth = 0.5) +
  coord_cartesian(ylim = c(0, 3)) +
  labs(x = "Original score", y = "Number of observations") # Saved at 395x300

### ROBUSTNESS check for LATE_j (D1 and D2): changing the estimator --------
tidy.rdrobust <- function(model, ...) {
  ret <- data.frame(
    term = row.names(model$coef),
    estimate = model$coef[, 1],
    std.error = model$se[, 1],
    p.value = model$pv[3]
  )
  row.names(ret) <- NULL
  ret
}

glance.rdrobust <- function(model, ...) {
  ret <- data.frame(
    Bandwidth = round(model$bws[1], 3),
    N_h = sum(model$N_h),
    Kernel = model$kernel,
    Polynomial_degree = round(model$p)
  )
  ret
}

### Outcome 1: lcris_OEC
rdd_classic_oec1 <- rdrobust(y = indi_ns_ss1_c$lcris_OEC, x = indi_ns_ss1_c$scoringD1_0,
                             kernel = "triangular",
                             c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(rdd_classic_oec1)

rdd_classic_oec2 <- rdrobust(y = indi_ns_ss2_c$lcris_OEC, x = indi_ns_ss2_c$scoringD2_0,
                             kernel = "triangular",
                             c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(rdd_classic_oec2)

cm <- c('Conventional' = 'Treatment')

# Table B22 (column 1)
#modelsummary(list(rdd_classic_oec1, rdd_classic_oecU1),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "rdd_classic_oec_d1d2.docx")

### Outcome 2: lcris_FTCm12
rdd_classic_ftcm12_1 <- rdrobust(y = indi_ns_ss1_c$lcris_FTCm12, x = indi_ns_ss1_c$scoringD1_0,
                                 kernel = "triangular",
                                 c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(rdd_classic_ftcm12_1)

rdd_classic_ftcm12_2 <- rdrobust(y = indi_ns_ss2_c$lcris_FTCm12, x = indi_ns_ss2_c$scoringD2_0,
                                 kernel = "triangular",
                                 c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(rdd_classic_ftcm12_2)

# Table B22 (column 2)
#modelsummary(list(rdd_classic_ftcm12_1,
#                  rdd_classic_ftcm12_2),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "rdd_classic_ftcm12__d1d2.docx")

### Outcome 3: lcris_FTC612
rdd_classic_FTC612_1 <- rdrobust(y = indi_ns_ss1_c$lcris_FTC612, x = indi_ns_ss1_c$scoringD1_0,
                                 kernel = "triangular",
                                 c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(rdd_classic_FTC612_1)

rdd_classic_FTC612_2 <- rdrobust(y = indi_ns_ss2_c$lcris_FTC612, x = indi_ns_ss2_c$scoringD2_0,
                                 kernel = "triangular",
                                 c = 0, p = 1, bwselect = "mserd", cluster = NULL)
summary(rdd_classic_FTC612_2)

# Table B22 (column 3)
#modelsummary(list(rdd_classic_FTC612_1,
#                  rdd_classic_FTC612_2),
#             statistic = "std.error", coef_map = cm,
#             stars = c('*' = .1, '**' = .05, '***' = 0.01),
#             output = "rdd_classic_FTC612__d1d2.docx")


# 5. ANALYSIS: LQTE ------------
indi_ns_ss1$treatedD1 <- ifelse(indi_ns_ss1$scoringD1_0 > 0, TRUE, FALSE)
indi_ns_ss2$treatedD2 <- ifelse(indi_ns_ss2$scoringD2_0 > 0, TRUE, FALSE)

running1 <- indi_ns_ss1$scoringD1_0
outcome1 <- indi_ns_ss1$post_interval6
outcome2 <- indi_ns_ss1$post_interval712
outcome3 <- indi_ns_ss1$post_interval1318
outcome4 <- indi_ns_ss1$post_interval1924
d1 <- indi_ns_ss1$treatedD1
x0 <- 0

running2 <- indi_ns_ss2$scoringD2_0
outcome1_d2 <- indi_ns_ss2$post_interval6
outcome2_d2 <- indi_ns_ss2$post_interval712
outcome3_d2 <- indi_ns_ss2$post_interval1318
outcome4_d2 <- indi_ns_ss2$post_interval1924
d2 <- indi_ns_ss2$treatedD2

contenedorqte <- data.frame(treat_out = NA,
                            hint = NA, hbdy = NA, hik = NA,
                            qte_q1 = NA, qte_q2 = NA, qte_q3 = NA)

# Estimation and inference following Qu and Yoon (2019)
source("C:/Users/1604834/OneDrive - UAB/Methodology/Causal inference/Regression discontinuity/Distributional effects/2018 Qu Yoon supplementary/code/qte_rdd.R")
## Following the same logic as Cattaneo et al. (2020, p.65), the point estimated obtained from a MSE-optimal bw is not bias-corrected.


## 5.1. D1 ----------
#out1 d1
bwd1o1 <- rdd.bandwidth(running1, outcome1, d1,
                        x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4,5), 
                        val=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08), kr = 3)

bwd1o1_min <- min(as.numeric(unname(bwd1o1[c(3, 4, 5)])))
bwd1o1_min2 <- nth(as.numeric(unname(bwd1o1[c(3, 4, 5)])), 2)

qte_d1o1 <- rdd.qte(running1, outcome1, d1,
                    x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o1_min, kr=3,bias=0,eql=0)

contenedorqte[1, 1] <- "d1_out1"
contenedorqte[1, c(2,3,4)] <- bwd1o1[c(3,4,5)]
contenedorqte[1, c(5:7)] <- qte_d1o1$qte
ucipci_d1o1 <- as.data.frame(cbind(qte_d1o1$uci, qte_d1o1$pci))
ucipci_d1o1al90 <- as.data.frame(cbind(qte_d1o1al90$uci, qte_d1o1al90$pci))
#openxlsx::write.xlsx(contenedorqte, 'contenedorqte_d1o1.xlsx')
#openxlsx::write.xlsx(ucipci_d1o1, 'ucipci_d1o1.xlsx')
#openxlsx::write.xlsx(ucipci_d1o1al90, 'ucipci_d1o1al90.xlsx')

Wd1o1_sh <- Wald(running1, outcome1, d1,
                 x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,bandw=bwd1o1_min, bandw2 = bwd1o1_min,
                 kr=3,eql=0, test.type = c(1, 2))

## robustness to bias
qte_d1o1_pct17b1e1 <- rdd.qte(running1, outcome1, d1,
                              x.eval=0, alpha=0.95, tt=c(0.1,0.9),
                              m=17,bandw=bwd1o1_min, kr=3,bias=1,eql=1)

#out2 (val from bws of average effects)
bwd1o2 <- rdd.bandwidth(running1, outcome2, d1,
                        x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4,5), 
                        val=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1), kr = 3)

bwd1o2_min <- min(as.numeric(unname(bwd1o2[c(3, 4, 5)])))

qte_d1o2 <- rdd.qte(running1, outcome2, d1,
                    x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o2_min, kr=3,bias=0,eql=0)

contenedorqte[1, 1] <- "d1_out2"
contenedorqte[1, c(2,3,4)] <- bwd1o2[c(3,4,5)]
contenedorqte[1, c(5:7)] <- qte_d1o2$qte
ucipci_d1o2 <- as.data.frame(cbind(qte_d1o2$uci, qte_d1o2$pci))
ucipci_d1o2al90 <- as.data.frame(cbind(qte_d1o2al90$uci, qte_d1o2al90$pci))
#openxlsx::write.xlsx(contenedorqte, 'contenedorqte_d1o2.xlsx')
#openxlsx::write.xlsx(ucipci_d1o2, 'ucipci_d1o2.xlsx')
#openxlsx::write.xlsx(ucipci_d1o2al90, 'ucipci_d1o2al90.xlsx')

Wd1o2_sh <- Wald(running1, outcome2, d1,
                 x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,bandw=bwd1o2_min, bandw2 = bwd1o2_min,
                 kr=3,eql=0, test.type = c(1, 2))

## robustness to bias
qte_d1o2_pct17b1e1 <- rdd.qte(running1, outcome2, d1,
                              x.eval=0, alpha=0.95, tt=c(0.1,0.9),
                              m=17,bandw=bwd1o2_min, kr=3,bias=1,eql=1)

table_bc_d1o2 <- data.frame(tau = qte_d1o2_pct17b1e1$taus,
                            qte = round(qte_d1o2_pct17b1e1$qte, 3),
                            uci_l = round(qte_d1o2_pct17b1e1$uci[, 1], 3),
                            uci_r = round(qte_d1o2_pct17b1e1$uci[, 2], 3))

table_bc_d1o2$sign95 <- ifelse(table_bc_d1o2$uci_l > 0 | table_bc_d1o2$uci_r < 0, "**", "")

#out3 (val from bws of average effects)
bwd1o3 <- rdd.bandwidth(running1, outcome3, d1,
                        x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4,5), 
                        val=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12), kr = 3)

bwd1o3_min <- min(as.numeric(unname(bwd1o3[c(3, 4, 5)])))

qte_d1o3 <- rdd.qte(running1, outcome3, d1,
                    x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o3_min, kr=3,bias=0,eql=0)

contenedorqte[1, 1] <- "d1_out3"
contenedorqte[1, c(2,3,4)] <- bwd1o3[c(3,4,5)]
contenedorqte[1, c(5:7)] <- qte_d1o3$qte
ucipci_d1o3 <- as.data.frame(cbind(qte_d1o3$uci, qte_d1o3$pci))
ucipci_d1o3al90 <- as.data.frame(cbind(qte_d1o3al90$uci, qte_d1o3al90$pci))
#openxlsx::write.xlsx(contenedorqte, 'contenedorqte_d1o3.xlsx')
#openxlsx::write.xlsx(ucipci_d1o3, 'ucipci_d1o3.xlsx')
#openxlsx::write.xlsx(ucipci_d1o3al90, 'ucipci_d1o3al90.xlsx')

Wd1o3_sh <- Wald(running1, outcome3, d1,
                 x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,bandw=bwd1o3_min, bandw2 = bwd1o3_min,
                 kr=3,eql=0, test.type = c(1, 2))

## Further explanation
qte_d1o3_pct8 <- rdd.qte(running1, outcome3, d1,
                         x.eval=0, alpha=0.95, tt=c(0.2,0.9),
                         m=8,bandw=bwd1o3_min, kr=3,bias=0,eql=0)

qte_d1o3_pct20 <- rdd.qte(running1, outcome3, d1,
                          x.eval=0, alpha=0.95, tt=c(0.25,0.75),
                          m=20,bandw=bwd1o3_min, kr=3,bias=0,eql=0)

qte_d1o3_pct20b1e1 <- rdd.qte(running1, outcome3, d1,
                          x.eval=0, alpha=0.95, tt=c(0.25,0.75),
                          m=20,bandw=bwd1o3_min, kr=3,bias=1,eql=1)

qte_d1o3_pct17b1e1 <- rdd.qte(running1, outcome3, d1,
                          x.eval=0, alpha=0.95, tt=c(0.1,0.9),
                          m=17,bandw=bwd1o3_min, kr=3,bias=1,eql=1)

table_bc_d1o3 <- data.frame(tau = qte_d1o3_pct17b1e1$taus,
                            qte = round(qte_d1o3_pct17b1e1$qte, 3),
                            uci_l = round(qte_d1o3_pct17b1e1$uci[, 1], 3),
                            uci_r = round(qte_d1o3_pct17b1e1$uci[, 2], 3))

table_bc_d1o3$sign95 <- ifelse(table_bc_d1o3$uci_l > 0 | table_bc_d1o3$uci_r < 0, "**", "")

#out4 (val from bws of average effects)
bwd1o4 <- rdd.bandwidth(running1, outcome4, d1,
                        x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4,5), 
                        val=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09), kr = 3)

bwd1o4_min <- min(as.numeric(unname(bwd1o4[c(3, 4, 5)])))

qte_d1o4 <- rdd.qte(running1, outcome4, d1,
                    x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o4_min, kr=3,bias=0,eql=0)


contenedorqte[1, 1] <- "d1_out4"
contenedorqte[1, c(2,3,4)] <- bwd1o4[c(3,4,5)]
contenedorqte[1, c(5:7)] <- qte_d1o4$qte
ucipci_d1o4 <- as.data.frame(cbind(qte_d1o4$uci, qte_d1o4$pci))
ucipci_d1o4al90 <- as.data.frame(cbind(qte_d1o4al90$uci, qte_d1o4al90$pci))
#openxlsx::write.xlsx(contenedorqte, 'contenedorqte_d1o4.xlsx')
#openxlsx::write.xlsx(ucipci_d1o4, 'ucipci_d1o4.xlsx')
#openxlsx::write.xlsx(ucipci_d1o4al90, 'ucipci_d1o4al90.xlsx')

Wd1o4_sh <- Wald(running1, outcome4, d1,
                 x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,bandw=bwd1o4_min, bandw2 = bwd1o4_min,
                 kr=3,eql=0, test.type = c(1, 2))

## robustness to bias
qte_d1o4_pct17b1e1 <- rdd.qte(running1, outcome4, d1,
                              x.eval=0, alpha=0.95, tt=c(0.1,0.9),
                              m=17,bandw=bwd1o4_min, kr=3,bias=1,eql=1)

table_bc_d1o4 <- data.frame(tau = qte_d1o4_pct17b1e1$taus,
                            qte = round(qte_d1o4_pct17b1e1$qte, 3),
                            uci_l = round(qte_d1o4_pct17b1e1$uci[, 1], 3),
                            uci_r = round(qte_d1o4_pct17b1e1$uci[, 2], 3))

table_bc_d1o4$sign95 <- ifelse(table_bc_d1o4$uci_l > 0 | table_bc_d1o4$uci_r < 0, "**", "")

### Plausibility -----------
indi_ns_ss1_rmT1$treatedD1 <- ifelse(indi_ns_ss1_rmT1$scoringD1_0 > 0, TRUE, FALSE)

running1rmt1 <- indi_ns_ss1_rmT1$scoringD1_0
outcomepre1 <- indi_ns_ss1_rmT1$prewd1_6
d1rmt1 <- indi_ns_ss1_rmT1$treatedD1
#x0 <- 0

bwd1preo1 <- rdd.bandwidth(running1rmt1, outcomepre1, d1rmt1,
                        x.eval = 0, tt = c(0.25,0.75), m=3, method = c(1, 3,4,5), 
                        val=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08), kr = 3)

bwd1preo1_min <- min(as.numeric(unname(bwd1preo1[c(3, 4, 5)])))

qte_d1preo1 <- rdd.qte(running1rmt1, outcomepre1, d1rmt1,
                    x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,
                    bandw=bwd1preo1_min, kr=3,bias=0,eql=0)

Wd1preo1_sh <- Wald(running1rmt1, outcomepre1, d1rmt1,
                 x.eval=0, alpha=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                 bandw=bwd1preo1_min, bandw2 = bwd1preo1_min,
                 kr=3,eql=0, test.type = c(1, 2))

### Robustness check --------------
robus1qte <- data.frame(treat_out = NA, bw = NA, kernel = NA,
                            qte_q1 = NA, qte_q2 = NA, qte_q3 = NA, Wald_TS = NA, Wald_TH = NA)
robus2qte <- data.frame(treat_out = NA, bw = NA, kernel = NA,
                        qte_q1 = NA, qte_q2 = NA, qte_q3 = NA, Wald_TS = NA, Wald_TH = NA)
robus3qte <- data.frame(treat_out = NA, bw = NA, kernel = NA,
                        qte_q1 = NA, qte_q2 = NA, qte_q3 = NA, Wald_TS = NA, Wald_TH = NA)
robus4qte <- data.frame(treat_out = NA, bw = NA, kernel = NA,
                        qte_q1 = NA, qte_q2 = NA, qte_q3 = NA, Wald_TS = NA, Wald_TH = NA)

#out1 d1
bwd1o1_min2 <- nth(as.numeric(unname(bwd1o1[c(3, 4, 5)])), 2)

qte_d1o1G <- rdd.qte(running1, outcome1, d1,
                      x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o1_min, kr=1,bias=0,eql=0)

qte_d1o1_r <- rdd.qte(running1, outcome1, d1,
                    x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o1_min2, kr=3,bias=0,eql=0)

qte_d1o1_rG <- rdd.qte(running1, outcome1, d1,
                      x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o1_min2, kr=1,bias=0,eql=0)

Wd1o1_sh_G <- Wald(running1, outcome1, d1,
                 x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                 bandw=bwd1o1_min, bandw2 = bwd1o1_min,
                 kr=1,eql=0, test.type = c(1, 2))

Wd1o1_sh_r <- Wald(running1, outcome1, d1,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd1o1_min2, bandw2 = bwd1o1_min2,
                   kr=3,eql=0, test.type = c(1, 2))

Wd1o1_sh_rG <- Wald(running1, outcome1, d1,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd1o1_min2, bandw2 = bwd1o1_min2,
                   kr=1,eql=0, test.type = c(1, 2))


## Export
robus1qte[1:3, 1] <- "d1_out1"

robus1qte[1, c(2)] <- round(bwd1o1_min, 3)
robus1qte[2:3, c(2)] <- round(bwd1o1_min2, 3)

robus1qte[c(1, 3), c(3)] <- "Gaussian"
robus1qte[c(2), c(3)] <- "Epanechnikov"

robus1qte[1, c(4:6)] <- round(qte_d1o1G$qte, 3)
robus1qte[2, c(4:6)] <- round(qte_d1o1_r$qte, 3)
robus1qte[3, c(4:6)] <- round(qte_d1o1_rG$qte, 3)

robus1qte[1, 7] <- case_when(Wd1o1_sh_G$wald.robust.test[1] > Wd1o1_sh_G$wald.robust.crit[3, 1] ~ "***",
                             Wd1o1_sh_G$wald.robust.test[1] > Wd1o1_sh_G$wald.robust.crit[2, 1] ~ "**",
                             Wd1o1_sh_G$wald.robust.test[1] > Wd1o1_sh_G$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus1qte[1, 8] <- case_when(Wd1o1_sh_G$wald.robust.test[2] > Wd1o1_sh_G$wald.robust.crit[3, 2] ~ "***",
                             Wd1o1_sh_G$wald.robust.test[2] > Wd1o1_sh_G$wald.robust.crit[2, 2] ~ "**",
                             Wd1o1_sh_G$wald.robust.test[2] > Wd1o1_sh_G$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robus1qte[2, 7] <- case_when(Wd1o1_sh_r$wald.robust.test[1] > Wd1o1_sh_r$wald.robust.crit[3, 1] ~ "***",
                             Wd1o1_sh_r$wald.robust.test[1] > Wd1o1_sh_r$wald.robust.crit[2, 1] ~ "**",
                             Wd1o1_sh_r$wald.robust.test[1] > Wd1o1_sh_r$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus1qte[2, 8] <- case_when(Wd1o1_sh_r$wald.robust.test[2] > Wd1o1_sh_r$wald.robust.crit[3, 2] ~ "***",
                             Wd1o1_sh_r$wald.robust.test[2] > Wd1o1_sh_r$wald.robust.crit[2, 2] ~ "**",
                             Wd1o1_sh_r$wald.robust.test[2] > Wd1o1_sh_r$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robus1qte[3, 7] <- case_when(Wd1o1_sh_rG$wald.robust.test[1] > Wd1o1_sh_rG$wald.robust.crit[3, 1] ~ "***",
                             Wd1o1_sh_rG$wald.robust.test[1] > Wd1o1_sh_rG$wald.robust.crit[2, 1] ~ "**",
                             Wd1o1_sh_rG$wald.robust.test[1] > Wd1o1_sh_rG$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus1qte[3, 8] <- case_when(Wd1o1_sh_rG$wald.robust.test[2] > Wd1o1_sh_rG$wald.robust.crit[3, 2] ~ "***",
                             Wd1o1_sh_rG$wald.robust.test[2] > Wd1o1_sh_rG$wald.robust.crit[2, 2] ~ "**",
                             Wd1o1_sh_rG$wald.robust.test[2] > Wd1o1_sh_rG$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")


#openxlsx::write.xlsx(robus1qte, 'robus1qte.xlsx')


#out2 d1
bwd1o2_min2 <- nth(as.numeric(unname(bwd1o2[c(3, 4, 5)])), 2)

qte_d1o2G <- rdd.qte(running1, outcome2, d1,
                     x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o2_min, kr=1,bias=0,eql=0)

qte_d1o2_r <- rdd.qte(running1, outcome2, d1,
                      x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o2_min2, kr=3,bias=0,eql=0)

qte_d1o2_rG <- rdd.qte(running1, outcome2, d1,
                       x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o2_min2, kr=1,bias=0,eql=0)

Wd1o2_sh_G <- Wald(running1, outcome2, d1,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd1o2_min, bandw2 = bwd1o2_min,
                   kr=1,eql=0, test.type = c(1, 2))

Wd1o2_sh_r <- Wald(running1, outcome2, d1,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd1o2_min2, bandw2 = bwd1o2_min2,
                   kr=3,eql=0, test.type = c(1, 2))

Wd1o2_sh_rG <- Wald(running1, outcome2, d1,
                    x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                    bandw=bwd1o2_min2, bandw2 = bwd1o2_min2,
                    kr=1,eql=0, test.type = c(1, 2))


## Export
robus2qte[1:3, 1] <- "d1_out2"

robus2qte[1, c(2)] <- round(bwd1o2_min, 3)
robus2qte[2:3, c(2)] <- round(bwd1o2_min2, 3)

robus2qte[c(1, 3), c(3)] <- "Gaussian"
robus2qte[c(2), c(3)] <- "Epanechnikov"

robus2qte[1, c(4:6)] <- round(qte_d1o2G$qte, 3)
robus2qte[2, c(4:6)] <- round(qte_d1o2_r$qte, 3)
robus2qte[3, c(4:6)] <- round(qte_d1o2_rG$qte, 3)

robus2qte[1, 7] <- case_when(Wd1o2_sh_G$wald.robust.test[1] > Wd1o2_sh_G$wald.robust.crit[3, 1] ~ "***",
                             Wd1o2_sh_G$wald.robust.test[1] > Wd1o2_sh_G$wald.robust.crit[2, 1] ~ "**",
                             Wd1o2_sh_G$wald.robust.test[1] > Wd1o2_sh_G$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus2qte[1, 8] <- case_when(Wd1o2_sh_G$wald.robust.test[2] > Wd1o2_sh_G$wald.robust.crit[3, 2] ~ "***",
                             Wd1o2_sh_G$wald.robust.test[2] > Wd1o2_sh_G$wald.robust.crit[2, 2] ~ "**",
                             Wd1o2_sh_G$wald.robust.test[2] > Wd1o2_sh_G$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robus2qte[2, 7] <- case_when(Wd1o2_sh_r$wald.robust.test[1] > Wd1o2_sh_r$wald.robust.crit[3, 1] ~ "***",
                             Wd1o2_sh_r$wald.robust.test[1] > Wd1o2_sh_r$wald.robust.crit[2, 1] ~ "**",
                             Wd1o2_sh_r$wald.robust.test[1] > Wd1o2_sh_r$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus2qte[2, 8] <- case_when(Wd1o2_sh_r$wald.robust.test[2] > Wd1o2_sh_r$wald.robust.crit[3, 2] ~ "***",
                             Wd1o2_sh_r$wald.robust.test[2] > Wd1o2_sh_r$wald.robust.crit[2, 2] ~ "**",
                             Wd1o2_sh_r$wald.robust.test[2] > Wd1o2_sh_r$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robus2qte[3, 7] <- case_when(Wd1o2_sh_rG$wald.robust.test[1] > Wd1o2_sh_rG$wald.robust.crit[3, 1] ~ "***",
                             Wd1o2_sh_rG$wald.robust.test[1] > Wd1o2_sh_rG$wald.robust.crit[2, 1] ~ "**",
                             Wd1o2_sh_rG$wald.robust.test[1] > Wd1o2_sh_rG$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus2qte[3, 8] <- case_when(Wd1o2_sh_rG$wald.robust.test[2] > Wd1o2_sh_rG$wald.robust.crit[3, 2] ~ "***",
                             Wd1o2_sh_rG$wald.robust.test[2] > Wd1o2_sh_rG$wald.robust.crit[2, 2] ~ "**",
                             Wd1o2_sh_rG$wald.robust.test[2] > Wd1o2_sh_rG$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

#openxlsx::write.xlsx(robus2qte, 'robus2qte.xlsx')


#out3 d1
bwd1o3_min2 <- nth(as.numeric(unname(bwd1o3[c(3, 4, 5)])), 2)

qte_d1o3G <- rdd.qte(running1, outcome3, d1,
                     x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o3_min, kr=1,bias=0,eql=0)

qte_d1o3_r <- rdd.qte(running1, outcome3, d1,
                      x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o3_min2, kr=3,bias=0,eql=0)

qte_d1o3_rG <- rdd.qte(running1, outcome3, d1,
                       x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o3_min2, kr=1,bias=0,eql=0)

Wd1o3_sh_G <- Wald(running1, outcome3, d1,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd1o3_min, bandw2 = bwd1o3_min,
                   kr=1,eql=0, test.type = c(1, 2))

Wd1o3_sh_r <- Wald(running1, outcome3, d1,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd1o3_min2, bandw2 = bwd1o3_min2,
                   kr=3,eql=0, test.type = c(1, 2))

Wd1o3_sh_rG <- Wald(running1, outcome3, d1,
                    x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                    bandw=bwd1o3_min2, bandw2 = bwd1o3_min2,
                    kr=1,eql=0, test.type = c(1, 2))


## Export
robus3qte[1:3, 1] <- "d1_out3"

robus3qte[1, c(2)] <- round(bwd1o3_min, 3)
robus3qte[2:3, c(2)] <- round(bwd1o3_min2, 3)

robus3qte[c(1, 3), c(3)] <- "Gaussian"
robus3qte[c(2), c(3)] <- "Epanechnikov"

robus3qte[1, c(4:6)] <- round(qte_d1o3G$qte, 3)
robus3qte[2, c(4:6)] <- round(qte_d1o3_r$qte, 3)
robus3qte[3, c(4:6)] <- round(qte_d1o3_rG$qte, 3)

robus3qte[1, 7] <- case_when(Wd1o3_sh_G$wald.robust.test[1] > Wd1o3_sh_G$wald.robust.crit[3, 1] ~ "***",
                             Wd1o3_sh_G$wald.robust.test[1] > Wd1o3_sh_G$wald.robust.crit[2, 1] ~ "**",
                             Wd1o3_sh_G$wald.robust.test[1] > Wd1o3_sh_G$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus3qte[1, 8] <- case_when(Wd1o3_sh_G$wald.robust.test[2] > Wd1o3_sh_G$wald.robust.crit[3, 2] ~ "***",
                             Wd1o3_sh_G$wald.robust.test[2] > Wd1o3_sh_G$wald.robust.crit[2, 2] ~ "**",
                             Wd1o3_sh_G$wald.robust.test[2] > Wd1o3_sh_G$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robus3qte[2, 7] <- case_when(Wd1o3_sh_r$wald.robust.test[1] > Wd1o3_sh_r$wald.robust.crit[3, 1] ~ "***",
                             Wd1o3_sh_r$wald.robust.test[1] > Wd1o3_sh_r$wald.robust.crit[2, 1] ~ "**",
                             Wd1o3_sh_r$wald.robust.test[1] > Wd1o3_sh_r$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus3qte[2, 8] <- case_when(Wd1o3_sh_r$wald.robust.test[2] > Wd1o3_sh_r$wald.robust.crit[3, 2] ~ "***",
                             Wd1o3_sh_r$wald.robust.test[2] > Wd1o3_sh_r$wald.robust.crit[2, 2] ~ "**",
                             Wd1o3_sh_r$wald.robust.test[2] > Wd1o3_sh_r$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robus3qte[3, 7] <- case_when(Wd1o3_sh_rG$wald.robust.test[1] > Wd1o3_sh_rG$wald.robust.crit[3, 1] ~ "***",
                             Wd1o3_sh_rG$wald.robust.test[1] > Wd1o3_sh_rG$wald.robust.crit[2, 1] ~ "**",
                             Wd1o3_sh_rG$wald.robust.test[1] > Wd1o3_sh_rG$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus3qte[3, 8] <- case_when(Wd1o3_sh_rG$wald.robust.test[2] > Wd1o3_sh_rG$wald.robust.crit[3, 2] ~ "***",
                             Wd1o3_sh_rG$wald.robust.test[2] > Wd1o3_sh_rG$wald.robust.crit[2, 2] ~ "**",
                             Wd1o3_sh_rG$wald.robust.test[2] > Wd1o3_sh_rG$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

#openxlsx::write.xlsx(robus3qte, 'robus3qte.xlsx')


#out4 d1
bwd1o4_min2 <- nth(as.numeric(unname(bwd1o4[c(3, 4, 5)])), 2)

qte_d1o4G <- rdd.qte(running1, outcome4, d1,
                     x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o4_min, kr=1,bias=0,eql=0)

qte_d1o4_r <- rdd.qte(running1, outcome4, d1,
                      x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o4_min2, kr=3,bias=0,eql=0)

qte_d1o4_rG <- rdd.qte(running1, outcome4, d1,
                       x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd1o4_min2, kr=1,bias=0,eql=0)

Wd1o4_sh_G <- Wald(running1, outcome4, d1,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd1o4_min, bandw2 = bwd1o4_min,
                   kr=1,eql=0, test.type = c(1, 2))

Wd1o4_sh_r <- Wald(running1, outcome4, d1,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd1o4_min2, bandw2 = bwd1o4_min2,
                   kr=3,eql=0, test.type = c(1, 2))

Wd1o4_sh_rG <- Wald(running1, outcome4, d1,
                    x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                    bandw=bwd1o4_min2, bandw2 = bwd1o4_min2,
                    kr=1,eql=0, test.type = c(1, 2))


## Export
robus4qte[1:3, 1] <- "d1_out4"

robus4qte[1, c(2)] <- round(bwd1o4_min, 3)
robus4qte[2:3, c(2)] <- round(bwd1o4_min2, 3)

robus4qte[c(1, 3), c(3)] <- "Gaussian"
robus4qte[c(2), c(3)] <- "Epanechnikov"

robus4qte[1, c(4:6)] <- round(qte_d1o4G$qte, 3)
robus4qte[2, c(4:6)] <- round(qte_d1o4_r$qte, 3)
robus4qte[3, c(4:6)] <- round(qte_d1o4_rG$qte, 3)

robus4qte[1, 7] <- case_when(Wd1o4_sh_G$wald.robust.test[1] > Wd1o4_sh_G$wald.robust.crit[3, 1] ~ "***",
                             Wd1o4_sh_G$wald.robust.test[1] > Wd1o4_sh_G$wald.robust.crit[2, 1] ~ "**",
                             Wd1o4_sh_G$wald.robust.test[1] > Wd1o4_sh_G$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus4qte[1, 8] <- case_when(Wd1o4_sh_G$wald.robust.test[2] > Wd1o4_sh_G$wald.robust.crit[3, 2] ~ "***",
                             Wd1o4_sh_G$wald.robust.test[2] > Wd1o4_sh_G$wald.robust.crit[2, 2] ~ "**",
                             Wd1o4_sh_G$wald.robust.test[2] > Wd1o4_sh_G$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robus4qte[2, 7] <- case_when(Wd1o4_sh_r$wald.robust.test[1] > Wd1o4_sh_r$wald.robust.crit[3, 1] ~ "***",
                             Wd1o4_sh_r$wald.robust.test[1] > Wd1o4_sh_r$wald.robust.crit[2, 1] ~ "**",
                             Wd1o4_sh_r$wald.robust.test[1] > Wd1o4_sh_r$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus4qte[2, 8] <- case_when(Wd1o4_sh_r$wald.robust.test[2] > Wd1o4_sh_r$wald.robust.crit[3, 2] ~ "***",
                             Wd1o4_sh_r$wald.robust.test[2] > Wd1o4_sh_r$wald.robust.crit[2, 2] ~ "**",
                             Wd1o4_sh_r$wald.robust.test[2] > Wd1o4_sh_r$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robus4qte[3, 7] <- case_when(Wd1o4_sh_rG$wald.robust.test[1] > Wd1o4_sh_rG$wald.robust.crit[3, 1] ~ "***",
                             Wd1o4_sh_rG$wald.robust.test[1] > Wd1o4_sh_rG$wald.robust.crit[2, 1] ~ "**",
                             Wd1o4_sh_rG$wald.robust.test[1] > Wd1o4_sh_rG$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robus4qte[3, 8] <- case_when(Wd1o4_sh_rG$wald.robust.test[2] > Wd1o4_sh_rG$wald.robust.crit[3, 2] ~ "***",
                             Wd1o4_sh_rG$wald.robust.test[2] > Wd1o4_sh_rG$wald.robust.crit[2, 2] ~ "**",
                             Wd1o4_sh_rG$wald.robust.test[2] > Wd1o4_sh_rG$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

#openxlsx::write.xlsx(robus4qte, 'robus4qte.xlsx')


## 5.2. D2 ---------
#out1 d2
bwd2o1 <- rdd.bandwidth(running2, outcome1_d2, d2,
                        x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4,5), 
                        val=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07), kr = 3)

bwd2o1_min <- min(as.numeric(unname(bwd2o1[c(3, 4, 5)])))

qte_d2o1 <- rdd.qte(running2, outcome1_d2, d2,
                    x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o1_min, kr=3,bias=0,eql=0)

contenedorqte[1, 1] <- "d2_out1"
contenedorqte[1, c(2,3,4)] <- bwd2o1[c(3,4,5)]
contenedorqte[1, c(5:7)] <- qte_d2o1$qte
ucipci_d2o1 <- as.data.frame(cbind(qte_d2o1$uci, qte_d2o1$pci))
ucipci_d2o1al99 <- as.data.frame(cbind(qte_d2o1al99$uci, qte_d2o1al99$pci))
ucipci_d2o1al90 <- as.data.frame(cbind(qte_d2o1al90$uci, qte_d2o1al90$pci))
#openxlsx::write.xlsx(contenedorqte, 'contenedorqte_d2o1.xlsx')
#openxlsx::write.xlsx(ucipci_d2o1, 'ucipci_d2o1.xlsx')
#openxlsx::write.xlsx(ucipci_d2o1al99, 'ucipci_d2o1al99.xlsx')
#openxlsx::write.xlsx(ucipci_d2o1al90, 'ucipci_d2o1al90.xlsx')

Wd2o1_s <- Wald(running2, outcome1_d2, d2,
              x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,bandw=bwd2o1_min, bandw2 = bwd2o1_min,
              kr=3,eql=0, sign.opt = 1, test.type = 1)

Wd2o1_h <- Wald(running2, outcome1_d2, d2,
                x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,bandw=bwd2o1_min, bandw2 = bwd2o1_min,
                kr=3,eql=0, sign.opt = 1, test.type = 2)

## robustness to bias
qte_d2o1_pct17b1e1 <- rdd.qte(running2, outcome1_d2, d2,
                              x.eval=0, alpha=0.95, tt=c(0.1,0.9),
                              m=17,bandw=bwd2o1_min, kr=3,bias=1,eql=1)

table_bc_d2o1 <- data.frame(tau = qte_d2o1_pct17b1e1$taus,
                            qte = round(qte_d2o1_pct17b1e1$qte, 3),
                            uci_l = round(qte_d2o1_pct17b1e1$uci[, 1], 3),
                            uci_r = round(qte_d2o1_pct17b1e1$uci[, 2], 3))

table_bc_d2o1$sign95 <- ifelse(table_bc_d2o1$uci_l > 0 | table_bc_d2o1$uci_r < 0, "**", "")

#out2 d2
bwd2o2 <- rdd.bandwidth(running2, outcome2_d2, d2,
                        x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4,5), 
                        val=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09), kr = 3)

bwd2o2_min <- min(as.numeric(unname(bwd2o2[c(3, 4, 5)])))

qte_d2o2 <- rdd.qte(running2, outcome2_d2, d2,
                    x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o2_min, kr=3,bias=0,eql=0)

contenedorqte[1, 1] <- "d2_out2"
contenedorqte[1, c(2,3,4)] <- bwd2o2[c(3,4,5)]
contenedorqte[1, c(5:7)] <- qte_d2o2$qte
ucipci_d2o2 <- as.data.frame(cbind(qte_d2o2$uci, qte_d2o2$pci))
ucipci_al99_d2o2 <- as.data.frame(cbind(qte_d2o2al99$uci, qte_d2o2al99$pci))
ucipci_al90_d2o2 <- as.data.frame(cbind(qte_d2o2al90$uci, qte_d2o2al90$pci))

#openxlsx::write.xlsx(contenedorqte, 'contenedorqte_d2o2.xlsx')
#openxlsx::write.xlsx(ucipci_d2o2, 'ucipci_d2o2.xlsx')
#openxlsx::write.xlsx(ucipci_al99_d2o2, 'ucipci_al99_d2o2.xlsx')
#openxlsx::write.xlsx(ucipci_al90_d2o2, 'ucipci_al90_d2o2.xlsx')

Wd2o2_sh <- Wald(running2, outcome2_d2, d2,
                x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,bandw=bwd2o2_min, bandw2 = bwd2o2_min,
                kr=3,eql=0, test.type = c(1, 2))

## robustness to bias
qte_d2o2_pct17b1e1 <- rdd.qte(running2, outcome2_d2, d2,
                              x.eval=0, alpha=0.95, tt=c(0.1,0.9),
                              m=17,bandw=bwd2o2_min, kr=3,bias=1,eql=1)

table_bc_d2o2 <- data.frame(tau = qte_d2o2_pct17b1e1$taus,
                            qte = round(qte_d2o2_pct17b1e1$qte, 3),
                            uci_l = round(qte_d2o2_pct17b1e1$uci[, 1], 3),
                            uci_r = round(qte_d2o2_pct17b1e1$uci[, 2], 3))

table_bc_d2o2$sign95 <- ifelse(table_bc_d2o2$uci_l > 0 | table_bc_d2o2$uci_r < 0, "**", "")


## Further explanation
qte_d2o2_pct <- rdd.qte(running2, outcome2_d2, d2,
                    x.eval=0, alpha=0.95, tt=c(0.2,0.7),
                    m=6,bandw=bwd2o2_min, kr=3,bias=0,eql=0)

qte_d2o2_pct8 <- rdd.qte(running2, outcome2_d2, d2,
                        x.eval=0, alpha=0.95, tt=c(0.2,0.9),
                        m=8,bandw=bwd2o2_min, kr=3,bias=0,eql=0)

qte_d2o2_pct20 <- rdd.qte(running2, outcome2_d2, d2,
                         x.eval=0, alpha=0.95, tt=c(0.25,0.75),
                         m=20,bandw=bwd2o2_min, kr=3,bias=0,eql=0)

bwd2o2_minsubs01 <- bwd2o2_min - 0.01
qte_d2o2_pct01 <- rdd.qte(running2, outcome2_d2, d2,
                        x.eval=0, alpha=0.95, tt=c(0.2,0.7),
                        m=6,bandw=bwd2o2_minsubs01, kr=3,bias=0,eql=0)

# Gaussian kernel
qte_d2o2_pctG <- rdd.qte(running2, outcome2_d2, d2,
                        x.eval=0, alpha=0.95, tt=c(0.2,0.7),
                        m=6,bandw=bwd2o2_min, kr=1,bias=0,eql=0) # gaussian kernel

# Triangular kernel
qte_d2o2_pctT <- rdd.qte.AFJ(running2, outcome2_d2, d2,
                         x.eval=0, alpha=0.95, tt=c(0.2,0.7),
                         m=6,bandw=bwd2o2_min, kr=0,bias=0,eql=0) # triangular kernel


#out3 d2
bwd2o3 <- rdd.bandwidth(running2, outcome3_d2, d2,
                        x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4,5), 
                        val=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1, 0.11, 0.12), kr = 3)

bwd2o3_min <- min(as.numeric(unname(bwd2o3[c(3, 4, 5)])))

qte_d2o3 <- rdd.qte(running2, outcome3_d2, d2,
                    x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o3_min, kr=3,bias=0,eql=0)


contenedorqte[1, 1] <- "d2_out3"
contenedorqte[1, c(2,3,4)] <- bwd2o3[c(3,4,5)]
contenedorqte[1, c(5:7)] <- qte_d2o3$qte
ucipci_d2o3 <- as.data.frame(cbind(qte_d2o3$uci, qte_d2o3$pci))
ucipci_d2o3al90 <- as.data.frame(cbind(qte_d2o3al90$uci, qte_d2o3al90$pci))

#openxlsx::write.xlsx(contenedorqte, 'contenedorqte_d2o3.xlsx')
#openxlsx::write.xlsx(ucipci_d2o3, 'ucipci_d2o3.xlsx')
#openxlsx::write.xlsx(ucipci_d2o3al90, 'ucipci_d2o3al90.xlsx')

Wd2o3_sh <- Wald(running2, outcome3_d2, d2,
                 x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,bandw=bwd2o3_min, bandw2 = bwd2o3_min,
                 kr=3,eql=0, test.type = c(1, 2))

## robustness to bias
qte_d2o3_pct17b1e1 <- rdd.qte(running2, outcome3_d2, d2,
                              x.eval=0, alpha=0.95, tt=c(0.1,0.9),
                              m=17,bandw=bwd2o3_min, kr=3,bias=1,eql=1)


#out4 d2
bwd2o4 <- rdd.bandwidth(running2, outcome4_d2, d2,
                        x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4,5), 
                        val=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07), kr = 3)

bwd2o4_min <- min(as.numeric(unname(bwd2o4[c(3, 4, 5)])))

qte_d2o4 <- rdd.qte(running2, outcome4_d2, d2,
                    x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o4_min, kr=3,bias=0,eql=0)

contenedorqte[1, 1] <- "d2_out4"
contenedorqte[1, c(2,3,4)] <- bwd2o4[c(3,4,5)]
contenedorqte[1, c(5:7)] <- qte_d2o4$qte
ucipci_d2o4 <- as.data.frame(cbind(qte_d2o4$uci, qte_d2o4$pci))
ucipci_d2o4al90 <- as.data.frame(cbind(qte_d2o4al90$uci, qte_d2o4al90$pci))

#openxlsx::write.xlsx(contenedorqte, 'contenedorqte_d2o4.xlsx')
#openxlsx::write.xlsx(ucipci_d2o4, 'ucipci_d2o4.xlsx')
#openxlsx::write.xlsx(ucipci_d2o4al90, 'ucipci_d2o4al90.xlsx')

Wd2o4_sh <- Wald(running2, outcome4_d2, d2,
                 x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,bandw=bwd2o4_min, bandw2 = bwd2o4_min,
                 kr=3,eql=0, test.type = c(1, 2))

## robustness to bias
qte_d2o4_pct17b1e1 <- rdd.qte(running2, outcome4_d2, d2,
                              x.eval=0, alpha=0.95, tt=c(0.1,0.9),
                              m=17,bandw=bwd2o4_min, kr=3,bias=1,eql=1)



### Plausibility -----------
indi_ns_ss2_rmT1$treatedD2 <- ifelse(indi_ns_ss2_rmT1$scoringD2_0 > 0, TRUE, FALSE)

running2rmt1 <- indi_ns_ss2_rmT1$scoringD2_0
outcomepre1_d2 <- indi_ns_ss2_rmT1$prewd1_6
d2rmt1 <- indi_ns_ss2_rmT1$treatedD2
#x0 <- 0

bwd2preo1_prev <- rdd.bandwidth.prev(running2rmt1, outcomepre1_d2, d2rmt1, prev = 0.08,
                                   x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4,5), kr = 3) # FUNCIONA

bwd2preo1_app <- rdd.bandwidth.app(running2rmt1, outcomepre1_d2, d2rmt1,
                           x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4,5), 
                           val=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08), kr = 3) # no funciona

bwd2preo1 <- rdd.bandwidth(running2rmt1, outcomepre1_d2, d2rmt1,
                           x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4,5), 
                           val=c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08), kr = 3) # no funcioina


bwd2preo1_othervals <- rdd.bandwidth(running2rmt1, outcomepre1_d2, d2rmt1,
                           x.eval = 0, tt = c(0.25,0.75), m=3, method = c(3,4), 
                           val=c(2:10/20), kr = 3) # no funciona




bwd2preo1_min <- min(as.numeric(unname(bwd2preo1_prev[c(3, 4, 5)])))

qte_d2preo1 <- rdd.qte(running2rmt1, outcomepre1_d2, d2rmt1,
                       x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,
                       bandw=bwd2preo1_min, kr=3,bias=0,eql=0)

Wd2preo1_sh <- Wald(running2rmt1, outcomepre1_d2, d2rmt1,
                    x.eval=0, alpha=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                    bandw=bwd2preo1_min, bandw2 = bwd2preo1_min,
                    kr=3,eql=0, test.type = c(1, 2))

### Robustness check --------------
robusto1qte <- data.frame(treat_out = NA, bw = NA, kernel = NA,
                        qte_q1 = NA, qte_q2 = NA, qte_q3 = NA, Wald_TS = NA, Wald_TH = NA)
robusto2qte <- data.frame(treat_out = NA, bw = NA, kernel = NA,
                        qte_q1 = NA, qte_q2 = NA, qte_q3 = NA, Wald_TS = NA, Wald_TH = NA)
robusto3qte <- data.frame(treat_out = NA, bw = NA, kernel = NA,
                        qte_q1 = NA, qte_q2 = NA, qte_q3 = NA, Wald_TS = NA, Wald_TH = NA)
robusto4qte <- data.frame(treat_out = NA, bw = NA, kernel = NA,
                        qte_q1 = NA, qte_q2 = NA, qte_q3 = NA, Wald_TS = NA, Wald_TH = NA)

#out1 d2
bwd2o1_min2 <- nth(as.numeric(unname(bwd2o1[c(3, 4, 5)])), 2)

qte_d2o1G <- rdd.qte(running2, outcome1_d2, d2,
                     x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o1_min, kr=1,bias=0,eql=0)

qte_d2o1_r <- rdd.qte(running2, outcome1_d2, d2,
                      x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o1_min2, kr=3,bias=0,eql=0)

qte_d2o1_rG <- rdd.qte(running2, outcome1_d2, d2,
                       x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o1_min2, kr=1,bias=0,eql=0)

Wd2o1_sh_G <- Wald(running2, outcome1_d2, d2,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd2o1_min, bandw2 = bwd2o1_min,
                   kr=1,eql=0, test.type = c(1, 2))

Wd2o1_sh_r <- Wald(running2, outcome1_d2, d2,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd2o1_min2, bandw2 = bwd2o1_min2,
                   kr=3,eql=0, test.type = c(1, 2))

Wd2o1_sh_rG <- Wald(running2, outcome1_d2, d2,
                    x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                    bandw=bwd2o1_min2, bandw2 = bwd2o1_min2,
                    kr=1,eql=0, test.type = c(1, 2))


## Export
robusto1qte[1:3, 1] <- "d2_out1"

robusto1qte[1, c(2)] <- round(bwd2o1_min, 3)
robusto1qte[2:3, c(2)] <- round(bwd2o1_min2, 3)

robusto1qte[c(1, 3), c(3)] <- "Gaussian"
robusto1qte[c(2), c(3)] <- "Epanechnikov"

robusto1qte[1, c(4:6)] <- round(qte_d2o1G$qte, 3)
robusto1qte[2, c(4:6)] <- round(qte_d2o1_r$qte, 3)
robusto1qte[3, c(4:6)] <- round(qte_d2o1_rG$qte, 3)

robusto1qte[1, 7] <- case_when(Wd2o1_sh_G$wald.robust.test[1] > Wd2o1_sh_G$wald.robust.crit[3, 1] ~ "***",
                             Wd2o1_sh_G$wald.robust.test[1] > Wd2o1_sh_G$wald.robust.crit[2, 1] ~ "**",
                             Wd2o1_sh_G$wald.robust.test[1] > Wd2o1_sh_G$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto1qte[1, 8] <- case_when(Wd2o1_sh_G$wald.robust.test[2] > Wd2o1_sh_G$wald.robust.crit[3, 2] ~ "***",
                             Wd2o1_sh_G$wald.robust.test[2] > Wd2o1_sh_G$wald.robust.crit[2, 2] ~ "**",
                             Wd2o1_sh_G$wald.robust.test[2] > Wd2o1_sh_G$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robusto1qte[2, 7] <- case_when(Wd2o1_sh_r$wald.robust.test[1] > Wd2o1_sh_r$wald.robust.crit[3, 1] ~ "***",
                             Wd2o1_sh_r$wald.robust.test[1] > Wd2o1_sh_r$wald.robust.crit[2, 1] ~ "**",
                             Wd2o1_sh_r$wald.robust.test[1] > Wd2o1_sh_r$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto1qte[2, 8] <- case_when(Wd2o1_sh_r$wald.robust.test[2] > Wd2o1_sh_r$wald.robust.crit[3, 2] ~ "***",
                             Wd2o1_sh_r$wald.robust.test[2] > Wd2o1_sh_r$wald.robust.crit[2, 2] ~ "**",
                             Wd2o1_sh_r$wald.robust.test[2] > Wd2o1_sh_r$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robusto1qte[3, 7] <- case_when(Wd2o1_sh_rG$wald.robust.test[1] > Wd2o1_sh_rG$wald.robust.crit[3, 1] ~ "***",
                             Wd2o1_sh_rG$wald.robust.test[1] > Wd2o1_sh_rG$wald.robust.crit[2, 1] ~ "**",
                             Wd2o1_sh_rG$wald.robust.test[1] > Wd2o1_sh_rG$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto1qte[3, 8] <- case_when(Wd2o1_sh_rG$wald.robust.test[2] > Wd2o1_sh_rG$wald.robust.crit[3, 2] ~ "***",
                             Wd2o1_sh_rG$wald.robust.test[2] > Wd2o1_sh_rG$wald.robust.crit[2, 2] ~ "**",
                             Wd2o1_sh_rG$wald.robust.test[2] > Wd2o1_sh_rG$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")


#openxlsx::write.xlsx(robusto1qte, 'robusto1qte.xlsx')




#out2 d2
bwd2o2_min2 <- nth(as.numeric(unname(bwd2o2[c(3, 4, 5)])), 2)

qte_d2o2G <- rdd.qte(running2, outcome2_d2, d2,
                     x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o2_min, kr=1,bias=0,eql=0)

qte_d2o2_r <- rdd.qte(running2, outcome2_d2, d2,
                      x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o2_min2, kr=3,bias=0,eql=0)

qte_d2o2_rG <- rdd.qte(running2, outcome2_d2, d2,
                       x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o2_min2, kr=1,bias=0,eql=0)

Wd2o2_sh_G <- Wald(running2, outcome2_d2, d2,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd2o2_min, bandw2 = bwd2o2_min,
                   kr=1,eql=0, test.type = c(1, 2))

Wd2o2_sh_r <- Wald(running2, outcome2_d2, d2,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd2o2_min2, bandw2 = bwd2o2_min2,
                   kr=3,eql=0, test.type = c(1, 2))

Wd2o2_sh_rG <- Wald(running2, outcome2_d2, d2,
                    x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                    bandw=bwd2o2_min2, bandw2 = bwd2o2_min2,
                    kr=1,eql=0, test.type = c(1, 2))


## Export
robusto2qte[1:3, 1] <- "d2_out2"

robusto2qte[1, c(2)] <- round(bwd2o2_min, 3)
robusto2qte[2:3, c(2)] <- round(bwd2o2_min2, 3)

robusto2qte[c(1, 3), c(3)] <- "Gaussian"
robusto2qte[c(2), c(3)] <- "Epanechnikov"

robusto2qte[1, c(4:6)] <- round(qte_d2o2G$qte, 3)
robusto2qte[2, c(4:6)] <- round(qte_d2o2_r$qte, 3)
robusto2qte[3, c(4:6)] <- round(qte_d2o2_rG$qte, 3)

robusto2qte[1, 7] <- case_when(Wd2o2_sh_G$wald.robust.test[1] > Wd2o2_sh_G$wald.robust.crit[3, 1] ~ "***",
                             Wd2o2_sh_G$wald.robust.test[1] > Wd2o2_sh_G$wald.robust.crit[2, 1] ~ "**",
                             Wd2o2_sh_G$wald.robust.test[1] > Wd2o2_sh_G$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto2qte[1, 8] <- case_when(Wd2o2_sh_G$wald.robust.test[2] > Wd2o2_sh_G$wald.robust.crit[3, 2] ~ "***",
                             Wd2o2_sh_G$wald.robust.test[2] > Wd2o2_sh_G$wald.robust.crit[2, 2] ~ "**",
                             Wd2o2_sh_G$wald.robust.test[2] > Wd2o2_sh_G$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robusto2qte[2, 7] <- case_when(Wd2o2_sh_r$wald.robust.test[1] > Wd2o2_sh_r$wald.robust.crit[3, 1] ~ "***",
                             Wd2o2_sh_r$wald.robust.test[1] > Wd2o2_sh_r$wald.robust.crit[2, 1] ~ "**",
                             Wd2o2_sh_r$wald.robust.test[1] > Wd2o2_sh_r$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto2qte[2, 8] <- case_when(Wd2o2_sh_r$wald.robust.test[2] > Wd2o2_sh_r$wald.robust.crit[3, 2] ~ "***",
                             Wd2o2_sh_r$wald.robust.test[2] > Wd2o2_sh_r$wald.robust.crit[2, 2] ~ "**",
                             Wd2o2_sh_r$wald.robust.test[2] > Wd2o2_sh_r$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robusto2qte[3, 7] <- case_when(Wd2o2_sh_rG$wald.robust.test[1] > Wd2o2_sh_rG$wald.robust.crit[3, 1] ~ "***",
                             Wd2o2_sh_rG$wald.robust.test[1] > Wd2o2_sh_rG$wald.robust.crit[2, 1] ~ "**",
                             Wd2o2_sh_rG$wald.robust.test[1] > Wd2o2_sh_rG$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto2qte[3, 8] <- case_when(Wd2o2_sh_rG$wald.robust.test[2] > Wd2o2_sh_rG$wald.robust.crit[3, 2] ~ "***",
                             Wd2o2_sh_rG$wald.robust.test[2] > Wd2o2_sh_rG$wald.robust.crit[2, 2] ~ "**",
                             Wd2o2_sh_rG$wald.robust.test[2] > Wd2o2_sh_rG$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

#openxlsx::write.xlsx(robusto2qte, 'robusto2qte.xlsx')


#out3 d2
bwd2o3_min2 <- nth(as.numeric(unname(bwd2o3[c(3, 4, 5)])), 2)

qte_d2o3G <- rdd.qte(running2, outcome3_d2, d2,
                     x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o3_min, kr=1,bias=0,eql=0)

qte_d2o3_r <- rdd.qte(running2, outcome3_d2, d2,
                      x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o3_min2, kr=3,bias=0,eql=0)

qte_d2o3_rG <- rdd.qte(running2, outcome3_d2, d2,
                       x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o3_min2, kr=1,bias=0,eql=0)

Wd2o3_sh_G <- Wald(running2, outcome3_d2, d2,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd2o3_min, bandw2 = bwd2o3_min,
                   kr=1,eql=0, test.type = c(1, 2))

Wd2o3_sh_r <- Wald(running2, outcome3_d2, d2,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd2o3_min2, bandw2 = bwd2o3_min2,
                   kr=3,eql=0, test.type = c(1, 2))

Wd2o3_sh_rG <- Wald(running2, outcome3_d2, d2,
                    x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                    bandw=bwd2o3_min2, bandw2 = bwd2o3_min2,
                    kr=1,eql=0, test.type = c(1, 2))


## Export
robusto3qte[1:3, 1] <- "d2_out3"

robusto3qte[1, c(2)] <- round(bwd2o3_min, 3)
robusto3qte[2:3, c(2)] <- round(bwd2o3_min2, 3)

robusto3qte[c(1, 3), c(3)] <- "Gaussian"
robusto3qte[c(2), c(3)] <- "Epanechnikov"

robusto3qte[1, c(4:6)] <- round(qte_d2o3G$qte, 3)
robusto3qte[2, c(4:6)] <- round(qte_d2o3_r$qte, 3)
robusto3qte[3, c(4:6)] <- round(qte_d2o3_rG$qte, 3)

robusto3qte[1, 7] <- case_when(Wd2o3_sh_G$wald.robust.test[1] > Wd2o3_sh_G$wald.robust.crit[3, 1] ~ "***",
                             Wd2o3_sh_G$wald.robust.test[1] > Wd2o3_sh_G$wald.robust.crit[2, 1] ~ "**",
                             Wd2o3_sh_G$wald.robust.test[1] > Wd2o3_sh_G$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto3qte[1, 8] <- case_when(Wd2o3_sh_G$wald.robust.test[2] > Wd2o3_sh_G$wald.robust.crit[3, 2] ~ "***",
                             Wd2o3_sh_G$wald.robust.test[2] > Wd2o3_sh_G$wald.robust.crit[2, 2] ~ "**",
                             Wd2o3_sh_G$wald.robust.test[2] > Wd2o3_sh_G$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robusto3qte[2, 7] <- case_when(Wd2o3_sh_r$wald.robust.test[1] > Wd2o3_sh_r$wald.robust.crit[3, 1] ~ "***",
                             Wd2o3_sh_r$wald.robust.test[1] > Wd2o3_sh_r$wald.robust.crit[2, 1] ~ "**",
                             Wd2o3_sh_r$wald.robust.test[1] > Wd2o3_sh_r$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto3qte[2, 8] <- case_when(Wd2o3_sh_r$wald.robust.test[2] > Wd2o3_sh_r$wald.robust.crit[3, 2] ~ "***",
                             Wd2o3_sh_r$wald.robust.test[2] > Wd2o3_sh_r$wald.robust.crit[2, 2] ~ "**",
                             Wd2o3_sh_r$wald.robust.test[2] > Wd2o3_sh_r$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robusto3qte[3, 7] <- case_when(Wd2o3_sh_rG$wald.robust.test[1] > Wd2o3_sh_rG$wald.robust.crit[3, 1] ~ "***",
                             Wd2o3_sh_rG$wald.robust.test[1] > Wd2o3_sh_rG$wald.robust.crit[2, 1] ~ "**",
                             Wd2o3_sh_rG$wald.robust.test[1] > Wd2o3_sh_rG$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto3qte[3, 8] <- case_when(Wd2o3_sh_rG$wald.robust.test[2] > Wd2o3_sh_rG$wald.robust.crit[3, 2] ~ "***",
                             Wd2o3_sh_rG$wald.robust.test[2] > Wd2o3_sh_rG$wald.robust.crit[2, 2] ~ "**",
                             Wd2o3_sh_rG$wald.robust.test[2] > Wd2o3_sh_rG$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

#openxlsx::write.xlsx(robusto3qte, 'robusto3qte.xlsx')


#out4 d2
bwd2o4_min2 <- nth(as.numeric(unname(bwd2o4[c(3, 4, 5)])), 2)

qte_d2o4G <- rdd.qte(running2, outcome4_d2, d2,
                     x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o4_min, kr=1,bias=0,eql=0)

qte_d2o4_r <- rdd.qte(running2, outcome4_d2, d2,
                      x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o4_min2, kr=3,bias=0,eql=0)

qte_d2o4_rG <- rdd.qte(running2, outcome4_d2, d2,
                       x.eval=0, alpha=0.95, tt=c(0.25,0.75), m=3,bandw=bwd2o4_min2, kr=1,bias=0,eql=0)

Wd2o4_sh_G <- Wald(running2, outcome4_d2, d2,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd2o4_min, bandw2 = bwd2o4_min,
                   kr=1,eql=0, test.type = c(1, 2))

Wd2o4_sh_r <- Wald(running2, outcome4_d2, d2,
                   x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                   bandw=bwd2o4_min2, bandw2 = bwd2o4_min2,
                   kr=3,eql=0, test.type = c(1, 2))

Wd2o4_sh_rG <- Wald(running2, outcome4_d2, d2,
                    x.eval=0, alpha=c(0.9, 0.95, 0.99), tt=c(0.25,0.75), m=3,
                    bandw=bwd2o4_min2, bandw2 = bwd2o4_min2,
                    kr=1,eql=0, test.type = c(1, 2))


## Export
robusto4qte[1:3, 1] <- "d2_out4"

robusto4qte[1, c(2)] <- round(bwd2o4_min, 3)
robusto4qte[2:3, c(2)] <- round(bwd2o4_min2, 3)

robusto4qte[c(1, 3), c(3)] <- "Gaussian"
robusto4qte[c(2), c(3)] <- "Epanechnikov"

robusto4qte[1, c(4:6)] <- round(qte_d2o4G$qte, 3)
robusto4qte[2, c(4:6)] <- round(qte_d2o4_r$qte, 3)
robusto4qte[3, c(4:6)] <- round(qte_d2o4_rG$qte, 3)

robusto4qte[1, 7] <- case_when(Wd2o4_sh_G$wald.robust.test[1] > Wd2o4_sh_G$wald.robust.crit[3, 1] ~ "***",
                             Wd2o4_sh_G$wald.robust.test[1] > Wd2o4_sh_G$wald.robust.crit[2, 1] ~ "**",
                             Wd2o4_sh_G$wald.robust.test[1] > Wd2o4_sh_G$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto4qte[1, 8] <- case_when(Wd2o4_sh_G$wald.robust.test[2] > Wd2o4_sh_G$wald.robust.crit[3, 2] ~ "***",
                             Wd2o4_sh_G$wald.robust.test[2] > Wd2o4_sh_G$wald.robust.crit[2, 2] ~ "**",
                             Wd2o4_sh_G$wald.robust.test[2] > Wd2o4_sh_G$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robusto4qte[2, 7] <- case_when(Wd2o4_sh_r$wald.robust.test[1] > Wd2o4_sh_r$wald.robust.crit[3, 1] ~ "***",
                             Wd2o4_sh_r$wald.robust.test[1] > Wd2o4_sh_r$wald.robust.crit[2, 1] ~ "**",
                             Wd2o4_sh_r$wald.robust.test[1] > Wd2o4_sh_r$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto4qte[2, 8] <- case_when(Wd2o4_sh_r$wald.robust.test[2] > Wd2o4_sh_r$wald.robust.crit[3, 2] ~ "***",
                             Wd2o4_sh_r$wald.robust.test[2] > Wd2o4_sh_r$wald.robust.crit[2, 2] ~ "**",
                             Wd2o4_sh_r$wald.robust.test[2] > Wd2o4_sh_r$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

robusto4qte[3, 7] <- case_when(Wd2o4_sh_rG$wald.robust.test[1] > Wd2o4_sh_rG$wald.robust.crit[3, 1] ~ "***",
                             Wd2o4_sh_rG$wald.robust.test[1] > Wd2o4_sh_rG$wald.robust.crit[2, 1] ~ "**",
                             Wd2o4_sh_rG$wald.robust.test[1] > Wd2o4_sh_rG$wald.robust.crit[1, 1] ~ "*",
                             TRUE ~ ".")

robusto4qte[3, 8] <- case_when(Wd2o4_sh_rG$wald.robust.test[2] > Wd2o4_sh_rG$wald.robust.crit[3, 2] ~ "***",
                             Wd2o4_sh_rG$wald.robust.test[2] > Wd2o4_sh_rG$wald.robust.crit[2, 2] ~ "**",
                             Wd2o4_sh_rG$wald.robust.test[2] > Wd2o4_sh_rG$wald.robust.crit[1, 2] ~ "*",
                             TRUE ~ ".")

openxlsx::write.xlsx(robusto4qte, 'robusto4qte.xlsx')



# 6. PLOTS  --------------------
## Treatment 1 (D1) ----------------
### Outcome 0a: number of hours of job search measures received ---------
summary(indi_ns_ss1$jshours)

plot_jsh_qs_mv <- rdplot(y = indi_ns_ss1$jshours, x = indi_ns_ss1$scoringD1_0, binselect = "qsmv",
                      x.lim = c(-0.40, 0.25), y.lim = c(0, 13), shade = F) # quantile-spaced 
summary(plot_jsh_qs_mv)


### Generate rdplot using ggplot2: preparing inputs
#plot1 <- plot24qs_imse
plot1 <- plot_jsh_qs_mv


#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss1$scoringD1_0
y=indi_ns_ss1$jshours
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y["HS"], ": hours of JS measures received"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[1]," (", S[1], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y["HS"]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 13)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 0.75) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 0.75) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = x.lim, ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot # Saved at 395x300

### Outcome 0b: number of hours of training measures received ---------
summary(indi_ns_ss1$attiv_form_ore_prev)

plot_trh_qs_mv <- rdplot(y = indi_ns_ss1$attiv_form_ore_prev, x = indi_ns_ss1$scoringD1_0, binselect = "qsmv",
                         x.lim = c(-0.40, 0.25), y.lim = c(0, 136), shade = F) # quantile-spaced 
summary(plot_trh_qs_mv)


### Generate rdplot using ggplot2: preparing inputs
#plot1 <- plot24qs_imse
plot1 <- plot_trh_qs_mv


#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss1$scoringD1_0
y=indi_ns_ss1$attiv_form_ore_prev
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y["HT"], ": hours of training received"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[1]," (", S[1], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y["HT"]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 136)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 0.75) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 0.75) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = x.lim, ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot # Saved at 395x300


### Outcome 1: Number of worked days in post [1, 6] months ------
summary(indi_ns_ss1$post_interval6)
summary(indi_ns_ss1$scoringD1_0)

summary(indi_ns_ss1$scoringD1_0)
summary(indi_ns_ss1$post_interval6)

plot6es_mv <- rdplot(y = indi_ns_ss1$post_interval6, x = indi_ns_ss1$scoringD1_0, binselect = "esmv",
                     x.lim = c(-0.40, 0.25), y.lim = c(0, 185), shade = F) # evenly-spaced
summary(plot6es_mv)

plot6qs_mv <- rdplot(y = indi_ns_ss1$post_interval6, x = indi_ns_ss1$scoringD1_0, binselect = "qsmv",
                x.lim = c(-0.40, 0.25), y.lim = c(0, 185), shade = F) # quantile-spaced 
summary(plot6qs_mv)

plot6qs_imse <- rdplot(y = indi_ns_ss1$post_interval6, x = indi_ns_ss1$scoringD1_0, binselect = "qs",
                     x.lim = c(-0.40, 0.25), y.lim = c(0, 185), ci = 95, hide = TRUE) # quantile-spaced 
summary(plot6qs_imse)

### Option A. rdplot
summary(indi_ns_ss1$post_interval6)

plot_s1d1_qs_mv <- rdplot(y = indi_ns_ss1$post_interval6, x = indi_ns_ss1$scoringD1_0, binselect = "qsmv",
                           x.lim = c(-0.37, 0.2), y.lim = c(0, 185), shade = F,
                           title = "Y1: worked days during months [1, 6]", x.label = "Score for D1 (S1)", y.label = "LSM of Y1",
                           col.dots = "dimgrey") # quantile-spaced, saved at  500 x 379
summary(plot_s1d1_qs_mv)



### Option B. Generate rdplot using ggplot2: preparing inputs

#plot1 <- plot6qs_imse
plot1 <- plot6qs_mv

#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss1$scoringD1_0
y=indi_ns_ss1$post_interval6
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y[1], ": worked days during months [+1, +6]"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[1]," (", S[1], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y[1]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 185)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 0.75) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 0.75) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = x.lim, ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot # 395 x 300



### Outcome 2: Number of worked days in post [7, 12] months ------
summary(indi_ns_ss1$post_interval712)
summary(indi_ns_ss1$scoringD1_0)

summary(indi_ns_ss1$scoringD1_0)
summary(indi_ns_ss1$post_interval12)

plot12es_mv <- rdplot(y = indi_ns_ss1$post_interval712, x = indi_ns_ss1$scoringD1_0, binselect = "esmv",
                     x.lim = c(-0.40, 0.25), y.lim = c(0, 185), shade = F) # evenly-spaced
summary(plot12es_mv)

plot12qs_mv <- rdplot(y = indi_ns_ss1$post_interval712, x = indi_ns_ss1$scoringD1_0, binselect = "qsmv",
                     x.lim = c(-0.40, 0.25), y.lim = c(0, 185), shade = F) # quantile-spaced 
summary(plot12qs_mv)

plot12qs_imse <- rdplot(y = indi_ns_ss1$post_interval12, x = indi_ns_ss1$scoringD1_0, binselect = "qs",
                       x.lim = c(-0.40, 0.25), y.lim = c(0, 375), ci = 95, hide = TRUE) # quantile-spaced 
summary(plot12qs_imse)


### Generate rdplot using ggplot2: preparing inputs
#plot1 <- plot12qs_imse
plot1 <- plot12qs_mv

#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss1$scoringD1_0
y=indi_ns_ss1$post_interval712
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y[2], ": worked days during months [+7, +12]"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[1]," (", S[1], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y[2]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 185)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 0.75) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 0.75) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = x.lim, ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot # Saved at 395x300


### Outcome 3: Number of worked days in post [13, 18] months ------
summary(indi_ns_ss1$post_interval1318)
summary(indi_ns_ss1$scoringD1_0)

summary(indi_ns_ss1$scoringD1_0)
summary(indi_ns_ss1$post_interval18)

plot18es_mv <- rdplot(y = indi_ns_ss1$post_interval1318, x = indi_ns_ss1$scoringD1_0, binselect = "esmv",
                      x.lim = c(-0.40, 0.25), y.lim = c(0, 185), shade = F) # evenly-spaced
summary(plot18es_mv)

plot18qs_mv <- rdplot(y = indi_ns_ss1$post_interval1318, x = indi_ns_ss1$scoringD1_0, binselect = "qsmv",
                      x.lim = c(-0.40, 0.25), y.lim = c(0, 185), shade = F) # quantile-spaced 
summary(plot18qs_mv)



### Generate rdplot using ggplot2: preparing inputs
#plot1 <- plot18qs_imse
plot1 <- plot18qs_mv

#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss1$scoringD1_0
y=indi_ns_ss1$post_interval1318
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y[3], ": worked days during months [+13, +18]"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[1]," (", S[1], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y[3]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 185)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 0.75) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 0.75) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = x.lim, ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot # Saved at 395x300

### Outcome 4: Number of worked days in post [19, 24] months ------
summary(indi_ns_ss1$post_interval1924)
summary(indi_ns_ss1$scoringD1_0)

summary(indi_ns_ss1$scoringD1_0)
summary(indi_ns_ss1$post_interval24)

plot24es_mv <- rdplot(y = indi_ns_ss1$post_interval1924, x = indi_ns_ss1$scoringD1_0, binselect = "esmv",
                      x.lim = c(-0.40, 0.25), y.lim = c(0, 750), shade = F) # evenly-spaced
summary(plot24es_mv)

plot24qs_mv <- rdplot(y = indi_ns_ss1$post_interval1924, x = indi_ns_ss1$scoringD1_0, binselect = "qsmv",
                      x.lim = c(-0.40, 0.25), y.lim = c(0, 185), shade = F) # quantile-spaced 
summary(plot24qs_mv)

plot24qs_imse <- rdplot(y = indi_ns_ss1$post_interval1924, x = indi_ns_ss1$scoringD1_0, binselect = "qs",
                        x.lim = c(-0.40, 0.25), y.lim = c(0, 750), ci = 95, hide = TRUE) # quantile-spaced 
summary(plot24qs_imse)


### Generate rdplot using ggplot2: preparing inputs
#plot1 <- plot24qs_imse
plot1 <- plot24qs_mv


#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss1$scoringD1_0
y=indi_ns_ss1$post_interval1924
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y[4], ": worked days during months [+19, +24]"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[1]," (", S[1], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y[4]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 185)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 0.75) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 0.75) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = x.lim, ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot # Saved at 395x300


## Treatment 2 (D2) ------------------------
### Outcome 0a: number of hours of job search measures received ---------
summary(indi_ns_ss2$jshours)

plot_jsh_qs_mv <- rdplot(y = indi_ns_ss2$jshours, x = indi_ns_ss2$scoringD2_0, binselect = "qsmv",
                         x.lim = c(-0.25, 0.40), y.lim = c(0, 27), shade = F) # quantile-spaced 
summary(plot_jsh_qs_mv)


### Generate rdplot using ggplot2: preparing inputs
#plot1 <- plot24qs_imse
plot1 <- plot_jsh_qs_mv


#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss2$scoringD2_0
y=indi_ns_ss2$jshours
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y[HS], ": hours of JS measures received"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[2]," (", S[2], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y[0]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 27)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 1) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = x.lim, ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot # Saved at 395x300

### Outcome 0b: number of hours of training measures received ---------
summary(indi_ns_ss2$attiv_form_ore_prev)

plot_trh_qs_mv <- rdplot(y = indi_ns_ss2$attiv_form_ore_prev, x = indi_ns_ss2$scoringD2_0, binselect = "qsmv",
                         x.lim = c(-0.25, 0.40), y.lim = c(0, 146), shade = F) # quantile-spaced 
summary(plot_trh_qs_mv)


### Generate rdplot using ggplot2: preparing inputs
#plot1 <- plot24qs_imse
plot1 <- plot_trh_qs_mv


#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss2$scoringD2_0
y=indi_ns_ss2$attiv_form_ore_prev
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y[HT], ": hours of training received"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[2]," (", S[2], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y[0]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 146)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 1) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = x.lim, ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot # Saved at 395x300

### Outcome 1: post [1, 6] months ---------------------
summary(indi_ns_ss2$scoringD2_0)
summary(indi_ns_ss2$post_interval6)

plotD2_6_qs_imse <- rdplot(y = indi_ns_ss2$post_interval6, x = indi_ns_ss2$scoringD2_0, binselect = "qs",
                        x.lim = c(-0.25, 0.40), y.lim = c(0, 200), ci = 95, hide = TRUE) # quantile-spaced 
summary(plotD2_6_qs_imse)

plotD2_6_qs_mv <- rdplot(y = indi_ns_ss2$post_interval6, x = indi_ns_ss2$scoringD2_0, binselect = "qsmv",
                           x.lim = c(-0.25, 0.40), y.lim = c(0, 185), shade = F) # quantile-spaced mv
summary(plotD2_6_qs_mv)

### Generate rdplot using ggplot2: preparing inputs
plot1 <- plotD2_6_qs_mv

#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss2$scoringD2_0
y=indi_ns_ss2$post_interval6
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y[1], ": worked days during months [+1, +6]"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[2]," (", S[2], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y[1]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 185)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 0.75) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 0.75) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = c(-0.25, 0.4), ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot # Saved at 395x300


### Outcome 2: post [7, 12] months ---------------------
summary(indi_ns_ss2$scoringD2_0)
summary(indi_ns_ss2$post_interval12)

plotD2_12_qs_imse <- rdplot(y = indi_ns_ss2$post_interval712, x = indi_ns_ss2$post_interval712, binselect = "qs",
                           x.lim = c(-0.25, 0.40), y.lim = c(0, 375), ci = 95, hide = TRUE) # quantile-spaced 
summary(plotD2_12_qs_imse)

plotD2_12_qs_mv <- rdplot(y = indi_ns_ss2$post_interval712, x = indi_ns_ss2$scoringD2_0, binselect = "qsmv",
                         x.lim = c(-0.25, 0.40), y.lim = c(0, 185), shade = F) # quantile-spaced mv
summary(plotD2_12_qs_mv)


### Generate rdplot using ggplot2: preparing inputs
plot1 <- plotD2_12_qs_mv

#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss2$scoringD2_0
y=indi_ns_ss2$post_interval712
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y[2], ": worked days during months [+7, +12]"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[2]," (", S[2], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y[2]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 185)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 0.75) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 0.75) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = x.lim, ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot # Saved at 395x300

# Shade (alpha controls transparency)
temp_plot +
  geom_ribbon(aes(x = rdplot_mean_bin, ymin = rdplot_cil_bin, ymax = rdplot_cir_bin), alpha=0.2) # Saved at 395x300

### Outcome 3: post 18 months ---------------------
summary(indi_ns_ss2$scoringD2_0)
summary(indi_ns_ss2$post_interval18)

plotD2_18_qs_imse <- rdplot(y = indi_ns_ss2$post_interval18, x = indi_ns_ss2$scoringD2_0, binselect = "qs",
                            x.lim = c(-0.25, 0.40), y.lim = c(0, 575), ci = 95, hide = TRUE) # quantile-spaced 
summary(plotD2_18_qs_imse)

plotD2_18_qs_mv <- rdplot(y = indi_ns_ss2$post_interval1318, x = indi_ns_ss2$scoringD2_0, binselect = "qsmv",
                          x.lim = c(-0.25, 0.40), y.lim = c(0, 185), shade = F) # quantile-spaced mv
summary(plotD2_18_qs_mv)


### Generate rdplot using ggplot2: preparing inputs
plot1 <- plotD2_18_qs_mv

#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss2$scoringD2_0
y=indi_ns_ss2$post_interval1318
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y[3], ": worked days during months [+13, +18]"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[2]," (", S[2], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y[3]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 185)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 0.75) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 0.75) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = x.lim, ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot # Saved at 395x300

# Shade (alpha controls transparency)
temp_plot +
  geom_ribbon(aes(x = rdplot_mean_bin, ymin = rdplot_cil_bin, ymax = rdplot_cir_bin), alpha=0.2) # Saved at 395x300


### Outcome 4: post 24 months ---------------------
summary(indi_ns_ss2$scoringD2_0)
summary(indi_ns_ss2$post_interval24)

plotD2_24_qs_imse <- rdplot(y = indi_ns_ss2$post_interval24, x = indi_ns_ss2$scoringD2_0, binselect = "qs",
                            x.lim = c(-0.25, 0.40), y.lim = c(0, 750), ci = 95, hide = TRUE) # quantile-spaced 
summary(plotD2_24_qs_imse)

plotD2_24_qs_mv <- rdplot(y = indi_ns_ss2$post_interval1924, x = indi_ns_ss2$scoringD2_0, binselect = "qsmv",
                          x.lim = c(-0.25, 0.40), y.lim = c(0, 185), shade = F) # quantile-spaced mv
summary(plotD2_24_qs_mv)


### Generate rdplot using ggplot2: preparing inputs
plot1 <- plotD2_24_qs_mv

#plot1 = rdplot(y,x, ci=95, hide=TRUE)
c=0
x=indi_ns_ss2$scoringD2_0
y=indi_ns_ss2$post_interval1924
rdplot_mean_bin = plot1$vars_bins[,"rdplot_mean_bin"]
rdplot_mean_y   = plot1$vars_bins[,"rdplot_mean_y"]
y_hat = plot1$vars_poly[,"rdplot_y"]
x_plot = plot1$vars_poly[,"rdplot_x"]
rdplot_cil_bin =  plot1$vars_bins[,"rdplot_ci_l"]
rdplot_cir_bin =  plot1$vars_bins[,"rdplot_ci_r"]
rdplot_mean_bin=  plot1$vars_bins[,"rdplot_mean_bin"]
y_hat_r=y_hat[x_plot>=c]
y_hat_l=y_hat[x_plot<c]
x_plot_r=x_plot[x_plot>=c]
x_plot_l=x_plot[x_plot<c]

col.lines = "blue"
col.dots  = 1
type.dots = 20
#title="Y1: worked days during months [+1, +6]"
title = expression(paste(Y[4], ": worked days during months [+19, +24]"))
#x.label="Score for D1 (S1)"
x.label = expression(paste("Score for ", D[2]," (", S[2], ")"))
#y.label="LSM of Y1"
y.label = expression(paste("LSM of ", Y[4]))
x.lim=c(min(x, na.rm=T),max(x, na.rm=T))
#y.lim=c(min(y, na.rm=T), max(y, na.rm=T))
y.lim = c(0, 185)

### Generate rdplot using ggplot2: executing ggplot2

temp_plot <- ggplot() + theme_bw() +
  geom_point(aes(x = rdplot_mean_bin, y = rdplot_mean_y), col = "dimgrey", na.rm = TRUE, size = 1) +
  geom_line(aes(x = x_plot_l, y = y_hat_l), col = "red", na.rm = TRUE, size = 0.75) +
  geom_line(aes(x = x_plot_r, y = y_hat_r), col = "blue", na.rm = TRUE, size = 0.75) +
  labs(x = x.label, y = y.label) + ggtitle(title) +
  labs(title = title, y = y.label, x = x.label) +
  coord_cartesian(xlim = x.lim, ylim = y.lim) +
  theme(legend.position = "None") +
  geom_vline(xintercept = c, size = 0.5) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))
temp_plot

# Shade (alpha controls transparency)
temp_plot +
  geom_ribbon(aes(x = rdplot_mean_bin, ymin = rdplot_cil_bin, ymax = rdplot_cir_bin), alpha=0.2) # Saved at 395x300

## Plots for logit models (employment quality) ---------
### D1---------
# D1 OEC
logit_oec <- binsglm(y = indi_ns_ss1_c$lcris_OEC,
                        x = indi_ns_ss1_c$scoringD1_0,
                        by = indi_ns_ss1_c$treatedD1,
                        randcut = 1, family = binomial(link = "logit"),
                        deriv = 0,
                        bycolors = c("dimgrey", "dimgrey"),
                        legendoff = T)
  
running1_left <- indi_ns_ss1_c$scoringD1_0[indi_ns_ss1_c$treatedD1 == F]
outcome1d1_left <- indi_ns_ss1_c$lcris_OEC[indi_ns_ss1_c$treatedD1 == F]
  
running1_right <- indi_ns_ss1_c$scoringD1_0[indi_ns_ss1_c$treatedD1 == T]
outcome1d1_right <- indi_ns_ss1_c$lcris_OEC[indi_ns_ss1_c$treatedD1 == T]
  
xlabeld1 <- expression(paste("Score for ", D[1]," (", S[1], ")"))
ylabelo1 <- expression(paste("Prop. of ", Y[OEC], " = YES"))
titleo1 <- expression(paste(Y[OEC], " = YES in [+1, +24]"))
  
xlimite <- c(min(indi_ns_ss1_c$scoringD1_0, na.rm=T), max(indi_ns_ss1_c$scoringD1_0, na.rm=T))
ylimite = c(0, 1)
  
logit_oec$bins_plot +
    geom_smooth(aes(running1_left, outcome1d1_left),
                  method = "glm", method.args = list(family = "binomial"),
                se = F, col = "red", linewidth = 0.75,
                  formula = y ~ poly(x, 4)) +
  geom_smooth(aes(running1_right, outcome1d1_right),
              method = "glm", method.args = list(family = "binomial"),
              se = F, col = "blue", linewidth = 0.75,
                  formula = y ~ poly(x, 4)) +
    geom_vline(xintercept = 0, linewidth = 0.5) +
    coord_cartesian(xlim = xlimite, ylim = ylimite) +
    labs(title = titleo1, y = ylabelo1, x = xlabeld1) +
    theme(axis.text.x = element_text(size=rel(1.2)),
          axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300

# D1 FTCm12
logit_FTCm12 <- binsglm(y = indi_ns_ss1_c$lcris_FTCm12,
                     x = indi_ns_ss1_c$scoringD1_0,
                     by = indi_ns_ss1_c$treatedD1,
                     randcut = 1, family = binomial(link = "logit"),
                     deriv = 0,
                     bycolors = c("dimgrey", "dimgrey"),
                     legendoff = T)

running1_left <- indi_ns_ss1_c$scoringD1_0[indi_ns_ss1_c$treatedD1 == F]
outcome1d1_left <- indi_ns_ss1_c$lcris_FTCm12[indi_ns_ss1_c$treatedD1 == F]

running1_right <- indi_ns_ss1_c$scoringD1_0[indi_ns_ss1_c$treatedD1 == T]
outcome1d1_right <- indi_ns_ss1_c$lcris_FTCm12[indi_ns_ss1_c$treatedD1 == T]

xlabeld1 <- expression(paste("Score for ", D[1]," (", S[1], ")"))
ylabelo1 <- expression(paste("Prop. of ", Y[FTCm12], " = YES"))
titleo1 <- expression(paste(Y[FTCm12], " = YES in [+1, +24]"))

xlimite <- c(min(indi_ns_ss1_c$scoringD1_0, na.rm=T), max(indi_ns_ss1_c$scoringD1_0, na.rm=T))
ylimite = c(0, 1)

logit_FTCm12$bins_plot +
  geom_smooth(aes(running1_left, outcome1d1_left),
              method = "glm", method.args = list(family = "binomial"),
              se = F, col = "red", linewidth = 0.75,
              formula = y ~ poly(x, 4)) +
  geom_smooth(aes(running1_right, outcome1d1_right),
              method = "glm", method.args = list(family = "binomial"),
              se = F, col = "blue", linewidth = 0.75,
              formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(xlim = xlimite, ylim = ylimite) +
  labs(title = titleo1, y = ylabelo1, x = xlabeld1) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300
  

# D1 FTC612
logit_FTC612 <- binsglm(y = indi_ns_ss1_c$lcris_FTC612,
                        x = indi_ns_ss1_c$scoringD1_0,
                        by = indi_ns_ss1_c$treatedD1,
                        randcut = 1, family = binomial(link = "logit"),
                        deriv = 0,
                        bycolors = c("dimgrey", "dimgrey"),
                        legendoff = T)

running1_left <- indi_ns_ss1_c$scoringD1_0[indi_ns_ss1_c$treatedD1 == F]
outcome1d1_left <- indi_ns_ss1_c$lcris_FTC612[indi_ns_ss1_c$treatedD1 == F]

running1_right <- indi_ns_ss1_c$scoringD1_0[indi_ns_ss1_c$treatedD1 == T]
outcome1d1_right <- indi_ns_ss1_c$lcris_FTC612[indi_ns_ss1_c$treatedD1 == T]

xlabeld1 <- expression(paste("Score for ", D[1]," (", S[1], ")"))
ylabelo1 <- expression(paste("Prop. of ", Y[FTC612], " = YES"))
titleo1 <- expression(paste(Y[FTC612], " = YES in [+1, +24]"))

xlimite <- c(min(indi_ns_ss1_c$scoringD1_0, na.rm=T), max(indi_ns_ss1_c$scoringD1_0, na.rm=T))
ylimite = c(0, 1)

logit_FTC612$bins_plot +
  geom_smooth(aes(running1_left, outcome1d1_left),
              method = "glm", method.args = list(family = "binomial"),
              se = F, col = "red", linewidth = 0.75,
              formula = y ~ poly(x, 4)) +
  geom_smooth(aes(running1_right, outcome1d1_right),
              method = "glm", method.args = list(family = "binomial"),
              se = F, col = "blue", linewidth = 0.75,
              formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(xlim = xlimite, ylim = ylimite) +
  labs(title = titleo1, y = ylabelo1, x = xlabeld1) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300

### D2---------
# D2 OEC
logit_oec <- binsglm(y = indi_ns_ss2_c$lcris_OEC,
                     x = indi_ns_ss2_c$scoringD2_0,
                     by = indi_ns_ss2_c$treatedD2,
                     randcut = 1, family = binomial(link = "logit"),
                     deriv = 0,
                     bycolors = c("dimgrey", "dimgrey"),
                     legendoff = T)

running1_left <- indi_ns_ss2_c$scoringD2_0[indi_ns_ss2_c$treatedD2 == F]
outcome1D2_left <- indi_ns_ss2_c$lcris_OEC[indi_ns_ss2_c$treatedD2 == F]

running1_right <- indi_ns_ss2_c$scoringD2_0[indi_ns_ss2_c$treatedD2 == T]
outcome1D2_right <- indi_ns_ss2_c$lcris_OEC[indi_ns_ss2_c$treatedD2 == T]

xlabelD2 <- expression(paste("Score for ", D[2]," (", S[2], ")"))
ylabelo1 <- expression(paste("Prop. of ", Y[OEC], " = YES"))
titleo1 <- expression(paste(Y[OEC], " = YES in [+1, +24]"))

xlimite <- c(min(indi_ns_ss2_c$scoringD2_0, na.rm=T), max(indi_ns_ss2_c$scoringD2_0, na.rm=T))
ylimite = c(0, 1)

logit_oec$bins_plot +
  geom_smooth(aes(running1_left, outcome1D2_left),
              method = "glm", method.args = list(family = "binomial"),
              se = F, col = "red", linewidth = 0.75,
              formula = y ~ poly(x, 4)) +
  geom_smooth(aes(running1_right, outcome1D2_right),
              method = "glm", method.args = list(family = "binomial"),
              se = F, col = "blue", linewidth = 0.75,
              formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(xlim = xlimite, ylim = ylimite) +
  labs(title = titleo1, y = ylabelo1, x = xlabelD2) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300

# D2 FTCm12
logit_FTCm12 <- binsglm(y = indi_ns_ss2_c$lcris_FTCm12,
                        x = indi_ns_ss2_c$scoringD2_0,
                        by = indi_ns_ss2_c$treatedD2,
                        randcut = 1, family = binomial(link = "logit"),
                        deriv = 0,
                        bycolors = c("dimgrey", "dimgrey"),
                        legendoff = T)

running1_left <- indi_ns_ss2_c$scoringD2_0[indi_ns_ss2_c$treatedD2 == F]
outcome1D2_left <- indi_ns_ss2_c$lcris_FTCm12[indi_ns_ss2_c$treatedD2 == F]

running1_right <- indi_ns_ss2_c$scoringD2_0[indi_ns_ss2_c$treatedD2 == T]
outcome1D2_right <- indi_ns_ss2_c$lcris_FTCm12[indi_ns_ss2_c$treatedD2 == T]

xlabelD2 <- expression(paste("Score for ", D[2]," (", S[2], ")"))
ylabelo1 <- expression(paste("Prop. of ", Y[FTCm12], " = YES"))
titleo1 <- expression(paste(Y[FTCm12], " = YES in [+1, +24]"))

xlimite <- c(min(indi_ns_ss2_c$scoringD2_0, na.rm=T), max(indi_ns_ss2_c$scoringD2_0, na.rm=T))
ylimite = c(0, 1)

logit_FTCm12$bins_plot +
  geom_smooth(aes(running1_left, outcome1D2_left),
              method = "glm", method.args = list(family = "binomial"),
              se = F, col = "red", linewidth = 0.75,
              formula = y ~ poly(x, 4)) +
  geom_smooth(aes(running1_right, outcome1D2_right),
              method = "glm", method.args = list(family = "binomial"),
              se = F, col = "blue", linewidth = 0.75,
              formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(xlim = xlimite, ylim = ylimite) +
  labs(title = titleo1, y = ylabelo1, x = xlabelD2) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300


# D2 FTC612
logit_FTC612 <- binsglm(y = indi_ns_ss2_c$lcris_FTC612,
                        x = indi_ns_ss2_c$scoringD2_0,
                        by = indi_ns_ss2_c$treatedD2,
                        randcut = 1, family = binomial(link = "logit"),
                        deriv = 0,
                        bycolors = c("dimgrey", "dimgrey"),
                        legendoff = T)

running1_left <- indi_ns_ss2_c$scoringD2_0[indi_ns_ss2_c$treatedD2 == F]
outcome1D2_left <- indi_ns_ss2_c$lcris_FTC612[indi_ns_ss2_c$treatedD2 == F]

running1_right <- indi_ns_ss2_c$scoringD2_0[indi_ns_ss2_c$treatedD2 == T]
outcome1D2_right <- indi_ns_ss2_c$lcris_FTC612[indi_ns_ss2_c$treatedD2 == T]

xlabelD2 <- expression(paste("Score for ", D[2]," (", S[2], ")"))
ylabelo1 <- expression(paste("Prop. of ", Y[FTC612], " = YES"))
titleo1 <- expression(paste(Y[FTC612], " = YES in [+1, +24]"))

xlimite <- c(min(indi_ns_ss2_c$scoringD2_0, na.rm=T), max(indi_ns_ss2_c$scoringD2_0, na.rm=T))
ylimite = c(0, 1)

logit_FTC612$bins_plot +
  geom_smooth(aes(running1_left, outcome1D2_left),
              method = "glm", method.args = list(family = "binomial"),
              se = F, col = "red", linewidth = 0.75,
              formula = y ~ poly(x, 4)) +
  geom_smooth(aes(running1_right, outcome1D2_right),
              method = "glm", method.args = list(family = "binomial"),
              se = F, col = "blue", linewidth = 0.75,
              formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(xlim = xlimite, ylim = ylimite) +
  labs(title = titleo1, y = ylabelo1, x = xlabelD2) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300


## Plots for quantile effects -------------

### D1 -------------
# o1
estbs1_y1 <- binsqreg(y = indi_ns_ss1$post_interval6,
                        x = indi_ns_ss1$scoringD1_0,
                        by = indi_ns_ss1$treatedD1,
                        quantile = 0.5, randcut = 1,
                        deriv = 0,
                        bycolors = c("dimgrey", "dimgrey"),
                        legendoff = T)
  
running1_left <- indi_ns_ss1$scoringD1_0[indi_ns_ss1$treatedD1 == F]
outcome1d1_left <- indi_ns_ss1$post_interval6[indi_ns_ss1$treatedD1 == F]
  
running1_right <- indi_ns_ss1$scoringD1_0[indi_ns_ss1$treatedD1 == T]
outcome1d1_right <- indi_ns_ss1$post_interval6[indi_ns_ss1$treatedD1 == T]
  
xlabeld1 <- expression(paste("Score for ", D[1]," (", S[1], ")"))
ylabelo1 <- expression(paste("LSMed of ", Y[1]))
titleo1 <- expression(paste(Y[1], ": worked days during months [+1, +6]"))
  
xlimite <- c(min(indi_ns_ss1$scoringD1_0, na.rm=T), max(indi_ns_ss1$scoringD1_0, na.rm=T))
ylimite = c(0, 185)
  
estbs1_y1$bins_plot +
    geom_quantile(aes(running1_left, outcome1d1_left),
                  quantiles = 0.5, col = "red", linewidth = 0.75,
                  formula = y ~ poly(x, 4)) +
    geom_quantile(aes(running1_right, outcome1d1_right),
                  quantiles = 0.5, col = "blue", linewidth = 0.75,
                  formula = y ~ poly(x, 4)) +
    geom_vline(xintercept = 0, linewidth = 0.5) +
    coord_cartesian(xlim = xlimite, ylim = ylimite) +
    labs(title = titleo1, y = ylabelo1, x = xlabeld1) +
    theme(axis.text.x = element_text(size=rel(1.2)),
          axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300
  

# o2
estbs1_y2 <- binsqreg(y = indi_ns_ss1$post_interval712,
                      x = indi_ns_ss1$scoringD1_0,
                      by = indi_ns_ss1$treatedD1,
                      quantile = 0.5, randcut = 1,
                      deriv = 0,
                      bycolors = c("dimgrey", "dimgrey"),
                      legendoff = T)

running1_left <- indi_ns_ss1$scoringD1_0[indi_ns_ss1$treatedD1 == F]
outcome2d1_left <- indi_ns_ss1$post_interval712[indi_ns_ss1$treatedD1 == F]

running1_right <- indi_ns_ss1$scoringD1_0[indi_ns_ss1$treatedD1 == T]
outcome2d1_right <- indi_ns_ss1$post_interval712[indi_ns_ss1$treatedD1 == T]

xlabeld1 <- expression(paste("Score for ", D[1]," (", S[1], ")"))
ylabelo2 <- expression(paste("LSMed of ", Y[2]))
titleo2 <- expression(paste(Y[2], ": worked days during months [+7, +12]"))

xlimite <- c(min(indi_ns_ss1$scoringD1_0, na.rm=T), max(indi_ns_ss1$scoringD1_0, na.rm=T))
ylimite = c(0, 185)

estbs1_y2$bins_plot +
  geom_quantile(aes(running1_left, outcome2d1_left),
                quantiles = 0.5, col = "red", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_quantile(aes(running1_right, outcome2d1_right),
                quantiles = 0.5, col = "blue", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(xlim = xlimite, ylim = ylimite) +
  labs(title = titleo2, y = ylabelo2, x = xlabeld1) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300

# o3
estbs1_y3 <- binsqreg(y = indi_ns_ss1$post_interval1318,
                      x = indi_ns_ss1$scoringD1_0,
                      by = indi_ns_ss1$treatedD1,
                      quantile = 0.5, randcut = 1,
                      deriv = 0,
                      bycolors = c("dimgrey", "dimgrey"),
                      legendoff = T)

running1_left <- indi_ns_ss1$scoringD1_0[indi_ns_ss1$treatedD1 == F]
outcome3d1_left <- indi_ns_ss1$post_interval1318[indi_ns_ss1$treatedD1 == F]

running1_right <- indi_ns_ss1$scoringD1_0[indi_ns_ss1$treatedD1 == T]
outcome3d1_right <- indi_ns_ss1$post_interval1318[indi_ns_ss1$treatedD1 == T]

xlabeld1 <- expression(paste("Score for ", D[1]," (", S[1], ")"))
ylabelo3 <- expression(paste("LSMed of ", Y[3]))
titleo3 <- expression(paste(Y[3], ": worked days during months [+13, +18]"))

xlimite <- c(min(indi_ns_ss1$scoringD1_0, na.rm=T), max(indi_ns_ss1$scoringD1_0, na.rm=T))
ylimite = c(0, 185)

estbs1_y3$bins_plot +
  geom_quantile(aes(running1_left, outcome3d1_left),
                quantiles = 0.5, col = "red", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_quantile(aes(running1_right, outcome3d1_right),
                quantiles = 0.5, col = "blue", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(xlim = xlimite, ylim = ylimite) +
  labs(title = titleo3, y = ylabelo3, x = xlabeld1) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300

# o4
estbs1_y4 <- binsqreg(y = indi_ns_ss1$post_interval1924,
                      x = indi_ns_ss1$scoringD1_0,
                      by = indi_ns_ss1$treatedD1,
                      quantile = 0.5, randcut = 1,
                      deriv = 0,
                      bycolors = c("dimgrey", "dimgrey"),
                      legendoff = T)

running1_left <- indi_ns_ss1$scoringD1_0[indi_ns_ss1$treatedD1 == F]
outcome4d1_left <- indi_ns_ss1$post_interval1924[indi_ns_ss1$treatedD1 == F]

running1_right <- indi_ns_ss1$scoringD1_0[indi_ns_ss1$treatedD1 == T]
outcome4d1_right <- indi_ns_ss1$post_interval1924[indi_ns_ss1$treatedD1 == T]

xlabeld1 <- expression(paste("Score for ", D[1]," (", S[1], ")"))
ylabelo4 <- expression(paste("LSMed of ", Y[4]))
titleo4 <- expression(paste(Y[4], ": worked days during months [+19, +24]"))

ylim = c(0, 185)

estbs1_y4$bins_plot +
  geom_quantile(aes(running1_left, outcome4d1_left),
                quantiles = 0.5, col = "red", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_quantile(aes(running1_right, outcome4d1_right),
                quantiles = 0.5, col = "blue", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(ylim = ylim) +
  labs(title = titleo4, y = ylabelo4, x = xlabeld1) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300
  

### D2 -------------
# o1
estbs2_y1 <- binsqreg(y = indi_ns_ss2$post_interval6,
                      x = indi_ns_ss2$scoringD2_0,
                      by = indi_ns_ss2$treatedD2,
                      quantile = 0.5, randcut = 1,
                      deriv = 0,
                      bycolors = c("dimgrey", "dimgrey"),
                      legendoff = T)

running2_left <- indi_ns_ss2$scoringD2_0[indi_ns_ss2$treatedD2 == F]
outcome1d2_left <- indi_ns_ss2$post_interval6[indi_ns_ss2$treatedD2 == F]

running2_right <- indi_ns_ss2$scoringD2_0[indi_ns_ss2$treatedD2 == T]
outcome1d2_right <- indi_ns_ss2$post_interval6[indi_ns_ss2$treatedD2 == T]

xlabeld2 <- expression(paste("Score for ", D[2]," (", S[2], ")"))
ylabelo1 <- expression(paste("LSMed of ", Y[1]))
titleo1 <- expression(paste(Y[1], ": worked days during months [+1, +6]"))

xlimite <- c(min(indi_ns_ss2$scoringD2_0, na.rm=T), max(indi_ns_ss2$scoringD2_0, na.rm=T))
ylimite = c(0, 185)

estbs2_y1$bins_plot +
  geom_quantile(aes(running2_left, outcome1d2_left),
                quantiles = 0.5, col = "red", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_quantile(aes(running2_right, outcome1d2_right),
                quantiles = 0.5, col = "blue", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(xlim = xlimite, ylim = ylimite) +
  labs(title = titleo1, y = ylabelo1, x = xlabeld2) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300


# o2
estbs2_y2 <- binsqreg(y = indi_ns_ss2$post_interval712,
                      x = indi_ns_ss2$scoringD2_0,
                      by = indi_ns_ss2$treatedD2,
                      quantile = 0.5, randcut = 1,
                      deriv = 0,
                      bycolors = c("dimgrey", "dimgrey"),
                      legendoff = T)

#running2_left <- indi_ns_ss2$scoringD2_0[indi_ns_ss2$treatedD2 == F]
outcome2d2_left <- indi_ns_ss2$post_interval712[indi_ns_ss2$treatedD2 == F]

#running2_right <- indi_ns_ss2$scoringD2_0[indi_ns_ss2$treatedD2 == T]
outcome2d2_right <- indi_ns_ss2$post_interval712[indi_ns_ss2$treatedD2 == T]

#xlabeld1 <- expression(paste("Score for ", D[1]," (", S[1], ")"))
ylabelo2 <- expression(paste("LSMed of ", Y[2]))
titleo2 <- expression(paste(Y[2], ": worked days during months [+7, +12]"))

xlimite <- c(min(indi_ns_ss2$scoringD2_0, na.rm=T), max(indi_ns_ss2$scoringD2_0, na.rm=T))
ylimite = c(0, 185)

estbs2_y2$bins_plot +
  geom_quantile(aes(running2_left, outcome2d2_left),
                quantiles = 0.5, col = "red", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_quantile(aes(running2_right, outcome2d2_right),
                quantiles = 0.5, col = "blue", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(xlim = xlimite, ylim = ylimite) +
  labs(title = titleo2, y = ylabelo2, x = xlabeld2) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300

# o3
estbs2_y3 <- binsqreg(y = indi_ns_ss2$post_interval1318,
                      x = indi_ns_ss2$scoringD2_0,
                      by = indi_ns_ss2$treatedD2,
                      quantile = 0.5, randcut = 1,
                      deriv = 0,
                      bycolors = c("dimgrey", "dimgrey"),
                      legendoff = T)

#running1_left <- indi_ns_ss2$scoringD2_0[indi_ns_ss2$treatedD2 == F]
outcome3d2_left <- indi_ns_ss2$post_interval1318[indi_ns_ss2$treatedD2 == F]

#running1_right <- indi_ns_ss2$scoringD2_0[indi_ns_ss2$treatedD2 == T]
outcome3d2_right <- indi_ns_ss2$post_interval1318[indi_ns_ss2$treatedD2 == T]

#xlabeld1 <- expression(paste("Score for ", D[1]," (", S[1], ")"))
ylabelo3 <- expression(paste("LSMed of ", Y[3]))
titleo3 <- expression(paste(Y[3], ": worked days during months [+13, +18]"))

xlimite <- c(min(indi_ns_ss2$scoringD2_0, na.rm=T), max(indi_ns_ss2$scoringD2_0, na.rm=T))
ylimite = c(0, 185)

estbs2_y3$bins_plot +
  geom_quantile(aes(running2_left, outcome3d2_left),
                quantiles = 0.5, col = "red", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_quantile(aes(running2_right, outcome3d2_right),
                quantiles = 0.5, col = "blue", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(xlim = xlimite, ylim = ylimite) +
  labs(title = titleo3, y = ylabelo3, x = xlabeld2) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300

# o4
estbs2_y4 <- binsqreg(y = indi_ns_ss2$post_interval1924,
                      x = indi_ns_ss2$scoringD2_0,
                      by = indi_ns_ss2$treatedD2,
                      quantile = 0.5, randcut = 1,
                      deriv = 0,
                      bycolors = c("dimgrey", "dimgrey"),
                      legendoff = T)

#running1_left <- indi_ns_ss2$scoringD2_0[indi_ns_ss2$treatedD2 == F]
outcome4d2_left <- indi_ns_ss2$post_interval1924[indi_ns_ss2$treatedD2 == F]

#running1_right <- indi_ns_ss2$scoringD2_0[indi_ns_ss2$treatedD2 == T]
outcome4d2_right <- indi_ns_ss2$post_interval1924[indi_ns_ss2$treatedD2 == T]

#xlabeld1 <- expression(paste("Score for ", D[1]," (", S[1], ")"))
ylabelo4 <- expression(paste("LSMed of ", Y[4]))
titleo4 <- expression(paste(Y[4], ": worked days during months [+19, +24]"))

ylim = c(0, 185)

estbs2_y4$bins_plot +
  geom_quantile(aes(running2_left, outcome4d2_left),
                quantiles = 0.5, col = "red", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_quantile(aes(running2_right, outcome4d2_right),
                quantiles = 0.5, col = "blue", linewidth = 0.75,
                formula = y ~ poly(x, 4)) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  coord_cartesian(ylim = ylim) +
  labs(title = titleo4, y = ylabelo4, x = xlabeld2) +
  theme(axis.text.x = element_text(size=rel(1.2)),
        axis.text.y = element_text(size=rel(1.2)))# Saved at 395x300

