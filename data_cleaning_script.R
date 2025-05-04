library(dplyr)

# using readr's `read_csv()` instead of `read.csv()` for increased efficiency
institutional = readr::read_csv("institutional.csv")

inst = institutional %>%
  filter(CURROPER == 1,
         PREDDEG == 3,
         ST_FIPS <= 56,
         CONTROL != 3) %>% 
  mutate(net_tuit = sum(NPT4_PUB, NPT4_PRIV, na.rm = TRUE),
         ROOMBOARD_DIF = ROOMBOARD_ON - ROOMBOARD_OFF,
         RATIO = MD_EARN_WNE_INC1_P6 / MD_EARN_WNE_INC3_P6) %>% 
  select(id = UNITID,
         govt_id = OPEID,
         name = INSTNM,
         alias = ALIAS,
         city = CITY,
         state = STABBR,
         url = INSTURL,
         control = CONTROL,
         deg_predomnt = PREDDEG,
         deg_highest = HIGHDEG,
         locale_type = LOCALE,
         locale_size = LOCALE,
         is_hbcu = HBCU,
         is_pbi = PBI,
         is_annhi = ANNHI,
         is_tribal = TRIBAL,
         is_aanapii = AANAPII,
         is_hsi = HSI,
         is_nanti = NANTI,
         is_only_men = MENONLY,
         is_only_women = WOMENONLY,
         religious_affil = RELAFFIL,
         adm_rate = ADM_RATE_ALL,
         sat_avg = SAT_AVG,
         act_p25 = ACTCM25,
         act_p75 = ACTCM75,
         sat_reading_p25 = SATVR25,
         sat_reading_p75 = SATVR75,
         sat_math_p25 = SATMT25,
         sat_math_p75 = SATMT75,
         ug_enrollment = UGDS,
         net_tuit,
         cost_avg_income_0_30k = NPT41_PRIV,
         cost_avg_income_30_48k = NPT42_PRIV,
         cost_avg_income_48_75k = NPT43_PRIV,
         cost_avg_income_75_110k = NPT44_PRIV,
         cost_avg_income_110k_plus = NPT45_PRIV,
         earnings_med_10y = MD_EARN_WNE_P10,
         rate_completion = C100_4,
         rm_board_dif = ROOMBOARD_DIF,
         incm_ratio = RATIO)

write.csv(inst, "cleaned_data.csv", row.names = FALSE)

