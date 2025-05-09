library(dplyr)

# using readr's `read_csv()` instead of `read.csv()` for increased efficiency
institutional = readr::read_csv("institutional.csv")

inst = institutional %>%
  filter(CURROPER == 1,
         PREDDEG == 3,
         ST_FIPS <= 56,
         CONTROL != 3,
         MAIN == 1,
         !ADM_RATE_SUPP %in% c("PS", "1")) %>% 
  distinct(OPEID6, .keep_all = TRUE) %>% 
  mutate(net_price = sum(NPT4_PUB, NPT4_PRIV, na.rm = TRUE),
         adm_rate = as.numeric(ADM_RATE_SUPP),
         ROOMBOARD_DIF = ROOMBOARD_ON - ROOMBOARD_OFF,
         RATIO = MD_EARN_WNE_INC1_P6 / MD_EARN_WNE_INC3_P6,
         sat_p25 = round(SATVR25, -1) + round(SATMT25, -1),
         sat_p75 = round(SATVR75, -1) + round(SATMT75, -1),
         sat_mid50 = round(SATVRMID, -1) + round(SATMTMID, -1)) %>% 
  select(name = INSTNM,
         alias = ALIAS,
         city = CITY,
         state = STABBR,
         url = INSTURL,
         control = CONTROL,
         deg_predomnt = PREDDEG,
         deg_highest = HIGHDEG,
         locale_type = LOCALE,
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
         adm_rate,
         sat_avg = SAT_AVG,
         sat_mid50,
         act_p25 = ACTCM25,
         act_p75 = ACTCM75,
         act_mid50 = ACTCMMID,
         sat_p25,
         sat_p75,
         ug_enrollment = UGDS,
         net_price,
         cost_avg_income_0_30k = NPT41_PRIV,
         cost_avg_income_30_48k = NPT42_PRIV,
         cost_avg_income_48_75k = NPT43_PRIV,
         cost_avg_income_75_110k = NPT44_PRIV,
         cost_avg_income_110k_plus = NPT45_PRIV,
         earnings_med_10y = MD_EARN_WNE_P10,
         rate_completion = C100_4,
         rm_board_dif = ROOMBOARD_DIF,
         ratio = RATIO,
         student_fac_ratio = STUFACR,
         no_aid = OPEFLAG,
         ROOMBOARD_ON,
         ROOMBOARD_OFF,
         MD_EARN_WNE_INC2_P6,
         TUITIONFEE_IN,
         TUITIONFEE_OUT)

write.csv(inst, "cleaned_data.csv", row.names = FALSE)

