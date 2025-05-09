library(dplyr)

# using readr's `read_csv()` instead of `read.csv()` for increased efficiency
institutional = readr::read_csv("institutional.csv")

# clean the institutional data
inst = institutional %>%
  filter(CURROPER == 1,  # currently operational
         PREDDEG == 3,   # predominantly bachelor's degree-awarding institutions
         ST_FIPS <= 56,  # exclude US territories
         CONTROL != 3,   # exclude for-profit institutions
         MAIN == 1,      # only main campuses to avoid duplicated rows of same values
         !ADM_RATE_SUPP %in% c("PS", "1")) %>% 
  distinct(OPEID6, .keep_all = TRUE) %>%  # more removing duplicate rows
  mutate(adm_rate = as.numeric(ADM_RATE_SUPP),
         RATIO = MD_EARN_WNE_INC1_P6 / MD_EARN_WNE_INC3_P6,
         sat_p25 = round(SATVR25, -1) + round(SATMT25, -1),
         sat_p75 = round(SATVR75, -1) + round(SATMT75, -1),
         sat_mid50 = round(SATVRMID, -1) + round(SATMTMID, -1)) %>% 
  select(name = INSTNM,
         city = CITY,
         state = STABBR,
         adm_rate,
         sat_mid50,
         act_p25 = ACTCM25,
         act_p75 = ACTCM75,
         act_mid50 = ACTCMMID,
         sat_p25,
         sat_p75,
         cost_avg_income_0_30k = NPT41_PRIV,
         cost_avg_income_30_48k = NPT42_PRIV,
         cost_avg_income_48_75k = NPT43_PRIV,
         cost_avg_income_75_110k = NPT44_PRIV,
         cost_avg_income_110k_plus = NPT45_PRIV,
         ratio = RATIO,
         ROOMBOARD_ON,
         ROOMBOARD_OFF,
         MD_EARN_WNE_INC2_P6,
         TUITIONFEE_IN,
         TUITIONFEE_OUT)

# save as a CSV file in working directory
write.csv(inst, "cleaned_data.csv", row.names = FALSE)

