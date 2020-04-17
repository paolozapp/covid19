# run clean_dataset.R: you need values 'days','aftermarch' and data 'data_italy'

known <- data_italy %>% filter (missing == FALSE)

daybyday <- known %>% group_by (GE) %>% summarise(TOTALE_15 = sum(TOTALE_15), TOTALE_16 = sum(TOTALE_16),TOTALE_17 = sum(TOTALE_17),TOTALE_18 = sum(TOTALE_18),TOTALE_19 = sum(TOTALE_19),TOTALE_20 = sum(TOTALE_20))
