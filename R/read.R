grants_raw <-
  "http://p3.snf.ch/P3Export/P3_GrantExport.csv" %>%
  read_csv2(guess_max = 50000) %>%
  clean_names()
