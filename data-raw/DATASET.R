library("readr")
library("RCurl")
library("janitor")
library("stringr")
library("lubridate")
library("dplyr")

grants <-
  RCurl::getURL("http://p3.snf.ch/P3Export/P3_GrantExport.csv")

grants <- grants %>%
  readr::read_csv2() %>%
  janitor::clean_names() %>%
  dplyr::select(
    instrument = funding_instrument_hierarchy,
    domain = discipline_number,
    institution = university,
    amount = approved_amount,
    start = start_date
  )

grants <- grants %>%
  dplyr::mutate(
    domain = stringr::str_sub(domain, 1, 1),
    # "round" domain names
    domain = dplyr::case_when(
      domain == "1" ~ "SSH ",
      domain == "2" ~ "STEM ",
      domain == "3" ~ "BIOMED ",
      TRUE ~ "NA "
    )
  )

# remove derivate instruments

main_instruments <- c(
  "Careers",
  "Infrastructure",
  "Programmes",
  "Project funding",
  "Science communication"
)

grants <- grants %>%
  dplyr::mutate(
    instrument = dplyr::case_when(
      stringr::str_detect(
        instrument,
        stringr::regex(main_instruments[1],
                       ignore_case = TRUE)
      ) ~ main_instruments[1],
      stringr::str_detect(
        instrument,
        stringr::regex(main_instruments[2],
                       ignore_case = TRUE)
      ) ~ main_instruments[2],
      stringr::str_detect(
        instrument,
        stringr::regex(main_instruments[3],
                       ignore_case = TRUE)
      ) ~ main_instruments[3],
      stringr::str_detect(
        instrument,
        stringr::regex(main_instruments[4],
                       ignore_case = TRUE)
      ) ~ main_instruments[4],
      stringr::str_detect(
        instrument,
        stringr::regex(main_instruments[5],
                       ignore_case = TRUE)
      ) ~ main_instruments[5],
      TRUE ~ "Other"
    )
  )

# clean institution names

grants <- grants %>%
  dplyr::mutate(
    institution = stringr::str_replace(institution,
                                       "(.*)[[:space:]]+[-][[:space:]]+",
                                       ""),
    institution = stringr::str_replace(institution,
                                       "IACH",
                                       "International"),
    institution = stringr::str_replace_na(institution, "Other"),
    institution = stringr::str_c(institution, " ")
  )

# let dates be dates

grants <- grants %>%
  dplyr::mutate(start = lubridate::dmy(start))

usethis::use_data(grants)
