time_range <- ymd(c(str_c(YEAR, "-01-01"), str_c(YEAR, "-12-31")))

# tidying grant data ------------------------------------------------------

grants <-
  grants_raw %>%
  select(
    funding_instrument_hierarchy,
    discipline_number,
    university,
    approved_amount,
    start_date
  ) %>%
  mutate(start_date = dmy(start_date)) %>%
  filter(
    between(start_date, time_range[1], time_range[2]),
    !str_detect(approved_amount, "not included")
  ) %>%
  mutate(
    instrument = map_chr(funding_instrument_hierarchy,
                         function(x)
                           pluck(str_split(x, ";"), 1, 1)),
    uni = str_extract(university, "(?<=( [-] ))(.*)$"),
    uni = fct_lump(uni, n = 12) %>% as.character(),
    approved_amount = as.numeric(approved_amount),
    domain = str_sub(discipline_number, 1, 1),
    domain = case_when(
      domain == '1' ~ "SSH ",
      domain == '2' ~ "STEM ",
      domain == '3' ~ "BIOMED ",
      is.na(domain) ~ "NA ",
      TRUE ~ "NA "))  %>%
  select(approved_amount, instrument, uni, domain)

# uid for each node -------------------------------------------------------

nodes <-
  tibble(name = grants %$% unique(c(instrument, uni, domain))) %>%
  rowid_to_column("id") %>%
  mutate(id = id - 1) # start at 0

# source -> middle --------------------------------------------------------

links_left <- grants %>%
  group_by(instrument, uni) %>%
  summarise(value = sum(approved_amount)) %>%
  ungroup() %>%
  left_join(nodes, by = c("instrument" = "name")) %>%
  rename("source" = id) %>%
  left_join(nodes, by = c("uni" = "name")) %>%
  rename("target" = id) %>%
  select(source, target, value)

# middle -> end -----------------------------------------------------------

links_right <- grants %>%
  group_by(uni, domain) %>%
  summarise(value = sum(approved_amount)) %>%
  ungroup() %>%
  left_join(nodes, by = c("uni" = "name")) %>%
  rename("source" = id) %>%
  left_join(nodes, by = c("domain" = "name")) %>%
  rename("target" = id) %>%
  select(source, target, value)

links <- bind_rows(links_left, links_right)
nodes <- nodes %>% select(-id) %>%
  as.data.frame(stringsAsFactors = FALSE)

links <- links %>%
  mutate(source = as.integer(source),
         target = as.integer(target)) %>%
  as.data.frame(stringsAsFactors = FALSE)
