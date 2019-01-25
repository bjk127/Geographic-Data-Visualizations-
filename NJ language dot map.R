library(tidycensus)
library(tidyverse)
library(sf)


vars <- c(English = "C16002_002", 
          Spanish = "C16002_003", 
          IndoEurope = "C16002_006", 
          Asian = "C16002_009", 
          Other = "C16002_012")


language <- get_acs(geography = "county", 
                    state = "NJ",
                    table = "C16001")

vars <- c(English = "C16002_002", 
          Spanish = "C16002_003", 
          IndoEurope = "C16002_006", 
          Asian = "C16002_009", 
          Other = "C16002_012")


nj_language <- get_acs(geography = "tract", 
                       variables = vars,
                       state = "NJ", 
                       geometry = TRUE, 
                       summary_var = "C16002_001") %>%
  st_transform(26918)


nj_dots <- map(names(vars), ~{
  nj_language %>%
    filter(variable == .x) %>%
    st_sample(., size = .$estimate / 10) %>%
    st_sf() %>%
    mutate(group = .x)
}) %>%
  reduce(rbind) %>%
  group_by(group) %>%
  summarize()


dot_map <- ggplot(nj_dots, aes(color = group, fill = group)) + 
  geom_sf(shape = "*") + 
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~group)



#Other variables of interest for NJ

library(tidycensus)


pop10 <- get_decennial(geography = "state", variables = "P001001")


median_income <- get_acs(geography = "state", 
                         variables = "B19013_001")



immig_rate <- get_estimates(geography = "state", 
                            variables = "RINTERNATIONALMIG")

vars <- load_variables(2016, "acs5", cache = TRUE)

View(vars)


other_lang <- get_acs(geography = "state", 
                      variables = "DP02_0112P", 
                      year = 2017, 
                      survey = "acs1")


nj_otherlang <- get_acs(geography = "county", 
                        variables = "DP02_0112P", 
                        state = "NJ")

foreign <- get_acs(geography = "county",
                   variables = "B05002_013",
                   state = "NJ")

nj_otherlang <- get_acs(geography = "county", 
                        variables = c(other_lang = "DP02_0112P"), 
                        state = "NJ")

nj_otherlang %>%
  mutate(NAME = gsub(" County, New Jersey", "", NAME)) %>%
  ggplot(aes(x = estimate, y = reorder(NAME, estimate))) +
  geom_errorbarh(aes(xmin = estimate - moe, xmax = estimate + moe)) +
  geom_point(color = "red", size = 2) +
  labs(title = "Percent of population by NJ county that speaks a language other than English",
       subtitle = "2012-2016 American Community Survey",
       y = "",
       x = "ACS estimate (bars represent margin of error)")




