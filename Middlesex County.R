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

vars <- load_variables(2016, "acs5", cach = TRUE)

#Each codes refering to number of people speaking each of those languages. 

vars <- c(English = "C16002_002", 
          Spanish = "C16002_003", 
          IndoEurope = "C16002_006", 
          Asian = "C16002_009", 
          Other = "C16002_012")


ms_language <- get_acs(geography = "tract", 
                       variables = vars,
                       state = "NJ", 
                       county = "Middlesex", 
                       geometry = TRUE, 
                       summary_var = "C16002_001") %>%
  st_transform(26918)

#Above specifying C1002_001 represents total population. We want to know proportion of households.
#Divide each one by C1002_001. 

ms_prop <- ms_language %>%
  mutate(prop = estimate / summary_est, 
         prop_moe = moe_prop(estimate, 
                             summary_est, 
                             moe, 
                             summary_moe))


ms_entropy <- ms_prop %>%
  group_by(GEOID) %>%
  summarize(entropy = sum(prop * log(1 / prop), 
                          na.rm = TRUE)) 


entropy_map <- ggplot(ms_entropy, aes(fill = entropy)) + 
  geom_sf() + 
  coord_sf(datum = NA) + 
  scale_fill_distiller(palette = "PRGn", direction = 0) + 
  theme_minimal()

#This could be used with other languages. 
prop_maps <- ggplot(ms_prop, aes(fill = prop, color = prop)) + 
  geom_sf() + 
  coord_sf(datum = NA) + 
  facet_wrap(~variable) + 
  scale_fill_viridis_c() + 
  scale_color_viridis_c()


#articulate that 1 dot is per 25 households. 
ms_dots <- map(names(vars), ~{
  ms_language %>%
    filter(variable == .x) %>%
    st_sample(., size = .$estimate / 10) %>%
    st_sf() %>%
    mutate(group = .x)
}) %>%
  reduce(rbind) %>%
  group_by(group) %>%
  summarize()


dot_map <- ggplot(ms_dots, aes(color = group, fill = group)) + 
  geom_sf(shape = "*") + 
  scale_color_brewer(palette = "Set1") + 
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~group)
