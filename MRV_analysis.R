library(tidyverse)
library(readxl)
library(prismatic)
library(patchwork)

emission_factors <- tibble(ef = c(6.35, 3.2, 3.14, 2.75),
                           name = c("q1","MDO","HFO","q2"))

ef_breaks <- c(Inf,
               emission_factors$ef[1:(length(emission_factors$ef)-1)] + (emission_factors$ef %>% diff()) / 2,
               -Inf)

emission_factors <- emission_factors %>% 
  mutate(ef_class = cut(ef, breaks = ef_breaks))

this_year <- 2019
this_report <- dir(pattern = str_c("^",as.character(this_year), ".*.xlsx"))
this_version <- str_extract(this_report, pattern = "-v[0-9]*-") %>%
  str_remove_all("[-v]*") %>%
  as.integer()

data <- read_excel(this_report, skip = 2)

data_clean <- data %>% 
  filter(`Total CO₂ emissions [m tonnes]` != 0) %>% 
  mutate(ef = `Total CO₂ emissions [m tonnes]`/ `Total fuel consumption [m tonnes]`,
         across(.cols = `Total fuel consumption [m tonnes]`:ef, .fns = as.numeric),
         ef_class = cut(ef, ef_breaks)) %>% 
  left_join(emission_factors %>% dplyr::select(ef_ideal = ef, ef_class)) %>% 
  mutate(ef_diff = ef - ef_ideal)

data_clean %>% 
  ggplot(aes(x = ef)) +
  geom_density(adjust = .3) +
  scale_x_continuous(limits = c(2, 6.5))

data_clean %>% 
  ggplot(aes(x = ef, 
             y = log10(`Annual average CO₂ emissions per transport work (mass) [g CO₂ / m tonnes · n miles]`))) +
  geom_vline(data = emission_factors,
              aes( xintercept = ef, color = name)) +
  geom_point(aes(fill = ef_class, color = after_scale(clr_darken(fill))), shape = 21 ) +
  scale_color_brewer(palette = "Set1") #+
  # facet_wrap(`Ship type` ~ .)

data_clean %>% 
  filter(ef < 2.5) %>% 
  dplyr::select(`IMO Number`:`Ship type`, `Technical efficiency`,
                `Total fuel consumption [m tonnes]`, `Total CO₂ emissions [m tonnes]`,
                `Annual average CO₂ emissions per transport work (mass) [g CO₂ / m tonnes · n miles]`) %>% 
  knitr::kable()


data_clean %>% 
  filter(`Total CO₂ emissions [m tonnes]` > 1.5e5) %>% 
  dplyr::select(`IMO Number`:`Ship type`, `Technical efficiency`,
                `Total fuel consumption [m tonnes]`, `Total CO₂ emissions [m tonnes]`,
                `Annual average CO₂ emissions per transport work (mass) [g CO₂ / m tonnes · n miles]`) %>% 
  knitr::kable()

data_clean %>% 
  ggplot(aes(x = `Total CO₂ emissions [m tonnes]`,
             y = `Total fuel consumption [m tonnes]`)) +
  geom_abline(data = emission_factors,
              aes( intercept = 0, slope = 1/ef, color = name)) +
  geom_point(aes(fill = `Ship type`, color = after_scale(clr_darken(fill))), shape = 21 ) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(`Ship type` ~ .)

data_clean %>% 
  ggplot(aes(x = `Total CO₂ emissions [m tonnes]`,
             y = `Total fuel consumption [m tonnes]`)) +
  geom_abline(data = emission_factors,
              aes( intercept = 0, slope = 1/ef, color = name)) +
  geom_point(aes(fill = ef_class, color = after_scale(clr_darken(fill))), shape = 21 ) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(`Ship type` ~ .)
