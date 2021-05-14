library(tidyverse)
library(openxlsx)
library(patchwork)

# import data
dat_all <- read.xlsx('~/Downloads/Net data_FORCIS dev 510_11.05.2021_V5.xlsx', startRow = 2)

# subset columns
dat <- dat_all[, c(grep('site_id', names(dat_all)),
               grep('profile_id', names(dat_all))[1],
               # grep('cast_id', names(dat_all))[1],
               grep('profile_depth_min', names(dat_all))[1],
               grep('profile_depth_max', names(dat_all))[1],
               grep('stratified', names(dat_all)),
               grep('sample_id', names(dat_all))[1],
               grep('subsample_id', names(dat_all))[1],
               grep('lon_start_decimal', names(dat_all)),
               grep('lat_start_decimal', names(dat_all)),
               grep('sample_date_time_start', names(dat_all))[1],
               grep('sample_include_in_profile', names(dat_all)),
               # grep('date_of_station', names(dat_all))[1],
               grep('sample_min_depth', names(dat_all)),
               grep('sample_max_depth', names(dat_all)),
               grep('size_fraction_min', names(dat_all))[1],
               grep('size_fraction_max', names(dat_all)),
               grep('living_or_dead', names(dat_all)),
               grep('all_species_counted_bool', names(dat_all)),
               grep('total_abundance_lumped_tax_number_m3_abs_lump', names(dat_all)),
               grep('concentration', names(dat_all))
)] %>%
  tibble() %>%
  rename(lon = site_lon_start_decimal,
         lat = site_lat_start_decimal,
         date = sample_date_time_start,
         concentration = total_abundance_lumped_tax_number_m3_abs_lump) %>%
  arrange(profile_id, sample_min_depth)

# standing stocks down to level

level <- 100

standingstock <- dat %>%
  # filter(site_id == 'FAEGAS_I_F1-ZA2' | site_id == 'AtlantisCruise_St1') %>%
  filter(all_species_counted_bool == 1) %>%
  filter(sample_include_in_profile == TRUE) %>%
  filter(profile_depth_min == 0 & profile_depth_max >= level) %>%
  group_by(site_id, profile_id, sample_id, lon, lat, sample_min_depth, sample_max_depth, date) %>%
  summarise(concentration = sum(concentration)) %>% # sum subsamples
  arrange(site_id, profile_id, sample_min_depth) %>%
  mutate(foo = lead(sample_min_depth, default = max(sample_max_depth)),
         gap = sample_max_depth - foo != 0) %>%
  group_by(site_id, profile_id) %>%
  mutate(gapInProfile = any(gap)) %>%
  ungroup() %>%
  filter(gapInProfile == FALSE,
         sample_min_depth < level) %>%
  mutate(intervalHeight = sample_max_depth - sample_min_depth,
         fractionAboveLevel = case_when(sample_max_depth <= level ~ 1,
                                        TRUE ~ 1 - (sample_max_depth - level)/intervalHeight),
         nForam = concentration * intervalHeight * fractionAboveLevel) %>%
  group_by(site_id, profile_id, lon, lat, date) %>%
  summarise(sumForam = sum(nForam)) %>%
  ungroup()

standingstock %>%
  mutate(logSumForam = log10(sumForam + 1)) %>%
  ggplot(aes(lon, lat, colour = logSumForam)) +
  geom_point() +
  scale_colour_viridis_c()
