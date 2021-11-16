# assess increase in abundance with size fraction
# relies on StandingStocks

## Import data ----
library(magrittr)
library(ggplot2)

dat <- readxl::read_xlsx(here::here("data", "Net data_FORCIS dev 510_10.11.2021_V7.xlsx"),
                         skip = 1)

minSize <- 100

sizeInt <- dat %>%
  dplyr::filter(subsample_all_shells_present_were_counted == 1) %>%
  dplyr::group_by(sample_id) %>%
  dplyr::filter(dplyr::n_distinct(subsample_size_fraction_min) > 1,
                min(subsample_size_fraction_min) == minSize) %>%
  dplyr::group_by(sample_id, subsample_size_fraction_min, 
                  subsample_size_fraction_max, site_lon_start_decimal, 
                  site_lat_start_decimal, sample_min_depth) %>%
  dplyr::summarise(totalRichness = sum(species_richness_abs_subsample)) %>%
  dplyr::group_by(sample_id) %>%
  dplyr::mutate(cumuRichness = cumsum(totalRichness),
         s = cumuRichness / dplyr::first(cumuRichness))

sizeInt %>%
  ggplot(aes(site_lon_start_decimal, site_lat_start_decimal)) +
  geom_point()

p1 <- sizeInt %>%
  dplyr::filter(is.finite(s)) %>%
  ggplot(aes(subsample_size_fraction_min, cumuRichness, 
             group = sample_id, colour = log10(sample_min_depth + 1))) +
  # ggplot(aes(size_fraction_min, cumuConcentration, group = sample_id, colour = lat)) +
  # ggplot(aes(size_fraction_min, cumuConcentration, group = sample_id, colour = lon)) +
  geom_path(alpha = 0.2) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  scale_colour_viridis_c() +
  labs(title = 'Cumulative richness with size') +
  theme_bw()

p2 <- sizeInt %>%
  dplyr::filter(is.finite(s)) %>%
  dplyr::mutate(depth_bin = cut(sample_min_depth, include.lowest = TRUE, breaks = c(0, 50, 100, 250, 500, 5000)),
         lon_bin = cut(site_lon_start_decimal, include.lowest = TRUE, breaks = c(-100, -40, 35, 80)),
         lat_bin = cut(site_lat_start_decimal, include.lowest = TRUE, breaks = c(0, 30, 60, 80))) %>%
  # View()
  ggplot(aes(subsample_size_fraction_min, s)) +
  # ggplot(aes(size_fraction_min, s, colour = lat)) +
  # ggplot(aes(size_fraction_min, s, colour = lon)) +
  geom_path(aes(group = sample_id), alpha = 0.1, size = 1) +
  geom_point(aes(group = sample_id), alpha = 0.1) +
  scale_x_log10() +
  scale_y_log10() +
  # scale_colour_viridis_c() +
  geom_smooth(aes(colour = depth_bin), method = 'loess', size = 2) +
  # facet_grid(vars(lat_bin), vars(lon_bin)) +
  labs(title = 'Multiplication factor') +
  theme_bw()


p1 + p2  

# fit asymptotic regression
# https://www.statforbiology.com/nonlinearregression/usefulequations


