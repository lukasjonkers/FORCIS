# assess increase in abundance with size fraction
# relies on StandingStocks

minSize <- 100

sizeInt <- dat %>%
  # filter(sample_id == 'M10-2_250B1_1_250-315') %>%
  filter(all_species_counted_bool == 1) %>%
  group_by(sample_id) %>%
  filter(n_distinct(size_fraction_min) > 1,
         min(size_fraction_min) == minSize) %>%
  group_by(sample_id, size_fraction_min, size_fraction_max, lon, lat, sample_min_depth) %>%
  summarise(totalConcentration = sum(concentration)) %>%
  group_by(sample_id) %>%
  mutate(cumuConcentration = cumsum(totalConcentration),
         s = cumuConcentration/first(cumuConcentration))

sizeInt %>%
  ggplot(aes(lon, lat)) +
  geom_point()

p1 <- sizeInt %>%
  filter(is.finite(s)) %>%
  ggplot(aes(size_fraction_min, cumuConcentration, group = sample_id, colour = log10(sample_min_depth + 1))) +
  # ggplot(aes(size_fraction_min, cumuConcentration, group = sample_id, colour = lat)) +
  # ggplot(aes(size_fraction_min, cumuConcentration, group = sample_id, colour = lon)) +
  geom_path(alpha = 0.2) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  scale_colour_viridis_c() +
  labs(title = 'Cumulative abundance with size') +
  theme_bw()

p2 <- sizeInt %>%
  filter(is.finite(s)) %>%
  mutate(depth_bin = cut(sample_min_depth, include.lowest = TRUE, breaks = c(0, 50, 100, 250, 500, 5000)),
         lon_bin = cut(lon, include.lowest = TRUE, breaks = c(-100, -40, 35, 80)),
         lat_bin = cut(lat, include.lowest = TRUE, breaks = c(0, 30, 60, 80))) %>%
  # View()
  ggplot(aes(size_fraction_min, s)) +
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
