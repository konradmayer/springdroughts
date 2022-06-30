library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(here)
library(stars)
library(patchwork)
library(rnaturalearth)


source(here("R/gaussfilter.R"))
source(here("R/theme_cool.R"))
source(here("R/stars.R"))
source(here("R/loess_smoother.R"))

# helpers ----------------------------------------------------------------------

average_season <- function(x, months_sub, lon = "longitude", lat = "latitude") {
  x <- filter(x, month(time) %in% months_sub)
  years <- unique(year(st_time(x)))
  x <- st_apply(
    x, c(lon, lat),
    function(.x) tapply(.x, year(st_time(x)), mean)
  ) %>%
    st_set_dimensions(which = 1, values = years, names = "time")
  x
}

calculate_anomaly <- function(x, time_subset,
                              lon = "longitude", lat = "latitude") {
  idx_drought <- which(st_time(x) %in% time_subset)
  anom <- st_apply(
    x, c(lon, lat),
    function(.x) mean(.x[idx_drought] - mean(.x, na.rm = TRUE))
  )
}

crop_domain <- function(x, xmin = -100, ymin = 0,
                        xmax = 60, ymax = 75, crs = 4326) {
  bb <- st_bbox(c(xmin = xmin, xmax = xmax, ymax = ymax, ymin = ymin),
                crs = crs
  )
  st_crop(x, bb)
}

label <- function(l) {
  ifelse(seq_along(l) %% 2 == 0, sprintf("%.1f", l), "") # only display every second label
}

# misc -------------------------------------------------------------------------

# parameters
months_sub <- c(3, 4) # march and april
box_forcing <- st_rectangle(-78, -58, 30, 45) # forcing region
box_response <- st_rectangle(-48, -28, 30, 45) # response region


# load additional dataset for plotting
land <- ne_countries(returnclass = "sf")

# sea surface temperature ------------------------------------------------------

sst <- read_ncdf(here("data/ersst.v5.1854-2021_remap.nc"),
                 var = "sst", proxy = FALSE
) %>%
  st_time2date() %>%
  st_set_dimensions(which = "lon", names = "longitude") %>%
  st_set_dimensions(which = "lat", names = "latitude") %>%
  crop_domain()

# subset march and april and calculate average for this season (i.e. annual avg)
sst <- average_season(sst, months_sub)


# calculate sst anomaly using residues from temporal linear detrending
linear_detrend <- function(x) {
  if (all(is.na(x))) {
    return(x)
  } else {
    return(as.numeric(resid(lm(x ~ seq_along(x)))))
  }
}

sst_dtr <- st_apply(sst, c("longitude", "latitude"), linear_detrend, keep = TRUE)


# sea level pressure -----------------------------------------------------------

slp <- read_ncdf(here("data/era5_mean_sea_level_pressure_1979_2020.nc"),
                 var = "msl", proxy = FALSE
) %>%
  crop_domain()

# convert fro Pa to hPA
slp <- slp / 100

# subset march and april and calculate average for this season (i.e. annual avg)
slp <- average_season(slp, months_sub)


# trends in SST ----------------------------------------------------------------

# helper function which calculates lagged differences and applies NA padding
differences_vec <- function(x, period = 30, lag = 10) {
  if (all(is.na(x))) {
    return(rep(NA, length(x)))
  } else {
    x %>%
      smooth_vec(period = period) %>%
      diff(., lag = lag) %>%
      c(rep(NA, lag), .)
  }
}

# calculate 10 year differences
sst_diff <- st_apply(sst_dtr, c("longitude", "latitude"), differences_vec,
                     keep = TRUE
)

# aggregate differences in forcing and response region
diff_forcing <- st_apply(sst_diff[box_forcing], "time", mean, na.rm = TRUE)[[1]]
diff_response <- st_apply(sst_diff[box_response], "time", mean, na.rm = TRUE)[[1]]

# prepare data for plotting
df_ts <- tibble(
  year = st_time(sst_dtr),
  `response region` = diff_response,
  `forcing region` = diff_forcing
) %>%
  pivot_longer(-year, names_to = "region", values_to = "value")


droughts <- readRDS(here("data/droughts.rds"))

# plot panel b of fig3
(plt_ts <- ggplot(data = df_ts) +
  geom_rect(aes(xmin = from - 0.5, xmax = to + 0.5,
                ymin = -Inf, ymax = Inf, fill = id),
            data = droughts, alpha = 0.5, show.legend = FALSE) +
  scale_fill_manual(values = rep(c("grey90", "grey70"), 2),
                    breaks = droughts$id) +
  geom_hline(yintercept = 0, col = "gray50") +
  geom_line(aes(year, value, color = region), size = 1) +
  scale_color_manual(
    values = c("gray20", "gray60"),
    breaks = c("forcing region", "response region")
  ) +
  scale_y_continuous(
    name = "Differences of\nsmoothed SST anomalies [°C]",
    breaks = seq(-1, 1, by = 0.2)
  ) +
  scale_x_continuous(name = "Year", breaks = seq(1800, 2020, by = 20)) +
  theme_cool() +
  ggtitle("b)") +
  theme(
    panel.grid.major = element_blank(), legend.position = c(0.21, 0.9),
    legend.title = element_blank(),
    legend.background = element_rect(color = NA, fill = NA),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.text = element_text(size = 8),
    plot.title = element_text(face = "bold")
  ) +
  coord_cartesian(xlim = c(1883, 2020)) +
  geom_point(aes(year, value),
             data = . %>% filter(year == 2013), size = 3, show.legend = FALSE) +
  annotate(geom = "label", x = 2014, y = -0.7,
             label = "ESD+2006",
             color = "grey20",
             size = 3, fontface = 2)
)

# Within the recent drought period, in the 10 years up to year 2013 there is a
# strong warming trend apparent, while differences are close to 0 for the
# response region (i.e. not yet responding).
# 2013 is therefore used in panel a to show the spatial distribution of the
# differences of the smoothed SST anomalies at for this point in time,
# and the mean sea level pressure for the same time period (2013 marked in panel
# b with points).

recent_pre <- st_apply(filter(sst_diff, time %in% 2013),
                       c("latitude", "longitude"), mean,
                       na.rm = TRUE
)
recent_pre[[1]][is.na(recent_pre[[1]])] <- 0 # workaround for smoother outlines in geom_contour around the coastlines

# calculate anomaly for the same time frame as the shown 10 year difference
slp_anom <- calculate_anomaly(slp, time_subset = 2003:2013)

# plot panel a of fig3
plt_spatial <- ggplot() +
  geom_contour_filled(
    data = as_tibble(recent_pre), aes(longitude, latitude, z = `mean`),
    breaks = c(seq(-0.6, 0.7, by = 0.1), Inf)
  ) +
  metR::scale_fill_divergent_discretised(label = label) +
  geom_sf(data = land, fill = "gray80", color = "gray80") +
  geom_contour(aes(longitude, latitude, z = msl, lty = msl < 0),
               data = as_tibble(slp_anom), color = "gray20", size = 0.3,
               show.legend = FALSE, breaks = c(
                 -seq(30, 0.45, by = -0.2),
                 seq(0.45, 30, by = 0.2)
               )
  ) +
  scale_linetype_manual(values = c(1, 3)) +
  geom_sf(data = box_forcing, color = "gray20", fill = "transparent", size = 1) +
  geom_sf(data = box_response, color = "gray60", fill = "transparent", size = 1) +
  labs(fill = "SST difference\n in ESD+2006", x = "Longitude", y = "Latitude",
       title = "a)") +
  theme_cool() +
  coord_sf(expand = TRUE, xlim = c(-90, 50), ylim = c(5, 70)) +
  theme(
    legend.position = c(0.73, 0.2), legend.direction = "horizontal",
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8, hjust = 1),
    plot.title = element_text(face = "bold")
  )


# save composite plot to disk
plt_spatial + plt_ts
ggsave(here("output/fig3.png"), width = 10.5, height = 3.5, type = "cairo-png")
