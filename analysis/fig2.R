library(dplyr)
library(purrr)
library(ggplot2)
library(metR)
library(here)
library(lubridate)
library(patchwork)
library(RColorBrewer)
library(rnaturalearth)
library(sf)

source(here("R/gaussfilter.R"))
source(here("R/theme_cool.R"))

set.seed(42)

# load additional datasets for plotting
coast <- ne_coastline(returnclass = "sf")
land <- ne_countries(returnclass = "sf")

# helpers -----------------------------------------------------------------
eof_geopotential_height <- function(x) {
  eof <- as_tibble(x) %>%
    group_by(lat, lon) %>%
    mutate(value = Anomaly(value) * sqrt(cos(lat * pi / 180))) %>%
    ungroup() %>%
    EOF(value ~ year | lon + lat, data = ., n = 1:4, rotate = TRUE)
}

# main --------------------------------------------------------------------


# load data for geopotential height of 500 hPa pressure level from 20CR

z500_20cr <- ReadNetCDF(here("data/20CR_z500_1851-2014_nh_MarApr.nc"),
                        subset = list(lon = -120:80, lat = 20:80,
                                      time = seq(as_date("1931-04-15"),
                                                 as_date("2014-04-15"),
                                                 by="year"))) %>%
  mutate(year = year(time)) %>%
  rename(value = hgt) %>%
  select(-level, -time_bnds, -time)

# load data for geopotential height of 500 hPa pressure level from ERA5

z500_era5 <- ReadNetCDF(here("data/ERA5_gp500_1979-2020_nh_MarApr.nc"),
                        subset = list(lon = -120:100, lat = 20:80)) %>%
  mutate(value = z / 9.81, # convert geopotential to geopotential height
         year = year(time)) %>%
  select(-z, -time_bnds, -time)

# calculate EOF
eof_z500_20cr <- eof_geopotential_height(z500_20cr)
eof_z500_era5 <- eof_geopotential_height(z500_era5)

# loadings
map2(list(eof_z500_20cr, eof_z500_era5),
     c("20CR", "ERA5"),
     ~ {
       ggplot(.x$right, aes(x=lon, y=lat)) +
         borders(fill = "grey50") +
         geom_contour(aes(z = value, lty = value > 0), binwidth = 0.008,
                      show.legend = FALSE) +
         coord_cartesian(xlim=c(-100,80), ylim=c(20,80))+
         facet_wrap(~PC) +
         ggtitle(.y) +
         theme_minimal()
     }) %>%
  wrap_plots()

# scores
bind_rows(list("ERA5" = eof_z500_era5$left, `20CR` = eof_z500_20cr$left),
          .id = "dataset") %>%
  ggplot(aes(year, value, color = PC)) +
  geom_line()+
  geom_smooth(se = FALSE, span = 0.3) +
  facet_grid(dataset ~ .)

# The loadings for PC2 (for ERA5 data) and PC4 (for 20CR data) show a EAWR
# pattern, their respective scores can therefore be used as EAWR index.

# standardize selected EOF scores

stat_refperiod <- function(dat, fun, refperiod = c(1931:1999), ...) {
  dat %>%
    filter(year %in% refperiod) %>%
    pull("value") %>%
    fun(.)
}

eof_z500_20cr_eawr <- cut(eof_z500_20cr, 4)$left %>%
  mutate(value = (value - stat_refperiod(., mean)) / stat_refperiod(., sd), # standardize
         value = -value) # invert sign of PC (the sign is random, but inversion must be also done for the loadings if done for scores)
eof_z500_era5_eawr <- cut(eof_z500_era5, 2)$left %>%
  mutate(value = (value - stat_refperiod(., mean)) / stat_refperiod(., sd), # standardize
         value = -value, # invert sign of PC (the sign is random, but inversion must be also done for the loadings if done for scores)
         value = value * 0.85 - 0.31) # manually correct the shorter reference period at standardization (0.31 is the difference of the means, 0.85 the ratio of sd of EAWR signal between 20CR and ERA5 EOF loadings)


# load EAWR reconstruction

z500_reconstruction <- read.table("data/eawr_reconstr.txt", header = TRUE) %>%
  select(1, 4) %>%
  set_names(c("time", "value")) %>%
  mutate(year = as.numeric(substr(time, 1, 4)),
         month = as.numeric(substr(time, 6, 7))) %>%
  filter(month %in% c(3, 4)) %>% #subset March and April
  group_by(year) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>% # aggregate per year
  filter(year >= 1860)



z500_reconstruction <- z500_reconstruction %>%
  mutate(value = resid(lm(value ~ year))) %>%  # detrend
  mutate(value = (value - stat_refperiod(., mean)) / stat_refperiod(., sd)) # standardize


# merge EAWR reconstruction data with EOF scores

eawr_timeseries <- list(
  "recon" = z500_reconstruction,
  "20CR" = eof_z500_20cr_eawr,
  "ERA5" = eof_z500_era5_eawr
) %>%
  bind_rows(.id = "dataset") %>%
  group_by(dataset) %>%
  mutate(value_filt = gaussfilter(value, 20))


# plot panel a

droughts <- readRDS(here("data/droughts.rds"))

(plt_a <- eawr_timeseries %>%
    ggplot() +
    geom_rect(aes(xmin = from - 0.5, xmax = to + 0.5,
                  ymin = -Inf, ymax = Inf, fill = id),
              data = droughts, alpha = 0.5, show.legend = FALSE) +
    geom_line(aes(x = year, y = value, color = dataset), size = 0.2) +
    geom_line(aes(x = year, y = value_filt, color = dataset),
              size = 0.8)+
    scale_color_manual(breaks = c("recon", "20CR", "ERA5"),
                       values = c("gray20", brewer.pal(9,"Set1")[1],
                                  brewer.pal(9,"Set1")[2]))+
    geom_hline(yintercept = 0)+
    scale_fill_manual(values = rep("lightgrey", 4))+
    scale_x_continuous(name = "Year", breaks = seq(1860, 2020, 20)) +
    scale_y_continuous(name = "EAWR", breaks = seq(-2, 2, 0.5)) +
    ggtitle("a)") +
    coord_cartesian(xlim = c(1860, 2020), ylim = c(-2, 2)) +
    theme_cool() +
    theme(plot.title = element_text(face = "bold"), legend.title = element_blank(),
          legend.position = c(0.01, 0.99), legend.justification = c(0, 1))
)


# plot panel b


(plt_b <-
    cut(eof_z500_era5, 2)$right %>%
    mutate(value = -value) %>% # invert loadings as well (scores were inverted above)
    ggplot() +
    geom_sf(data = land, color = "lightgray", fill = "lightgray") +
    geom_sf(data = coast, color = "gray40", size = 0.3) +
    geom_contour(aes(x = lon, y = lat, z = value, lty = value < 0), color = brewer.pal(9, "Set1")[2],
                 breaks = c(
                   seq(-0.04,0.014,0.005),
                   seq(0.014,0.08,0.002)
                 ), show.legend = FALSE) +
    scale_x_continuous(name = "Longitude", breaks = seq(-40, 60, 20), labels = c("40°W", "20°W", "0", "20°E", "40°E", "60°E")) +
    scale_y_continuous(name = "Latitude", breaks = seq(30, 70, 10), labels = paste0(seq(30, 70, 10), "°N")) +
    coord_sf(xlim = c(-40, 60), ylim = c(30, 72)) +
    ggtitle("b)") +
    theme_cool() +
    theme(plot.title = element_text(face = "bold"))
)

# load HISTALP precipitation data and regress against mean EAWR reconstruction

prec <- read.table(here("data/R01_LOW_MarApr.txt"), header = FALSE) %>%
  set_names("year", "prec")

model_dat <- eawr_timeseries  %>%
  group_by(year) %>%
  summarize(eawr = mean(value, na.rm = TRUE)) %>%
  left_join(prec)

linear_model <- lm(prec ~ eawr, data = model_dat)

summary(linear_model) # highly significant link


# plot panel c
plt_c_annotation <- paste0("r²: ",round(summary(linear_model)$r.squared,2),"\np.value: < 0.001")

(plt_c <-
    ggplot(model_dat) +
    geom_point(aes(x = eawr, y = prec), color = "darkgrey") +
    stat_smooth(aes(x = eawr, y = prec), method = "lm", se = FALSE,
                fullrange = TRUE, color = "black", size = 0.5) +
    stat_density2d(aes(x = eawr, y = prec, color = ..level..),
                   show.legend = FALSE, size = 0.2) +
    geom_hline(yintercept = 0, size = 0.2) +
    geom_vline(xintercept = 0, size = 0.2) +
    annotate(geom = "text", x = 1.5, y = 60, label = plt_c_annotation) +
    scale_x_continuous(limits = c(-2.4, 2.4), name = "EAWR") +
    scale_y_continuous(name = "ES precipitation anomaly [%]",
                       limits = c(-75, 75), breaks = seq(-60, 60, 20)) +
    ggtitle("c)") +
    coord_cartesian(expand = FALSE) +
    theme_cool() +
    theme(plot.title = element_text(face = "bold"))
)

# save composite plot to disk

plt_a / (plt_b | plt_c) + plot_layout(widths = c(1,0.8))

ggsave(here("output/fig2.png"), width = 10.5, height = 8, type = "cairo-png")
