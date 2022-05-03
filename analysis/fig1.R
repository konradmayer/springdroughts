library(dplyr)
library(purrr)
library(here)
library(ggplot2)
library(RColorBrewer)
library(ggnewscale)

source(here("R/gaussfilter.R"))
source(here("R/theme_cool.R"))

# load HISTALP data and smooth
dat <- read.table(here("data/R01_LOW_MarApr.txt"), header = FALSE) %>%
  set_names("yy", "pr_ano") %>%
  mutate(pr_ano_filt = gaussfilter(pr_ano, 20))

# identify drought periods
sign_change_group <- function(x) {
  as.factor(unlist(map(seq_along(x), ~ sum(.x > which(diff(sign(x)) != 0)))))
}

droughts <- dat %>%
  mutate(id = sign_change_group(dat$pr_ano_filt)) %>%
  group_by(id) %>%
  mutate(major = any(pr_ano_filt < -sd(dat$pr_ano_filt)),
         minor = any(pr_ano_filt < -sd(dat$pr_ano_filt) / 2)) %>%
  summarize(from = min(yy), to = max(yy),
            major = any(major), minor = any(minor)) %>%
  filter(major | minor) %>%
  mutate(mid = (from + to) / 2)


# plot fig1
(p1 <- ggplot() +
    geom_rect(aes(xmin = from - 0.5, xmax = to + 0.5,
                  ymin = -Inf, ymax = Inf, fill = id),
              data = droughts, alpha = 0.5) +
    scale_fill_manual(values = rep(c("grey90", "grey70"), 2),
                      breaks = droughts$id) +
    new_scale_fill() +
    geom_col(aes(x = yy, y = pr_ano, fill = pr_ano < 0),
             data = subset(dat, yy >= 1850)) +
    scale_fill_manual(values = c(brewer.pal(9, "Paired")[7],
                                 brewer.pal(9, "Paired")[1]),
                      breaks = c(TRUE, FALSE)) +
    geom_line(data = subset(dat, yy %in% 1870:2011),
              aes(x = yy, y = pr_ano_filt), size = 0.5) +
    geom_line(data = subset(dat, yy %in% c(1860:1870)),
              aes(x = yy, y = pr_ano_filt), size = 0.5, linetype = "dashed") +
    geom_line(data = subset(dat, yy %in% c(2011:2020)),
              aes(x = yy, y = pr_ano_filt), size = 0.5, linetype = "dashed") +
    annotate(geom = "label", x = unlist(droughts[droughts$major, "mid"]),
             y = 55, label = "Major\nES drought period", color = "grey20",
             size = 3, fontface = 2) +
    annotate(geom = "label", x = unlist(droughts[!droughts$major, "mid"]),
             y = -55, label = "Minor\nES drought period", color = "grey50",
             size = 3) +
    geom_hline(yintercept = sd(dat$pr_ano_filt) * c(-1, 1), size = 0.2,
               linetype = "dashed") +
    scale_x_continuous(name = "", breaks = seq(1800, 2020, 20)) +
    scale_y_continuous(name = "ES precipitation anomaly [%]",
                       breaks = seq(-60, 60, 20)) +
    coord_cartesian(xlim = c(1860, 2020), ylim = c(-60, 60)) +
    theme_cool() +
    theme(panel.grid.major = element_blank(), legend.position = "none")
)

# write to disk
ggsave(here("output/fig1.png"), width = 10.5, height = 4.5, type = "cairo-png")

saveRDS(droughts, here("data/droughts.rds"))
