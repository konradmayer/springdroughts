theme_cool <- function(base_size = 12, base_family = "Helvetica")
{
  col="grey20"
  ggplot2::theme_light(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      #panel.grid.major = element_line(colour = 'grey50', linetype = 'dotted', size=0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = col, fill=NA, size=0.5),
      axis.ticks.length = unit(-3 , "pt"),
      #axis.ticks.margin = unit(0.6 , "lines"),
      axis.ticks = element_line(size = 0.2, colour=col),
      # axis.text.x = element_text(margin=unit(rep(0.7,4), "lines"), colour=col),
      # axis.text.y = element_text(margin=unit(rep(0.7,4), "lines"), colour=col, hjust = 1),
      # axis.title.x=element_text(margin=unit(rep(0,4), "lines"), colour=col),
      # axis.title.y=element_text(margin=unit(rep(0,4), "lines"),angle=90, colour=col),
      strip.background = element_rect(fill="lightgrey", color="grey20"),
      strip.text.y = element_text(angle = 90, color="grey20"),
      strip.text.x = element_text(color="grey20"),
      legend.background = element_rect(fill="white", colour=NA),
      legend.key = element_rect(colour = NA)
      #legend.key.height = unit(3, units="line"),
      #legend.title = element_text(angle = 90, vjust = 1),
      #legend.title.align = 0.5
    )
}
