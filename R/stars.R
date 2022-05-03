st_time <- function(x) {
  stars::st_get_dimension_values(x, "time")
}

st_time2date <- function(x) {
  dates <- lubridate::as_date(stars::st_get_dimension_values(x, "time"))
  stars::st_set_dimensions(x, "time", values = dates)
}


st_rectangle <- function(xmin, xmax, ymin, ymax, crs = 4326) {
  poly <- sf::st_polygon(list(cbind(c(xmin,xmax,xmax,xmin,xmin),
                                c(ymin,ymin,ymax,ymax,ymin))))
  sf::st_sfc(poly, crs = crs)
}

