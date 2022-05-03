smooth_vec <- function(x, period = 30, span = NULL, degree = 2) {

  if (!is.numeric(x)) stop("'x' must be numeric.")

  if (is.null(span)) {
    span <- period / length(x)
  }

  model_loess <- stats::loess(x ~ seq_along(x), span = span, degree = degree)

  stats::predict(model_loess, seq_along(x))
}
