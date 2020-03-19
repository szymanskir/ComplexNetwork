point <- function(x, y) {
  structure(
    list(x = x, y = y),
    class = "point"
  )
}

is_point <- function(point) {
  inherits(point, "point")
}

assert_point <- function(point) {
  
}