#' Compute some key figures for a sphere
#' @title Solid Geometry Calculator for spheres
#'
#' @param radius_mm The radius of the sphere in millimeters.
#'
#' @return Prints the diameter, scope, circular area, surface and the volume of a sphere for the input radius_mm
#' @export
#'
#' @examples
#' calculate_sphere(radius_mm=1.4)

calculate_sphere <- function(radius_mm) {
  # Error handling
  if(!is.numeric(radius_mm)) {
    stop("radius is not numeric. \n Please input a numeric value!")
  }
  # Calculations
  diameter <- 2 * radius_mm
  scope <- 2 * pi * radius_mm
  circular_area <- pi * radius_mm^2
  surface <- 4 * pi * radius_mm^2
  volume <- (4/3) * pi * radius_mm^3

  # Output
  cat("Calculations for a sphere with radius =", radius_mm, "mm:\n")
  cat("------------------------------------------------------------\n")
  cat("Diameter:        ", diameter, "mm\n")
  cat("Scope:           ", round(scope, 2), "mm\n")
  cat("Circular Area:   ", round(circular_area, 2), "mm²\n")
  cat("Surface:         ", round(surface, 2), "mm²\n")
  cat("Volume:          ", round(volume, 2), "mm³\n")
}

