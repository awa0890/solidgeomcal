#' Compute some key figures for a sphere
#' @title Solid Geometry Calculator for spheres
#'
#' @param radius The radius of the sphere in millimeters. The User can select the input unit between millimeter ("mm"), centimeter ("cm") and meter ("m").
#'
#' @return Prints the diameter, scope, circular area, surface and the volume of a sphere for the input radius in the selected unit millimeter mm, centimeter cm or meter m.
#' @export
#'
#' @examples
#' calculate_sphere(radius=1.4, unit = "mm")

calculate_sphere <- function(radius, unit = "mm") {
  # Error handling
  if(!is.numeric(radius)) {
    stop("radius is not numeric. \n Please input a numeric value!")
  }
  if (!unit %in% c("mm", "cm", "m")) {
    stop("unit must be \"mm\", \"cm\" or \"m\".")
  }
  if(!is.character(unit)) {
    stop("unit must be a character. \n Please check your input!")
  }
  # Calculations
  if (unit == "cm") {
    radius_mm <- radius*10
  } else if(unit == "m") {
    radius_mm <- radius*1000
  } else {
    radius_mm <- radius
  }
  diameter <- 2 * radius_mm
  scope <- 2 * pi * radius_mm
  circular_area <- pi * radius_mm^2
  surface <- 4 * pi * radius_mm^2
  volume <- (4/3) * pi * radius_mm^3

  # Output
  cat("Calculations for a sphere with radius =", radius, "", unit, ":\n")
  cat("------------------------------------------------------------\n")
  cat("Diameter:        ", diameter, "mm\n")
  cat("Scope:           ", round(scope, 2), "mm\n")
  cat("Circular Area:   ", round(circular_area, 2), "mm²\n")
  cat("Surface:         ", round(surface, 2), "mm²\n")
  cat("Volume:          ", round(volume, 2), "mm³\n")
}

