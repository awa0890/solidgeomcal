#' Compute some key figures for a cone
#' @title Solid Geometry Calculator for cones
#'
#' @param radius_mm The radius of the cone in millimeters.
#' @param height_mm The height of the cone in millimeters.
#'
#' @return Prints the diameter, scope, base area, line surface, lateral surface, surface and the volume of a cone for the inputs radius_mm und height_mm
#' @export
#'
#' @examples
#' calculate_cone(radius_mm=50, height_mm=20)

calculate_cone <- function(radius_mm, height_mm) {
  # Error handling
  if(!is.numeric(radius_mm)| !is.numeric(height_mm)) {
    stop("radius or height is not numeric. \n Please check your inputs!")
  }
  # Calculations
  diameter <- 2 * radius_mm
  scope <- 2 * pi * radius_mm
  base_area <- pi * radius_mm^2
  surface_line <- sqrt(radius_mm^2 + height_mm^2)
  surface_lateral <- pi * radius_mm * surface_line
  surface <- (pi * radius_mm^2) + (pi * radius_mm * scope)
  volume <- (1/3) * pi * radius_mm^2 * height_mm

# Output
  cat("Calculations for a cone with radius =", radius_mm, "mm and height =", height_mm, "mm:\n")
  cat("------------------------------------------------------------\n")
  cat("Diameter:          ", diameter, "mm\n")
  cat("Scope:             ", round(scope, 2), "mm\n")
  cat("Base_area:         ", round(base_area, 2), "mm²\n")
  cat("Surface_line:      ", round(surface_line, 2), "mm\n")
  cat("Surface_lateral:   ", round(surface_lateral, 2), "mm²\n")
  cat("Surface:           ", round(surface, 2), "mm²\n")
  cat("Volume:            ", round(volume, 2), "mm³\n")
}

