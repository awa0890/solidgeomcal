#' Compute some key figures for a cylinder
#' @title Solid Geometry Calculator for cylinders
#'
#' @param radius_mm The radius of the cylinder in millimeters.
#' @param height_mm The height of the cylinder in millimeters.
#'
#' @return Prints the diameter, scope, base area, lateral surface, surface and the volume of a cylinder for the inputs radius_mm und height_mm
#' @export
#'
#' @examples
#' calculate_cylinder(radius_mm=50, height_mm=20)
#'
calculate_cylinder <- function(radius_mm, height_mm) {
  # Error handling
  if(!is.numeric(radius_mm)| !is.numeric(height_mm)) {
    stop("radius or height is not numeric. \n Please check your inputs!")
  }
  # Calculations
 diameter <- 2 * radius_mm
 scope <- 2 * pi * radius_mm
 base_area <- pi * radius_mm^2
 surface_lateral <- 2 * pi * radius_mm * height_mm
 surface <- 2 * pi * radius_mm * (radius_mm + height_mm)
 volume <- pi * radius_mm^2 * height_mm

 # Output
 cat("Calculations for a cylinder with radius =", radius_mm, "mm and height =", height_mm, "mm:\n")
 cat("------------------------------------------------------------\n")
 cat("Diameter:         ", diameter, "mm\n")
 cat("Scope :           ", round(scope, 2), "mm\n")
 cat("Base Area :       ", round(base_area, 2), "mm²\n")
 cat("Lateral Surface : ", round(surface_lateral, 2), "mm²\n")
 cat("Surface:          ", round(surface, 2), "mm²\n")
 cat("Volume:           ", round(volume, 2), "mm³\n")
}


