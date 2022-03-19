xfun::pkg_attach("dplyr", "tidyr",
                 "ggplot2", "scales", "ggridges")

# Named vector of colors
cp_2077_colors <- c(
  "electric_blue" = "#37EBF3",
  "b_purple" = "#9370DB",
  "blood_red" = "#710000",
  "keppel" = "#1AC5B0",
  "lemon" = "#FDF500",
  "f_pink" = "#E455AE",
  "s_pink" = "#CB1DCD",
  "pale_silver" = "#D1C5C0",
  "r_black" = "#272932")

# Function to access colors by name
cp_2077_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols)) {
    return(cp_2077_colors) 
  }
  cp_2077_colors[cols]
}

# Color palettes
cp_2077_palettes <- list(
  "main" = cp_2077_cols("electric_blue", "b_purple",  "f_pink", "keppel", "lemon", "blood_red", "s_pink"),
  "battle" = cp_2077_cols("blood_red", "f_pink", "b_purple"),
  "nc_neon" = cp_2077_cols("electric_blue", "lemon", "s_pink"),
  "nc_sunset" = cp_2077_cols("blood_red", "lemon"),
  "div1" = cp_2077_cols("f_pink", "electric_blue"),
  "nc_dark" = cp_2077_cols("pale_silver", "r_black"))

# Palette function
cp_2077_pal <- function(palette = "main", reverse = FALSE) {
  stopifnot(palette %in% names(cp_2077_palettes))
  pal <- cp_2077_palettes[[palette]]
  
  function(n) {
    if (n > length(pal)) {
      warning(paste0("Palette only has ", length(pal), " colors."))
    }
    color_lst <- unname(pal[1:n])
    if (reverse) {
      return (rev(color_lst))
    }
    color_lst
  }
}

# Discrete Color Scale
scale_color_cp_2077_d <- function(palette = "main", reverse = FALSE, ...) {
  pal <- cp_2077_pal(palette, reverse = reverse)
  
  discrete_scale(aesthetics = "color", 
                 scale_name = paste0("cp_2077_", palette), 
                 palette = pal, ...)
}

# Continuous Color Scale
scale_color_cp_2077_c <- function(palette = "nc_sunset", reverse = FALSE, ...) {
  pal <- cp_2077_palettes[[palette]][1:2]
  
  if (reverse) {
    pal <- rev(pal) 
  }
  cr_pal <- colorRampPalette(pal, ...)
  scale_color_gradientn(colors = cr_pal(256), ...)
}

# Discrete Fill Scale
scale_fill_cp_2077_d <- function(palette = "main", reverse = FALSE, ...) {
  pal <- cp_2077_pal(palette, reverse = reverse)
  
  discrete_scale(aesthetics = "fill", 
                 scale_name = paste0("cp_2077_", palette), 
                 palette = pal, ...)
}

# Continuous Fill Scale
scale_fill_cp_2077_c <- function(palette = "nc_sunset", reverse = FALSE, ...) {
  pal <- cp_2077_palettes[[palette]][1:2]
  
  if(reverse) {
    pal <- rev(pal) 
  }
  cr_pal <- colorRampPalette(pal, ...)
  scale_fill_gradientn(colors = cr_pal(256), ...)
}