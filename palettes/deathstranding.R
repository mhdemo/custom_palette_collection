xfun::pkg_attach("dplyr", "tidyr",
                 "ggplot2", "scales", "ggridges")

# Named vector of colors
ds_colors <- c(
  "rain" = "#63768A",
  "grass" = "#7B793D",
  "earth" = "#3B3425",
  "bd_blood" = "#813135",
  "yellow" = "#EED257",
  "disp_orange" = "#D08F38",
  "mist" = "#E6E5DA",
  "bd_black" = "#34343B")

# Function to access colors by name
ds_cols <- function(...) {
  cols <- c(...)
  if (is.null(cols)) {
    return(ds_colors) 
  }
  ds_colors[cols]
}

# Color palettes
ds_palettes <- list(
  "main" = ds_cols("rain", "grass", "bd_blood", "earth", "yellow", "disp_orange", "mist", "bd_black"),
  "attack" = ds_cols("bd_blood", "disp_orange", "yellow"),
  "calm" = ds_cols("rain", "mist", "earth"),
  "sunset" = ds_cols("bd_blood", "yellow"),
  "div1" = ds_cols("grass", "disp_orange"),
  "dark" = ds_cols("bd_black", "mist"))

# Palette function
ds_pal <- function(palette = "main", reverse = FALSE) {
  stopifnot(palette %in% names(ds_palettes))
  pal <- ds_palettes[[palette]]
  
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
scale_color_ds_d <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ds_pal(palette, reverse = reverse)
  
  discrete_scale(aesthetics = "color", 
                 scale_name = paste0("ds_", palette), 
                 palette = pal, ...)
}

# Continuous Color Scale
scale_color_ds_c <- function(palette = "sunset", reverse = FALSE, ...) {
  pal <- ds_palettes[[palette]][1:2]
  
  if (reverse) {
    pal <- rev(pal) 
  }
  cr_pal <- colorRampPalette(pal, ...)
  scale_color_gradientn(colors = cr_pal(256), ...)
}

# Discrete Fill Scale
scale_fill_ds_d <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ds_pal(palette, reverse = reverse)
  
  discrete_scale(aesthetics = "fill", 
                 scale_name = paste0("ds_", palette), 
                 palette = pal, ...)
}

# Continuous Fill Scale
scale_fill_ds_c <- function(palette = "sunset", reverse = FALSE, ...) {
  pal <- ds_palettes[[palette]][1:2]
  
  if(reverse) {
    pal <- rev(pal) 
  }
  cr_pal <- colorRampPalette(pal, ...)
  scale_fill_gradientn(colors = cr_pal(256), ...)
}
