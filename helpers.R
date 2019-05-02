# this file contains objects and functions that are needed to generate CJ charts

# load core packages
library("httr")
library("janitor")
library("tidyverse")

# define colour scheme, based on the UCL colour scheme defined at
# https://www.ucl.ac.uk/cam/brand/guidelines/colour
ucl_colours <- tribble(
  ~name, ~hex_code,
  "Dark Green", "#555025",
  "Dark Red", "#651D32",
  "Dark Purple", "#4B384C",
  "Dark Blue", "#003D4C",
  "Dark Brown", "#4E3629",
  "Mid Green", "#8F993E",
  "Mid Red", "#93272C",
  "Mid Purple", "#500778",
  "Mid Blue", "#002855",
  "Stone", "#D6D2C4",
  "Bright Green", "#B5BD00",
  "Bright Red", "#D50032",
  "Bright Blue", "#0097A9",
  "Bright Pink", "#AC145A",
  "Light Green", "#BBC592",
  "Light Red", "#E03C31",
  "Light Purple", "#C6B0BC",
  "Light Blue", "#8DB9CA",
  "Yellow", "#F6BE00",
  "Orange", "#EA7600",
  "Grey", "#8C8279",
  "Blue Celeste", "#A4DBE8"
)

ucl_colours_list <- ucl_colours$hex_code
names(ucl_colours_list) <- ucl_colours$name


# custom ggplot2 theme for the chart
theme_cjcharts <- function (...) {
  theme_minimal(base_family = "Source Sans Pro", ...) %+replace%
    theme(
    	axis.ticks = element_line(colour = "grey92"),
      axis.title = element_text(size = 9, hjust = 1),
      legend.key.height = unit(4, "mm"),
      legend.key.width = unit(10, "mm"),
      legend.position = "bottom",
      legend.spacing.x = unit(2, "mm"),
      legend.title = element_text(size = 9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      plot.caption = element_text(size = 9, colour = "grey33", hjust = 1),
      plot.tag = element_text(size = 12, face = "bold", colour = "grey33", 
                              hjust = 0),
      plot.tag.position = c(0.01, 0.01),
      plot.title = element_text(face = "bold", size = 16, hjust = 0),
      strip.text.y = element_text(angle = 0, hjust = 0)
    )
}

# format chart subtitle
format_subtitle <- function (..., .width = 100) {
  paste0("\n", str_wrap(glue::glue(..., .sep = " "), .width))
}

# format chart caption
format_caption <- function (chart_source, chart_id, chart_note = NA) {
	paste0(
		"\n\n", 
		glue::glue(
			ifelse(!is.na(chart_note), paste0(chart_note, "\n"), ""),
			"Data: {chart_source} | ", "Details: lesscrime.info/cj-charts/{chart_id}", 
			.sep = " " #, .envir = .GlobalEnv
		)
	)
}
