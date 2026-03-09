#########################################################################################################################################
############################################################### Packages ################################################################
#########################################################################################################################################

options(cores = 4, warn = -1) 
library(tidyverse); library(patchwork); library(ggridges); library(stringr); library(vegan); library(brms); library(readxl)

#########################################################################################################################################
############################################################### Functions ###############################################################
#########################################################################################################################################

`%notin%` = Negate(`%in%`)
element_textbox_highlight <- function(..., hi.labels = NULL, hi.fill = NULL, hi.col = NULL, hi.box.col = NULL, hi.family = NULL) {
  structure(c(element_textbox(...), list(hi.labels = hi.labels, hi.fill = hi.fill, hi.col = hi.col, 
                                         hi.box.col = hi.box.col, hi.family = hi.family)),
            class = c("element_textbox_highlight", "element_textbox", "element_text", "element"))}
element_grob.element_textbox_highlight <- function(element, label = "", ...) {
  if (label %in% element$hi.labels) {
    element$fill       <- element$hi.fill %||% element$fill
    element$colour     <- element$hi.col %||% element$colour
    element$box.colour <- element$hi.box.col %||% element$box.colour
    element$family     <- element$hi.family %||% element$family}
  NextMethod()}
theme_figure_3 <- function() {
  theme(axis.text.x      = element_text(size = 14),
        axis.text.y      = element_text(size = 0),
        axis.title       = element_text(size = 16),
        legend.text      = element_text(size = 14),
        axis.ticks.y     = element_blank(),
        legend.title     = element_blank(),
        panel.border     = element_rect(color = "black", fill = NA, size = 1),
        strip.text       = element_blank(),
        strip.background = element_blank(),
        legend.position  = "none")}
theme_extreme_low <- function(panel_background_color = "gray20") {
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = panel_background_color),
        plot.title = element_text(size = 18, color = "firebrick1", face = "bold"),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -3),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "firebrick1"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))}
theme_ambient <- function(panel_background_color = "gray20") {
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = panel_background_color),
        plot.title = element_text(size = 18, color = "cornflowerblue", face = "bold"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(vjust = 0.5),
        axis.text.y = element_text(hjust = 1),
        axis.title.x = element_text(size = 16, vjust = -1),
        axis.title.y = element_text(size = 16, vjust = 1),
        legend.position = "right",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "cornflowerblue"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))}
theme_low <- function(panel_background_color = "gray20") {
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        panel.background = element_rect(fill = panel_background_color),
        plot.title = element_text(size = 18, color = "gold", face = "bold"),
        axis.ticks.x = element_blank(),
        panel.grid = element_line(colour = NA),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 16, vjust = -3),
        axis.title.y = element_text(size = 16, vjust = 3),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        strip.background = element_rect(colour = "black", fill = "gold"),
        plot.margin = unit(c(.5, .5, .5, .5), "cm"))}