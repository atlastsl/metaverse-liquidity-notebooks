library(ggplot2)

y_axis_top <- 8

# Points voulus
B <- 5.0
C <- 8.2
y_vertex <- 1.3

# Parabole
a <- (y_axis_top - y_vertex) / (C - B)^2
f <- function(x) a * (x - B)^2 + y_vertex

# Droite
m <- 0.55
b <- 2.0
g <- function(x) m * x + b

# Intersection A
h <- function(x) f(x) - g(x)
A <- uniroot(h, lower = 2.0, upper = B)$root

# Données
x_par <- seq(2.7, C, length.out = 400)
parab_df <- data.frame(
  x = x_par,
  y = f(x_par),
  courbe = "Métavers"
)

x_line <- seq(2.2, 8.8, length.out = 2)
line_df <- data.frame(
  x = x_line,
  y = g(x_line),
  courbe = "Monde réel"
)

ggplot() +
  # Axe horizontal supérieur
  annotate("segment", x = 1, y = y_axis_top, xend = 10.2, yend = y_axis_top,
           arrow = arrow(length = grid::unit(0.25, "cm")), linewidth = 0.6) +
  
  # Axe vertical
  annotate("segment", x = 1.4, y = y_axis_top, xend = 1.4, yend = 0.6,
           arrow = arrow(length = grid::unit(0.25, "cm")), linewidth = 0.6) +
  
  # Ticks A, B, C
  annotate("segment", x = A, y = y_axis_top - 0.15, xend = A, yend = y_axis_top + 0.15, linewidth = 0.5) +
  annotate("segment", x = B, y = y_axis_top - 0.15, xend = B, yend = y_axis_top + 0.15, linewidth = 0.5) +
  annotate("segment", x = C, y = y_axis_top - 0.15, xend = C, yend = y_axis_top + 0.15, linewidth = 0.5) +
  
  # Labels
  annotate("text", x = A, y = y_axis_top - 1.1, label = "Régime Normal", size = 4, angle = 90) +
  annotate("text", x = B, y = y_axis_top - 1.25, label = "Régime Attention+", size = 4, angle = 90) +
  annotate("text", x = C, y = y_axis_top - 0.5, label = "Bulle", size = 4, angle = 90) +
  annotate("text", x = 9.55, y = y_axis_top + 0.35, label = "Attention", hjust = 0, size = 6) +
  annotate("text", x = 0.9, y = 4.2, label = "Effet", angle = 90, size = 6) +
  
  # Courbes avec légende
  geom_line(
    data = parab_df,
    aes(x = x, y = y, color = courbe),
    linewidth = 0.9,
    linetype = 2
  ) +
  geom_line(
    data = line_df,
    aes(x = x, y = y, color = courbe),
    linewidth = 0.9
  ) +
  
  scale_color_manual(
    name = "Légende",
    values = c("Métavers" = "black", "Monde réel" = "grey35")
  ) +
  
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 9), expand = FALSE) +
  theme_void() +
  theme(
    legend.position = c(0.82, 0.18),
    legend.background = element_blank(),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11)
  )
