# library(ggplot2)

gate_width = 0.03
normal_level = -0.52
avoid_level = -0.32
lly = -1
llx = -1
urx = 1
gate_height = 0
min_diff = 0.1
line_width = 1.25
gate = data.frame(x = c(-gate_width, gate_width, gate_width, -gate_width, -gate_width),
                  y = c(lly, lly, gate_height, gate_height, lly))
inland_levels = data.frame(x = c(llx, -gate_width),
                    y_normal = rep(normal_level,2),
                    y_avoid = rep(avoid_level,2))
sea_levels = data.frame(x = c(gate_width, urx),
                    y_normal = rep(normal_level,2) - min_diff,
                    y_avoid = rep(avoid_level,2) - min_diff)
water_inland = data.frame(x = c(llx, -gate_width, -gate_width, llx, llx),
                          y = c(lly, lly, avoid_level, avoid_level, -1))
water_sea = data.frame(x = c(urx, -gate_width, -gate_width, urx, urx),
                          y = c(lly, lly, avoid_level - min_diff, avoid_level - min_diff, -1))

sluice_plot <- ggplot() +
  geom_polygon(data = water_inland, aes(x = x, y = y),
               fill = "lightblue", alpha = 0.5) +
  geom_polygon(data = water_sea, aes(x = x, y = y),
               fill = "lightblue", alpha = 0.5) +
  geom_polygon(data = gate, aes(x = x, y = y),
               col = "black", fill = "lightgray") +
  geom_line(data = inland_levels, aes(x = x, y = y_normal),
            col = "forestgreen", linetype = "dashed", size = 1.25) +
  geom_line(data = inland_levels, aes(x = x, y = y_avoid),
            col = "red", linetype = "dotted", size = 1.25) +
  geom_line(data = sea_levels, aes(x = x, y = y_normal),
            col = "forestgreen", linetype = "dashed", size = 1.25) +
  geom_line(data = sea_levels, aes(x = x, y = y_avoid),
            col = "red", linetype = "dotted", size = 1.25) +
  geom_text(data = NULL, aes(x = (-gate_width + llx)/2, y = gate_height,
            label = "Canal Belt Side")) +
  geom_text(data = NULL, aes(x = (urx - gate_width)/2, y = gate_height,
                             label = "Sea Side")) +
  geom_line(data = NULL, aes(x = c(0,0),
                                y = c(normal_level, normal_level - min_diff)),
               col = "forestgreen" ,size = 1.25,
              arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
  geom_line(data = NULL, aes(x = c(0,0),
                                y = c(avoid_level, avoid_level - min_diff)),
               col = "red" ,size = 1.25,
            arrow = arrow(length = unit(0.03, "npc"), ends = "both")) +
  geom_text(data = NULL, aes(x = gate_width, y = normal_level - min_diff/2,
                             label = "0.1 m"),
            hjust = "left",
            col = "forestgreen") +
  geom_text(data = NULL, aes(x = gate_width, y = avoid_level - min_diff/2,
                             label = "0.1 m"), hjust = "left",
            col = "red") +
  ggtitle("Gravity based sluice at Harlingen") +
  theme_bw()

