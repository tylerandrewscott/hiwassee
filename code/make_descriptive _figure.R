library(tidyverse)
p1 <- readRDS('scratch/panel1_figure2.rds')
p2 <- readRDS('scratch/panel2_figure2.rds')

p1 <- p1 + scale_y_continuous(name = '# observations',breaks = seq(0, 41, by = 5),limits = c(0,41))
p2 <- p2 + scale_y_continuous(breaks = seq(0, 41, by = 5),limits = c(0,41))
p1 <- p1 + ggtitle('County-year ratings')
p2 <- p2 + ggtitle('Deal-level ratings')
library(cowplot)
plot_row <- plot_grid(p1 ,p2)

# now add the title
title <- ggdraw() + 
  draw_label(
    "Distribution of county-year and deal-level ratings",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(plot.background = element_rect(fill = 'white',color = NA),
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

sub <- ggdraw() + 
  draw_label('*AAA:AA+; AA+:(AA+,AA1); AA:(AA,AA2); AA-:(AA-,AA3);\nA+:(A+,A1); <=A:(A,A2,A-,A3,BBB+,BAA1,BBB,BAA2,BBB-,Baa3)',
    x = 0,
    hjust = -0.75,size = 8
  ) +
  theme(plot.background = element_rect(fill = 'white',color = NA),
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

fplot = plot_grid(
  title, plot_row,sub,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1,0.1))
  
ggsave(plot = fplot,filename = 'output/figures/fig2.png',dpi = 300,width = 6,height = 3.5,units = 'in')

ggsave(plot = fplot,filename = 'output/figures/fig2.tiff',dpi = 300,width = 6,height = 3.5,units = 'in')



