
library(purrr)
library(grid)
library(gridExtra)
library(ggplot2)

qplot(displ, year, data = mpg) 

ggplot(data = mpg, aes(x = displ, y = year)) + 
  geom_point() + 
  labs(y = 'stuff\n') + 
  theme(plot.margin = unit(c(.9, 1, 1, 1.2), "cm"))

title_align_no_clip(title_segments = c('Percent of ', 'men ', 'and ', 'women'),
                    colors = c('Grey25', 'red', 'Grey25', 'blue'),
                    axis_title_color = 'Grey40', 
                    axis_text_color =  'Grey40',
                    axis_x_line_color = 'Grey35',
                    axis_y_line_color = 'Grey35', 
                    tick_color = 'Grey35')

# Draw onto a 1*1 drawing surface
ggdraw(qplot(3,1)) + draw_text("Hello World!", x = 0.5, y = 0.5)
#
# Adorn a plot from the Anscombe data set of "identical" data.
p = qplot(x = x1, y = y1, geom = c("smooth", "point"), data = anscombe)
threeStrings = c("Hello World!", "to be or not to be", "over and out")
ggdraw() + p + draw_text(threeStrings, x = .01 + strwidth(threeStrings), y = 5, hjust = 0)

#Testing-------------------------------------------------------
title_segments <- c('Help ', 'me ', 'please', '!')
colors <- c('red', 'orange', 'green', 'blue')
nudge_x = 0

grobs <- NULL
grobs[1] <- 'gp = gpar(fontsize = 14, fontface = "bold"))'
grobs[2] <- 'textGrob(label = title_segments[1], name = "title1",  
                      x = unit(2.33 - nudge_x, "lines"), 
                      y = unit(-.5, "lines"), 
                      hjust = 0, vjust = 0, gp = gpar(col = colors[1]))'

if(length(title_segments) > 1){ # Need to figure out how to append list
  x <- unit(2.24 - nudge_x, "lines")
  more_grobs <- pmap_chr(list(title_segments[-1], colors[-1], seq_along(title_segments)[-1]), function(segment, color, i){
    grob <- textGrob(label = segment, name = paste0("title", i, sep = ""),
                     x = x + grobWidth(paste0("title", i - 1, sep = "")), 
                     y = unit(-.5, "lines"),
                     hjust = 0, vjust = 0, gp = gpar(col = color))
  })
}
grobs <- c(grobs, more_grobs)

grobs <- do.call(what = grobTree, args = grobs)

# Turn off clipping and draw plot
gb <- ggplot_build(last_plot()) # Puts plot into a usable format
gt <- ggplot_gtable(gb) # puts grobs into a table to allow the object to be manipulated
gt$layout$clip[gt$layout$name=="panel"] <- "off" # removes clipping with graph borders 
gg <- arrangeGrob(gt, top = grobs, padding = unit(2.6, "line")) # places above grobs into a gtable
grid.newpage() # makes a blank graphing window for ggplot2
grid.draw(gg) # draws the plot









#Function---------------------------------------------------------------------------------------------------------------
title_align_no_clip <- function(plot = last_plot(), title_segments, colors,
                                x = .06, y = .94, size = 14, 
                                hjust = 0, vjust = 1, ..., 
                                filename = NULL,
                                axis_title_color = 'Grey40',
                                axis_x_line_color = 'Grey55',
                                axis_y_line_color = 'Grey55',
                                axis_text_color = 'Grey48',
                                tick_color = 'Grey55',
                                plot_margin = unit(c(.9, 1, 1, 1.2), "cm"),
                                nudge_x = 0){
  
  
  # Preformat the graph for the title
  plot = plot +
         ggtitle('\n') + 
         theme(plot.margin = plot_margin,   
                      axis.title = element_text(color = axis_title_color, size = 14),
                      axis.title.y = element_text(hjust = .94),
                      axis.line.x = element_line(color = axis_x_line_color),
                      axis.line.y = element_line(color = axis_y_line_color),
                      axis.text = element_text(color = axis_text_color),
                      axis.ticks = element_line(color = tick_color))
  
  ggdraw(plot) 
  
  x_segments <- c(x, x + cumsum(strwidth(title_segments, font = 2)[-length((title_segments))] ))
  
  
  
  plot <- ggdraw(plot) + cowplot::draw_text(text = title_segments, colour = colors, 
                                            x = x_segments, y = y, hjust = hjust, vjust = vjust,
                                            size = size, ..., fontface = 'bold')  

  # # Turn off clipping and draw plot
  # gb <- ggplot_build(plot)
  # gt <- ggplot_gtable(gb)
  # 
  # gt$layout$clip[gt$layout$name=="panel"] <- "off"
  # 
  # grid.draw(gt)


  # Save the plot
  if(!is.null(filename)) ggsave(filename, plot = gg, width = save_width, height = save_height)
}





