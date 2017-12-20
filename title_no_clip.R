require(ggplot2)

title_align_no_clip <- function(plot = last_plot(), title_segments, colors,
                                nudge_x = 0, nudge_y = 0, size = 14, 
                                hjust = 0, vjust = 0, ..., 
                                filename = NULL,
                                axis.title.color,
                                axis.x.line.color,
                                axis.y.line.color,
                                axis.text.color,
                                tick.color,
                                plot.margin = unit(c(.9, 1, 1, 1.2), "cm")){
  
  if(all(c('ggplot2', 'cowplot','graphics') %in% rownames(installed.packages())) != TRUE){
    stop('You must have the following installed: ggplot2, cowplot, graphics')
  }
  
  # Preformat the graph for the title
  plot = plot +
         ggtitle('\n') + 
         theme(plot.margin = plot.margin,   
               axis.title = element_text(color = axis.title.color, size = 14),
               axis.title.y = element_text(hjust = .94),
               axis.line.x = element_line(color = axis.x.line.color),
               axis.line.y = element_line(color = axis.y.line.color),
               axis.text = element_text(color = axis.text.color),
               axis.ticks = element_line(color = tick.color))
  
  plot <- ggplot_build(plot)
  plot <- ggplot_gtable(plot)
  plot$layout$clip[plot$layout$name=="panel"] <- "off"
  
  cowplot::ggdraw(plot) 
  x <- .059 + nudge_x
  y <- .94 + nudge_y
  x_segments <- c(x, x + cumsum(strwidth(title_segments, font = 2)[-length((title_segments))] ))
  
  
  plot <- cowplot::ggdraw(plot) + cowplot::draw_text(text = title_segments, colour = colors, 
                                            x = x_segments, y = y, hjust = hjust, vjust = vjust,
                                            size = size, ..., fontface = 'bold')  
  
  # Save the plot
  if(!is.null(filename)) ggsave(filename, plot = gg, width = save_width, height = save_height)
  
  return(plot)
}





