## This function will allow you to assign color to each word in a plot annotation
# Dependencies: purrr, ggplot2

annotate_color <- function(geom = 'text', x = NULL, y = NULL, xmin = NULL, xmax = NULL,  
                           ymin = NULL, ymax = NULL, xend = NULL, yend = NULL, ...,
                           labels = NULL, colors = NULL, default_color = 'black'){
  
  # Checks for essential arguments
  if (is.null(colors) || is.null(x) || is.null(y) || is.null(labels)){
    stop('Missing one of the arguments: labels, colors, x, or y')}
  
  
  labels <- strsplit(labels, " ")[[1]] 
  n <- length(labels)
  
  if (length(colors) < length(labels)){   # Assigns any empty values in 'colors' to the 'default_color' 
    colors <- purrr::map_chr(seq_len(length(labels)), function(i){
      if (is.na(colors[i]) | colors[i] == ''){
        colors[i] <- default_color
      } else {colors[i] <- colors [i]}}
    )
  }
  
  if (length(colors) > length(labels)){   # Shortens the length of 'colors' to match the length of 'labels'
    colors = colors[1:length(labels)]
    warning('The length of the colors arg is longer than the number of words in the labels arg. Extra colors will be               ignored.')
  }
  
  # Formats the labels argument into usable arguments for each annotation function
  labels <- purrr::map_chr(seq_len(n), function(i) {  
    start0 <- labels[seq_along(labels) < i]    # Assigns the first part of the string 
    mid0 <- labels[i]                          # Assigns a single word 
    end0 <- labels[seq_along(labels) > i]      # Assigns the last part of the string
    start <- paste0('phantom("', paste(start0, collapse = " "), ' ")') # Wraps phantom() around the first part 
    end <- paste0('phantom("', paste(end0, collapse = " "), ' ")') # Wraps phantom() around the last part
    if(length(start0) > 0 && length(end0) > 0) {  # Conditional statements for the formatting depending...
      paste(start, paste0('"', paste(mid0, collapse = " "), '"'), end, sep = ' * ') # ... on the position of 'mid0'
    } else if (length(end0) > 0) {
      paste(paste0('"', paste(mid0, collapse = " "), '"'), end, sep = ' * ')
    } else if (length(start0) > 0) {
      paste(start, paste0('"', paste(mid0, collapse = " "), '"'), sep = ' * ')
    } else {
      stop("couldn't finish ...")
    } # Anonymous function above created with the assistance of
  })  # https://stackoverflow.com/users/3521006/docendo-discimus
  
  # Plugs all arguments into the annotate() function and stores them into a list
  annofuncs <- list()
  annofuncs <- purrr::map2(labels, colors, function(annolabel, annocolor){
    annofuncs[seq_along(annolabel)] <- list(annotate(geom, x, y, xmin, xmax, ymin, ymax, xend, yend, ...,
                                                     parse = T, label = annolabel, color = annocolor))
  })
  return(annofuncs) # Returns the list which can be added to a ggplot like any other layer
}
