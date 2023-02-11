library(tidyverse)
library(magick)
### Convert images into one PDF of plots
img_to_pdf <- function() {
  img_files <-  dir('figures/', full.names = T)
  img_files <- img_files[grepl('\\.png', img_files)]
  img_pdf <- reduce(map(img_files, image_read), c)
  image_write(img_pdf , format = 'pdf', 'figures/surv_plots.pdf')
}

img_to_pdf()