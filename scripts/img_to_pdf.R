library(tidyverse)
library(magick)
### Convert images into one PDF of plots
img_to_pdf <- function(fig_dir, outfile) {
  img_files <-  dir(fig_dir, full.names = T)
  img_files <- img_files[grepl('\\.png', img_files)]
  img_pdf <- reduce(map(img_files, image_read), c)
  image_write(img_pdf, format = 'PDF', paste0(fig_dir, '/', outfile))
}

img_to_pdf(fig_dir = 'figures/surv_plots', outfile = 'surv_plots.pdf')
img_to_pdf(fig_dir = 'figures/models/main', outfile = 'plots.pdf')
img_to_pdf(fig_dir = 'figures/models/sensitivity', outfile = 'plots.pdf')
img_to_pdf(fig_dir = 'figures/models/hte_bmi', outfile = 'plots.pdf')
img_to_pdf(fig_dir = 'figures/models/hte_age', outfile = 'plots.pdf')
img_to_pdf(fig_dir = 'figures/models/hte_sex', outfile = 'plots.pdf')
img_to_pdf(fig_dir = 'figures/models/hte_race', outfile = 'plots.pdf')
img_to_pdf(fig_dir = 'figures/models/white_subgroup', outfile = 'plots.pdf')
img_to_pdf(fig_dir = 'figures/models/black_subgroup', outfile = 'plots.pdf')

### Copy Changes to Dropbox
fs::dir_copy(path = 'figures/', 
             new_path = '~/Dropbox (Harvard University)/Haneuse/durable_vte/', 
             overwrite = T)
