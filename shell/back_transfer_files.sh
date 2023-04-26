rsync -av -e ssh --exclude-from 'shell/exclude_list.txt' lbenz@fasselogin.rc.fas.harvard.edu:~/arterburn_vte/* .
Rscript scripts/plots_models.R
Rscript scripts/table_2.R
Rscript scripts/img_to_pdf.R