rsync -av -e ssh --exclude-from 'shell/exclude_list.txt' lbenz@fasselogin.rc.fas.harvard.edu:~/arterburn_vte/* .
Rscript scripts/img_to_pdf.R