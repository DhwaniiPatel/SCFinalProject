all:
	Rscript -e "targets::tar_make()"
	open readme.html
