TARGETS := data slides docs/link_list.html
SLIDES_PDF := $(patsubst %.Rmd, %.pdf, $(wildcard slides/*.Rmd))

.phony: slides data all clean clean_slides_dir

all: $(TARGETS)

clean_slides_dir:
	rm -f -r slides/*_files slides/*_cache

clean: clean_slides_dir
	rm -f slides/*.pdf docs/*.html
	rm -f data/*
	rm -f code/intro_survey/sposm_survey.sqlite3

data: data/sub.csv data/num.csv data/pre.csv data/tag.csv

slides: data ${SLIDES_PDF} clean_slides_dir

%.pdf: %.Rmd code/utils.R
	Rscript -e "rmarkdown::render('$*.Rmd')"
	rm $*.tex
	
%.html: %.Rmd
	Rscript -e "rmarkdown::render('$*.Rmd')"

data/sub.csv: code/read_sec_fin_stat_data.R
	Rscript code/read_sec_fin_stat_data.R

data/num.csv data/pre.csv data/tag.csv: data/sub.csv
