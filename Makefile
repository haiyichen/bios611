.PHONY: clean
.PHONY: report

clean:
	rm -f report.pdf
	rm -f box.png pie.png winordraw_home.csv winordraw_home_adjusted.csv
	rm -f fixed_effects_yellow.csv fixed_effects_red.csv fixed_effects_goal.csv
	rm -f pca.png pca_var.png knn.png importance.png

box.png pie.png: plots.R data/2021-2022.csv
	Rscript plots.R

winordraw_home.csv winordraw_home_adjusted.csv fixed_effects_yellow.csv \
fixed_effects_red.csv fixed_effects_goal.csv: home_glm.R data/2021-2022.csv
	/usr/bin/Rscript home_glm.R

pca.png pca_var.png knn.png importance.png: cluster.R \
	data/england_premier_league_squad_defensive_actions_22.csv \
	data/england_premier_league_squad_passing_stats_22.csv \
	data/england_premier_league_squad_possession_22.csv \
	data/england_premier_league_squad_shooting_22.csv
	Rscript cluster.R

report: report.pdf

report.pdf: report.Rmd \
	box.png pie.png \
	winordraw_home.csv winordraw_home_adjusted.csv fixed_effects_yellow.csv \
	fixed_effects_red.csv fixed_effects_goal.csv \
	pca.png pca_var.png knn.png importance.png
	Rscript -e 'rmarkdown::render("report.Rmd", "pdf_document")'
