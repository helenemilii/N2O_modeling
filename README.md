# N2O_modeling
Modeling temporal variation of nitrous oxide (N2O) using Random forest with conditional inference trees

Example data includes ~4 years of daily mean N2O fluxes measured with automatic chamber in a drained peatland forest (Lettosuo, Finland). The code is a simplified version of the code used to model the temporal variation of N2O in an article by Rautakoski et al. (2014, in preparation). 

Code: Modeling_N2O_example.txt
R code includes the following parts: 1) download data from zenodo, 2) prepare data for modeling, 3) train random forest with conditional inference trees, 4) variable importance, 5) accumulated local effects and 6) model evaluation. In the article, models are run 10 times and means are used to report modeling results. Models are also run for all six chambers. The code has been run on R version 4.0.5.

Data: Example_data.csv
Example data includes daily mean N2O fluxes from one automatic chamber (Chamber 2 in the article) as well as 1-7 days time-lagged and unlagged versions of 
environmental variables used in the study. Environmental variables include soil moisture at 7 and 20 cm depth (columns: moist7 and moist20, unit: m3/m3), water table level (column: wtl, unit: cm below soil surface), precipitation (column: precip, unit: mm/day), air temperature (column: t_air, unit: °C), soil surface temperature (column: t_surface, unit: °C) and soil temperature at 5 cm depth (column: t_5, unit: °C). The unit of the N2O fluxes is µg N2O m⁻² h⁻¹.

Related publication: Rautakoski et al., (2024), Exploring temporal and spatial variation of nitrous oxide flux using several years of peatland forest automatic chamber data. In preparation. Article can be found as a preprint in EGUsphere: https://doi.org/10.5194/egusphere-2023-1795
