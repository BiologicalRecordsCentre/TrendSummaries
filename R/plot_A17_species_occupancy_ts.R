###### plot A17 species annual occupancy time-series ######

rm(list = ls()) # clear R

### source in the plotting function ###
source("R/PlotOcc.r")

### Add article 17 species data ###
PlotOcc(indata = "../data/model_runs/",
        output_path = "../output/",
        REGION_IN_Q = "psi.fs.r_GB",
        y_axis_choice = "variable",
        cluster_run = "jasmin",
        jasmin_min_year = 1970)



