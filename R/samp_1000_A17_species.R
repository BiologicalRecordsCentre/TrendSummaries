###### sample 1000 iterations of the posterior distribution - A17 species ######

rm(list = ls()) # clear R

### source in the plotting function ###
source("R/SampPost.r")

### Add article 17 species data ###
SampPost(indata = "../data/model_runs/", 
        output_path = "../data/sampled_posterior_1000/",
        REGION_IN_Q = "psi.fs.r_GB",
        sample_n = 1000)
