###### sample 1000 iterations of the posterior distribution - A17 species ######

rm(list = ls()) # clear R

### source in the plotting function ###
source("R/SampPost.r")

# "psi.fs" is the default, otherwise pick psi.fs.r_ENGLAND, psi.fs.r_SCOTLAND, psi.fs.r_GB, psi.fs.r_UK, etc...

### Add article 17 species data ###
SampPost(indata = "../data/model_runs/", 
        output_path = "../data/sampled_posterior_1000/",
        REGION_IN_Q = "psi.fs",
        sample_n = 1000)
