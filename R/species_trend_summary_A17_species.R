###### species trend summary - A17 species ######

rm(list = ls()) # clear R

### source in the plotting function ###
source("R/SppTrendSummary.r")

### Add article 17 species data ###
SppTrendSummary(indata = "../data/sampled_posterior_1000/", 
         output_path = "../trend_summary/",
         short_term_length = 10,
		     a_17 = "yes")
