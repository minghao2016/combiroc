demo_data # combiroc built-in demo data (proteomics data from Zingaretti et al. 2012 - PMC3518104)

combs <- Combi(data= demo_data, signalthr=450, combithr=1)  # compute combinations

combs_SE_SP <- SE_SP(data=demo_data, combinations_table=combs) # compute SE and SP
# of each combination

ranked_combs_SE_SP <- ranked_combs(data= demo_data, combo_table= combs_SE_SP,
                                   case_class='A', min_SE=40, min_SP=80) # rank combinations



#  To show the composition of combinations of interest.

show_markers(markers_table = ranked_combs_SE_SP, selected_combinations = c(1,11))
