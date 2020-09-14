This level indexes each unique publication.

ISSUES ILARIA
-Bird_2002b Table2
	-values around the mean (mean plusminus number) are not defined. Interpreted as SE, given that is the measure used in the other tables, although this is not stated

-BliegeBird_1995 Table 3
	Notes on time from Caption: Foraging time includes travel, search and handling (harvesting and field processing)
	Egg time does not include travel time to the patch, which was shared with travel to the intertidal patch
	since egg collecting occurred within an intertidal foraging episode. To obtain total egg time, add 161 minutes to boy 1 and 2,
	103 to boy 5, 58 to girl 8

-BliegeBird_2002a 
	I bypassed Tables 2, 3, 5. Can we extract data from these to compare with the raw data from other papers?
	Many figures show the same data. Attention to repeated samples.
	in detail:
		-Fig 1a, 1b and 2 all report beach fishing efficiency.  Not clear what's the difference among them. Maybe worth using only fig 1a?
		-fig 3 and 4 both report spearfishing efficiency. suggest to use fig 3 with data points
		-fig 5 a, b, c all are shellfishing efficiency. a and b are divided by sex, while c is a subset of the children and I think can be ignored

-BliegeBird_2002a Fig1a 
	y values are Residual overall returns (Kcal/hr+travel)

-BliegeBird_2002a Fig1b
	y values are Residual Beach Fishing Efficiency (Kcal/hr E/T)
	I think we can use the adult values of fig1a, same paper. But I would like second opinion.

-BliegeBird_2002a Fig2 and table
	Error reported as CI98. SD not calculated, because small sample size renders the calculation from CI98 unreliable.
	Hence, better to use the data and SD presented in the associated table (and the r script for that) instead of the figure.

-BliegeBird_2002a Fig4
	compared Children to Other men, ignored Best man
	sd was calculated from error, presented as CI95, with formula SD= vN X (upper limit - lower limit)/ 2*students' t value (for appropriate sample size) (2.776 for n=5, 2.306 for n=9)
	same data is presented in fig 3

-BliegeBird_2002a Fig5c
	Subset of figures 5a and b for children and teenagers, so probably redundant.
	Declared sample size is 15, but only 13 data points found.
	Adult averages and sd can be constructed from fig 5a and b, but probably not worth it, because data are repeated.

-BliegeBird_2002a Fig6 and table
	Error reported as CI98. SD not calculated, because small sample size renders the calculation from CI98 unreliable.
	Hence, better to use the data and SD presented in the associated table (and the r script for that) instead of the figure.

-BlurtonJones_1989 table2
	makalita and //ekwa tubers have been summed into tubers
	adult values only for fruits, but added for tubers can be obtained from table4. Check whether it's ok

-BlurtonJones_1997 
	figure 1 and 2 have data from multiple years. Some individuals are repeated, but no info is given, so they are treated as independent data

-Bock_2005
	extracted data from fig 5.2 only, I don't know how to deal with the regressions :/

-Froehle_2018
	data extracted only from fig 5b and table4, as the rest did seem redundant. Actually, probably fig5b is also redundant, as the most detailed, trip-level data are presented in table 4

-Hagino_2016
	data are quite poor: no error given, data are total weight over 6 days, either children or adults have zero returns in every item. Seems to suggest that children and adults hunt different things, more than difference in efficiency/success

-Walker_2002 fig1_a and b
	average adult return separate by decade of data collection
	fig 2b has same data as 2a, but for connected individual across decades. Subsetting to age<20 hides the connections, of course. Average adult values calculated for the whole sample

-Hawkes_1995 Table 2 and 4
	combined together to use age and sex information per child from the first, and returns from the other
	resources not combined. Each type of fruit or tuber is treated separately

-Hawkes_1995 table 5 
	assumed age composition from table 2 - so age interval, mean and sd entered from that table

-Hawkes_1995 table 6
	all the measures are for tafabe, a berry
	presents several correlated measures- to decide whether to bring them all on or chose only one (e.g. calories picked in a certain amount of time (tin measured), calories remaining in the stash even after people eat the berries while picking, consumed calories, proportion of picked berries stashed, insted of being eaten)
	also assumed age composition from table 2 - so age interval, mean and sd entered from that table
	check that the way I combined standard errors of adults to compare children values is correct

