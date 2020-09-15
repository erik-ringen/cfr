This level indexes each unique publication.

NOTES

	General: 

Age rounding - I rounded the ages when the raw age was very close to round numbers (e.g.14.96, 7.02), and not when some of the values were in between (e.g. 13.53).
I sincerely have no idea of how to process the data from regressions so that they are comparable to the rest of our data. Hence, I did not even try to write scripts for the post hoc test results.
Also, I did not try to approach some of the graphs, detailed below.
Finally, we need to pay attention to repeated data, presented in more than one figure.

	By paper:

Bird_2002b 
Table2: values around the mean (mean ± number) are not defined. Interpreted as SE, given that is the measure used in the other tables, although this is not stated

Bird_2005

BliegeBird_1995
 Table 3: “Foraging time includes travel, search and handling (harvesting and field processing)”, but for egg returns, as they were collected during other trips. I recalculated the time to include travel time per each resource, (i.e. I added the travel time to the time used to collect the eggs, as from text: “Egg time does not include travel time to the patch, which was shared with travel to the intertidal patch since egg collecting occurred within an intertidal foraging episode. To obtain total egg time, add 161 minutes to boy 1 and 2, 103 to boy 5, 58 to girl 8”).

BliegeBird_2002a 
Many figures show the same data. Attention to repeated samples. In detail:
-Fig 1a, 1b and 2 all report beach fishing efficiency.  Not clear what's the difference among them. Maybe worth using only fig 1a?
-fig 3 and 4 both report spearfishing efficiency. suggest to use fig 3 with data points
-fig 5 a, b, c all are shellfishing efficiency. a and b are divided by sex, while c is a subset of the children and I think can be ignored
Data are presented as Residual overall returns  (Kcal/hr+travel) and Efficiency. The first includes negative values. For both, I wrote “net kcal/hr” as unit. 
Fig2 and table: Error reported as CI98. I did not calculate SD from the figure, because the small sample size renders the calculation from CI98 unreliable. I think it’s better to use the data and SD presented in the associated table (and the r script for that) instead of that for the figure.
Fig2 and table: age given as groups, I included the 14-29 group in the analysis (adults values starting with the following group). We need to decide a rationale for the cut off of groups overlapping 20 years (see Kramer_2009a).
Fig4: compared Children to Other men, ignored Best man
Fig4: sd was calculated from error, presented as CI95, with formula SD= vN X (upper limit - lower limit)/ 2*students' t value (for appropriate sample size) (2.776 for n=5, 2.306 for n=9)
Fig5c: Declared sample size is 15, but only 13 data points found.
Fig5c: Adult averages and sd can be constructed from fig 5a and b, but probably not worth it, because data are repeated.
Fig6 and table: Error reported as CI98. I did not calculate SD from the figure, because the small sample size renders the calculation from CI98 unreliable. I think it’s better to use the data and SD presented in the associated table (and the r script for that) instead of that for the figure.

BlurtonJones_1989 
Table2: individuals identified by original ids (e.g. SUSA or ADAM), maybe we can get the sex of these children? ASK NBJ
Table2: makalita and //ekwa tubers have been summed into tubers. this way comparable with adult values from table 4
Table2: adult values are available in the table only for fruits (and even those are not necessarily reliable, as they are from only two women), but those for tubers can be obtained from table 4. Note that these are taken from ‘data_BlurtonJones_1989_table4.csv’, after processing of the original data.

BlurtonJones_1997 
Figure 1 and 2: have data from multiple years. Some individuals are repeated, but no info is given on which ones, so they are treated as independent data. Hence, the data from each figure is considered a single outcome
Figure 5: draws on the same data as figure 4, but subsetting to <30 y old and giving info on school attendance. Probably not worth using.

BlurtonJones_2002
Figure 1 shows data that might not be originally collected for this paper. Pay attention to that.

Bock_2005

Crittenden_2013 
fig 2 values in logarithm. Not clear base of the log. ASK ALYSSA

Froehle_2018
data extracted only from fig 5b and table4, as the rest did seem redundant. Actually, probably fig5b is also redundant, as the most detailed, trip-level data are presented in table 4
resource type is not specified, maybe input 'mixed' ? Or simply leave NA?

Gurven_2006
data reported as cal/h, but I changed to kcal/h. It seems to be on the same scale as Tucker_2006: it seems pointless for people to collect 200 calories= 0.2kcal/hr, right? ASK?
fig 6a and b have the same data, but 6b shows each data point per individual/day, 6a averages over individuals. One needs to be excluded.

Hagino_2016
no error given, data are total weight over 6 days, either children or adults have zero returns in every item. Seems to suggest that children and adults hunt different things, more than difference in efficiency/success, and I am not sure of how to interpret these zeroes: are they failures?


Hawkes_1995 
Table 2: was used as a source of information on children for the other tables (it does not need it’s own data extraction script). Used for all the tables.
Table 4: adult values for tin measured berries from table 5.  
Table 5: assumed age composition from table 2 - so age interval, mean and sd entered from that table
Table 5: these are tin measured rates
Table 6: all the measures are for tafabe, a berry, and differ from fig 5 because are in kcal instead of grams
Table 6: presents several measures for tafabe. Only one is probably worth using, e.g. calories picked in a certain amount of time (tin measured).  Other measures are calories remaining in the stash after people eat the berries while picking, consumed calories, proportion of picked berries stashed, instead of being eaten.
Table 6: assumed age composition from table 2 - so age interval, mean and sd entered from that table
Table 6: check that the way I combined standard errors of adults to compare children values is correct

Kaplan_1995
Only loesch curves: I am not sure of how to extract the data.

Kaplan_1994
There are no error bars (ok I was lazy)
	
Koster_2007
Data are a pain, did we already discuss asking Jeremy the originals? ASK JEREMY
	
Kramer_2009a 
Here only groups completely below 20y old were considered. We need to decide a rationale for the cut off of groups overlapping 20 years (see BliegeBird_2002a figure 2).
Transformed into grams from kg for comparability

Pollom_inpress
Not a clue on what to extract

Tucker_2005 
All ages are reported as age ranks and not estimated age. I assigned to the 2003 returns age group boundaries, but Erik used age rank as age. Needs to be made homogeneous.
I think y valuables should be allowed to be negative because they are net return rates, and the travel and energy expenses can be higher than the returns.

Walker_2002
fig 2b has same data as 2a, but with individual connected across decades. Subsetting to age <20 hides the connections, of course, so probably only redundant data. Average adult values calculated for the whole sample. Probably worth excluding and focus on 2a.

