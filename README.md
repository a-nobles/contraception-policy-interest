# Examining Public Interest in Medical Procedures in Response to Potential Health Policy Changes

This project was [published](https://www.contraceptionjournal.org/article/S0010-7824(18)30485-2/fulltext) in the Contraception.  

#### -- Project Status: [Completed]

## Project Introduction
The purpose of this project is to examine the public's interest in contraceptive options following heightened focus on a repeal of the Affordable Care Act (ACA) following the 2016 United States presidential election. Because, at the time of this study, little to no traditional data were available, we turned to internet searches to examine public interest. 

## Overview of Methods
* Methods
	* Time series modeling (ARIMA)
	* Data visualization
* Languages (libraries)
	* R (forecast, ggplot2)

## Steps for Analysis
Below is an overview of the steps of the analysis to support our [paper](https://www.contraceptionjournal.org/article/S0010-7824(18)30485-2/fulltext) with additional details supplied wihtin the paper.

1. We collected the fraction of Google searches emerging from the United States between January 2004 and October 2017 for the three most popular reversible contraceptive methods (i.e., birth control pills, intrauterine devices (IUDs), and condoms). 
2. We compared monthly search volumes after the 2016 presidential election against expected search volumes derived from ARIMA forecasts to produce a counterfactual both nationally and by state.
	1. [IUD\_ARIMA.R]()
	2. [IUD\_State.R]()

## Results
Searches for IUDs reached all time highs following the 2016 election. Searches were cumulatively 15% (95% CI: 10 to 20) higher than expected one year following the election, reflecting 10 to 21 million excesssearches.

![Web searches for birth control following the election of Donald Trump. Each panel (left to right, respectively) shows searches for the focal search terms relative to all searches (per10 million) against the predicted counterfactual and the observed search volumes against the counterfactual's 95% prediction interval.](/Users/anobles/Projects/contraception_for_github/figures/google_iud.pdf)
**Figure 1.** Web searches for birth control following the election of Donald Trump. Each panel (left to right, respectively) shows searches for the focal search terms relative to all searches (per10 million) against the predicted counterfactual and the observed search volumes against the counterfactual's 95% prediction interval. 

&nbsp;

IUD searches were statistically significantly higher than state specific projected counterfactuals in all states, except NV (2%; 95% CI: −4 to 10]), ranging from 9% greater in in FL, IN, TN and WA to 24% inMO. Average increases were statistically indistinguishable across stateswon by Trump or Clinton (Welch t test=0.60, p=.548), with both red and blue states similarly searching more for IUDs.

![Within-state percent changes in relative search volume (RSV) for intrauterine device (IUD) queries for the one year period following the 2016 US presidential election.](/Users/anobles/Projects/contraception_for_github/figures/state_difference_viz.png)
**Figure 2.** Within-state percent changes in relative search volume (RSV) for intrauterine device (IUD) queries for the one year period following the 2016 US presidential election.

&nbsp;

Searches for birth control pills and condoms were within expected volumes both nationally and by state.


## Discussion
Public interest in IUDs reached record numbers following the 2016 US presidential election, while interest in other forms of birth control remained stable. Google search volumes can serve as a proxy for public interest while waiting on traditional surveillance metrics that are often costly and lag consierably in time. It is critical that policy makers and health leaders consider record levels of demand for IUDs when considering changes to the ACA that might make IUDs lessaccessible.


## Authors

**Project Lead: [Alicia Nobles](https://a-nobles.github.io/)**

Other Members: [Mark Dredze](http://www.cs.jhu.edu/~mdredze/), [John Ayers](https://www.johnwayers.com/)

