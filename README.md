# Flooding and food security in Sub-Saharan Africa

This repository contains the source code for our analysis of where and to what extent flooding impacts food security in Sub-Saharan Africa. We make use of econometric time series analysis techniques, including static panel regression and panel Granger causality, to quantify the effects of floods on the integrated food security phase classification (IPC) metric at the place of their occurrence up to roughly a year after their onset.  

## Data and feature engineering

The data used in this analysis include:

 - IPC over sub-Saharan Africa at a seasonal timestep from 2009–2020 produced by FEWSNET [available here](https://fews.net/fews-data/333)
 - Flood data from shapefiles produced by the Dartmouth Flood Observatory as part of the *Global Active Archive of Large Flood Events, 1985–Present* [available here](http://floodobservatory.colorado.edu/Archives/index.html)
 - Population data as of 2020 produced by WorldPop [available here](https://www.worldpop.org/geodata/summary?id=24777)

All of the data was harmonized to a spatial scale at the intersection of administrative level 2 units and FEWS NET Livelihood Zones (a 'panel') and a time scale corresponding to the reporting period of the IPC data. Furthermore, all time series were first-differenced to enforce stationarity. Code is available in `scripts/`. 

<!--
This work is accompanied by qualitative case study analysis in our paper, which can be cited as follows:

> Reed, C. et al. The impact of flooding on food security across Sub-Saharan Africa. (In review).
-->

## Summary of results

### Where does flooding affect food security?
We used panel Granger causality analysis as a way to identify where there exists a significant Granger-causal relationship between any of the derived flood variables and the IPC using a lag of up to four seasons. The code for this analysis is available in `analysis/modeling/granger-causality.R`. A global test for homogeneous non-causality (i.e., a global null hypothesis of field significance) which accounts for the cross-sectional dependence of the data was conducted in Stata. 

The figure below highlights the panels for which the indicated flood variables significantly Granger-caused changes in food security at a .00579 level. This level was calculated to constrain the false discovery rate to 0.10 given the large number of hypothesis tests conducted and to help prevent overinterpretation of potential Type I errors. Some of the panels did not experience enough variance in flooding over the study period to fit a linear model needed to test for Granger causality, and so were filtered out from this analysis as shown in the figure. 

<p align="center" style="font-size:8px">
<img src="/outputs/figures/granger-map-final.png" width="500" style = "float: left; margin-right: 10px;"/><img src="/outputs/figures/granger-pop-insecure.png"     width="500" style = "float: left; margin-right: 10px;"/> 
<i>Map of where first-differenced flood variables Granger cause changes in food security based on a significance level determined using the false discovery rate method. Total population (as of 2020) living in panels indicated as experiencing a Granger-causal relationship between flooding and food security.</i>
</p>


### How do different aspects of floods contribute to fluctuations in food security?
We used static random effects panel models at multiple spatial scopes to quantify the relationship between each of the contemporaneous and lagged flood variables and IPC. Additionally, we fit these models to the full data and the Granger-filtered data at each spatial scope. Random effects were specified as the correct model type for each sub-dataset with the code in `analysis/modeling/panel-model-specification.R` and the models were subsequenty fit in `analysis/modeling/panel-modeling.R`. Significant coefficients are plotted below.

<p align="center" style="font-size:8px">
 <img src="/outputs/figures/all-africa-panel.png" width="500" /><br>
 <i>Significant (p-value < .05) coefficient estimates and 95% confidence intervals of All-Africa panel models for both the full and Granger-filtered datasets. </i>
</p>
 
<p align="center" style="font-size:8px">
  <img src="/outputs/figures/west-africa-panel-fdr.png" width="500" /><br>
  <img src="/outputs/figures/east-africa-panel-fdr.png" width="500" /><br>
  <img src="/outputs/figures/south-africa-panel-fdr.png" width="500" /><br>
 <i> Significant coefficient estimates and 95% confidence intervals for regional and country-specific panel models. Solid lines indicate coefficient p-value < $$p*FDR = .0124$$, representing the coefficients which are most confidently nonzero. Dashed lines indicate coefficient p-value < .05, representing coefficients that are confidently nonzero yet which warrant more targeted analysis to confirm. (top) West Africa and Chad, (middle) East Africa, and (bottom) Southeast Africa. Countries are indicated by colors in corresponding legends. </i>
</p>



