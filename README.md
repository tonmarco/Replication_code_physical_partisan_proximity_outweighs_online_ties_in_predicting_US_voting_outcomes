# Data and Replication code of the paper "Physical partisan proximity outweighs online ties in predicting US voting outcomes"
## Marco Tonin, Bruno Lepri, and Michele Tizzoni

The pre-print of the paper can be found here: https://arxiv.org/abs/2407.12146.

In this repository, you can find the following files:

**Data**:

1. **county_data.csv**: data of the counties in the contiguous US related to partisan exposure (to Democrats and Republicans, offline, online, and at the residential level), partisan segregation (offline, online, and at the residential level), FIPS codes, census data, online and offline network diversity, online and offline extroversion.
2. **county_shapefile.xxx**: files containing the geometries (and FIPS codes) to plot the maps.
3. **Font**: the folder contains the font used for the plots (credits to Simone Centellegher https://github.com/scentellegher). 

**Code**:

1. **compute-partisan-exposure-code.ipynb**: Python notebook containing the code to compute offline and online partisan exposure and extroversion. Note: Co-location data and Social Connectedness Index data are not provided. Co-location data is available through the Meta Data for Good portal, while the SCI is available through the Humanitarian Data Exchange portal.
2. **dominance-analysis-code.ipynb**: Python notebook with the analysis related to the Dominance Analysis (for metro and non-metro areas, swing and non-swing counties, and commuting flows).
3. **elastic-net-code.ipynb**: Python notebook with the ElasticNet model analysis.
4. **exposure-plot-test-code.ipynb**: Python notebook with the plots of the distribution of offline, online, and residential partisan exposure to Republicans and Democrats, and the t-test between dimensions.
5. **partisan-segregation-code.ipynb**: Python notebook with the distribution and maps of partisan segregation, Gradient Boosting Regression models and SHAP values.
6. **plot-entropy-introversion**: Python notebook with the plots of network diversity and extroversion (please refer to the Supplementary Information material).
7. **regression-models.R**: R script that includes all the models related to Spatial Lag Autoregressive and Ordinary Least Squares models.

To cite our work:
Tonin, M., Lepri, B., & Tizzoni, M. (2024). Physical partisan proximity outweighs online ties in predicting US voting outcomes. *arXiv preprint arXiv:2407.12146*.
