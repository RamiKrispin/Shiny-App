# ML Studio Package
A Shiny platform for ML applications (under construction). Current available features:
1. Data Management -
  - Ability to load data from installed R package, R environment and/or csv file
  - Modify variable attribution
2. Interactive data visualization tool with the Plotly package, that include:
- Scatter, line, histogram correlation, etc.
- Time series plots – seasonality, correlation etc.
3. Machine learning and deep learning algorithms with the H2O package, currently only classification models available (Deep Learning, Random Forest, GBM, GLM)

Features under construction:
-	Machine learning:

o	Deep learning models with Keras

o	Regression models

o	The caret training function and models

o	H2O Grid search

o	Unsupervised learning

o	Time series and forecasting with the forecast package 

-	Data manipulation tools – summary tables, sampling etc.
-	Data visualization – extending the current functionality

Please run the following R code to lunch the app into web browser (the app run best on google chrome):
source("https://raw.githubusercontent.com/RamiKrispin/Shiny-App/master/Shiny%20Modeling%20App%20Server.R")

Please note - the package automatically installed the required packages, however the installation of the H2O package may require some additional Java add-on and it recommended to install it in R before lunching the app. 

H2O installation code can be find here (under the “INSTALL IN R” tab):

https://h2o-release.s3.amazonaws.com/h2o/rel-weierstrass/3/index.html



