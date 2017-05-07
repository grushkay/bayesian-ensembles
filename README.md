# bayesian-ensembles
We introduce a class of aggregation rules, or Bayesian ensembles, that are non-linear in the experts' probabilities. This code is used in our paper "Bayesian Ensembles of Binary-Event Forecasts" found here: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2940740
Each of our two empirical study has several relevant R code files. To reproduce the results reported in the paper, start with "File 1" files and follow the documentation in the code. 

For the Fannie Mae study, we provide the input data files to run Fannie "File 1", so there is no need to go to the site https://loanperformancedata.fanniemae.com/lppub/index where the raw data is available. Nor is there a need to run the SQL code that joins the raw data files into a single training set. Nonetheless, the SQL code is included here.  

For the Carvana study, download the input files ('training.csv' and 'test.csv') from the Kaggle website https://www.kaggle.com/c/DontGetKicked/data. 

The "File 1" files generate output data files that are then used as inputs in the "File 2" files. These output data files are also provided in this zip file. 

 
