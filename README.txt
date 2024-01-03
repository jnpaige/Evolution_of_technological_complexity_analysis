#Read me

This repository contains the analysis of procedural unit data for a research paper
focused on measuring changes in technological complexity across the past 3 million years. 
The data analyzed are numbers of techniques (procedural units) present in tool-making sequences reported in the literature. 
These are stored in their own github repository:, and are also hosted on Zenodo:
The codebook used to build this dataset is stored in its own set of repositories: 


The analysis is broken up into four scripts. 

"Prepare data script.R" downloads the main dataset from github, and cleans it to prepare for analysis. 

"Statistical Analysis Code.R" runs a set of Bayesian models using the r package "brms", from a simple intercept only model, to more complex models. Those models are saved in the "models" folder, and then compared using a leave-one-out cross validation algorithm. The best performing model is then selected and assessed to make sure it is a good representation of the data.

"Make a map.R" generates a simple world map showing locations of sites (note, the site coordinates are randomly selected coordinates within a certain radius of the true site locations). This map is saved in the "Figures" folder. 

"Generate main figure script.R" builds the main figures summarizing the raw data, and the results of the Bayesian model. These figures are saved in the "Figures" folder. 

