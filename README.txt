#Read me

This repository contains the analysis of procedural unit data for a research paper
focused on measuring changes in technological complexity across the past 3 million years. 
The data analyzed are numbers of techniques (procedural units) present in tool-making sequences reported in the literature. 
These are stored in their own github repository. The original dataset is hosted on github here: https://github.com/jnpaige/Procedural.unit.dataset. 

The codebook used to build this dataset (version 1.1) is stored here: https://github.com/jnpaige/Procedural.unit.codebook


The analysis is broken up into four scripts. 

"Prepare data for model generation.R" takes the data, isolates only the archaeological assemblages, and prepares the data for input into brms.

"Generate models.R" runs a set of Bayesian models using the r package "brms", from a simple intercept only model, to more complex models. Those models are saved in the "models" folder, and then compared using a leave-one-out cross validation algorithm. The best performing model is then selected and assessed to make sure it is a good representation of the data.

"Generate figures.R" builds the main figures summarizing the raw data, and the results of the Bayesian model. These figures are saved in the "Figures" folder. 

