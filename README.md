RsimCity
========

R package for simulations of linear models

Introduction
------------
provides a suite to build and run simulations on linear and additive models. It provides a flexible way to define the data generating model, and useful commands to set up a simulation experiment. Models can be specified with an intuitive syntax inspired by *sem* package. It basically allows building datasets accodingly to a set of linear models with different distributions of residuals, and provides mechanisms to apply custom functions to process the generated data.   
See examples in  *simc.model* and *experiment* 

Details
-----------

The building blocks of one simulation experiment are: a command to declare the data generating model *simc.model*, a command to set up the experiment *experiment* to be build on this model, and a command *run* to run the simulation drawing several samples and computing statistics on it. Many different functions are provided to fine-tune the model and the experiment.
