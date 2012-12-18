 ----------------------------------------------------------------------
 Datasets and Code for "Color Compatibility From Large Datasets" ACM TOG, Proc. SIGGRAPH 2011
 Peter O'Donovan and Aseem Agarwala and Aaron Hertzmann
 
 Copyright (c) 2011 Peter O'Donovan
 Distributed under the Creative Commons BY-NC-SA license.
 
http://creativecommons.org/licenses/by-nc-sa/2.5/ca/
 
 Contact: Peter O'Donovan at <odonovan@dgp.toronto.edu>
 ----------------------------------------------------------------------
 
 Please look at run.m for a simple example of training and testing the LASSO regressor. 
 
 The datasets are in the data folder.
 
 The datasets include themes, names, unique ids, and mean ratings. The MTurk dataset includes user-normalized targets. That is, each user's ratings is first subtracted by the user's mean rating and then divided by the user's std deviation of ratings. 