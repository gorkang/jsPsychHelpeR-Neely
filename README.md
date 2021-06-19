# Registered Report: Religiosity's part on social adaptation

Please, address any correspondence to [aleneely\@gmail.com](mailto:aleneely@gmail.com)  


Manuscript title: Religiosity's part on social adaptation  
Manuscript ID: 519623  
Authors: Alejandra Neely-Prado, Gorka Navarrete, David Huepe, Michiel van Elk  
Journal: Frontiers in Psychology, section Personality and Social Psychology  
Article type: Registered Report  
URL: https://github.com/gorkang/jsPsychHelpeR-Neely



## Setup

1.  Run `setup.R` :

-   Makes sure all the necessary packages are present
-   Makes sure all the necessary folders are present

## Data preparation and analysis

We use the {targets} (<https://github.com/wlandau/targets>) package.


**The whole data preparation process can be reproduced running `targets::tar_make()`**


A nice visualization of all the pre-processing steps can be seen with `targets::tar_visnetwork(targets_only = TRUE)`

The file `_targets.R` contains the important parameters and calls to all the functions used when running `targets::tar_make()`

To see more detail about any specific step, you can:

1.  Go to the relevant function  
2.  Load the input parameters of the function with `targets::tar_load()`  
3.  Run the code step by step as you would normally do  

## Output tables and plots

-   **Raw data** (anonymized) are in `data/`

-   **Plots** and **tables** are in `outputs/plots` and `outputs/tables` respectively.

-   **Dataframes** for different stages of data processing can be seen in `outputs/data`
