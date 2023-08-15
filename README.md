
## PrestoGP File Structure 


We will utilize a strict file structure for all projects. This will ensure that the code and data from the project is accessible and reproducible in the long term. In short, we will utilize a relative path file setup with a few strictly define folder names. After that, there is more flexibility, but this will ensure consistently and reproducibility. 

* **Project Name Root Folder** - 
  If working in R and Rstudio, you can create an Rproject, which will add a .Rproj here
    + **Code** - location for all scripts - can add subfolders as needed
    +  **Input** - this is where data that comes into the project, either in raw form or processed goes 
    +  **Output** - this is where analysis or workflow intermediates and summary data goes - Can created subfolders as needed such as: 
        - **Tables**
        - **Figures**
    + **Manuscript** - to contain all materials directly related to the submission of the manuscript.

**Important**: The entirety of a project, including all data and code, are to live in the Project folder on the SET 
    
## Relative Paths 

With the basic file structure established for the project, we can utilize relative paths. Relative paths will ensure our code will run on any computer or any system. 
If we use an Rmarkdown file, we can specify in the Global Options for Rmarkdown files to be evaluated from project directory (i.e. where the .Rproj resides). 
If we are not using an R project (or something similar for Python, etc.) or are using basic R (e.g. on a Linux), then we may need to specify the working directory as the project/root folder. If 
you know an approach that works for things outside Rstudio, please let me know.


Here is an R example below. Assume the following file structure:

 * SET_Project is the root
 * Code, Input, Output, and Manuscript are all in SET_Project
 * Helper functions and Analysis are in Code


```{r Relative Path Example in Rstudio, eval=FALSE}

# This is an example of a relative path in Rstudio

# If we are using an Rproject, this is where the .Rproj file is located

data <- read.csv("Input/data.csv")

# source a helper function
source("Code/Helper_Functions.R")

mdl <- my_helper_function(data)

# save the model

save(mdl, "Output/mdl.RData")

# source an analysis script
source("Code/Analysis/Analysis_Script.R")
```

If we are not using an Rproject, we can specify the working directory as the root folder. 

```{r Relative Path with Root Specified, eval=FALSE}

setwd("SET_Project")

data <- read.csv("Input/data.csv")

```


## Code names with order number prefixes 

We will use a prefix naming convention for all code files. This will ensure that the code files are presented in the correct order.

The prefix will be a two digit number followed by an underscore. The two digit number will be the order in which the code is to be run. For example, 01_ would be the first code file to run, 02_ would be the second, and so on:


*  **01_Calculate_Vars.R**
*  **02_Fit_Model.R**
*  **03_Make_Predictions.R** 
  

The code file name should be descriptive of the code's purpose. For example, 01_Data_Cleaning.R would be the first code file to run and it would contain code for cleaning the data.



###  R File types
Functions are written using basic R files (.R) and scripts, or data analysis workflows, are written in RMarkdown (.Rmd). RMarkdown helps ensure that our code is well-organized and easy to read.





