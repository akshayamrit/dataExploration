# dataExploration
This package contains various functions which I find useful for Data Exploration. This package will focus on visualization, transformation and performing tests.

# Functions available:
  ## Graph:
    - graph.Univariate: This function lets the user perform univariate analysis of every column present in the dataframe by default. 
    User can specify the particular rows he wants to create visualizations for by assigning some value to the 'variable' argument. 
    The user can also choose to export these visualizations in png format to the directory specified by the user.
  ## Transformation:
    - transform.Group_and_factor: This function goes to through the whole dataframe and groups columns with same data types together. 
    User has the option to convert the columns into factors according to the number of unique values present in the column.
  ## Test:
    - test.Chisq: This function performs chi square test between a categorical target variable and user defined columns of the same dataset.
    - test.T_test.Cat: This function performs t test between a categorical target (upto 2 categories) and user defined columns of the same dataset.
    - test.All.Cat: This function is a wrapper function which lets us run all tests meant for categorical target together.
