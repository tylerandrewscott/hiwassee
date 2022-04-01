# hiwassee
measuring how local governments approach resilience using budget statements

This repository is for a paper  published in the Journal of Public Administration Research & Theory, [DOI and link to be filled in].

This repository contains all of the data and code materials necessary to replicate the project. For questions, please raise an issue and tag us here on Github. 

## Paper Abstract
Local governments consider a wide range of policies to increase resilience in the face of myriad risks, and employ a variety of tactics to communicate about these policies to external actors. An important platform to signal resilience as a policy priority is through the budget process wherein local communities decide ‘who gets what, when, and how’.  Using computational text mining techniques, we assess how county governments in California signal efforts toward resilience in their budgets during 2012-2017 fiscal years, as well as whether and how those signals are received by the capital market.  To test the relationship between local government resilience signals and capital market outcomes, we focus on county underlying credit ratings and the credit ratings of bond deals issued by counties. Empirical results show that county underlying credit ratings are insensitive to resilience signals in proposed budgets.  However, dynamic resilience signals that focus on risk management and preparedness appear to positively relate to bond deal credit ratings, while static resilience signals that focus on security and crisis management appear to do the opposite. By examining the efficacy of resilience signals and their effects on the capital market, we offer evidence on the link between policy signaling and financial outcomes at the local government level.

### Dependencies

* The project requires the R statistical programming language, version 4.1.2 (2021-11-01) and STATA version XXXX. Some of the R packages required might require loading underlying dependencies.



### Directory items

The directory has 5 folders:
- code: scripts for replicating the project
- input: 
  - prepped_county_input: these files are the prepared ingredients for the regression analysis, and include:
      - concept_results.txt, which has estimated resilience emphases by county and year
      - county_demographics.txt, which has population, unemployment, political ideology, and median household income by county and year (excepting political ideology, which is based on https://americanideologyproject.com/ and only estimated once per county)
      - SOMETHING HERE ABOUT PREPARED FINANCIAL DATA
  - raw_county_input: these files are the raw ingredients for the project, with the exception of files that are read directly in from online locations (e.g., Wikipedia articles on resilience). 

  
  
-output: log files, calculations, processed text, and figures/tables
-scratch: items in this folder are not synced to github. this folder is where the word embeddings file is downloaded if not already stored locally on the machine


### Replicating the project
1. run code/resilience_concept_coding.R to measure resilience emphases in budget documents.
2. run code/CountyRatings_Resilience.do to regress deal ratings as function of resilience emphasis and other covariates
3. run code/CountyRatings_Resilience.do to regress county credit ratings on resilience emphasis and other covariates.

## Help

Please feel free to ping us if any links appear broken or you have trouble with the code or data.

## Authors

Contributors names and contact info

Robert A. Greer, rgreer1@tamu.edu
Tima T. Moldogaziev,timatm@psu.edu 
Ryan P. Scott, ryan.p.scott@colostate.edu
Tyler A. Scott, tascott@ucdavis.edu, @tylerscottphd  



## License
Shield: [![CC BY 4.0][cc-by-shield]][cc-by]

This work is licensed under a
[Creative Commons Attribution 4.0 International License][cc-by].

[![CC BY 4.0][cc-by-image]][cc-by]

[cc-by]: http://creativecommons.org/licenses/by/4.0/
[cc-by-image]: https://i.creativecommons.org/l/by/4.0/88x31.png
[cc-by-shield]: https://img.shields.io/badge/License-CC%20BY%204.0-lightgrey.svg


## Acknowledgments

Inspiration, code snippets, etc.

