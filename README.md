![GitHub commit activity](https://img.shields.io/github/commit-activity/m/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021)
![GitHub commit activity (branch)](https://img.shields.io/github/commit-activity/w/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/main)
![GitHub contributors](https://img.shields.io/github/contributors/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021)
![GitHub last commit](https://img.shields.io/github/last-commit/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021)
![GitHub language count](https://img.shields.io/github/languages/count/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021)
![GitHub top language](https://img.shields.io/github/languages/top/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021)
![GitHub followers](https://img.shields.io/github/followers/jorgeluis8ar?style=social)

# Abebe et al (2021) - Anonymity or Distance? Job Search and  Market Exclusion in a Growing African City

This Repository holds a reproduction package for Abebe et al (2021)

## Abstract

> We show that helping young job seekers signal their skills to employers generates large and persistent improvements in their labour market outcomes. We do this by comparing an intervention that improves the ability to signal skills (the “job application workshop”) to a transport subsidy treatment designed to reduce the cost of job search. In the short run, both interventions have large positive effects on the probability of finding a formal job. The workshop also increases the probability of having a stable job with an open-ended contract. Four years later, the workshop significantly increases earnings, job satisfaction, and employment duration, but the effects of the transport subsidy have dissipated. Gains are concentrated on individuals who generally have worse labour market outcomes. Overall, our findings highlight that young people possess valuable skills that are unobservable to employers. Making these skills observable generates earnings gains that are far greater than the cost of the intervention.

---
## Repository Organization
### Description

The repository hosts all replication files for [Abebe et al (2021)](https://academic.oup.com/restud/article-abstract/88/3/1279/5912023) for the [Urban Economics](https://ignaciomsarmiento.github.io/teaching/Urban/2022/Urban.html) class at Universidad de los Andes. All replication files , giving the class purpose, are divided into two folders.

* **Original Replication Files**: Where all original replication files are hosted


```
Original Replication Files
└─── data   
│   │    attrition_bounds.dta
│   │    endline_data.dta
│   │    itt_other_studies.dta
│   │    phone_panel.dta
│   │    recall.dta
└─── do
│   │   bounds.do
│   │   comparison_other_studies.do
│   │   complete_family_tables_a15_a23.do
│   │   endogenous_stratification.do
│   │   main_endline_results.do
│   │   mediation_analysis.do
│   │   premia.do
│   │   rdd_a14.do
│   │   spillovers.do
│   │   trajectories.do
└─── utilities
│   │   _attrition_tables.do
│   │   _DefineQuadraticConstraintProgram_monthly.do
│   │   _DefineQuadraticConstraintProgram.do
│   │   _itt_bothendlines.do
│   │   _itt_het.do
│   │   _itt_oneendline.do
│   │   _itt_onetreat.do
│   │   dataout.ado
│   │   dataout.sthlp
└─── figures
│       │   All figures in the main paper and the appendix
└─── tables
│       │   All tables in the main paper and the appendix
```


* **Proposed Replication Files**: Where the class deliverable for the final project will be hosted.

```
Proposed Replication Files
└─── data   
│   │    attrition_bounds.dta
│   │    endline_data.dta
│   │    itt_other_studies.dta
│   │    phone_panel.dta
│   │    recall.dta
└─── do
│   └─── Results
│       │   bounds.do
│       │   comparison_other_studies.do
│       │   complete_family_tables_a15_a23.do
│       │   endogenous_stratification.do
│       │   main_endline_results.do
│       │   mediation_analysis.do
│       │   premia.do
│       │   rdd_a14.do
│       │   spillovers.do
│       │   trajectories.do
│   └─── Utilities
│       │   _attrition_tables.do
│       │   _DefineQuadraticConstraintProgram_monthly.do
│       │   _DefineQuadraticConstraintProgram.do
│       │   _itt_bothendlines.do
│       │   _itt_het.do
│       │   _itt_oneendline.do
│       │   _itt_onetreat.do
│       │   dataout.ado
│       │   dataout.sthlp
└─── results
│   └─── figures
│       │   All figures in the main paper and the appendix
│   └─── tables
│       │   All tables in the main paper and the appendix
```

***

### Deliverable 3
#### Objective
The third deliverable will focus on the initial improvement stage of the paper’s reproduction, and as with every deliverable, we will lean on the [ACRe Guidelines](https://bitss.github.io/ACRE/improvements.html). In this deliverable we will focus on understanding and improving the code.

***

#### Task
he task here is to improve documentation by adding comments to the code. You are expected to contribute substantially to the reproduction code, i.e., add lots of comments. What are lots? It will depend on your path since this deliverable will be a choose your own adventure type:

****

#### Solution
For this third deliverable I will be editing or commenting the existing code. To access the commit click on the comit's name. All commits are summarized in the following lists:

* **[Initial commit](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/db2f73317a593a30509195413c5c7fff3020f6f0)** (db22f733): This is the initiation commmit for the repo.

* **[main_end_line_results.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/do/main_endline_results.do)**: This Do File calls programs from the [Utilities](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/tree/main/Proposed%20Replication%20File/utilities) folder to make figures and tables using the end line results. This is the main script. The proposed changes or comments to the file are the following:

    + **[Changed lines of code for loops to create tables 5 and 6 and some organization](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/d5c11dcb7b29ba319e42c9d09c46756ca05923f4)** (tag: d5c11dc): This commit gives better organization to the Do File and creates a loop to create Tables 5 and 6. This tables were scattered over the Do File.
    + **[Commenting on Table A.9 - Regressions](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/a572971f09dbdd7fecb0b114e18fbccaf0c7d5d7)** (tag: a572971): This commit makes comments on all the code preceding the output of table A.9. The table presents evidence on the predictors of take-up of treatment. In detail, the Do File runs two regressions each for each branch of treatment. In other words, the regressions try to provide evidence of self selection of individuals into the treatment. In the comment, the regressions are fully explained.
    + **[Creation of table A.12 and quantile regression comments](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/8d01994ec7aae7555512a7db8c3aa07e08492e41)** (tag: 8d01994): This commit produces table a12 that was not within the original replication files. I created matrices and stored results from quantile regression in a matrix. Consequently, the matrices are merged together and the final table a12 is produced. There are also some coments on quantilie regression
    + **[Creation of table A.13 and quantile regression comments](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/de6c56101850e92d42a961a6d413bd8c84734601)** (tag: de6c561): This commit produces table a13 that was not within the original replication files. I created matrices and stored results from quantile regression in a matrix. Consequently, the matrices are merged together and the final table a13 is produced. There are also some coments on quantilie regression.
    + **[Renaming and reordering figure 3 to figure 1](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/b872bb2c72623db8017084a556cd8e0cae2cb34a)** (tag: b872bb2): This commit fixes a bug in the replication files. In detail, figure 1 in the original files was named as figured 3.  I fixed the bug and comment on what the code section is doing.

* **[bounds.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/do/bounds.do)**: This Do File proccesses data and creates tables a.8, a.27 - a.33. The proposed changes or comments to the file are the following:
    + **[Attrition analysis table A.8](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/19ff77e1dff8fdcff0cef8b42f2240cebde55e69)** (tag: 19ff77e): This commit comments and interprets regressions of table a.8. This table shows results on the predictors of attrition in the sample. The model is a Linear Probability Model for each of the follow up periods; endline 1 (2 years after the intervention) and endline 2 (6 years after the intervention).
    + **[Lee Bounds and table a.33](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/623b13491d86c3642d111acb86e442946c3f2350)** (tag: 632b134): This commit comments on the estimations presented in table a.33. In detail, the table presents estimates of lee bounds for the treatment effects of the two treatment branches in terms of formal work (written employmnt aggreement) and permanent work. 

* **[spillovers.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/do/spillovers.do)**: This Do File proccesses data and creates tables a.35 and a.36. The proposed changes or comments to the file are the following:

    + **[Frisch-Waugh-Lovell and spillover effects](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/c1222935e2bf0b50c2eb634b05204e26f6625980)** (tag: c122293): This commit comments on the estimates presented in tables a35 and a36. This tables calculate the spillover effect of the transport treatment on the treated and the untreated. In terms of the regression, the authors use the Frisch-Waugh-Lovell theorem to partial out covariates and keep the coeficients of interest.

* **[mediation_analysis.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/do/mediation_analysis.do)**: This Do File proccesses data and creates tables a.35 and a.36. The proposed changes or comments to the file are the following:

    + **[Frisch-Waugh-Lovell and determinants of 2018 wage earnings](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/510393cf5a80245095b881ccb230c5c3905f2f05)** (tag: 510393c): This commit comments on the estimates presented in tables a24. This tables calculate the determinants of 2018 wage earnings. In terms of the regression, the authors use the Frisch-Waugh-Lovell theorem to partial out covariates and keep the coeficients of interest and use different speciffication to see how estimators progress as new variables are introduced.
    + **[Figure 4 - Regression analysis and code description](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/58766b37ea6cfac637cf7145a142e087f2c0e8e7)** (tag: 58766b3): This commit comments on the estimates presented in figure 4. This figure analyses a mediation analysis of the determinants of wage earnings in endline number 2 (6 years after the intervention). The comments resumes the strategy (codewise) to get the estiamtes and the linear combinations to determine the point estimates and their confidence intervals. Furthermore, I also realise a brief analysis of the regression that produces such results.
    
* **[_itt_bothendlines.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/utilities/_itt_bothendlines.do)**: This Do File creates the program that produces Table 2 of the paper. The proposed changes or comments to the file are the following:

    + **[Stata conditional trick and regression discussion and some organizing](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/00e24c30c18470bd7ba3a671a93adb63077e9afe)** (tag: 00e24c3): This commit introduces a cool stata programming trick to get two cases to run in the same loop. Especially, if there exist a variable in the data set, the program should run a regression, but if a variable is not in the data set, then the program should run a different specification.  I also describe the regression used to get results. This description has sumsampling, weights, cluster standar errors and the use of locals to get the right specification.
    + **[Final comments on the matrix filling and the construction of linear hypotheses testing per period](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/653cfd44a4b794d232ddeaa8e165691c94b2622e)** (tag: 653cfd4): This commit gives the final comments of the matrices and gives comments on the constructin of linear hypotheses testing for every of the endline periods.
    + **[Q-values and final coments on the program](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/871feb1a4f6ccdebbcbd5250ddb309804220a147)** (tag: 871feb1): This commit gives comments on what going on in the definition of the Q-values in a two step methodology. Finally, all the process of the program is outlined for further reproduction.

* **[\_itt_oneendline.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/utilities/_itt_oneendline.do)**: This Do file creates the program that produces Table 3 of the paper. The proposed changes or comments to the file are the following:
    + **[Cool programming trick and Table 3 regression description](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/ce7b2d1546f0be8c1006b62b1fcb8fa08944bc8f)**  (tag: ce7b2d1) : This commit comments on a cool trick in stata in order to account for different cases extending from a command. Example: types of variables. Whether a variable is string or numeric, the program does two different things. In the code the authors use to run regressions whether the variables exists or not.

    + **[Final description of the Do File and scratching off unused lines](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/deddd0e0e7c3b0dc6bd1a2ce24abf29283bc6d8e)** (tag: deddd0e): This commit comprises the final complete dscription of the function, scratching off unused lines in the code and changes in the Do File. 
    
* **[\_itt_het.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/utilities/_itt_het.do)**: This Do file creates the program that produces Tables 4,5,6 and A.26 of the paper and online annex. The proposed changes or comments to the file are the following:
    + **[Cool Stata programing trick, regression description and modifying the output of the program](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/d6b2a2e8158fab04975ec35186ddacc6c33c7357)** (tag: d6b2a2e):  This commit comments on a cool trick used in Stata. In detail, the authors use it to account for the non existence of a baseline variable. If it does exists, the program runs a diferent specification that if it does no exist. I also describe the regression specification and explain what is been used in each option. Finally I propose a small change of the program so it does not print every single step of the loop.
    + **[Final comments on the matrices filling, modifying the program output](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/022d67398d62c3d46974c861532ff0fa189ca29a)** (tag: 022d673): This commit gives the finals comments on the matrices filing and modifies the program so not every step of the loops gets printed.
    + **[Q-values and finals comments in all the program](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/39a899a7236ea5c4388241921f426cc96647fd73)** (tag: 39a899a): This commit gives comments on what going on in the definition of the Q-values in a two step methodology. Finally, all the process of the program is outlined for further reproduction.

* **[\_itt_onetreat.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/utilities/_itt_onetreat.do)**: This Do File create the program that produces tables A.29-A.32 of the online annex. The propossed changes or comments to the file are the following:
    + **[Cool stata coding trick and regression analysis explanation](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/9611d540c46597e748a5878ead0ce6f49d80889d)** (tag: 9611d54): This commit comments on a Stata coding trick, but mainly it describes the regression analysis that is done. As a difference on previous estimations, this regressions takes a dummy variable as the intention to treatment if the individual was ever assinged to any treatment group.
    
    + **[Final comments on the matrices filling, modifying the program output](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/60b06721ee8e688e123f523b3c04092f99dadd1b)** (tag: 60b0672): This commit gives the finals comments on the matrices filing and modifies the program so not every step of the loops gets printed. Also I delete some parts of the code that did nothing and do not affect the end result
