# Abebe et al (2021) - Anonymity or Distance? Job Search and  Market Exclusion in a Growing African City

This Repository holds a reproduction package for Abebe et al (2021)

## Abstract

> We show that helping young job seekers signal their skills to employers generates large and persistent improvements in their labour market outcomes. We do this by comparing an intervention that improves the ability to signal skills (the “job application workshop”) to a transport subsidy treatment designed to reduce the cost of job search. In the short run, both interventions have large positive effects on the probability of finding a formal job. The workshop also increases the probability of having a stable job with an open-ended contract. Four years later, the workshop significantly increases earnings, job satisfaction, and employment duration, but the effects of the transport subsidy have dissipated. Gains are concentrated on individuals who generally have worse labour market outcomes. Overall, our findings highlight that young people possess valuable skills that are unobservable to employers. Making these skills observable generates earnings gains that are far greater than the cost of the intervention.

---
## Repository Organization
### Description

The repository hosts all replication files for [Abebe et al (2021)](https://academic.oup.com/restud/article-abstract/88/3/1279/5912023) for the [Urban Economics](https://ignaciomsarmiento.github.io/teaching/Urban/2022/Urban.html) class at Universidad de los Andes. All replication files , giving the class purpose, are divided into two folders.

* **Original Replication Files**: Where all original replication files are hosted
* **Proposed Replication File**: Where the class deliverable for the final project will be hosted.

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

* **[main_end_line_results.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/do/main_endline_results.do)**: This Do File calls programs from the [Utilities](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/tree/main/Proposed%20Replication%20File/utilities) folder to make figures and tables using the end line results. This is the main script. The proposed changes or comments to the file are the following:

    + **[Changed lines od code for loops to create tables 5 and 6 and ossome organization](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/d5c11dcb7b29ba319e42c9d09c46756ca05923f4)** (tag: d5c11dc): This commit gives better organization to the Do File and creates a loop to create Tables 5 and 6. This tables were scattered over the Do File.
    + **[Commenting on Table A.9 - Regressions](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/a572971f09dbdd7fecb0b114e18fbccaf0c7d5d7)** (tag: a572971): This commit makes comments on all the code preceding the output of table A.9. The table presents evidence on the predictors of take-up of treatment. In detail, the Do File runs two regressions each for each branch of treatment. In other words, the regressions try to provide evidence of self selection of individuals into the treatment. In the comment, the regressions are fully explained.
    + **[Creation of table A.12 and quantile regression comments](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/8d01994ec7aae7555512a7db8c3aa07e08492e41)** (tag: 8d01994): This commit produces table a12 that was not within the original replication files. I created matrices and stored results from quantile regression in a matrix. Consequently, the matrices are merged together and the final table a12 is produced. There are also some coments on quantilie regression
    + **[Creation of table A.13 and quantile regression comments](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/de6c56101850e92d42a961a6d413bd8c84734601)** (tag: de6c561): This commit produces table a13 that was not within the original replication files. I created matrices and stored results from quantile regression in a matrix. Consequently, the matrices are merged together and the final table a13 is produced. There are also some coments on quantilie regression.
    + **[Renaming and reodiring figure 3 to figure 1](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/b872bb2c72623db8017084a556cd8e0cae2cb34a)** (tag: b872bb2): This commit fixes a bug in the replication files. In detail, figure 1 in the original files was named as figured 3.  I fixed the bug and comment on what the code section is doing.

* **[_itt_bothendlines.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/utilities/_itt_bothendlines.do)**: This Do File creates the program that produces Table 2 of the paper. The proposed changes or comments to the file are the following:

    + **[Stata conditional trick and regression discussion and some organizing](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/00e24c30c18470bd7ba3a671a93adb63077e9afe)** (tag: 00e24c3): This commit introduces a cool stata programming trick to get two cases to run in the same loop. Especially, if there exist a variable in the data set, the program should run a regression, but if a variable is not in the data set, then the program should run a different specification.  I also describe the regression used to get results. This description has sumsampling, weights, cluster standar errors and the use of locals to get the right specification.
    + **[Final comments on the matrix filling and the construction of linear hypotheses testing per period](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/653cfd44a4b794d232ddeaa8e165691c94b2622e)** (tag: 653cfd4): This commit gives the final comments of the matrices and gives comments on the constructin of linear hypotheses testing for every of the endline periods.
    + **[Q-values and final coments on the program](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/871feb1a4f6ccdebbcbd5250ddb309804220a147)** (tag: 871feb1): This commit gives comments on what going on in the definition of the Q-values in a two step methodology. Finally, all the process of the program is outlined for further reproduction.

* **[\_itt_oneendline.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/utilities/_itt_oneendline.do)**: This Do file creates the program that produces Table 3 of the paper. The proposed changes or comments to the file are the following:
    + **[Cool programming trick and Table 3 regression description](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/ce7b2d1546f0be8c1006b62b1fcb8fa08944bc8f)**  (tag: ce7b2d1) : This commit comments on a cool trick in stata in order to account for different cases extending from a command. Example: types of variables. Whether a variable is string or numeric, the program does two different things. In the code the authors use to run regressions whether the variables exists or not.

    + **[Final description of the Do File and scratching off unsed lines](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/deddd0e0e7c3b0dc6bd1a2ce24abf29283bc6d8e)** (tag: deddd0e): This commit comprises the final complete dscription of the function, scratching off unused lines in the code and changes in the Do File. 
    
* **[\_itt_het.do](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/blob/main/Proposed%20Replication%20File/utilities/_itt_het.do)**: This Do file creates the program that produces Tables 4,5,6 and A.26 of the paper and online annex. The proposed changes or comments to the file are the following:
    + **[Cool Stata programing trick, regression description and modifying the output of the program](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/d6b2a2e8158fab04975ec35186ddacc6c33c7357)** (tag: d6b2a2e):  This commit comments on a cool trick used in Stata. In detail, the authors use it to account for the non existence of a baseline variable. If it does exists, the program runs a diferent specification that if it does no exist. I also describe the regression specification and explain what is been used in each option. Finally I propose a small change of the program so it does not print every single step of the loop.
    + **[Final comments on the matrices filling, modifying the program output](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/022d67398d62c3d46974c861532ff0fa189ca29a)** (tag: 022d673): This commit gives the finals comments on the matrices filing and modifies the program so not every step of the loops gets printed.
    + **[Q-values and finals commets in all the program](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/39a899a7236ea5c4388241921f426cc96647fd73)** (tag: 39a899a): This commit gives comments on what going on in the definition of the Q-values in a two step methodology. Finally, all the process of the program is outlined for further reproduction.
