# Revised-reproduction-package-for-Abebe-et-al-2021


This Repository holds a reproduction package for Abebe et al (2021) - Anonymity or Distance? Job Search and  Market Exclusion in a Growing African City

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

* **\_itt_oneendline.do**: This Do file creates the program that produces Table 3 of the paper. The proposed changes or comments to the files are the following:
    + **[Cool programming trick and Table 3 regression description](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/ce7b2d1546f0be8c1006b62b1fcb8fa08944bc8f)**  (tag: ce7b2d1) : This commit comments on a cool trick in stata in order to account for different cases extending from a command. Example: types of variables. Whether a variable is string or numeric, the program does two different things. In the code the authors use to run regressions whether the variables exists or not.

    + **[Final description of the Do File and scratching off unsed lines](https://github.com/jorgeluis8ar/Revised-reproduction-package-for-Abebe-et-al-2021/commit/deddd0e0e7c3b0dc6bd1a2ce24abf29283bc6d8e)** (tag: deddd0e): This commit comprises the final complete dscription of the function, scratching off unused lines in the code and changes in the Do File. 
    
