Donor Competition Experiment Dataset
================
Gento Kato
November 20, 2019

Meta Data
=========

<code>id</code>
---------------

-   *Description*: Respondent ID number
-   *Values*: Raw values

| Min. | 1st Qu. | Median |   Mean   | 3rd Qu. | Max. |
|:----:|:-------:|:------:|:--------:|:-------:|:----:|
|   4  | 1718.25 |  3666  | 3931.022 |  5964.5 | 9238 |

Pre-treatment Covariates
========================

<code>issint</code>
-------------------

-   *Original*: <code>q01</code>
-   *Description*: Interests toward international issues
-   *Values*: 0 Not interested; 1 Somewhat interested; 2 Very interested

|   0  |   1  |   2  |
|:----:|:----:|:----:|
| 1031 | 2110 | 1112 |

<code>threat.MMR</code>
-----------------------

-   *Original*: <code>q04\_8</code>
-   *Description*: Threat posed by Myanmar
-   *Values*: 0 Not threatened; 1 Neither; 2 Threatened

|   0  |   1  |  2  |
|:----:|:----:|:---:|
| 2805 | 1143 | 236 |

<code>threat.PHL</code>
-----------------------

-   *Original*: <code>q04\_3</code>
-   *Description*: Threat posed by Philippines
-   *Values*: 0 Not threatened; 1 Neither; 2 Threatened

|   0  |   1  |  2  |
|:----:|:----:|:---:|
| 2800 | 1137 | 247 |

<code>imp.MMR</code>
--------------------

-   *Original*: <code>q05\_8</code>
-   *Description*: Importance of Myanmar for Japan
-   *Values*: 0 Not important; 1 Neither; 2 Important

|  0  |   1  |   2  |
|:---:|:----:|:----:|
| 568 | 1729 | 1898 |

<code>imp.PHL</code>
--------------------

-   *Original*: <code>q05\_3</code>
-   *Description*: Importance of Philippines for Japan
-   *Values*: 0 Not important; 1 Neither; 2 Important

|  0  |   1  |   2  |
|:---:|:----:|:----:|
| 430 | 1625 | 2144 |

<code>odaimp</code>
-------------------

-   *Original*: <code>q06</code>
-   *Description*: Importance of ODA
-   *Values*: 0 Neither/Not important; 1 Important

|  0  |   1  |
|:---:|:----:|
| 855 | 3262 |

<code>potential.MMR</code>
--------------------------

-   *Original*: <code>q07\_5</code>
-   *Description*: Future economic potential of Myanmar for Japan
-   *Values*: ; 0 Low ; 1 Moderate/DK; 2 High

<!-- -->

    ## 
    ##    1    2    3    4    5    7    9 
    ##  229  615 1438  851  395  705   89

|  0  |   1  |   2  |
|:---:|:----:|:----:|
| 844 | 2143 | 1246 |

<code>potential.PHL</code>
--------------------------

-   *Original*: <code>q07\_3</code>
-   *Description*: Future economic potential of Philippines for Japan
-   *Values*: 0 Low; 1 Moderate/DK; 2 High

|  0  |   1  |   2  |
|:---:|:----:|:----:|
| 721 | 2061 | 1450 |

Pre-treatment Moderator
=======================

<code>threat.CHN</code>
-----------------------

-   *Original*: <code>q04\_2</code>
-   *Description*: Threat posed by China (Binary)
-   *Values*: 0 Neither/Not Threatened; 1 Threatened

|  0  |   1  |
|:---:|:----:|
| 984 | 3226 |

<code>threat.CHN.3cat</code>
----------------------------

-   *Original*: <code>q04\_2</code>
-   *Description*: Threat posed by China (3 categories)
-   *Values*: 0 Neither/Not Threatened; 1 Somewhat Threatened; 2 Highly Threatened

|  0  |   1  |
|:---:|:----:|
| 984 | 3226 |

Treatment Variables
===================

<code>treatment</code>
----------------------

-   *Original*: <code>pat\_q08\_q11</code>
-   *Description*: Treatment ID Number
-   *Values*: Raw values

|   1  |   2  |   3  |   5  |
|:----:|:----:|:----:|:----:|
| 1078 | 1059 | 1048 | 1137 |

<code>treat\_China</code>
-------------------------

-   *Original*: <code>pat\_q08\_q11</code>
-   *Description*: Treatment (China as the alternative donor)
-   *Values*: 1 Treated; 0 Control

|   0  |   1  |
|:----:|:----:|
| 2215 | 2107 |

<code>treat\_MMR</code>
-----------------------

-   *Original*: <code>pat\_q08\_q11</code>
-   *Description*: Recipient country (Myanmar)
-   *Values*: 1 Myanmar is recipient; 0 Philippines is recipient

|   0  |   1  |
|:----:|:----:|
| 2185 | 2137 |

<code>treat\_PHL</code>
-----------------------

-   *Original*: <code>pat\_q08\_q11</code>
-   *Description*: Recipient country (Philippines)
-   *Values*: 1 Philippines is recipient; 0 Myanmar is recipient

|   0  |   1  |
|:----:|:----:|
| 2137 | 2185 |

Mediator Variables
==================

<code>med\_econ</code>
----------------------

-   *Original*: <code>q09a</code>
-   *Description*: Damage to national economic interests
-   *Values*: 1 Promote interests greatly; 2 Promote interests a little; 3 No change; 4 Damage interests a little; 5 Damage interests greatly

|  1  |  2  |   3  |   4  |  5  |
|:---:|:---:|:----:|:----:|:---:|
|  42 | 193 | 1715 | 1817 | 347 |

<code>med\_secu</code>
----------------------

-   *Original*: <code>q09b</code>
-   *Description*: Damage to national security interests
-   *Values*: 1 Promote interests greatly; 2 Promote interests a little; 3 No change; 4 Damage interests a little; 5 Damage interests greatly

|  1  |  2  |   3  |   4  |  5  |
|:---:|:---:|:----:|:----:|:---:|
|  58 | 170 | 2057 | 1459 | 357 |

<code>med\_repu</code>
----------------------

-   *Original*: <code>q09c</code>
-   *Description*: Damage to international reputation
-   *Values*: 1 Improve greatly; 2 Improve a little; 3 No change; 4 Worsen a little; 5 Worsen greatly

|  1  |  2  |   3  |   4  |  5  |
|:---:|:---:|:----:|:----:|:---:|
|  60 | 364 | 2265 | 1181 | 249 |

<code>med\_effi</code>
----------------------

-   *Original*: <code>q09d</code>
-   *Description*: Effectiveness of aid cancellation to reduce human rights violations
-   *Values*: 1 Reduce violations greatly; 2 Reduce violations a little; 3 No change; 4 Increase violations a little; 5 Increase violations greatly

|  1  |  2  |   3  |  4  |  5  |
|:---:|:---:|:----:|:---:|:---:|
| 100 | 607 | 2637 | 560 | 172 |

Outcome Variable
================

<code>cancel\_aid</code>
------------------------

-   *Original*: <code>q10</code>, <code>q10sq1</code>, <code>q10sq2</code>
-   *Description*: Opinion on aid cancellation (9p scale)
-   *Values*: 1 Should not cancel (very strong); 2 Should not cancel (somewhat strong); 3 Should not cancel (not so strong); 4 Should not cancel, if forced (after probed); 5 Difficult to say, even if forced (after probed); 6 Should cancel, if forced (after probed); 7 Should cancel (not so strong); 8 Should cancel (somewhat strong); 9 Should cancel (very strong)

|  1  |  2  |  3  |  4  |  5  |  6  |  7  |  8  |  9  |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| 147 | 574 | 576 | 959 | 699 | 498 | 217 | 251 | 155 |

<code>cancel\_aid\_3cat</code>
------------------------------

-   *Original*: <code>q10</code>
-   *Description*: Opinion on aid cancellation (3p scale)
-   *Values*: 1 Should not cancel; 2 Difficult to say; 3 Should cancel

|   1  |   2  |  3  |
|:----:|:----:|:---:|
| 1308 | 2194 | 625 |

<code>cancel\_aid\_2cat</code>
------------------------------

-   *Original*: <code>q10</code>
-   *Description*: Opinion on aid cancellation (binary)
-   *Values*: ; 0 Should not cancel; 1 Difficult to say/Should cancel

|   0  |   1  |
|:----:|:----:|
| 1308 | 2819 |

Demographic and Ideology Variables
==================================

<code>fem</code>
----------------

-   *Original*: <code>f01</code>
-   *Description*: Respondent gender
-   *Values*: 0 Male; 1 Female

|   0  |   1  |
|:----:|:----:|
| 2182 | 2140 |

<code>age</code>
----------------

-   *Original*: <code>f02</code>
-   *Description*: Respondent age
-   *Values*: Raw values

| Min. | 1st Qu. | Median |   Mean   | 3rd Qu. | Max. |
|:----:|:-------:|:------:|:--------:|:-------:|:----:|
|  20  |    34   |   46   | 45.78876 |    58   |  69  |

<code>right</code>
------------------

-   *Original*: <code>q25</code>
-   *Description*: Right-wing ideology (binary)
-   *Values*: 1 Right-wing; 0 Not

|   0  |   1  |
|:----:|:----:|
| 2373 | 1596 |

<code>ide3</code>
-----------------

-   *Original*: <code>q25</code>
-   *Description*: Right-wing ideology (3 categories)
-   *Values*: 0 Left; 1 Moderate; 2 Right

|  0  |   1  |   2  |
|:---:|:----:|:----:|
| 927 | 1446 | 1596 |

Compliers
=========

<code>comply</code>
-------------------

-   *Original*: <code>q12</code>, <code>q24</code>
-   *Description*: Compliers and Non-compliers.
-   *Values*: 1 Compliers; Not-1 Non-compliers

|   1  |  2  |  3  |  4  |  5  |  6  |
|:----:|:---:|:---:|:---:|:---:|:---:|
| 3709 | 289 |  11 |  50 | 179 |  84 |

Save Data
=========

The contents of following two files are identical.

<code>data/donorexp.csv</code>
------------------------------

CSV file.

<code>data/donorexp.rds</code>
------------------------------

RDS file (R data format). This file is used for the analysis.
