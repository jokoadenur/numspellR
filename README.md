![Build Status](https://img.shields.io/badge/build-passing-brightgreen.svg)
![CRAN Version](https://img.shields.io/badge/CRAN-3.6.0-brightgreen)
![Open Issues](https://img.shields.io/badge/open%20issues-0-brightgreen)
![License](https://img.shields.io/badge/License-MIT-blue)
<img width="300" height="300" alt="numspellR_hex" src="https://github.com/user-attachments/assets/91901077-ab2d-415d-ace0-a0a06bcc8cf0" />

# numspellR
**Detection of Numeric Persistence and Rigidity Patterns**

`numspellR` is an R package for detecting numeric persistence (“spells”) and rigidity patterns in time-ordered numeric data. It is designed for applied researchers, data analysts, and policy practitioners who need interpretable measures of stability, stickiness, and adjustment dynamics in numeric series. The package automatically identifies numeric variables from vectors or data frames, detects periods of prolonged stability, and computes a set of spell-based rigidity metrics commonly used in empirical economics, policy analysis, and applied statistics.

**✨ Key Features**

📊 Automatic spell detection
Identifies consecutive periods of stability based on adaptive tolerance thresholds.

⏱️ Persistence & rigidity metrics
Computes interpretable indicators such as: **average, median, and maximum spell length, stability ratio, elasticity index, adjustment frequency, and spell concentration**

🧠 Plain-language interpretation
Produces qualitative persistence classifications (from highly flexible to highly rigid), accompanied by text explanations suitable for policy and applied analysis.

🌍 Bilingual output
Interpretation and labels are available in **English** and **Indonesian**.

🧩 Flexible input structure
Works with numeric vectors, data frames, or tibbles. Non-numeric columns are ignored automatically.

**Installation**

From CRAN:
```{r}
install.packages("numspellR")
```
From Github:

```{r}
remotes::install_github("jokoadenur/numspellR")
```

**How to use this package?**

Example (1)
```{r}
library(numspellR)
x <- c(10, 10, 10, 11, 11, 11, 11, 12)
numspellr(x, lang = "english")
```
**Result**
```
structure avg_spell median_spell max_spell stability_ratio
1 numeric_vector       3.5          3.5         4           0.875
  elasticity_index adjustment_frequency spell_concentration
1       0.02727273                 0.25           0.5102041
     persistence_status
1 Very High Persistence
                                                                                                                                                                                                                                              interpretation
1 The series shows numeric persistence. Values remain unchanged for about 3.5 periods on average. Adjustments occur periodically. When changes occur, their magnitude is small. From a policy perspective, this indicates strong rigidity and slow response.
  id variable
1  x        x
```
Example (2)
```{r}
library(numspellR)
df <- data.frame(
  time = 1:8,
  value = c(5, 5, 5, 5, 6, 6, 6, 7)
)
numspellr(df, lang = "indonesia")
```
**Result**
```
     structure avg_spell median_spell max_spell stability_ratio
1 seri_numerik       3.5          3.5         4           0.875
  elasticity_index adjustment_frequency spell_concentration
1       0.05238095                 0.25           0.5102041
  persistence_status
1        Sangat Kaku
                                                                                                                                                                                                                                                                                    interpretation
1 Rata-rata nilai bertahan sekitar 3.5 periode sebelum mengalami perubahan.   Perubahan cukup sering terjadi.  Perubahan terjadi secara bertahap.  Periode stagnan terkonsentrasi pada beberapa fase panjang.  Variabel menunjukkan penyesuaian terbatas dan cenderung lambat merespons perubahan.
     id variable
1 value    value
```

**Intended Use**

`numspellR` is particularly useful for: (1) price rigidity and stickiness analysis; (2) macroeconomic and microeconomic adjustment studies; (3) policy monitoring and evaluation; and (4) applied time-series diagnostics.

**Author**

Joko Ade Nursiyono
Data Analyst (East Java, BPS-Statistics Indonesia)

**Citation**

If you use `numspellR` in research or official reports, please cite:
```
Nursiyono, J. A. (2026). numspellR: Detection of Numeric Persistence and Rigidity Patterns.
```
**References**
```
Caballero, R. J., & Engel, E. M. R. A. (1993). Microeconomic adjustment hazards and aggregate dynamics. Quarterly Journal of Economics, 108(2), 359–383. (url: https://academic.oup.com/qje/article-abstract/108/2/359/1917542)

Nakamura, E., & Steinsson, J. (2008). Five facts about prices: A reevaluation of menu cost models. Quarterly Journal of Economics, 123(4), 1415–1464. (url: https://academic.oup.com/qje/article-abstract/123/4/1415/1933176)
```
