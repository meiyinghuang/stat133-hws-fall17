---
title: "rawscores-dictionary.md"
author: "Meiying Huang"
date: "11/26/2017"
output: html_document
---

```{r}
# download RData file into your working directory
github <- "https://github.com/ucb-stat133/stat133-fall-2017/raw/master/"
csv <- "data/rawscores.csv"
download.file(url = paste0(github, csv), destfile = 'rawscores.csv')
```

```{r}
raw_scores <- read.csv('../rawdata/rawscores.csv', stringsAsFactors = FALSE)
```



