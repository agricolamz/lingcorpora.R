# Lingcorpora

`lingcorpora` package provides R with API from different linguistic corpora. This package includes APIs for:

* [Abkhaz Text Corpus](http://baltoslav.eu/apsua/index.php)
* [Avar Text Corpus](http://baltoslav.eu/avar/index.php)
* [National Corpus of Polish](nkjp.pl)
* [National Corpus of Russian Language](http://www.ruscorpora.ru/)

## Instalation

You can install the development version from Github:
  ```{r, eval = F}
install.packages("devtools")
devtools::install_github("agricolamz/lingcorpora.R", dependencies = TRUE)
```

Load a library:
```{r}
library(lingcorpora)
```
In addition to the usual R package documentation, we also have a [website with examples](https://agricolamz.github.io/lingcorpora.R/).

Python version of this package by Alexey Koshevoy is located [here](https://github.com/alexeykosh/lingcorpora.py)
