<img src="inst/templates/hex_xray.png" width="15%" height="15%">

# dataxray

An interactive table interface for data summaries

Install package from GitHub with:

```
# install.packages("devtools")
devtools::install_github("agstn/dataxray")
```

## Report Xray
[Example](https://agstn.github.io/dataxray/) using ggplot2 [diamonds](https://ggplot2.tidyverse.org/reference/diamonds.html) data

```
library(dataxray)

diamonds <- diamonds %>% 
  mutate(price = structure(price, label = 'price in US dollars'),
         carat = structure(carat, label = 'weight of the diamond'),
         cut = structure(cut, label = 'quality of the cut (Fair, Good, Very Good, Premium, Ideal)'),
         color = structure(color, label = 'diamond colour, from D (best) to J (worst)'),
         clarity = structure(clarity, label = 'a measurement of how clear the diamond is 
                                               (I1 (worst), SI2, SI1, VS2, VS1, VVS2, VVS1, IF (best))'),
         x = structure(x, label = 'length in mm'),
         y = structure(y, label = 'width in mm'),
         z = structure(z, label = 'depth in mm'),
         depth = structure(depth, label = 'total depth percentage = z / mean(x, y) = 2 * z / (x + y)'),
         table = structure(table, label = 'width of top of diamond relative to widest point'))

diamonds %>% 
  report_xray(data_name = 'Diamonds', study = 'ggplot2')
```

<p><a href="https://agstn.github.io/dataxray/">
<img src="index.png" width="100%" height="100%">
</a></p>

## View Xray
RStudio IDE  Viewer pane  
```
library(dataxray)

diamonds %>% 
   make_xray() %>% 
   view_xray()
```
