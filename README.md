![](https://shawnmagic-1257599720.cos.ap-chengdu.myqcloud.com/picgologo_long.webp)

[![R version](https://img.shields.io/badge/R-v4.3.3-salmon)](https://www.r-project.org) [![TBtools version](https://img.shields.io/badge/TBtools-%3Ev2.096-greenyellow)](https://www.yuque.com/cjchen/hirv8i/fzc4g9) ![lifecycle](https://img.shields.io/badge/lifecycle-Experimental-lightcyan) [![license](https://img.shields.io/badge/license-MIT-red)](https://opensource.org/licenses/MIT) [![Myblog](https://img.shields.io/badge/Blog-ShanwLearnBioinfo-purple)](https://shawnwx2019.github.io/)

# Cookbook

MetMiner cookbook: [https://shawnwx2019.github.io/metminer-cookbook/](https://shawnwx2019.github.io/metminer-cookbook/)

# Quick start

## Install

``` r
##> for chinese users:
##> options("repos" = c(CRAN="https://mirrors.tuna.tsinghua.edu.cn/CRAN/"))

if (!require('remotes')) install.packages('remotes');
if (!require('Hmisc')) install.packages('Hmisc');
if (!require('tidymass')) {
  source("https://www.tidymass.org/tidymass-packages/install_tidymass.txt");
  install_tidymass(from = "tidymass.org")
};
if (!require('ropls')) remotes::install_github("SamGG/ropls");
if (!require('PCAtools')) remotes::install_github('kevinblighe/PCAtools');
if (!require('MDAtoolkits')) remotes::install_github('ShawnWx2019/MDAtoolkits',ref = 'master');
if (!require('shinyFiles')) remotes::install_github('thomasp85/shinyFiles');
if (!require('shinyWidgets')) remotes::install_github("dreamRs/shinyWidgets");
if (!require('ComplexHeatmap')) remotes::install_github('jokergoo/ComplexHeatmap');
if (!require('clusterProfiler')) remotes::install_github('YuLab-SMU/clusterProfiler');
if (!require('shinyjs')) install.packages('shinyjs');
if (!require('dashboardthemes')) install.packages('dashboardthemes');
if (!require("DT")) install.packages('DT');
if (!require('shiny')) install.packages('shiny');
if (!require('bsicons')) install.packages('bsicons');
if (!require('bslib')) install.packages('bslib');
if (!require('ggsci')) install.packages('ggsci');
if (!require('plotly')) install.packages('plotly');
if (!require('ggrepel')) install.packages('ggrepel');
if (!require('shinythemes')) install.packages('shinythemes');
if (!require('ggstatsplot')) install.packages('ggstatsplot');
if (!require('patchwork')) install.packages('patchwork');
if (!require('tidyverse')) install.packages('tidyverse');
if (!require('shinyjqui')) install.packages('shinyjqui');
if (!require('colourpicker')) install.packages('colourpicker');
if (!require('RCurl')) install.packages('RCurl');
if (!require('MetMiner')) remotes::install_github('ShawnWx2019/MetMiner');
```

## Start

```r
library(tidyverse)
library(tidymass)
library(MDAtoolkits)
library(MetMiner)
##> start metMiner shinyapp
run_metMiner(maxRequestSize = 300)
```

# Citation

If you have utilized MetMiner in your project, please cite:

Xiao Wang, Shuang Liang, Wenqi Yang, Ke Yu, Fei Liang, Bing Zhao, Xiang Zhu, Chao Zhou, Luis A. J. Mur, Jeremy A, Roberts, Junli Zhang,and Xuebin Zhang. 2024 “MetMiner: A user-friendly pipeline for large-scale plant metabolomics data analysis”

Shen, Xiaotao, Hong Yan, Chuchu Wang, Peng Gao, Caroline H. Johnson, and Michael P. Snyder. 2022. “TidyMass an Object-Oriented Reproducible Analysis Framework for LC Data.” Nature Communications 13 (1): 4365. https://doi.org/10.1038/s41467-022-32155-w.

If you started metMiner from TBtools plugin, please also cite:

Chen, Chengjie, Ya Wu, Jiawei Li, Xiao Wang, Zaohai Zeng, Jing Xu, Yuanlong Liu, et al. 2023. “TBtools-II: A "One for All, All for One" Bioinformatics Platform for Biological Big-Data Mining.” Molecular Plant 0 (0). https://doi.org/10.1016/j.molp.2023.09.010.

