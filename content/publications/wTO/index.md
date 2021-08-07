---
title: "wTO: an R package for computing weighted topological overlap and a consensus network with integrated visualization tool"
date: 2018-10-24
date_end: '2018-10-24'
event_url: https://doi.org/10.1186/s12859-018-2351-7
event: "BMC Bioinformatics"
author: "Deisy Morselli Gysi"
draft: false
# layout options: single, single-sidebar
layout: single
categories:
- publication
- R
- co-expression
- R package
links:
- icon: newspaper
  icon_pack: fas
  name: Publication
  url: https://doi.org/10.1186/s12859-018-2351-7
---
# Abstract

Network analyses, such as of gene co-expression networks, metabolic networks and ecological networks have become a central approach for the systems-level study of biological data. Several software packages exist for generating and analyzing such networks, either from correlation scores or the absolute value of a transformed score called weighted topological overlap (wTO). However, since gene regulatory processes can up- or down-regulate genes, it is of great interest to explicitly consider both positive and negative correlations when constructing a gene co-expression network.
Here, we present an R package for calculating the weighted topological overlap (wTO), that, in contrast to existing packages, explicitly addresses the sign of the wTO values, and is thus especially valuable for the analysis of gene regulatory networks. The package includes the calculation of p-values (raw and adjusted) for each pairwise gene score. Our package also allows the calculation of networks from time series (without replicates). Since networks from independent datasets (biological repeats or related studies) are not the same due to technical and biological noise in the data, we additionally, incorporated a novel method for calculating a consensus network (CN) from two or more networks into our R package. To graphically inspect the resulting networks, the R package contains a visualization tool, which allows for the direct network manipulation and access of node and link information. When testing the package on a standard laptop computer, we can conduct all calculations for systems of more than 20,000 genes in under two hours. We compare our new wTO package to state of art packages and demonstrate the application of the wTO and CN functions using 3 independently derived datasets from healthy human pre-frontal cortex samples. To showcase an example for the time series application we utilized a metagenomics data set.
In this work, we developed a software package that allows the computation of wTO networks, CNs and a visualization tool in the R statistical environment. It is publicly available on CRAN repositories under the GPL2 Open Source License (https://cran.r-project.org/web/packages/wTO/).

# Citation
Gysi, D. M., Voigt, A., Fragoso, T.M, Almaas, E., Nowick, K. (2018). wTO: an R package for computing weighted topological overlap and a consensus network with integrated visualization tool. BMC Bioinformatics, 19:392. https://doi.org/10.1186/s12859-018-2351-7
