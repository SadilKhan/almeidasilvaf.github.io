---
title: "Gene Co-expression Networks"
subtitle: "From constructing, combining and comparing co-expression networks"
excerpt: "From constructing, combining and comparing co-expression networks"
date: 2018-01-01
author: "Deisy Morselli Gysi"
draft: false
tags:
  - co-expression
  - projects
categories:
  - R packages
  - Bioinformatics
  - Network Science
layout: single
# links:
# - icon: door-open
#   icon_pack: fas
#   name: website
#   url: https://bakeoff.netlify.com/
# - icon: github
#   icon_pack: fab
#   name: code
#   url: https://github.com/apreshill/bakeoff
---

## Summary

In co-expression networks, a pair of nodes is typically connected by a link if the genes they represent show a *significantly correlated expression pattern* across a set of biological samples of interest. They are often built from genome-wide expression data measured by RNA-Seq or microarrays. Those networks are often weighted, and it represents the strength of a gene-pair relationship. Gene co-expression networks are also signed, and the sign of the link can be indicative of whether a gene pair is regulated in the same direction or oppositely controlled. The majority of the methods used for constructing those networks rely on a similarity measure, such as mutual information or correlation.

In this project, we aimed to develop and improve methodologies for constructing, combining, and comparing co-expression netwoks.

### Construction

For the network construction, we improved the **wTO** method, by giving a probability for each link, and therefore, allowing the networks to have higher accuracy. This is a correlation based method, that normalizes the effect of the interaction by all gene neibours and that also accounts for positive and negative interactions.

### Combining co-expression networks

Gene co-expression networks can vary when analyzing similar datasets, e.g. from a repeated experiment or independent studies on a similar subject. These differences may arise from several sources: (A) **technical differences**, such as the platform on which the expression data was measured, the facility where data was collected and prepared, or how data was processed; (B) **biological differences** from confounding factors, such as sex, age, and geographic origin of the individuals measured. Therefore, it is needed to have one sigle network that combines all independently derived networks as biological replicates and systematically identifies their commonalities. We developed a novel method to compute the network that captures all this information; we call this the consensus network (CN).

### Comparing multiple co-expression networks

When comparing more than one network, different measures can be used, for example, one can identify the hubs in one network, identify modules and clusters in each one and compare those results. However, none of those comparisons compare the network topology and structure. For that, we have to compare links and nodes, and it can be easily done using CoDiNA.

CoDiNA identifies links - and nodes -that are **common** to all networks; **specific** to one network; or **different** in one network.

-   Common ($\alpha$ \| `a`): A particular interaction exists in *all networks* under comparison *with the same sign*;
-   Different ($\beta$ \| `b.`): A particular interaction exists in *all networks* under comparison *with different sign*;
-   Specific ($\gamma$ \| `g.`): A particular interaction *does not* exists in *all networks* under comparison.

## Publications :books:

:page_facing_up: Kolora, S.R.R., **Gysi, D.M.**, Schaffer, S., Grimm-Seyfarth, A., Szabolcs, M., Faria, R., Henle, K.,  Stadler, P.F., Schlegel, M., Nowick, K. _Accelerated evolution of tissue-specific genes mediates divergence amidst gene flow in European green lizards_, **Genome Biology and Evolution**. (2021) doi:10.1093/gbe/evab109


:page_facing_up: **Gysi, D.M.**, Nowick, K. _Construction, comparison and evolution of networks in life sciences and other disciplines_, **Journal of The Royal Society Interface**. vol. 17, no. 166 (2020) doi:10.1098/rsif.2019.0610


:page_facing_up: **Gysi, D.M.**, de Miranda Fragoso, T., Zebardast, F., Bertoli, W., Busskamp, V., Almaas, E., Nowick, K. _Whole transcriptomic network analysis using Co-expression Differential Network Analysis (CoDiNA)_, **PLoS ONE**. vol. 15, no. 10 (2020) doi:10.1371/journal.pone.0240523

:page_facing_up: **Gysi, D.M.**, Voigt, A., Fragoso, T.M., Almaas, E., Nowick, K. _wTO: an R package for computing weighted topological overlap and a consensus network with integrated visualization tool_, **BMC Bioinformatics**. vol. 19, no. 1 (2018) doi:10.1186/s12859-018-2351-7

:page_facing_up: Kutsche, L.K., **Gysi, D.M.** , Fallmann, J., Lenk, K., Petri, R., Swiersy, A., Klapper, S.D., Pircs, K., Khattak, S., Stadler, P. F., Jakobsson, J., Nowick, K., Busskamp, V. _Combined Experimental , System-Level Analyses Reveal the Complex Regulatory Network of miR-124 during Human Neurogenesis_, **Cell Systems**. vol. 7, no. 4 (2018) doi:10.1016/j.cels.2018.08.011


## R packages :package:

- wTO
- CoDiNA

