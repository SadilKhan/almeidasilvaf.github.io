---
title: "cageminer"
subtitle: "An R/Bioconductor package to mine candidate genes by integrating GWAS and gene coexpression networks"
excerpt: "An R/Bioconductor package to mine candidate genes by integrating GWAS and gene coexpression networks"
date: 2021-08-07
author: "Fabrício Almeida-Silva"
draft: false
tags:
  - coexpression networks
  - GWAS
  - gene discovery
  - bioconductor
categories:
  - R packages
  - biotechnology
layout: single
links:
 - icon: github
   icon_pack: fab
   name: Repo
   url: https://github.com/almeidasilvaf/cageminer
---

## Summary

cageminer aims to integrate GWAS-derived SNPs and coexpression networks to mine candidate genes associated with a particular phenotype. For that, users must define a set of guide genes, which are known genes involved in the studied phenotype. Additionally, the mined candidates can be given a score that favor candidates that are hubs and/or transcription factors. The scores can then be used to rank and select the top n most promising genes for downstream experiments.