---
title: Links and other useful turorials
#description: "Give your projects a place to live online."
author: "Deisy Morselli Gysi"
show_post_thumbnail: true
show_author_byline: true
show_post_date: false
# for listing page layout
layout: single # list, list-sidebar, list-grid

# for list-sidebar layout
sidebar: 
  title: A Sidebar for Your Projects
  description: |
    Projects can be anything!
    Check out the _index.md file in the /project folder 
    to edit this content.
  author: "The R Markdown Team @RStudio"
  text_link_label: ""
  text_link_url: ""
  show_sidebar_adunit: false # show ad container

# set up common front matter for all individual pages inside project/
cascade:    
  show_author_byline: true
  show_post_date: true
  show_disqus_comments: false # see disqusShortname in site config
  # for single-sidebar layout only
  sidebar:
    text_link_label: View all projects
    text_link_url: /project/
    show_sidebar_adunit: false # show ad container
---
(Updated: July 20th, 2021)

Here I list some useful tutorials, links and other materials for learning some dataviz, data analysis, statistics, bioinformatics, network science, etc. 

## DataViz 📊

Presenting your results in a visual appealing manner is as important as your analysis. Follows a couple of sources of how to make different visualizations - and mainly - when to use them :smile:.

- [From Data to Viz](https://www.data-to-viz.com/#explore).
	- Shows the most correct way to DataViz 
	- Has examples in R and d3. <3
- [Vega and d3](https://vega.github.io/vega/)
- [Color Scale for diff viz type](https://blog.datawrapper.de/which-color-scale-to-use-in-data-vis/)
- [Data Vis with R - Book](https://rkabacoff.github.io/datavis/)

## Data Science, Statistics and Machine Learning :computer:

Analyse your data, perform the right hypothesis testing, use the right model, learn how to do inference in the right way:

- [Cool book on DS](https://mdsr-book.github.io/mdsr2e/)
	- Not deep, and much more on the application side, instead on the theory. 
- [Stats for Biologists](https://www.nature.com/collections/qghhqm/pointsofsignificance)
	- Quite useful for basic stats
- [Basic DS](https://robust-tools.djnavarro.net/)
	- Basic tools for DS. 
- [Basic Stats and data analysis for biologists](https://deisygysi.github.io/EVOP_2020/)
- [Statistical Inference via Data Science](https://moderndive.com/)
  - A pretty cool and decent book on Inference and DS

- [The amazing book from Susan Holmes](https://web.stanford.edu/class/bios221/book/)
- [A guide to ML - in Portuguese](http://www.rizbicki.ufscar.br/ame/)
- [A guide to Machine Learning & Bioinformatics](https://genomicsclass.github.io/book/)
- [Survival Analysis - I](https://www.nature.com/articles/6601118)
- [Survival Analysis - II](https://www.nature.com/articles/6601119)
- [Survival - Thomase Love's tutorial](https://thomaselove.github.io/432-notes/index.html) 
- [TSA - Time Series Analysis](http://stat565.cwick.co.nz/)
- [GLM - Generalized Linear Models](http://psych.colorado.edu/~carey/qmin/qminChapters/QMIN09-GLMIntro.pdf)
	- A very very gentle intro to GLMs
- [GLM - Generalized Linear Models - In Portuguese](https://www.ime.usp.br/~giapaula/cursosposi.htm)
	- From Prof. Gilberto de Paula.
- [GLM: The paper from Nelder](https://www.jstor.org/stable/2344614?origin=crossref&seq=1#metadata_info_tab_contents)
- [GLMM: Statistical inference in generalized linear mixed models: a review](https://pubmed.ncbi.nlm.nih.gov/17067411/)
- [How to use GLMM in biology (paper) - Generalized linear mixed models: a practical guide for ecology and evolution ](https://pubmed.ncbi.nlm.nih.gov/19185386/)

- [GAMs](https://noamross.github.io/gams-in-r-course/)
	- The best tutorial on GAMs <3
- [GAMLSS](https://www.gamlss.com/)
	- The package webpage. With tons of examples. 
- [Intro to Deep Learning - Video](http://introtodeeplearning.com/)
- [Deep Learning - Book](https://www.deeplearningbook.org/)
- [ML with graphs](https://web.stanford.edu/class/cs224w/index.html#schedule)
- [ML with graphs II](https://snap-stanford.github.io/cs224w-notes/)
- [Graph Representation Learning](https://www.cs.mcgill.ca/~wlh/grl_book/)
- [GNN](https://arxiv.org/pdf/1901.00596.pdf)
- [Tutorials for G(C)NNs](https://zitniklab.hms.harvard.edu/meetings/)
- [TensorFlow tutorial](https://tensorflow.rstudio.com/tutorials/)

## Network Science :spider_web:

Learn Network Science. 
- [Awesome Network](https://github.com/briatte/awesome-network-analysis)
	- Massive repo on many tools for NetSci. 

- [Network Analysis](https://www.jessesadler.com/post/network-analysis-with-r/)
- [Guide to NetVis](https://kateto.net/network-visualization)
- [Interactive vis visNetwork](https://datastorm-open.github.io/visNetwork/)
	- Interactive Networks (useful for shiny)
- [My tutorial for Network Medicine](https://deisygysi.github.io/NetMed_Workshop/)
- [Network Medicine and Gene-Co-expression Networks](https://deisygysi.github.io/teaching/learnNetSci_HTML)
- [How to use igraph](https://sites.fas.harvard.edu/~airoldi/pub/books/BookDraft-CsardiNepuszAiroldi2016.pdf)
- [How to use igraph - II](https://yunranchen.github.io/intro-net-r/igraph.html)

## Bioinformatics :dna:

Bioinformatics basic tools. 

- [Awesome Bioinf - A guide to almost everything in Bioinf](https://github.com/danielecook/Awesome-Bioinformatics#readme)

### NGS :dna:
- [A Gentle Intro to NGS](https://genestack-user-tutorials.readthedocs.io/guide/intro-to-ngs.html)
- [Another intro for NGS](https://github.com/crazyhottommy/RNA-seq-analysis)
- [And... another](https://bioinformaticsworkbook.org/dataAnalysis/RNA-Seq/RNA-SeqIntro/RNAseq-intro#gsc.tab=0)
- [RNAseq Workflow](https://rnaseq.uoregon.edu/)
- [Diff Expression](https://github.com/StefanoBertoUTSW/PEB_BOOK)
	- A Tutorial for PEB2020
- [Michael Love Tutorials](http://mikelove.github.io/pages/teaching.html)
  - Yes, his tutorials are always great. 

### Phylogeny :hatching_chick:
- [How to create trees](https://github.com/mmatschiner/tutorials)
- [MUSCLE webpage](http://www.phylogeny.fr/index.cgi)
	- Tool for creating the aligment & trees
- [Tutorial](https://evomics.org/learning/phylogenetics/) from [EVOLUTION AND GENOMICS](https://evomics.org/)
- [Plot Trees](https://yulab-smu.top/treedata-book/index.html)

## R :computer:

Some brief tutorials to learn R. 

### Tidy* :package:
- [Learn Tidyverse](https://www.tidyverse.org/learn/)
- [Model with Tidy](https://www.tidymodels.org/)
  - All tools for DS
- [R4DS](https://r4ds.had.co.nz/)

### Markdown :writing_hand:
- [Is it your first time using markdown? Check this short tutorial](https://commonmark.org/)
- [Improve the Rmd skills](https://holtzy.github.io/Pimp-my-rmd/)
- [Bookdown](https://www.bookdown.org/)
- [Making your md pretty using Distill](https://iyo-rstudio-global.netlify.app/package/distill/)

### Shiny :three_button_mouse:
Make your results interactive. And shine :sparkles:
- [Mastering Shiny](https://mastering-shiny.org/index.html)
