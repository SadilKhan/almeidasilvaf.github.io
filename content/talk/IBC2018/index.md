---
title: "Co-expression differential network analysis: CoDiNA"
excerpt: "Presentation about Co-expression differential network analysis at IBC2018."
date: 2018-05-15
location: "Barcelona, Spain"
date_end: "2018-05-15"
show_post_time: false
event: "International Biometric Conference IBC (2018)"
event_url: http://2018.biometricconference.org/
author: "Deisy Morselli Gysi"
location: "Barcelona, Spain"
draft: false
# layout options: single, single-sidebar
layout: single
categories:
- talk
- co-expression analysis
---
  
## XXIX International Biometric Conference

### Background
Biological networks can be represented and analyzed using systems
biology; Gene co-expression networks have been particularly helpful (Barabasi and Oltvai (2004), Bansal et al. (2007), Furlong (2013), Dempsey et al. (2013)) to the understanding of how factors interact. In such networks, nodes represent genes and a weighted link between a pair of genes is often a correlation, where the sign suggests an up- or down-regulation of one factor by another (Dam et al. (2017)), and it is expected that different conditions will lead to different networks for a given system (Berto et al. (2016), Kuntal, Dutta, and Mande (2016)). To identify similarities and differences between system-level activity patterns in different conditions, it is needed to compare multiple networks, derived from different conditions. Most of the times we are interested on the differences of the links and nodes. We propose a method, CoffinD, **Co**-expression **di**fferencial **n**etwork **a**nalysis, implemented in an R package, that can identify links and nodes that are specific, different or common to all networks, and present these relationships through an interactive tool for visualization.

### Method Description
In a set of independent networks, constructed using a correlation method and containing only statistically significant links, each link is classified into $\Phi$ categories; If a link is common to all networks, we call it $\alpha$. It is considered a different link, $\gamma$, if it belongs to all networks but has a weight-sign that differs in at least one network. A link is considered to be specific, $\beta$, if it does not exist in all networks, independent of its sign. Every category has a sub-category, that describes the link into its signs and it has a major importance for the $\beta$ and $\gamma$ links. Along with it, we define a group based on the absence and presence in each network. After that, links are scored to identify the stronger links. We define a link's strength based on the Euclidean distance to the central point. Trivially, links that are closer to the corners will always have a higher distance when compared to the others, all distances are penalized by the maximum theoretical distance a link assume in its category. Category definition of nodes is also important. We can define the $\Phi$ category of a particular node by testing if a node contain links
distributed equally in all categories.

### Conclusion
The CoffinD method, proposed here, is able to categorize both links and nodes into common, specific or different and sub-categories and groups that allow better understanding of the behaviour of those networks. CoDiNA is open source and freely available from CRAN.

References
------
Bansal, Mukesh, Vincenzo Belcastro, Alberto Ambesi-Impiombato, and Diego Di Bernardo. 2007. “How to Infer Gene Networks from Expression Profiles.” *Molecular Systems Biology* 3 (1). EMBO Press: 78.

Barabasi, Albert-Laszlo, and Zoltan N Oltvai. 2004. “Network Biology: Understanding the Cell’s Functional Organization.” *Nature Reviews Genetics* 5 (2). Nature Publishing Group: 101–13.

Berto, Stefano, Alvaro Perdomo-Sabogal, Daniel Gerighausen, Jing Qin, and Katja Nowick. 2016. “A Consensus Network of Gene Regulatory Factors in the Human Frontal Lobe.” *Frontiers in Genetics* 7. Frontiers: 31.

Dam, Sipko van, Urmo Võsa, Adriaan van der Graaf, Lude Franke, and João Pedro de Magalhães. 2017. “Gene Co-Expression Analysis for Functional Classification and Gene–Disease Predictions.” *Briefings in Bioinformatics*. Oxford Univ Press, bbw139.

Dempsey, Kathryn, Ishwor Thapa, Claudia Cortes, Zach Eriksen, Dhundy K Bastola, and Hesham Ali. 2013. “On Mining Biological Signals Using Correlation Networks.” In *Data Mining Workshops (Icdmw), 2013 Ieee 13th International Conference on*, 327–34. IEEE.

Furlong, Laura I. 2013. “Human Diseases Through the Lens of Network Biology.” *Trends in Genetics* 29 (3). Elsevier: 150–59.

Kuntal, Bhusan K., Anirban Dutta, and Sharmila S. Mande. 2016. “CompNet: A Gui Based Tool for Comparison of Multiple Biological Interaction Networks.” *BMC Bioinformatics* 17 (1): 185. doi:[10.1186/s12859-016-1013-x](https://doi.org/10.1186/s12859-016-1013-x).