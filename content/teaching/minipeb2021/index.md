---
title: "Network Analysis for Biological Data"
author: "Deisy Morselli Gysi"
subtitle: A surviving guide
bibliography: book.bib
biblio-style: apalike
link-citations: yes
event: "PEB"
location: "Online"
date: 2021-04-10
layout: single
categories:
- r packages
- co-expression
- networks
- network comparisson
- network medicine
links:
- icon: box-open
  icon_pack: fas
  name: package
  url: https://cran.r-project.org/web/packages/CoDiNA/
---

<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/vis/vis.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/vis/vis.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/visNetwork-binding/visNetwork.js"></script>
<link href="{{< blogdown/postref >}}index_files/ionicons/css/ionicons.min.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/vis/vis.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/vis/vis.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/visNetwork-binding/visNetwork.js"></script>
<link href="{{< blogdown/postref >}}index_files/ionicons/css/ionicons.min.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/vis/vis.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/vis/vis.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/visNetwork-binding/visNetwork.js"></script>
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/kePrint/kePrint.js"></script>
<link href="{{< blogdown/postref >}}index_files/lightable/lightable.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/vis/vis.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/vis/vis.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/visNetwork-binding/visNetwork.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/vis/vis.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/vis/vis.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/visNetwork-binding/visNetwork.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/vis/vis.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/vis/vis.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/visNetwork-binding/visNetwork.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/vis/vis.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/vis/vis.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/visNetwork-binding/visNetwork.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/vis/vis.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/vis/vis.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/visNetwork-binding/visNetwork.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/vis/vis.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/vis/vis.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/visNetwork-binding/visNetwork.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/vis/vis.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/vis/vis.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/visNetwork-binding/visNetwork.js"></script>
<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/vis/vis.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/vis/vis.min.js"></script>
<script src="{{< blogdown/postref >}}index_files/visNetwork-binding/visNetwork.js"></script>

miniPEB 2021: Network Analysis

Link to the course: [learnNetSci.](https://github.com/deisygysi/learnNetSci)

## Network Science: Overview

Network Science is broadly employed in many fields: from understanding *how friends bond in a party* to *how animals interact*; from *how superheroes appear in the same comic books* to *how genes can be related to a specific biological process*. Network analysis is especially beneficial for understanding complex systems, in all research fields. Examples of complex biological or medical systems include gene regulatory, ecological, and neuropsychology networks. In this workshop, the focus is given to applications of Network Science to the Medical Sciences.

Here, I will start by introducing the basic network terminologies and then explore how can we define and identify disease modules, identify disease commorbidities, and lastly, we will learn how to repurpuse drugs for diseases with known modules. For each step, I will then present some classical and some new studies.

It is expected some degree of familiarity with `R`, `ggplot2`, `tidyr`, and `igraph`.

-   How people interact in a party?

<div id="htmlwidget-1" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"nodes":{"id":["A","B","C","D","E","F","G","H","I","J"],"shadow":[true,true,true,true,true,true,true,true,true,true],"shape":["icon","icon","icon","icon","icon","icon","icon","icon","icon","icon"],"icon.face":["Ionicons","Ionicons","Ionicons","Ionicons","Ionicons","Ionicons","Ionicons","Ionicons","Ionicons","Ionicons"],"icon.code":["f3a0","f3c6","f31c","f4d7","f25d","f4d7","f3c6","f3a0","f31c","f3e4"],"icon.color":["#268bd2","#859900","#268bd2","#859900","#d33682","#268bd2","#cb4b16","#b58900","#cb4b16","#2aa198"]},"edges":{"from":["A","C","B","C","A","D","B","A","B","A","F"],"to":["D","H","F","J","I","B","A","E","G","C","C"],"color":["#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36"],"shadow":[true,true,true,true,true,true,true,true,true,true,true]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false},"layout":{"randomSeed":125}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","iconsRedraw":true},"evals":[],"jsHooks":[]}</script>

-   How people interact in a Social Media?

<div id="htmlwidget-2" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-2">{"x":{"nodes":{"id":["A","B","C","D","E","F","G","H","I","J"],"shadow":[true,true,true,true,true,true,true,true,true,true],"shape":["icon","icon","icon","icon","icon","icon","icon","icon","icon","icon"],"icon.face":["Ionicons","Ionicons","Ionicons","Ionicons","Ionicons","Ionicons","Ionicons","Ionicons","Ionicons","Ionicons"],"icon.code":["f350","f350","f350","f350","f350","f350","f350","f350","f350","f350"],"icon.color":["#2aa198","#dc322f","#d33682","#268bd2","#859900","#268bd2","#859900","#d33682","#268bd2","#cb4b16"]},"edges":{"from":["A","C","B","C","A","D","B","A","B","A","F"],"to":["D","H","F","J","I","B","A","E","G","C","C"],"color":["#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36"],"shadow":[true,true,true,true,true,true,true,true,true,true,true]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false},"layout":{"randomSeed":125}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","iconsRedraw":true},"evals":[],"jsHooks":[]}</script>

-   How genes interact?

<div id="htmlwidget-3" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-3">{"x":{"nodes":{"id":["A","B","C","D","E","F","G","H","I","J"],"shadow":[true,true,true,true,true,true,true,true,true,true],"shape":["image","image","image","image","image","image","image","image","image","image"],"image":["https://cdn.onlinewebfonts.com/svg/img_492877.png","https://cdn.onlinewebfonts.com/svg/img_492877.png","https://cdn.onlinewebfonts.com/svg/img_492877.png","https://cdn.onlinewebfonts.com/svg/img_492877.png","https://cdn.onlinewebfonts.com/svg/img_492877.png","https://cdn.onlinewebfonts.com/svg/img_492877.png","https://cdn.onlinewebfonts.com/svg/img_492877.png","https://cdn.onlinewebfonts.com/svg/img_492877.png","https://cdn.onlinewebfonts.com/svg/img_492877.png","https://cdn.onlinewebfonts.com/svg/img_492877.png"],"color":["#b58900","#2aa198","#cb4b16","#dc322f","#6c71c4","#859900","#268bd2","#6c71c4","#cb4b16","#b58900"]},"edges":{"from":["A","C","B","C","A","D","B","A","B","A","F"],"to":["D","H","F","J","I","B","A","E","G","C","C"],"color":["#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36"],"shadow":[true,true,true,true,true,true,true,true,true,true,true]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","shapeProperties":{"useBorderWithImage":true}},"manipulation":{"enabled":false},"layout":{"randomSeed":125}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)"},"evals":[],"jsHooks":[]}</script>

### Why networks are important?

Networks enable us to understand how **interactions** between entities can affect an outcome.

-   How gene **interactions** can be associated with a **disease** or **trait**

-   How genes can be **differentially co-expressed** in a **phenotype**

-   How **drugs** target different proteins and can affect drug response

### Terminology

While the nature of each system, i.e. what its entities are and what kind of interactions they have, is different, there are common notations.

The set of interactions among a set of entities is, in general, called a graph or a network ([Newman 2018](#ref-Newman2018); [Barabási 2016](#ref-Barabasi2016)). In graph theory, each entity is called a vertex, while in network notation it is called a node ([Barabási 2016](#ref-Barabasi2016)). Accordingly, the connections between two entities are called edges or links, respectively ([Barabási 2016](#ref-Barabasi2016)). In this workshop, I will always use the network notation, unless otherwise specified. The total number of nodes in a network is often denoted as **N**, and the number of links in a network is denoted as **L**. While nodes can receive a label, links in general, are not labeled ([Barabási 2016](#ref-Barabasi2016)) (although, in many cases, weights can also be perceived as a label).

A network can be represented mathematically as an adjacency matrix (usually denoted as **A**), an edge-list, or visually as a graph.

-   **Adjacency matrix**:

<table class="table" style="font-size: 20px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">
(\#tab:unnamed-chunk-5)Mathematical Representation of a Network: Adjacency Matrix.
</caption>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
A
</th>
<th style="text-align:right;">
B
</th>
<th style="text-align:right;">
C
</th>
<th style="text-align:right;">
D
</th>
<th style="text-align:right;">
E
</th>
<th style="text-align:right;">
F
</th>
<th style="text-align:right;">
G
</th>
<th style="text-align:right;">
H
</th>
<th style="text-align:right;">
I
</th>
<th style="text-align:right;">
J
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
A
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
B
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
C
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
</tr>
<tr>
<td style="text-align:left;">
D
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
E
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
F
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
G
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
H
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
I
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
<tr>
<td style="text-align:left;">
J
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
<td style="text-align:right;">
0
</td>
</tr>
</tbody>
</table>

-   **Edge list**:

<table class="table" style="font-size: 20px; margin-left: auto; margin-right: auto;">
<caption style="font-size: initial !important;">
(\#tab:unnamed-chunk-6)Mathematical Representation of a Network: Edge List.
</caption>
<thead>
<tr>
<th style="text-align:left;">
from
</th>
<th style="text-align:left;">
to
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
A
</td>
<td style="text-align:left;">
D
</td>
</tr>
<tr>
<td style="text-align:left;">
C
</td>
<td style="text-align:left;">
H
</td>
</tr>
<tr>
<td style="text-align:left;">
B
</td>
<td style="text-align:left;">
F
</td>
</tr>
<tr>
<td style="text-align:left;">
C
</td>
<td style="text-align:left;">
J
</td>
</tr>
<tr>
<td style="text-align:left;">
A
</td>
<td style="text-align:left;">
I
</td>
</tr>
<tr>
<td style="text-align:left;">
D
</td>
<td style="text-align:left;">
B
</td>
</tr>
<tr>
<td style="text-align:left;">
B
</td>
<td style="text-align:left;">
A
</td>
</tr>
<tr>
<td style="text-align:left;">
A
</td>
<td style="text-align:left;">
E
</td>
</tr>
<tr>
<td style="text-align:left;">
B
</td>
<td style="text-align:left;">
G
</td>
</tr>
<tr>
<td style="text-align:left;">
A
</td>
<td style="text-align:left;">
C
</td>
</tr>
<tr>
<td style="text-align:left;">
F
</td>
<td style="text-align:left;">
C
</td>
</tr>
</tbody>
</table>

-   A **network** is a pair **G = (N, L)** of a set **N** of nodes connected by a set **L** of links.

-   Two nodes are neighbors if they are **connected**.

-   The **degree** (d) of a node is the **number of nodes** it interacts with.

-   Nodes with high degree are called **hubs**.

-   The **strength** of a node is the **sum of the weights** attached to links belonging to a node.

<div id="htmlwidget-4" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-4">{"x":{"nodes":{"ID":["A","B","C","D","E","F","G","H","I","J"],"n":[5,4,4,2,1,2,1,1,1,1],"label":["5","4","4","2","1","2","1","1","1","1"],"value":[5,4,4,2,1,2,1,1,1,1],"id":["A","B","C","D","E","F","G","H","I","J"],"color":["#b58900","#2aa198","#cb4b16","#dc322f","#6c71c4","#859900","#268bd2","#6c71c4","#cb4b16","#b58900"],"shape":["dot","dot","dot","dot","dot","dot","dot","dot","dot","dot"]},"edges":{"from":["A","C","B","C","A","D","B","A","B","A","F"],"to":["D","H","F","J","I","B","A","E","G","C","C"],"color":["#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36"],"shadow":[true,true,true,true,true,true,true,true,true,true,true]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false},"layout":{"randomSeed":125}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","highlight":{"enabled":true,"hoverNearest":false,"degree":1,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}</script>

-   Networks come in all sorts of flavors. They can have **weights** and/ or **directions** on their edges.

<div id="htmlwidget-5" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-5">{"x":{"nodes":{"id":["A","B","C","D","E","F","G","H","I","J"],"shadow":[true,true,true,true,true,true,true,true,true,true],"color":["#b58900","#2aa198","#cb4b16","#dc322f","#6c71c4","#859900","#268bd2","#6c71c4","#cb4b16","#b58900"]},"edges":{"from":["A","C","B","C","A","D","B","A","B","A","F"],"to":["D","H","F","J","I","B","A","E","G","C","C"],"color":["#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36"],"shadow":[true,true,true,true,true,true,true,true,true,true,true],"width":[7.94536185567267,8.71165321953595,7.82297176774591,8.65271824575029,4.6836699063424,1.49515287275426,6.20310658286326,7.70546675147489,8.97682183003053,1.27709910972044,6.62396708549932]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false},"layout":{"randomSeed":125}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)"},"evals":[],"jsHooks":[]}</script>
<div id="htmlwidget-6" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-6">{"x":{"nodes":{"id":["A","B","C","D","E","F","G","H","I","J"],"shadow":[true,true,true,true,true,true,true,true,true,true],"color":["#b58900","#2aa198","#cb4b16","#dc322f","#6c71c4","#859900","#268bd2","#6c71c4","#cb4b16","#b58900"]},"edges":{"from":["A","C","B","C","A","D","B","A","B","A","F"],"to":["D","H","F","J","I","B","A","E","G","C","C"],"color":["#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36","#002b36"],"shadow":[true,true,true,true,true,true,true,true,true,true,true],"arrows":["to","to","to","to","to","to","to","to","to","to","to"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false},"layout":{"randomSeed":125}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)"},"evals":[],"jsHooks":[]}</script>

-   **Two nodes are connected** in a network, if a sequence of adjacent nodes, a **path**, connects them.

-   The **shortest path** length is the number of links along the shortest path connecting two nodes.

-   The **average path length** is the average of the shortest paths between all pairs of nodes.

<div id="htmlwidget-7" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-7">{"x":{"nodes":{"ID":["A","B","C","D","E","F","G","H","I","J"],"n":[5,4,4,2,1,2,1,1,1,1],"label":["A","B","C","D","E","F","G","H","I","J"],"value":[5,4,4,2,1,2,1,1,1,1],"id":["A","B","C","D","E","F","G","H","I","J"],"color":["#b58900","#2aa198","#cb4b16","#dc322f","#6c71c4","#859900","#268bd2","#6c71c4","#cb4b16","#b58900"],"shape":["dot","dot","dot","dot","dot","dot","dot","dot","dot","dot"]},"edges":{"from":["A","C","B","C","A","D","B","A","B","A","F"],"to":["D","H","F","J","I","B","A","E","G","C","C"],"color":["#fdf6e3","#fdf6e3","#fdf6e3","#d33682","#fdf6e3","#fdf6e3","#fdf6e3","#fdf6e3","#fdf6e3","#d33682","#fdf6e3"],"shadow":[true,true,true,true,true,true,true,true,true,true,true]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false},"layout":{"randomSeed":125}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","highlight":{"enabled":true,"hoverNearest":false,"degree":1,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}</script>
<div id="htmlwidget-8" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-8">{"x":{"nodes":{"ID":["A","B","C","D","E","F","G","H","I","J"],"n":[5,4,4,2,1,2,1,1,1,1],"label":["A","B","C","D","E","F","G","H","I","J"],"value":[5,4,4,2,1,2,1,1,1,1],"id":["A","B","C","D","E","F","G","H","I","J"],"color":["#b58900","#2aa198","#cb4b16","#dc322f","#6c71c4","#859900","#268bd2","#6c71c4","#cb4b16","#b58900"],"shape":["dot","dot","dot","dot","dot","dot","dot","dot","dot","dot"]},"edges":{"from":["A","C","B","C","A","D","B","A","B","A","F"],"to":["D","H","F","J","I","B","A","E","G","C","C"],"color":["#fdf6e3","#fdf6e3","#2aa198","#fdf6e3","#fdf6e3","#fdf6e3","#d33682","#fdf6e3","#fdf6e3","#d33682","#2aa198"],"shadow":[true,true,true,true,true,true,true,true,true,true,true]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false},"layout":{"randomSeed":125}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","highlight":{"enabled":true,"hoverNearest":false,"degree":1,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}</script>

-   An **induced subgraph** is a subgraph that contains a set of “defined” nodes.

## Data Commonly Used in Network Medicine

In Network Medicine, we are often interested in understanding *how genes associated to a particular disease can influence each other*, *how two diseases are similar (or different)*, *and how a drug can be used in different set-ups.*

For that, it is necessary to use data sets that are able to represent those associations: **Protein-Protein Interactions** are used as a map of the interactions inside our cells; **Gene-Disease-Associations** are used for us to identify genes that were previously associated to diseases, often using a GWAS approach.

### Protein-Protein Interaction Networks

In PPI networks, the nodes represent proteins, and they are connected by a link if they physically interact with each other ([Rual et al. 2005](#ref-rual2005)). Typically, these interactions are measured experimentally, for instance, with the Yeast-Two-Hybrid (Y2H) system ([Uetz et al. 2000](#ref-uetz2000)), or by protein complex immunoprecipitation followed by high-throughput Mass Spectrometry ([Zhang et al. 2008](#ref-zhang2008); [Koh et al. 2012](#ref-koh2012)), or inferred computationally based on sequence similarity ([Fong, Keating, and Singh 2004](#ref-fong2004)). PPI can be used to infer gene functions and the association of sub-networks to diseases ([Menche et al. 2015](#ref-Menche2015)). In this type of network, a highly connected protein tends to interact with proteins that are less connected, probably to prevent unwanted cross-talk of functional modules. Most of the methods in network medicine are based on PPI.

#### Measuring PPIs

Protein-Protein Interactions can be measured mainly using three different techniques:

1.  By the creation of Protein-Protein interaction maps derived from existing scientific literature;

2.  Using computational predictions of PPIs based on available orthogonal information; and

3.  By systematic experimental mapping of proteins identify complex association and/or binary interactions.

#### Commonly used data sources for PPIs

PPIs can be found from different sources. I list here some well-known databases for that.

1.  Binary PPIs derived from high-throughput yeast-two hybrid (Y2H) experiments;
2.  Binary PPIs three-dimensional (3D) protein structures;
3.  Binary PPIs literature curation;
4.  PPIs identified by affinity purification followed by mass spectrometry;
5.  Kinase substrate interactions;
6.  Signaling interactions;
7.  Regulatory interactions.

#### Understanding a PPI

For this workshop, we will be using for this workshop is a combination of a manually curated PPI that combines all previous data sets. The data can be [found here](https://github.com/deisygysi/learnNetSci/blob/main/inst/tutorials/PEB2021/data/PPI_Symbol_Entrez.csv). This PPI was previously published in [D. M. Gysi et al.](#ref-Gysi2020a) ([2020](#ref-Gysi2020a)).

Before we can start any analysis using this interactome, let us first understand this data.

The PPI contains the EntrezID and the HGNC symbol of each gene, and some might not have a proper map. Therefore, it should be removed from further analysis. Moreover, we might have loops, and those should also be removed.

Let us begin by preparing our environment and calling all libraries we will need at this point.

``` r
require(data.table)
require(tidyr)
require(igraph)
require(dplyr)
require(magrittr)
require(ggplot2)
```

Let’s read in our data.

``` r
PPI = fread("./data/PPI_Symbol_Entrez.csv")

head(PPI)
```

    ##    GeneA_ID GeneB_ID Symbol_A Symbol_B
    ## 1:     9796    56992   PHYHIP    KIF15
    ## 2:     7918     9240   GPANK1    PNMA1
    ## 3:     8233    23548    ZRSR2    TTC33
    ## 4:     4899    11253     NRF1   MAN1B1
    ## 5:     5297     8601    PI4KA    RGS20
    ## 6:     6564     8933  SLC15A1    RTL8C

Let’s transform our edge-list into a network.

How many genes do we have? How many interactions?

    ## IGRAPH 55e04a2 UN-- 18507 322289 -- 
    ## + attr: name (v/c)
    ## + edges from 55e04a2 (vertex names):
    ##  [1] PHYHIP--TTR      PHYHIP--NFE2     PHYHIP--DYRK1A   PHYHIP--HNRNPA1 
    ##  [5] PHYHIP--COPS6    PHYHIP--SUPT5H   PHYHIP--SMARCC2  PHYHIP--EEF1A1  
    ##  [9] PHYHIP--TRIP6    PHYHIP--NDUFV3   PHYHIP--CA10     PHYHIP--ERG28   
    ## [13] PHYHIP--S100A13  PHYHIP--PPIE     PHYHIP--LIMD1    PHYHIP--ANKRD12 
    ## [17] PHYHIP--ZZEF1    PHYHIP--PRMT5    PHYHIP--KIF15    PHYHIP--MED8    
    ## [21] PHYHIP--PRKD2    PHYHIP--PAQR5    PHYHIP--MAGED4B  PHYHIP--NDRG1   
    ## [25] PHYHIP--PTRH2    PHYHIP--HDAC11   PHYHIP--METTL18  PHYHIP--PNPLA2  
    ## [29] PHYHIP--TMEM255B PHYHIP--WDR89    PHYHIP--FAM131A  GPANK1--TAF1    
    ## + ... omitted several edges

Next, let’s check the degree distribution:

``` r
dd = degree(gPPI) %>% 
  table() %>% 
  as.data.frame()

names(dd) = c('Degree', "Nodes")
dd$Degree %<>% as.character %>% as.numeric()
dd$Nodes  %<>% as.character %>% as.numeric()

ggplot(dd) +
  aes(x = Degree, y = Nodes) +
  geom_point(colour = "#1d3557") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  theme_minimal()
```

    ## Warning: Transformation introduced infinite values in continuous x-axis

<div class="figure">

<img src="{{< blogdown/postref >}}index_files/figure-html/degree-distribution-1.png" alt="PPI Degree Distribution." width="672" />
<p class="caption">
Figure 1: PPI Degree Distribution.
</p>

</div>

Most of the proteins have few connections, and very few proteins have lots of connections. Who’s that protein?

    ##          .
    ## UBC   5199
    ## ETS1  1496
    ## GATA2 1369
    ## CTCF  1361
    ## EP300 1124
    ## MYC   1107
    ## AR    1099

#### Exercises

Now is your turn. Spend some minutes understanding the data and getting some familiarity with it.

1.  What are the top 10 genes with the highest degree?

2.  Are those genes connected?

<!-- -->

    ## IGRAPH a46ad50 UN-- 10 24 -- 
    ## + attr: name (v/c)
    ## + edges from a46ad50 (vertex names):
    ##  [1] MYC  --CTCF  EGR1 --ETS1  EGR1 --EP300 MYC  --EP300 ETS1 --EP300
    ##  [6] EGR1 --UBC   MYC  --UBC   CTCF --UBC   ETS1 --UBC   EP300--UBC  
    ## [11] EGR1 --AR    MYC  --AR    ETS1 --AR    EP300--AR    UBC  --AR   
    ## [16] EGR1 --GATA2 MYC  --GATA2 ETS1 --GATA2 EP300--GATA2 CTCF --APP  
    ## [21] UBC  --APP   MYC  --RAD21 CTCF --RAD21 UBC  --RAD21

### Gene Disease Association

A Gene-Disease-Association (GDA) database are typically used to understand the association of genes to diseases, and model the underlying mechanisms of complex diseases. Those associations often come from GWAS studies and knock-out studies.

#### Commonly used data sources for GDAs

As PPIs, GDAs can be found from different sources and with different evidences for each Gene-Disease association. I list here some well-known databases for that.

-   CTD – Curated scientific literature ([Davis et al. 2020](#ref-davis2020))

-   OMIM – Curated scientific literature ([McKusick 2007](#ref-mckusick2007))

-   DisGeNet – Based on OMIM, ClinVar, and other data bases ([Piñero et al. 2019](#ref-piñero2019))

-   Orphanet – Validated - and non-validated - GDAs

-   ClinGen – Validated - and non-validated - GDAs ([Rehm et al. 2015](#ref-rehm2015))

-   ClinVar – Different levels of evidence ([Landrum et al. 2019](#ref-landrum2019))

-   GWAS catalogue – GWAS associations to diseases ([Buniello et al. 2018](#ref-buniello2018))

-   PheGenI – GWAS associations to diseases ([Ramos et al. 2013](#ref-ramos2013))

-   lncRNADisease – Experimentally validated lncRNAs in diseases ([Chen et al. 2012](#ref-chen2012))

-   HMDD – Experimentally validated miRNAs in diseases ([Huang et al. 2018](#ref-huang2018))

#### Understanding a GDA dataset

We will use in this workshop Gene-Disease-Association from DisGeNet. It can be [found here](https://github.com/deisygysi/NetMed_Workshop/blob/master/data/curated_gene_disease_associations.tsv).

Similar to the PPI, let us first get some familiarity with the data, before performing any analysis.

Let’s read in the data and, again, do some basic statistics.

``` r
GDA = fread(file = './data/curated_gene_disease_associations.tsv', sep = '\t')

head(GDA)
```

    ##    geneId geneSymbol   DSI   DPI diseaseId              diseaseName diseaseType
    ## 1:      1       A1BG 0.700 0.538  C0019209             Hepatomegaly   phenotype
    ## 2:      1       A1BG 0.700 0.538  C0036341            Schizophrenia     disease
    ## 3:      2        A2M 0.529 0.769  C0002395      Alzheimer's Disease     disease
    ## 4:      2        A2M 0.529 0.769  C0007102 Malignant tumor of colon     disease
    ## 5:      2        A2M 0.529 0.769  C0009375        Colonic Neoplasms       group
    ## 6:      2        A2M 0.529 0.769  C0011265       Presenile dementia     disease
    ##    diseaseClass              diseaseSemanticType score    EI YearInitial
    ## 1:      C23;C06                          Finding  0.30 1.000        2017
    ## 2:          F03 Mental or Behavioral Dysfunction  0.30 1.000        2015
    ## 3:      C10;F03              Disease or Syndrome  0.50 0.769        1998
    ## 4:      C06;C04               Neoplastic Process  0.31 1.000        2004
    ## 5:      C06;C04               Neoplastic Process  0.30 1.000        2004
    ## 6:      C10;F03 Mental or Behavioral Dysfunction  0.30 1.000        1998
    ##    YearFinal NofPmids NofSnps    source
    ## 1:      2017        1       0 CTD_human
    ## 2:      2015        1       0 CTD_human
    ## 3:      2018        3       0 CTD_human
    ## 4:      2019        1       0 CTD_human
    ## 5:      2004        1       0 CTD_human
    ## 6:      2004        3       0 CTD_human

The first thing to notice is the inconsistency with the disease names, in order to be able to work with it, let’s first put every disease to lower-case.

Let’s also understand the degree distribution of the diseases.

``` r
numGenes = Cleaned_GDA %>% 
  group_by(diseaseName) %>%
  summarise(numGenes = n()) %>%
  ungroup() %>%
  group_by(numGenes) %>%
  summarise(numDiseases = n())
```

``` r
numGenes = Cleaned_GDA %>% 
  group_by(diseaseName) %>%
  summarise(numGenes = n()) %>%
  ungroup() %>%
  group_by(numGenes) %>%
  summarise(numDiseases = n())

ggplot(numGenes) +
  aes(x = numGenes, y = numDiseases) +
  geom_point(colour = "#1d3557") +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  labs(x = "Genes", y = "Diseases")+
  theme_minimal()
```

<div class="figure">

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-15-1.png" alt="Gene-Disease degree distribution." width="672" />
<p class="caption">
Figure 2: Gene-Disease degree distribution.
</p>

</div>

Because we want to focus in well studied diseases, and also that are known to be complex diseases, let’s filter for diseases with at least 10 genes.

``` r
Cleaned_GDA %<>% 
  group_by(diseaseName) %>%
  mutate(numGenes = n()) %>%
  filter(numGenes > 10)

Cleaned_GDA$diseaseName %>%
  unique() %>%  
  length()
```

    ## [1] 920

#### Exercises

Now is your turn. Spend some minutes understanding the data and getting some familiarity with it.

1.  What are the top 10 genes mostly involved with diseases? What are those diseases?

<!-- -->

    ## # A tibble: 10 x 3
    ##    geneSymbol numDis index
    ##    <chr>       <int> <int>
    ##  1 TNF           177     1
    ##  2 IL6           144     2
    ##  3 SOD2          144     3
    ##  4 TP53          138     4
    ##  5 PTGS2         128     5
    ##  6 MTHFR         117     6
    ##  7 IL1B          115     7
    ##  8 PTEN          105     8
    ##  9 TGFB1         105     9
    ## 10 KRAS          100    10

2.  What are the top 10 highly polygenic diseases?

<!-- -->

    ## # A tibble: 10 x 3
    ##    diseaseName                    numGenes index
    ##    <chr>                             <int> <int>
    ##  1 malignant neoplasm of breast       1074     1
    ##  2 schizophrenia                       883     2
    ##  3 liver cirrhosis, experimental       774     3
    ##  4 colorectal carcinoma                702     4
    ##  5 malignant neoplasm of prostate      616     5
    ##  6 breast carcinoma                    538     6
    ##  7 mammary carcinoma, human            525     7
    ##  8 mammary neoplasms, human            525     8
    ##  9 liver carcinoma                     507     9
    ## 10 bipolar disorder                    477    10

3.  What are the top 10 highly polygenic disease classes?

<!-- -->

    ## # A tibble: 10 x 3
    ##    diseaseSemanticType                               numGenes index
    ##    <chr>                                                <int> <int>
    ##  1 Disease or Syndrome                                  18033     1
    ##  2 Neoplastic Process                                   16163     2
    ##  3 Mental or Behavioral Dysfunction                      6015     3
    ##  4 Congenital Abnormality                                1325     4
    ##  5 Experimental Model of Disease                         1300     5
    ##  6 Injury or Poisoning                                   1027     6
    ##  7 Neoplastic Process; Experimental Model of Disease      330     7
    ##  8 Acquired Abnormality                                   106     8
    ##  9 Pathologic Function                                     72     9
    ## 10 Disease or Syndrome; Congenital Abnormality             68    10

## Methods for Disease Module Identification and Disease Similarity

In this chapter, I will introduce the main methods used in *Network Medicine*. We will start by understanding what a *Disease Module* is, how we can calculate its significance, and also understand its importance. Next, we will explore the *disease separation*, how to calculate, and make interpretations.

### Disease Module

In biological networks, genes are often involved in the same topological communities are also associated with similar biological processes ([Ahn, Bagrow, and Lehmann 2010](#ref-Ahn2010)). It also reflects on *how diseases localized themselves in the interaction*; meaning that, disease modules are highly localized in specific network neighborhoods ([Menche et al. 2015](#ref-Menche2015)).

#### Largest connected component

The size of the **largest connected component** (LCC) is the **number of nodes that form a connected subgraph** (in our case, it is the number of proteins that are interconnected in the PPI). Many properties of this quantity allow us to understand how a particular disease interacts with the interactome. It is important to note here that this measure is highly dependent on the completeness of an interactome. If a link between a protein and its counterparts is unknown – therefore missing – we might say that that particular node is not involved in a disease module (or that the LCC is not significant).

<div class="figure">

<div id="htmlwidget-9" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-9">{"x":{"nodes":{"id":["A","B","C","E","D","H","N","K","G","J","O","I","M","L"],"color":["#FFCDB2","#B65064","#B65064","#FFCDB2","#FFCDB2","#FFCDB2","#B65064","#B65064","#FFCDB2","#FFCDB2","#FFCDB2","#FFCDB2","#FFCDB2","#FFCDB2"],"size":[35,30,35,20,30,10,15,15,15,10,10,15,10,10],"font.color":["#B5838D","#B5838D","#B5838D","#B5838D","#B5838D","#B5838D","#B5838D","#B5838D","#B5838D","#B5838D","#B5838D","#B5838D","#B5838D","#B5838D"],"frame.color":["#FFCDB2","#B65064","#B65064","#FFCDB2","#FFCDB2","#FFCDB2","#B65064","#B65064","#FFCDB2","#FFCDB2","#FFCDB2","#FFCDB2","#FFCDB2","#FFCDB2"],"label":["A","B","C","E","D","H","N","K","G","J","O","I","M","L"],"x":[0.224501448688913,-0.623589346205697,-0.0875383204150721,-0.371890959337147,0.417875132936696,0.474841523622248,-0.558135282992741,-0.279498623630003,-0.840033136534571,-1,0.0198355380840074,0.0635557447899178,0.785179363946149,1],"y":[-0.509859506091702,0.578727185391074,-0.0253022182222237,-0.307403113543846,0.0679567897021223,-0.994009886551226,0.250197745106479,0.436394365194536,0.0581591831555206,1,-1,0.542063375522048,-0.721592914346396,0.234498868523327]},"edges":{"from":["A","A","A","A","A","A","B","B","B","B","B","C","C","C","C","E","D","D"],"to":["C","E","D","H","O","M","N","K","G","J","I","E","D","N","K","G","I","L"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)"},"evals":[],"jsHooks":[]}</script>
<p class="caption">
Figure 3: Disease-Module. In a schematic of a PPI, in pink, we see genes associated with a disease, forming a connected component of size 4.
</p>

</div>

However, just computing this number might not be informative, and it is expected a randomness. To calculate this randomness, we often calculate the significance of the LCC by selecting proteins in the interactome with similar degrees (aka degree preserving randomization).

To calculate the significance of the LCC, one can calculate its Z-Score or simply calculate the empirical probability under the curve from the empirical distribution. The Z-score is given by:

$$
Z-Score_{LCC} = \frac{LCC - \mu_{LCC}}{\sigma_{LCC}}.
$$

#### Example in real data

Our first task now is to understand if some diseases, from our `Cleaned_GDA` are able to form a Disease-Module. Let’s start doing it for Schizophrenia and later we will add some more diseases.

The idea now is: Gather the genes associated to our disease in the data, find them in the PPI, check if they form a connected component, check the significance of the component and visualize the Disease-Module.

``` r
# First, let's attach all packages we will need.
require(NetSci)
require(magrittr)
require(dplyr)
require(igraph)
```

``` r
#First, let's select genes that are associated with Schizophrenia.

SCZ_Genes = 
  Cleaned_GDA %>% 
  filter(diseaseName %in% 'schizophrenia') %>%
  pull(geneSymbol) %>% 
  unique()

# Next, let's see how they are localized in the PPI.
# Fist, we have to make sure all genes are in the PPI.
# Later, we calculate the LCC.
# And lastly, let's visualize it.

SCZ_PPI = SCZ_Genes[SCZ_Genes %in% V(gPPI)$name]
gScz = gPPI %>%
  induced.subgraph(., SCZ_PPI)
```

``` r
components(gScz)$csize %>% max
```

    ## [1] 683

The size of the LCC is 683. However, how does it compare to a random selection genes?

``` r
LCC_scz = LCC_Significance(N = 100, 
                           Targets = SCZ_PPI,
                           G = gPPI)
Histogram_LCC(LCC_scz)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/LCC-1.png" width="672" />

``` r
gScz 
```

    ## IGRAPH 67e539f UN-- 846 2564 -- 
    ## + attr: name (v/c)
    ## + edges from 67e539f (vertex names):
    ##  [1] PI4KA  --SP1    F2     --SP1    DNM1   --CTCF   GSK3B  --HSPA1A
    ##  [5] DNM1   --GRB2   SP1    --GRB2   MET    --GRB2   GSK3B  --MAPK14
    ##  [9] SP1    --MAPK14 CTCF   --MAPK14 GRB2   --MAPK14 MET    --ACTB  
    ## [13] GRB2   --ACTB   MAPK14 --ACTB   GSK3B  --SOX10  SP1    --SOX10 
    ## [17] PAX6   --SOX10  SP1    --CCNA2  ACTB   --MTNR1A ACTB   --GSN   
    ## [21] PI4KA  --JUN    GSK3B  --JUN    SMARCA2--JUN    SP1    --JUN   
    ## [25] MAPK14 --JUN    SOX10  --JUN    GSK3B  --ESR1   SMARCA2--ESR1  
    ## [29] SP1    --ESR1   HSPA1A --ESR1   FMR1   --ESR1   MAPK14 --ESR1  
    ## + ... omitted several edges

``` r
V(gScz)$size = degree(gScz) %>% 
  CoDiNA::normalize()
V(gScz)$size = (V(gScz)$size + 0.1)*5
V(gScz)$color = '#83c5be'
V(gScz)$frame.color = '#006d77'
V(gScz)$label = ifelse(V(gScz)$size  > 4, V(gScz)$name, NA )
V(gScz)$label.color = '#e29578'

E(gScz)$width = edge.betweenness(gScz, directed = F) %>% CoDiNA::normalize()
E(gScz)$width = E(gScz)$width + 0.01
E(gScz)$weight = E(gScz)$width
par(mar = c(0,0,0,0))
plot(gScz)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/SCZplot-1.png" width="672" />

Let’s remove all genes with degree = 0. (Genes that do not connect to any other gene).

``` r
gScz %<>% delete.vertices(., degree(.) == 0)

V(gScz)$size = degree(gScz) %>% 
  CoDiNA::normalize()
V(gScz)$size = (V(gScz)$size + 0.1)*5
V(gScz)$color = '#83c5be'
V(gScz)$frame.color = '#006d77'
V(gScz)$label = ifelse(V(gScz)$size  > 4, V(gScz)$name, NA )
V(gScz)$label.color = '#e29578'

E(gScz)$width = edge.betweenness(gScz, directed = F) %>% CoDiNA::normalize()
E(gScz)$width = E(gScz)$width + 0.01
E(gScz)$weight = E(gScz)$width
par(mar = c(0,0,0,0))
plot(gScz)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-19-1.png" width="672" />

#### Exercises

1.  Calculate the LCC, and visualize the modules for the following diseases:

-   Autistic Disorder;

<img src="{{< blogdown/postref >}}index_files/figure-html/LCC-autism-solution-1.png" width="672" /><img src="{{< blogdown/postref >}}index_files/figure-html/LCC-autism-solution-2.png" width="672" />

-   Bipolar Disorder;

-   Major Depressive Disorder;

-   Rheumatoid Arthritis;

-   Asthma

-   Parkinson Disease

-   Endometriosis

### Gene Overlap

A first intuitive way to measure the overlap of two gene sets is by calculating its overlap, or its normalized overlap, the **Jaccard Index**. The Jaccard index is calculated by taking the ratio of **Intersection of two sets over the Union of those sets**. The Jaccard coefficient measures similarity between finite sample sets, and is defined as the size of the intersection divided by the size of the union of the sample sets:

$$
J(A,B) = \frac{|A \cap B|}{|A \cup B|} = \frac{|A \cap B|}{|A| + |B| - |A \cap B|}.
$$

Note that by design, `\(0 \leq J(A,B) \leq 1\)`. If A and B are both empty, define `\(J(A,B) = 1\)`.

Let’s calculate the Jaccard Index for the diseases we calculated its LCCs.

``` r
Dis_Ex1 = c('schizophrenia', 
            'autistic disorder',
            'bipolar disorder',
            "major depressive disorder",
            'asthma', 
            'rheumatoid arthritis',
            'parkinson disease',
            'endometriosis')
GDA_Interest = Cleaned_GDA %>% 
  filter(diseaseName %in% Dis_Ex1) %>%
  select(diseaseName, geneSymbol) %>%
  unique()

Jaccard_Ex2 = Jaccard(GDA_Interest)
```

    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |=========                                                             |  12%
      |                                                                            
      |==================                                                    |  25%
      |                                                                            
      |==========================                                            |  38%
      |                                                                            
      |===================================                                   |  50%
      |                                                                            
      |============================================                          |  62%
      |                                                                            
      |====================================================                  |  75%
      |                                                                            
      |=============================================================         |  88%
      |                                                                            
      |======================================================================| 100%

``` r
Jaccard_Ex2
```

    ##                        Node.1                    Node.2 Jaccard.Index
    ##  1:             schizophrenia         autistic disorder    0.09578544
    ##  2:             schizophrenia          bipolar disorder    0.21428571
    ##  3:         autistic disorder          bipolar disorder    0.09658247
    ##  4:             schizophrenia major depressive disorder    0.11817279
    ##  5:         autistic disorder major depressive disorder    0.08154506
    ##  6:          bipolar disorder major depressive disorder    0.16316640
    ##  7:             schizophrenia             endometriosis    0.01655307
    ##  8:         autistic disorder             endometriosis    0.02676399
    ##  9:          bipolar disorder             endometriosis    0.02243590
    ## 10: major depressive disorder             endometriosis    0.03589744
    ## 11:             schizophrenia         parkinson disease    0.03308431
    ## 12:         autistic disorder         parkinson disease    0.03903904
    ## 13:          bipolar disorder         parkinson disease    0.03690037
    ## 14: major depressive disorder         parkinson disease    0.06493506
    ## 15:             endometriosis         parkinson disease    0.01652893
    ## 16:             schizophrenia                    asthma    0.02337938
    ## 17:         autistic disorder                    asthma    0.04281346
    ## 18:          bipolar disorder                    asthma    0.03148148
    ## 19: major depressive disorder                    asthma    0.02866242
    ## 20:             endometriosis                    asthma    0.02118644
    ## 21:         parkinson disease                    asthma    0.05095541
    ## 22:             schizophrenia      rheumatoid arthritis    0.02721088
    ## 23:         autistic disorder      rheumatoid arthritis    0.03571429
    ## 24:          bipolar disorder      rheumatoid arthritis    0.03169572
    ## 25: major depressive disorder      rheumatoid arthritis    0.04511278
    ## 26:             endometriosis      rheumatoid arthritis    0.02446483
    ## 27:         parkinson disease      rheumatoid arthritis    0.02777778
    ## 28:                    asthma      rheumatoid arthritis    0.04958678
    ##                        Node.1                    Node.2 Jaccard.Index

``` r
# Let's visualize the Venn diagram (Euler Diagram) of those overlaps. 

require(eulerr)
```

    ## Loading required package: eulerr

``` r
Euler_List = list (
  SCZ = GDA_Interest$geneSymbol[GDA_Interest$diseaseName == 'schizophrenia'],
  
  ASD = GDA_Interest$geneSymbol[GDA_Interest$diseaseName == 'autistic disorder'],
  
  BD = GDA_Interest$geneSymbol[GDA_Interest$diseaseName == 'bipolar disorder'],
  
  MDD = GDA_Interest$geneSymbol[GDA_Interest$diseaseName == 'major depressive disorder'],
  
  AT = GDA_Interest$geneSymbol[GDA_Interest$diseaseName == 'asthma'],
  
  RA = GDA_Interest$geneSymbol[GDA_Interest$diseaseName == 'rheumatoid arthritis'],
  
  ED = GDA_Interest$geneSymbol[GDA_Interest$diseaseName == 'endometriosis'],
  
  PD = GDA_Interest$geneSymbol[GDA_Interest$diseaseName == 'parkinson disease'])

EULER = euler(Euler_List)
plot(EULER, quantities = TRUE)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-21-1.png" width="672" />

### Disease Separation

When looking into the Jaccard Index, we have a sense of how similar two diseases are based on genes that are **known** to be associated with both diseases. The main problem with this is that we assume that all genes associated with a disease is known, and we do not take the topology of the underlying network into account.

The **separation** is a complementary quantity that is a bit less sensitive to the incompleteness of the PPI, we can measure the distances `\(d_s\)` of each disease-associated node to all other disease associated nodes. Taking into account only the shortest distance between them results in a distribution `\(P(d_s)\)`. The mean value `\(<d_s>\)` can be interpreted as the diameter of the disease model. **Note** the diameter here is the average distance instead of the maximal distance.

The **concept of network localization** can be further generalized to examine the relationship between any different sets of nodes, for example, proteins associated with two different diseases. The network serves as a **map**, where diseases are represented by different neighborhoods. How close and the degree of overlap of two network neighborhoods can be found to be highly predictive of the pathological similarity of those diseases ([Menche et al. 2015](#ref-Menche2015)).

To quantify the distance of two sets of nodes A and B, we first compute the distribution `\(P(d_{AB})\)` of all shortest distances `\(d_{AB}\)` between nodes A and B and the respective mean distance `\(<d_{AB}>\)`.

The network based separation `\(S_{AB}\)` can be obtained by comparing the mean shortest distance **within** the respective node sets and the mean shortest distance **between** them.

$$
S_{AB} = <d_{AB}> - \frac{<d_{AA}> + <d_BB>}{2}.
$$

**Note**: negative `\(S_{AB}\)` indicates topological overlap of the two node sets, while a positive `\(S_{AB}\)` indicates a topological separation of the two node sets.

The size of the overlap is highly predictive of pathological and functional similarity, elevated co-expression, symptoms similarity, and high comorbidity diseases.

<div class="figure">

<div id="htmlwidget-10" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-10">{"x":{"nodes":{"id":["A","B","C","E","F","D","G","J","O","I","M","K","N","L"],"color":["#74c69d","#FFCDB2","#B65064","#B65064","#FFCDB2","#FFCDB2","#74c69d","#FFCDB2","#B65064","#FFCDB2","#74c69d","#B65064","#B65064","#74c69d"],"size":[30,40,40,20,25,35,15,10,10,15,15,10,20,25],"font.color":["#2d6a4f","#B5838D","gray90","gray90","#B5838D","#B5838D","#2d6a4f","#B5838D","gray90","#B5838D","#2d6a4f","gray90","gray90","#2d6a4f"],"frame.color":["#74c69d","#FFCDB2","#B65064","#B65064","#FFCDB2","#FFCDB2","#74c69d","#FFCDB2","#B65064","#FFCDB2","#74c69d","#B65064","#B65064","#74c69d"],"label":["A","B","C","E","F","D","G","J","O","I","M","K","N","L"],"x":[0.154599777370275,0.432915856519429,-0.424600198418028,-0.170990433415739,-0.00494148830336016,-0.0485818075491187,-0.115600455244283,1,-1,0.399142732632487,0.681202830742508,-0.766591820377637,0.111160129549833,-0.311381868079331],"y":[-0.573790336885975,-0.115729257808543,0.336543029653178,0.507627987431501,0.00126928351315514,-0.26157032570764,-1,0.12674666973888,0.428538177840329,-0.65187339181175,-0.657322680587319,1,0.539076602718396,-0.2762631575825]},"edges":{"from":["A","A","A","A","A","B","B","B","B","B","B","C","C","C","C","C","C","C","E","E","F","D","D","D"],"to":["B","F","G","M","L","F","D","J","I","M","N","E","F","D","O","K","N","L","D","N","L","G","I","L"]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)"},"evals":[],"jsHooks":[]}</script>
<p class="caption">
Figure 4: Disease-Separation. In a schematic PPI, we see genes associated with a disease A (in pink), and genes associated to disease B (in green).
</p>

</div>

The separation of diseases A and B is given by:
$$
<d_{AA}> = 1.5
$$

$$
<d_{BB}> = 1.5
$$

$$
<d_{AB}> = 2.7
$$
$$
S_{AB} = 2.7 - \frac{1.5+ 1.5}2 = 1.2.
$$

#### Example in real data

``` r
sab = separation(gPPI, GDA_Interest)
```

    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |=========                                                             |  12%
      |                                                                            
      |==================                                                    |  25%
      |                                                                            
      |==========================                                            |  38%
      |                                                                            
      |===================================                                   |  50%
      |                                                                            
      |============================================                          |  62%
      |                                                                            
      |====================================================                  |  75%
      |                                                                            
      |=============================================================         |  88%
      |                                                                            
      |======================================================================| 100%

    ## Calculating S_ab..

    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |=========                                                             |  12%
      |                                                                            
      |==================                                                    |  25%
      |                                                                            
      |==========================                                            |  38%
      |                                                                            
      |===================================                                   |  50%
      |                                                                            
      |============================================                          |  62%
      |                                                                            
      |====================================================                  |  75%
      |                                                                            
      |=============================================================         |  88%
      |                                                                            
      |======================================================================| 100%

    ## Done..

``` r
sab
```

    ## $Sab
    ##                           schizophrenia autistic disorder bipolar disorder
    ## schizophrenia                         0        -0.1045849       -0.4157872
    ## autistic disorder                    NA         0.0000000       -0.1548042
    ## bipolar disorder                     NA                NA        0.0000000
    ## major depressive disorder            NA                NA               NA
    ## endometriosis                        NA                NA               NA
    ## parkinson disease                    NA                NA               NA
    ## asthma                               NA                NA               NA
    ## rheumatoid arthritis                 NA                NA               NA
    ##                           major depressive disorder endometriosis
    ## schizophrenia                            -0.1340951    0.22202922
    ## autistic disorder                        -0.1132453    0.09132126
    ## bipolar disorder                         -0.2881742    0.16886859
    ## major depressive disorder                 0.0000000    0.05226307
    ## endometriosis                                    NA    0.00000000
    ## parkinson disease                                NA            NA
    ## asthma                                           NA            NA
    ## rheumatoid arthritis                             NA            NA
    ##                           parkinson disease     asthma rheumatoid arthritis
    ## schizophrenia                    0.25050220 0.27686802           0.17548173
    ## autistic disorder                0.05937602 0.07992430           0.04855410
    ## bipolar disorder                 0.18588070 0.17716612           0.12075995
    ## major depressive disorder        0.04371349 0.14222353           0.02800574
    ## endometriosis                    0.14567472 0.06544036           0.02120766
    ## parkinson disease                0.00000000 0.03649188           0.11030856
    ## asthma                                   NA 0.00000000           0.01226364
    ## rheumatoid arthritis                     NA         NA           0.00000000
    ## 
    ## $Dab
    ##                           schizophrenia autistic disorder bipolar disorder
    ## schizophrenia                  1.176331          1.185962        0.8217292
    ## autistic disorder                    NA          1.404762        1.1969274
    ## bipolar disorder                     NA                NA        1.2987013
    ## major depressive disorder            NA                NA               NA
    ## endometriosis                        NA                NA               NA
    ## parkinson disease                    NA                NA               NA
    ## asthma                               NA                NA               NA
    ## rheumatoid arthritis                 NA                NA               NA
    ##                           major depressive disorder endometriosis
    ## schizophrenia                              1.120037      1.529940
    ## autistic disorder                          1.255102      1.513447
    ## bipolar disorder                           1.027143      1.537964
    ## major depressive disorder                  1.331933      1.437975
    ## endometriosis                                    NA      1.439490
    ## parkinson disease                                NA            NA
    ## asthma                                           NA            NA
    ## rheumatoid arthritis                             NA            NA
    ##                           parkinson disease   asthma rheumatoid arthritis
    ## schizophrenia                      1.600863 1.668831             1.483301
    ## autistic disorder                  1.523952 1.586103             1.470588
    ## bipolar disorder                   1.597426 1.630314             1.489764
    ## major depressive disorder          1.471875 1.611987             1.413625
    ## endometriosis                      1.627615 1.588983             1.460606
    ## parkinson disease                  1.524390 1.602484             1.592157
    ## asthma                                   NA 1.607595             1.535714
    ## rheumatoid arthritis                     NA       NA             1.439306

``` r
Sep_ex2 = sab$Sab %>% as.matrix()

Sep_ex2[lower.tri(Sep_ex2)] = t(Sep_ex2)[lower.tri(Sep_ex2)]
```

We can visualize the network separation of the diseases using a heatmap.

``` r
Sep_ex2 %>% heatmap(., symm = T)
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-23-1.png" width="672" />

### Exercises

1.  Calculate the **Jaccard Index** and the **Separation** for the diseases that have `diseaseSemanticType` as *Mental or Behavioral Dysfunction*.

<!-- -->

    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |=                                                                     |   1%
      |                                                                            
      |==                                                                    |   3%
      |                                                                            
      |===                                                                   |   4%
      |                                                                            
      |====                                                                  |   6%
      |                                                                            
      |=====                                                                 |   7%
      |                                                                            
      |======                                                                |   8%
      |                                                                            
      |=======                                                               |  10%
      |                                                                            
      |========                                                              |  11%
      |                                                                            
      |=========                                                             |  13%
      |                                                                            
      |==========                                                            |  14%
      |                                                                            
      |===========                                                           |  15%
      |                                                                            
      |============                                                          |  17%
      |                                                                            
      |=============                                                         |  18%
      |                                                                            
      |==============                                                        |  20%
      |                                                                            
      |===============                                                       |  21%
      |                                                                            
      |================                                                      |  23%
      |                                                                            
      |=================                                                     |  24%
      |                                                                            
      |==================                                                    |  25%
      |                                                                            
      |===================                                                   |  27%
      |                                                                            
      |====================                                                  |  28%
      |                                                                            
      |=====================                                                 |  30%
      |                                                                            
      |======================                                                |  31%
      |                                                                            
      |=======================                                               |  32%
      |                                                                            
      |========================                                              |  34%
      |                                                                            
      |=========================                                             |  35%
      |                                                                            
      |==========================                                            |  37%
      |                                                                            
      |===========================                                           |  38%
      |                                                                            
      |============================                                          |  39%
      |                                                                            
      |=============================                                         |  41%
      |                                                                            
      |==============================                                        |  42%
      |                                                                            
      |===============================                                       |  44%
      |                                                                            
      |================================                                      |  45%
      |                                                                            
      |=================================                                     |  46%
      |                                                                            
      |==================================                                    |  48%
      |                                                                            
      |===================================                                   |  49%
      |                                                                            
      |===================================                                   |  51%
      |                                                                            
      |====================================                                  |  52%
      |                                                                            
      |=====================================                                 |  54%
      |                                                                            
      |======================================                                |  55%
      |                                                                            
      |=======================================                               |  56%
      |                                                                            
      |========================================                              |  58%
      |                                                                            
      |=========================================                             |  59%
      |                                                                            
      |==========================================                            |  61%
      |                                                                            
      |===========================================                           |  62%
      |                                                                            
      |============================================                          |  63%
      |                                                                            
      |=============================================                         |  65%
      |                                                                            
      |==============================================                        |  66%
      |                                                                            
      |===============================================                       |  68%
      |                                                                            
      |================================================                      |  69%
      |                                                                            
      |=================================================                     |  70%
      |                                                                            
      |==================================================                    |  72%
      |                                                                            
      |===================================================                   |  73%
      |                                                                            
      |====================================================                  |  75%
      |                                                                            
      |=====================================================                 |  76%
      |                                                                            
      |======================================================                |  77%
      |                                                                            
      |=======================================================               |  79%
      |                                                                            
      |========================================================              |  80%
      |                                                                            
      |=========================================================             |  82%
      |                                                                            
      |==========================================================            |  83%
      |                                                                            
      |===========================================================           |  85%
      |                                                                            
      |============================================================          |  86%
      |                                                                            
      |=============================================================         |  87%
      |                                                                            
      |==============================================================        |  89%
      |                                                                            
      |===============================================================       |  90%
      |                                                                            
      |================================================================      |  92%
      |                                                                            
      |=================================================================     |  93%
      |                                                                            
      |==================================================================    |  94%
      |                                                                            
      |===================================================================   |  96%
      |                                                                            
      |====================================================================  |  97%
      |                                                                            
      |===================================================================== |  99%
      |                                                                            
      |======================================================================| 100%

    ## Calculating S_ab..

    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |=                                                                     |   1%
      |                                                                            
      |==                                                                    |   3%
      |                                                                            
      |===                                                                   |   4%
      |                                                                            
      |====                                                                  |   6%
      |                                                                            
      |=====                                                                 |   7%
      |                                                                            
      |======                                                                |   8%
      |                                                                            
      |=======                                                               |  10%
      |                                                                            
      |========                                                              |  11%
      |                                                                            
      |=========                                                             |  13%
      |                                                                            
      |==========                                                            |  14%
      |                                                                            
      |===========                                                           |  15%
      |                                                                            
      |============                                                          |  17%
      |                                                                            
      |=============                                                         |  18%
      |                                                                            
      |==============                                                        |  20%
      |                                                                            
      |===============                                                       |  21%
      |                                                                            
      |================                                                      |  23%
      |                                                                            
      |=================                                                     |  24%
      |                                                                            
      |==================                                                    |  25%
      |                                                                            
      |===================                                                   |  27%
      |                                                                            
      |====================                                                  |  28%
      |                                                                            
      |=====================                                                 |  30%
      |                                                                            
      |======================                                                |  31%
      |                                                                            
      |=======================                                               |  32%
      |                                                                            
      |========================                                              |  34%
      |                                                                            
      |=========================                                             |  35%
      |                                                                            
      |==========================                                            |  37%
      |                                                                            
      |===========================                                           |  38%
      |                                                                            
      |============================                                          |  39%
      |                                                                            
      |=============================                                         |  41%
      |                                                                            
      |==============================                                        |  42%
      |                                                                            
      |===============================                                       |  44%
      |                                                                            
      |================================                                      |  45%
      |                                                                            
      |=================================                                     |  46%
      |                                                                            
      |==================================                                    |  48%
      |                                                                            
      |===================================                                   |  49%
      |                                                                            
      |===================================                                   |  51%
      |                                                                            
      |====================================                                  |  52%
      |                                                                            
      |=====================================                                 |  54%
      |                                                                            
      |======================================                                |  55%
      |                                                                            
      |=======================================                               |  56%
      |                                                                            
      |========================================                              |  58%
      |                                                                            
      |=========================================                             |  59%
      |                                                                            
      |==========================================                            |  61%
      |                                                                            
      |===========================================                           |  62%
      |                                                                            
      |============================================                          |  63%
      |                                                                            
      |=============================================                         |  65%
      |                                                                            
      |==============================================                        |  66%
      |                                                                            
      |===============================================                       |  68%
      |                                                                            
      |================================================                      |  69%
      |                                                                            
      |=================================================                     |  70%
      |                                                                            
      |==================================================                    |  72%
      |                                                                            
      |===================================================                   |  73%
      |                                                                            
      |====================================================                  |  75%
      |                                                                            
      |=====================================================                 |  76%
      |                                                                            
      |======================================================                |  77%
      |                                                                            
      |=======================================================               |  79%
      |                                                                            
      |========================================================              |  80%
      |                                                                            
      |=========================================================             |  82%
      |                                                                            
      |==========================================================            |  83%
      |                                                                            
      |===========================================================           |  85%
      |                                                                            
      |============================================================          |  86%
      |                                                                            
      |=============================================================         |  87%
      |                                                                            
      |==============================================================        |  89%
      |                                                                            
      |===============================================================       |  90%
      |                                                                            
      |================================================================      |  92%
      |                                                                            
      |=================================================================     |  93%
      |                                                                            
      |==================================================================    |  94%
      |                                                                            
      |===================================================================   |  96%
      |                                                                            
      |====================================================================  |  97%
      |                                                                            
      |===================================================================== |  99%
      |                                                                            
      |======================================================================| 100%

    ## Done..

    ## 
      |                                                                            
      |                                                                      |   0%
      |                                                                            
      |=                                                                     |   1%
      |                                                                            
      |==                                                                    |   3%
      |                                                                            
      |===                                                                   |   4%
      |                                                                            
      |====                                                                  |   6%
      |                                                                            
      |=====                                                                 |   7%
      |                                                                            
      |======                                                                |   8%
      |                                                                            
      |=======                                                               |  10%
      |                                                                            
      |========                                                              |  11%
      |                                                                            
      |=========                                                             |  13%
      |                                                                            
      |==========                                                            |  14%
      |                                                                            
      |===========                                                           |  15%
      |                                                                            
      |============                                                          |  17%
      |                                                                            
      |=============                                                         |  18%
      |                                                                            
      |==============                                                        |  20%
      |                                                                            
      |===============                                                       |  21%
      |                                                                            
      |================                                                      |  23%
      |                                                                            
      |=================                                                     |  24%
      |                                                                            
      |==================                                                    |  25%
      |                                                                            
      |===================                                                   |  27%
      |                                                                            
      |====================                                                  |  28%
      |                                                                            
      |=====================                                                 |  30%
      |                                                                            
      |======================                                                |  31%
      |                                                                            
      |=======================                                               |  32%
      |                                                                            
      |========================                                              |  34%
      |                                                                            
      |=========================                                             |  35%
      |                                                                            
      |==========================                                            |  37%
      |                                                                            
      |===========================                                           |  38%
      |                                                                            
      |============================                                          |  39%
      |                                                                            
      |=============================                                         |  41%
      |                                                                            
      |==============================                                        |  42%
      |                                                                            
      |===============================                                       |  44%
      |                                                                            
      |================================                                      |  45%
      |                                                                            
      |=================================                                     |  46%
      |                                                                            
      |==================================                                    |  48%
      |                                                                            
      |===================================                                   |  49%
      |                                                                            
      |===================================                                   |  51%
      |                                                                            
      |====================================                                  |  52%
      |                                                                            
      |=====================================                                 |  54%
      |                                                                            
      |======================================                                |  55%
      |                                                                            
      |=======================================                               |  56%
      |                                                                            
      |========================================                              |  58%
      |                                                                            
      |=========================================                             |  59%
      |                                                                            
      |==========================================                            |  61%
      |                                                                            
      |===========================================                           |  62%
      |                                                                            
      |============================================                          |  63%
      |                                                                            
      |=============================================                         |  65%
      |                                                                            
      |==============================================                        |  66%
      |                                                                            
      |===============================================                       |  68%
      |                                                                            
      |================================================                      |  69%
      |                                                                            
      |=================================================                     |  70%
      |                                                                            
      |==================================================                    |  72%
      |                                                                            
      |===================================================                   |  73%
      |                                                                            
      |====================================================                  |  75%
      |                                                                            
      |=====================================================                 |  76%
      |                                                                            
      |======================================================                |  77%
      |                                                                            
      |=======================================================               |  79%
      |                                                                            
      |========================================================              |  80%
      |                                                                            
      |=========================================================             |  82%
      |                                                                            
      |==========================================================            |  83%
      |                                                                            
      |===========================================================           |  85%
      |                                                                            
      |============================================================          |  86%
      |                                                                            
      |=============================================================         |  87%
      |                                                                            
      |==============================================================        |  89%
      |                                                                            
      |===============================================================       |  90%
      |                                                                            
      |================================================================      |  92%
      |                                                                            
      |=================================================================     |  93%
      |                                                                            
      |==================================================================    |  94%
      |                                                                            
      |===================================================================   |  96%
      |                                                                            
      |====================================================================  |  97%
      |                                                                            
      |===================================================================== |  99%
      |                                                                            
      |======================================================================| 100%

2.  Optional: Try to make the network visualization for the heatmap of `Sep_ex2`. Use diseases as nodes, and their weight as links.

<img src="{{< blogdown/postref >}}index_files/figure-html/sab-net-solution-1.png" width="672" />

3.  Optional: If we go back to our PPI, can we identify that the modules are indeed close or separated? Plot the network for those diseases.

## Gene Co-expression Networks

In co-expression networks, a pair of nodes is typically connected by a link if the genes they represent show a *significantly correlated expression pattern* across a set of biological samples of interest. They are often built from genome-wide expression data measured by RNA-Seq or microarrays. Those networks are often weighted, and it represents the strength of a gene-pair relationship. Gene co-expression networks are also signed, and the sign of the link can be indicative of whether a gene pair is regulated in the same direction or oppositely controlled. The majority of the methods used for constructing those networks rely on a similarity measure, such as mutual information or correlation. In this course, we will use the **wTO** ([Deisy Morselli Gysi et al. 2018](#ref-Gysi2018); [Nowick et al. 2009](#ref-Nowick2009)) method . This is a correlation based method, that normalizes the effect of the interaction by all gene neibours and that also accounts for positive and negative interactions. It also calculates the probability for each gene pair, reducing the false positive links.

A next step for co-expression analysis is by comparing the resulting networks for different groups. For that the **CoDiNA** (Co-expression Differential Network Analysis) ([Morselli Gysi et al. 2020](#ref-morselligysi2020)) method can be used.

### Construction of co-expression networks

To run the weighted topological overlap (wTO) in R we can easily do by loading the wTO R package. It calculates the networks using expression data, where genes are on the rows and individuals in the columns. The user can select parallel computing for faster computation.

For the sake of time, we will be focusing here on the network of differential expressed genes. However, in a real set-up, the best approach is to use the full set of genes.

For the analysis we will be using the data from GSE12654 ([Iwamoto et al. 2004](#ref-Iwamoto2004)). The file here contains Pre Frontal Cortex samples from Control and patients with Bipolar Disorder, Schizophrenia and Major Depression, and is already filtered for only differential expressed genes.

-   Let’s read in the data:

``` r
require(wTO)
```

    ## Loading required package: wTO

``` r
require(CoDiNA)
```

    ## Loading required package: CoDiNA

    ## 
    ## Attaching package: 'CoDiNA'

    ## The following objects are masked from 'package:igraph':
    ## 
    ##     as.igraph, normalize

``` r
Brain = fread("./data/Brain.csv") %>% 
  as.data.frame()
row.names(Brain) = Brain$V1

Brain = Brain[,-1]
```

-   Let’s look into it:

<!-- -->

    ##          BD_ID1   BD_ID2   BD_ID3   BD_ID4   BD_ID5   BD_ID6   BD_ID7   BD_ID8
    ## HSF1   7.593522 7.414498 7.551313 7.604576 7.818765 7.452378 7.505604 7.570283
    ## PDLIM5 4.194690 4.300449 4.141026 4.067548 4.296824 4.309824 4.394849 4.166770
    ## MSX2   3.912240 3.841050 3.701938 3.912098 3.703137 3.700736 3.969032 3.650190
    ## PML    6.565121 6.569837 6.551878 6.570909 6.629373 6.553288 6.677511 6.553654
    ## DEDD   6.043961 5.914945 6.169624 5.811971 5.874334 6.040275 5.941909 6.045629
    ## RAB8A  6.235237 6.353685 6.318565 6.209181 6.272028 6.468419 6.246321 6.351017
    ## ELF3   4.879226 4.481001 4.630118 4.943885 4.642466 4.624869 4.515094 4.670392
    ## CDC5L  2.918337 2.742434 2.814158 2.760304 2.870200 2.748367 2.831504 2.768490
    ## ZNF211 4.448121 4.722869 4.285932 4.690489 4.267327 4.328995 4.376896 4.396760
    ## FOXF2  4.525340 4.818690 4.936654 4.772364 4.921325 4.825912 5.031065 4.687238
    ##          BD_ID9  BD_ID10
    ## HSF1   7.905238 7.704259
    ## PDLIM5 4.649225 3.909871
    ## MSX2   3.896163 3.918197
    ## PML    6.480951 6.526635
    ## DEDD   6.093379 6.216374
    ## RAB8A  6.258136 6.637794
    ## ELF3   4.605525 4.488687
    ## CDC5L  2.861687 2.910959
    ## ZNF211 4.793549 4.824614
    ## FOXF2  4.923211 4.978635

-   How to run the `wTO`?

You’ll need a `data.frame` with genes on the rows, individuals on the columns, gene names on the `row.names`. We also need to create one network per disease.

``` r
BD = Brain %>% 
  dplyr::select(starts_with('BD')) %>%
  wTO.fast(n = 100, Data = .)
```

    ## There are 198 overlapping nodes, 198 total nodes and 11 individuals.

    ## This function might take a long time to run. Don't turn off the computer.

    ##  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99  100 Done!

-   Let’s explore the output

<!-- -->

    ##    Node.1 Node.2    wTO pval  pval.adj
    ## 1:   HSF1 PDLIM5  0.221 0.56 0.5885160
    ## 2:   HSF1   MSX2  0.112 0.34 0.5780850
    ## 3: PDLIM5   MSX2 -0.076 0.46 0.5780850
    ## 4:   HSF1    PML -0.150 0.48 0.5780850
    ## 5: PDLIM5    PML  0.132 0.57 0.5936828
    ## 6:   MSX2    PML -0.043 0.45 0.5780850

``` r
SCZ = Brain %>% select(starts_with('SCZ')) %>% 
  wTO::wTO.fast(n = 100, Data = .)
```

    ## There are 198 overlapping nodes, 198 total nodes and 13 individuals.

    ## This function might take a long time to run. Don't turn off the computer.

    ##  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99  100 Done!

``` r
CTR = Brain %>% select(starts_with('CTR')) %>% 
  wTO::wTO.fast( n = 100, Data = .)
```

    ## There are 198 overlapping nodes, 198 total nodes and 15 individuals.
    ## This function might take a long time to run. Don't turn off the computer.
    ##  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99  100 Done!

``` r
MDD = Brain %>% select(starts_with('MDD')) %>% 
  wTO::wTO.fast( n = 100, Data = .)
```

    ## There are 198 overlapping nodes, 198 total nodes and 11 individuals.
    ## This function might take a long time to run. Don't turn off the computer.
    ##  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  94  95  96  97  98  99  100 Done!

-   The next step is to filter the edges of the network to keep only ones with a significant p-value. For that, we will set all other weights to zero.

``` r
BD_cl = BD %>% 
  mutate(wTO = ifelse(pval < 0.05, wTO, 0))

MDD_cl = MDD %>% 
  mutate(wTO = ifelse(pval < 0.05, wTO, 0))

CTR_cl = CTR %>% 
  mutate(wTO = ifelse(pval < 0.05, wTO, 0))

SCZ_cl = SCZ %>% 
  mutate(wTO = ifelse(pval < 0.05, wTO, 0))
```

### Comparing multiple networks

When comparing more than one network, different measures can be used, for example, one can identify the hubs in one network, identify modules and clusters in each one and compare those results. However, none of those comparisons compare the network topology and structure. For that, we have to compare links and nodes, and it can be easily done using CoDiNA.

CoDiNA identifies links - and nodes -that are **common** to all networks; **specific** to one network; or **different** in one network.

-   Common ($\alpha$ \| `a`): A particular interaction exists in *all networks* under comparison *with the same sign*;
-   Different ($\beta$ \| `b.`): A particular interaction exists in *all networks* under comparison *with different sign*;
-   Specific ($\gamma$ \| `g.`): A particular interaction *does not* exists in *all networks* under comparison.

To calculate it, you can use the function `MakeDiffNet` and you need to provide a edge list with the weights.

``` r
require(CoDiNA)
DiffNet = MakeDiffNet(Data = list(CTR_cl, BD_cl, MDD_cl, SCZ_cl),
                      Code = c("CTR", "BD", "MDD", "SCZ"))
```

    ## Starting now.

    ## CTR contains 19503 edges and 198 nodes.

    ## BD contains 19503 edges and 198 nodes.

    ## MDD contains 19503 edges and 198 nodes.

    ## SCZ contains 19503 edges and 198 nodes.

    ## Total of nodes: 198

    ## Total of edges: 19503

    ## Coding correlations.

    ## Total of edges (inside the cutoff): 1225

    ## Starting Phi categorization.

    ## Coding the groups.

    ## Recode is done!

After comparing the networks, we need to filter them for links that strongly well-clustered and representative of their class.

``` r
DiffNet_cl = DiffNet %>% 
  filter( Score_ratio > 1)

net_fig = plot(DiffNet_cl, cutoff.external = 0, cutoff.internal = 1)
```

``` r
e = net_fig$Edges 
names(e) = c("from", "to", "group", "Phi", "width")
e$weight = e$width
n = net_fig$Nodes
n$Phi_tilde %<>% as.factor()
n$group = n$Phi_tilde
# n$color = RColorBrewer::brewer.pal(nlevels(n$Phi_tilde), "Pastel2")[unclass(n$Phi_tilde)]
# n$frame.color = RColorBrewer::brewer.pal(nlevels(n$Phi_tilde), "Set2")[unclass(n$Phi_tilde)]
# n$label.color = RColorBrewer::brewer.pal(nlevels(n$Phi_tilde), "Dark2")[unclass(n$Phi_tilde)]


gDis = graph_from_data_frame(e, vertices = n, directed = F)
V(gDis)$size = strength(gDis) %>% 
  CoDiNA::normalize()
V(gDis)$size = (V(gDis)$size + 0.1)*30
V(gDis)$label = ifelse(V(gDis)$size  > 4, V(gDis)$name, NA )
V(gDis)$value = V(gDis)$size
E(gDis)$weight = E(gDis)$width * 100

E(gDis)$length = E(gDis)$width
x = visIgraph(gDis)

visNetwork(nodes = x$x$nodes, edges = x$x$edges) %>%
  visPhysics(enabled = F) %>% 
  visIgraphLayout(layout = "layout_with_fr") %>%
  visLegend()
```

<div id="htmlwidget-11" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-11">{"x":{"nodes":{"id":["ACVR1B","ACVRL1","APBB2","ARID3A","ARID4A","ARNT","ASB1","ATF2","ATF7","ATG4B","BMP7","BMPR1A","BRCA1","BRD1","CALCOCO1","CASK","CBX6","CCNT2","CDH1","CEBPZ","CHD3","CHD8","CHMP1A","CORO1A","CREG1","CRIP1","CRTC1","CTNNBIP1","DEDD","DHX57","DSP","E2F5","EIF5B","ELF3","EMX1","EP300","EP400","EPAS1","ERBB2","ERCC8","ESR2","ETV1","EYA1","FOS","FOXF2","FRY","FUBP3","GABPB1","GATAD1","GCLC","GPATCH8","GTF3C1","GTF3C2","HIC2","HIPK3","HIST1H2BN","HIVEP1","HMGN3","HOXA11","HSF1","ING1","INSM1","IRF5","JUND","KAT2A","KCNH2","KDM5C","KHDRBS2","KHSRP","L3MBTL1","LARP1","LEF1","LOC155060","LRRFIP1","MAML1","MAPK14","MAX","MBD1","MBD3","MEN1","MET","METTL3","MGA","MINK1","MITF","MLX","MNAT1","MSX2","MTA1","MYBL1","MYCN","MYT1","NFAT5","NFATC2IP","NFATC3","NFE2L2","NFKBIL1","NFRKB","NMI","NOTCH3","NPAT","NR2E1","NR4A3","ORC2","PAPOLA","PASK","PCGF2","PDE8B","PDLIM5","PHF14","PHF20","PHTF1","PLXNA3","PLXND1","PML","PMS2P3","POU6F1","POU6F2","PPP1R10","PPP1R8","PRDM2","PRKRIR","PRPF4B","PTGER3","RAB8A","RARB","RBBP5","RBBP8","RBCK1","RBFOX2","RBPMS","RCOR1","REL","RNF114","RPA2","RPS14","RXRB","SALL1","SETD1A","SF1","SFPQ","SKI","SMAD4","SMAD7","SMARCC2","SMARCE1","SNAPC1","STAT1","STAT6","SUPT7L","TBX5","TCF3","TCOF1","TERF2","TFAP4","TGIF2","TRIM27","TRIM32","TRIP13","TRIP4","TTF1","TTLL5","TULP3","VPS4B","WHSC1","WWOX","XPC","YBX1","ZBED4","ZBTB11","ZBTB48","ZC3H4","ZFR2","ZFYVE26","ZNF134","ZNF140","ZNF142","ZNF174","ZNF195","ZNF211","ZNF254","ZNF282","ZNF330","ZNF510","ZNF629","ZNF7","ZNF710","ZNF804A","ZNF85"],"Phi_tilde":["g.CTR","g.BD","U","g.CTR","U","g.SCZ","g.CTR","g.MDD","g.SCZ","U","g.BD","g.CTR","g.CTR","g.BD","g.SCZ","g.CTR","g.SCZ","U","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.CTR","g.CTR","U","g.BD","g.SCZ","U","U","g.SCZ","g.SCZ","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.SCZ","g.SCZ","g.BD","g.SCZ","g.CTR","g.BD","U","U","g.CTR","g.SCZ","g.CTR","g.SCZ","g.SCZ","U","g.MDD","g.CTR","g.CTR","g.SCZ","U","U","g.CTR","g.SCZ","g.CTR","g.SCZ","g.BD","g.CTR","g.SCZ","g.MDD","g.CTR","U","g.CTR","U","g.CTR","g.BD","g.SCZ","U","g.SCZ","g.CTR","U","g.SCZ","g.CTR","g.CTR","g.SCZ","g.SCZ","g.MDD","g.CTR","g.CTR","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","U","g.SCZ","g.CTR","g.BD","U","g.CTR","g.MDD","U","g.CTR","U","g.BD","g.CTR","g.BD","g.BD","g.SCZ","g.CTR","g.MDD","g.SCZ","g.CTR","U","g.BD","g.MDD","g.MDD","U","g.CTR","g.SCZ","g.BD","g.SCZ","g.SCZ","g.CTR","g.CTR","g.SCZ","g.SCZ","g.CTR","g.BD","g.CTR","U","U","g.CTR","U","g.SCZ","g.MDD","g.MDD","g.CTR","g.CTR","U","g.CTR","g.CTR","U","g.SCZ","g.CTR","g.SCZ","g.MDD","g.MDD","U","g.CTR","g.CTR","U","g.BD","g.BD","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.BD","g.BD","g.BD","g.CTR","U","U","g.SCZ","g.CTR","U","U","g.BD","g.CTR","U","g.CTR","U","g.SCZ","g.CTR","U","U","U","g.BD","U","g.SCZ","g.CTR","g.BD","g.CTR","U","g.SCZ","U","g.BD","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR"],"Phi":["g","g","U","g","U","g","g","g","g","U","g","g","g","g","g","g","g","U","g","g","g","g","g","g","U","g","g","U","U","g","g","g","g","g","g","g","g","g","g","g","g","g","U","U","g","g","g","g","g","U","g","g","g","g","U","U","g","g","g","g","g","g","g","g","g","U","g","U","g","g","g","U","g","g","U","g","g","g","g","g","g","g","g","g","g","g","g","g","g","U","g","g","g","U","g","g","U","g","U","g","g","g","g","g","g","g","g","g","U","g","g","g","U","g","g","g","g","g","g","g","g","g","g","g","g","U","U","g","U","g","g","g","g","g","U","g","g","U","g","g","g","g","g","U","g","g","U","g","g","g","g","g","g","g","g","g","g","g","U","U","g","g","U","U","g","g","U","g","U","g","g","U","U","U","g","U","g","g","g","g","U","g","U","g","g","g","g","g","g"],"Degree_Total":[13,4,1,22,6,25,12,14,25,7,11,15,22,5,7,14,9,4,6,8,16,5,7,9,10,8,4,1,3,25,8,19,5,26,2,9,15,13,10,6,33,4,8,1,6,31,20,4,5,1,3,15,5,9,33,1,7,16,22,17,28,8,17,6,20,8,22,1,9,15,12,1,21,12,1,3,6,11,16,4,20,11,5,4,26,15,16,14,13,1,26,38,19,4,8,7,8,20,1,11,2,4,4,17,3,4,21,5,1,14,17,21,1,13,14,9,8,40,5,5,5,27,2,7,13,5,1,8,15,26,11,21,23,6,7,19,6,3,9,8,39,23,28,22,8,6,4,13,5,5,26,37,37,19,2,17,8,9,1,1,18,8,3,1,4,7,1,9,1,28,7,19,3,10,6,1,31,6,26,14,12,28,1,4,13,8,18,10,7],"Degree_a":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"Degree_b":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],"Degree_g":[13,4,1,22,6,25,12,14,25,7,11,15,22,5,7,14,9,4,6,8,16,5,7,9,10,8,4,1,3,25,8,19,5,26,2,9,15,13,10,6,33,4,8,1,6,31,20,4,5,1,3,15,5,9,33,1,7,16,22,17,28,8,17,6,20,8,22,1,9,15,12,1,21,12,1,3,6,11,16,4,20,11,5,4,26,15,16,14,13,1,26,38,19,4,8,7,8,20,1,11,2,4,4,17,3,4,21,5,1,14,17,21,1,13,14,9,8,40,5,5,5,27,2,7,13,5,1,8,15,26,11,21,23,6,7,19,6,3,9,8,39,23,28,22,8,6,4,13,5,5,26,37,37,19,2,17,8,9,1,1,18,8,3,1,4,7,1,9,1,28,7,19,3,10,6,1,31,6,26,14,12,28,1,4,13,8,18,10,7],"group":["g.CTR","g.BD","U","g.CTR","U","g.SCZ","g.CTR","g.MDD","g.SCZ","U","g.BD","g.CTR","g.CTR","g.BD","g.SCZ","g.CTR","g.SCZ","U","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.CTR","g.CTR","U","g.BD","g.SCZ","U","U","g.SCZ","g.SCZ","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.SCZ","g.SCZ","g.BD","g.SCZ","g.CTR","g.BD","U","U","g.CTR","g.SCZ","g.CTR","g.SCZ","g.SCZ","U","g.MDD","g.CTR","g.CTR","g.SCZ","U","U","g.CTR","g.SCZ","g.CTR","g.SCZ","g.BD","g.CTR","g.SCZ","g.MDD","g.CTR","U","g.CTR","U","g.CTR","g.BD","g.SCZ","U","g.SCZ","g.CTR","U","g.SCZ","g.CTR","g.CTR","g.SCZ","g.SCZ","g.MDD","g.CTR","g.CTR","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","U","g.SCZ","g.CTR","g.BD","U","g.CTR","g.MDD","U","g.CTR","U","g.BD","g.CTR","g.BD","g.BD","g.SCZ","g.CTR","g.MDD","g.SCZ","g.CTR","U","g.BD","g.MDD","g.MDD","U","g.CTR","g.SCZ","g.BD","g.SCZ","g.SCZ","g.CTR","g.CTR","g.SCZ","g.SCZ","g.CTR","g.BD","g.CTR","U","U","g.CTR","U","g.SCZ","g.MDD","g.MDD","g.CTR","g.CTR","U","g.CTR","g.CTR","U","g.SCZ","g.CTR","g.SCZ","g.MDD","g.MDD","U","g.CTR","g.CTR","U","g.BD","g.BD","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.BD","g.BD","g.BD","g.CTR","U","U","g.SCZ","g.CTR","U","U","g.BD","g.CTR","U","g.CTR","U","g.SCZ","g.CTR","U","U","U","g.BD","U","g.SCZ","g.CTR","g.BD","g.CTR","U","g.SCZ","U","g.BD","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR"],"size":[10.8448829744276,5.96275437852425,3.20958838392565,19.1969276393363,6.28889754720899,19.8095319057502,9.95880081319824,12.4984138300761,21.7027843179413,7.44534091432594,11.0074637112492,12.837820014413,17.0477774018943,6.3474756256898,7.15784774230562,12.8599270417301,8.47785871450947,5.10143080609877,5.43404737504582,8.56627355413672,13.7233139201353,5.52380449670271,7.11065803662806,8.35752932369318,9.44082390174881,8.29743914194386,5.03891607371416,3.09936238175995,4.43446747805417,19.0293565806585,8.2940277001701,15.559357451096,5.89501669731337,21.8852039849158,3.92355389030572,8.242712864437,15.1791233679841,11.8261234578723,9.29579071237174,6.37909890595882,27.3142134732408,5.24611976047758,7.62932648698996,3.34174035166639,7.00490510965547,30.8854629911801,17.3465289904555,5.37326872032248,5.46971824023138,3.29900651251321,4.83021384281422,11.6600719450179,6.10917309706735,9.48868177142855,27.2148203858084,3.15603783566915,7.00241702321127,13.0867120899716,20.923822578997,14.0914883456673,26.3402667109468,7.69341215866769,15.4626270371885,6.6260802666441,17.1549376631591,7.55925526315265,19.4009363530334,3.16295430608632,9.10914351237425,15.5854712371402,11.4725128965472,3.20077825274294,17.4679047594571,9.74642930013266,3.34174035166639,5.28136028520844,6.48194073909428,10.0867200201201,13.2860492211392,4.96676696206185,18.2786614322163,9.53863768036925,6.58827369249705,5.32989851402919,22.4363869909824,12.21222811112,12.8291680165464,11.2751793242396,13.2063244979395,3.2360187774738,24.7681951541984,33,16.4976583258796,5.0472328111483,8.58647322310448,7.25978487074077,7.68112092596314,17.2147664462526,3.25804410543059,9.52751867218819,3.74672421105121,5.81298214841809,4.89672850541567,14.7019551971677,4.50671559848546,6.08922180104128,16.0698801747563,6.10570737897831,3.5179429753207,12.6831450274459,15.9733171389015,14.9866978399505,3.21999750688886,12.7693331838922,12.4278988164999,7.80285535844671,7.00562645933088,32.5528408841007,5.556406673209,5.85363102770062,5.6842694392033,23.4618426086093,3.6451412038199,6.43450398293966,11.322514788114,6.68314876594057,3.18078181391744,8.02550702950406,12.5524453777364,18.9376341641176,9.88161182452568,16.4552163422431,19.5075585056253,6.68277097144618,6.60224265253489,13.5879330224767,5.91498818244434,4.40965870686457,8.3226776833242,7.1137723341973,32.8012346765349,19.416342766681,24.5973200737732,18.1570167693487,6.98076111169615,7.6934748642277,5.30326432741098,10.888594615504,5.13946066744214,5.20820694165506,20.5904073961546,30.691531070252,31.8281842492943,16.9871332691992,3.74260132047994,15.6892681912003,7.16023208586507,8.27973416320365,3.12217683325871,3,13.0206634047574,7.91897289812676,4.54964297622802,3.21999750688886,4.50875152543699,7.69998917041407,3.28395717810858,8.46241747035471,3.12718384865274,23.64898215677,8.19595243253687,16.3659371862467,4.06373617722712,7.87829549488647,6.86511237513231,3.17484950367495,25.4124192922769,6.76411575918125,20.5506676759424,11.7073026686182,9.77913438321429,24.3936828282195,3.13513283919353,5.27100231678104,11.1135006991233,9.04393315758395,15.7475479136648,10.0132757734758,7.10648811688678],"label":["ACVR1B","ACVRL1","APBB2","ARID3A","ARID4A","ARNT","ASB1","ATF2","ATF7","ATG4B","BMP7","BMPR1A","BRCA1","BRD1","CALCOCO1","CASK","CBX6","CCNT2","CDH1","CEBPZ","CHD3","CHD8","CHMP1A","CORO1A","CREG1","CRIP1","CRTC1","CTNNBIP1","DEDD","DHX57","DSP","E2F5","EIF5B","ELF3","EMX1","EP300","EP400","EPAS1","ERBB2","ERCC8","ESR2","ETV1","EYA1","FOS","FOXF2","FRY","FUBP3","GABPB1","GATAD1","GCLC","GPATCH8","GTF3C1","GTF3C2","HIC2","HIPK3","HIST1H2BN","HIVEP1","HMGN3","HOXA11","HSF1","ING1","INSM1","IRF5","JUND","KAT2A","KCNH2","KDM5C","KHDRBS2","KHSRP","L3MBTL1","LARP1","LEF1","LOC155060","LRRFIP1","MAML1","MAPK14","MAX","MBD1","MBD3","MEN1","MET","METTL3","MGA","MINK1","MITF","MLX","MNAT1","MSX2","MTA1","MYBL1","MYCN","MYT1","NFAT5","NFATC2IP","NFATC3","NFE2L2","NFKBIL1","NFRKB","NMI","NOTCH3","NPAT","NR2E1","NR4A3","ORC2","PAPOLA","PASK","PCGF2","PDE8B","PDLIM5","PHF14","PHF20","PHTF1","PLXNA3","PLXND1","PML","PMS2P3","POU6F1","POU6F2","PPP1R10","PPP1R8","PRDM2","PRKRIR","PRPF4B","PTGER3","RAB8A","RARB","RBBP5","RBBP8","RBCK1","RBFOX2","RBPMS","RCOR1","REL","RNF114","RPA2","RPS14","RXRB","SALL1","SETD1A","SF1","SFPQ","SKI","SMAD4","SMAD7","SMARCC2","SMARCE1","SNAPC1","STAT1","STAT6","SUPT7L","TBX5","TCF3","TCOF1","TERF2","TFAP4","TGIF2","TRIM27","TRIM32","TRIP13","TRIP4","TTF1","TTLL5","TULP3","VPS4B","WHSC1","WWOX","XPC","YBX1","ZBED4","ZBTB11","ZBTB48","ZC3H4","ZFR2","ZFYVE26","ZNF134","ZNF140","ZNF142","ZNF174","ZNF195","ZNF211","ZNF254","ZNF282","ZNF330","ZNF510","ZNF629","ZNF7","ZNF710","ZNF804A","ZNF85"],"value":[10.8448829744276,5.96275437852425,3.20958838392565,19.1969276393363,6.28889754720899,19.8095319057502,9.95880081319824,12.4984138300761,21.7027843179413,7.44534091432594,11.0074637112492,12.837820014413,17.0477774018943,6.3474756256898,7.15784774230562,12.8599270417301,8.47785871450947,5.10143080609877,5.43404737504582,8.56627355413672,13.7233139201353,5.52380449670271,7.11065803662806,8.35752932369318,9.44082390174881,8.29743914194386,5.03891607371416,3.09936238175995,4.43446747805417,19.0293565806585,8.2940277001701,15.559357451096,5.89501669731337,21.8852039849158,3.92355389030572,8.242712864437,15.1791233679841,11.8261234578723,9.29579071237174,6.37909890595882,27.3142134732408,5.24611976047758,7.62932648698996,3.34174035166639,7.00490510965547,30.8854629911801,17.3465289904555,5.37326872032248,5.46971824023138,3.29900651251321,4.83021384281422,11.6600719450179,6.10917309706735,9.48868177142855,27.2148203858084,3.15603783566915,7.00241702321127,13.0867120899716,20.923822578997,14.0914883456673,26.3402667109468,7.69341215866769,15.4626270371885,6.6260802666441,17.1549376631591,7.55925526315265,19.4009363530334,3.16295430608632,9.10914351237425,15.5854712371402,11.4725128965472,3.20077825274294,17.4679047594571,9.74642930013266,3.34174035166639,5.28136028520844,6.48194073909428,10.0867200201201,13.2860492211392,4.96676696206185,18.2786614322163,9.53863768036925,6.58827369249705,5.32989851402919,22.4363869909824,12.21222811112,12.8291680165464,11.2751793242396,13.2063244979395,3.2360187774738,24.7681951541984,33,16.4976583258796,5.0472328111483,8.58647322310448,7.25978487074077,7.68112092596314,17.2147664462526,3.25804410543059,9.52751867218819,3.74672421105121,5.81298214841809,4.89672850541567,14.7019551971677,4.50671559848546,6.08922180104128,16.0698801747563,6.10570737897831,3.5179429753207,12.6831450274459,15.9733171389015,14.9866978399505,3.21999750688886,12.7693331838922,12.4278988164999,7.80285535844671,7.00562645933088,32.5528408841007,5.556406673209,5.85363102770062,5.6842694392033,23.4618426086093,3.6451412038199,6.43450398293966,11.322514788114,6.68314876594057,3.18078181391744,8.02550702950406,12.5524453777364,18.9376341641176,9.88161182452568,16.4552163422431,19.5075585056253,6.68277097144618,6.60224265253489,13.5879330224767,5.91498818244434,4.40965870686457,8.3226776833242,7.1137723341973,32.8012346765349,19.416342766681,24.5973200737732,18.1570167693487,6.98076111169615,7.6934748642277,5.30326432741098,10.888594615504,5.13946066744214,5.20820694165506,20.5904073961546,30.691531070252,31.8281842492943,16.9871332691992,3.74260132047994,15.6892681912003,7.16023208586507,8.27973416320365,3.12217683325871,3,13.0206634047574,7.91897289812676,4.54964297622802,3.21999750688886,4.50875152543699,7.69998917041407,3.28395717810858,8.46241747035471,3.12718384865274,23.64898215677,8.19595243253687,16.3659371862467,4.06373617722712,7.87829549488647,6.86511237513231,3.17484950367495,25.4124192922769,6.76411575918125,20.5506676759424,11.7073026686182,9.77913438321429,24.3936828282195,3.13513283919353,5.27100231678104,11.1135006991233,9.04393315758395,15.7475479136648,10.0132757734758,7.10648811688678],"x":[-0.161439164401021,-0.588936712251491,0.103555542783516,-0.291760047841087,-0.0289716462507,-0.253342813519864,-0.214285575715552,-0.204810861463349,-0.224999917401673,-0.476347824930566,-0.316686265434854,-0.0970811870497651,-0.133460480103165,0.0203238790390141,-0.0911516558787057,-0.0509074224756546,-0.0243104093708852,-0.00789384268468829,0.227852154637469,-0.429050549957889,-0.377624445431115,-0.647135140615614,-0.114180038074327,-0.357097083162795,-0.283254735450163,-0.235414716450423,0.0276972588481319,-0.0467384325499133,0.0963808427418087,-0.249715756952885,-0.0677899391794836,-0.21961234207343,0.12043502890732,-0.319505830339097,-0.493670480318363,-0.0547608678560583,-0.321848763935789,-0.130668674379085,-0.555727158392537,-0.549835249850108,-0.268026441173913,-0.397439495348305,-0.418885013550556,-0.507959087409045,0.0454908463727157,-0.201407580689366,-0.200579467383934,-0.0368466519954782,-0.567997536829367,1,-0.447691502589916,-0.305328120551022,-0.482718903526563,-0.264403573032678,-0.304075888964868,-0.181796569358665,-0.650784094511289,-0.160207318100387,-0.240000840150416,-0.300763970839665,-0.346756261059839,-0.11809010136892,-0.355939725238959,-0.30169214763257,-0.354120177714868,-0.443109279674931,-0.297670364330871,0.170293751421814,-0.364233400358358,-0.453440506263391,-0.298945355721756,-0.872130907568791,-0.169588923993642,-0.390245431035802,0.172058974177145,-0.536227382367735,-0.426620006433956,-0.517775298871381,-0.297368580735759,-0.615269376205126,-0.274089580630358,-0.246125958501418,-0.317379997831331,-0.372563296841417,-0.318849292746822,-0.392046121082745,-0.421745085888844,-0.139880102483668,-0.342769541617604,-0.875174287017735,-0.247541333903585,-0.308978846329979,-0.405602117306982,0.145661086065386,-0.117445031731178,-0.125283686231997,-0.256242888971456,-0.222234618373028,-0.518988889258768,-0.367532367430917,0.0314692241305028,-0.399628185538334,-0.441321538033671,-0.364487841894907,0.142263323547937,-0.0379315420964342,-0.114962096735286,0.134782629341108,-0.336795747681226,-0.347522037321578,-0.30456503944964,-0.234249101669962,0.127602737757634,-0.304619874310708,-0.355379878255916,-0.501690298836692,-0.528639998271787,-0.218115862675894,-0.523919756792694,-0.0929964600291624,-0.538324344499198,-0.393156070564191,-0.390663676240726,-0.47298282501278,-0.39493903935612,-0.0638994337215192,-0.54815164844757,-0.0430641969859383,-0.375686212772405,-0.161170386245341,-0.164439652843111,-0.379997515821085,-0.213626151111351,-0.236122257382195,-0.584122004201127,-0.261584257912629,-0.504165619095009,0.721100591347577,-0.639124845029169,-0.160410182927995,-0.281395141144774,-0.214077081646925,-0.278390938516203,-0.363360851838247,-0.645808556085734,0.0849302282157389,-0.426586432930417,-0.452494978525399,-0.246789689226167,-0.610979616103329,-0.273542909376657,-0.218297531399174,-0.26020524081509,-0.442798958772798,-0.301033689253193,-0.341967587273227,-0.187652958810041,-0.532694954287665,-0.258060018831574,-1,-0.265378972127792,-0.507156185358742,-0.596851407190412,-0.11712696619836,-0.156675949901155,-0.291328091962225,0.997632191070171,-0.0604477482288469,-0.962106708331361,-0.307580705050182,-0.0589143889163735,-0.161590356967081,0.0322733602935361,-0.511786615721749,-0.170969409200741,-0.315607379626457,-0.249374756515465,0.0613497166108463,-0.286207329327098,-0.213043326911994,-0.207779544589765,-0.166288202027023,0.0708753859872666,-0.595821156672463,-0.384198700970107,-0.514885212361447,-0.131207131062476,-0.0183551799170477,0.0794058089280787],"y":[0.0763906427814467,-0.0638810204155936,0.638308835379202,-0.0580599593997572,0.329587158279028,-0.10334841678607,0.325502512201458,-0.0751012845157597,0.0829516590754309,0.0989211359650477,0.50092774958482,0.0374086419119677,0.0970435735968933,0.109622864021336,0.161891308034966,0.139254127196902,0.0710930839225199,0.612525317848161,0.0685247274902252,-0.101881601320743,0.0336968774492481,0.520063510349238,0.277899520277629,-0.170282349827845,0.175280098596108,0.457228925501672,0.0296648847184957,-0.430768080313342,0.354008619260919,0.0180066304280226,0.0823461345932568,-0.0242250604065188,0.157894473170594,0.0336659121633449,-0.413860310129304,0.158461154450339,0.0635976600395622,-0.0330423518524108,0.10890540709966,0.372505932168701,0.00550895936813456,0.523618456917519,0.293610327819103,0.820935386932062,0.234071178618747,0.128300738882438,0.069764118971672,0.0112034487902364,0.407689471497755,-0.012847099386219,0.331467627779126,0.2944141566015,0.29697794627435,0.306691136791749,0.0958990477762383,-0.486112830542222,0.122329442789312,0.0106745678555422,-0.0133521099085735,0.133292227379089,0.105963757378385,-0.29258727394193,0.074926787706187,0.321485119435836,0.401435490799682,0.524060844171903,0.115387513492692,-0.290101673290969,0.19197537845139,0.0650555455785271,0.265406538090551,0.410236975783489,0.148852174970667,0.355732350898668,-0.186023717015785,-0.122588496175071,-0.214761079093622,0.193208564388486,0.217743985002073,0.572076074111951,0.149020042834066,0.370530113899968,0.647040771312438,-0.276672515298913,0.195780966979728,0.198216186575698,0.101506413575703,0.191725160862385,-0.101868035756895,-0.0778494826754281,0.0567768156815245,0.0163622818197646,0.142225410164039,0.116275455769958,0.476916377502733,-0.138882714530572,0.26077490543642,0.139509929160344,0.712611283029413,0.425019578009334,-0.719929133977413,0.614872689184305,-0.17589134631153,0.145511827835339,0.454554014218934,-0.0265518001637549,-0.0102830139387979,0.0586091802814639,1,0.128265979383699,-0.0305136847867772,0.114555259468145,-1,0.434449816125514,0.0301916983988637,-0.018746646148305,0.129433001181405,0.0136361365231015,-0.174874121552568,0.33750269822394,0.608512402146734,0.0372145896098617,-0.373865093257878,-0.0398129322893314,-0.131131265602941,-0.0869719263079747,-0.324625978521405,-0.135754463077525,0.460508658869766,0.128895459635115,-0.0627866531719209,0.0681858204032495,0.219483748668103,-0.181449384439727,0.327304339923797,0.203372893857493,0.0244565471461342,-0.0055907713814386,0.379942023462746,0.419959802201506,0.0498727121678875,0.0560705754110216,0.0875761415136649,0.339545794475801,0.0321600088999225,0.203568309208481,0.409966191458275,0.230630883671757,0.481433559589896,0.204033390133117,-0.0415986595737591,0.170138665661763,0.0729001297515239,0.00926226739358715,-0.286570275553143,0.0176360797646733,0.376685065811593,0.079784735057189,-0.497001434842789,-0.0772883550280912,0.117667337589003,0.425454147600184,0.622600634464063,0.84958100071634,0.649410790014842,0.353956358287497,-0.0839892299698302,-0.0568075181154378,0.548466476536678,0.172039706677677,0.297761391828208,0.345234279513227,-0.0346581991864149,0.285280596800582,0.190420029918733,0.816333305588121,0.125478536744294,0.161288446274358,0.0346312832681368,0.490138461239031,-0.14313441254029,0.0455902409948856,0.797040703094624,0.296510978655181,0.27641927403947,0.0527460698397497,0.148582703683132,0.234950779461714,0.0503284483215423]},"edges":{"from":["ACVR1B","ACVR1B","ACVR1B","ACVR1B","ACVR1B","ACVR1B","ACVR1B","ACVR1B","ACVR1B","ACVR1B","ACVR1B","ACVR1B","ACVR1B","ACVRL1","ACVRL1","ACVRL1","ACVRL1","APBB2","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID3A","ARID4A","ARID4A","ARID4A","ARID4A","ARID4A","ARID4A","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ARNT","ASB1","ASB1","ASB1","ASB1","ASB1","ASB1","ASB1","ASB1","ASB1","ASB1","ASB1","ASB1","ATF2","ATF2","ATF2","ATF2","ATF2","ATF2","ATF2","ATF2","ATF2","ATF2","ATF2","ATF2","ATF2","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATF7","ATG4B","ATG4B","ATG4B","ATG4B","ATG4B","ATG4B","ATG4B","BMP7","BMP7","BMP7","BMP7","BMP7","BMP7","BMP7","BMP7","BMP7","BMP7","BMP7","BMPR1A","BMPR1A","BMPR1A","BMPR1A","BMPR1A","BMPR1A","BMPR1A","BMPR1A","BMPR1A","BMPR1A","BMPR1A","BMPR1A","BMPR1A","BMPR1A","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRCA1","BRD1","BRD1","BRD1","BRD1","BRD1","CALCOCO1","CALCOCO1","CALCOCO1","CALCOCO1","CALCOCO1","CALCOCO1","CALCOCO1","CASK","CASK","CASK","CASK","CASK","CASK","CASK","CASK","CASK","CASK","CASK","CBX6","CBX6","CBX6","CBX6","CBX6","CBX6","CBX6","CBX6","CBX6","CCNT2","CCNT2","CCNT2","CCNT2","CDH1","CDH1","CDH1","CDH1","CDH1","CEBPZ","CEBPZ","CEBPZ","CEBPZ","CEBPZ","CEBPZ","CEBPZ","CHD3","CHD3","CHD3","CHD3","CHD3","CHD3","CHD3","CHD3","CHD3","CHD3","CHD3","CHD3","CHD3","CHD3","CHD3","CHD8","CHD8","CHD8","CHD8","CHD8","CHMP1A","CHMP1A","CHMP1A","CHMP1A","CHMP1A","CHMP1A","CORO1A","CORO1A","CORO1A","CORO1A","CORO1A","CORO1A","CREG1","CREG1","CREG1","CREG1","CREG1","CREG1","CREG1","CREG1","CREG1","CREG1","CRIP1","CRIP1","CRIP1","CRIP1","CRIP1","CRIP1","CRTC1","CRTC1","CRTC1","DEDD","DEDD","DEDD","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DHX57","DSP","DSP","DSP","DSP","DSP","DSP","E2F5","E2F5","E2F5","E2F5","E2F5","E2F5","E2F5","E2F5","E2F5","E2F5","E2F5","E2F5","E2F5","E2F5","E2F5","E2F5","EIF5B","EIF5B","EIF5B","EIF5B","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","ELF3","EMX1","EP300","EP300","EP300","EP300","EP300","EP300","EP300","EP300","EP400","EP400","EP400","EP400","EP400","EP400","EP400","EP400","EP400","EP400","EP400","EP400","EPAS1","EPAS1","EPAS1","EPAS1","EPAS1","EPAS1","EPAS1","EPAS1","EPAS1","ERBB2","ERBB2","ERBB2","ERBB2","ERBB2","ERBB2","ERBB2","ERBB2","ERBB2","ERCC8","ERCC8","ERCC8","ERCC8","ERCC8","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ESR2","ETV1","ETV1","ETV1","ETV1","EYA1","EYA1","EYA1","EYA1","EYA1","EYA1","FOS","FOXF2","FOXF2","FOXF2","FOXF2","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FRY","FUBP3","FUBP3","FUBP3","FUBP3","FUBP3","FUBP3","FUBP3","FUBP3","FUBP3","FUBP3","FUBP3","FUBP3","FUBP3","FUBP3","GABPB1","GABPB1","GABPB1","GATAD1","GATAD1","GATAD1","GATAD1","GCLC","GPATCH8","GPATCH8","GPATCH8","GTF3C1","GTF3C1","GTF3C1","GTF3C1","GTF3C1","GTF3C1","GTF3C1","GTF3C1","GTF3C1","GTF3C1","GTF3C1","GTF3C1","GTF3C1","GTF3C2","GTF3C2","GTF3C2","GTF3C2","GTF3C2","HIC2","HIC2","HIC2","HIC2","HIC2","HIC2","HIC2","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIPK3","HIVEP1","HIVEP1","HIVEP1","HIVEP1","HIVEP1","HIVEP1","HIVEP1","HMGN3","HMGN3","HMGN3","HMGN3","HMGN3","HMGN3","HMGN3","HMGN3","HMGN3","HMGN3","HOXA11","HOXA11","HOXA11","HOXA11","HOXA11","HOXA11","HOXA11","HOXA11","HOXA11","HOXA11","HOXA11","HOXA11","HOXA11","HOXA11","HOXA11","HSF1","HSF1","HSF1","HSF1","HSF1","HSF1","HSF1","HSF1","HSF1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","ING1","INSM1","INSM1","INSM1","INSM1","INSM1","IRF5","IRF5","IRF5","IRF5","IRF5","IRF5","IRF5","IRF5","IRF5","IRF5","IRF5","IRF5","IRF5","JUND","JUND","JUND","JUND","KAT2A","KAT2A","KAT2A","KAT2A","KAT2A","KAT2A","KAT2A","KAT2A","KAT2A","KAT2A","KAT2A","KAT2A","KAT2A","KAT2A","KAT2A","KCNH2","KCNH2","KCNH2","KCNH2","KCNH2","KCNH2","KDM5C","KDM5C","KDM5C","KDM5C","KDM5C","KDM5C","KDM5C","KDM5C","KDM5C","KDM5C","KDM5C","KDM5C","KDM5C","KHDRBS2","KHSRP","KHSRP","KHSRP","KHSRP","KHSRP","L3MBTL1","L3MBTL1","L3MBTL1","L3MBTL1","L3MBTL1","L3MBTL1","L3MBTL1","L3MBTL1","LARP1","LARP1","LARP1","LARP1","LARP1","LARP1","LARP1","LEF1","LOC155060","LOC155060","LOC155060","LOC155060","LOC155060","LOC155060","LOC155060","LOC155060","LOC155060","LOC155060","LOC155060","LOC155060","LRRFIP1","LRRFIP1","LRRFIP1","LRRFIP1","LRRFIP1","LRRFIP1","LRRFIP1","LRRFIP1","LRRFIP1","LRRFIP1","LRRFIP1","MAML1","MAPK14","MAPK14","MAX","MAX","MAX","MAX","MBD1","MBD1","MBD1","MBD1","MBD1","MBD1","MBD3","MBD3","MBD3","MBD3","MBD3","MBD3","MBD3","MBD3","MBD3","MBD3","MEN1","MEN1","MEN1","MET","MET","MET","MET","MET","MET","MET","MET","MET","MET","MET","MET","MET","MET","METTL3","METTL3","METTL3","METTL3","METTL3","METTL3","METTL3","MGA","MGA","MGA","MINK1","MINK1","MITF","MITF","MITF","MITF","MITF","MITF","MITF","MITF","MITF","MITF","MITF","MITF","MITF","MITF","MITF","MITF","MLX","MLX","MLX","MLX","MLX","MLX","MLX","MLX","MNAT1","MNAT1","MNAT1","MNAT1","MNAT1","MNAT1","MNAT1","MNAT1","MNAT1","MNAT1","MNAT1","MSX2","MSX2","MSX2","MSX2","MSX2","MSX2","MTA1","MTA1","MTA1","MTA1","MYBL1","MYCN","MYCN","MYCN","MYCN","MYCN","MYCN","MYCN","MYCN","MYCN","MYCN","MYCN","MYCN","MYCN","MYCN","MYCN","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","MYT1","NFAT5","NFAT5","NFAT5","NFAT5","NFAT5","NFAT5","NFAT5","NFAT5","NFAT5","NFAT5","NFATC2IP","NFATC2IP","NFATC3","NFATC3","NFATC3","NFATC3","NFATC3","NFE2L2","NFE2L2","NFE2L2","NFE2L2","NFKBIL1","NFKBIL1","NFKBIL1","NFRKB","NFRKB","NFRKB","NFRKB","NFRKB","NFRKB","NFRKB","NFRKB","NFRKB","NMI","NOTCH3","NOTCH3","NOTCH3","NOTCH3","NOTCH3","NOTCH3","NPAT","NR2E1","NR4A3","NR4A3","NR4A3","ORC2","ORC2","ORC2","ORC2","ORC2","PASK","PASK","PCGF2","PCGF2","PCGF2","PCGF2","PCGF2","PCGF2","PCGF2","PCGF2","PDE8B","PDE8B","PHF14","PHF14","PHF14","PHF14","PHF14","PHF14","PHF20","PHF20","PHF20","PHF20","PHF20","PHF20","PHF20","PHF20","PHF20","PHF20","PHF20","PHF20","PHTF1","PHTF1","PHTF1","PHTF1","PHTF1","PHTF1","PHTF1","PLXND1","PLXND1","PLXND1","PLXND1","PLXND1","PLXND1","PML","PML","PML","PML","PML","PML","PML","PML","PML","PMS2P3","PMS2P3","PMS2P3","PMS2P3","POU6F1","POU6F1","POU6F1","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","POU6F2","PPP1R10","PPP1R10","PPP1R10","PPP1R8","PRDM2","PRDM2","PRDM2","PRKRIR","PRKRIR","PRKRIR","PRKRIR","PRKRIR","PRKRIR","PRKRIR","PRKRIR","PRKRIR","PRKRIR","PRKRIR","PRKRIR","PRKRIR","PRPF4B","PRPF4B","PTGER3","PTGER3","PTGER3","PTGER3","PTGER3","RAB8A","RAB8A","RAB8A","RAB8A","RARB","RARB","RBBP8","RBBP8","RBBP8","RBCK1","RBCK1","RBCK1","RBCK1","RBCK1","RBCK1","RBFOX2","RBFOX2","RBFOX2","RBFOX2","RBFOX2","RBFOX2","RBFOX2","RBFOX2","RBPMS","RBPMS","RBPMS","RCOR1","RCOR1","RCOR1","RCOR1","RCOR1","RCOR1","RCOR1","REL","REL","REL","REL","REL","REL","REL","RNF114","RNF114","RNF114","RPA2","RPA2","RPA2","RPS14","RPS14","RPS14","RPS14","RPS14","RPS14","RPS14","RXRB","RXRB","SALL1","SETD1A","SETD1A","SF1","SF1","SF1","SFPQ","SFPQ","SFPQ","SFPQ","SFPQ","SFPQ","SFPQ","SFPQ","SFPQ","SFPQ","SKI","SKI","SKI","SKI","SKI","SKI","SMAD4","SMAD4","SMAD4","SMAD4","SMAD4","SMAD4","SMAD7","SMAD7","SMAD7","SMARCC2","SMARCC2","SMARCE1","SMARCE1","SMARCE1","SMARCE1","SNAPC1","STAT1","STAT1","STAT1","STAT6","STAT6","TBX5","TCF3","TCF3","TCF3","TCF3","TCF3","TCF3","TCOF1","TCOF1","TCOF1","TCOF1","TCOF1","TCOF1","TERF2","TERF2","TFAP4","TGIF2","TGIF2","TRIM32","TTF1","TTF1","TTLL5","VPS4B","WHSC1","WHSC1","ZBTB11","ZBTB11","ZBTB11","ZBTB48","ZBTB48","ZBTB48","ZNF142","ZNF174","ZNF195","ZNF195","ZNF710"],"to":["BRCA1","ESR2","KDM5C","MYT1","PHF14","POU6F2","RBBP8","RBPMS","RPS14","SKI","WWOX","ZBTB11","ZNF142","ARID3A","ERBB2","ING1","L3MBTL1","ZC3H4","ATF2","BRCA1","CORO1A","DHX57","E2F5","ESR2","FUBP3","ING1","INSM1","KDM5C","L3MBTL1","MYT1","NFAT5","NFRKB","PCGF2","PHTF1","PRKRIR","RCOR1","TCOF1","YBX1","ZNF254","CASK","MET","PAPOLA","SF1","SMAD4","ZC3H4","ATF7","BMPR1A","CEBPZ","DSP","EPAS1","HIPK3","HIST1H2BN","HSF1","LOC155060","MAX","MINK1","MTA1","MYCN","PCGF2","PML","POU6F2","PPP1R10","SFPQ","SKI","TBX5","TCF3","TCOF1","TRIP13","ZNF142","ZNF282","CHMP1A","CRIP1","ELF3","GTF3C1","KHSRP","MSX2","NOTCH3","PPP1R8","REL","TRIM27","ZBTB11","ZC3H4","BRCA1","CTNNBIP1","ELF3","EP400","FRY","ING1","MITF","PHF20","PRKRIR","TERF2","ZNF195","ZNF282","ZNF85","CHD3","DHX57","DSP","EPAS1","ESR2","HIC2","HIPK3","HOXA11","ING1","L3MBTL1","LOC155060","MYT1","NFAT5","PCGF2","PHF14","POU6F2","RBFOX2","RBPMS","TCF3","TGIF2","ZC3H4","ZNF134","ZNF142","ZNF282","CORO1A","HSF1","MTA1","SFPQ","SNAPC1","TTF1","ZFYVE26","CRIP1","EYA1","GTF3C1","KCNH2","LARP1","MGA","NOTCH3","NR2E1","SMAD7","TRIM27","ZC3H4","BRCA1","CASK","ELF3","FRY","HOXA11","MYT1","NFKBIL1","RARB","TCOF1","ZNF174","ZNF254","ZNF710","ZNF804A","ZNF85","CASK","EP400","ESR2","FOXF2","LOC155060","NFKBIL1","NFRKB","ORC2","PHTF1","RBFOX2","SFPQ","TCF3","TCOF1","ZNF142","ZNF174","ZNF195","ZNF282","ZNF804A","FRY","MSX2","PASK","POU6F2","REL","DHX57","ESR2","GABPB1","HSF1","MSX2","PLXND1","RARB","EIF5B","FRY","FUBP3","HMGN3","ING1","NFKBIL1","SMAD4","ZNF174","ZNF710","ZNF804A","ZNF85","CDH1","MYCN","NFATC2IP","POU6F2","SFPQ","TCF3","ZNF134","ZNF142","ZNF282","PAPOLA","WHSC1","ZC3H4","ZNF211","CRTC1","PDE8B","SALL1","TTF1","ZNF804A","MAX","MBD1","POU6F2","PPP1R10","SKI","TBX5","TERF2","CORO1A","HIPK3","HOXA11","KHSRP","LARP1","MET","MITF","ORC2","PHF14","POU6F1","RARB","SMARCC2","TERF2","ZNF134","ZNF195","ERCC8","GATAD1","KAT2A","RPA2","TULP3","ORC2","PHTF1","PPP1R8","REL","ZC3H4","ZNF282","EMX1","ESR2","INSM1","KDM5C","MTA1","ZNF195","DHX57","E2F5","EP300","ING1","L3MBTL1","MYT1","NFAT5","SMAD4","SMAD7","ZBTB11","LARP1","NOTCH3","RPS14","SMAD7","TRIM27","ZC3H4","MITF","RBBP8","TTF1","FOXF2","RBFOX2","ZC3H4","E2F5","ELF3","EPAS1","ESR2","FUBP3","HMGN3","HSF1","ING1","KDM5C","L3MBTL1","LOC155060","MTA1","MYT1","NFAT5","NFRKB","RBFOX2","SFPQ","TBX5","TCOF1","TGIF2","ZNF142","EPAS1","HIC2","MYCN","NFATC2IP","TCF3","ZNF142","ESR2","FUBP3","HOXA11","HSF1","INSM1","KDM5C","MBD3","MSX2","MTA1","MYT1","PCGF2","PHTF1","RAB8A","RPS14","TCOF1","TTF1","FRY","ZNF174","ZNF710","ZNF85","EP400","EYA1","FRY","FUBP3","LARP1","MAPK14","MINK1","MITF","MLX","MYCN","MYT1","ORC2","PHTF1","RAB8A","RCOR1","SFPQ","SMAD4","TCF3","TCOF1","ZNF142","ZNF254","ZNF282","RAB8A","METTL3","PAPOLA","PCGF2","RBBP8","REL","TBX5","YBX1","ZNF195","FRY","IRF5","MITF","MLX","MNAT1","MYCN","SFPQ","TCF3","TCOF1","TTF1","ZBTB11","ZNF142","HIPK3","HMGN3","LOC155060","NFE2L2","NFRKB","PCGF2","POU6F2","TCF3","ZNF282","ING1","MBD1","ORC2","PMS2P3","PTGER3","RPA2","SUPT7L","ZNF282","ZNF510","IRF5","RBCK1","TULP3","WWOX","ZNF7","FUBP3","HIPK3","HMGN3","HOXA11","HSF1","IRF5","KDM5C","MBD3","MET","MSX2","MTA1","MYT1","NFRKB","NR4A3","PML","POU6F2","RBFOX2","RBPMS","SFPQ","SKI","TBX5","TCOF1","ZNF142","ZNF254","ZNF7","METTL3","NFKBIL1","RBCK1","ZFYVE26","FRY","HIPK3","MBD1","NOTCH3","SMAD7","ZFYVE26","RBCK1","FRY","RBFOX2","ZNF710","ZNF804A","HIPK3","ING1","LARP1","MBD1","MET","MITF","MLX","MYCN","MYT1","ORC2","PASK","PDE8B","PRKRIR","RCOR1","SMAD4","SMAD7","TERF2","TGIF2","ZBTB11","ZNF195","ZNF710","ZNF804A","KDM5C","LOC155060","MNAT1","MYT1","NFRKB","PHTF1","RPS14","SFPQ","TCF3","TCOF1","YBX1","ZNF142","ZNF195","ZNF629","HSF1","TBX5","TCF3","KAT2A","L3MBTL1","RBCK1","ZNF629","SALL1","HIPK3","REL","ZBTB11","HMGN3","KAT2A","KCNH2","KDM5C","MBD3","NOTCH3","RBCK1","RPS14","SKI","TBX5","TRIM27","WWOX","ZNF629","KHSRP","MLX","SMAD7","ZBTB11","ZNF142","SNAPC1","STAT1","TCF3","TTLL5","ZBTB11","ZC3H4","ZNF142","HMGN3","HOXA11","ING1","JUND","KHSRP","LOC155060","MET","MTA1","MYCN","MYT1","NFAT5","PCGF2","PHF14","PHF20","PHTF1","RBFOX2","SKI","SMAD4","SMAD7","TBX5","TCF3","ZBTB11","ZNF195","ZNF282","ZNF7","MBD1","MNAT1","PRKRIR","SETD1A","SMARCC2","TERF2","TRIM32","HOXA11","HSF1","LOC155060","NFE2L2","PCGF2","POU6F2","SFPQ","SMAD4","ZNF254","ZNF282","IRF5","LOC155060","MET","MSX2","MYT1","NFE2L2","PHF20","PHTF1","POU6F2","RBPMS","SFPQ","SKI","SMAD4","TCOF1","ZNF7","KAT2A","MTA1","PLXND1","POU6F2","RBFOX2","RPA2","SFPQ","TBX5","TCOF1","JUND","L3MBTL1","MBD3","MYCN","MYT1","NFAT5","PMS2P3","PRKRIR","RCOR1","REL","SMAD4","SMAD7","STAT1","TERF2","TGIF2","ZBTB11","ZNF134","ZNF710","NPAT","PCGF2","RBBP8","YBX1","ZNF254","KDM5C","MITF","MLX","MNAT1","MYCN","PHTF1","PML","POU6F2","RBFOX2","RNF114","SFPQ","TCOF1","ZNF282","KAT2A","REL","STAT1","ZNF710","LRRFIP1","MBD3","METTL3","MITF","NFATC3","NR2E1","PLXND1","RBCK1","SETD1A","SF1","TCF3","TTLL5","ZBTB48","ZNF211","ZNF629","MBD3","MEN1","MGA","PRDM2","SETD1A","ZNF211","MYT1","NFRKB","ORC2","PHTF1","PPP1R8","RBFOX2","RBPMS","REL","SMARCC2","TCOF1","WWOX","ZC3H4","ZNF195","PCGF2","ORC2","PCGF2","TGIF2","ZBTB11","ZNF142","MYT1","NFAT5","PMS2P3","RAB8A","RCOR1","STAT1","TGIF2","ZNF195","MITF","MLX","ORC2","SFPQ","TRIM27","ZNF134","ZNF142","ZFYVE26","MYT1","PCGF2","POU6F2","RBFOX2","SF1","TCF3","TCOF1","ZC3H4","ZNF142","ZNF195","ZNF211","ZNF282","MITF","NFATC3","PLXND1","RPA2","RXRB","SF1","SMAD7","STAT1","TCF3","TTLL5","ZNF629","ZNF282","SMAD4","TERF2","PMS2P3","RXRB","TBX5","ZNF254","POU6F1","SETD1A","SMAD4","TTLL5","ZBTB11","ZNF510","MSX2","NFRKB","RBFOX2","SFPQ","TBX5","TCF3","TCOF1","WWOX","ZNF195","ZNF629","PRDM2","SETD1A","SMAD7","METTL3","MYT1","PHF20","PHTF1","POU6F2","REL","SKI","SMAD4","SMAD7","TBX5","TGIF2","ZBTB11","ZNF142","ZNF7","MSX2","NFRKB","RBCK1","REL","SF1","ZFYVE26","ZNF211","PDLIM5","PLXND1","RPS14","MTA1","RAB8A","MLX","MYCN","NFATC3","ORC2","PML","POU6F1","PRKRIR","RBCK1","SFPQ","TCOF1","TERF2","TRIM32","TTF1","ZFYVE26","ZNF195","ZNF211","MNAT1","ORC2","PLXND1","POU6F1","SFPQ","TCF3","TCOF1","ZNF142","MYCN","PRKRIR","RBFOX2","SFPQ","SMARCC2","TCOF1","TERF2","TRIM32","TTF1","ZFYVE26","ZNF710","NFRKB","PHF14","RBFOX2","REL","TCF3","ZNF710","POU6F2","RAB8A","SFPQ","TBX5","ZNF7","NFAT5","PHF14","PML","POU6F2","PRKRIR","RBFOX2","RPS14","SFPQ","SMAD4","TCF3","TCOF1","TTF1","ZBTB11","ZNF142","ZNF282","NFAT5","NFRKB","ORC2","PHF14","PHTF1","POU6F2","RBBP5","RCOR1","REL","SFPQ","SKI","SMAD4","TCOF1","TERF2","TFAP4","TGIF2","WWOX","ZBTB11","ZNF142","ZNF195","NFKBIL1","PHF14","PMS2P3","RCOR1","REL","SETD1A","SMAD7","TGIF2","ZBTB11","ZNF195","PDE8B","TCF3","PLXND1","SMARCE1","ZBTB48","ZNF211","ZNF330","PCGF2","POU6F2","SKI","SMAD4","RCOR1","SUPT7L","ZNF804A","POU6F2","RBFOX2","RBPMS","SFPQ","SMAD7","TCOF1","ZNF142","ZNF195","ZNF629","SMAD7","NR2E1","RBCK1","RPS14","SMAD7","TCF3","ZNF510","PLXNA3","SMAD7","RAB8A","RCOR1","SFPQ","PHTF1","POU6F1","SKI","TERF2","ZNF195","PHF20","ZBTB11","RBBP8","RPS14","TCF3","TRIM27","YBX1","ZFR2","ZNF254","ZNF282","SMARCE1","ZNF282","REL","SUPT7L","TBX5","TGIF2","ZNF195","ZNF710","PHTF1","POU6F2","PRKRIR","RCOR1","SFPQ","SKI","SMAD4","TBX5","TERF2","TGIF2","TRIM32","ZNF710","POU6F2","PPP1R8","PRKRIR","RCOR1","SKI","SMAD4","SMAD7","TCF3","TTF1","TULP3","ZNF140","ZNF211","ZNF629","POU6F2","PRKRIR","RBFOX2","SFPQ","TCOF1","TTF1","ZFYVE26","ZNF142","ZNF282","PTGER3","STAT1","TERF2","ZNF282","PRKRIR","SMARCC2","TTF1","PRKRIR","RAB8A","RBFOX2","RBPMS","REL","SFPQ","SKI","SMAD4","TBX5","TCF3","TCOF1","TGIF2","ZBTB11","ZNF142","ZNF195","ZNF254","ZNF282","ZNF710","PRKRIR","RXRB","TERF2","REL","SETD1A","STAT6","TTLL5","RCOR1","SFPQ","SKI","SMAD4","SMARCC2","TCF3","TCOF1","TERF2","TRIM32","TTF1","ZBTB11","ZNF142","ZNF282","RAB8A","TBX5","RCOR1","RNF114","STAT1","TCF3","TERF2","RCOR1","RPS14","TBX5","TRIM32","ZNF195","ZNF85","RNF114","TBX5","YBX1","RPS14","SMAD7","STAT1","TRIM27","ZC3H4","ZNF211","RBPMS","RPS14","SFPQ","TCF3","TCOF1","ZNF142","ZNF282","ZNF804A","TBX5","TCOF1","ZNF254","SFPQ","SMAD4","STAT1","TCOF1","TGIF2","ZBTB11","ZC3H4","SNAPC1","TCOF1","ZBTB11","ZNF142","ZNF174","ZNF195","ZNF710","SKI","TBX5","YBX1","SUPT7L","TTLL5","ZNF629","RXRB","TBX5","TCF3","TRIM27","YBX1","ZFR2","ZNF510","STAT1","TBX5","XPC","TRIM32","ZBED4","TCF3","ZC3H4","ZNF211","SKI","SMAD4","TBX5","TCF3","TCOF1","TGIF2","TTF1","ZNF142","ZNF282","ZNF710","SMAD4","STAT6","TBX5","TGIF2","ZBTB48","ZFR2","SMAD7","TCF3","ZBTB11","ZC3H4","ZNF254","ZNF7","ZBTB11","ZC3H4","ZFYVE26","SUPT7L","TRIP4","YBX1","ZBTB48","ZNF282","ZNF85","STAT6","TERF2","TTLL5","ZNF211","WHSC1","ZNF804A","ZNF254","TCOF1","ZBTB11","ZC3H4","ZNF142","ZNF282","ZNF629","TTF1","ZNF142","ZNF195","ZNF282","ZNF629","ZNF710","TRIM32","TTF1","ZNF195","ZBTB11","ZNF134","TTF1","ZFYVE26","ZNF710","ZNF629","ZNF211","WWOX","ZNF211","ZC3H4","ZNF142","ZNF195","ZNF211","ZNF282","ZNF85","ZNF282","ZNF710","ZNF629","ZNF7","ZNF804A"],"group":["g.CTR","g.CTR","g.CTR","g.CTR.BD","g.BD","g.BD","g.MDD","g.CTR","g.MDD","g.MDD","g.CTR","g.BD","g.MDD","g.BD","g.BD","g.BD","g.BD","g.BD","g.BD","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.BD","g.CTR","g.CTR","g.BD","g.CTR","g.BD","g.CTR","g.CTR","g.CTR","g.BD","g.CTR.BD","g.CTR","g.CTR","g.CTR","g.BD","g.SCZ","g.CTR","g.SCZ","g.BD","g.CTR","g.SCZ","g.BD","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.CTR","g.BD","g.BD","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.CTR.SCZ","g.MDD","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.CTR","g.BD","g.SCZ","g.BD","g.CTR","g.CTR","g.BD","g.CTR","g.CTR","g.BD","g.CTR","g.CTR.SCZ","g.MDD","g.SCZ","g.MDD","g.MDD","g.CTR.MDD.SCZ","g.CTR.BD","g.SCZ","g.CTR","g.MDD","g.SCZ","g.MDD","g.CTR","g.CTR","g.BD","g.BD","g.SCZ","g.SCZ","g.MDD","g.SCZ","g.SCZ","g.MDD","g.BD","g.BD","g.SCZ","g.BD","g.BD","g.SCZ","g.BD","g.SCZ","g.SCZ","g.MDD","g.SCZ","g.BD","g.SCZ","g.BD","g.SCZ","g.SCZ","g.BD","g.CTR","g.CTR","g.CTR.SCZ","g.BD","g.SCZ","g.SCZ","g.BD","g.BD","g.CTR","g.CTR","g.BD","g.CTR","g.BD","g.BD","g.BD","g.BD","g.BD","g.CTR","g.CTR","g.MDD","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.MDD","g.CTR","g.CTR","g.CTR","g.CTR","g.MDD","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.BD","g.CTR","g.CTR.SCZ","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.CTR","g.BD","g.SCZ","g.CTR","g.MDD","g.BD","g.MDD","g.BD","g.BD","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.BD","g.CTR","g.BD","g.CTR","g.CTR","g.BD","g.BD","g.CTR","g.CTR","g.BD","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.BD","g.CTR","g.BD","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.MDD","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.BD","g.SCZ","g.MDD","g.SCZ","g.SCZ","g.BD","g.BD","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.BD","g.CTR","g.CTR","g.CTR","g.CTR.SCZ","g.CTR","g.SCZ","g.BD","g.CTR","g.CTR","g.MDD","g.BD","g.BD","g.BD","g.MDD","g.MDD","g.MDD","g.BD","g.BD","g.BD","g.BD","g.BD","g.BD","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.CTR","g.SCZ","g.SCZ","g.MDD","g.CTR","g.CTR.SCZ","g.CTR","g.MDD","g.SCZ","g.BD","g.CTR","g.BD","g.SCZ","g.SCZ","g.CTR.BD","g.BD","g.CTR","g.CTR.SCZ","g.SCZ","g.SCZ","g.SCZ","g.BD","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR.SCZ","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.SCZ","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.MDD","g.SCZ","g.SCZ","g.MDD","g.MDD","g.SCZ","g.MDD.SCZ","g.SCZ","g.SCZ","g.SCZ","g.MDD","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.BD","g.BD","g.MDD","g.BD","g.BD","g.SCZ","g.SCZ","g.BD","g.BD","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR.MDD","g.SCZ","g.CTR","g.CTR","g.SCZ","g.CTR","g.SCZ","g.CTR","g.CTR","g.SCZ","g.BD","g.SCZ","g.CTR.MDD","g.SCZ","g.MDD","g.MDD.SCZ","g.MDD","g.SCZ","g.CTR","g.CTR","g.MDD","g.CTR","g.BD","g.SCZ","g.BD","g.BD","g.SCZ","g.SCZ","g.SCZ","g.BD","g.BD","g.BD","g.BD","g.CTR","g.CTR","g.CTR","g.CTR","g.MDD","g.CTR","g.SCZ","g.SCZ","g.MDD","g.SCZ","g.SCZ","g.SCZ","g.MDD","g.SCZ","g.MDD","g.CTR","g.SCZ","g.MDD","g.SCZ","g.MDD","g.SCZ","g.SCZ","g.MDD.SCZ","g.MDD","g.CTR.MDD","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.CTR.SCZ","g.CTR","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.BD","g.SCZ","g.SCZ","g.CTR","g.MDD","g.MDD","g.MDD","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.BD","g.BD","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.MDD","g.MDD","g.CTR","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR.SCZ","g.MDD.SCZ","g.MDD","g.MDD","g.CTR","g.SCZ","g.MDD","g.CTR","g.SCZ","g.BD.MDD","g.BD","g.SCZ","g.BD","g.MDD","g.MDD","g.SCZ","g.MDD","g.MDD","g.MDD","g.BD","g.SCZ","g.CTR.BD.MDD","g.BD","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR.SCZ","g.CTR.MDD","g.CTR.SCZ","g.CTR","g.SCZ","g.MDD","g.SCZ","g.CTR","g.CTR","g.MDD","g.MDD","g.SCZ","g.CTR.SCZ","g.SCZ","g.CTR","g.SCZ","g.CTR","g.MDD","g.CTR","g.MDD","g.MDD.SCZ","g.MDD","g.SCZ","g.MDD.SCZ","g.MDD","g.CTR.SCZ","g.CTR","g.CTR","g.CTR.SCZ","g.CTR","g.CTR","g.SCZ","g.BD","g.CTR.SCZ","g.SCZ","g.SCZ","g.MDD","g.BD","g.MDD","g.MDD","g.BD","g.BD.MDD","g.BD","g.SCZ","g.BD.MDD.SCZ","g.MDD","g.MDD.SCZ","g.MDD","g.BD","g.BD","g.BD","g.MDD.SCZ","g.BD","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR.SCZ","g.SCZ","g.CTR","g.SCZ","g.CTR.SCZ","g.SCZ","g.MDD","g.MDD","g.MDD","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.BD","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.SCZ","g.CTR","g.CTR.MDD","g.CTR","g.BD","g.BD","g.BD","g.BD","g.BD","g.CTR","g.BD","g.BD","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.BD","g.CTR","g.SCZ","g.BD","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.CTR.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.SCZ","g.CTR.SCZ","g.CTR","g.CTR","g.CTR","g.BD","g.SCZ","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.BD","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.CTR","g.CTR","g.CTR","g.SCZ","g.SCZ","g.CTR","g.MDD","g.MDD","g.CTR.MDD","g.MDD","g.MDD","g.MDD","g.MDD","g.MDD","g.MDD","g.BD","g.MDD","g.MDD","g.MDD","g.CTR","g.CTR","g.BD","g.BD","g.CTR","g.CTR","g.BD","g.CTR","g.BD","g.BD","g.CTR","g.CTR","g.CTR","g.SCZ","g.SCZ","g.MDD","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.BD","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.CTR","g.MDD","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.BD","g.SCZ","g.BD","g.CTR","g.CTR","g.CTR","g.CTR","g.BD","g.MDD","g.MDD","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.BD","g.CTR","g.CTR.SCZ","g.BD","g.MDD","g.BD.MDD","g.MDD","g.MDD","g.CTR","g.MDD","g.MDD","g.MDD","g.CTR","g.SCZ","g.BD","g.BD.MDD","g.CTR","g.BD.MDD","g.CTR","g.CTR.BD","g.SCZ","g.BD","g.BD","g.BD","g.MDD","g.MDD","g.MDD","g.BD","g.BD","g.BD","g.CTR","g.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.SCZ","g.MDD","g.MDD","g.MDD","g.SCZ","g.SCZ","g.CTR","g.SCZ","g.CTR.SCZ","g.SCZ","g.SCZ","g.BD","g.CTR","g.CTR","g.CTR","g.CTR","g.BD","g.BD","g.SCZ","g.BD","g.BD","g.CTR","g.BD","g.CTR","g.BD","g.BD","g.BD","g.BD","g.CTR","g.SCZ","g.MDD","g.SCZ","g.SCZ","g.MDD","g.MDD","g.CTR","g.CTR","g.SCZ","g.SCZ","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.CTR","g.SCZ","g.BD","g.BD","g.BD","g.BD","g.MDD","g.MDD","g.CTR","g.MDD","g.MDD","g.MDD","g.MDD","g.MDD","g.CTR","g.MDD","g.CTR","g.MDD","g.MDD","g.CTR","g.MDD","g.MDD","g.MDD","g.MDD","g.MDD","g.CTR","g.BD","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.BD","g.BD","g.CTR.BD","g.BD","g.CTR","g.CTR","g.SCZ","g.MDD.SCZ","g.CTR","g.SCZ","g.SCZ","g.BD","g.CTR.MDD.SCZ","g.CTR.MDD","g.MDD","g.CTR.MDD","g.SCZ","g.MDD.SCZ","g.MDD","g.BD","g.SCZ","g.BD","g.CTR","g.SCZ","g.BD.SCZ","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.MDD","g.MDD.SCZ","g.MDD","g.MDD","g.CTR","g.SCZ","g.SCZ","g.CTR.SCZ","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.CTR","g.BD","g.CTR","g.BD","g.BD","g.BD","g.BD","g.CTR","g.CTR","g.MDD","g.MDD","g.CTR","g.CTR","g.CTR","g.CTR","g.BD","g.BD","g.CTR","g.CTR","g.CTR","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.CTR","g.BD","g.CTR","g.MDD","g.MDD","g.MDD","g.BD","g.MDD","g.MDD","g.MDD.SCZ","g.SCZ","g.CTR","g.CTR","g.BD","g.CTR","g.CTR","g.CTR","g.BD","g.CTR","g.CTR","g.BD","g.SCZ","g.CTR","g.CTR","g.CTR","g.MDD","g.SCZ","g.BD","g.CTR","g.CTR","g.BD","g.SCZ","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.CTR","g.MDD","g.MDD","g.CTR.MDD","g.SCZ","g.SCZ","g.MDD","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.MDD","g.SCZ","g.CTR.MDD","g.MDD","g.SCZ","g.SCZ","g.MDD","g.SCZ","g.SCZ","g.SCZ","g.MDD","g.CTR","g.MDD","g.BD","g.BD","g.CTR","g.SCZ","g.BD","g.CTR","g.CTR","g.CTR","g.BD","g.BD","g.CTR","g.CTR","g.BD","g.BD","g.CTR","g.SCZ","g.SCZ","g.SCZ","g.SCZ","g.BD.SCZ","g.CTR","g.SCZ","g.CTR.SCZ","g.CTR","g.SCZ","g.CTR","g.SCZ","g.CTR.SCZ","g.CTR.SCZ","g.BD","g.CTR","g.BD","g.CTR","g.SCZ","g.SCZ","g.CTR","g.CTR","g.SCZ","g.BD","g.SCZ","g.SCZ","g.BD","g.CTR","g.CTR","g.CTR","g.SCZ","g.CTR","g.CTR","g.BD","g.CTR"],"Phi":["g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g","g"],"width":[0.437722419928826,0.590747330960854,0.754448398576513,0.676412241903577,0.7,0.625,0.510373443983403,0.501779359430605,0.468879668049793,0.630705394190871,0.686832740213523,0.608333333333333,0.597510373443983,0.779166666666666,0.7,0.9125,0.779166666666666,0.566666666666667,0.6875,0.494661921708185,0.508896797153025,0.526690391459075,0.740213523131673,0.871886120996441,0.829181494661922,0.958333333333333,0.836298932384342,0.782918149466192,0.929166666666667,0.861209964412811,0.7625,0.733096085409253,0.697508896797153,0.576512455516014,0.766666666666666,0.392638105269495,0.832740213523132,0.558718861209964,0.562277580071174,0.583333333333333,0.56390977443609,0.619217081850534,0.462406015037594,0.545833333333333,0.704626334519573,0.661654135338346,0.466666666666667,0.786476868327402,0.721804511278195,0.808270676691729,0.864661654135338,0.516014234875445,0.530075187969925,0.872180451127819,0.533807829181495,0.629166666666667,0.604166666666667,0.665413533834586,0.477443609022556,0.714285714285714,0.721804511278195,0.466192170818505,0.609588558405177,0.551867219917012,0.684210526315789,0.845864661654135,0.684210526315789,0.483985765124555,0.699248120300752,0.669172932330827,0.697508896797153,0.4875,0.631578947368421,0.520833333333333,0.629893238434164,0.519572953736655,0.4125,0.594306049822064,0.886120996441281,0.383333333333333,0.583629893238434,0.603839018112901,0.556016597510373,0.462406015037594,0.597510373443983,0.576763485477178,0.977391833622218,0.683181957061276,0.755639097744361,0.715302491103203,0.717842323651452,0.770676691729323,0.589211618257261,0.669039145907474,0.594306049822064,0.6125,0.7,0.548872180451128,0.684210526315789,0.614107883817427,0.800751879699248,0.654135338345864,0.850622406639004,0.858333333333333,0.866666666666666,0.75187969924812,0.641666666666666,0.679166666666667,0.680451127819549,0.7625,0.710526315789474,0.571428571428571,0.850622406639004,0.770676691729323,0.745833333333333,0.766917293233082,0.741666666666667,0.684210526315789,0.849624060150376,0.5875,0.925266903914591,0.804270462633452,0.509999388185068,0.7875,0.484962406015038,0.473684210526316,0.795833333333333,0.666666666666667,0.747330960854093,0.651245551601424,0.733333333333333,0.693950177935943,0.733333333333333,0.8375,0.704166666666667,0.641666666666666,0.7375,0.772241992882562,0.775800711743772,0.385892116182572,0.775800711743772,0.729537366548043,0.772241992882562,0.661921708185053,0.807829181494662,0.448398576512456,0.469750889679715,0.344398340248963,0.797153024911032,0.839857651245552,0.626334519572954,0.672597864768683,0.556016597510373,0.669039145907474,0.718861209964413,0.43984962406015,0.604982206405694,0.533807829181495,0.608333333333333,0.501779359430605,0.422753292383145,0.691729323308271,0.718045112781955,0.551601423487544,0.781954887218045,0.740213523131673,0.608333333333333,0.845864661654135,0.729537366548043,0.892116182572614,0.6375,0.771784232365145,0.554166666666667,0.679166666666667,0.635338345864662,0.526315789473684,0.526315789473684,0.616541353383459,0.670833333333333,0.580071174377224,0.745833333333333,0.647686832740214,0.889679715302491,0.620833333333333,0.35,0.754448398576513,0.779359430604982,0.5,0.654804270462634,0.935943060498221,0.871886120996441,0.658362989323844,0.476868327402135,0.796992481203007,0.572953736654804,0.654135338345864,0.5,0.593984962406015,0.454887218045113,0.642857142857143,0.857142857142857,0.715302491103203,0.35,0.736654804270463,0.554166666666667,0.56390977443609,0.455516014234876,0.413533834586466,0.424812030075188,0.33609958506224,0.580071174377224,0.779359430604982,0.697508896797153,0.565836298932385,0.743772241992883,0.672597864768683,0.807829181494662,0.597744360902255,0.608540925266904,0.644128113879004,0.679715302491103,0.593984962406015,0.747330960854093,0.665413533834586,0.691729323308271,0.8125,0.533834586466165,0.817427385892116,0.639097744360902,0.646616541353383,0.554166666666667,0.666666666666667,0.541353383458647,0.530075187969925,0.533834586466165,0.616541353383459,0.533834586466165,0.419928825622776,0.572953736654804,0.615658362989324,0.754448398576513,0.754448398576513,0.441666666666667,0.644128113879004,0.729537366548043,0.562277580071174,0.438797268895434,0.889679715302491,0.477443609022556,0.483333333333333,0.697508896797153,0.644128113879004,0.5850622406639,0.804166666666667,0.729166666666667,0.670833333333333,0.663900414937759,0.576763485477178,0.605809128630705,0.65,0.658333333333333,0.583333333333333,0.758333333333333,0.6875,0.758333333333333,0.718045112781955,0.590225563909774,0.424812030075188,0.640569395017794,0.498220640569395,0.586466165413534,0.424812030075188,0.302904564315352,0.469750889679715,0.688697562915721,0.715302491103203,0.473029045643154,0.744360902255639,0.8375,0.626334519572954,0.8375,0.571428571428571,0.631578947368421,0.728885995664836,0.625,0.708185053380783,0.509735666073672,0.744360902255639,0.721804511278195,0.680451127819549,0.658333333333333,0.484962406015038,0.680451127819549,0.597744360902255,0.635338345864662,0.631578947368421,0.853383458646616,0.706766917293233,0.717665619621052,0.658362989323844,0.693950177935943,0.37593984962406,0.658362989323844,0.850533807829182,0.511278195488722,0.507518796992481,0.818505338078292,0.879003558718861,0.637010676156584,0.633451957295374,0.697508896797153,0.544483985765125,0.818505338078292,0.383458646616541,0.658362989323844,0.651245551601424,0.708185053380783,0.441281138790036,0.932330827067669,0.766917293233082,1,0.703007518796992,0.906015037593985,0.834586466165413,0.487544483985765,0.845864661654135,0.68796992481203,0.902255639097744,0.784232365145228,0.87593984962406,0.612781954887218,0.473029045643154,0.676348547717842,0.661654135338346,0.706526750482646,0.733082706766917,0.744360902255639,0.759398496240601,0.41908713692946,0.800751879699248,0.597864768683274,0.419928825622776,0.459074733096086,0.644128113879004,0.686832740213523,0.814946619217082,0.651245551601424,0.466192170818505,0.540925266903915,0.973684210526316,0.800751879699248,0.887218045112782,0.654135338345864,0.894736842105263,0.977443609022556,0.744360902255639,0.804511278195489,0.823308270676692,0.5,0.924812030075188,0.838345864661654,0.81578947368421,0.616541353383459,0.902255639097744,0.560150375939849,0.750889679715303,0.616541353383459,0.545112781954887,0.744360902255639,0.522556390977443,0.8625,0.645833333333333,0.460580912863071,0.691666666666667,0.520833333333333,0.533834586466165,0.462406015037594,0.65,0.795833333333333,0.576512455516014,0.567669172932331,0.560150375939849,0.575187969924812,0.743772241992883,0.939501779359431,0.811387900355872,0.907473309608541,1,0.672932330827068,0.925266903914591,0.733096085409253,0.654135338345864,0.793594306049822,0.552631578947368,0.779359430604982,0.935943060498221,0.710526315789474,0.466666666666667,0.469924812030075,0.828914368712286,0.533834586466165,0.556016597510373,0.394977547668225,0.697095435684647,0.804511278195489,0.96085409252669,0.537366548042705,0.406639004149378,0.886120996441281,0.729166666666667,0.526315789473684,0.616666666666667,0.620833333333333,0.808270676691729,0.545112781954887,0.597744360902255,0.366666666666667,0.533333333333333,0.4625,0.691666666666667,0.711743772241993,0.619217081850534,0.740213523131673,0.725978647686833,0.867219917012448,0.786476868327402,0.954887218045113,0.778195488721804,0.908713692946058,0.887218045112782,0.676691729323308,0.906015037593985,0.925311203319502,0.977443609022556,1,0.825622775800712,0.988721804511278,0.709543568464731,0.898496240601504,0.887966804979254,0.87593984962406,0.774436090225564,1,0.87136929460581,0.728242377794158,0.829181494661922,0.683274021352313,0.693950177935943,0.466165413533835,0.857651245551602,0.864768683274022,0.533807829181495,0.558718861209964,0.695488721804511,0.657894736842105,0.703007518796992,0.804270462633452,0.733082706766917,0.515630077772378,0.704626334519573,0.612781954887218,0.81578947368421,0.658362989323844,0.556390977443609,0.745833333333333,0.469924812030075,0.402255639097744,0.651245551601424,0.775933609958506,0.697095435684647,0.62655601659751,0.43609022556391,0.412811387900356,0.505338078291815,0.736654804270463,0.715302491103203,0.518796992481203,0.529166666666666,0.4125,0.37218045112782,0.729323308270677,0.5,0.761565836298932,0.661921708185053,0.690391459074733,0.427046263345196,0.75103734439834,0.721991701244813,0.718861209964413,0.494661921708185,0.729323308270677,0.721804511278195,0.729323308270677,0.887218045112782,0.81578947368421,0.729323308270677,0.42518138678798,0.699298189139153,0.804979253112033,0.655601659751037,0.747330960854093,0.872180451127819,0.912863070539419,0.690391459074733,0.62781954887218,0.414801203432703,0.679166666666667,0.345864661654135,0.7,0.775933609958506,0.531120331950207,0.379699248120301,0.883817427385893,0.846473029045643,0.804979253112033,0.591666666666667,0.766917293233082,1,0.729166666666667,0.597744360902255,0.846975088967972,0.530249110320285,0.533807829181495,0.583629893238434,0.604982206405694,0.558718861209964,0.940632401619034,0.402209894890872,0.944990593334269,0.451957295373666,0.718045112781955,0.609958506224066,0.454887218045113,0.747330960854093,0.679715302491103,0.717842323651452,0.522821576763486,0.853383458646616,0.837682503017347,0.770676691729323,0.871886120996441,0.74812030075188,0.928825622775801,0.680497925311203,0.864768683274022,0.398340248962655,0.739923358115188,0.817427385892116,0.703007518796992,0.57874624843467,0.83402489626556,1,0.98576512455516,0.704626334519573,0.765288813971966,0.708185053380783,0.633451957295374,0.56390977443609,0.504166666666666,0.738131715830576,0.793233082706767,0.518796992481203,0.800829875518672,1,0.858921161825726,0.838174273858922,0.7375,1,0.791666666666667,0.808270676691729,1,0.763485477178423,0.438421341746006,0.796680497925311,0.791666666666667,0.658333333333333,0.833333333333333,0.79523638501336,0.770833333333333,0.718861209964413,0.498220640569395,0.750889679715303,0.619217081850534,0.455516014234876,0.427046263345196,0.483985765124555,0.718045112781955,0.37218045112782,0.763157894736842,0.736842105263158,0.684210526315789,0.755639097744361,0.880341981837404,0.567669172932331,0.562277580071174,0.830827067669173,0.950663025381366,0.710526315789474,0.385892116182572,0.643153526970955,0.647302904564315,0.665480427046264,0.836298932384342,0.711743772241993,0.725978647686833,0.690391459074733,0.850533807829182,0.854166666666667,0.903914590747331,0.718861209964413,0.466165413533835,0.654804270462634,0.725978647686833,0.683274021352313,0.800711743772242,0.765124555160142,0.775800711743772,0.533807829181495,0.586466165413534,0.701067615658363,0.601503759398496,0.616541353383459,0.484962406015038,0.9644128113879,0.857651245551602,0.644128113879004,0.914590747330961,0.701067615658363,0.711743772241993,0.661921708185053,0.850533807829182,0.364661654135338,0.836298932384342,0.790035587188612,0.818505338078292,0.676156583629894,0.522556390977443,0.708185053380783,0.605263157894737,0.807829181494662,0.623529566761572,0.654804270462634,0.920833333333333,0.916666666666667,0.654166666666667,0.845833333333333,0.816666666666667,0.622775800711744,0.766666666666666,0.766666666666666,0.849624060150376,0.447368421052631,0.808270676691729,0.593984962406015,0.441666666666667,0.715302491103203,0.68796992481203,0.558333333333333,0.711743772241993,0.665413533834586,0.672932330827068,0.556390977443609,0.608540925266904,0.646515642866656,0.622775800711744,0.725978647686833,0.640569395017794,0.519572953736655,0.462633451957295,0.62781954887218,0.480427046263345,0.615658362989324,0.683274021352313,0.565836298932385,0.379699248120301,0.409252669039146,0.541353383458647,0.390963486846373,0.601423487544484,0.622775800711744,0.622775800711744,0.691666666666667,0.774436090225564,0.917293233082707,0.476868327402135,0.629893238434164,0.697508896797153,0.743772241992883,0.437722419928826,0.615658362989324,0.755639097744361,0.5125,0.710526315789474,0.708185053380783,0.473684210526316,0.639097744360902,0.481203007518797,0.703007518796992,0.729323308270677,0.633451957295374,0.616541353383459,0.708185053380783,0.434163701067616,0.693950177935943,0.654135338345864,0.511278195488722,0.476868327402135,0.79253112033195,0.858921161825726,0.714438694653115,0.423236514522822,0.879668049792531,0.692946058091287,0.821576763485477,0.75103734439834,0.726141078838174,0.575,0.755186721991701,0.767634854771784,0.576763485477178,0.686832740213523,0.409252669039146,0.554166666666667,0.758333333333333,0.530249110320285,0.437722419928826,0.5375,0.658362989323844,0.858333333333333,0.904166666666667,0.604982206405694,0.804270462633452,0.651245551601424,0.74812030075188,0.887218045112782,0.448132780082987,0.75187969924812,0.763157894736842,0.759398496240601,0.984962406015037,0.537366548042705,0.729323308270677,0.744360902255639,0.890977443609022,0.755639097744361,0.676691729323308,0.357142857142857,0.533834586466165,0.686832740213523,0.469924812030075,0.515037593984962,0.970833333333333,0.357142857142857,0.721804511278195,0.657894736842105,0.635338345864662,0.740601503759398,0.860902255639098,0.508896797153025,0.484962406015038,0.827067669172932,0.576512455516014,0.883458646616541,0.555160142348755,0.307053941908713,0.601503759398496,0.402255639097744,0.530075187969925,0.62781954887218,0.409252669039146,0.537593984962406,0.679166666666667,0.789473684210526,0.633333333333333,0.775800711743772,0.9644128113879,0.779359430604982,0.715302491103203,0.591666666666667,0.651452282157676,0.589211618257261,0.81578947368421,0.842105263157895,0.958646616541353,0.691729323308271,0.857142857142857,0.849624060150376,0.849624060150376,0.864661654135338,0.834586466165413,0.575187969924812,0.913533834586466,0.973684210526316,0.857142857142857,0.833333333333333,0.843416370106762,0.973276248982738,0.65,0.535269709543568,0.485175159977669,0.539419087136929,0.692946058091287,0.893238434163701,0.821576763485477,0.854771784232366,0.83402489626556,1,0.849624060150376,0.508333333333333,0.400484538341342,0.750889679715303,0.277702828696636,0.672597864768683,1,0.575187969924812,0.65,0.366666666666667,0.695833333333333,0.510373443983403,0.821576763485477,0.518672199170125,0.808333333333333,0.645833333333333,0.725,0.487544483985765,0.612781954887218,0.846975088967972,0.843416370106762,0.882562277580071,0.669039145907474,0.496240601503759,0.526315789473684,0.692946058091287,0.605809128630705,0.721991701244813,0.417293233082707,0.530075187969925,0.701067615658363,0.672932330827068,0.511608447488481,0.541353383458647,0.718045112781955,0.575,0.843416370106762,0.483985765124555,0.797153024911032,0.846975088967972,0.6125,0.720833333333333,0.593984962406015,0.620833333333333,0.620833333333333,0.629893238434164,0.666666666666667,0.576512455516014,0.616666666666667,0.579166666666667,0.55,0.566666666666667,0.476868327402135,0.620300751879699,0.75103734439834,0.733082706766917,0.421052631578947,0.79253112033195,0.726141078838174,0.843416370106762,0.697508896797153,0.612781954887218,0.537593984962406,0.750889679715303,0.519572953736655,0.629893238434164,0.515037593984962,0.843416370106762,0.693950177935943,0.811387900355872,0.567669172932331,0.5875,0.733333333333333,0.808333333333333,0.745833333333333,0.54356846473029,0.817427385892116,0.637010676156584,0.663900414937759,0.825726141078838,0.842323651452282,0.800829875518672,0.651452282157676,0.793594306049822,0.755186721991701,0.790035587188612,0.655601659751037,0.514522821576763,0.473309608540925,0.45643153526971,0.526970954356846,0.589211618257261,0.709543568464731,0.497925311203319,0.558718861209964,0.933333333333333,0.740213523131673,0.533807829181495,0.825622775800712,0.419928825622776,0.714285714285714,0.526690391459075,0.511278195488722,0.917293233082707,0.81203007518797,0.533834586466165,0.409774436090226,0.75187969924812,0.590225563909774,0.558333333333333,0.4875,0.276147734693639,0.608333333333333,0.601423487544484,0.505338078291815,0.342105263157895,0.661269244972759,0.576512455516014,0.81203007518797,0.650375939849624,0.616666666666667,1,0.914638773694226,0.813278008298755,0.72179831019792,0.81203007518797,0.42884893255837,0.813278008298755,0.654166666666667,0.770676691729323,0.620833333333333,0.807829181494662,0.838345864661654,0.403292632691219,0.540925266903915,0.483985765124555,0.729537366548043,0.683274021352313,0.676691729323308,0.441281138790036,0.533807829181495,0.759336099585062,0.666335526320268,0.800829875518672,0.850622406639004,0.693950177935943,0.729323308270677,0.793233082706767,0.873908856706646,0.565836298932385,0.556390977443609,0.93984962406015,0.872180451127819,0.879699248120301,0.548042704626335,0.430604982206406,0.579166666666667,0.558718861209964,0.483333333333333,0.5375,0.379166666666667,0.666666666666667,0.548042704626335,0.544483985765125,0.547717842323651,0.780082987551867,0.701067615658363,0.562277580071174,0.679715302491103,0.629893238434164,0.579166666666667,0.679166666666667,0.711743772241993,0.711743772241993,0.569395017793594,0.669039145907474,0.650375939849624,0.402255639097744,0.800751879699248,0.74812030075188,0.830827067669173,0.650375939849624,0.74812030075188,0.647686832740214,0.416666666666667,0.807829181494662,0.423236514522822,0.609958506224066,0.809128630705394,0.604166666666667,0.360995850622407,0.485477178423237,0.559616797537285,0.518796992481203,0.669039145907474,0.804270462633452,0.366666666666667,0.473309608540925,0.747330960854093,0.761565836298932,0.654166666666667,0.790035587188612,0.832740213523132,0.545833333333333,0.469924812030075,0.661921708185053,0.423487544483986,0.523131672597865,0.323651452282158,0.834586466165413,0.4,0.640569395017794,0.437722419928826,0.345833333333333,0.507518796992481,0.601423487544484,0.637010676156584,0.601423487544484,0.488721804511278,0.519572953736655,0.508896797153025,0.658362989323844,0.842323651452282,0.784232365145228,0.613012139895251,0.789473684210526,0.913533834586466,0.946058091286307,0.733082706766917,0.778195488721804,0.755639097744361,0.766917293233082,0.817427385892116,0.492481203007519,0.596659920911592,0.730290456431535,0.571428571428571,0.417293233082707,0.813278008298755,0.74812030075188,0.883458646616541,0.616541353383459,0.66804979253112,0.644128113879004,0.763485477178423,0.729166666666667,0.720833333333333,0.427046263345196,0.368421052631579,0.683333333333333,0.97508896797153,0.882562277580071,0.580071174377224,0.595833333333333,0.583333333333333,0.761565836298932,0.508896797153025,0.370833333333333,0.491666666666667,0.825622775800712,0.759398496240601,0.609022556390977,0.650375939849624,0.906015037593985,1,0.715302491103203,0.744360902255639,0.49784238665183,0.790035587188612,0.864661654135338,0.690391459074733,0.804511278195489,0.830226137809144,0.486500922885963,0.5625,0.569395017793594,0.7875,0.562277580071174,0.43984962406015,0.443609022556391,0.516014234875445,0.576512455516014,0.541353383458647,0.533333333333333,0.842105263157895,0.789473684210526,0.504166666666666,0.551601423487544,0.850533807829182,0.651245551601424,0.733082706766917,0.665480427046264,0.569395017793594,0.7,0.829181494661922],"weight":[43.7722419928826,59.0747330960854,75.4448398576513,67.6412241903577,70,62.5,51.0373443983403,50.1779359430605,46.8879668049792,63.0705394190871,68.6832740213523,60.8333333333333,59.7510373443983,77.9166666666666,70,91.25,77.9166666666666,56.6666666666667,68.75,49.4661921708185,50.8896797153025,52.6690391459075,74.0213523131673,87.1886120996441,82.9181494661922,95.8333333333333,83.6298932384342,78.2918149466192,92.9166666666667,86.1209964412811,76.25,73.3096085409253,69.7508896797153,57.6512455516014,76.6666666666666,39.2638105269495,83.2740213523132,55.8718861209964,56.2277580071174,58.3333333333333,56.390977443609,61.9217081850534,46.2406015037594,54.5833333333333,70.4626334519573,66.1654135338346,46.6666666666667,78.6476868327402,72.1804511278195,80.8270676691729,86.4661654135338,51.6014234875445,53.0075187969925,87.2180451127819,53.3807829181495,62.9166666666667,60.4166666666667,66.5413533834586,47.7443609022556,71.4285714285714,72.1804511278195,46.6192170818505,60.9588558405177,55.1867219917012,68.4210526315789,84.5864661654135,68.4210526315789,48.3985765124555,69.9248120300752,66.9172932330827,69.7508896797153,48.75,63.1578947368421,52.0833333333333,62.9893238434164,51.9572953736655,41.25,59.4306049822064,88.6120996441281,38.3333333333333,58.3629893238434,60.3839018112901,55.6016597510373,46.2406015037594,59.7510373443983,57.6763485477179,97.7391833622218,68.3181957061276,75.5639097744361,71.5302491103203,71.7842323651452,77.0676691729323,58.9211618257261,66.9039145907474,59.4306049822064,61.25,70,54.8872180451128,68.4210526315789,61.4107883817427,80.0751879699248,65.4135338345864,85.0622406639004,85.8333333333333,86.6666666666666,75.187969924812,64.1666666666666,67.9166666666667,68.0451127819549,76.25,71.0526315789474,57.1428571428571,85.0622406639004,77.0676691729323,74.5833333333333,76.6917293233082,74.1666666666667,68.4210526315789,84.9624060150376,58.75,92.5266903914591,80.4270462633452,50.9999388185068,78.75,48.4962406015038,47.3684210526316,79.5833333333333,66.6666666666667,74.7330960854093,65.1245551601424,73.3333333333333,69.3950177935943,73.3333333333333,83.75,70.4166666666667,64.1666666666666,73.75,77.2241992882562,77.5800711743772,38.5892116182572,77.5800711743772,72.9537366548043,77.2241992882562,66.1921708185053,80.7829181494662,44.8398576512456,46.9750889679715,34.4398340248963,79.7153024911032,83.9857651245552,62.6334519572954,67.2597864768683,55.6016597510373,66.9039145907474,71.8861209964413,43.984962406015,60.4982206405694,53.3807829181495,60.8333333333333,50.1779359430605,42.2753292383145,69.1729323308271,71.8045112781955,55.1601423487544,78.1954887218045,74.0213523131673,60.8333333333333,84.5864661654135,72.9537366548043,89.2116182572614,63.75,77.1784232365145,55.4166666666667,67.9166666666667,63.5338345864662,52.6315789473684,52.6315789473684,61.6541353383459,67.0833333333333,58.0071174377224,74.5833333333333,64.7686832740214,88.9679715302491,62.0833333333333,35,75.4448398576513,77.9359430604982,50,65.4804270462634,93.5943060498221,87.1886120996441,65.8362989323844,47.6868327402135,79.6992481203007,57.2953736654804,65.4135338345864,50,59.3984962406015,45.4887218045113,64.2857142857143,85.7142857142857,71.5302491103203,35,73.6654804270463,55.4166666666667,56.390977443609,45.5516014234876,41.3533834586466,42.4812030075188,33.609958506224,58.0071174377224,77.9359430604982,69.7508896797153,56.5836298932385,74.3772241992883,67.2597864768683,80.7829181494662,59.7744360902255,60.8540925266904,64.4128113879004,67.9715302491103,59.3984962406015,74.7330960854093,66.5413533834586,69.1729323308271,81.25,53.3834586466165,81.7427385892116,63.9097744360902,64.6616541353383,55.4166666666667,66.6666666666667,54.1353383458647,53.0075187969925,53.3834586466165,61.6541353383459,53.3834586466165,41.9928825622776,57.2953736654804,61.5658362989324,75.4448398576513,75.4448398576513,44.1666666666667,64.4128113879004,72.9537366548043,56.2277580071174,43.8797268895434,88.9679715302491,47.7443609022556,48.3333333333333,69.7508896797153,64.4128113879004,58.50622406639,80.4166666666667,72.9166666666667,67.0833333333333,66.3900414937759,57.6763485477179,60.5809128630705,65,65.8333333333333,58.3333333333333,75.8333333333333,68.75,75.8333333333333,71.8045112781955,59.0225563909774,42.4812030075188,64.0569395017794,49.8220640569395,58.6466165413534,42.4812030075188,30.2904564315352,46.9750889679715,68.8697562915721,71.5302491103203,47.3029045643154,74.4360902255639,83.75,62.6334519572954,83.75,57.1428571428571,63.1578947368421,72.8885995664836,62.5,70.8185053380783,50.9735666073672,74.4360902255639,72.1804511278195,68.0451127819549,65.8333333333333,48.4962406015038,68.0451127819549,59.7744360902255,63.5338345864662,63.1578947368421,85.3383458646616,70.6766917293233,71.7665619621052,65.8362989323844,69.3950177935943,37.593984962406,65.8362989323844,85.0533807829182,51.1278195488722,50.7518796992481,81.8505338078292,87.9003558718861,63.7010676156584,63.3451957295374,69.7508896797153,54.4483985765125,81.8505338078292,38.3458646616541,65.8362989323844,65.1245551601424,70.8185053380783,44.1281138790036,93.2330827067669,76.6917293233082,100,70.3007518796992,90.6015037593985,83.4586466165413,48.7544483985765,84.5864661654135,68.796992481203,90.2255639097744,78.4232365145228,87.593984962406,61.2781954887218,47.3029045643154,67.6348547717842,66.1654135338346,70.6526750482646,73.3082706766917,74.4360902255639,75.9398496240601,41.9087136929461,80.0751879699248,59.7864768683274,41.9928825622776,45.9074733096086,64.4128113879004,68.6832740213523,81.4946619217082,65.1245551601424,46.6192170818505,54.0925266903915,97.3684210526316,80.0751879699248,88.7218045112782,65.4135338345864,89.4736842105263,97.7443609022556,74.4360902255639,80.4511278195489,82.3308270676691,50,92.4812030075188,83.8345864661654,81.578947368421,61.6541353383459,90.2255639097744,56.0150375939849,75.0889679715303,61.6541353383459,54.5112781954887,74.4360902255639,52.2556390977443,86.25,64.5833333333333,46.0580912863071,69.1666666666667,52.0833333333333,53.3834586466165,46.2406015037594,65,79.5833333333333,57.6512455516014,56.7669172932331,56.0150375939849,57.5187969924812,74.3772241992883,93.9501779359431,81.1387900355872,90.7473309608541,100,67.2932330827068,92.5266903914591,73.3096085409253,65.4135338345864,79.3594306049822,55.2631578947368,77.9359430604982,93.5943060498221,71.0526315789474,46.6666666666667,46.9924812030075,82.8914368712286,53.3834586466165,55.6016597510373,39.4977547668225,69.7095435684647,80.4511278195489,96.085409252669,53.7366548042705,40.6639004149378,88.6120996441281,72.9166666666667,52.6315789473684,61.6666666666667,62.0833333333333,80.8270676691729,54.5112781954887,59.7744360902255,36.6666666666667,53.3333333333333,46.25,69.1666666666667,71.1743772241993,61.9217081850534,74.0213523131673,72.5978647686833,86.7219917012448,78.6476868327402,95.4887218045113,77.8195488721804,90.8713692946058,88.7218045112782,67.6691729323308,90.6015037593985,92.5311203319502,97.7443609022556,100,82.5622775800712,98.8721804511278,70.9543568464731,89.8496240601504,88.7966804979254,87.593984962406,77.4436090225564,100,87.136929460581,72.8242377794158,82.9181494661922,68.3274021352313,69.3950177935943,46.6165413533835,85.7651245551602,86.4768683274022,53.3807829181495,55.8718861209964,69.5488721804511,65.7894736842105,70.3007518796992,80.4270462633452,73.3082706766917,51.5630077772378,70.4626334519573,61.2781954887218,81.578947368421,65.8362989323844,55.6390977443609,74.5833333333333,46.9924812030075,40.2255639097744,65.1245551601424,77.5933609958506,69.7095435684647,62.655601659751,43.609022556391,41.2811387900356,50.5338078291815,73.6654804270463,71.5302491103203,51.8796992481203,52.9166666666666,41.25,37.218045112782,72.9323308270677,50,76.1565836298932,66.1921708185053,69.0391459074733,42.7046263345196,75.103734439834,72.1991701244814,71.8861209964413,49.4661921708185,72.9323308270677,72.1804511278195,72.9323308270677,88.7218045112782,81.578947368421,72.9323308270677,42.518138678798,69.9298189139153,80.4979253112033,65.5601659751037,74.7330960854093,87.2180451127819,91.2863070539419,69.0391459074733,62.781954887218,41.4801203432703,67.9166666666667,34.5864661654135,70,77.5933609958506,53.1120331950207,37.9699248120301,88.3817427385893,84.6473029045643,80.4979253112033,59.1666666666667,76.6917293233082,100,72.9166666666667,59.7744360902255,84.6975088967972,53.0249110320285,53.3807829181495,58.3629893238434,60.4982206405694,55.8718861209964,94.0632401619034,40.2209894890872,94.4990593334269,45.1957295373666,71.8045112781955,60.9958506224066,45.4887218045113,74.7330960854093,67.9715302491103,71.7842323651452,52.2821576763486,85.3383458646616,83.7682503017347,77.0676691729323,87.1886120996441,74.8120300751879,92.8825622775801,68.0497925311203,86.4768683274022,39.8340248962655,73.9923358115188,81.7427385892116,70.3007518796992,57.874624843467,83.402489626556,100,98.576512455516,70.4626334519573,76.5288813971966,70.8185053380783,63.3451957295374,56.390977443609,50.4166666666666,73.8131715830576,79.3233082706767,51.8796992481203,80.0829875518672,100,85.8921161825726,83.8174273858921,73.75,100,79.1666666666667,80.8270676691729,100,76.3485477178423,43.8421341746006,79.6680497925311,79.1666666666667,65.8333333333333,83.3333333333333,79.523638501336,77.0833333333333,71.8861209964413,49.8220640569395,75.0889679715303,61.9217081850534,45.5516014234876,42.7046263345196,48.3985765124555,71.8045112781955,37.218045112782,76.3157894736842,73.6842105263158,68.4210526315789,75.5639097744361,88.0341981837404,56.7669172932331,56.2277580071174,83.0827067669173,95.0663025381366,71.0526315789474,38.5892116182572,64.3153526970954,64.7302904564315,66.5480427046264,83.6298932384342,71.1743772241993,72.5978647686833,69.0391459074733,85.0533807829182,85.4166666666667,90.3914590747331,71.8861209964413,46.6165413533835,65.4804270462634,72.5978647686833,68.3274021352313,80.0711743772242,76.5124555160142,77.5800711743772,53.3807829181495,58.6466165413534,70.1067615658363,60.1503759398496,61.6541353383459,48.4962406015038,96.44128113879,85.7651245551602,64.4128113879004,91.4590747330961,70.1067615658363,71.1743772241993,66.1921708185053,85.0533807829182,36.4661654135338,83.6298932384342,79.0035587188612,81.8505338078292,67.6156583629894,52.2556390977443,70.8185053380783,60.5263157894737,80.7829181494662,62.3529566761572,65.4804270462634,92.0833333333333,91.6666666666667,65.4166666666667,84.5833333333333,81.6666666666667,62.2775800711744,76.6666666666666,76.6666666666666,84.9624060150376,44.7368421052632,80.8270676691729,59.3984962406015,44.1666666666667,71.5302491103203,68.796992481203,55.8333333333333,71.1743772241993,66.5413533834586,67.2932330827068,55.6390977443609,60.8540925266904,64.6515642866656,62.2775800711744,72.5978647686833,64.0569395017794,51.9572953736655,46.2633451957295,62.781954887218,48.0427046263345,61.5658362989324,68.3274021352313,56.5836298932385,37.9699248120301,40.9252669039146,54.1353383458647,39.0963486846373,60.1423487544484,62.2775800711744,62.2775800711744,69.1666666666667,77.4436090225564,91.7293233082707,47.6868327402135,62.9893238434164,69.7508896797153,74.3772241992883,43.7722419928826,61.5658362989324,75.5639097744361,51.25,71.0526315789474,70.8185053380783,47.3684210526316,63.9097744360902,48.1203007518797,70.3007518796992,72.9323308270677,63.3451957295374,61.6541353383459,70.8185053380783,43.4163701067616,69.3950177935943,65.4135338345864,51.1278195488722,47.6868327402135,79.253112033195,85.8921161825726,71.4438694653115,42.3236514522822,87.9668049792531,69.2946058091287,82.1576763485478,75.103734439834,72.6141078838175,57.5,75.5186721991701,76.7634854771784,57.6763485477179,68.6832740213523,40.9252669039146,55.4166666666667,75.8333333333333,53.0249110320285,43.7722419928826,53.75,65.8362989323844,85.8333333333333,90.4166666666667,60.4982206405694,80.4270462633452,65.1245551601424,74.8120300751879,88.7218045112782,44.8132780082987,75.187969924812,76.3157894736842,75.9398496240601,98.4962406015037,53.7366548042705,72.9323308270677,74.4360902255639,89.0977443609022,75.5639097744361,67.6691729323308,35.7142857142857,53.3834586466165,68.6832740213523,46.9924812030075,51.5037593984962,97.0833333333333,35.7142857142857,72.1804511278195,65.7894736842105,63.5338345864662,74.0601503759398,86.0902255639098,50.8896797153025,48.4962406015038,82.7067669172932,57.6512455516014,88.3458646616541,55.5160142348755,30.7053941908713,60.1503759398496,40.2255639097744,53.0075187969925,62.781954887218,40.9252669039146,53.7593984962406,67.9166666666667,78.9473684210526,63.3333333333333,77.5800711743772,96.44128113879,77.9359430604982,71.5302491103203,59.1666666666667,65.1452282157676,58.9211618257261,81.578947368421,84.2105263157895,95.8646616541353,69.1729323308271,85.7142857142857,84.9624060150376,84.9624060150376,86.4661654135338,83.4586466165413,57.5187969924812,91.3533834586466,97.3684210526316,85.7142857142857,83.3333333333333,84.3416370106762,97.3276248982738,65,53.5269709543568,48.5175159977669,53.9419087136929,69.2946058091287,89.3238434163701,82.1576763485478,85.4771784232366,83.402489626556,100,84.9624060150376,50.8333333333333,40.0484538341342,75.0889679715303,27.7702828696636,67.2597864768683,100,57.5187969924812,65,36.6666666666667,69.5833333333333,51.0373443983403,82.1576763485478,51.8672199170125,80.8333333333333,64.5833333333333,72.5,48.7544483985765,61.2781954887218,84.6975088967972,84.3416370106762,88.2562277580071,66.9039145907474,49.6240601503759,52.6315789473684,69.2946058091287,60.5809128630705,72.1991701244814,41.7293233082707,53.0075187969925,70.1067615658363,67.2932330827068,51.1608447488481,54.1353383458647,71.8045112781955,57.5,84.3416370106762,48.3985765124555,79.7153024911032,84.6975088967972,61.25,72.0833333333333,59.3984962406015,62.0833333333333,62.0833333333333,62.9893238434164,66.6666666666667,57.6512455516014,61.6666666666667,57.9166666666667,55,56.6666666666667,47.6868327402135,62.0300751879699,75.103734439834,73.3082706766917,42.1052631578947,79.253112033195,72.6141078838175,84.3416370106762,69.7508896797153,61.2781954887218,53.7593984962406,75.0889679715303,51.9572953736655,62.9893238434164,51.5037593984962,84.3416370106762,69.3950177935943,81.1387900355872,56.7669172932331,58.75,73.3333333333333,80.8333333333333,74.5833333333333,54.356846473029,81.7427385892116,63.7010676156584,66.3900414937759,82.5726141078838,84.2323651452282,80.0829875518672,65.1452282157676,79.3594306049822,75.5186721991701,79.0035587188612,65.5601659751037,51.4522821576763,47.3309608540925,45.643153526971,52.6970954356846,58.9211618257261,70.9543568464731,49.7925311203319,55.8718861209964,93.3333333333333,74.0213523131673,53.3807829181495,82.5622775800712,41.9928825622776,71.4285714285714,52.6690391459075,51.1278195488722,91.7293233082707,81.203007518797,53.3834586466165,40.9774436090226,75.187969924812,59.0225563909774,55.8333333333333,48.75,27.6147734693639,60.8333333333333,60.1423487544484,50.5338078291815,34.2105263157895,66.1269244972759,57.6512455516014,81.203007518797,65.0375939849624,61.6666666666667,100,91.4638773694226,81.3278008298755,72.179831019792,81.203007518797,42.884893255837,81.3278008298755,65.4166666666667,77.0676691729323,62.0833333333333,80.7829181494662,83.8345864661654,40.3292632691219,54.0925266903915,48.3985765124555,72.9537366548043,68.3274021352313,67.6691729323308,44.1281138790036,53.3807829181495,75.9336099585062,66.6335526320268,80.0829875518672,85.0622406639004,69.3950177935943,72.9323308270677,79.3233082706767,87.3908856706646,56.5836298932385,55.6390977443609,93.984962406015,87.2180451127819,87.9699248120301,54.8042704626335,43.0604982206406,57.9166666666667,55.8718861209964,48.3333333333333,53.75,37.9166666666667,66.6666666666667,54.8042704626335,54.4483985765125,54.7717842323651,78.0082987551867,70.1067615658363,56.2277580071174,67.9715302491103,62.9893238434164,57.9166666666667,67.9166666666667,71.1743772241993,71.1743772241993,56.9395017793594,66.9039145907474,65.0375939849624,40.2255639097744,80.0751879699248,74.8120300751879,83.0827067669173,65.0375939849624,74.8120300751879,64.7686832740214,41.6666666666667,80.7829181494662,42.3236514522822,60.9958506224066,80.9128630705394,60.4166666666667,36.0995850622407,48.5477178423236,55.9616797537285,51.8796992481203,66.9039145907474,80.4270462633452,36.6666666666667,47.3309608540925,74.7330960854093,76.1565836298932,65.4166666666667,79.0035587188612,83.2740213523132,54.5833333333333,46.9924812030075,66.1921708185053,42.3487544483986,52.3131672597865,32.3651452282158,83.4586466165413,40,64.0569395017794,43.7722419928826,34.5833333333333,50.7518796992481,60.1423487544484,63.7010676156584,60.1423487544484,48.8721804511278,51.9572953736655,50.8896797153025,65.8362989323844,84.2323651452282,78.4232365145228,61.3012139895251,78.9473684210526,91.3533834586466,94.6058091286307,73.3082706766917,77.8195488721804,75.5639097744361,76.6917293233082,81.7427385892116,49.2481203007519,59.6659920911592,73.0290456431535,57.1428571428571,41.7293233082707,81.3278008298755,74.8120300751879,88.3458646616541,61.6541353383459,66.804979253112,64.4128113879004,76.3485477178423,72.9166666666667,72.0833333333333,42.7046263345196,36.8421052631579,68.3333333333333,97.508896797153,88.2562277580071,58.0071174377224,59.5833333333333,58.3333333333333,76.1565836298932,50.8896797153025,37.0833333333333,49.1666666666667,82.5622775800712,75.9398496240601,60.9022556390977,65.0375939849624,90.6015037593985,100,71.5302491103203,74.4360902255639,49.784238665183,79.0035587188612,86.4661654135338,69.0391459074733,80.4511278195489,83.0226137809144,48.6500922885963,56.25,56.9395017793594,78.75,56.2277580071174,43.984962406015,44.3609022556391,51.6014234875445,57.6512455516014,54.1353383458647,53.3333333333333,84.2105263157895,78.9473684210526,50.4166666666666,55.1601423487544,85.0533807829182,65.1245551601424,73.3082706766917,66.5480427046264,56.9395017793594,70,82.9181494661922],"length":[0.437722419928826,0.590747330960854,0.754448398576513,0.676412241903577,0.7,0.625,0.510373443983403,0.501779359430605,0.468879668049793,0.630705394190871,0.686832740213523,0.608333333333333,0.597510373443983,0.779166666666666,0.7,0.9125,0.779166666666666,0.566666666666667,0.6875,0.494661921708185,0.508896797153025,0.526690391459075,0.740213523131673,0.871886120996441,0.829181494661922,0.958333333333333,0.836298932384342,0.782918149466192,0.929166666666667,0.861209964412811,0.7625,0.733096085409253,0.697508896797153,0.576512455516014,0.766666666666666,0.392638105269495,0.832740213523132,0.558718861209964,0.562277580071174,0.583333333333333,0.56390977443609,0.619217081850534,0.462406015037594,0.545833333333333,0.704626334519573,0.661654135338346,0.466666666666667,0.786476868327402,0.721804511278195,0.808270676691729,0.864661654135338,0.516014234875445,0.530075187969925,0.872180451127819,0.533807829181495,0.629166666666667,0.604166666666667,0.665413533834586,0.477443609022556,0.714285714285714,0.721804511278195,0.466192170818505,0.609588558405177,0.551867219917012,0.684210526315789,0.845864661654135,0.684210526315789,0.483985765124555,0.699248120300752,0.669172932330827,0.697508896797153,0.4875,0.631578947368421,0.520833333333333,0.629893238434164,0.519572953736655,0.4125,0.594306049822064,0.886120996441281,0.383333333333333,0.583629893238434,0.603839018112901,0.556016597510373,0.462406015037594,0.597510373443983,0.576763485477178,0.977391833622218,0.683181957061276,0.755639097744361,0.715302491103203,0.717842323651452,0.770676691729323,0.589211618257261,0.669039145907474,0.594306049822064,0.6125,0.7,0.548872180451128,0.684210526315789,0.614107883817427,0.800751879699248,0.654135338345864,0.850622406639004,0.858333333333333,0.866666666666666,0.75187969924812,0.641666666666666,0.679166666666667,0.680451127819549,0.7625,0.710526315789474,0.571428571428571,0.850622406639004,0.770676691729323,0.745833333333333,0.766917293233082,0.741666666666667,0.684210526315789,0.849624060150376,0.5875,0.925266903914591,0.804270462633452,0.509999388185068,0.7875,0.484962406015038,0.473684210526316,0.795833333333333,0.666666666666667,0.747330960854093,0.651245551601424,0.733333333333333,0.693950177935943,0.733333333333333,0.8375,0.704166666666667,0.641666666666666,0.7375,0.772241992882562,0.775800711743772,0.385892116182572,0.775800711743772,0.729537366548043,0.772241992882562,0.661921708185053,0.807829181494662,0.448398576512456,0.469750889679715,0.344398340248963,0.797153024911032,0.839857651245552,0.626334519572954,0.672597864768683,0.556016597510373,0.669039145907474,0.718861209964413,0.43984962406015,0.604982206405694,0.533807829181495,0.608333333333333,0.501779359430605,0.422753292383145,0.691729323308271,0.718045112781955,0.551601423487544,0.781954887218045,0.740213523131673,0.608333333333333,0.845864661654135,0.729537366548043,0.892116182572614,0.6375,0.771784232365145,0.554166666666667,0.679166666666667,0.635338345864662,0.526315789473684,0.526315789473684,0.616541353383459,0.670833333333333,0.580071174377224,0.745833333333333,0.647686832740214,0.889679715302491,0.620833333333333,0.35,0.754448398576513,0.779359430604982,0.5,0.654804270462634,0.935943060498221,0.871886120996441,0.658362989323844,0.476868327402135,0.796992481203007,0.572953736654804,0.654135338345864,0.5,0.593984962406015,0.454887218045113,0.642857142857143,0.857142857142857,0.715302491103203,0.35,0.736654804270463,0.554166666666667,0.56390977443609,0.455516014234876,0.413533834586466,0.424812030075188,0.33609958506224,0.580071174377224,0.779359430604982,0.697508896797153,0.565836298932385,0.743772241992883,0.672597864768683,0.807829181494662,0.597744360902255,0.608540925266904,0.644128113879004,0.679715302491103,0.593984962406015,0.747330960854093,0.665413533834586,0.691729323308271,0.8125,0.533834586466165,0.817427385892116,0.639097744360902,0.646616541353383,0.554166666666667,0.666666666666667,0.541353383458647,0.530075187969925,0.533834586466165,0.616541353383459,0.533834586466165,0.419928825622776,0.572953736654804,0.615658362989324,0.754448398576513,0.754448398576513,0.441666666666667,0.644128113879004,0.729537366548043,0.562277580071174,0.438797268895434,0.889679715302491,0.477443609022556,0.483333333333333,0.697508896797153,0.644128113879004,0.5850622406639,0.804166666666667,0.729166666666667,0.670833333333333,0.663900414937759,0.576763485477178,0.605809128630705,0.65,0.658333333333333,0.583333333333333,0.758333333333333,0.6875,0.758333333333333,0.718045112781955,0.590225563909774,0.424812030075188,0.640569395017794,0.498220640569395,0.586466165413534,0.424812030075188,0.302904564315352,0.469750889679715,0.688697562915721,0.715302491103203,0.473029045643154,0.744360902255639,0.8375,0.626334519572954,0.8375,0.571428571428571,0.631578947368421,0.728885995664836,0.625,0.708185053380783,0.509735666073672,0.744360902255639,0.721804511278195,0.680451127819549,0.658333333333333,0.484962406015038,0.680451127819549,0.597744360902255,0.635338345864662,0.631578947368421,0.853383458646616,0.706766917293233,0.717665619621052,0.658362989323844,0.693950177935943,0.37593984962406,0.658362989323844,0.850533807829182,0.511278195488722,0.507518796992481,0.818505338078292,0.879003558718861,0.637010676156584,0.633451957295374,0.697508896797153,0.544483985765125,0.818505338078292,0.383458646616541,0.658362989323844,0.651245551601424,0.708185053380783,0.441281138790036,0.932330827067669,0.766917293233082,1,0.703007518796992,0.906015037593985,0.834586466165413,0.487544483985765,0.845864661654135,0.68796992481203,0.902255639097744,0.784232365145228,0.87593984962406,0.612781954887218,0.473029045643154,0.676348547717842,0.661654135338346,0.706526750482646,0.733082706766917,0.744360902255639,0.759398496240601,0.41908713692946,0.800751879699248,0.597864768683274,0.419928825622776,0.459074733096086,0.644128113879004,0.686832740213523,0.814946619217082,0.651245551601424,0.466192170818505,0.540925266903915,0.973684210526316,0.800751879699248,0.887218045112782,0.654135338345864,0.894736842105263,0.977443609022556,0.744360902255639,0.804511278195489,0.823308270676692,0.5,0.924812030075188,0.838345864661654,0.81578947368421,0.616541353383459,0.902255639097744,0.560150375939849,0.750889679715303,0.616541353383459,0.545112781954887,0.744360902255639,0.522556390977443,0.8625,0.645833333333333,0.460580912863071,0.691666666666667,0.520833333333333,0.533834586466165,0.462406015037594,0.65,0.795833333333333,0.576512455516014,0.567669172932331,0.560150375939849,0.575187969924812,0.743772241992883,0.939501779359431,0.811387900355872,0.907473309608541,1,0.672932330827068,0.925266903914591,0.733096085409253,0.654135338345864,0.793594306049822,0.552631578947368,0.779359430604982,0.935943060498221,0.710526315789474,0.466666666666667,0.469924812030075,0.828914368712286,0.533834586466165,0.556016597510373,0.394977547668225,0.697095435684647,0.804511278195489,0.96085409252669,0.537366548042705,0.406639004149378,0.886120996441281,0.729166666666667,0.526315789473684,0.616666666666667,0.620833333333333,0.808270676691729,0.545112781954887,0.597744360902255,0.366666666666667,0.533333333333333,0.4625,0.691666666666667,0.711743772241993,0.619217081850534,0.740213523131673,0.725978647686833,0.867219917012448,0.786476868327402,0.954887218045113,0.778195488721804,0.908713692946058,0.887218045112782,0.676691729323308,0.906015037593985,0.925311203319502,0.977443609022556,1,0.825622775800712,0.988721804511278,0.709543568464731,0.898496240601504,0.887966804979254,0.87593984962406,0.774436090225564,1,0.87136929460581,0.728242377794158,0.829181494661922,0.683274021352313,0.693950177935943,0.466165413533835,0.857651245551602,0.864768683274022,0.533807829181495,0.558718861209964,0.695488721804511,0.657894736842105,0.703007518796992,0.804270462633452,0.733082706766917,0.515630077772378,0.704626334519573,0.612781954887218,0.81578947368421,0.658362989323844,0.556390977443609,0.745833333333333,0.469924812030075,0.402255639097744,0.651245551601424,0.775933609958506,0.697095435684647,0.62655601659751,0.43609022556391,0.412811387900356,0.505338078291815,0.736654804270463,0.715302491103203,0.518796992481203,0.529166666666666,0.4125,0.37218045112782,0.729323308270677,0.5,0.761565836298932,0.661921708185053,0.690391459074733,0.427046263345196,0.75103734439834,0.721991701244813,0.718861209964413,0.494661921708185,0.729323308270677,0.721804511278195,0.729323308270677,0.887218045112782,0.81578947368421,0.729323308270677,0.42518138678798,0.699298189139153,0.804979253112033,0.655601659751037,0.747330960854093,0.872180451127819,0.912863070539419,0.690391459074733,0.62781954887218,0.414801203432703,0.679166666666667,0.345864661654135,0.7,0.775933609958506,0.531120331950207,0.379699248120301,0.883817427385893,0.846473029045643,0.804979253112033,0.591666666666667,0.766917293233082,1,0.729166666666667,0.597744360902255,0.846975088967972,0.530249110320285,0.533807829181495,0.583629893238434,0.604982206405694,0.558718861209964,0.940632401619034,0.402209894890872,0.944990593334269,0.451957295373666,0.718045112781955,0.609958506224066,0.454887218045113,0.747330960854093,0.679715302491103,0.717842323651452,0.522821576763486,0.853383458646616,0.837682503017347,0.770676691729323,0.871886120996441,0.74812030075188,0.928825622775801,0.680497925311203,0.864768683274022,0.398340248962655,0.739923358115188,0.817427385892116,0.703007518796992,0.57874624843467,0.83402489626556,1,0.98576512455516,0.704626334519573,0.765288813971966,0.708185053380783,0.633451957295374,0.56390977443609,0.504166666666666,0.738131715830576,0.793233082706767,0.518796992481203,0.800829875518672,1,0.858921161825726,0.838174273858922,0.7375,1,0.791666666666667,0.808270676691729,1,0.763485477178423,0.438421341746006,0.796680497925311,0.791666666666667,0.658333333333333,0.833333333333333,0.79523638501336,0.770833333333333,0.718861209964413,0.498220640569395,0.750889679715303,0.619217081850534,0.455516014234876,0.427046263345196,0.483985765124555,0.718045112781955,0.37218045112782,0.763157894736842,0.736842105263158,0.684210526315789,0.755639097744361,0.880341981837404,0.567669172932331,0.562277580071174,0.830827067669173,0.950663025381366,0.710526315789474,0.385892116182572,0.643153526970955,0.647302904564315,0.665480427046264,0.836298932384342,0.711743772241993,0.725978647686833,0.690391459074733,0.850533807829182,0.854166666666667,0.903914590747331,0.718861209964413,0.466165413533835,0.654804270462634,0.725978647686833,0.683274021352313,0.800711743772242,0.765124555160142,0.775800711743772,0.533807829181495,0.586466165413534,0.701067615658363,0.601503759398496,0.616541353383459,0.484962406015038,0.9644128113879,0.857651245551602,0.644128113879004,0.914590747330961,0.701067615658363,0.711743772241993,0.661921708185053,0.850533807829182,0.364661654135338,0.836298932384342,0.790035587188612,0.818505338078292,0.676156583629894,0.522556390977443,0.708185053380783,0.605263157894737,0.807829181494662,0.623529566761572,0.654804270462634,0.920833333333333,0.916666666666667,0.654166666666667,0.845833333333333,0.816666666666667,0.622775800711744,0.766666666666666,0.766666666666666,0.849624060150376,0.447368421052631,0.808270676691729,0.593984962406015,0.441666666666667,0.715302491103203,0.68796992481203,0.558333333333333,0.711743772241993,0.665413533834586,0.672932330827068,0.556390977443609,0.608540925266904,0.646515642866656,0.622775800711744,0.725978647686833,0.640569395017794,0.519572953736655,0.462633451957295,0.62781954887218,0.480427046263345,0.615658362989324,0.683274021352313,0.565836298932385,0.379699248120301,0.409252669039146,0.541353383458647,0.390963486846373,0.601423487544484,0.622775800711744,0.622775800711744,0.691666666666667,0.774436090225564,0.917293233082707,0.476868327402135,0.629893238434164,0.697508896797153,0.743772241992883,0.437722419928826,0.615658362989324,0.755639097744361,0.5125,0.710526315789474,0.708185053380783,0.473684210526316,0.639097744360902,0.481203007518797,0.703007518796992,0.729323308270677,0.633451957295374,0.616541353383459,0.708185053380783,0.434163701067616,0.693950177935943,0.654135338345864,0.511278195488722,0.476868327402135,0.79253112033195,0.858921161825726,0.714438694653115,0.423236514522822,0.879668049792531,0.692946058091287,0.821576763485477,0.75103734439834,0.726141078838174,0.575,0.755186721991701,0.767634854771784,0.576763485477178,0.686832740213523,0.409252669039146,0.554166666666667,0.758333333333333,0.530249110320285,0.437722419928826,0.5375,0.658362989323844,0.858333333333333,0.904166666666667,0.604982206405694,0.804270462633452,0.651245551601424,0.74812030075188,0.887218045112782,0.448132780082987,0.75187969924812,0.763157894736842,0.759398496240601,0.984962406015037,0.537366548042705,0.729323308270677,0.744360902255639,0.890977443609022,0.755639097744361,0.676691729323308,0.357142857142857,0.533834586466165,0.686832740213523,0.469924812030075,0.515037593984962,0.970833333333333,0.357142857142857,0.721804511278195,0.657894736842105,0.635338345864662,0.740601503759398,0.860902255639098,0.508896797153025,0.484962406015038,0.827067669172932,0.576512455516014,0.883458646616541,0.555160142348755,0.307053941908713,0.601503759398496,0.402255639097744,0.530075187969925,0.62781954887218,0.409252669039146,0.537593984962406,0.679166666666667,0.789473684210526,0.633333333333333,0.775800711743772,0.9644128113879,0.779359430604982,0.715302491103203,0.591666666666667,0.651452282157676,0.589211618257261,0.81578947368421,0.842105263157895,0.958646616541353,0.691729323308271,0.857142857142857,0.849624060150376,0.849624060150376,0.864661654135338,0.834586466165413,0.575187969924812,0.913533834586466,0.973684210526316,0.857142857142857,0.833333333333333,0.843416370106762,0.973276248982738,0.65,0.535269709543568,0.485175159977669,0.539419087136929,0.692946058091287,0.893238434163701,0.821576763485477,0.854771784232366,0.83402489626556,1,0.849624060150376,0.508333333333333,0.400484538341342,0.750889679715303,0.277702828696636,0.672597864768683,1,0.575187969924812,0.65,0.366666666666667,0.695833333333333,0.510373443983403,0.821576763485477,0.518672199170125,0.808333333333333,0.645833333333333,0.725,0.487544483985765,0.612781954887218,0.846975088967972,0.843416370106762,0.882562277580071,0.669039145907474,0.496240601503759,0.526315789473684,0.692946058091287,0.605809128630705,0.721991701244813,0.417293233082707,0.530075187969925,0.701067615658363,0.672932330827068,0.511608447488481,0.541353383458647,0.718045112781955,0.575,0.843416370106762,0.483985765124555,0.797153024911032,0.846975088967972,0.6125,0.720833333333333,0.593984962406015,0.620833333333333,0.620833333333333,0.629893238434164,0.666666666666667,0.576512455516014,0.616666666666667,0.579166666666667,0.55,0.566666666666667,0.476868327402135,0.620300751879699,0.75103734439834,0.733082706766917,0.421052631578947,0.79253112033195,0.726141078838174,0.843416370106762,0.697508896797153,0.612781954887218,0.537593984962406,0.750889679715303,0.519572953736655,0.629893238434164,0.515037593984962,0.843416370106762,0.693950177935943,0.811387900355872,0.567669172932331,0.5875,0.733333333333333,0.808333333333333,0.745833333333333,0.54356846473029,0.817427385892116,0.637010676156584,0.663900414937759,0.825726141078838,0.842323651452282,0.800829875518672,0.651452282157676,0.793594306049822,0.755186721991701,0.790035587188612,0.655601659751037,0.514522821576763,0.473309608540925,0.45643153526971,0.526970954356846,0.589211618257261,0.709543568464731,0.497925311203319,0.558718861209964,0.933333333333333,0.740213523131673,0.533807829181495,0.825622775800712,0.419928825622776,0.714285714285714,0.526690391459075,0.511278195488722,0.917293233082707,0.81203007518797,0.533834586466165,0.409774436090226,0.75187969924812,0.590225563909774,0.558333333333333,0.4875,0.276147734693639,0.608333333333333,0.601423487544484,0.505338078291815,0.342105263157895,0.661269244972759,0.576512455516014,0.81203007518797,0.650375939849624,0.616666666666667,1,0.914638773694226,0.813278008298755,0.72179831019792,0.81203007518797,0.42884893255837,0.813278008298755,0.654166666666667,0.770676691729323,0.620833333333333,0.807829181494662,0.838345864661654,0.403292632691219,0.540925266903915,0.483985765124555,0.729537366548043,0.683274021352313,0.676691729323308,0.441281138790036,0.533807829181495,0.759336099585062,0.666335526320268,0.800829875518672,0.850622406639004,0.693950177935943,0.729323308270677,0.793233082706767,0.873908856706646,0.565836298932385,0.556390977443609,0.93984962406015,0.872180451127819,0.879699248120301,0.548042704626335,0.430604982206406,0.579166666666667,0.558718861209964,0.483333333333333,0.5375,0.379166666666667,0.666666666666667,0.548042704626335,0.544483985765125,0.547717842323651,0.780082987551867,0.701067615658363,0.562277580071174,0.679715302491103,0.629893238434164,0.579166666666667,0.679166666666667,0.711743772241993,0.711743772241993,0.569395017793594,0.669039145907474,0.650375939849624,0.402255639097744,0.800751879699248,0.74812030075188,0.830827067669173,0.650375939849624,0.74812030075188,0.647686832740214,0.416666666666667,0.807829181494662,0.423236514522822,0.609958506224066,0.809128630705394,0.604166666666667,0.360995850622407,0.485477178423237,0.559616797537285,0.518796992481203,0.669039145907474,0.804270462633452,0.366666666666667,0.473309608540925,0.747330960854093,0.761565836298932,0.654166666666667,0.790035587188612,0.832740213523132,0.545833333333333,0.469924812030075,0.661921708185053,0.423487544483986,0.523131672597865,0.323651452282158,0.834586466165413,0.4,0.640569395017794,0.437722419928826,0.345833333333333,0.507518796992481,0.601423487544484,0.637010676156584,0.601423487544484,0.488721804511278,0.519572953736655,0.508896797153025,0.658362989323844,0.842323651452282,0.784232365145228,0.613012139895251,0.789473684210526,0.913533834586466,0.946058091286307,0.733082706766917,0.778195488721804,0.755639097744361,0.766917293233082,0.817427385892116,0.492481203007519,0.596659920911592,0.730290456431535,0.571428571428571,0.417293233082707,0.813278008298755,0.74812030075188,0.883458646616541,0.616541353383459,0.66804979253112,0.644128113879004,0.763485477178423,0.729166666666667,0.720833333333333,0.427046263345196,0.368421052631579,0.683333333333333,0.97508896797153,0.882562277580071,0.580071174377224,0.595833333333333,0.583333333333333,0.761565836298932,0.508896797153025,0.370833333333333,0.491666666666667,0.825622775800712,0.759398496240601,0.609022556390977,0.650375939849624,0.906015037593985,1,0.715302491103203,0.744360902255639,0.49784238665183,0.790035587188612,0.864661654135338,0.690391459074733,0.804511278195489,0.830226137809144,0.486500922885963,0.5625,0.569395017793594,0.7875,0.562277580071174,0.43984962406015,0.443609022556391,0.516014234875445,0.576512455516014,0.541353383458647,0.533333333333333,0.842105263157895,0.789473684210526,0.504166666666666,0.551601423487544,0.850533807829182,0.651245551601424,0.733082706766917,0.665480427046264,0.569395017793594,0.7,0.829181494661922]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot","physics":false},"manipulation":{"enabled":false},"physics":{"stabilization":false},"edges":{"smooth":false}},"groups":["g.CTR","g.BD","U","g.SCZ","g.MDD"],"width":null,"height":null,"idselection":{"enabled":false},"byselection":{"enabled":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","igraphlayout":{"type":"square"},"legend":{"width":0.2,"useGroups":true,"position":"left","ncol":1,"stepX":100,"stepY":100,"zoom":true}},"evals":[],"jsHooks":[]}</script>

## References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Ahn2010" class="csl-entry">

Ahn, Yong Yeol, James P. Bagrow, and Sune Lehmann. 2010. “<span class="nocase">Link communities reveal multiscale complexity in networks</span>.” *Nature*. <https://doi.org/10.1038/nature09182>.

</div>

<div id="ref-Barabasi2016" class="csl-entry">

Barabási, Albert-Láslzo. 2016. *Network Science*. 1st ed. Cambridge, UK: Cambridge University Press.

</div>

<div id="ref-buniello2018" class="csl-entry">

Buniello, Annalisa, Jacqueline A L MacArthur, Maria Cerezo, Laura W Harris, James Hayhurst, Cinzia Malangone, Aoife McMahon, et al. 2018. “The NHGRI-EBI GWAS Catalog of Published Genome-Wide Association Studies, Targeted Arrays and Summary Statistics 2019.” *Nucleic Acids Research* 47 (D1): D1005–12. <https://doi.org/10.1093/nar/gky1120>.

</div>

<div id="ref-chen2012" class="csl-entry">

Chen, Geng, Ziyun Wang, Dongqing Wang, Chengxiang Qiu, Mingxi Liu, Xing Chen, Qipeng Zhang, Guiying Yan, and Qinghua Cui. 2012. “LncRNADisease: A Database for Long-Non-Coding RNA-Associated Diseases.” *Nucleic Acids Research* 41 (D1): D983–86. <https://doi.org/10.1093/nar/gks1099>.

</div>

<div id="ref-davis2020" class="csl-entry">

Davis, Allan Peter, Cynthia J Grondin, Robin J Johnson, Daniela Sciaky, Jolene Wiegers, Thomas C Wiegers, and Carolyn J Mattingly. 2020. “Comparative Toxicogenomics Database (CTD): Update 2021.” *Nucleic Acids Research* 49 (D1): D1138–43. <https://doi.org/10.1093/nar/gkaa891>.

</div>

<div id="ref-fong2004" class="csl-entry">

Fong, Jessica H, Amy E Keating, and Mona Singh. 2004. *Genome Biology* 5 (2): R11. <https://doi.org/10.1186/gb-2004-5-2-r11>.

</div>

<div id="ref-Gysi2020a" class="csl-entry">

Gysi, D M, Í Do Valle Zitnik, M., A Ameli, X Gan, O Varol, H Sanchez, R M Baron, D Ghiassian, J Loscalzo, and A L Barabási. 2020. “<span class="nocase">Network Medicine Framework for Identifying Drug Repurposing Opportunities for COVID-19</span>.” *ArXiv*.

</div>

<div id="ref-Gysi2018" class="csl-entry">

Gysi, Deisy Morselli, Andre Voigt, Tiago de Miranda Fragoso, Eivind Almaas, and Katja Nowick. 2018. “wTO: An R Package for Computing Weighted Topological Overlap and a Consensus Network with Integrated Visualization Tool.” *BMC Bioinformatics* 19 (1). <https://doi.org/10.1186/s12859-018-2351-7>.

</div>

<div id="ref-huang2018" class="csl-entry">

Huang, Zhou, Jiangcheng Shi, Yuanxu Gao, Chunmei Cui, Shan Zhang, Jianwei Li, Yuan Zhou, and Qinghua Cui. 2018. “HMDD V3.0: A Database for Experimentally Supported Human microRNAdisease Associations.” *Nucleic Acids Research* 47 (D1): D1013–17. <https://doi.org/10.1093/nar/gky1010>.

</div>

<div id="ref-Iwamoto2004" class="csl-entry">

Iwamoto, K, C Kakiuchi, M Bundo, K Ikeda, and T Kato. 2004. “Molecular Characterization of Bipolar Disorder by Comparing Gene Expression Profiles of Postmortem Brains of Major Mental Disorders.” *Molecular Psychiatry* 9 (4): 406–16. <https://doi.org/10.1038/sj.mp.4001437>.

</div>

<div id="ref-koh2012" class="csl-entry">

Koh, Gavin C. K. W., Pablo Porras, Bruno Aranda, Henning Hermjakob, and Sandra E. Orchard. 2012. “Analyzing ProteinProtein Interaction Networks.” *Journal of Proteome Research* 11 (4): 2014–31. <https://doi.org/10.1021/pr201211w>.

</div>

<div id="ref-landrum2019" class="csl-entry">

Landrum, Melissa J, Shanmuga Chitipiralla, Garth R Brown, Chao Chen, Baoshan Gu, Jennifer Hart, Douglas Hoffman, et al. 2019. “ClinVar: Improvements to Accessing Data.” *Nucleic Acids Research* 48 (D1): D835–44. <https://doi.org/10.1093/nar/gkz972>.

</div>

<div id="ref-mckusick2007" class="csl-entry">

McKusick, Victor A. 2007. “Mendelian Inheritance in Man and Its Online Version, OMIM.” *The American Journal of Human Genetics* 80 (4): 588–604. <https://doi.org/10.1086/514346>.

</div>

<div id="ref-Menche2015" class="csl-entry">

Menche, Jörg, Amitabh Sharma, Maksim Kitsak, Susan Dina Ghiassian, Marc Vidal, Joseph Loscalzo, and Albert-Laszlo Barabasi. 2015. “<span class="nocase">Uncovering disease-disease relationships through the incomplete interactome</span>.” *Science* 347 (6224). <https://doi.org/10.1126/science.1065103>.

</div>

<div id="ref-morselligysi2020" class="csl-entry">

Morselli Gysi, Deisy, Tiago de Miranda Fragoso, Fatemeh Zebardast, Wesley Bertoli, Volker Busskamp, Eivind Almaas, and Katja Nowick. 2020. “Whole Transcriptomic Network Analysis Using Co-Expression Differential Network Analysis (CoDiNA).” Edited by Francisco J. Esteban. *PLOS ONE* 15 (10): e0240523. <https://doi.org/10.1371/journal.pone.0240523>.

</div>

<div id="ref-Newman2018" class="csl-entry">

Newman, Mark E J. 2018. *Networks*. [https://books.google.com.br/books?hl=en{\\&}lr={\\&}id=YdZjDwAAQBAJ{\\&}oi=fnd{\\&}pg=PP1{\\&}dq=networks+an+introduction{\\&}ots=V{\\\_}J-6Ig5uu{\\&}sig=wlDx19lsqamr-20U5ZUbEjwW3uM{\\\#}v=onepage{\\&}q=networks an introduction{\\&}f=false https://www.ebooks.com/96308191/networks/newman-mark/](https://books.google.com.br/books?hl=en{\&}lr={\&}id=YdZjDwAAQBAJ{\&}oi=fnd{\&}pg=PP1{\&}dq=networks+an+introduction{\&}ots=V{\_}J-6Ig5uu{\&}sig=wlDx19lsqamr-20U5ZUbEjwW3uM{\#}v=onepage{\&}q=networks an introduction{\&}f=false https://www.ebooks.com/96308191/networks/newman-mark/).

</div>

<div id="ref-Nowick2009" class="csl-entry">

Nowick, Katja, Tim Gernat, Eivind Almaas, and Lisa Stubbs. 2009. “Differences in Human and Chimpanzee Gene Expression Patterns Define an Evolving Network of Transcription Factors in Brain.” *Proceedings of the National Academy of Sciences* 106 (52): 22358–63. <https://doi.org/10.1073/pnas.0911376106>.

</div>

<div id="ref-piñero2019" class="csl-entry">

Piñero, Janet, Juan Manuel Ramírez-Anguita, Josep Saüch-Pitarch, Francesco Ronzano, Emilio Centeno, Ferran Sanz, and Laura I Furlong. 2019. “The DisGeNET Knowledge Platform for Disease Genomics: 2019 Update.” *Nucleic Acids Research*, November. <https://doi.org/10.1093/nar/gkz1021>.

</div>

<div id="ref-ramos2013" class="csl-entry">

Ramos, Erin M, Douglas Hoffman, Heather A Junkins, Donna Maglott, Lon Phan, Stephen T Sherry, Mike Feolo, and Lucia A Hindorff. 2013. “PhenotypeGenotype Integrator (PheGenI): Synthesizing Genome-Wide Association Study (GWAS) Data with Existing Genomic Resources.” *European Journal of Human Genetics* 22 (1): 144–47. <https://doi.org/10.1038/ejhg.2013.96>.

</div>

<div id="ref-rehm2015" class="csl-entry">

Rehm, Heidi L., Jonathan S. Berg, Lisa D. Brooks, Carlos D. Bustamante, James P. Evans, Melissa J. Landrum, David H. Ledbetter, et al. 2015. “ClinGen The Clinical Genome Resource.” *New England Journal of Medicine* 372 (23): 2235–42. <https://doi.org/10.1056/nejmsr1406261>.

</div>

<div id="ref-rual2005" class="csl-entry">

Rual, Jean-François, Kavitha Venkatesan, Tong Hao, Tomoko Hirozane-Kishikawa, Amélie Dricot, Ning Li, Gabriel F. Berriz, et al. 2005. “Towards a Proteome-Scale Map of the Human Proteinprotein Interaction Network.” *Nature* 437 (7062): 1173–78. <https://doi.org/10.1038/nature04209>.

</div>

<div id="ref-uetz2000" class="csl-entry">

Uetz, Peter, Loic Giot, Gerard Cagney, Traci A. Mansfield, Richard S. Judson, James R. Knight, Daniel Lockshon, et al. 2000. “A Comprehensive Analysis of Proteinprotein Interactions in Saccharomyces Cerevisiae.” *Nature* 403 (6770): 623–27. <https://doi.org/10.1038/35001009>.

</div>

<div id="ref-zhang2008" class="csl-entry">

Zhang, B., B.-H. Park, T. Karpinets, and N. F. Samatova. 2008. “From Pull-down Data to Protein Interaction Networks and Complexes with Biological Relevance.” *Bioinformatics* 24 (7): 979–86. <https://doi.org/10.1093/bioinformatics/btn036>.

</div>

</div>
