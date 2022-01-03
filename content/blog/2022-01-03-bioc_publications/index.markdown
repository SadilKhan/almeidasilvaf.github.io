---
title: "Where can I publish a paper describing my Bioconductor package?"
excerpt: "Check out where Bioc developers have published their papers"
date: 2022-01-02
author: "Fabrício Almeida-Silva"
draft: false
# layout options: single, single-sidebar
layout: single
categories:
- bioinformatics
- reproducible research
- bioconductor
- scientific writing
- rstats
---

<script src="{{< blogdown/postref >}}index_files/htmlwidgets/htmlwidgets.js"></script>
<link href="{{< blogdown/postref >}}index_files/datatables-css/datatables-crosstalk.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/datatables-binding/datatables.js"></script>
<script src="{{< blogdown/postref >}}index_files/jquery/jquery-3.6.0.min.js"></script>
<link href="{{< blogdown/postref >}}index_files/dt-core/css/jquery.dataTables.min.css" rel="stylesheet" />
<link href="{{< blogdown/postref >}}index_files/dt-core/css/jquery.dataTables.extra.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/dt-core/js/jquery.dataTables.min.js"></script>
<link href="{{< blogdown/postref >}}index_files/crosstalk/css/crosstalk.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/crosstalk/js/crosstalk.min.js"></script>

## Motivation

When I developed [BioNERO](https://bioconductor.org/packages/release/bioc/html/BioNERO.html), my first R/Bioconductor package, I didn’t know to which journals I could submit the paper describing it. Since then, I’ve seen many other R developers that have faced the same issue. To help solve this problem, here I will teach you how to do some web scraping to find out the main journals where Bioc developers publish papers describing their packages.

## Extracting citation information from packages’ landing pages

All Bioconductor packages have a landing page where there is a citation field. This field is created from a CITATION file inside the package directory or automatically from information in the DESCRIPTION file. First, let’s write two functions to helper functions:

-   `get_bioc_citation`, a function that scrapes packages’ landing pages to get citation information.
-   `create_citation_table`, a function that created a tidy data frame containing package name, journal where the paper is published (if that is the case), and year of publication.

``` r
#' Extract citation from Bioconductor landing page
#'
#' @param package Character of package name.
#'
#' @return Character of citation text.
#' @noRd
#' @importFrom rvest html_elements html_text2
#' @importFrom httr GET content
get_bioc_citation <- function(package = NULL) {
    url <- paste0("https://bioconductor.org/packages/release/bioc/citations/",
                  package, "/citation.html")
    request <- httr::GET(url)
    parsed_page <- httr::content(request, as = "parsed", encoding = "UTF-8")
    if("xml_node" %in% class(parsed_page)) {
        citations <- rvest::html_elements(parsed_page, "p")
        citations <- rvest::html_text2(citations)
    } else {
        citations <- NA
    }
    return(citations)
}

#' Create a table of citations for each Bioc package
#'
#' @param packages Character vector of package names.
#'
#' @return A data frame with the following variables:
#' \describe{
#'   \item{Package}{Chaarcter of package name.}
#'   \item{Journal}{Character of journal name.}
#'   \item{Year}{Numeric of journal year.}
#' }
#' @noRd
#' @importFrom stringr str_detect str_extract
create_citation_table <- function(packages = NULL) {
    table <- Reduce(rbind, lapply(packages, function(x) {
        df <- data.frame(Citation = get_bioc_citation(x))
        df$Package <- x
        return(df)
    }))
    
    year <- "([0-9]{4})"
    table$Year <- as.numeric(stringr::str_extract(table$Citation, year))
    journal <- gsub('.*” ', '', table$Citation)
    journal <- gsub("\\*{3} ", "", journal)
    journal <- gsub("(,|\\.).*", "", journal)
    table$Journal <- journal
    idx <- which(stringr::str_detect(table$Citation, "github|R package version"))
    table$Journal[idx] <- NA
    table <- table[, c("Package", "Journal", "Year")]
    table <- table[!is.na(table$Journal), ]
    return(table)
}
```

Now, we can create the citation table.

``` r
suppressPackageStartupMessages(library(tidyverse))

# List available Bioc packages
bioc_repo <- "https://www.bioconductor.org/packages/release/bioc"
bioc_pkgs <- available.packages(repos = bioc_repo)[, "Package"]

# Scrape CITATION field from Bioc landing pages
citation_table <- create_citation_table(bioc_pkgs)
```

## Summary stats

Now, let’s count the frequency of packages in each journal and show the top 20 journals based on highest article count.

``` r
citation_table %>%
    count(Journal) %>%
    arrange(-n) %>%
    slice_head(n = 20)
```

    ##                       Journal   n
    ## 1              Bioinformatics 262
    ## 2          BMC Bioinformatics  82
    ## 3                     bioRxiv  63
    ## 4              Genome Biology  51
    ## 5      Nucleic Acids Research  42
    ## 6              Nature Methods  29
    ## 7               F1000Research  25
    ## 8                BMC Genomics  22
    ## 9                    PLoS ONE  13
    ## 10            Genome Research  12
    ## 11          Nucleic Acids Res  12
    ## 12              Biostatistics  10
    ## 13         BMC bioinformatics  10
    ## 14       Analytical Chemistry   9
    ## 15                        Nat   7
    ## 16       Nature Biotechnology   7
    ## 17      Nature Communications   7
    ## 18 PLoS Computational Biology   7
    ## 19     Bioinformatics (Oxford   6
    ## 20                    doi: 10   6

We can see that there are some issues, such as different names for the same journal, the preprint server bioRxiv listed as a journal, and a weird ‘doi’ journal that is likely due to manual insertion of citation info. Let’s fix them.

``` r
citation_stats <- citation_table %>%
    mutate(Journal = str_replace_all(
        Journal, c("Nat$" = "Nature",
                   "Res$" = "Research",
                   " \\(Oxford.*" = "",
                   "bioinformatics" = "Bioinformatics"))
    ) %>%
    count(Journal) %>%
    arrange(-n) %>%
    filter(!Journal %in% c("bioRxiv", "BioRxiv", "doi: 10")) %>%
    slice_head(n = 20)
citation_stats
```

    ##                            Journal   n
    ## 1                   Bioinformatics 268
    ## 2               BMC Bioinformatics  92
    ## 3           Nucleic Acids Research  54
    ## 4                   Genome Biology  51
    ## 5                   Nature Methods  29
    ## 6                    F1000Research  27
    ## 7                     BMC Genomics  22
    ## 8                  Genome Research  13
    ## 9                         PLoS ONE  13
    ## 10                   Biostatistics  10
    ## 11            Analytical Chemistry   9
    ## 12                          Nature   8
    ## 13            Nature Biotechnology   7
    ## 14           Nature Communications   7
    ## 15      PLoS Computational Biology   7
    ## 16    Journal of Proteome Research   6
    ## 17       Molecular Systems Biology   6
    ## 18 Journal of Statistical Software   5
    ## 19 Molecular & Cellular Proteomics   5
    ## 20                 Nature Genetics   5

Exploring it visually:

``` r
bioc_logo <- png::readPNG(
    here::here("content", "blog", 
               "2022-01-03-bioc_publications", 
               "featured-bioc.png"), 
    native = TRUE)

last_updated <- format(Sys.Date(), "%Y-%m-%d")

ggplot(citation_stats, aes(x = n, y = reorder(Journal, n))) +
    geom_col() +
    geom_text(aes(label = n), hjust = -0.3) +
    xlim(0, 300) +
    labs(title = "Where are papers associated with BioC packages published?",
         subtitle = paste0("Last updated: ", last_updated),
         x = "Number of papers", y = "") +
    theme_bw() +
    patchwork::inset_element(bioc_logo,
                             left = 0.5,
                             bottom = 0.55,
                             right = 0.95,
                             top = 0.3) +
    theme_void()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-4-1.png" width="720" />

And voilà! In case you want to explore the whole table, here it is:

<div id="htmlwidget-1" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200","201","202","203","204","205","206","207","208","209","210","211","212","213","214","215","216","217","218","219","220","221","222","223","224","225","226","227","228","229","230","231","232","233","234","235","236","237","238","239","240","241","242","243","244","245","246","247","248","249","250","251","252","253","254","255","256","257","258","259","260","261","262","263","264","265","266","267","268","269","270","271"],["Bioinformatics","BMC Bioinformatics","Nucleic Acids Research","Genome Biology","Nature Methods","F1000Research","BMC Genomics","Genome Research","PLoS ONE","Biostatistics","Analytical Chemistry","Nature","Nature Biotechnology","Nature Communications","PLoS Computational Biology","Journal of Proteome Research","Molecular Systems Biology","Journal of Statistical Software","Molecular &amp; Cellular Proteomics","Nature Genetics","Nucl","PLoS computational biology","BMC Systems Biology","Briefings in Bioinformatics","Frontiers in Genetics","Nucleic acids research","PLOS Computational Biology","Proceedings of the National Academy of Sciences","Statistical Applications in Genetics and Molecular Biology","Annals of Applied Statistics","Biometrics","Cell Reports","Cytometry Part A","Genome Biol","Johns Hopkins University","Molecular Biology and Evolution","PLOS ONE","Aibar S","Analytica Chimica Acta","Animal Genetics","BIOINFORMATICS","BioMed Research International","Clinical Cancer Research","Epigenetics &amp; Chromatin","Epigenomics","F1000","Frontiers in genetics","Genomics","Gigascience","Human Mutation","in preparation","Int J Mol Sci","J","Journal of Biomedical Informatics","Luo X","manuscript in preparation","Manuscript in preparation","Methods","Methods in Ecology and Evolution","Methods in Molecular Biology","Microbiome","Mol","Mol Cell Proteomics","Nature communications","Nature Protocols","Nucleic Acids Research (Database issue)","OMICS: A Journal of Integrative Biology","PeerJ","PLoS Comput Biol","PLoS One","PNAS","Scientific reports","Scientific Reports","Shokoohi F (2020)","Source Code for Biology and Medicine","Stat Appl Genet Mol Biol","The Journal of Open Source Software","","10","Aging (Albany NY)","AISTATS","Algorithmica","Algorithms for Molecular Biology","Algorithms Mol Biol","American Journal of Human Genetics","Ann","Applications in Genetics and Molecular Biology","Applications in Plant Sciences","arxiv","arXiv","arXiv preprint arXiv:1805","Baek M","BBAGRM","Bhuva DD","Biochemical and Biophysical Research Communications","Biochimica et Biophysica Acta (BBA) - Bioenergetics","Bioconductor Package","Bioinformatics and Biomedical Engineering","Bioinformatics Journal","Bioinformation","Biology Direct","Bionformatics","biorxiv","biorXiv","Biostatistics (accepted)","BMC BioData Mining","BMC Cancer","BMC Epigenetics &amp; Chromatin","BMC genomics","BMC Infectious Diseases","BMC Medical Genomics","BMC Syst Biol","Boileau P","Bolstad BM (2004)","Cancer Informatics","Cancer Research","Cell","Cell Reports Methods","Cell Systems","Chromosome Research","Clinical Epigenetics","Comput Methods Programs Biomed","Computer Methods and Programs in Biomedicine","Computers in Biology and Medicine","Cuklina J","Current Protocols in Bioinformatics","Dalmolin","Data Integration Life Sci","Database","Developmental Dynamics","eLife","EMBnet","Epigenesys - Bioinformatics Protocol","Epigenetics","Epigenetics Chromatin","F1000 Research","f1000research","Fraley C","Front","Frontiers in Molecular Biosciences","Frontiers in Oncology","Genes","Genes &amp; Genomics","Genetics","Genome Biology and Evolution","Genome Medicine","Haibe-Kains B","Hayden N","Henriques D","http://plato","http://www","https://doi","Important note to the maintainer of the BRAIN package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the ChemmineR package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the clippda package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the diggit package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the DMRcate package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the eiR package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the flowFP package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the fmcsR package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the GOSim package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the GSRI package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the MEDME package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the pandaR package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the rfPred package: An error occured while trying to generate the citation from the CITATION file","Important note to the maintainer of the SIM package: An error occured while trying to generate the citation from the CITATION file","In Gentleman R","In Lee T","In Mathé E","In Proc","In Schölkopf B","International journal of epidemiology","iScience","J Comput Biol","J Mach Learn Research","J Proteomics","J R Stat Soc Ser C-Appl Stat","Jain N","Japanese J Appl Stat","JASA","JCO Clinical Cancer Informatics","JCO Precision Oncology","Journal","Journal of Alzheimer's Disease","Journal of Applied Statistics","Journal of Bioinformatics and Computational Biology","Journal of Biological Rhythms","Journal of cachexia","Journal of Clinical Bioinformatics","Journal of Computational and Graphical Statistics","Journal of Immunological Methods","Journal of Machine Learning Research","Journal of Mass Spectrometry","Journal of Open Source Software","Journal of Statistical Planning and Infererence","Journal of the International Biometric Society","Journal of the National Cancer Institute","Journal of the Royal Statistical Society","Kim V (2021)","Kohl M (2007)","Kopp W (2017)","Lahti L","Lai W","Life Science Alliance","Mallick H","Manuscript submitted for publication","Marini F","Metabolomics","Methods Enzymol","Methods Inf Med","Mol Syst Biol","Molecular BioSystems","Molecular Cell","Molecular systems biology","NAR Genomics and Bioinformatics","Nat Biotech","Nat Biotechnol","Nat Commun","Nat Meth","Nature genetics","New Phytologist","npj Systems Biology and Applications","Oncotarget","Oxford Bioinformatics","Patterns","Paulson JN","PeerJ Preprints","Physiol Genomics","Plants","PLoS Comput","PLoS Computat Biol","PLoS Genetics","Plos one","PLoS one","Pollard KS","Preprint available in https://www","Preprint in Mathematical Sciences 18","Proc Natl Acad Sci USA","Proceedings of the National Academy of Sciences of the United States of America","QG14","Raborn RT","RNA Biology","Robertson DS","Ruffieux Y","SAGMB","Schlosser P","Source Code Biol Med","STAR Protocols","Stark R","Statistica Sinica","Stokowy T (2016)","Submitted","Systems Biomedicine","tba","TCBB/IEEE","Technical Report 745","Technometrics","Terfve C","The American Journal of Human Genetics","The Annals of Applied Statistics","The Innovation","The Pharmacogenomics Journal","The Prostate","The R Journal","Under review","University of Regensburg","Wellcome open research","X","Yeung KY","Zenkova D","Zhang JD (2021)"],[268,92,54,51,29,27,22,13,13,10,9,8,7,7,7,6,6,5,5,5,5,5,4,4,4,4,4,4,4,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Journal<\/th>\n      <th>n<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>

## Session information

``` r
sessionInfo()
```

    ## R version 4.1.0 (2021-05-18)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS High Sierra 10.13.6
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRblas.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] forcats_0.5.1   stringr_1.4.0   dplyr_1.0.7     purrr_0.3.4    
    ## [5] readr_2.0.2     tidyr_1.1.4     tibble_3.1.5    ggplot2_3.3.5  
    ## [9] tidyverse_1.3.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.7        here_1.0.1        lubridate_1.8.0   png_0.1-7        
    ##  [5] rprojroot_2.0.2   assertthat_0.2.1  digest_0.6.29     utf8_1.2.2       
    ##  [9] R6_2.5.1          cellranger_1.1.0  backports_1.2.1   reprex_2.0.1     
    ## [13] evaluate_0.14     highr_0.9         httr_1.4.2        blogdown_1.5     
    ## [17] pillar_1.6.4      rlang_0.4.12      curl_4.3.2        readxl_1.3.1     
    ## [21] rstudioapi_0.13   jquerylib_0.1.4   DT_0.19           rmarkdown_2.11   
    ## [25] labeling_0.4.2    selectr_0.4-2     htmlwidgets_1.5.4 munsell_0.5.0    
    ## [29] broom_0.7.9       compiler_4.1.0    modelr_0.1.8      xfun_0.29        
    ## [33] pkgconfig_2.0.3   htmltools_0.5.2   tidyselect_1.1.1  bookdown_0.24    
    ## [37] codetools_0.2-18  fansi_0.5.0       crayon_1.4.1      tzdb_0.1.2       
    ## [41] dbplyr_2.1.1      withr_2.4.2       grid_4.1.0        jsonlite_1.7.2   
    ## [45] gtable_0.3.0      lifecycle_1.0.1   DBI_1.1.1         magrittr_2.0.1   
    ## [49] scales_1.1.1      cli_3.0.1         stringi_1.7.6     farver_2.1.0     
    ## [53] fs_1.5.2          xml2_1.3.2        bslib_0.3.1       ellipsis_0.3.2   
    ## [57] generics_0.1.0    vctrs_0.3.8       tools_4.1.0       glue_1.6.0       
    ## [61] crosstalk_1.1.1   hms_1.1.1         fastmap_1.1.0     yaml_2.2.1       
    ## [65] colorspace_2.0-2  rvest_1.0.2       knitr_1.37        haven_2.4.3      
    ## [69] patchwork_1.1.1   sass_0.4.0
