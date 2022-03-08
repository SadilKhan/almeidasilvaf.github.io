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
<link href="{{< blogdown/postref >}}index_files/crosstalk/css/crosstalk.min.css" rel="stylesheet" />
<script src="{{< blogdown/postref >}}index_files/crosstalk/js/crosstalk.min.js"></script>

## Motivation

When I developed [BioNERO](https://bioconductor.org/packages/release/bioc/html/BioNERO.html), my first R/Bioconductor package, I didn’t know to which journals I could submit the paper describing it. Since then, I’ve seen many other R developers that have faced the same issue. To help solve this problem, here I will guide you on how to do some web scraping to find out the main journals where Bioc developers publish papers describing their packages.

## Extracting citation information from packages’ landing pages

All Bioconductor packages have a landing page where there is a citation field. This field is created from a CITATION file inside the package directory or automatically from information in the DESCRIPTION file. First, let’s write two helper functions:

-   `get_bioc_citation`, a function that scrapes packages’ landing pages to get citation information.
-   `create_citation_table`, a function that creates a tidy data frame containing package name, journal where the paper is published (if that is the case), and year of publication.
-   `process_titles`, a function to process journal titles and remove inconsistencies across packages.

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
#'   \item{Citation}{Character of full citation.}
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
    table <- table[, c("Package", "Journal", "Year", "Citation")]
    table <- table[!is.na(table$Journal), ]
    return(table)
}

#' Process journal titles
#'
#' @param citation_table Data frame created with \code{create_citation_table}.
#'
#' @return The same data frame as in \strong{citation_table}, but with
#' processed titles in the variable 'Journal'.
#' @noRd
#' @importFrom dplyr mutate filter
#' @importFrom stringr str_to_title str_replace_all str_detect str_trim
process_titles <- function(citation_table = NULL) {
    pcit <- dplyr::mutate(citation_table, 
                          Journal = stringr::str_to_title(Journal))
    
    sagmb <- "Statistical Applications In Genetics And Molecular Biology"
    pnas <- "Proceedings Of The National Academy Of Sciences Of The United States Of America"
    ijms <- "International Journal Of Molecular Sciences"
    cmpbiomed <- "Computer Methods And Programs In Biomedicine"
    jcompbio <- "Journal Of Computational Biology"
    pcit <- mutate(pcit, Journal = stringr::str_replace_all(
        Journal, c("^Nat$" = "Nature",
                   "Res$" = "Research",
                   " \\(Oxford.*" = "",
                   "Bmc" = "BMC",
                   "Plos" = "PLoS",
                   "Rna" = "RNA",
                   "^Nucl$" = "Nucleic Acids Research",
                   "^Genome Bio$" = "Genome Biology",
                   "^Genome Biol$" = "Genome Biology",
                   "^F1000$" = "F1000Research",
                   "F1000research" = "F1000Research",
                   "F1000res" = "F1000Research",
                   "F1000 Research" = "F1000Research",
                   "Stat Appl Genet Mol Biol" = sagmb,
                   "Sagmb" = sagmb,
                   "Proceedings Of The National Academy of Sciences" = pnas,
                   "Proc Natl Acad Sci Usa" = pnas,
                   "Pnas" = pnas,
                   "Comput Biol" = "Computational Biology",
                   "Computat Biol" = "Computational Biology",
                   "Physiol Genomics" = "Physiological Genomics",
                   "Oxford " = "",
                   "Bioinformatics Journal" = "Bioinformatics",
                   " \\(.*\\)" = "",
                   "Nat Meth" = "Nature Methods",
                   "Nat Commun" = "Nature Communications",
                   "Nat Biotech" = "Nature Biotechnology",
                   "Nat Biotechnol" = "Nature Biotechnology",
                   "Nar Genomics And Bioinformatics" = "Nucleic Acids Research",
                   "Mol Cell Proteomics" = "Molecular & Cellular Proteomics",
                   "Methods Inf Med" = "Methods Of Information In Medicine",
                   "Jco.*" = "Journal Of Clinical Oncology",
                   "Jasa" = "Journal Of the American Statistical Association",
                   "J Proteomics" = "Journal Of Proteomics",
                   "J Comput Biol" = jcompbio,
                   "J Computational Biology" = jcompbio,
                   "J Mach Learn" = "Journal Of Machine Learning",
                   "Int J Mol Sci" = ijms,
                   "Comput Methods Programs Biomed" = cmpbiomed,
                   "Syst Biol" = "Systems Biology",
                   "Mol Systems Biology" = "Molecular Systems Biology",
                   "Algorithms Mol Biol" = "Algorithms For Molecular Biology"
                   ))
    )
    pcit <- mutate(pcit, Journal = stringr::str_trim(Journal))
    remove <- "Biorxiv|Arxiv|Doi|Preprint|Submitted|In Preparation|Error|Http"
    pcit <- dplyr::filter(pcit, 
                          !stringr::str_detect(Journal, remove))
    
    return(pcit)
}
```

Now, we can create the citation table.

``` r
suppressPackageStartupMessages(library(tidyverse))

# List available Bioc packages
bioc_repo <- "https://www.bioconductor.org/packages/release/bioc"
bioc_pkgs <- available.packages(repos = bioc_repo)[, "Package"]
```

``` r
# Scrape CITATION field from Bioc landing pages
citation_table <- create_citation_table(bioc_pkgs)

# Process journal names
citation_table <- process_titles(citation_table)
```

## Summary stats

Now, let’s count the frequency of packages in each journal and show the top 20 journals based on highest article count.

``` r
citation_stats <- citation_table %>%
    count(Journal) %>%
    arrange(-n) %>%
    slice_head(n = 20)

citation_stats
```

    ##                                                       Journal   n
    ## 1                                              Bioinformatics 272
    ## 2                                          BMC Bioinformatics  90
    ## 3                                      Nucleic Acids Research  67
    ## 4                                              Genome Biology  55
    ## 5                                               F1000Research  31
    ## 6                                              Nature Methods  30
    ## 7                                                BMC Genomics  23
    ## 8                                                    PLoS One  20
    ## 9                                  PLoS Computational Biology  19
    ## 10                                            Genome Research  13
    ## 11                                              Biostatistics  11
    ## 12                                      Nature Communications  10
    ## 13                                       Analytical Chemistry   9
    ## 14                                  Molecular Systems Biology   8
    ## 15                                                     Nature   8
    ## 16                                       Nature Biotechnology   8
    ## 17                            Molecular & Cellular Proteomics   7
    ## 18 Statistical Applications In Genetics And Molecular Biology   7
    ## 19                                      Frontiers In Genetics   6
    ## 20                               Journal Of Proteome Research   6

Exploring it visually:

``` r
bioc_logo <- png::readPNG(
    here::here("content", "blog", 
               "2022-01-03-bioc_publications", 
               "featured-bioc.png"), 
    native = TRUE)

last_updated <- format(Sys.Date(), "%Y-%m-%d")
xmax <- max(citation_stats$n) + 30
xmax <- round(xmax / 10) * 10

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
                             top = 0.55,
                             right = 0.95,
                             bottom = 0.3) +
    theme_void()
```

<img src="{{< blogdown/postref >}}index_files/figure-html/unnamed-chunk-3-1.png" width="864" />

And voilà! In case you want to explore the whole table, here it is:

<div id="htmlwidget-1" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-1">{"x":{"filter":"none","vertical":false,"data":[["1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189","190","191","192","193","194","195","196","197","198","199","200"],["Bioinformatics","BMC Bioinformatics","Nucleic Acids Research","Genome Biology","F1000Research","Nature Methods","BMC Genomics","PLoS One","PLoS Computational Biology","Genome Research","Biostatistics","Nature Communications","Analytical Chemistry","Molecular Systems Biology","Nature","Nature Biotechnology","Molecular &amp; Cellular Proteomics","Statistical Applications In Genetics And Molecular Biology","Frontiers In Genetics","Journal Of Proteome Research","Nature Genetics","BMC Systems Biology","Journal Of Statistical Software","Scientific Reports","Briefings In Bioinformatics","Proceedings Of The National Academy Of Sciences","Proceedings Of The National Academy Of Sciences Of The United States Of America","Annals Of Applied Statistics","Biometrics","Cell Reports","Cytometry Part A","Epigenetics &amp; Chromatin","Genomics","Johns Hopkins University","Molecular Biology And Evolution","Peerj","Aibar S","Algorithms For Molecular Biology","Analytica Chimica Acta","Animal Genetics","Biomed Research International","Clinical Cancer Research","Computer Methods And Programs In Biomedicine","Epigenomics","Gigascience","Human Mutation","International Journal Of Molecular Sciences","J","Journal Of Biomedical Informatics","Journal Of Clinical Oncology","Journal Of Machine Learning Research","Luo X","Metabolites","Methods","Methods In Ecology And Evolution","Methods In Molecular Biology","Microbiome","Mol","Nature Protocols","Omics: A Journal Of Integrative Biology","Shokoohi F","Source Code For Biology And Medicine","The Journal Of Open Source Software","","10","Aging","Algorithmica","American Journal Of Human Genetics","Ann","Applications In Genetics And Molecular Biology","Applications In Plant Sciences","Baek M","Bbagrm","Bhuva Dd","Biochemical And Biophysical Research Communications","Biochimica Et Biophysica Acta - Bioenergetics","Bioconductor Package","Bioinformatics And Biomedical Engineering","Bioinformation","Biology Direct","Bionformatics","BMC Biodata Mining","BMC Cancer","BMC Epigenetics &amp; Chromatin","BMC Infectious Diseases","BMC Medical Genomics","Boileau P","Bolstad Bm","Cancer Informatics","Cancer Research","Cell","Cell Reports Methods","Cell Systems","Chromosome Research","Clinical Epigenetics","Computers In Biology And Medicine","Cuklina J","Current Protocols In Bioinformatics","Dalmolin","Data Integration Life Sci","Database","Developmental Dynamics","Elife","Embnet","Epigenesys - Bioinformatics Protocol","Epigenetics","Epigenetics Chromatin","Fraley C","Front","Frontiers In Molecular Biosciences","Frontiers In Oncology","Genes","Genes &amp; Genomics","Genetics","Genome Biology And Evolution","Genome Medicine","Haibe-Kains B","Hayden N","Henriques D","In Gentleman R","In Lee T","In Mathé E","In Proc","In Schölkopf B","International Journal Of Epidemiology","Iscience","J R Stat Soc Ser C-Appl Stat","Jain N","Japanese J Appl Stat","Journal","Journal Of Alzheimer's Disease","Journal Of Applied Statistics","Journal Of Bioinformatics And Computational Biology","Journal Of Biological Rhythms","Journal Of Cachexia","Journal Of Clinical Bioinformatics","Journal Of Computational And Graphical Statistics","Journal Of Computational Biology","Journal Of Immunological Methods","Journal Of Mass Spectrometry","Journal Of Open Source Software","Journal Of Proteomics","Journal Of Statistical Planning And Infererence","Journal Of the American Statistical Association","Journal Of The International Biometric Society","Journal Of The National Cancer Institute","Journal Of The Royal Statistical Society","Kim V","Kohl M","Kopp W","Lahti L","Lai W","Life Science Alliance","Mallick H","Marini F","Metabolomics","Methods Enzymol","Methods Of Information In Medicine","Molecular Biosystems","Molecular Cell","Nature Biotechnologynol","New Phytologist","Npj Systems Biology And Applications","Oncotarget","Patterns","Paulson Jn","Physiological Genomics","Plants","PLoS Comput","Pollard Ks","Qg14","Raborn Rt","RNA Biology","Robertson Ds","Ruffieux Y","Schlosser P","Source Code Biol Med","Star Protocols","Stark R","Statistica Sinica","Stokowy T","Systems Biomedicine","Tba","Tcbb/Ieee","Technical Report 745","Technometrics","Terfve C","The American Journal Of Human Genetics","The Annals Of Applied Statistics","The Innovation","The Pharmacogenomics Journal","The Prostate","The R Journal","Under Review","University Of Regensburg","Wellcome Open Research","X","Yeung Ky","Zenkova D","Zhang Jd"],[272,90,67,55,31,30,23,20,19,13,11,10,9,8,8,8,7,7,6,6,6,5,5,5,4,4,4,3,3,3,3,3,3,3,3,3,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th> <\/th>\n      <th>Journal<\/th>\n      <th>n<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"columnDefs":[{"className":"dt-right","targets":2},{"orderable":false,"targets":0}],"order":[],"autoWidth":false,"orderClasses":false}},"evals":[],"jsHooks":[]}</script>

## Session information

``` r
sessionInfo()
```

    ## R version 4.1.2 (2021-11-01)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 20.04.4 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
    ## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=de_BE.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=de_BE.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=de_BE.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=de_BE.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] forcats_0.5.1   stringr_1.4.0   dplyr_1.0.8     purrr_0.3.4    
    ## [5] readr_2.1.2     tidyr_1.2.0     tibble_3.1.6    ggplot2_3.3.5  
    ## [9] tidyverse_1.3.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.8        lubridate_1.8.0   here_1.0.1        png_0.1-7        
    ##  [5] rprojroot_2.0.2   assertthat_0.2.1  digest_0.6.29     utf8_1.2.2       
    ##  [9] R6_2.5.1          cellranger_1.1.0  backports_1.4.1   reprex_2.0.1     
    ## [13] evaluate_0.15     highr_0.9         httr_1.4.2        blogdown_1.8     
    ## [17] pillar_1.7.0      rlang_1.0.2       curl_4.3.2        readxl_1.3.1     
    ## [21] rstudioapi_0.13   jquerylib_0.1.4   DT_0.21           rmarkdown_2.12   
    ## [25] labeling_0.4.2    selectr_0.4-2     htmlwidgets_1.5.4 munsell_0.5.0    
    ## [29] broom_0.7.12      compiler_4.1.2    modelr_0.1.8      xfun_0.30        
    ## [33] pkgconfig_2.0.3   htmltools_0.5.2   tidyselect_1.1.2  bookdown_0.24    
    ## [37] fansi_1.0.2       crayon_1.5.0      tzdb_0.2.0        dbplyr_2.1.1     
    ## [41] withr_2.5.0       grid_4.1.2        jsonlite_1.8.0    gtable_0.3.0     
    ## [45] lifecycle_1.0.1   DBI_1.1.2         magrittr_2.0.2    scales_1.1.1     
    ## [49] cli_3.2.0         stringi_1.7.6     farver_2.1.0      fs_1.5.2         
    ## [53] xml2_1.3.3        bslib_0.3.1       ellipsis_0.3.2    generics_0.1.2   
    ## [57] vctrs_0.3.8       tools_4.1.2       glue_1.6.2        crosstalk_1.2.0  
    ## [61] hms_1.1.1         fastmap_1.1.0     yaml_2.3.5        colorspace_2.0-3 
    ## [65] rvest_1.0.2       knitr_1.37        haven_2.4.3       patchwork_1.1.1  
    ## [69] sass_0.4.0
