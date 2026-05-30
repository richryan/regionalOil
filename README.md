PROJECT regionalOil
================

<!-- This document generates README.md upon Knitting in RStudio -->

<!-- output: -->

<!--   github_document: -->

<!--     md_extensions: -autolink_bare_uris+hard_line_breaks -->

# Introduction

California policymakers have adopted the goal of carbon neutrality by
2045 or earlier. Within California, Kern County accounts for over 70
percent of oil produced within the state. To get a sense of how the
transition may affect opportunities in Kern—the extent to which Kern’s
economy depends on oil—the project estimates a structural vector
autoregressive model that jointly explains the global crude-oil market
and the evolution of employment within Kern. The estimates are based on
data from [FRED](https://fred.stlouisfed.org/) and the [Quarterly Census
of Employment and Wages](https://www.bls.gov/cew/), and the project
contains code to retrieve these data (which will make updating the
analysis straightforward). In addition, the project contains **R code**
that

- estimates a structural vector autoregressive model,
- computes structural impulse response functions,
- conducts inference for the structural IRFs based on a residual-based
  block bootstrap procedure proposed by Brüggemann et al.
  ([2016](#ref-bruggemann_jentsch_trenkler_2016)) as discussed by Kilian
  and Lütkepohl ([2017](#ref-kilian_lutkepohl_2017)),
- computes historical decompositions and forecast error variance
  decompositions,
- constructs counterfactual employment series based on the Wold
  decomposition and moving-average representation of the system.

A summary of the project is available as a working paper at
[arXiv](https://arxiv.org/abs/2602.23462) ([Ryan and Michieka
2026](#ref-ryan_michieka_2026)). A published version is forthcoming:

> **Employment, Input–Output Linkages, and the Energy Transition in
> California’s Top Oil-Producing Region** (with Nyakundi Michieka).
> Forthcoming *Networks and Spatial Economics*

Please note:

- The code relies on the R package kilianr, which was installed by
  running `devtools::install_github(repo = "richryan/kilianr")`.
- *Updating* results will require API keys. *Replicating* results does
  not.
- The code relies on the `targets` package for reproducibility.
- [Rich Ryan](https://richryan.github.io/) wrote the code and the paper,
  so please address correspondence to <richryan@csub.edu>. He’d be
  delighted to hear from you.

# Push-button replication

The project relies on the R packages

- renv
- targets

for replicability. Here is a workflow that will closely reproduce the
results in the paper.

1.  Create a new project from git
2.  To be sure you are updating results, you can delete the contents of
    out/
3.  From within R at the root of the project, source
    `push_button_replication.R`. This will install the relevant packages
    and run the targets pipeline.
    - In a Posit Cloud environment, I ran into several errors. Running
      `update.packages(ask = FALSE, checkBuilt = TRUE)` solved the
      issues.
4.  The file paper-contents.tex collects the results featured by Ryan
    and Michieka ([2026](#ref-ryan_michieka_2026))

In just over 6 minutes, I was able to replicate the results in the
following environment:

    R version 4.6.0 (2026-04-24)
    Platform: x86_64-pc-linux-gnu
    Running under: Ubuntu 24.04.4 LTS

    Matrix products: default
    BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
    LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0

    locale:
     [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8        LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8    LC_PAPER=C.UTF-8      
     [8] LC_NAME=C              LC_ADDRESS=C           LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   

    time zone: UTC
    tzcode source: system (glibc)

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    loaded via a namespace (and not attached):
     [1] vctrs_0.7.3       cli_3.6.6         knitr_1.51        rlang_1.2.0       xfun_0.57         otel_0.2.0        processx_3.9.0    targets_1.12.0    renv_1.2.3       
    [10] data.table_1.18.4 glue_1.8.1        prettyunits_1.2.0 backports_1.5.1   ps_1.9.3          evaluate_1.0.5    tibble_3.3.1      base64url_1.4     yaml_2.3.12      
    [19] lifecycle_1.0.5   compiler_4.6.0    codetools_0.2-20  igraph_2.3.1      pkgconfig_2.0.3   rstudioapi_0.18.0 R6_2.6.1          tidyselect_1.2.1  pillar_1.11.1    
    [28] callr_3.7.6       magrittr_2.0.5    tools_4.6.0       withr_3.0.2       secretbase_1.2.2 

Note: I haven’t tested this workflow much, but I suspect the workflow
will break under Box, as the files in renv/library/ will not always be
available.

<!-- 1. Install the targets and tarchetypes packages from R -->

<!-- 1. Use `renv::restore()` -->

The script `_targets.R` also contains directions for *updating* the
results; that is, extending the results to include the latest available
data.

In an effort of full and forthright disclosure, I don’t think my code is
seeding the bootstrap replications correctly, and I haven’t confirmed
that the confidence sets are always the same. Nevertheless, the points
estimates should be the same.

## API key in .Renviron

The project contains code that retrieves data from two sources that both
require an API key:

1.  [FRED](https://fred.stlouisfed.org/)
2.  [US Energy Information Administration](https://www.eia.gov/)

I saved my API key registered with [FRED](https://fred.stlouisfed.org/)
in my `.Renviron` file: `FRED_API_KEY=my-api-key`. I added the API key
by running `usethis::edit_r_environ()`. (The command can be useful
because it shows you *where* the file lives on your machine.)

I saved my API key from the [US Energy Information
Administration](https://www.eia.gov/) in my `.Renviron` file. Which I
accomplished running `usethis::edit_r_environ()`. (The command can be
useful because it shows you *where* the file lives on your machine.)

You can learn about the API program at <https://www.eia.gov/developer/>
and <https://www.eia.gov/opendata/>. You register an API key. The API
key allows code to collect data directly from the EIA website.
Importantly, the interface means data can be updated automatically.

A good place to read about the .Renviron file is [What They Forgot to
Teach You About R](https://rstats.wtf/) by Jennifer Bryan, Jim Hester,
Shannon Pileggi, and E. David Aja. Another useful reference is
[Efficient R programming](https://csgillespie.github.io/efficientR/) by
Colin Gillespie and Robin Lovelace.

[Introduction to Computational and Data
Sciences](https://book.cds101.com/) by James K. Glasbrenner, Ajay
Kulkarni, and Dominic White was also helpfully consulted.

The references on targets also provide information on worflow and
project management.

## Resources on the renv package

- “[Using {renv} for Reproducible R
  Projects](https://youtu.be/Zu01z_ZpPgQ?si=ijiMSuRAe3THKVvn)” by Susan
  B.
- “[Introduction to
  renv](https://rstudio.github.io/renv/articles/renv.html)”
- The `renv::init()` creates
  - `renv.lock` records what packages were used to produce results
    (including specific version numbers)
  - \`renv::restore()\`\` will match
  - The directory renv/library/ now contains package files (or symbolic
    links to those files). The command `.libPaths()` will show this by
    telling you where packages in the project are being sourced from.
    With the renv package, the links will not point to the system-wide R
    package library.
- renv workflow
  - `install.package("somepackage")`
  - `renv::snapshot()` will update the renv.lock file
  - When starting from a fresh R environment, `renv:restore()` will make
    sure another R environment has the same version of packages
- Version-controlled files (not renv/library/, which are
  system-specific—for people working on Windows and Mac machines
  collaborating on a project, eg—this directory is managed by renv
  \[same for renv/staging/\])
  - renv.lock
  - .Rprofile (instructs to use renv/activate.R)
  - renv/activate.R (set up how the renv package works)
  - renv/.gitignore
  - renv/settings.json

From the renv package: “projects using renv will normally use a private,
per-project R library, in which new packages will be installed. This
project library is isolated from other R libraries on your system.” The
idea is to work from an R environment within the project directory.

## Resources on the targets package

Here are resources on the targets package:

- [Building reproducible analytical pipelines with
  R](https://raps-with-r.dev/) by Bruno Rodrigues
- [Introduction to
  targets](https://carpentries-incubator.github.io/targets-workshop/) at
  Carpentries
- Dr Anna Krystalli’s [Putting the R into Reproducible
  Research](https://annakrystalli.me/talks/r-in-repro-research.html#1)
  at 2019-05-14 RSS Sheffield Local Group meeting
- Louisa Smith’s [Reproducible Epidemiology in
  R](https://www.reproducible-epi-workshop.louisahsmith.com/)
- Flight ([2022](#ref-flight_2022))

Krystalli cites Buckheit and Donoho ([1995](#ref-buckheit_donoho_1995))
who “distill” Jon Claerbout’s insights into the slogan:

> An article about computational science in a scientic publication is
> **not** the scholarship itself, it is merely advertising of the
> scholarship. The actual scholarship is the complete software
> development environment and the complete set of instructions which
> generated the figures.

A contribution of the project is integrating analysis of policy-relevant
question about the clean-energy transition with a software development
environment within the R ecosystem.

Wilson et al. ([2017](#ref-wilson_etal_2017)) and Noble
([2009](#ref-noble_2009)) discuss managing a project.

# Data

The [Quarterly Census of Employment and
Wages](https://www.bls.gov/cew/classifications/industry/industry-titles.htm)
program uses “the North American Industry Classification System (NAICS)
as to assign establishments to industries and to report industry data at
highly detailed as well as at aggregated levels.” The classification
systems are

- For the years 1990 to 2006, QCEW data were coded using the 2002
  version of NAICS.
- For periods from 2007 to 2010, QCEW data were coded using the 2007
  version of NAICS.
- For periods from 2011 to 2016, QCEW data were coded using the 2012
  version of NAICS.
- For periods from 2017 to 2021, QCEW data were coded using the 2017
  version of NAICS.
- QCEW data from 2022-forward are and will be coded using the 2022
  version of NAICS.

NAICS codes are available from the [Census
Bureau](https://www.census.gov/naics/).

# License

<a href="https://github.com/richryan/regionalOil">Employment,
Input–Output Linkages, and the Energy Transition in California’s Top
Oil-Producing Region</a> © 2026 by
<a href="https://richryan.github.io/">Rich Ryan</a> is licensed under
<a href="https://creativecommons.org/licenses/by-nc-sa/4.0/">CC BY-NC-SA
4.0</a><img src="https://mirrors.creativecommons.org/presskit/icons/cc.svg" alt="" style="max-width: 1em;max-height:1em;margin-left: .2em;"><img src="https://mirrors.creativecommons.org/presskit/icons/by.svg" alt="" style="max-width: 1em;max-height:1em;margin-left: .2em;"><img src="https://mirrors.creativecommons.org/presskit/icons/nc.svg" alt="" style="max-width: 1em;max-height:1em;margin-left: .2em;"><img src="https://mirrors.creativecommons.org/presskit/icons/sa.svg" alt="" style="max-width: 1em;max-height:1em;margin-left: .2em;">

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-bruggemann_jentsch_trenkler_2016" class="csl-entry">

Brüggemann, Ralf, Carsten Jentsch, and Carsten Trenkler. 2016.
“Inference in VARs with Conditional Heteroskedasticity of Unknown Form.”
*Journal of Econometrics* 191 (1): 69–85.
<https://doi.org/10.1016/j.jeconom.2015.10.004>.

</div>

<div id="ref-buckheit_donoho_1995" class="csl-entry">

Buckheit, Jonathan B., and David L. Donoho. 1995. “WaveLab and
Reproducible Research.” In *Wavelets and Statistics*, edited by Anestis
Antoniadis and Georges Oppenheim. Springer New York.
<https://doi.org/10.1007/978-1-4612-2544-7_5>.

</div>

<div id="ref-flight_2022" class="csl-entry">

Flight, Robert M. 2022. “Creating an Analysis With a Targets Workflow.”
September 27.
<https://rmflight.github.io/posts/2022-09-27-creating-an-analysis-using-targets>.

</div>

<div id="ref-kilian_lutkepohl_2017" class="csl-entry">

Kilian, Lutz, and Helmut Lütkepohl. 2017. *Structural Vector
Autoregressive Analysis*. Cambridge University Press.
<https://doi.org/10.1017/9781108164818>.

</div>

<div id="ref-noble_2009" class="csl-entry">

Noble, William Stafford. 2009. “A Quick Guide to Organizing
Computational Biology Projects.” *PLoS Computational Biology* 5 (7):
e1000424. <https://doi.org/10.1371/journal.pcbi.1000424>.

</div>

<div id="ref-ryan_michieka_2026" class="csl-entry">

Ryan, Rich, and Nyakundi Michieka. 2026. *Employment, Input-Output
Linkages, and the Energy Transition in California’s Top Oil-Producing
Region*. <https://arxiv.org/abs/2602.23462>.

</div>

<div id="ref-wilson_etal_2017" class="csl-entry">

Wilson, Greg, Jennifer Bryan, Karen Cranston, Justin Kitzes, Lex
Nederbragt, and Tracy K. Teal. 2017. “Good Enough Practices in
Scientific Computing.” *PLOS Computational Biology* 13 (6): e1005510.
<https://doi.org/10.1371/journal.pcbi.1005510>.

</div>

</div>
