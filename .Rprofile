.First <- function()
{
  suppressPackageStartupMessages(library(arni))
  suppressPackageStartupMessages(suppressWarnings(library(gdata)))
  library(TAF)

  ## .libPaths(sort(.libPaths())) # user library in first slot
  options(continue="  ")
  options(help_type="html")
  options(repos=c(
            ## https may not work on a VM
            CRAN="https://cloud.r-project.org",
            ## "http://mirrors.dotsrc.org/cran",
            ## "http://cran.uib.no",
            ## "http://cran.hafro.is",
            ## "http://cran.r-project.org",
            ## "http://r-forge.r-project.org",
            ## "http://www.stats.ox.ac.uk/pub/RWin",
            ## "http://flr-project.org/R",
            ## "http://r.hafro.is",
            NULL))
  options(stringsAsFactors=FALSE)
  if(.Platform$OS.type=="windows")
  {
    Sys.setenv(TZ="UTC")
    options(editor="emacs")   # write file
    options(pager="runemacs") # Emacs hide
    options(pkgType="binary") # fast default, pass type="source" when needed
    options(width=112)
  }
  else
  {
    options(browser="firefox")
    options(editor="'emacs --no-site-file'")
    options(width=92)
  }

  ## Area plot
  autoload("areaplot", "areaplot") # stacked areas
  autoload("confplot", "areaplot") # confidence bands

  ## MCMC diagnostics
  autoload("autocorr",      "coda") # autocorrelation
  autoload("autocorr.plot", "coda") # diagnostics: thinning
  autoload("cumuplot",      "coda") # diagnostics: convergence
  autoload("densplot",      "coda") # MCMC posterior
  autoload("geweke.diag",   "coda") # MCMC test
  autoload("heidel.diag",   "coda") # MCMC test
  autoload("mcmc",          "coda") # create mcmc object
  autoload("traceplot",     "coda") # diagnostics: convergence

  ## CRAN downloads
  autoload("cran_downloads",     "cranlogs") # any package
  autoload("cran_top_downloads", "cranlogs") # most popular

  ## Data table
  autoload("data.table", "data.table") # data table

  ## Developer tools
  autoload("check",    "devtools") # call rcmdcheck
  autoload("document", "devtools") # call roxygenize

  ## Correlation plot
  autoload("ellipse",  "ellipse") # 2d confidence region
  autoload("plotcorr", "ellipse") # plot correlation matrix

  ## Greg Warnes's toolbox
  ## autoload("Args",     "gdata")  # show function args
  ## autoload("env",      "gdata")  # show environments
  ## autoload("is.what",  "gdata")  # show is.* test results
  ## autoload("keep",     "gdata")  # remove objects
  ## autoload("ll",       "gdata")  # show objects or elements
  autoload("catch.d",     "gplots") # example data
  autoload("catch.r",     "gplots") # example data
  ## autoload("bubbleplot",  "gplots") # bubble plot, not in CRAN version
  autoload("hist2d",      "gplots") # 3d symbol
  autoload("plotCI",      "gplots") # 2d error bars
  autoload("plotmeans",   "gplots") # 2d error bars
  autoload("rich.colors", "gplots") # color palettes
  autoload("ASCIIfy",     "gtools") # encoding
  autoload("logit",       "gtools") # logit(p), same as binomial()$linkfun(mu)
  autoload("inv.logit",   "gtools") # inv.logit(eta) = binomial()$linkinv(eta)

  ## STAR
  autoload("append.id",             "gfcmSTAR") # modify existing Assessment_ID
  autoload("basic.time",            "gfcmSTAR") # convert to truncated POSIXct
  autoload("combo",                 "gfcmSTAR") # combine refyear, species, GSA
  autoload("comma2period",          "gfcmSTAR") # convert string
  autoload("diff.stars",            "gfcmSTAR") # show differences between STARs
  autoload("export.many.csv",       "gfcmSTAR") # export STAR objects to CSV
  autoload("gsa.names",             "gfcmSTAR") # convert GSA numbers to names
  autoload("identical.stars",       "gfcmSTAR") # check if STARs are identical
  autoload("import.many.csv",       "gfcmSTAR") # import CSV files
  autoload("import.many.templates", "gfcmSTAR") # import templates
  autoload("peek",                  "gfcmSTAR") # extract metadata field
  autoload("qc",                    "gfcmSTAR") # run all quality checks
  autoload("qc.exists",             "gfcmSTAR") # file exists
  autoload("qc.star",               "gfcmSTAR") # file is a STAR template
  autoload("qc.ts.names",           "gfcmSTAR") # ts column names are intact
  autoload("qc.ts.numbers",         "gfcmSTAR") # ts are numbers and not strings
  autoload("qc.vpa",                "gfcmSTAR") # VPA_Model is Yes or No
  autoload("qc.xlsx",               "gfcmSTAR") # file extension is xlsx
  autoload("read.properties",       "gfcmSTAR") # read SharePoint properties
  autoload("read.star.csv",         "gfcmSTAR") # read STAR CSV files
  autoload("read.template",         "gfcmSTAR") # read STAR template
  autoload("read.template.v10",     "gfcmSTAR") # read STAR template v1.0
  autoload("read.template.v21",     "gfcmSTAR") # read STAR template v2.1
  autoload("readTable.logical",     "gfcmSTAR") # read single-cell logical
  autoload("readTable.transpose",   "gfcmSTAR") # read transposed table
  autoload("report",                "gfcmSTAR") # report files imported
  autoload("set.classes",           "gfcmSTAR") # convert data types
  autoload("Start_Case",            "gfcmSTAR") # convert column names
  autoload("template.version",      "gfcmSTAR") # read transposed table
  autoload("write.star.csv",        "gfcmSTAR") # write STAR object to CSV
  autoload("write.report",          "gfcmSTAR") # write report to text file
  autoload("lookup.gsa",            "gfcmSTAR") # lookup GSA
  autoload("lookup.species",        "gfcmSTAR") # lookup species

  ## ggplot
  autoload("ggplot", "ggplot2") # plot
  autoload("ggsave", "ggplot2") # export
  autoload("qplot",  "ggplot2") # plot

  ## glmmADMB
  autoload("glmmadmb", "glmmADMB") # glmm

  ## glmmTMB
  autoload("glmmTMB", "glmmTMB") # glmm

  ## GMT
  autoload("deg2num", "gmt") # degree or time to number
  autoload("geodist", "gmt") # distance on a sphere
  autoload("gmt",     "gmt") # initialize GMT
  autoload("pscoast", "gmt") # draw map

  ## Frank Harrell's toolbox
  autoload("bpplot",          "Hmisc") # 1d boxplot
  autoload("contents",        "Hmisc") # summary
  autoload("datadensity",     "Hmisc") # multivariate: hist and jitter
  autoload("describe",        "Hmisc") # summary
  autoload("hist.data.frame", "Hmisc") # multivariate: hist
  autoload("panel.xYplot",    "Hmisc") # 2d error bars

  ## ICES advice
  autoload("agesFbar",  "icesAdvice") # ages for Fbar
  autoload("Bpa",       "icesAdvice") # Bpa from Blim
  autoload("DLS3.2",    "icesAdvice") # DLS method 3.2
  autoload("Fpa",       "icesAdvice") # Fpa from Flim
  autoload("gss",       "icesAdvice") # agesFbar example
  autoload("icesRound", "icesAdvice") # rounding method
  autoload("mohn",      "icesAdvice") # retro bias
  autoload("read.dls",  "icesAdvice") # read DLS output
  autoload("shake",     "icesAdvice") # retro example
  autoload("sigmaCI",   "icesAdvice") # sigma from confint
  autoload("sigmaPA",   "icesAdvice") # sigma from refpts
  autoload("write.dls", "icesAdvice") # write DLS output

  ## ICES DATRAS web service
  autoload("getCAdata",                "icesDatras") # age, weight, maturity
  autoload("getDATRAS",                "icesDatras") # wrapper for get*data
  autoload("getDatrasDataOverview",    "icesDatras") # surveys by year-quarter
  autoload("getHHdata",                "icesDatras") # year, lat, lon, depth
  autoload("getHLdata",                "icesDatras") # length, count
  autoload("getIndices",               "icesDatras") # official ICES indices
  autoload("getSurveyList",            "icesDatras") # survey info: surveys
  autoload("getSurveyYearList",        "icesDatras") # survey info: years
  autoload("getSurveyYearQuarterList", "icesDatras") # survey info: quarters

  ## ICES Stock Assessment Graphs
  autoload("findKey",                     "icesSAG") # deprecated
  autoload("findAssessmentKey",           "icesSAG") # look up stock-year key
  autoload("getFishStockReferencePoints", "icesSAG") # Blim, Bpa, Fmsy, Btrigger
  autoload("getListStocks",               "icesSAG") # key, species, region
  autoload("getSAG",                      "icesSAG") # wrapper for get*functions
  autoload("getSummaryTable",             "icesSAG") # year, Rec, SSB, F, catch

  ## ICES Stock Database
  autoload("getSD",     "icesSD") # all stocks, all years
  autoload("showStock", "icesSD") # one stock, one year

  ## ICES TAF extra
  autoload("zoom.ggplot", "icesTAFextra") # ggplot text size

  ## ICES Vocab reference codes
  autoload("findAphia",       "icesVocab") # look up species code
  autoload("getCodeDetail",   "icesVocab") # detail (parents, children)
  autoload("getCodeList",     "icesVocab") # code definition
  autoload("getCodeTypeList", "icesVocab") # available codes

  ## JSON
  autoload("fromJSON", "jsonlite") # parse JSON

  ## Knitr
  autoload("knit", "knitr") # compile Rmd
  autoload("knit", "spin")  # compile R

  ## Lattice graphics
  autoload("barchart",       "lattice") # 1d bar plot
  autoload("bwplot",         "lattice") # 1d boxplot
  autoload("cloud",          "lattice") # 3d scatter
  autoload("contourplot",    "lattice") # 3d contour
  autoload("densityplot",    "lattice") # 1d empirical pdf
  autoload("dotplot",        "lattice") # 1d dot plot
  autoload("histogram",      "lattice") # 1d hist
  autoload("levelplot",      "lattice") # 3d contour
  autoload("parallel",       "lattice") # multivariate parallel
  autoload("splom",          "lattice") # multivariate scatter
  autoload("stripplot",      "lattice") # 1d by factor
  autoload("trellis.device", "lattice") # device
  autoload("wireframe",      "lattice") # 3d surface
  autoload("xyplot",         "lattice") # 2d scatterplot

  ## Check style
  autoload("lint", "lintr") # check style

  ## Mixed effects
  autoload("glmer",      "lme4") # generalized linear mixed effects
  autoload("lmer",       "lme4") # linear mixed effects
  autoload("nlmer",      "lme4") # nonlinear mixed effects
  autoload("sleepstudy", "lme4") # example dataset used in glmmTMB

  ## Venables and Ripley toolbox
  autoload("as.fractions", "MASS") # fractions: 1.333<->4:3
  autoload("boxcox",       "MASS") # transform: ^ and log
  autoload("fitdistr",     "MASS") # estimate beta/gamma/negbinom
  autoload("glm.nb",       "MASS") # glm neg binom family
  autoload("glmmPQL",      "MASS") # glmm (requires lme4)
  autoload("logtrans",     "MASS") # transform: log(y+alpha)
  autoload("mvrnorm",      "MASS") # multivariate normal
  autoload("parcoord",     "MASS") # multivariate parallel

  ## GAM
  autoload("gam",         "mgcv") # gam (simon wood)
  autoload("summary.gam", "mgcv") # summary

  ## MS SQL Server
  autoload("dbOverview",    "MSSQL")  # dims and colnames
  autoload("dbStorage",     "MSSQL")  # storage size
  autoload("dbTime",        "MSSQL")  # time created and modified
  autoload("tableDim",      "MSSQL")  # dim
  autoload("tableHead",     "MSSQL")  # first rows
  autoload("tableNcol",     "MSSQL")  # ncol
  autoload("tableNrow",     "MSSQL")  # nrow
  autoload("tableOverview", "MSSQL")  # data types and nrow
  autoload("tableQuote",    "MSSQL")  # quote name

  ## Bravington's toolbox
  autoload("foodweb", "mvbutils") # visualize function dependencies

  ## Mixed effects
  autoload("fixef",     "nlme") # estimated fixed effects
  autoload("intervals", "nlme") # confidence intervals
  autoload("lme",       "nlme") # lme model
  autoload("nlme",      "nlme") # nlme model
  autoload("nlsList",   "nlme") # many separate nls models
  autoload("ranef",     "nlme") # estimated random effects
  autoload("VarCorr",   "nlme") # sigmas: as.numeric(VarCorr(model)[,2])

  ## Numerical derivatives
  autoload("hessian", "numDeriv") # hessian
  autoload("grad",    "numDeriv") # gradient

  ## Parallel processing
  autoload("detectCores", "parallel") # count available cores
  autoload("mclapply",    "parallel") # parallel apply (list)
  autoload("mcmapply",    "parallel") # parallel apply (multi args)
  autoload("pvec",        "parallel") # parallel apply (vector)

  ## Build packages
  autoload("build", "pkgbuild") # build

  ## MCMC
  autoload("plotAuto",  "plotMCMC") # autocorrelation
  autoload("plotCumu",  "plotMCMC") # cumulative quantiles
  autoload("plotDens",  "plotMCMC") # density
  autoload("plotQuant", "plotMCMC") # quantiles
  autoload("plotSplom", "plotMCMC") # scatterplot matrix
  autoload("plotTrace", "plotMCMC") # traces

  ## Bivariate confint/freq
  autoload("conf2d",   "r2d2") # bivariate confint
  autoload("freq2d",   "r2d2") # bivariate frequency
  autoload("shrink2d", "r2d2") # bivariate confset

  ## Check package
  autoload("rcmdcheck", "rcmdcheck") # build and check

  ## RCT3
  autoload("rct3", "rct3")  # recruitment forecast

  ## Install from repositories
  autoload("install_github",  "remotes") # install package
  autoload("install_version", "remotes") # install package
  autoload("parse_repo_spec", "remotes") # parse repo

  ## Reshape
  autoload("melt", "reshape2") # unxtab

  ## Gadget
  autoload("gadget.fit",       "Rgadget") # get model fit
  autoload("gadget.iterative", "Rgadget") # run model

  ## R markdown
  autoload("render", "rmarkdown") # Rmd -> HTML

  ## ODBC
  autoload("odbcConnect",     "RODBC") # connect
  autoload("odbcClose",       "RODBC") # close
  autoload("odbcDataSources", "RODBC") # list databases
  autoload("sqlColumns",      "RODBC") # table columns
  autoload("sqlFetch",        "RODBC") # get whole table
  autoload("sqlQuery",        "RODBC") # query part of table
  autoload("sqlTables",       "RODBC") # list tables

  ## Write Rd pages
  autoload("roxygenize", "roxygen2") # write Rd pages

  ## Coleraine
  autoload("estN",        "scape") # effective sample size
  autoload("estSigmaI",   "scape") # observation noise
  autoload("estSigmaR",   "scape") # recruitment variability
  autoload("getN",        "scape") # effective sample size
  autoload("getSigmaI",   "scape") # observation noise
  autoload("getSigmaR",   "scape") # recruitment variability
  autoload("importADCAM", "scape") # import
  autoload("importCol",   "scape") # import
  autoload("importMCMC",  "scape") # import coleraine
  autoload("importProj",  "scape") # import coleraine projections
  autoload("iterate",     "scape") # iterate
  autoload("plotB",       "scape") # biomass
  autoload("plotCA",      "scape") # catch at age
  autoload("plotCL",      "scape") # catch at length
  autoload("plotIndex",   "scape") # abundance index
  autoload("plotLA",      "scape") # length at age
  autoload("plotN",       "scape") # numbers at age
  autoload("plotSel",     "scape") # selectivity and maturity
  autoload("x.cod",       "scape") # assessment
  autoload("x.ling",      "scape") # assessment
  autoload("x.oreo",      "scape") # assessment
  autoload("x.saithe",    "scape") # assessment
  autoload("x.sbw",       "scape") # assessment

  ## Session info
  autoload("package_info", "sessioninfo") # package info
  autoload("session_info", "sessioninfo") # session info

  ## Martin Maechler's toolbox
  autoload("primes",    "sfsmisc") # prime numbers
  autoload("factorize", "sfsmisc") # factorize into prime numbers

  ## Shiny
  autoload("fluidPage", "shiny") # html ui generator
  autoload("runApp",    "shiny") # run app
  autoload("shinyApp",  "shiny") # run app

  ## SOFIA
  autoload("addDriors", "SOFIA")  # add driors column to stock object
  autoload("addEffort", "SOFIA")  # add effort column to catch data
  autoload("calcCat",   "SOFIA")  # calculate stock status categories
  autoload("compCat",   "SOFIA")  # old name for calcCat
  autoload("groupData", "SOFIA")  # group primary data files
  autoload("plotCat",   "SOFIA")  # plot stock status categories
  autoload("plotProp",  "SOFIA")  # old name for plotCat

  ## Splines
  autoload("bs", "splines") # polynomial spline
  autoload("ns", "splines") # natural cubic spline

  ## SAM
  autoload("fbarplot",   "stockassessment") # plot Fbar
  autoload("fitfromweb", "stockassessment") # read fit from SAO server
  autoload("ssbplot",    "stockassessment") # plot SSB
  autoload("read.ices",  "stockassessment") # ICES data format

  ## Template Model Builder
  autoload("compile",    "TMB") # compile model
  autoload("gdbsource",  "TMB") # debug model
  autoload("MakeADFun",  "TMB") # construct model
  autoload("precompile", "TMB") # compile main library
  autoload("sdreport",   "TMB") # calculate standard errors

  ## Package development
  autoload("compactPDF",            "tools") # compress PDF files
  autoload("dependsOnPkgs",         "tools") # which local pkgs depend on X
  autoload("file_ext",              "tools") # filename extension
  autoload("file_path_as_absolute", "tools") # full path
  autoload("file_path_sans_ext",    "tools") # filename prefix
  autoload("package_dependencies",  "tools") # which local pkgs X depends on
  autoload("Rdindex",               "tools") # write index of functions
  autoload("showNonASCII",          "tools") # special characters
  autoload("toTitleCase",           "tools") # title case
  autoload("write_PACKAGES",        "tools") # create repository

  ## Excel
  autoload("loadWorkbook",          "XLConnect") # workbook
  autoload("readTable",             "XLConnect") # table object
  autoload("readWorksheet",         "XLConnect") # sheet
  autoload("readWorksheetFromFile", "XLConnect") # sheet

  ## Export table
  autoload("xtable", "xtable") # export table
}

.Last <- function()
{
  setwd(if(dir.exists("c:/home/r")) "c:/home/r"
        else if(dir.exists(path.expand("~/r"))) path.expand("~/r")
        else getwd())

  ## Backup user workspace
  if(file.exists(".RData"))
    file.rename(".RData", ".RBack")

  ## Delete .First, .Last, and some common residual objects
  suppressWarnings(rm(.First, .Last,
                      worldMapEnv, .Last.projection, .map.range,  # maps
                      .rdired.objects, pos=1))                    # r-dired
}
