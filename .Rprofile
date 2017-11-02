.First <- function()
{
  suppressPackageStartupMessages(require(gdata))  # load gdata first to
  suppressPackageStartupMessages(require(arni))   # get mv() from arni

  ## .libPaths(sort(.libPaths())) # user library in first slot
  options(continue="  ")
  options(help_type="html")
  options(repos=c(
            "http://mirrors.dotsrc.org/cran",
            ## "http://cran.uib.no",
            ## "http://cran.hafro.is", # https does not work on a VM
            ## "http://cran.r-project.org",
            ## "http://r-forge.r-project.org",
            ## "http://www.stats.ox.ac.uk/pub/RWin",
            ## "http://flr-project.org/R",
            ## "http://r.hafro.is",
            NULL))
  options(stringsAsFactors=FALSE)
  if(.Platform$OS.type=="windows")
  {
    options(editor="emacs")   # write file
    options(pager="runemacs") # Emacs hide
    options(width=112)
  }
  else
  {
    options(browser="google-chrome")
    options(editor=paste0("'/opt/emacs/", Sys.getenv("EMACS_VERSION"),
                          "/bin/emacs --no-site-file'"))
    options(width=100)
  }

  ## MCMC diagnostics
  autoload("autocorr",      "coda") # autocorrelation
  autoload("autocorr.plot", "coda") # diagnostics: thinning
  autoload("cumuplot",      "coda") # diagnostics: convergence
  autoload("densplot",      "coda") # MCMC posterior
  autoload("geweke.diag",   "coda") # MCMC test
  autoload("heidel.diag",   "coda") # MCMC test
  autoload("mcmc",          "coda") # create mcmc object
  autoload("traceplot",     "coda") # diagnostics: convergence

  ## Developer tools
  autoload("build",          "devtools") # build
  autoload("check",          "devtools") # build and check
  autoload("document",       "devtools") # roxygenize
  autoload("install_github", "devtools") # install package
  autoload("lint",           "devtools") # check style

  ## Correlation plot
  autoload("ellipse",  "ellipse") # 2d confidence region
  autoload("plotcorr", "ellipse") # plot correlation matrix

  ## Greg Warnes's toolbox
  ## autoload("Args",     "gdata")  # show function args
  ## autoload("env",      "gdata")  # show environments
  ## autoload("is.what",  "gdata")  # show is.* test results
  ## autoload("keep",     "gdata")  # remove objects
  ## autoload("ll",       "gdata")  # show objects or elements
  autoload("balloonplot", "gplots") # 3d symbol
  autoload("hist2d",      "gplots") # 3d symbol
  autoload("plotCI",      "gplots") # 2d error bars
  autoload("plotmeans",   "gplots") # 2d error bars
  autoload("rich.colors", "gplots") # color palettes
  autoload("ASCIIfy",     "gtools") # encoding
  autoload("logit",       "gtools") # logit(p), same as binomial()$linkfun(mu)
  autoload("inv.logit",   "gtools") # inv.logit(eta) = binomial()$linkinv(eta)

  ## ggplot
  autoload("ggplot", "ggplot2") # ggplot

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
  autoload("Bpa",       "icesAdvice") # Bpa from Blim
  autoload("DLS3.2",    "icesAdvice") # DLS method 3.2
  autoload("Fpa",       "icesAdvice") # Fpa from Flim
  autoload("icesRound", "icesAdvice") # rounding method
  autoload("sigmaCI",   "icesAdvice") # sigma from confint
  autoload("sigmaPA",   "icesAdvice") # sigma from refpts

  ## ICES DATRAS web service
  autoload("getCAdata",                "icesDatras") # age, weight, maturity
  autoload("getDATRAS",                "icesDatras") # wrapper for get*data
  autoload("getDatrasDataOverview",    "icesDatras") # surveys by year-quarter
  autoload("getHHdata",                "icesDatras") # year, lat, lon, depth
  autoload("getHLdata",                "icesDatras") # length, count
  autoload("getSurveyList",            "icesDatras") # survey info: surveys
  autoload("getSurveyYearList",        "icesDatras") # survey info: years
  autoload("getSurveyYearQuarterList", "icesDatras") # survey info: quarters

  ## ICES Stock Assessment Graphs
  autoload("findKey",                     "icesSAG") # look up stock-year key
  autoload("getFishStockReferencePoints", "icesSAG") # Blim, Bpa, Fmsy, Btrigger
  autoload("getListStocks",               "icesSAG") # key, species, region
  autoload("getSAG",                      "icesSAG") # wrapper for get*functions
  autoload("getSummaryTable",             "icesSAG") # year, Rec, SSB, F, catch

  ## ICES Stock Database
  autoload("getSD",     "icesSD") # all stocks, all years
  autoload("showStock", "icesSD") # one stock, one year

  ## ICES Transparent Assessment Framework
  autoload("catage.long", "icesTAF") # catage example
  autoload("catage.taf",  "icesTAF") # catage example
  autoload("catage.xtab", "icesTAF") # catage example
  autoload("clean",       "icesTAF") # remove TAF directories
  autoload("cp",          "icesTAF") # copy file
  autoload("dos2unix",    "icesTAF") # convert line endings
  autoload("flr2taf",     "icesTAF") # FLR -> TAF
  autoload("long2taf",    "icesTAF") # long -> TAF
  autoload("make",        "icesTAF") # run script if needed
  autoload("mkdir",       "icesTAF") # create directory
  autoload("msg",         "icesTAF") # show message
  autoload("read.dls",    "icesTAF") # read DLS output
  autoload("read.taf",    "icesTAF") # read TAF table
  autoload("sourceAtoZ",  "icesTAF") # run all scripts
  autoload("sourceTAF",   "icesTAF") # run script
  autoload("taf2long",    "icesTAF") # TAF -> long
  autoload("taf2xtab",    "icesTAF") # TAF -> crosstab
  autoload("tt",          "icesTAF") # transpose
  autoload("unix2dos",    "icesTAF") # convert line endings
  autoload("upload",      "icesTAF") # upload file to server
  autoload("write.dls",   "icesTAF") # write DLS output
  autoload("write.taf",   "icesTAF") # write TAF table
  autoload("xtab2taf",    "icesTAF") # crosstab -> TAF

  ## ICES Vocab reference codes
  autoload("findAphia",       "icesVocab") # look up species code
  autoload("getCodeDetail",   "icesVocab") # detail (parents, children)
  autoload("getCodeList",     "icesVocab") # code definition
  autoload("getCodeTypeList", "icesVocab") # available codes

  ## Knitr
  autoload("knit", "knitr") # literate data analysis

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
  autoload("mclapply", "parallel") # parallel apply (list)
  autoload("mcmapply", "parallel") # parallel apply (multi args)
  autoload("pvec",     "parallel") # parallel apply (vector)

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

  ## Reshape
  autoload("melt", "reshape") # unxtab

  ## R markdown
  autoload("render", "rmarkdown") # Rmd -> HTML

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

  ## Martin Maechler's toolbox
  autoload("primes",    "sfsmisc") # prime numbers
  autoload("factorize", "sfsmisc") # factorize into prime numbers

  ## Splines
  autoload("bs", "splines") # polynomial spline
  autoload("ns", "splines") # natural cubic spline

  ## SAM
  autoload("read.ices", "stockassessment") # ICES data format

  ## Template Model Builder
  autoload("compile",    "TMB") # compile model
  autoload("gdbsource",  "TMB") # debug model
  autoload("MakeADFun",  "TMB") # construct model
  autoload("precompile", "TMB") # compile main library
  autoload("sdreport",   "TMB") # calculate standard errors

  ## Package development
  autoload("compactPDF",           "tools") # compress PDF files
  autoload("dependsOnPkgs",        "tools") # which local pkgs depend on X
  autoload("file_ext",             "tools") # filename extension
  autoload("file_path_sans_ext",   "tools") # filename prefix
  autoload("package_dependencies", "tools") # which local pkgs X depends on
  autoload("Rdindex",              "tools") # write index of functions
  autoload("showNonASCII",         "tools") # special characters
  autoload("write_PACKAGES",       "tools") # create repository

  ## Export table
  autoload("xtable", "xtable") # export table
}

.Last <- function()
{
  ## Set directory
  setwd(if(.Platform$OS.type=="windows") "c:/home/r" else path.expand("~/r"))

  ## Backup user workspace
  if(file.exists(".RData"))
    file.rename(".RData", ".RBack")

  ## Delete .First, .Last, and some common residual objects
  suppressWarnings(rm(.First, .Last,
                      worldMapEnv, .Last.projection, .map.range,  # maps
                      .rdired.objects, pos=1))                    # r-dired
}
