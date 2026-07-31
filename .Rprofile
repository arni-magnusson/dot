.First <- function()
{
  # suppressPackageStartupMessages(library(arni))
  # suppressPackageStartupMessages(suppressWarnings(library(gdata)))
  # library(TAF)
  # .libPaths(sort(.libPaths())) # user library in first slot
  Sys.setlocale(locale="en_US.UTF-8")
  options(continue="  ")
  options(help_type="html")
  options(Ncpus=4)    # https may not work on a VM
  options(repos=c(CRAN="https://cloud.r-project.org"))
  if(.Platform$OS.type=="windows")
  {
    options(editor="emacs")   # write file
    options(pager="runemacs") # Emacs hide
    options(pkgType="binary")
    options(width=80)
  }
  else
  {
    options(browser="firefox")
    options(editor="'emacs --no-site-file'")
    options(width=92)
  }

  # remotes
  autoload("install_github", "remotes") # install package from GitHub

  # sessioninfo
  autoload("package_info", "sessioninfo") # package info
  autoload("session_info", "sessioninfo") # session info

  # tools
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
}

.Last <- function()
{
  setwd(if(dir.exists("c:/home/r")) "c:/home/r"
        else if(dir.exists(path.expand("~/r"))) path.expand("~/r")
        else getwd())

  # Backup user workspace
  if(file.exists(".RData"))
    file.rename(".RData", ".RBack")

  # Delete .First, .Last, and some common residual objects
  suppressWarnings(rm(.First, .Last,
                      worldMapEnv, .Last.projection, .map.range,  # maps
                      .rdired.objects, pos=1))                    # r-dired
}
