\name{lav_make_tikz}
\alias{lav_make_tikz}
\title{Creates code to show a diagram in tikz format.}
\usage{
lav_make_tikz(nodes.edges,
              outfile = "",
              cex = 1.3,
              sloped.labels = TRUE,
              standalone = FALSE,
              mlovcolors = c("lightgreen", "lightblue")
              )
}
\arguments{
\item{nodes.edges}{The list with nodes, edges and mlrij as returned from
    lav_position_nodes}
\item{outfile}{A connection or a character string, the file to store the code.}
\item{cex}{Minimum distance between nodes in cm.}
\item{sloped.labels}{logical, sloped labels for the edges}
\item{standalone}{logical, add code to make teX file standalone?}
\item{mlovcolors}{Array of two colors for distinguishing ov nodes with same name
between the levels in a  multilevel model.}
}
\value{
NULL, invisible
}
\description{
Creates the code to make a diagram in tikz.
}
\examples{
model <- 'alpha  =~ x_1 + x_2 + x_3       # latent variable
          beta <~ x_4 + x_5 + x_6         # composite
          gamma =~ x_7 + x_8 + x_9        # latent variable
          Xi =~ x_10 + x_11 + x_12 + x_13 # latent variable
          # regressions
          Xi ~ v * alpha + t * beta + 1
          alpha ~ yy * Theta_1 + tt * beta + ss * gamma
          '
test <- lav_get_model_info(model)
test <- lav_position_nodes(test)
lav_make_tikz(test) # no file given, so output to R console
modelml <- '
        level: 1
        fw =~ 1*y_1 + y_2 + y_3 + y_5
        level: 2
        fb =~ 1*y_1 + y_2 + y_3 + y_4
        y_2 ~~ cv24 * y_4
  '
testml <- lav_get_model_info(modelml)
testml <- lav_position_nodes(testml)
# in the line hereunder no file is given, so output to R console
lav_make_tikz(testml, cex = 1.4, sloped.labels = FALSE, standalone = TRUE)
\dontrun{
# example creating tex file with the above diagrams
zz <- file("testtikz.tex", open="w")
writeLines(c(
  '\\documentclass{article}',
  '\\usepackage{amsmath, amssymb}',
  '\\usepackage{amsfonts}',
  '\\usepackage[utf8]{inputenc}',
  '\\usepackage[english]{babel}',
  '\\usepackage{xcolor}',
  '\\usepackage{color}',
  '\\usepackage{tikz}',
  '\\usetikzlibrary {shapes.geometric}',
  '\\begin{document}'),
  zz)
lav_make_tikz(test, outfile = "temp.tex")
tmp <- readLines("tmp.tex")
writeLines(tmp, zz)
writeLines(" ", zz)
lav_make_tikz(testml, outfile = "temp.tex", cex = 1.4, sloped.labels = FALSE)
tmp <- readLines("tmp.tex")
writeLines(tmp, zz)
writeLines("\\end{document}", zz)
close(zz)
openPDF <- function(f) {
  os <- .Platform$OS.type
  if (os=="windows")
    shell.exec(normalizePath(f))
  else {
    pdf <- getOption("pdfviewer", default='')
    if (nchar(pdf)==0)
      stop("The 'pdfviewer' option is not set. Use options(pdfviewer=...)")
    system2(pdf, args=c(f))
  }
}
tools::texi2dvi("testtikz.tex", pdf = TRUE, clean = TRUE)
openPDF("testtikz.pdf")
}
}
