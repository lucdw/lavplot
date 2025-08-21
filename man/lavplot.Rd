\name{lavplot}
\alias{lavplot}
\title{Creates diagram code for tikz, svg or Rplot}
\usage{
lavplot(model = NULL, infile = NULL,
        varlv = FALSE,
        texfile = NULL,
        cex = 1.3,
        pngfile = NA_character_,
        svgfile = NULL,
        sloped_labels = TRUE,
        standalone = FALSE,
        placenodes = NULL,
        edgelabelsbelow = NULL)
}
\arguments{
\item{model}{A character vector specifying the model in lavaan syntax or a list
(or data.frame) with at least members lhs, op, rhs, label and fixed or a fitted
lavaan object (in which case the column 'est' is used as value to show).
Should be NULL if infile is given.}
\item{infile}{A character string specifying the file which contains the model
syntax.}
\item{varlv}{A logical indicating that the (residual) variance of a variable
should be plotted as a separate latent variable (with a smaller circle then
ordinary LV's). In this case covariances between two such variables will be
plotted as covariance between the latent variables for the variance.}
\item{texfile}{Character, path to file to store the diagram in tikz or
NULL to disable generating tikz code.}
\item{cex}{Minimum distance between nodes in cm. Only used for tikz code.}
\item{pngfile}{Character, path to file to store the diagram in PNG or NA to
show the diagram in R. Set to NULL if no Rplot is required.}
\item{svgfile}{Character, path to file to store the diagram in SVG,
embedded in basic HTML if standalone = TRUE. If NULL (default) no svg
code is generated.}
\item{sloped_labels}{logical, sloped labels above for the edges}
\item{standalone}{Logical, add code to make teX/svg file standalone}
\item{placenodes}{optional list with members nodename = c(row, column),
row and colom don't have to be integers.}
\item{edgelabelsbelow}{optional list with members c(nodename1, nodename2).}
}
\value{
NULL, invisible
}
\description{
Creates the code to make a diagram in tikz or svg, plot the diagram or store
it in a png file. More details on the parameters can be found in the help for
the \code{lvp_...} functions.
}
\seealso{
\code{\link{lvp_get_model_info}}, \code{\link{lvp_position_nodes}},
\code{\link{lvp_make_rplot}}, \code{\link{lvp_make_tikz}},
\code{\link{lvp_make_svg}}
}
\examples{
model <- 'alpha_11  =~ 1 * x_1 + x_2 + x_3     # latent variable
          alpha_12 <~ x_4 + x_5 + x_6          # composite
          gamma =~ 1 * x_7 + x_8 + x_9         # latent variable
          xi =~ 1 * x_10 + x_11 + x_12 + x_13  # latent variable
          x_1 ~~ x_3
          x_2 ~~ epsilon_2 * x_2
          x_12 ~~ epsilon_12 * x_12
          x_4 ~~ epsilon_4 * x_4
          x_7 ~~ x_9
          x_10 ~~ x_11 + x_13
          # regressions
          xi ~ v * alpha_11 + t * alpha_12 + 1
          alpha_11 ~ yy * Theta_1 + tt_1 * 0.12 * alpha_12 + ss * gamma
          '
lavplot(model, texfile=stdout());
lavplot(model, texfile=stdout(), placenodes=list(Theta_1 = c(2, 2.5)));
modelml <- '
        level: 1
        fw =~ y_1 + y_2 + y_3 + y_4
        level: 2
        fb =~ y_1 + y_2 + y_3 + y_5
        y_2 ~~ cv24 * y_5
  '
tikzcodeml <- lavplot(modelml, svgfile=stdout())

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
lavplot(model, texfile = "tmp.tex", pngfile = NULL)
tmp <- readLines("tmp.tex")
writeLines(tmp, zz)
writeLines(" ", zz)
lavplot(modelml, texfile = "tmp.tex", cex = 1.4, sloped_labels = FALSE,
        pngfile = NULL)
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

# example creating html file with the above diagrams
zz <- file("demosvg.html", open="w")
writeLines(c(
  '<!DOCTYPE html>',
  '<html>',
  '<body>',
  '<h2>SVG diagrams created by lavplot R package</h2>'),
  zz)
lavplot(model, svgfile = "temp.svg", pngfile = NULL)
tmp <- readLines("tmp.svg")
writeLines(tmp, zz)
writeLines("<br />", zz)
lavplot(modelml, svgfile = "temp.svg", sloped_labels = FALSE,
        pngfile = NULL)
tmp <- readLines("tmp.svg")
writeLines(tmp, zz)
writeLines(c("</body>", "</html>"), zz)
close(zz)
browseURL("demosvg.html")
}
}
