\name{lav_make_svg}
\alias{lav_make_svg}
\title{Creates code to show a diagram in svg format.}
\usage{
lav_make_svg(nodes_edges,
             outfile = "",
             sloped_labels = TRUE,
             standalone = FALSE,
             strokeWidth = 2L,
             svgFontSize = 20L,
             svgIdxFontSize = 15L,
             svgDy = 5L,
             mlovcolors = c("lightgreen", "lightblue")
             )
}
\arguments{
\item{nodes_edges}{The list with nodes, edges and mlrij as returned from
    lav_position_nodes}
\item{outfile}{A connection or a character string, the file to store the code.}
\item{sloped_labels}{logical, sloped labels for the edges}
\item{standalone}{logical, add code to make a html file with svg in it? If
FALSE (default) the outfile - if specified - must have a file extension
'svg', if TRUE the file extension must be 'htm' or 'html'.}
\item{strokeWidth}{Value for stroke-width parameter in svg.}
\item{svgFontSize}{An integer specifying normal font size to use.}
\item{svgIdxFontSize}{An integer specifying font size to use for a subscript.}
\item{svgDy}{An integer specifying the distance to move the baseline of the subscript.}
\item{mlovcolors}{Array of two colors for distinguishing ov nodes with same name
between the levels in a  multilevel model.}
}
\value{
NULL (invisible)}
\description{
Create svg code to show a diagram of the model.
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
lav_make_svg(test) # no file given, so output to R console
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
lav_make_svg(testml, sloped_labels = FALSE, standalone = TRUE)
\dontrun{
# example creating html file with the above diagrams
zz <- file("demosvg.html", open="w")
writeLines(c(
  '<!DOCTYPE html>',
  '<html>',
  '<body>',
  '<h2>SVG diagrams created by lavplot R package</h2>'),
  zz)
lav_make_svg(test, outfile = "temp.svg")
tmp <- readLines("tmp.svg")
writeLines(tmp, zz)
writeLines("<br />", zz)
lav_make_svg(testml, outfile = "temp.svg", sloped_labels = FALSE,
             standalone = TRUE)
tmp <- readLines("tmp.svg")
writeLines(tmp, zz)
writeLines(c("</body>", "</html>"), zz)
close(zz)
browseURL("demosvg.html")
}
}
