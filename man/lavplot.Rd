\name{lavplot}
\alias{lavplot}
\title{Creates diagram code for tikz, svg or diagram}
\usage{
lavplot(model = NULL, infile = NULL, varlv = FALSE,
        allowbottom = FALSE, cex = 1.3,
        texfile = NULL,
        standalone = FALSE,
        pngfile = NULL,
        svgfile = NULL,
        sloped_labels = TRUE,
        placenodes = NULL,
        edgelabelsbelow = NULL,
        verbose = FALSE)
}
\arguments{
\item{model}{A character vector specifying the model in lavaan syntax or a list
(or data.frame) with at least members lhs, op, rhs, label and fixed. Optional if
infile is given.}
\item{infile}{A character string specifying the file which contains the model
syntax.}
\item{varlv}{A logical indicating that the (residual) variance of a variable
should be plotted as a seperate latent variable (with a smaller circle then
ordinary LV's). In this case covariances between two such variables will be
plotted as covariance between the latent variables for the variance.}
\item{allowbottom}{A logical indicating if indicators can be placed at the
bottom of the graph.}
\item{cex}{Minimum distance between nodes in cm.}
\item{texfile}{Character, path to file to store the diagram in tikz.}
\item{standalone}{Logical, add code to make TeX file standalone}
\item{pngfile}{Character, path to file to store the diagram in PNG.}
\item{svgfile}{Character, path to file to store the diagram in SVG.}
\item{sloped_labels}{logical, sloped labels above (or under) the edges}
\item{placenodes}{optional list with members nodename = c(row, column)}
\item{edgelabelsbelow}{optional list with members c(nodename1, nodename2)}
\item{verbose}{logical, if TRUE intermediate results are printed}
}
\value{
NULL, invisible
}
\description{
Creates the code to make a dagram in tikz or svg, plot the diagram or store
it in a png file.
}
\examples{
model <- 'alpha_11  =~ 1 * x_1 + x_2 + x_3     # latent variable
          alpha_12 <~ x_4 + x_5 + x_6      # composite
          gamma =~ 1 * x_7 + x_8 + x_9     # latent variable
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
tikzcode <- lavplot(model)
modelml <- '
        level: 1
        fw =~ y_1 + y_2 + y_3 + y_4
        level: 2
        fb =~ y_1 + y_2 + y_3 + y_5
        y_2 ~~ cv24 * y_5
  '
tikzcodeml <- lavplot(modelml)
}
