\name{lvp_make_tikz}
\alias{lvp_make_tikz}
\title{Creates diagram code for tikz}
\usage{
lvp_make_tikz(nodes_edges, outfile, cex = 1.3,
          sloped_labels = TRUE, standalone = FALSE)
}
\arguments{
\item{nodes_edges}{The list with nodes, edges and mlrij as returned from
    lvp_position_nodes}
\item{outfile}{Where to write the result.}
\item{cex}{Minimum distance between nodes in cm.}
\item{sloped_labels}{logical, sloped labels above (or under) the edges}
\item{standalone}{logical, add code to make TeX file standalone?}
}
\value{
NULL, invisible
}
\description{
Creates the code to make a tikz diagram in tikz.
}
\examples{
model <- 'alpha  =~ x_1 + x_2 + x_3     # latent variable
          beta <~ x_4 + x_5 + x_6      # composite
          gamma =~ x_7 + x_8 + x_9     # latent variable
          Xi =~ x_10 + x_11 + x_12 + x_13  # latent variable
          x_4 ~~ vx4 * x_4
          x_7 ~~ x_9
          # regressions
          Xi ~ v * alpha + t * beta + 1
          alpha ~ yy * Theta_1 + tt * beta + ss * gamma
          '
test <- lvp_get_model_info(model)
testpos <- lvp_position_nodes(test)
lvp_make_tikz(testpos, stdout())
}
