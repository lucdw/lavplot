\name{lvp_make_rplot}
\alias{lvp_make_rplot}
\title{Plots a diagram.}
\usage{
lvp_make_rplot(nodes_edges, sloped_labels = TRUE, addgrid = TRUE, pngfile = NULL)
}
\arguments{
\item{nodes_edges}{The data.frames and mlrij as returned from position_nodes}
\item{sloped_labels}{logical, sloped labels above (or under) the edges}
\item{addgrid}{Logical, add a grid with indicated 'positions' to the graph?}
\item{pngfile}{A character string naming the file to store the diagram in PNG.}
}
\value{
NULL (invisible)}
\description{
Create R code to show a simple diagram of the model as an R-plot.
}
\examples{
model <- 'alpha  =~ x_1 + x_2 + x_3     # latent variable
          beta <~ x_4 + x_5 + x_6      # composite
          gamma =~ x_7 + x_8 + x_9     # latent variable
          Xi =~ x_10 + x_11 + x_12 + x_13  # latent variable
          # regressions
          Xi ~ v * alpha + t * beta + 1
          alpha ~ yy * Theta_1 + tt * beta + ss * gamma
          '
test <- lvp_get_model_info(model)
test <- lvp_position_nodes(test)
lvp_make_rplot(test)
modelml <- '
        level: 1
        fw =~ 1*y_1 + y_2 + y_3 + y_5
        level: 2
        fb =~ 1*y_1 + y_2 + y_3 + y_4
        y_2 ~~ cv24 * y_4
  '
test <- lvp_get_model_info(modelml)
test <- lvp_position_nodes(test)
lvp_make_rplot(test)
}
