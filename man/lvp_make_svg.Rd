\name{lvp_make_svg}
\alias{lvp_make_svg}
\title{Creates code to show a diagram in svg format.}
\usage{
lvp_make_svg(nodes_edges, sloped_labels, outfile = "")
}
\arguments{
\item{nodes_edges}{The list with nodes, edges and mlrij as returned from
    lvp_position_nodes}
\item{sloped_labels}{logical, sloped labels above (or under) the edges}
\item{outfile}{A connection or a character string naming the file to print to.}
}
\value{
NULL (invisible)}
\description{
Create svg code to show a diagram of the model.
}
\examples{
\dontrun{
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
lvp_make_svg(test)
modelml <- '
        level: 1
        fw =~ 1*y_1 + y_2 + y_3 + y_5
        level: 2
        fb =~ 1*y_1 + y_2 + y_3 + y_4
        y_2 ~~ cv24 * y_4
  '
testml <- lvp_get_model_info(modelml)
nodesml <- lvp_position_nodes(testml$nodes, testml$edges)
lvp_make_svg(nodesml, testml$edges)
}
}
