\name{lvp_position_nodes}
\alias{lvp_position_nodes}
\title{Position the nodes in the diagram}
\usage{
lvp_position_nodes(nodes, edges, allowbottom = FALSE)
}
\arguments{
\item{nodes}{The nodes data.frame as returned from lvp_get_model_info}
\item{edges}{The edges data.frame as returned from lvp_get_model_info}
\item{allowbottom}{A logical indicating if indicators can be placed at the
bottom of the graph.}
}
\value{
A modified nodes data.frame, where the columns 'rij' and 'kolom' are filled in.}
\description{
Computes the positions for the nodes in the diagram.
}
\examples{
model <- 'alfa  =~ x_1 + x_2 + x_3     # latent variable
          beta <~ x_4 + x_5 + x_6      # composite
          gamma =~ x_7 + x_8 + x_9     # latent variable
          Xi =~ x_10 + x_11 + x_12 + x_13  # latent variable
          # regressions
          Xi ~ v * alfa + t * beta
          alfa ~ tt * beta + ss * gamma + yy * Theta_1
          '
test <- lvp_get_model_info(model)
nodes <- lvp_position_nodes(test$nodes, test$edges)
}
