\name{lvp_position_nodes}
\alias{lvp_position_nodes}
\title{Position the nodes in the diagram}
\usage{
lvp_position_nodes(nodes_edges,
                   placenodes = NULL,
                   edgelabelsbelow = NULL,
                   allowbottom = FALSE)
}
\arguments{
\item{nodes_edges}{The nodes and edges data.frames in a list as
        returned from lvp_get_model_info}
 \item{placenodes}{optional list with members nodename = c(row, column),
row and colom don't have to be integers}
\item{edgelabelsbelow}{optional list with members c(nodename1, nodename2)}
\item{allowbottom}{A logical indicating if indicators can be placed at the
      bottom of the graph.}
}
\value{
A list with modified nodes and edges data.frames, and an integer mlrij giving
the position at which a line should drawn for multilevel models.}
\description{
Computes the positions for the nodes and anchors for edges in the diagram.
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
test_positioned <- lvp_position_nodes(test)
}
