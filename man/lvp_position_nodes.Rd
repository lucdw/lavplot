\name{lvp_position_nodes}
\alias{lvp_position_nodes}
\title{Position the nodes in the diagram}
\usage{
lvp_position_nodes(nodes_edges,
                   placenodes = NULL,
                   edgelabelsbelow = NULL)
}
\arguments{
\item{nodes_edges}{The nodes and edges data.frames in a list as
        returned from lvp_get_model_info}
 \item{placenodes}{optional list with members nodename = c(row, column),
row and colom don't have to be integers}
\item{edgelabelsbelow}{optional list with members c(nodename1, nodename2)}
}
\value{
A list with modified nodes and edges data.frames, and an integer \code{mlrij} giving
the position at which a line should be drawn for multilevel models.}
\description{
Computes the positions for the nodes and anchors for edges in the diagram.
}
\details{
This functions tries to arrange the nodes and anchor points for the edges in
a way that gives a not-to-large diagram and taking into account the \code{voorkeur}
column in the input nodes data.frame. If the result is not what you want, you can
take a look at the plot via \code{lvp_make_rplot} of \code{lavplot} functions and
decide yourself to place some nodes at another location (\code{placenodes})
and/or put some edge labels at the other side of the edge (\code{labelsbelow}).

For multilevel models the nodes in block 2 are grouped at the top of the
diagram and those in block 1 at the bottom. The item \code{mlrij} gives the position
of the separation between the blocks.
}
\examples{
model <- 'alfa  =~ 1 * x_1 + x_2 + x_3         # latent variable
          beta <~ x_4 + x_5 + x_6              # composite
          gamma =~ 1 * x_7 + x_8 + x_9         # latent variable
          Xi =~ 1 * x_10 + x_11 + x_12 + x_13  # latent variable
          # regressions
          Xi ~ v * alfa + t * beta + cc * 1
          alfa ~ tt * beta + ss * gamma + yy * Theta_1
          # variances and covariances
          x_2 ~~ cc_25 * x_5
          x_3 ~~ cc_36 * x_6
          gamma ~~ 0.55 * gamma
          '
test <- lvp_get_model_info(model)
(test_positioned <- lvp_position_nodes(test))
}
