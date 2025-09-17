\name{lav_position_nodes}
\alias{lav_position_nodes}
\title{Position the nodes in the diagram}
\usage{
lav_position_nodes(nodes.edges,
                   placenodes = NULL,
                   edgelabelsbelow = NULL,
                   group.covar.indicators = FALSE)
}
\arguments{
\item{nodes.edges}{The nodes and edges data.frames in a list as
        returned from \code{lav_get_model_info}}
 \item{placenodes}{optional list with members \code{\var{nodename} = c(row, column)},
\code{row} and \code{column} don't have to be integers.}
\item{edgelabelsbelow}{optional list with members \code{c(\var{nodename1},
\var{nodename2})}.}
\item{group.covar.indicators}{logical, should items with indicators which
have an explicit covariance link be placed in the same group, i.e. forced
to be on the same side of the diagram?}
}
\value{
A list with modified \code{nodes} and \code{edges} data.frames, and an
integer \code{mlrij} giving the position at which a line should be drawn
for multilevel models.

The data.frames have new columns defined as follows:
\enumerate{
\item \code{nodes}
  \describe{
    \item{rij}{place for the row where the node will be placed, initialized NA}
    \item{kolom}{place for the column where the node will be placed,
    initialized NA}
  }
\item \code{edges}
  \describe{
    \item{vananker}{character, anchor point for starting node, initialized NA}
    \item{naaranker}{character, anchor point for destination node,
    initialized NA}
    \item{controlpt.kol}{real, column position of control point if the
    edge has to be drawn as a quadratic Béziers curve}
    \item{controlpt.row}{real, row position of control point if the
    edge has to be drawn as a quadratic Béziers curve}
    \item{labelbelow}{logical, if label has to be positioned under the line,
    initalized FALSE}
  }
}
}
\description{
Computes the positions for the nodes and anchors for edges in the diagram.
}
\details{
This functions tries to arrange the nodes and anchor points for the edges in
a way that gives a not-to-large diagram and taking into account the \code{voorkeur}
column in the input nodes data.frame. If the result is not what you want, you can
take a look at the plot via \code{lav_make_rplot} or \code{lavplot} functions and
decide yourself to place some nodes at another location (\code{placenodes})
and/or put some edge labels at the other side of the edge (\code{labelsbelow}).

For multilevel models the nodes in block 2 are grouped at the top of the
diagram and those in block 1 at the bottom. The item \code{mlrij} gives the
position of the separation between the blocks.
}
\seealso{
\code{\link{lav_get_model_info}}, \code{\link{lav_make_rplot}},
\code{\link{lavplot}}
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
          x_3 ~~ cc_34 * x_4
          gamma ~~ 0.55 * gamma
          '
test <- lav_get_model_info(model)
(test_positioned <- lav_position_nodes(test))
}
