\name{lvp_get_model_info}
\alias{lvp_get_model_info}
\title{Get model info}
\usage{
lvp_get_model_info(model = NULL, infile = NULL, varlv = FALSE)
}
\arguments{
\item{model}{A character vector specifying the model in lavaan syntax or a list
(or data.frame) with at least members lhs, op, rhs, label and fixed or a fitted
lavaan object (in which case the column 'est' is used as value to show.
Should be NULL if infile is given.}
\item{infile}{A character string specifying the file which contains the model
syntax.}
\item{varlv}{A logical indicating that the (residual) variance of a variable
should be plotted as a seperate latent variable (with a smaller circle then
ordinary LV's). In this case covariances between two such variables will be
plotted as covariance between the latent variables for the variance.}
}

\value{
A list with members nodes and edges, containing the necessary data to create
a diagram.
}

\description{
Gets information from model necessary to produce a plot.
}

\details{
The return value consists of a list of two data.frames:
\enumerate{
\item nodes
  \describe{
    \item{id}{integer, autoincrement identification of the node}
    \item{naam}{character, name of the node as specified in the model, but
    prefixed if in a multilevel model two nodes are generated from the same
    model variable}
    \item{tiepe}{character, type of node: ov (observed variable),
    lv (latent variable), varlv (variance as latent variable),
    cv (composite variable), wov (within level variable in multilevel model),
    bov (between variable in multilevel model), const (intercept of regression)}
    \item{blok}{integer, level (0 if not a multilevel model)}
    \item{voorkeur}{character, preferred place in the plot, l (left), r(right)
    or m = (middle)}
    \item{indicatoren}{integer, number of indicators for a latent variable or
    a composite}
    \item{rij}{place for the row where the node will be placed, initialized NA}
    \item{kolom}{place for the column where the node will be placed,
    initialized NA}
  }
\item edges
  \describe{
    \item{id}{integer, autoincrement identification of the edge}
    \item{label}{character, label for the edge, made of the label specified in
    the model and the fixed value if present}
    \item{van}{integer, id of the starting node}
    \item{naar}{integer, id of the destination node}
    \item{vananker}{character, anchor point for starting node, initialized NA}
    \item{naaranker}{character, anchor point for destination node,
    initialized NA}
    \item{tiepe}{character,  like op in lavaan model, except (residual)
    variance, coded here as '~~~'.}
    \item{labelbelow}{logical, if label has to be positioned under the line,
    initalized FALSE}
  }
}
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
}
