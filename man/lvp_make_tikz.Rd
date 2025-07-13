\name{lvp_make_tikz}
\alias{lvp_make_tikz}
\title{Creates diagram code for tikz}
\usage{
lvp_make_tikz(nodes, edges, cex = 1.75, outfile = NULL)
}
\arguments{
\item{nodes}{The nodes data.frame as returned from position_nodes}
\item{edges}{The edges data.frame as returned from get_model_info}
\item{cex}{Minimum distance between nodes in cm.}
\item{outfile}{Where to write the result.}
}
\value{
A character array - invisible - with the generated code.
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
nodes <- lvp_position_nodes(test$nodes, test$edges)
print(nodes)
nodes$kolom[19L] <- 2 # move Theta_1 one position to the left
print(test$edges)
test$edges$labelbelow[19L] <- TRUE # label of arrow below the line
tikzcode <- lvp_make_tikz(nodes, test$edges)
}
