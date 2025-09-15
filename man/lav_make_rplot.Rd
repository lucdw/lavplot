\name{lav_make_rplot}
\alias{lav_make_rplot}
\title{Plots a diagram.}
\usage{
lav_make_rplot(nodes.edges,
               sloped.labels = TRUE,
               outfile = "",
               addgrid = TRUE,
               mlovcolors = c("lightgreen", "lightblue"),
               lightness = 1
               )
}
\arguments{
\item{nodes.edges}{The data.frames and mlrij as returned from position_nodes}
\item{sloped.labels}{Logical, sloped labels above (or under) the edges}
\item{outfile}{Character string naming the file to store the diagram in PNG or
  NA to show the plot in R.}
\item{addgrid}{Logical, add a grid with indicated 'positions' to the graph?}
\item{mlovcolors}{Array of two colors for distinguishing ov nodes with same name
between the levels in a  multilevel model.}
\item{lightness}{A scalar factor to modify the distances between nodes.}
}
\value{
NULL (invisible)}
\description{
Creates a plot in R showing a simple diagram of the model.
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
test <- lav_get_model_info(model)
test <- lav_position_nodes(test)
lav_make_rplot(test)
modelml <- '
        level: 1
        fw =~ 1*y_1 + y_2 + y_3 + y_5
        level: 2
        fb =~ 1*y_1 + y_2 + y_3 + y_4
        y_2 ~~ cv24 * y_4
  '
test <- lav_get_model_info(modelml)
test <- lav_position_nodes(test)
lav_make_rplot(test, sloped.labels = FALSE, addgrid = FALSE)
\dontrun{
# example where plot is stored in a PNG file
lav_make_rplot(test, outfile="demo_rplot.png")
}
}
