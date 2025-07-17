lvp_plot <- function(nodes, edges, addgrid = TRUE) {
  mlrij <- attr(nodes, "mlrij", exact = TRUE)
  if (is.null(mlrij)) stop("nodes hasn't been processed by position_nodes !")
  noderadius <- 0.3
  arrowangle <- pi * 30 / 180
  arrowlength <- noderadius / 3
  vecrotate <- function(vec, angle) {
    c(cos(angle)*vec[1]+sin(angle)*vec[2],
      -sin(angle)*vec[1]+cos(angle)*vec[2])
  }
  plot_edge <- function(van, naar, label = "", dubbel = FALSE, bend = 0) {
    unitvec <- (naar - van) / sqrt(sum((naar - van) * (naar - van)))
    if (bend == 0) {
      vanpt <- van + unitvec * noderadius
      naarpt <- naar - unitvec * noderadius
      if (dubbel) {
        args <- rbind(vanpt, vanpt + vecrotate(unitvec*arrowlength, arrowangle),
                      vanpt + vecrotate(unitvec*arrowlength, -arrowangle),
                      vanpt, naarpt,
                      naarpt + vecrotate(-unitvec*arrowlength, arrowangle),
                      naarpt + vecrotate(-unitvec*arrowlength, -arrowangle),
                      naarpt)
      } else {
        args <- rbind(vanpt, naarpt,
                      naarpt + vecrotate(-unitvec*arrowlength, arrowangle),
                      naarpt + vecrotate(-unitvec*arrowlength, -arrowangle),
                      naarpt)
      }
      lines(args, lwd = 2)
    } else {
      # gebogen lijn (cirkelsegment) door van en naar met raaklijn in
      # van die hoek van bend °  maakt met unitvec
      lengte <- sqrt(sum(naar - van) * (naar - van))
      orthovec <- c(unitvec[2], -unitvec[1])
      middelpt <- van + lengte * unitvec / 2  + lengte * tan(pi/2 - blend) * orthovec
      #todo: hier verder
    }
  }
  plot_node <- function(waar, tiepe, label = "") {
    boxtype <- switch(tiepe,
                          lv = "circle",
                          varlv = "circle",
                          ov = "rect",
                          wov = "rect",
                          bov = "rect",
                          cv = "hexa",
                          const = "triangle")
    boxcol <- switch(tiepe,
                         lv = NA_integer_,
                         varlv = "lightgrey",
                         ov = NA_integer_,
                         wov = "lightgreen",
                         bov = "lightblue",
                         cv = NA_integer_,
                         const = NA_integer_)
    localradius <- noderadius
    if (tiepe == "varlv") localradius <- noderadius * .8
    if (boxtype == "circle") {
      thetas <- seq(0, 2 * pi, length.out = 50L)
    } else if (boxtype == "rect") {
      thetas <- seq(pi / 4, 2 * pi, by = pi / 2)
    } else if (boxtype == "hexa") {
      thetas <- seq(0, 2 * pi, by = pi / 3)
    } else { # triangle
      thetas = seq(pi / 2, 2 * pi, by = 2 * pi / 3 )
    }
    x <- waar[1] + localradius * cos(thetas)
    y <- waar[2] + localradius * sin(thetas)
    polygon(x, y, col = boxcol, lwd = 1)
    text(waar[1L], waar[2L], label, adj <- 0.5)
  }
  rijen <- max(nodes$rij)
  kolommen <- max(nodes$kolom)
  namen <- sub("_([[:digit:]]*)$", "[\\1]", nodes$naam)
  namen <- str2expression(namen)
  edgelabels <- sub("_([[:digit:]]*)$", "[\\1]", edges$label)
  edgelabels <- str2expression(edgelabels)
  opar <- par(mar = c(1L,1L, 1L,1L) + 0.1)
  plot.default(x = c(0.25, kolommen + 0.75), c(0.25, rijen + 0.75), type="n",
               xlab = "", ylab = "", axes = FALSE, asp = 1)
  if (addgrid) {
    abline(v = seq.int(kolommen), h = seq.int(rijen), lwd = 1,
           lty = "dotted", col = "lightgray")
    text(seq.int(kolommen), 0.3, labels=seq.int(kolommen), adj = 1, cex = 0.7)
    text(0.3, seq.int(rijen), labels=seq.int(rijen, 1), adj = 1, cex = 0.7)
  }
  if (mlrij > 0L) abline(h = rijen - mlrij, lwd = 2)

  for (j in seq.int(nrow(edges))) {
    if (edges$naar[j] != edges$van[j]) {
      van <- which(nodes$id == edges$naar[j])
      naar <- which(nodes$id == edges$van[j])
      adrvan <- c(nodes$kolom[van], rijen - nodes$rij[van] + 1)
      adrnaar <- c(nodes$kolom[naar], rijen - nodes$rij[naar] + 1)
      plot_edge(adrvan, adrnaar, edgelabels[j], dubbel = edges$tiepe[j] == "d")
    } else {

    }
  }
  for (j in seq.int(nrow(nodes))) {
    plot_node(c(nodes$kolom[j], rijen - nodes$rij[j] + 1),
              nodes$tiepe[j],
              namen[j])
  }
  return(invisible(NULL))
}
