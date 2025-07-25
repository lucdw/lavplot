node_elements <- function(nodetiepe, noderadius) {
  # define form, color and anchors for a node type
  thetas <- switch(nodetiepe,
                   lv = ,
                   varlv = seq(0, 2 * pi, length.out = 50L),
                   ov = ,
                   wov = ,
                   bov = seq(pi / 4, 2 * pi, by = pi / 2),
                   cv = seq(0, 2 * pi, by = pi / 3),
                   const = seq(pi / 2, 2 * pi, by = 2 * pi / 3 )
  )
  localradius <- noderadius
  if (nodetiepe == "varlv") localradius <- noderadius * .8
  drawx <- localradius * cos(thetas)
  drawy <- localradius * sin(thetas)
  boxcol <- switch(nodetiepe,
                   lv = NA_integer_,
                   varlv = NA_integer_,
                   ov = NA_integer_,
                   wov = "lightgreen",
                   bov = "lightblue",
                   cv = NA_integer_,
                   const = NA_integer_)
  n <- c(0, switch(nodetiepe,
                   lv = , varlv = , const = localradius,
                   ov = , wov = , bov = localradius * sqrt(2) / 2,
                   cv = localradius * sqrt(3) / 2))
  s <- c(0, switch(nodetiepe,
                   lv = , varlv = -localradius,
                   ov = , wov = , bov = -localradius * sqrt(2) / 2,
                   cv = -localradius * sqrt(3) / 2,
                   const = -localradius * 0.5))
  e <- switch(nodetiepe,
              lv = , varlv = , cv = c(localradius, 0),
              ov = , wov = , bov = c(localradius * sqrt(2) / 2, 0),
              const = c(localradius * sqrt(3) / 2, -localradius * 0.5))
  w <- -e
  ne <- switch(nodetiepe,
               lv = , varlv = , ov = , wov = ,
               bov = localradius * sqrt(0.5) * c(1, 1),
               cv = localradius * c(0.5, sqrt(3) / 2),
               const = e)
  nw <- c(-ne[1L], ne[2L])
  se <- switch(nodetiepe,
               lv = , varlv = , ov = , wov = ,
               bov = localradius * sqrt(0.5) * c(1, -1),
               cv = localradius * c(-0.5, sqrt(3) / 2),
               const = e)
  sw <- c(-se[1L], se[2L])
  list(drawx = drawx, drawy = drawy, boxcol = boxcol, n = n, ne = ne, e = e,
       se = se, s = s, sw = sw, w = w, nw = nw)
}
lvp_plot <- function(nodes_edges,
                     sloped_labels = TRUE,
                     addgrid = TRUE,
                     pngfile = NULL) {
  mlrij <- nodes_edges$mlrij
  if (is.null(mlrij))
    stop("nodes_edges hasn't been processed by lvp_position_nodes!")
  nodes <- nodes_edges$nodes
  edges <- nodes_edges$edges
  noderadius <- 0.3
  arrowlength <- noderadius / 3
  vecrotate <- function(vec, angle) {
    c(cos(angle)*vec[1]+sin(angle)*vec[2],
      -sin(angle)*vec[1]+cos(angle)*vec[2])
  }
  plot_arrow <- function(tip, unitvec) {
    arrowangle <- pi * 25 / 180
    arrowinset <- 0.4
    args <- rbind(tip,
                  tip + vecrotate(-unitvec * arrowlength, arrowangle),
                  tip - unitvec * arrowlength * (1 - arrowinset),
                  tip + vecrotate(-unitvec * arrowlength, -arrowangle))
    polygon(args, col = "black", border = NA)
  }
  plot_edge <- function(van, naar, label = "", dubbel = FALSE,
                        bend = 0, below = FALSE, txtcex = 0.9) {
    labele <- sub("_([[:digit:]]*)", "[\\1]", label)
    labele <- sub("varepsilon", "epsilon", labele, fixed = TRUE)
    labele <- sub("=(.*)$", "(\\1)", labele)
    labele <- str2expression(labele)
    unitvec <- (naar - van) / sqrt(sum((naar - van) * (naar - van)))
    theta <- atan2(naar[2] - van[2], naar[1] - van[1])
    srt <- ifelse(sloped_labels, 180 * theta / pi, 0)
    if (srt > 90) srt <- srt - 180
    if (srt < -90) srt <- srt + 180
    if (bend == 0) {
      args <- rbind(van, naar)
      lines(args, lwd = 2)
      plot_arrow(naar, unitvec)
      if (dubbel) plot_arrow(van, -unitvec)
      midden <- (van + naar) * 0.5
    } else {
      # gebogen lijn (cirkelsegment) door van en naar met raaklijn in
      # van die hoek van bend °  maakt met unitvec
      lengte <- sqrt(sum((naar - van) * (naar - van)))
      orthovec <- c(unitvec[2], -unitvec[1])
      middelpt <- van + lengte * unitvec / 2  +
        lengte * tan(pi/2 - bend) * orthovec / 2
      vantheta <- atan2(van[2] - middelpt[2], van[1] - middelpt[1])
      naartheta <- atan2(naar[2] - middelpt[2], naar[1] - middelpt[1])
      if (abs(naartheta-vantheta) > pi) {
        if (vantheta < naartheta) {
          vantheta <- vantheta + 2 * pi
        } else {
          naartheta <- naartheta + 2 * pi
        }
      }
      thetas <- seq(vantheta, naartheta, length.out = 40)
      straal <- veclen(van - middelpt)
      xs <- middelpt[1] + cos(thetas) * straal
      ys <- middelpt[2] + sin(thetas) * straal
      midden <- c(xs[20],ys[20])
      lines(xs, ys)
      plot_arrow(naar, vecrotate(unitvec, bend))
      if (dubbel) plot_arrow(van, vecrotate(-unitvec, -bend))
    }
    if (label != "") {
      if (below) {
        if (theta >= 0 && theta < pi / 2) {
          text(midden[1L], midden[2L], labele, adj = c(0, 1),
               srt = srt, cex = txtcex)
        } else if (theta >= pi / 2) {
          text(midden[1L], midden[2L], labele, adj = c(1, 1),
               srt = srt, cex = txtcex)
        } else if (theta < -pi/2) {
          text(midden[1L], midden[2L], labele, adj = c(0, 1),
               srt = srt, cex = txtcex)
        } else {
          text(midden[1L], midden[2L], labele, adj = c(1, 1),
               srt = srt, cex = txtcex)
        }
      } else {
        if (theta >= 0 && theta < pi / 2) {
          text(midden[1L], midden[2L], labele, adj = c(1, 0),
               srt = srt, cex = txtcex)
        } else if (theta >= pi / 2) {
          text(midden[1L], midden[2L], labele, adj = c(0, 0),
               srt = srt, cex = txtcex)
        } else if (theta < -pi/2) {
          text(midden[1L], midden[2L], labele, adj = c(1, 0),
               srt = srt, cex = txtcex)
        } else {
          text(midden[1L], midden[2L], labele, adj = c(0, 0),
               srt = srt, cex = txtcex)
        }
      }
    }
  }
  plot_var <- function(waar, noderadius, label = "", side = "n", txtcex = 0.9) {
    labele <- sub("_([[:digit:]]*)", "[\\1]", label)
    labele <- sub("varepsilon", "epsilon", labele, fixed = TRUE)
    labele <- sub("=(.*)$", "(\\1)", labele)
    labele <- str2expression(labele)
    thetarange <- c(pi / 6, 11 * pi / 6)
    if (side == "s") thetarange <- thetarange + pi / 2
    if (side == "e") thetarange <- thetarange + pi
    if (side == "n") thetarange <- thetarange + 3 * pi / 2
    localradius <- noderadius * 0.8
    middelpt <- switch(side,
                       n = c(0, localradius),
                       w = c(-localradius, 0),
                       s = c(0, -localradius),
                       e = c(localradius, 0))
    middelpt <- middelpt + waar
    # cirkelsegment
    thetas <- seq(thetarange[1L], thetarange[2L], length.out = 40)
    straal <- localradius
    xs <- middelpt[1] + cos(thetas) * straal
    ys <- middelpt[2] + sin(thetas) * straal
    lines(xs, ys)
    # pijlen
    plot_arrow(c(xs[1], ys[1]), c(sin(thetarange[1]), -cos(thetarange[1])))
    plot_arrow(c(xs[40], ys[40]), c(-sin(thetarange[2]), cos(thetarange[2])))
    # label
    if (label != "")
      text(middelpt[1L], middelpt[2L], labele, adj <- 0.5, cex = txtcex * 0.8)
  }
  plot_node <- function(waar, tiepe, label = "", txtcex = 0.9) {
    labele <- sub("_([[:digit:]]*)$", "[\\1]", label)
    labele <- sub("varepsilon", "epsilon", labele, fixed = TRUE)
    labele <- str2expression(labele)
    elems <- node_elements(tiepe, noderadius)
    x <- waar[1] + elems$drawx
    y <- waar[2] + elems$drawy
    polygon(x, y, col = elems$boxcol, lwd = 1)
    text(waar[1L], waar[2L], labele, adj <- 0.5, cex = txtcex)
  }

  rijen <- max(nodes$rij)
  kolommen <- max(nodes$kolom)
  if (!is.null(pngfile)) png(pngfile, 960, 960, "px")
  opar <- par(mar = c(1L,1L, 1L,1L) + 0.1)
  plot.default(x = c(0.25, kolommen + 0.75), c(0.25, rijen + 0.75), type="n",
               xlab = "", ylab = "", axes = FALSE, asp = 1)
  if (addgrid) {
    abline(v = seq.int(kolommen), h = seq.int(rijen), lwd = 1,
           lty = "dotted", col = "lightgray")
    text(seq.int(kolommen), 0.3, labels=seq.int(kolommen), adj = 1, cex = 0.7)
    text(0.3, seq.int(rijen), labels=seq.int(rijen, 1), adj = 1, cex = 0.7)
  }
  if (mlrij > 0L) abline(h = rijen - mlrij + 1, lwd = 2)
  yrange <- rijen - range(nodes$rij) + 1
  xrange <- range(nodes$kolom)
  midxy <- c(mean(xrange), mean(yrange))
  for (j in seq.int(nrow(edges))) {
    if (edges$naar[j] != edges$van[j]) {
      van <- which(nodes$id == edges$van[j])
      naar <- which(nodes$id == edges$naar[j])
      adrvan <- c(nodes$kolom[van], rijen - nodes$rij[van] + 1)
      elems <- node_elements(nodes$tiepe[van], noderadius)
      adrvan <- adrvan + elems[[edges$vananker[j]]]
      adrnaar <- c(nodes$kolom[naar], rijen - nodes$rij[naar] + 1)
      elems <- node_elements(nodes$tiepe[naar], noderadius)
      adrnaar <- adrnaar + elems[[edges$naaranker[j]]]
      if (edges$tiepe[j] != "~~") {
        plot_edge(adrvan, adrnaar, edges$label[j], dubbel = FALSE,
                  below = edges$labelbelow[j])
      } else {
        thetavan <- atan2(adrvan[2] - midxy[2], adrvan[1] - midxy[1])
        thetanaar <- atan2(adrnaar[2] - midxy[2], adrnaar[1] - midxy[1])
        deltatheta <- thetanaar - thetavan
        if (deltatheta < 0) deltatheta <- deltatheta + 2 * pi
        benddirection <- ifelse(deltatheta < pi, -1, 1)
        plot_edge(adrvan, adrnaar, edges$label[j], dubbel = TRUE,
                  below = edges$labelbelow[j],
                  bend = benddirection * pi/4
                  )
      }
    } else {
      van <- which(nodes$id == edges$van[j])
      adrvan <- c(nodes$kolom[van], rijen - nodes$rij[van] + 1)
      elems <- node_elements(nodes$tiepe[van], noderadius)
      adrvan <- adrvan + elems[[edges$vananker[j]]]
      plot_var(adrvan, noderadius, edges$label[j], edges$vananker[j])
    }
  }
  for (j in seq.int(nrow(nodes))) {
    plot_node(c(nodes$kolom[j], rijen - nodes$rij[j] + 1),
              nodes$tiepe[j],
              nodes$naam[j])
  }
  if (!is.null(pngfile)) dev.off()
  return(invisible(NULL))
}
