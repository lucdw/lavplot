delta_node_edge <- function(edgevan, edgenaar, node) {
  van <- edgevan - node
  naar <- edgenaar - node
  # scherpe hoek tussen 0-van en 0-naar, dus 0 ligt buiten cirkel met
  # middellijn van-naar en afstand derhalve minimaal sqrt(2)/2
  if (sum(van * naar) > 0) return(sqrt(2)/2)
  determ <- van[1L] * naar[2L] - van[2L] * naar[1L]
  if (determ == 0) return(0)
  inverse <- matrix(c(naar[2L], -naar[1L], -van[2L], van[1L]),
                    nrow=2, byrow=TRUE) / determ
  a <- sum(inverse[1L, ])
  b <- sum(inverse[2L, ])
  return(1/sqrt(a*a + b*b))
}
delta_nodes <- function(node1, node2) {
  if (any(is.na(node2))) return(-1)
  delta <- node2 - node1
  sqrt(sum(delta*delta))
}
search_position <- function(j, nodes, edges) {
  thisnode <- c(nodes$rij[j], nodes$kolom[j])
  availrows <- range(nodes$rij, na.rm = TRUE)
  availcols <- range(nodes$kolom, na.rm = TRUE)
  availpositions <- list((availrows[2L] - availrows[1L] + 1L) *
                           (availcols[2L] - availcols[1L] + 1L))
  positie <- 0L
  nodestocheck <- nodes[!is.na(nodes$rij), ]
  for (trij in seq.int(availrows[1L], availrows[2L])) {
    for (tkol in seq.int(availcols[1L], availcols[2L])) {
      if (any(nodestocheck$rij == trij & nodestocheck$kolom == tkol)) next
      positie <- positie + 1L
      availpositions[[positie]] <- c(trij, tkol)
    }
  }
  availpositions <- availpositions[seq_len(positie)]
  mogelijk <- vapply(availpositions, function(availpos) {
    distances <- vapply(seq_len(nrow(edges)), function(i) {
      if (edges$van[i] != edges$naar[i] &&
          !is.na(nodes$rij[edges$van[i]]) &&
          !is.na(nodes$rij[edges$naar[i]])) {
        afstand <- delta_node_edge(
          c(nodes$rij[edges$van[i]], nodes$kolom[edges$van[i]]),
          c(nodes$rij[edges$naar[i]], nodes$kolom[edges$naar[i]]),
          availpos
        )
        afstand
      } else {
        99
      }
    }, 0.0)
    if (min(distances) > 0.5) return(TRUE)
    return(FALSE)
  }, TRUE)
  availpositions <- availpositions[mogelijk]
  meandistances <- vapply(availpositions, function(availpos) {
    distances <- vapply(seq_len(nrow(edges)), function(i) {
      if (edges$van[i] == j && edges$naar[i] != j) {
        return(delta_nodes(availpos, c(nodes$rij[edges$naar[i]],
                                       nodes$kolom[edges$naar[i]])))
      }
      if (edges$naar[i] == j && edges$van[i] != j) {
        return(delta_nodes(availpos, c(nodes$rij[edges$van[i]],
                                       nodes$kolom[edges$van[i]])))
      }
      -1
    }, 0.0)
    if (all(distances < 0)) {
      bestposition <- as.integer(c(mean(availrows), mean(availcols)))
      return(delta_nodes(availpos, bestposition))
    } else {
      return(mean(distances[distances > 0]))
    }
  }, 0.0)
  mindistance <- min(meandistances)
  k <- which(meandistances == mindistance)[1L]
  return(availpositions[[k]])
}
lvp_position_nodes <- function(nodes, edges, allowbottom = FALSE) {
  if (length(nodes$rij) == 1L) {
    nodes$rij[1L] = 1L
    nodes$kolom[1L] = 1L
    return(nodes)
  }
  if (any(nodes$blok > 0L)) { # multilevel, only level:1 and level:2 accepted
    nodes1 <- nodes[nodes$blok == 2L, ]
    nodes1$blok <- 0L
    nodes2 <- nodes[nodes$blok == 1L, ]
    nodes2$blok <- 0L
    nodes1 <- lvp_position_nodes(nodes1, edges)
    nodes2 <- lvp_position_nodes(nodes2, edges)
    rijen1 <- max(nodes1$rij)
    nodes2$rij <- nodes2$rij + rijen1 + 1L
    nodes1$blok <- 2L
    nodes2$blok <- 1L
    nodes <- rbind(nodes1, nodes2)
    attr(nodes, "mlrij") <- rijen1 + 1L
    return(nodes)
  }
  # structural part
  if (any(nodes$voorkeur == "l" & nodes$tiepe %in% c("lv", "cv"))) {
    strucs <- which(nodes$voorkeur == "l" & nodes$tiepe %in% c("lv", "cv"))
    maxindicatoren <- max(nodes$indicatoren[strucs])
    nodes$rij[strucs] <- 2L + seq.int(length(strucs)) * maxindicatoren
    nodes$kolom[strucs] <- 2L
  }
  if (any(nodes$voorkeur == "r" & nodes$tiepe %in% c("lv", "cv"))) {
    strucs <- which(nodes$voorkeur == "r" & nodes$tiepe %in% c("lv", "cv"))
    maxindicatoren <- max(nodes$indicatoren[strucs])
    nodes$rij[strucs] <- 2L + seq.int(length(strucs)) * maxindicatoren
    nodes$kolom[strucs] <- 99L
  }
  if (any(nodes$voorkeur == "m" & nodes$tiepe %in% c("lv", "cv"))) {
    strucs <- which(nodes$voorkeur == "m" & nodes$tiepe %in% c("lv", "cv"))
    if (length(strucs) > 1L && allowbottom) {
      vanaf <- integer(length(strucs) / 2)
      nodes$voorkeur[strucs[seq.int(vanaf, length(strucs))]] <- "b"
    }
  }
  if (any(nodes$voorkeur == "m" & nodes$tiepe %in% c("lv", "cv"))) {
    strucs <- which(nodes$voorkeur == "m" & nodes$tiepe %in% c("lv", "cv"))
    maxindicatoren <- max(nodes$indicatoren[strucs])
    nodes$kolom[strucs] <- 2L + seq.int(length(strucs)) * maxindicatoren
    nodes$rij[strucs] <- 2L
  }
  if (any(nodes$voorkeur == "b" & nodes$tiepe %in% c("lv", "cv"))) {
    strucs <- which(nodes$voorkeur == "b" & nodes$tiepe %in% c("lv", "cv"))
    maxindicatoren <- max(nodes$indicatoren[strucs])
    nodes$kolom[strucs] <- 2L + seq.int(length(strucs)) * maxindicatoren
    nodes$rij[strucs] <- 99L
  }
  # indicators
  allindicators <- nodes$id[nodes$voorkeur == ""]
  strucs <- which(nodes$voorkeur == "l" & nodes$tiepe %in% c("lv", "cv"))
  for (j in strucs) {
    indicatorids <- c(edges$van[edges$naar == nodes$id[j]],
                      edges$naar[edges$van == nodes$id[j]])
    indicatorids <- intersect(indicatorids, allindicators)
    indicators <- which(nodes$id %in% indicatorids)
    rijtje <- nodes$rij[j]
    addedindicators <- 0L
    for (k in indicators) {
      if (is.na(nodes$rij[k])) {
        nodes$rij[k] <- rijtje
        rijtje <- rijtje + 1L
        nodes$kolom[k] <- 1L
        addedindicators <- addedindicators + 1L
      }
    }
    if (addedindicators > 2) {
      nodes$rij[j] <- nodes$rij[j] + as.integer((addedindicators - 1) / 2)
    }
  }
  strucs <- which(nodes$voorkeur == "r" & nodes$tiepe %in% c("lv", "cv"))
  for (j in strucs) {
    indicatorids <- c(edges$van[edges$naar == nodes$id[j]],
                      edges$naar[edges$van == nodes$id[j]])
    indicatorids <- intersect(indicatorids, allindicators)
    indicators <- which(nodes$id %in% indicatorids)
    rijtje <- nodes$rij[j]
    addedindicators <- 0L
    for (k in indicators) {
      if (is.na(nodes$rij[k])) {
        nodes$rij[k] <- rijtje
        rijtje <- rijtje + 1L
        nodes$kolom[k] <- 100L
        addedindicators <- addedindicators + 1L
      }
    }
    if (addedindicators > 2) {
      nodes$rij[j] <- nodes$rij[j] + as.integer((addedindicators - 1) / 2)
    }
  }
  strucs <- which(nodes$voorkeur == "m" & nodes$tiepe %in% c("lv", "cv"))
  for (j in strucs) {
    indicatorids <- c(edges$van[edges$naar == nodes$id[j]],
                      edges$naar[edges$van == nodes$id[j]])
    indicatorids <- intersect(indicatorids, allindicators)
    indicators <- which(nodes$id %in% indicatorids)
    kolompje <- nodes$kolom[j]
    addedindicators <- 0L
    for (k in indicators) {
      if (is.na(nodes$rij[k])) {
        nodes$kolom[k] <- kolompje
        kolompje <- kolompje + 1L
        nodes$rij[k] <- 1L
        addedindicators <- addedindicators + 1L
      }
    }
    if (addedindicators > 2) {
      nodes$kolom[j] <- nodes$kolom[j] + as.integer((addedindicators - 1) / 2)
    }
  }
  strucs <- which(nodes$voorkeur == "b" & nodes$tiepe %in% c("lv", "cv"))
  for (j in strucs) {
    indicatorids <- c(edges$van[edges$naar == nodes$id[j]],
                      edges$naar[edges$van == nodes$id[j]])
    indicatorids <- intersect(indicatorids, allindicators)
    indicators <- which(nodes$id %in% indicatorids)
    kolompje <- nodes$kolom[j]
    addedindicators <- 0L
    for (k in indicators) {
      if (is.na(nodes$rij[k])) {
        nodes$kolom[k] <- kolompje
        kolompje <- kolompje + 1L
        nodes$rij[k] <- 100L
        addedindicators <- addedindicators + 1L
      }
    }
    if (addedindicators > 2) {
      nodes$kolom[j] <- nodes$kolom[j] + as.integer((addedindicators - 1) / 2)
    }
  }
  # remove the holes in rows and columns
  rijen <- sort(unique(nodes$rij))
  nodes$rij <- match(nodes$rij, rijen)
  kolommen <- sort(unique(nodes$kolom))
  nodes$kolom <- match(nodes$kolom, kolommen)
  attr(nodes, "mlrij") <- 0L
  # handle nodes which are not yet placed
  while(any(is.na(nodes$rij))) {
    j <- which(is.na(nodes$rij))[1L]
    x <- search_position(j, nodes, edges)
    nodes$rij[j] <- x[1L]
    nodes$kolom[j] <- x[2L]
  }
  return(nodes)
}
