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
  if (all(is.na(nodes$rij))) {
    availcols <- availrows <- c(1L, 3L)
  } else {
    availrows <- range(nodes$rij, na.rm = TRUE)
    availcols <- range(nodes$kolom, na.rm = TRUE)
    availrows[1L] <- availrows[1] - 1L
    availrows[2L] <- availrows[2L] + 1L
    availcols[1L] <- availcols[1] - 1L
    availcols[2L] <- availcols[2L] + 1L
  }
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
  varlvs <- which(nodes$tiepe == "varlv")
  varlv <-length(varlvs) > 0L
  d_lv <- ifelse(varlv, 3L, 2L)  # distance from border
  d_ind <- ifelse(varlv, 2L, 1L) # depends on presence varlv's
  # structural part
  lvcvs <- nodes$tiepe %in% c("lv", "cv")
  if (any(lvcvs)) { # lv's or cv's present
    if (any(nodes$voorkeur == "l" & lvcvs)) {
      strucs <- which(nodes$voorkeur == "l" & lvcvs)
      maxindicatoren <- max(nodes$indicatoren[strucs])
      nodes$rij[strucs] <- d_lv + seq.int(length(strucs)) * maxindicatoren
      nodes$kolom[strucs] <- d_lv
    }
    if (any(nodes$voorkeur == "r" & lvcvs)) {
      strucs <- which(nodes$voorkeur == "r" & lvcvs)
      maxindicatoren <- max(nodes$indicatoren[strucs])
      nodes$rij[strucs] <- d_lv + seq.int(length(strucs)) * maxindicatoren
      nodes$kolom[strucs] <- 101L - d_lv
    }
    if (any(nodes$voorkeur == "m" & lvcvs)) {
      strucs <- which(nodes$voorkeur == "m" & lvcvs)
      if (length(strucs) > 1L && allowbottom) {
        vanaf <- as.integer(1L + length(strucs) / 2)
        nodes$voorkeur[strucs[seq.int(vanaf, length(strucs))]] <- "b"
      }
    }
    if (any(nodes$voorkeur == "m" & lvcvs)) {
      strucs <- which(nodes$voorkeur == "m" & lvcvs)
      maxindicatoren <- max(nodes$indicatoren[strucs])
      nodes$kolom[strucs] <- d_lv + seq.int(length(strucs)) * maxindicatoren
      nodes$rij[strucs] <- d_lv
    }
    if (any(nodes$voorkeur == "b" & lvcvs)) {
      strucs <- which(nodes$voorkeur == "b" & lvcvs)
      maxindicatoren <- max(nodes$indicatoren[strucs])
      nodes$kolom[strucs] <- d_lv + seq.int(length(strucs)) * maxindicatoren
      nodes$rij[strucs] <- 101L - d_lv
    }
    # indicators
    allindicators <- nodes$id[nodes$voorkeur == ""]
    strucs <- which(nodes$voorkeur == "l" & lvcvs)
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
          nodes$kolom[k] <- d_ind
          addedindicators <- addedindicators + 1L
          if (varlv) { # variances of indicators?
            varlvedges <- which(edges$naar == nodes$id[k] & edges$van %in% varlvs)
            if (length(varlvedges) > 0L) {
              lvvarid <- edges$van[varlvedges[1L]]
              nodes$kolom[lvvarid] <- 1L          # kolom 1
              nodes$rij[lvvarid] <- nodes$rij[k]  # zelfde rij als indicator
            }
          }
        }
      }
      if (addedindicators > 2) {
        nodes$rij[j] <- nodes$rij[j] + as.integer((addedindicators - 1) / 2)
      }
    }
    strucs <- which(nodes$voorkeur == "r" & lvcvs)
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
          nodes$kolom[k] <- 101L - d_ind
          addedindicators <- addedindicators + 1L
          if (varlv) { # variances of indicators?
            varlvedges <- which(edges$naar == nodes$id[k] & edges$van %in% varlvs)
            if (length(varlvedges) > 0L) {
              lvvarid <- edges$van[varlvedges[1L]]
              nodes$kolom[lvvarid] <- 100L        # kolom 100
              nodes$rij[lvvarid] <- nodes$rij[k]  # zelfde rij als indicator
            }
          }
        }
      }
      if (addedindicators > 2) {
        nodes$rij[j] <- nodes$rij[j] + as.integer((addedindicators - 1) / 2)
      }
    }
    strucs <- which(nodes$voorkeur == "m" & lvcvs)
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
          nodes$rij[k] <- d_ind
          addedindicators <- addedindicators + 1L
          if (varlv) { # variances of indicators?
            varlvedges <- which(edges$naar == nodes$id[k] & edges$van %in% varlvs)
            if (length(varlvedges) > 0L) {
              lvvarid <- edges$van[varlvedges[1L]]
              nodes$rij[lvvarid] <- 1L                # rij 1
              nodes$kolom[lvvarid] <- nodes$kolom[k]  # zelfde kol als indicator
            }
          }
        }
      }
      if (addedindicators > 2) {
        nodes$kolom[j] <- nodes$kolom[j] + as.integer((addedindicators - 1) / 2)
      }
    }
    strucs <- which(nodes$voorkeur == "b" & lvcvs)
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
          nodes$rij[k] <- 101L - d_ind
          addedindicators <- addedindicators + 1L
          if (varlv) { # variances of indicators?
            varlvedges <- which(edges$naar == nodes$id[k] & edges$van %in% varlvs)
            if (length(varlvedges) > 0L) {
              lvvarid <- edges$van[varlvedges[1L]]
              nodes$rij[lvvarid] <- 100L              # rij 100
              nodes$kolom[lvvarid] <- nodes$kolom[k]  # zelfde kol als indicator
            }
          }
        }
      }
      if (addedindicators > 2) {
        nodes$kolom[j] <- nodes$kolom[j] + as.integer((addedindicators - 1) / 2)
      }
    }
  } else { # only observed variables
    if (any(nodes$voorkeur == "l")) {
      strucs <- which(nodes$voorkeur == "l")
      nodes$rij[strucs] <- 1L + seq.int(length(strucs))
      nodes$kolom[strucs] <- 1L
    }
    if (any(nodes$voorkeur == "r")) {
      strucs <- which(nodes$voorkeur == "r")
      nodes$rij[strucs] <- 1L + seq.int(length(strucs))
      nodes$kolom[strucs] <- 100L
    }
    if (any(nodes$voorkeur == "m")) {
      strucs <- which(nodes$voorkeur == "m")
      if (length(strucs) > 1L && allowbottom) {
        vanaf <- as.integer(1L + length(strucs) / 2)
        nodes$voorkeur[strucs[seq.int(vanaf, length(strucs))]] <- "b"
      }
    }
    if (any(nodes$voorkeur == "m")) {
      strucs <- which(nodes$voorkeur == "m")
      nodes$kolom[strucs] <- 1L + seq.int(length(strucs))
      nodes$rij[strucs] <- 1L
    }
    if (any(nodes$voorkeur == "b")) {
      strucs <- which(nodes$voorkeur == "b")
      nodes$kolom[strucs] <- 1L + seq.int(length(strucs))
      nodes$rij[strucs] <- 100L
    }
  }
  if (all(nodes$voorkeur == "")) { # no regressions defined
      # do nothing, should be covered by search_position subroutine
  } else {
    # remove the holes in rows and columns
    rijen <- sort(unique(nodes$rij))
    nodes$rij <- match(nodes$rij, rijen)
    kolommen <- sort(unique(nodes$kolom))
    nodes$kolom <- match(nodes$kolom, kolommen)
  }
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
