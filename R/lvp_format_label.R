lvp_format_label <- function(label = "", value = "", show = TRUE,
                             svgIdxFontSize = 20L, svgDy = 7L) {
  latexsymbols <- c("varepsilon",
    "Alpha", "Beta", "Gamma", "Delta", "Epsilon", "Zeta", "Eta", "Theta",
    "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", "Pi",
    "Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", "Omega",
    "alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", "theta",
    "iota", "kappa", "lambda", "mu", "nu", "xi", "omicron", "pi",
    "rho", "sigma", "tau", "upsilon", "phi", "chi", "psi", "omega"
  )
  unicodes <- c("1013",
    "913", "914", "915", "916", "917", "918", "919", "920",
    "921", "922", "923", "924", "925", "926", "927", "928",
    "929", "931", "932", "933", "934", "935", "936", "937",
    "945", "946", "947", "948", "949", "950", "951", "952",
    "953", "954", "955", "956", "957", "958", "959", "960",
    "961", "963", "964", "965", "966", "967", "968", "969"
  )
  unicodesH <- c("\u3f5",
    "\u391", "\u392", "\u393", "\u394", "\u395", "\u396", "\u397", "\u398",
    "\u399", "\u39a", "\u39b", "\u39c", "\u39d", "\u39e", "\u39f", "\u3a0",
    "\u3a1", "\u3a3", "\u3a4", "\u3a5", "\u3a6", "\u3a7", "\u3a8", "\u3a9",
    "\u3b1", "\u3b2", "\u3b3", "\u3b4", "\u3b5", "\u3b6", "\u3b7", "\u3b8",
    "\u3b9", "\u3ba", "\u3bb", "\u3bc", "\u3bd", "\u3be", "\u3bf", "\u3c0",
    "\u3c1", "\u3c3", "\u3c4", "\u3c5", "\u3c6", "\u3c7", "\u3c8", "\u3c9"
  )
  if (label == "" && value == "") return(list(svg="", tikz="", r=""))
  if (value == "") {
    splitted <- strsplit(label, "=", fixed =  TRUE)[[1L]]
    if (length(splitted) > 1L) {
      label <- splitted[1L]
      value <- splitted[2L]
    }
  }
  svgpart <- tikzpart <- rpart <- character(3)
  rpart[3] <- tikzpart[3] <- svgpart[3] <- value
  if (label != "") {
    # separate label and value by equal sign
    if (svgpart[3] != "") svgpart[3] = paste0('=', svgpart[3])
    if (tikzpart[3] != "") tikzpart[3] = paste0('=', tikzpart[3])
    # subscript and greek character set handling
    splitunderscore <- strsplit(label, "_", TRUE)[[1L]]
    svgpart[1] <- tikzpart[1] <- rpart[1] <- splitunderscore[1L]
    if (rpart[1] %in% latexsymbols) {
      svgpart[1] <- paste0("&#", unicodes[latexsymbols == svgpart[1]], ";")
      tikzpart[1] <- paste0("\\", tikzpart[1])
      rpart[1] <- unicodesH[latexsymbols == rpart[1]]
    }
    if (length(splitunderscore) > 1L) {
      svgpart[2L] <- paste0('<tspan dy="', svgDy  ,'" font-size="', svgIdxFontSize,
                      '">', splitunderscore[2L], '</tspan>')
      if (svgpart[3L] != "") {
        svgpart[3L] <- paste0('<tspan dy="-', svgDy, '">', svgpart[3L], '</tspan>')
      }
      tikzpart[2L] <- paste0('_{', splitunderscore[2L], '}')
      rpart[2L] <- paste0(splitunderscore[2L])
    }
  }
  if (rpart[1L] == "") {
    rexpr <- substitute(val, list(val = rpart[3L]))
  } else {
    if (rpart[3L] == "") {
      if (rpart[2L] == "") {
        rexpr <- substitute(naam, list(naam = rpart[1L]))
      } else {
        rexpr <- substitute(naam[index],
                            list(naam = rpart[1], index = rpart[2]))
      }
    } else {
      if (rpart[2L] == "") {
        rexpr <- substitute(naam == val,
                            list(naam = rpart[1L], val = rpart[3L]))
      } else {
        rexpr <- substitute(naam[index] == val,
                            list(naam = rpart[1], index = rpart[2], val = rpart[3L]))
      }
    }
  }
  if (show) {
    plot(c(0,3), c(0,2), type = "n")
    text(1, 1, rexpr)
  }
  list(svg = paste(svgpart, collapse = ""),
       tikz = paste(c("$", tikzpart, "$"), collapse = ""),
       r = rexpr)
}
