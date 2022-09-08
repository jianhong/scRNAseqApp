scDRcoexLeg <- function(inp1, inp2, inpcol, inpfsz){
  # Generate coex color palette
  cInp = strsplit(inpcol, "; ")[[1]]
  if(cInp[1] == "Red (Gene1)"){
    c10 = c(255,0,0)
  } else if(cInp[1] == "Orange (Gene1)"){
    c10 = c(255,140,0)
  } else {
    c10 = c(0,255,0)
  }
  if(cInp[2] == "Green (Gene2)"){
    c01 = c(0,255,0)
  } else {
    c01 = c(0,0,255)
  }
  c00 = c(217,217,217) ; c11 = c10 + c01
  nGrid = 16; nPad = 2; nTot = nGrid + nPad * 2
  gg = data.table(v1 = rep(0:nTot,nTot+1), v2 = sort(rep(0:nTot,nTot+1)))
  gg$vv1 = gg$v1 - nPad ; gg[vv1 < 0]$vv1 = 0; gg[vv1 > nGrid]$vv1 = nGrid
  gg$vv2 = gg$v2 - nPad ; gg[vv2 < 0]$vv2 = 0; gg[vv2 > nGrid]$vv2 = nGrid
  gg$cR = bilinear(gg$vv1, gg$vv2, nGrid, c00[1], c10[1], c01[1], c11[1])
  gg$cG = bilinear(gg$vv1, gg$vv2, nGrid, c00[2], c10[2], c01[2], c11[2])
  gg$cB = bilinear(gg$vv1, gg$vv2, nGrid, c00[3], c10[3], c01[3], c11[3])
  gg$cMix = rgb(gg$cR, gg$cG, gg$cB, maxColorValue = 255)
  gg = gg[, c("v1", "v2", "cMix")]

  # Actual ggplot
  ggOut = ggplot(gg, aes(v1, v2)) +
    geom_tile(fill = gg$cMix) +
    xlab(inp1) + ylab(inp2) + coord_fixed(ratio = 1) +
    scale_x_continuous(breaks = c(0, nTot), label = c("low", "high")) +
    scale_y_continuous(breaks = c(0, nTot), label = c("low", "high")) +
    sctheme(base_size = sList[inpfsz], XYval = TRUE)
  return(ggOut)
}
