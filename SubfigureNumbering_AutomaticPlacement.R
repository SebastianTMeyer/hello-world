#########################################################################
# Function to add labels/numbers or other text in the corners of a plot #
# V1.0 (2015/04/24) by Sebastian Meyer; sebastian.t.meyer@tum.de        #
#########################################################################

#Added text for testing

SubFigNumber <- function(label, place="ul", dist=0.1, dist.x=dist, dist.y=dist, abs=TRUE) {
  if (abs) {
    size <- par('din')-par("mai")[c(2,1)]-par("mai")[c(4,3)]
    dist.x <- dist.x/size[1]*100
    dist.y <- dist.y/size[2]*100
  }
  limits <- par('usr')
  limits[5] <- limits[2]-limits[1]
  limits[6] <- limits[4]-limits[3]
  if (place=="ul" | place=="upperleft") {X <- limits[1]+limits[5]*dist.x/100; Y <- limits[4]-limits[6]*dist.y/100; a <- c(0,1)}  
  if (place=="ll" | place=="lowerleft") {X <- limits[1]+limits[5]*dist.x/100; Y <- limits[3]+limits[6]*dist.y/100; a <- c(0,0)}  
  if (place=="ur" | place=="upperright") {X <- limits[2]-limits[5]*dist.x/100; Y <- limits[4]-limits[6]*dist.y/100; a <- c(1,1)}  
  if (place=="lr" | place=="lowerright") {X <- limits[2]-limits[5]*dist.x/100; Y <- limits[3]+limits[6]*dist.y/100; a <- c(1,0)}  
  text(X, Y, label, adj=a)
} 

################################################
label takes the text to be placed
place is given the information in which corner to place the text; valid values are
	"upperleft" = "ul"
	"lowerleft" = "ll"
	"upperright" = "ur"
	"lowerright" = "lr"
dist takes the distance to the edge of the plotting region in percent or in absolute distance 
  when abs=TRUE, absolute distance is given in inches
