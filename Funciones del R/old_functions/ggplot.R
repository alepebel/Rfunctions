require(ggplot2,quietly=TRUE,warn.conflicts = FALSE)
#source("~/R/geom-gfit.r")
#source("~/R/geom-gsmooth.r")
#source("~/R/geom-pieron.r")
#source("~/R/geom-ellipse.r")
#source("~/R/geom-optic.r")
source("~/R/geom-errorh.r")
#stat
source("~/R/stat-gfit.r")
source("~/R/stat-gsmooth.r")
source("~/R/stat-lm.r")
source("~/R/stat-drawf.r")

#geom_gfit <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",...) 
#	GeomGfit$new(mapping = mapping, data = data, stat = stat, position = position,...)
	
geom_ellipse <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",...) 
	GeomEllipse$new(mapping = mapping, data = data, stat = stat, position = position,...)

geom_errorbarh <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",...) 
	GeomErrorbarh$new(mapping = mapping, data = data, stat = stat, position = position,...)

stat_gfit <- function (mapping = NULL, data = NULL, geom = "path", position = "identity", 
     n = 101, args = list(), ...) 
StatFit$new(mapping = mapping, data = data, geom = geom, 
    position = position, n = n, args = args, ...)

stat_gsmooth <- function (mapping = NULL, data = NULL, geom = "smooth", position = "identity", 
     n = 101, w=10,args = list(), ...) 
StatGsmooth$new(mapping = mapping, data = data, geom = geom, 
    position = position, n = n, w=w,args = args, ...)

stat_lm <- function (mapping = NULL, data = NULL, geom = "smooth", position = "identity", 
    method = "lm", formula = y ~ x, se = TRUE, n = 80, fullrange = FALSE, 
    level = 0.95, na.rm = FALSE, ...) 
StatLm$new(mapping = mapping, data = data, geom = geom, position = position, 
    method = method, formula = formula, se = se, n = n, fullrange = fullrange, 
    level = level, na.rm = na.rm, ...)

stat_drawf <- function (mapping = NULL, data = NULL, geom = "path", position = "identity", 
     n = 101, args = list(), ...) 
StatDrawF$new(mapping = mapping, data = data, geom = geom, 
    position = position, n = n, args = args, ...)



legend.key <- function(pos=c(0.9,0.8),cex=1.5)
{
	base_size <- 10
	theme_update(legend.position=pos,legend.title=theme_blank(),legend.text=theme_text(colour="black",size=base_size*cex))
}

tema <- function(index=1,base_size=12,hjust=0.3){

	if(index==1){ #gray
		theme_update(theme_grey())
		ot<-theme_update( 
		# plot.background=themerect(fill ="#3366FF"), 
		plot.background=theme_rect(fill =NA,colour=NA), 
	# panel.background=themerect(fill ="#003DF5"), 
		axis.text.x=theme_text(colour="black",size=base_size*1.25,vjust=0.9), 
		axis.text.y=theme_text(colour="black",hjust=1,size=base_size*1.25), 
		axis.title.x=theme_text(colour="black",size=base_size*1.7), 
		axis.title.y=theme_text(colour="black",size=base_size*1.7,angle=90,hjust=hjust), 
		legend.title=theme_blank(),
		legend.text=theme_text(colour="black",size=base_size*1.5),
		legend.background=theme_rect(fill=NA,colour=NA),
		legend.key=theme_rect(fill = NA, colour = NA),
#		legend.key.size=1.5,
		strip.background=theme_rect(fill="grey85",colour=NA),
		strip.text.x=theme_text(size = base_size * 1.2),
		strip.text.y=theme_text(size = base_size * 1.2, angle = -90)
		
		) 
	}else if(index==2){ # black_white
		theme_set(theme_bw())	
		ot<-theme_update( 
		plot.background=theme_rect(fill =NA,colour=NA), 
		axis.text.x=theme_text(colour="black",size=base_size*1.25), 
		axis.text.y=theme_text(colour="black",hjust=1,size=base_size*1.25), 
		axis.title.x=theme_text(colour="black",size=base_size*1.7), 
		axis.title.y=theme_text(colour="black",size=base_size*1.7,angle=90,hjust=hjust), 
		legend.title=theme_blank(),
		legend.text=theme_text(colour="black",size=base_size*1.5),
		legend.background=theme_rect(fill=NA,colour=NA),
		legend.key=theme_rect(fill = NA, colour = NA),
		strip.background=theme_rect(fill=NA,colour=NA),
		strip.text.x=theme_text(size = base_size * 1.2),
		strip.text.y=theme_text(size = base_size * 1.2, angle = -90)		
		) 	
	}else if(index==3){ # sense panel grids
		theme_set(theme_bw())	
		ot<-theme_update( 
		plot.background=theme_rect(fill =NA,colour=NA), 
		axis.text.x=theme_text(colour="black",size=base_size*1.25), 
		axis.text.y=theme_text(colour="black",hjust=1,size=base_size*1.25), 
		axis.title.x=theme_text(colour="black",size=base_size*1.7), 
		axis.title.y=theme_text(colour="black",size=base_size*1.7,angle=90,hjust=hjust), 
		legend.title=theme_blank(),
		legend.text=theme_text(colour="black",size=base_size*1.5),
		legend.background=theme_rect(fill=NA,colour=NA),
		legend.key=theme_rect(fill = NA, colour = NA),
		panel.grid.major=theme_line(colour=NA),
		panel.grid.minor=theme_line(colour=NA),		
		strip.background=theme_rect(fill=NA,colour=NA),
		strip.text.x=theme_text(size = base_size * 1.2),
		strip.text.y=theme_text(size = base_size * 1.2, angle = -90)		
		) 	
	}else if(index==4){
		theme_set(theme_bw())
		ot <- theme_update(axis.text.x=theme_text(colour="black",size=base_size*1.25), axis.text.y=theme_text(colour="black",size=base_size*1.25), axis.title.x=theme_text(colour="black",size=base_size*1.7),axis.title.y=theme_text(colour="black",size=base_size*1.7,angle=90,hjust=hjust), legend.key=theme_blank(),legend.title=theme_blank(),legend.text=theme_blank(),panel.border=theme_blank(),panel.grid.major=theme_blank(),panel.grid.minor=theme_blank(),plot.background=theme_rect(fill= NA,colour=NA),panel.background=theme_rect(fill=NA, colour = NA) )	
	}else if(index==5){
		theme_set(theme_bw())
		ot <- theme_update(axis.text.x=theme_blank(), axis.text.y=theme_blank(), axis.ticks=theme_blank(),axis.title.x=theme_text(colour="black",size=base_size*1.7),axis.title.y=theme_text(colour="black",size=base_size*1.7,angle=90,hjust=hjust), legend.key=theme_blank(),legend.title=theme_blank(),legend.text=theme_blank(),panel.border=theme_blank(),panel.grid.major=theme_blank(),panel.grid.minor=theme_blank(),plot.background=theme_rect(fill= alpha("white",0),colour=NA),panel.background=theme_rect(fill = NA, colour = NA) )	
	}
	else{ #blank axes
		theme_set(theme_bw())
		ot <- theme_update(axis.text.x=theme_blank(), axis.text.y=theme_blank(), axis.ticks=theme_blank(),axis.title.x=theme_blank(),axis.title.y=theme_blank(), legend.key=theme_blank(),legend.title=theme_blank(),legend.text=theme_blank(),panel.border=theme_blank(),panel.grid.major=theme_blank(),panel.grid.minor=theme_blank(),plot.background=theme_rect(fill=NA,colour=NA),panel.background=theme_rect(fill = alpha("white",0), colour = NA) )
	}
#	(ot)
}

add_hist <- function(p,ylim=c(0,1),scale_w=3.0,scale_h=0.01,x=0.5, y=0.5,angle=0,size=0.5,fill="gray",colour=NA)
{
	ot <- theme_get()
	tema(0)
	rx <- diff(range(p$x))
	width <- (rx/diff(ylim))*scale_w
	height <- max(p$y)*scale_h 
#	print(height)
	pl <- qplot(x=p$x,y=p$y,geom="blank") +geom_polygon(fill=fill,colour=colour,size=size)
	pushViewport(viewport(height=height, width=width, x=x, y=y,just=c(0,0),angle=angle)) 
	print(pl, newpage=FALSE) 
	upViewport() 
	theme_set(ot)
}

add_curve <- function(p,ylim=c(0,1),scale_w=3.0,scale_h=0.01,x=0.5, y=0.5,angle=0,size=0.5,colour="black",print=FALSE)
{
	ot <- theme_get()
	tema(0)
	rx <- diff(range(p$x))
	width <- (rx/diff(ylim))*scale_w
	height <- max(p$y)*scale_h 
	if(print==TRUE)
		print(c(width,height))
	pl <- qplot(x=p$x,y=p$y,geom="blank") + geom_line(colour=colour,size=size)
	pushViewport(viewport(height=height, width=width, x=x, y=y,just=c(0,0),angle=angle)) 
	print(pl, newpage=FALSE) 
	upViewport() 
	theme_set(ot)
}


white_bg <- function() 
{
	tema(1)
	theme_update(panel.background=theme_rect(fill="white"))
}

set_default_scale("colour","discrete", "hue")

color_scale <- c(rgb(228,26,28,maxColorValue=255), rgb(77,175,74,maxColorValue=255),rgb(55,126,184,maxColorValue=255),rgb(152,78,163,maxColorValue=255), rgb(255,127,0,maxColorValue=255),rgb(141,160,203,maxColorValue=255),rgb(231,138,195,maxColorValue=255),rgb(166,216,84,maxColorValue=255))


pal <- function(n=3,mode="colour")
{
	if(mode=="colour")
		(scale_colour_manual(values=color_scale[1:n]))
	else
		(scale_fill_manual(values=color_scale[1:n]))
}


palcb <- function(palette="Set1",alpha=1.0)
{
		scale_colour_brewer(palette=palette,alpha=alpha)
}

palfb <- function(palette="Set1",alpha=1.0)
{
		scale_fill_brewer(palette=palette,alpha=alpha) 
}

theme_grey2 <- function(base_size = 12,key.pos="right")  #per modificar
{ 
	structure(list(axis.line = theme_blank(), axis.text.x = theme_text(colour = "grey30",size = base_size * 0.8, lineheight = 0.9, vjust = 1), axis.text.y = theme_text(colour = "grey30",size = base_size * 0.8, hjust = 1, lineheight = 0.9), axis.ticks = theme_segment(colour = "grey50", size = 0.2), axis.title.x = theme_text(size = base_size, vjust = 0.5), axis.title.y = theme_text(size = base_size, angle = 90, vjust = 0.5), axis.ticks.length = unit(0.15,"cm"), axis.ticks.margin = unit(0.1, "cm"), legend.background = theme_rect(colour = "white"), legend.key = theme_rect(fill="grey95",colour = "white"), legend.key.size = unit(1.2,"lines"), legend.text = theme_text(size = base_size * 0.8), legend.title = theme_text(size = base_size * 0.8, face = "bold", hjust = 0), legend.position = key.pos, panel.background = theme_rect(fill = "grey90", colour = NA), panel.border = theme_blank(), panel.grid.major = theme_line(colour = "white"), panel.grid.minor = theme_line(colour = "grey95", size = 0.25), panel.margin = unit(0.25, "lines"), strip.background = theme_rect(fill = "grey80", colour = NA), strip.label = function(variable, value) value, strip.text.x = theme_text(size = base_size * 0.8), strip.text.y = theme_text(size = base_size * 0.8, angle = -90), plot.background = theme_blank(), plot.title = theme_text(size = base_size * 1.2), plot.margin = unit(c(1,1,0.5,0.5), "lines")), class = "options") 
} 

theme_bw2 <- function(base_size = 12,key.pos="right") 
{ 
	structure(list(axis.line = theme_blank(), axis.text.x = theme_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1), axis.text.y = theme_text(size = base_size * 0.8, hjust = 1, lineheight = 0.9), axis.ticks = theme_segment(colour = "black", size = 0.2), axis.title.x = theme_text(size = base_size, vjust = 0.5), axis.title.y = theme_text(size = base_size, angle = 90, vjust = 0.5), axis.ticks.length = unit(0.3,"lines"), axis.ticks.margin = unit(0, "lines"), legend.background = theme_rect(colour = NA), legend.key = theme_rect(colour = "grey80"), legend.key.size = unit(1.2,"lines"), legend.text = theme_text(size = base_size * 0.8), legend.title = theme_text(size = base_size * 0.8, face = "bold", hjust = 0), legend.position = key.pos, panel.background = theme_blank(), panel.border = theme_rect(fill = NA, colour = "grey20"), panel.grid.major = theme_line(colour = "grey90", size = 0.2), panel.grid.minor = theme_line(colour = "grey98", size = 0.5), panel.margin = unit(0.25, "lines"), strip.background = theme_rect(fill = "grey80", colour = "grey50"), strip.label = function(variable, value) value, strip.text.x = theme_text(size = base_size * 0.8), strip.text.y = theme_text(size = base_size * 0.8, angle = -90), plot.background = theme_blank(), plot.title = theme_text(size = base_size * 1.2), plot.margin = unit(c(1,1,0.5,0.5), "lines")), class = "options") 
} 

theme_noaxis <- function(base_size = 12,key.pos="right") 
{ 
	structure(list(axis.line = theme_blank(), axis.text.x = theme_blank(), axis.text.y = theme_blank(), axis.ticks = theme_blank(), axis.title.x = theme_blank(), axis.title.y = theme_blank(), axis.ticks.length = unit(0.3,"lines"), axis.ticks.margin = unit(0, "lines"), legend.background = theme_rect(colour = NA), legend.key = theme_blank(), legend.key.size = unit(1.2,"lines"), legend.text = theme_blank(), legend.title = theme_blank(), legend.position =key.pos, panel.background = theme_blank(), panel.border = theme_blank(), panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(), panel.margin = unit(0.25, "lines"), strip.background = theme_rect(fill = "grey80", colour = "grey50"), strip.label = function(variable, value) value, strip.text.x = theme_text(size = base_size * 0.8), strip.text.y = theme_text(size = base_size * 0.8, angle = -90), plot.background = theme_blank(), plot.title = theme_text(size = base_size * 1.2), plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines")), class = "options") 
} 

theme_black <- function (base_size = 12,key.pos="right") 
{ 
	structure(list(axis.line = theme_blank(), axis.text.x = theme_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1,colour = "white"), axis.text.y = theme_text(size = base_size * 0.8, hjust = 1, lineheight = 0.9,colour = "white"), axis.ticks = theme_segment(colour = "white", size = 0.2), axis.title.x = theme_text(size = base_size, vjust = 1,colour = "white"), axis.title.y = theme_text(size = base_size, vjust = 0.5, angle = 90,colour = "white"), axis.ticks.length = unit(0.3,"lines"), axis.ticks.margin = unit(0, "lines"), legend.background = theme_rect(colour = NA), legend.key = theme_rect(colour = "grey20"), legend.key.size = unit(1.2,"lines"), legend.text = theme_text(size = base_size * 0.8,,colour = "white"), legend.title = theme_text(size = base_size * 0.8, face = "bold", hjust = 0,colour = "white"), legend.position = key.pos, panel.background = theme_rect(fill = "black", colour = NA), panel.border = theme_rect(fill = NA, colour = "grey50"), panel.grid.major = theme_line(colour = "grey10", size = 0.2), panel.grid.minor = theme_line(colour = "grey2", size = 0.5), panel.margin = unit(0.25, "lines"), strip.background = theme_rect(fill = "grey20", colour = "grey50"), strip.label = function(variable, value) value, strip.text.x = theme_text(size = base_size * 0.8,colour = "white"), strip.text.y = theme_text(size = base_size * 0.8, angle = -90,colour = "white"), plot.background = theme_rect(fill = "black", colour = NA), plot.title = theme_text(size = base_size * 1.2), plot.margin = unit(c(1,1,0.5,0.5), "lines")), class = "options") 
} 



#vermell <- "#FF6C91FF"
#verd <-  rgb(0,193,169,maxColorValue=255)
#blau <- "#00A9FFFF"

################## EXAMPLES

#qplot() + opts(drop = "legend") 
#plotc <- plotb + geom_text(legend=False) 

###### canvi color text axes POST PLOT 

#grid.gedit(gPath("axis_h","axis_v", "text"), gp = gpar(col="black"))

# canvi mida font POST PLOT 

#grid.gedit("text", gp = gpar(fontsize=14))

########## Inset

#p <- qplot(time,distance_change, data=dd1, geom="line",ylab="Distance change")

## grid.newpage() # Volem mantenir el grafic 

#pushViewport(viewport(height=0.4, width=0.3, x=0.55, y=0.3)) 
#grid.rect(gp=gpar(lwd=1))
#print(p, newpage=FALSE) 
#upViewport() 
