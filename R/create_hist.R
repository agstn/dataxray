#' Create interactive histogram
#'
#' Code adapted from Hmisc package's spike histograms.
#'
#' @importFrom dplyr %>%
#' @import forcats
#' @importFrom Hmisc htmlSN plotlyParm
#' @import plotly
#'
#' @param x Variable from dataset in vector form.
#' @param counts Counts component of Hmisc::describe() output
#' @param values Values component of Hmisc::describe() output
#'
#' @return Plotly figure containing spike histogram (numeric) or traditional histogram (character).
#'
#' @examples
#' adsl <- safetyData::adam_adsl
#' create_hist(adsl$AGE)
#' create_hist(adsl$SITEID)
#'
#' @export
create_hist <- function(x, counts, values) {
  UseMethod("create_hist", x)
}

#' @export
create_hist.character <- function(x, counts, values){

  y <- -1
  fmt <- function(x) Hmisc::htmlSN(x, digits=5)
  height <- Hmisc::plotlyParm$heightDotchart(1.2)

  n_distinct <- counts[["distinct"]] %>% as.numeric

  if (n_distinct>0){
    dh <- data.frame(x = as.character(values$value), freq = values$frequency, y = y) %>%
      mutate(prop = freq/sum(freq),
             txt = paste0(x, '<br>', round(prop, 3), '<br>n=', freq))

    if (is.factor(x)){
      x_levs <- levels(x)
      dh <- dh %>% mutate(x = factor(x, levels = x_levs))
    } else {
      if (n_distinct > 30){
        dh <- dh %>%
          arrange(x) %>%
          mutate(x = factor(x))
      } else {
        dh <- dh %>%
          mutate(x = fct_reorder(x, prop) %>% fct_rev)
      }
    }

    min_width = 50
    max_width = 250

    dh$prop <- 0.6 * dh$prop / max(dh$prop)  # scale the proportions

    width <- ifelse(n_distinct == 1, 17,
                    ifelse(n_distinct == 2, 35,
                           ifelse(n_distinct == 3, min_width,
                                  ifelse(n_distinct < max_width, scales::rescale(log(n_distinct),
                                                                                 to = c(min_width, max_width),
                                                                                 from = log(c(3, max_width))),
                                         max_width)) ) )


    # initiate plot
    p <- plotly::plot_ly(width = max_width+20,
                         height = 60)

    # add spike histogram lines
    p <- plotly::add_bars(p,
                          data = dh,
                          x = ~x,
                          y = ~ prop,
                          hoverinfo = 'text',
                          hovertext = ~ txt,
                          color = I('gray50'),
                          showlegend = FALSE)


    plotly::layout(p,
                   margin = list(l = 10, r = max_width+10-width, b = 0, t = 0, pad = 0),
                   xaxis = list(title='',
                                zeroline=FALSE,
                                visible = FALSE,
                                fixedrange = TRUE),
                   yaxis = list(title='',
                                visible = FALSE,
                                tickvals= -1,
                                ticktext = 1,
                                fixedrange = TRUE),
                   hoverlabel = list(font=list(size=12))) %>%
      plotly::config(displayModeBar = F) %>%
      plotly::partial_bundle()
  }
}

#' @export
create_hist.hms  <- create_hist.character

#' @export
create_hist.difftime  <- create_hist.character

#' @export
create_hist.factor <- create_hist.character

#' @export
create_hist.logical <- create_hist.character

#' @export
create_hist.numeric <- function(x, counts, values){
  
  n_distinct <- counts[["distinct"]] %>% as.numeric
  
  if (n_distinct>0) {
  y <- -1
  fmt <- function(x) Hmisc::htmlSN(x, digits=5)
  height <- Hmisc::plotlyParm$heightDotchart(1.2)


      dh <- data.frame(x = values$value, freq = values$frequency, y = y) %>%
        mutate(prop = freq/sum(freq),
               txt = paste0(x, '<br>', round(prop, 3), '<br>n=', freq))

      #  mean
      dm <- data.frame(Mean=counts[["Mean"]] %>% as.numeric,
                       n=counts[["n"]] %>% as.numeric,
                       miss=counts[["missing"]] %>% as.numeric,
                       y=y)%>%
        mutate(txt = paste0('Mean: ', Mean, '<br>n=', n, '<br>', miss, ' missing'))


    height <- Hmisc::plotlyParm$heightDotchart(1.2)

    dh$prop <- 0.6 * dh$prop / max(dh$prop)  # scale the proportions

    p <- plotly::plot_ly(width  = 270,
                         height = 60)


    # add spike histogram lines
    p <- plotly::add_segments(p,
                              data=dh,
                              x = ~ x,       # segments to be drawn at data values on x-axis
                              xend = ~ x,
                              y = -1,        # segments to start at y=-1 and be length of % of values
                              yend = ~ -1 + prop,
                              text = ~ txt,       # hovertext to display for data values
                              hoverinfo = 'text',
                              color = I('gray50'),
                              showlegend = FALSE)

    a <- 0.05
    b <- 0.4
    k <- (a + b) / 6
    w <- (b - a) / 8

    # add mean point
    p <- plotly::add_markers(p,
                             data=dm,
                             mode='markers',
                             color=I('black'),
                             x = ~ Mean,
                             y = ~ y - k,
                             text = ~ txt,
                             hoverinfo = 'text',
                             size=I(10),
                             name='Mean',
                             showlegend=FALSE)
}

    if (n_distinct>=10) {
      #  quartiles
      probs <- c(".05", ".25", ".50", ".75", ".95")
      qu <- counts[probs] %>% as.numeric
      nam <- paste0('Q0', probs)
      txt <- paste0(nam, ': ', fmt(qu))
      dq1 <- data.frame(Median=qu[3], txt=txt[3], y=y)              # hovertext for Median
      dq2 <- data.frame(quartiles=qu[c(2,4)], txt=txt[c(2,4)], y=y) # hovertext for 25th & 75th
      dq3 <- data.frame(outer=qu[c(1,5)], txt=txt[c(1,5)], y=y)     # hovertext for 5th & 95th

    # function to add segments for the quartiles
    segs <- function(p, x, y, yend, text, data, color, name, width=2) {

      plotly::add_segments(p,
                           data=data,
                           x=x,    y=y,
                           xend=x, yend=yend,
                           text=text,
                           hoverinfo='text',
                           name=name,
                           showlegend=FALSE,
                           color=color,
                           line=list(width=width))
    }

    p <- segs(p, x=~Median,    y=~y-k-w,    yend=~y-k+w,    text=~txt, data=dq1, color=I('gray50'),name='Median', width=3)
    p <- segs(p, x=~quartiles, y=~y-k-w*.8, yend=~y-k+w*.8, text=~txt, data=dq2, color=I('blue'),  name='Quartiles')
    onam <- '0.05, 0.95<br>Quantiles'
    p <- segs(p, x=~outer,    y=~y-k-w*.64, yend=~y-k+w*.64, text=~txt, data=dq3, color=I('red'),  name=onam)

    # connect the quartiles w/ a line
    ys <- -1 - k
    qs <- function(p, x, xend, color, lg){

      plotly::add_segments(p,
                           x=x,   xend=xend,
                           y=~ys, yend=~ys,
                           hoverinfo='none',
                           showlegend=FALSE,
                           alpha=0.3, color=color,
                           name='ignored')
    }

    p <- qs(p, x= ~ qu[1], xend=~ qu[2], color=I('red'),  lg=onam)
    p <- qs(p, x= ~ qu[2], xend=~ qu[4], color=I('blue'), lg='Quartiles')
    p <- qs(p, x= ~ qu[4], xend=~ qu[5], color=I('red'),  lg=onam)

    }
    
    if (n_distinct==0){
      # initiate plot
      
      min_width = 50
      max_width = 250
      width = 17
      
      p <- plotly::plot_ly(width = max_width+20,
                           height = 60)
    }

    plotly::layout(p,
                   margin = list(l = 10, r = 10, b = 0, t = 0, pad = 0),
                   xaxis = list(title='',
                                zeroline=FALSE,
                                visible = FALSE,
                                fixedrange = FALSE),
                   yaxis = list(title='',
                                visible = FALSE,
                                tickvals= -1,
                                ticktext = 1,
                                fixedrange = TRUE),
                   hoverlabel = list(font=list(size=12),
                                     align = "left"))%>%
      plotly::config(displayModeBar = F) %>%
      plotly::partial_bundle()

}


#' @export
create_hist.Date <- function(x, counts, values){

  y <- -1
  fmt <- function(x) Hmisc::htmlSN(x, digits=5)
  height <- Hmisc::plotlyParm$heightDotchart(1.2)

  n_distinct <- counts[["distinct"]] %>% as.numeric

  
  if (n_distinct >0){
    dh <- data.frame(x = values$value, freq = values$frequency, y = y) %>%
      mutate(prop = freq/sum(freq),
             txt = paste0(x, '<br>', round(prop, 3), '<br>n=', freq))

    #  mean
    dm <- data.frame(Mean=counts[["Mean"]] %>% as.Date,
                     n=counts[["n"]] %>% as.numeric,
                     miss=counts[["missing"]] %>% as.numeric,
                     y=y)%>%
      mutate(txt = paste0('Mean: ', Mean, '<br>n=', n, '<br>', miss, ' missing'))

  height <- Hmisc::plotlyParm$heightDotchart(1.2)

  dh$prop <- 0.6 * dh$prop / max(dh$prop)  # scale the proportions

  p <- plotly::plot_ly(width  = 270,
                       height = 60)


  # add spike histogram lines
  p <- plotly::add_segments(p,
                            data=dh,
                            x = ~ x,       # segments to be drawn at data values on x-axis
                            xend = ~ x,
                            y = -1,        # segments to start at y=-1 and be length of % of values
                            yend = ~ -1 + prop,
                            text = ~ txt,       # hovertext to display for data values
                            hoverinfo = 'text',
                            color = I('gray50'),
                            showlegend = FALSE)

  a <- 0.05
  b <- 0.4
  k <- (a + b) / 6
  w <- (b - a) / 8

  # add mean point
  p <- plotly::add_markers(p,
                           data=dm,
                           mode='markers',
                           color=I('black'),
                           x = ~ Mean,
                           y = ~ y - k,
                           text = ~ txt,
                           hoverinfo = 'text',
                           size=I(10),
                           name='Mean',
                           showlegend=FALSE)


  if (n_distinct >=10){

    #  quartiles
    probs <- c(".05", ".25", ".50", ".75", ".95")
    qu <- counts[probs] %>% as.Date
    nam <- paste0('Q0', probs)
    txt <- paste0(nam, ': ', fmt(qu))
    dq1 <- data.frame(Median=qu[3], txt=txt[3], y=y)              # hovertext for Median
    dq2 <- data.frame(quartiles=qu[c(2,4)], txt=txt[c(2,4)], y=y) # hovertext for 25th & 75th
    dq3 <- data.frame(outer=qu[c(1,5)], txt=txt[c(1,5)], y=y)     # hovertext for 5th & 95th

  # function to add segments for the quartiles
  segs <- function(p, x, y, yend, text, data, color, name, width=2) {

    plotly::add_segments(p,
                         data=data,
                         x=x,    y=y,
                         xend=x, yend=yend,
                         text=text,
                         hoverinfo='text',
                         name=name,
                         showlegend=FALSE,
                         color=color,
                         line=list(width=width))
  }

  p <- segs(p, x=~Median,    y=~y-k-w,    yend=~y-k+w,    text=~txt, data=dq1, color=I('gray50'),name='Median', width=3)
  p <- segs(p, x=~quartiles, y=~y-k-w*.8, yend=~y-k+w*.8, text=~txt, data=dq2, color=I('blue'),  name='Quartiles')
  onam <- '0.05, 0.95<br>Quantiles'
  p <- segs(p, x=~outer,    y=~y-k-w*.64, yend=~y-k+w*.64, text=~txt, data=dq3, color=I('red'),  name=onam)

  # connect the quartiles w/ a line
  ys <- -1 - k
  qs <- function(p, x, xend, color, lg){

    plotly::add_segments(p,
                         x=x,   xend=xend,
                         y=~ys, yend=~ys,
                         hoverinfo='none',
                         showlegend=FALSE,
                         alpha=0.3, color=color,
                         name='ignored')
  }

  p <- qs(p, x= ~ qu[1], xend=~ qu[2], color=I('red'),  lg=onam)
  p <- qs(p, x= ~ qu[2], xend=~ qu[4], color=I('blue'), lg='Quartiles')
  p <- qs(p, x= ~ qu[4], xend=~ qu[5], color=I('red'),  lg=onam)

  }
  

  plotly::layout(p,
                 margin = list(l = 10, r = 10, b = 0, t = 0, pad = 0),
                 xaxis = list(title='',
                              zeroline=FALSE,
                              visible = FALSE,
                              fixedrange = FALSE),
                 yaxis = list(title='',
                              visible = FALSE,
                              tickvals= -1,
                              ticktext = 1,
                              fixedrange = TRUE),
                 hoverlabel = list(font=list(size=12),
                                   align = "left"))%>%
    plotly::config(displayModeBar = F) %>%
    plotly::partial_bundle()
  }

}


#' @export
create_hist.POSIXct  <- create_hist.Date

#' @export
create_hist.POSIXlt  <- create_hist.Date


#' @export
create_hist.timePOSIXt <- function(x, counts, values){

  y <- -1
  fmt <- function(x) Hmisc::htmlSN(x, digits=5)
  height <- Hmisc::plotlyParm$heightDotchart(1.2)

  n_distinct <- counts[["distinct"]] %>% as.numeric


    dh <- data.frame(x = values$value, freq = values$frequency, y = y) %>%
      mutate(prop = freq/sum(freq)) %>%
      separate(x, c("date","time"), sep = " ", remove = FALSE) %>%
      mutate(txt = paste0(time, '<br>', round(prop, 3), '<br>n=', freq)) %>%
      select(-date)

    #  mean
    dm <- data.frame(Mean=counts[["Mean"]] %>% hms::as_hms() %>% lubridate::as_datetime(),
                     n=counts[["n"]] %>% as.numeric,
                     miss=counts[["missing"]] %>% as.numeric,
                     y=y) %>%
      mutate(txt = paste0('Mean: ', hms::as_hms(Mean), '<br>n=', n, '<br>', miss, ' missing'))


  height <- Hmisc::plotlyParm$heightDotchart(1.2)

  dh$prop <- 0.6 * dh$prop / max(dh$prop)  # scale the proportions

  p <- plotly::plot_ly(width  = 270,
                       height = 60)


  # add spike histogram lines
  p <- plotly::add_segments(p,
                            data=dh,
                            x = ~ x,       # segments to be drawn at data values on x-axis
                            xend = ~ x,
                            y = -1,        # segments to start at y=-1 and be length of % of values
                            yend = ~ -1 + prop,
                            text = ~ txt,       # hovertext to display for data values
                            hoverinfo = 'text',
                            color = I('gray50'),
                            showlegend = FALSE)

  a <- 0.05
  b <- 0.4
  k <- (a + b) / 6
  w <- (b - a) / 8

  # add mean point
  p <- plotly::add_markers(p,
                           data=dm,
                           mode='markers',
                           color=I('black'),
                           x = ~ Mean,
                           y = ~ y - k,
                           text = ~ txt,
                           hoverinfo = 'text',
                           size=I(10),
                           name='Mean',
                           showlegend=FALSE)

  if (n_distinct >=10){

    #  quartiles
    probs <- c(".05", ".25", ".50", ".75", ".95")
    qu0 <- counts[probs] %>% hms::as_hms()
    qu <- qu0 %>% lubridate::as_datetime()
    nam <- paste0('Q0', probs)
    txt <- paste0(nam, ': ', qu0)
    dq1 <- data.frame(Median=qu[3], txt=txt[3], y=y)              # hovertext for Median
    dq2 <- data.frame(quartiles=qu[c(2,4)], txt=txt[c(2,4)], y=y) # hovertext for 25th & 75th
    dq3 <- data.frame(outer=qu[c(1,5)], txt=txt[c(1,5)], y=y)     # hovertext for 5th & 95th

  # function to add segments for the quartiles
  segs <- function(p, x, y, yend, text, data, color, name, width=2) {

    plotly::add_segments(p,
                         data=data,
                         x=x,    y=y,
                         xend=x, yend=yend,
                         text=text,
                         hoverinfo='text',
                         name=name,
                         showlegend=FALSE,
                         color=color,
                         line=list(width=width))
  }

  p <- segs(p, x=~Median,    y=~y-k-w,    yend=~y-k+w,    text=~txt, data=dq1, color=I('gray50'),name='Median', width=3)
  p <- segs(p, x=~quartiles, y=~y-k-w*.8, yend=~y-k+w*.8, text=~txt, data=dq2, color=I('blue'),  name='Quartiles')
  onam <- '0.05, 0.95<br>Quantiles'
  p <- segs(p, x=~outer,    y=~y-k-w*.64, yend=~y-k+w*.64, text=~txt, data=dq3, color=I('red'),  name=onam)

  # connect the quartiles w/ a line
  ys <- -1 - k
  qs <- function(p, x, xend, color, lg){

    plotly::add_segments(p,
                         x=x,   xend=xend,
                         y=~ys, yend=~ys,
                         hoverinfo='none',
                         showlegend=FALSE,
                         alpha=0.3, color=color,
                         name='ignored')
  }

  p <- qs(p, x= ~ qu[1], xend=~ qu[2], color=I('red'),  lg=onam)
  p <- qs(p, x= ~ qu[2], xend=~ qu[4], color=I('blue'), lg='Quartiles')
  p <- qs(p, x= ~ qu[4], xend=~ qu[5], color=I('red'),  lg=onam)

  }

  plotly::layout(p,
                 margin = list(l = 10, r = 10, b = 0, t = 0, pad = 0),
                 xaxis = list(title='',
                              zeroline=FALSE,
                              visible = FALSE,
                              fixedrange = FALSE),
                 yaxis = list(title='',
                              visible = FALSE,
                              tickvals= -1,
                              ticktext = 1,
                              fixedrange = TRUE),
                 hoverlabel = list(font=list(size=12),
                                   align = "left"))%>%
    plotly::config(displayModeBar = F) %>%
    plotly::partial_bundle()

}
