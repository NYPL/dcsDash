gen_hc_tooltip <- "function() {
                        dot = '<span style=\"color:' + this.point.color + '\">\u25CF</span> ';
                        return dot + this.point.tooltip;
                      }"

pie_tooltip <- "function() {
                  dot = '<span style=\"color:' + this.point.color + '\">\u25CF</span> ';
                  perc = Highcharts.numberFormat(this.point.percentage, 0, '.',',');
                  return dot + this.point.tooltip ;
                }"

element_tooltip <- "function() {
                    header = '<span style=\"font-size: 10px\">' + this.key + '</span><br/>';
                    dot = '<span style=\"color:' + this.point.series.color + '\">\u25CF</span> ';
                    month = this.series.name;
                    num = '<b> ' + Highcharts.numberFormat(this.point.y, 1) + '% </b>';
                    
                    if (this.point.series.color === '#fc8d59') {
                      return dot + num + ' of ' + this.key + ' elements had a failing score in ' + month;
                    } 
                    else if (this.point.series.color === '#fee08b') {
                      return dot + num + ' of ' + this.key + ' elements had a partially passing score in ' + month;
                    } else {
                      return dot + num + ' of ' + this.key + ' elements had a passing score in ' + month;
                    }
                  }"

prop_tooltip <- "function() {
                        header = '<span style=\"font-size: 10px\">' + this.key + '</span><br/>';
                        dot = '<span style=\"color:' + this.point.series.color + '\">\u25CF</span> ';
                        rec_type = this.series.name;
                        num = '<b> ' + Math.abs(Highcharts.numberFormat(this.point.y, 1, '.',',')) + '% </b>';
                        return header + dot + num + 'of items ' + rec_type + ' minimum mandatory requirements.';
                      }"

line_tooltip <- "function() {
                        header = '<span style=\"font-size: 10px\">' + this.key + '</span><br/>';
                        dot = '<span style=\"color:' + this.point.series.color + '\">\u25CF</span> ';
                        rec_type = this.series.name;
                        num = '<b> ' + Math.abs(Highcharts.numberFormat(this.point.y, 1, '.',',')) + '% </b>';
                        return header + dot + num + 'of items ' + rec_type + '.';
                     }"

approvals_tooltip <- "function() {
                      header = '<span style=\"font-size: 10px\">' + this.key + '</span><br/>';
                      dot = '<span style=\"color:' + this.point.series.color + '\">\u25CF</span> ';
                      rec_type = this.series.name.split(' ')[0];
                      
                      month = Highcharts.dateFormat('%B %Y', new Date(this.key));
                      num = '<b> ' + Highcharts.numberFormat(this.point.y, -1, '.',',') + '</b> ';
                      rec_type_sp = (this.point.y > 1) ? rec_type : rec_type.slice(0, -1);
                      was_were = (this.point.y > 1) ? 'were' : 'was';
                      
                      if (this.series.name.includes('other rights')) {
                        return dot + num + rec_type_sp + ' with other rights '+ was_were +' approved in ' + month;
                      } else if (this.series.name.includes('on DC')) {
                        return dot + num + rec_type_sp + ' on Digital Collections '+ was_were +' approved in ' + month;
                      } else {
                        return dot + num + rec_type_sp + ' that can be displayed on NYPL premises '+ was_were +' approved in ' + month;
                      }
                    }"

qhex <- c('#022b43','#045a8d','#2b8cbe','#74a9cf','#bdc9e1')
nhex <- c('#016c59','#2ca25f')
tor_tooltip <- function(q_to_label_js){
  paste0("function() {
         var header = '<span style=\"font-size: 10px\">' + this.key + '</span><br/>';
         ",q_to_label_js,"
         var value = change[this.series.name];
         var num = '<b> ' + Highcharts.numberFormat(this.point.y, -1, '.',',') + '</b>';
         var dot = '<span style=\"color:' + this.point.series.color + '\">\u25CF</span> ';
         return header + dot + value + ': ' + num;
        }")
}

name_role_tooltip <- function(q_to_label_js){
        paste0("function() {",q_to_label_js,"
         var header = '<span style=\"font-size: 10px\">' + change[this.key] + '</span><br/>';
         var cat = this.series.name;
         var num = '<b> ' + Highcharts.numberFormat(this.point.y, -1, '.',',') + '%</b>';
         var dot = '<span style=\"color:' + this.point.series.color + '\">\u25CF</span> ';
         return header + dot + cat + ': ' + num;
        }")
}

#### what plotlines do we need (proportion by division plot) ####
create_facet_line <- function(line_text, line_val, rotation = 0) {
  list(
    label = list(text = line_text, rotation = rotation, align = "left", verticalAlign = "top", y = 20), #, textAlign = "center"
    color = "lightgrey",
    width = 5,
    value = line_val
  )
}

calc_line_value <- function(df, center_str, line_val) {
  line_val+(df %>% filter(center == center_str) %$% length(unique(code)))
}

get_center_facet_lines <- function(df) {
  centers <- unique(df$center)
  # print(centers)
  plines <- list()
  idx <- 1
  line_val <- -0.5
  
  if ('LPA' %in% centers) {
    plines[[idx]] <- create_facet_line('LPA',line_val)
    idx <- idx+1
    line_val <- calc_line_value(df, 'LPA',line_val)
  }
  if ('SASB' %in% centers) {
    plines[[idx]] <- create_facet_line('SASB',line_val)
    idx <- idx+1
    line_val <- calc_line_value(df, 'SASB',line_val)
  }
  if ('SCH' %in% centers) {
    plines[[idx]] <- create_facet_line('SCH',line_val)
    idx <- idx+1
    line_val <- calc_line_value(df, 'SCH',line_val)
  }
  if ('SIBL' %in% centers) {
    plines[[idx]] <- create_facet_line('SIBL',line_val)
  }
  return(plines)
}

get_FY_facet_lines <- function(month_start, month_end) {
  fiscal_year_start <- if_else(between(month(month_start),7,12),year(month_start)+1,year(month_start))
  fiscal_year_end <- if_else(between(month(month_end),7,12),year(month_end)+1,year(month_end))
  line_val <- -0.5
  year_range <- fiscal_year_start:fiscal_year_end
  plines <- list(rep(NA, length(year_range)))
  for(i in year_range) {
    idx <- which(i == year_range)
    plines[[idx]] <- create_facet_line(paste('FY',i),line_val)
    this_ymd <- if_else(idx == 1, ymd(paste0(i,'-',month(month_start),'-',day(month_start))),  ymd(paste0(i,'-07-01')))
    if (month(this_ymd) == 7) {
      line_val <- line_val+12
    } else {
      line_val <- line_val+6
    }
  }
  return(plines)
}
