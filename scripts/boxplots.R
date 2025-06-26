lop <- c('raster', 'data.table', 'ncdf4', 'lubridate', 'ggplot2', 'tidyverse', 'tidyr', 'plyr', 'assertthat')
to.instal <- lop[which(!lop %in% installed.packages()[,'Package'])]
if(length(to.instal) != 0) install.packages(to.instal)
temp <- lapply(lop, library, character.only = T)
rm(temp)

dta <- readRDS("./data/dta_final_daily.rds")
dta<-dta[year>=1979 & year<=2022,]
dta<-dta[, count := .N, by = .(charLONLAT)] 
max(dta[, count])
dta<-dta[count==16071,]
dta<-dta[, mean_region_pe_p:=mean(PE-dp),by='DTM'] 
dta<-dta[,mean_TG_region:=mean(dtg), by='DTM']
dta<-dta[,mean_P_region:=mean(dp), by='DTM']

dta_JS<-dta[month %in% c(6,9),]
dta_JS[month==5,]
dta_JS[DTM=='1979-06-01',]
dta_JS<-unique(dta_JS, by='DTM')
dta_JS[DTM=='1979-06-01',]

meanJS<-mean(dta_JS[,mean_region_pe_p]) 
medianJS<-median(dta_JS[,mean_region_pe_p])

###
dta <- readRDS("./data/dta_final_daily.rds")
dta<-dta[year>=1979 & year<=2022,]
dta<-dta[, count := .N, by = .(charLONLAT)] 
max(dta[, count])
dta<-dta[count==16071,]

WF_types <- as.data.table(readRDS('./data/cir_types.rds'))
database = fread('./data/databaze_3DHWs_ME.csv',sep=';')

database<-database[,.(Start, End, Length, Extremity, Type)]
database[,DTM:=as.Date(Start)]
database<-database[,ID:=c(1:.N)]

database_missing<-database[, {lst1 <- Map(seq, as.IDate(Start, "%Y-%m-%d"),
                                          as.IDate(End, "%Y-%m-%d"),
                                          MoreArgs = list(by = "day"))
.(ID = rep(ID, lengths(lst1)), DTM = do.call(c, lst1))}]
database_missing<-join(database_missing, database, by=c('DTM', 'ID'))
database_missing<-database_missing %>% fill(Start, End, Length, Extremity, Type)
database_missing<-as.data.table(database_missing)
database<-database_missing

WF_types_join<-join(dta, WF_types,by = "DTM")
WF_types_join2<-join(WF_types_join, database,by = "DTM")
WF_types_join2[DTM=='1979-01-01',]
WF_types_join2<-WF_types_join2[, mean_region_pe_p:=mean(PE-dp),by='DTM']
WF_types_join2<-WF_types_join2[,mean_TG_region:=mean(dtg), by='DTM']
WF_types_join2<-WF_types_join2[,mean_P_region:=mean(dp), by='DTM']
WF_types_join2[DTM=='1979-07-25',]

mer_during<-unique(WF_types_join2, by='DTM')
mer_during<-na.omit(mer_during, by='Start')
WF_types_join2[DTM=='1980-07-24',]
mer_during[DTM=='1980-07-24',]
mer_unique<-unique(WF_types_join2, by='DTM')

mer_unique<-mer_unique[, c("pred1", "pred2", "pred3", "po1", "po2", "po3") := shift(mean_region_pe_p, n = c(1,2,3, -1, -2, -3))]

merging_Start<-mer_unique[DTM==Start,]
merging_End<-mer_unique[DTM==End,]
merging_Start1<-merging_Start[,mean_before_Type:= mean(c(pred1), na.rm=TRUE), by=Start] 
merging_Start2<-merging_Start[,mean_before_Type2:= mean(c(pred2), na.rm=TRUE), by=Start] 
merging_Start3<-merging_Start[,mean_before_Type3:= mean(c(pred3), na.rm=TRUE), by=Start] 
merging_Start<-rbind(merging_Start1[, .(Type, mean_before_Type)], merging_Start2[, .(Type, mean_before_Type2)], merging_Start3[, .(Type, mean_before_Type3)], use.names=FALSE)

merging_End1<-merging_End[,mean_after_Type:= mean(c(po1), na.rm=TRUE), by=End]
merging_End2<-merging_End[,mean_after_Type2:= mean(c(po2), na.rm=TRUE), by=End] 
merging_End3<-merging_End[,mean_after_Type3:= mean(c(po3), na.rm=TRUE), by=End] 
merging_End<-rbind(merging_End1[, .(Type, mean_after_Type)], merging_End2[, .(Type, mean_after_Type2)], merging_End3[, .(Type, mean_after_Type3)], use.names=FALSE)

start_end<-cbind(merging_Start[,.(Type, mean_before_Type)], merging_End[, .(mean_after_Type)])
setnames(start_end, c('mean_before_Type', 'mean_after_Type'), c('3_days_before', '3_days_after'))
start_end <- melt(start_end, id='Type')
mer_during<-mer_during[,.(Type, mean_region_pe_p)]
setnames(mer_during, c('mean_region_pe_p'), c('during_HW'))
MER2 <- melt(mer_during[,.(Type, during_HW)], id='Type')
LLL<-rbind(start_end, MER2)

dta_JS2<-dta_JS[,.(mean_region_pe_p)]
setnames(dta_JS2, c('mean_region_pe_p'), c('June-Sep.'))
dta_JS2<-dta_JS2[,Type:=rep('June-Sep.', 2640)]
dta_JS2_melt <- melt(dta_JS2, id='Type')
LLL<-rbind(LLL, dta_JS2_melt)

position <- as.factor(c('HWG', 'HWL', 'HWH', 'HWO', 'June-Sep.'))
LLL$variable <- factor(LLL$variable, levels = c('3_days_before', 'during_HW', '3_days_after', 'June-Sep.'))

f <- function(x) {
  r <- quantile(LLL, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

### whisker definition function
stat_boxplot_custom <- function(mapping = NULL, data = NULL,
                                geom = "boxplot", position = "dodge2",
                                ...,
                                qs = c(.05, .25, 0.5, 0.75, 0.95),
                                na.rm = FALSE,
                                orientation = NA,
                                show.legend = NA,
                                inherit.aes = TRUE) {
  assert_that(
    length(qs) == 5 && is.numeric(qs),
    msg = "`qs` should be a numeric vector with 5 values."
  )
  
  assert_that(
    all(qs == sort(qs)), 
    msg = "`qs` should be provided in ascending order."
  )
  
  assert_that(
    all(qs <= 1) && all(qs >= 0),
    msg = "`qs` should only span values [0, 1]."
  )
  
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatBoxplotCustom,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      orientation = orientation,
      qs = qs,
      ...
    )
  )
}

StatBoxplotCustom <- ggplot2::ggproto("StatBoxplotCustom", ggplot2::Stat,
                                      required_aes = c("y|x"),
                                      non_missing_aes = "weight",
                                      dropped_aes = c("x", "y", "weight"),
                                      setup_data = function(self, data, params) {
                                        data <- ggplot2::flip_data(data, params$flipped_aes)
                                        data$x <- ggplot2:::"%||%"(data$x, 0)
                                        data <- ggplot2::remove_missing(
                                          data,
                                          na.rm = params$na.rm,
                                          vars = "x",
                                          name = "stat_boxplot_custom"
                                        )
                                        ggplot2::flip_data(data, params$flipped_aes)
                                      },
                                      
                                      setup_params = function(self, data, params) {
                                        params$flipped_aes <- ggplot2::has_flipped_aes(data, params, 
                                                                                       main_is_orthogonal = TRUE,
                                                                                       group_has_equal = TRUE,
                                                                                       main_is_optional = TRUE)
                                        data <- ggplot2::flip_data(data, params$flipped_aes)
                                        
                                        has_x <- !(is.null(data$x) && is.null(params$x))
                                        has_y <- !(is.null(data$y) && is.null(params$y))
                                        if (!has_x && !has_y) {
                                          abort("stat_boxplot() requires an x or y aesthetic.")
                                        }
                                        
                                        params$width <- ggplot2:::"%||%"(
                                          params$width, 
                                          (ggplot2::resolution(ggplot2:::"%||%"(data$x, 0) * 0.75))
                                        ) 
                                        
                                        if (!ggplot2:::is_mapped_discrete(data$x) && is.double(data$x) && 
                                            !ggplot2:::has_groups(data) && any(data$x != data$x[1L])) {
                                          rlang::warn(glue::glue(
                                            "Continuous {flipped_names(params$flipped_aes)$x} aesthetic -- did you forget aes(group=...)?"
                                          ))
                                        }
                                        
                                        params
                                      },
                                      
                                      extra_params = c("na.rm", "orientation"),
                                      
                                      compute_group = function(data, scales, width = NULL, na.rm = FALSE, 
                                                               qs = c(.05, .25, 0.5, 0.75, 0.95), flipped_aes = FALSE) {
                                        
                                        data <- ggplot2::flip_data(data, flipped_aes)
                                        
                                        if (!is.null(data$weight)) {
                                          mod <- quantreg::rq(y ~ 1, weights = weight, data = data, tau = qs)
                                          stats <- as.numeric(stats::coef(mod))
                                        } else {
                                          stats <- as.numeric(stats::quantile(data$y, qs))
                                        }
                                        names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
                                        iqr <- diff(stats[c(2, 4)])
                                        
                                        outliers <- (data$y < stats[1]) | (data$y > stats[5])
                                        
                                        if (vctrs::vec_unique_count(data$x) > 1)
                                          width <- diff(range(data$x)) * 0.9
                                        
                                        df <- ggplot2:::data_frame0(!!!as.list(stats))
                                        df$outliers <- list(data$y[outliers])
                                        
                                        if (is.null(data$weight)) {
                                          n <- sum(!is.na(data$y))
                                        } else {
                                          n <- sum(data$weight[!is.na(data$y) & !is.na(data$weight)])
                                        }
                                        
                                        df$notchupper <- df$middle + 1.58 * iqr / sqrt(n)
                                        df$notchlower <- df$middle - 1.58 * iqr / sqrt(n)
                                        
                                        df$x <- if (is.factor(data$x)) data$x[1] else mean(range(data$x))
                                        df$width <- width
                                        df$relvarwidth <- sqrt(n)
                                        df$flipped_aes <- flipped_aes
                                        ggplot2::flip_data(df, flipped_aes)
                                      }
)

###

boxplots<-ggplot(data = LLL, aes(x = factor(Type, levels = position), y = value, fill = variable)) + 
  theme_bw()+
  scale_y_continuous(breaks = c(-5, -3, -1, 1, 3, 5))+
  theme(panel.grid.major.x = element_blank())+
  geom_vline(xintercept = c(1.5, 2.5, 3.5, 4.5), color='grey')+
  coord_cartesian(ylim=c(-3.5,5))+ 
  scale_fill_manual(name= '                   ', values = c("3_days_before" = "#D84315", 
                                                            "during_HW" = "#4CAF50", 
                                                            "3_days_after" = "#0288D1", 
                                                            "June-Sep." = "#9E9E9E"),
                    labels = c("3_days_before"="3 days before     ", "during_HW"="during HW     ", "3_days_after"="3 days after     ", "June-Sep."="June-Sep.     ") , 
                    breaks = c('3_days_before', 'during_HW', '3_days_after', 'June-Sep.'))+
  stat_boxplot_custom(qs = c(0.1, 0.25, 0.50, 0.75, 0.9), 
                      outlier.shape = NA, 
                      notch = FALSE, 
                      width = 0.67, 
                      position = position_dodge2(width = 0.67, preserve = "single"))+
  labs(y= "PET-P [mm/d]")+
  theme(axis.ticks.x=element_blank())+
  theme(axis.title.x = element_blank())+
  theme(axis.text.x = element_text(size=12, face="bold", colour = "black"))+
  theme(legend.position="bottom")+
  theme(legend.title=element_blank())

#ggsave(boxplots, file="./figures/boxplots.png", height = 10 , width = 15, units = "cm", bg='white')

