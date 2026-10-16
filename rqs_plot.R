require(tidyverse)
require(viridis)
require(scales)

# ----------------------
# Figure 1 Panel A B timeseries of use sustainability terms in reviews

rq1_review_timeseries <- read_csv("data/rq1_review_timeseries.csv")

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442","black" ,"#D55E00")


rq1_panelA<-rq1_review_timeseries%>%
        mutate(product_category = str_replace_all(product_category,'_',' '))%>%
  ggplot(aes(x=year,y=share_review_certification_mention,
                    ymin = ci_lo_review_certification,
                    ymax = ci_hi_review_certification,
                    fill=product_category,
                    color=product_category))+
                    geom_point()+geom_line()+
                    geom_ribbon(alpha=0.5,linetype = 0)+
    scale_color_manual(values = okabe_ito)+
    scale_fill_manual(values = okabe_ito)+
    scale_y_continuous(labels = label_percent())+
                    theme_minimal(base_size = 16)+
    theme(axis.text = element_text(color="black"),
          legend.position = 'bottom')+
    labs(x="",y="Percent [%]",
         subtitle='A) Ecolabel mention',
         fill="Product Category",
         color="Product Category")

rq1_panelB<-rq1_review_timeseries%>%
  mutate(product_category = str_replace_all(product_category,'_',' '))%>%
  ggplot(aes(x=year,y=share_review_sustainability_mention,
             ymin = ci_lo_review_sustainability,
             ymax = ci_hi_review_sustainability,
             fill=product_category,
             color=product_category))+
  geom_point()+geom_line()+
  geom_ribbon(alpha=0.5,linetype = 0)+
  scale_color_manual(values = okabe_ito)+
  scale_fill_manual(values = okabe_ito)+
  scale_y_continuous(labels = label_percent())+
  theme_minimal(base_size = 16)+
  theme(axis.text = element_text(color="black"),
        legend.position = 'none')+
  labs(x="",y="",
       subtitle='B) Informal sustainability language use',
       fill="Product Category",
       color="Product Category")

require(patchwork)
rq1_panelA+rq1_panelB

# ----------------------
# RQ1 small plot 

rq1_durable_smallplot <- read_csv("data/rq1_durable_smallplot.csv")

rq1_durable_smallplot%>%pivot_longer(cols = c(full_pct,excl_durable_pct),
                                     names_to = "share_type",values_to = "values")%>%

ggplot(aes(year, values/100, colour = share_type, linetype = share_type)) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.6) +
  scale_colour_manual(values = c("full_pct" = "#1B7837","excl_durable_pct" = "#999999")) +
  scale_linetype_manual(values = c("full_pct" = "solid", "excl_durable_pct" = "22")) +
  scale_y_continuous(labels = label_percent())+
 # scale_x_continuous(breaks = 2015:2023) +
  labs(x = NULL, y = "Informal sustainability \nlanguage use(%)", 
       colour = NULL, linetype = NULL) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "None",
        axis.text = element_text(color="black"))





# ------------------------------
# RQ2 panel A and B 
rq2_panelA_cert_reproduction <- read_csv("data/rq2_panelA_cert_reproduction.csv")
#rq2_panelB_cert_informal_lift <- read_csv("data/rq2_panelB_cert_informal_lift.csv")


rq2_panelA_cert_reproduction%>%view()
#rq2_panelB_cert_informal_lift%>%view()


# Barchart for panel A
rq2_panelA_cert_reproduction%>%
  ggplot(aes(y=reorder(certification,reproduction_rate),x=reproduction_rate))+geom_col()+
  theme_minimal(base_size = 18)+labs(x="Reproduction rate in reviews",y='Certification')+
  scale_x_continuous(labels = label_percent())+
  theme(axis.text = element_text(color="black"))->panel_a

# --------------- 
o <- read_csv("data/rq2_overall_cooccurrence.csv")
o %>% mutate(low_support = ifelse(n_term>4,FALSE,TRUE)) -> o
#keep_c <- o |> filter(!low_support) |> distinct(certification) |> pull()
#keep_t <- o |> filter(!low_support) |> distinct(term) |> pull()
#op <- o |> filter(certification %in% keep_c, term %in% keep_t) |>
op <- o%>%mutate(fill = ifelse(low_support, NA, rate*100))

ggplot(op, aes(term, 
               reorder(certification, fill, 
                             max, na.rm=TRUE), fill = fill)) +
  geom_tile(color = "grey92") +
  scale_fill_viridis_c(trans = "sqrt", 
                       na.value = "white", 
                       name = "% of reviews",
                       breaks = c(0.1,1,2.5,5.5,6.5),
                       labels = c("0.1%","1%","2.5%","5.5%",""),
                       guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  #scale_x_discrete(guide = guide_axis(n.dodge = 3))+
  theme_minimal(base_size = 18) +
  theme(legend.position="bottom",
        legend.text = element_text(size = 10),
  axis.text.x = element_text(angle = 45, hjust = 1),
  axis.text = element_text(color="black")) +
  labs(x = "Informal sustainability term in reviews", 
       y = "Certification in product descriptions")->panel_b

require(patchwork)

panel_a + panel_b



# ---------
# Figure 2 the funnel as a slope chart 

require(ggrepel)
require(scales)
have_repel <- requireNamespace("ggrepel", quietly = TRUE)
w  <- read_csv("data/funnel_term_stage.csv", show_col_types = FALSE)

stage_lab <- c(search="Search\n(queries)", supply="Supply\n(product descriptions)", experience="Experience\n(reviews)")
long <- w %>%
  transmute(term, term_type, typology,
            Search = composition_search*100, Supply = composition_supply*100, Experience = composition_experience*100) %>%
  pivot_longer(c(Search, Supply, Experience), names_to = "stage", values_to = "composition") %>%
  mutate(stage = factor(stage, levels = c("Search","Supply","Experience")))

# terms legible enough to show as labelled lines: >=3% composition in any stage
keep <- w %>% filter(pmax(composition_search, composition_supply, composition_experience) >= 0.03) %>% pull(term)
sl <- long %>% filter(term %in% keep)

typ_cols <- c("search-led"="#D55E00","supply-led"="#0072B2","experience-led"="#009E73","shared"="#999999")


lab_left  <- sl %>% filter(stage == "Search")
lab_right <- sl %>% filter(stage == "Experience")


ggplot(sl, aes(stage, composition/100, group = term, colour = typology)) +
  geom_line(linewidth = 0.7, alpha = 0.9) +
  geom_point(size = 1.8) +
  ggrepel::geom_text_repel(data = lab_left,  aes(label = term), 
                           hjust = 1, nudge_x = -0.15, size = 4, direction = "y", segment.alpha = .3,show.legend = FALSE) +
  ggrepel::geom_text_repel(data = lab_right, aes(label = term), 
                           hjust = 0, nudge_x =  0.15, size = 4, direction = "y", segment.alpha = .3,show.legend = FALSE)+
  scale_colour_manual(values = typ_cols, name = "Typology") +
  scale_y_continuous(labels=label_percent())+
  labs(x = NULL, y = "Share of stage's\nsustainability vocabulary (%)") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        axis.text = element_text(color="black"))


