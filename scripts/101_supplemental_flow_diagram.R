#model flow diagram for supplemental material
#how does the state transition model work
#use unrolled diagram for clarity

source("R/illustrate_flow_diagram.R")


pdf(sprintf("figs/%s_supplemental_figure_one_flow_diagram.pdf",figure_date_code),
    width=16,height=16)

make_unrolled_flow_diagram()
dev.off()