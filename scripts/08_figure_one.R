library(diagram)
source("R/explanatory_diagrams.R")

pdf(sprintf("figs/%s_figure_one_joint_diagram.pdf",figure_date_code),
    width=16,height=32)

par(mfrow=c(2,1))
plot_conditional_diagram()
mtext("A",side=3,line=1,cex=3,adj=-0.01)
plot_CSO_prediction_diagram()
mtext("B",side=3,line=1,cex=3,adj=-0.01)
dev.off()