# build-reports.R
# Builds output Rmd files

library(lmisc)
require(knitr)
require(markdown)

p_root <- proj_root("trademod")
p_vignettes <- full_path(p_root, "trademod/vignettes")

# Create .md, .html, and .pdf files
knit("vignettes/model-overview.Rmd", output = "vignettes/model-overview.md")
markdownToHTML("vignettes/model-overview.md", "vignettes/model-overview.html")

knit("vignettes/model-demo.Rmd", output = "vignettes/model-demo.md")
markdownToHTML("vignettes/model-demo.md", "vignettes/model-demo.html", options=c("use_xhml"))

