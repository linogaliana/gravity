## code to prepare `templateLatex` dataset goes here

trimed_objects <- aws.s3::s3readRDS("light_models/marseille.rds.rds", bucket = mybucket)
object <- aws.s3::s3readRDS("output_rds/zero_inflated_poisson_d9_logit_marseille.Rds",
                            bucket = mybucket)


# FIX TO BE ABLE TO USE STARGAZER
object$call[1] <- str2lang("zeroinfl()")

templateLatex <- stargazer::stargazer(
  object,
  type = "latex",
  keep.stat = c("n","ll"),
  dep.var.labels = "depvarname",
  header = FALSE,
  omit = c("Constant"),
  notes.append = TRUE, notes.align = "r",
  add.lines = "Bayesian information criterion",
  notes = "This will be replaced")

# writeLines(template_stargazer, con = "tempfile.txt")

usethis::use_data(templateLatex)
