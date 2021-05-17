# Format and check code:
OhdsiRTools::formatRFolder()
OhdsiRTools::checkUsagePackage("AssociationRuleMining")
OhdsiRTools::updateCopyrightYearFolder()

rmarkdown::render("vignettes/AdvancedUsers.Rmd",
                  output_file = "../doc/AdvancedUsers.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

rmarkdown::render("examples/AR_example_workflow_Eunomia.Rmd",
                  output_file = "../doc/AR_example_workflow_Eunomia.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

rmarkdown::render("examples/FP_example_workflow_Eunomia.Rmd",
                  output_file = "../doc/FP_example_workflow_Eunomia.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))

# rmarkdown::render("vignettes/ComparingOutput_FPM.Rmd",
#                   output_file = "../doc/ComparisonsOutput.pdf",
#                   rmarkdown::pdf_document(latex_engine = "pdflatex",
#                                           toc = TRUE,
#                                           number_sections = TRUE))

rmarkdown::render("vignettes/RunningAllFP.Rmd",
                  output_file = "../doc/RunningAllFP.pdf",
                  rmarkdown::pdf_document(latex_engine = "pdflatex",
                                          toc = TRUE,
                                          number_sections = TRUE))