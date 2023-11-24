flwrs <- as.character(round(runif(1, 10000, 1000000), digits = 0))
posts <- as.character(round(runif(1, 13, 60), digits = 0))
flwing <- as.character(round(runif(1, 0, 48), digits = 0))
template <- image_read("profiletemplate.png")
image_annotate(template, flwrs, size = 50, gravity = "north", location = "+115+200", font = "Helvetica") -> template_1
image_annotate(template_1, posts, size = 50, gravity = "north", location = "-130+200") -> template_2
image_annotate(template_2, flwing, size = 50, gravity = "north", location = "+360+200")