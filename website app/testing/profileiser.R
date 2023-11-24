username <- "TestUser100000"
flwrs <- as.character(round(runif(1, 10000, 1000000), digits = 0))
posts <- as.character(round(runif(1, 13, 60), digits = 0))
flwing <- as.character(round(runif(1, 0, 48), digits = 0))
percent <- as.character(round(runif(1, 0, 100), digits = 0))

template <- image_read("profiletemplate.png")
circle <- image_read("https://i.imgur.com/zN2qVed.png")
icon <- image_read("doggy.jpeg")

circleicon <- c(circle, image_resize(icon, geometry_size_pixels(width = 438, height = 438, preserve_aspect = FALSE)))

image_resize(image_flatten(circleicon, 'in'), "280x280") -> igicon

image_annotate(template, flwrs, font = 'sans', size = 50, gravity = "north", location = "+115+250") -> template_1
image_annotate(template_1, posts, size = 50, gravity = "north", location = "-130+250") -> template_1
image_annotate(template_1,flwing, size = 50, gravity = "north", location = "+360+250") -> template_1
image_annotate(template_1, paste0("You're more famous than \n", percent,"% of K-pop!"), 
               size = 40, gravity = "north", location = "+140+150", color = "tomato") -> template_1
image_annotate(template_1, paste0("@", username), size = 50, gravity = "north", location ="+140+50") -> template_1

image_composite(template_1, igicon, offset = "-10+70")



