require(hexSticker)
require(showtext)
require(rsvg)

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Indie Flower", "indi")
## Automatically use showtext to render text for future devices
showtext_auto()

p <- system.file("sticker/users-cog-solid.svg", package = "shinymanager")

# sticker(p, package="shinymanager", spotlight = F, 
#         s_x=1, s_y=.75,
#         p_family = "indi", p_size = 40,
#         s_width = 0.6, s_height = 0, 
#         h_color = "#136ba0", h_size = 2,
#         h_fill = "#459dd3",
#         white_around_sticker = FALSE, 
#         l_x = 1, l_y = 0, l_width = 3, l_height = 3, l_alpha = 0.6, 
#         dpi = 600, asp = 0.8, 
#         url = "Developed by Datastorm & DreamRs", u_size = 6, 
#         u_family = "indi", u_color = "white",
#         u_x = 0.2, u_y = 0.47,
#         u_angle =-30, filename = "inst/sticker/shinymanager.png")

sticker(p, package="shinymanager", spotlight = F, 
        s_x=1, s_y=.75,
        p_family = "indi", p_size = 40,
        s_width = 0.6, s_height = 0, 
        h_color = "#136ba0", h_size = 1,
        h_fill = "#459dd3",
        white_around_sticker = FALSE, 
        l_x = 1, l_y = 0, l_width = 3, l_height = 3, l_alpha = 0.6, 
        dpi = 600, asp = 0.8, 
        url = "Developed by Datastorm & DreamRs", u_size = 8, 
        u_family = "indi", u_color = "white",
        u_x = 0.2, u_y = 0.52,
        u_angle =-30, filename = "man/figures/shinymanager.png")
