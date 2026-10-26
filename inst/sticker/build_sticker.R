# The sticker is drawn in 'shinymanager.svg' (hexagon, shield with a verified
# user, name in Segoe UI). To rebuild the PNG (520 x 600, transparent
# background), render the SVG at 2x with a headless browser and resize it:
#
#   msedge --headless=new --hide-scrollbars --force-device-scale-factor=2 \
#     --default-background-color=00000000 --window-size=520,600 \
#     --screenshot=sticker_2x.png file:///path/to/page.html
#
# where page.html only displays the SVG at 520 x 600:
#   <img src="shinymanager.svg" style="display:block;width:520px;height:600px">
#
# Or with the rsvg package (fonts: Segoe UI must be installed):

require(rsvg)

svg <- "inst/sticker/shinymanager.svg"
for (f in c("man/figures/shinymanager.png",
            "inst/sticker/shinymanager.png",
            "inst/demo_app/www/figures/shinymanager.png")) {
  rsvg_png(svg, file = f, width = 520, height = 600)
}
