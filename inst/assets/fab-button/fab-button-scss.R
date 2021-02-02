
# Compile CSS for FAB button ----------------------------------------------

# From https://github.com/nobitagit/material-floating-button

library(sass)

sass(
  input = list(
    "main-color" = "#FFF",
    "button-text-color" = "#2E2E2E",
    "label-text-color" = "#F1F1F1",
    "main-button-size" = "48px",
    "child_button_size" = "40px",
    "border-distance" = "15px",
    "button-space" = "60px",
    "number-of-child-buttons" = "4",
    sass_file(
      input = "inst/assets/fab-button/mfb.scss"
    )
  ),
  output = "inst/assets/fab-button/fab-button.min.css",
  options = sass_options(output_style = "compressed")
)
