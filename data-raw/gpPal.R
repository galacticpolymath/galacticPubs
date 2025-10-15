## code to prepare `gpPal` dataset goes here

#GP Palette
gpPal = list(
  main =
    data.frame(
      name = c(
        "hydrogen blue",
        "lightning purple",
        "flare fucsia",
        "atomic blue",
        "burst purple",
        "galactic black",
        "sparkle white",
        "plus"
      ),
      hex = c(
        "#2c83c3",
        "#6c2d82",
        "#ff3dac",
        "#6812d1",
        "#cb1f8e",
        "#363636",
        "#f0f4ff",
        "#1826bc"
      ),
      rgb = c(
        "rgb(44,131,195,maxColorValue=255)",
        "rgb(108,45,130,maxColorValue=255)",
        "rgb(255,61,172,maxColorValue=255)",
        "rgb(104,18,209,maxColorValue=255)",
        "rgb(203,31,142,maxColorValue=255)",
        "rgb(54,54,54,maxColorValue=255)",
        "rgb(240,244,255,maxColorValue=255)",
        "rgb(24,38,188,maxColorValue=255)"
      )
    ),
  extended = data.frame(
    name = c(
      "dark hydrogen blue",
      "hydrogen blue",
      "light hydrogen blue",
      "dark lightning purple",
      "lightning purple",
      "light lightning purple",
      "flare fucsia",
      "light flare fucsia",
      "pale flare fucsia",
      "dark atomic blue",
      "atomic blue",
      "light atomic blue",
      "dark burst purple",
      "burst purple",
      "light burst purple",
      "galactic black",
      "sparkle white"
    ),
    hex = c(
      "#005792",
      "#2c83c3",
      "#69b2f6",
      "#3E0D55",
      "#6c2d82",
      "#9c5ab2",
      "#ff3dac",
      "#ff5dbe",
      "#ff78de",
      "#291c9e",
      "#6812d1",
      "#9f4bff",
      "#96195F",
      "#cb1f8e",
      "#c7247d",
      "#363636",
      "#f0f4ff"
    ),
    rgb = c(
      "rgb(0,87,46,maxColorValue=255)",
      "rgb(44,131,195,maxColorValue=255)",
      "rgb(105,178,246,maxColorValue=255)",
      "rgb(62,513,85,maxColorValue=255)",
      "rgb(108,45,130,maxColorValue=255)",
      "rgb(156,90,178,maxColorValue=255)",
      "rgb(255,61,172,maxColorValue=255)",
      "rgb(255,93,190,maxColorValue=255)",
      "rgb(255,120,222,maxColorValue=255)",
      "rgb(41,28,158,maxColorValue=255)",
      "rgb(104,18,209,maxColorValue=255)",
      "rgb(159,75,255,maxColorValue=255)",
      "rgb(150,25,95,maxColorValue=255)",
      "rgb(203,31,142,maxColorValue=255)",
      "rgb(199,36,125,maxColorValue=255)",
      "rgb(54,54,54,maxColorValue=255)",
      "rgb(240,244,255,maxColorValue=255)"
    )
  ),
  subjects = data.frame(
    name = c("math", "ela", "socstudies", "science", "extra","sust","sel"),
    hex = c("#db4125", "#eca14d", "#633a9a", "#b798e8", "#f4f0d9","#349964","#0070da"),
    rgb = c(
      "rgb(219,65,37,maxColorValue=255))",
      "rgb(236,161,77,maxColorValue=255))",
      "rgb(99,58,154,maxColorValue=255))",
      "rgb(183,152,232,maxColorValue=255)",
      "rgb(244,240,217,maxColorValue=255))",
      "",
      ""
    )
  ),
  discrete = data.frame(
    name= c("aqua","dkpurple","pink","orange","periwinkle"),
    hex= c("#3DA8A1", "#490E59", "#DB4F7F", "#F5AA3B", "#45488C"),
    rgb=NA
  )
)

# to update, use run de below
 usethis::use_data(gpPal, overwrite = TRUE)
