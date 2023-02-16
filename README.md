
<!-- README.md is generated from README.Rmd. Please edit that file -->

# hugodownplus

<!-- badges: start -->

![Release
status](https://img.shields.io/badge/status-first%20release-yellow)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
![CRAN status](https://img.shields.io/badge/CRAN-not%20published-red)

<!-- badges: end -->

## Overview

<p id="logop">

<a id="logo" href="https://raw.githubusercontent.com/TimTeaFan/hugodownplus/main/man/figures/logo_big.png"><img src="https://raw.githubusercontent.com/TimTeaFan/hugodownplus/main/man/figures/logo.png" alt="hugodownplus&apos; logo a notebook computer showing &apos;hugo down&apos; on the screen and next to it a plus symbol containing the word &apos;plus&apos;" align="right"/></a>

</p>

{hugodownplus} extends {hugodown}’s functionality in two ways:

1.  It offers a drop-in replacement for the minimalistic
    `hugodown::md_markdown()` output format. Using
    `hugodownplus::md_markdown()` instead allows the use of a number of
    additional arguments - most of them borrowed from
    `rmarkdown::md_document()`:
    - a table of content `toc` with a specified `toc_depth`
    - HTML boxes via lua filters (more on this below) `use_boxes`
    - additional content to be included within the document `includes`
    - additional command line options to pass to pandoc `pandoc_args`
2.  A new argument-less function `child_session_info()` which can be
    used as inline code in an Rmd file to create a child document
    containing an expandable box showing the session info.

## Installation

`{hugodownplus}` is not on CRAN yet. You can install the latest version
from [GitHub](https://github.com/TimTeaFan/hugodownplus) with:

``` r
remotes::install_github("TimTeaFan/hugodownplus", build_vignettes = TRUE)
```

## How to use HTML boxes

While most of the new arguments to `md_document()` are already described
and implemented in `rmarkdown::md_document()`, we will focus on the one
completely new feature of {hugodownplus}: its expandable HTML boxes.

The are two ways of using expandable HTML boxes with {hugodownplus}. One
is wrapping text and or code (or a child Rmd document) into a fenced
(pandoc) div with `:::`. The content will be rendered into an info, warn
or output box. The other way is to call `child_session_info()` as inline
R code to generate an expandable box containing the session info.

Let’s elaborate on both options a little.

#### Info, warn and output boxes

`hugodownplus::md_document()` can generate info, warn and output boxes.
The idea is that, in a blog post on topic X, we might want to talk a bit
more about details of a related concept Y. This might not be interesting
for every reader, so we can put this part in an expandable info box, and
those interested, can dive in further. Similarly, we can create warn
boxes, which draw the attention to one specific issue not every reader
might be interested in. Finally output boxes can be used to show the
output of a code chunk, only if the reader wants to see it.

To generate an info, warn or output box we just wrap text and/ or code
(or a child document) into a fenced (pandoc) div using three colons
`:::` before and after the part that we want to put into a box:

    ::: {.info-box title="Title of my info box"}
    Some text goes here.
    ```{r}
    # Here is a code comment and below some code
     x <- 1 + 1
    ```
    :::

All we have to do is to specify either `{.info-box}`, `{.warn-box}` or
`{.output-box}` and a `title` which should be shown in the header of the
box.

Further, we have to set the `use_boxes` argument in the Rmarkdown header
either to `"minimal"` or to `"styled"`. In the case of minimal, this
will just render an expandable box using the `<details>` and `<summary>`
HTML tags. All the formatting that we may want to apply has to happen in
CSS. When `use_boxes` is set to `"styled"` a predefined default CSS will
be applied. This comes with two benefits: (1) the exandable HTML box can
be visually inspected when knitting the document as HTML preview, so we
don’t need run a hugo server with a local preview. (2) the prstyled CSS
defines a number of variables which can be used to easily change the
appearance of the HTML boxes. This saves us from thinking about how to
target specific elements of the boxes in CSS, since we can just override
the existing variables. For me information on how to style the HTML
boxes see the vignette .

Below lets look at two examples, first when using
`use_boxes: "minimal"`:

    ---
    output:
      hugodownplus::md_document:
        use_boxes: "minimal"

    title: "Article title"
    # other arguments continuing here ...
    ---

    # Heading 1

    Some text

    ::: {.info-box title="Title of my info box"}
    Some text goes here.
    ```{r}
    # Here is a code comment and below some code
     x <- 1 + 1
    ```
    :::

… this yields …

#### child info session

As mentioned above, we can create an expandable box containing the
current session info by using the `child_info_session()` function as
inline R code in an Rmd file.

Below is an example:

    ---
    output: hugodownplus::md_document

    title: "Article title"
    # other arguments continuing here ...
    ---

    # Heading 1

    Some text

    `r child_info_session()`

## History & Idea

After I spent quite some time creating and customizing my
[website](https://tim-tiefenbach.de), which I made with Hugo and
{hugodown}, quarto became a big thing and I saw a lot of stuff I liked
and wanted to bring to my own blog. Especially a table of content, the
expandable session info as well as warn and info boxes. While I could
figure out the former two, I got help from Shafayet on SO regarding the
info, warn and output boxes. After implementing all of this, I had a lot
of custom functions and files in my website project and the idea was to
package it up, so that it is easier to maintain, and others might
benefit from it too.

## Acknowledgements

This package doesn’t contain much original content. The main function
`md_document()` is basically copied from {hugodown} and extended by code
found in `rmarkdown::md_document()` (both hugodown’s and rmarkdown’s
licenses and copyrights apply!). The only unique part about this package
is the use of HTML boxes and this part was written by Shafayet Khan
Shafee who answered two of my Rmarkdown questions on
[SO](https://stackoverflow.com/questions/75251741/wrap-rmarkdown-child-in-additional-html).

If this package is working correctly, all the credit should go to the
creators and maintainers of the original packages, {hugodown} and
{rmarkdown}, as well as to Shafayet.

## Disclaimer

1.  This package doesn’t come with any official tests 😱. Nevertheless,
    I’m using it to create blog posts on my website, and so far I
    haven’t encountered any issues - which is of course no guarantee
    that you won’t. If you run into one, I’d be happy if you file an
    issue [here](https://github.com/TimTeaFan/hugodownplus/issues).

2.  This package more or less contradicts the original philosophy of the
    {hugodown} package, which was designed to yield a streamlined,
    highly minimal HTML output. I do like {hugodown} very much, and
    think extending it is the better way (for me) compared to switching
    to a different package like {blogdown}.
