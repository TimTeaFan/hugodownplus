
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
remotes::install_github("TimTeaFan/hugodownplus")
```

## How to use HTML boxes

While most of the new arguments to `md_document()` are already described
and implemented in `rmarkdown::md_document()`, we will focus on the one
completely new feature of {hugodownplus}: its expandable HTML boxes.

The are two ways of using expandable HTML boxes with {hugodownplus}. One
is wrapping a child Rmd document into a fenced (pandoc) div with `:::`
and link a child document whose content will be rendered into an info,
warn or output box. The other way is to call `child_session_info()` as
inline R code to generate an expandable box containing the session info.

Let’s elaborate on both options a little.

#### Info, warn and output boxes

`hugodownplus::md_document()` can generate info, warn and output boxes.
The idea is that, in a blog post on topic X, we might want to talk a bit
more about details of a related concept Y. This might not be interesting
for every reader, so we can put this part in an expandable box, and
those interested, can dive in further.Similarly, we can create warn
boxes, which draw the attention to one specific issue not every reader
might be interested in. Finally output boxes can be used to show the
output of a code chunk, only if the reader wants to see it.

To generate an info, warn or output box we just wrap text and/ or code
(or a child document) into a fenced (pandoc) div using three colons :::
before and after the part that we want to put into a box:

    ::: {.info-box title="Title of my info box"}

    Here goes some text.

    ```{r}
    # Here is a code comment and below some code
    x <- 1 + 1
    ```

    :::

All we have to do is to specify either `{.info-box}`, `{.warn-box}` or
`{.output-box}` and a `title` inside the div fence `:::`. The `title`
will be shown in the header of the box.

Below is a full example:

    ---
    output:
      hugodownplus::md_document:
        use_boxes: TRUE

    title: "Article title"
    # other arguments continuing here ...
    ---

    # Heading 1

    Some text

    ::: {.info-box title="Title of my info box"}

    Here goes some text.

    ```{r}
    # Here is a code comment and below some code
    x <- 1 + 1
    ```

    :::

This alone will just render a “naked” expandable box using the
`<details>` and `<summary>` HTML tags:

<div class="info-box" title="Title of my info box">

<div class="note" markdown="1">

<details class="open">
<summary class="note-header">
Title of my info box <i class="fas fa-tools"></i>
</summary>

<div class="note-details">

<p>
Here goes some text
</p>

``` r
# Here is a code comment and below some code
1 + 1
[1] 2
```

</div>

</details>

</div>

<br> All the formatting that we may want to apply has to happen in CSS.
The following classes can be used for the three main parts of each box
(info, warn and output):

``` css
.note, .warn, .output {
  /* customize the whole box */
}

summary.note-header,
summary.warn-header,
summary.output-header {
  /* customize the box header */
}

.note-details,
.warn-details,
.output-details {
  /* customize the content of the box */
}
```

Note that the class for info box inside the fenced (pandoc) div `:::` is
called `.info-box`, but the CSS classes are called `.note`. This is
because `info` is a very common class which is why the CSS selectors
take `.note` as class instead.

If you are running a Hugo website you can just add the CSS classes and
your prefered style to your custom.css file.

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

    `r child_session_info()`

Which will render a “naked” expandable box containing the session info:

<div class="session" markdown="1">

<details class="sess">
<summary class="session-header">
Session Info <i class="fas fa-tools"></i>
</summary>

    ─ Session info ───────────────────────────────────────────────────────────────
     setting  value
     version  R version 4.2.1 (2022-06-23)
     os       macOS Big Sur ... 10.16
     system   x86_64, darwin17.0
     ui       X11
     language (EN)
     collate  en_US.UTF-8
     ctype    en_US.UTF-8
     tz       Europe/Berlin
     date     2023-02-16
     pandoc   2.19.2 @ /Applications/RStudio.app/Contents/MacOS/quarto/bin/tools/ (via rmarkdown)

    ─ Packages ───────────────────────────────────────────────────────────────────
     package  * version    date (UTC) lib source
     hugodown * 0.0.0.9000 2023-02-13 [1] Github (r-lib/hugodown@f6f23dd)

     [1] /Library/Frameworks/R.framework/Versions/4.2/Resources/library

    ──────────────────────────────────────────────────────────────────────────────

</details>

</div>

<br>

Here too the styling has to happen via CSS. We can use the same classes
as for the other boxes by just interchanging the class name `.note` for
`.session`.

## History & Idea

After I spent quite some time creating and customizing my
[website](https://tim-tiefenbach.de), which I made with Hugo and
{hugodown}, quarto became a big thing and I saw a lot of stuff I liked
and wanted to bring to my own blog. Especially a table of content, the
expandable session info as well as warn and info boxes. While I could
figure out the former two, I got help from Shafayet on SO regarding the
info and warn boxes. After implementing all of this, I had a lot of
custom functions and files in my website project and the idea was to
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
