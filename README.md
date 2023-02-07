
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

While most of the new arguments to `md_document()` already described and
implemented in `rmarkdown::md_document()`, we will focus on the one
completely new feature of {hugodownplus}, its expandable HTML boxes.

The are two ways of using expandable HTML boxes with {hugodownplus}. One
is wrapping a child Rmd document into a fenced (pandoc) div with `:::`
and link a child document whose content will be rendered into a info or
warn box. The other way is to call `child_session_info()` as inline R
code to generate an expandable box containing the session info.

Let’s elaborate on both options a litte.

#### info and warn boxes

We can generate info and warn boxes based on external Rmd child
documents. The idea is that, in a blog post on topic X, we might want to
talk a bit more about details of a related concept Y. This might not be
interesting for every reader, so we can put this part in an expandable
box, and those interested, can dive in furhter.

To generate an info or warn box we need an actual Rmd file containing
text and or code. Usually we would include this child document in our
main (blog post) document as follows:

    ```{r, child="path_to_child_document.Rmd"}
    ```

To render this part into a special info or warn box we can wrap it using
a fenced (pandoc) div using three colons `:::`:

    ::: {.info-box title="Title of my info box"}
    :::

All we have to do is to specify either `{.info-box}` or `{.warn-box}`
and a `title` which should be shown in the header of the box.

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

    :::

This alone will just render an expandable box using the `<details>` and
`<summary>` HTML tags. All the formatting that we may want to apply has
to happen in CSS. The following classes can be used for the info-box:

``` css
.note {
  /* customize the whole box */
}

summary.note-header,
.note-header {
  /* customize the box header */
}

.note-details {
  /* customize the content of the box */
}
```

<style type="text/css">
.note {
  /* customize the whole box */
}

summary.note-header,
.note-header {
  /* customize the box header */
}

.note-details {
  /* customize the content of the box */
}
</style>

Note that the class inside the fenced (pandoc) div `:::` is called
`.info-box`, but the CSS classes are called `.note`. The `info` is a too
common class which is why the CSS selectors take `.note` as class.

Similarly, the CSS selectors for the warn box look like this:

``` css
.warn {
  /* customize the whole box */
}

summary.warn-header,
.warn-header {
  /* customize the box header */
}

.warn-details {
  /* customize the content of the box */
}
```

<style type="text/css">
.warn {
  /* customize the whole box */
}

summary.warn-header,
.warn-header {
  /* customize the box header */
}

.warn-details {
  /* customize the content of the box */
}
</style>

#### child info session

As mentioned above, we can create an expandable box containing the
current info session by using the `child_info_session()` function as
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
info and warn boxes. After implementing all of this, I had a lot of
custom functions in my website project and the idea was to package it
up, so that it is easier to maintain, and others might benefit from it.

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

This package does’t come with any official tests 😱. Nevertheless, I’m
regularly using it to create blog posts on my website, and so far I
haven’t encountered any issues - which is of course no guarantee that
you won’t. If you run into one, I’d be happy if you file an issue
[here](https://github.com/TimTeaFan/hugodownplus/issues).
