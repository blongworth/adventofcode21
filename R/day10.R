#' Day 10: Syntax Scoring
#'
#' [Syntax Scoring](https://adventofcode.com/2021/day/10)
#'
#' @name day10
#' @rdname day10
#' @details
#'
#' **Part One**
#'
#' You ask the submarine to determine the best route out of the deep-sea
#' cave, but it only replies:
#'
#'     Syntax error in navigation subsystem on line: all of them
#'
#' *All of them?!* The damage is worse than you thought. You bring up a
#' copy of the navigation subsystem (your puzzle input).
#'
#' The navigation subsystem syntax is made of several lines containing
#' *chunks*. There are one or more chunks on each line, and chunks contain
#' zero or more other chunks. Adjacent chunks are not separated by any
#' delimiter; if one chunk stops, the next chunk (if any) can immediately
#' start. Every chunk must *open* and *close* with one of four legal pairs
#' of matching characters:
#'
#' -   If a chunk opens with `(`, it must close with `)`.
#' -   If a chunk opens with `[`, it must close with `]`.
#' -   If a chunk opens with `{`, it must close with `}`.
#' -   If a chunk opens with `<`, it must close with `>`.
#'
#' So, `()` is a legal chunk that contains no other chunks, as is `[]`.
#' More complex but valid chunks include `([])`, `{()()()}`, `<([{}])>`,
#' `[<>({}){}[([])<>]]`, and even `(((((((((())))))))))`.
#'
#' Some lines are *incomplete*, but others are *corrupted*. Find and
#' discard the corrupted lines first.
#'
#' A corrupted line is one where a chunk *closes with the wrong character*
#' - that is, where the characters it opens and closes with do not form one
#' of the four legal pairs listed above.
#'
#' Examples of corrupted chunks include `(]`, `{()()()>`, `(((()))}`, and
#' `<([]){()}[{}])`. Such a chunk can appear anywhere within a line, and
#' its presence causes the whole line to be considered corrupted.
#'
#' For example, consider the following navigation subsystem:
#'
#'     [({(<(())[]>[[{[]{<()<>>
#'     [(()[<>])]({[<{<<[]>>(
#'     {([(<{}[<>[]}>{[]{[(<()>
#'     (((({<>}<{<{<>}{[]{[]{}
#'     [[<[([]))<([[{}[[()]]]
#'     [{[{({}]{}}([{[{{{}}([]
#'     {<[[]]>}<{[{[{[]{()[[[]
#'     [<(<(<(<{}))><([]([]()
#'     <{([([[(<>()){}]>(<<{{
#'     <{([{{}}[<[[[<>{}]]]>[]]
#'
#' Some of the lines aren\'t corrupted, just incomplete; you can ignore
#' these lines for now. The remaining five lines are corrupted:
#'
#' -   `{([(<{}[<>[]}>{[]{[(<()>` - Expected `]`, but found `}` instead.
#' -   `[[<[([]))<([[{}[[()]]]` - Expected `]`, but found `)` instead.
#' -   `[{[{({}]{}}([{[{{{}}([]` - Expected `)`, but found `]` instead.
#' -   `[<(<(<(<{}))><([]([]()` - Expected `>`, but found `)` instead.
#' -   `<{([([[(<>()){}]>(<<{{` - Expected `]`, but found `>` instead.
#'
#' Stop at the first incorrect closing character on each corrupted line.
#'
#' Did you know that syntax checkers actually have contests to see who can
#' get the high score for syntax errors in a file? It\'s true! To calculate
#' the syntax error score for a line, take the *first illegal character* on
#' the line and look it up in the following table:
#'
#' -   `)`: `3` points.
#' -   `]`: `57` points.
#' -   `}`: `1197` points.
#' -   `>`: `25137` points.
#'
#' In the above example, an illegal `)` was found twice (`2*3 = 6` points),
#' an illegal `]` was found once (`57` points), an illegal `}` was found
#' once (`1197` points), and an illegal `>` was found once (`25137`
#' points). So, the total syntax error score for this file is
#' `6+57+1197+25137 = 26397` points!
#'
#' Find the first illegal character in each corrupted line of the
#' navigation subsystem. *What is the total syntax error score for those
#' errors?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f10a(x)` returns .... For Part Two,
#'   `f10b(x)` returns ....
#' @export
#' @examples
#' f10a(example_data_10())
#' f10b()
f10a <- function(x) {
  scores <- map(x, bracketeer)
  sum(map_dbl(scores, 1))
}


#' @rdname day10
#' @export
f10b <- function(x) {
  scores <- map(x, bracketeer)
  autoscores <- sort(map_dbl(scores, 2))
  autoscores <- autoscores[which(autoscores != 0)]
  autoscores[ceiling(length(autoscores)/2)]
}

# using stack approach
bracketeer <- function(line) {
  opener <- c("(", "[", "{", "<")
  closer <- c(")", "]", "}", ">")
  points <- c(3, 57, 1197, 25137)
  autopoints <- c(1, 2, 3, 4)
  score <-  0
  autoscore = 0
  stack <- character()
  for (char in unlist(strsplit(line, ''))) {
    if (char %in% openers) {
      stack <- c(stack, closer[which(opener == char)])
    } else if (char == stack[length(stack)]) {
      stack <- stack[1:(length(stack)-1)]
    } else {
      return(list(score = points[which(closer == char)], autoscore = 0))
    }
  }
  for (i in length(stack):1) {
    autoscore <- 5 * autoscore + autopoints[which(closer == stack[i])]
  }
  list(score = 0, autoscore = autoscore)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day10
#' @export
example_data_10 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
