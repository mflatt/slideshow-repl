#lang scribble/manual
@(require (for-label slideshow/repl
                     slideshow/base
                     racket/base
                     racket/contract/base
                     racket/class
                     racket/draw))

@(define pict @tech[#:doc '(lib "pict/scribblings/pict.scrbl")]{pict})

@title{Interactive Evaluation in Slideshow}

@defmodule[slideshow/repl]{The @racketmodname[slideshow/repl] module
provides support for picts (using @racket[interactive]) that allow
interactive evaluation within a slide presentation.}

@defproc[(repl-area [#:width width real? (* client-w 2/3)]
                    [#:height height real? (* client-h 1/4)]
                    [#:font-size font-size (or/c #f (integer-in 1 1024)) #f]
                    [#:background background (or/c #f (is-a?/c color%) string?) #f]
                    [#:prompt prompt-str string? "> "])
         pict?]{

Creates a @|pict| that displays as an interactive evaluation (i.e., a
@racket[read]-@racket[eval]-@racket[print] loop). Each such pict
has its own evaluation context that is reset when the slide enclosing
the pict is displayed.

The @racket[width] and @racket[height] arguments determine the size of
the resulting pict.

If @racket[font-size] is not @racket[#f], then it determines a font
size used, but with the constraint that all
@racket[slideshow/repl]-based picts that appear on the slide will use
the same font size (so all such picts should specify a consistent
size, or else an unspecified pict's size is used).

When @racket[background] is not @racket[#f], it determines a
background for the area.

The @racket[prompt-str] determines a prompt that is show for input
expressions in the interactive-evaluation area.}


@defproc[(make-repl-group [#:log-file log-file path-string? "eval-log.rktl"]
                          [#:prompt prompt-str (or/c #f string?) #f])
         repl-group?]{

Returns an evaluation context to be shared by multiple module areas
(created with @racket[module-area], each with a backing from
@racket[make-module-backing]) and result areas (created with
@racket[result-area]).

When a module area's content is evaluated, the content of the module
is recorded to @racket[log-file].

The @racket[prompt-str] argument determines the prompt that is shown
in an result area. If it is not @racket[#f], then he result area
supports interactive evaluation in the same way as @racket[repl-area].}


@defproc[(repl-group? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a context created by
@racket[make-repl-group], @racket[#f] otherwise.}



@defproc[(result-area [group repl-group?]
                      [#:width width real? (* client-w 2/3)]
                      [#:height height real? (* client-h 1/4)]
                      [#:background background (or/c (is-a?/c color%) string?) "white"]
                      [#:font-size font-size (or/c #f (integer-in 1 1024)) #f])
         pict?]{

Like @racket[repl-area], but for the result area (analogous to
DrRacket's interactions window) for a context created by
@racket[make-repl-group].

Multiple result areas created for a group display the same interaction
content.}


@defproc[(make-module-backing [group repl-group?]
                              [#:module-name module-name path-string? "program.rkt"]
                              [content-line string?] ...)
         module-backing?]{

Creates a module representing @racket[module-name] in the context
represented by @racket[group]. Each such module backing should have a
distinct @racket[module-name]. The module content is initialized by
the @racket[content-line] strings.

When the module is evaluated, a @racket[require] of each can use one
of the other modules in the group by using the other's module's
@racket[module-name].

Multiple modules areas created with @racket[module-area] can share a
backing, so that they provide the same view on the underlying
content. For example, a narrow view on one slide might be replaced by
a wider view on another side with the same module backing, so that
edits via the first are preserved in the second area's display.}


@defproc[(module-backing? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a context created by
@racket[make-module-backing], @racket[#f] otherwise.}


@defproc[(module-area [backing module-backing?]
                      [#:width width real? (* client-w 1/4)]
                      [#:height height real? (* client-h 1/4)]
                      [#:background background (or/c (is-a?/c color%) string?) "white"]
                      [#:font-size font-size (or/c #f (integer-in 1 1024))]
                      [#:auto-eval? auto-eval? any/c #f])
          pict?]{

Similar to @racket[repl-area], but for the content (analogous to
DrRacket's definitions window) of a particular module created by
@racket[make-module-backing] within a context created by
@racket[make-repl-group].

When the keyboard focus in the area, typing F5 @racket[require]s the module.
Typing F6 @racket[require]s the module's @racketidfont{test} submodule.}

