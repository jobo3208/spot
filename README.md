# spot - expense splitter

Under development.
Pre-alpha.
Not fully functional.

As is often the case with my Clojure projects, flexibility is paramount.
I put off decisions as long as I can (ideally forever).
All code that can work for both Clojure and ClojureScript should.

The core/backend works in Clojure and ClojureScript.
For now, the only frontend is a ClojureScript/reagent app.

## Useful commands

To connect nvim/fireplace:

    $ npx shadow-cljs watch frontend
    (then, in nvim)
    :Connect 3333
    (hit enter)
    :CljEval (shadow/repl :frontend)
    (hit enter)

To run tests:

    $ npx shadow-cljs compile test
