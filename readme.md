# Rethinking State With Elm

![Elm Logo](./images/Elm_logo.svg.png)

Hello! Welcome to the home of my talk/presentation on Elm, a beautiful web framework that was inspired by Haskell and that, in turn, inspired features of Redux and Rust. This repository is both an example application and also contains the presentation.

## Running The Presentation

I wrote the slides using [Marp](https://marp.app/). To view the presentation, either open `index.html` in your browser, or run `marp presentation.md` to regenerate the HTML slides.

## Neat Links

I think one of the most useful links I can share here is Elm's interactive online REPL [Ellie](https://ellie-app.com/new). Go and play around with Elm there, or see some examples of what people have made.

### Docs

* [Elm Language Guide](https://guide.elm-lang.org/)
* [SPA Example App (With Router)](https://github.com/rtfeldman/elm-spa-example/tree/master)

### Talks By/With Creator

* [Elm & The Future of Open Source](https://www.youtube.com/watch?v=0SUM4869ODc)
* [Functional Reactive Programming in Elm](https://www.youtube.com/watch?v=Ju4ICobPNfw)

### Philosophy

* [Compiler Errors for Humans](https://elm-lang.org/news/compiler-errors-for-humans)
* [Shape of Errors To Come (Rust)](https://blog.rust-lang.org/2016/08/10/Shape-of-errors-to-come.html)
* [Parse Don't Validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)

## Running The Example App

Make sure you have [Elm](https://elm-lang.org/) and [elm-live](https://github.com/wking-io/elm-live) installed and run `elm-live src/Main.elm --open -- --output=main.js` to launch the example app.

<img width="1693" alt="image" src="https://github.com/user-attachments/assets/8cffc359-5bcc-4b31-9274-a81bb4bf6c84" />

### Using the Time Traveling Debugger

If you'd like to demo the time traveling debugger, run the app using the `--debug` flag in addition to the normal arguments.

# Thank You

I wanted to give a shoutout to my coworker Taylor who figured out how to set up a shader as a website background. You can find his project where he did that [here](https://github.com/tfausak/daylatro) (it's the source for our company Balatro weekly seed leaderboard).
