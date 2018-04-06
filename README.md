# [Chess.js](https://github.com/jhlywa/chess.js) bindings for Bucklescript in Reason

Disclaimer: Work in progress

# Documentation
My hope is that the interface file
[Chess.rei](https://github.com/russelldmatt/bs-chess.js/blob/master/src/Chess.rei)
is sufficiently clear to serve as documentation.  Let me know if
that's not the case.

# Examples/Tests (needs more)
For right now, you can look at
[Test.re](https://github.com/russelldmatt/bs-chess.js/blob/master/src/Test.re)
for some examples/tests.  I'll try to make this better in the future.

# Quickstart

## Build
```
yarn install
yarn build
```

## Run with Node:
```
node src/Chess.bs.js
```

## Run in Browser (Works now thanks to @chenglou!):
```
yarn run webpack
open index.html
```
