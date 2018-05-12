# Λambd'em Poker

## Description
Client for playing Texas Hold`em versus AI or human beings with some additional features, possibly. \
Full initial project description can be read [here](/docs/description.md).

![layout](docs/images/layout.png)

List of implemented features:
- table
- player seats
- dealing cards
- dealing board
- posting blinds
- interactable slider
- change of positions
- bet % of pot buttons
- bet display in chips
- time delayed actions
- fade on button click
- bottom & up seatbolds
- automove on timebank end
- hightlighting of active player
- calculation of possible actions
- handling bet rounds until showdown
- detecting & comparing of poker combinations
- *another not really cool stuff*

~~Link to huge haddocs here, although no one will really read them...~~

## Status

One phrase review: \
*Almost done ... base version*

Nothing really complete here yet,
but you can watch the flop, do some moves and play to
your heart content with a slider.

Developing ...

![serious coding](/docs/images/serious%20coding.gif)

## Build and run

First of all - install [Stack](https://www.haskellstack.org).

Setup it on first launch:

`stack setup`

For launching client:

`stack build && stack exec poker`

## Testing

Added some hand templates to test detecting and comparing of combinations.
Although isn't pretending to be a good test coverage and still requires a human eye
to verify results.

For launching tests:

`stack test`
