# The Do Again List

There are things you want to do somewhat regularly, but you can't remember them at the right time? This could be

* tasty meals to prepare again
* play a certain game you haven't played in a long time
* doing some house work when you have free time
* anything else you don't want to forget, but for which a reminder at a specific time is not the right fit

How does it work? Just put things on the Do Again list and check them each time you've done it. The Do Again list looks at the time intervals between the completions of the same entry and sorts those entries which are due (or overdue) to the top. So when you ask yourself "What could I cook today?" you will see the tasty meal you've prepared frequently (but not recently) at the top.

Apply tags to your entries to keep off topic items from you meals list.

## Build Instructions

Run the following command from the root of this project:

```bash
elm-make src/Main.elm --output elm.js
```

Then open `index.html` in your browser!

## Credits

Concept and code changes for the Do Again List by [Karl Bartel](http://karl.berlin).

Based on [TodoMVC in Elm](https://github.com/evancz/elm-todomvc) by [Evan Czaplicki](https://github.com/evancz).
