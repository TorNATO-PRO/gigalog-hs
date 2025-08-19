# Gigalog

This is my attempt at reimplementing gigalog in idiomatic Haskell. 
I believe the code has been cleaned up greatly from its original state and reads much more clearly now.

Gigalog supports **parallel** seminaive evaluation for now.

## Usage

Right now, we are just reading datalog files from stdin and we don't support many datalog
language features. Here is a sample program so you may get an idea of what is supported as I
am too lazy to specify the specs!

```datalog
// name this triangles.datalog
edge(a,b). edge(b,c). edge(c,a).
edge(b,d). edge(d,e). edge(e,b).
edge(c,d). edge(e,f).

/* enforce a total ordering on symbols */
lt(a,b). lt(a,c). lt(a,d). lt(a,e). lt(a,f).
lt(b,c). lt(b,d). lt(b,e). lt(b,f).
lt(c,d). lt(c,e). lt(c,f).
lt(d,e). lt(d,f).
lt(e,f).

path(X,Y) :- edge(X,Y).
path(X,Z) :- path(X,Y), edge(Y,Z).

triangle(X,Y,Z) :- edge(X,Y), edge(Y,Z), edge(Z,X), lt(X,Y), lt(Y,Z).

person(alice). person(bob). person(carol). person(dave). person(erin). person(frank).

parent(alice,bob).
parent(alice,carol).
parent(bob,dave).
parent(carol,erin).
parent(erin,frank).

samegen(X,X) :- person(X).

samegen(X,Y) :- parent(P,X), parent(P,Y).

samegen(X,Y) :- parent(Px,X), parent(Py,Y), samegen(Px,Py).
```

This can be evaluated by doing `cabal run < triangles.dl` with an output like this:

```
cabal run < examples/ok/triangles.dl
edge/2 (8 rows)
┌───┬───┐
│ a │ b │
├───┼───┤
│ b │ c │
├───┼───┤
│ b │ d │
├───┼───┤
│ c │ a │
├───┼───┤
│ c │ d │
├───┼───┤
│ d │ e │
├───┼───┤
│ e │ b │
├───┼───┤
│ e │ f │
└───┴───┘


lt/2 (15 rows)
┌───┬───┐
│ a │ b │
├───┼───┤
│ a │ c │
├───┼───┤
│ a │ d │
├───┼───┤
│ a │ e │
├───┼───┤
│ a │ f │
├───┼───┤
│ b │ c │
├───┼───┤
│ b │ d │
├───┼───┤
│ b │ e │
├───┼───┤
│ b │ f │
├───┼───┤
│ c │ d │
├───┼───┤
│ c │ e │
├───┼───┤
│ c │ f │
├───┼───┤
│ d │ e │
├───┼───┤
│ d │ f │
├───┼───┤
│ e │ f │
└───┴───┘


parent/2 (5 rows)
┌───────┬───────┐
│ alice │ bob   │
├───────┼───────┤
│ alice │ carol │
├───────┼───────┤
│ bob   │ dave  │
├───────┼───────┤
│ carol │ erin  │
├───────┼───────┤
│ erin  │ frank │
└───────┴───────┘


path/2 (30 rows)
┌───┬───┐
│ a │ a │
├───┼───┤
│ a │ b │
├───┼───┤
│ a │ c │
├───┼───┤
│ a │ d │
├───┼───┤
│ a │ e │
├───┼───┤
│ a │ f │
├───┼───┤
│ b │ a │
├───┼───┤
│ b │ b │
├───┼───┤
│ b │ c │
├───┼───┤
│ b │ d │
├───┼───┤
│ b │ e │
├───┼───┤
│ b │ f │
├───┼───┤
│ c │ a │
├───┼───┤
│ c │ b │
├───┼───┤
│ c │ c │
├───┼───┤
│ c │ d │
├───┼───┤
│ c │ e │
├───┼───┤
│ c │ f │
├───┼───┤
│ d │ a │
├───┼───┤
│ d │ b │
├───┼───┤
│ d │ c │
├───┼───┤
│ d │ d │
├───┼───┤
│ d │ e │
├───┼───┤
│ d │ f │
├───┼───┤
│ e │ a │
├───┼───┤
│ e │ b │
├───┼───┤
│ e │ c │
├───┼───┤
│ e │ d │
├───┼───┤
│ e │ e │
├───┼───┤
│ e │ f │
└───┴───┘


person/1 (6 rows)
┌───────┐
│ alice │
├───────┤
│ bob   │
├───────┤
│ carol │
├───────┤
│ dave  │
├───────┤
│ erin  │
├───────┤
│ frank │
└───────┘


samegen/2 (10 rows)
┌───────┬───────┐
│ alice │ alice │
├───────┼───────┤
│ bob   │ bob   │
├───────┼───────┤
│ bob   │ carol │
├───────┼───────┤
│ carol │ bob   │
├───────┼───────┤
│ carol │ carol │
├───────┼───────┤
│ dave  │ dave  │
├───────┼───────┤
│ dave  │ erin  │
├───────┼───────┤
│ erin  │ dave  │
├───────┼───────┤
│ erin  │ erin  │
├───────┼───────┤
│ frank │ frank │
└───────┴───────┘


triangle/3 (2 rows)
┌───┬───┬───┐
│ a │ b │ c │
├───┼───┼───┤
│ b │ d │ e │
└───┴───┴───┘
```
