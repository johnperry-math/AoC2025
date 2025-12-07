# Advent of Code 202t in (HAC) Ada -- and C#?!?

[<img src="ada_logo.svg" height="200">](ada_logo.svg)
[<img src="hac_logo.svg" height="200">](hac_logo.svg)
[<img src="Logo_C_sharp.svg" height="150">](Logo_C_sharp.svg)

Because _(insert number here; I've lost track)_ years of pain and suffering aren't enough. 😁

The 🧝🧝🧝 (elves) were so busy getting ready for Christmas that
they forgot to decorate the North Pole.

I don't even decorate _my_ house for Christmas!
(I'm not quite a Grinch;
I do at least try to to keep Christ in Christmas.)
So the _real_ me would tell the elves to stuff it.
Alas, I want those darned ⭐⭐, so I reckon I'll help after all.

**Double alas:** For a project at work I need to use C#,
and it's been almost a decade since I (briefly) studied it,
so I'll work on that, too.
I'll document a few observations and issues I encounter along the way.

**Mitigating the alases:** I thought I'd experiment with the [HAC Ada Compiler](https://github.com/zertovitch/hac),
which does not compile _full_ Ada, but a subset of it.
While I'm at it, I'll document differences I find between HAC and Ada 2022.

* Problems in order of appearance
  * 🚪 [Day 1](#-day-1-secret-entrance): Secret Entrance
  * 🎁 [Day 2](#-day-2-gift-shop): Gift Shop
  * 🔋 [Day 3](#-day-3-lobby): Lobby
  * 🧻 [Day 4](#-day-4-printing-department): Printing Department
  * 🦠 [Day 5](#-day-5-cafeteria): Cafeteria
  * 🦑 [Day 6](#-day-6-trash-compactor): Trash Compactor

## Problems in order of appearance

### 🚪 Day 1: Secret Entrance

To get the password, you must open a safe.
The safe has a dial.
The dial has instructions on how to rotate the dial.

It turns out that the safe is a red herring;
the actual password depends on how many times the dial points to 0.

In part 1, you count how many instructions leave the dial pointing to 0.

In part 2, you count how many times the dial crosses or lands on 0.

#### Unusual tools

Well, both HAC and C# are unusual for me...

#### Experience

Pretty straightforward. I did this one in HAC first.

With HAC:
* I was unable to define `Length` as a `constant Positive`,
  then use it in a range while defining an array.
  Zertovich (the maintainer of HAC) tells me that if I define `Length`
  as a `constant` _without specifying further_, it should work.
  I tried this on Day 4 and it did, but I'll leave the earlier days as is.
* There is no modular type, as in Ada 95 (?) onwards.
* There are no `of`-style iterations.
* Integers do not have a `'Max` attribute, as in Ada.
  Zertovich tells me that `HAT` has a `Max` function,
  but it turns out I didn't use `Max` in the end.
* No `'Image` attribute; I have to use `HAT.Image` instead.
* No `use type` nor `use all type`, so I cannot access the `&` operator for strings without a `use HAT`, which I am loath to do.

With C#:
* I had forgotten that C# was created because
  Sun forbade Microsoft from monkeying with Java.
  Microsoft subsequently looked at the verbosity of Java and said, "Hold my 🍺." (That's "beer".)

  Seriously, I don't ever want to hear anyone say that _Ada_ is too verbose ever again.
* Hey, at least C# has `enum`s.
* I don't understand why `record`s use `()` while `struct`s use `{}`.
  (I figure this out a while later: it has to do with construction.)
* Codium's C# plugin is proving pretty helpful at finding ways to make the code more concise.
* 🤬 zero-based everything makes its confusing comeback.
* Back to the weird operators `+=` and `-=`.
  I don't dislike them, though; they sort of make sense.
  Ada's `@` operator is pretty weird, too, but quite useful.
* Formatted strings are nice in any language.

### 🎁 Day 2: Gift Shop

A younger elf entered a bunch of invalid gift IDs, because he could.

In part 1, you add up all the invalid IDs
that consist of the doubling of a number.

In part 2, you add up all the invalid IDs
that consists of any repetition of a number.

#### Unusual tools

In Ada I had to introduce my own integer type,
because `Integer` is only 32 bits.

In C# I wrote my own enumerator, which was pretty time-consuming.
I say once more that I don't ever want to hear anyone say that _Ada_ is too verbose ever again.

#### Experience

Also straightforward. I also did this one in HAC first.

With HAC:
* I didn't have to define a custom integer type;
  apparently it defaults to the largest available size.
* I found a bug in the documentation: `Truncate` should be `Trunc`.

With C#:
* Just like Java does, they call vectors `List`s.
* Just like Java does, you `Add` to a vector rather than `Append`.
* You use `Parse` and `ToString` to convert from `string` to `int` and vice versa.
  (That may be just like Java, but I can't remember.)
* In Ada you can `for Ith in Start .. Finish`.
  In C# you instead do one of these:
  * `for (var ith = start; ith <= finish; ++ith)`
  * `foreach (var ith in Enumerable.Range(start, finish + 1))`

  You can replace `var` by the correct type if you care to.
  (I usually do.)

  I repeat, I don't ever want to hear anyone say that _Ada_ is too verbose ever again.
* There is a `System.Range` type but the geniuses at Microsoft
  didn't think to make it enumerable.
* There's a decent facility for slicing `string`s.
* The `=` / `==` thing annoys me on occasion.

### 🔋 Day 3: Lobby

An electrical surge has fried the elevators.
It has also fried the escalators.

Fortunately, there are batteries nearby to supply power to the escalators.
(Why there are no batteriess available to supply power to the elevators is left unexplained.)

In part 1, you determine which 2 batteries you should turn on
in order to maximize joltage.

Part 2 is the same, but with 12 batteries.

#### Unusual tools

* Another one where I had to define a custom integer type in Ada.

#### Experience

I switched to starting with Ada, since by now I'm most comfortable with that.

In my solution, one makes two checks for whether to deactivate one battery
and activate another:
* The battery to activate has a larger value than the one to deactivate.
* The battery to deactivate has a smaller value than the activated battery _immediately to its right_.

With HAC:
* HAC doesn't allow one to return `record` types from `function`s;
  one has to use a `procedure` with an `out` parameter instead.
* HAC doesn't have Ada 2022's aggregate initialization of records.
* HAC doesn't have Ada's `declare` block statements.
* HAC doesn't have Ada 2022's `when` expressions for `for` loops.
* For some reason, the offsets didn't work quite the same as they did with Ada.

With C#:
* `Enumerable.Range` turned out to be trash, in that
  I would ask it to `foreach (var thing in Enumerable.Range(a, b))`
  and it would routinely give me a `thing` whose value was larger than `b`.
  I wasted far too much time trying to debug this.
  It occurred on multiple occasions, in multiple places.
  So I gave up and changed most of them back to
  `for (var thing = a; thing < b; ++thing)`.
* 

### 🧻 Day 4: Printing Department

The Printing Department can make their own decorations,
but the way to the Cafeteria is blocked by thousands of rolls of toilet paper.

Cue the puns...

#### Unusual tools

I had to create a vector type in HAC.

#### Experience

Again, I started with Ada rather than HAC.

I was surprised by how easy it was.
It makes me worry about Days 5-12.

With C#:
* For some inexplicable reason, `var offset_range = Enumerable.Range(-1, 2)`
  does not produce a `1` when you iterate over it.
  But `Enumerable.Range(-1, 3)` does, without producing a `2`.
  This doesn't make a lot of sense,
  but as I discovered when converting Day 3 to C# (as I did after Day 4),
  there's not a lot about `Enumerable.Range` that makes sense.

### 🦠 Day 5: Cafeteria

The elves have such crack programmers writing the cafeteria's inventory software
that they can't figure out which ingredients are spoiled and which are fresh.

In part 1, you identify the fresh ingredients.

In part 2, you determine how many fresh ingredients are possible.

#### Unusual tools

* I dunno if large numbers were required, but
  in Ada I went ahead and defined a range of 64-bit numbers,
  and in C# I used `long`.
* I had to create a vector type in HAC again.

#### Experience

Again, very straightforward.

With C#:
* Unlike Ada's `for ... of`, `foreach` does not return a reference,
  but rather a copy of an element.
  (This may be because I used a `struct` rather than a `class`.
  -- **confirmed**!)
* Also unlike Ada, I cannot modify directly the fields of a structure in an array;
  I have to replace the array element entirely.
  (The same cause.)
  
### 🦑 Day 6: Trash Compactor

You do something stupid, fall down a trash chute, and end up in a trash compactor.
Shades of Star Wars, except that
these cephalopods remember you from the 2021 Advent of Code,
so they're not inclined to drag you under water
before performing unspeakably monstrous acts on you.
No, their plans are yet more insidious:

😨 help their children complete their math homework! 😱

In part 1, you add or multiply the numbers arranged in each row,
separated by empty columns.

In part 2, the adults explain that you misread the numbers:
they are arranged by columns, separated by empty columns.
(This part was pretty cool.)

#### Unusual tools
* Made use of Ada 2022's `'Reduce` attribute.
* In C#, made use of
  * `String.Split`
  * `IEnumerable.Aggregate`
  * `String.IndexOfAny`

#### Experience

I really enjoyed the Ada implementation of this one.

With Ada:
* Encountered a bug in `case` expressions and the `'Reduce` attribute.
  I need to investigate this further and report a bug.

With HAC:
* The inability to create an array with a length not known at compile time
  makes things much more difficult than Ada.
  Likewise the lack of generics.

With C#:
* I am beginning to dread having to work with C#.
* At least `Enumerable.Range` worked properly today.
* The static analzyer isn't always smart enough to figure out
  when a nullable variable is not null.
  I had an `Enumerable.Range(0, 4)` that initializes each element of 
  a `string?[4] number_lines` and it kept complaining that
  some element might be null.
* In Ada I managed to make parsing Part 2 work in a very natural,
  intuitive fashion,
  but I really struggled with parsing in C# today, especially in Part 2.
  