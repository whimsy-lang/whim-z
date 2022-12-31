# Whimsy

Simple embeddable programming language.

[Zig](https://ziglang.org/) implementation, last compiled with 0.11.0-dev.987+a1d82352d.

Whimsy is based on [Crafting Interpreters](http://craftinginterpreters.com/) by Robert Nystrom.

---

```
# Whimsy, a simple embeddable language.

# constant and variable declarations
myConstant :: "hello"
myVariable := "initialized"

# variables must be declared before use, assignment or other usage without declaration is an error
myVar := 0
myVar = 7

# mutability is per level, so a constant can have mutable fields

# scoped blocks
do
  # statements
/do

# number literals
12        # decimal
0b10110   # binary
0o71205   # octal
0x8fa12   # hexadecimal

# _ is allowed in number literals for ease of reading
1_234_567_890 == 1234567890

# function
myFunc :: fn(x, y, z)
/fn

# all functions are static and require an explicit self

# these are equivalent, : is used to pass the left hand side as the first argument
first:func(second)
func(first, second)

# property access by string or identifier, if propName :: 'property' then these are equivalent:
i.property
i.'property'
i['property']
i[propName]

# property name resolution order: object, type, superclass (recursively)

# blocks end with /block, eg /fn, /if, /for

# class
MyClass :: class is Parent      # optional inheritance, Parent is assigned to MyClass.super
  # items here are scoped to the class

  # initializer
  init :: fn(self, x, y)
    MyClass.super.init(self, x, y)      # super is the superclass, not an instance
                                        # have to explicitly call init with the current instance
    self.x := x
    self.y := y
  /fn

  # method
  myMethod :: fn(self)          # self is a convention, not a requirement
    return self.x + self.y      # return always returns a value
  /fn

  # static variable
  var := 3

  # functions can be mutable as well
  myFunc := fn(z)
    return z * z
  /fn

  # same with classes, which can also be nested
  NestedClass := class
  /class

  # can also use strings as keys
  'something' :: 42

  # operator overloading is done with strings
  "+" :: fn(a, b)
    return MyClass(a.x + b.x, a.y + b.y)
  /fn
/class

# custom operators can use the following: ~!@$%^&*-+={}\|;:<>.?/
a ->> b
# you can also call any two-parameter function as an infix operator
# by prefixing it with `
a `calc b

# initialization is just the type used as a function,
# this automatically creates a new instance and passes it in as the first argument
myInst :: MyClass(1, 2)

# lists are resizable indexable collections of any type denoted with ()
# () is an empty list, (1,) is a list with one item (the comma is required to distinguish it from grouping)
# (1, 2, 3,) items are separated by commas, the final comma is optional
# indexes start at 0
# a negative index is equivalent to (length + index)
list := (1, 2.3, 'hi', myObj, MyClass, myFunc)
list[1]        # this is 2.3
list[2] = 42   # this has now changed from a string to an int

# sets are unordered and unindexed collections of any type denoted with []
# sets cannot contain duplicates
# std.set() is the empty set
set := [1, 3, "hi"]
set:add("yay")
set[key]  # returns whether the set contains the key
set:remove('hi')

# maps are unordered indexable collections of key/value pairs denoted with []
# [] is an empty map
# items are represented as key (:: or :=) value, and separated by commas, the last of which is optional
map := [
  1 :: 'ONE',

  'constantVal' :: 'hi',

  'var' := 3,

  'someFn' :: fn()
    return 'hello'
  /fn,
]
map[1]        # 'ONE'
map.var       # 3
map.someFn()  # 'hello'

# Ranges
[from]..[to(exclusive)] [by step]
[from]..=[to(inclusive)] [by step]
# step defaults to 1
1..3        # 1, 2
1..4 by 2   # 1, 3
4..1 by -1  # 4, 3, 2

# looping
loop
  # infinite loop
  break x < 3       # break and continue take an expression
  continue y > z    # and break or continue if it is truthy
/loop

for i in item   # creates an iterator from item, and i contains the next value
/for

# a semicolon is an explicit empty statement for grammar ambiguities
# this is myList[1]
myList
[1]
# this is two statements, myList and a set literal [1]
myList;
[1]

# without a semicolon this is a - b
a
-b

# Type       Type Name
# ---------  ---------
# f64        number
# string     string        "", ''
# boolean    bool          true, false
# nil        nil           nil
# function   function      fn
# class      class         class
# ---------  ---------
# list       list          ()
# set        set           [val1, val2]
# map        map           [key :: const, key := var, 'string key' :: val]
# range      range         from..to by step, from..=to by step

# Operators: + - * / % ! == != and or :: := = += -= *= /= %= is

# no implicit conversions
val := 3 + someStr:to_number()
message :: "Value is " + val:to_string()
```

extension: *.whim  

## TODO

* character (byte?) number instead of line number
* error handling
* import system
* debugging
* io
* custom to_string that works with std.print
* strings - flexible array members
* string hash set (instead of table with nil values) for interning
* separate sized jumps
* have exit jumps jump straight to the end instead of chaining
* don't emit (nil and return) if the last line is already a return
* closures only for functions that need it?
* generational gc

### Standard Library

* std
  * assert()
  * bool
    * to_string()
  * class
    * to_string()
  * error()
  * function
    * to_string()
  * gc_collect()
  * list
    * add()
    * all()
    * any()
    * filter()
    * first()
    * index_of()
    * join()
    * last()
    * last_index_of()
    * length()
    * map()
    * pop()
    * reduce()
    * remove()
    * reverse()
    * to_set()
    * to_string()
  * map
    * keys()
    * length()
    * remove()
    * to_string()
    * values()
  * math
    * max()
    * min()
    * pi
  * nil
    * to_string()
  * number
    * abs()
    * acos()
    * asin()
    * atan()
    * ceiling()
    * cos()
    * floor()
    * log()
    * max
    * min
    * pow()
    * sin()
    * sqrt()
    * tan()
    * to_char()
    * to_degrees()
    * to_radians()
    * to_string()
  * print()
  * range
    * to_string()
    * values()
  * set
    * add()
    * length()
    * remove()
    * to_string()
    * values()
  * string
    * char_to_number()
    * chars()
    * _ format()
    * index_of()
    * last_index_of()
    * length()
    * repeat()
    * _ reverse()
    * split()
    * _ to_bool()
    * _ to_lower()
    * _ to_number()
    * to_string()
    * _ to_upper()
    * _ trim()
    * _ trim_end()
    * _ trim_start()
  * time()
  * version
