# The Common Lisp surface area a language server has to cover

Written because the honest answer to "what does clef need to support?" had never
been written down. Development so far followed what was in front of it, which is
a reasonable way to start and a bad way to find out what is missing.

**The reassuring headline: this set is finite and closed.** ANSI Common Lisp was
standardised in 1994 and has not changed. The language is exactly 978 exported
symbols, 25 special operators, 91 macros, 18 defining forms. Every construct a
conforming program can use is on a list, and this document is that list. Whatever
else is hard about this project, "we can never know if we've covered it all" is
not one of the difficulties.

## 0. Method

Enumerated from the running SBCL image rather than from memory or from prose
documentation, because the image is authoritative and prose is not:

```lisp
(do-external-symbols (s (find-package :common-lisp)) ...)
```

classifying each symbol by `special-operator-p`, `macro-function`, `fboundp`,
`find-class`, `sb-int:info :type :kind`, `constantp`, `boundp`. The script is
`tmp/enumerate-cl.lisp`. Re-run it to reproduce every count here.

```
ANSI Common Lisp: 978 external symbols in the CL package
  SPECIAL-OPERATOR     25        CLASS               85
  MACRO                91        TYPE                16
  FUNCTION            606        CONSTANT            62
  GENERIC-FUNCTION     30        SPECIAL-VARIABLE    54
```

## 1. The part that makes Lisp different: namespaces

The single most important structural fact, and the one most likely to be got
wrong by a server written with a Python or TypeScript mental model:

**A Common Lisp symbol is not one thing. It can be several at once, in separate
namespaces that do not shadow each other.** `list` in the standard is
simultaneously a function, a type, and a class. This is legal and idiomatic:

```lisp
(defun tree (x) x)          ; TREE names a function
(defvar tree nil)           ; TREE names a variable -- a DIFFERENT binding
(deftype tree () 'cons)     ; TREE names a type    -- a THIRD binding
(defclass tree () ())       ; TREE names a class   -- a FOURTH
```

All four coexist. `(tree tree)` calls the function on the variable.

The namespaces a server must keep apart:

| namespace | introduced by | referenced from |
|---|---|---|
| **function** | `defun`, `defmethod`, `flet`, `labels`, `defgeneric`, `(setf f)` | head of a form, `#'f`, `funcall`/`apply` |
| **variable** | `let`, `let*`, lambda lists, `defvar`, `defparameter`, `defconstant`, `do`, `dolist`, ... | argument position |
| **macro** | `defmacro`, `macrolet`, `define-symbol-macro`, `symbol-macrolet` | head of a form |
| **type / class** | `defclass`, `defstruct`, `deftype`, `define-condition` | `the`, `declare`, `typep`, specializers, `make-instance` |
| **block** | `block`, and implicitly `defun`, `defmethod`, `loop`, `dolist`, `dotimes`, `do` | `return-from`, `return` |
| **tagbody tag** | `tagbody`, and implicitly `do`, `dolist`, `dotimes`, `prog` | `go` |
| **package** | `defpackage`, `make-package` | `in-package`, `pkg:sym`, `pkg::sym` |
| **declaration** | `declaim`, `declare`, `proclaim` | — |
| **restart** | `restart-case`, `restart-bind` | `invoke-restart` |
| **method combination** | `define-method-combination` | `defgeneric :method-combination` |

**clef today models two of these**: function and variable, and it does not
reliably separate them — `symbol-kind` records a kind on the definition, but
resolution is by name. So `(defvar tree)` and `(defun tree)` in the same file
collide. Types and classes are indexed but not namespace-separated; blocks, tags,
restarts and method combinations are not modelled at all.

This is the single largest structural gap, and it is upstream of everything
below. A perfect list of forms will still resolve to the wrong binding if all
bindings live in one namespace.

## 2. Defining forms — all 18

Every standard defining form, by convention all named `def*`. This list is
complete; there is no nineteenth.

| form | introduces | clef |
|---|---|---|
| `defun` | function | **yes** |
| `defmacro` | macro | **yes** |
| `defparameter` | special variable | **yes** |
| `defvar` | special variable | **yes** |
| `defconstant` | constant | **yes** |
| `defclass` | class + type, and accessors/readers/writers from slots | **yes** |
| `defstruct` | class + type + constructor + copier + predicate + accessors | **yes** |
| `deftype` | type | **yes** |
| `define-condition` | condition class + type | **yes** |
| `defmethod` | method on a generic function | **partial** — indexed as `:method`, specializers ignored |
| `defgeneric` | generic function, and methods via `:method` options | **no** |
| `defpackage` | package, and its exported symbol names | **no** |
| `defsetf` | setf expander | **no** |
| `define-setf-expander` | setf expander | **no** |
| `define-symbol-macro` | global symbol macro | **no** |
| `define-compiler-macro` | compiler macro | **no** |
| `define-modify-macro` | macro | **no** |
| `define-method-combination` | method combination | **no** |

**Not on the list but needed:** `(defun (setf foo) (v x) ...)` — a function whose
*name is a list*. Legal, common, and it breaks any indexer assuming a name is an
atom. Same for `(defmethod (setf foo) ...)`.

The two highest-value gaps here are **`defgeneric`** and **`defpackage`**.
`defgeneric` because it is the declaration site users navigate to, and because
`:method` options define methods inline. `defpackage` because its export list is
the API surface of every file, and because clef already resolves qualified names
— it just learns packages from `in-package` alone, not from their definitions.

## 3. Binding forms — what establishes a scope

This is where the `let`/`let*` work landed, and the list is longer than the four
forms clef currently handles. Marked with the visibility rule each one needs,
using the vocabulary now in `lexical-scope`.

### Variable bindings

| form | rule | clef |
|---|---|---|
| `let` | `:none` | **yes** |
| `let*` | `:preceding` | **yes** |
| lambda lists (`defun`, `lambda`, ...) | `:preceding` — defaults see earlier params | **partial** — no `&optional`/`&key` default handling |
| `destructuring-bind` | `:preceding`, **nested** — the lambda list is a tree | **no** |
| `multiple-value-bind` | `:none` | **no** |
| `do` | `:none` (like `let`) | **no** |
| `do*` | `:preceding` | **no** |
| `dolist`, `dotimes` | single binding, not visible in the count/list form | **no** |
| `do-symbols`, `do-external-symbols`, `do-all-symbols` | as `dolist` | **no** |
| `loop` | its own sublanguage — `with`, `for`, `into`, `being` | **no** |
| `prog`, `prog*` | `let` / `let*` plus an implicit `tagbody` and `block nil` | **no** |
| `symbol-macrolet` | `:none`, and references *expand* rather than resolve | **no** |
| `with-slots`, `with-accessors` | binds names to slot access on an instance | **no** |
| `with-open-file`, `with-open-stream` | `:none`, single binding | **no** |
| `with-input-from-string`, `with-output-to-string` | `:none` | **no** |
| `with-hash-table-iterator`, `with-package-iterator` | binds a *macro*, not a variable | **no** |
| `handler-case` | each clause binds the condition | **no** |
| `restart-case` | clause lambda lists | **no** |
| `pprint-logical-block` | `:none` | **no** |
| `progv` | **runtime-computed names — genuinely undecidable statically** | n/a |

### Function bindings

| form | rule | clef |
|---|---|---|
| `flet` | `:none` | **yes** |
| `labels` | `:all` (mutual recursion) | **yes** |
| `macrolet` | `:none` | **yes** |

### Blocks and tags

| form | namespace | clef |
|---|---|---|
| `block` / `return-from` | block | **no** |
| implicit block from `defun`/`defmethod` (named after the function) | block | **no** |
| implicit `block nil` from `loop`, `do`, `dolist`, `dotimes` | block | **no** |
| `tagbody` / `go` | tag | **no** |

`return-from` is worth calling out: it takes a **block name, not a variable**, and
`(return-from foo)` inside `(defun foo ...)` refers to the implicit block. A
server that resolves that name in the variable or function namespace gets it
wrong, and `return-from` is extremely common.

## 4. Reference contexts — where a symbol appears

Indexing definitions is half the job; the other half is knowing what a symbol
*occurrence* means. The same token means different things by position:

| context | example | namespace |
|---|---|---|
| operator position | `(foo x)` | function or macro |
| argument position | `(bar foo)` | variable |
| function quote | `#'foo` | function |
| quoted datum | `'foo`, `(quote foo)` | **none — it is data, not a reference** |
| backquote template | `` `(a ,foo) `` | data, except inside unquotes |
| type position | `(the foo x)`, `(declare (type foo x))` | type |
| specializer | `(defmethod m ((x foo)) ...)` | class |
| slot name | `(slot-value x 'foo)` | slot, not variable |
| keyword | `:foo` | never a reference |
| package-qualified | `pkg:foo`, `pkg::foo` | resolves in `pkg`, not current |
| `setf` place | `(setf (foo x) v)` | setf expander / function |
| `loop` keyword | `for`, `collect`, `into` | **not symbols at all** — loop syntax |

**Quoting is the big one.** `'foo` is a literal symbol, not a use of `foo`, and
counting it as a reference makes find-references and rename wrong in a way that
corrupts code. clef does not currently distinguish quoted from evaluated
position.

`loop` deserves its own note: `(loop for x in xs collect x)` contains `for`,
`in` and `collect` as bare symbols that are **loop syntax, not references to
anything**. Treating them as references pollutes the index.

## 5. Reader-level constructs

Below the form level, and easy to forget because they are not forms:

| construct | example | effect |
|---|---|---|
| reader conditionals | `#+sbcl`, `#-(or a b)` | code that may not exist for this build |
| read-time eval | `#.(foo)` | arbitrary computation during read |
| package markers | `pkg:sym`, `pkg::sym` | grammar splits these; clef indexes both halves |
| uninterned symbols | `#:foo` | no package, common in `defpackage` |
| character literals | `#\a`, `#\Space` | `#\(` will unbalance a naive paren counter |
| block comments | `#\|` ... `\|#`, nestable | |
| multi-escape | `\|weird name\|` | symbol names with spaces and case |
| dispatch macros | `#(`, `#*`, `#x`, `#b` | vectors, bit vectors, radix |
| `*read-eval*`, custom readtables | | **statically undecidable in general** |

**Reader conditionals matter most.** `#+sbcl` / `#-sbcl` mean parts of a file
are conditionally present, and a server that ignores them will index code that
does not exist for this implementation — or fail to index code that does.

## 6. CLOS specifically

You flagged this as the area with "tons and tons of options", and that is right.
Concretely, what varies:

**`defclass`** — slot options `:initarg`, `:initform`, `:accessor`, `:reader`,
`:writer`, `:allocation`, `:type`, `:documentation`. Class options `:default-initargs`,
`:metaclass`, `:documentation`. Each `:accessor` **defines two functions** (a
reader and a `setf` writer) whose names appear nowhere else in the file. clef
handles the common accessor cases via `record-slot-accessors`.

**`defgeneric`** — lambda list, `:method` options defining methods inline,
`:method-combination`, `:argument-precedence-order`, `:generic-function-class`.

**`defmethod`** — the hard part. Method *qualifiers* (`:before`, `:after`,
`:around`, or user-defined for custom combinations), and *specialized lambda
lists*:

```lisp
(defmethod draw ((s square) (c (eql :red)) &key (size 1)) ...)
;;               ^^^^^^^^^  class specializer
;;                          ^^^^^^^^^^^^^^ EQL specializer -- a value, not a type
```

Every method is a separate definition of the *same* generic function name, so
go-to-definition on a generic call has **many** correct answers — this is what
`textDocument/implementation` is for, and clef implements it.

**Is CLOS syntax or library?** Both, and the distinction matters here. `defclass`,
`defmethod` etc. are *macros* — ordinary ones, expanding into calls to
`ensure-class` and `ensure-generic-function`. There is no special operator in
CLOS at all. So for a language server they are pure pattern-matching on form
shape, exactly like `defun`. Nothing about CLOS requires evaluator cooperation.
The MOP (`sb-mop`, 102 symbols) is a further layer and explicitly out of scope
for now.

## 7. Genuinely hard, and why

Recorded honestly rather than quietly skipped. Per the scoping decision these are
**deferred follow-ups, not refusals** — none is being written off as permanently
unsupported.

| construct | difficulty | reasonable stance |
|---|---|---|
| `progv` | binds names computed at runtime | no static answer exists; leave |
| `#.` read-time eval | arbitrary code at read time | do not evaluate; treat as opaque |
| custom readtables | program-defined syntax | out of scope until someone needs it |
| user macros that bind | `(with-my-thing (x) ...)` — clef cannot know `x` is bound | needs macroexpansion, or a declared pattern list |
| `loop` | a full sublanguage with ~40 keywords | worth a dedicated parser; high value, high effort |
| method combination | user-defined qualifiers change method semantics | index the definitions; do not model semantics |

**The user-macro problem is the deep one.** Any project can define
`(with-connection (c) ...)` binding `c`, and no amount of standard-form coverage
helps. The two real options are macroexpanding during indexing (accurate,
requires a live image and is unsafe on untrusted code) or a declarative list of
known binding-macro shapes (safe, incomplete, and what most editors do). Worth a
decision later; not needed for ANSI coverage.

## 8. Where this leaves the corpus

The existing corpus is six files, and §3d of the LSP review measured the older
fixture set as median-2-lines with zero uses of `declare`, `let*`, `lambda`,
`handler-case` or `destructuring-bind`. The taxonomy above says what the gap is,
concretely.

Corpus files needed, one per section, each a **valid compilable program** so that
SBCL itself checks the fixture is real Lisp:

| file | covers |
|---|---|
| `07-defining-forms.lisp` | all 18 `def*` forms, plus `(setf f)` names |
| `08-binding-forms.lisp` | every binding form in §3 |
| `09-namespaces.lisp` | one name in six namespaces at once |
| `10-reference-contexts.lisp` | quoting, backquote, `#'`, type positions, `loop` keywords |
| `11-blocks-and-tags.lisp` | `block`/`return-from`, `tagbody`/`go`, implicit blocks |
| `12-reader.lisp` | reader conditionals, block comments, `\|escapes\|`, char literals |

CLOS did not need a new file — `03-clos.lisp` already covered qualifiers and
`eql` specializers, and `07` adds `defgeneric :method` options and `(setf f)`
methods.

**Every corpus file must compile.** `compile-corpus.lisp` enforces it. A fixture
that is not valid Lisp tests nothing, and §3d of the LSP review found the older
fixture set had drifted exactly that way. This caught three real errors while
the files above were being written, including one where a `defvar` of an
unadorned name made `with-slots` illegal everywhere in the file — a genuine
Common Lisp constraint neither of us would have predicted.

SBCL extension coverage (`sb-ext`, `sb-int`, `sb-thread`, `sb-alien` — 845
external symbols between them) is a **second phase**, deliberately after ANSI is
green.

---

## 9. Measured baseline

`05-index-coverage.lisp` reads each corpus file with the **actual Lisp reader**
to compute what it defines, then asks `textDocument/documentSymbol` what clef
found, and names the difference. Reading with `read` rather than by regex means
reader conditionals, block comments and dotted syntax are handled correctly and
for free.

The pre-existing sweep (`03-corpus-sweep.lisp`) reported the whole corpus clean,
because its invariants are *robustness* invariants — nothing crashed, everything
was answered, every range was sane. Five sane ranges satisfy all of them. This
measures the other axis.

```
129 of 179 definitions indexed across the corpus (72%)
```

| file | indexed | notable misses |
|---|---|---|
| `01-packages` | 100% | — |
| `02-bindings` | 86% | `defpackage` |
| `03-clos` | 89% | `defpackage` |
| `04-macros` | 67% | `defpackage`, `threshold`, `label` |
| `05-types-and-declarations` | 78% | `defpackage`, `*default-node*` |
| `06-lexical-edges` | 67% | `naïve-average` (non-ASCII), `\|weird name\|` |
| `07-defining-forms` | 75% | `define-setf-expander`, `define-modify-macro`, `define-symbol-macro`, `define-method-combination` |
| `08-binding-forms` | 97% | `defpackage` |
| `09-namespaces` | 92% | `defpackage` |
| `10-reference-contexts` | 94% | `defpackage` |
| `11-blocks-and-tags` | 92% | `defpackage` |
| `12-reader` | **15%** | see below |

### 9.1 `defpackage` is never indexed — 12 files out of 12

The single most universal gap, and exactly what §2 predicted. Every corpus file
misses its own package name. Cheap to fix and worth doing first.

### 9.2 Four defining forms are unrecognised

`define-setf-expander`, `define-modify-macro`, `define-symbol-macro` and
`define-method-combination` define names that never enter the index. These are
the tail of the 18 and are correspondingly rare, but they are ANSI.

### 9.3 Nested block comments terminate early — **the serious one**

`12-reader.lisp` indexes 15% of what it defines, and the cause is not the
reader constructs it was written to test. Bisected to a precise mechanism:

```lisp
#| outer #| inner |# (defparameter *should-not-appear* 9) |#
```

`*should-not-appear*` **is indexed.** Common Lisp block comments *nest*; clef's
parse stops at the first `|#`, so everything between the inner and outer
terminators is read as code.

Usually that produces harmless junk. When the stranded text contains a vertical
bar, it opens a `|bar-quoted symbol|` that runs to the next bar anywhere in the
file — and every definition after it is lost. That is the whole of the 85% miss
in `12-reader.lisp`: one comment, and the rest of the file goes dark.

Two distinct faults, both real:

- **commented-out code is indexed** — go-to-definition can land inside a comment
- **one nested comment can silently disable indexing for a whole file**

Isolated reproduction in `tmp/bar-probe.lisp`. Every individual construct
(`#\|`, `|bar symbols|`, simple block comments, reader conditionals, `#.`,
char literals) passes on its own; only the nesting fails.

**Deferred, not dismissed.** The parse comes from a precompiled tree-sitter
grammar (`lsp/src/parser/`), so fixing it means grammar work rather than a patch
to clef's walk — a different kind of task, and the one place in this survey where
the fix is not obviously small. It is the highest-priority follow-up.

### 9.4 What the measurement itself got wrong

Recorded because it is the same class of mistake as everything else here. The
first version of `expected-names` bound `*read-eval*` to `nil`, which is the
safe-looking choice — but `read` then *signals* on `#.` and cannot resynchronise,
so the expected list silently stopped at the first read-time-eval. It reported
`12-reader.lisp` as 16 definitions when it defines 33, and coverage as 31% when
it is 15%. A measurement that understates the gap is worse than none.

