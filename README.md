# Lazy Desugaring

This is the companion code repository for the paper **A Lazy Desugaring System for Evaluating Programs with Sugars**.

Given a language defined by reduction semantics (a core language) and a sugar definition, the system will generate its lazy desugaring reduction semantics, with which one can execute programs with sugars. By filtering the evaluation sequence, they can get a sequence consisting solely of terms in the sugar form (the surface language). A lazy desugaring example is shown below.

The core language (in PLT Redex syntax):
```
(e ::= v (if e e e))
(v ::= true false)
```

Its context:
```
(E ::= hole (if E e e))
```

Its reduction semantics:
```
(--> (if true e_1 e_2) e_1)
(--> (if false e_1 e_2) e_2)
```

The sugar `And` is defined on top of the core language.
```
             desugar
(And e1 e2) ---------> (if e1 e2 false)
```

We can then write programs like
```
(And (And true false) false)
```

If we desugar the term whenever a sugar construct matches the context, we will get the following evaluation sequence.
```
    (And (And true false) false)
--> (if (And true false) false false)
--> (if (if true false false) false false)
--> (if false false false)
--> false
```

With our generated semantics, the evaluation sequence is instead as follows.
```
    (And (And true false) false)
--> (And (if true false false) false)
--> (And false false)
--> (if false false false)
--> false
```

It contains
```
    (And (And true false) false)
--> (And false false)
--> false
```
as a subsequence, which is an evaluation sequence consisting solely of sugar `And`s (without `if`).

## Code Structure

The code is written in Racket. Users should install Racket from https://racket-lang.org.

The code structure is as follows.
- `Extend.rkt`: The main logic of the generation of lazy desugaring semantics, which should be required by all example programs.
- `Demo-*.rkt`: Examples of different programs using our system. Open any of them in DrRacket and run it (Ctrl-R or Cmd-R). A trace window will be opened and display the lazy desugaring evaluation sequence (without filtering). `Demo-Boolean.rkt` is the example introduced above. You can follow this code to learn the usage.

## Citation

Please consider citing the following work if you find it useful.
```bibtex
@inproceedings{yang2022lazy,
    author="Yang, Ziyi and Xiao, Yushuo and Guan, Zhichao and Hu, Zhenjiang",
    title="A Lazy Desugaring System for Evaluating Programs with Sugars",
    booktitle="Functional and Logic Programming",
    year="2022",
    publisher="Springer International Publishing",
    pages="243--261",
    isbn="978-3-030-99461-7"
}
```
