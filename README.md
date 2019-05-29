# booleanSimplified

[![Build Status](https://travis-ci.org/falgon/bsimplified.svg?branch=master)](https://travis-ci.org/falgon/bsimplified)

For [the blog post](https://falgon.github.io/roki.log/posts/2019/%205%E6%9C%88/29/BooleanAlgebra/).

## Build

```bash
$ stack build
```

## Run

```bash
$ stack exec bsimplified -- "A*~B+~A*~B+A*~B+A*B"
Minterms (Truth patterns):
        m_0 = { A = True, B = True }
        m_1 = { A = True, B = False }
        m_3 = { A = False, B = False }
The state of compression #1: ["m_0 m_1","m_1 m_3"]
Found prime implicants: ["m_0 m_1","m_1 m_3"]
Simplified terms: (m_0 m_1), (m_1 m_3)
```
