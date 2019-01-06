![Newcastle University logo](img/logo-newcastle.svg)
<!-- .element width="350" -->

# Algebraic Graphs

---

Andrey Mokhov

GitHub: [@snowleopard](https://github.com/snowleopard),
Twitter: [@andreymokhov](https://twitter.com/andreymokhov")

----

### Example code

```haskell
-- Construct the induced subgraph of a given graph.
induce :: (a -> Bool) -> Graph a -> Graph a
induce p x = x >>= \a -> if p a then Vertex a else Empty
```

----

### Example math

`$$\sum_{k=1}^{n} x_k$$`