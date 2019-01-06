<div style="text-align: right;">
<img src="img/logo-newcastle.svg" width="350" />
</div><br/>

# Algebraic Graphs

---

Andrey Mokhov

<small>GitHub: [@snowleopard](https://github.com/snowleopard),
Twitter: [@andreymokhov](https://twitter.com/andreymokhov")</small><br/>

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