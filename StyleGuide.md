

Type class instances

- When defined in the companion object, they are written in lower camel case. Example:
```scala

object Data {
  implicit val `eq` = new Eq[Data] { ... }
}
```

Macros

- Generated macro symbols should be preceded by a dollar sign.
