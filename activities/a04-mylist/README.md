## Activity 4 -- Convert from Haskell Lists to Your Own Lists (and back...)

You have been given the following data type:

```
data MyList a = Cons a (MyList a)
              | Nil
     deriving (Show,Eq)
```

Your work is to write two functions that convert from this type to Haskell lists and back:
- `list2mylist :: [a] -> MyList a`
- `mylist2list :: MyList a -> [a]`

Hand this in by committing it to your repository as usual.

