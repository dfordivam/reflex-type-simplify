# reflex-type-simplify
Use Hedgehog (like QuickCheck) to simplify types (and life) when using reflex-frp

Work in progess.. This is just a preview...

Also See https://github.com/hedgehogqa/haskell-hedgehog

```
Enter the starting type
Dynamic t (m (Event t Int))
Enter the target type
m (Event t Int)
  ✗ <interactive> failed after 117 tests and 86 shrinks.

        ┏━━ Main.hs ━━━
    108 ┃ testStr :: String -> String -> Property
    109 ┃ testStr str1 str2 = withTests (TestLimit 1000000) . property $ do
    110 ┃   let
    111 ┃       (Right init) = parseSourceType str1
    112 ┃       (Right res) = parseSourceType str2
    113 ┃
    114 ┃
    115 ┃   -- Result --------------------------
    116 ┃   s <- forAll $ (opsManualRecurse init)
        ┃   │ ((join) . (fmap (fmap (switchPromptlyDyn))) . (fmap (holdDyn)) . (dyn))
    117 ┃   assert $ (applyOpTree init s) /= (Just res)
        ┃   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    This failure can be reproduced by running:
    > recheck (Size 16) (Seed 3033341105344009770 5793933252947687363) <property>
```


This work is inspired by 
http://clrnd.com.ar/posts/2017-04-21-the-water-jug-problem-in-hedgehog.html
