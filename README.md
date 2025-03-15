# BudgetLUT

Discord bot written in Haskell for uploading, managing, and applying LUTs* to images. (*Not a true LUT system, simply maps the colors of an input to the closest color in the LUT.) 

## Commands

```
!help
!lut help
!lut add <name> [with image]
!lut rename <code> <new name>
!lut delete <code>
!lut view <code>
!lut list
!lut apply <code> [with images]
```

## TODO

- Update all error and help and success and etc messages
- Not forcably convert file formats to PNG
- Don't over use the LUT kv store as a shortcut to make the apply filenames unique
- Actually understand the monads and side effects instead of just lifting/idk even know