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

- LUT apply multi thread
- On boot: clean apply folder
- Use all but 1 core
- LUT apply on Multiple files at once
- Update all error and help and success and etc messages
- Not forcably convert file formats to PNG?
- During apply step reply (but no mention) about the current state of the process
- Add lut does the dedup on the spot?
- Perserve apply filenames (see ImageConvertBot)
- Applying store is not KV, just list unique keys
- Actually understand the monads and side effects instead of just lifting/idk even know