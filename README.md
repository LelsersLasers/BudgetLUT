# BudgetLUT

Discord bot written in Haskell for uploading, managing, and applying LUTs* to images.
(*Not a true LUT system, simply maps the colors of an input to the closest color in the "LUT".) 

## Commands

All commands:
```
!help
!lut help
!lut add <name> [with image]
!lut rename <code> <new name>
!lut delete <code>
!lut view <code>
!lut list
!lut apply <code> [with image(s)]
```
Note: names can be multi-worded!

## TODO

- Create a lut using a list of raw hex codes
- View a deduped lut (show as a color strip or color square of sorted colors)
- HSV and H options instead of just RGB
- Custom distance function
	- Ex: weight the R, G, and B differences differently
- Not forcably convert file formats to PNG?
- Applying store is not KV, just list unique keys
- Actually understand the monads and side effects instead of just lifting/idk even know
	- And get rid of all the random side effects
- Use Maybe monad
- Use all but 1 core
	- Maybe working?