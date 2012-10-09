# MaterialRegionRenderer

Segmenting images into material regions using 3D renders as a prior.

## Dependencies

- GraphicsEngine & FLTKGraphicsApp
- ImageStack

NOTE: If using Visual Studio, you must change the "Runtime Library" of ImageStack.lib to be "Multithreaded DLL" (Release) / "Multithreaded Debug DLL" (Debug). Otherwise, ImageStack.lib will be built against a different version of the core runtime library and you'll get multiply defined symbol errors when attempting to use ImageStack and GraphicsEngine (or most code, for that matter) in the same roject.