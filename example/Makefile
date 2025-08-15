# Only for compiler feedback; shader source is compiled within example.lisp

example.metallib: example.air
	xcrun metal $< -o $@

example.air: example.metal
	xcrun metal -c $< -o $@
