Napa-FFT3: Overview
===================

Napa-FFT3 is a complete rewrite of Napa-FFT (version 2 is an aborted
experiment).  The goal is still the same: to provide, via a mixture of
cache-friendly algorithms and code generation, FFT routines in Common
Lisp that offer performance comparable to the state of the art.  In
that regard, it is a success: depending on how it's used, Napa-FFT3
is, at most, around three times as slow as FFTW on small or medium
inputs, and can be faster than FFTW for large inputs.  The complete
picture is more complicated than this; see the _Performance_ section
for details.

The goal of Napa-FFT3 isn't only to provide Discrete Fourier Transform
(DFT) routines, but also (rather) to provide buildings blocks to
express common operations that involve DFTs: filtering, convolutions,
etc.  This is what enables Napa-FFT to achieve such high performance
without optimizing at the assembly level.  The _Easy Interface_
section should suffice for most developers; the _Low-level Interface_
is described in another section, and may be of interest to some.

Napa-FFT3 also expressly supports FFTs on real data and inverse FFTs
back to real data.  The _Real Interface_ section describes the
facility, and is used in conjunction with the _Easy Interface_.

Finally, see the _Installation_ section for installation instructions,
and the _Implementation_ section for all the gory details.

Note that Napa-FFT3 currently only supports power-of-two-sized inputs;
even when/if it will gain code for arbitrary sizes, power-of-twos will
most likely be much more efficient, both in terms of runtime and space
usage.

To recapitulate:

 * Installation: installation instructions;
 * Easy Interface: convenience functions;
 * Real Interface: convenience functions for real-only input or
   output;
 * Examples: more examples;
 * Low-level Interface: unsafe access to low-level facilities;
 * Implementation: details on algorithms, the code generation process,
   etc.
 * Performance: benchmarks.
 
Installation
------------

Napa-FFT3 is a regular ASDF system defined in `napa-fft3.asd`.  If
Quicklisp is installed, it suffices to copy the Napa-FFT3 directory
under `~/quicklisp/local-projects`.

Once registered with ASDF, Napa-FFT3 can be loaded by executing
`(asdf:oos 'asdf:load-op "napa-fft3")`, or, with Quicklisp, 
`(ql:quickload "napa-fft3")`.

Easy Interface
--------------

The core of the "easy" interface consists of:

 * `NAPA-FFT:FFT`: forward DFT
 * `NAPA-FFT:IFFT`: inverse DFT
 * `NAPA-FFT:BIT-REVERSE`: bit-reversal routine
 * Windowing support

### `FFT`

Syntax: `fft vec &key dst size in-order scale window => object`.

Arguments and Values:

 * `vec`: sequence of complex samples (`(complex double-float)` values).
 * `dst`: `nil` (default) or a simple vector of complex samples
   (destructively reused).
 * `size`: size of the transform to perform (must be a power of
   two). The default is `(length vec)`.
 * `in-order`: whether the result should be in-order (default, `t`) or
   bit-reversed (`nil`).
 * `scale`: how the result should be scaled: not at all (default,
   `nil`), by _1/sqrt(size)_ (`:sqrt` or `sqrt`), or by _1/n _ (`t`,
   or `:inv`).
 * `window`: whether the input should be multiplied element-wise by 1
   (default, `nil`), a simple array of doubles (`real-sample` or
   `float`), or by a simple array of complex doubles (`complex-sample`
   or `complex`).
 * `object`: a simple array of complex doubles. `dst` if not `nil`,
   otherwise a newly-allocated array.
   
`FFT` computes the DFT of the first `size` values in `vec`.

First, `vec` is converted to a simple array of complex samples if
necessary.  The result is stored in `dst`, or a fresh array of complex
doubles. `dst` may be the same object as `vec` for an in-place
transform.

If `window` is provided, the values in `vec` are multiplied by the
corresponding values in `window` during the transform; similarly, the
values are scaled according to the value of `scale`.

If `in-order` is true, the result is then converted to be in order,
which can take more than half as much time as the FFT itself.

Example:

    CL-USER> (napa-fft:fft '(0 1 2 3 4 5 6 7))
    #(#C(28.0d0 0.0d0) #C(-4.0d0 9.65685424949238d0) #C(-4.0d0 4.0d0)
      #C(-4.0d0 1.6568542494923806d0) #C(-4.0d0 0.0d0)
      #C(-4.0d0 -1.6568542494923806d0) #C(-4.0d0 -4.0d0)
      #C(-4.0d0 -9.65685424949238d0))
    
    CL-USER> (napa-fft:fft '(0 1 2 3 4 5 6 7) :in-order nil)
    #(#C(28.0d0 0.0d0) #C(-4.0d0 0.0d0) #C(-4.0d0 4.0d0) #C(-4.0d0 -4.0d0)
      #C(-4.0d0 9.65685424949238d0) #C(-4.0d0 -1.6568542494923806d0)
      #C(-4.0d0 1.6568542494923806d0) #C(-4.0d0 -9.65685424949238d0))
    
    CL-USER> (napa-fft:fft '(0 1 2 3) :scale nil)
    #(#C(6.0d0 0.0d0) #C(-2.0d0 2.0d0) #C(-2.0d0 0.0d0) #C(-2.0d0 -2.0d0))
    
    CL-USER> (napa-fft:fft '(0 1 2 3) :scale t)
    #(#C(1.5d0 0.0d0) #C(-0.5d0 0.5d0) #C(-0.5d0 0.0d0) #C(-0.5d0 -0.5d0))
    
    ;; specifying :size but not :dst may lead to surprising results
    CL-USER> (napa-fft:fft '(0 1 2 3 5 6 7 8) :size 4 :scale t)
    #(#C(1.5d0 0.0d0) #C(-0.5d0 0.5d0) #C(-0.5d0 0.0d0) #C(-0.5d0 -0.5d0)
      #C(5.0d0 0.0d0) #C(6.0d0 0.0d0) #C(7.0d0 0.0d0) #C(8.0d0 0.0d0))


### `IFFT`

Syntax: `fft vec &key dst size in-order scale window => object`.

Arguments and Values:

 * `vec`: sequence of complex samples (`(complex double-float)` values).
 * `dst`: `nil` (default) or a simple vector of complex samples
   (destructively reused).
 * `size`: size of the transform to perform (must be a power of
   two). The default is `(length vec)`.
 * `in-order`: whether the result should be in-order (default, `t`) or
   bit-reversed (`nil`).
 * `scale`: how the result should be scaled: not at all (`nil`), by
   _1/sqrt(size)_ (`:sqrt` or `sqrt`), or by _1/n _ (default, `t` or `:inv`).
 * `window`: whether the input should be multiplied element-wise by 1
   (default, `nil`), a simple array of doubles (`real-sample` or
   `float`), or by a simple array of complex doubles (`complex-sample`
   or `complex`).
 * `object`: a simple array of complex doubles. `dst` if not `nil`,
   otherwise a newly-allocated array.
   
`IFFT` computes the inverse DFT of the first `size` values in `vec`.

First, `vec` is converted to a simple array of complex samples if
necessary.  The result is stored in `dst`, or a fresh array of complex
doubles. `dst` may be the same object as `vec` for an in-place
transform.

If `in-order` is true, the result is then converted to be
bit-reversed, which can take more than half as much time as the FFT
itself.

If `window` is provided, the values in `vec` are multiplied by the
corresponding values in `window` during the transform; similarly, the
values are scaled according to the value of `scale`.  Note that this
happens *after* the bit-reversal.  The `window` should thus be
bit-reversed itself.  Since this corresponds to a convolution, this is
usually easily satisfied.

Example:

    CL-USER> (napa-fft:ifft (napa-fft:fft '(0 1 2 3)))
    #(#C(0.0d0 0.0d0) #C(1.0d0 0.0d0) #C(2.0d0 0.0d0) #C(3.0d0 0.0d0))
    
    ;; Skipping both bit-reversals saves a lot of time, without
    ;; changing the result; similarly, scaling can happen at any
    ;; step.
    CL-USER> (napa-fft:ifft (napa-fft:fft '(0 1 2 3) :in-order nil
                                      :scale :sqrt)
                        :in-order nil
                        :scale :sqrt)
    #(#C(0.0d0 0.0d0) #C(1.0d0 0.0d0) #C(2.0d0 0.0d0) #C(3.0d0 0.0d0))
    
    ;; The :window argument performs convolutions; simply make
    ;; sure that :in-order is nil.
    CL-USER> (napa-fft:ifft (napa-fft:fft '(0 1 2 3))
                            :window (napa-fft:fft '(0 1/2 0 0)
                                                  :in-order nil))
    #(#C(1.5d0 0.0d0) #C(0.0d0 0.0d0) #C(0.5d0 0.0d0) #C(1.0d0 0.0d0))

### `BIT-REVERSE`

Syntax: `bit-reverse vec &optional dst size => object`.

Arguments and values:

 * `vec`: array of complex or real samples to bit-reverse.
 * `dst`: `nil`, or the destination array of the same type as `vec`.
 * `size`: number of elements to reorder. If `nil`, defaults to the
   size of `vec`. Must be a power of two.
 * `object`: bit-reversed permutation of `vec`.
 
`BIT-REVERSE` permutes the first `size` elements in `vec` to
bit-reversed indices, storing the result in `dst` if provided.  `vec`
may be the same as `dst` for an in-place reversal.

It should usually not be necessary to bit-reverse values explicitly,
but it may still be useful to convert an in-order vector to
out-of-order or vice-versa.

Example:

    CL-USER> (napa-fft:bit-reverse (coerce '(0d0 1d0 2d0 3d0)
                                           'napa-fft:real-sample-array))
    #(0.0d0 2.0d0 1.0d0 3.0d0)
    CL-USER> (napa-fft:bit-reverse * *)
    #(0.0d0 1.0d0 2.0d0 3.0d0)

### Windowing support

TODO. It's the same as Bordeaux FFT.

Real Interface
--------------

The real interface offers three functions specialized to operate on
real (not complex) data:

 * `NAPA-FFT:RFFT` performs in-order real-input FFT.
 * `NAPA-FFT:WINDOWED-RFFT` performs windowed in-order real-input FFTs.
 * `NAPA-FFT:RIFFT` performs in-order real-output inverse FFT.

There are convenient because the result is a vector of real values,
but also offer strong performance improvements (almost halving
computation times) for in-order, out-of-place, transforms.

### `RFFT`

