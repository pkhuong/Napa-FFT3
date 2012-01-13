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
even when/if it will gain code for arbitrary sizes, powers of two will
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
 * `NAPA-FFT:WINDOWED-FFT`: windows forward DFT

### FFT

Syntax: `fft vec &key dst size in-order scale window => object`.

Arguments and Values:

 * _vec_: sequence of samples.
 * _dst_: nil (default) or a simple vector of complex samples
   (destructively reused).
 * _size_: size of the transform to perform (must be a power of
   two). `(length vec)` if nil (default).
 * _in-order_: whether the result should be in-order (default, t) or
   bit-reversed (nil).
 * _scale_: how the result should be scaled: not at all (default,
   nil), by _1/sqrt(size)_ (:sqrt or sqrt), or by _1/n_ (t,
   or :inv).
 * _window_: simple array of double or complex doubles by which the
   input is multiplied point-wise; no multiplication if nil
   (default).
 * _object_: a simple array of complex doubles. _dst_ if not nil,
   otherwise a newly-allocated array.
   
`FFT` computes the DFT of the first _size_ values in _vec_.

First, _vec_ is converted to a simple array of complex samples if
necessary.  The result is stored in _dst_, or a fresh array of complex
doubles. _dst_ may be the same object as _vec_ for an in-place
transform.

If _window_ is non-nil, each value in _vec_ is multiplied by the
corresponding value in _window_ during the transform; similarly, the
values are scaled according to the value of _scale_.

If _in-order_ is true, the result is then converted to be in order,
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
    
    CL-USER> (napa-fft:fft '(0 1 2 3 5 6 7 8) :size 4 :scale t)
    #(#C(1.5d0 0.0d0) #C(-0.5d0 0.5d0) #C(-0.5d0 0.0d0) #C(-0.5d0 -0.5d0))


### IFFT

Syntax: `fft vec &key dst size in-order scale window => object`.

Arguments and Values:

 * _vec_: sequence of samples.
 * _dst_: nil (default) or a simple vector of complex samples
   (destructively reused).
 * _size_: size of the transform to perform (must be a power of
   two). `(length vec)` if nil (default).
 * _in-order_: whether the result should be in-order (default, t) or
   bit-reversed (nil).
 * _scale_: how the result should be scaled: not at all (nil), by
   _1/sqrt(size)_ (:sqrt or sqrt), or by _1/n_ (default, t or :inv).
 * _window_: simple array of real or complex sample by which the
   out-of-order input samples are multiplied elementwise.  If nil
   (default), no multiplication is performed.
 * _object_: a simple array of complex doubles. _dst_ if not nil,
   otherwise a newly-allocated array.
   
`IFFT` computes the inverse DFT of the first _size_ values in _vec_.

First, _vec_ is converted to a simple array of complex samples if
necessary.  The result is stored in _dst_, or a fresh array of complex
doubles. _dst_ may be the same object as _vec_ for an in-place
transform.

If _in-order_ is true, the result is then converted to be
bit-reversed, which can take more than half as much time as the FFT
itself.

If _window_ is non-nil, each value in _vec_ is multiplied by the
corresponding value in _window_ during the transform; similarly, the
values are scaled according to _scale_.  Note that this happens
_after_ the bit-reversal.  _window_ should thus be bit-reversed
itself.  Since this corresponds to a convolution, this is usually
easily satisfied.

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

### BIT-REVERSE

Syntax: `bit-reverse vec &optional dst size => object`.

Arguments and values:

 * _vec_: array of complex or real samples to bit-reverse.
 * _dst_: nil, or the destination array of the same type as _vec_.
 * _size_: number of elements to reorder. If nil, defaults to the
   size of _vec_. Must be a power of two.
 * _object_: bit-reversed permutation of _vec_.
 
`BIT-REVERSE` permutes the first _size_ elements in _vec_ to
bit-reversed indices, storing the result in _dst_ if provided.  _vec_
may be the same as _dst_ for an in-place reversal.

It should usually not be necessary to bit-reverse values explicitly,
but it may still be useful to convert an in-order vector to
out-of-order or vice-versa.

Example:

    CL-USER> (napa-fft:bit-reverse (coerce '(0d0 1d0 2d0 3d0)
                                           'napa-fft:real-sample-array))
    #(0.0d0 2.0d0 1.0d0 3.0d0)
    CL-USER> (napa-fft:bit-reverse * *)
    #(0.0d0 1.0d0 2.0d0 3.0d0)

### WINDOWED-FFT

TODO. It's the same as Bordeaux FFT.

Real Interface
--------------

The real interface offers three functions specialized to operate on
real (not complex) data:

 * `NAPA-FFT:RFFT` performs in-order real-input FFT.
 * `NAPA-FFT:RIFFT` performs in-order real-output inverse FFT.
 * `NAPA-FFT:WINDOWED-RFFT` performs windowed in-order real-input FFTs.

There are convenient because the result is a vector of real values,
but also offer strong performance improvements (almost halving
computation times) for in-order, out-of-place, transforms.

### RFFT

