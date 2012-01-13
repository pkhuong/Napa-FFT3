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
 * `NAPA-FFT:WINDOWED-FFT`: windowed forward DFT

### FFT

Syntax: `fft vec &key dst size in-order scale window => vector`.

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
 * _vector_: a simple array of complex doubles. _dst_ if not nil,
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
    
    ;; the same, but bit reversed
    CL-USER> (napa-fft:fft '(0 1 2 3 4 5 6 7) :in-order nil)
    #(#C(28.0d0 0.0d0) #C(-4.0d0 0.0d0) #C(-4.0d0 4.0d0) #C(-4.0d0 -4.0d0)
      #C(-4.0d0 9.65685424949238d0) #C(-4.0d0 -1.6568542494923806d0)
      #C(-4.0d0 1.6568542494923806d0) #C(-4.0d0 -9.65685424949238d0))

    ;; :scale nil is the default
    CL-USER> (napa-fft:fft '(0 1 2 3) :scale nil)
    #(#C(6.0d0 0.0d0) #C(-2.0d0 2.0d0) #C(-2.0d0 0.0d0) #C(-2.0d0 -2.0d0))
    
    ;; the same, but scaled by 1/4
    CL-USER> (napa-fft:fft '(0 1 2 3) :scale t)
    #(#C(1.5d0 0.0d0) #C(-0.5d0 0.5d0) #C(-0.5d0 0.0d0) #C(-0.5d0 -0.5d0))
    
    ;; again, scaled by 1/sqrt(4) = 1/2
    CL-USER> (napa-fft:fft '(0 1 2 3 5 6 7 8) :size 4 :scale :sqrt)
    #(#C(3.0d0 0.0d0) #C(-1.0d0 1.0d0) #C(-1.0d0 0.0d0) #C(-1.0d0 -1.0d0))


### IFFT

Syntax: `ifft vec &key dst size in-order scale window => vector`.

Arguments and Values:

 * _vec_: sequence of Fourier coefficients.
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
 * _vector_: a simple array of complex doubles. _dst_ if not nil,
   otherwise a newly-allocated array.
   
`IFFT` computes the inverse DFT of the first _size_ Fourier
coefficients in _vec_.

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

    ;; the defaults ensure fft and ifft are inverses
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
    ;; sure that :in-order is nil when transforming the convolved
    ;; vector.  Here, the input is shifted one element to the
    ;; right and scaled by 1/2.
    CL-USER> (napa-fft:ifft (napa-fft:fft '(0 1 2 3))
                            :window (napa-fft:fft '(0 1/2 0 0)
                                                  :in-order nil))
    #(#C(1.5d0 0.0d0) #C(0.0d0 0.0d0) #C(0.5d0 0.0d0) #C(1.0d0 0.0d0))

### BIT-REVERSE

Syntax: `bit-reverse vec &optional dst size => vector`.

Arguments and values:

 * _vec_: array of complex or real samples to bit-reverse.
 * _dst_: nil, or the destination array of the same type as _vec_.
 * _size_: number of elements to reorder. If nil, defaults to the
   size of _vec_. Must be a power of two.
 * _vector_: bit-reversed permutation of _vec_.
 
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
    
    ;; bit-reverse is its own inverse.
    CL-USER> (napa-fft:bit-reverse * *)
    #(0.0d0 1.0d0 2.0d0 3.0d0)

### WINDOWED-FFT

TODO. It's the same as Bordeaux FFT.

Real Interface
--------------

The real interface offers three functions specialized to operate on
real (not complex) data:

 * `NAPA-FFT:RFFT` performs in-order real-input FFTs.
 * `NAPA-FFT:RIFFT` performs in-order real-output inverse FFTs.
 * `NAPA-FFT:WINDOWED-RFFT` performs windowed in-order real-input FFTs.

There are convenient because the result is a vector of real values,
but also offer strong performance improvements (almost halving
computation times) for in-order, out-of-place, transforms, at the
expense of a little precision.

### RFFT

Syntax: `rfft vec &key dst size scale => vector`

Arguments and values:

 * _vec_: sequence of real samples.
 * _dst_: nil, or a simple vector of complex doubles (destructively
   reused).
 * _size_: size of the transform to perform. Defaults to `(length vec)`.
   Must be a power of two.
 * _scale_: how the result should be scaled: not at all (default,
   nil), by _1/sqrt(size)_ (:sqrt or sqrt), or by _1/n_ (t,
   or :inv).
 * _vector_: a simple array of complex doubles. _dst_ if not nil,
   otherwise a newly-allocated array.

`RFFT` computes the in-order DFT of the first _size_ samples in _vec_.

_vec_ is converted, if necessary, to a simple array of doubles.  Its
DFT is then re-expressed as a half-size DFT of complex samples, and
the result is written in _dst_.

Example:

    ;; this should always get the same value
    CL-USER> (napa-fft:rfft '(0 1 2 3))
    #(#C(6.0d0 0.0d0) #C(-2.0d0 2.0d0) #C(-2.0d0 0.0d0)
      #C(-1.9999999999999998d0 -2.0d0))


### RIFFT

Syntax: `rifft vec &key dst size scale => vector`

Arguments and values:
 * _vec_: sequence of complex Fourier coefficients. Destroyed.
 * _dst_: nil, or a simple vector of doubles. Destructively reused.
 * _size_: size of the inverse transform to perform. Defaults to
   `(length vec)`.  Must be a power of two.
 * _scale_: how the result should be scaled: not at all (nil), but
   _1/sqrt(size)_ (:sqrt or sqrt) or by _1/n_ (default, t or :inv).
 * _vector_: a simple array of doubles. _dst_ if not nil, otherwise a
   newly-allocated array.

`RIFFT` computes the in-order inverse DFT of the first _size_ Fourier
coefficients in _vec_.

_vec_ is converted, if necessary, to a simple array of complex
doubles.  Its inverse DFT is then re-expressed, destructively as a
half-size DFT of complex doubles.

Example:

    CL-USER> (napa-fft:rifft (napa-fft:rfft '(0 1 2 3)))
    #(0.0d0 1.0d0 2.0d0 3.0d0)

### WINDOWED-RFFT

Same.

Examples
--------

I'm no good at this signal processing stuff; most of what I know about
this comes, indirectly, from classical music training.

I'll use sox to play sound files easily, and the following function to
convert float samples to `s32` files:

    (defun emit-raw32-file (file data &optional (max 1d0))
      (with-open-file (s file :direction :output :element-type '(signed-byte 32)
                              :if-exists :supersede)
        (write-sequence (map-into (make-array (length data) :element-type '(signed-byte 32))
                                  (lambda (data)
                                    (let ((x (/ (float (realpart data) 1d0)
                                                max)))
                                      (floor (* x (1- (ash 1 31))))))
                                  data)
                        s)
        file))

Napa-FFT3 does a fair amount of runtime compilation and cached
computations.  If an operation is slow (more than one second), it was
almost certainly due to a slow one-time operation; try running it
again.  Also, there's a lot of gratuitous consing, here.  Nearly all
the operations could be in-place and (nearly) non-consing without any
change to the code.

### Generate a single tone

The standard frequency of the middle A is 440 Hz nowadays.  Sound is
commonly generated, on computers, at 44100 Hz.  Let's generate a
signal of 64k points that, when played back at 44100 Hz, will result
in an A440.  The following function returns a vector with zeros
everywhere except where specified in the first argument, a list
designator of indices.

    (defun impulse (i n &optional (value 1d0))
      (let ((vec (make-array n :element-type 'napa-fft:complex-sample
                               :initial-element (complex 0d0 0d0))))
        (dolist (i (if (listp i) i (list i)) vec)
          (setf (aref vec i) (complex (float value 1d0))))))

The DFT computes a vector such that each entry corresponds to the
amount of energy in the frequency(ies) corresponding to that entry.
The _i_ th entry of an _N_ -element Fourier-coefficient vector, with an
original sampling frequency of _F_ corresponds to a signal frequency
of _iF/N_ (+/- _F/N_ ).  So, if we want a wave at 440 Hz in in a
vector of 64k played back at 44100 Hz, we have to put energy in the
_(440/44100)*65536_ th bin.

    ;; create a vector with 1 only in the bin corresponding to 440 Hz,
    ;; convert back to the time domain, and save the double values as
    ;; a file of (signed-byte 32) values.
    CL-USER> (emit-raw32-file "~/napa-fft3/example/a440.s32"
                              (napa-fft:ifft (impulse (round (* 440 65536) 44100) 65536)
                                             :scale nil))
    "~/napa-fft3/example/foo.s32"

    $ play -r44100 a440.s32 # play is a sox command; play a file os
                           # signed 32 bit samples at 44100 Hz.
                           # Should sound like an A440!

### Generate a chord

We (westerners) are mostly used to a scale system based on 12
equispaced (not quite, but close enough) semi-tones.  Reality tends to
enforce that the scale covers a range of frequences for _F_ to _2F_.
For example, while the middle A is at 440 Hz, the one in the next
octave is at 880 Hz.  The middle C is 9 half-tones lower than the
middle A, at `(* 440 (expt .5 (/ 9 12)))`, around 262 Hz (264 Hz in
the real world).  E is then 2 tones higher, at `(* 262 (expt 2 (/ 4 12)))`
~= 330 Hz, and G a tone and a half higher again, `(* 330 (expt 2 (/ 3 12)))`
~= 392 Hz.

Thus, to head the boring middle C/E/G chord, we need energy at 262,
330 and 392 Hz; note how the energy is 1/3 at each point, so that the
total comes to 1.

    CL-USER> (emit-raw32-file "~/napa-fft3/example/chord.s32"
                              (napa-fft:ifft (impulse (list (round (* 262 65536)
                                                                   44100)
                                                            (round (* 330 65536)
                                                                   44100)
                                                            (round (* 392 65536)
                                                                   44100))
                                                      65536
                                                      (/ 3d0))
                                             :scale nil))
    "~/napa-fft3/example/foo.s32"
    
    $ play -r44100 chord.s32 # you should recognize this sound

### Filter frequencies out

First, let's save our time-domain chord signal:

    CL-USER> (defparameter *chord* (napa-fft:ifft
                                    (impulse (list (round (* 262 65536) 44100)
                                                   (round (* 330 65536) 44100)
                                                   (round (* 392 65536) 44100))
                                             65536
                                             (/ 3d0))
                                    :scale nil))
    *CHORD*

Here's a function to generate random noise and another to average two vectors:

    (defun noise (n &optional (range .5d0))
      (let ((2range (* 2 range)))
        (map-into (make-array n :element-type 'napa-fft:complex-sample)
                  (lambda ()
                    (complex (- (random 2range) range))))))
    
    (defun m+ (x y &optional (scale .5d0))
      (map 'napa-fft:complex-sample-array
           (lambda (x y)
             (* scale (+ x y)))
           x y))

We can noise our signal up by adding noise to the chord:

    CL-USER> (defparameter *noisy-chord* (m+ *chord* (noise 65536)))
    *NOISY-CHORD*
    
    $ play -r44100 noised-chord.s32 # still recognizable, but
                                    # annoying.

Let's say that we know that the only interesting stuff is in the
central octave.  We could zero out all the frequencies outside the
262-524 Hz range.

First, we have to convert the noisy signals in the frequency domain:

    CL-USER> (defparameter *noisy-chord-freq* (napa-fft:fft *noisy-chord*))
    *NOISY-CHORD-FREQ*

Then, we want to replace everything outside `(round (* 262 65536) 44100)` and
`(round (* 524 65536) 44100)` with 0:

    CL-USER> (defparameter *octave-chord-freq* (copy-seq *noisy-chord-freq*))
    *OCTAVE-CHORD-FREQ*
    CL-USER> (prog1 nil
               (fill *octave-chord-freq* (complex 0d0)
                     :end (round (* 262 65536) 44100))
               (fill *octave-chord-freq* (complex 0d0)
                     :start (1+ (round (* 524 65536) 44100))))

Now, we can convert back in the time domain and listen to the result:

    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord.s32"
                              (napa-fft:ifft *octave-chord-freq*))

Much better.

#### Filter frequencies out in a real signal

Note that, as is often the case, we know that our signal is real (has
no imaginary component).  In this case, we can use real-only FFT/IFFT
instead:

    CL-USER> (defparameter *noisy-chord-freq*
               (napa-fft:rfft (map 'napa-fft:real-sample-array
                                   #'realpart
                                   *noisy-chord*)))
    *NOISY-CHORD-FREQ*
    CL-USER> (prog1 nil
               (fill *octave-chord-freq* (complex 0d0)
                     :end (round (* 262 65536) 44100))
               (fill *octave-chord-freq* (complex 0d0)
                     :start (1+ (round (* 524 65536) 44100))))
    NIL
    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord.s32"
                              (napa-fft:rifft *octave-chord-freq*))

The result is the same, but (once the routines are compiled), each
FFT/IFFT is about twice as fast.  Mind the fact that rifft is
destructive on its input, however.

#### Filter frequencies out during the IFFT

We can do this directly, by filtering during the inverse FFT.
Replacing most values with 0 and leaving the rest along is equivalent
to multiplying by 0 or 1.  Usually, we want a more gradual dampening,
so we'll multiply by intermediate values as well.

    CL-USER> (defparameter *filter* (make-array 65536 
                                                :element-type 'napa-fft:real-sample
                                                :initial-element 0d0))
    *FILTER*
    CL-USER> (prog1 nil
               (fill *filter* 1d0
                     :start (round (* 262 65536) 44100)
                     :end   (1+ (round (* 524 65536) 44100))))
    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord2.s32"
                              (napa-fft:ifft *noisy-chord-freq*
                                             :window *filter*))
    "~/napa-fft3/example/octave-chord.s32"

And we have the same final result.  Obviously, an advantage is that we
can compute the filter once, and easily apply it directly.

Another advantage is that we can bit-reverse (permute) the filter
instead of bit-reversing after the FFT and before the IFFT.  Again,
the final result is the same, but we only bit-reverse the constant
filter vector instead of many frequency domain vector.

    CL-USER> (defparameter *bit-reversed-filter* (napa-fft:bit-reverse *filter*))
    *BIT-REVERSED-FILTER*
    ;; neither the fft or the ifft is in order; the filter itself is
    ;; out of order
    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord3.s32"
                              (napa-fft:ifft 
                               (napa-fft:fft *noisy-chord* :in-order nil)
                               :in-order nil
                               :window *bit-reversed-filter*))
    "~/napa-fft3/example/octave-chord3.s32"

### Filter low-amplitude noise out

In the previous example we already knew at what frequency the
interesting signal was.  That's sometimes the case (e.g. when some of
us unconsciously filter out annoyingly-high-pitched voices), but
somewhat uncommon.

Instead of applying a constant filter on frequencies, we can attempt
to find which frequencies dominate (have a high energy), and eliminate
the rest.

#### Filter by strength

The easiest way is to find the frequency with the most energy, and
clamp out everything that's below a fixed fraction (e.g. 1%) of that.

    (defun amplitude-clamp-fraction (vector fraction)
      (let ((limit (* (reduce #'max vector :key #'abs)
                      fraction)))
        (map 'napa-fft:complex-sample-array
             (lambda (x)
               (if (< (abs x) limit)
                   (complex 0d0)
                   x))
             vector)))

This function performs the same operation, regardless of the order in
which its data is permuted.  We can thus use out-of-order FFTs and
IFFTs here as well.

    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord4.s32"
                              (napa-fft:ifft
                               (amplitude-clamp-fraction
                                (napa-fft:fft *noisy-chord* :in-order nil)
                                1d-2)
                               :in-order nil))
    "~/napa-fft3/example/octave-chord4.s32"

When frequencies below 1% of the max energy are zeroed out, the amount
of noise is significantly reduced, but there's still an annoying
ringing.  Increasing the limit to 2% improves matters a lot; however,
if we set too high a limit, we'll start to filter out interesting, if
somewhat weak, sounds.

#### Filter by population

We could also know ahead of time that there are only a few (e.g. 3)
interesting frequencies in the signal.  In that case, we can find the
k frequencies with the most energy, and remove everything else:

    (defun amplitude-clamp-k (vector k)
      (let* ((n      (length vector))
             (values (make-array n)))
        (dotimes (i n)
          (setf (aref values i) (cons (abs (aref vector i)) i)))
        ;; this would be a heap or a quickselect if I cared
        (sort values #'> :key #'car)
        (let ((result (make-array n
                                  :element-type 'napa-fft:complex-sample
                                  :initial-element (complex 0d0))))
          (loop repeat k
                for (nil . i) across values
                do (setf (aref result i) (aref vector i))
                finally (return result)))))

Again, the input may be permuted without changing the result, so we
can stick to out-of-order transforms:

    CL-USER> (emit-raw32-file "~/napa-fft3/example/octave-chord5.s32"
                              (napa-fft:ifft
                               (amplitude-clamp-k
                                (napa-fft:fft *noisy-chord* :in-order nil)
                                3) ; we're cheating here (:
                               :in-order nil))
    "~/napa-fft3/example/octave-chord5.s32"

### Windowing chunks of long signals

TODO.  Let's filter Justin Bieber out of his songs.

### Multiplying integers or polynomials via a convolution

So far, we've been using point-wise multiplication in the frequency
domain to filter frequencies out.  We can also see it as a nice way to
execute convolutions.  We can use this to implement fast
multiplication of polynomials, or integer, with the right encoding.
Please don't use it for bignum multiplication without checking the
precision of the transforms.

Let's multiply 1005 by 1234. 1005 is 1\*10^3 + 0\*10^2 + 0\*10^1 +
5\*10^0, which we can encode, as a vector: #(1 0 0 5 0 0 0 0) (note
the padding at the end), and similarly for 1234.  Again, filtering is
oblivious to any permutation (as long as the windowing vector is
bit-reversed), so we can do everything out of order.

    CL-USER> (napa-fft:fft #(1 0 0 5 0 0 0 0) :in-order nil)
    #(#C(6.0d0 0.0d0) #C(-4.0d0 0.0d0) #C(1.0d0 5.0d0) #C(1.0d0 -5.0d0)
      #C(-2.5355339059327378d0 -3.5355339059327378d0)
      #C(4.535533905932738d0 3.5355339059327378d0)
      #C(4.535533905932738d0 -3.5355339059327378d0)
      #C(-2.5355339059327378d0 3.5355339059327378d0))
    CL-USER> (napa-fft:fft #(1 2 3 4 0 0 0 0) :in-order nil)
    #(#C(10.0d0 0.0d0) #C(-2.0d0 0.0d0) #C(-2.0d0 2.0d0) #C(-2.0d0 -2.0d0)
      #C(-0.41421356237309515d0 -7.242640687119286d0)
      #C(2.414213562373095d0 1.2426406871192857d0)
      #C(2.414213562373095d0 -1.2426406871192857d0)
      #C(-0.41421356237309515d0 7.242640687119286d0))
    CL-USER> (napa-fft:ifft * :window ** :in-order nil)
    #(#C(0.9999999999999982d0 0.0d0) #C(1.9999999999999991d0 0.0d0)
      #C(3.0d0 0.0d0) #C(9.0d0 0.0d0) #C(10.000000000000002d0 0.0d0)
      #C(15.0d0 0.0d0) #C(20.0d0 0.0d0) #C(-8.881784197001252d-16 0.0d0))

If we remove the imaginary portions (which are all 0) and round some
numerical errors away, we find #(1 2 3 9 10 15 20 0), this time with only
one element of padding: 1\*10^6 + 2\*10^5 + 3\*10^4 + 9\*10^3 +
10\*10^2, etc.  If we take care of the carries, we find 1240170, which
is indeed 1005 \* 1234.

Of course, we can also exploit the fact that the input and output are
all reals to use `rfft` and `rifft`.  We can do even better with
`%2rfft`, which performs 2 real fft at at the time.  However, if we do
that, we hae to use in-order transforms.  It's a trade off, and even
if we're only concerned with computation times, the right answer
depends on a lot of variables.

    ;; the results are returned one after the other in a single
    ;; vector of complex doubles
    CL-USER> (napa-fft:%2rfft '(1 0 0 5 0 0 0 0)
                              '(1 2 3 4 0 0 0 0))
    #(#C(6.0d0 0.0d0) #C(-2.5355339059327378d0 -3.5355339059327378d0)
      #C(1.0d0 5.0d0) #C(4.535533905932738d0 -3.535533905932738d0)
      ...)
      
    CL-USER> (let ((x (subseq * 0 8))
                   (y (subseq * 8)))
               (napa-fft:rifft (map-into x #'* x y)))
    #(0.9999999999999991d0 2.0d0 2.9999999999999964d0 9.0d0 10.0d0 15.0d0
      20.000000000000004d0 -8.881784197001252d-16)

